#=========================================================================
# Purpose: Main R file for Preparing + Cleaning MGH Data
#          Parallel structure to Mayo clean.R
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(lfe)

source("src/utils.R")  # contains complaint_map (named 'complaints')

path <- "~/Sue Goldie Dropbox/Jacob Jameson/ED MGH"

#=========================================================================
# 1. LOAD ALL SHEETS
#=========================================================================

data_enc <- read_excel(
  paste0(path, "/2024 03 13 ED Effectiveness Data.xlsx"), 
  sheet = "encounters", col_types = c("text", 
                                      "text", "text", "text", "text", "text", 
                                      "numeric", "date", "text", "text", 
                                      "date", "numeric", "text", "text", 
                                      "text", "date", "text", "text", 
                                      "numeric", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "numeric", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "text", "date", "date", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text"))

data_img <- read_excel(
  paste0(path, "/2024 03 13 ED Effectiveness Data.xlsx"), 
  sheet = "imaging", col_types = c("text", 
                                   "date", "date", "date", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric"))

data_lab <- read_excel(
  paste0(path, "/2024 03 13 ED Effectiveness Data.xlsx"), 
  sheet = "labs", col_types = c("text", "date", "date", "text"))

data_prov <- read_excel(
  paste0(path, "/2024 03 13 ED Effectiveness Data.xlsx"), 
  sheet = "providers", col_types = c("text", "date", "text", "text", "text"))

data_iv <- read_excel(
  paste0(path, "/2024 03 13 ED Effectiveness Data.xlsx"), 
  sheet = "medications", col_types = c("text", "text", "date", "text", "text"))


#=========================================================================
# 2. PROCESS ENCOUNTERS
#=========================================================================

# Filter to adults
data_enc <- filter(data_enc, age_at_arrival_years >= 18)

# Convert datetimes
data_enc <- data_enc %>%
  mutate(
    arrival_dts = as_datetime(arrival_dts),
    ed_departure_dts = as_datetime(ed_departure_dts),
    triage_started_dts = as_datetime(triage_started_dts)
  )

# --- ED LOS (minutes) ---
# Parallel to Mayo's ED_LOS
data_enc$ED_LOS <- as.numeric(difftime(data_enc$ed_departure_dts, 
                                       data_enc$arrival_dts, units = "mins"))

# --- Disposition ---
# Parallel to Mayo's ED_DISPOSITION
data_enc$admit <- ifelse(grepl("Admit", data_enc$ed_disposition, ignore.case = TRUE), 1, 0)
data_enc$discharge <- ifelse(grepl("Discharge", data_enc$ed_disposition, ignore.case = TRUE), 1, 0)
data_enc$observation <- ifelse(grepl("Observation|Obs", data_enc$ed_disposition, ignore.case = TRUE) &
                                 data_enc$admit == 0, 1, 0)

# --- Disposition time ---
# place_in_ed_obs_dts and earliest_bed_req_dts read directly as dates
# dispo_time = min(departure, bed request, obs placement) -- parallel to Mayo
data_enc$dispo_time <- pmin(data_enc$ed_departure_dts,
                            data_enc$earliest_bed_req_dts,
                            data_enc$place_in_ed_obs_dts,
                            na.rm = TRUE)

data_enc$time_to_dispo <- as.numeric(difftime(data_enc$dispo_time, 
                                              data_enc$arrival_dts, units = "mins"))
data_enc$time_to_dispo <- ifelse(data_enc$time_to_dispo < 0, NA, data_enc$time_to_dispo)
data_enc$time_to_dispo <- ifelse(data_enc$time_to_dispo > data_enc$ED_LOS, 
                                 data_enc$ED_LOS, data_enc$time_to_dispo)

# --- Wait time (parallel to Mayo: rooming - arrival) ---
# Use triage_started_dts as proxy for first contact
#data_enc$wait_time <- as.numeric(difftime(data_enc$triage_started_dts, 
#                                          data_enc$arrival_dts, units = "mins"))
#data_enc$wait_time <- ifelse(is.na(data_enc$wait_time) | data_enc$wait_time <= 0, 
#                             1, data_enc$wait_time)

# --- Vital signs ---
data_enc <- data_enc %>%
  mutate(
    systolic_bp = as.numeric(sub("/.*", "", first_bp_reading)),
    diastolic_bp = as.numeric(sub(".*/", "", first_bp_reading)),
    pulse = as.numeric(first_pulse_reading),
    respiratory_rate = as.numeric(first_rr_reading),
    across(c(hypertension, diabetes, cad, chf, copd, asthma, aud, ivdu, afib, cva, cancer), 
           ~ifelse(is.na(.), 0, as.numeric(.)))
  )

data_enc$tachycardic <- ifelse(!is.na(data_enc$pulse) & data_enc$pulse > 100, 1, 0)
data_enc$tachypneic <- ifelse(!is.na(data_enc$respiratory_rate) & data_enc$respiratory_rate > 20, 1, 0)
# NOTE: MGH temps are in Fahrenheit, Mayo in Celsius. Use 99.5F threshold.
data_enc$febrile <- ifelse(!is.na(data_enc$first_temp_reading) & data_enc$first_temp_reading > 99.5, 1, 0)
data_enc$hypotensive <- ifelse(!is.na(data_enc$systolic_bp) & data_enc$systolic_bp < 90, 1, 0)

# --- Race ---
data_enc <- data_enc %>%
  mutate(
    race = case_when(
      str_detect(str_to_lower(race_list), "black|african") ~ "black",
      str_detect(str_to_lower(race_list), "asian|pacific islander") ~ "asian",
      str_detect(str_to_lower(race_list), "native") ~ "native",
      str_detect(str_to_lower(race_list), "white") ~ "white",
      str_detect(str_to_lower(race_list), "unknown|choose not|unable|null") ~ "unknown",
      TRUE ~ "other"
    )
  )

data_enc <- data_enc %>%
  mutate(care_area = case_when(
    str_detect(initial_care_area, "^Eval|Eval Virtual") ~ "Eval",
    str_detect(initial_care_area, "Fast Track|FT |FT Virtual|FT ARRIVAL") ~ "Fast track",
    str_detect(initial_care_area, "Pedi") ~ "Pedi",
    str_detect(initial_care_area, "Urgent|Acute|Orange") ~ "Urgent",
    str_detect(initial_care_area, "Super Track|ST |ST Virtual|ST ARRIVAL") ~ "Super track",
    str_detect(initial_care_area, "CDU|Virtual Observation") ~ "Observation",
    TRUE ~ "Other"
  ))

# --- ESI (acuity) ---
data_enc$ESI <- data_enc$acuity

# --- Chief complaint mapping ---
# (uses same complaint_map from utils.R, named 'complaints')
data_enc$rfv <- str_to_lower(data_enc$rfv)
data_enc$CHIEF_COMPLAINT <- data_enc$rfv

complaints <- list(
  'Abdominal Complaints' = c(
    'abdominal pain', 'pelvic pain', 'testicle pain',
    'rectal pain', 'constipation', 'diarrhea', 'nausea', 'vomiting',
    'nausea/vomiting', 'hematemesis', 'inguinal hernia', 'nephrolithiasis',
    'gi problem', 'gi bleeding', 'rectal bleeding', 'groin pain', 
    'rectal prolapse', 'bloated', 'abdominal cramping',
    'hernia', 'fecal impaction', 'diverticulitis', 'ascites',
    'pancreatitis', 'cholelithiasis', 'abdominal injury',
    'vomiting blood', 'black or bloody stool', 'vomitting with diarrhea',
    'gas', 'gastritis', 'umbilical hernia', 'hernia', 'abdominal injury',
    'irritable bowel syndrome', 'fecal incontinence', 'gastroesophageal reflux',
    'reflux'
  ),
  
  'Abnormal Test Results' = c(
    'abnormal lab', 'hypotension', 'abnormal potassium', 'elevated lfts',
    'abnormal sodium', 'abnormal ecg',
    'abnormal x-ray', 'abnormal calcium', 'abnormal cxr', 'abnormal potassium',
    'abnormal sodium', 'leukocytosis', 'hypercalcemia', 'proteinuria',
    'phs amb ren electrolyte abnormality', 'phs amb nut underweight',
    'anemia','blood sugar problem','hypertension','hyperglycemia',
    'hypoglycemia','diabetes','diabetes - other','diabetic ketoacidosis',
    'coagulation disorder','neutropenia','thrombophilia'
  ),
  
  'Allergic Reaction' = c(
    'allergic reaction', 'medication reaction', 'urticaria', 'allergies',
    'immunization reactions',
    'hives', 'angioedema', 'anaphylaxis', 'allergic rhinitis',
    'skin irritation', 'eczema', 'psoriasis', 'itching', 'urticaria',
    'poison ivy', 'dermatitis', 'alopecia', 'eye allergy', 'lip swelling'
  ),
  
  'Back or Flank Pain' = c(
    'back pain', 'flank pain', 'phs amb chi lower back pain',
    'low back pain', 'sciatica', 'tailbone pain',
    'back injury', 'phs amb chi upper back pain', 'phs amb chi mid back pain'
  ),
  
  'Breast Complaints' = c(
    'breast pain', 'breast mass', 
    'breast discharge', 'breast problem', 'breast mass', 'mastitis'
  ),
  
  'Cardiac Arrhythmias' = c(
    'tachycardia', 'atrial fibrillation', 'palpitations',
    'irregular heart beat', 'cardiac arrest',
    'bradycardia', 'rapid heart rate', 'atrial flutter',
    'ventricular tachycardia',
    'ventricular tachycardia', 'premature ventricular contractions',
    'supraventricular tachycardia', 'pacemaker problem', 'pacemaker check',
    'paralysis', 'cardiomyopathy', 'pericardial effusion'
  ),
  
  'Chest Pain' = c(
    'chest pain', 'chest discomfort',
    'chest wall pain',
    'thoracic pain', 'angina', 'pleurisy', 'chest wall pain', 
    'aortic aneurysm','aortic dissection','cardiomyopathy',
    'heart problem','pericardial effusion'
  ),
  
  'Dizziness/Lightheadedness/Syncope' = c(
    'dizziness', 'lightheadedness', 'pre syncope', 'syncope',
    'loss of consciousness',
    'vertigo', 'photophobia', 'lightheadedness'
  ),
  
  'Ear Complaints' = c(
    'ear pain', 'otalgia', 'ear laceration', 'ear drainage',
    'hearing problem', 'otitis media',
    'ear injury', 'cerumen impaction', 'ear problem',
    'ear fullness', 'hearing loss',
    'earache', 'otitis externa', 'earache', 'tinnitus', 'ear fullness',
    'phs amb aural fullness', 'hearing loss', 'hearing problem'
  ),
  
  'Epistaxis' = c(
    'nose bleed',
    'epistaxis', 'phs amb nosebleed', 'nose injury', 'nasal drainage',
    'nasal swelling', 'nasal polyps', 'nose injury'
  ),
  
  'Exposures, Bites, and Envenomations' = c(
    'animal bite', 'insect bite', 'tick bite', 'tick removal',
    'body fluid exposure',
    'chemical exposure', 'heat exposure', 'smoke inhalation',
    'electric shock', 'human bite', 'bite',
    'animal bite', 'cold exposure', 'toxic inhalation', 'chemical exposure',
    'smoke inhalation', 'battery', 'poisoning', 'venom exposure',
    'post-exposure prophylaxis'
    
  ),
  
  'Extremity Complaints' = c(
    'foot pain', 'hand pain', 'shoulder pain', 'leg pain', 'knee pain',
    'neck pain', 'arm pain', 'hip pain', 'ankle pain', 'wrist pain',
    'finger injury', 'toe injury', 'shoulder injury', 'hip injury',
    'leg injury', 'arm injury', 'foot injury', 'wrist injury',
    'ankle injury', 'foot problem', 'arm swelling', 'swelling',
    'joint swelling', 'joint pain', 'extremity weakness',
    'extremity laceration', 'hand injury', 'foot infection',
    'finger laceration', 'puncture wound', 'rib injury',
    'elbow pain', 'toe pain', 'finger pain', 'elbow injury',
    'clavicle injury', 'leg cramps', 'foot blister', 'gout',
    'trigger finger', 'foot numbness', 'ingrown toenail',
    'knee injury', 'foot swelling', 'foot burn', 'hand burn',
    'muscle pain', 'joint stiffness', 'tailbone pain',
    'toe problem', 'arthritis',
    'cold feet',
    'flat foot',
    'groin injury',
    'groin swelling',
    "phs amb vas lower extremity ulcer",
    'leg swelling',
    'neck swelling',
    'phs amb pls hand/finger fracture',
    'toenail problem',
    'varicose veins',
    'heel pain', 'hand problem', 'arm problem', 'leg problem',
    'foot ulcer', 'foot blister', 'foot numbness', 'foot/ankle fracture',
    'elbow injury', 'knee injury', 'dislocation', 'cold extremity',
    'skin injury', 'burning sensation', 'achilles pain', 'toe pain',
    'toe problem', 'wrist pain', 'ankle injury', 'clavicle injury',
    'phs amb chi lower extremity weakness', 'phs amb chi upper extremity weakness',
    'phs amb pcpt humerus fracture', 'phs amb pcpt multiple rib fractures',
    'phs amb pcpt femur fracture', 'fracture', 'bursitis', 'muscle tension',
    'muscle pain', 'limb pain'
  ),
  
  'Eye Complaints' = c(
    'eye pain', 'eye problem', 'eye trauma', 'conjunctivitis',
    'blurred vision', 'diplopia', 'loss of vision', 'phs change in vision',
    'eye drainage', 'eye twitching', 'eye irritation',
    'burning eyes', 'blepharitis', 'foreign body in eye',
    'photophobia', 'stye', 'uveitis', 'eyelid pain', 'eye allergy',
    'spots and/or floaters', 'vision disturbance', 'foreign body in eye',
    'eye irritation', 'blepharitis', 'burning eyes', 'eyelid problem',
    'orbital mass',
    'ptosis',
    'strabismus'
  ),
  
  'Falls, Motor Vehicle Crashes, Assaults, and Trauma' = c(
    'fall', 'assault victim', 'head injury', 'head laceration',
    'laceration', 'sexual assault', 'motor vehicle crash',
    'motorcycle crash', 'burn', 'chest injury',
    'facial burn', 'facial injury', 'stab wound', 'lip laceration',
    'domestic violence', 'concussion', 'fracture', 'mouth injury',
    'dental injury', 'abdominal injury',
    'gun shot wound', 'stab wound', 'motorcycle vs pedestrian',
    'traumatic brain injury', 'subdural hematoma', 'epidural hematoma',
    'skull fractures', 'injury', 'back injury', 'neck injury',
    'phs amb pls facial/mandibular fractures', 'phs amb pcpt multiple rib fractures',
    'phs amb pcpt femur fracture', 'fracture', 'abrasion', 'dislocation',
    'concussion', 'facial laceration',
    'trauma'
  ),
  
  'Fatigue and Weakness' = c(
    'fatigue', 'weakness', 'weakness - generalized',
    'generalized body aches',
    'lethargy', 'anemia',
    'fatigue', 'lethargy', 'weakness', 'phs amb pulr weakness/fatigue',
    'impaired balance', 'decreased functional mobility', 'restless leg syndrome'
  ),
  
  'Fevers, Sweats or Chills' = c(
    'fever', 'chills', 'excessive sweating', 'bacteremia',
    'blood infection',
    'sickle cell anemia',
    'fever/infection', 'hot flashes', 'diaphoresis', 'chills', 'sweating'
  ),
  
  'Foreign Body' = c(
    'foreign body', 'swallowed foreign body', 'foreign body in vagina',
    'foreign body in ear', 'foreign body in eye', 'foreign body in nose',
    'foreign body in rectum', 'foreign body in skin',
    'foreign body in ear', 'foreign body in eye',
    'foreign body in skin', 'choking',
    'ingestion'
  ),
  
  'Gastrointestinal Issues' = c(
    'vomiting', 'nausea', 'nausea/vomiting', 'diarrhea', 'constipation',
    'dehydration', 'hematemesis', 'gi bleeding', 'gi problem',
    'rectal bleeding', 'dry mouth',
    'eating issues',
    'emesis',
    'gerd', 'dysphagia',
    'hiccups',
    'jaundice',
    'pinworms',
    'poor appetite',
    'spitting up (reflux)',
    'vomiting blood', 'black or bloody stool', 'vomitting with diarrhea',
    'gas', 'gastritis', 'umbilical hernia', 'hernia', 'irritable bowel syndrome',
    'fecal incontinence', 'gastroesophageal reflux', 'reflux',
    'pancreatitis', 'liver mass', 'liver lesion', 'cirrhosis',
    'ascites', 'diverticulitis', 'ulcerative colitis', 'crohn\'s disease',
    'hepatitis', 'abdominal injury', 'gastritis', 'colitis',
    'melena', 'brbpr', 'heartburn', 'gastroesophageal reflux',
    'crohn\'s disease', 'ulcerative colitis', 'hematochezia',
    'constipation', 'rectal problems', 'diverticulitis',
    'pancreatitis', 'fecal impaction', 'hemorrhoids'
  ),
  
  'Genital Complaints' = c(
    'testicle pain', 'vaginal bleeding', 'vaginal itching',
    'vaginal pain', 'concerns of sti', 'foreign body in vagina',
    'female gu problem', 'male gu problem', 'penile discharge', 'penis injury',
    'penis pain', 'erectile dysfunction', 'genital warts', 'bartholin\'s cyst',
    'vaginal discharge', 'sti screening', 'std', 'exposure to std',
    'genital herpes', 'phs amb hiv exposure', 'gonorrhea', 'vaginitis',
    'vaginal bleeding - pregnant', 'menorrhagia', 'amenorrhea',
    'decreased fetal movement', 'ectopic pregnancy', 'rupture of membranes',
    'mastitis', 'ovarian cyst', 'ovarian torsion', 'pelvic mass',
    'postpartum care', 'genital problem', 'prostate check',
    'testicle injury',
    'testicular mass',
    'unplanned sexual encounter', 'std exposure', 'penis pain',
    'vaginal discharge', 'vaginal prolapse', 'vaginitis',
    'ovarian cyst', 'ovarian torsion', 'pelvic mass'
  ),
  
  'Medical Device or Treatment Issue' = c(
    'aicd problem', 'vascular access problem', 'medication refill',
    'suture / staple removal', 'suture/staple questions', 'wound check',
    'post-op problem', 'phs fistula', 'assess success with recommended aid',
    'foot wound check',
    'heart transplant',
    'phs amb new diabetic patient',
    'phs amb nwh art us',
    'pacemaker problem', 'pacemaker check', 'left ventricle assist device',
    'feeding tube', 'ostomy care', 'stoma complication', 'cast removal',
    'cast repair', 'cast check', 'cast problem', 'phs amb medication backorder issue',
    'medication management', 'medication change request', 'medication follow up',
    'medication problem', 'medication question', 'iv medication', 'wound care',
    'wound management', 'wound dehiscence', 'drainage from incision',
    'dressing change', 'procedure', 'chemo related symptoms', 'dialysis access',
    'cast removal', 'cast problem', 'shunt',
    'tracheostomy tube change', 'dressing change',
    'stoma complication', 'wound dehiscence'
  ),
  
  'Medication Request' = c(
    'medication refill',
    'medication change request', 'medication follow up', 'medication problem',
    'medication question', 'new medication request', 'medication visit',
    'phs amb medication backorder issue',
    'new medication request'
  ),
  
  'Neurological Issue' = c(
    'headache', 'seizures', 'febrile seizure', 'altered mental status',
    'slurred speech', 'numbness', 'stroke', 'aphasia',
    'cerebrovascular accident', 'confusion', 'gait problem','neurologic problem',
    'speech problem',
    'transient ischemic attack', 'paralysis', 'spasms', 'tremors', 'myasthenia gravis',
    'multiple sclerosis', 'neuralgia', 'dementia', 'stroke', 'vertigo',
    'brain tumor', 'headache', 'migraine', 'seizure disorder', 'traumatic brain injury',
    'tics', 'memory loss', 'word finding problems', 'difficulty swallowing',
    'dysarthria', 'difficulty walking', 'gait abnormality', 'tingling',
    'numbness', 'peripheral neuropathy', 'chronic nerve pain', 'restless leg syndrome',
    'phs amb chi lower extremity numbness', 'phs amb chi upper extremity weakness',
    'phs amb chi lower extremity weakness', 'hydrocephalus', 'meningitis',
    'neuralgia', 'torticollis', 'cerebrospinal fluid leak',
    'facial droop', 'trigeminal neuralgia', 'phs amb nsr brain mass',
    'tremors', 'shaking', 'memory loss', 'seizure disorder',
    'cerebrospinal fluid leak', 'gait abnormality', 'brain tumor',
    'phs amb nsr stroke', 'myasthenia gravis', 'difficulty swallowing',
    'tingling', 'head and neck pain', 'facial paralysis',
    'difficulty walking'
  ),
  
  'Other' = c(
    'other', 'null', 'phs amb new patient', 'phs amb return pt',
    'school/sports/camp physical', 'blood pressure check', 'infection',
    'weight gain', 'failure to thrive', 'phs amb vas dvt',
    'phs amb nsr radiculopathy', "symptom flare",
    'lump', 'mass', 'skin (lump/localized swelling)', 'lymphadenopathy',
    'lymph nodes, swollen', 'phs amb nut underweight', 'immunizations',
    'flu vaccine', 'vaccine', 'vaccine follow-up', 'housing', 'forms and paperwork',
    'letter for school/work', 'inpatient admission', 'baseline evaluation',
    'new evaluation', 'labs', 'labs only', 'procedure', 'symptom management',
    'palliative care follow-up', 'phs amb substance use - alcohol related',
    'phs amb substance use - benzodiaepines', 'phs amb substance use - opioid related',
    'phs amb pcpt unexplained bruising', 'ip case management stem cell transplant continuum',
    'feeding difficulty', 'feeding tube', 'newborn', 'infusion', 'flu symptoms',
    'upper respiratory infection', 'uri', 'viral infection', 'cold extremity',
    'social service visit', 'symptom management', 'phs amb globus sensation',
    'weight loss', 'lung nodule', 'anasarca', 'lymphadenopathy',
    'lymph nodes, swollen', 'homeless', 'evaluate for admission',
    'phs amb neck mass', 'thyroid problem', 'covid-19 post discharge follow-up',
    'fussy', 'annual exam', 'weekly visit', 'follow-up',
    'social complaints', '2nd opinion', 'pain', 'labs only',
    'edema', 'polydipsia', 'mass', 'mental health problem',
    'behavior problem','circulatory problem',
    'consultation with social work',
    'covid-19 return to work inquiry',
    'crying (3 months or older)', 'lupus',
    'lymphoma', 'bleeding',
    'osteomyelitis',
    'thyroid nodule'
  ),
  
  'Other Pain' = c(
    'facial pain', 'facial swelling', 'jaw pain', 'mouth pain',
    'back pain', 'neck pain','bleeding gums',
    'dental pain',
    'dental problem',
    'fibromyalgia',
    'infection of the mouth',
    'sickle cell pain crisis',
    'chronic pain', 'pain', 'incisional pain', 'cancer pain', 'hand pain',
    'foot pain', 'neck pain', 'arm pain', 'leg pain', 'facial jaw pain',
    'temporomandibular joint pain', 'head and neck pain', 'oral pain',
    'eye pain', 'ear pain', 'tooth pain', 'broken tooth',
    'chronic pain', 'mouth lesions', 'oral swelling',
    'head and neck pain', 'tailbone pain'
  ),
  
  'Post-Op Issue' = c(
    'post-op problem', 'wound infection',
    'post-op', 'post-op problem', 'wound dehiscence', 'drainage from incision',
    'wound care', 'wound management', 'chemo related symptoms', 'ostomy care',
    'stoma complication', 'postpartum care', 'incisional pain',
    'wound dehiscence'
  ),
  
  'Psychiatric Complaints' = c(
    'suicidal', 'psychiatric evaluation', 'anxiety', 'panic attack',
    'hallucinations', 'depression', 'agitation', 'insomnia',
    'delirium tremens (dts)',  'manic behavior',
    'grief/bereavement', 'paranoia', 'delusional', 'psychotic symptoms',
    'psychological issue', 'depression', 'anxiety', 'stress',
    'adjustment disorder with mixed anxiety and depressed mood',
    'post-traumatic stress disorder', 'suicide attempt', 'phs amb substance use - alcohol related',
    'phs amb substance use - benzodiaepines', 'phs amb substance use - opioid related',
    'racing thoughts', 'sleeping problem', 'insomnia', 'eating disorder',
    'psychosocial problems', 'dementia',
    'paranoid', 'aggressive behavior', 'suicide attempt',
    'homicidal', 'eating disorder', 'adjustment disorder with mixed anxiety and depressed mood',
    'psychosocial problems'
  ),
  
  'Shortness of Breath' = c(
    'shortness of breath', 'respiratory distress', 'asthma', 'wheezing',
    'copd', 'shortness of breath', 'phs amb difficulty breathing',
    'tachypnea', 'hyperventilating', 'cyanosis', 'asthma', 'wheezing',
    'stridor', 'pleural effusion', 'pulmonary embolism', 'pneumonia',
    'lung nodule', 'respiratory distress', 'near drowning', 'bronchitis',
    'hypoxia', 'congestive heart failure', 'pneumonia',
    'airway obstruction', 'near drowning', 'aspiration',
    'sleep apnea',
    'tracheal stenosis'
  ),
  
  'Skin Complaints' = c(
    'rash', 'abscess', 'cellulitis', 'pruritus', 'skin check',
    'wound infection', 'nail problem', 'cyst',  'bleeding/bruising',
    'pilonidal symptoms',
    'sore',
    'thrush',
    'vasculitis',
    'skin irritation', 'skin discoloration', 'eczema', 'psoriasis',
    'itching', 'rash', 'urticaria', 'hives', 'diaper rash', 'skin ulcer',
    'skin injury', 'burning sensation', 'burn', 'blister', 'frostbite',
    'sunburn', 'skin (lump/localized swelling)', 'changing mole', 'dermatitis',
    'skin problem', 'alopecia', 'cellulitis', 'abscess', 'herpes zoster',
    'poison ivy', 'varicella', 'stye', 'genital warts', 'skin infection',
    'recurrent skin infections', 'melanoma',
    'rash or redness', 'blister', 'sunburn', 'skin problem',
    'skin ulcer', 'herpes zoster', 'mrsa', 'head lice'
  ),
  
  'Substance Abuse Issues' = c(
    'alcohol problem', 'alcohol intoxication', 'addiction problem',
    'drug overdose',
    'phs amb substance use - alcohol related', 'phs amb substance use - benzodiaepines',
    'phs amb substance use - opioid related', 'drug screen', 'alcohol problem',
    'addiction problem', 'drug overdose', 'withdrawal', 'substance abuse',
    'withdrawal', 'drug / alcohol assessment'
  ),
  
  'Upper Respiratory Symptoms' = c(
    'sore throat', 'cough', 'nasal congestion',
    'upper respiratory infection', 'uri', 'influenza', 'flu symptoms',
    'coughing up blood', 'throat problem', 'sore throat', 'strep throat',
    'laryngitis', 'hoarseness', 'nasal congestion', 'sinus congestion',
    'sinus headache', 'sinusitis', 'allergic rhinitis', 'phs amb noisy breathing',
    'sinus congestion', 'sinusitis', 'nasal polyps', 'covid-19 inquiry',
    'croup',
    'hemoptysis',
    'sinus cancer',
    'sneezing',
    'snoring'
  ),
  
  'Pregnancy Related' = c(
    'non-stress test', 'vaginal bleeding', 'vaginal pain',
    'concerns of sti', 'weight gain', 'contraception',
    'vaginal bleeding - pregnant', 'ectopic pregnancy', 'rupture of membranes',
    'decreased fetal movement', 'emesis during pregnancy', 'vomiting during pregnancy',
    'morning sickness', 'nausea/vomiting in pregnancy', 'laboring',
    'pregnancy problem', 'postpartum care', 'threatened miscarriage',
    'miscarriage', 'pregnancy-related issue',
    'contractions', 'possible pregnancy', 'threatened miscarriage',
    'miscarriage', 'vaginal bleeding - pregnant', 'emesis during pregnancy',
    'laboring', 'pregnancy problem'
  ),
  'Renal' = c(
    'urinary tract infection', 'hematuria', 'urinary retention',
    'pyelonephritis', 'nephrolithiasis',
    'recurrent uti', 'cystitis', 'urinary urgency', 'urine leakage',
    'nephritis', 'nephrotic syndrome', 'kidney transplant pre-evaluation',
    'chronic kidney disease', 'aki (acute kidney injury)', 'acute renal failure',
    'diabetic nephropathy', 'aki (acute kidney injury)', 'acute renal failure'
  ),
  
  'Urinary Complaints' = c(
    'urinary retention', 'dysuria', 'hematuria', 'urinary incontinence',
    'blood in urine', 'urinary urgency', 'urine leakage', 'bladder pain',
    'bladder problem', 'difficulty urinating', 'urinary frequency',
    'polyuria',
    'urinary symptom', 'lower urinary tract symptoms', 'fecal incontinence',
    'proteinuria', 'phs amb vas dialysis access',
    'blood in urine', 'urinary frequency', 'urination pain',
    'urinary problem', 'difficulty urinating',
    'lower urinary tract symptoms', 'urinary symptom'
  )
)


for (i in seq_along(complaints)) {
  name <- names(complaints[i])
  complaint_terms <- complaints[[i]]
  data_enc$CHIEF_COMPLAINT <- ifelse(
    data_enc$CHIEF_COMPLAINT %in% complaint_terms, name, data_enc$CHIEF_COMPLAINT
  )
}

data_enc$complaint_esi <- paste(data_enc$ESI, data_enc$CHIEF_COMPLAINT)
data_enc$complaint_esi <- factor(data_enc$complaint_esi)

# --- 72-hour return ---
# Flag on THIS encounter: does the patient return within 72 hours AFTER this visit?
# (parallel to Mayo's RTN_72_HR)
data_enc <- data_enc %>%
  arrange(deid_pat_id, arrival_dts) %>%
  group_by(deid_pat_id) %>%
  mutate(
    time_to_next_arrival = as.numeric(difftime(lead(arrival_dts), ed_departure_dts, units = "hours")),
    RTN_72_HR = ifelse(!is.na(time_to_next_arrival) & time_to_next_arrival <= 72 & 
                         time_to_next_arrival >= 0, 1, 0),
    next_disposition = lead(ed_disposition),
    RTN_72_HR_ADMIT = ifelse(RTN_72_HR == 1 & 
                               grepl("Admit", next_disposition, ignore.case = TRUE), 1, 0)
  ) %>%
  ungroup()


#=========================================================================
# 3. PROCESS PROVIDERS — identify attending per encounter
#=========================================================================

# The "judge" for the IV: first attending assigned to each encounter
# Parallel to Mayo's ED_PROVIDER

attending <- data_prov %>%
  filter(provider_type == "Attending") %>%
  arrange(deid_enc_id, assign_event_dts) %>%
  group_by(deid_enc_id) %>%
  slice(1) %>%   # first attending assigned
  ungroup() %>%
  dplyr::select(deid_enc_id, ED_PROVIDER = deid_prov_id)

data_enc <- data_enc %>%
  left_join(attending, by = "deid_enc_id")


#=========================================================================
# 4. PROCESS IMAGING — modalities, batching
#=========================================================================

# --- 4a. Determine CT contrast vs non-contrast ---
# Use medications sheet: IV contrast agent ordered within 60 min of CT order

data_iv_filtered <- data_iv %>%
  filter(medication_route_dsc == "Intravenous", med_orderstatus == "Completed")

contrast_agents <- c("CARDIOVASCULAR DIAGNOSTICS-RADIOPAQUE")

data_iv_contrast <- data_iv_filtered %>%
  filter(pharmaceutical_class_dsc %in% contrast_agents) %>%
  dplyr::select(deid_enc_id, med_order_dts) %>%
  distinct()

# For each CT order, check if contrast was administered within ±60 minutes
data_ct <- data_img %>% filter(ct == 1)

data_ct <- data_ct %>%
  left_join(data_iv_contrast, by = "deid_enc_id", relationship = "many-to-many") %>%
  mutate(
    time_diff_contrast = as.numeric(difftime(img_order_dts, med_order_dts, units = "mins")),
    has_contrast = !is.na(time_diff_contrast) & 
      time_diff_contrast >= -60 & time_diff_contrast <= 60
  ) %>%
  group_by(deid_enc_id, img_order_dts) %>%
  summarise(
    ct_contrast = as.integer(any(has_contrast)),
    ct_no_contrast = as.integer(!any(has_contrast)),
    img_available_dts = first(img_available_dts),
    .groups = "drop"
  )

# --- 4b. Build per-order modality table ---
# Each row = one imaging order with its modality type

# Non-CT orders from original imaging data
img_orders <- data_img %>%
  mutate(
    # Formal US only (exclude bedside)
    us_formal = ifelse(us == 1 & us_bedside == 0, 1, 0)
  ) %>%
  dplyr::select(deid_enc_id, img_order_dts, img_available_dts, 
                xr, us_formal, mri) %>%
  pivot_longer(cols = c(xr, us_formal, mri),
               names_to = "modality", values_to = "flag") %>%
  filter(flag == 1) %>%
  dplyr::select(deid_enc_id, img_order_dts, img_available_dts, modality)

# CT orders with contrast determination
ct_orders_contrast <- data_ct %>%
  filter(ct_contrast == 1) %>%
  mutate(modality = "ct_contrast") %>%
  dplyr::select(deid_enc_id, img_order_dts, img_available_dts, modality)

ct_orders_no_contrast <- data_ct %>%
  filter(ct_no_contrast == 1) %>%
  mutate(modality = "ct_no_contrast") %>%
  dplyr::select(deid_enc_id, img_order_dts, img_available_dts, modality)

# Combine all imaging orders
all_img_orders <- bind_rows(img_orders, ct_orders_contrast, ct_orders_no_contrast)

# --- 4c. Aggregate to encounter level ---

# Count of imaging tests per encounter (by distinct modality types)
# Parallel to Mayo's imgTests
enc_img_counts <- all_img_orders %>%
  group_by(deid_enc_id) %>%
  summarise(
    imgTests = n_distinct(modality),
    n_img_orders = n(),
    PLAIN_XRAY = as.integer(any(modality == "xr")),
    US_PERF = as.integer(any(modality == "us_formal")),
    NON_CON_CT_PERF = as.integer(any(modality == "ct_no_contrast")),
    CON_CT_PERF = as.integer(any(modality == "ct_contrast")),
    MRI_PERF = as.integer(any(modality == "mri")),
    .groups = "drop"
  )

# --- 4d. Determine batching ---
# Batched = 2+ different modalities ordered within 5 minutes of each other
# as the FIRST orders (parallel to Mayo)

# Get earliest order time per modality per encounter
first_orders <- all_img_orders %>%
  group_by(deid_enc_id, modality) %>%
  summarise(first_order_time = min(img_order_dts), .groups = "drop")

# For each encounter, find min order time across all modalities
min_order_times <- first_orders %>%
  group_by(deid_enc_id) %>%
  summarise(min_time = min(first_order_time), .groups = "drop")

first_orders <- first_orders %>%
  left_join(min_order_times, by = "deid_enc_id") %>%
  mutate(time_from_first = as.numeric(difftime(first_order_time, min_time, units = "mins")))

# Count how many modalities were ordered within 5 minutes of the first order
batch_info <- first_orders %>%
  group_by(deid_enc_id) %>%
  summarise(
    batch_count = sum(time_from_first >= 0 & time_from_first <= 5),
    min_time = first(min_time),
    .groups = "drop"
  ) %>%
  mutate(batched = ifelse(batch_count > 1, 1, 0))

# Get max result time for total testing time
max_result_times <- all_img_orders %>%
  group_by(deid_enc_id) %>%
  summarise(max_result_time = max(img_available_dts, na.rm = TRUE), .groups = "drop")

# Combine imaging summary
enc_img <- enc_img_counts %>%
  left_join(batch_info %>% dplyr::select(deid_enc_id, batch_count, batched, min_time), 
            by = "deid_enc_id") %>%
  left_join(max_result_times, by = "deid_enc_id") %>%
  mutate(
    total_testing_time = as.numeric(difftime(max_result_time, min_time, units = "mins"))
  )


#=========================================================================
# 5. PROCESS LABS
#=========================================================================

# LAB_PERF: binary indicator for any lab ordered (parallel to Mayo)
lab_perf <- data_lab %>%
  distinct(deid_enc_id) %>%
  mutate(LAB_PERF = 1)


#=========================================================================
# 6. MERGE EVERYTHING INTO ENCOUNTER-LEVEL DATASET
#=========================================================================

data <- data_enc %>%
  left_join(enc_img, by = "deid_enc_id") %>%
  left_join(lab_perf, by = "deid_enc_id") %>%
  mutate(
    # Fill NAs for encounters with no imaging/labs
    imgTests = replace_na(imgTests, 0),
    n_img_orders = replace_na(n_img_orders, 0),
    PLAIN_XRAY = replace_na(PLAIN_XRAY, 0),
    US_PERF = replace_na(US_PERF, 0),
    NON_CON_CT_PERF = replace_na(NON_CON_CT_PERF, 0),
    CON_CT_PERF = replace_na(CON_CT_PERF, 0),
    MRI_PERF = replace_na(MRI_PERF, 0),
    batched = replace_na(batched, 0),
    batch_count = replace_na(batch_count, 0),
    LAB_PERF = replace_na(LAB_PERF, 0),
    imaging = ifelse(imgTests > 0, 1, 0)
  )


#=========================================================================
# 7. TEMPORAL VARIABLES
#=========================================================================

# --- Time features (parallel to Mayo) ---
data <- data %>%
  mutate(
    hour_of_day = hour(arrival_dts),
    time = hour_of_day,
    hour_of_day = cut(
      hour_of_day,
      breaks = c(-Inf, 6, 12, 18, Inf),
      labels = c("0:00-6:00", "6:00-12:00", "12:00-18:00", "18:00-24:00"),
      right = FALSE
    ),
    day_of_week = wday(arrival_dts, label = TRUE, abbr = FALSE),
    month_of_year = month(arrival_dts, label = TRUE),
    is_weekend = ifelse(wday(arrival_dts) %in% c(1, 7), "Weekend", "Weekday"),
    dayofweekt = paste(day_of_week, hour_of_day),
    date = as.Date(arrival_dts)
  )

# --- Treatment time (parallel to Mayo) ---
#data$treatment_time <- data$time_to_dispo - data$wait_time
#data$treatment_time <- ifelse(is.na(data$treatment_time) | data$treatment_time <= 0, 
#                              1, data$treatment_time)

# --- Capacity (parallel to Mayo) ---
# patients_waiting: count of patients who have arrived but not yet been triaged
# This is computationally expensive on 111K rows — use vectorized approach
cat("Computing patients waiting (this may take a few minutes)...\n")
#data <- data %>% arrange(arrival_dts)

# Vectorized: for each patient, count others where arrival <= this arrival & triage > this arrival
#data$patients_waiting <- sapply(
#  seq_len(nrow(data)),
#  function(i) {
#    sum(data$arrival_dts <= data$arrival_dts[i] & 
#          data$triage_started_dts > data$arrival_dts[i], na.rm = TRUE)
#  }
#)

#data <- data %>%
#  mutate(
#    capacity_level = case_when(
#      wait_time > 90 | patients_waiting > 20 ~ "Major Overcapacity",
#      (wait_time >= 21 & wait_time <= 90) | patients_waiting >= 10 ~ "Minor Overcapacity",
#      wait_time < 20 & patients_waiting < 10 ~ "Normal Operations",
#      TRUE ~ "Normal Operations"
#    )
#  )

# --- Concurrent patients (ED census at arrival) ---
data <- data %>%
  mutate(
    rel_minutes_arrival = as.numeric(arrival_dts),
    rel_minutes_depart = as.numeric(ed_departure_dts)
  )

data$concurrent_patients <- sapply(
  seq_len(nrow(data)),
  function(i) {
    sum(data$rel_minutes_arrival <= data$rel_minutes_arrival[i] & 
          data$rel_minutes_depart > data$rel_minutes_arrival[i]) - 1
  }
)


#=========================================================================
# 8. PROVIDER SHIFT VARIABLES
#=========================================================================

# --- Shifts based on time gaps > 8 hours (parallel to Mayo) ---
data <- data %>%
  arrange(ED_PROVIDER, arrival_dts) %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    time_diff_shift = as.numeric(difftime(arrival_dts, lag(arrival_dts), units = "hours")),
    new_shift = ifelse(is.na(time_diff_shift) | time_diff_shift > 8, 1, 0),
    shift_id = cumsum(new_shift)
  ) %>%
  group_by(ED_PROVIDER, shift_id) %>%
  mutate(
    shift_start = min(arrival_dts),
    hrs_in_shift = as.numeric(difftime(arrival_dts, shift_start, units = "hours"))
  ) %>%
  ungroup()

# --- Per-day metrics ---
data <- data %>%
  group_by(ED_PROVIDER, date) %>%
  mutate(
    patients_seen = n(),
    patient_order_of_day = row_number(),
    patients_tbs = patients_seen - patient_order_of_day
  ) %>%
  ungroup()


#=========================================================================
# 9. SAMPLE FILTERS (parallel to Mayo)
#=========================================================================

# Filter valid LOS and disposition times
data <- data %>%
  filter(!is.na(time_to_dispo),
         time_to_dispo > 0,
         ED_LOS <= 1440,    # ≤ 24 hours
         ED_LOS > 0,
         !is.na(ED_LOS))

# Drop encounters with no attending provider
data <- data %>% filter(!is.na(ED_PROVIDER))

# Drop providers with < 500 encounters (parallel to Mayo)
provider_counts <- table(data$ED_PROVIDER)
providers_less_than_500 <- names(provider_counts[provider_counts < 500])
data <- data[!(data$ED_PROVIDER %in% providers_less_than_500), ]

# Drop complaints that appear < 1000 times
complaint_counts <- table(data$CHIEF_COMPLAINT)
complaints_less_than_1000 <- names(complaint_counts[complaint_counts < 1000])
data <- data[!(data$CHIEF_COMPLAINT %in% complaints_less_than_1000), ]

# Refresh complaint_esi and filter singletons
data$complaint_esi <- paste(data$CHIEF_COMPLAINT, data$ESI)
data <- filter(data, !is.na(data$ESI))

data <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup()


#=========================================================================
# 10. BUILD INSTRUMENT (parallel to Mayo)
#=========================================================================

data$ln_ED_LOS <- log(data$ED_LOS)

# Residualize batch on controls (for leave-one-out instrument)
# Note: no EXPERIENCE or PROVIDER_SEX available for MGH
data$residual_batch <- resid(
  felm(batched ~ tachycardic + tachypneic + febrile + hypotensive 
       | dayofweekt + month_of_year + complaint_esi + race + sex + care_area| 0 | ED_PROVIDER, 
       data = data)
)


# Leave-one-out batch tendency
data <- data %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    Sum_Resid_batch = sum(residual_batch, na.rm = TRUE),
    batch.tendency = (Sum_Resid_batch - residual_batch) / (n() - 1),
  ) %>%
  ungroup() %>%
  dplyr::select(-Sum_Resid_batch)


#=========================================================================
# 11. FINAL SAMPLE (parallel to Mayo)
#=========================================================================

# Keep only imaging encounters in complaint-ESI cells with >5% batch rate
final <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>%
  filter(batchmean > 0.05, imaging == 1)

# Log outcomes
final$ln_ED_LOS <- log(final$ED_LOS)
final$ln_disp_time <- log(final$time_to_dispo)
#final$ln_treat_time <- log(final$treatment_time)

#final$total_testing_time <- ifelse(final$total_testing_time > 1440, 
#                                   1440, final$total_testing_time)
#final$ln_total_testing_time <- log(final$total_testing_time)

#final$capacity_level <- factor(final$capacity_level,
#                               levels = c("Normal Operations", 
#                                          "Minor Overcapacity", 
#                                          "Major Overcapacity"))

# Rename sex to match Mayo's GENDER in formulas
final$GENDER <- final$sex

cat("\n=========================================\n")
cat("MGH SAMPLE SUMMARY\n")
cat("=========================================\n")
cat("Full encounters (adults):", nrow(data_enc), "\n")
cat("After filters:", nrow(data), "\n")
cat("Final analytic sample:", nrow(final), "\n")
cat("N providers:", n_distinct(final$ED_PROVIDER), "\n")
cat("Batch rate:", round(mean(final$batched), 3), "\n")
cat("Mean imgTests:", round(mean(final$imgTests), 2), "\n")
cat("Mean LOS (min):", round(mean(final$ED_LOS), 1), "\n")
cat("Admit rate:", round(mean(final$admit), 3), "\n")
cat("=========================================\n")


#=========================================================================
# 12. CLEAN UP AND SAVE
#=========================================================================

rm(list = setdiff(ls(), c("data", "final")))

# Save for downstream analysis
# save(data, final, file = paste0(path, "/mgh_clean.RData"))


#=========================================================================
# 13. RUN ANALYSIS SCRIPTS (parallel to Mayo)
#=========================================================================

# NOTE: These scripts need minor modifications for MGH:
#   - Replace GENDER references (already renamed above)
#   - Remove EXPERIENCE and PROVIDER_SEX from control sets
#   - The formulas below show what the MGH control sets should look like

# NECESSARY CONTROLS (assignment controls only):
#   dayofweekt + month_of_year

# PRECISION CONTROLS (everything):
#   complaint_esi + race + GENDER + capacity_level +
#   tachycardic + tachypneic + febrile + hypotensive +
#   LAB_PERF + age + hrs_in_shift

# Example first stage:
# feols(batched ~ batch.tendency |
#         dayofweekt + month_of_year, 
#       vcov = 'HC1', data = final)
#
# feols(batched ~ batch.tendency +
#         tachycardic + tachypneic + febrile + hypotensive +
#         LAB_PERF + hrs_in_shift + age + capacity_level |
#         dayofweekt + month_of_year + complaint_esi + race + GENDER,
#       vcov = 'HC1', data = final)

# source('src/tables/table2_first_stage_mgh.R')
# source('src/tables/table3_reduced_form_mgh.R')
# source('src/tables/table4_iv_results_mgh.R')


#=========================================================================
# MGH IV Results — parallel to Mayo table4_iv_results.R
# Run after clean_mgh.R (requires 'data' and 'final' in environment)
#=========================================================================
library(fixest)
library(ManyIV)

#=========================================================================
# CONTROL SET DEFINITIONS (MGH — no EXPERIENCE or PROVIDER_SEX)
#=========================================================================

# Necessary controls (assignment mechanism only)
NECESSARY <- "dayofweekt + month_of_year + care_area + complaint_esi"

# Precision controls (everything else)
PRECISION_VARS <- paste(
  "tachycardic", "tachypneic", "febrile", "hypotensive",
  "LAB_PERF", "hrs_in_shift",
  sep = " + "
)

PRECISION_FE <- paste(
  "race", "GENDER",
  sep = " + "
)


#=========================================================================
# A. MAIN 2SLS + OLS MODELS (Table 4 equivalent)
#=========================================================================

run_models <- function(data, y_var) {
  
  cat("\n=====================================================\n")
  cat("              RESULTS FOR", y_var, "\n")
  cat("=====================================================\n\n")
  
  # --- Summary stats for sequenced (batched == 0) ---
  mean_seq <- mean(data[[y_var]][data$batched == 0], na.rm = TRUE)
  sd_seq   <- sd(data[[y_var]][data$batched == 0], na.rm = TRUE)
  
  # --- 2SLS without precision controls ---
  model_2sls_nec <- feols(
    as.formula(paste(y_var, "~ 0 |", NECESSARY, "| batched ~ batch.tendency")),
    vcov = "HC1", data = data
  )
  
  # --- 2SLS with precision controls ---
  model_2sls_full <- feols(
    as.formula(paste(
      y_var, "~", PRECISION_VARS, "|",
      NECESSARY, "+", PRECISION_FE,
      "| batched ~ batch.tendency"
    )),
    vcov = "HC1", data = data
  )
  
  # --- OLS without precision controls ---
  model_ols_nec <- feols(
    as.formula(paste(y_var, "~ batched |", NECESSARY)),
    vcov = "HC1", data = data
  )
  
  # --- OLS with precision controls ---
  model_ols_full <- feols(
    as.formula(paste(
      y_var, "~ batched +", PRECISION_VARS, "|",
      NECESSARY, "+", PRECISION_FE
    )),
    vcov = "HC1", data = data
  )
  
  # --- Extract results ---
  extract_coef <- function(m, t) ifelse(t %in% names(coef(m)), coef(m)[t], NA)
  extract_se   <- function(m, t) ifelse(t %in% names(se(m)), se(m)[t], NA)
  
  results <- data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", 
              "2SLS No Controls", "2SLS Full Controls",
              "OLS No Controls", "OLS Full Controls"),
    Coefficient = c(
      mean_seq, sd_seq,
      extract_coef(model_2sls_nec, "fit_batched"),
      extract_coef(model_2sls_full, "fit_batched"),
      extract_coef(model_ols_nec, "batched"),
      extract_coef(model_ols_full, "batched")
    ),
    SE = c(
      NA, NA,
      extract_se(model_2sls_nec, "fit_batched"),
      extract_se(model_2sls_full, "fit_batched"),
      extract_se(model_ols_nec, "batched"),
      extract_se(model_ols_full, "batched")
    )
  )
  
  print(results)
  
  cat("\nRegression table:\n")
  print(etable(model_2sls_nec, model_2sls_full, model_ols_nec, model_ols_full,
               keep = c("batched", "fit_batched")))
  
  return(invisible(list(
    tsls_nec = model_2sls_nec, tsls_full = model_2sls_full,
    ols_nec = model_ols_nec, ols_full = model_ols_full
  )))
}


#=========================================================================
# B. UJIVE MODELS (using ManyIV package)
#=========================================================================

run_models_iv <- function(data, y_var) {
  
  cat("\n=====================================================\n")
  cat("          UJIVE + 2SLS FOR", y_var, "\n")
  cat("=====================================================\n\n")
  
  NECESSARY_factor <- "factor(dayofweekt) + factor(month_of_year)"
  
  PRECISION_all <- paste(
    "factor(complaint_esi)",
    "factor(race)",
    "factor(GENDER)",
    "tachycardic", "tachypneic", "febrile", "hypotensive",
    "LAB_PERF", "age", "hrs_in_shift",
    sep = " + "
  )
  
  # --- UJIVE (necessary controls only) ---
  ujive_form_nec <- as.formula(paste0(
    y_var, " ~ batched + ", NECESSARY_factor, " | ",
    "factor(ED_PROVIDER) + ", NECESSARY_factor
  ))
  ujive_nec <- ujive(ujive_form_nec, data = data)
  
  # --- UJIVE (full controls) ---
  ujive_form_full <- as.formula(paste0(
    y_var, " ~ batched + ", NECESSARY_factor, " + ", PRECISION_all, " | ",
    "factor(ED_PROVIDER) + ", NECESSARY_factor, " + ", PRECISION_all
  ))
  ujive_full <- ujive(ujive_form_full, data = data)
  
  # --- 2SLS batch tendency (necessary only) ---
  tsls_nec <- feols(
    as.formula(paste0(
      y_var, " ~ 0 | ", NECESSARY, " | batched ~ batch.tendency"
    )),
    data = data, vcov = "HC1"
  )
  
  # --- 2SLS batch tendency (full controls) ---
  tsls_full <- feols(
    as.formula(paste0(
      y_var, " ~ ", PRECISION_VARS, " | ",
      NECESSARY, " + ", PRECISION_FE,
      " | batched ~ batch.tendency"
    )),
    data = data, vcov = "HC1"
  )
  
  # --- Collect ---
  est <- data.frame(
    Model = c(
      "UJIVE (Necessary)", "UJIVE (Full Controls)",
      "2SLS Tendency (Necessary)", "2SLS Tendency (Full Controls)"
    ),
    Coefficient = c(
      ujive_nec$estimate["ujive", "estimate"],
      ujive_full$estimate["ujive", "estimate"],
      coef(tsls_nec)["fit_batched"],
      coef(tsls_full)["fit_batched"]
    ),
    SE = c(
      ujive_nec$estimate["ujive", "se_hte"],
      ujive_full$estimate["ujive", "se_hte"],
      se(tsls_nec)["fit_batched"],
      se(tsls_full)["fit_batched"]
    )
  )
  
  print(est)
  
  cat("\n2SLS Regression table:\n")
  print(etable(tsls_nec, tsls_full, keep = "%fit_batched",
               dict = c("fit_batched" = "Batched")))
  
  return(invisible(est))
}


#=========================================================================
# C. UJIVE INSTRUMENT (manual, for use in feols)
#=========================================================================

compute_ujive_instrument <- function(data, treat, judge, assign_controls) {
  
  x <- data[[treat]]
  judge_f <- factor(data[[judge]])
  Z <- model.matrix(~ judge_f - 1)
  
  if (!is.null(assign_controls)) {
    W <- model.matrix(assign_controls, data = data)
    XtX_W <- crossprod(W)
    beta_W_x <- solve(XtX_W, crossprod(W, x))
    beta_W_Z <- solve(XtX_W, crossprod(W, Z))
    x_tilde <- x - as.vector(W %*% beta_W_x)
    Z_tilde <- Z - W %*% beta_W_Z
  } else {
    x_tilde <- x - mean(x, na.rm = TRUE)
    Z_tilde <- scale(Z, center = TRUE, scale = FALSE)
  }
  
  XtX_Z <- crossprod(Z_tilde)
  pi_hat <- solve(XtX_Z, crossprod(Z_tilde, x_tilde))
  xhat_tilde <- as.vector(Z_tilde %*% pi_hat)
  e <- x_tilde - xhat_tilde
  
  inv_XtX_Z <- solve(XtX_Z)
  H_diag <- rowSums((Z_tilde %*% inv_XtX_Z) * Z_tilde)
  
  denom <- 1 - H_diag
  if (any(abs(denom) < 1e-10)) {
    warning("Some 1 - h_ii are ~0; setting those UJIVE instruments to NA.")
    denom[abs(denom) < 1e-10] <- NA_real_
  }
  
  return(as.numeric(x_tilde - e / denom))
}

# Build UJIVE instrument
final$z_ujive <- compute_ujive_instrument(
  data = final,
  treat = "batched",
  judge = "ED_PROVIDER",
  assign_controls = ~ dayofweekt + month_of_year
)


#=========================================================================
# D. RUN ALL OUTCOMES
#=========================================================================

cat("\n\n###############################################\n")
cat("# MAIN RESULTS (2SLS + OLS)\n")
cat("###############################################\n")

outcomes_main <- c("ln_disp_time", 
                   "ln_ED_LOS", "imgTests", "RTN_72_HR_ADMIT", "RTN_72_HR",
                   "PLAIN_XRAY", "US_PERF", "NON_CON_CT_PERF", "CON_CT_PERF",
                   "MRI_PERF", "admit")

for (y in outcomes_main) {
  run_models(final, y)
}


#=========================================================================
# F. FIRST STAGE
#=========================================================================

cat("\n\n###############################################\n")
cat("# FIRST STAGE\n")
cat("###############################################\n")

fs_1 <- feols(batched ~ batch.tendency | dayofweekt + month_of_year, 
              vcov = "HC1", data = final)

fs_2 <- feols(
  as.formula(paste0(
    "batched ~ batch.tendency + ", PRECISION_VARS, 
    " | ", NECESSARY, " + ", PRECISION_FE
  )),
  vcov = "HC1", data = final
)

etable(fs_1, fs_2, keep = "batch.tendency")
cat("F-stat (no controls):", wald(fs_1, keep = "batch.tendency")$stat, "\n")
cat("F-stat (full controls):", wald(fs_2, keep = "batch.tendency")$stat, "\n")
cat("Sample batch rate:", mean(final$batched), "\n")
cat("Full data batch rate:", mean(data$batched), "\n")


#=========================================================================
# G. REDUCED FORM
#=========================================================================

cat("\n\n###############################################\n")
cat("# REDUCED FORM\n")
cat("###############################################\n")

rf_formula <- function(y) {
  as.formula(paste0(
    y, " ~ batch.tendency + ", PRECISION_VARS,
    "  | ", NECESSARY, " + ", PRECISION_FE
  ))
}

rf_disp <- feols(rf_formula("ln_disp_time"), data = final, vcov = "HC1")
rf_los  <- feols(rf_formula("ln_ED_LOS"), data = final, vcov = "HC1")
rf_img  <- feols(rf_formula("imgTests"), data = final, vcov = "HC1")
rf_rtn  <- feols(rf_formula("RTN_72_HR_ADMIT"), data = final, vcov = "HC1")

etable(rf_disp, rf_los, rf_img, rf_rtn, keep = "batch.tendency")





#=========================================================================
# Coefficient Comparison Plot: Mayo vs MGH
# 2SLS with Full Controls — Single Panel, No Admission
#=========================================================================
library(tidyverse)
library(ggplot2)

# =========================================================================
# DATA
# =========================================================================

mayo <- tribble(
  ~outcome,           ~label,                         ~coef,    ~se,
  "ln_disp_time",     "Log Time to Disposition",       0.651,   0.101,
  "ln_ED_LOS",        "Log LOS",                       0.597,   0.088,
  "imgTests",         "Number of Imaging Tests",        1.241,   0.116,
  "RTN_72_HR_ADMIT",  "72-Hour Return with Admission", -0.0146,  0.019
)

mgh <- tribble(
  ~outcome,           ~label,                         ~coef,    ~se,
  "ln_disp_time",     "Log Time to Disposition",       0.4990,  0.2784,
  "ln_ED_LOS",        "Log LOS",                       0.5320,  0.2637,
  "imgTests",         "Number of Imaging Tests",        1.3716,  0.2694,
  "RTN_72_HR_ADMIT",  "72-Hour Return with Admission", -0.0422,  0.0430
)

# =========================================================================
# DIFFERENCE TEST
# =========================================================================

diff_test <- mayo %>%
  dplyr::select(outcome, label, coef_mayo = coef, se_mayo = se) %>%
  left_join(
    mgh %>% dplyr::select(outcome, coef_mgh = coef, se_mgh = se),
    by = "outcome"
  ) %>%
  mutate(
    diff = coef_mayo - coef_mgh,
    se_diff = sqrt(se_mayo^2 + se_mgh^2),
    z_stat = diff / se_diff,
    p_value = 2 * pnorm(-abs(z_stat))
  )

cat("\n=========================================================\n")
cat("  Test of Equality: Mayo vs MGH (independent samples)\n")
cat("=========================================================\n\n")
print(diff_test %>% 
        dplyr::select(label, coef_mayo, coef_mgh, diff, se_diff, z_stat, p_value) %>%
        mutate(across(where(is.numeric), ~round(., 4))),
      n = 10)

# =========================================================================
# BUILD PLOT DATA
# =========================================================================

mayo$site <- "Mayo Clinic"
mgh$site   <- "Massachusetts General Hospital"
df <- bind_rows(mayo, mgh)

outcome_order <- c(
  "Log Time to Disposition",
  "Log LOS",
  "Number of Imaging Tests",
  "72-Hour Return with Admission"
)

df <- df %>%
  mutate(
    ci_low  = coef - 1.96 * se,
    ci_high = coef + 1.96 * se,
    label = factor(label, levels = rev(outcome_order)),
    site = factor(site, levels = c("Mayo Clinic", "Massachusetts General Hospital")),
    significant = ifelse(abs(coef / se) > 1.96, "TRUE", "FALSE")
  )

# =========================================================================
# BRACKET DATA — positioned relative to each outcome's max CI
# =========================================================================

BRACKET_OFFSET <- 0.08  # gap between end of widest CI and bracket

bracket_data <- diff_test %>%
  mutate(
    label = factor(label, levels = rev(outcome_order)),
    y_num = as.numeric(label),
    p_label = case_when(
      p_value < 0.001 ~ "p < 0.001",
      TRUE ~ paste0("p = ", formatC(p_value, format = "f", digits = 2))
    ),
    # bracket x = just past the wider of the two CIs
    bracket_x = pmax(
      coef_mayo + 1.96 * se_mayo,
      coef_mgh + 1.96 * se_mgh
    ) + BRACKET_OFFSET
  )

# =========================================================================
# PLOT
# =========================================================================

p <- ggplot(df, aes(x = coef, y = label)) +
  
  geom_vline(xintercept = 0, color = "#1a365d", linewidth = 0.7) +
  
  # Confidence intervals
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high, color = significant, group = site),
    height = 0, 
    linewidth = 0.9,
    position = position_dodge(width = 0.6)
  ) +
  
  # Point estimates
  geom_point(
    aes(fill = significant, shape = site, group = site),
    size = 3.5,
    color = "white",
    stroke = 0.5,
    position = position_dodge(width = 0.6)
  ) +
  
  # Bracket: vertical line
  geom_segment(
    data = bracket_data,
    aes(x = bracket_x, xend = bracket_x,
        y = y_num - 0.25, yend = y_num + 0.25),
    inherit.aes = FALSE,
    linewidth = 0.4, color = "gray40"
  ) +
  # Bracket: top tick
  geom_segment(
    data = bracket_data,
    aes(x = bracket_x - 0.03, xend = bracket_x,
        y = y_num + 0.25, yend = y_num + 0.25),
    inherit.aes = FALSE,
    linewidth = 0.4, color = "gray40"
  ) +
  # Bracket: bottom tick
  geom_segment(
    data = bracket_data,
    aes(x = bracket_x - 0.03, xend = bracket_x,
        y = y_num - 0.25, yend = y_num - 0.25),
    inherit.aes = FALSE,
    linewidth = 0.4, color = "gray40"
  ) +
  # P-value label
  geom_text(
    data = bracket_data,
    aes(x = bracket_x + 0.04, y = y_num, label = p_label),
    inherit.aes = FALSE,
    hjust = 0, size = 3.3, color = "gray30", fontface = "italic"
  ) +
  
  # Significance colors
  scale_color_manual(
    values = c("TRUE" = "#dc2626", "FALSE" = "#dc2626"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#dc2626", "FALSE" = "#dc2626"),
    guide = "none"
  ) +
  
  # Site shapes
  scale_shape_manual(
    values = c("Mayo Clinic" = 21, "Massachusetts General Hospital" = 24),
    name = "Site"
  ) +
  
  guides(
    shape = guide_legend(
      override.aes = list(
        fill = "#dc2626",
        color = "white",
        size = 4
      )
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(-0.5, 2.0, 0.5),
    limits = c(-0.6, 2.5)
  ) +
  
  # Labels
  labs(
    x = "2SLS Coefficient Estimate",
    y = NULL
  ) +
  
  # Theme
  theme_bw(base_size = 13) +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#64748b"),
    plot.caption = element_text(size = 9, hjust = 0, color = "#64748b"),
    axis.title.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  
  coord_cartesian(clip = "off")

print(p)

ggsave("outputs/figures/coef_comparison_mayo_mgh.pdf", p, 
       width = 10, height = 4.5, dpi = 300)
ggsave("outputs/figures/coef_comparison_mayo_mgh.png", p, 
       width = 10, height = 4.5, dpi = 300)




#=========================================================================
# Two-Panel Coefficient Comparison: Mayo vs MGH
# Panel A: 2SLS with Full Controls
# Panel B: Reduced Form scaled by 10th→90th percentile of batch tendency
#=========================================================================
library(tidyverse)
library(ggplot2)
library(patchwork)

# =========================================================================
# PANEL A DATA: 2SLS
# =========================================================================

mayo_2sls <- tribble(
  ~outcome,           ~label,                         ~coef,    ~se,
  "ln_disp_time",     "Log Time to Disposition",       0.651,   0.101,
  "ln_ED_LOS",        "Log LOS",                       0.597,   0.088,
  "imgTests",         "Number of Imaging Tests",        1.241,   0.116,
  "RTN_72_HR_ADMIT",  "72-Hour Return with Admission", -0.0146,  0.019
)

mgh_2sls <- tribble(
  ~outcome,           ~label,                         ~coef,    ~se,
  "ln_disp_time",     "Log Time to Disposition",       0.4990,  0.2784,
  "ln_ED_LOS",        "Log LOS",                       0.5320,  0.2637,
  "imgTests",         "Number of Imaging Tests",        1.3716,  0.2694,
  "RTN_72_HR_ADMIT",  "72-Hour Return with Admission", -0.0422,  0.0430
)

# =========================================================================
# PANEL B DATA: Reduced Form (unscaled coefficients)
# =========================================================================

# Mayo RF (from table)
mayo_rf_raw <- tribble(
  ~outcome,           ~label,                         ~coef,    ~se,
  "ln_disp_time",     "Log Time to Disposition",       1.140,   0.1592,
  "ln_ED_LOS",        "Log LOS",                       1.046,   0.1350,
  "imgTests",         "Number of Imaging Tests",        2.173,   0.2103,
  "RTN_72_HR_ADMIT",  "72-Hour Return with Admission", -0.0257,  0.0341
)

# MGH RF (from your output)
mgh_rf_raw <- tribble(
  ~outcome,           ~label,                         ~coef,    ~se,
  "ln_disp_time",     "Log Time to Disposition",       0.4917,  0.2687,
  "ln_ED_LOS",        "Log LOS",                       0.5243,  0.2519,
  "imgTests",         "Number of Imaging Tests",        1.352,   0.2829,
  "RTN_72_HR_ADMIT",  "72-Hour Return with Admission", -0.0415,  0.0418
)

# 10th-90th percentile gaps
mayo_delta <- 0.050
mgh_delta  <- 0.01398795 - (-0.01774625)  # = 0.03173

# Scale: coef_scaled = coef * delta, SE_scaled = SE * delta
mayo_rf <- mayo_rf_raw %>%
  mutate(coef = coef * mayo_delta,
         se   = se * mayo_delta)

mgh_rf <- mgh_rf_raw %>%
  mutate(coef = coef * mgh_delta,
         se   = se * mgh_delta)


# =========================================================================
# HELPER: build panel data + difference test + bracket data
# =========================================================================

build_panel_data <- function(mayo_df, mgh_df, outcome_order) {
  
  # Difference test
  diff_test <- mayo_df %>%
    dplyr::select(outcome, label, coef_mayo = coef, se_mayo = se) %>%
    left_join(
      mgh_df %>% dplyr::select(outcome, coef_mgh = coef, se_mgh = se),
      by = "outcome"
    ) %>%
    mutate(
      diff = coef_mayo - coef_mgh,
      se_diff = sqrt(se_mayo^2 + se_mgh^2),
      z_stat = diff / se_diff,
      p_value = 2 * pnorm(-abs(z_stat))
    )
  
  # Plot data
  mayo_df$site <- "Mayo Clinic"
  mgh_df$site  <- "MGH"
  df <- bind_rows(mayo_df, mgh_df) %>%
    mutate(
      ci_low  = coef - 1.96 * se,
      ci_high = coef + 1.96 * se,
      label = factor(label, levels = rev(outcome_order)),
      site = factor(site, levels = c("Mayo Clinic", "MGH")),
      significant = ifelse(abs(coef / se) > 1.96, "TRUE", "FALSE")
    )
  
  # Bracket data
  BRACKET_OFFSET <- 0.08
  bracket_data <- diff_test %>%
    mutate(
      label = factor(label, levels = rev(outcome_order)),
      y_num = as.numeric(label),
      p_label = case_when(
        p_value < 0.001 ~ "p < 0.001",
        TRUE ~ paste0("p = ", formatC(p_value, format = "f", digits = 2))
      ),
      bracket_x = pmax(
        coef_mayo + 1.96 * se_mayo,
        coef_mgh + 1.96 * se_mgh
      ) + BRACKET_OFFSET
    )
  
  list(df = df, bracket_data = bracket_data, diff_test = diff_test)
}

outcome_order <- c(
  "Log Time to Disposition",
  "Log LOS",
  "Number of Imaging Tests",
  "72-Hour Return with Admission"
)

panel_a <- build_panel_data(mayo_2sls, mgh_2sls, outcome_order)
panel_b <- build_panel_data(mayo_rf, mgh_rf, outcome_order)

# Scale bracket offset for panel B (much smaller numbers)
panel_b$bracket_data$bracket_x <- pmax(
  panel_b$diff_test$coef_mayo + 1.96 * panel_b$diff_test$se_mayo,
  panel_b$diff_test$coef_mgh + 1.96 * panel_b$diff_test$se_mgh
) + 0.008


# =========================================================================
# PRINT DIFFERENCE TESTS
# =========================================================================

cat("\n=========================================================\n")
cat("  Panel A: 2SLS Difference Tests\n")
cat("=========================================================\n\n")
print(panel_a$diff_test %>% 
        dplyr::select(label, coef_mayo, coef_mgh, z_stat, p_value) %>%
        mutate(across(where(is.numeric), ~round(., 4))), n = 10)

cat("\n=========================================================\n")
cat("  Panel B: Reduced Form (scaled) Difference Tests\n")
cat("=========================================================\n\n")
print(panel_b$diff_test %>% 
        dplyr::select(label, coef_mayo, coef_mgh, z_stat, p_value) %>%
        mutate(across(where(is.numeric), ~round(., 4))), n = 10)


# =========================================================================
# HELPER: make a single panel plot
# =========================================================================

make_panel <- function(panel_list, panel_title, x_limits, x_breaks) {
  
  df <- panel_list$df
  bd <- panel_list$bracket_data
  
  ggplot(df, aes(x = coef, y = label)) +
    
    geom_vline(xintercept = 0, color = "#1a365d", linewidth = 0.7) +
    
    geom_errorbarh(
      aes(xmin = ci_low, xmax = ci_high, color = significant, group = site),
      height = 0, linewidth = 0.9,
      position = position_dodge(width = 0.6)
    ) +
    
    geom_point(
      aes(fill = significant, shape = site, group = site),
      size = 3.5, color = "white", stroke = 0.5,
      position = position_dodge(width = 0.6)
    ) +
    
    # Bracket: vertical
    geom_segment(
      data = bd,
      aes(x = bracket_x, xend = bracket_x,
          y = y_num - 0.25, yend = y_num + 0.25),
      inherit.aes = FALSE, linewidth = 0.4, color = "gray40"
    ) +
    # Bracket: top tick
    geom_segment(
      data = bd,
      aes(x = bracket_x - (diff(x_limits) * 0.008), xend = bracket_x,
          y = y_num + 0.25, yend = y_num + 0.25),
      inherit.aes = FALSE, linewidth = 0.4, color = "gray40"
    ) +
    # Bracket: bottom tick
    geom_segment(
      data = bd,
      aes(x = bracket_x - (diff(x_limits) * 0.008), xend = bracket_x,
          y = y_num - 0.25, yend = y_num - 0.25),
      inherit.aes = FALSE, linewidth = 0.4, color = "gray40"
    ) +
    # P-value label
    geom_text(
      data = bd,
      aes(x = bracket_x + (diff(x_limits) * 0.01), y = y_num, label = p_label),
      inherit.aes = FALSE, hjust = 0, size = 3.1, color = "gray30", fontface = "italic"
    ) +
    
    scale_color_manual(
      values = c("TRUE" = "#dc2626", "FALSE" = "#9ca3af"), guide = "none"
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#dc2626", "FALSE" = "#9ca3af"), guide = "none"
    ) +
    scale_shape_manual(
      values = c("Mayo Clinic" = 21, "MGH" = 24), name = "Site"
    ) +
    guides(
      shape = guide_legend(
        override.aes = list(fill = "#dc2626", color = "white", size = 4)
      )
    ) +
    
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    
    labs(
      title = panel_title,
      x = "Coefficient Estimate",
      y = NULL
    ) +
    
    theme_bw(base_size = 13) +
    theme(
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 11),
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 11),
      legend.key.size = unit(1.2, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(clip = "off")
}


# =========================================================================
# BUILD PANELS
# =========================================================================

p_a <- make_panel(
  panel_a, 
  "A. 2SLS Estimates",
  x_limits = c(-0.6, 2.5),
  x_breaks = seq(-0.5, 2.0, 0.5)
)

p_b <- make_panel(
  panel_b,
  expression(bold("B. Reduced Form (scaled: 10th" %->% "90th percentile)")),
  x_limits = c(-0.04, 0.17),
  x_breaks = seq(-0.04, 0.16, 0.04)
)


# =========================================================================
# COMBINE
# =========================================================================

combined <- p_a + p_b +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Effect of Discretionary Batching: Mayo Clinic vs. MGH",
    subtitle = "Full controls with 95% confidence intervals",
    caption = expression(
      paste("Note: Red = significant at 5%; Grey = not significant. p-values test H"[0], ": ", beta[Mayo], " = ", beta[MGH], ".")
    ),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#64748b"),
      plot.caption = element_text(size = 9, hjust = 0, color = "#64748b"),
      legend.position = "top"
    )
  ) &
  theme(legend.position = "top")

print(combined)

ggsave("outputs/figures/coef_comparison_mayo_mgh.pdf", combined, 
       width = 14, height = 5, dpi = 300)
ggsave("outputs/figures/coef_comparison_mayo_mgh.png", combined, 
       width = 14, height = 5, dpi = 300)




################################################################################
# Author: J Jameson
################################################################################

# Libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

# Load the data ----------------------------------------------------------------
path <- "~/Sue Goldie Dropbox/Jacob Jameson/ED MGH"

data_enc <- read_excel(
  paste0(path, "/2024 03 13 ED Effectiveness Data.xlsx"), 
  sheet = "encounters", col_types = c("text", 
                                      "text", "text", "text", "text", "text", 
                                      "numeric", "date", "text", "text", 
                                      "date", "numeric", "text", "text", 
                                      "text", "date", "text", "text", 
                                      "numeric", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "numeric", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text"))
glimpse(data_enc)
data_enc <- filter(data_enc, age_at_arrival_years >= 18)
data_enc$rfv <- str_to_lower(data_enc$rfv)
table(data_enc$initial_care_area)
data_enc <- data_enc %>%
  mutate(care_area = case_when(
    str_detect(initial_care_area, "^Eval|Eval Virtual") ~ "Eval",
    str_detect(initial_care_area, "Fast Track|FT |FT Virtual|FT ARRIVAL") ~ "Fast track",
    str_detect(initial_care_area, "Pedi") ~ "Pedi",
    str_detect(initial_care_area, "Urgent|Acute|Orange") ~ "Urgent",
    str_detect(initial_care_area, "Super Track|ST |ST Virtual|ST ARRIVAL") ~ "Super track",
    str_detect(initial_care_area, "CDU|Virtual Observation") ~ "Observation",
    TRUE ~ "Other"
  ))

# make data_enc$triage_started_dts datatime
summary(as.numeric((data_enc$triage_started_dts - data_enc$arrival_dts)/60))


summary(as.numeric((data_enc$ed_departure_dts - data_enc$arrival_dts)/60))

table(data_enc$sex)



data_enc <- data_enc %>%
  mutate(
    arrival_dts = as_datetime(arrival_dts),
    ed_departure_dts = as_datetime(ed_departure_dts)
  )

# Sort by patient ID and arrival time
data_enc <- data_enc %>%
  arrange(deid_pat_id, arrival_dts)

# Calculate the time difference between arrivals and departures
data_enc <- data_enc %>%
  group_by(deid_pat_id) %>%
  mutate(
    time_since_last_departure = difftime(arrival_dts, lag(ed_departure_dts), units = "hours"),
    returned_within_72hrs = ifelse(!is.na(time_since_last_departure) & time_since_last_departure <= 72, 1, 0)
  ) %>%
  ungroup()

# Identify patients who returned within 72 hours
patients_returned <- data_enc %>%
  group_by(deid_pat_id) %>%
  summarize(returned_within_72hrs = max(returned_within_72hrs, na.rm = TRUE)) %>%
  filter(returned_within_72hrs == 1)

# View patients who returned within 72 hours
patients_returned


complaint_map <- list(
  'Abdominal Complaints' = c(
    'abdominal pain', 'pelvic pain', 'testicle pain',
    'rectal pain', 'constipation', 'diarrhea', 'nausea', 'vomiting',
    'nausea/vomiting', 'hematemesis', 'inguinal hernia', 'nephrolithiasis',
    'gi problem', 'gi bleeding', 'rectal bleeding', 'groin pain', 
    'rectal prolapse', 'bloated', 'abdominal cramping',
    'hernia', 'fecal impaction', 'diverticulitis', 'ascites',
    'pancreatitis', 'cholelithiasis', 'abdominal injury',
    'vomiting blood', 'black or bloody stool', 'vomitting with diarrhea',
    'gas', 'gastritis', 'umbilical hernia', 'hernia', 'abdominal injury',
    'irritable bowel syndrome', 'fecal incontinence', 'gastroesophageal reflux',
    'reflux'
  ),
  
  'Abnormal Test Results' = c(
    'abnormal lab', 'hypotension', 'abnormal potassium', 'elevated lfts',
    'abnormal sodium', 'abnormal ecg',
    'abnormal x-ray', 'abnormal calcium', 'abnormal cxr', 'abnormal potassium',
    'abnormal sodium', 'leukocytosis', 'hypercalcemia', 'proteinuria',
    'phs amb ren electrolyte abnormality', 'phs amb nut underweight',
    'anemia','blood sugar problem','hypertension','hyperglycemia',
    'hypoglycemia','diabetes','diabetes - other','diabetic ketoacidosis',
    'coagulation disorder','neutropenia','thrombophilia'
  ),
  
  'Allergic Reaction' = c(
    'allergic reaction', 'medication reaction', 'urticaria', 'allergies',
    'immunization reactions',
    'hives', 'angioedema', 'anaphylaxis', 'allergic rhinitis',
    'skin irritation', 'eczema', 'psoriasis', 'itching', 'urticaria',
    'poison ivy', 'dermatitis', 'alopecia', 'eye allergy', 'lip swelling'
  ),
  
  'Back or Flank Pain' = c(
    'back pain', 'flank pain', 'phs amb chi lower back pain',
    'low back pain', 'sciatica', 'tailbone pain',
    'back injury', 'phs amb chi upper back pain', 'phs amb chi mid back pain'
  ),
  
  'Breast Complaints' = c(
    'breast pain', 'breast mass', 
    'breast discharge', 'breast problem', 'breast mass', 'mastitis'
  ),
  
  'Cardiac Arrhythmias' = c(
    'tachycardia', 'atrial fibrillation', 'palpitations',
    'irregular heart beat', 'cardiac arrest',
    'bradycardia', 'rapid heart rate', 'atrial flutter',
    'ventricular tachycardia',
    'ventricular tachycardia', 'premature ventricular contractions',
    'supraventricular tachycardia', 'pacemaker problem', 'pacemaker check',
    'paralysis', 'cardiomyopathy', 'pericardial effusion'
  ),
  
  'Chest Pain' = c(
    'chest pain', 'chest discomfort',
    'chest wall pain',
    'thoracic pain', 'angina', 'pleurisy', 'chest wall pain', 
    'aortic aneurysm','aortic dissection','cardiomyopathy',
    'heart problem','pericardial effusion'
  ),
  
  'Dizziness/Lightheadedness/Syncope' = c(
    'dizziness', 'lightheadedness', 'pre syncope', 'syncope',
    'loss of consciousness',
    'vertigo', 'photophobia', 'lightheadedness'
  ),
  
  'Ear Complaints' = c(
    'ear pain', 'otalgia', 'ear laceration', 'ear drainage',
    'hearing problem', 'otitis media',
    'ear injury', 'cerumen impaction', 'ear problem',
    'ear fullness', 'hearing loss',
    'earache', 'otitis externa', 'earache', 'tinnitus', 'ear fullness',
    'phs amb aural fullness', 'hearing loss', 'hearing problem'
  ),
  
  'Epistaxis' = c(
    'nose bleed',
    'epistaxis', 'phs amb nosebleed', 'nose injury', 'nasal drainage',
    'nasal swelling', 'nasal polyps', 'nose injury'
  ),
  
  'Exposures, Bites, and Envenomations' = c(
    'animal bite', 'insect bite', 'tick bite', 'tick removal',
    'body fluid exposure',
    'chemical exposure', 'heat exposure', 'smoke inhalation',
    'electric shock', 'human bite', 'bite',
    'animal bite', 'cold exposure', 'toxic inhalation', 'chemical exposure',
    'smoke inhalation', 'battery', 'poisoning', 'venom exposure',
    'post-exposure prophylaxis'
    
  ),
  
  'Extremity Complaints' = c(
    'foot pain', 'hand pain', 'shoulder pain', 'leg pain', 'knee pain',
    'neck pain', 'arm pain', 'hip pain', 'ankle pain', 'wrist pain',
    'finger injury', 'toe injury', 'shoulder injury', 'hip injury',
    'leg injury', 'arm injury', 'foot injury', 'wrist injury',
    'ankle injury', 'foot problem', 'arm swelling', 'swelling',
    'joint swelling', 'joint pain', 'extremity weakness',
    'extremity laceration', 'hand injury', 'foot infection',
    'finger laceration', 'puncture wound', 'rib injury',
    'elbow pain', 'toe pain', 'finger pain', 'elbow injury',
    'clavicle injury', 'leg cramps', 'foot blister', 'gout',
    'trigger finger', 'foot numbness', 'ingrown toenail',
    'knee injury', 'foot swelling', 'foot burn', 'hand burn',
    'muscle pain', 'joint stiffness', 'tailbone pain',
    'toe problem', 'arthritis',
    'cold feet',
    'flat foot',
    'groin injury',
    'groin swelling',
    "phs amb vas lower extremity ulcer",
    'leg swelling',
    'neck swelling',
    'phs amb pls hand/finger fracture',
    'toenail problem',
    'varicose veins',
    'heel pain', 'hand problem', 'arm problem', 'leg problem',
    'foot ulcer', 'foot blister', 'foot numbness', 'foot/ankle fracture',
    'elbow injury', 'knee injury', 'dislocation', 'cold extremity',
    'skin injury', 'burning sensation', 'achilles pain', 'toe pain',
    'toe problem', 'wrist pain', 'ankle injury', 'clavicle injury',
    'phs amb chi lower extremity weakness', 'phs amb chi upper extremity weakness',
    'phs amb pcpt humerus fracture', 'phs amb pcpt multiple rib fractures',
    'phs amb pcpt femur fracture', 'fracture', 'bursitis', 'muscle tension',
    'muscle pain', 'limb pain'
  ),
  
  'Eye Complaints' = c(
    'eye pain', 'eye problem', 'eye trauma', 'conjunctivitis',
    'blurred vision', 'diplopia', 'loss of vision', 'phs change in vision',
    'eye drainage', 'eye twitching', 'eye irritation',
    'burning eyes', 'blepharitis', 'foreign body in eye',
    'photophobia', 'stye', 'uveitis', 'eyelid pain', 'eye allergy',
    'spots and/or floaters', 'vision disturbance', 'foreign body in eye',
    'eye irritation', 'blepharitis', 'burning eyes', 'eyelid problem',
    'orbital mass',
    'ptosis',
    'strabismus'
  ),
  
  'Falls, Motor Vehicle Crashes, Assaults, and Trauma' = c(
    'fall', 'assault victim', 'head injury', 'head laceration',
    'laceration', 'sexual assault', 'motor vehicle crash',
    'motorcycle crash', 'burn', 'chest injury',
    'facial burn', 'facial injury', 'stab wound', 'lip laceration',
    'domestic violence', 'concussion', 'fracture', 'mouth injury',
    'dental injury', 'abdominal injury',
    'gun shot wound', 'stab wound', 'motorcycle vs pedestrian',
    'traumatic brain injury', 'subdural hematoma', 'epidural hematoma',
    'skull fractures', 'injury', 'back injury', 'neck injury',
    'phs amb pls facial/mandibular fractures', 'phs amb pcpt multiple rib fractures',
    'phs amb pcpt femur fracture', 'fracture', 'abrasion', 'dislocation',
    'concussion', 'facial laceration',
    'trauma'
  ),
  
  'Fatigue and Weakness' = c(
    'fatigue', 'weakness', 'weakness - generalized',
    'generalized body aches',
    'lethargy', 'anemia',
    'fatigue', 'lethargy', 'weakness', 'phs amb pulr weakness/fatigue',
    'impaired balance', 'decreased functional mobility', 'restless leg syndrome'
  ),
  
  'Fevers, Sweats or Chills' = c(
    'fever', 'chills', 'excessive sweating', 'bacteremia',
    'blood infection',
    'sickle cell anemia',
    'fever/infection', 'hot flashes', 'diaphoresis', 'chills', 'sweating'
  ),
  
  'Foreign Body' = c(
    'foreign body', 'swallowed foreign body', 'foreign body in vagina',
    'foreign body in ear', 'foreign body in eye', 'foreign body in nose',
    'foreign body in rectum', 'foreign body in skin',
    'foreign body in ear', 'foreign body in eye',
    'foreign body in skin', 'choking',
    'ingestion'
  ),
  
  'Gastrointestinal Issues' = c(
    'vomiting', 'nausea', 'nausea/vomiting', 'diarrhea', 'constipation',
    'dehydration', 'hematemesis', 'gi bleeding', 'gi problem',
    'rectal bleeding', 'dry mouth',
    'eating issues',
    'emesis',
    'gerd', 'dysphagia',
    'hiccups',
    'jaundice',
    'pinworms',
    'poor appetite',
    'spitting up (reflux)',
    'vomiting blood', 'black or bloody stool', 'vomitting with diarrhea',
    'gas', 'gastritis', 'umbilical hernia', 'hernia', 'irritable bowel syndrome',
    'fecal incontinence', 'gastroesophageal reflux', 'reflux',
    'pancreatitis', 'liver mass', 'liver lesion', 'cirrhosis',
    'ascites', 'diverticulitis', 'ulcerative colitis', 'crohn\'s disease',
    'hepatitis', 'abdominal injury', 'gastritis', 'colitis',
    'melena', 'brbpr', 'heartburn', 'gastroesophageal reflux',
    'crohn\'s disease', 'ulcerative colitis', 'hematochezia',
    'constipation', 'rectal problems', 'diverticulitis',
    'pancreatitis', 'fecal impaction', 'hemorrhoids'
  ),
  
  'Genital Complaints' = c(
    'testicle pain', 'vaginal bleeding', 'vaginal itching',
    'vaginal pain', 'concerns of sti', 'foreign body in vagina',
    'female gu problem', 'male gu problem', 'penile discharge', 'penis injury',
    'penis pain', 'erectile dysfunction', 'genital warts', 'bartholin\'s cyst',
    'vaginal discharge', 'sti screening', 'std', 'exposure to std',
    'genital herpes', 'phs amb hiv exposure', 'gonorrhea', 'vaginitis',
    'vaginal bleeding - pregnant', 'menorrhagia', 'amenorrhea',
    'decreased fetal movement', 'ectopic pregnancy', 'rupture of membranes',
    'mastitis', 'ovarian cyst', 'ovarian torsion', 'pelvic mass',
    'postpartum care', 'genital problem', 'prostate check',
    'testicle injury',
    'testicular mass',
    'unplanned sexual encounter', 'std exposure', 'penis pain',
    'vaginal discharge', 'vaginal prolapse', 'vaginitis',
    'ovarian cyst', 'ovarian torsion', 'pelvic mass'
  ),
  
  'Medical Device or Treatment Issue' = c(
    'aicd problem', 'vascular access problem', 'medication refill',
    'suture / staple removal', 'suture/staple questions', 'wound check',
    'post-op problem', 'phs fistula', 'assess success with recommended aid',
    'foot wound check',
    'heart transplant',
    'phs amb new diabetic patient',
    'phs amb nwh art us',
    'pacemaker problem', 'pacemaker check', 'left ventricle assist device',
    'feeding tube', 'ostomy care', 'stoma complication', 'cast removal',
    'cast repair', 'cast check', 'cast problem', 'phs amb medication backorder issue',
    'medication management', 'medication change request', 'medication follow up',
    'medication problem', 'medication question', 'iv medication', 'wound care',
    'wound management', 'wound dehiscence', 'drainage from incision',
    'dressing change', 'procedure', 'chemo related symptoms', 'dialysis access',
    'cast removal', 'cast problem', 'shunt',
    'tracheostomy tube change', 'dressing change',
    'stoma complication', 'wound dehiscence'
  ),
  
  'Medication Request' = c(
    'medication refill',
    'medication change request', 'medication follow up', 'medication problem',
    'medication question', 'new medication request', 'medication visit',
    'phs amb medication backorder issue',
    'new medication request'
  ),
  
  'Neurological Issue' = c(
    'headache', 'seizures', 'febrile seizure', 'altered mental status',
    'slurred speech', 'numbness', 'stroke', 'aphasia',
    'cerebrovascular accident', 'confusion', 'gait problem','neurologic problem',
    'speech problem',
    'transient ischemic attack', 'paralysis', 'spasms', 'tremors', 'myasthenia gravis',
    'multiple sclerosis', 'neuralgia', 'dementia', 'stroke', 'vertigo',
    'brain tumor', 'headache', 'migraine', 'seizure disorder', 'traumatic brain injury',
    'tics', 'memory loss', 'word finding problems', 'difficulty swallowing',
    'dysarthria', 'difficulty walking', 'gait abnormality', 'tingling',
    'numbness', 'peripheral neuropathy', 'chronic nerve pain', 'restless leg syndrome',
    'phs amb chi lower extremity numbness', 'phs amb chi upper extremity weakness',
    'phs amb chi lower extremity weakness', 'hydrocephalus', 'meningitis',
    'neuralgia', 'torticollis', 'cerebrospinal fluid leak',
    'facial droop', 'trigeminal neuralgia', 'phs amb nsr brain mass',
    'tremors', 'shaking', 'memory loss', 'seizure disorder',
    'cerebrospinal fluid leak', 'gait abnormality', 'brain tumor',
    'phs amb nsr stroke', 'myasthenia gravis', 'difficulty swallowing',
    'tingling', 'head and neck pain', 'facial paralysis',
    'difficulty walking'
  ),
  
  'Other' = c(
    'other', 'null', 'phs amb new patient', 'phs amb return pt',
    'school/sports/camp physical', 'blood pressure check', 'infection',
    'weight gain', 'failure to thrive', 'phs amb vas dvt',
    'phs amb nsr radiculopathy', "symptom flare",
    'lump', 'mass', 'skin (lump/localized swelling)', 'lymphadenopathy',
    'lymph nodes, swollen', 'phs amb nut underweight', 'immunizations',
    'flu vaccine', 'vaccine', 'vaccine follow-up', 'housing', 'forms and paperwork',
    'letter for school/work', 'inpatient admission', 'baseline evaluation',
    'new evaluation', 'labs', 'labs only', 'procedure', 'symptom management',
    'palliative care follow-up', 'phs amb substance use - alcohol related',
    'phs amb substance use - benzodiaepines', 'phs amb substance use - opioid related',
    'phs amb pcpt unexplained bruising', 'ip case management stem cell transplant continuum',
    'feeding difficulty', 'feeding tube', 'newborn', 'infusion', 'flu symptoms',
    'upper respiratory infection', 'uri', 'viral infection', 'cold extremity',
    'social service visit', 'symptom management', 'phs amb globus sensation',
    'weight loss', 'lung nodule', 'anasarca', 'lymphadenopathy',
    'lymph nodes, swollen', 'homeless', 'evaluate for admission',
    'phs amb neck mass', 'thyroid problem', 'covid-19 post discharge follow-up',
    'fussy', 'annual exam', 'weekly visit', 'follow-up',
    'social complaints', '2nd opinion', 'pain', 'labs only',
    'edema', 'polydipsia', 'mass', 'mental health problem',
    'behavior problem','circulatory problem',
    'consultation with social work',
    'covid-19 return to work inquiry',
    'crying (3 months or older)', 'lupus',
    'lymphoma', 'bleeding',
    'osteomyelitis',
    'thyroid nodule'
  ),
  
  'Other Pain' = c(
    'facial pain', 'facial swelling', 'jaw pain', 'mouth pain',
    'back pain', 'neck pain','bleeding gums',
    'dental pain',
    'dental problem',
    'fibromyalgia',
    'infection of the mouth',
    'sickle cell pain crisis',
    'chronic pain', 'pain', 'incisional pain', 'cancer pain', 'hand pain',
    'foot pain', 'neck pain', 'arm pain', 'leg pain', 'facial jaw pain',
    'temporomandibular joint pain', 'head and neck pain', 'oral pain',
    'eye pain', 'ear pain', 'tooth pain', 'broken tooth',
    'chronic pain', 'mouth lesions', 'oral swelling',
    'head and neck pain', 'tailbone pain'
  ),
  
  'Post-Op Issue' = c(
    'post-op problem', 'wound infection',
    'post-op', 'post-op problem', 'wound dehiscence', 'drainage from incision',
    'wound care', 'wound management', 'chemo related symptoms', 'ostomy care',
    'stoma complication', 'postpartum care', 'incisional pain',
    'wound dehiscence'
  ),
  
  'Psychiatric Complaints' = c(
    'suicidal', 'psychiatric evaluation', 'anxiety', 'panic attack',
    'hallucinations', 'depression', 'agitation', 'insomnia',
    'delirium tremens (dts)',  'manic behavior',
    'grief/bereavement', 'paranoia', 'delusional', 'psychotic symptoms',
    'psychological issue', 'depression', 'anxiety', 'stress',
    'adjustment disorder with mixed anxiety and depressed mood',
    'post-traumatic stress disorder', 'suicide attempt', 'phs amb substance use - alcohol related',
    'phs amb substance use - benzodiaepines', 'phs amb substance use - opioid related',
    'racing thoughts', 'sleeping problem', 'insomnia', 'eating disorder',
    'psychosocial problems', 'dementia',
    'paranoid', 'aggressive behavior', 'suicide attempt',
    'homicidal', 'eating disorder', 'adjustment disorder with mixed anxiety and depressed mood',
    'psychosocial problems'
  ),
  
  'Shortness of Breath' = c(
    'shortness of breath', 'respiratory distress', 'asthma', 'wheezing',
    'copd', 'shortness of breath', 'phs amb difficulty breathing',
    'tachypnea', 'hyperventilating', 'cyanosis', 'asthma', 'wheezing',
    'stridor', 'pleural effusion', 'pulmonary embolism', 'pneumonia',
    'lung nodule', 'respiratory distress', 'near drowning', 'bronchitis',
    'hypoxia', 'congestive heart failure', 'pneumonia',
    'airway obstruction', 'near drowning', 'aspiration',
    'sleep apnea',
    'tracheal stenosis'
  ),
  
  'Skin Complaints' = c(
    'rash', 'abscess', 'cellulitis', 'pruritus', 'skin check',
    'wound infection', 'nail problem', 'cyst',  'bleeding/bruising',
    'pilonidal symptoms',
    'sore',
    'thrush',
    'vasculitis',
    'skin irritation', 'skin discoloration', 'eczema', 'psoriasis',
    'itching', 'rash', 'urticaria', 'hives', 'diaper rash', 'skin ulcer',
    'skin injury', 'burning sensation', 'burn', 'blister', 'frostbite',
    'sunburn', 'skin (lump/localized swelling)', 'changing mole', 'dermatitis',
    'skin problem', 'alopecia', 'cellulitis', 'abscess', 'herpes zoster',
    'poison ivy', 'varicella', 'stye', 'genital warts', 'skin infection',
    'recurrent skin infections', 'melanoma',
    'rash or redness', 'blister', 'sunburn', 'skin problem',
    'skin ulcer', 'herpes zoster', 'mrsa', 'head lice'
  ),
  
  'Substance Abuse Issues' = c(
    'alcohol problem', 'alcohol intoxication', 'addiction problem',
    'drug overdose',
    'phs amb substance use - alcohol related', 'phs amb substance use - benzodiaepines',
    'phs amb substance use - opioid related', 'drug screen', 'alcohol problem',
    'addiction problem', 'drug overdose', 'withdrawal', 'substance abuse',
    'withdrawal', 'drug / alcohol assessment'
  ),
  
  'Upper Respiratory Symptoms' = c(
    'sore throat', 'cough', 'nasal congestion',
    'upper respiratory infection', 'uri', 'influenza', 'flu symptoms',
    'coughing up blood', 'throat problem', 'sore throat', 'strep throat',
    'laryngitis', 'hoarseness', 'nasal congestion', 'sinus congestion',
    'sinus headache', 'sinusitis', 'allergic rhinitis', 'phs amb noisy breathing',
    'sinus congestion', 'sinusitis', 'nasal polyps', 'covid-19 inquiry',
    'croup',
    'hemoptysis',
    'sinus cancer',
    'sneezing',
    'snoring'
  ),
  
  'Pregnancy Related' = c(
    'non-stress test', 'vaginal bleeding', 'vaginal pain',
    'concerns of sti', 'weight gain', 'contraception',
    'vaginal bleeding - pregnant', 'ectopic pregnancy', 'rupture of membranes',
    'decreased fetal movement', 'emesis during pregnancy', 'vomiting during pregnancy',
    'morning sickness', 'nausea/vomiting in pregnancy', 'laboring',
    'pregnancy problem', 'postpartum care', 'threatened miscarriage',
    'miscarriage', 'pregnancy-related issue',
    'contractions', 'possible pregnancy', 'threatened miscarriage',
    'miscarriage', 'vaginal bleeding - pregnant', 'emesis during pregnancy',
    'laboring', 'pregnancy problem'
  ),
  'Renal' = c(
    'urinary tract infection', 'hematuria', 'urinary retention',
    'pyelonephritis', 'nephrolithiasis',
    'recurrent uti', 'cystitis', 'urinary urgency', 'urine leakage',
    'nephritis', 'nephrotic syndrome', 'kidney transplant pre-evaluation',
    'chronic kidney disease', 'aki (acute kidney injury)', 'acute renal failure',
    'diabetic nephropathy', 'aki (acute kidney injury)', 'acute renal failure'
  ),
  
  'Urinary Complaints' = c(
    'urinary retention', 'dysuria', 'hematuria', 'urinary incontinence',
    'blood in urine', 'urinary urgency', 'urine leakage', 'bladder pain',
    'bladder problem', 'difficulty urinating', 'urinary frequency',
    'polyuria',
    'urinary symptom', 'lower urinary tract symptoms', 'fecal incontinence',
    'proteinuria', 'phs amb vas dialysis access',
    'blood in urine', 'urinary frequency', 'urination pain',
    'urinary problem', 'difficulty urinating',
    'lower urinary tract symptoms', 'urinary symptom'
  )
)

data_enc$complaint <- data_enc$rfv

for (i in seq(1,length(complaint_map))){
  name <- names(complaint_map[i])
  complaint <- complaint_map[[i]]
  
  data_enc$complaint <- ifelse(
    data_enc$complaint %in% complaint, name, data_enc$complaint
  )
}

table(data_enc$complaint)



data_enc %>%
  mutate(
    systolic_bp = as.numeric(sub("/.*", "", first_bp_reading)),
    diastolic_bp = as.numeric(sub(".*/", "", first_bp_reading)),
    pulse = as.numeric(first_pulse_reading),
    respiratory_rate = as.numeric(first_rr_reading),
    
    # Ensure comorbidities are numeric (0 or 1)
    across(c(hypertension, diabetes, cad, chf, copd, asthma, aud, ivdu, afib, cva, cancer), 
           ~ifelse(is.na(.), 0, as.numeric(.)))
  ) -> data_enc


data_enc$tachycardic <- ifelse(
  is.na(data_enc$pulse) == F & data_enc$pulse > 100, 1, 0
)

data_enc$tachypneic <- ifelse(
  is.na(data_enc$respiratory_rate)  == F  & data_enc$respiratory_rate > 20, 1, 0
)

data_enc$febrile <- ifelse(
  is.na(data_enc$first_temp_reading)  == F  & data_enc$first_temp_reading > 99.5, 1, 0
)

data_enc$hypotensive <- ifelse(
  is.na(data_enc$systolic_bp)  == F  & data_enc$systolic_bp < 90, 1, 0
)


summary(data_enc$tachycardic)
summary(data_enc$tachypneic)
summary(data_enc$febrile)
summary(data_enc$hypotensive)

summary(data_enc$age_at_arrival_years)
table(data_enc$race_list)




data_img <-  read_excel("~/Sue Goldie Dropbox/Jacob Jameson/ED MGH/2024 03 13 ED Effectiveness Data.xlsx", 
                        sheet = "imaging", col_types = c("text", 
                                                         "date", "date", "date", "date", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric"))



data_img %>%
  mutate(us = us - us_bedside) %>%
  group_by(deid_enc_id) %>%
  arrange(deid_enc_id) %>%
  mutate(time_diff = as.numeric(difftime(img_available_dts, img_order_dts, units = "mins"))) %>%
  filter(us == 1, time_diff < 10000) %>%
  select(deid_enc_id, time_diff) %>%
  summary()

data_img <- data_img %>%
  dplyr::select(deid_enc_id, img_order_dts, img_available_dts, xr, ct, us, mri)  %>%
  group_by(deid_enc_id) %>%
  arrange(deid_enc_id) %>%
  mutate(time_diff = as.numeric(difftime(img_available_dts, img_order_dts, units = "mins"))) 

filter(data_img, us == 1)$time_diff %>% summary()



data_iv <-  read_excel("~/Sue Goldie Dropbox/Jacob Jameson/ED MGH/2024 03 13 ED Effectiveness Data.xlsx", 
                       sheet = "medications", col_types = c("text", 
                                                            "text", "date", 'text', 'text'))

data_iv <- data_iv %>%
  filter(medication_route_dsc == 'Intravenous', med_orderstatus == 'Completed') 


table(data_iv$pharmaceutical_class_dsc)


# Filter for IV contrast agents in data_iv
contrast_agents <- c("CARDIOVASCULAR DIAGNOSTICS-RADIOPAQUE")

data_iv_contrast <- data_iv %>%
  filter(pharmaceutical_class_dsc %in% contrast_agents) %>%
  distinct(deid_enc_id, .keep_all = TRUE) 

# Filter CT scans in data_img
data_ct <- data_img %>%
  filter(ct == 1)

# Join data_iv_contrast with data_ct
data_ct_with_contrast <- data_ct %>%
  left_join(data_iv_contrast, by = "deid_enc_id", relationship = 'many-to-one') %>%
  mutate(
    time_diff = difftime(img_order_dts, med_order_dts, units = "mins"),
    ct_type = case_when(
      !is.na(time_diff) & time_diff >= -60 & time_diff <= 60 ~ "With Contrast",
      TRUE ~ "Without Contrast"
    )
  )

# Select relevant columns and ensure uniqueness
data_ct_with_contrast <- data_ct_with_contrast %>%
  mutate(time_diff = as.numeric(difftime(img_available_dts, img_order_dts, units = "mins"))) 


# get summary of time_diff for CT scans with contrast
filter(data_ct_with_contrast, ct_type == 'With Contrast')$time_diff %>% summary()
filter(data_ct_with_contrast, ct_type == 'Without Contrast')$time_diff %>% summary()


summary(data_img$mri)





data_lab <-  read_excel("~/Sue Goldie Dropbox/Jacob Jameson/ED MGH/2024 03 13 ED Effectiveness Data.xlsx", 
                        sheet = "labs")

length(unique(data_lab$deid_enc_id))






# Define categories for IV fluids and meds
iv_fluids <- c("ELECTROLYTE MAINTENANCE", "SODIUM/SALINE PREPARATIONS", 
               "IV SOLUTIONS: DEXTROSE AND LACTATED RINGERS", 
               "IV SOLUTIONS: DEXTROSE-SALINE", 
               "IV SOLUTIONS: DEXTROSE-WATER")

data_iv <- data_iv %>%
  mutate(
    iv_type = case_when(
      pharmaceutical_class_dsc %in% iv_fluids ~ "IV Fluid",
      TRUE ~ "IV Med"
    )
  )


data_iv %>%
  filter(iv_type == "IV Med") %>%
  distinct(deid_enc_id) %>% nrow()





library(readxl)
img <- read_excel("~/Sue Goldie Dropbox/Jacob Jameson/ED MGH/2024 03 13 ED Effectiveness Data.xlsx", 
                                                sheet = "imaging", col_types = c("text", 
                                                                                 "date", "date", "date", "date", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric"))
glimpse(img)

lab <- read_excel("~/Sue Goldie Dropbox/Jacob Jameson/ED MGH/2024 03 13 ED Effectiveness Data.xlsx", 
                                                sheet = "labs", col_types = c("text", 
                                                                              "date", "date", "text"))
glimpse(lab)


prov <- read_excel("~/Sue Goldie Dropbox/Jacob Jameson/ED MGH/2024 03 13 ED Effectiveness Data.xlsx", 
                                                sheet = "providers", col_types = c("text", 
                                                                                   "date", "text", "text", "text"))
glimpse(prov)
