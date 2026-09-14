################################################################################
#
#   Script:      P11_Add_clinical_codes.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		consultation, patient, clinmeasure, codes, codes_freq_used
#
#   Data created:	R object: clinical
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script adds "realistic" clinical (diagnosis) codes to the 
#               simulated comorbidity data.
#
################################################################################
#
#   Packages needed: 
#
################################################################################





#############################################
#  Attach clinical codes to diagnoses etc   #
#############################################


eventlist <- c("disease_onset", "data_transfer", "event")


# Filter consultations and merge sex
comorbid_cd <- consultation[consultation_type %in% eventlist][
  patient[, .(patient_id, sex)],  on = "patient_id"]

# Rename "other" cancers separately for men and women (different codes assigned)
comorbid_cd[sex == "Male" & trigger_event == "cancer_oth_onset_date",
  trigger_event := "cancer_other_m_onset_date"]
comorbid_cd[sex == "Female" & trigger_event == "cancer_oth_onset_date",
  trigger_event := "cancer_other_f_onset_date"]

# List of conditions
cond <- c("barretts", "cancer_lung", "cancer_breast", "cancer_prostate",
        "cancer_bowel", "cancer_gastric", "cancer_other_m", "cancer_other_f",
        "chd", "ckd3a", "ckd3b", "ckd4","ckd5",
        "copd", "dementia", "diabetes", "fibrosis", "endoscopy",
        "gerd_event", "heartfail", "hpylori", "ht_event",
        "mi_event", "oesophagitis", "athero",
        "pancreatitis", "peptic_ulcer",
        "pvd", "stroke_event", "upper_gi_bleed")

# Attach codes to diagnoses
for (c in cond) {
    tmp <- comorbid_cd[grepl(c, trigger_event),
    .(consultation_id, patient_id, event_date = date,
    clinical_code = sample(x = codes$clinical[[c]], size = .N, 
                             prob = codes_freq_used$clinical[[c]],
                             replace = TRUE),
      entity = codes$entity$med_hist)]
  assign(c, tmp)
}

# Remove unneeded objects
rm(c, tmp, eventlist)





##################################
#  Codes for entry to registers  #
##################################

register_conds <- c("cancer", "chd", "stroke_event", "diabetes")

# Only keep rows with condition above, add entity code for relevant register
registers <- comorbid_cd[ grepl(paste(register_conds, collapse = "|"), trigger_event)][
  , entity := fcase(
      grepl("stroke",   trigger_event), codes$entity$register_stroke,
      grepl("chd",      trigger_event), codes$entity$register_chd,
      grepl("diabetes", trigger_event), codes$entity$register_diabetes,
      grepl("cancer",   trigger_event), codes$entity$register_cancer,
      default = NA_real_)]

# Add clinical code (admin code)
registers[ ,  `:=`(event_date    = date,
                   clinical_code = codes$general$admin)]

# Keep only required variables
registers<-registers[, .(consultation_id, patient_id, event_date, clinical_code, entity)]

# Remove unneeded objects
rm(register_conds)





##########################
#  Codes for follow-ups  #
##########################


# Keep only follow-up consultations 
eventlist <- c("fup_disease_onset", "fup_event",
               "chronic_disease_review", "health_check")
follow_up <- consultation[consultation_type %in% eventlist]
rm(eventlist)

# Add clinical codes
follow_up[, clinical_code := fifelse(
  grepl("gastric|bowel|diabetes|gerd|endoscopy|heartfail|mi|oesophagitis|peptic_ulcer",
        trigger_event), codes$general$diet,
  sample(c(codes$general$health_ed,codes$general$surgery, codes$general$advice,
           codes$general$counselling), prob = c(0.25, 0.25, 0.25, 0.25),
         size = .N, replace = TRUE))]

# Add entity codes
follow_up[,  entity := fcase(
    grepl("chd", trigger_event), 
      sample(c(codes$entity$consult_cardio,codes$entity$hp_chd), 
      prob = c(0.8, 0.2), size = .N, replace = TRUE),
    grepl("stroke", trigger_event), codes$entity$hp_stroke, 
    grepl("ht_event", trigger_event), codes$entity$hp_hypertension,
    grepl("copd", trigger_event), 
      sample(c(codes$entity$consult_copd,codes$entity$status_copd), 
      prob = c(0.6, 0.4), size = .N, replace = TRUE),
    grepl("ckd", trigger_event),               codes$entity$consult_kidney,
    grepl("diabetes", trigger_event), 
      sample(c(codes$entity$hp_diabetes,codes$entity$status_diabetes), 
      prob = c(0.6, 0.4), size = .N, replace = TRUE),
    default =  sample(c(codes$entity$fam_hist,codes$entity$med_hist), 
      prob = c(0.1, 0.9), size = .N, replace = TRUE))]


# Keep only required variables
follow_up <- follow_up[, .(consultation_id, patient_id, event_date = date,
    clinical_code, entity)]






#################################
#  Codes for clinical measures  #
#################################

### Height 

# Height measurements only
height <- clinmeasure[measure == "height" & !is.na(data1),
  .(suppl_info_id, consultation_id, patient_id,
    event_date = date, data1, data2)]

# Add clinical and entity codes
height[, `:=`(clinical_code = codes$measure$height,
              entity= codes$entity$height)]

# Separate into coded and supplementary (numerical) data
height_code <- height[, .(consultation_id, patient_id, event_date,
    clinical_code, entity, suppl_info_id)]
height_suppl <- height[,.(suppl_info_id, patient_id, entity, data1,data2)]
rm(height)


### Weight/BMI
weight <- clinmeasure[measure %chin% c("weight", "bmi") & !(is.na(data1)&is.na(data2)),
                      .(suppl_info_id, consultation_id, patient_id,
                        event_date = date, data1, data2, measure_detail, 
                        entity= codes$entity$weight)]
# Add clinical codes
weight_code <- copy(weight)
weight_code[, clinical_code := fcase(
    measure_detail=="Underweight",  sample(codes$measure$bmi_under,  size=.N, replace=TRUE),
    measure_detail=="Healthy",      sample(codes$measure$bmi_normal, size=.N, replace=TRUE),
    measure_detail=="Overweight",   sample(codes$measure$bmi_over,   size=.N, replace=TRUE),
    measure_detail=="Obese I",      sample(codes$measure$bmi_obese1, size=.N, replace=TRUE),
    measure_detail=="Obese II",     sample(codes$measure$bmi_obese2, size=.N, replace=TRUE),
    measure_detail=="Obese III",    sample(codes$measure$bmi_obese3, size=.N, replace=TRUE),
    is.na(measure_detail), codes$measure$weight,
    default = NA)][,c("data1", "data2"):=NULL] 

# Separate out supplementary (numerical) data
weight_suppl <- copy(weight)
weight_suppl <- weight_suppl[,.(suppl_info_id, patient_id, entity, data1,data2)]
rm(weight)

# Choose some patients to have BMI-category codes without supplementary
# numerical BMI values. The merge/removal still happens by suppl_info_id.
patients_no_suppl <- unique(weight_code[!is.na(measure_detail), .(patient_id)])[
  runif(.N) < 0.04]
no_suppl_info <- weight_code[
  patient_id %chin% patients_no_suppl$patient_id & !is.na(measure_detail),
  .(suppl_info_id)][, toremove := 1L]

# Remove from supplementary data
weight_suppl <- merge(weight_suppl, no_suppl_info, by = "suppl_info_id",
  all.x = TRUE)[is.na(toremove), !"toremove"]

# Mark as no supplementary data in coded data
weight_code <- merge(weight_code, no_suppl_info, by = "suppl_info_id",
  all.x = TRUE)[, suppl_info_id := fifelse(!is.na(toremove), "0", suppl_info_id)]
weight_code <- weight_code[, .(consultation_id, patient_id, event_date,
    clinical_code, entity, suppl_info_id)]
rm(no_suppl_info, patients_no_suppl)




### Smoking
smoke <- clinmeasure[measure == "smoking_status" & !(is.na(data1)&is.na(data2)),
                      .(suppl_info_id, consultation_id, patient_id,
                        event_date = date, data1, data2, 
                        entity= codes$entity$smoke)]
# Add clinical codes
smoke_code <- copy(smoke)[,u:=runif(.N)]
smoke_code[, clinical_code := fcase(
  data1==0  & u<0.4,  sample(codes$measure$smoke_no,  size=.N, replace=TRUE),
  data1==1  & u<0.4,  sample(codes$measure$smoke_cr, size=.N, replace=TRUE),
  data1==2  & u<0.4,  sample(codes$measure$smoke_ex,   size=.N, replace=TRUE),
  data1==-1,      codes$measure$smoking,
  default = codes$measure$smoking)][,c("u", "data1", "data2"):=NULL] 

# Separate out supplementary (numerical) data
smoke_suppl <- copy(smoke)
smoke_suppl <- smoke_suppl[,.(suppl_info_id, patient_id, entity, data1,data2)]
rm(smoke)

# Choose some code people to have no supplementary (numerical) info
no_suppl_info <- smoke_code[clinical_code!=codes$measure$smoking, .(suppl_info_id)][
            runif(.N) < 0.04][,toremove := 1L]

# Remove from supplementary data
smoke_suppl <- merge(smoke_suppl,no_suppl_info, by = "suppl_info_id",
                      all.x = TRUE)[is.na(toremove),!"toremove"]

# Mark as no supplementary data in coded data
smoke_code <- merge(smoke_code,no_suppl_info, by = "suppl_info_id",
  all.x = TRUE)[, suppl_info_id := fifelse(!is.na(toremove), "0", suppl_info_id)]
smoke_code<-smoke_code[, .(consultation_id, patient_id, event_date,
                             clinical_code, entity, suppl_info_id)]
rm(no_suppl_info)






### Alcohol
alcohol <- clinmeasure[measure == "alcohol_consumption" & !(is.na(data1)&is.na(data2)),
                     .(suppl_info_id, consultation_id, patient_id,
                       event_date = date, data1, data2, 
                       entity= codes$entity$alcohol)]
# Add clinical codes
alcohol_code <- copy(alcohol)[,u:=runif(.N)]
alcohol_code[, clinical_code := fcase(
  data2==1 & u<0.4,  sample(codes$measure$alc_hm,  size=.N, replace=TRUE),
  data1==0  & u<0.4,  sample(codes$measure$alc_no, size=.N, replace=TRUE),
  data1==1  & u<0.4,  sample(codes$measure$alc_cr, size=.N, replace=TRUE),
  data1==2  & u<0.4,  sample(codes$measure$alc_ex, size=.N, replace=TRUE),
  data1==-1,      codes$measure$alcohol,
  default = codes$measure$alcohol)][,c("u", "data1", "data2"):=NULL] 

# Separate out supplementary (numerical) data
alcohol_suppl <- copy(alcohol)
alcohol_suppl <- alcohol_suppl[,.(suppl_info_id, patient_id, entity, data1,data2)]
rm(alcohol)

# Choose some code people to have no supplementary (numerical) info
no_suppl_info <- alcohol_code[clinical_code!=codes$measure$alcohol, .(suppl_info_id)][
  runif(.N) < 0.04][,toremove := 1L]

# Remove from supplementary data
alcohol_suppl <- merge(alcohol_suppl,no_suppl_info, by = "suppl_info_id",
                     all.x = TRUE)[is.na(toremove),!"toremove"]

# Mark as no supplementary data in coded data
alcohol_code <- merge(alcohol_code,no_suppl_info, by = "suppl_info_id",
  all.x = TRUE)[, suppl_info_id := fifelse(!is.na(toremove), "0", suppl_info_id)]
alcohol_code<-alcohol_code[, .(consultation_id, patient_id, event_date,
                               clinical_code, entity, suppl_info_id)]
rm(no_suppl_info)



### Any additional consultations not coded above (i.e. with no clinical measure)

# Weight/BMI
weight_other<-consultation[consultation_type=="weight_management"]
weight_other<-weight_other[!weight_code, on="consultation_id"]
weight_other[, `:=`(clinical_code=codes$general$advice,
       entity=codes$entity$weight,
       event_date=date,
       suppl_info_id=0)]
weight_other<-weight_other[, .(consultation_id, patient_id, event_date,
                               clinical_code, entity, suppl_info_id)]
# Smoking
smoke_other<-consultation[consultation_type=="smoke_cessation"]
smoke_other<-smoke_other[!smoke_code, on="consultation_id"]
smoke_other[, `:=`(clinical_code=codes$general$advice,
                    entity=codes$entity$smoke,
                    event_date=date,
                    suppl_info_id=0)]
smoke_other<-smoke_other[, .(consultation_id, patient_id, event_date,
                               clinical_code, entity, suppl_info_id)]

# Alcohol
alcohol_other<-consultation[consultation_type%in%c("harmful_alcohol_use","alcohol_advice")]
alcohol_other<-alcohol_other[!alcohol_code, on="consultation_id"]
alcohol_other[, `:=`(clinical_code=codes$general$advice,
                     entity=codes$entity$alcohol,
                     event_date=date,
                     suppl_info_id=0)]
alcohol_other<-alcohol_other[, .(consultation_id, patient_id, event_date,
                             clinical_code, entity, suppl_info_id)]




#################################
#  Codes for diabetes and COPD  #
#################################

# Keep COPD and diabetes consultations and generate clinical and entity codes
copd_diab <- consultation[consultation_type %chin% c("copd", "diabetes")][
  , `:=`(entity = fcase(
      consultation_type == "copd", 
        sample(c(codes$entity$consult_copd, codes$entity$status_copd),
        prob = c(0.7, 0.3), size = .N, replace = TRUE),
      consultation_type == "diabetes",
        sample(c(codes$entity$consult_diabetes, codes$entity$status_diabetes),
        prob = c(0.7, 0.3), size = .N, replace = TRUE),
      default = NA_real_),
    clinical_code = sample(
      c(codes$general$advice, codes$general$counselling, codes$general$diet,
        codes$general$health_ed, codes$general$review, codes$general$surgery),
      prob = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1), size = .N, replace = TRUE),
    event_date = date)]

# Keep only required variables
copd_diab<-copd_diab[, .(consultation_id, patient_id,
                         event_date, clinical_code, entity)]




#################################
#  Codes for HRT/contraception  #
#################################

# Keep HRT/contraception consultations and generate clinical and entity codes
hrt_pill <- consultation[  consultation_type %chin% c("hrt", "contraception")][
  , `:=`(entity = fifelse(consultation_type == "contraception", 
                          codes$entity$contraception, codes$entity$hrt),
      clinical_code = sample(c(codes$general$advice, codes$general$medication_review,
        codes$general$surgery),prob = c(0.1, 0.7, 0.2), size = .N, replace = TRUE),
      event_date = date)]

# Keep only required variables
hrt_pill<-hrt_pill[, .(consultation_id, patient_id,
                           event_date, clinical_code, entity)]
  




#####################################
#  Codes for medication management  #
#####################################

medicine <- consultation[consultation_type %chin% c("medicine_management", "repeat_issue")][
  , `:=`(entity = codes$entity$med_review, 
         clinical_code = sample(c(codes$general$advice, codes$general$medication_review, 
               codes$general$surgery),prob = c(0.1, 0.7, 0.2), size = .N, replace = TRUE),
         event_date = date)]

# Keep only required variables
medicine<-medicine[, .(consultation_id, patient_id,
                       event_date, clinical_code, entity)]



 


#####################
#  Surgery/clinic   #
#####################

# GP surgery
surgery <- consultation[consultation_type %chin% c("surgery")][
  , `:=`(entity = codes$entity$med_review, 
         clinical_code = sample(
           c(codes$general$surgery,
             codes$common$resp_tract_infect,
             codes$common$ear_infect1,
             codes$common$ear_infect2,
             codes$common$acute_sinusitis,
             codes$common$acute_pharyngitis,
             codes$general$diet,
             codes$general$health_ed,
             codes$general$counselling,
             codes$general$advice), 
           prob=c(0.4, 0.1, 0.025, 0.025, 0.05, 0.05, 
                  0.1, 0.1, 0.05, 0.1), size=.N, replace=TRUE),
         event_date = date)]

# Keep only required variables
surgery<-surgery[, .(consultation_id, patient_id,
                       event_date, clinical_code, entity)]

# Clinics
clinic <- consultation[consultation_type %chin% c("clinic")][
  , `:=`(entity = sample(
                    c( codes$entity$diet,
                       codes$entity$advice,
                       codes$entity$hp_chd,
                       codes$entity$hp_hypertension,
                       codes$entity$bp,
                       codes$entity$pulse,
                       codes$entity$consult_cardio,
                       codes$entity$consult_kidney), 
                    prob=c(0.3, 0.3, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05), 
                    size=.N, replace=TRUE),   
         clinical_code = sample(
           c( codes$general$diet,
              codes$general$health_ed,
              codes$general$advice,
              codes$general$review), 
           prob=c(0.25, 0.25, 0.25, 0.25), size=.N, replace=TRUE),
         event_date = date)]

# Keep only required variables
clinic<-clinic[, .(consultation_id, patient_id,
                     event_date, clinical_code, entity)]




#########################
#  Other consultations  #
#########################


other <- consultation[consultation_type %chin% c("administration", "data_transfer",   
           "mail_to_patient", "other", "outpatients_letter", 
           "phone_call_from_patient", "phone_call_to_patient",   
            "referral_letter", "results", "third_party_consult")][
  , `:=`(entity = fcase(
    consultation_type %in% c("administration", "data_transfer", "other"), codes$entity$admin,
    consultation_type %in% c("outpatients_letter", 
                             "mail_to_patient",
                             "phone_call_from_patient", 
                             "phone_call_to_patient",   
                             "referral_letter", 
                             "third_party_consult"), codes$entity$communication,     
    consultation_type == "results", codes$entity$test_result,
    default= NA),
    clinical_code =  fcase(
      consultation_type %in% c("administration", "data_transfer", "other"), 
        sample(c(codes$general$admin, codes$general$notes_made), 
               prob=c(0.8, 0.2), size=.N, replace=TRUE),
      consultation_type %in% c("outpatients_letter", 
                               "referral_letter", 
                               "third_party_consult"),  codes$general$communication,  
      consultation_type %in% c("phone_call_from_patient", 
                               "phone_call_to_patient"),
        sample(c(codes$general$communication, codes$general$phone), 
               prob=c(0.8, 0.2), size=.N, replace=TRUE),
      consultation_type == "mail_to_patient",        
        sample(c(codes$general$communication, codes$general$letter_to_patient), 
               prob=c(0.8, 0.2), size=.N, replace=TRUE),
      consultation_type == "results", codes$entity$test_result,
      default= NA),
  event_date = date)]

# Keep only required variables
other<-other[, .(consultation_id, patient_id,
                   event_date, clinical_code, entity)]

 



#######################
#  Combine code data  #
#######################

# Combine chronic disease data and remove unneeded objects
clinical <- do.call(rbind, mget(cond))

# Add follow-ups, adding to registries, HRT, contraception, 
# COPD and diabetes consultations
clinical <- rbind(clinical, follow_up, registers, hrt_pill, copd_diab)

# Add general appointments, repeat meds, and admin
clinical <- rbind(clinical, medicine, surgery, clinic, other)

# Add clinical measures 
clinical$suppl_info_id="0" 
clinical <- rbind(clinical, weight_code,  height_code, 
                smoke_code, alcohol_code, 
                weight_other, smoke_other, alcohol_other)
rm(weight_code,  height_code, smoke_code, alcohol_code, 
   weight_other, smoke_other, alcohol_other)

# Tidy data
setorder(clinical, patient_id, event_date)






############################################
#  Combine supplementary (numerical) data  #
############################################


supplement <- rbind(height_suppl, weight_suppl, smoke_suppl, alcohol_suppl)
rm(height_suppl, weight_suppl, smoke_suppl, alcohol_suppl)


# Tidy data
setorder(supplement, patient_id, suppl_info_id)









