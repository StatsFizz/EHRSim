################################################################################
#
#   Script:       P5_b_Simulate_comorbidities_otherdis.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient frailty clinicalmeasure
#
#   Data created:	R object: comorbid, deathdate_comorbid (for creating death dates)
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates a dataset resembling real EHR data,
#					      containing information about the patient's 
#					      (non-CVD) comorbidities.  
#
################################################################################
#
#   Packages needed: data.table
#
################################################################################


##############################
#  Gather the required data  #
##############################


# Initialize variables to zero
comorbid_add <- as.data.frame(matrix(0, nrow = 1, ncol = 17))
add_vars <- c("pancreatitis_event", "fibrosis_prev",
                "dementia_prev","copd_prev",
                "upper_gi_bleed_event", "peptic_ulcer_event",
                "hpylori_event","gerd_event","gerd_prev",
                "oesophagitis_event","barretts_prev",
                "cancer_lung_prev","cancer_breast_prev","cancer_prostate_prev",
                "cancer_bowel_prev","cancer_gastric_prev", "cancer_oth_prev")
names(comorbid_add)<- add_vars 
comorbid<-cbind(comorbid, comorbid_add) 
comorbid[,ckd:= "No CKD"]
rm(comorbid_add, add_vars)




###########################
#  Generate pancreatitis  #
###########################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$pancreatitis_coefs,"logitpancreatitis") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$pancreatitis_coefs,"logitpancreatitis") 
comorbid<-genind(comorbid, "logitpancreatitis", "pancreatitis_event") 




#############################
#  Generate liver fibrosis  #
#############################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$fibrosis_coefs,"logitfibrosis") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$fibrosis_coefs,"logitfibrosis") 
comorbid<-genind(comorbid, "logitfibrosis", "fibrosis_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "fibrosis_event", "fibrosis_prev", "fibrosis_onset")
comorbid[,c("fibrosis_event"):=NULL]



####################
#  COPD diagnosis  #
####################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$copd_coefs,"logitcopd") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$copd_coefs,"logitcopd") 
comorbid<-genind(comorbid, "logitcopd", "copd_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "copd_event", "copd_prev", "copd_onset")
comorbid[,c("copd_event"):=NULL]





########################
#  Dementia diagnosis  #
########################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$dementia_coefs,"logitdementia") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$dementia_coefs,"logitdementia") 
comorbid<-genind(comorbid, "logitdementia", "dementia_event") 
# Only retain dementia diagnoses post-40 years old
comorbid[,dementia_event:=fifelse(agein_det<40, 0, dementia_event)]
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "dementia_event", "dementia_prev", "dementia_onset")
comorbid[,c("dementia_event"):=NULL]




############################
#  Chronic Kidney Disease  #
############################


comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$ckd_coefs,"logitckd") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$ckd_coefs,"logitckd") 
q<-quantile(comorbid$logitckd, probs=c(0.9,0.95,0.98,0.99))

comorbid[,ckd := fifelse(logitckd<q[1], 0,  
                  fifelse(logitckd<q[2], 1, 
                    fifelse(logitckd<q[3], 2, 
                      fifelse(logitckd<q[4], 3, 4))))]
comorbid[,c("logitckd") := NULL ] 

# Set CKD to not improve
comorbid[order(patient_id, agein_det), ckd := cummax(ckd), by=patient_id]

##  Mark first onset and CKD changes (including onset)
comorbid[order(patient_id, agein_det),
  ckd_lag := shift(ckd), by=patient_id]
comorbid[order(patient_id, agein_det),
         ckd_first:= seq_len(.N)==1 & ckd>=1 | seq_len(.N)>1 & ckd>=1 & ckd_lag==0,
         by=patient_id]
comorbid[order(patient_id, agein_det),
         ckd_change:= ckd_first==1 | seq_len(.N)>1 & ckd!=ckd_lag,
         by=patient_id]
comorbid[,c("ckd_lag"):=NULL]

# Make CKD into a factor variable
comorbid[, ckd := factor(ckd, levels=0:4,
    labels = c("No CKD", "Stage 3a", "Stage 3b", "Stage 4", "Stage 5"))]



########################
#  H.Pylori diagnosis  #
########################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$hpylori_coefs,"logithpylori") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$hpylori_coefs,"logithpylori") 
comorbid<-genind(comorbid, "logithpylori", "hpylori_event") 




#########################
#  GERD/GORD diagnosis  #
#########################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$gerd_coefs,"logitgerd") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$gerd_coefs,"logitgerd") 
comorbid<-genind(comorbid, "logitgerd", "gerd_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "gerd_event", "gerd_prev")






##################
#  Peptic ulcer  #
##################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$peptic_ulcer_coefs,"logitpulcer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$peptic_ulcer_coefs,"logitpulcer") 
comorbid<-genind(comorbid, "logitpulcer", "peptic_ulcer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "peptic_ulcer_event", "peptic_ulcer_prev")



  
  
#######################
#  Upper GI bleeding  #
#######################
  
  
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$upper_gi_bleed_coefs,"logitgibleed") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$upper_gi_bleed_coefs,"logitgibleed")
### KEEP logitgibleed FOR MEDICATION GENERATION
# Aspirin and oral anticoagulant stopped if high risk of bleeding
comorbid[,propbleed:=logitgibleed]
comorbid<-genind(comorbid, "logitgibleed", "upper_gi_bleed_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "upper_gi_bleed_event", "upper_gi_bleed_prev")






##################
#  Oesophagitis  #
##################


comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$oesophagitis_coefs,"logitoesophagitis") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$oesophagitis_coefs,"logitoesophagitis") 
comorbid<-genind(comorbid, "logitoesophagitis", "oesophagitis_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "oesophagitis_event", "oesophagitis_prev")





##########################
#  Barrett's oesophagus  #
##########################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$barretts_coefs,"logitbarretts") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$barretts_coefs,"logitbarretts") 
comorbid<-genind(comorbid, "logitbarretts", "barretts_event") 
# NB: Assume GERD is a pre-requisite
comorbid[,barretts_event:=fifelse(gerd_prev==0, 0, barretts_event)]
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "barretts_event", "barretts_prev", "barretts_onset")
comorbid[,c("barretts_event"):=NULL]


   

############
#  Cancer  #
############

# Lung
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$cancer_lung_coefs,"logitlungcancer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$cancer_lung_coefs,"logitlungcancer") 

comorbid[
  order(patient_id, agein_det),
  cumprop_smoke := cumsum(smokecat == "Current") /
    cumsum(!is.na(smokecat)),
  by = patient_id
]

comorbid[,logitlungcancer := logitlungcancer
         + fifelse(cumprop_smoke==0, 0, 
                   fifelse(cumprop_smoke<0.2, 0.5,
                     fifelse(cumprop_smoke<0.4, 1,
                       fifelse(cumprop_smoke<0.6, 1.5,
                         fifelse(cumprop_smoke<0.8, 2,
                           2.5)))))]
comorbid[,c("cumprop_smoke"):=NULL]
comorbid<-genind(comorbid, "logitlungcancer", "lungcancer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "lungcancer_event", "cancer_lung_prev", "cancer_lung_onset")
comorbid[,c("lungcancer_event"):=NULL]


# Prostate
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$cancer_prostate_coefs,"logitprostatecancer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$cancer_prostate_coefs,"logitprostatecancer") 
comorbid<-genind(comorbid, "logitprostatecancer", "prostatecancer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "prostatecancer_event", "cancer_prostate_prev", "cancer_prostate_onset")
comorbid[,c("prostatecancer_event"):=NULL]


# Breast
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$cancer_breast_coefs,"logitbreastcancer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$cancer_breast_coefs,"logitbreastcancer") 
comorbid<-genind(comorbid, "logitbreastcancer", "breastcancer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "breastcancer_event", "cancer_breast_prev", "cancer_breast_onset")
comorbid[,c("breastcancer_event"):=NULL]


# Bowel
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$cancer_bowel_coefs,"logitbowelcancer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$cancer_bowel_coefs,"logitbowelcancer") 
comorbid<-genind(comorbid, "logitbowelcancer", "bowelcancer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "bowelcancer_event", "cancer_bowel_prev", "cancer_bowel_onset")
comorbid[,c("bowelcancer_event"):=NULL]


# Gastric
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$cancer_gastric_coefs,"logitgastriccancer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$cancer_gastric_coefs,"logitgastriccancer") 
comorbid<-genind(comorbid, "logitgastriccancer", "gastriccancer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "gastriccancer_event", "cancer_gastric_prev", "cancer_gastric_onset")
comorbid[,c("gastriccancer_event"):=NULL]

# Other
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$cancer_oth_coefs,"logitothcancer") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$cancer_oth_coefs,"logitothcancer") 
comorbid<-genind(comorbid, "logitothcancer", "othcancer_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "othcancer_event", "cancer_oth_prev", "cancer_oth_onset")
comorbid[,c("othcancer_event"):=NULL]




###############
#  Endoscopy  #
###############

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$endoscopy_coefs,"logitendoscopy") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$endoscopy_coefs,"logitendoscopy") 
comorbid<-genind(comorbid, "logitendoscopy", "endoscopy_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", "endoscopy_event", "endoscopy_prev")
