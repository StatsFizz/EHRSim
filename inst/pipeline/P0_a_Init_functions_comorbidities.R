################################################################################
#
#   Script:       P0_a_Init_functions_comorbidities.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: None
#
#   Data created:	R functions: createlogit_cm, createlogit_oth,
#                 createlogit_med, genind, genprevonset
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script creates functions used in generating the comorbidity
#               and medication data.  
#
################################################################################
#
#   Packages needed: data.table
#
################################################################################


########################################################
# Program to rapidly generate a linear predictor: CVD  #
########################################################


createlogit_cm <- function(indata, coef_list, logitvar) {
  indata <- as.data.table(indata)
  
  frailty_lu <- data.table(frailty = c(1,2,3,4), val = coef_list$frailty)
  male_age_lu <- data.table(
    mage = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95),
    val = coef_list$mage)
  female_age_lu <- data.table(
    fage = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95),
    val = coef_list$fage)
  deprivation_lu <- data.table(deprivation_person = c(1,2,3,4,5), val = coef_list$deprivation)
  smoke_lu <- data.table(smokecat = c("No", "Current", "Ex"), val = coef_list$smoke)
  alc_lu <- data.table(alccat = c("None", "Low", "Medium", "High"),
    val = coef_list$alc)
  eth_lu <- data.table(ethnicity = c("White", "Asian", "Black", "Mixed", "Other"),
    val = coef_list$eth)

  indata[frailty_lu,       frailty_val   := i.val, on = "frailty"]
  indata[male_age_lu,      male_age_val  := i.val, on = "mage"]
  indata[female_age_lu,  female_age_val  := i.val, on = "fage"]
  indata[deprivation_lu, deprivation_val := i.val,on = "deprivation_person"]
  indata[smoke_lu,       smoke_val := i.val, on = "smokecat"]
  indata[alc_lu,         alc_val   := i.val, on = "alccat"]
  indata[eth_lu,         eth_val   := i.val, on = "ethnicity"]

  indata[,  (logitvar) := coef_list$cons +
        fcoalesce(frailty_val, 0) +
        fifelse(sex == "Male",   fcoalesce(male_age_val,0), 0) +
        fifelse(sex == "Female", fcoalesce(female_age_val,0), 0) +
        fifelse(sex == "Female", coef_list$female, 0) +
        fcoalesce(deprivation_val, 0) +
        coef_list$bmi*(fcoalesce(bmi, 25)-25) +
        fcoalesce(smoke_val, 0) +
        fcoalesce(alc_val, 0) +
        fcoalesce(eth_val, 0) +
        coef_list$harmalc*fcoalesce(harmalc, 0L) +
        coef_list$ht_prev*ht_prev +
        coef_list$ht_event*ht_event +
        coef_list$diabetes_prev*diabetes_prev +
        coef_list$mi_event*mi_event+
        coef_list$mi_prev*mi_prev+
        coef_list$chd_prev*chd_prev +
        coef_list$heartfail_prev*heartfail_prev +
        coef_list$other_athero_prev*other_athero_prev +
        coef_list$pvd_prev*pvd_prev +
        coef_list$stroke_event*stroke_event +
        coef_list$stroke_prev*stroke_prev]
  
  indata[,frailty_val := NULL]
  indata[,male_age_val := NULL]
  indata[,female_age_val :=NULL]
  indata[,deprivation_val := NULL]
  indata[,smoke_val := NULL]
  indata[,alc_val := NULL]
  indata[,eth_val := NULL]

 return(indata)
}

 




##################################################################
# Program to rapidly generate a linear predictor: other disease  #
##################################################################


createlogit_oth <- function(indata, coef_list, logitvar) {
  indata <- as.data.table(indata)
  
  
  ckd_lu <- data.table(
    ckd = c("No CKD", "Stage 3a", "Stage 3b", "Stage 4", "Stage 5"),
    val = coef_list$ckd)
  indata[ckd_lu, ckd_val := i.val,on = "ckd"]
  
  indata[,  (logitvar) := get(logitvar) + 
           coef_list$pancreatitis_event*pancreatitis_event +
           coef_list$fibrosis_prev*fibrosis_prev +
           coef_list$dementia_prev*dementia_prev +
           coef_list$copd_prev*copd_prev +
           fcoalesce(ckd_val, 0)+
           coef_list$hpylori_event*hpylori_event+
           coef_list$gerd_event*gerd_event+
           coef_list$gerd_prev*gerd_prev+
           coef_list$peptic_ulcer_event*peptic_ulcer_event +
           coef_list$upper_gi_bleed_event*upper_gi_bleed_event+
           coef_list$oesophagitis_event*oesophagitis_event+
           coef_list$barretts_prev*barretts_prev +
           coef_list$cancer_lung_prev*cancer_lung_prev +
           coef_list$cancer_breast_prev*cancer_breast_prev +
           coef_list$cancer_prostate_prev*cancer_prostate_prev+
           coef_list$cancer_bowel_prev*cancer_bowel_prev+      
           coef_list$cancer_gastric_prev*cancer_gastric_prev +
           coef_list$cancer_oth_prev*cancer_oth_prev
  ]
  
  indata[,ckd_val := NULL]
  
  return(indata)
}







###################################################
# Program to rapidly generate a linear predictor  #
###################################################


createlogit_med <- function(indata, coef_list, logitvar) {
  indata <- as.data.table(indata)
  
  indata[,  (logitvar) := get(logitvar) + 
           coef_list$aspirin*aspirin +
           coef_list$clopidogrel*clopidogrel +
           coef_list$oac*oac +
           coef_list$inhaled_steroid*inhaled_steroid +
           coef_list$systemic_steroid*systemic_steroid+
           coef_list$nsaid*nsaid
  ]
  
  return(indata)
}



##############################################################
# Program to generate indicator from a provided logit score  #
##############################################################

genind <- function(indata, logitvar, outvar) {
  indata <- as.data.table(indata)
  indata[, pvar := 1 / (1 + exp(-get(logitvar)))]
  indata[, (outvar) := rbinom(.N, 1, pvar)]
  indata[, c("pvar", logitvar) := NULL]
  return(indata)
}


#######################################################################
# Program to generate indicators of previous disease and mark onset   #
#######################################################################

# Assume currentvar is a binary (0/1) indicator of current disease
genprevonset <- function(indata, patient_id, datevar,
                         currentvar, prevvar, onsetvar = NULL) {
  indata <- as.data.table(indata)
  setorderv(indata, c(patient_id, datevar))
  indata[, (prevvar) := cummax(get(currentvar)), 
         by = patient_id]
  
  if (!is.null(onsetvar)) {
    indata[, (onsetvar) :=
             as.integer(get(currentvar) == 1 &
                          cumsum(get(currentvar)) == 1),
           by = patient_id]
  }
  return(indata)
}

