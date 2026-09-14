################################################################################
#
#   Script:       P0_d_Init_parameters_death.R
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
#   Purpose:		This script contains the parameters for the models used to
#               generate comorbidity data.
#
################################################################################
#
#   Packages needed: None
#
################################################################################
lambda <- log(1)


#####################################################
#  Parameters:  Non-communicable disease mortality  #
#####################################################


# Time-fixed contributions to hazard
lnhr_noncom_fix <- list(
  deprivation1=-0.2,
  deprivation2=-0.1,
  deprivation3=0,
  deprivation4=0.2,
  deprivation5=0.25,
  ethwhite=0,
  ethasian=0,
  ethblack=0,
  ethmixed=0,
  ethother=0,
  year=0.00333
)

# Time-varying contributions to hazard          
lnhr_noncom_clin <- list(
  frailty1 =-0.15,
  frailty2 =0,
  frailty3 =0.25,
  frailty4 =0.45,
  mage0 = 0,
  mage18 = -7.9+lambda,
  mage25 = -7.5+lambda,
  mage30 = -7.1+lambda,
  mage35 = -6.75+lambda,
  mage40 = -6.4+lambda,
  mage45 = -6+lambda,
  mage50 = -5.5+lambda,
  mage55 = -5.2+lambda,
  mage60 = -4.8+lambda,
  mage65 = -4.3+lambda,
  mage70 = -3.9+lambda,
  mage75 = -3.4+lambda,
  mage80 = -2.8+lambda,
  mage85 = -2.2+lambda,
  mage90 = -1.5+lambda,
  mage95 = 0,
  fage0  = 0,
  fage18 = -8.6+lambda,   
  fage25 = -8.2+lambda,
  fage30 = -7.9+lambda,
  fage35 = -7.4+lambda,
  fage40 = -6.9+lambda,
  fage45 = -6.5+lambda,
  fage50 = -6.1+lambda, 
  fage55 = -5.7+lambda,
  fage60 = -5.2+lambda, 
  fage65 = -4.7+lambda,
  fage70 = -4.3+lambda,
  fage75 = -3.8+lambda,
  fage80 = -3.1+lambda, 
  fage85 = -2.5+lambda,
  fage90 = -1.6+lambda,
  fage95 = 0,
  bmi= -0.02,
  smokecatcurrent = 0.7,
  smokecatex = 0.4,
  smokecatno = 0,
  harmalc= 0.3,
  alccatnone   = 0.05,
  alccatlow    = 0,
  alccatmedium = 0.1,
  alccathigh   = 0.2
)

# Comorbidity contributions to hazard
lnhr_noncom_comorb <- list(
  barretts_onset = 0.7,
  cancer_bowel_onset = 2.5,
  cancer_breast_onset = 1,
  cancer_gastric_onset  = 2.8,    
  cancer_lung_onset = 2.5,
  cancer_oth_onset   =  2,
  cancer_prostate_onset = 0.8,
  chd_onset = 0.35,
  ckd3a= 0.4,
  ckd3b= 0.8,
  ckd4 = 1.5,
  ckd5 = 2.5,                   
  copd_onset  = -0.03,
  dementia_onset = -0.3,
  diabetes_onset = 0.3,
  endoscopy       = 0,
  fibrosis_onset = 2,
  gerd_event    = 0,
  gerd_event_first = -0.15,
  heartfail_onset  = 1,     
  hpylori_event   = 0,
  hpylori_event_first = 0,
  ht_event = 0,
  ht_event_first = 0.03,          
  mi_event = 0.4,   # Initial risk increase
  mi_event_b = -0.2, # drop after a week
  mi_event_c = -0.2, # drop after a month
  mi_event_first    = 0.45, # ongoing elevation in risk
  oesophagitis_event = 0,
  oesophagitis_event_first = 0.2, 
  other_athero_onset = 0.25,
  pancreatitis_event = 0,
  pancreatitis_event_first = 0.4, 
  peptic_ulcer_event   = 0,
  peptic_ulcer_event_first = 0.42,
  pvd_onset        = 0.64,
  stroke_event= 0.6,   # Initial risk increase
  stroke_event_b= -0.3, # drop after a week
  stroke_event_c= -0.3, # drop after a month
  stroke_event_first = 0.4, # ongoing elevation in risk
  upper_gi_bleed_event= 0.05,
  upper_gi_bleed_event_first=0.2
)

# Medication contributions to hazard
lnhr_noncom_presc <- list(
  presc_start_aspirin=-0.13,
  presc_start_clopidogrel=-0.15,
  presc_start_h2ra     =0,
  presc_start_isteroid  =  -0.04, 
  presc_start_nsaid      = -0.08,
  presc_start_oac         = 0.2, 
  presc_start_ppi          = 0,
  presc_start_ssteroid_copd = 0.15,
  presc_start_ssteroid_other = 0.15,
  presc_stop_aspirin    =  0.13,
  presc_stop_clopidogrel = 0.15,
  presc_stop_h2ra     =0,
  presc_stop_isteroid  = 0.04,
  presc_stop_nsaid      = 0.08,
  presc_stop_oac        = -0.2,
  presc_stop_ppi        = 0, 
  presc_stop_ssteroid_copd  = -0.15,
  presc_stop_ssteroid_other  = -0.15
)


# Medication effects represented by the coefficients above:
# PPI/H2RA: no mortality effect.
# Aspirin: reduces non-communicable mortality while actively prescribed.
# OAC: increases non-communicable mortality while actively prescribed in this
#      parameter set. This may represent confounding/high-risk indication rather
#      than a protective treatment effect.
# Clopidogrel: reduces non-communicable mortality while actively prescribed.
# NSAID: reduces non-communicable mortality while actively prescribed.
# Inhaled steroid: reduces non-communicable mortality while actively prescribed.
# Systemic steroid: increases non-communicable mortality while actively prescribed.
# Stop effects reverse the corresponding start effects because P7 accumulates
# active prescription state using start/stop events.





lnhr_noncom_fix <- unlist(lnhr_noncom_fix, use.names = TRUE)
lnhr_noncom_clin <- unlist(lnhr_noncom_clin, use.names = TRUE)
lnhr_noncom_comorb <- unlist(lnhr_noncom_comorb, use.names = TRUE)
lnhr_noncom_presc <- unlist(lnhr_noncom_presc, use.names = TRUE)

 



#################################################
#  Parameters:  Communicable disease mortality  #
#################################################


# Time-fixed contributions to hazard
lnhr_comm_fix <- list(
  deprivation1=0,
  deprivation2=0.04,
  deprivation3=0.15,
  deprivation4=0.24,
  deprivation5=0.30,
  ethwhite=0,
  ethasian=0.01,
  ethblack=0.02,
  ethmixed=0,
  ethother=0,
  year=0.001
)

# Time-varying contributions to hazard          
lnhr_comm_clin <- list(
  frailty1 =-0.1,
  frailty2 =0,
  frailty3 =0.2,
  frailty4 =0.3,
  mage0 = 0,
  mage18 = -10.3+lambda,
  mage25 = -9.9+lambda,
  mage30 = -9.5+lambda,
  mage35 = -9.2+lambda,
  mage40 = -8.8+lambda,
  mage45 = -8.4+lambda,
  mage50 = -8.0+lambda,
  mage55 = -7.6+lambda,
  mage60 = -7.2+lambda,
  mage65 = -6.7+lambda,
  mage70 = -6.3+lambda,
  mage75 = -5.8+lambda,   
  mage80 = -5.2+lambda,
  mage85 = -4.6+lambda,
  mage90 = -3.9+lambda,
  mage95 = 0,
  fage0  = 0,
  fage18 = -11.0+lambda, 
  fage25 = -10.6+lambda,
  fage30 = -10.3+lambda,
  fage35 = -9.8+lambda,
  fage40 = -9.3+lambda,
  fage45 = -8.9+lambda,
  fage50 = -8.5+lambda, 
  fage55 = -8.1+lambda,
  fage60 = -7.6+lambda, 
  fage65 = -7.2+lambda,
  fage70 = -6.7+lambda,
  fage75 = -6.2+lambda,
  fage80 = -5.5+lambda, 
  fage85 = -4.9+lambda,
  fage90 = -4.1+lambda,
  fage95 = 0,
  bmi= -0.02,
  smokecatcurrent = 0.3,
  smokecatex = 0.2,
  smokecatno = 0,
  harmalc= 0,
  alccatnone   = 0.05,
  alccatlow    = 0,
  alccatmedium = 0.1,
  alccathigh   = 0.2
)


# Comorbidity contributions to hazard
lnhr_comm_comorb <- list(
  barretts_onset = 0.05,
  cancer_bowel_onset = 0.1,
  cancer_breast_onset = 0.1,
  cancer_gastric_onset  = 0.1,    
  cancer_lung_onset = 0.1,
  cancer_oth_onset   =  0.1,
  cancer_prostate_onset = 0.1,
  chd_onset   = 0.35,
  ckd3a= 0.02,
  ckd3b= 0.12,
  ckd4 = 0.15,
  ckd5 = 0.65,                   
  copd_onset  = -0.03,
  dementia_onset=-0.3,
  diabetes_onset = 0.3,
  endoscopy       = 0,
  fibrosis_onset= 0.7,
  gerd_event    = 0,
  gerd_event_first= 0.1,
  heartfail_onset  = 0.4,     
  hpylori_event   = 0,
  hpylori_event_first= 0,
  ht_event= 0,
  ht_event_first=  0,          
  mi_event= 0.2,   # Initial risk increase
  mi_event_b= -0.2, # drop after a week
  mi_event_c= 0, # drop after a month
  mi_event_first    = 0.05, # ongoing elevation in risk
  oesophagitis_event = 0,
  oesophagitis_event_first = 0, 
  other_athero_onset= 0.05,
  pancreatitis_event = 0,
  pancreatitis_event_first= 0.4, 
  peptic_ulcer_event   =0,
  peptic_ulcer_event_first =0.2,
  pvd_onset        = 0.08,
  stroke_event= 0.1,   # Initial risk increase
  stroke_event_b= -0.1, # drop after a week
  stroke_event_c= 0, # drop after a month
  stroke_event_first = 0, # ongoing elevation in risk
  upper_gi_bleed_event= 0,
  upper_gi_bleed_event_first=0
)

# Medication contributions to hazard
lnhr_comm_presc <- list(
  presc_start_aspirin=0,
  presc_start_clopidogrel=0,
  presc_start_h2ra     =0,
  presc_start_isteroid  =  0, 
  presc_start_nsaid      = 0,
  presc_start_oac         = 0, 
  presc_start_ppi          = 0,
  presc_start_ssteroid_copd = 0,
  presc_start_ssteroid_other = 0,
  presc_stop_aspirin    =  0,
  presc_stop_clopidogrel = 0,
  presc_stop_h2ra     =0,
  presc_stop_isteroid  = 0,
  presc_stop_nsaid      = 0,
  presc_stop_oac        = 0,
  presc_stop_ppi        = 0, 
  presc_stop_ssteroid_copd  = 0,
  presc_stop_ssteroid_other  = 0
)


lnhr_comm_fix <- unlist(lnhr_comm_fix, use.names = TRUE)
lnhr_comm_clin <- unlist(lnhr_comm_clin, use.names = TRUE)
lnhr_comm_comorb <- unlist(lnhr_comm_comorb, use.names = TRUE)
lnhr_comm_presc <- unlist(lnhr_comm_presc, use.names = TRUE)






##################################################
#  Parameters:  External cause/injury mortality  #
##################################################


# Time-fixed contributions to hazard
lnhr_ext_fix <- list(
  deprivation1=0,
  deprivation2=0.1,
  deprivation3=0.2,
  deprivation4=0.35,
  deprivation5=0.45,
  ethwhite=0,
  ethasian=0.01,
  ethblack=0.02,
  ethmixed=0,
  ethother=0,
  year=0
)


# Time-varying contributions to hazard          
lnhr_ext_clin <- list(
  frailty1 =0.2,
  frailty2 =0,
  frailty3 =-0.1,
  frailty4 =-0.3,
  mage0 = 0,
  mage18 = -10.7+lambda,
  mage25 = -10.3+lambda,
  mage30 = -10.9+lambda,
  mage35 = -10.6+lambda,
  mage40 = -10.2+lambda,
  mage45 = -9.8+lambda,
  mage50 = -9.4+lambda,
  mage55 = -9.0+lambda,
  mage60 = -8.6+lambda,
  mage65 = -8.1+lambda,
  mage70 = -7.7+lambda,
  mage75 = -7.2+lambda,
  mage80 = -6.6+lambda,
  mage85 = -6.0+lambda,
  mage90 = -5.3+lambda,
  mage95 = 0,
  fage0  = 0,
  fage18 = -12.4+lambda, 
  fage25 = -12.0+lambda,
  fage30 = -11.7+lambda,
  fage35 = -11.2+lambda,
  fage40 = -10.7+lambda,
  fage45 = -10.3+lambda,
  fage50 = -9.9+lambda, 
  fage55 = -9.5+lambda,
  fage60 = -9.0+lambda, 
  fage65 = -8.5+lambda,
  fage70 = -8.1+lambda,
  fage75 = -7.6+lambda,
  fage80 = -6.9+lambda,
  fage85 = -6.2+lambda,
  fage90 = -5.4+lambda,
  fage95 = 0,
  bmi= -0.04,
  smokecatcurrent = 0.5,
  smokecatex = 0.3,
  smokecatno = 0,
  harmalc= 1,
  alccatnone   = 0.05,
  alccatlow    = 0,
  alccatmedium = 0.1,
  alccathigh   = 0.2
)


# Comorbidity contributions to hazard
lnhr_ext_comorb <- list(
  barretts_onset = 0,
  cancer_bowel_onset = 0,
  cancer_breast_onset = 0,
  cancer_gastric_onset  = 0,    
  cancer_lung_onset = 0,
  cancer_oth_onset   =  0,
  cancer_prostate_onset = 0,
  chd_onset   = 0,
  ckd3a= 0,
  ckd3b= 0,
  ckd4 = 0,
  ckd5 = 0,                   
  copd_onset  = 0,
  dementia_onset=0.2,
  diabetes_onset = 0,
  endoscopy       = 0,
  fibrosis_onset= 0,
  gerd_event    = 0,
  gerd_event_first= 0,
  heartfail_onset  = 0,     
  hpylori_event   = 0,
  hpylori_event_first= 0,
  ht_event= 0,
  ht_event_first=  0,          
  mi_event= 0,   # Initial risk increase
  mi_event_b= 0, # drop after a week
  mi_event_c= 0, # drop after a month
  mi_event_first    = 0, # ongoing elevation in risk
  oesophagitis_event = 0,
  oesophagitis_event_first = 0, 
  other_athero_onset= 0,
  pancreatitis_event = 0,
  pancreatitis_event_first= 0, 
  peptic_ulcer_event   =0,
  peptic_ulcer_event_first =0,
  pvd_onset        = 0,
  stroke_event= 0,   # Initial risk increase
  stroke_event_b= 0, # drop after a week
  stroke_event_c= 0, # drop after a month
  stroke_event_first = 0, # ongoing elevation in risk
  upper_gi_bleed_event= 0,
  upper_gi_bleed_event_first=0
)



# Medication contributions to hazard
lnhr_ext_presc <- list(
  presc_start_aspirin=0,
  presc_start_clopidogrel=0,
  presc_start_h2ra     =0,
  presc_start_isteroid  =  0, 
  presc_start_nsaid      = 0,
  presc_start_oac         = 0, 
  presc_start_ppi          = 0,
  presc_start_ssteroid_copd = 0,
  presc_start_ssteroid_other = 0,
  presc_stop_aspirin    =  0,
  presc_stop_clopidogrel = 0,
  presc_stop_h2ra     =0,
  presc_stop_isteroid  = 0,
  presc_stop_nsaid      = 0,
  presc_stop_oac        = 0,
  presc_stop_ppi        = 0, 
  presc_stop_ssteroid_copd  = 0,
  presc_stop_ssteroid_other  = 0
)


lnhr_ext_fix <- unlist(lnhr_ext_fix, use.names = TRUE)
lnhr_ext_clin <- unlist(lnhr_ext_clin, use.names = TRUE)
lnhr_ext_comorb <- unlist(lnhr_ext_comorb, use.names = TRUE)
lnhr_ext_presc <- unlist(lnhr_ext_presc, use.names = TRUE)
