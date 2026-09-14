################################################################################
#
#   Script:       P0_b_Init_parameters_comorbidities.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: None
#
#   Data created:	R object: template, coef_names, comorbidity_coef_lists
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



################################
#  Parameters:  template list  #
################################

template <- list(
  cons= 0,
  frailty = c(0, 0, 0, 0),
  mage = rep(0, times=16),
  female = 0,
  fage = rep(0, times=16),
  deprivation = rep(0, times=5),
  eth = rep(0, times=5),
  bmi = 0,
  smoke = rep(0, times=3),
  alc = rep(0, times=4),
  harmalc=0,
  ht_prev=0,
  ht_event=0,
  diabetes_prev=0,
  mi_event=0, 
  mi_prev=0, 
  chd_prev=0, 
  heartfail_prev=0,
  other_athero_prev=0,
  pvd_prev=0,
  stroke_event=0,
  stroke_prev=0, 
  pancreatitis_event=0,
  fibrosis_prev=0, 
  dementia_prev=0, 
  copd_prev=0,
  ckd = rep(0, times=5),
  hpylori_event=0,
  gerd_event=0,
  gerd_prev=0,
  peptic_ulcer_event=0,
  upper_gi_bleed_event=0,
  oesophagitis_event=0, 
  barretts_prev=0, 
  cancer_lung_prev=0,
  cancer_breast_prev=0, 
  cancer_prostate_prev=0,
  cancer_bowel_prev=0, 
  cancer_gastric_prev = 0,
  cancer_oth_prev = 0,
  aspirin=0,
  clopidogrel=0,
  oac=0,
  inhaled_steroid=0, 
  systemic_steroid=0, 
  nsaid=0
)


# Initialise lists to zero
coef_names <- c("ht", "diabetes", "mi", "chd", "heartfail", "athero", 
                "pvd", "stroke", 
                "pancreatitis","fibrosis","copd", "dementia", "ckd", 
                "hpylori", "gerd", "peptic_ulcer", "upper_gi_bleed", 
                "oesophagitis", "barretts", 
                "cancer_lung", "cancer_breast", "cancer_prostate", 
                "cancer_bowel", "cancer_gastric", "cancer_oth",
                "endoscopy",
                "ht_undiag", "aspirin", "clopidogrel", "oac", 
                "isteroid", "ssteroid", "nsaid",
                "ppiind", "ppivsh2ra", "asp_stop"
                )
comorbidity_coef_lists <- setNames(
  replicate(length(coef_names), template, simplify = FALSE),
  paste0(coef_names, "_coefs")
)




#####################################
#  Parameters:  Hypertension model  #
#####################################

# Constant for: male, frail=2, medium deprivation, white, bmi=25, non-smoker, non-drinker 
ht_updates <- list(
  cons= -1.5,
  female = -0.3,
  mage = c(-4.3, -3.4, -3, -2.4, -1.5, -0.8, -0.4,  0,  0.3,  0.6,  0.8,  0.9,  1.1, 0.95,  0.95, 0.9),
  fage = c(-4.3, -3.4, -3, -2.5, -1.6, -1.5 ,-0.9,  0,  0.3,  0.7,  1.0,  1.1,  1.1, 1.2,  1.2,  1.2),
  frailty = c(-0.5, 0, 0.3, 0.7),
  deprivation = c(0.06, -0.04,  0,  0.04,  0.09),
  eth = c(0, 0.4, 1.0, 0.5, 0.3),
  bmi = 0.02,
  smoke = c(0,-0.14, 0.14),
  alc = c(0, 0.005, 0.15, 0.30)
)

comorbidity_coef_lists$ht_coefs[names(ht_updates)] <- ht_updates




#################################
#  Parameters:  Diabetes model  #
#################################


diabetes_updates <- list(
  cons= -2.3,
  female=-0.7,
  mage = c(-1.8, -1.7, -0.6, -1.3, -0.7, -0.4, -0.2, 0, 0.1, 0.3, 0.4,  0.3,  0.5, 0.2, 0.2, 0.2),
  fage = c(-1.3, -1.2, -0.2, -0.7, -0.7, -0.5, -0.3, 0, 0.2, 0.4, 0.5,  0.6,  0.7, 0.5, 0.5, 0.5),
  frailty = c(-0.5, 0, 0.3, 0.7),
  deprivation=c(-0.20, -0.12,  0,  0.05,  0.15),
  bmi=0.03,
  smoke=c(0, 0.15, 0.30),
  alc=c(0, -0.1, -0.5, -0.3),
  eth = c(0, 0.7, 0.5, -0.2, -0.1),
  ht_event=0.9
)

comorbidity_coef_lists$diabetes_coefs[names(diabetes_updates)] <- diabetes_updates





########################################
#  Parameters:  Myocardial infarction  #
########################################


mi_updates <- list(
  cons= -4.8,
  female=-0.7,
  mage = c(-2.8, -2.8, -2.78, -2.2, -1.5, -0.8, -0.3, 0, 0.3, 0.7,  1, 1.3, 1.5, 1.7, 1.7, 1.7),
  fage = c(-3.2, -2.6, -2.58, -2, -1.3, -0.8,  -0.3,  0, 0.3, 0.7, 1.1, 1.4, 1.6, 1.9, 2, 2.1),
  frailty = c(-0.5,  0,  0.3,  0.7),
  deprivation=c(-0.11, -0.05, 0, 0.14,0.165),
  bmi=0.01,
  smoke=c(0,0.15, 0.30),
  alc=c(0, -0.1, -0.5, -0.3),
  eth=c(0, 0.7, 0.5, -0.2, -0.1),
  ht_event=0.7,
  diabetes_prev=0.3
)

comorbidity_coef_lists$mi_coefs[names(mi_updates)] <- mi_updates





#########################################
#  Parameters:  Coronary heart disease  #
#########################################

chd_updates <- list(
  cons= -5.1,
  female=-0.9,
  mage = c(-4.6, -4.1, -3.6, -2.4, -1.2, -0.8, -0.4, 0, 0.2, 0.5, 0.7, 0.9, 1.0, 1.0, 1.0, 0.9),
  fage = c(-4.6, -4.3, -3.8, -2.6, -1.4, -0.9, -0.5, 0, 0.2, 0.6, 0.9, 1.2, 1.4, 1.5, 1.6, 1.6),
  frailty = c(-0.5,  0,  0.3,  0.7),
  deprivation=c(-0.18, -0.1, 0, 0.1, 0.2),
  bmi=0.01,
  smoke=c(0,0.3, 0.5),
  alc=c(0, 0.1, 0.15, 0.2),
  eth = c(0, 0.5, 0.5, -0.1, -0.2),
  ht_event=0.5,
  diabetes_prev=0.4,
  mi_prev=0.5
)

comorbidity_coef_lists$chd_coefs[names(chd_updates)] <- chd_updates



################################
#  Parameters:  Heart failure  #
################################

heartfail_updates <- list(
  cons= -5.5,
  female=-0.7,
  mage = c(-2.9,-2.8,-2.5,-2.3,-1.4,-0.7,-0.3,0,0.4,0.8,1.2,1.6,2,2.1,2.1,2.1),
  fage = c(-2.8,-2.9,-2.6,-2.4,-1.5,-0.8,-0.3,0,0.5,1.1,1.7,2.2,2.7,2.9,3,3.1),
  frailty = c(-0.5, 0, 0.3, 0.7),
  deprivation=c(-0.18, -0.1, 0, 0.1, 0.2),
  bmi=0.03,
  smoke=c(0, 0.3, 0.5),
  alc=c(0, 0.1, 0.15, 0.2),
  eth = c(0, 0.2, 0.2, -0.1, -0.1),
  ht_event=0.1,
  diabetes_prev=0.4,
  mi_prev=0.4,
  chd_prev=1.6
)

comorbidity_coef_lists$heartfail_coefs[names(heartfail_updates)]  <- heartfail_updates




########################################
#  Parameters:  Other atherosclerosis  #
########################################


athero_updates <- list(
  cons= -9.7,
  female=-0.7,
  mage = c( -2,-2,-2,-2,-1.75,-1.5,-0.5,0,0.2,0.3,0.5,0.6,0.6,0.6,0.3,0.1 ),
  fage = c( -2,-2,-2,-2,-1.75,-1.5,-0.5,0,0.2,0.3,0.5,0.6,0.6,0.6,0.3,0.1 ),
  frailty = c(-0.5,  0.0,  0.3,  0.7),
  bmi=-0.01,
  smoke=c(0, 1.0, 0.7),
  alc=c(0, 0.1, 0.15, 0.2),
  eth = c(0, 0.2, 0.2, -0.1, -0.1),
  ht_event=0.6,
  diabetes_prev=0.1,
  mi_prev=0.1,
  chd_prev=1.2,
  heartfail_prev=0.2
)

comorbidity_coef_lists$athero_coefs[names(athero_updates)] <- athero_updates





##############################################
#  Parameters:  Peripheral vascular disease  #
##############################################

pvd_updates <- list(
  cons= -5.25,
  mage = c(-4,-3.2,-2.8,-2,-1.8,-1.2,-0.5,0,0.6,0.9,1.2,1.3,1.4,1.5,1.5,1),
  fage = c(-4,-3.2,-2.8,-2,-1.8,-1.2,-0.5,0,0.6,0.9,1.2,1.3,1.4,1.5,1.5,1),
  frailty = c(-0.5,  0.0,  0.3,  0.7),
  deprivation=c(-0.2, -0.1, 0, 0.1, 0.2),
  bmi=-0.03,
  smoke=c(0,1.3, 1.3),
  alc=c(0, 0.05, 0.75, 0.1),
  eth = c(0, -0.2, 0.4, -0.1, -0.1),
  ht_event=0.5,
  diabetes_prev=0.6,
  mi_prev=0.5,
  chd_prev=1.0,
  heartfail_prev=0.3,
  other_athero_prev=1.9
)

comorbidity_coef_lists$pvd_coefs[names(pvd_updates)] <- pvd_updates



#############################
#  Parameters:  Stroke/TIA  #
#############################


stroke_updates <- list(
  cons= -6.5,
  female=-1.5,
  mage = c(-10.3,-8.3,-6.3,-4.3,-1.3,1.5,-0.3,0,0.4,0.7,1,1.5,2,2.2,2.7,3.2 ),
  fage = c(-10.3,-8.3,-6.3,-4.3,-1.3,1.5,-0.3,0,0.43,1,1.35,1.9,2.4,2.6,3.1,3.6 ),
  frailty = c(-0.5,  0.0,  0.3,  0.7),
  deprivation=c(-0.4, -0.2, 0, 0.2, 0.4),
  bmi=-0.003,
  smoke=c(0,0.4, 0.2),
  alc=c(0, 0.05, 0.2, 0.4),
  eth = c(0, 0.6, 1.0, -0.1, -0.1),
  ht_event=0.7,
  diabetes_prev=0.3,
  mi_prev=0.2,
  chd_prev=0.4,
  heartfail_prev=0.3,
  other_athero_prev=0.4,
  pvd_prev=0.5
)

comorbidity_coef_lists$stroke_coefs[names(stroke_updates)] <- stroke_updates






###############################
#  Parameters:  Pancreatitis  #
###############################


pancreatitis_updates <- list(
  cons=-4.5,
  frailty= c(-0.2, 0,  0.1, 0.6),
  bmi=0.01,
  deprivation=c(-0.075, -0.025, 0, 0.175, 0.425),
  smoke=c(0,0.3,0.1),
  alc=c(0, -0.2, 0.4, 1.0),
  ht_prev=0.2,
  diabetes_prev=0.45,
  mi_prev=-0.1,
  chd_prev=0.1,
  heartfail_prev=-0.1,
  other_athero_prev=0.3,
  pvd_prev=0.2
)

comorbidity_coef_lists$pancreatitis_coefs[names(pancreatitis_updates)] <- pancreatitis_updates



#################################
#  Parameters:  Liver fibrosis  #
#################################


fibrosis_updates <- list(
  cons= -4.5,
  female=-0.8,
  mage=c(-2,-2,-2.7,-1.7,-1.2,-0.5,-0.1,0,0.3,0.1,-0.1,-0.3,-0.4,0.1,0.3,1.4),
  fage=c(-2,-2,-2.7,-1.7,-1.2,-0.5,-0.1,0,0.3,0.1,-0.1,-0.3,-0.4,0.1,0.3,1.4),
  frailty= c(-0.2, 0, 0.2, 0.6),
  bmi=-0.02,
  deprivation=c(-0.10, -0.05, 0, 0.1, 0.2),
  smoke=c(0,0.2,0.2),
  alc=c(0, -0.05, 0.2, 0.8),
  harmalc=0.5,
  ht_prev=0.2,
  diabetes_prev=1.1,
  mi_prev=0.3,
  chd_prev=0.2,
  heartfail_prev=0.5,
  other_athero_prev=0.8,
  pancreatitis_event=0.8
)

comorbidity_coef_lists$fibrosis_coefs[names(fibrosis_updates)] <- fibrosis_updates
  
     



#######################
#  Parameters:  COPD  #
#######################


copd_updates <- list(
  cons= -5,
  female=0.6,
  mage =c(-4.9,-4.7,-3.9,-3.1,-2.2,-1.4,-0.6,0,0.5,0.9,1.2,1.3,1.4,1.3,1.1,1.1),
  fage=c(-4.3,-4.5,-3.8,-3,  -2.1,-1.3,-0.5,0,0.4,0.8,1,  1.1,1.2,1,  0.7,0.3),
  frailty= c(-0.5, 0.0, 0.3, 0.7),
  bmi=-0.02,
  deprivation=c(-0.4, -0.2, 0, 0.3, 0.5),
  smoke=c(0,2,1.8),
  alc=c(0, 0,0,0),
  harmalc=0.3,
  ht_prev=-0.1,
  diabetes_prev=-0.2,
  chd_prev=0.03,
  heartfail_prev=0.8,
  other_athero_prev=-0.2,
  pvd_prev=0.4,
  pancreatitis_event=0.8
)

comorbidity_coef_lists$copd_coefs[names(copd_updates)] <- copd_updates

   



###########################
#  Parameters:  Dementia  #
###########################


dementia_updates <- list(
  cons= -11,
  female=0,
  mage=c(-3.1,-3.1,-3.1,-3.1,-3.1,-1.5,-0.7,0,0.9,1.5,2.3,3.1,3.5,3.9,4.1,4.2),
  fage=c(-3.3,-3.3,-3.3,-3.3,-3.3,-1.5,-0.7,0,0.8,1.4,2.2,3.1,3.8,4.2,4.6,4.7),
  frailty= c(-0.5, 0.0, 0.3, 0.7),
  bmi=-0.06,
  smoke=c(0,-0.2,0),
  alc=c(0, 0,0.2,0.4),
  ht_prev=-0.2,
  diabetes_prev=-0.2,
  mi_prev=0.6,
  chd_prev=-0.3,
  heartfail_prev=-0.2,
  other_athero_prev=0.1,
  pvd_prev=-0.3,
  stroke_event=0.3,
  stroke_prev=0.6,
  copd_prev=0.6,
  pancreatitis_event=0.8
)

comorbidity_coef_lists$dementia_coefs[names(dementia_updates)] <- dementia_updates



#########################################
#  Parameters:  Chronic kidney disease  #
#########################################


ckd_updates <- list(
  cons=-2.75,
  mage=c(-2.4,-2.1,-2,-1.7,-1.4,-0.9,-0.5,0,0.5,1,1.7,2.2,2.7,3.3,3.7,4.3),
  fage=c(-2.4,-2.1,-2,-1.7,-1.4,-0.9,-0.5,0,0.5,1,1.7,2.2,2.7,3.3,3.7,4.3),
  frailty=c(-0.6, 0, 0.3, 0.7),
  deprivation=c(-0.02, -0.02, 0, 0.01, 0.03),
  bmi=0.02,
  smoke=c(0,0.3,0.35),
  eth=c(0,0.9,1.2,-0.2,0),
  alc=c(0, 0.1, 0.2, 0.3),
  ht_prev=0.6,
  ht_event=0.3,
  diabetes_prev=-0.6,
  mi_prev=0.1,
  chd_prev=-0.2,
  heartfail_prev=0.8,
  pvd_prev=0.4,
  fibrosis_prev=0.5,
  dementia_prev=-0.3
)

comorbidity_coef_lists$ckd_coefs[names(ckd_updates)] <- ckd_updates
 	 




###########################
#  Parameters:  H.Pylori  #
###########################


hpylori_updates <- list(
  cons= -3,
  deprivation=c(-0.2 , 0.1, 0, 0.2, 0.3),
  bmi=-0.02,
  smoke=c(0,-0.3,-0.2),
  alc=c(0,-0.05,-0.08,0.1),
  diabetes_prev=0.12,
  mi_event=-0.2,
  heartfail_prev=-0.2
)

comorbidity_coef_lists$hpylori_coefs[names(hpylori_updates)] <- hpylori_updates





############################
#  Parameters:  GERD/GORD  #
############################


gerd_updates <- list(
  cons= -3,
  frailty=c(-0.2, 0, 0.1, 0.4),
  deprivation=c(-0.2, -0.1, 0,  0.2, 0.3),
  bmi=-0.02,
  smoke=c(0,-0.3,-0.2),
  alc=c(0,-0.05,-0.08,0.1),
  diabetes_prev=0.12,
  mi_prev=-0.2,
  heartfail_prev=-0.2,
  hpylori_event=-0.2
)

comorbidity_coef_lists$gerd_coefs[names(gerd_updates)] <- gerd_updates

  
         

###############################
#  Parameters:  Peptic ulcer  #
###############################


peptic_ulcer_updates <- list(
  cons= -5,
  female = -2,
  mage=c(-1.2,-1.2,-1.2,-1,   -0.8,-0.6,-0.4,0,0.2,0.4,0.6,0.6,0.5,0.4,0.1,0.1),
  fage=c(-1.6,-1.2,-1.1,-0.75,-0.4,-0.6,-0.4,0,0.4,0.7,0.8,0.9,0.8,0.9,0.4,0.15),
  frailty=c(-0.1, 0, 0.1, 0.3),
  deprivation=c(-0.15, -0.1, 0, 0.15, 0.254),
  eth=c(0,0.4,0.8,-0.02,0),
  bmi=-0.01,
  smoke=c(0,0.6,0.4),
  alc=c(0,0.2,0.3,0.4),
  ht_event=0.15,
  diabetes_prev=0.05,
  heartfail_prev=0.3,
  pvd_prev=0.3,
  pancreatitis_event=0.5,
  fibrosis_prev=1.5,
  dementia_prev=-0.4,
  ckd = c(0, 0.1, 0.2, 0.6, 1.0),
  hpylori_event=2
)

comorbidity_coef_lists$peptic_ulcer_coefs[names(peptic_ulcer_updates)] <- peptic_ulcer_updates






#################################
#  Parameters:  Upper GI bleed  #
#################################


upper_gi_bleed_updates <- list(
  cons= -6,
  mage=c(-0.3,-0.7,-1.1,-0.7,-0.6,-0.5,-0.3,0,0.1,0.2,0.4,0.5,0.6,0.9,1,1.1),
  fage=c(-0.3,-0.7,-1.1,-0.7,-0.6,-0.5,-0.3,0,0.1,0.2,0.4,0.5,0.6,0.9,1,1.1),  
  frailty=c(-0.1, 0, 0.05, 0.1),
  deprivation=c(-0.4, -0.3, 0.0, 0.0, -0.2),
  smoke=c(0,0.2,0.3),
  alc=c(0,0.01,0.1,0.2),
  eth=c(0,0.8,1.4,-0.4,-0.2),
  ht_event=0.7,
  mi_event=0.8,
  mi_prev=0.5,
  chd_prev=-0.14,
  diabetes_prev=0.05,
  heartfail_prev=0.05,
  pvd_prev=0.1,
  other_athero_prev=1.0,
  stroke_event=0.2,
  stroke_prev=0.1,
  fibrosis_prev=1.8,
  dementia_prev=-0.6,
  ckd = c(0, 0.1, 0.2, 0.6, 1.0),
  peptic_ulcer_event=4.5
)

comorbidity_coef_lists$upper_gi_bleed_coefs[names(upper_gi_bleed_updates)] <- upper_gi_bleed_updates





###############################
#  Parameters:  Oesophagitis  #
###############################


oesophagitis_updates <- list(
  cons= -6,
  female=-1.5,
  mage=c(-1.3,-1.3,-1.3,-1.26,-1.22,-1.1,0,0,0,0,0,0.1,0.2,-1.4,-1.8,-1.5),
  fage=c(-1.3,-1.2,-1.2,-1.16,-1.12,-1,0.2,0,0,0,0,0.2,0.3,-1.2,-1.5,-0.5),
  frailty=c(-0.1, 0, 0.05, 0.1),
  deprivation=c(0.16, 0.02, 0.00, -0.02, -0.04),
  bmi=-0.007,
  alc=c(0,0.0,0.0,0.2),
  ht_event=-0.02,
  mi_prev=0.1,
  chd_prev=0.1,
  diabetes_prev=0.03,
  heartfail_prev=0.06,
  pvd_prev=0.1,
  other_athero_prev=-0.6,
  fibrosis_prev=1.0,
  dementia_prev=0.2,
  ckd = c(0, 0.03, 0.05, 0.3, 0.4),
  hpylori_event=0.4,
  gerd_event=5.0,
  gerd_prev=7.0,
  peptic_ulcer_event=1.3,
  upper_gi_bleed_event=0.6
)

comorbidity_coef_lists$oesophagitis_coefs[names(oesophagitis_updates)] <- oesophagitis_updates

     
       




#######################################
#  Parameters:  Barrett's oesophagus  #
#######################################


barretts_updates <- list(
  cons= -6.5,
  mage=c(-3.1,-2.7,-2.2,-1.8,-1.2,-0.6,-0.4,0,0.2,0.3,0.6,0.9,1.1,0.9,0.8,0.2),
  fage=c(-3.1,-2.7,-2.2,-1.8,-1.2,-0.6,-0.4,0,0.2,0.3,0.6,0.9,1.1,0.9,0.8,0.2),
  frailty=c(-0.1, 0, 0.05, 0.1),
  bmi=-0.01,
  smoke=c(0,0.2,0.3),
  alc=c(0,0.05,0.1,0.2),
  ht_prev=0.14,
  chd_prev=-0.4,
  pancreatitis_event=1.5, 
  fibrosis_prev=2.6,
  dementia_prev=0.3,
  copd_prev=0.2,
  ckd = c(0, 0.1,0.3,0.5,0.6),
  hpylori_event=1.2,
  peptic_ulcer_event=1.5,
  upper_gi_bleed_event=1.1,
  oesophagitis_event= -1.75
)

comorbidity_coef_lists$barretts_coefs[names(barretts_updates)] <- barretts_updates




#########################
#  Parameters:  Cancer  #
#########################


# Generic cancer risk
cancer_updates <- list(
  female=-0.25,
  mage=c(-2.6,-2.4,-2,-1.7,-1.3,-0.9,-0.4,0,0.4,0.8,0.4,0,-0.4,-0.5,-0.6,-0.6),
  fage=c(-2.6,-2.4,-2,-1.7,-1.3,-0.9,-0.4,0,0.4,0.8,0.4,0,-0.4,-0.5,-0.6,-0.6),
  frailty=c(-0.1, 0, 0.08, 0.2),
  bmi=-0.012,
  smoke=c(0,0.07,0.06),
  alc=c(0,0.1,0.2,0.5),
  eth=c(0,-0.3,-0.2,-0.2,-0.3),
  ht_prev=0.02,
  chd_prev=-0.1,
  heartfail_prev=-0.1,
  pvd_prev=0.07,
  pancreatitis_event=0.25, 
  dementia_prev=-0.2,
  copd_prev=-0.1
)

# Lung 
cancer_lung_updates <- list(
  cons= -6.25,
  deprivation=c(-0.5, -0.2, 0, 0.1, 0.3),
  bmi=-0.04,
  smoke=c(0,0.5,0.7),  
  copd_prev=1.75
)
# Prostate - more older diagnoses
cancer_prostate_updates <- list(
  cons=-3.3,
  mage=c(-2.6,-2.4,-2,-1.7,-1.3,-0.9,-0.4,0,0.4,0.8,0.8,0.4,0.4,0.4,0.4,0.4),
  fage=c(-2.6,-2.4,-2,-1.7,-1.3,-0.9,-0.4,0,0.4,0.8,0.8,0.4,0.4,0.4,0.4,0.4),
  eth=c(0,-0.2,1.0,0.3,0),
  female=-9
)
# Breast
cancer_breast_updates <- list(
  cons = -13,
  female = 9
)
# Bowel - more older diagnoses
cancer_bowel_updates <- list(
  cons=-4.25,
  mage=c(-2.6,-2.4,-2,-1.7,-1.3,-0.9,-0.4,0,0.4,0.8,0.8,0.4,0.4,0.4,0.4,0.4),
  fage=c(-2.6,-2.4,-2,-1.7,-1.3,-0.9,-0.4,0,0.4,0.8,0.8,0.4,0.4,0.4,0.4,0.4)
)
# Gastric - more deprivation, BMI, smoking and CKD influence
cancer_gastric_updates <- list(
  cons= -6.2,
  deprivation=c(-0.5, -0.2, 0, 0.1, 0.3),
  bmi=0.04,
  smoke=c(0,0.2,0.3),  
  ckd=c(0,0.3,0.3,-0.4,-0.2),
  hpylori_event=0.5,
  peptic_ulcer_event=1.75,
  oesophagitis_event=0.3
)
cancer_oth_updates <- list(
  cons=-11
)
comorbidity_coef_lists$cancer_lung_coefs[names(cancer_updates)] <- cancer_updates
comorbidity_coef_lists$cancer_breast_coefs[names(cancer_updates)] <- cancer_updates
comorbidity_coef_lists$cancer_prostate_coefs[names(cancer_updates)] <- cancer_updates
comorbidity_coef_lists$cancer_bowel_coefs[names(cancer_updates)] <- cancer_updates
comorbidity_coef_lists$cancer_gastric_coefs[names(cancer_updates)] <- cancer_updates
comorbidity_coef_lists$cancer_oth_coefs[names(cancer_updates)] <- cancer_updates

comorbidity_coef_lists$cancer_lung_coefs[names(cancer_lung_updates)] <- cancer_lung_updates
comorbidity_coef_lists$cancer_breast_coefs[names(cancer_breast_updates)] <- cancer_breast_updates
comorbidity_coef_lists$cancer_prostate_coefs[names(cancer_prostate_updates)] <- cancer_prostate_updates
comorbidity_coef_lists$cancer_bowel_coefs[names(cancer_bowel_updates)] <- cancer_bowel_updates
comorbidity_coef_lists$cancer_gastric_coefs[names(cancer_gastric_updates)] <- cancer_gastric_updates
comorbidity_coef_lists$cancer_oth_coefs[names(cancer_oth_updates)] <- cancer_oth_updates




############################
#  Parameters:  Endoscopy  #
############################



endoscopy_updates <- list(
  cons= -5.7,
  female=-0.25,
  mage=c( -1.4,-1.4,-1.2,-0.75,-0.65,-0.4,-0.1,0,0.1,0.2,0.3,0.4,0.4,0.5,0.4,0.2),
  fage=c( -1.4,-1.4,-1.2,-0.75,-0.65,-0.4,-0.1,0,0.1,0.2,0.3,0.4,0.4,0.5,0.4,0.2),
  frailty=c(-0.1, 0, 0.1, 0.3),
  bmi=-0.02,
  alc=c(0,0.1,0.2,0.4),
  ht_prev=-0.02,
  diabetes_prev=0.09,
  mi_prev=0.1,
  chd_prev=0.1,
  heartfail_prev=0.3,
  other_athero_prev=0.4,
  pvd_prev=0.2,
  pancreatitis_event=0.8, 
  fibrosis_prev=1.0,
  dementia_prev=0.3,
  copd_prev=-0.1,
  ckd = c(0, 0.03,0.05,0.3,0.4),
  hpylori_event=1.5,
  gerd_event=0.25,
  peptic_ulcer_event=2,
  upper_gi_bleed_event=1.1,
  oesophagitis_event= 1.3,
  barretts_prev=1.9,
  cancer_lung_prev=0.1,
  cancer_breast_prev=0.1,
  cancer_prostate_prev=0.2,
  cancer_bowel_prev=2.0,
  cancer_oth_prev=0.1,
  cancer_gastric_prev=2.0
)

comorbidity_coef_lists$endoscopy_coefs[names(endoscopy_updates)] <- endoscopy_updates

 




##########################
#  Parameters:  Aspirin  #
##########################

aspirin_updates <- list(
  cons= -8.5,
  female=2.3,
  mage = c(-2.8,-2.6,-2.3,-1.5,-1,  -0.6,-0.3,0,0.6,0.9,1.2,1.3,1.5,1.7,1.8,2.5),
  fage = c(-2.1,-1.9,-1.7,-1.2,-0.9,-0.6,-0.3,0,0.5,0.8,1.1,1.2,1.4,1.5,1.6,2.3),
  bmi=0.016,
  eth=c(0, -0.05, -0.05, -0.05, -0.05),
  smoke = c(0, 0.3, 0.4),
  alc=c(0, -0.1, -0.2, -0.6),
  ht_event=1,
  diabetes_prev=1.5
)

comorbidity_coef_lists$aspirin_coefs[names(aspirin_updates)] <- aspirin_updates





##############################
#  Parameters:  Clopidogrel  #
##############################

clopidogrel_updates <- list(
  cons= -8.5,
  female = 2,
  mage = c(-2.5,-1.8,-1.5,-0.5,-0.4,-0.3,-0.2,0,0.2,0.4,0.5,0.6,0.8,1,0.5,0.4),
  fage = c(-5,  -3.6,-3,  -1,  -0.8,-0.6,-0.4,0,0.4,0.8,1,1.2,1.6,2,1,0.8),
  bmi=0.016,
  smoke=c(0,0.3,0.4),
  alc=c(0, -0.1, -0.15, -0.3),
  ht_event=0.05,
  ht_prev=0.05,
  diabetes_prev=0.15,
  mi_prev=1.3,
  chd_prev=2.5,
  heartfail_prev=0.3,
  other_athero_prev=0.1,
  pvd_prev=0.2
)

comorbidity_coef_lists$clopidogrel_coefs[names(clopidogrel_updates)] <- clopidogrel_updates




######################################
#  Parameters:  Oral anticoagulants  #
######################################

oac_updates <- list(
  cons= -10.5,
  female=-0.3,
  mage = c(-2,-2,-0.7,-1.6,-1.1,-0.8,-0.2,0,0.4,0.7,1,1.1,1.3,1.5,1,0.9),
  fage = c(-2,-2,-0.7,-1.6,-1.1,-0.8,-0.2,0,0.4,0.7,1,1.1,1.3,1.5,1,0.9),
  bmi=0.03,
  deprivation=c(0.03, 0.01, 0, -0.01, -0.02),
  smoke=c(0,-0.2,-0.3),
  alc=c(0, -0.0, -0.05, -0.05),
  ht_event=0.1,
  ht_prev=0.25,
  diabetes_prev=0.2,
  mi_prev=1.2,
  chd_prev=1.2,
  heartfail_prev=1.8,
  other_athero_prev=0.3,
  pvd_prev=0.4
)

comorbidity_coef_lists$oac_coefs[names(oac_updates)] <- oac_updates





###################################
#  Parameters:  Inhaled steroids  #
###################################


isteroid_updates <- list(
  cons= -3.5,
  mage=c(-0.3,-0.2,-0.1,-0.1,0,0,0,0,-0.1,-0.1,-0.2,-0.3,-0.3,-0.3,-0.4,-0.8),
  fage=c(-0.3,-0.2,-0.1,-0.1,0,0,0,0,-0.1,-0.1,-0.2,-0.3,-0.3,-0.3,-0.4,-0.8),
  deprivation=c(0.05, 0, 0, 0.02, 0.15),
  bmi=0.02,
  smoke=c(0,0.2,-0.2),
  alc=c(0, 0, 0.2, 0.4),
  ht_prev=0.1,
  diabetes_prev=-0.08,
  mi_prev=-0.1,
  chd_prev=-0.1,
  heartfail_prev=-0.04,
  other_athero_prev=-0.05,
  pvd_prev=-0.1,
  pancreatitis_event=-0.3,
  fibrosis_prev=-0.5,
  dementia_prev=-0.5,
  copd_prev=2.8
)

comorbidity_coef_lists$isteroid_coefs[names(isteroid_updates)] <- isteroid_updates




####################################
#  Parameters:  Systemic steroids  #
####################################


ssteroid_updates <- list(
  cons= -3.75,
  mage=c(-0.6,-0.5,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.3,0.4,0.5,0.6,0.6,0.6,0.4),
  fage=c(-0.6,-0.5,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.3,0.4,0.5,0.6,0.6,0.6,0.4),
  smoke=c(0,0.2,0.3),
  alc=c(0, -0.1, -0.2, -0.2),
  ht_event=0.02,
  diabetes_prev=-0.08,
  mi_prev=0.08,
  chd_prev=-0.08,
  heartfail_prev=0.2,
  other_athero_prev=-0.01,
  pvd_prev=0.06,
  pancreatitis_event=-0.3,
  fibrosis_prev=0.3,
  dementia_prev=-0.3,
  copd_prev=1.2
)

comorbidity_coef_lists$ssteroid_coefs[names(ssteroid_updates)] <- ssteroid_updates





########################
#  Parameters:  NSAID  #
########################


nsaid_updates <- list(
  cons= -2.5,
  female=0.1,
  mage=c( -1.3,-1,-0.9,-0.7,-0.5,-0.4,-0.3,0,0.3,0.4,0.4,0.4,0.3,0.2,0.1,-0.1),
  fage=c(-0.95,-0.75,-0.66,-0.5,-0.3,-0.25,-0.25,0,0.2,0.3,0.3,0.3,0.2,0.1,-1,-0.2),
  frailty=c(-0.1, 0, 0.1, 0.23),
  deprivation=c(-0.07, -0.03, 0, 0.03, 0.08),
  bmi=0.03,
  smoke=c(0,0.2,0.3),
  alc=c(0,0.01,0.02,0.03),
  ht_event=-0.03,
  diabetes_prev=-0.05,
  mi_prev=-0.1,
  mi_event=-0.3,
  chd_prev=-0.2,
  heartfail_prev=-0.3,
  pvd_prev=-0.1,
  fibrosis_prev=-1.5,
  dementia_prev=-0.6,
  copd_prev=-0.3,
  ckd=c(0, 0, -1, 2, 3)
)

comorbidity_coef_lists$nsaid_coefs[names(nsaid_updates)] <- nsaid_updates




######################################
#  Parameters:  PPI/H2RA indication  #
######################################

ppiind_updates <- list(
  cons= -8,
  mage=c(-2.2,-2.1,-1.6,-1.3,-1,-0.7,-0.4,0,0.5,0.8,0.8,1.1,1.3,1.5,2.3,2.8),
  fage=c(-2.2,-2.1,-1.6,-1.3,-1,-0.7,-0.4,0,0.5,0.8,0.8,1.1,1.3,1.5,2.3,2.8),
  deprivation=c(-0.05,-0.02, 0, 0.02, 0.04),
  bmi=0.05,
  smoke=c(0,0.1,0.2),
  alc=c(0,0.05,0.1,0.3),
  diabetes_prev=0.1,
  mi_prev=0.1,
  chd_prev=0.05,
  heartfail_prev=0.05,
  fibrosis_prev=0.3,
  dementia_prev=0.15,
  copd_prev=0.3,
  ckd=c(0,0.02, 0.07, 0.1, 0.15),
  nsaid=0.2,
  upper_gi_bleed_event=0.8,
  peptic_ulcer_event=1.6,
  hpylori_event=1.4,
  gerd_event=1.5,
  gerd_prev=0.8,
  oesophagitis_event=1.3,
  barretts_prev=2.0,
  cancer_lung_prev=0.12,
  cancer_breast_prev=0.03,
  cancer_prostate_prev=0.05,
  cancer_bowel_prev=0.15,
  cancer_gastric_prev=1,
  cancer_oth_prev=0.02
  )

comorbidity_coef_lists$ppiind_coefs[names(ppiind_updates)] <- ppiind_updates



#####################################
#  Parameters:  PPI vs H2RA choice  #
#####################################

ppivsh2ra_updates <- list(
  cons= -3.5,
  mage=c(-0.7,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,0,0,0,0,0.1,0.1,0.1,0.1,-0.7),
  fage=c(-1.05,-0.35,-0.25,-0.15,-0.15,-0.15,-0.1,0,0.1,0.1,0.1,0.15,0.2,0.2,0.25,-0.5),
  deprivation=c(0.03, 0.01, 0, -0.02, -0.047),
  bmi=0.05,
  smoke=c(0,0.05,0.1),
  alc=c(0,0.05,0.1,0.2),
  diabetes_prev=0.07,
  mi_prev=0.13,
  chd_prev=0.04,
  heartfail_prev=0.3,
  other_athero_prev=0.25,
  aspirin=0.1,
  clopidogrel=-0.4,
  oac=-0.08,
  pancreatitis_event=0.25,
  fibrosis_prev=1.25,
  dementia_prev=0.15,
  copd_prev=0.12,
  ckd=c(0,0.02, 0.07, -0.05, -0.7),
  nsaid=0.45,
  upper_gi_bleed_event=0.8,
  peptic_ulcer_event=1.6,
  hpylori_event=1.4,
  gerd_event=1.25,
  gerd_prev=0.8,
  oesophagitis_event=0.6,
  barretts_prev=2.5,
  cancer_lung_prev=0.12,
  cancer_breast_prev=0.03,
  cancer_prostate_prev=0.05,
  cancer_bowel_prev=0.15,
  cancer_gastric_prev=1,
  cancer_oth_prev=0.02,
  inhaled_steroid=0.05,
  systemic_steroid=-0.006
)

comorbidity_coef_lists$ppivsh2ra_coefs[names(ppivsh2ra_updates)] <- ppivsh2ra_updates



    


###################################
#  Parameters:  Stopping aspirin  #
###################################

asp_stop_updates <- list(
  cons= -2.5,
  female = -0.2,
  deprivation=c(-0.02, -0.01, 0, 0.01,0.02),
  smoke=c(0,+0.1,-0.1),
  alc=c(0,-0.05,-0.1,+0.2),
  diabetes_prev=-0.07,
  mi_prev=-0.13,
  chd_prev=-0.04,
  heartfail_prev=-0.3,
  other_athero_prev=-0.25,
  dementia_prev=0.15,
  upper_gi_bleed_event=9,
  peptic_ulcer_event=1,
  hpylori_event=0.2,
  gerd_event=0.2,
  oesophagitis_event=0.6
)

comorbidity_coef_lists$asp_stop_coefs[names(asp_stop_updates)] <- asp_stop_updates
