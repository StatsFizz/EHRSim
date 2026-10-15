################################################################################
#
#   Script:      P0_e_Codes_used.R
#
#   Created by:   Elizabeth Williamson 26/11/2025
#
#   Data used:		None
#
#   Data created:	R objects: codes, codes_freq_used
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script provides the codes used in the generation of the
#                simulated EHR data.
#
################################################################################
#
#   Packages needed: None
#
################################################################################



###########################
#  Set up template lists  #
###########################


codes <- list(
  clinical = list(),
  presc = list(),
  general = list(),
  common = list(),
  measure = list(),
  entity = list()
)

codes_freq_used <- list(
  clinical = list(),
  presc = list(),
  general = list(),
  common = list(),
  measure = list(),
  entity = list()
)


###############################################
#  Provide clinical codes that might be used  #
###############################################

### Barretts ###

codes$clinical$barretts <- c(4614, 99526, 5596)
codes_freq_used$clinical$barretts <- c(0.50, 0.4, 0.1)


### Cancer ###

codes$clinical$cancer_lung <- c(2587,13243, 3903)
codes$clinical$cancer_breast<- c(348, 3968, 9470)
codes$clinical$cancer_prostate<- c(780,780)
codes$clinical$cancer_bowel <- c(1220, 1800, 5901)
codes$clinical$cancer_gastric<- c(8386,32022, 48237) 

codes_freq_used$clinical$cancer_lung <- c(0.40, 0.4, 0.2)
codes_freq_used$clinical$cancer_breast <- c(0.50, 0.3, 0.2)
codes_freq_used$clinical$cancer_prostate <- c(0.5, 0.5)
codes_freq_used$clinical$cancer_bowel <- c(0.4, 0.35, 0.25)
codes_freq_used$clinical$cancer_gastric <- c(0.4, 0.3, 0.3)

code_cancer_bladder <- c(779, 31102) 
code_cancer_skin <- c(865, 876, 3028, 3445, 1624, 93352)
code_cancer_lymphoma <- c(1483, 3604)
code_cancer_pancreas <- c(8166)
code_cancer_blood <- c(4944, 10726)  
code_cancer_adeno <- c(2272)
code_cancer_other <- c(6170)
code_cancer_uterus <- c(2747, 2890)

codes$clinical$cancer_other_m <- c( code_cancer_bladder, 
                          code_cancer_skin, 
                          code_cancer_lymphoma, 
                          code_cancer_pancreas,
                          code_cancer_blood, 
                          code_cancer_adeno, 
                          code_cancer_other)
      
codes$clinical$cancer_other_f <- c(codes$clinical$cancer_other_m, code_cancer_uterus)

rm(code_cancer_bladder, code_cancer_blood, code_cancer_lymphoma,
    code_cancer_other, code_cancer_pancreas, code_cancer_skin, 
   code_cancer_uterus, code_cancer_adeno)


codes_freq_used$clinical$cancer_other_m <- c(0.15, 0.1, 
                      0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                      0.08, 0.07, 
                      0.1, 
                      0.04, 0.04,
                      0.03, 
                      0.09)
codes_freq_used$clinical$cancer_other_f <- c(codes_freq_used$clinical$cancer_other_m*0.98, 0.01, 0.01)


### Coronary heart disease ###

codes$clinical$chd <- c(240, 1430, 1792, 14658, 241, 1677, 737, 1204, 1344,
              2901, 11983, 5703, 7137, 8942, 10562, 11983)
codes_freq_used$clinical$chd <- c(0.24, 0.23, 0.09, 0.08, 0.05, 0.04, 0.04,
                  0.04, 0.03, 0.05, 0.03, 0.02, 0.02, 0.02,
                  0.01, 0.01)


### COPD ###

codes$clinical$copd <- c(11287, 28755, 1001, 10802, 10863, 34202,
               9876, 18501, 34215, 37371, 998, 18621,
               794, 3243)
codes_freq_used$clinical$copd <- c(0.3, 0.15, 0.12, 0.10, 0.07, 0.06,
                   0.05, 0.03, 0.04, 0.02, 0.02, 0.02,
                   0.01, 0.01)


### Dementia ###
codes$clinical$dementia <- c(1350, 1917, 4693, 6578, 1916, 7664, 8195,
                   9509, 4357, 5931, 7572, 31016, 9565,
                   12621, 16797, 19393, 19477, 43346)
codes_freq_used$clinical$dementia <- c(0.27, 0.14, 0.12, 0.12, 0.08, 0.04,
                       0.04, 0.04, 0.03, 0.02, 0.02, 0.02,
                       0.01, 0.01, 0.01, 0.01, 0.01, 0.01)

### Diabetes ###
codes$clinical$diabetes <- c(711, 758, 506, 4513, 1410, 18505,
                  1038, 1549, 1647, 7795)
codes_freq_used$clinical$diabetes <- c(0.53, 0.17, 0.10, 0.06, 0.03,
	                      0.03, 0.02, 0.01, 0.02, 0.01)
   

### Fibrosis ###
codes$clinical$fibrosis <- c(4743, 1638, 5638, 6863, 16725)
codes_freq_used$clinical$fibrosis <- c(0.33, 0.25, 0.25, 0.09, 0.08)

### Hypertension ###
codes$clinical$ht_event <- c(799, 13186, 204, 4444, 5215,
                      10976, 11056, 10961, 4372, 10818, 2666)
codes_freq_used$clinical$ht_event <- c(0.43, 0.23, 0.12, 0.09, 0.04,
                           0.03, 0.03, 0.02, 0.003, 0.002, 0.002)

### Heart failure ###
codes$clinical$heartfail <- c(398, 884, 2906, 2062, 5942,
                    1223, 13189, 94870, 4024, 9913,
                    10154, 12627, 18853, 19066, 32898)
codes_freq_used$clinical$heartfail <- c(0.42, 0.23, 0.09, 0.08, 0.06,
                         0.03, 0.02, 0.01, 0.01, 0.01,
                         0.01, 0.01, 0.01, 0.005, 0.005)

### Atherosclerosis ###
codes$clinical$athero <- c(5168, 5640, 996, 3995, 36609,
                 56621, 100579)
codes_freq_used$clinical$athero <- c(0.8, 0.14, 0.02, 0.01, 0.01,
                      0.01, 0.01)

  
### Pancreatitis ###
codes$clinical$pancreatitis <- c(1113, 12316, 1419)
codes_freq_used$clinical$pancreatitis <- c(0.73, 0.20, 0.07)

  
### PVD ###
codes$clinical$pvd <- c(1517, 2760, 1735, 5943, 1736,
               3530, 6356, 6827, 10827, 1826)
codes_freq_used$clinical$pvd <- c(0.37, 0.21, 0.15, 0.06, 0.05, 0.04, 0.04, 0.035, 0.0225, 0.0225)


### MI ###

codes$clinical$mi_event <- c(14658, 1677, 10562, 241, 1678,
                  12229, 3704, 14897)
codes_freq_used$clinical$mi_event <- c(0.38, 0.25, 0.18, 0.08, 0.05,
                      0.04, 0.01, 0.01)


### Endoscopy ###

codes$clinical$endoscopy <- c(377,1968,4022,6019,11611,
                    1190,951,1877,7231,282)
codes_freq_used$clinical$endoscopy <- c(0.61,0.20,0.07,0.06,0.02,
                        0.01,0.01,0.01,0.01,0.01)


### GERD ###

codes$clinical$gerd_event <- c(7577, 7577)
codes_freq_used$clinical$gerd_event <- c(0.5,0.5)


### Oesophagitis ###

codes$clinical$oesophagitis <- c(2535, 592, 7104, 5283, 10060)
codes_freq_used$clinical$oesophagitis <- c(0.53, 0.36, 0.10, 0.05, 0.005)


### H.Pylori ###

codes$clinical$hpylori <- c(5759,2013,2263,5323,6279,
                     14311,86391,10906,17387,5789,
                     10210,17819,99480)
codes_freq_used$clinical$hpylori <- c(0.28,0.17,0.11,0.10,0.06,
                        0.05,0.06,0.04,0.04,0.03,
                        0.03,0.02,0.02)


### Peptic ulcer ###

codes$clinical$peptic_ulcer <- c(352,1262,670,2877,3876,
                        18001,20677,29317,55933)
codes_freq_used$clinical$peptic_ulcer <- c(0.71,0.13,0.06,0.03,0.02,
                           0.02,0.01,0.01,0.01)



### Stroke ###

codes$clinical$stroke_event <- c(1298, 1469, 5363, 3149, 5602,
                      1786, 8837, 5051, 10504, 6960,
                      5871, 6116, 7912, 9943, 9985,
                      10792)
codes_freq_used$clinical$stroke_event <- c(0.18, 0.17, 0.15, 0.10, 0.08,
                      0.07, 0.03, 0.02, 0.02, 0.02,
                      0.02, 0.01, 0.01, 0.01, 0.01,
                      0.01)

### Upper GI bleed ###

codes$clinical$upper_gi_bleed <- c(1188, 397, 1642, 2873, 3097,
                     6151, 18001, 20859, 4354)
codes_freq_used$clinical$upper_gi_bleed <- c(0.31, 0.21, 0.18, 0.13, 0.06,
                            0.03, 0.03, 0.03, 0.02)


### CKD ###

codes$clinical$ckd3a <- c(5, 5)
codes$clinical$ckd3b <- c(522, 522)
codes$clinical$ckd4 <- c(2998, 2998)
codes$clinical$ckd5 <- c(5458, 5458)

codes_freq_used$clinical$ckd3a <- c(0.5, 0.5)
codes_freq_used$clinical$ckd3b <- c(0.5, 0.5)
codes_freq_used$clinical$ckd4  <- c(0.5, 0.5)
codes_freq_used$clinical$ckd5  <- c(0.5, 0.5)





##################################
#  Codes and relative frequency  # 
##################################


####  PPIs  ####

codes$presc$omeprazole  <- c(18,89,1451,1232,6114,9989,5236,4921)
codes_freq_used$presc$omeprazole <- c(0.76, 0.17, 0.03, 0.02, 0.009, 0.001, 0.0099, 0.0001)

codes$presc$pantoprazole  <- c(1986, 5419, 6165, 6727)
codes_freq_used$presc$pantoprazole <- c(0.58, 0.39, 0.02, 0.01)

codes$presc$lansoprazole  <- c(39,30,6063,519,6245,6300,1947,5358,6291,5662)
codes_freq_used$presc$lansoprazole <- c(0.52,0.45, 0.01, 0.002, 0.005, 
                                        0.003, 0.003, 0.003, 0.003, 0.001)

codes$presc$esomeprazole  <- c(5604, 5178, 43948, 43995)
codes_freq_used$presc$esomeprazole <- c(0.56,0.34,0.05, 0.05)

codes$presc$rabeprazole  <- c(2151, 1977, 5834)
codes_freq_used$presc$rabeprazole <- c(0.56, 0.42, 0.02)


####  H2RAs  ####

codes$presc$cimetidine  <- c(68,446,7848,3857,1230,2057)
codes_freq_used$presc$cimetidine <- c(0.79,0.11, 0.07, 0.01, 0.01, 0.01)

codes$presc$ranitidine  <- c(21,1556,1229,7666,1963)
codes_freq_used$presc$ranitidine <- c(0.77,0.18, 0.02, 0.02, 0.01)

codes$presc$famotidine  <- c(8389,10555,10746,8388,690)
codes_freq_used$presc$famotidine <- c(0.83,0.15, 0.01, 0.009, 0.001)

codes$presc$nizatidine  <- c(690)
codes_freq_used$presc$nizatidine <- c(1)



####  Aspirin  ####

codes$presc$aspirin  <- c(3, 34, 16, 434, 377, 5288, 6007, 7518, 1137, 
                    1049, 3155, 254, 10310, 3386, 6666, 11326)
codes_freq_used$presc$aspirin <- c(0.53, 0.33, 0.1, 0.01, 0.01, 0.006,
                             0.003, 0.003, 0.002, 0.002, 0.001,
                             0.001, 0.001, 0.0003, 0.0002, 0.0005)

####  OAC   ####


codes$presc$oac  <- c(45, 61, 1781, 6262, 54066, 5305)
codes_freq_used$presc$oac<- c(0.34, 0.33, 0.19, 0.12, 0.01, 0.01)



####  Clopidogrel  ####


codes$presc$clopidogrel  <- c(489, 836)
codes_freq_used$presc$clopidogrel <- c(0.995, 0.005)


####  NSAID  ####


codes$presc$nsaid  <- c(15, 40, 3053, 5254, 3431, 661, 474, 2243, 807, 
                  1692, 1469, 580, 447, 650, 177, 649, 
                  1115, 6464, 5080, 2387, 416, 3043, 
                  1116, 1073, 6498, 3416, 1755, 4880,
                  5812, 5938, 1086, 736, 3972, 4806, 259)
codes_freq_used$presc$nsaid<- c(0.17,0.17, 0.06, 0.07, 0.05, 0.04, 0.05,
                                0.04, 0.02, 0.03, 0.03, 0.01, 0.02, 0.02,
                                0.01, 0.01, 0.02, 0.01, 0.01, 0.01, 0.01,
                                0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
                                0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)

####  Inhaled steroids  ####


codes$presc$isteroid  <- c(38, 5172, 99, 5161, 13290, 6325, 3666, 1236, 
                     908, 1259, 665, 956, 454, 1537, 5683,
                     16151, 1258, 1242, 5143, 16158, 911,
                     2335, 11618)
codes_freq_used$presc$isteroid<- c(0.1375, 0.1125, 0.1, 0.0875, 0.0625,
                                   0.05, 0.05, 0.05, 0.0375, 0.0375, 0.0375,
                                   0.0375, 0.0375, 0.0125, 0.025, 0.0125,
                                   0.0375, 0.0125, 0.0125, 0.0125, 0.0125,
                                   0.0125, 0.0125)



####  Systemic steroids  ####


codes$presc$ssteroid  <- c(44, 557, 95, 578, 955, 2704, 2949, 3557,
                     63066, 5913, 5490)
codes_freq_used$presc$ssteroid<- c(0.35,0.25, 0.20, 0.03, 0.04, 0.03, 0.02,
                                   0.03, 0.01, 0.02, 0.02)






####################
#  General codes   #
####################

codes$general$admin <- 1649
codes$general$diet <- 47
codes$general$health_ed <- 29
codes$general$surgery <-56
codes$general$counselling <- 278
codes$general$advice <- 461 
codes$general$review <- 26
codes$general$medication_review <- 11955

codes$general$letter_to_patient <- 7091
codes$general$phone <- 16 
codes$general$notes_made <- 34  
codes$general$patient_anxiety <-636
codes$general$communication <-9246

codes$common$resp_tract_infect <- 293
codes$common$ear_infect1 <- 267
codes$common$ear_infect2 <- 630
codes$common$acute_sinusitis <- 980
codes$common$acute_pharyngitis <- 407



#############################
#  Codes for measurements   #
#############################


codes$measure$bmi <- 1
codes$measure$weight <- 2
codes$measure$height <- 3
codes$measure$smoking <- 4
codes$measure$tobacco <- 54 
codes$measure$alcohol <- 5   # NB: made this one up 

# Codes by smoking status
codes$measure$smoke_no <-  c(33, 60, 11788, 98177, 101878)
codes$measure$smoke_ex <-  c(90, 776, 10898, 12878, 12946,
                           12955, 12956, 12957, 12959, 100963)
codes$measure$smoke_cr <-  c(93,2111,1823,12943,1878,
                           3568,10558,12941)

# Codes by drinking status
codes$measure$alc_no <-  c(27, 4447, 12949, 12970, 967)
codes$measure$alc_ex <-  c(19495,19493,385)
codes$measure$alc_cr <-  c(649, 956, 322, 8999, 99877, 12982) 
codes$measure$alc_hm <-  c(102448, 12976, 12982, 94670, 1399, 30460)

# Synthetic codes by BMI category
codes$measure$bmi_under <- c(1626, 2298, 2300)
codes$measure$bmi_normal <- c(2310, 2299, 2301)
codes$measure$bmi_over <- c(1625, 2302, 2303)
codes$measure$bmi_obese1 <- c(1627, 2304, 2305)
codes$measure$bmi_obese2 <- c(1628, 2306, 2307)
codes$measure$bmi_obese3 <- c(1629, 2308, 2309)


###############
#  Entities (meaning/category of that record)   #
###############

codes$entity <- list()

codes$entity$weight<-1 
codes$entity$height<-2 
codes$entity$smoke <-3
codes$entity$alcohol<-4
codes$entity$bp<-5
codes$entity$pulse <-6
codes$entity$med_hist<-7
codes$entity$fam_hist <-8
codes$entity$advice<-9
codes$entity$diet<-10
codes$entity$exercise<-11
codes$entity$hp_smoking<-12
codes$entity$hp_alcohol<-13
codes$entity$hp_hypertension<-14
codes$entity$hp_chd<-15
codes$entity$hp_stroke<-16
codes$entity$hp_diabetes<-17
codes$entity$status_diabetes<-18
codes$entity$status_copd<-19
codes$entity$status_asthma<-20
codes$entity$consult_diabetes<-21
codes$entity$consult_asthma<-22
codes$entity$consult_copd <-23
codes$entity$consult_cardio<-24
codes$entity$consult_kidney<-25
codes$entity$contraception<-26
codes$entity$hrt<-27
codes$entity$med_review<-28
codes$entity$test_result<-29
codes$entity$register_stroke<-30
codes$entity$register_chd<-31
codes$entity$register_diabetes<-32
codes$entity$register_cancer<-33
codes$entity$communication <- 34
codes$entity$admin <- 35
codes$entity$bmi <- 36

