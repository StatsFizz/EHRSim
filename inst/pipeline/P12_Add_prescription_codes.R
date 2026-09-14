################################################################################
#
#   Script:      P12_Add_prescription_codes.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		medication 
#
#   Data created:	R object: prescription
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script adds "realistic" medication codes to the 
#               simulated prescription data.
#
################################################################################


#############################################
#  Program to allocate prescription codes   # 
#############################################


allocate_med_code <- function(data, drugvarname, datevar, druggp, codes, freq, psame) {
  
  # Order prescription data by patient, drug and prescription date
  DT <- copy(as.data.table(data))
  setorderv(DT, c("patient_id", drugvarname, datevar))
  
  # Generate uniform random variables to assign codes probabilistically
  DT[get(drugvarname) == druggp, `:=`(u1  = runif(.N), u2 = runif(.N))]
  
  # Cumulative probabilities of different possible drug codes
  freq <- freq / sum(freq)
  cumfreq <- cumsum(freq)
  
  # Randomly select a prescription code
  DT[get(drugvarname) == druggp, 
     code_prescription := codes[findInterval(u1, cumfreq) + 1L]  ]
  
  # Prefer previous final selected code (for that patient) with probability psame
  carry_previous_code <- function(code, u) {
    for (i in seq_along(code)[-1]) {
      if (isTRUE(u[i] < psame) && !is.na(code[i - 1])) {
        code[i] <- code[i - 1]
      }
    }
    code
  }
  DT[get(drugvarname) == druggp,
    code_prescription := carry_previous_code(code_prescription, u2),
    by = c("patient_id", drugvarname)]
  
  DT[, code_prescription]
}





####################
#  Generate codes  # 
####################


prescription <- copy(medication)
setorder(prescription, patient_id, drug, date)
prescription[, code_prescription := NA_integer_]


####  PPIs  ####

code1 <-allocate_med_code(prescription, "drug", "date", "omeprazole", 
          codes$presc$omeprazole, codes_freq_used$presc$omeprazole, 0.8)
code2 <-allocate_med_code(prescription, "drug", "date", "pantoprazole", 
          codes$presc$pantoprazole, codes_freq_used$presc$pantoprazole, 0.8)
code3 <-allocate_med_code(prescription, "drug", "date", "lansoprazole", 
          codes$presc$lansoprazole, codes_freq_used$presc$lansoprazole, 0.8)
code4 <-allocate_med_code(prescription, "drug", "date", "esomeprazole", 
          codes$presc$esomeprazole, codes_freq_used$presc$esomeprazole, 0.8)
code5 <-allocate_med_code(prescription, "drug", "date", "rabeprazole", 
         codes$presc$rabeprazole, codes_freq_used$presc$rabeprazole, 0.8)

prescription[, code_prescription := fcoalesce(code_prescription, code1, code2, code3, code4, code5)]

      



####  H2RAs  ####

code1 <-allocate_med_code(prescription, "drug", "date","cimetidine", 
                          codes$presc$cimetidine, codes_freq_used$presc$cimetidine, 0.8)
code2 <-allocate_med_code(prescription, "drug", "date", "ranitidine", 
                          codes$presc$ranitidine, codes_freq_used$presc$ranitidine, 0.8)
code3 <-allocate_med_code(prescription, "drug", "date","famotidine", 
                          codes$presc$famotidine, codes_freq_used$presc$famotidine, 0.8)
code4 <-allocate_med_code(prescription, "drug", "date","nizatidine", 
                          codes$presc$nizatidine, codes_freq_used$presc$nizatidine, 0.8)

prescription[, code_prescription := fcoalesce(code_prescription, code1, code2, code3, code4)]





####  Aspirin, OAC, Clopidogrel, NSAID  ####



code1 <-allocate_med_code(prescription, "drug", "date", "aspirin", 
                          codes$presc$aspirin, codes_freq_used$presc$aspirin, 0.8)
code2 <-allocate_med_code(prescription, "drug", "date", "OAC", 
                          codes$presc$oac, codes_freq_used$presc$oac, 0.8)
code3 <-allocate_med_code(prescription, "drug", "date", "clopidogrel", 
                          codes$presc$clopidogrel, codes_freq_used$presc$clopidogrel, 0.8)
code4 <-allocate_med_code(prescription, "drug", "date", "NSAID", 
                          codes$presc$nsaid, codes_freq_used$presc$nsaid, 0.8)

prescription[, code_prescription := fcoalesce(code_prescription, code1, code2, code3, code4)]




#### Steroids  ####



code1 <-allocate_med_code(prescription, "drug", "date", "inhaled_steroid", 
                          codes$presc$isteroid, codes_freq_used$presc$isteroid, 0.8)
code2 <-allocate_med_code(prescription, "drug", "date", "systemic_steroid_copd", 
                          codes$presc$ssteroid, codes_freq_used$presc$ssteroid, 0.8)
code3 <-allocate_med_code(prescription, "drug", "date", "systemic_steroid_other", 
                          codes$presc$ssteroid, codes_freq_used$presc$ssteroid, 0.8)
prescription[, code_prescription := fcoalesce(code_prescription, code1, code2, code3)]

unmatched_drugs <- prescription[is.na(code_prescription), unique(drug)]
if (length(unmatched_drugs) > 0) {
  stop("Medication drugs missing prescription codes: ",
       paste(unmatched_drugs, collapse = ", "), call. = FALSE)
}





############################
#  Tidy prescription data  #
############################

# Remove unneeded objects
rm(allocate_med_code)


# Tidy data while retaining the private P9-P13 linkage key
prescription<-prescription[,.(
  prescription_id_internal, patient_id, code_prescription, date, days
)]
setorder(prescription, patient_id, date) 

