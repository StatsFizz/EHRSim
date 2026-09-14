################################################################################
#
#   Script:       P6_a_Simulate_medication_dates.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R objects: patient, comorbid_meds, comorbidity_coef_lists
#
#   Data created:	R object: comorbid
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates information about the patient's 
#					      treatments (aspirin, clopidogrel, oral anticoagulants, 
#               non-steroidal anti-inflammatories, inhaled and systemic steroids
#               and proton pump inhibitors and H2RAs.  Start and intended end
#               dates of medication duration are generated.   
#
################################################################################
#
#   Packages needed:  data.table
#
################################################################################



##############################
#  Gather the required data  #
##############################


add_vars <- c("aspirin", "clopidogrel",
              "oac", "inhaled_steroid","systemic_steroid",
              "nsaid","h2ra", "ppi")
setDT(patient)
comorbid <- copy(comorbid_meds)
for (v in add_vars) {
  comorbid[, (v) := 0L]
}
rm(add_vars)

# Use the patient's exact follow-up boundary when placing treatment dates. Some
# interval dateouts are reconstructed from rounded ages and can drift slightly
# beyond enddate_init.
comorbid[patient, enddate_init := i.enddate_init, on = "patient_id"]
comorbid[, dateout := pmin(dateout, enddate_init)]

first_date_or_na <- function(x) {
  if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
}

last_date_or_na <- function(x) {
  if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
}

################z##########
#  Aspirin prescription  #
##########################

# Start aspirin for 80-90% of people with known relevant conditions:
# - heartfailure (80%)
# - MI (85%)
# - chd (90%)
# - PVD (85%)
# - atherosclerosis (85%)
#  Additionally start for small number of other individuals

# Those without prespecified conditions
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$aspirin_coefs,"logitaspirin") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$aspirin_coefs,"logitaspirin") 
comorbid<-genind(comorbid, "logitaspirin", "aspirin") 

# Those with prespecified conditions:
comorbid[, u := runif(.N)]
comorbid[, aspirin := fifelse(
  u < 0.90 & chd_onset == 1, 1,
  fifelse(u < 0.85 & mi_event == 1, 1,
    fifelse(u < 0.85 & pvd_onset == 1,1,
      fifelse(u < 0.85 & other_athero_onset == 1, 1,
        fifelse(u < 0.80 & heartfail_onset == 1, 1,
            fifelse(u < 0.50 & stroke_event == 1,  1, aspirin))))))]
comorbid[, u := NULL]

# Once a patient has started aspirin mark as always user  
comorbid[order(patient_id, agein_det), 
         aspirin := as.integer(cumsum(aspirin) >= 1), by = patient_id]


### Date of starting aspirin

# Condition for "no relevant disease"
comorbid[, no_relevant :=
           chd_onset == 0 &
           mi_prev   == 0 &
           pvd_onset == 0 &
           other_athero_onset == 0 &
           heartfail_onset    == 0 &
           stroke_prev == 0]

# If no prior conditions: random date between datein and dateout
comorbid[, u := runif(.N)]
comorbid[, start1 := as.Date(as.numeric(datein) +
         round(u * (as.numeric(dateout) - as.numeric(datein))))]
comorbid[,u:=NULL]

# If prior conditions: earliest onset date ± ≤ 7 days, bounded by datein/dateout
comorbid[, start2 := fifelse(no_relevant == FALSE,
           {
           as.Date(pmin( pmax(
               pmin(as.numeric(heartfail_onset_date),
                    as.numeric(mi_event_first_date),
                    as.numeric(other_athero_onset_date),
                    as.numeric(pvd_onset_date),
                    as.numeric(chd_onset_date),
                    as.numeric(stroke_event_first_date),
                na.rm = TRUE) + round(runif(.N)*7), 
               datein, na.rm=TRUE), dateout, na.rm=TRUE))
           }, as.Date(NA))]

# Assign relevant start date
comorbid[, aspirin_start_date := fifelse(no_relevant, start1, start2)]
comorbid[, aspirin_start_date := fifelse(aspirin==1, aspirin_start_date, as.Date(NA))]

# Final: minimum per patient
comorbid[, aspirin_start_date := first_date_or_na(aspirin_start_date), by = patient_id]

# Clean up
comorbid[, c("no_relevant", "start1", "start2") := NULL]


### Date of ending aspirin

##  Aspirin generally forever
comorbid[, aspirin_end_date := fifelse(!is.na(aspirin_start_date), as.Date("3000-01-01"), as.Date(NA))]
comorbid[, aspirin_end_date := last_date_or_na(aspirin_end_date), by = patient_id]



# Aspirin stopped if high risk of bleeding
# Stop aspirin after experiencing upper GI bleed or peptic ulcer
comorbid[,aspirin_end_date := fifelse(!is.na(upper_gi_bleed_event_date) &
                                        !is.na(aspirin_start_date) &
                                        !is.na(aspirin_end_date) &
                                        upper_gi_bleed_event_date >= aspirin_start_date,
                                      pmin(upper_gi_bleed_event_date, aspirin_end_date),
                                      aspirin_end_date)]
comorbid[,aspirin_end_date := fifelse(!is.na(peptic_ulcer_event_date) &
                                        !is.na(aspirin_start_date) &
                                        !is.na(aspirin_end_date) &
                                        peptic_ulcer_event_date >= aspirin_start_date,
                                      pmin(peptic_ulcer_event_date, aspirin_end_date),
                                      aspirin_end_date)]
comorbid[,aspirin_end_date := first_date_or_na(aspirin_end_date), by = patient_id]


### Model aspirin propensity to drop-out 
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$asp_stop_coefs,"logitaspstop")
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$asp_stop_coefs,"logitaspstop")

qs <- quantile(comorbid$propbleed, probs = c(0,0.25, 0.5, 0.75,1), na.rm = TRUE)
comorbid[, bleedcat := cut(propbleed, breaks = qs,include.lowest = TRUE,
  labels = FALSE)]
comorbid[,logitaspstop:=logitaspstop + 0.4*(bleedcat-2.5)]

### Convert to a probability
comorbid[,prob_aspirin_stop:=exp(logitaspstop)/(1+exp(logitaspstop))]






##############################
#  Clopidogrel prescription  #
##############################


# Those without stroke
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$clopidogrel_coefs,"logitclopidogrel") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$clopidogrel_coefs,"logitclopidogrel") 
comorbid<-genind(comorbid, "logitclopidogrel", "clopidogrel") 

# Start clopidogrel for 90% of people with stroke, following stroke
comorbid[, clopidogrel := fifelse(runif(.N) < 0.90 & stroke_event == 1, 1, clopidogrel)]



## Date of clopidogrel start and end


# Start date
comorbid[, clopidogrel_start_date := fifelse(stroke_event == 1,
            as.Date(pmax(pmin(as.numeric(stroke_event_date) + round(runif(.N)*7), dateout, na.rm=TRUE),datein, na.rm=TRUE)), 
            as.Date(pmax(pmin(as.numeric(datein) + round(runif(.N)*(as.numeric(dateout)-as.numeric(datein))),dateout, na.rm=TRUE),datein, na.rm=TRUE))) 
]
comorbid[, clopidogrel_start_date := fifelse(clopidogrel == 1, clopidogrel_start_date, as.Date(NA))]


# Clopidogrel typically prescribed forever for stroke and
# 1 year / 4 months for other reasons
comorbid[, c("u1", "u2") := .(runif(.N), runif(.N))]
comorbid[,clopidogrel_end_date := fifelse(stroke_event==0 & clopidogrel==1, 
        as.Date(as.numeric(clopidogrel_start_date) + round(fifelse(u1<0.4, 365.25 + u2*21, 122 + u2*7))),
        fifelse(stroke_event==1 & clopidogrel==1, as.Date("3000-01-01"), as.Date(NA)))]
comorbid[,c("u1","u2"):=NULL]

# Restrict to people with prescriptions
rows <- which(!is.na(comorbid[["clopidogrel_start_date"]]))
tmp<-comorbid[rows,c("patient_id", "agein_det", 
                     "clopidogrel_start_date", "clopidogrel_end_date")]
# Mark cumulative latest end date and next start date 
tmp[order(patient_id, agein_det),
         max_end := as.Date(cummax(as.numeric(clopidogrel_end_date))),
         by = patient_id]
tmp[order(patient_id, agein_det),
         next_start := shift(clopidogrel_start_date, n=1, type="lead", fill=NA),
         by = patient_id]
# Adapt end dates so prescriptions don't overlap but ultimate end date is preserved
tmp[,clopidogrel_end_date := fifelse(clopidogrel_end_date > next_start & !is.na(next_start),
                                 next_start-1, clopidogrel_end_date)]
tmp[,clopidogrel_end_date := fifelse(clopidogrel_end_date < max_end & is.na(next_start),
                                     max_end, clopidogrel_end_date)]

comorbid[,c("clopidogrel_start_date","clopidogrel_end_date"):=NULL]
comorbid[tmp, `:=`(
    clopidogrel_start_date = i.clopidogrel_start_date,
    clopidogrel_end_date   = i.clopidogrel_end_date  ),
  on = .(patient_id, agein_det)]


### Clopidogrel propensity to drop-out: assume same as aspirin

# Clopidogrel stopped if high risk of bleeding
# Stop after experiencing upper GI bleed or peptic ulcer
comorbid[,clopidogrel_end_date := fifelse(!is.na(upper_gi_bleed_event_date) &
                                            !is.na(clopidogrel_start_date) &
                                            !is.na(clopidogrel_end_date) &
                                            upper_gi_bleed_event_date >= clopidogrel_start_date,
                                          pmin(upper_gi_bleed_event_date, clopidogrel_end_date),
                                          clopidogrel_end_date)]
comorbid[,clopidogrel_end_date := fifelse(!is.na(peptic_ulcer_event_date) &
                                            !is.na(clopidogrel_start_date) &
                                            !is.na(clopidogrel_end_date) &
                                            peptic_ulcer_event_date >= clopidogrel_start_date,
                                          pmin(peptic_ulcer_event_date, clopidogrel_end_date),
                                          clopidogrel_end_date)]

# Clopidogrel episodes can span later intervals, so look across the patient's
# event history rather than relying only on events recorded in the same row.
clopidogrel_stop_events <- unique(comorbid[, .(
  patient_id,
  clopidogrel_stop_date = c(upper_gi_bleed_event_date, peptic_ulcer_event_date)
)][!is.na(clopidogrel_stop_date)])

if (nrow(clopidogrel_stop_events) > 0) {
  clopidogrel_rows <- comorbid[
    !is.na(clopidogrel_start_date) & !is.na(clopidogrel_end_date),
    .(patient_id, agein_det, clopidogrel_start_date)
  ]

  if (nrow(clopidogrel_rows) > 0) {
    clopidogrel_rows[, clopidogrel_episode_id := .I]
    clopidogrel_stop_candidates <- clopidogrel_stop_events[
      clopidogrel_rows,
      on = "patient_id",
      allow.cartesian = TRUE
    ]
    clopidogrel_stop_candidates <- clopidogrel_stop_candidates[
      clopidogrel_stop_date >= clopidogrel_start_date
    ]
    clopidogrel_stop_candidates <- clopidogrel_stop_candidates[
      ,
      .(clopidogrel_stop_date = min(clopidogrel_stop_date, na.rm = TRUE)),
      by = clopidogrel_episode_id
    ]

    clopidogrel_rows[
      clopidogrel_stop_candidates,
      clopidogrel_stop_date := i.clopidogrel_stop_date,
      on = "clopidogrel_episode_id"
    ]
    comorbid[
      clopidogrel_rows[!is.na(clopidogrel_stop_date)],
      clopidogrel_end_date := pmin(clopidogrel_end_date, i.clopidogrel_stop_date),
      on = .(patient_id, agein_det)
    ]

    rm(clopidogrel_rows, clopidogrel_stop_candidates)
  }
}
rm(clopidogrel_stop_events)

### Assume clopidogrel propensity to drop-out is same as aspirin
comorbid[,prob_clopidogrel_stop:=prob_aspirin_stop]










##############################################
#  Start of oral anticoagulant prescription  #
##############################################

# Those without stroke
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$oac_coefs,"logitoac") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$oac_coefs,"logitoac") 
comorbid<-genind(comorbid, "logitoac", "oac") 

# Start OAC for 90% of people with stroke, following stroke
comorbid[, oac := fifelse(runif(.N) < 0.90 & stroke_event == 1, 1, oac)]

# Once a patient has started OAC, mark them as an ongoing user
comorbid[order(patient_id, agein_det),
         oac := as.integer(cumsum(oac) >= 1), by = patient_id]



## Date of OAC start (always user)

# Start date
comorbid[, oac_start_date := fifelse(stroke_event == 1,
          as.Date(pmax(pmin(as.numeric(stroke_event_date) 
      + round(runif(.N)*7), dateout, na.rm=TRUE),datein, na.rm=TRUE)), 
          as.Date(pmax(pmin(as.numeric(datein) 
    + round(runif(.N)*(as.numeric(dateout)-as.numeric(datein))),dateout, 
    na.rm=TRUE),datein, na.rm=TRUE)))]
comorbid[, oac_start_date := fifelse(oac == 1, oac_start_date, as.Date(NA))]

# Final: minimum per patient
comorbid[, oac_start_date := first_date_or_na(oac_start_date), by = patient_id]
comorbid[, oac_end_date := fifelse(!is.na(oac_start_date), as.Date("3000-01-01"), as.Date(NA))]

### OAC propensity to drop-out: assume same as aspirin

# OAC stopped if high risk of bleeding
# Stop after experiencing upper GI bleed or peptic ulcer
comorbid[,oac_end_date := fifelse(!is.na(upper_gi_bleed_event_date) &
                                    !is.na(oac_start_date) &
                                    !is.na(oac_end_date) &
                                    upper_gi_bleed_event_date >= oac_start_date,
                                  pmin(upper_gi_bleed_event_date, oac_end_date),
                                  oac_end_date)]
comorbid[,oac_end_date := fifelse(!is.na(peptic_ulcer_event_date) &
                                    !is.na(oac_start_date) &
                                    !is.na(oac_end_date) &
                                    peptic_ulcer_event_date >= oac_start_date,
                                  pmin(peptic_ulcer_event_date, oac_end_date),
                                  oac_end_date)]
comorbid[,oac_end_date := first_date_or_na(oac_end_date), by=patient_id]

### Assume OAC propensity to drop-out is same as aspirin
comorbid[,prob_oac_stop:=prob_aspirin_stop]






#######################
#  Inhaled steroids   #
#######################

# Generate inhaled steroid prescription  
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$isteroid_coefs,"logitister") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$isteroid_coefs,"logitister") 
comorbid<-genind(comorbid, "logitister", "inhaled_steroid") 


# Period of potential inhaled steroid use: randomly generate start/end dates
comorbid[, inhaled_steroid_start_date := as.Date(datein + runif(.N)*(dateout-datein))]
comorbid[, inhaled_steroid_end_date := as.Date(inhaled_steroid_start_date + runif(.N)*(dateout-inhaled_steroid_start_date))]

comorbid[, inhaled_steroid_start_date := fifelse(inhaled_steroid == 1, inhaled_steroid_start_date, as.Date(NA))]
comorbid[, inhaled_steroid_end_date := fifelse(inhaled_steroid == 1, inhaled_steroid_end_date, as.Date(NA))]





########################
#  Systemic steroids   #
########################


# Generate systemic steroid prescription  
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$ssteroid_coefs,"logitsster") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$ssteroid_coefs,"logitsster") 
comorbid<-genind(comorbid, "logitsster", "systemic_steroid") 


# Period of potential systemic steroid use: randomly generate start/end dates
comorbid[, systemic_steroid_start_date := as.Date(datein + runif(.N)*(dateout-datein))]
comorbid[, systemic_steroid_end_date := as.Date(systemic_steroid_start_date + runif(.N)*(dateout-systemic_steroid_start_date))]

comorbid[, systemic_steroid_start_date := fifelse(systemic_steroid == 1, systemic_steroid_start_date, as.Date(NA))]
comorbid[, systemic_steroid_end_date := fifelse(systemic_steroid == 1, systemic_steroid_end_date, as.Date(NA))]





########################
#  NSAID prescription  #
########################

# Generate NSAID prescription  
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$nsaid_coefs,"logitnsaid") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$nsaid_coefs,"logitnsaid") 
comorbid<-genind(comorbid, "logitnsaid", "nsaid") 


# Period of potential NSAID use
comorbid[, nsaid_start_date := as.Date(datein + runif(.N)*(dateout-datein))]
comorbid[, nsaid_end_date := as.Date(nsaid_start_date + runif(.N)*(dateout-nsaid_start_date))]

comorbid[, nsaid_start_date := fifelse(nsaid == 1, nsaid_start_date, as.Date(NA))]
comorbid[, nsaid_end_date := fifelse(nsaid == 1, nsaid_end_date, as.Date(NA))]





##############################
#  Indication for PPI/H2RA   #
##############################

# Generate PPI/H2RA indication
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$ppiind_coefs,"logitppiind") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$ppiind_coefs,"logitppiind") 

#  Add in some interactions and a calendar time effect
breaks  <- c(-Inf, 1990, 1993, 1996, 1999, 2002, 2005, 
             2008, 2011, 2014, 2017, 2020, Inf)
effects <- c(-2, -1.8, -1.7, -1.5, -1.2, -0.9, -0.6, -0.4, -0.3, -0.2, -0.1, 0)
comorbid[,logitppiind:= logitppiind + 0.2*nsaid*upper_gi_bleed_event
                        + 0.2*nsaid*peptic_ulcer_event
                        + 0.2*nsaid*(agein_det>=65)]
comorbid[, logitppiind := logitppiind + effects[findInterval(yearmid, breaks)]]
comorbid<-genind(comorbid, "logitppiind", "ppiind") 
## Ignore dates prior to 1981 - wouldn't have resulted in H2RA or PPI prescription
comorbid[,ppiind:=fifelse(dateout<as.Date("1981-01-01"), 0, ppiind)]




##########################################
#  Choose PPI or H2RA given indication   #
##########################################

# Generate PPI/H2RA indication
comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$ppivsh2ra_coefs,"logitppi") 
comorbid<-createlogit_oth(comorbid, comorbidity_coef_lists$ppivsh2ra_coefs,"logitppi") 

#  Add a strong calendar time effect (PPI prescribing overtook H2RAs quickly)
breaks  <- c(-Inf, 1990, 1993, 1996, 1999, 2002, 2005, 
             2008, 2011, 2014, 2017, 2020, Inf)
effects <- c(0,0.2,0.4,0.6,0.8,1.2,2.0,3.0,3.3,3.5,3.6,3.7,0)
comorbid[, logitppi := logitppi + effects[findInterval(yearmid, breaks)]]
comorbid<-genind(comorbid, "logitppi", "ppi_presc") 

# Allocate prescription to PPI or H2RA
comorbid[,ppi:=fifelse(ppiind==1,ppi_presc,0)]
comorbid[,h2ra:=fifelse(ppiind==1,1-ppi_presc,0)]
comorbid[,c("ppi_presc"):=NULL]

# Dates of PPI and H2RA prescriptions 

# PPI: Start and end date
comorbid[, ppi_start_date := as.Date(datein + runif(.N)*(dateout-datein))]
comorbid[, ppi_end_date := as.Date(ppi_start_date + runif(.N)*(dateout-ppi_start_date))]
comorbid[, ppi_start_date := fifelse(ppi == 1, ppi_start_date, as.Date(NA))]
comorbid[, ppi_end_date := fifelse(ppi == 1, ppi_end_date, as.Date(NA))]

# H2RA: Start and end date
comorbid[, h2ra_start_date := as.Date(datein + runif(.N)*(dateout-datein))]
comorbid[, h2ra_end_date := as.Date(h2ra_start_date + runif(.N)*(dateout-h2ra_start_date))]
comorbid[, h2ra_start_date := fifelse(h2ra == 1, h2ra_start_date, as.Date(NA))]
comorbid[, h2ra_end_date := fifelse(h2ra == 1, h2ra_end_date, as.Date(NA))]
comorbid[, enddate_init := NULL]
