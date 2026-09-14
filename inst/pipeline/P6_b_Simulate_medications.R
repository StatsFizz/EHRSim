################################################################################
#
#   Script:       P6_b_Simulate_medications.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient comorbid 
#
#   Data created:	R object: medication
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script creates simulated medications, for: PPI, H2RA, 
#					      NSAID, oral anticoagulants, aspirin, clopidogrel, systemic  
#               and inhaled corticosteroids.
#
################################################################################
#
#   Packages needed: lubridate, data.table 
#
################################################################################


#####################
#  PPIs and H2RAs   #
#####################

###  Gather data together

setDT(patient)
setDT(comorbid)


# Gather needed data, keeping only prescriptions
ppi_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  comorbid[, .(patient_id, ppi_start_date, ppi_end_date,
               h2ra_start_date, h2ra_end_date)],
  by = "patient_id",
  all.x = TRUE
)[!(is.na(ppi_start_date) & is.na(h2ra_start_date)),]

# Reshape PPI/H2RA episode windows to long format
ppi_presc_long <- melt(
  ppi_presc,
  id.vars = c("patient_id", "reg_date", "enddate"),
  measure.vars = list(
    date = c("ppi_start_date", "h2ra_start_date"),
    end_period = c("ppi_end_date", "h2ra_end_date")
  ),
  variable.name = "drug_class",
)[!is.na(date)]
setDT(ppi_presc_long) 

# Remove episodes starting after end-date or ending before registration
ppi_presc_long<-ppi_presc_long[!(date>enddate | end_period<reg_date), ]
ppi_presc_long[, end_period := fifelse(is.na(end_period), enddate, end_period)]
ppi_presc_long[, end_period := fifelse(end_period>enddate, enddate, end_period)]
ppi_presc_long[, date := fifelse(date<reg_date, reg_date, date)]
ppi_presc_long <- ppi_presc_long[date <= end_period]

# Create a binary PPI/H2RA variable
ppi_presc_long[
  , ppi_h2ra := fifelse(drug_class == 1L, "PPI", "H2RA")
][, drug_class := NULL]

# Create start-end dates for each PPI/H2RA treatment episode
setorder(ppi_presc_long, patient_id, date)
ppi_presc_long[, `:=`(
    date_presc1 = date,
    days_presc1 = NA_integer_,
    drug_presc1 = NA_character_,
    strength_presc1 = NA_character_
  )]
ppi_presc_long[, c("reg_date", "enddate", "date") := NULL]



###  Information required to simulate prescription details


dates_ppi <-list(
  start_date = ppi_presc_long$date_presc1, 
  end_date=ppi_presc_long$end_period,
  pdisc=0.06,
  maxgap_fn = function(n, maxgap) {round(-7 + runif(n, 0, maxgap))},
  maxgap_fn_args=list(maxgap=18)
)
drug_change_ppi <-list(
  nextdrug_fn = choose_ppi_h2ra,
  nextdrug_fn_args = list(last_drug=ppi_presc_long$drug_presc1, presc_date=ppi_presc_long$date_presc1)
)
duration_ppi <- list(
  length_presc = ppi_presc_long$days_presc1,
  pchange=0.04,
  prescdays_fn = function(n, min, ave, std) round(rtruncnorm(n=n, a=min,mean=ave,sd=std)),
  prescdays_fn_args=list(min=30,ave=200,std=100)
)
strength_ppi <- list(
  current_strength=ppi_presc_long$strength_presc1,
  pnewstrength=0.2,
  strength_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  strength_fn_args = list(items=c("low", "medium", "high"),
                          probs = c(0.2, 0.6, 0.2))
)


###  Allocate initial prescription details

# Initial drug classes  
ppi_presc_long[, drug_presc1 := do.call(drug_change_ppi$nextdrug_fn, 
                               c(list(this_drug_class=ppi_presc_long$ppi_h2ra,
                                 presc_date=ppi_presc_long$date_presc1)))]

# Strength of initial prescription
ppi_presc_long[, strength_presc1 := do.call(strength_ppi$strength_fn, 
                               c(list(.N), strength_ppi$strength_fn_args))]

## Days prescribed
ppi_presc_long[, days_presc1 := do.call(duration_ppi$prescdays_fn, 
                               c(list(.N), duration_ppi$prescdays_fn_args))]



###  Allocate subsequent prescription details
i<-1
mindatepresc <- min(as.numeric(ppi_presc_long$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_ppi[["start_date"]] <- as.Date(ppi_presc_long[[paste0("date_presc", i)]] + 
                             ppi_presc_long[[paste0("days_presc", i)]])
  drug_change_ppi[["nextdrug_fn_args"]] <-
    list(last_drug=ppi_presc_long[[paste0("drug_presc", i)]], presc_date=ppi_presc_long[[paste0("date_presc", i)]])
  duration_ppi[["length_presc"]]<-ppi_presc_long[[paste0("days_presc", i)]]
  strength_ppi[["current_strength"]] <- ppi_presc_long[[paste0("strength_presc", i)]]
  
  # Use function to draw next prescription
  temp <- draw_next_presc(dates_ppi, duration_ppi, drug_change_ppi, strength_ppi)
  
  # Store new prescription details
  ppi_presc_long[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  ppi_presc_long[[paste0("days_presc", i + 1)]] <- temp[[2]]
  ppi_presc_long[[paste0("drug_presc", i + 1)]] <- temp[[3]]
  ppi_presc_long[[paste0("strength_presc", i + 1)]] <-temp[[4]]
  
  mindatepresc <- min(as.numeric(ppi_presc_long[[paste0("date_presc", i + 1)]]))
  i<- i+1
}


###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(ppi_presc_long) 
ppi_presc_long[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(ppi_presc_long), value = TRUE)
drug_cols    <- grep("^drug_presc", names(ppi_presc_long), value = TRUE)
strength_cols<- grep("^strength_presc", names(ppi_presc_long), value = TRUE)
days_cols    <- grep("^days_presc", names(ppi_presc_long), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(ppi_presc_long, id.vars = c("newid","patient_id"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_drug <- melt(ppi_presc_long, id.vars = c("newid","patient_id"), measure.vars = drug_cols,
               variable.name = "presc", value.name = "drug_presc", na.rm = TRUE)
m_strength <- melt(ppi_presc_long, id.vars = c("newid","patient_id"), measure.vars = strength_cols,
                   variable.name = "presc", value.name = "strength_presc", na.rm = TRUE)
m_days <- melt(ppi_presc_long, id.vars = c("newid","patient_id"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_drug[, presc := as.integer(gsub("\\D", "", presc))]
m_strength[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
ppi_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","presc"), all=TRUE),
                   list(m_date, m_drug, m_strength, m_days))

# Tidy dataset
ppi_presc_long[, c("newid", "presc") := NULL]
ppi_presc_long<-ppi_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(ppi_presc_long,
         old = c("date_presc", "drug_presc", "strength_presc", "days_presc"),
         new = c("date", "drug", "strength", "days"))


#####################################
#  Simulate aspirin prescriptions   #
#####################################


# Gather needed data, keeping only prescriptions, removing duplicate records
aspirin_presc<-comorbid[aspirin==1][, .(patient_id, aspirin_start_date, aspirin_end_date, prob_aspirin_stop)]
aspirin_presc<-aspirin_presc[,mpstop:=mean(prob_aspirin_stop), by=.(patient_id, aspirin_start_date)]
aspirin_presc<-aspirin_presc[,prob_aspirin_stop:=NULL]
aspirin_presc<-unique(aspirin_presc)

aspirin_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  aspirin_presc[, .(patient_id, aspirin_start_date, aspirin_end_date, mpstop)],
  by = "patient_id",
  all.x = TRUE
)[!is.na(aspirin_start_date),]

# Remove prescriptions starting after end-date or ending before registration
aspirin_presc <-aspirin_presc[(aspirin_start_date<=enddate),]
aspirin_presc <-aspirin_presc[(aspirin_end_date>=reg_date | is.na(aspirin_end_date)), ]

aspirin_presc[, aspirin_end_date := fifelse(is.na(aspirin_end_date),
                                          enddate, aspirin_end_date)]
aspirin_presc[, aspirin_end_date := fifelse(aspirin_end_date>enddate,
                                            enddate, aspirin_end_date)]
aspirin_presc[, aspirin_start_date := fifelse(aspirin_start_date<reg_date,
                                            reg_date, aspirin_start_date)]

setnames(aspirin_presc, old = c("aspirin_start_date"), new = c("date_presc1"))
aspirin_presc[,days_presc1:=NA]



###  Information required to simulate prescription details

dates_aspirin <-list(
  start_date = aspirin_presc$date_presc1, 
  end_date=aspirin_presc$aspirin_end_date,
  pdisc=aspirin_presc$mpstop,
  maxgap_fn = function(n, maxgap) {round(-7 + runif(n, 0, maxgap))},
  maxgap_fn_args=list(maxgap=21)
)
duration_aspirin <- list(
  length_presc = aspirin_presc$days_presc1,
  pchange=0.15,
  prescdays_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  prescdays_fn_args = list(items=c(30, 60, 90, 180, 360),
                        probs = c(0.2, 0.05, 0.5, 0.15, 0.1))
)


###  Allocate initial prescription details

## Days prescribed
aspirin_presc[, days_presc1 := do.call(duration_aspirin$prescdays_fn, 
              c(list(.N), duration_aspirin$prescdays_fn_args))]

### Draw subsequent prescriptions

i<-1
mindatepresc <- min(as.numeric(aspirin_presc$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_aspirin[["start_date"]] <- as.Date(aspirin_presc[[paste0("date_presc", i)]] + 
                                         aspirin_presc[[paste0("days_presc", i)]])
  duration_aspirin[["length_presc"]]<-aspirin_presc[[paste0("days_presc", i)]]

  # Use function to draw next prescription
  temp <- draw_next_presc(dates_aspirin, duration_aspirin, NULL, NULL)
  
  # Store new prescription details
  aspirin_presc[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  aspirin_presc[[paste0("days_presc", i + 1)]] <- temp[[2]]

  mindatepresc <- min(as.numeric(aspirin_presc[[paste0("date_presc", i + 1)]]))
  i<- i+1
}

###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(aspirin_presc) 
aspirin_presc[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(aspirin_presc), value = TRUE)
days_cols    <- grep("^days_presc", names(aspirin_presc), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(aspirin_presc, id.vars = c("newid","patient_id"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_days <- melt(aspirin_presc, id.vars = c("newid","patient_id"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
aspirin_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","presc"), all=TRUE),
                         list(m_date, m_days))

# Tidy dataset
aspirin_presc_long[, c("newid", "presc") := NULL]
aspirin_presc_long<-aspirin_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(aspirin_presc_long,
         old = c("date_presc", "days_presc"),
         new = c("date", "days"))
aspirin_presc_long[,`:=`(
          drug = "aspirin",
          strength = NA_character_
        )]
rm(aspirin_presc)



#################################################
#  Simulate oral anticoagulants prescriptions   #
#################################################



# Gather needed data, keeping only prescriptions, removing duplicate records
oac_presc<-comorbid[oac==1][, .(patient_id, oac_start_date, oac_end_date, prob_oac_stop)]
oac_presc<-oac_presc[,mpstop:=mean(prob_oac_stop), by=.(patient_id, oac_start_date)]
oac_presc<-oac_presc[,prob_oac_stop:=NULL]
oac_presc<-unique(oac_presc)

# Gather needed data, keeping only prescriptions
oac_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  oac_presc[, .(patient_id, oac_start_date, oac_end_date, mpstop)],
  by = "patient_id",
  all.x = TRUE
)[!is.na(oac_start_date),]

# Remove prescriptions starting after end-date or ending before registration
oac_presc <-oac_presc[(oac_start_date<=enddate),]
oac_presc <-oac_presc[(oac_end_date>=reg_date | is.na(oac_end_date)), ]

oac_presc[, oac_end_date := fifelse(is.na(oac_end_date),
                                            enddate, oac_end_date)]
oac_presc[, oac_end_date := fifelse(oac_end_date>enddate,
                                            enddate, oac_end_date)]
oac_presc[, oac_start_date := fifelse(oac_start_date<reg_date,
                                              reg_date, oac_start_date)]

setnames(oac_presc, old = c("oac_start_date"), new = c("date_presc1"))
oac_presc[,days_presc1:=NA]



###  Information required to simulate prescription details

dates_oac  <-list(
  start_date = oac_presc$date_presc1, 
  end_date=oac_presc$oac_end_date,
  pdisc=oac_presc$mpstop,
  maxgap_fn = function(n, maxgap) {round(-7 + runif(n, 0, maxgap))},
  maxgap_fn_args=list(maxgap=28)
)
duration_oac <- list(
  length_presc = oac_presc$days_presc1,
  pchange=0.2,
  prescdays_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  prescdays_fn_args = list(items=c(30, 60, 90, 183, 366),
                           probs =c(0.2, 0.05, 0.5, 0.15, 0.1))
)



###  Allocate initial prescription details

## Days prescribed
oac_presc[, days_presc1 := do.call(duration_oac$prescdays_fn, 
                            c(list(.N), duration_oac$prescdays_fn_args))]

### Draw subsequent prescriptions

i<-1
mindatepresc <- min(as.numeric(oac_presc$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_oac[["start_date"]] <- as.Date(oac_presc[[paste0("date_presc", i)]] + 
                                             oac_presc[[paste0("days_presc", i)]])
  duration_oac[["length_presc"]]<-oac_presc[[paste0("days_presc", i)]]
  
  # Use function to draw next prescription
  temp <- draw_next_presc(dates_oac, duration_oac, NULL, NULL)
  
  # Store new prescription details
  oac_presc[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  oac_presc[[paste0("days_presc", i + 1)]] <- temp[[2]]
  
  mindatepresc <- min(as.numeric(oac_presc[[paste0("date_presc", i + 1)]]))
  i<- i+1
}

###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(oac_presc) 
oac_presc[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(oac_presc), value = TRUE)
days_cols    <- grep("^days_presc", names(oac_presc), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(oac_presc, id.vars = c("newid","patient_id"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_days <- melt(oac_presc, id.vars = c("newid","patient_id"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
oac_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","presc"), all=TRUE),
                             list(m_date, m_days))

# Tidy dataset
oac_presc_long[, c("newid", "presc") := NULL]
oac_presc_long<-oac_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(oac_presc_long,
         old = c("date_presc", "days_presc"),
         new = c("date", "days"))
oac_presc_long[,`:=`(
  drug = "OAC",
  strength = NA_character_
)]
rm(oac_presc)








#########################################
#  Simulate clopidogrel prescriptions   #
#########################################


# Gather needed data, keeping only prescriptions, removing duplicate records
clopidogrel_presc<-comorbid[clopidogrel==1][, .(patient_id, clopidogrel_start_date, clopidogrel_end_date, prob_clopidogrel_stop)]
clopidogrel_presc<-clopidogrel_presc[,mpstop:=mean(prob_clopidogrel_stop), by=.(patient_id, clopidogrel_start_date)]
clopidogrel_presc<-clopidogrel_presc[,prob_clopidogrel_stop:=NULL]
clopidogrel_presc<-unique(clopidogrel_presc)


# Gather needed data, keeping only prescriptions
clopidogrel_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  clopidogrel_presc[, .(patient_id, clopidogrel_start_date, clopidogrel_end_date, mpstop)],
  by = "patient_id",
  all.x = TRUE
)[!is.na(clopidogrel_start_date),]

# Remove prescriptions starting after end-date or ending before registration
clopidogrel_presc <-clopidogrel_presc[(clopidogrel_start_date<=enddate),]
clopidogrel_presc <-clopidogrel_presc[(clopidogrel_end_date>=reg_date | is.na(clopidogrel_end_date)), ]

clopidogrel_presc[, clopidogrel_end_date := fifelse(is.na(clopidogrel_end_date),
                                    enddate, clopidogrel_end_date)]
clopidogrel_presc[, clopidogrel_end_date := fifelse(clopidogrel_end_date>enddate,
                                    enddate, clopidogrel_end_date)]
clopidogrel_presc[, clopidogrel_start_date := fifelse(clopidogrel_start_date<reg_date,
                                      reg_date, clopidogrel_start_date)]

setnames(clopidogrel_presc, old = c("clopidogrel_start_date"), new = c("date_presc1"))
clopidogrel_presc[,days_presc1:=NA]



###  Information required to simulate prescription details

dates_clopidogrel  <-list(
  start_date = clopidogrel_presc$date_presc1, 
  end_date=clopidogrel_presc$clopidogrel_end_date,
  pdisc=clopidogrel_presc$mpstop,
  maxgap_fn = function(n, maxgap) {round(-7 + runif(n, 0, maxgap))},
  maxgap_fn_args=list(maxgap=21)
)
duration_clopidogrel <- list(
  length_presc = clopidogrel_presc$days_presc1,
  pchange=0.08,
  prescdays_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  prescdays_fn_args = list(items=c(122, 366),
                           probs =c(0.6, 0.4))
)


###  Allocate initial prescription details

## Days prescribed
clopidogrel_presc[, days_presc1 := do.call(duration_clopidogrel$prescdays_fn, 
                                   c(list(.N), duration_clopidogrel$prescdays_fn_args))]

### Draw subsequent prescriptions

i<-1
mindatepresc <- min(as.numeric(clopidogrel_presc$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_clopidogrel[["start_date"]] <- as.Date(clopidogrel_presc[[paste0("date_presc", i)]] + 
                                         clopidogrel_presc[[paste0("days_presc", i)]])
  duration_clopidogrel[["length_presc"]]<-clopidogrel_presc[[paste0("days_presc", i)]]
  
  # Use function to draw next prescription
  temp <- draw_next_presc(dates_clopidogrel, duration_clopidogrel, NULL, NULL)
  
  # Store new prescription details
  clopidogrel_presc[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  clopidogrel_presc[[paste0("days_presc", i + 1)]] <- temp[[2]]
  
  mindatepresc <- min(as.numeric(clopidogrel_presc[[paste0("date_presc", i + 1)]]))
  i<- i+1
}

###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(clopidogrel_presc) 
clopidogrel_presc[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(clopidogrel_presc), value = TRUE)
days_cols    <- grep("^days_presc", names(clopidogrel_presc), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(clopidogrel_presc, id.vars = c("newid","patient_id"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_days <- melt(clopidogrel_presc, id.vars = c("newid","patient_id"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
clopidogrel_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","presc"), all=TRUE),
                         list(m_date, m_days))

# Tidy dataset
clopidogrel_presc_long[, c("newid", "presc") := NULL]
clopidogrel_presc_long<-clopidogrel_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(clopidogrel_presc_long,
         old = c("date_presc", "days_presc"),
         new = c("date", "days"))
clopidogrel_presc_long[,`:=`(
  drug = "clopidogrel",
  strength = NA_character_
)]
rm(clopidogrel_presc)








###################################
#  Simulate NSAID prescriptions   #
###################################


# Gather needed data, keeping only prescriptions, removing duplicate records
nsaid_presc<-comorbid[nsaid==1][, .(patient_id, nsaid_start_date, nsaid_end_date)]
nsaid_presc<-unique(nsaid_presc)

nsaid_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  nsaid_presc[, .(patient_id, nsaid_start_date, nsaid_end_date)],
  by = "patient_id",
  all.x = TRUE
)[!is.na(nsaid_start_date),]

# Remove prescriptions starting after end-date or ending before registration
nsaid_presc <-nsaid_presc[(nsaid_start_date<=enddate),]
nsaid_presc <-nsaid_presc[(nsaid_end_date>=reg_date | is.na(nsaid_end_date)), ]

nsaid_presc[, nsaid_end_date := fifelse(is.na(nsaid_end_date),
                                                    enddate, nsaid_end_date)]
nsaid_presc[, nsaid_end_date := fifelse(nsaid_end_date>enddate,
                                                    enddate, nsaid_end_date)]
nsaid_presc[, nsaid_start_date := fifelse(nsaid_start_date<reg_date,
                                                      reg_date, nsaid_start_date)]

setnames(nsaid_presc, old = c("nsaid_start_date"), new = c("date_presc1"))
nsaid_presc[,days_presc1:=NA]


###  Information required to simulate prescription details

dates_nsaid  <-list(
  start_date = nsaid_presc$date_presc1, 
  end_date=nsaid_presc$nsaid_end_date,
  pdisc=0.4,
  maxgap_fn = function(n, maxgap) {round(-7 + runif(n, 0, maxgap))},
  maxgap_fn_args=list(maxgap=180)
)
duration_nsaid <- list(
  length_presc = nsaid_presc$days_presc1,
  pchange=0.08,
  prescdays_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  prescdays_fn_args = list(items=c(5, 7, 28, 60),
                           probs=c(0.05, 0.3, 0.6, 0.05))
)



###  Allocate initial prescription details

## Days prescribed
nsaid_presc[, days_presc1 := do.call(duration_nsaid$prescdays_fn, 
                                           c(list(.N), duration_nsaid$prescdays_fn_args))]

### Draw subsequent prescriptions

i<-1
mindatepresc <- min(as.numeric(nsaid_presc$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_nsaid[["start_date"]] <- as.Date(nsaid_presc[[paste0("date_presc", i)]] + 
                                           nsaid_presc[[paste0("days_presc", i)]])
  duration_nsaid[["length_presc"]]<-nsaid_presc[[paste0("days_presc", i)]]
  
  # Use function to draw next prescription
  temp <- draw_next_presc(dates_nsaid, duration_nsaid, NULL, NULL)
  
  # Store new prescription details
  nsaid_presc[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  nsaid_presc[[paste0("days_presc", i + 1)]] <- temp[[2]]
  
  mindatepresc <- min(as.numeric(nsaid_presc[[paste0("date_presc", i + 1)]]))
  i<- i+1
}

###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(nsaid_presc) 
nsaid_presc[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(nsaid_presc), value = TRUE)
days_cols    <- grep("^days_presc", names(nsaid_presc), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(nsaid_presc, id.vars = c("newid","patient_id"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_days <- melt(nsaid_presc, id.vars = c("newid","patient_id"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
nsaid_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","presc"), all=TRUE),
                                 list(m_date, m_days))

# Tidy dataset
nsaid_presc_long[, c("newid", "presc") := NULL]
nsaid_presc_long<-nsaid_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(nsaid_presc_long,
         old = c("date_presc", "days_presc"),
         new = c("date", "days"))
nsaid_presc_long[,`:=`(
  drug = "NSAID",
  strength = NA_character_
)]
rm(nsaid_presc)





#############################################
#  Simulate inhaled steroid prescriptions   #
#############################################



# Gather needed data, keeping only prescriptions, removing duplicate records
isteroid_presc<-comorbid[inhaled_steroid==1][, 
          .(patient_id, inhaled_steroid_start_date, inhaled_steroid_end_date)]
isteroid_presc<-unique(isteroid_presc)

isteroid_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  isteroid_presc[, .(patient_id, 
               istart = inhaled_steroid_start_date,
               iend   = inhaled_steroid_end_date)],
  by = "patient_id",
  all.x = TRUE
)[!is.na(istart),]

# Remove prescriptions starting after end-date or ending before registration
isteroid_presc <-isteroid_presc[(istart<=enddate),]
isteroid_presc <-isteroid_presc[(iend>=reg_date | is.na(iend)), ]

isteroid_presc[, iend := fifelse(is.na(iend),enddate, iend)]
isteroid_presc[, iend := fifelse(iend>enddate,enddate, iend)]
isteroid_presc[, istart := fifelse(istart<reg_date, reg_date, istart)]

setnames(isteroid_presc, old = c("istart"), new = c("date_presc1"))
isteroid_presc[,days_presc1:=NA]


###  Information required to simulate prescription details

dates_isteroid  <-list(
  start_date = isteroid_presc$date_presc1, 
  end_date=isteroid_presc$iend,
  pdisc=0.4,
  maxgap_fn = function(n, maxgap) {round(-7 + runif(n, 0, maxgap))},
  maxgap_fn_args=list(maxgap=21)
)
duration_isteroid <- list(
  length_presc = isteroid_presc$days_presc1,
  pchange=0.08,
  prescdays_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  prescdays_fn_args = list(items=c(30, 60, 90, 183),
                           probs=c(0.1, 0.3, 0.2, 0.4))
)



###  Allocate initial prescription details

## Days prescribed
isteroid_presc[, days_presc1 := do.call(duration_isteroid$prescdays_fn, 
                                        c(list(.N), duration_isteroid$prescdays_fn_args))]

### Draw subsequent prescriptions

i<-1
mindatepresc <- min(as.numeric(isteroid_presc$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_isteroid[["start_date"]] <- as.Date(isteroid_presc[[paste0("date_presc", i)]] + 
                                              isteroid_presc[[paste0("days_presc", i)]])
  duration_isteroid[["length_presc"]]<-isteroid_presc[[paste0("days_presc", i)]]
  
  # Use function to draw next prescription
  temp <- draw_next_presc(dates_isteroid, duration_isteroid, NULL, NULL)
  
  # Store new prescription details
  isteroid_presc[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  isteroid_presc[[paste0("days_presc", i + 1)]] <- temp[[2]]
  
  mindatepresc <- min(as.numeric(isteroid_presc[[paste0("date_presc", i + 1)]]))
  i<- i+1
}

###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(isteroid_presc) 
isteroid_presc[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(isteroid_presc), value = TRUE)
days_cols    <- grep("^days_presc", names(isteroid_presc), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(isteroid_presc, id.vars = c("newid","patient_id"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_days <- melt(isteroid_presc, id.vars = c("newid","patient_id"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
isteroid_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","presc"), all=TRUE),
                              list(m_date, m_days))

# Tidy dataset
isteroid_presc_long[, c("newid", "presc") := NULL]
isteroid_presc_long<-isteroid_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(isteroid_presc_long,
         old = c("date_presc", "days_presc"),
         new = c("date", "days"))
isteroid_presc_long[,`:=`(
  drug = "inhaled_steroid",
  strength = NA_character_
)]
rm(isteroid_presc)









##############################################
#  Generate systemic steroid prescriptions   #
##############################################

### Prescription pattern assumed:
###   Not continual use
###   Bouts from 3/5 days to 3/6 weeks. 

#  For other reasons than COPD  
# Gather needed data, keeping only prescriptions, removing duplicate records
ssteroid_presc_all<-comorbid[systemic_steroid==1][, .(patient_id, agein_det, 
                    systemic_steroid_start_date, systemic_steroid_end_date, copd_onset_date)]
ssteroid_presc_all<-unique(ssteroid_presc_all)

ssteroid_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  ssteroid_presc_all[, .(
    patient_id,
    agein_det,
    sstart = systemic_steroid_start_date,
    send   = systemic_steroid_end_date,
    copd_onset_date
  )],
  by = "patient_id",
  all.x = TRUE
)[!is.na(sstart), ]

# If COPD onset exists, truncate steroid end at COPD onset
ssteroid_presc[, `:=`(
  send = fifelse(!is.na(copd_onset_date) & copd_onset_date < send,
                 copd_onset_date, send),
  copd ="pre")
]
ssteroid_presc[, c("copd_onset_date", "agein_det") := NULL]

#  For COPD exacerbations  
ssteroid_copd_presc <- merge(
  patient[, .(patient_id, reg_date, enddate = enddate_init)],
  ssteroid_presc_all[, .(
    patient_id,
    agein_det,
    sstart = systemic_steroid_start_date,
    send   = systemic_steroid_end_date,
    copd_onset_date
  )],
  by = "patient_id",
  all.x = TRUE
)[!is.na(sstart),  ][!is.na(copd_onset_date), ]
rm(ssteroid_presc_all)

# Start post-COPD time at COPD onset 
ssteroid_copd_presc[, `:=`(
  sstart = fifelse(copd_onset_date > sstart,
                 copd_onset_date, sstart),
  copd ="post")
]
ssteroid_copd_presc[, c("copd_onset_date", "agein_det") := NULL]

# Combine pre- and post-COPD, marking the periods
ssteroid_presc <- rbindlist(
  list(ssteroid_presc, ssteroid_copd_presc),
  use.names = TRUE
)
rm(ssteroid_copd_presc)

# Keep only periods ending after registration
ssteroid_presc <- ssteroid_presc[send > reg_date]
ssteroid_presc[, send := fifelse(is.na(send),enddate, send)]
ssteroid_presc[, send := fifelse(send>enddate,enddate, send)]
ssteroid_presc[, sstart := fifelse(sstart<reg_date, reg_date, sstart)]
ssteroid_presc <- ssteroid_presc[send > sstart]
ssteroid_presc[, c("reg_date", "enddate") := NULL]

# Order and deduplicate
setorder(ssteroid_presc, patient_id, sstart)
ssteroid_presc <- unique(ssteroid_presc)

# Generate a multiplicative factor (for gap between prescriptions based on COPD status)
ssteroid_presc[,mult_fact:= fcase(
  copd=="pre", 1,
  copd=="post" & as.numeric(send-sstart)/365.25<5, 2.5,
  copd=="post" & as.numeric(send-sstart)/365.25>=5, 5
)]

# Generate first prescription details
setnames(ssteroid_presc, old = c("sstart"), new = c("date_presc1"))
ssteroid_presc[,days_presc1:=NA]



###  Information required to simulate prescription details

dates_ssteroid  <-list(
  start_date = ssteroid_presc$date_presc1, 
  end_date=ssteroid_presc$send,
  pdisc=0.4,
  maxgap_fn = function(n, startgap, maxgap, mult_fact) {round((startgap + (runif(n, 0, maxgap)-maxgap/2))/mult_fact)},
  maxgap_fn_args=list(startgap=183, maxgap=121, mult_fact=ssteroid_presc$mult_fact)
)
duration_ssteroid <- list(
  length_presc = ssteroid_presc$days_presc1,
  pchange=0.7,
  prescdays_fn = function(n,items,probs) sample(items, size=n, prob=probs, replace=TRUE),
  prescdays_fn_args = list(items=c(3, 4, 5, 21, 28, 42),
                           probs=c(0.1, 0.1, 0.3, 0.4, 0.05, 0.05))
)



###  Allocate initial prescription details

## Days prescribed
ssteroid_presc[, days_presc1 := do.call(duration_ssteroid$prescdays_fn, 
                                        c(list(.N), duration_ssteroid$prescdays_fn_args))]

### Draw subsequent prescriptions

i<-1
mindatepresc <- min(as.numeric(ssteroid_presc$date_presc1))

while(mindatepresc<as.numeric(as.Date("3000-01-01"))) {
  
  # Update current start date and other details
  dates_ssteroid[["start_date"]] <- as.Date(ssteroid_presc[[paste0("date_presc", i)]] + 
                                              ssteroid_presc[[paste0("days_presc", i)]])
  duration_ssteroid[["length_presc"]]<-ssteroid_presc[[paste0("days_presc", i)]]
  
  # Use function to draw next prescription
  temp <- draw_next_presc(dates_ssteroid, duration_ssteroid, NULL, NULL)
  
  # Store new prescription details
  ssteroid_presc[[paste0("date_presc", i + 1)]] <- as.Date(temp[[1]])
  ssteroid_presc[[paste0("days_presc", i + 1)]] <- temp[[2]]
  
  mindatepresc <- min(as.numeric(ssteroid_presc[[paste0("date_presc", i + 1)]]))
  i<- i+1
}

###  Tidy dataset, reshaping long

# Add row number as newid and reshape long
setDT(ssteroid_presc) 
ssteroid_presc[, newid := .I]  

# Identify variable groups by prefix
date_cols    <- grep("^date_presc", names(ssteroid_presc), value = TRUE)
days_cols    <- grep("^days_presc", names(ssteroid_presc), value = TRUE)

# Melt each group separately to preserve types
m_date <- melt(ssteroid_presc, id.vars = c("newid","patient_id", "copd"), measure.vars = date_cols,
               variable.name = "presc", value.name = "date_presc", na.rm = TRUE)
m_days <- melt(ssteroid_presc, id.vars = c("newid","patient_id", "copd"), measure.vars = days_cols,
               variable.name = "presc", value.name = "days_presc", na.rm = TRUE)

# Extract the numeric prescription number from column names
m_date[, presc := as.integer(gsub("\\D", "", presc))]
m_days[, presc := as.integer(gsub("\\D", "", presc))]

# Merge the melted tables together
ssteroid_presc_long <- Reduce(function(x,y) merge(x, y, by=c("newid","patient_id","copd","presc"), all=TRUE),
                              list(m_date, m_days))

# Tidy dataset
ssteroid_presc_long[, c("newid", "presc") := NULL]
ssteroid_presc_long<-ssteroid_presc_long[date_presc!=as.Date("3000-01-01"),]
setnames(ssteroid_presc_long,
         old = c("date_presc", "days_presc"),
         new = c("date", "days"))
ssteroid_presc_long[,`:=`(
  drug = fifelse(copd=="post", "systemic_steroid_copd", "systemic_steroid_other"),
  strength = NA_character_
)][,copd:=NULL]

# Remove unneeded dataset
rm(ssteroid_presc)





###################################
#  Combine prescription datasets  #
###################################

# Append all prescription datasets
medication <- rbindlist(
  list(
    aspirin_presc_long,
    oac_presc_long,
    clopidogrel_presc_long,
    nsaid_presc_long,
    isteroid_presc_long,
    ssteroid_presc_long,
    ppi_presc_long
  ),
  use.names = TRUE
)


# Delete unneeded datasets
rm(ppi_presc_long, aspirin_presc_long, oac_presc_long, 
   clopidogrel_presc_long, nsaid_presc_long, isteroid_presc_long, 
   ssteroid_presc_long)


# NB: COPD steroid dataset used to generate consultations
# so do not delete now: ssteroid_copd_presc_long
