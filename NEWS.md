# EHRSim 0.0.2

* Alcohol-advice eligibility is now evaluated within each clinical interval, so
  harmful alcohol use arising later cannot suppress advice earlier in follow-up.

* Medication-generated consultations and prescription rows now retain a private
  identifier through P9-P13. This replaces independent same-day row-order
  matching and is removed before the user-facing tables are returned.

* Early repeat-prescription adjustments remain anchored to expected supply end,
  but are now bounded by the current supply duration so that short courses
  cannot generate a subsequent issue before the preceding issue date.

* Treatment-truth cohorts now require P7 latent eligibility and observability
  in P13 at time zero, based on registration, transfer, linked death, and study
  end dates. Diagnosis recording no longer controls inclusion.
  
* Removed the redundant public cohort object and detailed cohort metadata;
  treatment results retain one concise `cohort_basis` metadata field.

# EHRSim 0.0.1

* Initial development version of EHRSim.
* Added wrapper functions for simulating one GP practice and multiple GP practices.
* Added bundled simulation pipeline scripts.
* Added package documentation, tests, README, and basic build checks.
