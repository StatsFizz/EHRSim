# EHRSim pipeline audit and remediation register

## Purpose and limits

This register consolidates the detailed manual audit of the inherited simulation
scripts with the later controlled audit, remediation record, package tests and
treatment-truth extension. It covers implementation correctness, consistency
between pipeline stages, temporal logic, data integrity and scientifically
relevant mechanism choices.

This is a pipeline audit, not a claim of complete scientific validation. Source
inspection and successful tests can establish what the software does and whether
specified invariants hold. They cannot establish epidemiological realism,
external validity or estimator performance without calibration and simulation
studies against prespecified targets.

## Document control

- Current package version: 0.0.2
- Integrated register date: 2026-09-14
- Audited scope: package wrappers and P0a-P13 bundled pipeline
- Controlled scenario: `pracid = 1`, `gp_rel_size = 1`, `seed = 34637`
- Historical quantitative baseline: `BASELINE_2026-08-03.md`
- Historical working source: 33-page Script Audit dated 2026-07-09

The historical baseline is intentionally preserved. Since that baseline, P0c
now bounds early-refill adjustments, P0f restricts the treatment cohort to
patients observable in the final P13 EHR at latent time zero, P9 uses
interval-specific alcohol-advice eligibility and carries a private prescription
key, P12 retains that key during code allocation, and P13 uses it for exact
prescription-to-consultation linkage. The remediation entries below record these
changes.

## Evidence used

The integrated audit draws on five evidence types:

1. Manual, script-by-script inspection of the inherited driver and P0a-P13.
2. Direct inspection of the current 0.0.2 package source.
3. A deterministic controlled audit run using the inputs above.
4. Targeted `testthat` checks of package interfaces, treatment mechanics,
   reproducibility, mathematics, validation and EHR cohort restriction.
5. A clean package check and external installation smoke test.

The controlled priority audit was rerun on 2026-09-14. It confirmed that
interval-specific alcohol eligibility permits pre-onset advice and that exact
prescription linkage eliminates the previously observed same-day mismatch.
Because the alcohol correction generates additional consultations and consumes
additional random draws, later P10-P13 counts were regenerated rather than
assumed to equal the historical baseline.

## Status definitions

- `VERIFIED-FIXED`: the historical defect is absent from current source and the
  intended correction is visible or tested.
- `OPEN-CONFIRMED`: the mechanism is present in current source and reproduced by
  source inspection or the controlled audit.
- `RETAINED-ASSUMPTION`: the implementation is being retained as a modelling
  choice; this does not imply external validation.
- `RATIONALE-TO-CONFIRM`: current behaviour is clear, but the final rationale
  selected during development is not recoverable from source or tests alone.
- `CALIBRATION-REQUIRED`: appropriateness depends on external evidence or a
  prespecified scientific target.
- `STRUCTURAL-PASS`: a defined integrity property passed in the controlled run.
- `DESCRIPTIVE`: contextual output that is neither a pass nor a failure alone.
- `HISTORICAL`: retained for provenance and not a description of an open defect.

## Pipeline map

| Stage | Current role |
|---|---|
| Legacy `_Simulate.R` | Historical driver superseded by package wrappers. |
| P0a | Defines comorbidity logit, indicator and history/onset helpers. |
| P0b | Defines coefficient templates and condition-specific parameter lists. |
| P0c | Defines medication groups and repeat-prescription helpers. |
| P0d | Defines cause-specific mortality log-hazard parameters. |
| P0e | Defines clinical, entity and prescription code dictionaries and weights. |
| P0f | Defines treatment-strategy, counterfactual hazard and RMST machinery. |
| P1 | Generates practice-level characteristics. |
| P2 | Generates patients and initial follow-up boundaries. |
| P3 | Generates latent frailty histories. |
| P4 | Generates latent longitudinal clinical measures. |
| P5a | Generates cardiometabolic disease histories. |
| P5b | Generates other diseases and CKD stages. |
| P5c | Converts interval disease states into event and onset dates. |
| P6a | Generates medication exposure windows. |
| P6b | Expands exposure windows into prescription issues. |
| P7 | Constructs time-varying hazards and simulates death and transfer. |
| P8 | Preserves data-generating tables and censors analyst-facing working tables. |
| P9 | Generates consultations from latent histories and medication issues. |
| P10 | Generates observed clinical measurements and recording errors. |
| P11 | Adds clinical and entity codes. |
| P12 | Adds prescription product codes. |
| P13 | Adds final EHR errors and extracts released tables. |

## Verified historical remediation

These corrections were made during the initial script audit and remain present
in the current package. Cosmetic comment and header edits are omitted; this
table records changes capable of affecting execution, interpretation or data
integrity.

| ID | Stage | Historical problem | Current correction and evidence | Status |
|---|---|---|---|---|
| FIX-DRV-01 | Driver | Pipeline execution depended on a legacy top-level runner and working-directory assumptions. | `simulate_one_gp()` and `simulate_many_gps()` now locate bundled scripts with `system.file()` and run them in order. | VERIFIED-FIXED |
| FIX-P0A-01 | P0a | Disease predictors used shortened or nonexistent coefficient names, including diabetes, GI disease and cancer histories. | Lookups now use the explicit event/history names present in P0b and the comorbidity table. | VERIFIED-FIXED |
| FIX-P0A-02 | P0a | A character column name was used as a value when adding medication effects to an existing logit. | `get(logitvar)` retrieves and updates the numeric predictor column. | VERIFIED-FIXED |
| FIX-P0B-01 | P0b | Misspelled coefficient names could create unused list entries while leaving intended coefficients unchanged; the dementia list typo left the intended list at defaults. | Current templates and updates use names such as `diabetes_prev`, `fage`, `dementia_coefs`, `hpylori_event`, `copd_prev`, `ht_prev` and `inhaled_steroid`. | VERIFIED-FIXED |
| FIX-P0C-01 | P0c | H2RA selection ignored its intended probability weights. | `choose_ppi_h2ra()` supplies `prob_h2ra` to `sample()`. | VERIFIED-FIXED |
| FIX-P0C-02 | P0c | Optional repeat-prescription components defaulted to empty lists although the helper tested for `NULL`. | `drug_change` and `strength` now default to `NULL`, matching control flow. | VERIFIED-FIXED |
| FIX-P0C-03 | P0c/P6b | P6b correctly anchored refill adjustments at expected supply end, but a five-day NSAID course combined with an adjustment below -5 days could still generate the next issue before the preceding issue date. | `draw_next_presc()` now bounds the negative adjustment by the current supply duration. Early refill overlap is retained, including possible same-day issue for very short courses, while issue dates cannot move backwards. Focused tests cover both 28-day and five-day courses. | VERIFIED-FIXED |
| FIX-P0E-01 | P0e | Code/frequency objects were assembled through fragile placeholders and a diabetes frequency object sat outside the expected hierarchy. | Named `codes` and `codes_freq_used` containers are used consistently. | VERIFIED-FIXED |
| FIX-P0E-02 | P0e | Esomeprazole had a code/weight length mismatch. | Four product codes now have four corresponding weights. | VERIFIED-FIXED |
| FIX-P1-01 | P1 | `table()` indexing could shift ethnicity categories when a category was absent from 100 practice-level draws. | `tabulate(..., nbins = 5)` preserves all category positions. | VERIFIED-FIXED |
| FIX-P1-02 | P1 | A rare draw could produce `max_reg <= min_reg`. | A guard moves `min_reg` back one year when the ordering fails. | VERIFIED-FIXED |
| FIX-P2-01 | P2 | Patient-ID and chronology checks printed results but did not stop invalid simulations. | Hard failures now enforce unique IDs, birth before registration and exact adult registration-age bounds. | VERIFIED-FIXED |
| FIX-P2-02 | P2 | Year-only age construction could leave patients younger than 18 or older than 95 on their exact registration date. | DOB is clamped using date-based 18- and 95-year boundaries. | VERIFIED-FIXED |
| FIX-P4-01 | P4 | Strict/open trajectory boundaries omitted edge values and an earlier random rate draw was overwritten. | Lower bounds are inclusive, upper bounds explicit, missing trajectories default safely and only the retained rate draw remains. | VERIFIED-FIXED |
| FIX-P4-02 | P4 | The minimum alcohol propensity could fall outside the first quartile. | `cut(..., include.lowest = TRUE)` includes the full observed range. | VERIFIED-FIXED |
| FIX-P4-03 | P4 | BMI-dependent smoking logic contained uncovered BMI ranges. | Current probability rules cover the intended adjoining BMI groups. | VERIFIED-FIXED |
| FIX-P5B-01 | P5b | GERD and gastric-cancer additional effects used coefficient lists belonging to other conditions. | Both calls now use their own `gerd_coefs` and `cancer_gastric_coefs`. | VERIFIED-FIXED |
| FIX-P5B-02 | P5b | CKD first/change indicators did not restart their row sequence for each patient. | Lag, first onset and change calculations are grouped by `patient_id`. | VERIFIED-FIXED |
| FIX-P5C-01 | P5c | Row-wise `pmin()` did not calculate a patient's earliest CKD date across intervals and stages. | Stage dates are combined and minimised within patient. | VERIFIED-FIXED |
| FIX-P5C-02 | P5c | Reconstructed interval endpoints could drift beyond `enddate_init`. | `dateout` is clamped to the patient-level boundary before dates are sampled. | VERIFIED-FIXED |
| FIX-P6A-01 | P6a | All-missing date summaries could create infinite sentinel values. | Explicit first/last-date helpers return missing dates when every candidate is missing. | VERIFIED-FIXED |
| FIX-P6A-02 | P6a | A second aspirin indication draw could overwrite prior use and random start-date arithmetic rounded the random fraction before multiplication. | Existing aspirin use is retained and date sampling now spans the interval. | VERIFIED-FIXED |
| FIX-P6A-03 | P6a | Aspirin, clopidogrel and OAC stopping conditions were inverted or confined to the interval containing treatment initiation. | Valid post-initiation GI events shorten treatment and clopidogrel stop candidates are searched across patient history. | VERIFIED-FIXED |
| FIX-P6A-04 | P6a | OAC row-level use did not remain active after first initiation. | Patient-grouped cumulative use carries OAC forward. | VERIFIED-FIXED |
| FIX-P6A-05 | P6a | PPI/H2RA class choice reused indication coefficients and exposure lacked an explicit episodic window. | Class choice uses `ppivsh2ra_coefs`; PPI and H2RA receive interval-bounded start/end dates. | VERIFIED-FIXED |
| FIX-P7-01 | P7 | The CKD helper date could enter the comorbidity hazard event stream in addition to stage-specific events. | `ckd_first` is excluded from the melted hazard events. | VERIFIED-FIXED |
| FIX-P7-02 | P7 | Medication names absent from `drug_groups` could disappear silently from hazard mapping. | Unmatched drugs cause a clear error. | VERIFIED-FIXED |
| FIX-P7-03 | P7 | Episode splitting considered only the immediately previous grace period and could split overlapping coverage incorrectly. | A patient/drug running maximum of `grace_end` defines episode continuity. | VERIFIED-FIXED |
| FIX-P8-01 | P8 | Cleanup occurred before checks and risked removing fields needed to verify censoring. | Cleanup occurs after data-generating copies and analyst-facing censoring are constructed. | VERIFIED-FIXED |
| FIX-P9-01 | P9 | Transferred-in chronic disease did not initiate annual reviews. | Both `disease_onset` and `data_transfer` feed the annual-review population. | VERIFIED-FIXED |
| FIX-P9-02 | P9 | The empty-diabetes fallback created the COPD object. | It now creates an empty `consult_diab` table with the expected schema. | VERIFIED-FIXED |
| FIX-P9-03 | P9 | Patients without a later annual review received no general health-check endpoint. | Missing review dates fall back to patient follow-up end. | VERIFIED-FIXED |
| FIX-P9-04 | P9 | Global `.I` was used where the first row within each patient was intended. | Patient-grouped `seq_len(.N)` is used for harmful-alcohol and background-contact logic. | VERIFIED-FIXED |
| FIX-P9-05 | P9 | Smoking-cessation intervals crossing COPD onset could generate nominally pre-COPD contacts after onset. | The interval endpoint is clipped to COPD onset before appointment dates are sampled. | VERIFIED-FIXED |
| FIX-P9-06 | P9 | Contraception/HRT retention probabilities were reversed and HRT timing relied on the last retained consultation rather than the latent pill end. | Approximately 85% are retained and HRT uses the simulated pill end date. | VERIFIED-FIXED |
| FIX-P9-07 | P9 | Whole-history `max(harmalc)` excluded all earlier medium/high-risk intervals from ordinary alcohol advice when a patient became harmful later. | Ordinary alcohol-advice eligibility now uses `harmalc == 0` in each interval. Harmful-use follow-up remains anchored at first harmful onset; the controlled run now contains advice before later onset. | VERIFIED-FIXED |
| FIX-P10-01 | P10 | Joined alcohol fields referenced the receiving columns instead of `i.` columns. | Joined values are taken explicitly from the clinical-measure source. | VERIFIED-FIXED |
| FIX-P10-02 | P10 | BMI was not emitted as its own long-format measure. | Height, weight and BMI are constructed as separate long-format rows. | VERIFIED-FIXED |
| FIX-P10-03 | P10 | Sequential category mutation could reverse an error introduced by an earlier statement. | Misclassification is anchored to an original stored category. | VERIFIED-FIXED |
| FIX-P10-04 | P10 | A line break excluded the practice-ID length from ID-padding arithmetic. | The complete expression is evaluated as one statement. | VERIFIED-FIXED |
| FIX-P10-05 | P10 | Weight rounding probabilities and harmful-alcohol consultation measurement lists did not match the generation specification. | Current bands represent 60%, 10% and 30% rounding groups, and harmful-alcohol contacts are included in the relevant lists. | VERIFIED-FIXED |
| FIX-P11-01 | P11 | Transferred disease history was omitted from disease-specific coding and hypertension follow-up matched the wrong trigger text. | `data_transfer` is coded with disease events and matching uses `ht_event`. | VERIFIED-FIXED |
| FIX-P11-02 | P11 | BMI rows created by P10 could be dropped because only `measure == "weight"` was consumed. | Both weight and BMI enter coded and supplementary output construction. | VERIFIED-FIXED |
| FIX-P12-01 | P12 | Reuse of a previous product code referenced the original vector rather than the progressively updated selection. | A sequential helper carries the final prior code forward within patient and drug. | VERIFIED-FIXED |
| FIX-P13-01 | P13 | Patients without consultations retained a missing count, propagating missing ethnicity-recording probabilities. | Missing consultation counts are set to zero. | VERIFIED-FIXED |
| FIX-P13-02 | P13 | GP-record death timing and transfer timing were not reconciled consistently. | GP death dates on or after transfer are removed while official linked mortality remains separate. | VERIFIED-FIXED |
| FIX-P13-03 | P13 | Joining prescriptions to same-day consultations could cause a many-to-many row explosion. | The interim within-day row-index solution preserved prescription row count; exact semantic pairing was subsequently resolved by FIX-P13-06. | VERIFIED-FIXED |
| FIX-P13-04 | P13 | Dementia/cancer delay formulas could produce invalid probabilities. | Delay probabilities use `plogis()`. | VERIFIED-FIXED |
| FIX-P13-05 | P13 | Post-transfer/death removal overwrote earlier hypertension-removal flags. | Removal conditions are combined rather than replaced. | VERIFIED-FIXED |
| FIX-P13-06 | P9/P12/P13 | Independently ordered within-day row indices could pair a prescription with the wrong medication-generated consultation type. | P9 assigns a private prescription identifier, P12 carries it through product-code allocation, and P13 joins using that identifier plus patient/date before removing it from released tables. Duplicate, unmatched and row-count-changing linkage now fail explicitly; the controlled mismatch count fell from 22 to 0. | VERIFIED-FIXED |

## Current controlled audit

The priority audit was rerun against 0.0.2 on 2026-09-14. The following metrics
were reproduced. A nonzero value is not automatically a defect: interpretation
depends on the stated target and status.

| ID | Metric | Current result | Status |
|---|---|---:|---|
| CAL-006 | Health checks after age 74 | 38 | CALIBRATION-REQUIRED |
| DES-015 | Missing alcohol-status rows retaining harmful-use `data2` | 833 | RATIONALE-TO-CONFIRM |
| DES-017 | Source medication rows whose strength is not released in P12 | 86 | RETAINED-ASSUMPTION |
| P12-STRUCTURE | Medication rows lost during code allocation | 0 | STRUCTURAL-PASS |
| SCI-002 | COPD patients with contacts before the later steroid history used to classify consultation rate | 19 | OPEN-CONFIRMED |
| SCI-003a | Eventual harmful drinkers with earlier medium/high intervals | 192 | DESCRIPTIVE |
| SCI-003b | Those patients receiving alcohol advice before harmful onset | 4 | VERIFIED-FIXED |
| SCI-004a | Never-smoker recording probability at surgery | 30.3% | OPEN-CONFIRMED |
| SCI-004b | Non-drinker recording probability at surgery | 32.2% | OPEN-CONFIRMED |
| SCI-005 | Direct BMI rows assigned the generic weight code | 3,784 | OPEN-CONFIRMED |
| SCI-006 | Results rows using an entity value as the clinical code | 2,900 | OPEN-CONFIRMED |
| SCI-007a | Unrelated rows caught by the cancer/dementia code collision | 129 | OPEN-CONFIRMED |
| SCI-007b | Unrelated rows delayed as diagnoses | 52 | OPEN-CONFIRMED |
| SCI-008a | Clinical dates differing from their consultation dates | 676 | OPEN-CONFIRMED |
| SCI-008b | Delayed cancer diagnoses retaining an original-date register row | 624 | OPEN-CONFIRMED |
| SCI-009 | Prescriptions paired to the wrong internal same-day contact type | 0 | VERIFIED-FIXED |
| SCI-010a | Hypertension rows removed using lifetime consultation use | 14 | OPEN-CONFIRMED |
| SCI-010b | Patients made completely unobserved for hypertension | 11 | OPEN-CONFIRMED |

The controlled run also passed the following structural checks:

- No final table referenced an unknown patient.
- Every clinical and prescription row referenced an existing consultation.
- Nonzero supplementary links resolved in both directions.
- Patient, consultation and supplementary identifiers were unique.
- No exact final clinical or prescription duplicates were found.
- No prescription occurred before registration or after the truth-layer endpoint.
- Every diagnosis consultation received a configured condition code in P11.
- Eligible cancer, CHD, stroke and diabetes diagnoses received register rows.

## Open confirmed mechanisms

These findings were identified after the historical remediation above. The
controlled rerun demonstrates that they are still present and must not be
described as fixed.

| ID | Priority | Stage | Current mechanism and consequence | Required resolution |
|---|---|---|---|---|
| SCI-001 | P0 | P0a/P5a/P5b | `genprevonset()` uses `cummax(current)` on the current row. First onset therefore has `*_prev = 1`, and later conditions generated in the same interval can use that state. | Confirm inclusive end-of-interval semantics or change to lagged history; record the decision and add a direct test. |
| SCI-002 | P0 | P9 | COPD consultation intensity uses steroid prescriptions accumulated over later COPD follow-up, allowing future prescribing to affect earlier observation intensity. | Use time-available steroid history or an independent latent severity process. |
| SCI-004 | P1 | P10 | Surgery rows retain the 30% `maybe` flag when the 15% `occasionally` flag is added for never-smokers/non-drinkers. | Reset mutually exclusive tiers or explicitly retain the approximately 30% target. |
| SCI-005 | P0 | P11 | Direct numerical BMI with missing BMI category receives the generic weight code/entity. | Preserve measure identity and use an explicit numerical BMI code/entity. |
| SCI-006 | P0 | P11 | Results contacts use `codes$entity$test_result` as `clinical_code`, mixing namespaces. | Add or designate a clinical result code and keep entity values separate. |
| SCI-007 | P0 | P0e/P13 | Code 1624 is shared by normal BMI and other cancer, while diagnosis transformations select on code alone. | Select using diagnosis context/entity and validate dictionary collisions where semantic uniqueness is required. |
| SCI-008 | P0 | P11/P13 | Diagnostic delay moves clinical event dates without moving the linked consultation or register row. | Define whether `event_date` may differ intentionally; otherwise move the complete observed episode. |
| SCI-010 | P0 | P13 | Hypertension visibility depends on total consultations over complete follow-up, allowing future use to affect earlier recording. | Use consultation history available by the event date or a baseline recording propensity. |
| SCI-012 | P1 | P9/P11 | Medication-generated surgery contacts receive generic diagnosis codes sampled independently of the originating drug/indication. | Condition codes on indication or represent the contact as generic medication activity. |

## Retained modelling assumptions

The following are not presented as coding failures. They describe current
behaviour retained in the pipeline. `RATIONALE-TO-CONFIRM` identifies cases
where source inspection verifies the behaviour but cannot recover the final
scientific rationale selected by the package author.

| ID | Status | Stage | Retained behaviour and interpretation |
|---|---|---|---|
| DES-001 | RETAINED-ASSUMPTION | P3 | Frailty begins at age 18, changes at broad age thresholds and is monotonic. It is a latent cumulative vulnerability construct rather than a fluctuating measured score. |
| DES-002 | RETAINED-ASSUMPTION | P4 | Weight, smoking and alcohol evolve in age intervals; harmful-alcohol history is irreversible and consultations do not feed back into latent behaviour. |
| DES-003 | RATIONALE-TO-CONFIRM | P5a/P5b | Diseases are generated sequentially and inclusive current-row history creates directional same-interval dependencies. |
| DES-004 | RETAINED-ASSUMPTION | P5a | Hypertension is semi-persistent and can fluctuate after prior history, representing repeated elevated-blood-pressure states rather than a strictly irreversible diagnosis. |
| DES-005 | RETAINED-ASSUMPTION | P5b | CKD stages are assigned by relative quantiles and cannot improve once reached. This fixes relative stage prevalence rather than allowing absolute prevalence to emerge. |
| DES-006 | RATIONALE-TO-CONFIRM | P5b | GERD can enable Barrett's oesophagus in the same interval because history includes the current interval. |
| DES-007 | RETAINED-ASSUMPTION | P5c | Exact disease dates are sampled uniformly after interval states are generated; explicit priority rules resolve some same-interval GI/endoscopy triggers. |
| DES-008 | RETAINED-ASSUMPTION | P6a/P6b | Some therapies are long-term and others episodic; a prescription issued within a window can supply medication beyond the nominal window end. |
| DES-009 | RETAINED-ASSUMPTION | P7 | Mortality has three broad competing causes and transfer is generated independently from clinical state using a practice-level rate. |
| DES-010 | RETAINED-ASSUMPTION | P8 | Events on the exact death/transfer day are retained, and clinical-measure intervals are removed by start date rather than clipped. |
| DES-011 | RETAINED-ASSUMPTION | P9 | Annual review begins from the earliest chronic condition rather than generating separate reviews for every condition. |
| DES-012 | RETAINED-ASSUMPTION | P5a/P9 | Hypertension generates repeated event contacts rather than joining the chronic annual-review pathway. |
| DES-013 | RETAINED-ASSUMPTION | P9 | Contraception and HRT create consultation-style activity but are not full medication-truth rows comparable with P6 therapies. |
| DES-014 | RETAINED-ASSUMPTION | P10 | Zero and -1 are deliberate raw-EHR invalid/missing sentinels rather than conventional R missing values. |
| DES-015 | RATIONALE-TO-CONFIRM | P10 | Harmful-use `data2` can remain observed when general alcohol status is replaced by a missing sentinel. |
| DES-016 | RATIONALE-TO-CONFIRM | P11 | Removal of numerical information for BMI-category records is selected at patient level, while smoking/alcohol missingness is applied at record level. |
| DES-017 | RETAINED-ASSUMPTION | P12/P13 | Released prescription rows prioritise product codes and omit latent drug name, indication and strength; uncensored medication truth remains in the data-generating tables. |
| DES-018 | RETAINED-ASSUMPTION | P13 | Error-prone GP death dates can disagree with valid consultation history; official linked mortality is retained separately. |

## Treatment-truth extension

P0f and the three public treatment wrappers were added after the initial legacy
script audit. They implement persistent, fixed-course and episodic strategy
families over a shared latent mortality history.

Verified treatment-layer properties include:

- The wrapper reproduces P0-P7, preserves the latent hazard state, continues
  through P8-P13, constructs the final EHR observation window, then restores
  the P7 state for counterfactual calculations.
- The public cohort is the intersection of P7 latent eligibility and presence in
  the P13 EHR at latent time zero, with registration begun and transfer, linked
  death and study end not already passed.
- Diagnosis recording is not used as a second eligibility gate.
- Natural, forced-treatment and forced-no-treatment worlds use the same eligible
  patients and retained hazard histories.
- Persistent, fixed and episodic wrappers return a common core structure;
  episodic output additionally retains trigger and episode-trigger tables.
- Fixed-course treatment respects scheduled start/end boundaries; treatment is
  binary at interval level.
- Episodic triggers can generate overlapping/extended courses, with restart
  behaviour controlled by the strategy specification.
- Counterfactual survival, risk and RMST calculations are bounded and preserve
  the expected direction under beneficial treatment coefficients.
- Repeated calls with the same inputs and seed are reproducible under the tested
  package and dependency environment.

Interpretation limits:

- Treatment truth is simulator-defined for the selected strategy and cohort; it
  is not an empirically identified effect from the released EHR alone.
- Transfer after time zero currently limits EHR observation but does not redefine
  the latent biological treatment strategy.
- Scientific recovery, bias and coverage results must be regenerated after the
  0.0.2 EHR-observable cohort change.
- Reproducibility across future R/dependency versions requires the software
  environment to be recorded in addition to the seed and inputs.

## Calibration and scientific-validation queue

| ID | Status | Area | Evidence still required |
|---|---|---|---|
| CAL-001 | CALIBRATION-REQUIRED | Practice and patient population | Practice size, deprivation, ethnicity, registration year, age and sex distributions. |
| CAL-002 | CALIBRATION-REQUIRED | Clinical trajectories | BMI, smoking, alcohol and frailty prevalence, transitions and correlations. |
| CAL-003 | CALIBRATION-REQUIRED | Disease model | Age/sex-specific prevalence, incidence, recurrence, multimorbidity and temporal ordering. |
| CAL-004 | CALIBRATION-REQUIRED | Medication model | Indication, initiation, discontinuation, switching, duration and calendar trends. |
| CAL-005 | CALIBRATION-REQUIRED | Mortality | Overall and cause-specific mortality, hazard directions and survival calibration. |
| CAL-006 | CALIBRATION-REQUIRED | Consultation process | Contacts per person-year, consultation mix, multimorbidity gradients and calendar change. |
| CAL-007 | CALIBRATION-REQUIRED | Measurement process | Recording frequency, missingness, misclassification, duplicates, units and extreme values. |
| CAL-008 | CALIBRATION-REQUIRED | Code dictionaries | Provenance, semantic uniqueness, relative frequencies and calendar availability. |
| CAL-009 | CALIBRATION-REQUIRED | Final EHR errors | Ethnicity/IMD missingness, diagnostic delay, underdiagnosis and death-date disagreement. |
| CAL-010 | CALIBRATION-REQUIRED | Treatment truth | Multi-seed treatment-state stability, known-effect recovery, estimator bias and coverage. |

## Package-level verification

The package currently provides five exported functions:

- `simulate_one_gp()`
- `simulate_many_gps()`
- `truth_persistent_course_treatment()`
- `truth_fixed_course_treatment()`
- `truth_episodic_course_treatment()`

Current verification establishes that:

- bundled pipeline scripts are located without personal absolute paths;
- one- and many-practice simulations run through the full pipeline;
- treatment wrappers validate inputs and internal state;
- the test suite passes;
- `R CMD check` completes with zero errors, warnings and notes in the verified
  local environment; and
- the installed package can be loaded and its exported functions called without
  manually sourcing package files.

These package checks establish software integrity, not the completion of the
scientific-validation queue above.

## Acceptance gates and current state

| Gate | Current state |
|---|---|
| Historical remediation documented | Complete for the substantive corrections recovered from the detailed audit. |
| Controlled mechanism audit | Reproducible; SCI-003 and SCI-009 are remediated, while the remaining listed mechanisms or decisions are unresolved. |
| Structural package tests | Passing in the verified local environment. |
| Package check | Zero errors, warnings and notes in the verified local environment. |
| External installation smoke test | Completed locally. |
| Multi-seed pipeline stability | Not demonstrated by the repository audit material. |
| External epidemiological calibration | Not completed. |
| Treatment-effect recovery after cohort revision | Must be regenerated for 0.0.2. |
| GitHub release plumbing | Repository initialisation, CI, remote metadata and release tag remain outstanding. |
| Licensing | MIT is declared; ownership/permission for inherited code must be confirmed before public release. |

## Reproducing the audit

From the EHRSim package root, run:

```r
source("validation/audits/run_priority_audit.R")
```

The script reports controlled metrics and does not modify the pipeline. Package
tests and checks can then be run with:

```r
devtools::test()
devtools::check()
```

## Items requiring author confirmation

Source inspection cannot establish the reasoning behind a retained choice. The
following rationales should be confirmed before the register is treated as
final:

1. Whether inclusive current-row `*_prev` history was deliberately selected as
   an end-of-interval state rather than merely left unchanged.
2. Whether same-interval GERD-to-Barrett's dependence is intended.
3. Whether retaining harmful-alcohol `data2` when general status is missing is
   an intentional representation of independently recorded information.
4. Whether patient-level removal of BMI supplementary values, rather than
   record-level removal, is the intended 4% mechanism.
5. Whether diagnosis `event_date` is deliberately allowed to differ from its
   parent consultation date and original-date register entry.
Until these are confirmed, the register records current implementation without
claiming that each behaviour is the preferred scientific model.
