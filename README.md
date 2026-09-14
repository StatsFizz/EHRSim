# EHRSim

EHRSim packages a multi-stage synthetic electronic health record (EHR)
simulation pipeline behind a small set of user-facing R functions. It can
simulate one GP practice, combine several independently simulated practices,
and construct treatment-effect truth under persistent, fixed-course, and
episodic treatment strategies.

EHRSim is research software. Its software UI and treatment mechanisms
are tested here.

## Installation

Install the development version of EHRSim from GitHub:

```r
install.packages("remotes")
remotes::install_github("StatsFizz/EHRSim")
```

Load the installed package with:

```r
library(EHRSim)
```

## Simulate one GP practice

```r
one_gp <- simulate_one_gp(
  pracid = 1L,
  gp_rel_size = 1,
  seed = 34637L,
  write_csv = FALSE
)

names(one_gp$tables)
one_gp$metadata
```

Set `write_csv = TRUE` to write EHR records and data-generating tables. If
`output_dir = NULL`, files are placed under `Simulated_data` in the current
working directory.

## Simulate multiple GP practices

Supply one row per practice. Practice IDs must be unique and every practice
must have a positive relative size and a non-negative integer seed.

```r
gp_specs <- data.frame(
  pracid = 1:3,
  gp_rel_size = c(0.8, 1, 1.2),
  seed = c(34637L, 34638L, 34639L)
)

many_gps <- simulate_many_gps(
  gp_specs = gp_specs,
  write_csv = FALSE,
  keep_gp_results = FALSE,
  save_mode = "both"
)

many_gps$metadata
```

`save_mode` controls written files when `write_csv = TRUE`: `"combined"`
writes merged tables, `"per_gp"` writes separate practice folders, and
`"both"` writes both forms.

## Returned simulation objects

`simulate_one_gp()` returns:

| Element | Contents |
|---|---|
| `gp` | GP-level simulated characteristics. |
| `tables` | A named list of analyst-visible EHR tables. |
| `tables_dg` | A named list of underlying data-generating tables. |
| `metadata` | Practice ID, relative size, seed, and patient count. |
| `output_dir` | The directory selected for saved output. |
| `output_paths` | Written CSV paths, or an empty list when `write_csv = FALSE`. |

`simulate_many_gps()` returns:

| Element | Contents |
|---|---|
| `gp_specs` | The input specification for every simulated practice. |
| `tables` | A named list of combined analyst-visible EHR tables. |
| `tables_dg` | A named list of combined data-generating tables. |
| `metadata` | Number of practices, total patient count, and save mode. |
| `output_dir` | The directory selected for saved output. |
| `output_paths` | Paths to combined CSV files when combined output is written; otherwise an empty list. |
| `gp_results` | Individual practice results when `keep_gp_results = TRUE`; otherwise `NULL`. |

## Treatment-effect truth

All treatment wrappers require `pracid`, `gp_rel_size`, `seed`, and a named
`specs` list. They reproduce the complete simulation for those inputs, preserve
the latent mortality-hazard state at P7, and continue through P13 to construct
the analyst-visible EHR. The treatment engine first identifies latent-eligible
patients and their time zero, then retains patients who are registered and have
not died, transferred, or passed the study end by that date according to the
final P13 tables. A recorded diagnosis code is not required for inclusion.
Counterfactual hazards and RMST are calculated after this observability check.

This aligns follow-up with the EHR while retaining the latent information needed
to calculate treatment truth. The same seed is used for both layers; users do
not need to supply patient IDs or run the latent pipeline separately.

Common specification fields are:

| Field | Purpose |
|---|---|
| `drug_name` | Drug whose treatment strategy is being evaluated. |
| `population` | Label for the target population. |
| `time_zero_col` | Name assigned to the event date that begins follow-up. |
| `affected_cause` | Outcome cause affected by treatment. |
| `affected_hazard_column` | Hazard column modified by treatment. |
| `horizons` | Positive follow-up horizons in years. |
| `stop_trigger_labels` | Human-readable names for optional stopping events. |
| `stop_trigger_cols` | Date columns corresponding to the stopping events. |

Stop-trigger labels and columns must either both be supplied with matching
lengths or both be omitted.

### Persistent strategy

Persistent treatment starts at the qualifying event and otherwise continues
through follow-up, subject to the selected stopping version.

```r
persistent_specs <- list(
  drug_name = "aspirin",
  population = "first_mi_aspirin_naive",
  start_trigger_label = "first_mi",
  strategy_context_label = "after_first_mi",
  start_trigger_col = "mi_event_first_date",
  time_zero_col = "first_mi_date",
  stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
  stop_trigger_cols = c(
    "upper_gi_bleed_event_date",
    "peptic_ulcer_event_date"
  ),
  affected_cause = "non_communicable_death",
  affected_hazard_column = "lnhr_all_1",
  horizons = c(1, 5, 10)
)

persistent_truth <- truth_persistent_course_treatment(
  pracid = 1L,
  gp_rel_size = 1,
  seed = 34637L,
  specs = persistent_specs
)
```

### Fixed-course strategy

Fixed-course specifications additionally require a supported `strategy_type`,
`start_trigger_cols`, and a positive `course_duration_days`.

```r
fixed_specs <- list(
  strategy_type = "fixed_course_once",
  drug_name = "aspirin",
  population = "first_mi_aspirin_naive",
  start_trigger_cols = "mi_event_first_date",
  time_zero_col = "first_mi_date",
  course_duration_days = 365L,
  course_delay_days = 0L,
  affected_cause = "non_communicable_death",
  affected_hazard_column = "lnhr_all_1",
  horizons = c(1, 5, 10)
)

fixed_truth <- truth_fixed_course_treatment(
  pracid = 1L,
  gp_rel_size = 1,
  seed = 34637L,
  specs = fixed_specs
)
```

### Episodic strategy

Episodic strategies can start a new course after each qualifying event. The
restart form allows later events to initiate another course after stopping.

```r
episodic_specs <- list(
  strategy_type = "episodic_course_restart",
  restart_after_stop = TRUE,
  drug_name = "aspirin",
  population = "first_vascular_event_aspirin_naive",
  start_trigger_cols = c(
    "mi_event_first_date",
    "stroke_event_first_date",
    "chd_onset_date",
    "pvd_onset_date",
    "other_athero_onset_date"
  ),
  time_zero_col = "first_vascular_event_date",
  course_duration_days = 365L,
  course_delay_days = 0L,
  stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
  stop_trigger_cols = c(
    "upper_gi_bleed_event_date",
    "peptic_ulcer_event_date"
  ),
  affected_cause = "non_communicable_death",
  affected_hazard_column = "lnhr_all_1",
  horizons = c(1, 5, 10)
)

episodic_truth <- truth_episodic_course_treatment(
  pracid = 1L,
  gp_rel_size = 1,
  seed = 34637L,
  specs = episodic_specs
)
```

## Returned treatment objects

Every treatment wrapper returns the same core elements:

| Element | Contents |
|---|---|
| `strategy_family` | `"persistent"`, `"fixed"`, or `"episodic"`. |
| `specs` | The validated input specification. |
| `eligible` | Patients meeting the strategy eligibility rules. |
| `treatment_cohort` | Follow-up intervals used to construct treatment worlds. |
| `truth_worlds` | Natural, forced-treatment, and forced-no-treatment intervals. |
| `truth_treatment_effects` | Horizon, contrast, survival, metadata, and coefficient tables. |

Episodic results additionally include `triggers` and `episode_triggers` because
individual patients can have multiple treatment-starting events.

Software checks establish reproducibility, structural correctness, interval
consistency, and expected treatment-effect direction under the supplied
coefficients.
