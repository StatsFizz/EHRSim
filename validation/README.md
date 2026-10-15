# EHRSim pipeline audit workspace

This directory contains the audit trail, controlled mechanism checks and
scientific-validation plan for the EHRSim data-generating pipeline. It is
separate from `tests/testthat`, which checks package interfaces,
input validation and specified structural behaviour.

The material establishes what the implementation does and records identified
defects, corrections and retained assumptions.

## Contents

- `audits/PIPELINE_AUDIT_REGISTER.md`: integrated script-by-script audit,
  remediation record, current findings and validation limits.
- `audits/BASELINE_2026-08-03.md`: fixed-seed evidence from the audited pipeline.
- `audits/run_priority_audit.R`: reruns the priority mechanism checks.

The Markdown register is the maintained audit record. Generated HTML previews
are not versioned and must be regenerated from the current Markdown before
use; an older local preview may describe defects that have since been fixed.

## Running the priority audit

From the EHRSim package root:

```r
source("validation/audits/run_priority_audit.R")
```

The default controlled scenario is one relative-size practice with
`pracid = 1` and `seed = 34637`. The script reports metrics; it does not modify
the pipeline or treat known baseline failures as package-test failures.

## Interpretation

Passing `R CMD check` or `testthat` demonstrates software integrity against the
specified checks, not epidemiological validity. The integrated register keeps
historical fixes separate from currently reproducible findings and lists the
additional evidence needed for scientific validation.
