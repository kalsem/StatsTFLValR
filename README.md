# StatsTFLValR <img src="Images/Logo1.png" align="left" height="200" />

<!-- Badges -->
<!-- Replace username with your GitHub ID -->
[![R-CMD-check](https://github.com/kalsem/StatsTFLValR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kalsem/StatsTFLValR/actions)
![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![GitHub issues](https://img.shields.io/github/issues/kalsem/StatsTFLValR)](https://github.com/kalsem/StatsTFLValR/issues)

---

## Overview

**StatsTFLValR** provides reusable, audit-ready utilities for validating clinical trial data across  
**SDTM → ADaM → TFL** workflows.  

The package supports:

- SDTM loading & SUPP domain merging  
- ADaM variable checks, lineage tracing, and structure validation  
- Big-N, population counts, grouping, and percentage formatting  
- TFL-ready summary tables (AE SOC/ PT, Region/Country, Demographics, etc.)  
- Compare-style reports similar to **SAS PROC COMPARE**  
- Logging and traceability for GxP-compliant workflows  

The goal is to standardize and streamline clinical programming validation in R.

---

## Key Features

### 🔹 **SDTM Utilities**
- `stval_read_xpt()` – load XPT consistently  
- `stval_sdtm_merge_supp()` – merge SUPPxx → SDTM  
- `stval_sdtm_check_keys()` – validate STUDYID/USUBJID/domain keys  

### 🔹 **ADaM Validation**
- `stval_adam_check_structure()` – ensure analysis dataset compliance  
- `stval_trace_lineage()` – detailed SDTM → ADaM derivation trace  
- `stval_validate_paramcd()` – PARAMCD/PARAM/AVAL integrity checks  

### 🔹 **TFL Counts**
- `stval_tfl_bigN()` – population Big-N engine  
- `stval_tfl_count_by()` – grouped N (%) summaries  
- `stval_tfl_ae_socpt()` – SOC–PT-level AE table builder  

### 🔹 **Compare Reports**
- `stval_compare_datasets()` – PROC COMPARE emulation  
- `stval_compare_tfl()` – validate output tables  

---

## Installation

### Install development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("kalsem/StatsTFLValR")
