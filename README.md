# CUSUM Anomaly Detection - Monthly Time Series

This project demonstrates how CUSUM control charts can be used to detect unusual shifts in monthly time-series data.

The original analysis uses monthly premium data across organizational units, but the method is transferable to other operational monitoring problems where the goal is to flag unusual changes over time.

## What This Project Demonstrates

- Converting tabular monthly data into time-series form
- Normalizing values before comparing patterns across units
- Computing baseline mean and standard deviation for each series
- Applying CUSUM control charts to detect process shifts
- Extracting violations that exceed control limits
- Using statistical monitoring to prioritize follow-up review

## Problem

Operational metrics can shift gradually or suddenly across different units. Manual inspection is hard when many time series need review. This project applies a reproducible statistical process control method to identify series with unusual movement.

## Method

| Step | Purpose |
|---|---|
| Data normalization | Put monthly series on a comparable scale |
| Time-series construction | Convert monthly columns into sequential observations |
| Baseline calculation | Estimate mean and standard deviation for each unit |
| CUSUM charting | Detect sustained shifts from the baseline process |
| Violation extraction | Identify units and periods that exceed control limits |

## Workflow

```text
Monthly tabular data
  -> Min-Max normalization
  -> matrix and time-series conversion
  -> baseline mean and standard deviation
  -> CUSUM control chart
  -> violation list for follow-up
```

## Current Repository Notes

- Raw Excel data is not included.
- The script expects a local Excel file path from the original analysis environment.
- The current code is a compact analysis script, not a packaged command-line tool.

To reproduce with a new dataset, update the `read_excel()` path and sheet name in `cusum_revised_v2.R`.

## How to Run

Install the required R packages:

```r
install.packages(c("qcc", "readxl"))
```

Then update the local data path and run:

```bash
Rscript cusum_revised_v2.R
```

## Next Improvements

- Replace the hardcoded Excel path with a configurable input parameter
- Save CUSUM violation results to `outputs/violations.csv`
- Add plots to an `outputs/figures/` folder
- Add a synthetic sample dataset for reproducible demonstration
- Wrap the CUSUM workflow into a reusable function

## Tech Stack

`R` · `qcc` · `readxl`
