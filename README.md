# CUSUM Anomaly Detection — Insurance Premium Sales

Applies the **CUSUM (Cumulative Sum Control Chart)** statistical method to detect anomalies in monthly insurance premium sales across subsidiaries. Designed to flag branches with abnormal sales patterns that may warrant audit attention.

## Method

1. **Data Normalization** — Min-Max scaling applied to monthly premium time series per branch
2. **Time Series Construction** — Converted tabular data into time series format for sequential analysis
3. **CUSUM Control Chart** — Applied via the `qcc` R package to detect shifts in the process mean
4. **Anomaly Flagging** — Branches where the cumulative sum exceeds control limits are flagged for review

## Use Case

Internal audit support — provides a systematic, statistical basis for selecting which subsidiaries to investigate, replacing ad-hoc judgment with a reproducible method.

## Tech Stack

`R` · `qcc` · `readxl`
