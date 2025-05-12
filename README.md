# Biomedical Informatics Project: Hospital Data Analysis

This project analyzes healthcare data from the Horton General Hospital dataset to identify patterns and anomalies in patient care metrics. Through statistical modeling and time series analysis, models were developed to detect unusual patterns potentially related to quality or safety issues.

## Project Structure

```
.
├── analysis/              # R scripts for data analysis
├── data/                  # Raw and processed data files
├── final_visualizations/  # Generated plots and visualizations used in final report
├── functions/             # Helper functions
├── old_figures/           # Raw Rplots
└── reports/               # Generated reports and documentation
```

## Data Sources

**Horton General Hospital Dataset**
- Monthly emergency room admissions (1999–2011)
- Critical care events (cardio-respiratory arrests, respiratory arrests, hypoglycaemic arrests)
- Ben Geen incident period (December 2003 – February 2004) for validation

## Note on Visualizations

The repository contains two directories of visualizations:
- **old_figures/**: Visualizations created during the exploratory phase
- **final_visualizations/**: Visualizations used in the final analysis

Two WHO-related visualizations appear in both directories but were not included in the final report:
- `ii.top_regional_life_expectancy.png`
- `viii.global_health_indicators.png`

These were created during an early attempt to integrate WHO Global Health Observatory data with the hospital dataset. Integration was not successful due to non-overlapping time periods and geographical differences. These visualizations are retained for transparency but were not used in the final analysis.

## Analysis Components

### Exploratory Data Analysis
- Time series visualization of admission patterns
- Seasonal decomposition of hospital metrics
- Correlation analysis between critical events and admissions
- Statistical summaries by time period and event type

### Time Series Modeling
- ARIMA models for trend and seasonality
- ETS models for level, trend, and seasonal components
- Poisson regression for count-based critical care events
- Negative binomial models for overdispersed count data

### Anomaly Detection
- Z-score analysis for point anomalies
- Moving average methods for trend deviations
- CUSUM algorithm for detecting sustained shifts
- Performance evaluation using known incident period

### Early Warning System
- Combined detection methods for robust anomaly identification
- Sensitivity and specificity evaluation
- Performance metrics across different event types
- Statistical validation against Ben Geen incident period

## Key Findings
- Emergency room admissions follow clear seasonal patterns with winter peaks
- Critical care events show more erratic patterns with less predictable seasonality
- During the incident period, respiratory events were 1244% higher than normal periods
- Early warning system achieved 100% sensitivity for admissions anomalies
- Combined anomaly detection methods outperformed any single method alone

## Technical Details
- **Language:** R (version 4.1.0+)
- **Key packages:** forecast, ggplot2, dplyr, lubridate, tseries
- **Statistical methods:** time series analysis, count-based regression, anomaly detection
- **Visualization:** ggplot2 for publication-quality figures

## Future Work
- Explore more specialized models for count data (zero-inflated models)
- Investigate shorter time intervals (weekly or daily) for earlier detection
- Incorporate additional hospital data sources (staffing, demographics)
- Develop user-friendly dashboard for real-time monitoring

## References

- Horton General Hospital. (2019). *UCI Machine Learning Repository*. [https://doi.org/10.24432/C5G321](https://doi.org/10.24432/C5G321)

## License
This project is licensed under the MIT License – see the LICENSE file for details.
