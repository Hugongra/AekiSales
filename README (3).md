# AekiSales Data Analytics Project

This superstore retail, is facing a sales crisis. The aim of this project is to identify problems and suggest improvements through detailed data analysis and visualizations. This involves careful examination of sales data gotten from 2014 to 2017 in order to uncover insights and trends that can help AEKI improve its sales performance.

## Project Structure

The repository is organized as follows:

- `data/`: Contains the sales datasets.
- `R_Markdown/`: R Markdown files used for creating the final report.
- `FINALPROJECT.pdf`: The comprehensive report generated from the analysis.
- `README.md`: This file.

## Data Sources

- Sales data from 2014 to 2017 provided by AEKI in Excel format:
  - [AEKI_2016.xlsx](https://github.com/Hugongra/AekiSales/blob/main/AEKI_2016.xlsx)
  - [AEKI_Data.xlsx](https://github.com/Hugongra/AekiSales/blob/main/AEKI_Data.xlsx)
- Geographic information from `worldcities.csv`:
  - [worldcities.csv](https://github.com/Hugongra/AekiSales/blob/main/worldcities.csv)

## Analysis and Results

**The comprehensive report generated from the analysis**:
[FINALPROJECT.pdf](https://github.com/Hugongra/AekiSales/blob/main/FINALPROJECT.pdf)

This project focuses on several key areas of analysis:

- **Data Quality**: Identification and correction of any manual errors found in the dataset.
- **Pricing Analysis**: Evaluation of average, maximum, and minimum prices for the products annually.
- **Sales Trends**: Examination of sales performance over time and also across different seasons.
- **Returns Analysis**: Detailed report on sales returns and their impact on overall performance.
- **Geographic Analysis**: Correlation of sales data with the geographic information in order to identify areas for improvement.

## Installation

### Requirements

- R (version 4.0 or higher)
- RStudio (optional, but recommended)
- The following R packages:
  - `dplyr`
  - `ggplot2`
  - `readxl`
  - `rmarkdown`

### Setup

**Clone the repository**:

```bash
git clone https://github.com/Hugongra/AekiSales.git
cd AekiSales
```

**Install the required R packages;**

```bash
install.packages(c("dplyr", "ggplot2", "readxl", "rmarkdown"))
```
