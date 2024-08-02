# Regenerating Tables

This document outlines the process for regenerating tables using different R scripts, each with specific configurations.

## Scripts

There are four main scripts, located in the `code` folder:

1. `gen_tab.R`
2. `gen_tab_svi.R`
3. `gen_tab_svi_policy.R`
4. `gen_tab_svi_policy_pval.R`

### Script Details

| Script | SVI Controls | Control Period | Treatment Period | Output Folder | Additional Info |
|--------|--------------|----------------|------------------|---------------|-----------------|
| `gen_tab.R` | No | 2011-2017 | 2018-2019 | `tables` | - |
| `gen_tab_svi.R` | Yes | 2011-2017 | 2018-2019 | `tables-svi` | - |
| `gen_tab_svi_policy.R` | Yes | 2011-2016 | 2018-2019 | `tables-svi-policy` | - |
| `gen_tab_svi_policy_pval.R` | Yes | 2011-2016 | 2018-2019 | `tables-svi-policy-pval` | Reports p-values next to coefficients |

## Steps to Regenerate Tables

1. Open the desired script from the `code` folder.
2. Run the script without modifications.
3. If you encounter issues, update the project directory path to match your local setup.

## Data Requirements

The following data files are required, located in the `data` folder:

1. `census_regions.xls`
2. `hepb_mortality3.csv`
3. `hepb_mortality_svi.csv` (augments data in file 2 with CDC SVI control variables)
4. `hepb_mortality_svi2.csv` (adjusts data in file 3 by excluding the year 2017)

Note: The mortality data used in this analysis comes from the [Restricted-Use Vital Statistics Data](https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm). All mortality data are not made public due to the agreement of restricted use.

## Additional Information

- `clean-svi.R`: This script merges and identifies common variables, performs linear interpolation, and then left joins to the original data.
- `data_processing.R`: This script processes CDC's private access mortality data, downloads health rank data, and performs other data checking operations.
- Running the R scripts in RStudio is recommended but not mandatory.
- Generated tables are saved in DOCX format.
- Table filenames correspond to their titles in the paper.

## Notes

- Ensure all required data files are present in the `data` folder before running the scripts.
- Review the output in the respective folders (`tables`, `tables-svi`, `tables-svi-policy`, or `tables-svi-policy-pval`) after running each script.
- The `data_processing.R` script handles sensitive data and performs crucial data preparation tasks. Ensure you have the necessary permissions and understand the implications of running this script.
- Due to the restricted-use agreement, all mortality data used in this analysis are not made public to protect privacy and confidentiality.
