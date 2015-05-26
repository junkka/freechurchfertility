# Instruction for replicating analysis

This project contained R scripts for replicating the analysis in the article "Gender and fertility within the free churches in the Sundsvall region, Sweden, 1860-1921" (Junkka & Edvinsson, 2015). All figures and tables except for the map (Figure 1) is produced through these scripts. The analysis is split into 7 scripts. 

To re-run the full analysis source the `run-all.R` script.

## Data

The source data for the main analysis is an extract from the POPUM database from the Demographic databse at Umeå university, a dataset that cannot be distributed openly. Instead to enable replication of the study we supply a generated dataset. The code for reproducing the generated dataset is found in `1-sample-data.R`, and given that you have the original dataset the generated one can be reproduced.

The `movement` data is a sample of the popular movement archive dataset, containing membership numbers for all popular movements in the Sundsvall region 1881-1950. Documentation at [http://snd.gu.se/catalogue/file/207]

The `agg_mfrt` dataset contains period marital fertility rates for Sweden taken from Hofsten & Lundström (1976).

## Libraries

The `libs` directory holds all helper functions, most are used for formating tables and plots. The `make_variables.R` script holds functions that transforms the source data into specific forms that is used in multiple scripts. 

## References

Junkka, J., & Edvinsson, S. (2015). Gender and fertility within the free churches in the Sundsvall region, Sweden, 1860-1921. The History of the Family. [doi: 10.1080/1081602X.2015.1043929](http://doi.org/10.1080/1081602X.2015.1043929)
