# HEI_PMF_nationalSA
The repository includes all scripts used for national source apportionment in the US based on observation data from CSN &amp; IMPROVE. Three types of programs are used, R, Python, and bash script. The scripts are used to explore the PM2.5 species measurements, process and prepare input datasets for GUI and non-GUI Positive Matrix Factorization (PMF) source apportionment (SA) analyses, and analyze the SA results.

## Data used in this work
This repository references two datasets: 1) Daily PM2.5 species measurements from two monitoring networks, Chemical Speciation Network (CSN) and Interagency Monitoring of Protected Visual Environments (IMPROVE), downloaded from the [Federal Land Manager Environmental Database](http://views.cira.colostate.edu/fed/QueryWizard/Default.aspx); 2) [ECMWF Reanalysis v5 (ERA5)](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form) hourly atmospheric data of the global climate. We used the boundary laryer height and wind speed data for dispersion normalization PMF analyses; 3) County-level population, industry, and traffic data from the US census.

## R scripts in this repository
### Functions used for the program
1. `0. US_SA_functions.R` documents all functions used in other R scripts

### Explore the CSN & IMPROVE dataset
2. `1.1 Data exploration - CSN.R` and `1.2 Data exploration - IMPROVE.R`document exploration of all variables in CSN monitoring data and metadata, the spatiotemporal trend analyses of species concentrations, the included flags/qualifiers, the concentrations of flagged and unflagged data, etc. in CSN and IMPROVE datasets. 
3. `1.3 CSN & IMPROVE sites.R` document the idenfitication of identical sites in two monitoring networks and comparions of the measurements from the two datasets, and preparation of datasets for interpolation.
4. `1.4 EPA_PM_monitor.R` documents the matching of EPA AQS sites with CSN sites and replacement of PM2.5 concentrations
   
### Data interpolation, clustering, and replacement
5. `2.1 PM_Comp_Interpolate.R` documentd the comparison of four interpolation methods
6. `2.2 PM_Comp_Clust_ML.R` documents the clustering based on CSN and IMPROVE datasets, separately, as well as the clutering conducted based on different variables

### PMF input preparation
7. ` `
8. `3.2 PMF_nonGUI_prepare.R` documents the preparation of concentration and uncertainty inputs for GUI and non-GUI PMF analyses, including the process of handling the extreme concentration points, dispersion normalization, etc.
9. `3.3 PMF_CMD_txt_fn.R` and `3.4 PMF_CMD_txt_Site.R` document the preparation of input parameter files for non-GUI PMF analyses

### Batch non-GUI PMF analyses
10. `4.1.3 minQ_Task_numoldsol.R` documents the determination of number of task that provides the lowest Q numbers in base model run, and updates of input parameter files for error estimation
11. `4.3.2 summary_server_outputs.R` and `4.3.4 summary_server_outputs_site.R` document the examination of progress check during batch analyses for for cluster- and site-based analyses

### Post-PMF source apportionment and summary
12. `5.1 SPECIATE_expl.R` documents the exploration of EPA SPECIATE repository
13. `5.2 census_US_forPMF.R` documents the preparaton of US census data used for SA
14. `5.3 CMD_PMF_Base_mac.R` and `5.3.1 CMD_PMF_to_Conc_Site.R` document the data summary and plotting of PMF results from based model run and error estimations for cluster- and site-based results
15. `5.4 PMF_merge&plotting_cluster.R` and `5.5 PMF_merge&plotting_site.R` document the post-analyses of SA results from PMF for cluster- and site-based results

## Bash scripts in this repository

### Batch non-GUI PMF analyses
16. `4.1.1 Slurm_PMF_from_base.sh` documents the progress of starting PMF analyses from base model running for cluster data
17. `4.1.2 Slurm_PMF_bunch_select.sh` and `4.1.4 Slurm_PMF_bunch_site.sh` document the progress of starting PMF from any detected step for cluster and site data
18. `4.3.1. Slurm_summary_outputs.sh` and `4.3.3 Slurm_summary_outputs_site.sh` document the examination of progress check during batch analyses
19. `4.4.1 Slurm_base_disp_extract.sh` and `4.4.2 Slurm_base_disp_extract_site.sh` document the extraction of PMF results

## Python scripts in this repository

### PMF input preparation
20. `1.4 US_Meteorology_ERA5_2011-20.ipynb` documents the preparation of site-specific boundary and wind speed data
