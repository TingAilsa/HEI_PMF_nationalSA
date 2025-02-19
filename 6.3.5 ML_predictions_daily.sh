#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=Sul_pred

#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=20G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/err_out/%x_%A_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction/err_out/%x_%A_%a.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

#SBATCH --array=1-117 # Change based on the day count in file


#load modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# Source and period to process (set these when submitting the job)
SOURCE_TEST="Sulfate" # "Sulfate", "Traffic", "Dust", "Biomass", "Industry", "Nitrate"
CMAQ_PERIOD="2017-01_2017-12" # 1-10， 2011-2020, "2017-01_2017-12"
No_of_days="117" 

# cmaq_period = c("2011-01_2011-12", 117,
#                 "2012-01_2012-12", 117,
#                 "2013-01_2013-12", 117, 
#                 "2014-01_2014-12", 117,
#                 "2015-01_2015-12", 117, 
#                 "2016-01_2016-12", 117,
#                 "2017-01_2017-12", 117,
#                 "2018-01_2018-12", 117, 
#                 "2019-01_2019-12", 117,
#                 "2020-01_2020-12", 117)

# Run R script for this date
Rscript predict_single_day.R "${SLURM_ARRAY_TASK_ID}" "$SOURCE_TEST" "$CMAQ_PERIOD" "$No_of_days"

