#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=HDpara_combine

#SBATCH --nodes=1  # number of nodes
#SBATCH --ntasks-per-node=1  # tasks per node, up to 128;
#SBATCH --mem-per-cpu=10G  # memory per CORE

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/err_out/%x.err

#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

#SBATCH --time=05-00:00 

# Load necessary modules
module load gnu10 openmpi r

# Run R script
Rscript allYear_ML_HD_parameter_combine.R