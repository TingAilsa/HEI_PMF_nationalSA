#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=RF_du11

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=50G  # amount of memory the job requires, default is 2G  # memory per CORE
#SBATCH --cpus-per-task=8
##SBATCH --mem=200G

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_script/err_out/%x_%A.out # output file
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_script/err_out/%x_%A.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes


#load modules
module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

# run R
#Rscript RF_no_unc_dust.R
Rscript RF_no_unc_noCoords_dust_2.R

