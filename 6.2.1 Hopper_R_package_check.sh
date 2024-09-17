#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=R_pkg_check

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=15G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/R_pkg_check/err_out/%x_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/R_pkg_check/err_out/%x_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=00-02:00  # Total time needed for job: Days-Hours:Minutes

#load modules
module load gnu10/10.3.0-ya
module load r

# Set up a personal R library path in your home directory
export R_LIBS_USER="/projects/HAQ_LAB/tzhang/pmf_no_gui/R_pkg_check/R/libs"
mkdir -p "$R_LIBS_USER"

Rscript --vanilla Hopper_R_pkg_check_install.R
