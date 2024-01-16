#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=pmf_file_count

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=10G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/err_out/%x.out
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/err_out/%x.err

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=01-00:00  # Total time needed for job: Days-Hours:Minutes

# Load modules
module load r


# Temporary file to hold the results
temp_file_PMF="temp_results_PMF.txt"

# Initialize the temporary file
echo "" > $temp_file_PMF

# Summarize the number of different files
for cluster in Cluster_{1..25}; do
  for factor in $cluster/Factor_{6..11}; do
  
    # Count total files
    total_files=$(find $factor -type f | wc -l)
    
    # Count files with specific patterns
    files_base=$(find $factor -type f -name '*base*' | wc -l)
    files_BS=$(find $factor -type f -name '*BS*' | wc -l)
    files_DISP=$(find $factor -type f -name '*DISP*' | wc -l)
    files_DISPres1=$(find $factor -type f -name '*_DISPres1*' | wc -l)
    
    # Write results to the temporary file
    echo "$cluster,$factor,$total_files,$files_base,$files_BS,$files_DISP,$files_DISPres1" >> $temp_file_PMF
  done
done

# Convert the temporary file into CSV
Rscript summary_server_outputs.R $temp_file_PMF