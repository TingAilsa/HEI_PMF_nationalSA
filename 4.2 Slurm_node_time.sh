#!/bin/bash

#SBATCH --job-name=node_time

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out/%x_%N.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out/%x_%N.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

# Output file to store all the information
output_file="combined_task_times.txt"

# Initialize the output file
echo "Cluster, Factor, Node, Time used" > $output_file

# Iterate through the clusters and factors
for cluster in Cluster_{1..25}; do
  for factor in $cluster/Factor_{6..11}; do
    task_time_file="$cluster/$factor/task_times.txt"
    
    # Check if the task_times.txt file exists in the folder
    if [ -f "$task_time_file" ]; then
      # Extract the required information using awk
      awk -F '[ :]+' '{
        print $2 "," $4 "," $6 "," $8 " hours, " $10 " minutes, " $12 " seconds"
      }' $task_time_file >> $output_file
    fi
  done
done
