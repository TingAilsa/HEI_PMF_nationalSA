#!/bin/bash

#SBATCH --job-name=file_extract

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out/%x_%j_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out/%x_%j_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu


# Path to the original directory containing the Cluster folders
original_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme"

# Path to the new directory where you want to create the structure
new_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/base_DISPres1"

# Loop through each Cluster folder
for cluster_number in {1..25}; do
  cluster="Cluster_${cluster_number}"
  
  # Loop through each Factor subfolder inside the Cluster folder
  for factor_number in {6..11}; do
    factor="Factor_${factor_number}"
    
    # Create the corresponding folders in the new directory
    new_folder_path="${new_path}/${cluster}/${factor}"
    mkdir -p "${new_folder_path}"

    # Copy the *base.txt and *DISPres1.txt files to the new folder
    cp "${original_path}/${cluster}/${factor}"/*base.txt "${new_folder_path}/"
    cp "${original_path}/${cluster}/${factor}"/*DISPres1.txt "${new_folder_path}/"
  done
done

echo "Files copied successfully!"
