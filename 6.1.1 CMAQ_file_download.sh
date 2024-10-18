#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=CMAQ_download

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=15G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/%j_%a.out # output file
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/%j_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=00-10:00  # Total time needed for job: Days-Hours:Minutes

# Define the base path on the server and the local download directory
cmaq_base_url="/scratch/shussa29"
local_dir="/Users/TingZhang/Downloads"

# Define the years of files 
years=("2011")  # included years

# Define the month mapping between abbreviations and numbers
declare -A month_map=( ["02"]="Feb" ["06"]="Jun" ["07"]="Jul" )  # Add more months as necessary

# Loop through each month to download files
for month_num in "${!month_map[@]}"; do
    # Get the month abbreviation from the map
    month_abbr=${month_map[$month_num]}

    # Construct the folder path
    remote_folder="$base_url/$year"
    
    # Define the two file names to download
    combine_file="$remote_folder/COMBINE_ACONC_v54_gcc_CMAQ_ISAM_${year}${month_num}.nc"
    hr2day_file="$remote_folder/hr2day_v54_gcc_CMAQ_ISAM_${year}${month_num}.nc"
    
    # Download the COMBINE file
    echo "Downloading $combine_file..."
    wget -P "$local_dir" "$combine_file"
    
    # Download the hr2day file
    echo "Downloading $hr2day_file..."
    wget -P "$local_dir" "$hr2day_file"
done

echo "Download complete."