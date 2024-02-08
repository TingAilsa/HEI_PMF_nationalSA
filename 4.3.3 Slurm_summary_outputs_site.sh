#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=site_file_count

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=10G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site/err_out/%x_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site/err_out/%x_%A_%a.err # error file
##SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_noSeason99_site/err_out/%x_%A_%a.out # output file
##SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_noSeason99_site/err_out/%x_%A_%a.err # error file

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

# Create an associative array
declare -A SLURM_MAP

# Define the cluster_site combinations
#cluster_sites=("1_390350060" "1_391510017" "2_170434002" "3_481410044" "4_81230008" "5_51190007" "6_60658001" "7_60850005" "7_120110034" "8_380171004" "9_530530031" "9_550790010" "10_100032004" "11_110010043" "12_130690002" "12_540390020" "13_540511002" "14_350010023" "15_170310057" "16_180190010" "17_180650003" "18_371190041" "19_550090005" "20_201730010" "21_482011039" "22_540390011" "23_421010048" "24_380150003" "24_490050007" "25_420030064")
cluster_sites=("1_10732003" "1_391510017" "10_421010055" "11_270530963" "12_450790007" "12_540390020" "13_540511002" "14_160010010" "15_180890022" "16_420450109" "17_360551007" "18_371190041" "19_550090005" "2_420950025" "20_201730010" "21_220330009" "22_500070012" "23_360050110" "24_460990008" "24_490050007" "25_420030064" "3_320030540" "4_320310031" "5_130210007" "6_60731022" "7_120110034" "7_60670006" "8_380171004" "9_530530031" "9_550790010")

# Summarize the number of different files
for cluster_site in "${cluster_sites[@]}"; do
for factor_number in {6..11}; do
  factor_folder="C_${cluster_site}/Factor_${factor_number}"  
    # Count total files
    total_files=$(find $factor_folder -type f | wc -l)
    
    # Count files with specific patterns
    files_base=$(find $factor_folder -type f -name '*base*' | wc -l)
    files_BS=$(find $factor_folder -type f -name '*BS*' | wc -l)
    files_DISP=$(find $factor_folder -type f -name '*DISP*' | wc -l)
    files_DISPres1=$(find $factor_folder -type f -name '*_DISPres1*' | wc -l)
    
    # Write results to the temporary file
    echo "$cluster_site,$factor_number,$total_files,$files_base,$files_BS,$files_DISP,$files_DISPres1" >> $temp_file_PMF
  done
done

# Convert the temporary file into CSV
Rscript summary_server_outputs.R $temp_file_PMF