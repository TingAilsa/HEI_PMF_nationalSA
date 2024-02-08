#!/bin/bash

#SBATCH --job-name=site_extract

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site/err_out/%x_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site/err_out/%x_%A_%a.err # error file
##SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_noSeason99_site/err_out/%x_%A_%a.out # output file
##SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_noSeason99_site/err_out/%x_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu


# Path to the original directory containing the Cluster folders
original_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site"

# Path to the new directory where you want to create the structure
new_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site/base_DISPres1"

# Create an associative array
declare -A SLURM_MAP

# Define the cluster_site combinations
#cluster_sites=("1_390350060" "1_391510017" "2_170434002" "3_481410044" "4_81230008" "5_51190007" "6_60658001" "7_60850005" "7_120110034" "8_380171004" "9_530530031" "9_550790010" "10_100032004" "11_110010043" "12_130690002" "12_540390020" "13_540511002" "14_350010023" "15_170310057" "16_180190010" "17_180650003" "18_371190041" "19_550090005" "20_201730010" "21_482011039" "22_540390011" "23_421010048" "24_380150003" "24_490050007" "25_420030064")
cluster_sites=("1_10732003" "1_391510017" "10_421010055" "11_270530963" "12_450790007" "12_540390020" "13_540511002" "14_160010010" "15_180890022" "16_420450109" "17_360551007" "18_371190041" "19_550090005" "2_420950025" "20_201730010" "21_220330009" "22_500070012" "23_360050110" "24_460990008" "24_490050007" "25_420030064" "3_320030540" "4_320310031" "5_130210007" "6_60731022" "7_120110034" "7_60670006" "8_380171004" "9_530530031" "9_550790010")

echo ${Site_serial}
echo ${Factor_number}

# Loop through each Cluster folder
for cluster_site in "${cluster_sites[@]}"; do
  C_cluster_site="C_${cluster_site}"
  
  # Loop through each Factor subfolder inside the Cluster folder
  for factor_number in {6..11}; do
    factor="Factor_${factor_number}"
    
    # Create the corresponding folders in the new directory
    new_folder_path="${new_path}/${C_cluster_site}/${factor}"
    mkdir -p "${new_folder_path}"

    # Copy the *base.txt and *DISPres1.txt files to the new folder
    cp "${original_path}/${C_cluster_site}/${factor}"/*base.txt "${new_folder_path}/"
    cp "${original_path}/${C_cluster_site}/${factor}"/*DISPres1.txt "${new_folder_path}/"
    cp "${original_path}/${C_cluster_site}/${factor}"/*BS_.txt "${new_folder_path}/"
    # cp "${original_path}/${cluster}/${factor}"/*BS_.txt "${new_folder_path}/"
  done
done

echo "Files copied successfully!"
