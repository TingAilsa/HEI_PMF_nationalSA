#!/bin/bash

#SBATCH --job-name=site_extract

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site_all/err_out/%x_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site_all/err_out/%x_%A_%a.err # error file
##SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_noSeason99_site/err_out/%x_%A_%a.out # output file
##SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_noSeason99_site/err_out/%x_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu


# Path to the original directory containing the Cluster folders
original_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site_all"

# Path to the new directory where you want to create the structure
new_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_25TimesMean_site_all/base_DISPres1"

# Create an associative array
declare -A SLURM_MAP

# Define the cluster_site combinations
#cluster_sites=("1_10732003" "1_391510017" "10_421010055" "11_270530963" "12_450790007" "12_540390020" "13_540511002" "14_160010010" "15_180890022" "16_420450109" "17_360551007" "18_371190041" "19_550090005" "2_420950025" "20_201730010" "21_220330009" "22_500070012" "23_360050110" "24_460990008" "24_490050007" "25_420030064" "3_320030540" "4_320310031" "5_130210007" "6_60731022" "7_120110034" "7_60670006" "8_380171004" "9_530530031" "9_550790010")
cluster_sites=("001" "002" "003" "004" "005" "007" "008" "009" "010" "011" "012" "013" "014" "015" "017" "018" "019" "020" "021" "022" "023" "024" "025" "026" "027" "028" "029" "030" "031" "032" "033" "034" "035" "036" "037" "038" "039" "040" "041" "042" "043" "044" "045" "046" "047" "048" "049" "050" "051" "052" "053" "054" "055" "056" "057" "058" "060" "061" "062" "063" "064" "065" "066" "067" "068" "069" "070" "071" "072" "073" "074" "075" "076" "077" "080" "081" "082" "083" "084" "085" "086" "087" "088" "089" "090" "091" "092" "093" "094" "095" "096" "097" "098" "099" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132" "133" "134" "135" "136" "138" "139" "140" "141" "142" "144" "145" "147" "149" "150" "151")

echo ${Site_serial}
echo ${Factor_number}

# Loop through each Cluster folder
for cluster_site in "${cluster_sites[@]}"; do
  C_cluster_site="C_${cluster_site}"
  
  # Loop through each Factor subfolder inside the Cluster folder
  for factor_number in {5..11}; do
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
