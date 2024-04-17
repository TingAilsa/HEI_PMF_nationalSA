#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=site_extract

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_15t1mdl0unc_site/err_out/%x_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_15t1mdl0unc_site/err_out/%x_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu


# Path to the original directory containing the Cluster folders
original_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_15t1mdl0unc_site"

# Path to the new directory where you want to create the structure
new_path="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_15t1mdl0unc_site/base_DISPres1"

# Create an associative array
declare -A SLURM_MAP

# Define the site_serialNo combinations
# site_serial=("001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "011" "012" "013" "014" "015" "017" "018"  "019" "020" "021" "022" "023" "024" "025" "026" "027" "028" "029" "030" "031" "032" "033" "034" "035" "036" "037" "038" "039" "040" "041" "042" "043" "044" "045" "046" "047" "048" "049" "050" "051" "052"  "053" "054" "055" "056" "057" "058" "059" "060" "061" "062" "063" "064" "065" "066" "067" "068" "069" "070" "071" "072" "073" "074" "075" "076" "077" "080" "081" "082" "083" "084" "085" "086" "087" "088" "089" "090" "091" "092" "093" "094" "095" "096" "097" "098" "099" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132" "133" "134" "135" "136" "138" "139" "140" "141" "142" "144" "145" "147" "149" "150" "151")
# 034, 071, 127, 129, 134, 145
site_serial=("001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "011" "012" "013" "014" "015" "017" "018"  "019" "020" "021" "022" "023" "024" "025" "026" "027" "028" "029" "030" "031" "032" "033" "035" "036" "037" "038" "039" "040" "041" "042" "043" "044" "045" "046" "047" "048" "049" "050" "051" "052"  "053" "054" "055" "056" "057" "058" "059" "060" "061" "062" "063" "064" "065" "066" "067" "068" "069" "070" "072" "073" "074" "075" "076" "077" "080" "081" "082" "083" "084" "085" "086" "087" "088" "089" "090" "091" "092" "093" "094" "095" "096" "097" "098" "099" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "128" "130" "131" "132" "133" "135" "136" "138" "139" "140" "141" "142" "144" "147" "149" "150" "151")

echo ${Site_serial}
echo ${Factor_number}

# Loop through each Cluster folder
for site_serialNo in "${site_serial[@]}"; do
  S_site_serialNo="S_${site_serialNo}"
  
  # Loop through each Factor subfolder inside the Cluster folder
  for factor_number in {3..11}; do
    factor="Factor_${factor_number}"
    original_folder_path="${original_path}/${S_site_serialNo}/${factor}"
    
    # Create the corresponding folders in the new directory
    new_folder_path="${new_path}/${S_site_serialNo}/${factor}"
    mkdir -p "${new_folder_path}"

    # Copy the *base.txt and *DISPres1.txt files to the new folder
    cp "${original_folder_path}"/*base.txt "${new_folder_path}/"
    cp "${original_folder_path}"/*DISPres1.txt "${new_folder_path}/"
    cp "${original_folder_path}"/*BS_.txt "${new_folder_path}/"
    cp "${original_folder_path}"/*_PMFreport.txt "${new_folder_path}/"
  done
done

echo "Files copied successfully!"
