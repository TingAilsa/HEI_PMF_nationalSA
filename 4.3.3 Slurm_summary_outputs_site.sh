#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=site_file_count

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=10G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_15t1mdl0unc_site/err_out/%x_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_15t1mdl0unc_site/err_out/%x_%A_%a.err # error file

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

# Define the site_serial combinations
# all_site_serial=("001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "011" "012" "013" "014" "015" "017" "018"  "019" "020" "021" "022" "023" "024" "025" "026" "027" "028" "029" "030" "031" "032" "033" "034" "035" "036" "037" "038" "039" "040" "041" "042" "043" "044" "045" "046" "047" "048" "049" "050" "051" "052"  "053" "054" "055" "056" "057" "058" "059" "060" "061" "062" "063" "064" "065" "066" "067" "068" "069" "070" "071" "072" "073" "074" "075" "076" "077" "080" "081" "082" "083" "084" "085" "086" "087" "088" "089" "090" "091" "092" "093" "094" "095" "096" "097" "098" "099" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132" "133" "134" "135" "136" "138" "139" "140" "141" "142" "144" "145" "147" "149" "150" "151")
# 034, 071, 127, 129, 134, 145
all_site_serial=("001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "011" "012" "013" "014" "015" "017" "018"  "019" "020" "021" "022" "023" "024" "025" "026" "027" "028" "029" "030" "031" "032" "033" "035" "036" "037" "038" "039" "040" "041" "042" "043" "044" "045" "046" "047" "048" "049" "050" "051" "052"  "053" "054" "055" "056" "057" "058" "059" "060" "061" "062" "063" "064" "065" "066" "067" "068" "069" "070" "072" "073" "074" "075" "076" "077" "080" "081" "082" "083" "084" "085" "086" "087" "088" "089" "090" "091" "092" "093" "094" "095" "096" "097" "098" "099" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "128" "130" "131" "132" "133" "135" "136" "138" "139" "140" "141" "142" "144" "147" "149" "150" "151")

# Summarize the number of different files
for site_serial in "${all_site_serial[@]}"; do
for factor_number in {5..11}; do
  factor_folder="S_${site_serial}/Factor_${factor_number}"  
    # Count total files
    total_files=$(find $factor_folder -type f | wc -l)
    
    # Count files with specific patterns
    files_base=$(find $factor_folder -type f -name '*base*' | wc -l)
    files_BS=$(find $factor_folder -type f -name '*BS*' | wc -l)
    files_DISP=$(find $factor_folder -type f -name '*DISP*' | wc -l)
    files_DISPres1=$(find $factor_folder -type f -name '*_DISPres1*' | wc -l)
    
    # Write results to the temporary file
    echo "$site_serial,$factor_number,$total_files,$files_base,$files_BS,$files_DISP,$files_DISPres1" >> $temp_file_PMF
  done
done

# Convert the temporary file into CSV
Rscript summary_server_outputs.R $temp_file_PMF