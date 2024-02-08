#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=C_site25times

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
## SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=20G  # amount of memory the job requires, default is 2G  # memory per CORE

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
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes

## create an array of jobs
#SBATCH --array=1-180

# Select specific nodes, those potentially more efficient
### SBATCH --nodelist=hop051,hop053,hop054,hop055,hop059,hop061,hop064,hop065,hop066,hop072,hop073
##SBATCH --constraint=amd ##intel

#load modules
module load gnu10/10.3.0-ya
module load r
module load singularity

# set the LANG environment variable
export LANG=C.UTF-8

# script to exit immediately if a command exits with a non-zero status
set -x

#### 0. Basic setup ####

#dataset="IMPROVE"
dataset="CSN"

extremeMethod="25TimesMean"
#extremeMethod="noSeason99"

# Singularity container set-up
SINGULARITY_BASE=/containers/hopper/Containers
CONTAINER=${SINGULARITY_BASE}/wine/wine.sif

# Create an associative array
declare -A SLURM_MAP

# Define the cluster_site combinations
#cluster_sites=("1_390350060" "1_391510017" "2_170434002" "3_481410044" "4_81230008" "5_51190007" "6_60658001" "7_60850005" "7_120110034" "8_380171004" "9_530530031" "9_550790010" "10_100032004" "11_110010043" "12_130690002" "12_540390020" "13_540511002" "14_350010023" "15_170310057" "16_180190010" "17_180650003" "18_371190041" "19_550090005" "20_201730010" "21_482011039" "22_540390011" "23_421010048" "24_380150003" "24_490050007" "25_420030064")
cluster_sites=("1_10732003" "1_391510017" "10_421010055" "11_270530963" "12_450790007" "12_540390020" "13_540511002" "14_160010010" "15_180890022" "16_420450109" "17_360551007" "18_371190041" "19_550090005" "2_420950025" "20_201730010" "21_220330009" "22_500070012" "23_360050110" "24_460990008" "24_490050007" "25_420030064" "3_320030540" "4_320310031" "5_130210007" "6_60731022" "7_120110034" "7_60670006" "8_380171004" "9_530530031" "9_550790010")

# Populate the associative array
task_id=1
for cluster_site in "${cluster_sites[@]}"; do
    for factor_number in {6..11}; do
        SLURM_MAP[$task_id]="${cluster_site}:${factor_number}"
        ((task_id++))
    done
done

# Use SLURM_ARRAY_TASK_ID to get B_AAAAA and Factor_number
mapped_value=${SLURM_MAP[$SLURM_ARRAY_TASK_ID]}
Site_serial=${mapped_value%:*}
Factor_number=${mapped_value#*:}

echo ${Site_serial}
echo ${Factor_number}

# Set up the files path for the ME-2.exe and the key file"
ShR_SCRIPT_DIR="/projects/HAQ_LAB/tzhang/pmf_no_gui/${dataset}_${extremeMethod}_site"
BASE_SCRIPT_DIR="$ShR_SCRIPT_DIR/C_${Site_serial}/Factor_${Factor_number}"

# Set up singularity run
SINGULARITY_RUN="singularity exec  -B ${BASE_SCRIPT_DIR}:/host_pwd --pwd /host_pwd"
SCRIPT=PMF_bs_6f8xx_sealed_GUI_MOD.ini

# Define the DOS command to be used
DOS_COMMAND="${SINGULARITY_RUN} ${CONTAINER} wine ${BASE_SCRIPT_DIR}/ME-2.exe ${SCRIPT}"

# Set the directory path for the Cluster and Factor folders
cd "C_${Site_serial}/Factor_${Factor_number}"
cp ../*.csv . #save to check if some code is useful for this path
cp ${ShR_SCRIPT_DIR}/me2key.key .
cp ${ShR_SCRIPT_DIR}/PMF_ab_base.dat .
cp ${ShR_SCRIPT_DIR}/ME-2.exe .
cp ${ShR_SCRIPT_DIR}/PMF_bs_6f8xx_sealed_GUI_MOD.ini .
echo "Key&exe copy finished"
echo $PWD


# Check if the necessary files are present, stop the rest runs if present
base_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt" | wc -l)
base_txt_file=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt")
# base_size=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt" -exec du -b {} + | awk '{s+=$1} END {print s}')
base_size=$(du -b "$base_txt_file" | awk '{print $1}')
base_size=${base_size:-0}

Qm_task=$(find ${BASE_SCRIPT_DIR} -type f -name "*_Qm_task.txt" | wc -l)
use_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_use.txt" | wc -l)
BS_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_BS_.txt" | wc -l)
DISP_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_DISP_.txt" | wc -l)
DISPres1=$(find ${BASE_SCRIPT_DIR} -type f -name "*_DISPres1.txt" | wc -l)
BS_size=$(find ${BASE_SCRIPT_DIR} -type f -name "*_BS_.txt" -exec du -b {} + | awk '{s+=$1} END {print s}')
BS_size=${BS_size:-0} # BS_size is set to 0 when no files are found
DISP_size=$(find ${BASE_SCRIPT_DIR} -type f -name "*_DISP_.txt" -exec du -b {} + | awk '{s+=$1} END {print s}')
DISP_size=${DISP_size:-0}

# Check if the number of tasks included in the base result if it exits
if [ -n "$base_txt_file" ]; then
    base_task_count=$(grep -c "Results written by postprocessing for task" "$base_txt_file")
else
    base_task_count=0
fi

echo "base_result.txt: " ${base_txt}
echo "base_result size: " ${base_size}
echo "base_task_count: " ${base_task_count}

echo "Qm_task.txt: " ${Qm_task}
echo "use.txt: " ${use_txt}
echo "_BS_.txt: " ${BS_txt} " & size: " ${BS_size}
echo "_DISP_.txt: " ${DISP_txt}  " & size: " ${DISP_size}
echo "DISPres1.txt: " ${DISPres1}

#### function_1. No need to run ####
function part1_NoRun {
    echo "Base files already present, skipping this run."
}

#### function_2. Base run ####

function part2_Base {
    echo "Start from Base run"

    cp iniparams_base_C_${Site_serial}_F_${Factor_number}.txt iniparams.txt
    echo "Base parameter file changed"
    #echo $DOS_COMMAND
    #echo $PWD
    $DOS_COMMAND
    if [ $? -ne 0 ]; then echo "Error in Part Base"; return; fi
    rm iniparams.txt
    echo "Base Model Run completes"

    ### Analyze the output .txt file, generate the new value for numoldsol, and replace it in other iniparams.txt series using R
    mv ${dataset}_noCsub_${extremeMethod}_C_${Site_serial}_F_${Factor_number}_.txt ${dataset}_noCsub_${extremeMethod}_C_${Site_serial}_F_${Factor_number}_base.txt
    if [ $? -ne 0 ]; then echo "Error in Part Base"; return; fi

    echo "Base run finished!"
}

#### function_3. from R, then run BS, DISP, and BS-DISP ####
##### Run BS, DISP, and BS-DISP analyses if there are base results 

function part3_BS_DISP {
  echo "Execute BS & DISP"
    
    Rscript ${ShR_SCRIPT_DIR}/minQ_Task_numoldsol_site.R ${dataset}_noCsub_${extremeMethod}_C_${Site_serial}_F_${Factor_number}_base.txt ${Site_serial} ${Factor_number} ${BASE_SCRIPT_DIR}
    if [ $? -ne 0 ]; then echo "Error in Part 1"; return; fi
    echo "minQ changed"

    # Run DOS command for BS, DISP, and BS-DISP analyses in turn
    for param_file in iniparams_BS_C_${Site_serial}_F_${Factor_number}_use.txt iniparams_DISP_C_${Site_serial}_F_${Factor_number}_use.txt
    do
      cp ${param_file} iniparams.txt
      if [ $? -ne 0 ]; then echo "Error in Part BS/DISP run"; return; fi
      $DOS_COMMAND
      if [ $? -ne 0 ]; then echo "Error in Part BS/DISP run"; return; fi
      rm iniparams.txt
    done

    # Rename the output from DISP
    mv DISPres1.txt ${dataset}_C_${Site_serial}_F_${Factor_number}_DISPres1.txt

    echo "All runs executed!"
}

#### 1. Having all correct results ####

if [ "$DISPres1" -eq 1 ] && [ "$use_txt" -eq 2 ] && [ "$BS_size" -gt 10000 ] && [ "$DISP_size" -gt 10000 ]; then
    part1_NoRun
fi

#### 2. Partially Missing results (no BS/DISP) ####
##### Run BS, DISP, and BS-DISP analyses if there are base results 

if [ "$base_txt" -eq 1 ] && [ "$DISPres1" -eq 0 ] && [ "$DISP_txt" -eq 0 ]; then
    part3_BS_DISP
fi

#### 3. No results OR BS/DISP not match base reuslts ####

if [[ "$base_txt" -eq 0 ]] || [[ "$base_task_count" -lt 20 ]] || [[ "$DISPres1" -eq 0 ]] || [[ "$BS_size" -lt 10000 ]] || [[ "$DISP_size" -lt 10000 ]]; then
    part2_Base
    part3_BS_DISP
fi

exit 0 #terminates the script execution