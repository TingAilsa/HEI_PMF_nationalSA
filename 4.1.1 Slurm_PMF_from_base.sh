#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=C_select

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --ntasks-per-node=1  # tasks per node # up to 128;
#SBATCH --cpus-per-task=1
##SBATCH --exclusive
#SBATCH --mem-per-cpu=10G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
##SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out_select/%x_%A_%a.out # output file
##SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out_select/%x_%A_%a.err # error file
##SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/IMPROVE_CMD_noCsub_noExtreme/err_out_select/%x_%A_%a.out # output file
##SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/IMPROVE_CMD_noCsub_noExtreme/err_out_select/%x_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes

## create an array of jobs
#SBATCH --array=1-150%10
##SBATCH --array=1,2,8,13,15,22,46,57,59,60,67,74,80,85,93,104,115,122,123,128,143,144,146%6 ##CSN

# Select specific nodes
###SBATCH --nodelist=amd006,amd011,amd012,amd013,amd015,amd020,amd038,amd043,amd046,amd047,amd048,hop051,hop053,hop054,hop055,hop059,hop061,hop064,hop065,hop066,hop072,hop073
#SBATCH --constraint=amd ##intel

#load modules
module load gnu10/10.3.0-ya
module load r
module load singularity

# set the LANG environment variable
export LANG=C.UTF-8

# script to exit immediately if a command exits with a non-zero status
set -x

#### 0. Basic setup ####

dataset="IMPROVE"
#dataset="CSN"

# Singularity container set-up
SINGULARITY_BASE=/containers/hopper/Containers
CONTAINER=${SINGULARITY_BASE}/wine/wine.sif

# Calculate the Cluster and Factor numbers based on the array index
Cluster_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) / 6 ) + 1 ))
Factor_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) % 6 ) + 6 ))
# Cluster_number=1
# Factor_number=9
echo ${Cluster_number}
echo ${Factor_number}

# Set up the files path for the ME-2.exe and the key file"
ShR_SCRIPT_DIR="/projects/HAQ_LAB/tzhang/pmf_no_gui/${dataset}_CMD_txt_noCsub_noExtreme"
#ShR_SCRIPT_DIR="/projects/HAQ_LAB/tzhang/pmf_no_gui/${dataset}_CMD_noCsub_noExtreme"
BASE_SCRIPT_DIR="$ShR_SCRIPT_DIR/Cluster_${Cluster_number}/Factor_${Factor_number}"

# Set up singularity run
SINGULARITY_RUN="singularity exec  -B ${BASE_SCRIPT_DIR}:/host_pwd --pwd /host_pwd"
SCRIPT=PMF_bs_6f8xx_sealed_GUI_MOD.ini

# Define the DOS command to be used
DOS_COMMAND="${SINGULARITY_RUN} ${CONTAINER} wine ${BASE_SCRIPT_DIR}/ME-2.exe ${SCRIPT}"

# Set the directory path for the Cluster and Factor folders
cd "Cluster_${Cluster_number}/Factor_${Factor_number}"
cp ../*.csv . #save to check if some code is useful for this path
#cp ${ShR_SCRIPT_DIR}/me2key.key .
#cp ${ShR_SCRIPT_DIR}/PMF_ab_base.dat .
#cp ${ShR_SCRIPT_DIR}/ME-2.exe .
#cp ${ShR_SCRIPT_DIR}/PMF_bs_6f8xx_sealed_GUI_MOD.ini .
#echo "Key&exe copy finished"
echo $PWD


# Check if the necessary files are present, stop the rest runs if present
base_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt" | wc -l)
base_txt_file=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt")
# base_size=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt" -exec du -b {} + | awk '{s+=$1} END {print s}')
base_size=$(du -b "$base_txt_file" | awk '{print $1}')
base_size=${base_size:-0}

# Check if the number of tasks included in the base result if it exits
if [ -n "$base_txt_file" ]; then
    base_task_count=$(grep -c "Results written by postprocessing for task" "$base_txt_file")
else
    base_task_count=0
fi

echo "base_result.txt: " ${base_txt}
echo "base_result size: " ${base_size}
echo "base_task_count: " ${base_task_count}

#### function_1. No need to run ####
function part1_NoRun {
    echo "Base files already present, skipping this run."
}

#### function_2. Base run ####

function part2_Base {
    echo "Start from Base run"

    cp iniparams_base_C_${Cluster_number}_F_${Factor_number}.txt iniparams.txt
    echo "Base parameter file changed"
    #echo $DOS_COMMAND
    #echo $PWD
    $DOS_COMMAND
    if [ $? -ne 0 ]; then echo "Error in Part Base"; return; fi
    rm iniparams.txt
    echo "Base Model Run completes"

    ### Analyze the output .txt file, generate the new value for numoldsol, and replace it in other iniparams.txt series using R
    mv ${dataset}_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_.txt ${dataset}_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_base.txt
    if [ $? -ne 0 ]; then echo "Error in Part Base"; return; fi

    echo "Base run finished!"
}

#### function_3. from R, then run BS, DISP, and BS-DISP ####
##### Run BS, DISP, and BS-DISP analyses if there are base results 

function part3_BS_DISP {
  echo "Execute BS & DISP"
    
    Rscript ${ShR_SCRIPT_DIR}/minQ_Task_numoldsol.R ${dataset}_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_base.txt ${Cluster_number} ${Factor_number} ${BASE_SCRIPT_DIR}
    if [ $? -ne 0 ]; then echo "Error in Part 1"; return; fi
    echo "minQ changed"

    # Run DOS command for BS, DISP, and BS-DISP analyses in turn
    for param_file in iniparams_BS_C_${Cluster_number}_F_${Factor_number}_use.txt iniparams_DISP_C_${Cluster_number}_F_${Factor_number}_use.txt
    do
      cp ${param_file} iniparams.txt
      if [ $? -ne 0 ]; then echo "Error in Part BS/DISP run"; return; fi
      $DOS_COMMAND
      if [ $? -ne 0 ]; then echo "Error in Part BS/DISP run"; return; fi
      rm iniparams.txt
    done

    # Rename the output from DISP
    mv DISPres1.txt ${dataset}_C_${Cluster_number}_F_${Factor_number}_DISPres1.txt

    echo "All runs executed!"
}

#### Execute from base run ####

if [[ "$base_txt" -eq 0 ]] || [[ "$base_task_count" -lt 20 ]] || [[ "$base_size" -lt 1000 ]]; then
    part2_Base
    part3_BS_DISP
fi

exit 0 #terminates the script execution