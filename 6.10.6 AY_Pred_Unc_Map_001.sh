#!/bin/bash
#SBATCH --partition=bigmem
#SBATCH --job-name=RFavgMap001

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4

## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_script/err_out/RFmap_001_%a.out 
#SBATCH --error=/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_script/err_out/RFmap_001_%a.err

#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=tzhang23@gmu.edu
#SBATCH --time=8:00:00 

## Array jobs
#SBATCH --array=1-30
## sbatch --array=1-23,25-30 AY_Pred_Unc_Map_001.sh
## sbatch --array=24 --mem=128G AY_Pred_Unc_Map_001.sh
   
# Array IDs:
#   1-10   annual            2011-2020  (five sources in one figure + PM2.5)
#   11-22  monthly           1-12
#   23     overall
#   24     daily             (may need more memory, e.g. 128G)
#   25-30  annual_by_source  traffic, nitrate, sulfate, bbsoa, dust, pm (10 years in one figure)

module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
module load netcdf-c netcdf-fortran

R

i=$SLURM_ARRAY_TASK_ID
SOURCES=(traffic nitrate sulfate bbsoa dust pm)

if   [ $i -le 10 ]; then MAP_TYPE=annual;           INDEX=$((2010 + i))
elif [ $i -le 22 ]; then MAP_TYPE=monthly;          INDEX=$((i - 10))
elif [ $i -eq 23 ]; then MAP_TYPE=overall;          INDEX=""
elif [ $i -eq 24 ]; then MAP_TYPE=daily;            INDEX=""
else                     MAP_TYPE=annual_by_source; INDEX=${SOURCES[$((i - 25))]}
fi

echo "Task $i: $MAP_TYPE $INDEX"
Rscript AY_Pred_Unc_Map_001.R $MAP_TYPE $INDEX
