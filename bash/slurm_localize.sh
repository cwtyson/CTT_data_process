#!/bin/bash
##---------- Name of the job ----------
#SBATCH --job-name=localise_birds
##---------- Mail address ----------
#SBATCH --mail-user=hugo.loning@wur.nl
#SBATCH --mail-type=ALL
##---------- Output files ----------
#SBATCH --output=outputs/output_%A.%a.txt
#SBATCH --error=outputs/error_%A.%a.txt
##---------- Other information ----------
#SBATCH --comment='4100000605'
##---------- required resources ----------
#SBATCH --time=20-23:55  # 4 days 0 hours 0 minutes
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4000

####specify when calling this script using the --array=1-NR_OF_ARRAY_ELEMENTS%nr_of_parallel_cpus_you_want


##---------- Environment, Operations and job step ----------
module load 2023
module load R-bundle-CRAN

# Get the to-be-localised file
FILE=$(head -$SLURM_ARRAY_TASK_ID filenames.txt | tail -1)

# Run R script and pass the filename of the to-be-localised file
echo File $SLURM_ARRAY_TASK_ID is $FILE
Rscript /lustre/nobackup/SHARED/BHE/mousebird_test/localisation_script/R/projects/hpc_localize_w_docstring.R $FILE
