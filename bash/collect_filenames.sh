#!/bin/bash

#IFS=$'\n'; set -f

PREP_DIR='/lustre/nobackup/SHARED/BHE/mousebird_first_run/ml_prepared'
LOC_DIR='/lustre/nobackup/SHARED/BHE/mousebird_first_run/ml_localized'

# Define empty array
TO_LOCALISE=()

# Search prep-dir for all .csv.gz files (recursively)
for f in $(find $PREP_DIR -iname '*csv.gz'); do
  # Determine path for corresponding file in loc-dir
  # Bash magic, file without directory
  FILE=${f##$PREP_DIR}
  # RDS is file without extension in the loc-dir
  RDS=$LOC_DIR${FILE%.*.*}.RDS
  # (note double .* for .csv.gz filtering)

  if [ ! -f $RDS ]; then
    # RDS file corresponding to prep-file not found
    # so add to array
    TO_LOCALISE+=($f)
  fi
done

echo The number of files to localise is ${#TO_LOCALISE[@]} 1>&2;
echo So, use sbatch --array=1-${#TO_LOCALISE[@]}%nr_parallel_CPUs slurm_localize.sh 1>&2;

rm -f filenames.txt
touch filenames.txt

for ff in ${!TO_LOCALISE[*]}; do
  echo ${TO_LOCALISE[$ff]} >> filenames.txt
done
