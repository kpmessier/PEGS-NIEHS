#!/bin/bash

#SBATCH --partition=highmem
#SBATCH --cpus-per-task=4
#SBATCH --mem=144g
#SBATCH --mail-user=melissa.lowe@nih.gov
#SBATCH --mail-type=END, FAIL
#SBATCH --output=out.%j
#SBATCH --error=error.%j
#SBATCH --account=loweme

export HDF5_DISABLE_VERSION_CHECK=1

/ddn/gs1/biotools/R402/bin/R CMD BATCH cluster_test1.R
