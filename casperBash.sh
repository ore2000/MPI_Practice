#! /bin/bash -1
#
#SBATCH --job-name = Daxpy
#SBATCH --account =  NTDD0002
#SBATCH --ntasks = 108
#SBATCH --ntasks-per-node = 36
#SBATCH --time = 00:10:00
#SBATCH --partition = dav
#SBATCH --output = mpi_job.out.%j

module purge 
module load openmpi
module load intel

#Go to project directory
cd /glade/scratch/obabatun/MPI_Practice

srun  ./run.exe

