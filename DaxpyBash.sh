#! /bin/bash -1
#
#PBS -A NTDD0002
#PBS -l walltime=0:01:00
#PBS -l select=1536:ncpus=36:mpiprocs=50
#PBS -N Daxpy
#PBS -o checker.out
#PBS -e errcheck.err
#PBS -q regular

module purge 
module load pgi 
module load openmpi

#Go to project directory
cd /glade/scratch/obabatun/MPI_Practice

#compile the code 
mpif90 mpiDaxpyCode.f90 -o executable.exe

mpirun -np 36 ./executable.exe
