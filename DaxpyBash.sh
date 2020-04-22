#! /bin/bash -1
#
#PBS -A NTDD0002
#PBS -l walltime=0:05:00
#PBS -l select=3:ncpus=36:mpiprocs=36
#PBS -N Daxpy
#PBS -o checker.out
#PBS -e errcheck.err
#PBS -q regular

module purge 
module load openmpi
module load intel

#Go to project directory
cd /glade/scratch/obabatun/MPI_Practice

mpirun -np 108 ./run.exe

