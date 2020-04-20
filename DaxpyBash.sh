#! /bin/bash -1
#
#PBS -A NTDD0002
#PBS -l walltime=0:00:01
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
mpif90 ProcMpi.f90 -o executable.exe

mpirun -np 20 ./executable.exe
