#!/bin/bash 
#
#PBS -N Daxpy
#PBS -A NTDD0002
#PBS -l walltime=0:05:00
#PBS -q regular
#PBS -l select=2:ncpus=36:mpiprocs=36
#PBS -o checker.out
#PBS -e errcheck.err

#module purge 
#module load openmpi
#module load pgi

#Go to project directory
cd /glade/scratch/obabatun/MPI_Practice


mpirun -np 72 ./run.exe

