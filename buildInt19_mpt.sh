!#/bin/bash -l

module purge
module load intel/19.0.2
module load mpt/2.21
#module load cuda
which mpif90
export PIO=/glade/work/cmille73/intel19_mpt/intel19libs
export NETCDF=/glade/work/cmille73/intel19_mpt/intel19libs
export PNETCDF=/glade/work/cmille73/intel19_mpt/intel19libs
export PATH=$NETCDF/bin:$PATH
export LD_LIBRARY_PATH=$NETCDF/lib:$LD_LIBRARY_PATH
export MPAS_EXTERNAL_LIBS="-L$NETCDF/lib -lhdf5_hl -lhdf5 -lz"
alias ls='ls -l'


#Your project folder here:
cd /glade/scratch/obabatun/MPI_Practice


module list
make clean CORE=atmosphere
make ifort CORE=atmosphere OPENACC=true PRECISION=single USE_PIO2=true
