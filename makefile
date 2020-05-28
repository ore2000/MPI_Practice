#This is my Daxpy makefile
#start of the Makefile

obj = oneDimensionMPINew.o oneDimensionMPINew.mod run.exe resultFile.log 
objects = oneDimensionMPINew.f90
f90comp = mpif90

run.exe: $(objects)
	$(f90comp) -o run.exe $(objects)

oneDimensionMPINew.mod: oneDimensionMPINew.o oneDimensionMPINew.f90
	$(f90comp) -c oneDimensionMPINew.f90
$(objects): oneDimensionMPINew.f90
	$(f90comp) -c oneDimensionMPINew.f90

clean: 
	rm $(obj)

