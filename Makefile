FF90=gfortran

definitions.o:
	$(FF90) -c definitions.f90
mat_cal.o: definitions.o
	$(FF90) -c mat_cal.f90
read_data_comp.o:
	$(FF90) -c read_data_comp.f90
a_x_def: definitions.o mat_cal.o read_data_comp.o
	$(FF90) -o a_x_def a_x_def.f90 definitions.o read_data_comp.o mat_cal.o
clean:
	rm *.o *.mod