FF90=gfortran

definitions:
	$(FF90) -c definitions.f90
mat_cal: definitions.o
	$(FF90) -c mat_cal.f90
a_x_2: definitions.o mat_cal.o
	$(FF90) -o a_x_2 a_x_final.f90 definitions.o mat_cal.o
clean:
	rm *.o