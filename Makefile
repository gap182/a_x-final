FF90=gfortran

definitions.o:
	$(FF90) -c definitions.f90
mat_cal.o: definitions.o
	$(FF90) -c mat_cal.f90
statistic.o: definitions.o
	$(FF90) -c statistic.f90
a_x_2: definitions.o statistic.o mat_cal.o
	$(FF90) -o a_x_2 a_x_final.f90 definitions.o statistic.o mat_cal.o
a_x_scolnic: definitions.o statistic.o mat_cal.o
	$(FF90) -o a_x_scol a_x_scolnic.f90 definitions.o statistic.o mat_cal.o
clean:
	rm *.o
