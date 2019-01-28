rm scolnic mat_cal_scolnic.o
gfortran -c mat_cal_scolnic.f90
gfortran -o scolnic definitions.o mat_cal_scolnic.o a_x_scolnic.f90
./scolnic < data.dat