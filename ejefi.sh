rm *.o *.mod a_x_2
gfortran -c definitions.f90
gfortran -c mat_cal.f90
gfortran -c statistic.f90
gfortran -o a_x_2 definitions.o mat_cal.o statistic.o a_x_final.f90
./a_x_2 < data.dat
