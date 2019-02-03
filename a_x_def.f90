program a_x_def

USE definitions
USE mat_cal

!Parámetros de restricción 
READ(*,*) z_min
READ(*,*) z_max 
READ(*,*) q_0
READ(*,*) j_0
!Parámetros del archivo de datos
read(*,*) name  !nombre del archivo incluyendo la extensión
read(*,*) disc  !número de filas iniciales para descartar de la lectura
read(*,*) col   !número de columnas a leer del archivo
read(*,*) intcol    !número de columnas del archivo que contienen datos enteros =
read(*,*) charcol   !número de columnas del archivo que contienen datos caracteres

open(10, file=name, form='formatted', status='old',action='read')

arrays_dimension=0


!Lectura de las primeras filas que hay que descartar del archivo de texto de datos

    do i=1, disc
        read(10,*)
    end do

    do

!Lectura del número de datos en el archivo de texto

    READ(10,*,iostat=stat) 
    
        If (stat .ne. 0) then
            exit
        Else
            arrays_dimension=arrays_dimension+1
        End If
    
    end do

close(10)

write(*,*) 'The number of data points are:', arrays_dimension

allocate(realmatrix(arrays_dimension,col),intmatrix(arrays_dimension,col),charmatrix(arrays_dimension,col), &
intpos(intcol),charpos(charcol),logz(arrays_dimension), mb_02(arrays_dimension), dmb(arrays_dimension))

!Se debe agregar manualmente el arreglo intpos y charpos que contienen
!la posición de columnas que contienen enteros y caracteres respectivamente

intpos = (/3,4,5,7,13,32,47,54/)
charpos = (/1,2,6/)

call read_data(name,col,arrays_dimension,realmatrix,intcol,intmatrix,charcol,charmatrix,disc,intpos,charpos)

do i=1,arrays_dimension 
    write(600,*) realmatrix(i,8)
end do

write(*,*) realmatrix(700,8)

CALL matrices(z_min,z_max,q_0,j_0,A,Th,Y,V,Inv,AT)

write(*,*) 'the number of fitter data are:', arrays_dimension_outliers 

end program a_x_def