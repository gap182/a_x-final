program a_x_def
implicit none

USE definitions
USE mat_cal

READ(*,*) z_min
READ(*,*) z_max 
READ(*,*) q_0
READ(*,*) j_0
read(*,*) name 
read(*,*) disc
read(*,*) col 
read(*,*) intcol
read(*,*) charcol


 
open(10, file=name, form='formatted', satus='old',action='read')

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
intpos(intcol),charpos(charcol))



end program a_x_def