!To use this subroutine you must indicate the name of the file with its extension
!row is the number of rows of the data file
!col is the number of columns of the data file
!a is a (row,col-intcol-charcol) matrix to save the real numbers data of the file
!b is a (row,intcol) matrix to save the integer numbers data of the file
!c is a (row,charcol) matrix to save the characters data of the file
!disc is the number of initial rows to be discard of the file
!dp is the parameter to define the precision of the real data
!charcol is the number of character data (number of columns with character type data)
!intcol is the number of integer data (number of colums with integer type data)
!intpos is an array with the positions of the integer columns
!charpos is an array with the positions of the character columns

subroutine read_data(name,col,row, a,intcol,b,charcol,c,disc,intpos,charpos)

    implicit none

    INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(P=15)
    character(30) :: name, prueba
    character(100) :: renam 
    character(30) :: aux(row,col)
    integer :: i,j,k, cont_int, cont_re, cont_char, realnum
    integer, intent(in) :: disc, col, intcol, charcol, row
    integer, intent(in) :: intpos(intcol), charpos(charcol) 
    real(dp), intent(out) :: a(row,col-intcol-charcol)
    integer, intent(out), allocatable :: b(:,:)
    character(30), intent(out) :: c(row,charcol)

    allocate(b(row,intcol))

    cont_int=1
    cont_re=1
    cont_char=1
    realnum=col-intcol-charcol
    ! row = 0

    !the program will calculate the number of rows to read from the text file

    ! open(107, file=name)

    !     do i=1, disc
    !         read(107,*)
    !     end do
    
    !     do 
    !         read(107,*,iostat=io)
            
    !             if (io .ne. 0) then
    !                 exit
    !             else
    !                 row=row+1
    !             end if
    !     end do

        ! allocate(a(row,realnum), intpos(intcol), charpos(charcol))

    !     write(*,*) 'the number of rows to read of the file are:', row

    ! close(107)

    ! write(*,*) 'write the column positions of the integer data:'
    
    ! do j=1,intcol
    !     read(*,*) intpos(j) 
    ! end do

    ! write(*,*) 'write the column positions of the character data'

    ! do j=1,charcol
    !     read(*,*) charpos(j)
    ! end do
    ! renam=trim("/home/gap182/Documents/Git/extras/")//trim(name)

 

!     ! write(*,*) renam 
!     !the program is going to star to save the data in the respective matrices
! write(*,*) renam, trim(name)

    ! prueba=name
    ! write(*,*) prueba, 'yes'  

    ! ! if (prueba .eq. 'yes') then 
    ! !     write(*,*) 'si'
    ! ! end if


    !     if (name .eq. 'jla_lcparams.txt') then 
    !         write(*,*) 'sipi'
    !     end if 
    open(307, file=name, status="unknown", form="formatted")
    
    
!     ! open(110, file='auxchar')

! read(307,*)
        do i=1, disc
            read(307,*)
        end do

    

        do i=1,row
            read(307,*) (aux(i,j), j=1, col)
            ! write(*,*) aux(i,18)
        end do

        close(307)

            do i=1, row
                do j=1, col           
                
                    if (any(intpos==j)) then
                        ! write(*,*) i,j
                        
                        open(109, file='auxint')
                        ! write(*,*) i, j, aux(i,j), 'esto es a'
                        write(109,*) aux(i,j)
                        ! write(*,*) aux(i,j)
                        close(109)
                        open(109, file='auxint')
                        read(109,"(I5)") b(i,cont_int)

                        ! write(*,*) i,j, cont_int, b(i,cont_int), 'esto es b'
                        ! write(*,*) aux(1,4), 'esto es a'
                        close(109)
                        ! write(*,*) b(1,1), 'esto es b'
                        cont_int=cont_int+1
                            if (cont_int > intcol) then
                                cont_int=1
                            end if
                            
                    ! write(*,*) b(i,cont_int), 'estoy aquí'
                    else if (any(charpos==j)) then
                        c(i,cont_char)=aux(i,j)
                        
                        cont_char=cont_char+1
                        
                            if (cont_char > charcol) then
                                cont_char=1
                            end if

                    else
                        open(108, file='auxrel')
                        write(108,*) aux(i,j)
                        close(108)
                        open(108, file='auxrel')
                        read(108,"(F15.8)") a(i,cont_re)
                        close(108)
                        cont_re=cont_re+1
                            if (cont_re > realnum) then
                                cont_re=1
                            end if
                    end if 
                end do
            end do

            ! do i=1,row    
            
            !     write(100,*) (a(i,j), j=1, realnum)
                
            ! end do
        
end subroutine read_data