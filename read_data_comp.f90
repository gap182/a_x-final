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
!the real trash values are 1*10^8, the integer trash values are -1000000 and the character 
!trash values are blank spaces

subroutine read_data(name,col,row, a,intcol,b,charcol,c,disc,intpos,charpos)

    implicit none

    INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(P=15)
    character(80) :: name
    character(30) :: aux(row,col)
    integer :: i,j, cont_int, cont_re, cont_char, realnum
    integer, intent(in) :: disc, col, intcol, charcol, row
    integer, intent(in) :: intpos(intcol), charpos(charcol) 
    real(dp), intent(out) :: a(row,col)
    integer, intent(out) :: b(row,col)
    character(30), intent(out) :: c(row,col)

    cont_int=1
    cont_re=1
    cont_char=1
    realnum=col-intcol-charcol
  
    open(307, file=name, status="unknown", form="formatted")
    

        do i=1, disc
            read(307,*)
        end do

        do i=1,row
            read(307,*) (aux(i,j), j=1, col)
        end do

        close(307)

        a(:,:) = 1*(10**8)
        b(:,:) = -1000000
        c(:,:) = ' '

            do i=1, row
                do j=1, col           
                
                    if (any(intpos==j)) then
                       
                        
                        open(109, file='auxint')
                        write(109,*) aux(i,j)
                        close(109)
                        open(109, file='auxint')
                        read(109,"(I5)") b(i,j)
                        close(109)

                    else if (any(charpos==j)) then
                        c(i,j)=aux(i,j)
                        
                    else
                        open(108, file='auxrel')
                        write(108,*) aux(i,j)
                        close(108)
                        open(108, file='auxrel')
                        read(108,"(F15.8)") a(i,j)
                        close(108)

                    end if 
                end do
            end do

            ! !!! Use this to probe that the data saved is correct
            ! do i=1,row    
            !     write(100,*) (a(i,j), j=1, col)
            !     write(200,*) (b(i,j), j=1, col)
            !     write(300,*) (c(i,j), j=1, col)
            ! end do
        
end subroutine read_data