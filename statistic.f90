module statistic

implicit none

CONTAINS 

subroutine statis(xs,ys_1)
USE definitions
REAL(dp),intent(in)  :: xs(arrays_dimension)
REAL(dp), intent(out) :: ys_1(arrays_dimension_outliers)  
    
    DO i=1, arrays_dimension_outliers
        ys_1(i) = linear(xs(i))
    END DO 



end subroutine statis

subroutine deviation(mb_02_m,mb_02_e,devi_s)

USE definitions

REAL(dp),intent(in)  :: mb_02_m(arrays_dimension), mb_02_e(arrays_dimension_outliers)
REAL(dp),intent(out)  :: devi_s

devi_s=0.0_dp

DO i=1,arrays_dimension_outliers
devi_s = devi_s+(mb_02_m(i)-mb_02_e(i))**2
END DO 

devi_s=sqrt(devi_s/arrays_dimension_outliers)

WRITE(*,*) devi_s 

END subroutine deviation

subroutine fit(devi_s,mb_02_m,mb_02_e)

    USE definitions
    integer :: cuenta
    REAL(dp),intent(in) :: devi_s, mb_02_e(arrays_dimension_outliers), mb_02_m(arrays_dimension_outliers)
    REAL(dp)::devi3
    devi3=devi_s*3
    cuenta=0
    OPEN(210, FILE='fitterdata.txt', FORM='formatted', STATUS='unknown')
    
    DO i=1, arrays_dimension_outliers
        IF (mb_02_m(i) < mb_02_e(i)+devi3 .and. mb_02_m(i) > mb_02_e(i)-devi3) THEN
            WRITE(210,*) logz(i), mb_02_m(i)
            cuenta=cuenta+1
        END IF 
    END DO 

    WRITE(*,*) 'The number of final fitter data with no more of 3sigma is:', cuenta

    CLOSE(210)

    ! OPEN(210, FILE='fitterdata.txt', FORM='formatted', STATUS='unknown')

    ! IF (ALLOCATED(logz)) DEALLOCATE(logz)
    ! IF (ALLOCATED(mb_02)) DEALLOCATE(mb_02)

    ! ALLOCATE(logz(cuenta),mb_02(cuenta))

    ! DO i=1, cuenta
    !     READ(210,*) logz(i), mb_02(i)

    ! END DO 

END subroutine fit 

function linear(xl) result(yl)
USE definitions
REAL(dp) :: xl, yl 

yl = Th(2,1)*xl + Th(1,1)
    
end function linear



end module statistic
