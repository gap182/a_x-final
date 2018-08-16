PROGRAM a_x_final

USE definitions !module with definitions and parameters
USE mat_cal !module with matrices calculation subroutine
IMPLICIT NONE

READ(*,*) z_min
READ(*,*) z_max 
READ(*,*) q_0
READ(*,*) j_0

!Open and use the data from jla written in the doccument jla_lcparams

OPEN (10, file='jla_lcparams.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')

arrays_dimension=0

!Reading the names of the table in the file

READ(10,"(21A)") (table(i),i=1,21)

!Counting the number of data represented in the rows of the file

Do

    READ(10,*,iostat=stat) 
    
        If (stat .ne. 0) then
            exit
        Else
            arrays_dimension=arrays_dimension+1
        End If
    
End Do

CLOSE(10)

!#############################
!probing the program till here
!#############################


! WRITE(*,*) (table(i),i=1,21)
! WRITE(*,*) arrays_dimension

!#############################
!#############################

WRITE(*,*) 'El número de datos es', arrays_dimension

!Assigment of the variable's dimmension

allocate (zcmb(arrays_dimension),zhel(arrays_dimension),dz(arrays_dimension),mb(arrays_dimension),dmb(arrays_dimension), &
xone(arrays_dimension),dxone(arrays_dimension),color(arrays_dimension),dcolor(arrays_dimension), &
threerdvar(arrays_dimension),dthreerdvar(arrays_dimension),tmax(arrays_dimension),dtmax(arrays_dimension), &
 cov_m_s(arrays_dimension),cov_m_c(arrays_dimension),cov_s_c(arrays_dimension),set(arrays_dimension),ra(arrays_dimension), &
logz(arrays_dimension),mb_02(arrays_dimension),dec(arrays_dimension),biascor(arrays_dimension),name(arrays_dimension))

!Open the file with the data and creating other files to save specific variables

OPEN (10, file='jla_lcparams.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')
OPEN (7, file='Values_z', status='unknown', form='formatted')
OPEN (8, file='Values_mb', status='unknown', form='formatted')
OPEN (9, file='z_mb', status='unknown', form='formatted')
OPEN (11, file='z_mb_outliers', status='unknown', form='formatted')
OPEN (12, file='final1', status='unknown', form='formatted')
OPEN (18, file='probe1', status='unknown', form='formatted')
OPEN (17, file='probe', status='unknown', form='formatted')
OPEN (19, file='probe2', status='unknown', form='formatted')
OPEN (20, file='probe3', status='unknown', form='formatted')
OPEN (21, file='Theta', status='unknown', form='formatted')


WRITE(7,"(7X,A,8X,A,9X,A)") 'zcmb', 'dz', 'name'
WRITE(8,"(7X,A,8X,A,9X,A)") 'mb', 'dmb', 'name'
WRITE(9,"(7X,A,8X,A,9X,A)") 'zcmb', 'mb', 'name'
READ(10,*)

Do p=1, arrays_dimension
    READ(10,*) name(p), zcmb(p),zhel(p),dz(p),mb(p),&
    dmb(p),xone(p),dxone(p), &
    color(p),dcolor(p),threerdvar(p), &
    dthreerdvar(p),tmax(p),dtmax(p), &
    cov_m_s(p),cov_m_c(p),cov_s_c(p), &
    set(p),ra(p),dec(p),biascor(p)

    WRITE(7,"(2F14.6, 2X, A, A)") zcmb(p), dz(p), '#', name(p)
    WRITE(8,"(2F14.6, 2X, A, A)") mb(p), dmb(p), '#', name(p)
    WRITE(9,"(2F14.6, 2X, A, A)") zcmb(p), mb(p), '#', name(p)
End Do

CLOSE(7)
CLOSE(8)
CLOSE(9)
CLOSE(10)
CLOSE(11)
CLOSE(12)


!Subroutine to define the matrices 

CALL matrices(z_min,z_max,q_0,j_0,A,Th,Y,V,Inv,AT)

!-----------------
!Probe the program
!-----------------
! OPEN (16, file='probe', status='unknown', form='formatted')
! 
! WRITE(*,*) arrays_dimension_outliers
! 
! DO i=1,arrays_dimension_outliers
!     DO j=1, arrays_dimension_outliers  
!         WRITE(*,*) Inv(i,j)
!     END DO
! END DO

! 
! DO i=1,arrays_dimension_outliers
!     WRITE(*,*) A(i,1), A(i,2)
!  END DO
! ! 
!  DO i=1,arrays_dimension_outliers
!    
!         WRITE(18,*) (Inv(i,j),j=1,arrays_dimension_outliers)
!     
! END DO
!-------------------------------------------------------------
 
!----------------

!Re -> V-1*A
!Ree -> AT*Re
!ReeInv -> Ree-1


CALL mat_mult(2,arrays_dimension_outliers,arrays_dimension_outliers,AT,Inv,Re)

!--------------------------------------
!Probe of the multiplication subroutine
! WRITE(*,*) Ree(3,2)
! 
! ! 
! DO i=1,arrays_dimension_outliers
!         WRITE(17,*) Re(i,1), Re(i,2)
! END DO

! ! DO i=1, arrays_dimension_outliers
! !     prueba2=prueba2+Inv(2,i)*A(i,2)
! ! END DO
! ! 
! ! WRITE(*,*) prueba2
!--------------------------------------

CALL mat_mult(2,arrays_dimension_outliers,2,Re,A,Ree)

! ------------------------------
! Probe of the multiplication 

! WRITE(17,*) Ree(1,1), Ree(1,2)
! WRITE(17,*) Ree(2,1), Ree(2,2)
!!!!

! ------------------------------

CALL matinv2(Ree,ReeInv)

! ! !------------------------------
! ! !Probe of the inverse 
! ! !
! WRITE(19,*) ReeInv(1,1), ReeInv(1,2)
! WRITE(19,*) ReeInv(2,1), ReeInv(2,2)
! ! !!!!!
! ! !
! ! !------------------------------

CALL mat_mult(2,2,arrays_dimension_outliers,ReeInv,Re,L)

! ! !------------------------------
! ! !Probe of product
! ! ! !
!     DO i=1,arrays_dimension_outliers+1
! WRITE(20,*) L(1,i), L(2,i)
!     end do
! ! ! !!!!!
! ! !
! ! !------------------------------

CALL mat_mult(2,arrays_dimension_outliers,1,L,Y,Th)

WRITE(21,*) Th(1,1), Th(2,1)

END PROGRAM a_x_final
