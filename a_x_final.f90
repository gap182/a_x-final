PROGRAM a_x_final

USE definitions !module with definitions and parameters
USE mat_cal !module with matrices calculation subroutine
USE statistic
IMPLICIT NONE

READ(*,*) z_min
READ(*,*) z_max 
READ(*,*) q_0
READ(*,*) j_0

!Open and use the data from jla written in the doccument jla_lcparams

OPEN (10, file='supercal_vH0.fitres',FORM='FORMATTED',STATUS='OLD',ACTION='READ')

arrays_dimension=0

!Reading initial information

DO i=1,14
READ(10,*)
END DO 




!Reading the names of the table in the file

! READ(10,"(21A)") (table(i),i=1,21)

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

WRITE(*,*) ''
WRITE(*,*) 'the number of data points are:', arrays_dimension

!Assigment of the variable's dimmension

allocate (zcmb(arrays_dimension),zhel(arrays_dimension),dz(arrays_dimension),mb(arrays_dimension),dmb(arrays_dimension), &
logz(arrays_dimension),mb_02(arrays_dimension),name(arrays_dimension), &
nome(arrays_dimension),idsurvey(arrays_dimension),ty(arrays_dimension), field(arrays_dimension), &
z2(arrays_dimension), z2e(arrays_dimension),  host_logmass(arrays_dimension), host_logmass_e(arrays_dimension), &
snrmax1(arrays_dimension), snrmax2(arrays_dimension), snrmax3(arrays_dimension), pkmjd(arrays_dimension), &
pkmjde(arrays_dimension), x1(arrays_dimension), x1e(arrays_dimension), paramc(arrays_dimension), &
paramce(arrays_dimension), x0(arrays_dimension), x0e(arrays_dimension), cov_x1_c(arrays_dimension), &
cov_x1_x0(arrays_dimension), cov_c_x0(arrays_dimension), ndof(arrays_dimension), fitchi2(arrays_dimension), &
fitprob(arrays_dimension), ra(arrays_dimension), decl(arrays_dimension), tgapmax(arrays_dimension), &
mu(arrays_dimension), mue(arrays_dimension), mures(arrays_dimension), mupull(arrays_dimension), sbar(arrays_dimension), &
cbar(arrays_dimension), errcode(arrays_dimension), cidint(arrays_dimension))
!Open the file with the data and creating other files to save specific variables

OPEN (10, file='supercal_vH0.fitres',FORM='FORMATTED',STATUS='OLD',ACTION='READ')
OPEN (7, file='Values_z', status='unknown', form='formatted')
OPEN (8, file='Values_mb', status='unknown', form='formatted')
OPEN (9, file='z_mb', status='unknown', form='formatted')
OPEN (11, file='z_mb_outliers', status='unknown', form='formatted')
! OPEN (12, file='final1', status='unknown', form='formatted') 
! OPEN (18, file='probe1', status='unknown', form='formatted')  !uncomment to make the partial probe of the program
! OPEN (17, file='probe', status='unknown', form='formatted')
! OPEN (19, file='probe2', status='unknown', form='formatted')
! OPEN (20, file='probe3', status='unknown', form='formatted')
OPEN (21, file='Theta', status='unknown', form='formatted')


WRITE(7,"(7X,A,8X,A,9X,A)") 'zcmb', 'dz', 'name'
WRITE(8,"(7X,A,8X,A,9X,A)") 'mb', 'dmb', 'name'
WRITE(9,"(7X,A,8X,A,9X,A)") 'zcmb', 'mb', 'name'

DO i=1,14
READ(10,*)
END DO 

Do p=1, arrays_dimension
    READ(10,*) nome(p), name(p), cidint(p), idsurvey(p), ty(p), field(p), zcmb(p), dz(p), &
    z2(p), z2e(p), host_logmass(p), host_logmass_e(p), snrmax1(p), snrmax2(p), snrmax3(p), &
    pkmjd(p), pkmjde(p), x1(p), x1e(p), paramc(p), paramce(p), mb(p), dmb(p), x0(p), x0e(p), &
    cov_x1_c(p), cov_x1_x0(p), cov_c_x0(p), ndof(p), fitchi2(p), fitprob(p), ra(p), decl(p), &
    tgapmax(p), mu(p), mue(p),  mures(p),  mupull(p), sbar(p), cbar(p), errcode(p)

   

    !#####################################################################################
    WRITE(7,"(2F14.6, 2X, A, A)") zcmb(p), dz(p), '#', name(p)
    WRITE(8,"(2F14.6, 2X, A, A)") mb(p), dmb(p), '#', name(p)
    WRITE(9,"(2F14.6, 2X, A, A)") zcmb(p), mb(p), '#', name(p)
End Do

! WRITE(*,*) nome(1), name(1), cidint(1), idsurvey(1), ty(1), field(1), zcmb(1), dz(1), &
! z2(1), z2e(1), host_logmass(1), host_logmass_e(1), snrmax1(1), snrmax2(1), snrmax3(1), &
! pkmjd(1), pkmjde(1), x1(1), x1e(1), paramc(1), paramce(1), mb(1), dmb(1), x0(1), x0e(1), &
! cov_x1_c(1), cov_x1_x0(1), cov_c_x0(1), ndof(1), fitchi2(1), fitprob(1), ra(1), decl(1), &
! tgapmax(1), mu(1), mue(1),  mures(1),  mupull(1), sbar(1), cbar(1), errcode(1)

CLOSE(7)
CLOSE(8)
CLOSE(9)
CLOSE(10)
CLOSE(11)
CLOSE(12)


!Subroutine to define the matrices 

CALL matrices(z_min,z_max,q_0,j_0,A,Th,Y,V,Inv,AT)

WRITE(*,*) 'the number of data points after the cuts are:', arrays_dimension_outliers



ALLOCATE(ys(arrays_dimension_outliers))

ys(:) = 0.0_dp


!##################################################################################3

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

!################################################################################################

WRITE(*,*) ''
WRITE(*,*) 'the number of data points between z_min and z_max are:', arrays_dimension_outliers

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
! ! !------------------------------!

CALL mat_mult(2,arrays_dimension_outliers,1,L,Y,Th)

CALL statis(logz,ys)

DO i=1, arrays_dimension_outliers
WRITE(100,*) logz(i), ys(i)
END DO 

CALL deviation(mb_02,ys,devi)

CALL fit(devi,mb_02,ys)

WRITE(*,*) 'the standard deviation for the fitter data is:', devi 

WRITE(21,*) Th(1,1), Th(2,1)
WRITE(*,*) ''
WRITE(*,*) 'The files generated by the code are:'
WRITE(*,*) ''
WRITE(*,*) 'Values_z -> redshift values, redshift uncertainty, name'
WRITE(*,*) 'Values_mb -> apparent magnitude, apparent magnitude uncertainty, name'
WRITE(*,*) 'z_mb -> redshift values, apparent magnitude, name (all the values)'
WRITE(*,*) 'z_mb_outliers ->redshift values, apparent magnitude (between z_min and z_max)'
WRITE(*,*) 'final -> log(redshift values), 0.2*apparent magnitude, 0.2*apparent magnitude uncertainty &
(between z_min and z_max)'
WRITE(*,*) 'graph -> log(redshift values), 0.2*apparent magnitude'
WRITE(*,*) 'Theta -> intercept, slope'
WRITE(*,*) ''
END PROGRAM a_x_final
