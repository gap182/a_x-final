MODULE mat_cal
    IMPLICIT NONE

    CONTAINS
    
    SUBROUTINE matrices(z_min_1,z_max_1,q_0_1,j_0_1,A_1,Th_1,Y_1,V_1,Inv_1,AT_1)
        USE definitions
        REAL(dp), INTENT(IN) :: z_min_1,z_max_1,q_0_1,j_0_1
        REAL(dp), ALLOCATABLE,  INTENT(OUT) :: A_1(:,:),Th_1(:,:),Y_1(:,:),V_1(:,:),Inv_1(:,:),AT_1(:,:)
        
        OPEN (11, file='z_mb_outliers', status='unknown', form='formatted')
        OPEN (9, file='z_mb', status='unknown', form='formatted')
        OPEN (12, file='final', status='unknown', form='formatted')
        OPEN (14, file='graph', status='unknown', form='formatted')
        
        !Reading the redshift and mb data, the data into the limits are selected and write in a new file
        !Calculate the log(z) and 0.2mb
        
    
        arrays_dimension_outliers=0


        DO i=1, arrays_dimension   
            
                IF (realmatrix(i,8) > z_min_1 .and. realmatrix(i,8) < z_max_1) THEN
                    IF (realmatrix(i,21) < 3.0 .and. realmatrix(i,21)>-3.0) THEN
                        IF (realmatrix(i,23) < 0.3 .and. realmatrix(i,23)>-0.3) THEN
                            IF (realmatrix(i,34)>0.001) THEN
                                IF (realmatrix(i,20)<2.0) THEN
                                    IF (realmatrix(i,22)<1.5) THEN
                                        IF (realmatrix(i,26)<0.2) THEN
                                  write(100,*) i     
                    ! write(11,*) realmatrix(i,8),realmatrix(i,25)
                    !     logz(i)=LOG10(299792.458*realmatrix(i,8)*&
                    !     (1+0.5*(1-q_0_1)*realmatrix(i,8)-((1-q_0_1-3*(q_0_1**2)+j_0_1)*(realmatrix(i,8))**2)/6))
                    
                    !     mb_02(i)=0.2*realmatrix(i,25)
         
                    ! write(12,*) logz(i),mb_02(i),realmatrix(i,26)
                    ! write(14,*) logz(i),mb_02(i)
        arrays_dimension_outliers=arrays_dimension_outliers+1
        ! write(*,*) "I'm here", arrays_dimension_outliers
        ! write(*,*) "I'm here"
                                        END IF 
                                    END IF 
                                END IF 
                            END IF 
                        END IF
                    END IF 
                END IF
        END DO
            
        CLOSE(9)
        CLOSE(11)
        CLOSE(12)
        CLOSE(14)
        
        ! write(*,*) 'salí'

        !Open the file with logz and 0.2mb
        
!         OPEN (13, file='final', status='old', form='formatted') 
        
!         ALLOCATE(A_1(arrays_dimension_outliers,2),Th_1(2,1),Y_1(arrays_dimension_outliers,1), &
!         V_1(arrays_dimension_outliers,arrays_dimension_outliers), &
!         Inv_1(arrays_dimension_outliers,arrays_dimension_outliers), &
!         AT_1(2,arrays_dimension_outliers), Re(2,arrays_dimension_outliers), &
!         Ree(2,2), ReeInv(2,2),L(2,arrays_dimension_outliers))
        
!         A_1(:,:)=0.0_dp
!         Th_1(:,:)=0.0_dp
!         Y_1(:,:)=0.0_dp
!         V_1(:,:)=0.0_dp
!         Inv_1(:,:)=0.0_dp
        
!         write(*,*) 'flag1'

!         DO i=1, arrays_dimension_outliers
!         read(13,*) logz(i),mb_02(i),dmb(i)
!         Y_1(i,1)=mb_02(i)
!         END DO
! ! 
!       write(*,*) 'flag2'

!         DO i=1, arrays_dimension_outliers
!             A_1(i,1)=1
!             A_1(i,2)=logz(i)
!         END DO
! ! 
!       write(*,*) 'flag3'

!         DO i=1, arrays_dimension_outliers
!             DO j=1, arrays_dimension_outliers
!                 IF (i==j) then  
!                     Inv_1(i,j)=1/((0.2*dmb(i))**2)
!                 ELSE
!                     Inv_1(i,j)=0.0_dp
!                 END IF
!             END DO
!         END DO

!         write(*,*) 'flag3'

!         DO i=1, arrays_dimension_outliers
!             DO j=1,2
!                 AT_1(j,i)=A_1(i,j)
!             END DO
!         END DO
        
        
        
    END SUBROUTINE matrices
    
    SUBROUTINE mat_mult(dim_row,dim_col,dim_col2,C,D,E)
    
        USE definitions
     
        INTEGER, INTENT(IN) :: dim_row, dim_col,dim_col2
        REAL(dp), INTENT(INOUT) :: C(:,:),D(:,:)
        REAL(dp), INTENT(OUT) :: E(:,:)
    
!       This subroutine calculates the product of a matrix C(dim_row,dim_col)*D(dim_col,dim_row)=E(dim_row,dim_row)
        
!         ALLOCATE(C(dim_row,dim_col),D(dim_col,dim_row),E(dim_row,dim_row))
        
       E(:,:)=0.0_dp
        
        DO i=1,dim_row
            DO k=1,dim_col2
                DO j=1, dim_col
                E(i,k)=E(i,k)+C(i,j)*D(j,k)
                END DO
            END DO
        END DO
        
    END SUBROUTINE mat_mult
    
    subroutine matinv2(Ainv,B)
  
    implicit NONE



    !! Performs a direct calculation of the inverse of a 2×2 matrix.
    INTEGER, PARAMETER :: dp = selected_real_kind(14,200)
    REAL(dp), intent(in) :: Ainv(2,2)   !! Matrix
    REAL(dp)             :: B(2,2)   !! Inverse matrix
    REAL(dp)             :: detinv

    ! Calculate the inverse determinant of the matrix
    detinv = 1/(Ainv(1,1)*Ainv(2,2) - Ainv(1,2)*Ainv(2,1))

    ! Calculate the inverse of the matrix
    B(1,1) = +detinv * Ainv(2,2)
    B(2,1) = -detinv * Ainv(2,1)
    B(1,2) = -detinv * Ainv(1,2)
    B(2,2) = +detinv * Ainv(1,1)
    end subroutine                
 


END MODULE mat_cal
