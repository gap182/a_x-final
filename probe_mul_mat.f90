program probe_mul_mat   
    
    USE definitions
    USE mat_cal
    
    IMPLICIT NONE
    INTEGER :: fil,col,col2
    REAL(dp), ALLOCATABLE :: AP(:,:),BP(:,:),CP(:,:),CPI(:,:)
    
    READ(*,*) fil
    READ(*,*) col
    READ(*,*) col2

    ALLOCATE(AP(fil,col),BP(col,col2),CP(fil,col2),CPI(fil,col2))

    AP(:,:)=0.0
    BP(:,:)=0.0
    
    
    DO i=1,fil
        DO j=1,col
            AP(i,j)=i+j
        END DO
    END DO
    
    DO i=1,col
        DO j=1,col2
            BP(i,j)=i*j
        END DO
    END DO
    
    
!         DO i=1,fil
!             DO j=1,col
!                 WRITE(*,*) AP(i,j)
!             END DO
!         END DO
!         
!         DO i=1,col
!             DO j=1,col2
!                 WRITE(*,*) BP(i,j)
!             END DO
!         END DO
!     
    CALL mat_mult(fil,col,col2,AP,BP,CP)
        DO i=1,fil
            
                WRITE(*,*) (CP(i,j),j=1,col2)
           
        END DO
        
        CPI(:,:)=0.0_dp
        
    CALL inverse(AP,CPI,fil)
! 
!     
    DO i=1, fil
        WRITE(*,*) (CPI(i,j),j=1,col2)
    END DO
END PROGRAM probe_mul_mat
            
