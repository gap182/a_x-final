MODULE mat_cal
    IMPLICIT NONE

    CONTAINS
    
    SUBROUTINE matrices(z_min_1,z_max_1,q_0_1,j_0_1,A_1,Th_1,Y_1,V_1,Inv_1,AT_1)
        USE definitions
        REAL(dp), INTENT(IN) :: z_min_1,z_max_1,q_0_1,j_0_1
        REAL(dp), ALLOCATABLE,  INTENT(OUT) :: A_1(:,:),Th_1(:),Y_1(:),V_1(:,:),Inv_1(:,:),AT_1(:,:)
        
        OPEN (11, file='z_mb_outliers', status='unknown', form='formatted')
        OPEN (9, file='z_mb', status='unknown', form='formatted')
        OPEN (12, file='final', status='unknown', form='formatted')
        OPEN (14, file='graph', status='unknown', form='formatted')
        
        !Reading the redshift and mb data, the data into the limits are selected and write in a new file
        !Calculate the log(z) and 0.2mb
        
        READ(9,*)
        arrays_dimension_outliers=0
        DO i=1, arrays_dimension   
            READ(9,*) zcmb(i),mb(i)    
                IF (zcmb(i) > z_min_1 .and. zcmb(i) < z_max_1) THEN
                
                    write(11,*) zcmb(i),mb(i)
                        logz(i)=LOG10(299792.458*zcmb(i)*(1+0.5*(1-q_0_1)*zcmb(i)-((1-q_0_1-3*(q_0_1**2)+j_0_1)*(zcmb(i))**2)/6))
                        
                        mb_02(i)=0.2*mb(i)
         
                    write(12,*) logz(i),mb_02(i),dmb(i)
                    write(14,*) logz(i),mb_02(i)
        arrays_dimension_outliers=arrays_dimension_outliers+1
                END IF
        END DO
            
        CLOSE(9)
        CLOSE(11)
        CLOSE(12)
        CLOSE(14)
        
        !Open the file with logz and 0.2mb
        
        OPEN (13, file='final', status='old', form='formatted') 
        
        ALLOCATE(A_1(arrays_dimension_outliers,2),Th_1(2),Y_1(arrays_dimension_outliers), &
        V_1(arrays_dimension_outliers,arrays_dimension_outliers), &
        Inv_1(arrays_dimension_outliers,arrays_dimension_outliers), &
        AT_1(2,arrays_dimension_outliers), Re(arrays_dimension_outliers,2), &
        Ree(2,2))
        
        A_1(:,:)=0.0_dp
        Th_1(:)=0.0_dp
        Y_1(:)=0.0_dp
        V_1(:,:)=0.0_dp
        Inv_1(:,:)=0.0_dp
        
        DO i=1, arrays_dimension_outliers
        read(13,*) logz(i),mb_02(i),dmb(i)
        Y_1(i)=mb_02(i)
        END DO
! 
! 
        DO i=1, arrays_dimension_outliers
            A_1(i,1)=1
            A_1(i,2)=logz(i)
        END DO
! 
! 
        DO i=1, arrays_dimension_outliers
            DO j=1, arrays_dimension_outliers
                IF (i==j) then  
                    Inv_1(i,j)=1/((0.2*dmb(i))**2)
                ELSE
                    Inv_1(i,j)=0.0_dp
                END IF
            END DO
        END DO

        DO i=1, arrays_dimension_outliers
            DO j=1,2
                AT_1(j,i)=A_1(i,j)
            END DO
        END DO
        
        
        
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
    
!  SUBROUTINE inverse(g,t,fin)
! !============================================================
! ! Inverse matrix
! ! Method: Based on Doolittle LU factorization for Ax=b
! ! Alex G. December 2009
! !-----------------------------------------------------------
! ! input ...
! ! a(n,n) - array of coefficients for matrix A
! ! n      - dimension
! ! output ...
! ! c(n,n) - inverse matrix of A
! ! comments ...
! ! the original matrix a(n,n) will be destroyed 
! ! during the calculation
! !===========================================================
! implicit none
! INTEGER, PARAMETER :: dp = selected_real_kind(14,200)
! integer n
! REAL(dp), ALLOCATABLE, INTENT(INOUT) :: g(:,:)
! REAL(dp), ALLOCATABLE :: L(:,:), U(:,:), h(:), t(:), x(:)
! REAL(dp), ALLOCATABLE, INTENT(OUT) :: fin(:,:)
! REAL(dp) :: coeff
! integer i, j, k
! 
! ALLOCATE(L(n,n),U(n,n),h(n),t(n),x(n))
! 
! ! step 0: initialization for matrices L and U and b
! ! Fortran 90/95 aloows such operations on matrices
! L=0.0_dp
! U=0.0_dp
! h=0.0_dp
!  
!  coeff=0.0_dp
! 
! ! step 1: forward elimination
! do k=1, n-1
!    do i=k+1,n
!       coeff=g(i,k)/g(k,k)
!       L(i,k) = coeff
!       do j=k+1,n
!          g(i,j) = g(i,j)-coeff*g(k,j)
!       end do
!    end do
! end do
! 
! ! Step 2: prepare L and U matrices 
! ! L matrix is a matrix of the elimination coefficient
! ! + the diagonal elements are 1.0
! do i=1,n
!   L(i,i) = 1.0
! end do
! ! U matrix is the upper triangular part of A
! do j=1,n
!   do i=1,j
!     U(i,j) = g(i,j)
!   end do
! end do
! 
! ! Step 3: compute columns of the inverse matrix C
! do k=1,n
!   h(k)=1.0
!   t(1) = h(1)
! ! Step 3a: Solve Ld=b using the forward substitution
!   do i=2,n
!     t(i)=h(i)
!     do j=1,i-1
!       t(i) = t(i) - L(i,j)*t(j)
!     end do
!   end do
! ! Step 3b: Solve Ux=d using the back substitution
!   x(n)=t(n)/U(n,n)
!   do i = n-1,1,-1
!     x(i) = t(i)
!     do j=n,i+1,-1
!       x(i)=x(i)-U(i,j)*x(j)
!     end do
!     x(i) = x(i)/u(i,i)
!   end do
! ! Step 3c: fill the solutions x(n) into column k of C
!   do i=1,n
!     fin(i,k) = x(i)
!   end do
!   h(k)=0.0
! end do
! END SUBROUTINE inverse

 subroutine inverse(As,Cs,Ns)
!============================================================
! Inverse matrix
! Method: Based on Doolittle LU factorization for Ax=b
! Alex G. December 2009
!-----------------------------------------------------------
! input ...
! a(n,n) - array of coefficients for matrix A
! n      - dimension
! output ...
! c(n,n) - inverse matrix of A
! comments ...
! the original matrix a(n,n) will be destroyed 
! during the calculation
!===========================================================

USE definitions
integer n
REAL(dp)  As(n,n), Cs(n,n)
REAL(dp)  Ls(n,n), Us(n,n), Bs(n), Ds(n), Xs(n)
REAL(dp)  coeff


! step 0: initialization for matrices L and U and b
! Fortran 90/95 aloows such operations on matrices
L=0.0
U=0.0
b=0.0

! step 1: forward elimination
do k=1, n-1
   do i=k+1,n
      coeff=As(i,k)/As(k,k)
      Ls(i,k) = coeff
      do j=k+1,n
         As(i,j) = As(i,j)-coeff*As(k,j)
      end do
   end do
end do

! Step 2: prepare L and U matrices 
! L matrix is a matrix of the elimination coefficient
! + the diagonal elements are 1.0
do i=1,n
  Ls(i,i) = 1.0
end do
! U matrix is the upper triangular part of A
do j=1,n
  do i=1,j
    Us(i,j) = As(i,j)
  end do
end do

! Step 3: compute columns of the inverse matrix C
do k=1,n
  Bs(k)=1.0
  Ds(1) = Bs(1)
! Step 3a: Solve Ld=b using the forward substitution
  do i=2,n
    Ds(i)=Bs(i)
    do j=1,i-1
      Ds(i) = Ds(i) - Ls(i,j)*Ds(j)
    end do
  end do
! Step 3b: Solve Ux=d using the back substitution
  Xs(n)=Ds(n)/Us(n,n)
  do i = n-1,1,-1
    Xs(i) = Ds(i)
    do j=n,i+1,-1
      Xs(i)=Xs(i)-Us(i,j)*Xs(j)
    end do
    Xs(i) = Xs(i)/Us(i,i)
  end do
! Step 3c: fill the solutions x(n) into column k of C
  do i=1,n
    Cs(i,k) = Xs(i)
  end do
  Bs(k)=0.0
end do
end subroutine inverse
 

END MODULE mat_cal
