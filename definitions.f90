Module definitions

IMPLICIT NONE
INTEGER, PARAMETER :: dp = selected_real_kind(14,200)
INTEGER :: i, j, k, nlines, stat, arrays_dimension,p,arrays_dimension_outliers,pr
REAL(dp) :: z_min, z_max, q_0, j_0, m, a_x, sxy,sx,sy,sx2,sx_2,prueba2, devi
REAL(dp), ALLOCATABLE :: zcmb(:),zhel(:),dz(:),mb(:),dmb(:),logz(:), mb_02(:), &
A(:,:),AT(:,:),Th(:,:),Y(:,:),V(:,:),Inv(:,:),Re(:,:),Ree(:,:),tmax(:),dthreerdvar(:),threerdvar(:), &
ReeInv(:,:), L(:,:),z2(:),z2e(:), host_logmass(:), host_logmass_e(:), snrmax1(:), snrmax2(:), &
snrmax3(:), pkmjd(:), pkmjde(:), x1(:), x1e(:), paramc(:), paramce(:), x0(:), x0e(:), cov_x1_c(:), &
cov_x1_x0(:), cov_c_x0(:), ndof(:), fitchi2(:), fitprob(:), ra(:), decl(:), tgapmax(:), mu(:), &
mue(:), mures(:), mupull(:), sbar(:), cbar(:), errcode(:),  trestmin(:), trestmax(:), &
mwebv(:), ys(:)
! INTEGER, ALLOCATABLE :: 
! CHARACTER(len=7) :: name
! CHARACTER(len=10), ALLOCATABLE :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21
CHARACTER(len=10), DIMENSION(21) :: table
CHARACTER(len=7), ALLOCATABLE :: name(:), field(:), cidint(:), idsurvey(:), ty(:), trash(:)
CHARACTER(len=3), ALLOCATABLE :: nome(:)
! CHARACTER(len=120) :: format="(*, 2F8.6, F1.0, F9.6, F8.6, F9.6, F8.6, F9.6, &
! !!!F8.6, F9.6, F8.6, F12.6, F8.6, 3F9.6, F1.0, 2F10.6, F9.6)"!!!!



End Module definitions


