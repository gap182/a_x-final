!voy a diseñar una subrutina para leer el archivo de datos de scolnic (2015), luego de ver que sea funcional lo cambio a una subrutina y la implemento en el código principal

program read_scolnic


USE definitions

OPEN (110, file='scolnicdata.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')

arrays_dimension=0




Do

    READ(110,*,iostat=stat) 
    
        If (stat .ne. 0) then
            exit
        Else
            arrays_dimension=arrays_dimension+1
        End If
    
End Do

arrays_dimension=arrays_dimension-9

CLOSE(110)

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

OPEN (110, file='scolnicdata.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')

READ(110,*)
READ(110,*)
READ(110,*)
READ(110,*)
READ(110,*)
READ(110,*)
READ(110,*)
READ(110,*)
READ(110,*)

! READ(110,*) nome(1), name(1), cidint(1),idsurvey(1), ty(1), field(1),zcmb(1), dz(1), &
! z2(1), z2e(1), host_logmass(1), host_logmass_e(1), snrmax1(1), snrmax2(1), snrmax3(1), &
! pkmjd(1), pkmjde(1), x1(1), x1e(1), paramc(1), paramce(1), mb(1), dmb(1), x0(1), x0e(1), &
! cov_x1_c(1), cov_x1_x0(1), cov_c_x0(1), ndof(1), fitchi2(1), fitprob(1), ra(1), decl(1), &
! tgapmax(1), mu(1), mue(1),  mures(1),  mupull(1), sbar(1), cbar(1), errcode(1)

! WRITE(*,*) nome(1), name(1), cidint(1),idsurvey(1), ty(1), field(1),zcmb(1), dz(1), &
! z2(1), z2e(1), host_logmass(1), host_logmass_e(1), snrmax1(1), snrmax2(1), snrmax3(1), &
! pkmjd(1), pkmjde(1), x1(1), x1e(1), paramc(1), paramce(1), mb(1), dmb(1), x0(1), x0e(1), &
! cov_x1_c(1), cov_x1_x0(1), cov_c_x0(1), ndof(1), fitchi2(1), fitprob(1), ra(1), decl(1), &
! tgapmax(1), mu(1), mue(1),  mures(1),  mupull(1), sbar(1), cbar(1), errcode(1)

DO i=1, arrays_dimension

READ(110,*) nome(p), name(p), cidint(p), idsurvey(p), ty(p), field(p), zcmb(p), dz(p), &
    z2(p), z2e(p), host_logmass(p), host_logmass_e(p), snrmax1(p), snrmax2(p), snrmax3(p), &
    pkmjd(p), pkmjde(p), x1(p), x1e(p), paramc(p), paramce(p), mb(p), dmb(p), x0(p), x0e(p), &
    cov_x1_c(p), cov_x1_x0(p), cov_c_x0(p), ndof(p), fitchi2(p), fitprob(p), ra(p), decl(p), &
    tgapmax(p), mu(p), mue(p),  mures(p),  mupull(p), sbar(p), cbar(p), errcode(p)
END DO



end program read_scolnic

