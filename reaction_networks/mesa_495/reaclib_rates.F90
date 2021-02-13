module reaclib_rates

  use amrex_fort_module, only: rt => amrex_real
  use screening_module, only: add_screening_factor, &
                              screening_init, screening_finalize, &
                              plasma_state, fill_plasma_state
  use network

  implicit none

  logical, parameter :: screen_reaclib = .true.

  ! Temperature coefficient arrays (numbers correspond to reaction numbers in net_info)
  real(rt), allocatable :: ctemp_rate(:,:)

  ! Index into ctemp_rate, dimension 2, where each rate's coefficients start
  integer, allocatable :: rate_start_idx(:)

  ! Reaction multiplicities-1 (how many rates contribute - 1)
  integer, allocatable :: rate_extra_mult(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: ctemp_rate, rate_start_idx, rate_extra_mult
#endif

  !$acc declare create(ctemp_rate, rate_start_idx, rate_extra_mult)
  !$acc declare copyin(screen_reaclib)

contains

  subroutine init_reaclib()

    implicit none

    integer :: unit, ireaclib, icoeff

    allocate( ctemp_rate(7, number_reaclib_sets) )
    allocate( rate_start_idx(nrat_reaclib) )
    allocate( rate_extra_mult(nrat_reaclib) )

    open(newunit=unit, file='reaclib_rate_metadata.dat')

    do ireaclib = 1, number_reaclib_sets
       do icoeff = 1, 7
          read(unit, *) ctemp_rate(icoeff, ireaclib)
       enddo
    enddo

    do ireaclib = 1, nrat_reaclib
       read(unit, *) rate_start_idx(ireaclib)
    enddo

    do ireaclib = 1, nrat_reaclib
       read(unit, *) rate_extra_mult(ireaclib)
    enddo

    close(unit)

    !$acc update device(ctemp_rate, rate_start_idx, rate_extra_mult)

  end subroutine init_reaclib

  subroutine term_reaclib()
    deallocate( ctemp_rate )
    deallocate( rate_start_idx )
    deallocate( rate_extra_mult )
  end subroutine term_reaclib


  subroutine net_screening_init()
    ! Adds screening factors and calls screening_init

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp), aion(jp))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jd), aion(jd))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jd), aion(jd))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jhe4), aion(jhe4))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jt), aion(jt))

    call add_screening_factor(zion(jt), aion(jt), &
      zion(jhe4), aion(jhe4))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jhe3), aion(jhe3))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jhe3), aion(jhe3))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jli6), aion(jli6))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jli6), aion(jli6))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jli7), aion(jli7))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbe7), aion(jbe7))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbe7), aion(jbe7))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbe9), aion(jbe9))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jb10), aion(jb10))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jb11), aion(jb11))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jc11), aion(jc11))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jc12), aion(jc12))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jc12), aion(jc12))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jc13), aion(jc13))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jc14), aion(jc14))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jc14), aion(jc14))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jn13), aion(jn13))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jn14), aion(jn14))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jn14), aion(jn14))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jn15), aion(jn15))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jn15), aion(jn15))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jo14), aion(jo14))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jo15), aion(jo15))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jo16), aion(jo16))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jo16), aion(jo16))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jo17), aion(jo17))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jo17), aion(jo17))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jo18), aion(jo18))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jo18), aion(jo18))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jo19), aion(jo19))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jo19), aion(jo19))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jf17), aion(jf17))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jf17), aion(jf17))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jf18), aion(jf18))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jf18), aion(jf18))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jf19), aion(jf19))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jf19), aion(jf19))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jf20), aion(jf20))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jf20), aion(jf20))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jf21), aion(jf21))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jf21), aion(jf21))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne17), aion(jne17))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne18), aion(jne18))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne18), aion(jne18))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne19), aion(jne19))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne19), aion(jne19))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne20), aion(jne20))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne20), aion(jne20))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne21), aion(jne21))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne21), aion(jne21))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne22), aion(jne22))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne22), aion(jne22))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne23), aion(jne23))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne23), aion(jne23))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jne24), aion(jne24))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jne24), aion(jne24))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna19), aion(jna19))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna19), aion(jna19))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna20), aion(jna20))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna20), aion(jna20))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna21), aion(jna21))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna21), aion(jna21))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna22), aion(jna22))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna22), aion(jna22))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna23), aion(jna23))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna23), aion(jna23))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna24), aion(jna24))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna24), aion(jna24))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna25), aion(jna25))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna25), aion(jna25))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna26), aion(jna26))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna26), aion(jna26))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jna27), aion(jna27))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jna27), aion(jna27))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg20), aion(jmg20))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg21), aion(jmg21))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg21), aion(jmg21))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg22), aion(jmg22))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg22), aion(jmg22))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg23), aion(jmg23))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg23), aion(jmg23))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg24), aion(jmg24))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg24), aion(jmg24))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg25), aion(jmg25))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg25), aion(jmg25))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg26), aion(jmg26))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg26), aion(jmg26))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg27), aion(jmg27))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg27), aion(jmg27))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg28), aion(jmg28))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg28), aion(jmg28))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmg29), aion(jmg29))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmg29), aion(jmg29))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal22), aion(jal22))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal23), aion(jal23))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal23), aion(jal23))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal24), aion(jal24))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal24), aion(jal24))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal25), aion(jal25))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal25), aion(jal25))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal26), aion(jal26))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal26), aion(jal26))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal27), aion(jal27))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal27), aion(jal27))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal28), aion(jal28))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal28), aion(jal28))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal29), aion(jal29))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal29), aion(jal29))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal30), aion(jal30))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal30), aion(jal30))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jal31), aion(jal31))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal31), aion(jal31))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi24), aion(jsi24))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi25), aion(jsi25))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi26), aion(jsi26))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi26), aion(jsi26))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi27), aion(jsi27))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi27), aion(jsi27))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi28), aion(jsi28))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi28), aion(jsi28))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi29), aion(jsi29))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi29), aion(jsi29))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi30), aion(jsi30))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi30), aion(jsi30))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi31), aion(jsi31))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi31), aion(jsi31))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi32), aion(jsi32))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi32), aion(jsi32))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi33), aion(jsi33))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi33), aion(jsi33))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi34), aion(jsi34))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsi34), aion(jsi34))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp27), aion(jp27))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp27), aion(jp27))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp28), aion(jp28))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp28), aion(jp28))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp29), aion(jp29))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp29), aion(jp29))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp30), aion(jp30))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp30), aion(jp30))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp31), aion(jp31))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp31), aion(jp31))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp32), aion(jp32))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp32), aion(jp32))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp33), aion(jp33))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp33), aion(jp33))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp34), aion(jp34))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp34), aion(jp34))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp35), aion(jp35))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp35), aion(jp35))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp36), aion(jp36))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp36), aion(jp36))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp37), aion(jp37))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp37), aion(jp37))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jp38), aion(jp38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jp38), aion(jp38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js28), aion(js28))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js29), aion(js29))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js30), aion(js30))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js30), aion(js30))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js31), aion(js31))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js31), aion(js31))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js32), aion(js32))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js32), aion(js32))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js33), aion(js33))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js33), aion(js33))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js34), aion(js34))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js34), aion(js34))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js35), aion(js35))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js35), aion(js35))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js36), aion(js36))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js36), aion(js36))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js37), aion(js37))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js37), aion(js37))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js38), aion(js38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js38), aion(js38))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js39), aion(js39))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js39), aion(js39))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js40), aion(js40))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js40), aion(js40))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js41), aion(js41))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js41), aion(js41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(js42), aion(js42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(js42), aion(js42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl31), aion(jcl31))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl31), aion(jcl31))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl32), aion(jcl32))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl32), aion(jcl32))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl33), aion(jcl33))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl33), aion(jcl33))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl34), aion(jcl34))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl34), aion(jcl34))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl35), aion(jcl35))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl35), aion(jcl35))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl36), aion(jcl36))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl36), aion(jcl36))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl37), aion(jcl37))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl37), aion(jcl37))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl38), aion(jcl38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl38), aion(jcl38))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl39), aion(jcl39))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl39), aion(jcl39))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl40), aion(jcl40))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl40), aion(jcl40))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl41), aion(jcl41))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl41), aion(jcl41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl42), aion(jcl42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl42), aion(jcl42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl43), aion(jcl43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl43), aion(jcl43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl44), aion(jcl44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl44), aion(jcl44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcl45), aion(jcl45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcl45), aion(jcl45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar32), aion(jar32))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar33), aion(jar33))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar34), aion(jar34))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar34), aion(jar34))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar35), aion(jar35))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar35), aion(jar35))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar36), aion(jar36))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar36), aion(jar36))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar37), aion(jar37))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar37), aion(jar37))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar38), aion(jar38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar38), aion(jar38))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar39), aion(jar39))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar39), aion(jar39))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar40), aion(jar40))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar40), aion(jar40))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar41), aion(jar41))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar41), aion(jar41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar42), aion(jar42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar42), aion(jar42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar43), aion(jar43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar43), aion(jar43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar44), aion(jar44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar44), aion(jar44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar45), aion(jar45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar45), aion(jar45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jar46), aion(jar46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk35), aion(jk35))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk36), aion(jk36))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk36), aion(jk36))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk37), aion(jk37))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk37), aion(jk37))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk38), aion(jk38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk38), aion(jk38))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk39), aion(jk39))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk39), aion(jk39))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk40), aion(jk40))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk40), aion(jk40))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk41), aion(jk41))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk41), aion(jk41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk42), aion(jk42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk42), aion(jk42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk43), aion(jk43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk43), aion(jk43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk44), aion(jk44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk44), aion(jk44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk45), aion(jk45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk45), aion(jk45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk46), aion(jk46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk46), aion(jk46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk47), aion(jk47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk47), aion(jk47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk48), aion(jk48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca37), aion(jca37))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca38), aion(jca38))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca39), aion(jca39))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca39), aion(jca39))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca40), aion(jca40))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca40), aion(jca40))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca41), aion(jca41))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca41), aion(jca41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca42), aion(jca42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca42), aion(jca42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca43), aion(jca43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca43), aion(jca43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca44), aion(jca44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca44), aion(jca44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca45), aion(jca45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca45), aion(jca45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca46), aion(jca46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca46), aion(jca46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca47), aion(jca47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca47), aion(jca47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca48), aion(jca48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca48), aion(jca48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca49), aion(jca49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca49), aion(jca49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc40), aion(jsc40))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc40), aion(jsc40))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc41), aion(jsc41))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc41), aion(jsc41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc42), aion(jsc42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc42), aion(jsc42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc43), aion(jsc43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc43), aion(jsc43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc44), aion(jsc44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc44), aion(jsc44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc45), aion(jsc45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc45), aion(jsc45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc46), aion(jsc46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc46), aion(jsc46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc47), aion(jsc47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc47), aion(jsc47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc48), aion(jsc48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc48), aion(jsc48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc49), aion(jsc49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc49), aion(jsc49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc50), aion(jsc50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc50), aion(jsc50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsc51), aion(jsc51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsc51), aion(jsc51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti41), aion(jti41))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti42), aion(jti42))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti42), aion(jti42))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti43), aion(jti43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti43), aion(jti43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti44), aion(jti44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti44), aion(jti44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti45), aion(jti45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti45), aion(jti45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti46), aion(jti46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti46), aion(jti46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti47), aion(jti47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti47), aion(jti47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti48), aion(jti48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti48), aion(jti48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti49), aion(jti49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti49), aion(jti49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti50), aion(jti50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti50), aion(jti50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti51), aion(jti51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti51), aion(jti51))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti52), aion(jti52))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti52), aion(jti52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jti53), aion(jti53))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jti53), aion(jti53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv43), aion(jv43))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv43), aion(jv43))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv44), aion(jv44))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv44), aion(jv44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv45), aion(jv45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv45), aion(jv45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv46), aion(jv46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv46), aion(jv46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv47), aion(jv47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv47), aion(jv47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv48), aion(jv48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv48), aion(jv48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv49), aion(jv49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv49), aion(jv49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv50), aion(jv50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv50), aion(jv50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv51), aion(jv51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv51), aion(jv51))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv52), aion(jv52))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv52), aion(jv52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv53), aion(jv53))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv53), aion(jv53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv54), aion(jv54))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv54), aion(jv54))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jv55), aion(jv55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jv55), aion(jv55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr44), aion(jcr44))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr45), aion(jcr45))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr45), aion(jcr45))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr46), aion(jcr46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr46), aion(jcr46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr47), aion(jcr47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr47), aion(jcr47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr48), aion(jcr48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr48), aion(jcr48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr49), aion(jcr49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr49), aion(jcr49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr50), aion(jcr50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr50), aion(jcr50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr51), aion(jcr51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr51), aion(jcr51))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr52), aion(jcr52))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr52), aion(jcr52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr53), aion(jcr53))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr53), aion(jcr53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr54), aion(jcr54))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr54), aion(jcr54))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr55), aion(jcr55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr55), aion(jcr55))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr56), aion(jcr56))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr56), aion(jcr56))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr57), aion(jcr57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr57), aion(jcr57))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcr58), aion(jcr58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcr58), aion(jcr58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn46), aion(jmn46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn46), aion(jmn46))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn47), aion(jmn47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn47), aion(jmn47))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn48), aion(jmn48))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn48), aion(jmn48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn49), aion(jmn49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn49), aion(jmn49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn50), aion(jmn50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn50), aion(jmn50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn51), aion(jmn51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn51), aion(jmn51))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn52), aion(jmn52))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn52), aion(jmn52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn53), aion(jmn53))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn53), aion(jmn53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn54), aion(jmn54))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn54), aion(jmn54))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn55), aion(jmn55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn55), aion(jmn55))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn56), aion(jmn56))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn56), aion(jmn56))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn57), aion(jmn57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn57), aion(jmn57))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn58), aion(jmn58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn58), aion(jmn58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn59), aion(jmn59))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn59), aion(jmn59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn60), aion(jmn60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn60), aion(jmn60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmn61), aion(jmn61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmn61), aion(jmn61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe47), aion(jfe47))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe48), aion(jfe48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe49), aion(jfe49))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe49), aion(jfe49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe50), aion(jfe50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe50), aion(jfe50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe51), aion(jfe51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe51), aion(jfe51))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe52), aion(jfe52))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe52), aion(jfe52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe53), aion(jfe53))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe53), aion(jfe53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe54), aion(jfe54))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe54), aion(jfe54))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe55), aion(jfe55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe55), aion(jfe55))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe56), aion(jfe56))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe56), aion(jfe56))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe57), aion(jfe57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe57), aion(jfe57))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe58), aion(jfe58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe58), aion(jfe58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe59), aion(jfe59))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe59), aion(jfe59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe60), aion(jfe60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe60), aion(jfe60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe61), aion(jfe61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe61), aion(jfe61))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe62), aion(jfe62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe62), aion(jfe62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe63), aion(jfe63))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe63), aion(jfe63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe64), aion(jfe64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe64), aion(jfe64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe65), aion(jfe65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jfe66), aion(jfe66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco50), aion(jco50))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco51), aion(jco51))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco51), aion(jco51))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco52), aion(jco52))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco52), aion(jco52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco53), aion(jco53))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco53), aion(jco53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco54), aion(jco54))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco54), aion(jco54))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco55), aion(jco55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco55), aion(jco55))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco56), aion(jco56))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco56), aion(jco56))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco57), aion(jco57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco57), aion(jco57))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco58), aion(jco58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco58), aion(jco58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco59), aion(jco59))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco59), aion(jco59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco60), aion(jco60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco60), aion(jco60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco61), aion(jco61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco61), aion(jco61))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco62), aion(jco62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco62), aion(jco62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco63), aion(jco63))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco63), aion(jco63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco64), aion(jco64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco64), aion(jco64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco65), aion(jco65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco65), aion(jco65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco66), aion(jco66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jco67), aion(jco67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni53), aion(jni53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni54), aion(jni54))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni54), aion(jni54))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni55), aion(jni55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni55), aion(jni55))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni56), aion(jni56))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni56), aion(jni56))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni57), aion(jni57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni57), aion(jni57))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni58), aion(jni58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni58), aion(jni58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni59), aion(jni59))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni59), aion(jni59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni60), aion(jni60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni60), aion(jni60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni61), aion(jni61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni61), aion(jni61))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni62), aion(jni62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni62), aion(jni62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni63), aion(jni63))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni63), aion(jni63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni64), aion(jni64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni64), aion(jni64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni65), aion(jni65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni65), aion(jni65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni66), aion(jni66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni66), aion(jni66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni67), aion(jni67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni67), aion(jni67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni68), aion(jni68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni68), aion(jni68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu55), aion(jcu55))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu56), aion(jcu56))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu56), aion(jcu56))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu57), aion(jcu57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu57), aion(jcu57))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu58), aion(jcu58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu58), aion(jcu58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu59), aion(jcu59))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu59), aion(jcu59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu60), aion(jcu60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu60), aion(jcu60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu61), aion(jcu61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu61), aion(jcu61))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu62), aion(jcu62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu62), aion(jcu62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu63), aion(jcu63))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu63), aion(jcu63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu64), aion(jcu64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu64), aion(jcu64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu65), aion(jcu65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu65), aion(jcu65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu66), aion(jcu66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu66), aion(jcu66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu67), aion(jcu67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu67), aion(jcu67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu68), aion(jcu68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu68), aion(jcu68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu69), aion(jcu69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jcu69), aion(jcu69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn58), aion(jzn58))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn58), aion(jzn58))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn59), aion(jzn59))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn59), aion(jzn59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn60), aion(jzn60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn60), aion(jzn60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn61), aion(jzn61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn61), aion(jzn61))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn62), aion(jzn62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn62), aion(jzn62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn63), aion(jzn63))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn63), aion(jzn63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn64), aion(jzn64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn64), aion(jzn64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn65), aion(jzn65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn65), aion(jzn65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn66), aion(jzn66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn66), aion(jzn66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn67), aion(jzn67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn67), aion(jzn67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn68), aion(jzn68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn68), aion(jzn68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn69), aion(jzn69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn69), aion(jzn69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn70), aion(jzn70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn70), aion(jzn70))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn71), aion(jzn71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn71), aion(jzn71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzn72), aion(jzn72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn72), aion(jzn72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga61), aion(jga61))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga61), aion(jga61))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga62), aion(jga62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga62), aion(jga62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga63), aion(jga63))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga63), aion(jga63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga64), aion(jga64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga64), aion(jga64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga65), aion(jga65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga65), aion(jga65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga66), aion(jga66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga66), aion(jga66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga67), aion(jga67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga67), aion(jga67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga68), aion(jga68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga68), aion(jga68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga69), aion(jga69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga69), aion(jga69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga70), aion(jga70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga70), aion(jga70))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga71), aion(jga71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga71), aion(jga71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga72), aion(jga72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga72), aion(jga72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga73), aion(jga73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga73), aion(jga73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga74), aion(jga74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga74), aion(jga74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga75), aion(jga75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga75), aion(jga75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge63), aion(jge63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge64), aion(jge64))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge64), aion(jge64))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge65), aion(jge65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge65), aion(jge65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge66), aion(jge66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge66), aion(jge66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge67), aion(jge67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge67), aion(jge67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge68), aion(jge68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge68), aion(jge68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge69), aion(jge69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge69), aion(jge69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge70), aion(jge70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge70), aion(jge70))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge71), aion(jge71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge71), aion(jge71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge72), aion(jge72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge72), aion(jge72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge73), aion(jge73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge73), aion(jge73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge74), aion(jge74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge74), aion(jge74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge75), aion(jge75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge75), aion(jge75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge76), aion(jge76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge76), aion(jge76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge77), aion(jge77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge77), aion(jge77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge78), aion(jge78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge78), aion(jge78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas65), aion(jas65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas66), aion(jas66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas66), aion(jas66))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas67), aion(jas67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas67), aion(jas67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas68), aion(jas68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas68), aion(jas68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas69), aion(jas69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas69), aion(jas69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas70), aion(jas70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas70), aion(jas70))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas71), aion(jas71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas71), aion(jas71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas72), aion(jas72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas72), aion(jas72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas73), aion(jas73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas73), aion(jas73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas74), aion(jas74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas74), aion(jas74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas75), aion(jas75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas75), aion(jas75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas76), aion(jas76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas76), aion(jas76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas77), aion(jas77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas77), aion(jas77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas78), aion(jas78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas78), aion(jas78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas79), aion(jas79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jas79), aion(jas79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse67), aion(jse67))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse67), aion(jse67))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse68), aion(jse68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse68), aion(jse68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse69), aion(jse69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse69), aion(jse69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse70), aion(jse70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse70), aion(jse70))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse71), aion(jse71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse71), aion(jse71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse72), aion(jse72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse72), aion(jse72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse73), aion(jse73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse73), aion(jse73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse74), aion(jse74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse74), aion(jse74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse75), aion(jse75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse75), aion(jse75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse76), aion(jse76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse76), aion(jse76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse77), aion(jse77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse77), aion(jse77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse78), aion(jse78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse78), aion(jse78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse79), aion(jse79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse79), aion(jse79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse80), aion(jse80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse80), aion(jse80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse81), aion(jse81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse81), aion(jse81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse82), aion(jse82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse82), aion(jse82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jse83), aion(jse83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr68), aion(jbr68))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr69), aion(jbr69))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr69), aion(jbr69))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr70), aion(jbr70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr70), aion(jbr70))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr71), aion(jbr71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr71), aion(jbr71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr72), aion(jbr72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr72), aion(jbr72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr73), aion(jbr73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr73), aion(jbr73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr74), aion(jbr74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr74), aion(jbr74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr75), aion(jbr75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr75), aion(jbr75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr76), aion(jbr76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr76), aion(jbr76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr77), aion(jbr77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr77), aion(jbr77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr78), aion(jbr78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr78), aion(jbr78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr79), aion(jbr79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr79), aion(jbr79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr80), aion(jbr80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr80), aion(jbr80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr81), aion(jbr81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr81), aion(jbr81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr82), aion(jbr82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jbr83), aion(jbr83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr70), aion(jkr70))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr71), aion(jkr71))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr72), aion(jkr72))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr72), aion(jkr72))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr73), aion(jkr73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr73), aion(jkr73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr74), aion(jkr74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr74), aion(jkr74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr75), aion(jkr75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr75), aion(jkr75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr76), aion(jkr76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr76), aion(jkr76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr77), aion(jkr77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr77), aion(jkr77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr78), aion(jkr78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr78), aion(jkr78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr79), aion(jkr79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr79), aion(jkr79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr80), aion(jkr80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr80), aion(jkr80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr81), aion(jkr81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr82), aion(jkr82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr83), aion(jkr83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr84), aion(jkr84))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb73), aion(jrb73))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb73), aion(jrb73))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb74), aion(jrb74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb74), aion(jrb74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb75), aion(jrb75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb75), aion(jrb75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb76), aion(jrb76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb76), aion(jrb76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb77), aion(jrb77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb77), aion(jrb77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb78), aion(jrb78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb78), aion(jrb78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb79), aion(jrb79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb79), aion(jrb79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb80), aion(jrb80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb80), aion(jrb80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb81), aion(jrb81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb81), aion(jrb81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb82), aion(jrb82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb82), aion(jrb82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb83), aion(jrb83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb83), aion(jrb83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr74), aion(jsr74))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr74), aion(jsr74))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr75), aion(jsr75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr75), aion(jsr75))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr76), aion(jsr76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr76), aion(jsr76))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr77), aion(jsr77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr77), aion(jsr77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr78), aion(jsr78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr78), aion(jsr78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr79), aion(jsr79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr79), aion(jsr79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr80), aion(jsr80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr80), aion(jsr80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr81), aion(jsr81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr81), aion(jsr81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr82), aion(jsr82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr82), aion(jsr82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr83), aion(jsr83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr83), aion(jsr83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsr84), aion(jsr84))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jsr84), aion(jsr84))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy77), aion(jy77))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy78), aion(jy78))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy78), aion(jy78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy79), aion(jy79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy79), aion(jy79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy80), aion(jy80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy80), aion(jy80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy81), aion(jy81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy81), aion(jy81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy82), aion(jy82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy82), aion(jy82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy83), aion(jy83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy83), aion(jy83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy84), aion(jy84))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy84), aion(jy84))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy85), aion(jy85))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy85), aion(jy85))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy86), aion(jy86))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy86), aion(jy86))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jy87), aion(jy87))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr79), aion(jzr79))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr80), aion(jzr80))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr81), aion(jzr81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr81), aion(jzr81))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr82), aion(jzr82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr82), aion(jzr82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr83), aion(jzr83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr83), aion(jzr83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr84), aion(jzr84))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr84), aion(jzr84))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr85), aion(jzr85))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr85), aion(jzr85))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr86), aion(jzr86))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr86), aion(jzr86))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr87), aion(jzr87))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr88), aion(jzr88))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr89), aion(jzr89))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb82), aion(jnb82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb83), aion(jnb83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb84), aion(jnb84))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb85), aion(jnb85))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb85), aion(jnb85))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb86), aion(jnb86))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb86), aion(jnb86))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb87), aion(jnb87))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb87), aion(jnb87))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb88), aion(jnb88))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb89), aion(jnb89))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmo88), aion(jmo88))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmo89), aion(jmo89))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmo90), aion(jmo90))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jt), aion(jt))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jhe3), aion(jhe3))

    call add_screening_factor(zion(jt), aion(jt), &
      zion(jhe3), aion(jhe3))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jhe4), aion(jhe4))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jhe4), aion(jhe4))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jli6), aion(jli6))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jli7), aion(jli7))

    call add_screening_factor(zion(jt), aion(jt), &
      zion(jli7), aion(jli7))

    call add_screening_factor(zion(jt), aion(jt), &
      zion(jbe9), aion(jbe9))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbe9), aion(jbe9))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jb8), aion(jb8))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jb10), aion(jb10))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jb11), aion(jb11))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jc11), aion(jc11))

    call add_screening_factor(zion(jc12), aion(jc12), &
      zion(jc12), aion(jc12))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jc13), aion(jc13))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jc13), aion(jc13))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jc14), aion(jc14))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jn12), aion(jn12))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jn13), aion(jn13))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jo15), aion(jo15))

    call add_screening_factor(zion(jc12), aion(jc12), &
      zion(jo16), aion(jo16))

    call add_screening_factor(zion(jo16), aion(jo16), &
      zion(jo16), aion(jo16))

    call add_screening_factor(zion(jc12), aion(jc12), &
      zion(jne20), aion(jne20))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jal22), aion(jal22))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jsi25), aion(jsi25))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jar46), aion(jar46))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk35), aion(jk35))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jk48), aion(jk48))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jk49), aion(jk49))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jca38), aion(jca38))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jfe65), aion(jfe65))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco50), aion(jco50))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jco66), aion(jco66))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jni52), aion(jni52))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jni53), aion(jni53))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jcu55), aion(jcu55))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzn57), aion(jzn57))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga59), aion(jga59))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jga60), aion(jga60))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jga60), aion(jga60))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge62), aion(jge62))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jge62), aion(jge62))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jge63), aion(jge63))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jas65), aion(jas65))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jse83), aion(jse83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr68), aion(jbr68))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr82), aion(jbr82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jbr83), aion(jbr83))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr71), aion(jkr71))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr81), aion(jkr81))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jkr82), aion(jkr82))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr85), aion(jkr85))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jkr86), aion(jkr86))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb84), aion(jrb84))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jrb84), aion(jrb84))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jrb85), aion(jrb85))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy75), aion(jy75))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy76), aion(jy76))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy77), aion(jy77))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jy87), aion(jy87))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr78), aion(jzr78))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr79), aion(jzr79))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr80), aion(jzr80))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jzr87), aion(jzr87))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jzr90), aion(jzr90))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb82), aion(jnb82))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb83), aion(jnb83))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb84), aion(jnb84))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jnb88), aion(jnb88))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jnb90), aion(jnb90))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmo85), aion(jmo85))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmo86), aion(jmo86))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmo86), aion(jmo86))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jmo87), aion(jmo87))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmo87), aion(jmo87))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jmo88), aion(jmo88))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jtc89), aion(jtc89))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jtc90), aion(jtc90))

    call add_screening_factor(zion(jp), aion(jp), &
      zion(jtc91), aion(jtc91))

    call add_screening_factor(zion(jt), aion(jt), &
      zion(jt), aion(jt))

    call add_screening_factor(zion(jhe3), aion(jhe3), &
      zion(jhe3), aion(jhe3))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jli7), aion(jli7))

    call add_screening_factor(zion(jd), aion(jd), &
      zion(jbe7), aion(jbe7))

    call add_screening_factor(zion(jhe4), aion(jhe4), &
      zion(jca36), aion(jca36))

    call add_screening_factor(zion(jhe3), aion(jhe3), &
      zion(jli7), aion(jli7))

    call add_screening_factor(zion(jt), aion(jt), &
      zion(jbe7), aion(jbe7))

    call add_screening_factor(zion(jhe3), aion(jhe3), &
      zion(jbe7), aion(jbe7))


    call screening_init()
  end subroutine net_screening_init


  subroutine net_screening_finalize()
    ! Call screening_finalize

    call screening_finalize()

  end subroutine net_screening_finalize


  subroutine reaclib_evaluate(pstate, temp, iwhich, rate, drate_dt)

    !$acc routine seq

    implicit none

    type(plasma_state), intent(in) :: pstate
    real(rt), intent(in) :: temp
    integer, intent(in) :: iwhich

    real(rt), intent(out) :: rate     ! Reaction rate
    real(rt), intent(out) :: drate_dt ! Reaction rate temperature derivative

    real(rt) :: ri, T9, T9_exp, lnirate, irate, dirate_dt, dlnirate_dt
    integer :: i, j, m, istart

    !$gpu

    ri = 0.0e0_rt
    rate = 0.0e0_rt
    drate_dt = 0.0e0_rt
    irate = 0.0e0_rt
    dirate_dt = 0.0e0_rt
    T9 = temp/1.0e9_rt
    T9_exp = 0.0e0_rt

    ! Get the number of additional Reaclib sets for this rate
    ! Total number of Reaclib sets for this rate is m + 1
    m = rate_extra_mult(iwhich)

    istart = rate_start_idx(iwhich)

    do i = 0, m
       lnirate = ctemp_rate(1, istart+i) + ctemp_rate(7, istart+i) * LOG(T9)
       dlnirate_dt = ctemp_rate(7, istart+i)/T9
       do j = 2, 6
          T9_exp = (2.0e0_rt*dble(j-1)-5.0e0_rt)/3.0e0_rt
          lnirate = lnirate + ctemp_rate(j, istart+i) * T9**T9_exp
          dlnirate_dt = dlnirate_dt + &
               T9_exp * ctemp_rate(j, istart+i) * T9**(T9_exp-1.0e0_rt)
       end do
       ! If the rate will be in the approx. interval [0.0, 1.0E-100], replace by 0.0
       ! This avoids issues with passing very large negative values to EXP
       ! and getting results between 0.0 and 1.0E-308, the limit for IEEE 754.
       ! And avoids SIGFPE in CVODE due to tiny rates.
       lnirate = max(lnirate, -230.0e0_rt)
       irate = EXP(lnirate)
       rate = rate + irate
       dirate_dt = irate * dlnirate_dt/1.0e9_rt
       drate_dt = drate_dt + dirate_dt
    end do

  end subroutine reaclib_evaluate

end module reaclib_rates
