module actual_rhs_module

  use amrex_fort_module, only: rt => amrex_real
  use amrex_constants_module
  use physical_constants, only: N_AVO
  use network
  use table_rates
  use burn_type_module

  implicit none

  ! Indices into rate groups in the rate_eval_t type
  integer, parameter :: i_rate        = 1
  integer, parameter :: i_drate_dt    = 2
  integer, parameter :: i_scor        = 3
  integer, parameter :: i_dscor_dt    = 4

  type :: rate_eval_t
     real(rt) :: unscreened_rates(num_rate_groups, nrates)
     real(rt) :: screened_rates(nrates)
     real(rt) :: add_energy_rate(nrat_tabular)
  end type rate_eval_t
  
contains

  subroutine actual_rhs_init()
    ! STUB FOR MAESTRO'S TEST_REACT. ALL THE INIT IS DONE BY BURNER_INIT
    return
  end subroutine actual_rhs_init


  subroutine update_unevolved_species(state)
    ! STUB FOR INTEGRATOR
    type(burn_t)     :: state

    !$gpu
    
    return
  end subroutine update_unevolved_species


  subroutine zero_rate_eval(rate_eval)

    implicit none

    type(rate_eval_t), intent(inout) :: rate_eval

    !$gpu

    rate_eval % unscreened_rates(i_rate, :) = ZERO
    rate_eval % unscreened_rates(i_drate_dt, :) = ZERO
    rate_eval % unscreened_rates(i_scor, :) = ONE
    rate_eval % unscreened_rates(i_dscor_dt, :) = ZERO
    rate_eval % screened_rates = ZERO
    rate_eval % add_energy_rate = ZERO

  end subroutine zero_rate_eval


  subroutine evaluate_rates(state, rate_eval)
    !$acc routine seq

    use reaclib_rates, only: screen_reaclib, reaclib_evaluate
    use screening_module, only: screen5, plasma_state, fill_plasma_state

    implicit none
    
    type(burn_t)     :: state
    type(rate_eval_t), intent(out) :: rate_eval
    type(plasma_state) :: pstate
    real(rt) :: Y(nspec)
    integer :: i, j
    real(rt) :: rhoy
    real(rt) :: rate, drate_dt, edot_nu
    real(rt) :: scor, dscor_dt, dscor_dd

    !$gpu

    Y(:) = state % xn(:) * aion_inv(:)
    rhoy = state % rho * state % y_e

    ! Zero out the rates
    call zero_rate_eval(rate_eval)

    ! Calculate Reaclib rates
    call fill_plasma_state(pstate, state % T, state % rho, Y)
    do i = 1, nrat_reaclib
       call reaclib_evaluate(pstate, state % T, i, rate, drate_dt)
       rate_eval % unscreened_rates(i_rate, i) = rate
       rate_eval % unscreened_rates(i_drate_dt, i) = drate_dt
    end do

    ! Evaluate screening factors
    if (screen_reaclib) then

      call screen5(pstate, 1, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,82) = scor
      rate_eval % unscreened_rates(i_dscor_dt,82) = dscor_dt
      rate_eval % unscreened_rates(i_scor,83) = scor
      rate_eval % unscreened_rates(i_dscor_dt,83) = dscor_dt
      rate_eval % unscreened_rates(i_scor,679) = scor
      rate_eval % unscreened_rates(i_dscor_dt,679) = dscor_dt


      call screen5(pstate, 2, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,85) = scor
      rate_eval % unscreened_rates(i_dscor_dt,85) = dscor_dt


      call screen5(pstate, 3, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,86) = scor
      rate_eval % unscreened_rates(i_dscor_dt,86) = dscor_dt
      rate_eval % unscreened_rates(i_scor,390) = scor
      rate_eval % unscreened_rates(i_dscor_dt,390) = dscor_dt
      rate_eval % unscreened_rates(i_scor,391) = scor
      rate_eval % unscreened_rates(i_dscor_dt,391) = dscor_dt


      call screen5(pstate, 4, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,87) = scor
      rate_eval % unscreened_rates(i_dscor_dt,87) = dscor_dt


      call screen5(pstate, 5, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,89) = scor
      rate_eval % unscreened_rates(i_dscor_dt,89) = dscor_dt


      call screen5(pstate, 6, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,91) = scor
      rate_eval % unscreened_rates(i_dscor_dt,91) = dscor_dt


      call screen5(pstate, 7, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,92) = scor
      rate_eval % unscreened_rates(i_dscor_dt,92) = dscor_dt


      call screen5(pstate, 8, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,94) = scor
      rate_eval % unscreened_rates(i_dscor_dt,94) = dscor_dt
      rate_eval % unscreened_rates(i_scor,398) = scor
      rate_eval % unscreened_rates(i_dscor_dt,398) = dscor_dt


      call screen5(pstate, 9, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,95) = scor
      rate_eval % unscreened_rates(i_dscor_dt,95) = dscor_dt


      call screen5(pstate, 10, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,96) = scor
      rate_eval % unscreened_rates(i_dscor_dt,96) = dscor_dt


      call screen5(pstate, 11, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,99) = scor
      rate_eval % unscreened_rates(i_dscor_dt,99) = dscor_dt


      call screen5(pstate, 12, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,100) = scor
      rate_eval % unscreened_rates(i_dscor_dt,100) = dscor_dt
      rate_eval % unscreened_rates(i_scor,404) = scor
      rate_eval % unscreened_rates(i_dscor_dt,404) = dscor_dt


      call screen5(pstate, 13, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,101) = scor
      rate_eval % unscreened_rates(i_dscor_dt,101) = dscor_dt


      call screen5(pstate, 14, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,103) = scor
      rate_eval % unscreened_rates(i_dscor_dt,103) = dscor_dt


      call screen5(pstate, 15, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,105) = scor
      rate_eval % unscreened_rates(i_dscor_dt,105) = dscor_dt
      rate_eval % unscreened_rates(i_scor,411) = scor
      rate_eval % unscreened_rates(i_dscor_dt,411) = dscor_dt


      call screen5(pstate, 16, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,106) = scor
      rate_eval % unscreened_rates(i_dscor_dt,106) = dscor_dt
      rate_eval % unscreened_rates(i_scor,412) = scor
      rate_eval % unscreened_rates(i_dscor_dt,412) = dscor_dt


      call screen5(pstate, 17, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,108) = scor
      rate_eval % unscreened_rates(i_dscor_dt,108) = dscor_dt
      rate_eval % unscreened_rates(i_scor,413) = scor
      rate_eval % unscreened_rates(i_dscor_dt,413) = dscor_dt


      call screen5(pstate, 18, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,109) = scor
      rate_eval % unscreened_rates(i_dscor_dt,109) = dscor_dt
      rate_eval % unscreened_rates(i_scor,414) = scor
      rate_eval % unscreened_rates(i_dscor_dt,414) = dscor_dt


      call screen5(pstate, 19, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,110) = scor
      rate_eval % unscreened_rates(i_dscor_dt,110) = dscor_dt
      rate_eval % unscreened_rates(i_scor,415) = scor
      rate_eval % unscreened_rates(i_dscor_dt,415) = dscor_dt


      call screen5(pstate, 20, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,111) = scor
      rate_eval % unscreened_rates(i_dscor_dt,111) = dscor_dt
      rate_eval % unscreened_rates(i_scor,416) = scor
      rate_eval % unscreened_rates(i_dscor_dt,416) = dscor_dt


      call screen5(pstate, 21, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,113) = scor
      rate_eval % unscreened_rates(i_dscor_dt,113) = dscor_dt


      call screen5(pstate, 22, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,114) = scor
      rate_eval % unscreened_rates(i_dscor_dt,114) = dscor_dt
      rate_eval % unscreened_rates(i_scor,419) = scor
      rate_eval % unscreened_rates(i_dscor_dt,419) = dscor_dt


      call screen5(pstate, 23, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,116) = scor
      rate_eval % unscreened_rates(i_dscor_dt,116) = dscor_dt
      rate_eval % unscreened_rates(i_scor,420) = scor
      rate_eval % unscreened_rates(i_dscor_dt,420) = dscor_dt
      rate_eval % unscreened_rates(i_scor,421) = scor
      rate_eval % unscreened_rates(i_dscor_dt,421) = dscor_dt


      call screen5(pstate, 24, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,117) = scor
      rate_eval % unscreened_rates(i_dscor_dt,117) = dscor_dt
      rate_eval % unscreened_rates(i_scor,422) = scor
      rate_eval % unscreened_rates(i_dscor_dt,422) = dscor_dt


      call screen5(pstate, 25, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,119) = scor
      rate_eval % unscreened_rates(i_dscor_dt,119) = dscor_dt
      rate_eval % unscreened_rates(i_scor,423) = scor
      rate_eval % unscreened_rates(i_dscor_dt,423) = dscor_dt
      rate_eval % unscreened_rates(i_scor,424) = scor
      rate_eval % unscreened_rates(i_dscor_dt,424) = dscor_dt


      call screen5(pstate, 26, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,120) = scor
      rate_eval % unscreened_rates(i_dscor_dt,120) = dscor_dt
      rate_eval % unscreened_rates(i_scor,425) = scor
      rate_eval % unscreened_rates(i_dscor_dt,425) = dscor_dt
      rate_eval % unscreened_rates(i_scor,426) = scor
      rate_eval % unscreened_rates(i_dscor_dt,426) = dscor_dt


      call screen5(pstate, 27, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,121) = scor
      rate_eval % unscreened_rates(i_dscor_dt,121) = dscor_dt
      rate_eval % unscreened_rates(i_scor,427) = scor
      rate_eval % unscreened_rates(i_dscor_dt,427) = dscor_dt
      rate_eval % unscreened_rates(i_scor,428) = scor
      rate_eval % unscreened_rates(i_dscor_dt,428) = dscor_dt


      call screen5(pstate, 28, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,123) = scor
      rate_eval % unscreened_rates(i_dscor_dt,123) = dscor_dt


      call screen5(pstate, 29, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,124) = scor
      rate_eval % unscreened_rates(i_dscor_dt,124) = dscor_dt
      rate_eval % unscreened_rates(i_scor,432) = scor
      rate_eval % unscreened_rates(i_dscor_dt,432) = dscor_dt


      call screen5(pstate, 30, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,126) = scor
      rate_eval % unscreened_rates(i_dscor_dt,126) = dscor_dt


      call screen5(pstate, 31, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,127) = scor
      rate_eval % unscreened_rates(i_dscor_dt,127) = dscor_dt


      call screen5(pstate, 32, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,129) = scor
      rate_eval % unscreened_rates(i_dscor_dt,129) = dscor_dt


      call screen5(pstate, 33, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,130) = scor
      rate_eval % unscreened_rates(i_dscor_dt,130) = dscor_dt
      rate_eval % unscreened_rates(i_scor,436) = scor
      rate_eval % unscreened_rates(i_dscor_dt,436) = dscor_dt


      call screen5(pstate, 34, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,132) = scor
      rate_eval % unscreened_rates(i_dscor_dt,132) = dscor_dt


      call screen5(pstate, 35, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,133) = scor
      rate_eval % unscreened_rates(i_dscor_dt,133) = dscor_dt
      rate_eval % unscreened_rates(i_scor,437) = scor
      rate_eval % unscreened_rates(i_dscor_dt,437) = dscor_dt


      call screen5(pstate, 36, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,135) = scor
      rate_eval % unscreened_rates(i_dscor_dt,135) = dscor_dt
      rate_eval % unscreened_rates(i_scor,438) = scor
      rate_eval % unscreened_rates(i_dscor_dt,438) = dscor_dt


      call screen5(pstate, 37, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,136) = scor
      rate_eval % unscreened_rates(i_dscor_dt,136) = dscor_dt
      rate_eval % unscreened_rates(i_scor,439) = scor
      rate_eval % unscreened_rates(i_dscor_dt,439) = dscor_dt


      call screen5(pstate, 38, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,139) = scor
      rate_eval % unscreened_rates(i_dscor_dt,139) = dscor_dt


      call screen5(pstate, 39, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,140) = scor
      rate_eval % unscreened_rates(i_dscor_dt,140) = dscor_dt


      call screen5(pstate, 40, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,142) = scor
      rate_eval % unscreened_rates(i_dscor_dt,142) = dscor_dt


      call screen5(pstate, 41, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,143) = scor
      rate_eval % unscreened_rates(i_dscor_dt,143) = dscor_dt
      rate_eval % unscreened_rates(i_scor,448) = scor
      rate_eval % unscreened_rates(i_dscor_dt,448) = dscor_dt


      call screen5(pstate, 42, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,145) = scor
      rate_eval % unscreened_rates(i_dscor_dt,145) = dscor_dt
      rate_eval % unscreened_rates(i_scor,449) = scor
      rate_eval % unscreened_rates(i_dscor_dt,449) = dscor_dt
      rate_eval % unscreened_rates(i_scor,450) = scor
      rate_eval % unscreened_rates(i_dscor_dt,450) = dscor_dt


      call screen5(pstate, 43, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,146) = scor
      rate_eval % unscreened_rates(i_dscor_dt,146) = dscor_dt
      rate_eval % unscreened_rates(i_scor,451) = scor
      rate_eval % unscreened_rates(i_dscor_dt,451) = dscor_dt


      call screen5(pstate, 44, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,147) = scor
      rate_eval % unscreened_rates(i_dscor_dt,147) = dscor_dt
      rate_eval % unscreened_rates(i_scor,452) = scor
      rate_eval % unscreened_rates(i_dscor_dt,452) = dscor_dt
      rate_eval % unscreened_rates(i_scor,453) = scor
      rate_eval % unscreened_rates(i_dscor_dt,453) = dscor_dt


      call screen5(pstate, 45, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,148) = scor
      rate_eval % unscreened_rates(i_dscor_dt,148) = dscor_dt
      rate_eval % unscreened_rates(i_scor,454) = scor
      rate_eval % unscreened_rates(i_dscor_dt,454) = dscor_dt
      rate_eval % unscreened_rates(i_scor,455) = scor
      rate_eval % unscreened_rates(i_dscor_dt,455) = dscor_dt


      call screen5(pstate, 46, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,151) = scor
      rate_eval % unscreened_rates(i_dscor_dt,151) = dscor_dt
      rate_eval % unscreened_rates(i_scor,461) = scor
      rate_eval % unscreened_rates(i_dscor_dt,461) = dscor_dt


      call screen5(pstate, 47, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,153) = scor
      rate_eval % unscreened_rates(i_dscor_dt,153) = dscor_dt
      rate_eval % unscreened_rates(i_scor,462) = scor
      rate_eval % unscreened_rates(i_dscor_dt,462) = dscor_dt


      call screen5(pstate, 48, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,154) = scor
      rate_eval % unscreened_rates(i_dscor_dt,154) = dscor_dt


      call screen5(pstate, 49, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,156) = scor
      rate_eval % unscreened_rates(i_dscor_dt,156) = dscor_dt


      call screen5(pstate, 50, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,157) = scor
      rate_eval % unscreened_rates(i_dscor_dt,157) = dscor_dt
      rate_eval % unscreened_rates(i_scor,463) = scor
      rate_eval % unscreened_rates(i_dscor_dt,463) = dscor_dt


      call screen5(pstate, 51, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,159) = scor
      rate_eval % unscreened_rates(i_dscor_dt,159) = dscor_dt


      call screen5(pstate, 52, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,160) = scor
      rate_eval % unscreened_rates(i_dscor_dt,160) = dscor_dt
      rate_eval % unscreened_rates(i_scor,464) = scor
      rate_eval % unscreened_rates(i_dscor_dt,464) = dscor_dt


      call screen5(pstate, 53, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,161) = scor
      rate_eval % unscreened_rates(i_dscor_dt,161) = dscor_dt
      rate_eval % unscreened_rates(i_scor,465) = scor
      rate_eval % unscreened_rates(i_dscor_dt,465) = dscor_dt


      call screen5(pstate, 54, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,162) = scor
      rate_eval % unscreened_rates(i_dscor_dt,162) = dscor_dt
      rate_eval % unscreened_rates(i_scor,466) = scor
      rate_eval % unscreened_rates(i_dscor_dt,466) = dscor_dt


      call screen5(pstate, 55, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,164) = scor
      rate_eval % unscreened_rates(i_dscor_dt,164) = dscor_dt
      rate_eval % unscreened_rates(i_scor,469) = scor
      rate_eval % unscreened_rates(i_dscor_dt,469) = dscor_dt


      call screen5(pstate, 56, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,166) = scor
      rate_eval % unscreened_rates(i_dscor_dt,166) = dscor_dt


      call screen5(pstate, 57, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,167) = scor
      rate_eval % unscreened_rates(i_dscor_dt,167) = dscor_dt
      rate_eval % unscreened_rates(i_scor,472) = scor
      rate_eval % unscreened_rates(i_dscor_dt,472) = dscor_dt


      call screen5(pstate, 58, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,169) = scor
      rate_eval % unscreened_rates(i_dscor_dt,169) = dscor_dt
      rate_eval % unscreened_rates(i_scor,473) = scor
      rate_eval % unscreened_rates(i_dscor_dt,473) = dscor_dt


      call screen5(pstate, 59, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,170) = scor
      rate_eval % unscreened_rates(i_dscor_dt,170) = dscor_dt
      rate_eval % unscreened_rates(i_scor,474) = scor
      rate_eval % unscreened_rates(i_dscor_dt,474) = dscor_dt
      rate_eval % unscreened_rates(i_scor,475) = scor
      rate_eval % unscreened_rates(i_dscor_dt,475) = dscor_dt


      call screen5(pstate, 60, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,172) = scor
      rate_eval % unscreened_rates(i_dscor_dt,172) = dscor_dt
      rate_eval % unscreened_rates(i_scor,476) = scor
      rate_eval % unscreened_rates(i_dscor_dt,476) = dscor_dt
      rate_eval % unscreened_rates(i_scor,477) = scor
      rate_eval % unscreened_rates(i_dscor_dt,477) = dscor_dt


      call screen5(pstate, 61, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,173) = scor
      rate_eval % unscreened_rates(i_dscor_dt,173) = dscor_dt
      rate_eval % unscreened_rates(i_scor,478) = scor
      rate_eval % unscreened_rates(i_dscor_dt,478) = dscor_dt
      rate_eval % unscreened_rates(i_scor,479) = scor
      rate_eval % unscreened_rates(i_dscor_dt,479) = dscor_dt


      call screen5(pstate, 62, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,174) = scor
      rate_eval % unscreened_rates(i_dscor_dt,174) = dscor_dt
      rate_eval % unscreened_rates(i_scor,480) = scor
      rate_eval % unscreened_rates(i_dscor_dt,480) = dscor_dt
      rate_eval % unscreened_rates(i_scor,481) = scor
      rate_eval % unscreened_rates(i_dscor_dt,481) = dscor_dt


      call screen5(pstate, 63, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,175) = scor
      rate_eval % unscreened_rates(i_dscor_dt,175) = dscor_dt
      rate_eval % unscreened_rates(i_scor,482) = scor
      rate_eval % unscreened_rates(i_dscor_dt,482) = dscor_dt
      rate_eval % unscreened_rates(i_scor,483) = scor
      rate_eval % unscreened_rates(i_dscor_dt,483) = dscor_dt


      call screen5(pstate, 64, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,177) = scor
      rate_eval % unscreened_rates(i_dscor_dt,177) = dscor_dt
      rate_eval % unscreened_rates(i_scor,487) = scor
      rate_eval % unscreened_rates(i_dscor_dt,487) = dscor_dt


      call screen5(pstate, 65, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,179) = scor
      rate_eval % unscreened_rates(i_dscor_dt,179) = dscor_dt


      call screen5(pstate, 66, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,180) = scor
      rate_eval % unscreened_rates(i_dscor_dt,180) = dscor_dt


      call screen5(pstate, 67, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,182) = scor
      rate_eval % unscreened_rates(i_dscor_dt,182) = dscor_dt


      call screen5(pstate, 68, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,183) = scor
      rate_eval % unscreened_rates(i_dscor_dt,183) = dscor_dt


      call screen5(pstate, 69, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,185) = scor
      rate_eval % unscreened_rates(i_dscor_dt,185) = dscor_dt


      call screen5(pstate, 70, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,186) = scor
      rate_eval % unscreened_rates(i_dscor_dt,186) = dscor_dt


      call screen5(pstate, 71, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,188) = scor
      rate_eval % unscreened_rates(i_dscor_dt,188) = dscor_dt
      rate_eval % unscreened_rates(i_scor,488) = scor
      rate_eval % unscreened_rates(i_dscor_dt,488) = dscor_dt


      call screen5(pstate, 72, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,189) = scor
      rate_eval % unscreened_rates(i_dscor_dt,189) = dscor_dt
      rate_eval % unscreened_rates(i_scor,489) = scor
      rate_eval % unscreened_rates(i_dscor_dt,489) = dscor_dt


      call screen5(pstate, 73, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,190) = scor
      rate_eval % unscreened_rates(i_dscor_dt,190) = dscor_dt


      call screen5(pstate, 74, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,192) = scor
      rate_eval % unscreened_rates(i_dscor_dt,192) = dscor_dt


      call screen5(pstate, 75, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,193) = scor
      rate_eval % unscreened_rates(i_dscor_dt,193) = dscor_dt
      rate_eval % unscreened_rates(i_scor,492) = scor
      rate_eval % unscreened_rates(i_dscor_dt,492) = dscor_dt


      call screen5(pstate, 76, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,195) = scor
      rate_eval % unscreened_rates(i_dscor_dt,195) = dscor_dt


      call screen5(pstate, 77, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,196) = scor
      rate_eval % unscreened_rates(i_dscor_dt,196) = dscor_dt
      rate_eval % unscreened_rates(i_scor,494) = scor
      rate_eval % unscreened_rates(i_dscor_dt,494) = dscor_dt


      call screen5(pstate, 78, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,198) = scor
      rate_eval % unscreened_rates(i_dscor_dt,198) = dscor_dt
      rate_eval % unscreened_rates(i_scor,495) = scor
      rate_eval % unscreened_rates(i_dscor_dt,495) = dscor_dt


      call screen5(pstate, 79, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,199) = scor
      rate_eval % unscreened_rates(i_dscor_dt,199) = dscor_dt
      rate_eval % unscreened_rates(i_scor,496) = scor
      rate_eval % unscreened_rates(i_dscor_dt,496) = dscor_dt


      call screen5(pstate, 80, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,201) = scor
      rate_eval % unscreened_rates(i_dscor_dt,201) = dscor_dt
      rate_eval % unscreened_rates(i_scor,498) = scor
      rate_eval % unscreened_rates(i_dscor_dt,498) = dscor_dt
      rate_eval % unscreened_rates(i_scor,499) = scor
      rate_eval % unscreened_rates(i_dscor_dt,499) = dscor_dt


      call screen5(pstate, 81, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,202) = scor
      rate_eval % unscreened_rates(i_dscor_dt,202) = dscor_dt


      call screen5(pstate, 82, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,203) = scor
      rate_eval % unscreened_rates(i_dscor_dt,203) = dscor_dt
      rate_eval % unscreened_rates(i_scor,500) = scor
      rate_eval % unscreened_rates(i_dscor_dt,500) = dscor_dt


      call screen5(pstate, 83, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,204) = scor
      rate_eval % unscreened_rates(i_dscor_dt,204) = dscor_dt


      call screen5(pstate, 84, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,207) = scor
      rate_eval % unscreened_rates(i_dscor_dt,207) = dscor_dt
      rate_eval % unscreened_rates(i_scor,506) = scor
      rate_eval % unscreened_rates(i_dscor_dt,506) = dscor_dt


      call screen5(pstate, 85, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,209) = scor
      rate_eval % unscreened_rates(i_dscor_dt,209) = dscor_dt


      call screen5(pstate, 86, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,210) = scor
      rate_eval % unscreened_rates(i_dscor_dt,210) = dscor_dt


      call screen5(pstate, 87, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,212) = scor
      rate_eval % unscreened_rates(i_dscor_dt,212) = dscor_dt


      call screen5(pstate, 88, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,213) = scor
      rate_eval % unscreened_rates(i_dscor_dt,213) = dscor_dt


      call screen5(pstate, 89, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,215) = scor
      rate_eval % unscreened_rates(i_dscor_dt,215) = dscor_dt


      call screen5(pstate, 90, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,216) = scor
      rate_eval % unscreened_rates(i_dscor_dt,216) = dscor_dt
      rate_eval % unscreened_rates(i_scor,510) = scor
      rate_eval % unscreened_rates(i_dscor_dt,510) = dscor_dt
      rate_eval % unscreened_rates(i_scor,511) = scor
      rate_eval % unscreened_rates(i_dscor_dt,511) = dscor_dt


      call screen5(pstate, 91, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,217) = scor
      rate_eval % unscreened_rates(i_dscor_dt,217) = dscor_dt
      rate_eval % unscreened_rates(i_scor,513) = scor
      rate_eval % unscreened_rates(i_dscor_dt,513) = dscor_dt


      call screen5(pstate, 92, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,218) = scor
      rate_eval % unscreened_rates(i_dscor_dt,218) = dscor_dt
      rate_eval % unscreened_rates(i_scor,514) = scor
      rate_eval % unscreened_rates(i_dscor_dt,514) = dscor_dt


      call screen5(pstate, 93, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,220) = scor
      rate_eval % unscreened_rates(i_dscor_dt,220) = dscor_dt
      rate_eval % unscreened_rates(i_scor,517) = scor
      rate_eval % unscreened_rates(i_dscor_dt,517) = dscor_dt


      call screen5(pstate, 94, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,222) = scor
      rate_eval % unscreened_rates(i_dscor_dt,222) = dscor_dt


      call screen5(pstate, 95, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,223) = scor
      rate_eval % unscreened_rates(i_dscor_dt,223) = dscor_dt
      rate_eval % unscreened_rates(i_scor,520) = scor
      rate_eval % unscreened_rates(i_dscor_dt,520) = dscor_dt


      call screen5(pstate, 96, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,225) = scor
      rate_eval % unscreened_rates(i_dscor_dt,225) = dscor_dt
      rate_eval % unscreened_rates(i_scor,523) = scor
      rate_eval % unscreened_rates(i_dscor_dt,523) = dscor_dt


      call screen5(pstate, 97, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,226) = scor
      rate_eval % unscreened_rates(i_dscor_dt,226) = dscor_dt
      rate_eval % unscreened_rates(i_scor,524) = scor
      rate_eval % unscreened_rates(i_dscor_dt,524) = dscor_dt


      call screen5(pstate, 98, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,228) = scor
      rate_eval % unscreened_rates(i_dscor_dt,228) = dscor_dt
      rate_eval % unscreened_rates(i_scor,526) = scor
      rate_eval % unscreened_rates(i_dscor_dt,526) = dscor_dt


      call screen5(pstate, 99, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,229) = scor
      rate_eval % unscreened_rates(i_dscor_dt,229) = dscor_dt


      call screen5(pstate, 100, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,230) = scor
      rate_eval % unscreened_rates(i_dscor_dt,230) = dscor_dt


      call screen5(pstate, 101, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,231) = scor
      rate_eval % unscreened_rates(i_dscor_dt,231) = dscor_dt


      call screen5(pstate, 102, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,233) = scor
      rate_eval % unscreened_rates(i_dscor_dt,233) = dscor_dt
      rate_eval % unscreened_rates(i_scor,529) = scor
      rate_eval % unscreened_rates(i_dscor_dt,529) = dscor_dt


      call screen5(pstate, 103, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,235) = scor
      rate_eval % unscreened_rates(i_dscor_dt,235) = dscor_dt


      call screen5(pstate, 104, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,236) = scor
      rate_eval % unscreened_rates(i_dscor_dt,236) = dscor_dt


      call screen5(pstate, 105, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,238) = scor
      rate_eval % unscreened_rates(i_dscor_dt,238) = dscor_dt


      call screen5(pstate, 106, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,239) = scor
      rate_eval % unscreened_rates(i_dscor_dt,239) = dscor_dt


      call screen5(pstate, 107, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,241) = scor
      rate_eval % unscreened_rates(i_dscor_dt,241) = dscor_dt


      call screen5(pstate, 108, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,242) = scor
      rate_eval % unscreened_rates(i_dscor_dt,242) = dscor_dt


      call screen5(pstate, 109, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,243) = scor
      rate_eval % unscreened_rates(i_dscor_dt,243) = dscor_dt
      rate_eval % unscreened_rates(i_scor,533) = scor
      rate_eval % unscreened_rates(i_dscor_dt,533) = dscor_dt


      call screen5(pstate, 110, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,244) = scor
      rate_eval % unscreened_rates(i_dscor_dt,244) = dscor_dt


      call screen5(pstate, 111, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,247) = scor
      rate_eval % unscreened_rates(i_dscor_dt,247) = dscor_dt


      call screen5(pstate, 112, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,248) = scor
      rate_eval % unscreened_rates(i_dscor_dt,248) = dscor_dt
      rate_eval % unscreened_rates(i_scor,539) = scor
      rate_eval % unscreened_rates(i_dscor_dt,539) = dscor_dt


      call screen5(pstate, 113, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,250) = scor
      rate_eval % unscreened_rates(i_dscor_dt,250) = dscor_dt
      rate_eval % unscreened_rates(i_scor,542) = scor
      rate_eval % unscreened_rates(i_dscor_dt,542) = dscor_dt


      call screen5(pstate, 114, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,251) = scor
      rate_eval % unscreened_rates(i_dscor_dt,251) = dscor_dt


      call screen5(pstate, 115, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,253) = scor
      rate_eval % unscreened_rates(i_dscor_dt,253) = dscor_dt
      rate_eval % unscreened_rates(i_scor,544) = scor
      rate_eval % unscreened_rates(i_dscor_dt,544) = dscor_dt
      rate_eval % unscreened_rates(i_scor,545) = scor
      rate_eval % unscreened_rates(i_dscor_dt,545) = dscor_dt


      call screen5(pstate, 116, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,254) = scor
      rate_eval % unscreened_rates(i_dscor_dt,254) = dscor_dt
      rate_eval % unscreened_rates(i_scor,546) = scor
      rate_eval % unscreened_rates(i_dscor_dt,546) = dscor_dt


      call screen5(pstate, 117, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,256) = scor
      rate_eval % unscreened_rates(i_dscor_dt,256) = dscor_dt
      rate_eval % unscreened_rates(i_scor,547) = scor
      rate_eval % unscreened_rates(i_dscor_dt,547) = dscor_dt
      rate_eval % unscreened_rates(i_scor,548) = scor
      rate_eval % unscreened_rates(i_dscor_dt,548) = dscor_dt


      call screen5(pstate, 118, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,257) = scor
      rate_eval % unscreened_rates(i_dscor_dt,257) = dscor_dt
      rate_eval % unscreened_rates(i_scor,549) = scor
      rate_eval % unscreened_rates(i_dscor_dt,549) = dscor_dt
      rate_eval % unscreened_rates(i_scor,550) = scor
      rate_eval % unscreened_rates(i_dscor_dt,550) = dscor_dt


      call screen5(pstate, 119, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,258) = scor
      rate_eval % unscreened_rates(i_dscor_dt,258) = dscor_dt
      rate_eval % unscreened_rates(i_scor,551) = scor
      rate_eval % unscreened_rates(i_dscor_dt,551) = dscor_dt
      rate_eval % unscreened_rates(i_scor,552) = scor
      rate_eval % unscreened_rates(i_dscor_dt,552) = dscor_dt


      call screen5(pstate, 120, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,259) = scor
      rate_eval % unscreened_rates(i_dscor_dt,259) = dscor_dt
      rate_eval % unscreened_rates(i_scor,553) = scor
      rate_eval % unscreened_rates(i_dscor_dt,553) = dscor_dt


      call screen5(pstate, 121, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,261) = scor
      rate_eval % unscreened_rates(i_dscor_dt,261) = dscor_dt


      call screen5(pstate, 122, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,263) = scor
      rate_eval % unscreened_rates(i_dscor_dt,263) = dscor_dt


      call screen5(pstate, 123, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,265) = scor
      rate_eval % unscreened_rates(i_dscor_dt,265) = dscor_dt


      call screen5(pstate, 124, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,266) = scor
      rate_eval % unscreened_rates(i_dscor_dt,266) = dscor_dt


      call screen5(pstate, 125, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,268) = scor
      rate_eval % unscreened_rates(i_dscor_dt,268) = dscor_dt
      rate_eval % unscreened_rates(i_scor,559) = scor
      rate_eval % unscreened_rates(i_dscor_dt,559) = dscor_dt


      call screen5(pstate, 126, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,269) = scor
      rate_eval % unscreened_rates(i_dscor_dt,269) = dscor_dt


      call screen5(pstate, 127, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,271) = scor
      rate_eval % unscreened_rates(i_dscor_dt,271) = dscor_dt


      call screen5(pstate, 128, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,272) = scor
      rate_eval % unscreened_rates(i_dscor_dt,272) = dscor_dt
      rate_eval % unscreened_rates(i_scor,560) = scor
      rate_eval % unscreened_rates(i_dscor_dt,560) = dscor_dt


      call screen5(pstate, 129, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,273) = scor
      rate_eval % unscreened_rates(i_dscor_dt,273) = dscor_dt


      call screen5(pstate, 130, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,274) = scor
      rate_eval % unscreened_rates(i_dscor_dt,274) = dscor_dt


      call screen5(pstate, 131, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,276) = scor
      rate_eval % unscreened_rates(i_dscor_dt,276) = dscor_dt
      rate_eval % unscreened_rates(i_scor,563) = scor
      rate_eval % unscreened_rates(i_dscor_dt,563) = dscor_dt


      call screen5(pstate, 132, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,277) = scor
      rate_eval % unscreened_rates(i_dscor_dt,277) = dscor_dt
      rate_eval % unscreened_rates(i_scor,564) = scor
      rate_eval % unscreened_rates(i_dscor_dt,564) = dscor_dt


      call screen5(pstate, 133, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,279) = scor
      rate_eval % unscreened_rates(i_dscor_dt,279) = dscor_dt
      rate_eval % unscreened_rates(i_scor,567) = scor
      rate_eval % unscreened_rates(i_dscor_dt,567) = dscor_dt


      call screen5(pstate, 134, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,280) = scor
      rate_eval % unscreened_rates(i_dscor_dt,280) = dscor_dt
      rate_eval % unscreened_rates(i_scor,568) = scor
      rate_eval % unscreened_rates(i_dscor_dt,568) = dscor_dt


      call screen5(pstate, 135, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,282) = scor
      rate_eval % unscreened_rates(i_dscor_dt,282) = dscor_dt
      rate_eval % unscreened_rates(i_scor,570) = scor
      rate_eval % unscreened_rates(i_dscor_dt,570) = dscor_dt


      call screen5(pstate, 136, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,283) = scor
      rate_eval % unscreened_rates(i_dscor_dt,283) = dscor_dt
      rate_eval % unscreened_rates(i_scor,571) = scor
      rate_eval % unscreened_rates(i_dscor_dt,571) = dscor_dt


      call screen5(pstate, 137, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,285) = scor
      rate_eval % unscreened_rates(i_dscor_dt,285) = dscor_dt
      rate_eval % unscreened_rates(i_scor,572) = scor
      rate_eval % unscreened_rates(i_dscor_dt,572) = dscor_dt


      call screen5(pstate, 138, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,286) = scor
      rate_eval % unscreened_rates(i_dscor_dt,286) = dscor_dt
      rate_eval % unscreened_rates(i_scor,573) = scor
      rate_eval % unscreened_rates(i_dscor_dt,573) = dscor_dt


      call screen5(pstate, 139, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,287) = scor
      rate_eval % unscreened_rates(i_dscor_dt,287) = dscor_dt
      rate_eval % unscreened_rates(i_scor,574) = scor
      rate_eval % unscreened_rates(i_dscor_dt,574) = dscor_dt
      rate_eval % unscreened_rates(i_scor,575) = scor
      rate_eval % unscreened_rates(i_dscor_dt,575) = dscor_dt


      call screen5(pstate, 140, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,288) = scor
      rate_eval % unscreened_rates(i_dscor_dt,288) = dscor_dt
      rate_eval % unscreened_rates(i_scor,576) = scor
      rate_eval % unscreened_rates(i_dscor_dt,576) = dscor_dt
      rate_eval % unscreened_rates(i_scor,577) = scor
      rate_eval % unscreened_rates(i_dscor_dt,577) = dscor_dt


      call screen5(pstate, 141, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,290) = scor
      rate_eval % unscreened_rates(i_dscor_dt,290) = dscor_dt
      rate_eval % unscreened_rates(i_scor,580) = scor
      rate_eval % unscreened_rates(i_dscor_dt,580) = dscor_dt


      call screen5(pstate, 142, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,292) = scor
      rate_eval % unscreened_rates(i_dscor_dt,292) = dscor_dt
      rate_eval % unscreened_rates(i_scor,583) = scor
      rate_eval % unscreened_rates(i_dscor_dt,583) = dscor_dt


      call screen5(pstate, 143, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,294) = scor
      rate_eval % unscreened_rates(i_dscor_dt,294) = dscor_dt


      call screen5(pstate, 144, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,295) = scor
      rate_eval % unscreened_rates(i_dscor_dt,295) = dscor_dt
      rate_eval % unscreened_rates(i_scor,586) = scor
      rate_eval % unscreened_rates(i_dscor_dt,586) = dscor_dt


      call screen5(pstate, 145, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,297) = scor
      rate_eval % unscreened_rates(i_dscor_dt,297) = dscor_dt


      call screen5(pstate, 146, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,298) = scor
      rate_eval % unscreened_rates(i_dscor_dt,298) = dscor_dt


      call screen5(pstate, 147, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,300) = scor
      rate_eval % unscreened_rates(i_dscor_dt,300) = dscor_dt


      call screen5(pstate, 148, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,301) = scor
      rate_eval % unscreened_rates(i_dscor_dt,301) = dscor_dt


      call screen5(pstate, 149, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,303) = scor
      rate_eval % unscreened_rates(i_dscor_dt,303) = dscor_dt


      call screen5(pstate, 150, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,304) = scor
      rate_eval % unscreened_rates(i_dscor_dt,304) = dscor_dt


      call screen5(pstate, 151, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,305) = scor
      rate_eval % unscreened_rates(i_dscor_dt,305) = dscor_dt


      call screen5(pstate, 152, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,307) = scor
      rate_eval % unscreened_rates(i_dscor_dt,307) = dscor_dt


      call screen5(pstate, 153, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,308) = scor
      rate_eval % unscreened_rates(i_dscor_dt,308) = dscor_dt
      rate_eval % unscreened_rates(i_scor,591) = scor
      rate_eval % unscreened_rates(i_dscor_dt,591) = dscor_dt


      call screen5(pstate, 154, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,310) = scor
      rate_eval % unscreened_rates(i_dscor_dt,310) = dscor_dt


      call screen5(pstate, 155, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,311) = scor
      rate_eval % unscreened_rates(i_dscor_dt,311) = dscor_dt
      rate_eval % unscreened_rates(i_scor,594) = scor
      rate_eval % unscreened_rates(i_dscor_dt,594) = dscor_dt


      call screen5(pstate, 156, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,313) = scor
      rate_eval % unscreened_rates(i_dscor_dt,313) = dscor_dt


      call screen5(pstate, 157, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,314) = scor
      rate_eval % unscreened_rates(i_dscor_dt,314) = dscor_dt
      rate_eval % unscreened_rates(i_scor,597) = scor
      rate_eval % unscreened_rates(i_dscor_dt,597) = dscor_dt


      call screen5(pstate, 158, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,316) = scor
      rate_eval % unscreened_rates(i_dscor_dt,316) = dscor_dt
      rate_eval % unscreened_rates(i_scor,599) = scor
      rate_eval % unscreened_rates(i_dscor_dt,599) = dscor_dt


      call screen5(pstate, 159, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,317) = scor
      rate_eval % unscreened_rates(i_dscor_dt,317) = dscor_dt
      rate_eval % unscreened_rates(i_scor,600) = scor
      rate_eval % unscreened_rates(i_dscor_dt,600) = dscor_dt


      call screen5(pstate, 160, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,319) = scor
      rate_eval % unscreened_rates(i_dscor_dt,319) = dscor_dt
      rate_eval % unscreened_rates(i_scor,601) = scor
      rate_eval % unscreened_rates(i_dscor_dt,601) = dscor_dt
      rate_eval % unscreened_rates(i_scor,602) = scor
      rate_eval % unscreened_rates(i_dscor_dt,602) = dscor_dt


      call screen5(pstate, 161, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,320) = scor
      rate_eval % unscreened_rates(i_dscor_dt,320) = dscor_dt


      call screen5(pstate, 162, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,321) = scor
      rate_eval % unscreened_rates(i_dscor_dt,321) = dscor_dt
      rate_eval % unscreened_rates(i_scor,603) = scor
      rate_eval % unscreened_rates(i_dscor_dt,603) = dscor_dt


      call screen5(pstate, 163, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,322) = scor
      rate_eval % unscreened_rates(i_dscor_dt,322) = dscor_dt


      call screen5(pstate, 164, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,324) = scor
      rate_eval % unscreened_rates(i_dscor_dt,324) = dscor_dt
      rate_eval % unscreened_rates(i_scor,606) = scor
      rate_eval % unscreened_rates(i_dscor_dt,606) = dscor_dt


      call screen5(pstate, 165, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,326) = scor
      rate_eval % unscreened_rates(i_dscor_dt,326) = dscor_dt


      call screen5(pstate, 166, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,327) = scor
      rate_eval % unscreened_rates(i_dscor_dt,327) = dscor_dt
      rate_eval % unscreened_rates(i_scor,609) = scor
      rate_eval % unscreened_rates(i_dscor_dt,609) = dscor_dt


      call screen5(pstate, 167, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,329) = scor
      rate_eval % unscreened_rates(i_dscor_dt,329) = dscor_dt


      call screen5(pstate, 168, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,330) = scor
      rate_eval % unscreened_rates(i_dscor_dt,330) = dscor_dt
      rate_eval % unscreened_rates(i_scor,612) = scor
      rate_eval % unscreened_rates(i_dscor_dt,612) = dscor_dt


      call screen5(pstate, 169, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,332) = scor
      rate_eval % unscreened_rates(i_dscor_dt,332) = dscor_dt


      call screen5(pstate, 170, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,333) = scor
      rate_eval % unscreened_rates(i_dscor_dt,333) = dscor_dt


      call screen5(pstate, 171, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,335) = scor
      rate_eval % unscreened_rates(i_dscor_dt,335) = dscor_dt


      call screen5(pstate, 172, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,336) = scor
      rate_eval % unscreened_rates(i_dscor_dt,336) = dscor_dt


      call screen5(pstate, 173, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,337) = scor
      rate_eval % unscreened_rates(i_dscor_dt,337) = dscor_dt


      call screen5(pstate, 174, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,338) = scor
      rate_eval % unscreened_rates(i_dscor_dt,338) = dscor_dt


      call screen5(pstate, 175, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,340) = scor
      rate_eval % unscreened_rates(i_dscor_dt,340) = dscor_dt
      rate_eval % unscreened_rates(i_scor,618) = scor
      rate_eval % unscreened_rates(i_dscor_dt,618) = dscor_dt


      call screen5(pstate, 176, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,342) = scor
      rate_eval % unscreened_rates(i_dscor_dt,342) = dscor_dt


      call screen5(pstate, 177, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,343) = scor
      rate_eval % unscreened_rates(i_dscor_dt,343) = dscor_dt
      rate_eval % unscreened_rates(i_scor,621) = scor
      rate_eval % unscreened_rates(i_dscor_dt,621) = dscor_dt


      call screen5(pstate, 178, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,345) = scor
      rate_eval % unscreened_rates(i_dscor_dt,345) = dscor_dt


      call screen5(pstate, 179, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,346) = scor
      rate_eval % unscreened_rates(i_dscor_dt,346) = dscor_dt
      rate_eval % unscreened_rates(i_scor,624) = scor
      rate_eval % unscreened_rates(i_dscor_dt,624) = dscor_dt


      call screen5(pstate, 180, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,348) = scor
      rate_eval % unscreened_rates(i_dscor_dt,348) = dscor_dt


      call screen5(pstate, 181, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,349) = scor
      rate_eval % unscreened_rates(i_dscor_dt,349) = dscor_dt
      rate_eval % unscreened_rates(i_scor,627) = scor
      rate_eval % unscreened_rates(i_dscor_dt,627) = dscor_dt


      call screen5(pstate, 182, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,351) = scor
      rate_eval % unscreened_rates(i_dscor_dt,351) = dscor_dt
      rate_eval % unscreened_rates(i_scor,629) = scor
      rate_eval % unscreened_rates(i_dscor_dt,629) = dscor_dt


      call screen5(pstate, 183, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,352) = scor
      rate_eval % unscreened_rates(i_dscor_dt,352) = dscor_dt
      rate_eval % unscreened_rates(i_scor,630) = scor
      rate_eval % unscreened_rates(i_dscor_dt,630) = dscor_dt


      call screen5(pstate, 184, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,354) = scor
      rate_eval % unscreened_rates(i_dscor_dt,354) = dscor_dt
      rate_eval % unscreened_rates(i_scor,632) = scor
      rate_eval % unscreened_rates(i_dscor_dt,632) = dscor_dt


      call screen5(pstate, 185, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,355) = scor
      rate_eval % unscreened_rates(i_dscor_dt,355) = dscor_dt


      call screen5(pstate, 186, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,356) = scor
      rate_eval % unscreened_rates(i_dscor_dt,356) = dscor_dt
      rate_eval % unscreened_rates(i_scor,633) = scor
      rate_eval % unscreened_rates(i_dscor_dt,633) = dscor_dt


      call screen5(pstate, 187, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,358) = scor
      rate_eval % unscreened_rates(i_dscor_dt,358) = dscor_dt
      rate_eval % unscreened_rates(i_scor,636) = scor
      rate_eval % unscreened_rates(i_dscor_dt,636) = dscor_dt


      call screen5(pstate, 188, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,360) = scor
      rate_eval % unscreened_rates(i_dscor_dt,360) = dscor_dt


      call screen5(pstate, 189, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,361) = scor
      rate_eval % unscreened_rates(i_dscor_dt,361) = dscor_dt
      rate_eval % unscreened_rates(i_scor,639) = scor
      rate_eval % unscreened_rates(i_dscor_dt,639) = dscor_dt


      call screen5(pstate, 190, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,363) = scor
      rate_eval % unscreened_rates(i_dscor_dt,363) = dscor_dt


      call screen5(pstate, 191, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,364) = scor
      rate_eval % unscreened_rates(i_dscor_dt,364) = dscor_dt
      rate_eval % unscreened_rates(i_scor,642) = scor
      rate_eval % unscreened_rates(i_dscor_dt,642) = dscor_dt


      call screen5(pstate, 192, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,366) = scor
      rate_eval % unscreened_rates(i_dscor_dt,366) = dscor_dt


      call screen5(pstate, 193, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,367) = scor
      rate_eval % unscreened_rates(i_dscor_dt,367) = dscor_dt


      call screen5(pstate, 194, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,369) = scor
      rate_eval % unscreened_rates(i_dscor_dt,369) = dscor_dt


      call screen5(pstate, 195, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,370) = scor
      rate_eval % unscreened_rates(i_dscor_dt,370) = dscor_dt


      call screen5(pstate, 196, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,371) = scor
      rate_eval % unscreened_rates(i_dscor_dt,371) = dscor_dt


      call screen5(pstate, 197, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,372) = scor
      rate_eval % unscreened_rates(i_dscor_dt,372) = dscor_dt


      call screen5(pstate, 198, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,374) = scor
      rate_eval % unscreened_rates(i_dscor_dt,374) = dscor_dt


      call screen5(pstate, 199, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,376) = scor
      rate_eval % unscreened_rates(i_dscor_dt,376) = dscor_dt


      call screen5(pstate, 200, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,378) = scor
      rate_eval % unscreened_rates(i_dscor_dt,378) = dscor_dt


      call screen5(pstate, 201, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,380) = scor
      rate_eval % unscreened_rates(i_dscor_dt,380) = dscor_dt


      call screen5(pstate, 202, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,382) = scor
      rate_eval % unscreened_rates(i_dscor_dt,382) = dscor_dt
      rate_eval % unscreened_rates(i_scor,660) = scor
      rate_eval % unscreened_rates(i_dscor_dt,660) = dscor_dt


      call screen5(pstate, 203, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,383) = scor
      rate_eval % unscreened_rates(i_dscor_dt,383) = dscor_dt
      rate_eval % unscreened_rates(i_scor,663) = scor
      rate_eval % unscreened_rates(i_dscor_dt,663) = dscor_dt


      call screen5(pstate, 204, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,392) = scor
      rate_eval % unscreened_rates(i_dscor_dt,392) = dscor_dt


      call screen5(pstate, 205, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,394) = scor
      rate_eval % unscreened_rates(i_dscor_dt,394) = dscor_dt


      call screen5(pstate, 206, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,395) = scor
      rate_eval % unscreened_rates(i_dscor_dt,395) = dscor_dt
      rate_eval % unscreened_rates(i_scor,676) = scor
      rate_eval % unscreened_rates(i_dscor_dt,676) = dscor_dt


      call screen5(pstate, 207, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,396) = scor
      rate_eval % unscreened_rates(i_dscor_dt,396) = dscor_dt
      rate_eval % unscreened_rates(i_scor,397) = scor
      rate_eval % unscreened_rates(i_dscor_dt,397) = dscor_dt


      call screen5(pstate, 208, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,399) = scor
      rate_eval % unscreened_rates(i_dscor_dt,399) = dscor_dt


      call screen5(pstate, 209, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,400) = scor
      rate_eval % unscreened_rates(i_dscor_dt,400) = dscor_dt


      call screen5(pstate, 210, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,401) = scor
      rate_eval % unscreened_rates(i_dscor_dt,401) = dscor_dt


      call screen5(pstate, 211, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,402) = scor
      rate_eval % unscreened_rates(i_dscor_dt,402) = dscor_dt


      call screen5(pstate, 212, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,405) = scor
      rate_eval % unscreened_rates(i_dscor_dt,405) = dscor_dt
      rate_eval % unscreened_rates(i_scor,406) = scor
      rate_eval % unscreened_rates(i_dscor_dt,406) = dscor_dt


      call screen5(pstate, 213, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,407) = scor
      rate_eval % unscreened_rates(i_dscor_dt,407) = dscor_dt
      rate_eval % unscreened_rates(i_scor,408) = scor
      rate_eval % unscreened_rates(i_dscor_dt,408) = dscor_dt
      rate_eval % unscreened_rates(i_scor,409) = scor
      rate_eval % unscreened_rates(i_dscor_dt,409) = dscor_dt


      call screen5(pstate, 214, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,429) = scor
      rate_eval % unscreened_rates(i_dscor_dt,429) = dscor_dt
      rate_eval % unscreened_rates(i_scor,430) = scor
      rate_eval % unscreened_rates(i_dscor_dt,430) = dscor_dt


      call screen5(pstate, 215, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,433) = scor
      rate_eval % unscreened_rates(i_dscor_dt,433) = dscor_dt
      rate_eval % unscreened_rates(i_scor,434) = scor
      rate_eval % unscreened_rates(i_dscor_dt,434) = dscor_dt
      rate_eval % unscreened_rates(i_scor,435) = scor
      rate_eval % unscreened_rates(i_dscor_dt,435) = dscor_dt


      call screen5(pstate, 216, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,440) = scor
      rate_eval % unscreened_rates(i_dscor_dt,440) = dscor_dt


      call screen5(pstate, 217, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,441) = scor
      rate_eval % unscreened_rates(i_dscor_dt,441) = dscor_dt


      call screen5(pstate, 218, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,443) = scor
      rate_eval % unscreened_rates(i_dscor_dt,443) = dscor_dt


      call screen5(pstate, 219, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,458) = scor
      rate_eval % unscreened_rates(i_dscor_dt,458) = dscor_dt


      call screen5(pstate, 220, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,503) = scor
      rate_eval % unscreened_rates(i_dscor_dt,503) = dscor_dt


      call screen5(pstate, 221, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,536) = scor
      rate_eval % unscreened_rates(i_dscor_dt,536) = dscor_dt


      call screen5(pstate, 222, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,588) = scor
      rate_eval % unscreened_rates(i_dscor_dt,588) = dscor_dt


      call screen5(pstate, 223, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,649) = scor
      rate_eval % unscreened_rates(i_dscor_dt,649) = dscor_dt


      call screen5(pstate, 224, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,652) = scor
      rate_eval % unscreened_rates(i_dscor_dt,652) = dscor_dt


      call screen5(pstate, 225, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,655) = scor
      rate_eval % unscreened_rates(i_dscor_dt,655) = dscor_dt


      call screen5(pstate, 226, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,658) = scor
      rate_eval % unscreened_rates(i_dscor_dt,658) = dscor_dt


      call screen5(pstate, 227, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,661) = scor
      rate_eval % unscreened_rates(i_dscor_dt,661) = dscor_dt


      call screen5(pstate, 228, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,675) = scor
      rate_eval % unscreened_rates(i_dscor_dt,675) = dscor_dt


      call screen5(pstate, 229, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,677) = scor
      rate_eval % unscreened_rates(i_dscor_dt,677) = dscor_dt


      call screen5(pstate, 230, scor, dscor_dt, dscor_dd)
      rate_eval % unscreened_rates(i_scor,678) = scor
      rate_eval % unscreened_rates(i_dscor_dt,678) = dscor_dt

    end if


    ! Compute screened rates
    rate_eval % screened_rates = rate_eval % unscreened_rates(i_rate, :) * &
                                 rate_eval % unscreened_rates(i_scor, :)

  end subroutine evaluate_rates


  subroutine actual_rhs(state, ydot)
    
    !$acc routine seq

    use extern_probin_module, only: do_constant_volume_burn, disable_thermal_neutrinos
    use burn_type_module, only: net_itemp, net_ienuc, neqs
    use sneut_module, only: sneut5
    use temperature_integration_module, only: temperature_rhs

    implicit none

    type(burn_t), intent(in) :: state
    real(rt), intent(inout) :: ydot(neqs)

    type(rate_eval_t) :: rate_eval
    real(rt) :: Y(nspec), ydot_nuc(nspec)
    integer :: i, j
    real(rt) :: rhoy, ye, enuc
    real(rt) :: sneut, dsneutdt, dsneutdd, snuda, snudz

    !$gpu

    ! Set molar abundances
    Y(:) = state % xn(:) * aion_inv(:)

    call evaluate_rates(state, rate_eval)

    call rhs_nuc(state, ydot_nuc, Y, rate_eval % screened_rates)
    ydot(1:nspec) = ydot_nuc

    ! ion binding energy contributions
    call ener_gener_rate(ydot_nuc, enuc)

    ! include reaction neutrino losses (non-thermal)

    ! Get the thermal neutrino losses
    if (.not. disable_thermal_neutrinos) then
       call sneut5(state % T, state % rho, state % abar, state % zbar, sneut, dsneutdt, dsneutdd, snuda, snudz)
    else
       sneut = ZERO
    end if

    ! Append the energy equation (this is erg/g/s)
    ydot(net_ienuc) = enuc - sneut

    ! Append the temperature equation
    call temperature_rhs(state, ydot)

  end subroutine actual_rhs


  subroutine rhs_nuc(state, ydot_nuc, Y, screened_rates)

    !$acc routine seq

    implicit none

    type (burn_t), intent(in) :: state
    real(rt), intent(out) :: ydot_nuc(nspec)
    real(rt), intent(in)  :: Y(nspec)
    real(rt), intent(in)  :: screened_rates(nrates)

    !$gpu



    ydot_nuc(jn) = ( &
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*Y(jne20)*state % rho + &
      screened_rates(k_d_c13__n_n14)*Y(jc13)*Y(jd)*state % rho + &
      screened_rates(k_d_c14__n_n15)*Y(jc14)*Y(jd)*state % rho + 0.5e0_rt* &
      screened_rates(k_d_d__n_he3)*Y(jd)**2*state % rho + screened_rates(k_d_t__n_he4)* &
      Y(jd)*Y(jt)*state % rho + screened_rates(k_he4_al27__n_p30)*Y(jal27)* &
      Y(jhe4)*state % rho + screened_rates(k_he4_al28__n_p31)*Y(jal28)* &
      Y(jhe4)*state % rho + screened_rates(k_he4_al29__n_p32)*Y(jal29)* &
      Y(jhe4)*state % rho + screened_rates(k_he4_c13__n_o16)*Y(jc13)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_ca43__n_ti46)*Y(jca43)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_f20__n_na23)*Y(jf20)*Y(jhe4)*state % rho &
      + screened_rates(k_he4_f21__n_na24)*Y(jf21)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*Y(jk41)*state % rho + &
      screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*Y(jk42)*state % rho + &
      screened_rates(k_he4_mg25__n_si28)*Y(jhe4)*Y(jmg25)*state % rho + &
      screened_rates(k_he4_mg26__n_si29)*Y(jhe4)*Y(jmg26)*state % rho + &
      screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*Y(jmg27)*state % rho + &
      screened_rates(k_he4_na24__n_al27)*Y(jhe4)*Y(jna24)*state % rho + &
      screened_rates(k_he4_ne21__n_mg24)*Y(jhe4)*Y(jne21)*state % rho + &
      screened_rates(k_he4_ne22__n_mg25)*Y(jhe4)*Y(jne22)*state % rho + &
      screened_rates(k_he4_ne23__n_mg26)*Y(jhe4)*Y(jne23)*state % rho + &
      screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*Y(jne24)*state % rho + &
      screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*Y(jo17)*state % rho + &
      screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*Y(jo18)*state % rho + &
      screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*Y(jo19)*state % rho + &
      screened_rates(k_he4_s34__n_ar37)*Y(jhe4)*Y(js34)*state % rho + &
      screened_rates(k_he4_s35__n_ar38)*Y(jhe4)*Y(js35)*state % rho + &
      screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*Y(jsc46)*state % rho + &
      screened_rates(k_he4_si31__n_s34)*Y(jhe4)*Y(jsi31)*state % rho + &
      screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*Y(jti49)*state % rho - &
      screened_rates(k_n__p__weak__wc12)*Y(jn) - screened_rates(k_n_al25__al26)* &
      Y(jal25)*Y(jn)*state % rho - screened_rates(k_n_al25__he4_na22)* &
      Y(jal25)*Y(jn)*state % rho - screened_rates(k_n_al25__p_mg25)*Y(jal25)* &
      Y(jn)*state % rho - screened_rates(k_n_al26__al27)*Y(jal26)*Y(jn)* &
      state % rho - screened_rates(k_n_al26__he4_na23)*Y(jal26)*Y(jn)*state % rho &
      - screened_rates(k_n_al26__p_mg26)*Y(jal26)*Y(jn)*state % rho - &
      screened_rates(k_n_al27__al28)*Y(jal27)*Y(jn)*state % rho - &
      screened_rates(k_n_al28__al29)*Y(jal28)*Y(jn)*state % rho - &
      screened_rates(k_n_ar35__ar36)*Y(jar35)*Y(jn)*state % rho - &
      screened_rates(k_n_ar35__he4_s32)*Y(jar35)*Y(jn)*state % rho - &
      screened_rates(k_n_ar35__p_cl35)*Y(jar35)*Y(jn)*state % rho - &
      screened_rates(k_n_ar36__ar37)*Y(jar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ar36__he4_s33)*Y(jar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ar36__p_cl36)*Y(jar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ar37__ar38)*Y(jar37)*Y(jn)*state % rho - &
      screened_rates(k_n_ar37__p_cl37)*Y(jar37)*Y(jn)*state % rho - &
      screened_rates(k_n_ar38__ar39)*Y(jar38)*Y(jn)*state % rho - &
      screened_rates(k_n_c12__c13)*Y(jc12)*Y(jn)*state % rho - &
      screened_rates(k_n_c13__c14)*Y(jc13)*Y(jn)*state % rho - &
      screened_rates(k_n_ca39__ca40)*Y(jca39)*Y(jn)*state % rho - &
      screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*Y(jn)*state % rho - &
      screened_rates(k_n_ca39__p_k39)*Y(jca39)*Y(jn)*state % rho - &
      screened_rates(k_n_ca40__ca41)*Y(jca40)*Y(jn)*state % rho - &
      screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*Y(jn)*state % rho - &
      screened_rates(k_n_ca41__ca42)*Y(jca41)*Y(jn)*state % rho - &
      screened_rates(k_n_ca41__he4_ar38)*Y(jca41)*Y(jn)*state % rho - &
      screened_rates(k_n_ca42__ca43)*Y(jca42)*Y(jn)*state % rho - &
      screened_rates(k_n_ca42__he4_ar39)*Y(jca42)*Y(jn)*state % rho - &
      screened_rates(k_n_ca43__ca44)*Y(jca43)*Y(jn)*state % rho - &
      screened_rates(k_n_cl33__cl34)*Y(jcl33)*Y(jn)*state % rho - &
      screened_rates(k_n_cl33__he4_p30)*Y(jcl33)*Y(jn)*state % rho - &
      screened_rates(k_n_cl33__p_s33)*Y(jcl33)*Y(jn)*state % rho - &
      screened_rates(k_n_cl34__cl35)*Y(jcl34)*Y(jn)*state % rho - &
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*Y(jn)*state % rho - &
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__cl36)*Y(jcl35)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*Y(jn)*state % rho - &
      screened_rates(k_n_cl36__cl37)*Y(jcl36)*Y(jn)*state % rho - &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*Y(jn)*state % rho - &
      screened_rates(k_n_co53__co54)*Y(jco53)*Y(jn)*state % rho - &
      screened_rates(k_n_co53__he4_mn50)*Y(jco53)*Y(jn)*state % rho - &
      screened_rates(k_n_co53__p_fe53)*Y(jco53)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__co55)*Y(jco54)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__p_fe54)*Y(jco54)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__co56)*Y(jco55)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__co57)*Y(jco56)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__he4_mn53)*Y(jco56)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__p_fe56)*Y(jco56)*Y(jn)*state % rho - &
      screened_rates(k_n_co57__co58)*Y(jco57)*Y(jn)*state % rho - &
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*Y(jn)*state % rho - &
      screened_rates(k_n_co58__he4_mn55)*Y(jco58)*Y(jn)*state % rho - &
      screened_rates(k_n_cr47__cr48)*Y(jcr47)*Y(jn)*state % rho - &
      screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*Y(jn)*state % rho - &
      screened_rates(k_n_cr47__p_v47)*Y(jcr47)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__cr49)*Y(jcr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__p_v48)*Y(jcr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__cr50)*Y(jcr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__p_v49)*Y(jcr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr50__cr51)*Y(jcr50)*Y(jn)*state % rho - &
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__cr52)*Y(jcr51)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__p_v51)*Y(jcr51)*Y(jn)*state % rho - &
      screened_rates(k_n_d__t)*Y(jd)*Y(jn)*state % rho - screened_rates(k_n_f18__f19)* &
      Y(jf18)*Y(jn)*state % rho - screened_rates(k_n_f18__he4_n15)*Y(jf18)* &
      Y(jn)*state % rho - screened_rates(k_n_f18__p_o18)*Y(jf18)*Y(jn)* &
      state % rho - screened_rates(k_n_f19__f20)*Y(jf19)*Y(jn)*state % rho - &
      screened_rates(k_n_f20__f21)*Y(jf20)*Y(jn)*state % rho - &
      screened_rates(k_n_fe51__fe52)*Y(jfe51)*Y(jn)*state % rho - &
      screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*Y(jn)*state % rho - &
      screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*Y(jn)*state % rho - &
      screened_rates(k_n_fe52__fe53)*Y(jfe52)*Y(jn)*state % rho - &
      screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*Y(jn)*state % rho - &
      screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__fe54)*Y(jfe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__fe55)*Y(jfe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__fe56)*Y(jfe55)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__p_mn55)*Y(jfe55)*Y(jn)*state % rho - &
      screened_rates(k_n_he3__he4)*Y(jhe3)*Y(jn)*state % rho - &
      screened_rates(k_n_he3__p_t)*Y(jhe3)*Y(jn)*state % rho - &
      screened_rates(k_n_k37__he4_cl34)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_n_k37__k38)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_n_k37__p_ar37)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_n_k38__he4_cl35)*Y(jk38)*Y(jn)*state % rho - &
      screened_rates(k_n_k38__k39)*Y(jk38)*Y(jn)*state % rho - &
      screened_rates(k_n_k38__p_ar38)*Y(jk38)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__he4_cl36)*Y(jk39)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__k40)*Y(jk39)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__p_ar39)*Y(jk39)*Y(jn)*state % rho - &
      screened_rates(k_n_k40__he4_cl37)*Y(jk40)*Y(jn)*state % rho - &
      screened_rates(k_n_k40__k41)*Y(jk40)*Y(jn)*state % rho - &
      screened_rates(k_n_k41__k42)*Y(jk41)*Y(jn)*state % rho - &
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_mg22__mg23)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_mg22__p_na22)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__c12_c12)*Y(jmg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__mg24)*Y(jmg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg24__mg25)*Y(jmg24)*Y(jn)*state % rho - &
      screened_rates(k_n_mg25__mg26)*Y(jmg25)*Y(jn)*state % rho - &
      screened_rates(k_n_mg26__mg27)*Y(jmg26)*Y(jn)*state % rho - &
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_mn49__mn50)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_mn50__he4_v47)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn50__mn51)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_n_mn51__mn52)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_n_mn52__mn53)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*Y(jn)*state % rho - &
      screened_rates(k_n_mn53__mn54)*Y(jmn53)*Y(jn)*state % rho - &
      screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*Y(jn)*state % rho - &
      screened_rates(k_n_mn54__mn55)*Y(jmn54)*Y(jn)*state % rho - &
      screened_rates(k_n_n13__n14)*Y(jn13)*Y(jn)*state % rho - &
      screened_rates(k_n_n14__n15)*Y(jn14)*Y(jn)*state % rho - &
      screened_rates(k_n_n14__p_c14)*Y(jn14)*Y(jn)*state % rho - &
      screened_rates(k_n_na20__na21)*Y(jn)*Y(jna20)*state % rho - &
      screened_rates(k_n_na20__p_ne20)*Y(jn)*Y(jna20)*state % rho - &
      screened_rates(k_n_na21__he4_f18)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__na22)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__p_ne21)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_n_na22__he4_f19)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__na23)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__p_ne22)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_n_na23__na24)*Y(jn)*Y(jna23)*state % rho - &
      screened_rates(k_n_ne19__he4_o16)*Y(jn)*Y(jne19)*state % rho - &
      screened_rates(k_n_ne19__ne20)*Y(jn)*Y(jne19)*state % rho - &
      screened_rates(k_n_ne20__ne21)*Y(jn)*Y(jne20)*state % rho - &
      screened_rates(k_n_ne21__ne22)*Y(jn)*Y(jne21)*state % rho - &
      screened_rates(k_n_ne22__ne23)*Y(jn)*Y(jne22)*state % rho - &
      screened_rates(k_n_ne23__ne24)*Y(jn)*Y(jne23)*state % rho - &
      screened_rates(k_n_ni54__he4_fe51)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni54__ni55)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni54__p_co54)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni55__he4_fe52)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__ni56)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__p_co55)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni56__he4_fe53)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__ni57)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__p_co56)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni57__he4_fe54)*Y(jn)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__ni58)*Y(jn)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__p_co57)*Y(jn)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni58__he4_fe55)*Y(jn)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni58__ni59)*Y(jn)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni58__p_co58)*Y(jn)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni59__he4_fe56)*Y(jn)*Y(jni59)*state % rho - &
      screened_rates(k_n_ni59__ni60)*Y(jn)*Y(jni59)*state % rho - &
      screened_rates(k_n_o16__o17)*Y(jn)*Y(jo16)*state % rho - &
      screened_rates(k_n_o17__he4_c14)*Y(jn)*Y(jo17)*state % rho - &
      screened_rates(k_n_o17__o18)*Y(jn)*Y(jo17)*state % rho - &
      screened_rates(k_n_o18__o19)*Y(jn)*Y(jo18)*state % rho - &
      screened_rates(k_n_p29__he4_al26)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_n_p29__p30)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_n_p29__p_si29)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_n_p30__p31)*Y(jn)*Y(jp30)*state % rho - &
      screened_rates(k_n_p30__p_si30)*Y(jn)*Y(jp30)*state % rho - &
      screened_rates(k_n_p31__p32)*Y(jn)*Y(jp31)*state % rho - &
      screened_rates(k_n_p32__p33)*Y(jn)*Y(jp32)*state % rho - &
      screened_rates(k_n_p32__p_si32)*Y(jn)*Y(jp32)*state % rho - &
      screened_rates(k_n_p__d)*Y(jn)*Y(jp)*state % rho - 0.5e0_rt* &
      screened_rates(k_n_p_p__p)*Y(jn)*Y(jp)**2*state % rho**2 - &
      screened_rates(k_n_s30__he4_si27)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_s30__p_p30)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_s30__s31)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_s31__he4_si28)*Y(jn)*Y(js31)*state % rho - &
      screened_rates(k_n_s31__p_p31)*Y(jn)*Y(js31)*state % rho - &
      screened_rates(k_n_s31__s32)*Y(jn)*Y(js31)*state % rho - &
      screened_rates(k_n_s32__he4_si29)*Y(jn)*Y(js32)*state % rho - &
      screened_rates(k_n_s32__s33)*Y(jn)*Y(js32)*state % rho - &
      screened_rates(k_n_s33__he4_si30)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_n_s33__p_p33)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_n_s33__s34)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_n_s34__s35)*Y(jn)*Y(js34)*state % rho - &
      screened_rates(k_n_s35__he4_si32)*Y(jn)*Y(js35)*state % rho - &
      screened_rates(k_n_sc42__he4_k39)*Y(jn)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__p_ca42)*Y(jn)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__sc43)*Y(jn)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc43__he4_k40)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc43__p_ca43)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc43__sc44)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc44__p_ca44)*Y(jn)*Y(jsc44)*state % rho - &
      screened_rates(k_n_sc44__sc45)*Y(jn)*Y(jsc44)*state % rho - &
      screened_rates(k_n_sc45__sc46)*Y(jn)*Y(jsc45)*state % rho - &
      screened_rates(k_n_si27__c12_o16)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__he4_mg24)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__p_al27)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__si28)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si28__si29)*Y(jn)*Y(jsi28)*state % rho - &
      screened_rates(k_n_si29__si30)*Y(jn)*Y(jsi29)*state % rho - &
      screened_rates(k_n_si30__si31)*Y(jn)*Y(jsi30)*state % rho - &
      screened_rates(k_n_si31__si32)*Y(jn)*Y(jsi31)*state % rho - &
      screened_rates(k_n_ti43__he4_ca40)*Y(jn)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__p_sc43)*Y(jn)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__ti44)*Y(jn)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti44__he4_ca41)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__p_sc44)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__ti45)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti45__he4_ca42)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__p_sc45)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__ti46)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti46__ti47)*Y(jn)*Y(jti46)*state % rho - &
      screened_rates(k_n_ti47__he4_ca44)*Y(jn)*Y(jti47)*state % rho - &
      screened_rates(k_n_ti47__ti48)*Y(jn)*Y(jti47)*state % rho - &
      screened_rates(k_n_ti48__ti49)*Y(jn)*Y(jti48)*state % rho - &
      screened_rates(k_n_v46__he4_sc43)*Y(jn)*Y(jv46)*state % rho - &
      screened_rates(k_n_v46__p_ti46)*Y(jn)*Y(jv46)*state % rho - &
      screened_rates(k_n_v46__v47)*Y(jn)*Y(jv46)*state % rho - &
      screened_rates(k_n_v47__he4_sc44)*Y(jn)*Y(jv47)*state % rho - &
      screened_rates(k_n_v47__p_ti47)*Y(jn)*Y(jv47)*state % rho - &
      screened_rates(k_n_v47__v48)*Y(jn)*Y(jv47)*state % rho - &
      screened_rates(k_n_v48__he4_sc45)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_n_v48__p_ti48)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_n_v48__v49)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_n_v49__p_ti49)*Y(jn)*Y(jv49)*state % rho - &
      screened_rates(k_n_v49__v50)*Y(jn)*Y(jv49)*state % rho - &
      screened_rates(k_n_v50__v51)*Y(jn)*Y(jv50)*state % rho + 0.5e0_rt* &
      screened_rates(k_o16_o16__n_s31)*Y(jo16)**2*state % rho + &
      screened_rates(k_p_al28__n_si28)*Y(jal28)*Y(jp)*state % rho + &
      screened_rates(k_p_al29__n_si29)*Y(jal29)*Y(jp)*state % rho + &
      screened_rates(k_p_c13__n_n13)*Y(jc13)*Y(jp)*state % rho + &
      screened_rates(k_p_f19__n_ne19)*Y(jf19)*Y(jp)*state % rho + &
      screened_rates(k_p_f20__n_ne20)*Y(jf20)*Y(jp)*state % rho + &
      screened_rates(k_p_f21__n_ne21)*Y(jf21)*Y(jp)*state % rho + &
      screened_rates(k_p_k40__n_ca40)*Y(jk40)*Y(jp)*state % rho + &
      screened_rates(k_p_k41__n_ca41)*Y(jk41)*Y(jp)*state % rho + &
      screened_rates(k_p_k42__n_ca42)*Y(jk42)*Y(jp)*state % rho + &
      screened_rates(k_p_mg27__n_al27)*Y(jmg27)*Y(jp)*state % rho + &
      screened_rates(k_p_na23__n_mg23)*Y(jna23)*Y(jp)*state % rho + &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*Y(jp)*state % rho + &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*Y(jp)*state % rho + &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*Y(jp)*state % rho + &
      screened_rates(k_p_o19__n_f19)*Y(jo19)*Y(jp)*state % rho + &
      screened_rates(k_p_p32__n_s32)*Y(jp32)*Y(jp)*state % rho + &
      screened_rates(k_p_sc46__n_ti46)*Y(jp)*Y(jsc46)*state % rho + &
      screened_rates(k_p_si31__n_p31)*Y(jp)*Y(jsi31)*state % rho + &
      screened_rates(k_p_v50__n_cr50)*Y(jp)*Y(jv50)*state % rho + &
      screened_rates(k_t_he3__n_p_he4)*Y(jhe3)*Y(jt)*state % rho + &
      screened_rates(k_t_t__n_n_he4)*Y(jt)**2*state % rho &
       )

    ydot_nuc(jp) = ( &
      0.5e0_rt*screened_rates(k_c12_c12__p_na23)*Y(jc12)**2*state % rho + &
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*Y(jne20)*state % rho + &
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*Y(jo16)*state % rho + 0.5e0_rt* &
      screened_rates(k_d_d__p_t)*Y(jd)**2*state % rho + screened_rates(k_d_he3__p_he4)* &
      Y(jd)*Y(jhe3)*state % rho + screened_rates(k_he3_he3__p_p_he4)*Y(jhe3) &
      **2*state % rho + screened_rates(k_he4_al25__p_si28)*Y(jal25)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_al26__p_si29)*Y(jal26)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_al27__p_si30)*Y(jal27)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_al28__p_si31)*Y(jal28)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_al29__p_si32)*Y(jal29)*Y(jhe4)* &
      state % rho + screened_rates(k_he4_ar35__p_k38)*Y(jar35)*Y(jhe4)*state % rho &
      + screened_rates(k_he4_cl33__p_ar36)*Y(jcl33)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cl34__p_ar37)*Y(jcl34)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cl35__p_ar38)*Y(jcl35)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_co53__p_ni56)*Y(jco53)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_co54__p_ni57)*Y(jco54)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_co55__p_ni58)*Y(jco55)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_co56__p_ni59)*Y(jco56)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_co57__p_ni60)*Y(jco57)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cr47__p_mn50)*Y(jcr47)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cr48__p_mn51)*Y(jcr48)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cr49__p_mn52)*Y(jcr49)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_f18__p_ne21)*Y(jf18)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_f20__p_ne23)*Y(jf20)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_f21__p_ne24)*Y(jf21)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe51__p_co54)*Y(jfe51)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe52__p_co55)*Y(jfe52)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe53__p_co56)*Y(jfe53)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*Y(jk37)*state % rho + &
      screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*Y(jk38)*state % rho + &
      screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*Y(jk40)*state % rho + &
      screened_rates(k_he4_k41__p_ca44)*Y(jhe4)*Y(jk41)*state % rho + &
      screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*Y(jmg22)*state % rho + &
      screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*Y(jmg23)*state % rho + &
      screened_rates(k_he4_mn49__p_fe52)*Y(jhe4)*Y(jmn49)*state % rho + &
      screened_rates(k_he4_mn50__p_fe53)*Y(jhe4)*Y(jmn50)*state % rho + &
      screened_rates(k_he4_mn51__p_fe54)*Y(jhe4)*Y(jmn51)*state % rho + &
      screened_rates(k_he4_mn52__p_fe55)*Y(jhe4)*Y(jmn52)*state % rho + &
      screened_rates(k_he4_mn53__p_fe56)*Y(jhe4)*Y(jmn53)*state % rho + &
      screened_rates(k_he4_n13__p_o16)*Y(jhe4)*Y(jn13)*state % rho + &
      screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*Y(jna20)*state % rho + &
      screened_rates(k_he4_na22__p_mg25)*Y(jhe4)*Y(jna22)*state % rho + &
      screened_rates(k_he4_na23__p_mg26)*Y(jhe4)*Y(jna23)*state % rho + &
      screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*Y(jna24)*state % rho + &
      screened_rates(k_he4_ne19__p_na22)*Y(jhe4)*Y(jne19)*state % rho + &
      screened_rates(k_he4_p29__p_s32)*Y(jhe4)*Y(jp29)*state % rho + &
      screened_rates(k_he4_p30__p_s33)*Y(jhe4)*Y(jp30)*state % rho + &
      screened_rates(k_he4_p31__p_s34)*Y(jhe4)*Y(jp31)*state % rho + &
      screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*Y(js30)*state % rho + &
      screened_rates(k_he4_s31__p_cl34)*Y(jhe4)*Y(js31)*state % rho + &
      screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*Y(js34)*state % rho + &
      screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*Y(jsc42)*state % rho + &
      screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*Y(jsc43)*state % rho + &
      screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*Y(jsc44)*state % rho + &
      screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*Y(jsc45)*state % rho + &
      screened_rates(k_he4_sc46__p_ti49)*Y(jhe4)*Y(jsc46)*state % rho + &
      screened_rates(k_he4_si27__p_p30)*Y(jhe4)*Y(jsi27)*state % rho + &
      screened_rates(k_he4_ti43__p_v46)*Y(jhe4)*Y(jti43)*state % rho + &
      screened_rates(k_he4_ti44__p_v47)*Y(jhe4)*Y(jti44)*state % rho + &
      screened_rates(k_he4_ti45__p_v48)*Y(jhe4)*Y(jti45)*state % rho + &
      screened_rates(k_he4_v46__p_cr49)*Y(jhe4)*Y(jv46)*state % rho + &
      screened_rates(k_he4_v47__p_cr50)*Y(jhe4)*Y(jv47)*state % rho + &
      screened_rates(k_he4_v48__p_cr51)*Y(jhe4)*Y(jv48)*state % rho + &
      screened_rates(k_he4_v49__p_cr52)*Y(jhe4)*Y(jv49)*state % rho + &
      screened_rates(k_n__p__weak__wc12)*Y(jn) + screened_rates(k_n_al25__p_mg25)* &
      Y(jal25)*Y(jn)*state % rho + screened_rates(k_n_al26__p_mg26)*Y(jal26)* &
      Y(jn)*state % rho + screened_rates(k_n_ar35__p_cl35)*Y(jar35)*Y(jn)* &
      state % rho + screened_rates(k_n_ar36__p_cl36)*Y(jar36)*Y(jn)*state % rho + &
      screened_rates(k_n_ar37__p_cl37)*Y(jar37)*Y(jn)*state % rho + &
      screened_rates(k_n_ca39__p_k39)*Y(jca39)*Y(jn)*state % rho + &
      screened_rates(k_n_cl33__p_s33)*Y(jcl33)*Y(jn)*state % rho + &
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*Y(jn)*state % rho + &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*Y(jn)*state % rho + &
      screened_rates(k_n_co53__p_fe53)*Y(jco53)*Y(jn)*state % rho + &
      screened_rates(k_n_co54__p_fe54)*Y(jco54)*Y(jn)*state % rho + &
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*Y(jn)*state % rho + &
      screened_rates(k_n_co56__p_fe56)*Y(jco56)*Y(jn)*state % rho + &
      screened_rates(k_n_cr47__p_v47)*Y(jcr47)*Y(jn)*state % rho + &
      screened_rates(k_n_cr48__p_v48)*Y(jcr48)*Y(jn)*state % rho + &
      screened_rates(k_n_cr49__p_v49)*Y(jcr49)*Y(jn)*state % rho + &
      screened_rates(k_n_cr51__p_v51)*Y(jcr51)*Y(jn)*state % rho + &
      screened_rates(k_n_f18__p_o18)*Y(jf18)*Y(jn)*state % rho + &
      screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*Y(jn)*state % rho + &
      screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*Y(jn)*state % rho + &
      screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*Y(jn)*state % rho + &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*Y(jn)*state % rho + &
      screened_rates(k_n_fe55__p_mn55)*Y(jfe55)*Y(jn)*state % rho + &
      screened_rates(k_n_he3__p_t)*Y(jhe3)*Y(jn)*state % rho + &
      screened_rates(k_n_k37__p_ar37)*Y(jk37)*Y(jn)*state % rho + &
      screened_rates(k_n_k38__p_ar38)*Y(jk38)*Y(jn)*state % rho + &
      screened_rates(k_n_k39__p_ar39)*Y(jk39)*Y(jn)*state % rho + &
      screened_rates(k_n_mg22__p_na22)*Y(jmg22)*Y(jn)*state % rho + &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*Y(jn)*state % rho + &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*Y(jn)*state % rho + &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*Y(jn)*state % rho + &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*Y(jn)*state % rho + &
      screened_rates(k_n_n14__p_c14)*Y(jn14)*Y(jn)*state % rho + &
      screened_rates(k_n_na20__p_ne20)*Y(jn)*Y(jna20)*state % rho + &
      screened_rates(k_n_na21__p_ne21)*Y(jn)*Y(jna21)*state % rho + &
      screened_rates(k_n_na22__p_ne22)*Y(jn)*Y(jna22)*state % rho + &
      screened_rates(k_n_ni54__p_co54)*Y(jn)*Y(jni54)*state % rho + &
      screened_rates(k_n_ni55__p_co55)*Y(jn)*Y(jni55)*state % rho + &
      screened_rates(k_n_ni56__p_co56)*Y(jn)*Y(jni56)*state % rho + &
      screened_rates(k_n_ni57__p_co57)*Y(jn)*Y(jni57)*state % rho + &
      screened_rates(k_n_ni58__p_co58)*Y(jn)*Y(jni58)*state % rho + &
      screened_rates(k_n_p29__p_si29)*Y(jn)*Y(jp29)*state % rho + &
      screened_rates(k_n_p30__p_si30)*Y(jn)*Y(jp30)*state % rho + &
      screened_rates(k_n_p32__p_si32)*Y(jn)*Y(jp32)*state % rho - &
      screened_rates(k_n_p__d)*Y(jn)*Y(jp)*state % rho - 1.0e0_rt* &
      screened_rates(k_n_p_p__p)*Y(jn)*Y(jp)**2*state % rho**2 + &
      screened_rates(k_n_s30__p_p30)*Y(jn)*Y(js30)*state % rho + &
      screened_rates(k_n_s31__p_p31)*Y(jn)*Y(js31)*state % rho + &
      screened_rates(k_n_s33__p_p33)*Y(jn)*Y(js33)*state % rho + &
      screened_rates(k_n_sc42__p_ca42)*Y(jn)*Y(jsc42)*state % rho + &
      screened_rates(k_n_sc43__p_ca43)*Y(jn)*Y(jsc43)*state % rho + &
      screened_rates(k_n_sc44__p_ca44)*Y(jn)*Y(jsc44)*state % rho + &
      screened_rates(k_n_si27__p_al27)*Y(jn)*Y(jsi27)*state % rho + &
      screened_rates(k_n_ti43__p_sc43)*Y(jn)*Y(jti43)*state % rho + &
      screened_rates(k_n_ti44__p_sc44)*Y(jn)*Y(jti44)*state % rho + &
      screened_rates(k_n_ti45__p_sc45)*Y(jn)*Y(jti45)*state % rho + &
      screened_rates(k_n_v46__p_ti46)*Y(jn)*Y(jv46)*state % rho + &
      screened_rates(k_n_v47__p_ti47)*Y(jn)*Y(jv47)*state % rho + &
      screened_rates(k_n_v48__p_ti48)*Y(jn)*Y(jv48)*state % rho + &
      screened_rates(k_n_v49__p_ti49)*Y(jn)*Y(jv49)*state % rho + 0.5e0_rt* &
      screened_rates(k_o16_o16__p_p31)*Y(jo16)**2*state % rho - &
      screened_rates(k_p_al26__si27)*Y(jal26)*Y(jp)*state % rho - &
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*Y(jp)*state % rho - &
      screened_rates(k_p_al27__si28)*Y(jal27)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__n_si28)*Y(jal28)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__si29)*Y(jal28)*Y(jp)*state % rho - &
      screened_rates(k_p_al29__he4_mg26)*Y(jal29)*Y(jp)*state % rho - &
      screened_rates(k_p_al29__n_si29)*Y(jal29)*Y(jp)*state % rho - &
      screened_rates(k_p_al29__si30)*Y(jal29)*Y(jp)*state % rho - &
      screened_rates(k_p_ar36__k37)*Y(jar36)*Y(jp)*state % rho - &
      screened_rates(k_p_ar37__k38)*Y(jar37)*Y(jp)*state % rho - &
      screened_rates(k_p_ar38__k39)*Y(jar38)*Y(jp)*state % rho - &
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*Y(jp)*state % rho - &
      screened_rates(k_p_ar39__k40)*Y(jar39)*Y(jp)*state % rho - &
      screened_rates(k_p_c12__n13)*Y(jc12)*Y(jp)*state % rho - &
      screened_rates(k_p_c13__n14)*Y(jc13)*Y(jp)*state % rho - &
      screened_rates(k_p_c13__n_n13)*Y(jc13)*Y(jp)*state % rho - &
      screened_rates(k_p_c14__n15)*Y(jc14)*Y(jp)*state % rho - &
      screened_rates(k_p_ca41__sc42)*Y(jca41)*Y(jp)*state % rho - &
      screened_rates(k_p_ca42__he4_k39)*Y(jca42)*Y(jp)*state % rho - &
      screened_rates(k_p_ca42__sc43)*Y(jca42)*Y(jp)*state % rho - &
      screened_rates(k_p_ca43__sc44)*Y(jca43)*Y(jp)*state % rho - &
      screened_rates(k_p_ca44__sc45)*Y(jca44)*Y(jp)*state % rho - &
      screened_rates(k_p_cl34__ar35)*Y(jcl34)*Y(jp)*state % rho - &
      screened_rates(k_p_cl35__ar36)*Y(jcl35)*Y(jp)*state % rho - &
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*Y(jp)*state % rho - &
      screened_rates(k_p_cl36__ar37)*Y(jcl36)*Y(jp)*state % rho - &
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*Y(jp)*state % rho - &
      screened_rates(k_p_cl37__ar38)*Y(jcl37)*Y(jp)*state % rho - &
      screened_rates(k_p_co53__ni54)*Y(jco53)*Y(jp)*state % rho - &
      screened_rates(k_p_co54__ni55)*Y(jco54)*Y(jp)*state % rho - &
      screened_rates(k_p_co55__ni56)*Y(jco55)*Y(jp)*state % rho - &
      screened_rates(k_p_co56__ni57)*Y(jco56)*Y(jp)*state % rho - &
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*Y(jp)*state % rho - &
      screened_rates(k_p_co57__ni58)*Y(jco57)*Y(jp)*state % rho - &
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*Y(jp)*state % rho - &
      screened_rates(k_p_co58__ni59)*Y(jco58)*Y(jp)*state % rho - &
      screened_rates(k_p_cr48__mn49)*Y(jcr48)*Y(jp)*state % rho - &
      screened_rates(k_p_cr49__mn50)*Y(jcr49)*Y(jp)*state % rho - &
      screened_rates(k_p_cr50__mn51)*Y(jcr50)*Y(jp)*state % rho - &
      screened_rates(k_p_cr51__mn52)*Y(jcr51)*Y(jp)*state % rho - &
      screened_rates(k_p_cr52__mn53)*Y(jcr52)*Y(jp)*state % rho - &
      screened_rates(k_p_d__he3)*Y(jd)*Y(jp)*state % rho - screened_rates(k_p_f18__ne19) &
      *Y(jf18)*Y(jp)*state % rho - screened_rates(k_p_f19__he4_o16)*Y(jf19)* &
      Y(jp)*state % rho - screened_rates(k_p_f19__n_ne19)*Y(jf19)*Y(jp)* &
      state % rho - screened_rates(k_p_f19__ne20)*Y(jf19)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__he4_o17)*Y(jf20)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__n_ne20)*Y(jf20)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__ne21)*Y(jf20)*Y(jp)*state % rho - &
      screened_rates(k_p_f21__he4_o18)*Y(jf21)*Y(jp)*state % rho - &
      screened_rates(k_p_f21__n_ne21)*Y(jf21)*Y(jp)*state % rho - &
      screened_rates(k_p_f21__ne22)*Y(jf21)*Y(jp)*state % rho - &
      screened_rates(k_p_fe52__co53)*Y(jfe52)*Y(jp)*state % rho - &
      screened_rates(k_p_fe53__co54)*Y(jfe53)*Y(jp)*state % rho - &
      screened_rates(k_p_fe54__co55)*Y(jfe54)*Y(jp)*state % rho - &
      screened_rates(k_p_fe55__co56)*Y(jfe55)*Y(jp)*state % rho - &
      screened_rates(k_p_fe56__co57)*Y(jfe56)*Y(jp)*state % rho - &
      screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jhe3)*Y(jp)*state % rho - &
      screened_rates(k_p_k38__ca39)*Y(jk38)*Y(jp)*state % rho - &
      screened_rates(k_p_k39__ca40)*Y(jk39)*Y(jp)*state % rho - &
      screened_rates(k_p_k39__he4_ar36)*Y(jk39)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__ca41)*Y(jk40)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__he4_ar37)*Y(jk40)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__n_ca40)*Y(jk40)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__ca42)*Y(jk41)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__he4_ar38)*Y(jk41)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__n_ca41)*Y(jk41)*Y(jp)*state % rho - &
      screened_rates(k_p_k42__ca43)*Y(jk42)*Y(jp)*state % rho - &
      screened_rates(k_p_k42__he4_ar39)*Y(jk42)*Y(jp)*state % rho - &
      screened_rates(k_p_k42__n_ca42)*Y(jk42)*Y(jp)*state % rho - &
      screened_rates(k_p_mg24__al25)*Y(jmg24)*Y(jp)*state % rho - &
      screened_rates(k_p_mg24__he4_na21)*Y(jmg24)*Y(jp)*state % rho - &
      screened_rates(k_p_mg25__al26)*Y(jmg25)*Y(jp)*state % rho - &
      screened_rates(k_p_mg26__al27)*Y(jmg26)*Y(jp)*state % rho - &
      screened_rates(k_p_mg27__al28)*Y(jmg27)*Y(jp)*state % rho - &
      screened_rates(k_p_mg27__n_al27)*Y(jmg27)*Y(jp)*state % rho - &
      screened_rates(k_p_mn50__fe51)*Y(jmn50)*Y(jp)*state % rho - &
      screened_rates(k_p_mn51__fe52)*Y(jmn51)*Y(jp)*state % rho - &
      screened_rates(k_p_mn52__fe53)*Y(jmn52)*Y(jp)*state % rho - &
      screened_rates(k_p_mn53__fe54)*Y(jmn53)*Y(jp)*state % rho - &
      screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)*Y(jp)*state % rho - &
      screened_rates(k_p_mn54__fe55)*Y(jmn54)*Y(jp)*state % rho - &
      screened_rates(k_p_mn54__he4_cr51)*Y(jmn54)*Y(jp)*state % rho - &
      screened_rates(k_p_mn55__fe56)*Y(jmn55)*Y(jp)*state % rho - &
      screened_rates(k_p_mn55__he4_cr52)*Y(jmn55)*Y(jp)*state % rho - &
      screened_rates(k_p_n15__he4_c12)*Y(jn15)*Y(jp)*state % rho - &
      screened_rates(k_p_n15__o16)*Y(jn15)*Y(jp)*state % rho - &
      screened_rates(k_p_na21__mg22)*Y(jna21)*Y(jp)*state % rho - &
      screened_rates(k_p_na22__mg23)*Y(jna22)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__he4_ne20)*Y(jna23)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__mg24)*Y(jna23)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__n_mg23)*Y(jna23)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__he4_ne21)*Y(jna24)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__mg25)*Y(jna24)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*Y(jp)*state % rho - &
      screened_rates(k_p_ne19__na20)*Y(jne19)*Y(jp)*state % rho - &
      screened_rates(k_p_ne20__na21)*Y(jne20)*Y(jp)*state % rho - &
      screened_rates(k_p_ne21__na22)*Y(jne21)*Y(jp)*state % rho - &
      screened_rates(k_p_ne22__na23)*Y(jne22)*Y(jp)*state % rho - &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*Y(jp)*state % rho - &
      screened_rates(k_p_ne23__na24)*Y(jne23)*Y(jp)*state % rho - &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*Y(jp)*state % rho - &
      screened_rates(k_p_o17__f18)*Y(jo17)*Y(jp)*state % rho - &
      screened_rates(k_p_o17__he4_n14)*Y(jo17)*Y(jp)*state % rho - &
      screened_rates(k_p_o18__f19)*Y(jo18)*Y(jp)*state % rho - &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*Y(jp)*state % rho - &
      screened_rates(k_p_o19__f20)*Y(jo19)*Y(jp)*state % rho - &
      screened_rates(k_p_o19__n_f19)*Y(jo19)*Y(jp)*state % rho - &
      screened_rates(k_p_p29__s30)*Y(jp29)*Y(jp)*state % rho - &
      screened_rates(k_p_p30__s31)*Y(jp30)*Y(jp)*state % rho - &
      screened_rates(k_p_p31__he4_si28)*Y(jp31)*Y(jp)*state % rho - &
      screened_rates(k_p_p31__s32)*Y(jp31)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__he4_si29)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__n_s32)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__s33)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_p33__he4_si30)*Y(jp33)*Y(jp)*state % rho - &
      screened_rates(k_p_p33__s34)*Y(jp33)*Y(jp)*state % rho - &
      screened_rates(k_p_p__d__weak__bet_pos_)*Y(jp)**2*state % rho - &
      screened_rates(k_p_p__d__weak__electron_capture)*Y(jp)**2*state % rho**2* &
      state % y_e - screened_rates(k_p_s32__cl33)*Y(jp)*Y(js32)*state % rho - &
      screened_rates(k_p_s33__cl34)*Y(jp)*Y(js33)*state % rho - &
      screened_rates(k_p_s34__cl35)*Y(jp)*Y(js34)*state % rho - &
      screened_rates(k_p_s35__cl36)*Y(jp)*Y(js35)*state % rho - &
      screened_rates(k_p_s35__he4_p32)*Y(jp)*Y(js35)*state % rho - &
      screened_rates(k_p_sc42__he4_ca39)*Y(jp)*Y(jsc42)*state % rho - &
      screened_rates(k_p_sc42__ti43)*Y(jp)*Y(jsc42)*state % rho - &
      screened_rates(k_p_sc43__he4_ca40)*Y(jp)*Y(jsc43)*state % rho - &
      screened_rates(k_p_sc43__ti44)*Y(jp)*Y(jsc43)*state % rho - &
      screened_rates(k_p_sc44__he4_ca41)*Y(jp)*Y(jsc44)*state % rho - &
      screened_rates(k_p_sc44__ti45)*Y(jp)*Y(jsc44)*state % rho - &
      screened_rates(k_p_sc45__he4_ca42)*Y(jp)*Y(jsc45)*state % rho - &
      screened_rates(k_p_sc45__ti46)*Y(jp)*Y(jsc45)*state % rho - &
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_sc46__n_ti46)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_sc46__ti47)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_si28__p29)*Y(jp)*Y(jsi28)*state % rho - &
      screened_rates(k_p_si29__p30)*Y(jp)*Y(jsi29)*state % rho - &
      screened_rates(k_p_si30__p31)*Y(jp)*Y(jsi30)*state % rho - &
      screened_rates(k_p_si31__n_p31)*Y(jp)*Y(jsi31)*state % rho - &
      screened_rates(k_p_si31__p32)*Y(jp)*Y(jsi31)*state % rho - &
      screened_rates(k_p_si32__p33)*Y(jp)*Y(jsi32)*state % rho - &
      screened_rates(k_p_t__he4)*Y(jp)*Y(jt)*state % rho - screened_rates(k_p_ti45__v46) &
      *Y(jp)*Y(jti45)*state % rho - screened_rates(k_p_ti46__v47)*Y(jp)* &
      Y(jti46)*state % rho - screened_rates(k_p_ti47__v48)*Y(jp)*Y(jti47)* &
      state % rho - screened_rates(k_p_ti48__v49)*Y(jp)*Y(jti48)*state % rho - &
      screened_rates(k_p_ti49__v50)*Y(jp)*Y(jti49)*state % rho - &
      screened_rates(k_p_v46__cr47)*Y(jp)*Y(jv46)*state % rho - &
      screened_rates(k_p_v47__cr48)*Y(jp)*Y(jv47)*state % rho - &
      screened_rates(k_p_v48__cr49)*Y(jp)*Y(jv48)*state % rho - &
      screened_rates(k_p_v49__cr50)*Y(jp)*Y(jv49)*state % rho - &
      screened_rates(k_p_v49__he4_ti46)*Y(jp)*Y(jv49)*state % rho - &
      screened_rates(k_p_v50__cr51)*Y(jp)*Y(jv50)*state % rho - &
      screened_rates(k_p_v50__he4_ti47)*Y(jp)*Y(jv50)*state % rho - &
      screened_rates(k_p_v50__n_cr50)*Y(jp)*Y(jv50)*state % rho - &
      screened_rates(k_p_v51__cr52)*Y(jp)*Y(jv51)*state % rho - &
      screened_rates(k_p_v51__he4_ti48)*Y(jp)*Y(jv51)*state % rho + &
      screened_rates(k_t_he3__n_p_he4)*Y(jhe3)*Y(jt)*state % rho &
       )

    ydot_nuc(jd) = ( &
      -screened_rates(k_d_c13__n_n14)*Y(jc13)*Y(jd)*state % rho - &
      screened_rates(k_d_c14__n_n15)*Y(jc14)*Y(jd)*state % rho - &
      screened_rates(k_d_d__he4)*Y(jd)**2*state % rho - screened_rates(k_d_d__n_he3)* &
      Y(jd)**2*state % rho - screened_rates(k_d_d__p_t)*Y(jd)**2*state % rho - &
      screened_rates(k_d_he3__p_he4)*Y(jd)*Y(jhe3)*state % rho - &
      screened_rates(k_d_t__n_he4)*Y(jd)*Y(jt)*state % rho - screened_rates(k_n_d__t)* &
      Y(jd)*Y(jn)*state % rho + screened_rates(k_n_p__d)*Y(jn)*Y(jp)* &
      state % rho - screened_rates(k_p_d__he3)*Y(jd)*Y(jp)*state % rho + 0.5e0_rt* &
      screened_rates(k_p_p__d__weak__bet_pos_)*Y(jp)**2*state % rho + 0.5e0_rt* &
      screened_rates(k_p_p__d__weak__electron_capture)*Y(jp)**2*state % rho**2* &
      state % y_e + screened_rates(k_t_he3__d_he4)*Y(jhe3)*Y(jt)*state % rho &
       )

    ydot_nuc(jt) = ( &
      0.5e0_rt*screened_rates(k_d_d__p_t)*Y(jd)**2*state % rho - screened_rates(k_d_t__n_he4)* &
      Y(jd)*Y(jt)*state % rho + &
      screened_rates(k_he3__t__weak__electron_capture)*Y(jhe3)*state % rho* &
      state % y_e + screened_rates(k_n_d__t)*Y(jd)*Y(jn)*state % rho + &
      screened_rates(k_n_he3__p_t)*Y(jhe3)*Y(jn)*state % rho - &
      screened_rates(k_p_t__he4)*Y(jp)*Y(jt)*state % rho - &
      screened_rates(k_t__he3__weak__wc12)*Y(jt) - screened_rates(k_t_he3__d_he4)* &
      Y(jhe3)*Y(jt)*state % rho - screened_rates(k_t_he3__n_p_he4)*Y(jhe3)* &
      Y(jt)*state % rho - screened_rates(k_t_t__n_n_he4)*Y(jt)**2*state % rho &
       )

    ydot_nuc(jhe3) = ( &
      0.5e0_rt*screened_rates(k_d_d__n_he3)*Y(jd)**2*state % rho - screened_rates(k_d_he3__p_he4)* &
      Y(jd)*Y(jhe3)*state % rho - &
      screened_rates(k_he3__t__weak__electron_capture)*Y(jhe3)*state % rho* &
      state % y_e - screened_rates(k_he3_he3__p_p_he4)*Y(jhe3)**2*state % rho - &
      screened_rates(k_n_he3__he4)*Y(jhe3)*Y(jn)*state % rho - &
      screened_rates(k_n_he3__p_t)*Y(jhe3)*Y(jn)*state % rho + &
      screened_rates(k_p_d__he3)*Y(jd)*Y(jp)*state % rho - &
      screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jhe3)*Y(jp)*state % rho + &
      screened_rates(k_t__he3__weak__wc12)*Y(jt) - screened_rates(k_t_he3__d_he4)* &
      Y(jhe3)*Y(jt)*state % rho - screened_rates(k_t_he3__n_p_he4)*Y(jhe3)* &
      Y(jt)*state % rho &
       )

    ydot_nuc(jhe4) = ( &
      0.5e0_rt*screened_rates(k_c12_c12__he4_ne20)*Y(jc12)**2*state % rho + &
      screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*Y(jne20)*state % rho + &
      screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*Y(jo16)*state % rho + 0.5e0_rt* &
      screened_rates(k_d_d__he4)*Y(jd)**2*state % rho + screened_rates(k_d_he3__p_he4)* &
      Y(jd)*Y(jhe3)*state % rho + screened_rates(k_d_t__n_he4)*Y(jd)*Y(jt)* &
      state % rho + 0.5e0_rt*screened_rates(k_he3_he3__p_p_he4)*Y(jhe3)**2*state % rho &
      - screened_rates(k_he4_al25__p29)*Y(jal25)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al25__p_si28)*Y(jal25)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al26__p30)*Y(jal26)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al26__p_si29)*Y(jal26)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al27__n_p30)*Y(jal27)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al27__p31)*Y(jal27)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al27__p_si30)*Y(jal27)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al28__n_p31)*Y(jal28)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al28__p32)*Y(jal28)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al28__p_si31)*Y(jal28)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al29__n_p32)*Y(jal29)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al29__p33)*Y(jal29)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al29__p_si32)*Y(jal29)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ar35__ca39)*Y(jar35)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ar35__p_k38)*Y(jar35)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ar36__ca40)*Y(jar36)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ar37__ca41)*Y(jar37)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ar38__ca42)*Y(jar38)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ar39__ca43)*Y(jar39)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_c12__o16)*Y(jc12)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_c13__n_o16)*Y(jc13)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_c14__o18)*Y(jc14)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca39__ti43)*Y(jca39)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca40__ti44)*Y(jca40)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca41__ti45)*Y(jca41)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca42__ti46)*Y(jca42)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca43__n_ti46)*Y(jca43)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca43__ti47)*Y(jca43)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca44__ti48)*Y(jca44)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl33__k37)*Y(jcl33)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl33__p_ar36)*Y(jcl33)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl34__k38)*Y(jcl34)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl34__p_ar37)*Y(jcl34)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl35__k39)*Y(jcl35)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl35__p_ar38)*Y(jcl35)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl36__k40)*Y(jcl36)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cl37__k41)*Y(jcl37)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_co53__p_ni56)*Y(jco53)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_co54__p_ni57)*Y(jco54)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_co55__p_ni58)*Y(jco55)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_co56__p_ni59)*Y(jco56)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_co57__p_ni60)*Y(jco57)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr47__fe51)*Y(jcr47)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr47__p_mn50)*Y(jcr47)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr48__fe52)*Y(jcr48)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr48__p_mn51)*Y(jcr48)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr49__fe53)*Y(jcr49)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr49__p_mn52)*Y(jcr49)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr50__fe54)*Y(jcr50)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr51__fe55)*Y(jcr51)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_cr52__fe56)*Y(jcr52)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f18__na22)*Y(jf18)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f18__p_ne21)*Y(jf18)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f19__na23)*Y(jf19)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f20__n_na23)*Y(jf20)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f20__na24)*Y(jf20)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f20__p_ne23)*Y(jf20)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f21__n_na24)*Y(jf21)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f21__p_ne24)*Y(jf21)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe51__ni55)*Y(jfe51)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe51__p_co54)*Y(jfe51)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe52__ni56)*Y(jfe52)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe52__p_co55)*Y(jfe52)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe53__ni57)*Y(jfe53)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe53__p_co56)*Y(jfe53)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe54__ni58)*Y(jfe54)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe55__ni59)*Y(jfe55)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe56__ni60)*Y(jfe56)*Y(jhe4)*state % rho - 0.5e0_rt* &
      screened_rates(k_he4_he4_he4__c12)*Y(jhe4)**3*state % rho**2 - &
      screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*Y(jk37)*state % rho - &
      screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*Y(jk38)*state % rho - &
      screened_rates(k_he4_k38__sc42)*Y(jhe4)*Y(jk38)*state % rho - &
      screened_rates(k_he4_k39__sc43)*Y(jhe4)*Y(jk39)*state % rho - &
      screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*Y(jk40)*state % rho - &
      screened_rates(k_he4_k40__sc44)*Y(jhe4)*Y(jk40)*state % rho - &
      screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*Y(jk41)*state % rho - &
      screened_rates(k_he4_k41__p_ca44)*Y(jhe4)*Y(jk41)*state % rho - &
      screened_rates(k_he4_k41__sc45)*Y(jhe4)*Y(jk41)*state % rho - &
      screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*Y(jk42)*state % rho - &
      screened_rates(k_he4_k42__sc46)*Y(jhe4)*Y(jk42)*state % rho - &
      screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*Y(jmg22)*state % rho - &
      screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*Y(jmg23)*state % rho - &
      screened_rates(k_he4_mg23__si27)*Y(jhe4)*Y(jmg23)*state % rho - &
      screened_rates(k_he4_mg24__si28)*Y(jhe4)*Y(jmg24)*state % rho - &
      screened_rates(k_he4_mg25__n_si28)*Y(jhe4)*Y(jmg25)*state % rho - &
      screened_rates(k_he4_mg25__si29)*Y(jhe4)*Y(jmg25)*state % rho - &
      screened_rates(k_he4_mg26__n_si29)*Y(jhe4)*Y(jmg26)*state % rho - &
      screened_rates(k_he4_mg26__si30)*Y(jhe4)*Y(jmg26)*state % rho - &
      screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_mg27__si31)*Y(jhe4)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_mn49__co53)*Y(jhe4)*Y(jmn49)*state % rho - &
      screened_rates(k_he4_mn49__p_fe52)*Y(jhe4)*Y(jmn49)*state % rho - &
      screened_rates(k_he4_mn50__co54)*Y(jhe4)*Y(jmn50)*state % rho - &
      screened_rates(k_he4_mn50__p_fe53)*Y(jhe4)*Y(jmn50)*state % rho - &
      screened_rates(k_he4_mn51__co55)*Y(jhe4)*Y(jmn51)*state % rho - &
      screened_rates(k_he4_mn51__p_fe54)*Y(jhe4)*Y(jmn51)*state % rho - &
      screened_rates(k_he4_mn52__co56)*Y(jhe4)*Y(jmn52)*state % rho - &
      screened_rates(k_he4_mn52__p_fe55)*Y(jhe4)*Y(jmn52)*state % rho - &
      screened_rates(k_he4_mn53__co57)*Y(jhe4)*Y(jmn53)*state % rho - &
      screened_rates(k_he4_mn53__p_fe56)*Y(jhe4)*Y(jmn53)*state % rho - &
      screened_rates(k_he4_mn54__co58)*Y(jhe4)*Y(jmn54)*state % rho - &
      screened_rates(k_he4_n13__p_o16)*Y(jhe4)*Y(jn13)*state % rho - &
      screened_rates(k_he4_n14__f18)*Y(jhe4)*Y(jn14)*state % rho - &
      screened_rates(k_he4_n15__f19)*Y(jhe4)*Y(jn15)*state % rho - &
      screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*Y(jna20)*state % rho - &
      screened_rates(k_he4_na21__al25)*Y(jhe4)*Y(jna21)*state % rho - &
      screened_rates(k_he4_na22__al26)*Y(jhe4)*Y(jna22)*state % rho - &
      screened_rates(k_he4_na22__p_mg25)*Y(jhe4)*Y(jna22)*state % rho - &
      screened_rates(k_he4_na23__al27)*Y(jhe4)*Y(jna23)*state % rho - &
      screened_rates(k_he4_na23__p_mg26)*Y(jhe4)*Y(jna23)*state % rho - &
      screened_rates(k_he4_na24__al28)*Y(jhe4)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__n_al27)*Y(jhe4)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*Y(jna24)*state % rho - &
      screened_rates(k_he4_ne19__mg23)*Y(jhe4)*Y(jne19)*state % rho - &
      screened_rates(k_he4_ne19__p_na22)*Y(jhe4)*Y(jne19)*state % rho - &
      screened_rates(k_he4_ne20__mg24)*Y(jhe4)*Y(jne20)*state % rho - &
      screened_rates(k_he4_ne21__mg25)*Y(jhe4)*Y(jne21)*state % rho - &
      screened_rates(k_he4_ne21__n_mg24)*Y(jhe4)*Y(jne21)*state % rho - &
      screened_rates(k_he4_ne22__mg26)*Y(jhe4)*Y(jne22)*state % rho - &
      screened_rates(k_he4_ne22__n_mg25)*Y(jhe4)*Y(jne22)*state % rho - &
      screened_rates(k_he4_ne23__mg27)*Y(jhe4)*Y(jne23)*state % rho - &
      screened_rates(k_he4_ne23__n_mg26)*Y(jhe4)*Y(jne23)*state % rho - &
      screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*Y(jne24)*state % rho - &
      screened_rates(k_he4_o16__ne20)*Y(jhe4)*Y(jo16)*state % rho - &
      screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*Y(jo17)*state % rho - &
      screened_rates(k_he4_o17__ne21)*Y(jhe4)*Y(jo17)*state % rho - &
      screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*Y(jo18)*state % rho - &
      screened_rates(k_he4_o18__ne22)*Y(jhe4)*Y(jo18)*state % rho - &
      screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*Y(jo19)*state % rho - &
      screened_rates(k_he4_o19__ne23)*Y(jhe4)*Y(jo19)*state % rho - &
      screened_rates(k_he4_p29__cl33)*Y(jhe4)*Y(jp29)*state % rho - &
      screened_rates(k_he4_p29__p_s32)*Y(jhe4)*Y(jp29)*state % rho - &
      screened_rates(k_he4_p30__cl34)*Y(jhe4)*Y(jp30)*state % rho - &
      screened_rates(k_he4_p30__p_s33)*Y(jhe4)*Y(jp30)*state % rho - &
      screened_rates(k_he4_p31__cl35)*Y(jhe4)*Y(jp31)*state % rho - &
      screened_rates(k_he4_p31__p_s34)*Y(jhe4)*Y(jp31)*state % rho - &
      screened_rates(k_he4_p32__cl36)*Y(jhe4)*Y(jp32)*state % rho - &
      screened_rates(k_he4_p33__cl37)*Y(jhe4)*Y(jp33)*state % rho - &
      screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*Y(js30)*state % rho - &
      screened_rates(k_he4_s31__ar35)*Y(jhe4)*Y(js31)*state % rho - &
      screened_rates(k_he4_s31__p_cl34)*Y(jhe4)*Y(js31)*state % rho - &
      screened_rates(k_he4_s32__ar36)*Y(jhe4)*Y(js32)*state % rho - &
      screened_rates(k_he4_s33__ar37)*Y(jhe4)*Y(js33)*state % rho - &
      screened_rates(k_he4_s34__ar38)*Y(jhe4)*Y(js34)*state % rho - &
      screened_rates(k_he4_s34__n_ar37)*Y(jhe4)*Y(js34)*state % rho - &
      screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*Y(js34)*state % rho - &
      screened_rates(k_he4_s35__ar39)*Y(jhe4)*Y(js35)*state % rho - &
      screened_rates(k_he4_s35__n_ar38)*Y(jhe4)*Y(js35)*state % rho - &
      screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*Y(jsc42)*state % rho - &
      screened_rates(k_he4_sc42__v46)*Y(jhe4)*Y(jsc42)*state % rho - &
      screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*Y(jsc43)*state % rho - &
      screened_rates(k_he4_sc43__v47)*Y(jhe4)*Y(jsc43)*state % rho - &
      screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*Y(jsc44)*state % rho - &
      screened_rates(k_he4_sc44__v48)*Y(jhe4)*Y(jsc44)*state % rho - &
      screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*Y(jsc45)*state % rho - &
      screened_rates(k_he4_sc45__v49)*Y(jhe4)*Y(jsc45)*state % rho - &
      screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_sc46__p_ti49)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_sc46__v50)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_si27__p_p30)*Y(jhe4)*Y(jsi27)*state % rho - &
      screened_rates(k_he4_si27__s31)*Y(jhe4)*Y(jsi27)*state % rho - &
      screened_rates(k_he4_si28__s32)*Y(jhe4)*Y(jsi28)*state % rho - &
      screened_rates(k_he4_si29__s33)*Y(jhe4)*Y(jsi29)*state % rho - &
      screened_rates(k_he4_si30__s34)*Y(jhe4)*Y(jsi30)*state % rho - &
      screened_rates(k_he4_si31__n_s34)*Y(jhe4)*Y(jsi31)*state % rho - &
      screened_rates(k_he4_si31__s35)*Y(jhe4)*Y(jsi31)*state % rho - &
      screened_rates(k_he4_ti43__cr47)*Y(jhe4)*Y(jti43)*state % rho - &
      screened_rates(k_he4_ti43__p_v46)*Y(jhe4)*Y(jti43)*state % rho - &
      screened_rates(k_he4_ti44__cr48)*Y(jhe4)*Y(jti44)*state % rho - &
      screened_rates(k_he4_ti44__p_v47)*Y(jhe4)*Y(jti44)*state % rho - &
      screened_rates(k_he4_ti45__cr49)*Y(jhe4)*Y(jti45)*state % rho - &
      screened_rates(k_he4_ti45__p_v48)*Y(jhe4)*Y(jti45)*state % rho - &
      screened_rates(k_he4_ti46__cr50)*Y(jhe4)*Y(jti46)*state % rho - &
      screened_rates(k_he4_ti47__cr51)*Y(jhe4)*Y(jti47)*state % rho - &
      screened_rates(k_he4_ti48__cr52)*Y(jhe4)*Y(jti48)*state % rho - &
      screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*Y(jti49)*state % rho - &
      screened_rates(k_he4_v46__mn50)*Y(jhe4)*Y(jv46)*state % rho - &
      screened_rates(k_he4_v46__p_cr49)*Y(jhe4)*Y(jv46)*state % rho - &
      screened_rates(k_he4_v47__mn51)*Y(jhe4)*Y(jv47)*state % rho - &
      screened_rates(k_he4_v47__p_cr50)*Y(jhe4)*Y(jv47)*state % rho - &
      screened_rates(k_he4_v48__mn52)*Y(jhe4)*Y(jv48)*state % rho - &
      screened_rates(k_he4_v48__p_cr51)*Y(jhe4)*Y(jv48)*state % rho - &
      screened_rates(k_he4_v49__mn53)*Y(jhe4)*Y(jv49)*state % rho - &
      screened_rates(k_he4_v49__p_cr52)*Y(jhe4)*Y(jv49)*state % rho - &
      screened_rates(k_he4_v50__mn54)*Y(jhe4)*Y(jv50)*state % rho - &
      screened_rates(k_he4_v51__mn55)*Y(jhe4)*Y(jv51)*state % rho + &
      screened_rates(k_n_al25__he4_na22)*Y(jal25)*Y(jn)*state % rho + &
      screened_rates(k_n_al26__he4_na23)*Y(jal26)*Y(jn)*state % rho + &
      screened_rates(k_n_ar35__he4_s32)*Y(jar35)*Y(jn)*state % rho + &
      screened_rates(k_n_ar36__he4_s33)*Y(jar36)*Y(jn)*state % rho + &
      screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*Y(jn)*state % rho + &
      screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*Y(jn)*state % rho + &
      screened_rates(k_n_ca41__he4_ar38)*Y(jca41)*Y(jn)*state % rho + &
      screened_rates(k_n_ca42__he4_ar39)*Y(jca42)*Y(jn)*state % rho + &
      screened_rates(k_n_cl33__he4_p30)*Y(jcl33)*Y(jn)*state % rho + &
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*Y(jn)*state % rho + &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*Y(jn)*state % rho + &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*Y(jn)*state % rho + &
      screened_rates(k_n_co53__he4_mn50)*Y(jco53)*Y(jn)*state % rho + &
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*Y(jn)*state % rho + &
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*Y(jn)*state % rho + &
      screened_rates(k_n_co56__he4_mn53)*Y(jco56)*Y(jn)*state % rho + &
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*Y(jn)*state % rho + &
      screened_rates(k_n_co58__he4_mn55)*Y(jco58)*Y(jn)*state % rho + &
      screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*Y(jn)*state % rho + &
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*Y(jn)*state % rho + &
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*Y(jn)*state % rho + &
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*Y(jn)*state % rho + &
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*Y(jn)*state % rho + &
      screened_rates(k_n_f18__he4_n15)*Y(jf18)*Y(jn)*state % rho + &
      screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*Y(jn)*state % rho + &
      screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*Y(jn)*state % rho + &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*Y(jn)*state % rho + &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*Y(jn)*state % rho + &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*Y(jn)*state % rho + &
      screened_rates(k_n_he3__he4)*Y(jhe3)*Y(jn)*state % rho + &
      screened_rates(k_n_k37__he4_cl34)*Y(jk37)*Y(jn)*state % rho + &
      screened_rates(k_n_k38__he4_cl35)*Y(jk38)*Y(jn)*state % rho + &
      screened_rates(k_n_k39__he4_cl36)*Y(jk39)*Y(jn)*state % rho + &
      screened_rates(k_n_k40__he4_cl37)*Y(jk40)*Y(jn)*state % rho + &
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*Y(jn)*state % rho + &
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*Y(jn)*state % rho + &
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*Y(jn)*state % rho + &
      screened_rates(k_n_mn50__he4_v47)*Y(jmn50)*Y(jn)*state % rho + &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*Y(jn)*state % rho + &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*Y(jn)*state % rho + &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*Y(jn)*state % rho + &
      screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*Y(jn)*state % rho + &
      screened_rates(k_n_na21__he4_f18)*Y(jn)*Y(jna21)*state % rho + &
      screened_rates(k_n_na22__he4_f19)*Y(jn)*Y(jna22)*state % rho + &
      screened_rates(k_n_ne19__he4_o16)*Y(jn)*Y(jne19)*state % rho + &
      screened_rates(k_n_ni54__he4_fe51)*Y(jn)*Y(jni54)*state % rho + &
      screened_rates(k_n_ni55__he4_fe52)*Y(jn)*Y(jni55)*state % rho + &
      screened_rates(k_n_ni56__he4_fe53)*Y(jn)*Y(jni56)*state % rho + &
      screened_rates(k_n_ni57__he4_fe54)*Y(jn)*Y(jni57)*state % rho + &
      screened_rates(k_n_ni58__he4_fe55)*Y(jn)*Y(jni58)*state % rho + &
      screened_rates(k_n_ni59__he4_fe56)*Y(jn)*Y(jni59)*state % rho + &
      screened_rates(k_n_o17__he4_c14)*Y(jn)*Y(jo17)*state % rho + &
      screened_rates(k_n_p29__he4_al26)*Y(jn)*Y(jp29)*state % rho + &
      screened_rates(k_n_s30__he4_si27)*Y(jn)*Y(js30)*state % rho + &
      screened_rates(k_n_s31__he4_si28)*Y(jn)*Y(js31)*state % rho + &
      screened_rates(k_n_s32__he4_si29)*Y(jn)*Y(js32)*state % rho + &
      screened_rates(k_n_s33__he4_si30)*Y(jn)*Y(js33)*state % rho + &
      screened_rates(k_n_s35__he4_si32)*Y(jn)*Y(js35)*state % rho + &
      screened_rates(k_n_sc42__he4_k39)*Y(jn)*Y(jsc42)*state % rho + &
      screened_rates(k_n_sc43__he4_k40)*Y(jn)*Y(jsc43)*state % rho + &
      screened_rates(k_n_si27__he4_mg24)*Y(jn)*Y(jsi27)*state % rho + &
      screened_rates(k_n_ti43__he4_ca40)*Y(jn)*Y(jti43)*state % rho + &
      screened_rates(k_n_ti44__he4_ca41)*Y(jn)*Y(jti44)*state % rho + &
      screened_rates(k_n_ti45__he4_ca42)*Y(jn)*Y(jti45)*state % rho + &
      screened_rates(k_n_ti47__he4_ca44)*Y(jn)*Y(jti47)*state % rho + &
      screened_rates(k_n_v46__he4_sc43)*Y(jn)*Y(jv46)*state % rho + &
      screened_rates(k_n_v47__he4_sc44)*Y(jn)*Y(jv47)*state % rho + &
      screened_rates(k_n_v48__he4_sc45)*Y(jn)*Y(jv48)*state % rho + &
      screened_rates(k_na20__he4_o16__weak__wc12)*Y(jna20) + 0.5e0_rt* &
      screened_rates(k_o16_o16__he4_si28)*Y(jo16)**2*state % rho + &
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*Y(jp)*state % rho + &
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*Y(jp)*state % rho + &
      screened_rates(k_p_al29__he4_mg26)*Y(jal29)*Y(jp)*state % rho + &
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*Y(jp)*state % rho + &
      screened_rates(k_p_ca42__he4_k39)*Y(jca42)*Y(jp)*state % rho + &
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*Y(jp)*state % rho + &
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*Y(jp)*state % rho + &
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*Y(jp)*state % rho + &
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*Y(jp)*state % rho + &
      screened_rates(k_p_f19__he4_o16)*Y(jf19)*Y(jp)*state % rho + &
      screened_rates(k_p_f20__he4_o17)*Y(jf20)*Y(jp)*state % rho + &
      screened_rates(k_p_f21__he4_o18)*Y(jf21)*Y(jp)*state % rho + &
      screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jhe3)*Y(jp)*state % rho + &
      screened_rates(k_p_k39__he4_ar36)*Y(jk39)*Y(jp)*state % rho + &
      screened_rates(k_p_k40__he4_ar37)*Y(jk40)*Y(jp)*state % rho + &
      screened_rates(k_p_k41__he4_ar38)*Y(jk41)*Y(jp)*state % rho + &
      screened_rates(k_p_k42__he4_ar39)*Y(jk42)*Y(jp)*state % rho + &
      screened_rates(k_p_mg24__he4_na21)*Y(jmg24)*Y(jp)*state % rho + &
      screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)*Y(jp)*state % rho + &
      screened_rates(k_p_mn54__he4_cr51)*Y(jmn54)*Y(jp)*state % rho + &
      screened_rates(k_p_mn55__he4_cr52)*Y(jmn55)*Y(jp)*state % rho + &
      screened_rates(k_p_n15__he4_c12)*Y(jn15)*Y(jp)*state % rho + &
      screened_rates(k_p_na23__he4_ne20)*Y(jna23)*Y(jp)*state % rho + &
      screened_rates(k_p_na24__he4_ne21)*Y(jna24)*Y(jp)*state % rho + &
      screened_rates(k_p_o17__he4_n14)*Y(jo17)*Y(jp)*state % rho + &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*Y(jp)*state % rho + &
      screened_rates(k_p_p31__he4_si28)*Y(jp31)*Y(jp)*state % rho + &
      screened_rates(k_p_p32__he4_si29)*Y(jp32)*Y(jp)*state % rho + &
      screened_rates(k_p_p33__he4_si30)*Y(jp33)*Y(jp)*state % rho + &
      screened_rates(k_p_s35__he4_p32)*Y(jp)*Y(js35)*state % rho + &
      screened_rates(k_p_sc42__he4_ca39)*Y(jp)*Y(jsc42)*state % rho + &
      screened_rates(k_p_sc43__he4_ca40)*Y(jp)*Y(jsc43)*state % rho + &
      screened_rates(k_p_sc44__he4_ca41)*Y(jp)*Y(jsc44)*state % rho + &
      screened_rates(k_p_sc45__he4_ca42)*Y(jp)*Y(jsc45)*state % rho + &
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*Y(jsc46)*state % rho + &
      screened_rates(k_p_t__he4)*Y(jp)*Y(jt)*state % rho + &
      screened_rates(k_p_v49__he4_ti46)*Y(jp)*Y(jv49)*state % rho + &
      screened_rates(k_p_v50__he4_ti47)*Y(jp)*Y(jv50)*state % rho + &
      screened_rates(k_p_v51__he4_ti48)*Y(jp)*Y(jv51)*state % rho + &
      screened_rates(k_t_he3__d_he4)*Y(jhe3)*Y(jt)*state % rho + &
      screened_rates(k_t_he3__n_p_he4)*Y(jhe3)*Y(jt)*state % rho + 0.5e0_rt* &
      screened_rates(k_t_t__n_n_he4)*Y(jt)**2*state % rho &
       )

    ydot_nuc(jc12) = ( &
      -screened_rates(k_c12_c12__he4_ne20)*Y(jc12)**2*state % rho - &
      screened_rates(k_c12_c12__p_na23)*Y(jc12)**2*state % rho - &
      screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*Y(jne20)*state % rho - &
      screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*Y(jo16)*state % rho - &
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*Y(jo16)*state % rho - &
      screened_rates(k_he4_c12__o16)*Y(jc12)*Y(jhe4)*state % rho + &
      0.16666666666666667e0_rt*screened_rates(k_he4_he4_he4__c12)*Y(jhe4)**3* &
      state % rho**2 - screened_rates(k_n_c12__c13)*Y(jc12)*Y(jn)*state % rho + &
      2.0e0_rt*screened_rates(k_n_mg23__c12_c12)*Y(jmg23)*Y(jn)*state % rho + &
      screened_rates(k_n_si27__c12_o16)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_p_c12__n13)*Y(jc12)*Y(jp)*state % rho + &
      screened_rates(k_p_n15__he4_c12)*Y(jn15)*Y(jp)*state % rho &
       )

    ydot_nuc(jc13) = ( &
      -screened_rates(k_d_c13__n_n14)*Y(jc13)*Y(jd)*state % rho - &
      screened_rates(k_he4_c13__n_o16)*Y(jc13)*Y(jhe4)*state % rho + &
      screened_rates(k_n13__c13__weak__wc12)*Y(jn13) + screened_rates(k_n_c12__c13)* &
      Y(jc12)*Y(jn)*state % rho - screened_rates(k_n_c13__c14)*Y(jc13)* &
      Y(jn)*state % rho - screened_rates(k_p_c13__n14)*Y(jc13)*Y(jp)*state % rho &
      - screened_rates(k_p_c13__n_n13)*Y(jc13)*Y(jp)*state % rho &
       )

    ydot_nuc(jc14) = ( &
      -screened_rates(k_c14__n14__weak__wc12)*Y(jc14) - screened_rates(k_d_c14__n_n15)* &
      Y(jc14)*Y(jd)*state % rho - screened_rates(k_he4_c14__o18)*Y(jc14)* &
      Y(jhe4)*state % rho + screened_rates(k_n_c13__c14)*Y(jc13)*Y(jn)* &
      state % rho + screened_rates(k_n_n14__p_c14)*Y(jn14)*Y(jn)*state % rho + &
      screened_rates(k_n_o17__he4_c14)*Y(jn)*Y(jo17)*state % rho - &
      screened_rates(k_p_c14__n15)*Y(jc14)*Y(jp)*state % rho &
       )

    ydot_nuc(jn13) = ( &
      -screened_rates(k_he4_n13__p_o16)*Y(jhe4)*Y(jn13)*state % rho - &
      screened_rates(k_n13__c13__weak__wc12)*Y(jn13) - screened_rates(k_n_n13__n14)* &
      Y(jn13)*Y(jn)*state % rho + screened_rates(k_p_c12__n13)*Y(jc12)* &
      Y(jp)*state % rho + screened_rates(k_p_c13__n_n13)*Y(jc13)*Y(jp)* &
      state % rho &
       )

    ydot_nuc(jn14) = ( &
      screened_rates(k_c14__n14__weak__wc12)*Y(jc14) + screened_rates(k_d_c13__n_n14)* &
      Y(jc13)*Y(jd)*state % rho - screened_rates(k_he4_n14__f18)*Y(jhe4)* &
      Y(jn14)*state % rho + screened_rates(k_n_n13__n14)*Y(jn13)*Y(jn)* &
      state % rho - screened_rates(k_n_n14__n15)*Y(jn14)*Y(jn)*state % rho - &
      screened_rates(k_n_n14__p_c14)*Y(jn14)*Y(jn)*state % rho + &
      screened_rates(k_p_c13__n14)*Y(jc13)*Y(jp)*state % rho + &
      screened_rates(k_p_o17__he4_n14)*Y(jo17)*Y(jp)*state % rho &
       )

    ydot_nuc(jn15) = ( &
      screened_rates(k_d_c14__n_n15)*Y(jc14)*Y(jd)*state % rho - screened_rates(k_he4_n15__f19) &
      *Y(jhe4)*Y(jn15)*state % rho + screened_rates(k_n_f18__he4_n15)*Y(jf18) &
      *Y(jn)*state % rho + screened_rates(k_n_n14__n15)*Y(jn14)*Y(jn)* &
      state % rho + screened_rates(k_p_c14__n15)*Y(jc14)*Y(jp)*state % rho - &
      screened_rates(k_p_n15__he4_c12)*Y(jn15)*Y(jp)*state % rho - &
      screened_rates(k_p_n15__o16)*Y(jn15)*Y(jp)*state % rho + &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*Y(jp)*state % rho &
       )

    ydot_nuc(jo16) = ( &
      -screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*Y(jo16)*state % rho - &
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*Y(jo16)*state % rho + &
      screened_rates(k_he4_c12__o16)*Y(jc12)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_c13__n_o16)*Y(jc13)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_n13__p_o16)*Y(jhe4)*Y(jn13)*state % rho - &
      screened_rates(k_he4_o16__ne20)*Y(jhe4)*Y(jo16)*state % rho + &
      screened_rates(k_n_ne19__he4_o16)*Y(jn)*Y(jne19)*state % rho - &
      screened_rates(k_n_o16__o17)*Y(jn)*Y(jo16)*state % rho + &
      screened_rates(k_n_si27__c12_o16)*Y(jn)*Y(jsi27)*state % rho + &
      screened_rates(k_na20__he4_o16__weak__wc12)*Y(jna20) - &
      screened_rates(k_o16_o16__he4_si28)*Y(jo16)**2*state % rho - &
      screened_rates(k_o16_o16__n_s31)*Y(jo16)**2*state % rho - &
      screened_rates(k_o16_o16__p_p31)*Y(jo16)**2*state % rho + &
      screened_rates(k_p_f19__he4_o16)*Y(jf19)*Y(jp)*state % rho + &
      screened_rates(k_p_n15__o16)*Y(jn15)*Y(jp)*state % rho &
       )

    ydot_nuc(jo17) = ( &
      -screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*Y(jo17)*state % rho - &
      screened_rates(k_he4_o17__ne21)*Y(jhe4)*Y(jo17)*state % rho + &
      screened_rates(k_n_o16__o17)*Y(jn)*Y(jo16)*state % rho - &
      screened_rates(k_n_o17__he4_c14)*Y(jn)*Y(jo17)*state % rho - &
      screened_rates(k_n_o17__o18)*Y(jn)*Y(jo17)*state % rho + &
      screened_rates(k_p_f20__he4_o17)*Y(jf20)*Y(jp)*state % rho - &
      screened_rates(k_p_o17__f18)*Y(jo17)*Y(jp)*state % rho - &
      screened_rates(k_p_o17__he4_n14)*Y(jo17)*Y(jp)*state % rho &
       )

    ydot_nuc(jo18) = ( &
      screened_rates(k_f18__o18__weak__wc12)*Y(jf18) + screened_rates(k_he4_c14__o18)* &
      Y(jc14)*Y(jhe4)*state % rho - screened_rates(k_he4_o18__n_ne21)*Y(jhe4) &
      *Y(jo18)*state % rho - screened_rates(k_he4_o18__ne22)*Y(jhe4)*Y(jo18)* &
      state % rho + screened_rates(k_n_f18__p_o18)*Y(jf18)*Y(jn)*state % rho + &
      screened_rates(k_n_o17__o18)*Y(jn)*Y(jo17)*state % rho - &
      screened_rates(k_n_o18__o19)*Y(jn)*Y(jo18)*state % rho + &
      screened_rates(k_p_f21__he4_o18)*Y(jf21)*Y(jp)*state % rho - &
      screened_rates(k_p_o18__f19)*Y(jo18)*Y(jp)*state % rho - &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*Y(jp)*state % rho &
       )

    ydot_nuc(jo19) = ( &
      -screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*Y(jo19)*state % rho - &
      screened_rates(k_he4_o19__ne23)*Y(jhe4)*Y(jo19)*state % rho + &
      screened_rates(k_n_o18__o19)*Y(jn)*Y(jo18)*state % rho - &
      screened_rates(k_o19__f19__weak__wc12)*Y(jo19) - screened_rates(k_p_o19__f20)* &
      Y(jo19)*Y(jp)*state % rho - screened_rates(k_p_o19__n_f19)*Y(jo19)* &
      Y(jp)*state % rho &
       )

    ydot_nuc(jf18) = ( &
      -screened_rates(k_f18__o18__weak__wc12)*Y(jf18) - screened_rates(k_he4_f18__na22)* &
      Y(jf18)*Y(jhe4)*state % rho - screened_rates(k_he4_f18__p_ne21)*Y(jf18) &
      *Y(jhe4)*state % rho + screened_rates(k_he4_n14__f18)*Y(jhe4)*Y(jn14)* &
      state % rho - screened_rates(k_n_f18__f19)*Y(jf18)*Y(jn)*state % rho - &
      screened_rates(k_n_f18__he4_n15)*Y(jf18)*Y(jn)*state % rho - &
      screened_rates(k_n_f18__p_o18)*Y(jf18)*Y(jn)*state % rho + &
      screened_rates(k_n_na21__he4_f18)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_p_f18__ne19)*Y(jf18)*Y(jp)*state % rho + &
      screened_rates(k_p_o17__f18)*Y(jo17)*Y(jp)*state % rho &
       )

    ydot_nuc(jf19) = ( &
      -screened_rates(k_he4_f19__na23)*Y(jf19)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_n15__f19)*Y(jhe4)*Y(jn15)*state % rho + &
      screened_rates(k_n_f18__f19)*Y(jf18)*Y(jn)*state % rho - &
      screened_rates(k_n_f19__f20)*Y(jf19)*Y(jn)*state % rho + &
      screened_rates(k_n_na22__he4_f19)*Y(jn)*Y(jna22)*state % rho + &
      screened_rates(k_ne19__f19__weak__wc12)*Y(jne19) + &
      screened_rates(k_o19__f19__weak__wc12)*Y(jo19) - screened_rates(k_p_f19__he4_o16) &
      *Y(jf19)*Y(jp)*state % rho - screened_rates(k_p_f19__n_ne19)*Y(jf19)* &
      Y(jp)*state % rho - screened_rates(k_p_f19__ne20)*Y(jf19)*Y(jp)* &
      state % rho + screened_rates(k_p_o18__f19)*Y(jo18)*Y(jp)*state % rho + &
      screened_rates(k_p_o19__n_f19)*Y(jo19)*Y(jp)*state % rho &
       )

    ydot_nuc(jf20) = ( &
      -screened_rates(k_f20__ne20__weak__wc12)*Y(jf20) - screened_rates(k_he4_f20__n_na23)* &
      Y(jf20)*Y(jhe4)*state % rho - screened_rates(k_he4_f20__na24)*Y(jf20)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_f20__p_ne23)*Y(jf20)*Y(jhe4) &
      *state % rho + screened_rates(k_n_f19__f20)*Y(jf19)*Y(jn)*state % rho - &
      screened_rates(k_n_f20__f21)*Y(jf20)*Y(jn)*state % rho - &
      screened_rates(k_p_f20__he4_o17)*Y(jf20)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__n_ne20)*Y(jf20)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__ne21)*Y(jf20)*Y(jp)*state % rho + &
      screened_rates(k_p_o19__f20)*Y(jo19)*Y(jp)*state % rho &
       )

    ydot_nuc(jf21) = ( &
      -screened_rates(k_f21__ne21__weak__wc12)*Y(jf21) - screened_rates(k_he4_f21__n_na24)* &
      Y(jf21)*Y(jhe4)*state % rho - screened_rates(k_he4_f21__p_ne24)*Y(jf21) &
      *Y(jhe4)*state % rho + screened_rates(k_n_f20__f21)*Y(jf20)*Y(jn)* &
      state % rho - screened_rates(k_p_f21__he4_o18)*Y(jf21)*Y(jp)*state % rho - &
      screened_rates(k_p_f21__n_ne21)*Y(jf21)*Y(jp)*state % rho - &
      screened_rates(k_p_f21__ne22)*Y(jf21)*Y(jp)*state % rho &
       )

    ydot_nuc(jne19) = ( &
      -screened_rates(k_he4_ne19__mg23)*Y(jhe4)*Y(jne19)*state % rho - &
      screened_rates(k_he4_ne19__p_na22)*Y(jhe4)*Y(jne19)*state % rho + &
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_ne19__he4_o16)*Y(jn)*Y(jne19)*state % rho - &
      screened_rates(k_n_ne19__ne20)*Y(jn)*Y(jne19)*state % rho - &
      screened_rates(k_ne19__f19__weak__wc12)*Y(jne19) + screened_rates(k_p_f18__ne19)* &
      Y(jf18)*Y(jp)*state % rho + screened_rates(k_p_f19__n_ne19)*Y(jf19)* &
      Y(jp)*state % rho - screened_rates(k_p_ne19__na20)*Y(jne19)*Y(jp)* &
      state % rho &
       )

    ydot_nuc(jne20) = ( &
      0.5e0_rt*screened_rates(k_c12_c12__he4_ne20)*Y(jc12)**2*state % rho - &
      screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*Y(jne20)*state % rho + &
      screened_rates(k_f20__ne20__weak__wc12)*Y(jf20) - &
      screened_rates(k_he4_ne20__mg24)*Y(jhe4)*Y(jne20)*state % rho + &
      screened_rates(k_he4_o16__ne20)*Y(jhe4)*Y(jo16)*state % rho + &
      screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*Y(jo17)*state % rho + &
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*Y(jn)*state % rho + &
      screened_rates(k_n_na20__p_ne20)*Y(jn)*Y(jna20)*state % rho + &
      screened_rates(k_n_ne19__ne20)*Y(jn)*Y(jne19)*state % rho - &
      screened_rates(k_n_ne20__ne21)*Y(jn)*Y(jne20)*state % rho + &
      screened_rates(k_na20__ne20__weak__wc12)*Y(jna20) + screened_rates(k_p_f19__ne20) &
      *Y(jf19)*Y(jp)*state % rho + screened_rates(k_p_f20__n_ne20)*Y(jf20)* &
      Y(jp)*state % rho + screened_rates(k_p_na23__he4_ne20)*Y(jna23)*Y(jp)* &
      state % rho - screened_rates(k_p_ne20__na21)*Y(jne20)*Y(jp)*state % rho &
       )

    ydot_nuc(jne21) = ( &
      screened_rates(k_f21__ne21__weak__wc12)*Y(jf21) + screened_rates(k_he4_f18__p_ne21)* &
      Y(jf18)*Y(jhe4)*state % rho - screened_rates(k_he4_ne21__mg25)*Y(jhe4)* &
      Y(jne21)*state % rho - screened_rates(k_he4_ne21__n_mg24)*Y(jhe4)* &
      Y(jne21)*state % rho + screened_rates(k_he4_o17__ne21)*Y(jhe4)*Y(jo17)* &
      state % rho + screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*Y(jo18)*state % rho &
      + screened_rates(k_n_na21__p_ne21)*Y(jn)*Y(jna21)*state % rho + &
      screened_rates(k_n_ne20__ne21)*Y(jn)*Y(jne20)*state % rho - &
      screened_rates(k_n_ne21__ne22)*Y(jn)*Y(jne21)*state % rho + &
      screened_rates(k_na21__ne21__weak__wc12)*Y(jna21) + screened_rates(k_p_f20__ne21) &
      *Y(jf20)*Y(jp)*state % rho + screened_rates(k_p_f21__n_ne21)*Y(jf21)* &
      Y(jp)*state % rho + screened_rates(k_p_na24__he4_ne21)*Y(jna24)*Y(jp)* &
      state % rho - screened_rates(k_p_ne21__na22)*Y(jne21)*Y(jp)*state % rho &
       )

    ydot_nuc(jne22) = ( &
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ne22__mg26)*Y(jhe4)*Y(jne22)*state % rho - &
      screened_rates(k_he4_ne22__n_mg25)*Y(jhe4)*Y(jne22)*state % rho + &
      screened_rates(k_he4_o18__ne22)*Y(jhe4)*Y(jo18)*state % rho + &
      screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*Y(jo19)*state % rho + &
      screened_rates(k_n_na22__p_ne22)*Y(jn)*Y(jna22)*state % rho + &
      screened_rates(k_n_ne21__ne22)*Y(jn)*Y(jne21)*state % rho - &
      screened_rates(k_n_ne22__ne23)*Y(jn)*Y(jne22)*state % rho + &
      screened_rates(k_na22__ne22__weak__wc12)*Y(jna22) + screened_rates(k_p_f21__ne22) &
      *Y(jf21)*Y(jp)*state % rho - screened_rates(k_p_ne22__na23)*Y(jne22)* &
      Y(jp)*state % rho &
       )

    ydot_nuc(jne23) = ( &
      screened_rates(k_he4_f20__p_ne23)*Y(jf20)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ne23__mg27)*Y(jhe4)*Y(jne23)*state % rho - &
      screened_rates(k_he4_ne23__n_mg26)*Y(jhe4)*Y(jne23)*state % rho + &
      screened_rates(k_he4_o19__ne23)*Y(jhe4)*Y(jo19)*state % rho + &
      screened_rates(k_n_ne22__ne23)*Y(jn)*Y(jne22)*state % rho - &
      screened_rates(k_n_ne23__ne24)*Y(jn)*Y(jne23)*state % rho - &
      screened_rates(k_ne23__na23__weak__wc12)*Y(jne23) - &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*Y(jp)*state % rho - &
      screened_rates(k_p_ne23__na24)*Y(jne23)*Y(jp)*state % rho &
       )

    ydot_nuc(jne24) = ( &
      screened_rates(k_he4_f21__p_ne24)*Y(jf21)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*Y(jne24)*state % rho + &
      screened_rates(k_n_ne23__ne24)*Y(jn)*Y(jne23)*state % rho - &
      screened_rates(k_ne24__na24__weak__wc12)*Y(jne24) - &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*Y(jp)*state % rho &
       )

    ydot_nuc(jna20) = ( &
      -screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*Y(jna20)*state % rho - &
      screened_rates(k_n_na20__na21)*Y(jn)*Y(jna20)*state % rho - &
      screened_rates(k_n_na20__p_ne20)*Y(jn)*Y(jna20)*state % rho - &
      screened_rates(k_na20__he4_o16__weak__wc12)*Y(jna20) - &
      screened_rates(k_na20__ne20__weak__wc12)*Y(jna20) + &
      screened_rates(k_p_ne19__na20)*Y(jne19)*Y(jp)*state % rho &
       )

    ydot_nuc(jna21) = ( &
      -screened_rates(k_he4_na21__al25)*Y(jhe4)*Y(jna21)*state % rho + &
      screened_rates(k_n_na20__na21)*Y(jn)*Y(jna20)*state % rho - &
      screened_rates(k_n_na21__he4_f18)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__na22)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__p_ne21)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_na21__ne21__weak__wc12)*Y(jna21) + &
      screened_rates(k_p_mg24__he4_na21)*Y(jmg24)*Y(jp)*state % rho - &
      screened_rates(k_p_na21__mg22)*Y(jna21)*Y(jp)*state % rho + &
      screened_rates(k_p_ne20__na21)*Y(jne20)*Y(jp)*state % rho &
       )

    ydot_nuc(jna22) = ( &
      screened_rates(k_he4_f18__na22)*Y(jf18)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_na22__al26)*Y(jhe4)*Y(jna22)*state % rho - &
      screened_rates(k_he4_na22__p_mg25)*Y(jhe4)*Y(jna22)*state % rho + &
      screened_rates(k_he4_ne19__p_na22)*Y(jhe4)*Y(jne19)*state % rho + &
      screened_rates(k_mg22__na22__weak__wc12)*Y(jmg22) + &
      screened_rates(k_n_al25__he4_na22)*Y(jal25)*Y(jn)*state % rho + &
      screened_rates(k_n_mg22__p_na22)*Y(jmg22)*Y(jn)*state % rho + &
      screened_rates(k_n_na21__na22)*Y(jn)*Y(jna21)*state % rho - &
      screened_rates(k_n_na22__he4_f19)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__na23)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__p_ne22)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_na22__ne22__weak__wc12)*Y(jna22) - &
      screened_rates(k_p_na22__mg23)*Y(jna22)*Y(jp)*state % rho + &
      screened_rates(k_p_ne21__na22)*Y(jne21)*Y(jp)*state % rho &
       )

    ydot_nuc(jna23) = ( &
      0.5e0_rt*screened_rates(k_c12_c12__p_na23)*Y(jc12)**2*state % rho + &
      screened_rates(k_he4_f19__na23)*Y(jf19)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_f20__n_na23)*Y(jf20)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_na23__al27)*Y(jhe4)*Y(jna23)*state % rho - &
      screened_rates(k_he4_na23__p_mg26)*Y(jhe4)*Y(jna23)*state % rho + &
      screened_rates(k_mg23__na23__weak__wc12)*Y(jmg23) + &
      screened_rates(k_n_al26__he4_na23)*Y(jal26)*Y(jn)*state % rho + &
      screened_rates(k_n_na22__na23)*Y(jn)*Y(jna22)*state % rho - &
      screened_rates(k_n_na23__na24)*Y(jn)*Y(jna23)*state % rho + &
      screened_rates(k_ne23__na23__weak__wc12)*Y(jne23) - &
      screened_rates(k_p_na23__he4_ne20)*Y(jna23)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__mg24)*Y(jna23)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__n_mg23)*Y(jna23)*Y(jp)*state % rho + &
      screened_rates(k_p_ne22__na23)*Y(jne22)*Y(jp)*state % rho + &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*Y(jp)*state % rho &
       )

    ydot_nuc(jna24) = ( &
      screened_rates(k_he4_f20__na24)*Y(jf20)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_f21__n_na24)*Y(jf21)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_na24__al28)*Y(jhe4)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__n_al27)*Y(jhe4)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*Y(jna24)*state % rho + &
      screened_rates(k_n_na23__na24)*Y(jn)*Y(jna23)*state % rho - &
      screened_rates(k_na24__mg24__weak__wc12)*Y(jna24) + &
      screened_rates(k_ne24__na24__weak__wc12)*Y(jne24) - &
      screened_rates(k_p_na24__he4_ne21)*Y(jna24)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__mg25)*Y(jna24)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*Y(jp)*state % rho + &
      screened_rates(k_p_ne23__na24)*Y(jne23)*Y(jp)*state % rho + &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*Y(jp)*state % rho &
       )

    ydot_nuc(jmg22) = ( &
      -screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*Y(jmg22)*state % rho - &
      screened_rates(k_mg22__na22__weak__wc12)*Y(jmg22) - &
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_mg22__mg23)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_mg22__p_na22)*Y(jmg22)*Y(jn)*state % rho + &
      screened_rates(k_p_na21__mg22)*Y(jna21)*Y(jp)*state % rho &
       )

    ydot_nuc(jmg23) = ( &
      -screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*Y(jmg23)*state % rho - &
      screened_rates(k_he4_mg23__si27)*Y(jhe4)*Y(jmg23)*state % rho + &
      screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*Y(jna20)*state % rho + &
      screened_rates(k_he4_ne19__mg23)*Y(jhe4)*Y(jne19)*state % rho - &
      screened_rates(k_mg23__na23__weak__wc12)*Y(jmg23) + &
      screened_rates(k_n_mg22__mg23)*Y(jmg22)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__c12_c12)*Y(jmg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__mg24)*Y(jmg23)*Y(jn)*state % rho + &
      screened_rates(k_p_na22__mg23)*Y(jna22)*Y(jp)*state % rho + &
      screened_rates(k_p_na23__n_mg23)*Y(jna23)*Y(jp)*state % rho &
       )

    ydot_nuc(jmg24) = ( &
      screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*Y(jo16)*state % rho - &
      screened_rates(k_he4_mg24__si28)*Y(jhe4)*Y(jmg24)*state % rho + &
      screened_rates(k_he4_ne20__mg24)*Y(jhe4)*Y(jne20)*state % rho + &
      screened_rates(k_he4_ne21__n_mg24)*Y(jhe4)*Y(jne21)*state % rho + &
      screened_rates(k_n_mg23__mg24)*Y(jmg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg24__mg25)*Y(jmg24)*Y(jn)*state % rho + &
      screened_rates(k_n_si27__he4_mg24)*Y(jn)*Y(jsi27)*state % rho + &
      screened_rates(k_na24__mg24__weak__wc12)*Y(jna24) + &
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*Y(jp)*state % rho - &
      screened_rates(k_p_mg24__al25)*Y(jmg24)*Y(jp)*state % rho - &
      screened_rates(k_p_mg24__he4_na21)*Y(jmg24)*Y(jp)*state % rho + &
      screened_rates(k_p_na23__mg24)*Y(jna23)*Y(jp)*state % rho + &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*Y(jp)*state % rho &
       )

    ydot_nuc(jmg25) = ( &
      screened_rates(k_al25__mg25__weak__wc12)*Y(jal25) - screened_rates(k_he4_mg25__n_si28)* &
      Y(jhe4)*Y(jmg25)*state % rho - screened_rates(k_he4_mg25__si29)*Y(jhe4) &
      *Y(jmg25)*state % rho + screened_rates(k_he4_na22__p_mg25)*Y(jhe4)* &
      Y(jna22)*state % rho + screened_rates(k_he4_ne21__mg25)*Y(jhe4)* &
      Y(jne21)*state % rho + screened_rates(k_he4_ne22__n_mg25)*Y(jhe4)* &
      Y(jne22)*state % rho + screened_rates(k_n_al25__p_mg25)*Y(jal25)*Y(jn)* &
      state % rho + screened_rates(k_n_mg24__mg25)*Y(jmg24)*Y(jn)*state % rho - &
      screened_rates(k_n_mg25__mg26)*Y(jmg25)*Y(jn)*state % rho + &
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*Y(jp)*state % rho - &
      screened_rates(k_p_mg25__al26)*Y(jmg25)*Y(jp)*state % rho + &
      screened_rates(k_p_na24__mg25)*Y(jna24)*Y(jp)*state % rho &
       )

    ydot_nuc(jmg26) = ( &
      screened_rates(k_al26__mg26__weak__wc12)*Y(jal26) - screened_rates(k_he4_mg26__n_si29)* &
      Y(jhe4)*Y(jmg26)*state % rho - screened_rates(k_he4_mg26__si30)*Y(jhe4) &
      *Y(jmg26)*state % rho + screened_rates(k_he4_na23__p_mg26)*Y(jhe4)* &
      Y(jna23)*state % rho + screened_rates(k_he4_ne22__mg26)*Y(jhe4)* &
      Y(jne22)*state % rho + screened_rates(k_he4_ne23__n_mg26)*Y(jhe4)* &
      Y(jne23)*state % rho + screened_rates(k_n_al26__p_mg26)*Y(jal26)*Y(jn)* &
      state % rho + screened_rates(k_n_mg25__mg26)*Y(jmg25)*Y(jn)*state % rho - &
      screened_rates(k_n_mg26__mg27)*Y(jmg26)*Y(jn)*state % rho + &
      screened_rates(k_p_al29__he4_mg26)*Y(jal29)*Y(jp)*state % rho - &
      screened_rates(k_p_mg26__al27)*Y(jmg26)*Y(jp)*state % rho &
       )

    ydot_nuc(jmg27) = ( &
      -screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_mg27__si31)*Y(jhe4)*Y(jmg27)*state % rho + &
      screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*Y(jna24)*state % rho + &
      screened_rates(k_he4_ne23__mg27)*Y(jhe4)*Y(jne23)*state % rho + &
      screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*Y(jne24)*state % rho - &
      screened_rates(k_mg27__al27__weak__wc12)*Y(jmg27) + &
      screened_rates(k_n_mg26__mg27)*Y(jmg26)*Y(jn)*state % rho - &
      screened_rates(k_p_mg27__al28)*Y(jmg27)*Y(jp)*state % rho - &
      screened_rates(k_p_mg27__n_al27)*Y(jmg27)*Y(jp)*state % rho &
       )

    ydot_nuc(jal25) = ( &
      -screened_rates(k_al25__mg25__weak__wc12)*Y(jal25) - screened_rates(k_he4_al25__p29)* &
      Y(jal25)*Y(jhe4)*state % rho - screened_rates(k_he4_al25__p_si28)* &
      Y(jal25)*Y(jhe4)*state % rho + screened_rates(k_he4_mg22__p_al25)* &
      Y(jhe4)*Y(jmg22)*state % rho + screened_rates(k_he4_na21__al25)*Y(jhe4) &
      *Y(jna21)*state % rho - screened_rates(k_n_al25__al26)*Y(jal25)*Y(jn)* &
      state % rho - screened_rates(k_n_al25__he4_na22)*Y(jal25)*Y(jn)*state % rho &
      - screened_rates(k_n_al25__p_mg25)*Y(jal25)*Y(jn)*state % rho + &
      screened_rates(k_p_mg24__al25)*Y(jmg24)*Y(jp)*state % rho &
       )

    ydot_nuc(jal26) = ( &
      -screened_rates(k_al26__mg26__weak__wc12)*Y(jal26) - screened_rates(k_he4_al26__p30)* &
      Y(jal26)*Y(jhe4)*state % rho - screened_rates(k_he4_al26__p_si29)* &
      Y(jal26)*Y(jhe4)*state % rho + screened_rates(k_he4_mg23__p_al26)* &
      Y(jhe4)*Y(jmg23)*state % rho + screened_rates(k_he4_na22__al26)*Y(jhe4) &
      *Y(jna22)*state % rho + screened_rates(k_n_al25__al26)*Y(jal25)*Y(jn)* &
      state % rho - screened_rates(k_n_al26__al27)*Y(jal26)*Y(jn)*state % rho - &
      screened_rates(k_n_al26__he4_na23)*Y(jal26)*Y(jn)*state % rho - &
      screened_rates(k_n_al26__p_mg26)*Y(jal26)*Y(jn)*state % rho + &
      screened_rates(k_n_p29__he4_al26)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_p_al26__si27)*Y(jal26)*Y(jp)*state % rho + &
      screened_rates(k_p_mg25__al26)*Y(jmg25)*Y(jp)*state % rho &
       )

    ydot_nuc(jal27) = ( &
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*Y(jo16)*state % rho - &
      screened_rates(k_he4_al27__n_p30)*Y(jal27)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al27__p31)*Y(jal27)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al27__p_si30)*Y(jal27)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_na23__al27)*Y(jhe4)*Y(jna23)*state % rho + &
      screened_rates(k_he4_na24__n_al27)*Y(jhe4)*Y(jna24)*state % rho + &
      screened_rates(k_mg27__al27__weak__wc12)*Y(jmg27) + &
      screened_rates(k_n_al26__al27)*Y(jal26)*Y(jn)*state % rho - &
      screened_rates(k_n_al27__al28)*Y(jal27)*Y(jn)*state % rho + &
      screened_rates(k_n_si27__p_al27)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*Y(jp)*state % rho - &
      screened_rates(k_p_al27__si28)*Y(jal27)*Y(jp)*state % rho + &
      screened_rates(k_p_mg26__al27)*Y(jmg26)*Y(jp)*state % rho + &
      screened_rates(k_p_mg27__n_al27)*Y(jmg27)*Y(jp)*state % rho + &
      screened_rates(k_si27__al27__weak__wc12)*Y(jsi27) &
       )

    ydot_nuc(jal28) = ( &
      -screened_rates(k_al28__si28__weak__wc12)*Y(jal28) - screened_rates(k_he4_al28__n_p31)* &
      Y(jal28)*Y(jhe4)*state % rho - screened_rates(k_he4_al28__p32)*Y(jal28) &
      *Y(jhe4)*state % rho - screened_rates(k_he4_al28__p_si31)*Y(jal28)* &
      Y(jhe4)*state % rho + screened_rates(k_he4_na24__al28)*Y(jhe4)*Y(jna24) &
      *state % rho + screened_rates(k_n_al27__al28)*Y(jal27)*Y(jn)*state % rho - &
      screened_rates(k_n_al28__al29)*Y(jal28)*Y(jn)*state % rho - &
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__n_si28)*Y(jal28)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__si29)*Y(jal28)*Y(jp)*state % rho + &
      screened_rates(k_p_mg27__al28)*Y(jmg27)*Y(jp)*state % rho &
       )

    ydot_nuc(jal29) = ( &
      -screened_rates(k_al29__si29__weak__wc12)*Y(jal29) - screened_rates(k_he4_al29__n_p32)* &
      Y(jal29)*Y(jhe4)*state % rho - screened_rates(k_he4_al29__p33)*Y(jal29) &
      *Y(jhe4)*state % rho - screened_rates(k_he4_al29__p_si32)*Y(jal29)* &
      Y(jhe4)*state % rho + screened_rates(k_n_al28__al29)*Y(jal28)*Y(jn)* &
      state % rho - screened_rates(k_p_al29__he4_mg26)*Y(jal29)*Y(jp)*state % rho &
      - screened_rates(k_p_al29__n_si29)*Y(jal29)*Y(jp)*state % rho - &
      screened_rates(k_p_al29__si30)*Y(jal29)*Y(jp)*state % rho &
       )

    ydot_nuc(jsi27) = ( &
      screened_rates(k_he4_mg23__si27)*Y(jhe4)*Y(jmg23)*state % rho - &
      screened_rates(k_he4_si27__p_p30)*Y(jhe4)*Y(jsi27)*state % rho - &
      screened_rates(k_he4_si27__s31)*Y(jhe4)*Y(jsi27)*state % rho + &
      screened_rates(k_n_s30__he4_si27)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_si27__c12_o16)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__he4_mg24)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__p_al27)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__si28)*Y(jn)*Y(jsi27)*state % rho + &
      screened_rates(k_p_al26__si27)*Y(jal26)*Y(jp)*state % rho - &
      screened_rates(k_si27__al27__weak__wc12)*Y(jsi27) &
       )

    ydot_nuc(jsi28) = ( &
      screened_rates(k_al28__si28__weak__wc12)*Y(jal28) + screened_rates(k_c12_ne20__he4_si28) &
      *Y(jc12)*Y(jne20)*state % rho + screened_rates(k_he4_al25__p_si28)* &
      Y(jal25)*Y(jhe4)*state % rho + screened_rates(k_he4_mg24__si28)*Y(jhe4) &
      *Y(jmg24)*state % rho + screened_rates(k_he4_mg25__n_si28)*Y(jhe4)* &
      Y(jmg25)*state % rho - screened_rates(k_he4_si28__s32)*Y(jhe4)*Y(jsi28) &
      *state % rho + screened_rates(k_n_s31__he4_si28)*Y(jn)*Y(js31)*state % rho + &
      screened_rates(k_n_si27__si28)*Y(jn)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si28__si29)*Y(jn)*Y(jsi28)*state % rho + 0.5e0_rt* &
      screened_rates(k_o16_o16__he4_si28)*Y(jo16)**2*state % rho + &
      screened_rates(k_p_al27__si28)*Y(jal27)*Y(jp)*state % rho + &
      screened_rates(k_p_al28__n_si28)*Y(jal28)*Y(jp)*state % rho + &
      screened_rates(k_p_p31__he4_si28)*Y(jp31)*Y(jp)*state % rho - &
      screened_rates(k_p_si28__p29)*Y(jp)*Y(jsi28)*state % rho &
       )

    ydot_nuc(jsi29) = ( &
      screened_rates(k_al29__si29__weak__wc12)*Y(jal29) + screened_rates(k_he4_al26__p_si29)* &
      Y(jal26)*Y(jhe4)*state % rho + screened_rates(k_he4_mg25__si29)*Y(jhe4) &
      *Y(jmg25)*state % rho + screened_rates(k_he4_mg26__n_si29)*Y(jhe4)* &
      Y(jmg26)*state % rho - screened_rates(k_he4_si29__s33)*Y(jhe4)*Y(jsi29) &
      *state % rho + screened_rates(k_n_p29__p_si29)*Y(jn)*Y(jp29)*state % rho + &
      screened_rates(k_n_s32__he4_si29)*Y(jn)*Y(js32)*state % rho + &
      screened_rates(k_n_si28__si29)*Y(jn)*Y(jsi28)*state % rho - &
      screened_rates(k_n_si29__si30)*Y(jn)*Y(jsi29)*state % rho + &
      screened_rates(k_p29__si29__weak__wc12)*Y(jp29) + screened_rates(k_p_al28__si29)* &
      Y(jal28)*Y(jp)*state % rho + screened_rates(k_p_al29__n_si29)*Y(jal29)* &
      Y(jp)*state % rho + screened_rates(k_p_p32__he4_si29)*Y(jp32)*Y(jp)* &
      state % rho - screened_rates(k_p_si29__p30)*Y(jp)*Y(jsi29)*state % rho &
       )

    ydot_nuc(jsi30) = ( &
      screened_rates(k_he4_al27__p_si30)*Y(jal27)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_mg26__si30)*Y(jhe4)*Y(jmg26)*state % rho + &
      screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_si30__s34)*Y(jhe4)*Y(jsi30)*state % rho + &
      screened_rates(k_n_p30__p_si30)*Y(jn)*Y(jp30)*state % rho + &
      screened_rates(k_n_s33__he4_si30)*Y(jn)*Y(js33)*state % rho + &
      screened_rates(k_n_si29__si30)*Y(jn)*Y(jsi29)*state % rho - &
      screened_rates(k_n_si30__si31)*Y(jn)*Y(jsi30)*state % rho + &
      screened_rates(k_p30__si30__weak__wc12)*Y(jp30) + screened_rates(k_p_al29__si30)* &
      Y(jal29)*Y(jp)*state % rho + screened_rates(k_p_p33__he4_si30)*Y(jp33)* &
      Y(jp)*state % rho - screened_rates(k_p_si30__p31)*Y(jp)*Y(jsi30)* &
      state % rho &
       )

    ydot_nuc(jsi31) = ( &
      screened_rates(k_he4_al28__p_si31)*Y(jal28)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_mg27__si31)*Y(jhe4)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_si31__n_s34)*Y(jhe4)*Y(jsi31)*state % rho - &
      screened_rates(k_he4_si31__s35)*Y(jhe4)*Y(jsi31)*state % rho + &
      screened_rates(k_n_si30__si31)*Y(jn)*Y(jsi30)*state % rho - &
      screened_rates(k_n_si31__si32)*Y(jn)*Y(jsi31)*state % rho - &
      screened_rates(k_p_si31__n_p31)*Y(jp)*Y(jsi31)*state % rho - &
      screened_rates(k_p_si31__p32)*Y(jp)*Y(jsi31)*state % rho - &
      screened_rates(k_si31__p31__weak__wc12)*Y(jsi31) &
       )

    ydot_nuc(jsi32) = ( &
      screened_rates(k_he4_al29__p_si32)*Y(jal29)*Y(jhe4)*state % rho + &
      screened_rates(k_n_p32__p_si32)*Y(jn)*Y(jp32)*state % rho + &
      screened_rates(k_n_s35__he4_si32)*Y(jn)*Y(js35)*state % rho + &
      screened_rates(k_n_si31__si32)*Y(jn)*Y(jsi31)*state % rho - &
      screened_rates(k_p_si32__p33)*Y(jp)*Y(jsi32)*state % rho - &
      screened_rates(k_si32__p32__weak__wc12)*Y(jsi32) &
       )

    ydot_nuc(jp29) = ( &
      screened_rates(k_he4_al25__p29)*Y(jal25)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_p29__cl33)*Y(jhe4)*Y(jp29)*state % rho - &
      screened_rates(k_he4_p29__p_s32)*Y(jhe4)*Y(jp29)*state % rho - &
      screened_rates(k_n_p29__he4_al26)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_n_p29__p30)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_n_p29__p_si29)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_p29__si29__weak__wc12)*Y(jp29) - screened_rates(k_p_p29__s30)* &
      Y(jp29)*Y(jp)*state % rho + screened_rates(k_p_si28__p29)*Y(jp)* &
      Y(jsi28)*state % rho &
       )

    ydot_nuc(jp30) = ( &
      screened_rates(k_he4_al26__p30)*Y(jal26)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_al27__n_p30)*Y(jal27)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_p30__cl34)*Y(jhe4)*Y(jp30)*state % rho - &
      screened_rates(k_he4_p30__p_s33)*Y(jhe4)*Y(jp30)*state % rho + &
      screened_rates(k_he4_si27__p_p30)*Y(jhe4)*Y(jsi27)*state % rho + &
      screened_rates(k_n_cl33__he4_p30)*Y(jcl33)*Y(jn)*state % rho + &
      screened_rates(k_n_p29__p30)*Y(jn)*Y(jp29)*state % rho - &
      screened_rates(k_n_p30__p31)*Y(jn)*Y(jp30)*state % rho - &
      screened_rates(k_n_p30__p_si30)*Y(jn)*Y(jp30)*state % rho + &
      screened_rates(k_n_s30__p_p30)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_p30__si30__weak__wc12)*Y(jp30) - screened_rates(k_p_p30__s31)* &
      Y(jp30)*Y(jp)*state % rho + screened_rates(k_p_si29__p30)*Y(jp)* &
      Y(jsi29)*state % rho + screened_rates(k_s30__p30__weak__wc12)*Y(js30) &
       )

    ydot_nuc(jp31) = ( &
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*Y(jne20)*state % rho + &
      screened_rates(k_he4_al27__p31)*Y(jal27)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_al28__n_p31)*Y(jal28)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_p31__cl35)*Y(jhe4)*Y(jp31)*state % rho - &
      screened_rates(k_he4_p31__p_s34)*Y(jhe4)*Y(jp31)*state % rho + &
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*Y(jn)*state % rho + &
      screened_rates(k_n_p30__p31)*Y(jn)*Y(jp30)*state % rho - &
      screened_rates(k_n_p31__p32)*Y(jn)*Y(jp31)*state % rho + &
      screened_rates(k_n_s31__p_p31)*Y(jn)*Y(js31)*state % rho + 0.5e0_rt* &
      screened_rates(k_o16_o16__p_p31)*Y(jo16)**2*state % rho - &
      screened_rates(k_p_p31__he4_si28)*Y(jp31)*Y(jp)*state % rho - &
      screened_rates(k_p_p31__s32)*Y(jp31)*Y(jp)*state % rho + &
      screened_rates(k_p_si30__p31)*Y(jp)*Y(jsi30)*state % rho + &
      screened_rates(k_p_si31__n_p31)*Y(jp)*Y(jsi31)*state % rho + &
      screened_rates(k_s31__p31__weak__wc12)*Y(js31) + &
      screened_rates(k_si31__p31__weak__wc12)*Y(jsi31) &
       )

    ydot_nuc(jp32) = ( &
      screened_rates(k_he4_al28__p32)*Y(jal28)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_al29__n_p32)*Y(jal29)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_p32__cl36)*Y(jhe4)*Y(jp32)*state % rho + &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*Y(jn)*state % rho + &
      screened_rates(k_n_p31__p32)*Y(jn)*Y(jp31)*state % rho - &
      screened_rates(k_n_p32__p33)*Y(jn)*Y(jp32)*state % rho - &
      screened_rates(k_n_p32__p_si32)*Y(jn)*Y(jp32)*state % rho - &
      screened_rates(k_p32__s32__weak__wc12)*Y(jp32) - &
      screened_rates(k_p_p32__he4_si29)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__n_s32)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__s33)*Y(jp32)*Y(jp)*state % rho + &
      screened_rates(k_p_s35__he4_p32)*Y(jp)*Y(js35)*state % rho + &
      screened_rates(k_p_si31__p32)*Y(jp)*Y(jsi31)*state % rho + &
      screened_rates(k_si32__p32__weak__wc12)*Y(jsi32) &
       )

    ydot_nuc(jp33) = ( &
      screened_rates(k_he4_al29__p33)*Y(jal29)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_p33__cl37)*Y(jhe4)*Y(jp33)*state % rho + &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*Y(jn)*state % rho + &
      screened_rates(k_n_p32__p33)*Y(jn)*Y(jp32)*state % rho + &
      screened_rates(k_n_s33__p_p33)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_p33__s33__weak__wc12)*Y(jp33) - &
      screened_rates(k_p_p33__he4_si30)*Y(jp33)*Y(jp)*state % rho - &
      screened_rates(k_p_p33__s34)*Y(jp33)*Y(jp)*state % rho + &
      screened_rates(k_p_si32__p33)*Y(jp)*Y(jsi32)*state % rho &
       )

    ydot_nuc(js30) = ( &
      -screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*Y(js30)*state % rho - &
      screened_rates(k_n_s30__he4_si27)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_s30__p_p30)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_s30__s31)*Y(jn)*Y(js30)*state % rho + &
      screened_rates(k_p_p29__s30)*Y(jp29)*Y(jp)*state % rho - &
      screened_rates(k_s30__p30__weak__wc12)*Y(js30) &
       )

    ydot_nuc(js31) = ( &
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*Y(jne20)*state % rho - &
      screened_rates(k_he4_s31__ar35)*Y(jhe4)*Y(js31)*state % rho - &
      screened_rates(k_he4_s31__p_cl34)*Y(jhe4)*Y(js31)*state % rho + &
      screened_rates(k_he4_si27__s31)*Y(jhe4)*Y(jsi27)*state % rho + &
      screened_rates(k_n_s30__s31)*Y(jn)*Y(js30)*state % rho - &
      screened_rates(k_n_s31__he4_si28)*Y(jn)*Y(js31)*state % rho - &
      screened_rates(k_n_s31__p_p31)*Y(jn)*Y(js31)*state % rho - &
      screened_rates(k_n_s31__s32)*Y(jn)*Y(js31)*state % rho + 0.5e0_rt* &
      screened_rates(k_o16_o16__n_s31)*Y(jo16)**2*state % rho + &
      screened_rates(k_p_p30__s31)*Y(jp30)*Y(jp)*state % rho - &
      screened_rates(k_s31__p31__weak__wc12)*Y(js31) &
       )

    ydot_nuc(js32) = ( &
      screened_rates(k_he4_p29__p_s32)*Y(jhe4)*Y(jp29)*state % rho - &
      screened_rates(k_he4_s32__ar36)*Y(jhe4)*Y(js32)*state % rho + &
      screened_rates(k_he4_si28__s32)*Y(jhe4)*Y(jsi28)*state % rho + &
      screened_rates(k_n_ar35__he4_s32)*Y(jar35)*Y(jn)*state % rho + &
      screened_rates(k_n_s31__s32)*Y(jn)*Y(js31)*state % rho - &
      screened_rates(k_n_s32__he4_si29)*Y(jn)*Y(js32)*state % rho - &
      screened_rates(k_n_s32__s33)*Y(jn)*Y(js32)*state % rho + &
      screened_rates(k_p32__s32__weak__wc12)*Y(jp32) + &
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*Y(jp)*state % rho + &
      screened_rates(k_p_p31__s32)*Y(jp31)*Y(jp)*state % rho + &
      screened_rates(k_p_p32__n_s32)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_s32__cl33)*Y(jp)*Y(js32)*state % rho &
       )

    ydot_nuc(js33) = ( &
      screened_rates(k_cl33__s33__weak__wc12)*Y(jcl33) + screened_rates(k_he4_p30__p_s33)* &
      Y(jhe4)*Y(jp30)*state % rho - screened_rates(k_he4_s33__ar37)*Y(jhe4)* &
      Y(js33)*state % rho + screened_rates(k_he4_si29__s33)*Y(jhe4)*Y(jsi29)* &
      state % rho + screened_rates(k_n_ar36__he4_s33)*Y(jar36)*Y(jn)*state % rho + &
      screened_rates(k_n_cl33__p_s33)*Y(jcl33)*Y(jn)*state % rho + &
      screened_rates(k_n_s32__s33)*Y(jn)*Y(js32)*state % rho - &
      screened_rates(k_n_s33__he4_si30)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_n_s33__p_p33)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_n_s33__s34)*Y(jn)*Y(js33)*state % rho + &
      screened_rates(k_p33__s33__weak__wc12)*Y(jp33) + &
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*Y(jp)*state % rho + &
      screened_rates(k_p_p32__s33)*Y(jp32)*Y(jp)*state % rho - &
      screened_rates(k_p_s33__cl34)*Y(jp)*Y(js33)*state % rho &
       )

    ydot_nuc(js34) = ( &
      screened_rates(k_cl34__s34__weak__wc12)*Y(jcl34) + screened_rates(k_he4_p31__p_s34)* &
      Y(jhe4)*Y(jp31)*state % rho - screened_rates(k_he4_s34__ar38)*Y(jhe4)* &
      Y(js34)*state % rho - screened_rates(k_he4_s34__n_ar37)*Y(jhe4)*Y(js34) &
      *state % rho - screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*Y(js34)*state % rho &
      + screened_rates(k_he4_si30__s34)*Y(jhe4)*Y(jsi30)*state % rho + &
      screened_rates(k_he4_si31__n_s34)*Y(jhe4)*Y(jsi31)*state % rho + &
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*Y(jn)*state % rho + &
      screened_rates(k_n_s33__s34)*Y(jn)*Y(js33)*state % rho - &
      screened_rates(k_n_s34__s35)*Y(jn)*Y(js34)*state % rho + &
      screened_rates(k_p_p33__s34)*Y(jp33)*Y(jp)*state % rho - &
      screened_rates(k_p_s34__cl35)*Y(jp)*Y(js34)*state % rho &
       )

    ydot_nuc(js35) = ( &
      -screened_rates(k_he4_s35__ar39)*Y(jhe4)*Y(js35)*state % rho - &
      screened_rates(k_he4_s35__n_ar38)*Y(jhe4)*Y(js35)*state % rho + &
      screened_rates(k_he4_si31__s35)*Y(jhe4)*Y(jsi31)*state % rho + &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*Y(jn)*state % rho + &
      screened_rates(k_n_s34__s35)*Y(jn)*Y(js34)*state % rho - &
      screened_rates(k_n_s35__he4_si32)*Y(jn)*Y(js35)*state % rho - &
      screened_rates(k_p_s35__cl36)*Y(jp)*Y(js35)*state % rho - &
      screened_rates(k_p_s35__he4_p32)*Y(jp)*Y(js35)*state % rho - &
      screened_rates(k_s35__cl35__weak__wc12)*Y(js35) &
       )

    ydot_nuc(jcl33) = ( &
      -screened_rates(k_cl33__s33__weak__wc12)*Y(jcl33) - screened_rates(k_he4_cl33__k37)* &
      Y(jcl33)*Y(jhe4)*state % rho - screened_rates(k_he4_cl33__p_ar36)* &
      Y(jcl33)*Y(jhe4)*state % rho + screened_rates(k_he4_p29__cl33)*Y(jhe4)* &
      Y(jp29)*state % rho + screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*Y(js30) &
      *state % rho - screened_rates(k_n_cl33__cl34)*Y(jcl33)*Y(jn)*state % rho - &
      screened_rates(k_n_cl33__he4_p30)*Y(jcl33)*Y(jn)*state % rho - &
      screened_rates(k_n_cl33__p_s33)*Y(jcl33)*Y(jn)*state % rho + &
      screened_rates(k_p_s32__cl33)*Y(jp)*Y(js32)*state % rho &
       )

    ydot_nuc(jcl34) = ( &
      -screened_rates(k_cl34__s34__weak__wc12)*Y(jcl34) - screened_rates(k_he4_cl34__k38)* &
      Y(jcl34)*Y(jhe4)*state % rho - screened_rates(k_he4_cl34__p_ar37)* &
      Y(jcl34)*Y(jhe4)*state % rho + screened_rates(k_he4_p30__cl34)*Y(jhe4)* &
      Y(jp30)*state % rho + screened_rates(k_he4_s31__p_cl34)*Y(jhe4)*Y(js31) &
      *state % rho + screened_rates(k_n_cl33__cl34)*Y(jcl33)*Y(jn)*state % rho - &
      screened_rates(k_n_cl34__cl35)*Y(jcl34)*Y(jn)*state % rho - &
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*Y(jn)*state % rho - &
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*Y(jn)*state % rho + &
      screened_rates(k_n_k37__he4_cl34)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_p_cl34__ar35)*Y(jcl34)*Y(jp)*state % rho + &
      screened_rates(k_p_s33__cl34)*Y(jp)*Y(js33)*state % rho &
       )

    ydot_nuc(jcl35) = ( &
      screened_rates(k_ar35__cl35__weak__wc12)*Y(jar35) - screened_rates(k_he4_cl35__k39)* &
      Y(jcl35)*Y(jhe4)*state % rho - screened_rates(k_he4_cl35__p_ar38)* &
      Y(jcl35)*Y(jhe4)*state % rho + screened_rates(k_he4_p31__cl35)*Y(jhe4)* &
      Y(jp31)*state % rho + screened_rates(k_n_ar35__p_cl35)*Y(jar35)*Y(jn)* &
      state % rho + screened_rates(k_n_cl34__cl35)*Y(jcl34)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__cl36)*Y(jcl35)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*Y(jn)*state % rho + &
      screened_rates(k_n_k38__he4_cl35)*Y(jk38)*Y(jn)*state % rho - &
      screened_rates(k_p_cl35__ar36)*Y(jcl35)*Y(jp)*state % rho - &
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*Y(jp)*state % rho + &
      screened_rates(k_p_s34__cl35)*Y(jp)*Y(js34)*state % rho + &
      screened_rates(k_s35__cl35__weak__wc12)*Y(js35) &
       )

    ydot_nuc(jcl36) = ( &
      -screened_rates(k_cl36__ar36__weak__wc12)*Y(jcl36) - screened_rates(k_he4_cl36__k40)* &
      Y(jcl36)*Y(jhe4)*state % rho + screened_rates(k_he4_p32__cl36)*Y(jhe4)* &
      Y(jp32)*state % rho + screened_rates(k_n_ar36__p_cl36)*Y(jar36)*Y(jn)* &
      state % rho + screened_rates(k_n_cl35__cl36)*Y(jcl35)*Y(jn)*state % rho - &
      screened_rates(k_n_cl36__cl37)*Y(jcl36)*Y(jn)*state % rho - &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*Y(jn)*state % rho + &
      screened_rates(k_n_k39__he4_cl36)*Y(jk39)*Y(jn)*state % rho + &
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*Y(jp)*state % rho - &
      screened_rates(k_p_cl36__ar37)*Y(jcl36)*Y(jp)*state % rho - &
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*Y(jp)*state % rho + &
      screened_rates(k_p_s35__cl36)*Y(jp)*Y(js35)*state % rho &
       )

    ydot_nuc(jcl37) = ( &
      screened_rates(k_ar37__cl37__weak__wc12)*Y(jar37) - screened_rates(k_he4_cl37__k41)* &
      Y(jcl37)*Y(jhe4)*state % rho + screened_rates(k_he4_p33__cl37)*Y(jhe4)* &
      Y(jp33)*state % rho + screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*Y(js34) &
      *state % rho + screened_rates(k_n_ar37__p_cl37)*Y(jar37)*Y(jn)*state % rho + &
      screened_rates(k_n_cl36__cl37)*Y(jcl36)*Y(jn)*state % rho + &
      screened_rates(k_n_k40__he4_cl37)*Y(jk40)*Y(jn)*state % rho - &
      screened_rates(k_p_cl37__ar38)*Y(jcl37)*Y(jp)*state % rho &
       )

    ydot_nuc(jar35) = ( &
      -screened_rates(k_ar35__cl35__weak__wc12)*Y(jar35) - screened_rates(k_he4_ar35__ca39)* &
      Y(jar35)*Y(jhe4)*state % rho - screened_rates(k_he4_ar35__p_k38)* &
      Y(jar35)*Y(jhe4)*state % rho + screened_rates(k_he4_s31__ar35)*Y(jhe4)* &
      Y(js31)*state % rho - screened_rates(k_n_ar35__ar36)*Y(jar35)*Y(jn)* &
      state % rho - screened_rates(k_n_ar35__he4_s32)*Y(jar35)*Y(jn)*state % rho - &
      screened_rates(k_n_ar35__p_cl35)*Y(jar35)*Y(jn)*state % rho + &
      screened_rates(k_p_cl34__ar35)*Y(jcl34)*Y(jp)*state % rho &
       )

    ydot_nuc(jar36) = ( &
      screened_rates(k_cl36__ar36__weak__wc12)*Y(jcl36) - screened_rates(k_he4_ar36__ca40)* &
      Y(jar36)*Y(jhe4)*state % rho + screened_rates(k_he4_cl33__p_ar36)* &
      Y(jcl33)*Y(jhe4)*state % rho + screened_rates(k_he4_s32__ar36)*Y(jhe4)* &
      Y(js32)*state % rho + screened_rates(k_n_ar35__ar36)*Y(jar35)*Y(jn)* &
      state % rho - screened_rates(k_n_ar36__ar37)*Y(jar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ar36__he4_s33)*Y(jar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ar36__p_cl36)*Y(jar36)*Y(jn)*state % rho + &
      screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*Y(jn)*state % rho - &
      screened_rates(k_p_ar36__k37)*Y(jar36)*Y(jp)*state % rho + &
      screened_rates(k_p_cl35__ar36)*Y(jcl35)*Y(jp)*state % rho + &
      screened_rates(k_p_k39__he4_ar36)*Y(jk39)*Y(jp)*state % rho &
       )

    ydot_nuc(jar37) = ( &
      -screened_rates(k_ar37__cl37__weak__wc12)*Y(jar37) - screened_rates(k_he4_ar37__ca41)* &
      Y(jar37)*Y(jhe4)*state % rho + screened_rates(k_he4_cl34__p_ar37)* &
      Y(jcl34)*Y(jhe4)*state % rho + screened_rates(k_he4_s33__ar37)*Y(jhe4)* &
      Y(js33)*state % rho + screened_rates(k_he4_s34__n_ar37)*Y(jhe4)*Y(js34) &
      *state % rho + screened_rates(k_k37__ar37__weak__wc12)*Y(jk37) + &
      screened_rates(k_n_ar36__ar37)*Y(jar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ar37__ar38)*Y(jar37)*Y(jn)*state % rho - &
      screened_rates(k_n_ar37__p_cl37)*Y(jar37)*Y(jn)*state % rho + &
      screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*Y(jn)*state % rho + &
      screened_rates(k_n_k37__p_ar37)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_p_ar37__k38)*Y(jar37)*Y(jp)*state % rho + &
      screened_rates(k_p_cl36__ar37)*Y(jcl36)*Y(jp)*state % rho + &
      screened_rates(k_p_k40__he4_ar37)*Y(jk40)*Y(jp)*state % rho &
       )

    ydot_nuc(jar38) = ( &
      -screened_rates(k_he4_ar38__ca42)*Y(jar38)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cl35__p_ar38)*Y(jcl35)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_s34__ar38)*Y(jhe4)*Y(js34)*state % rho + &
      screened_rates(k_he4_s35__n_ar38)*Y(jhe4)*Y(js35)*state % rho + &
      screened_rates(k_k38__ar38__weak__wc12)*Y(jk38) + screened_rates(k_n_ar37__ar38)* &
      Y(jar37)*Y(jn)*state % rho - screened_rates(k_n_ar38__ar39)*Y(jar38)* &
      Y(jn)*state % rho + screened_rates(k_n_ca41__he4_ar38)*Y(jca41)*Y(jn)* &
      state % rho + screened_rates(k_n_k38__p_ar38)*Y(jk38)*Y(jn)*state % rho - &
      screened_rates(k_p_ar38__k39)*Y(jar38)*Y(jp)*state % rho + &
      screened_rates(k_p_cl37__ar38)*Y(jcl37)*Y(jp)*state % rho + &
      screened_rates(k_p_k41__he4_ar38)*Y(jk41)*Y(jp)*state % rho &
       )

    ydot_nuc(jar39) = ( &
      -screened_rates(k_ar39__k39__weak__wc12)*Y(jar39) - screened_rates(k_he4_ar39__ca43)* &
      Y(jar39)*Y(jhe4)*state % rho + screened_rates(k_he4_s35__ar39)*Y(jhe4)* &
      Y(js35)*state % rho + screened_rates(k_n_ar38__ar39)*Y(jar38)*Y(jn)* &
      state % rho + screened_rates(k_n_ca42__he4_ar39)*Y(jca42)*Y(jn)*state % rho &
      + screened_rates(k_n_k39__p_ar39)*Y(jk39)*Y(jn)*state % rho - &
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*Y(jp)*state % rho - &
      screened_rates(k_p_ar39__k40)*Y(jar39)*Y(jp)*state % rho + &
      screened_rates(k_p_k42__he4_ar39)*Y(jk42)*Y(jp)*state % rho &
       )

    ydot_nuc(jk37) = ( &
      screened_rates(k_he4_cl33__k37)*Y(jcl33)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*Y(jk37)*state % rho - &
      screened_rates(k_k37__ar37__weak__wc12)*Y(jk37) - &
      screened_rates(k_n_k37__he4_cl34)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_n_k37__k38)*Y(jk37)*Y(jn)*state % rho - &
      screened_rates(k_n_k37__p_ar37)*Y(jk37)*Y(jn)*state % rho + &
      screened_rates(k_p_ar36__k37)*Y(jar36)*Y(jp)*state % rho &
       )

    ydot_nuc(jk38) = ( &
      screened_rates(k_he4_ar35__p_k38)*Y(jar35)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_cl34__k38)*Y(jcl34)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*Y(jk38)*state % rho - &
      screened_rates(k_he4_k38__sc42)*Y(jhe4)*Y(jk38)*state % rho - &
      screened_rates(k_k38__ar38__weak__wc12)*Y(jk38) + screened_rates(k_n_k37__k38)* &
      Y(jk37)*Y(jn)*state % rho - screened_rates(k_n_k38__he4_cl35)*Y(jk38)* &
      Y(jn)*state % rho - screened_rates(k_n_k38__k39)*Y(jk38)*Y(jn)*state % rho &
      - screened_rates(k_n_k38__p_ar38)*Y(jk38)*Y(jn)*state % rho + &
      screened_rates(k_p_ar37__k38)*Y(jar37)*Y(jp)*state % rho - &
      screened_rates(k_p_k38__ca39)*Y(jk38)*Y(jp)*state % rho &
       )

    ydot_nuc(jk39) = ( &
      screened_rates(k_ar39__k39__weak__wc12)*Y(jar39) + &
      screened_rates(k_ca39__k39__weak__wc12)*Y(jca39) + &
      screened_rates(k_he4_cl35__k39)*Y(jcl35)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_k39__sc43)*Y(jhe4)*Y(jk39)*state % rho + &
      screened_rates(k_n_ca39__p_k39)*Y(jca39)*Y(jn)*state % rho + &
      screened_rates(k_n_k38__k39)*Y(jk38)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__he4_cl36)*Y(jk39)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__k40)*Y(jk39)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__p_ar39)*Y(jk39)*Y(jn)*state % rho + &
      screened_rates(k_n_sc42__he4_k39)*Y(jn)*Y(jsc42)*state % rho + &
      screened_rates(k_p_ar38__k39)*Y(jar38)*Y(jp)*state % rho + &
      screened_rates(k_p_ca42__he4_k39)*Y(jca42)*Y(jp)*state % rho - &
      screened_rates(k_p_k39__ca40)*Y(jk39)*Y(jp)*state % rho - &
      screened_rates(k_p_k39__he4_ar36)*Y(jk39)*Y(jp)*state % rho &
       )

    ydot_nuc(jk40) = ( &
      screened_rates(k_he4_cl36__k40)*Y(jcl36)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*Y(jk40)*state % rho - &
      screened_rates(k_he4_k40__sc44)*Y(jhe4)*Y(jk40)*state % rho - &
      screened_rates(k_k40__ca40__weak__wc12)*Y(jk40) + screened_rates(k_n_k39__k40)* &
      Y(jk39)*Y(jn)*state % rho - screened_rates(k_n_k40__he4_cl37)*Y(jk40)* &
      Y(jn)*state % rho - screened_rates(k_n_k40__k41)*Y(jk40)*Y(jn)*state % rho &
      + screened_rates(k_n_sc43__he4_k40)*Y(jn)*Y(jsc43)*state % rho + &
      screened_rates(k_p_ar39__k40)*Y(jar39)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__ca41)*Y(jk40)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__he4_ar37)*Y(jk40)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__n_ca40)*Y(jk40)*Y(jp)*state % rho &
       )

    ydot_nuc(jk41) = ( &
      screened_rates(k_ca41__k41__weak__wc12)*Y(jca41) + screened_rates(k_he4_cl37__k41)* &
      Y(jcl37)*Y(jhe4)*state % rho - screened_rates(k_he4_k41__n_sc44)* &
      Y(jhe4)*Y(jk41)*state % rho - screened_rates(k_he4_k41__p_ca44)*Y(jhe4) &
      *Y(jk41)*state % rho - screened_rates(k_he4_k41__sc45)*Y(jhe4)*Y(jk41)* &
      state % rho + screened_rates(k_n_k40__k41)*Y(jk40)*Y(jn)*state % rho - &
      screened_rates(k_n_k41__k42)*Y(jk41)*Y(jn)*state % rho - &
      screened_rates(k_p_k41__ca42)*Y(jk41)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__he4_ar38)*Y(jk41)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__n_ca41)*Y(jk41)*Y(jp)*state % rho &
       )

    ydot_nuc(jk42) = ( &
      -screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*Y(jk42)*state % rho - &
      screened_rates(k_he4_k42__sc46)*Y(jhe4)*Y(jk42)*state % rho - &
      screened_rates(k_k42__ca42__weak__wc12)*Y(jk42) + screened_rates(k_n_k41__k42)* &
      Y(jk41)*Y(jn)*state % rho - screened_rates(k_p_k42__ca43)*Y(jk42)* &
      Y(jp)*state % rho - screened_rates(k_p_k42__he4_ar39)*Y(jk42)*Y(jp)* &
      state % rho - screened_rates(k_p_k42__n_ca42)*Y(jk42)*Y(jp)*state % rho &
       )

    ydot_nuc(jca39) = ( &
      -screened_rates(k_ca39__k39__weak__wc12)*Y(jca39) + screened_rates(k_he4_ar35__ca39)* &
      Y(jar35)*Y(jhe4)*state % rho - screened_rates(k_he4_ca39__ti43)* &
      Y(jca39)*Y(jhe4)*state % rho - screened_rates(k_n_ca39__ca40)*Y(jca39)* &
      Y(jn)*state % rho - screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*Y(jn)* &
      state % rho - screened_rates(k_n_ca39__p_k39)*Y(jca39)*Y(jn)*state % rho + &
      screened_rates(k_p_k38__ca39)*Y(jk38)*Y(jp)*state % rho + &
      screened_rates(k_p_sc42__he4_ca39)*Y(jp)*Y(jsc42)*state % rho &
       )

    ydot_nuc(jca40) = ( &
      screened_rates(k_he4_ar36__ca40)*Y(jar36)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca40__ti44)*Y(jca40)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*Y(jk37)*state % rho + &
      screened_rates(k_k40__ca40__weak__wc12)*Y(jk40) + screened_rates(k_n_ca39__ca40)* &
      Y(jca39)*Y(jn)*state % rho - screened_rates(k_n_ca40__ca41)*Y(jca40)* &
      Y(jn)*state % rho - screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*Y(jn)* &
      state % rho + screened_rates(k_n_ti43__he4_ca40)*Y(jn)*Y(jti43)*state % rho &
      + screened_rates(k_p_k39__ca40)*Y(jk39)*Y(jp)*state % rho + &
      screened_rates(k_p_k40__n_ca40)*Y(jk40)*Y(jp)*state % rho + &
      screened_rates(k_p_sc43__he4_ca40)*Y(jp)*Y(jsc43)*state % rho &
       )

    ydot_nuc(jca41) = ( &
      -screened_rates(k_ca41__k41__weak__wc12)*Y(jca41) + screened_rates(k_he4_ar37__ca41)* &
      Y(jar37)*Y(jhe4)*state % rho - screened_rates(k_he4_ca41__ti45)* &
      Y(jca41)*Y(jhe4)*state % rho + screened_rates(k_he4_k38__p_ca41)* &
      Y(jhe4)*Y(jk38)*state % rho + screened_rates(k_n_ca40__ca41)*Y(jca40)* &
      Y(jn)*state % rho - screened_rates(k_n_ca41__ca42)*Y(jca41)*Y(jn)* &
      state % rho - screened_rates(k_n_ca41__he4_ar38)*Y(jca41)*Y(jn)*state % rho &
      + screened_rates(k_n_ti44__he4_ca41)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_p_ca41__sc42)*Y(jca41)*Y(jp)*state % rho + &
      screened_rates(k_p_k40__ca41)*Y(jk40)*Y(jp)*state % rho + &
      screened_rates(k_p_k41__n_ca41)*Y(jk41)*Y(jp)*state % rho + &
      screened_rates(k_p_sc44__he4_ca41)*Y(jp)*Y(jsc44)*state % rho &
       )

    ydot_nuc(jca42) = ( &
      screened_rates(k_he4_ar38__ca42)*Y(jar38)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca42__ti46)*Y(jca42)*Y(jhe4)*state % rho + &
      screened_rates(k_k42__ca42__weak__wc12)*Y(jk42) + screened_rates(k_n_ca41__ca42)* &
      Y(jca41)*Y(jn)*state % rho - screened_rates(k_n_ca42__ca43)*Y(jca42)* &
      Y(jn)*state % rho - screened_rates(k_n_ca42__he4_ar39)*Y(jca42)*Y(jn)* &
      state % rho + screened_rates(k_n_sc42__p_ca42)*Y(jn)*Y(jsc42)*state % rho + &
      screened_rates(k_n_ti45__he4_ca42)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_p_ca42__he4_k39)*Y(jca42)*Y(jp)*state % rho - &
      screened_rates(k_p_ca42__sc43)*Y(jca42)*Y(jp)*state % rho + &
      screened_rates(k_p_k41__ca42)*Y(jk41)*Y(jp)*state % rho + &
      screened_rates(k_p_k42__n_ca42)*Y(jk42)*Y(jp)*state % rho + &
      screened_rates(k_p_sc45__he4_ca42)*Y(jp)*Y(jsc45)*state % rho + &
      screened_rates(k_sc42__ca42__weak__wc12)*Y(jsc42) &
       )

    ydot_nuc(jca43) = ( &
      screened_rates(k_he4_ar39__ca43)*Y(jar39)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca43__n_ti46)*Y(jca43)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ca43__ti47)*Y(jca43)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*Y(jk40)*state % rho + &
      screened_rates(k_n_ca42__ca43)*Y(jca42)*Y(jn)*state % rho - &
      screened_rates(k_n_ca43__ca44)*Y(jca43)*Y(jn)*state % rho + &
      screened_rates(k_n_sc43__p_ca43)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_p_ca43__sc44)*Y(jca43)*Y(jp)*state % rho + &
      screened_rates(k_p_k42__ca43)*Y(jk42)*Y(jp)*state % rho + &
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*Y(jsc46)*state % rho + &
      screened_rates(k_sc43__ca43__weak__wc12)*Y(jsc43) &
       )

    ydot_nuc(jca44) = ( &
      -screened_rates(k_he4_ca44__ti48)*Y(jca44)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_k41__p_ca44)*Y(jhe4)*Y(jk41)*state % rho + &
      screened_rates(k_n_ca43__ca44)*Y(jca43)*Y(jn)*state % rho + &
      screened_rates(k_n_sc44__p_ca44)*Y(jn)*Y(jsc44)*state % rho + &
      screened_rates(k_n_ti47__he4_ca44)*Y(jn)*Y(jti47)*state % rho - &
      screened_rates(k_p_ca44__sc45)*Y(jca44)*Y(jp)*state % rho + &
      screened_rates(k_sc44__ca44__weak__wc12)*Y(jsc44) &
       )

    ydot_nuc(jsc42) = ( &
      screened_rates(k_he4_k38__sc42)*Y(jhe4)*Y(jk38)*state % rho - &
      screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*Y(jsc42)*state % rho - &
      screened_rates(k_he4_sc42__v46)*Y(jhe4)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__he4_k39)*Y(jn)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__p_ca42)*Y(jn)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__sc43)*Y(jn)*Y(jsc42)*state % rho + &
      screened_rates(k_p_ca41__sc42)*Y(jca41)*Y(jp)*state % rho - &
      screened_rates(k_p_sc42__he4_ca39)*Y(jp)*Y(jsc42)*state % rho - &
      screened_rates(k_p_sc42__ti43)*Y(jp)*Y(jsc42)*state % rho - &
      screened_rates(k_sc42__ca42__weak__wc12)*Y(jsc42) &
       )

    ydot_nuc(jsc43) = ( &
      screened_rates(k_he4_k39__sc43)*Y(jhe4)*Y(jk39)*state % rho - &
      screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*Y(jsc43)*state % rho - &
      screened_rates(k_he4_sc43__v47)*Y(jhe4)*Y(jsc43)*state % rho + &
      screened_rates(k_n_sc42__sc43)*Y(jn)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc43__he4_k40)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc43__p_ca43)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc43__sc44)*Y(jn)*Y(jsc43)*state % rho + &
      screened_rates(k_n_ti43__p_sc43)*Y(jn)*Y(jti43)*state % rho + &
      screened_rates(k_n_v46__he4_sc43)*Y(jn)*Y(jv46)*state % rho + &
      screened_rates(k_p_ca42__sc43)*Y(jca42)*Y(jp)*state % rho - &
      screened_rates(k_p_sc43__he4_ca40)*Y(jp)*Y(jsc43)*state % rho - &
      screened_rates(k_p_sc43__ti44)*Y(jp)*Y(jsc43)*state % rho - &
      screened_rates(k_sc43__ca43__weak__wc12)*Y(jsc43) + &
      screened_rates(k_ti43__sc43__weak__wc12)*Y(jti43) &
       )

    ydot_nuc(jsc44) = ( &
      screened_rates(k_he4_k40__sc44)*Y(jhe4)*Y(jk40)*state % rho + &
      screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*Y(jk41)*state % rho - &
      screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*Y(jsc44)*state % rho - &
      screened_rates(k_he4_sc44__v48)*Y(jhe4)*Y(jsc44)*state % rho + &
      screened_rates(k_n_sc43__sc44)*Y(jn)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc44__p_ca44)*Y(jn)*Y(jsc44)*state % rho - &
      screened_rates(k_n_sc44__sc45)*Y(jn)*Y(jsc44)*state % rho + &
      screened_rates(k_n_ti44__p_sc44)*Y(jn)*Y(jti44)*state % rho + &
      screened_rates(k_n_v47__he4_sc44)*Y(jn)*Y(jv47)*state % rho + &
      screened_rates(k_p_ca43__sc44)*Y(jca43)*Y(jp)*state % rho - &
      screened_rates(k_p_sc44__he4_ca41)*Y(jp)*Y(jsc44)*state % rho - &
      screened_rates(k_p_sc44__ti45)*Y(jp)*Y(jsc44)*state % rho - &
      screened_rates(k_sc44__ca44__weak__wc12)*Y(jsc44) + &
      screened_rates(k_ti44__sc44__weak__wc12)*Y(jti44) &
       )

    ydot_nuc(jsc45) = ( &
      screened_rates(k_he4_k41__sc45)*Y(jhe4)*Y(jk41)*state % rho + &
      screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*Y(jk42)*state % rho - &
      screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*Y(jsc45)*state % rho - &
      screened_rates(k_he4_sc45__v49)*Y(jhe4)*Y(jsc45)*state % rho + &
      screened_rates(k_n_sc44__sc45)*Y(jn)*Y(jsc44)*state % rho - &
      screened_rates(k_n_sc45__sc46)*Y(jn)*Y(jsc45)*state % rho + &
      screened_rates(k_n_ti45__p_sc45)*Y(jn)*Y(jti45)*state % rho + &
      screened_rates(k_n_v48__he4_sc45)*Y(jn)*Y(jv48)*state % rho + &
      screened_rates(k_p_ca44__sc45)*Y(jca44)*Y(jp)*state % rho - &
      screened_rates(k_p_sc45__he4_ca42)*Y(jp)*Y(jsc45)*state % rho - &
      screened_rates(k_p_sc45__ti46)*Y(jp)*Y(jsc45)*state % rho + &
      screened_rates(k_ti45__sc45__weak__wc12)*Y(jti45) &
       )

    ydot_nuc(jsc46) = ( &
      screened_rates(k_he4_k42__sc46)*Y(jhe4)*Y(jk42)*state % rho - &
      screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_sc46__p_ti49)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_sc46__v50)*Y(jhe4)*Y(jsc46)*state % rho + &
      screened_rates(k_n_sc45__sc46)*Y(jn)*Y(jsc45)*state % rho - &
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_sc46__n_ti46)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_sc46__ti47)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_sc46__ti46__weak__wc12)*Y(jsc46) &
       )

    ydot_nuc(jti43) = ( &
      screened_rates(k_he4_ca39__ti43)*Y(jca39)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ti43__cr47)*Y(jhe4)*Y(jti43)*state % rho - &
      screened_rates(k_he4_ti43__p_v46)*Y(jhe4)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__he4_ca40)*Y(jn)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__p_sc43)*Y(jn)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__ti44)*Y(jn)*Y(jti43)*state % rho + &
      screened_rates(k_p_sc42__ti43)*Y(jp)*Y(jsc42)*state % rho - &
      screened_rates(k_ti43__sc43__weak__wc12)*Y(jti43) &
       )

    ydot_nuc(jti44) = ( &
      screened_rates(k_he4_ca40__ti44)*Y(jca40)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_ti44__cr48)*Y(jhe4)*Y(jti44)*state % rho - &
      screened_rates(k_he4_ti44__p_v47)*Y(jhe4)*Y(jti44)*state % rho + &
      screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*Y(jn)*state % rho + &
      screened_rates(k_n_ti43__ti44)*Y(jn)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti44__he4_ca41)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__p_sc44)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__ti45)*Y(jn)*Y(jti44)*state % rho + &
      screened_rates(k_p_sc43__ti44)*Y(jp)*Y(jsc43)*state % rho - &
      screened_rates(k_ti44__sc44__weak__wc12)*Y(jti44) &
       )

    ydot_nuc(jti45) = ( &
      screened_rates(k_he4_ca41__ti45)*Y(jca41)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*Y(jsc42)*state % rho - &
      screened_rates(k_he4_ti45__cr49)*Y(jhe4)*Y(jti45)*state % rho - &
      screened_rates(k_he4_ti45__p_v48)*Y(jhe4)*Y(jti45)*state % rho + &
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*Y(jn)*state % rho + &
      screened_rates(k_n_ti44__ti45)*Y(jn)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti45__he4_ca42)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__p_sc45)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__ti46)*Y(jn)*Y(jti45)*state % rho + &
      screened_rates(k_p_sc44__ti45)*Y(jp)*Y(jsc44)*state % rho - &
      screened_rates(k_p_ti45__v46)*Y(jp)*Y(jti45)*state % rho - &
      screened_rates(k_ti45__sc45__weak__wc12)*Y(jti45) &
       )

    ydot_nuc(jti46) = ( &
      screened_rates(k_he4_ca42__ti46)*Y(jca42)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_ca43__n_ti46)*Y(jca43)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*Y(jsc43)*state % rho - &
      screened_rates(k_he4_ti46__cr50)*Y(jhe4)*Y(jti46)*state % rho + &
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*Y(jn)*state % rho + &
      screened_rates(k_n_ti45__ti46)*Y(jn)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti46__ti47)*Y(jn)*Y(jti46)*state % rho + &
      screened_rates(k_n_v46__p_ti46)*Y(jn)*Y(jv46)*state % rho + &
      screened_rates(k_p_sc45__ti46)*Y(jp)*Y(jsc45)*state % rho + &
      screened_rates(k_p_sc46__n_ti46)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_ti46__v47)*Y(jp)*Y(jti46)*state % rho + &
      screened_rates(k_p_v49__he4_ti46)*Y(jp)*Y(jv49)*state % rho + &
      screened_rates(k_sc46__ti46__weak__wc12)*Y(jsc46) + &
      screened_rates(k_v46__ti46__weak__wc12)*Y(jv46) &
       )

    ydot_nuc(jti47) = ( &
      screened_rates(k_he4_ca43__ti47)*Y(jca43)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*Y(jsc44)*state % rho - &
      screened_rates(k_he4_ti47__cr51)*Y(jhe4)*Y(jti47)*state % rho + &
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*Y(jn)*state % rho + &
      screened_rates(k_n_ti46__ti47)*Y(jn)*Y(jti46)*state % rho - &
      screened_rates(k_n_ti47__he4_ca44)*Y(jn)*Y(jti47)*state % rho - &
      screened_rates(k_n_ti47__ti48)*Y(jn)*Y(jti47)*state % rho + &
      screened_rates(k_n_v47__p_ti47)*Y(jn)*Y(jv47)*state % rho + &
      screened_rates(k_p_sc46__ti47)*Y(jp)*Y(jsc46)*state % rho - &
      screened_rates(k_p_ti47__v48)*Y(jp)*Y(jti47)*state % rho + &
      screened_rates(k_p_v50__he4_ti47)*Y(jp)*Y(jv50)*state % rho + &
      screened_rates(k_v47__ti47__weak__wc12)*Y(jv47) &
       )

    ydot_nuc(jti48) = ( &
      screened_rates(k_he4_ca44__ti48)*Y(jca44)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*Y(jsc45)*state % rho - &
      screened_rates(k_he4_ti48__cr52)*Y(jhe4)*Y(jti48)*state % rho + &
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*Y(jn)*state % rho + &
      screened_rates(k_n_ti47__ti48)*Y(jn)*Y(jti47)*state % rho - &
      screened_rates(k_n_ti48__ti49)*Y(jn)*Y(jti48)*state % rho + &
      screened_rates(k_n_v48__p_ti48)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_p_ti48__v49)*Y(jp)*Y(jti48)*state % rho + &
      screened_rates(k_p_v51__he4_ti48)*Y(jp)*Y(jv51)*state % rho + &
      screened_rates(k_v48__ti48__weak__wc12)*Y(jv48) &
       )

    ydot_nuc(jti49) = ( &
      screened_rates(k_he4_sc46__p_ti49)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*Y(jti49)*state % rho + &
      screened_rates(k_n_ti48__ti49)*Y(jn)*Y(jti48)*state % rho + &
      screened_rates(k_n_v49__p_ti49)*Y(jn)*Y(jv49)*state % rho - &
      screened_rates(k_p_ti49__v50)*Y(jp)*Y(jti49)*state % rho + &
      screened_rates(k_v49__ti49__weak__wc12)*Y(jv49) &
       )

    ydot_nuc(jv46) = ( &
      screened_rates(k_he4_sc42__v46)*Y(jhe4)*Y(jsc42)*state % rho + &
      screened_rates(k_he4_ti43__p_v46)*Y(jhe4)*Y(jti43)*state % rho - &
      screened_rates(k_he4_v46__mn50)*Y(jhe4)*Y(jv46)*state % rho - &
      screened_rates(k_he4_v46__p_cr49)*Y(jhe4)*Y(jv46)*state % rho + &
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_v46__he4_sc43)*Y(jn)*Y(jv46)*state % rho - &
      screened_rates(k_n_v46__p_ti46)*Y(jn)*Y(jv46)*state % rho - &
      screened_rates(k_n_v46__v47)*Y(jn)*Y(jv46)*state % rho + &
      screened_rates(k_p_ti45__v46)*Y(jp)*Y(jti45)*state % rho - &
      screened_rates(k_p_v46__cr47)*Y(jp)*Y(jv46)*state % rho - &
      screened_rates(k_v46__ti46__weak__wc12)*Y(jv46) &
       )

    ydot_nuc(jv47) = ( &
      screened_rates(k_cr47__v47__weak__wc12)*Y(jcr47) + screened_rates(k_he4_sc43__v47)* &
      Y(jhe4)*Y(jsc43)*state % rho + screened_rates(k_he4_ti44__p_v47)* &
      Y(jhe4)*Y(jti44)*state % rho - screened_rates(k_he4_v47__mn51)*Y(jhe4)* &
      Y(jv47)*state % rho - screened_rates(k_he4_v47__p_cr50)*Y(jhe4)*Y(jv47) &
      *state % rho + screened_rates(k_n_cr47__p_v47)*Y(jcr47)*Y(jn)*state % rho + &
      screened_rates(k_n_mn50__he4_v47)*Y(jmn50)*Y(jn)*state % rho + &
      screened_rates(k_n_v46__v47)*Y(jn)*Y(jv46)*state % rho - &
      screened_rates(k_n_v47__he4_sc44)*Y(jn)*Y(jv47)*state % rho - &
      screened_rates(k_n_v47__p_ti47)*Y(jn)*Y(jv47)*state % rho - &
      screened_rates(k_n_v47__v48)*Y(jn)*Y(jv47)*state % rho + &
      screened_rates(k_p_ti46__v47)*Y(jp)*Y(jti46)*state % rho - &
      screened_rates(k_p_v47__cr48)*Y(jp)*Y(jv47)*state % rho - &
      screened_rates(k_v47__ti47__weak__wc12)*Y(jv47) &
       )

    ydot_nuc(jv48) = ( &
      screened_rates(k_cr48__v48__weak__wc12)*Y(jcr48) + screened_rates(k_he4_sc44__v48)* &
      Y(jhe4)*Y(jsc44)*state % rho + screened_rates(k_he4_ti45__p_v48)* &
      Y(jhe4)*Y(jti45)*state % rho - screened_rates(k_he4_v48__mn52)*Y(jhe4)* &
      Y(jv48)*state % rho - screened_rates(k_he4_v48__p_cr51)*Y(jhe4)*Y(jv48) &
      *state % rho + screened_rates(k_n_cr48__p_v48)*Y(jcr48)*Y(jn)*state % rho + &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*Y(jn)*state % rho + &
      screened_rates(k_n_v47__v48)*Y(jn)*Y(jv47)*state % rho - &
      screened_rates(k_n_v48__he4_sc45)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_n_v48__p_ti48)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_n_v48__v49)*Y(jn)*Y(jv48)*state % rho + &
      screened_rates(k_p_ti47__v48)*Y(jp)*Y(jti47)*state % rho - &
      screened_rates(k_p_v48__cr49)*Y(jp)*Y(jv48)*state % rho - &
      screened_rates(k_v48__ti48__weak__wc12)*Y(jv48) &
       )

    ydot_nuc(jv49) = ( &
      screened_rates(k_cr49__v49__weak__wc12)*Y(jcr49) + screened_rates(k_he4_sc45__v49)* &
      Y(jhe4)*Y(jsc45)*state % rho + screened_rates(k_he4_sc46__n_v49)* &
      Y(jhe4)*Y(jsc46)*state % rho - screened_rates(k_he4_v49__mn53)*Y(jhe4)* &
      Y(jv49)*state % rho - screened_rates(k_he4_v49__p_cr52)*Y(jhe4)*Y(jv49) &
      *state % rho + screened_rates(k_n_cr49__p_v49)*Y(jcr49)*Y(jn)*state % rho + &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*Y(jn)*state % rho + &
      screened_rates(k_n_v48__v49)*Y(jn)*Y(jv48)*state % rho - &
      screened_rates(k_n_v49__p_ti49)*Y(jn)*Y(jv49)*state % rho - &
      screened_rates(k_n_v49__v50)*Y(jn)*Y(jv49)*state % rho + &
      screened_rates(k_p_ti48__v49)*Y(jp)*Y(jti48)*state % rho - &
      screened_rates(k_p_v49__cr50)*Y(jp)*Y(jv49)*state % rho - &
      screened_rates(k_p_v49__he4_ti46)*Y(jp)*Y(jv49)*state % rho - &
      screened_rates(k_v49__ti49__weak__wc12)*Y(jv49) &
       )

    ydot_nuc(jv50) = ( &
      screened_rates(k_he4_sc46__v50)*Y(jhe4)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_v50__mn54)*Y(jhe4)*Y(jv50)*state % rho + &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*Y(jn)*state % rho + &
      screened_rates(k_n_v49__v50)*Y(jn)*Y(jv49)*state % rho - &
      screened_rates(k_n_v50__v51)*Y(jn)*Y(jv50)*state % rho + &
      screened_rates(k_p_ti49__v50)*Y(jp)*Y(jti49)*state % rho - &
      screened_rates(k_p_v50__cr51)*Y(jp)*Y(jv50)*state % rho - &
      screened_rates(k_p_v50__he4_ti47)*Y(jp)*Y(jv50)*state % rho - &
      screened_rates(k_p_v50__n_cr50)*Y(jp)*Y(jv50)*state % rho - &
      screened_rates(k_v50__cr50__weak__wc12)*Y(jv50) &
       )

    ydot_nuc(jv51) = ( &
      screened_rates(k_cr51__v51__weak__wc12)*Y(jcr51) - screened_rates(k_he4_v51__mn55)* &
      Y(jhe4)*Y(jv51)*state % rho + screened_rates(k_n_cr51__p_v51)*Y(jcr51)* &
      Y(jn)*state % rho + screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*Y(jn)* &
      state % rho + screened_rates(k_n_v50__v51)*Y(jn)*Y(jv50)*state % rho - &
      screened_rates(k_p_v51__cr52)*Y(jp)*Y(jv51)*state % rho - &
      screened_rates(k_p_v51__he4_ti48)*Y(jp)*Y(jv51)*state % rho &
       )

    ydot_nuc(jcr47) = ( &
      -screened_rates(k_cr47__v47__weak__wc12)*Y(jcr47) - screened_rates(k_he4_cr47__fe51)* &
      Y(jcr47)*Y(jhe4)*state % rho - screened_rates(k_he4_cr47__p_mn50)* &
      Y(jcr47)*Y(jhe4)*state % rho + screened_rates(k_he4_ti43__cr47)*Y(jhe4) &
      *Y(jti43)*state % rho - screened_rates(k_n_cr47__cr48)*Y(jcr47)*Y(jn)* &
      state % rho - screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*Y(jn)*state % rho &
      - screened_rates(k_n_cr47__p_v47)*Y(jcr47)*Y(jn)*state % rho + &
      screened_rates(k_p_v46__cr47)*Y(jp)*Y(jv46)*state % rho &
       )

    ydot_nuc(jcr48) = ( &
      -screened_rates(k_cr48__v48__weak__wc12)*Y(jcr48) - screened_rates(k_he4_cr48__fe52)* &
      Y(jcr48)*Y(jhe4)*state % rho - screened_rates(k_he4_cr48__p_mn51)* &
      Y(jcr48)*Y(jhe4)*state % rho + screened_rates(k_he4_ti44__cr48)*Y(jhe4) &
      *Y(jti44)*state % rho + screened_rates(k_n_cr47__cr48)*Y(jcr47)*Y(jn)* &
      state % rho - screened_rates(k_n_cr48__cr49)*Y(jcr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__p_v48)*Y(jcr48)*Y(jn)*state % rho + &
      screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*Y(jn)*state % rho - &
      screened_rates(k_p_cr48__mn49)*Y(jcr48)*Y(jp)*state % rho + &
      screened_rates(k_p_v47__cr48)*Y(jp)*Y(jv47)*state % rho &
       )

    ydot_nuc(jcr49) = ( &
      -screened_rates(k_cr49__v49__weak__wc12)*Y(jcr49) - screened_rates(k_he4_cr49__fe53)* &
      Y(jcr49)*Y(jhe4)*state % rho - screened_rates(k_he4_cr49__p_mn52)* &
      Y(jcr49)*Y(jhe4)*state % rho + screened_rates(k_he4_ti45__cr49)*Y(jhe4) &
      *Y(jti45)*state % rho + screened_rates(k_he4_v46__p_cr49)*Y(jhe4)* &
      Y(jv46)*state % rho + screened_rates(k_mn49__cr49__weak__wc12)*Y(jmn49) + &
      screened_rates(k_n_cr48__cr49)*Y(jcr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__cr50)*Y(jcr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__p_v49)*Y(jcr49)*Y(jn)*state % rho + &
      screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*Y(jn)*state % rho + &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_p_cr49__mn50)*Y(jcr49)*Y(jp)*state % rho + &
      screened_rates(k_p_v48__cr49)*Y(jp)*Y(jv48)*state % rho &
       )

    ydot_nuc(jcr50) = ( &
      -screened_rates(k_he4_cr50__fe54)*Y(jcr50)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_ti46__cr50)*Y(jhe4)*Y(jti46)*state % rho + &
      screened_rates(k_he4_v47__p_cr50)*Y(jhe4)*Y(jv47)*state % rho + &
      screened_rates(k_mn50__cr50__weak__wc12)*Y(jmn50) + &
      screened_rates(k_n_cr49__cr50)*Y(jcr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr50__cr51)*Y(jcr50)*Y(jn)*state % rho - &
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*Y(jn)*state % rho + &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*Y(jn)*state % rho + &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_p_cr50__mn51)*Y(jcr50)*Y(jp)*state % rho + &
      screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)*Y(jp)*state % rho + &
      screened_rates(k_p_v49__cr50)*Y(jp)*Y(jv49)*state % rho + &
      screened_rates(k_p_v50__n_cr50)*Y(jp)*Y(jv50)*state % rho + &
      screened_rates(k_v50__cr50__weak__wc12)*Y(jv50) &
       )

    ydot_nuc(jcr51) = ( &
      -screened_rates(k_cr51__v51__weak__wc12)*Y(jcr51) - screened_rates(k_he4_cr51__fe55)* &
      Y(jcr51)*Y(jhe4)*state % rho + screened_rates(k_he4_ti47__cr51)*Y(jhe4) &
      *Y(jti47)*state % rho + screened_rates(k_he4_v48__p_cr51)*Y(jhe4)* &
      Y(jv48)*state % rho + screened_rates(k_mn51__cr51__weak__wc12)*Y(jmn51) + &
      screened_rates(k_n_cr50__cr51)*Y(jcr50)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__cr52)*Y(jcr51)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__p_v51)*Y(jcr51)*Y(jn)*state % rho + &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*Y(jn)*state % rho + &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_p_cr51__mn52)*Y(jcr51)*Y(jp)*state % rho + &
      screened_rates(k_p_mn54__he4_cr51)*Y(jmn54)*Y(jp)*state % rho + &
      screened_rates(k_p_v50__cr51)*Y(jp)*Y(jv50)*state % rho &
       )

    ydot_nuc(jcr52) = ( &
      -screened_rates(k_he4_cr52__fe56)*Y(jcr52)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_ti48__cr52)*Y(jhe4)*Y(jti48)*state % rho + &
      screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*Y(jti49)*state % rho + &
      screened_rates(k_he4_v49__p_cr52)*Y(jhe4)*Y(jv49)*state % rho + &
      screened_rates(k_mn52__cr52__weak__wc12)*Y(jmn52) + &
      screened_rates(k_n_cr51__cr52)*Y(jcr51)*Y(jn)*state % rho + &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*Y(jn)*state % rho + &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_p_cr52__mn53)*Y(jcr52)*Y(jp)*state % rho + &
      screened_rates(k_p_mn55__he4_cr52)*Y(jmn55)*Y(jp)*state % rho + &
      screened_rates(k_p_v51__cr52)*Y(jp)*Y(jv51)*state % rho &
       )

    ydot_nuc(jmn49) = ( &
      -screened_rates(k_he4_mn49__co53)*Y(jhe4)*Y(jmn49)*state % rho - &
      screened_rates(k_he4_mn49__p_fe52)*Y(jhe4)*Y(jmn49)*state % rho - &
      screened_rates(k_mn49__cr49__weak__wc12)*Y(jmn49) - &
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_mn49__mn50)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*Y(jn)*state % rho + &
      screened_rates(k_p_cr48__mn49)*Y(jcr48)*Y(jp)*state % rho &
       )

    ydot_nuc(jmn50) = ( &
      screened_rates(k_he4_cr47__p_mn50)*Y(jcr47)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_mn50__co54)*Y(jhe4)*Y(jmn50)*state % rho - &
      screened_rates(k_he4_mn50__p_fe53)*Y(jhe4)*Y(jmn50)*state % rho + &
      screened_rates(k_he4_v46__mn50)*Y(jhe4)*Y(jv46)*state % rho - &
      screened_rates(k_mn50__cr50__weak__wc12)*Y(jmn50) + &
      screened_rates(k_n_co53__he4_mn50)*Y(jco53)*Y(jn)*state % rho + &
      screened_rates(k_n_mn49__mn50)*Y(jmn49)*Y(jn)*state % rho - &
      screened_rates(k_n_mn50__he4_v47)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn50__mn51)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*Y(jn)*state % rho + &
      screened_rates(k_p_cr49__mn50)*Y(jcr49)*Y(jp)*state % rho - &
      screened_rates(k_p_mn50__fe51)*Y(jmn50)*Y(jp)*state % rho &
       )

    ydot_nuc(jmn51) = ( &
      screened_rates(k_fe51__mn51__weak__wc12)*Y(jfe51) + screened_rates(k_he4_cr48__p_mn51)* &
      Y(jcr48)*Y(jhe4)*state % rho - screened_rates(k_he4_mn51__co55)*Y(jhe4) &
      *Y(jmn51)*state % rho - screened_rates(k_he4_mn51__p_fe54)*Y(jhe4)* &
      Y(jmn51)*state % rho + screened_rates(k_he4_v47__mn51)*Y(jhe4)*Y(jv47)* &
      state % rho - screened_rates(k_mn51__cr51__weak__wc12)*Y(jmn51) + &
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*Y(jn)*state % rho + &
      screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*Y(jn)*state % rho + &
      screened_rates(k_n_mn50__mn51)*Y(jmn50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_n_mn51__mn52)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*Y(jn)*state % rho + &
      screened_rates(k_p_cr50__mn51)*Y(jcr50)*Y(jp)*state % rho - &
      screened_rates(k_p_mn51__fe52)*Y(jmn51)*Y(jp)*state % rho &
       )

    ydot_nuc(jmn52) = ( &
      screened_rates(k_fe52__mn52__weak__wc12)*Y(jfe52) + screened_rates(k_he4_cr49__p_mn52)* &
      Y(jcr49)*Y(jhe4)*state % rho - screened_rates(k_he4_mn52__co56)*Y(jhe4) &
      *Y(jmn52)*state % rho - screened_rates(k_he4_mn52__p_fe55)*Y(jhe4)* &
      Y(jmn52)*state % rho + screened_rates(k_he4_v48__mn52)*Y(jhe4)*Y(jv48)* &
      state % rho - screened_rates(k_mn52__cr52__weak__wc12)*Y(jmn52) + &
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*Y(jn)*state % rho + &
      screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*Y(jn)*state % rho + &
      screened_rates(k_n_mn51__mn52)*Y(jmn51)*Y(jn)*state % rho - &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_n_mn52__mn53)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*Y(jn)*state % rho + &
      screened_rates(k_p_cr51__mn52)*Y(jcr51)*Y(jp)*state % rho - &
      screened_rates(k_p_mn52__fe53)*Y(jmn52)*Y(jp)*state % rho &
       )

    ydot_nuc(jmn53) = ( &
      screened_rates(k_fe53__mn53__weak__wc12)*Y(jfe53) - screened_rates(k_he4_mn53__co57)* &
      Y(jhe4)*Y(jmn53)*state % rho - screened_rates(k_he4_mn53__p_fe56)* &
      Y(jhe4)*Y(jmn53)*state % rho + screened_rates(k_he4_v49__mn53)*Y(jhe4)* &
      Y(jv49)*state % rho + screened_rates(k_n_co56__he4_mn53)*Y(jco56)*Y(jn) &
      *state % rho + screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*Y(jn)*state % rho + &
      screened_rates(k_n_mn52__mn53)*Y(jmn52)*Y(jn)*state % rho - &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*Y(jn)*state % rho - &
      screened_rates(k_n_mn53__mn54)*Y(jmn53)*Y(jn)*state % rho + &
      screened_rates(k_p_cr52__mn53)*Y(jcr52)*Y(jp)*state % rho - &
      screened_rates(k_p_mn53__fe54)*Y(jmn53)*Y(jp)*state % rho - &
      screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)*Y(jp)*state % rho &
       )

    ydot_nuc(jmn54) = ( &
      -screened_rates(k_he4_mn54__co58)*Y(jhe4)*Y(jmn54)*state % rho + &
      screened_rates(k_he4_v50__mn54)*Y(jhe4)*Y(jv50)*state % rho - &
      screened_rates(k_mn54__fe54__weak__wc12)*Y(jmn54) + &
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*Y(jn)*state % rho + &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*Y(jn)*state % rho + &
      screened_rates(k_n_mn53__mn54)*Y(jmn53)*Y(jn)*state % rho - &
      screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*Y(jn)*state % rho - &
      screened_rates(k_n_mn54__mn55)*Y(jmn54)*Y(jn)*state % rho - &
      screened_rates(k_p_mn54__fe55)*Y(jmn54)*Y(jp)*state % rho - &
      screened_rates(k_p_mn54__he4_cr51)*Y(jmn54)*Y(jp)*state % rho &
       )

    ydot_nuc(jmn55) = ( &
      screened_rates(k_fe55__mn55__weak__wc12)*Y(jfe55) + screened_rates(k_he4_v51__mn55)* &
      Y(jhe4)*Y(jv51)*state % rho + screened_rates(k_n_co58__he4_mn55)* &
      Y(jco58)*Y(jn)*state % rho + screened_rates(k_n_fe55__p_mn55)*Y(jfe55)* &
      Y(jn)*state % rho + screened_rates(k_n_mn54__mn55)*Y(jmn54)*Y(jn)* &
      state % rho - screened_rates(k_p_mn55__fe56)*Y(jmn55)*Y(jp)*state % rho - &
      screened_rates(k_p_mn55__he4_cr52)*Y(jmn55)*Y(jp)*state % rho &
       )

    ydot_nuc(jfe51) = ( &
      -screened_rates(k_fe51__mn51__weak__wc12)*Y(jfe51) + screened_rates(k_he4_cr47__fe51)* &
      Y(jcr47)*Y(jhe4)*state % rho - screened_rates(k_he4_fe51__ni55)* &
      Y(jfe51)*Y(jhe4)*state % rho - screened_rates(k_he4_fe51__p_co54)* &
      Y(jfe51)*Y(jhe4)*state % rho - screened_rates(k_n_fe51__fe52)*Y(jfe51)* &
      Y(jn)*state % rho - screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*Y(jn)* &
      state % rho - screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*Y(jn)*state % rho + &
      screened_rates(k_n_ni54__he4_fe51)*Y(jn)*Y(jni54)*state % rho + &
      screened_rates(k_p_mn50__fe51)*Y(jmn50)*Y(jp)*state % rho &
       )

    ydot_nuc(jfe52) = ( &
      -screened_rates(k_fe52__mn52__weak__wc12)*Y(jfe52) + screened_rates(k_he4_cr48__fe52)* &
      Y(jcr48)*Y(jhe4)*state % rho - screened_rates(k_he4_fe52__ni56)* &
      Y(jfe52)*Y(jhe4)*state % rho - screened_rates(k_he4_fe52__p_co55)* &
      Y(jfe52)*Y(jhe4)*state % rho + screened_rates(k_he4_mn49__p_fe52)* &
      Y(jhe4)*Y(jmn49)*state % rho + screened_rates(k_n_fe51__fe52)*Y(jfe51)* &
      Y(jn)*state % rho - screened_rates(k_n_fe52__fe53)*Y(jfe52)*Y(jn)* &
      state % rho - screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*Y(jn)*state % rho &
      - screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*Y(jn)*state % rho + &
      screened_rates(k_n_ni55__he4_fe52)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_p_fe52__co53)*Y(jfe52)*Y(jp)*state % rho + &
      screened_rates(k_p_mn51__fe52)*Y(jmn51)*Y(jp)*state % rho &
       )

    ydot_nuc(jfe53) = ( &
      screened_rates(k_co53__fe53__weak__wc12)*Y(jco53) - &
      screened_rates(k_fe53__mn53__weak__wc12)*Y(jfe53) + &
      screened_rates(k_he4_cr49__fe53)*Y(jcr49)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe53__ni57)*Y(jfe53)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe53__p_co56)*Y(jfe53)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_mn50__p_fe53)*Y(jhe4)*Y(jmn50)*state % rho + &
      screened_rates(k_n_co53__p_fe53)*Y(jco53)*Y(jn)*state % rho + &
      screened_rates(k_n_fe52__fe53)*Y(jfe52)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__fe54)*Y(jfe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*Y(jn)*state % rho + &
      screened_rates(k_n_ni56__he4_fe53)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_p_fe53__co54)*Y(jfe53)*Y(jp)*state % rho + &
      screened_rates(k_p_mn52__fe53)*Y(jmn52)*Y(jp)*state % rho &
       )

    ydot_nuc(jfe54) = ( &
      screened_rates(k_co54__fe54__weak__wc12)*Y(jco54) + screened_rates(k_he4_cr50__fe54)* &
      Y(jcr50)*Y(jhe4)*state % rho - screened_rates(k_he4_fe54__ni58)* &
      Y(jfe54)*Y(jhe4)*state % rho + screened_rates(k_he4_mn51__p_fe54)* &
      Y(jhe4)*Y(jmn51)*state % rho + screened_rates(k_mn54__fe54__weak__wc12)* &
      Y(jmn54) + screened_rates(k_n_co54__p_fe54)*Y(jco54)*Y(jn)*state % rho &
      + screened_rates(k_n_fe53__fe54)*Y(jfe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__fe55)*Y(jfe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*Y(jn)*state % rho + &
      screened_rates(k_n_ni57__he4_fe54)*Y(jn)*Y(jni57)*state % rho + &
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*Y(jp)*state % rho - &
      screened_rates(k_p_fe54__co55)*Y(jfe54)*Y(jp)*state % rho + &
      screened_rates(k_p_mn53__fe54)*Y(jmn53)*Y(jp)*state % rho &
       )

    ydot_nuc(jfe55) = ( &
      screened_rates(k_co55__fe55__weak__wc12)*Y(jco55) - &
      screened_rates(k_fe55__mn55__weak__wc12)*Y(jfe55) + &
      screened_rates(k_he4_cr51__fe55)*Y(jcr51)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_fe55__ni59)*Y(jfe55)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_mn52__p_fe55)*Y(jhe4)*Y(jmn52)*state % rho + &
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*Y(jn)*state % rho + &
      screened_rates(k_n_fe54__fe55)*Y(jfe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__fe56)*Y(jfe55)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__p_mn55)*Y(jfe55)*Y(jn)*state % rho + &
      screened_rates(k_n_ni58__he4_fe55)*Y(jn)*Y(jni58)*state % rho + &
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*Y(jp)*state % rho - &
      screened_rates(k_p_fe55__co56)*Y(jfe55)*Y(jp)*state % rho + &
      screened_rates(k_p_mn54__fe55)*Y(jmn54)*Y(jp)*state % rho &
       )

    ydot_nuc(jfe56) = ( &
      screened_rates(k_co56__fe56__weak__wc12)*Y(jco56) + screened_rates(k_he4_cr52__fe56)* &
      Y(jcr52)*Y(jhe4)*state % rho - screened_rates(k_he4_fe56__ni60)* &
      Y(jfe56)*Y(jhe4)*state % rho + screened_rates(k_he4_mn53__p_fe56)* &
      Y(jhe4)*Y(jmn53)*state % rho + screened_rates(k_n_co56__p_fe56)* &
      Y(jco56)*Y(jn)*state % rho + screened_rates(k_n_fe55__fe56)*Y(jfe55)* &
      Y(jn)*state % rho + screened_rates(k_n_ni59__he4_fe56)*Y(jn)*Y(jni59)* &
      state % rho - screened_rates(k_p_fe56__co57)*Y(jfe56)*Y(jp)*state % rho + &
      screened_rates(k_p_mn55__fe56)*Y(jmn55)*Y(jp)*state % rho &
       )

    ydot_nuc(jco53) = ( &
      -screened_rates(k_co53__fe53__weak__wc12)*Y(jco53) - screened_rates(k_he4_co53__p_ni56)* &
      Y(jco53)*Y(jhe4)*state % rho + screened_rates(k_he4_mn49__co53)*Y(jhe4) &
      *Y(jmn49)*state % rho - screened_rates(k_n_co53__co54)*Y(jco53)*Y(jn)* &
      state % rho - screened_rates(k_n_co53__he4_mn50)*Y(jco53)*Y(jn)*state % rho &
      - screened_rates(k_n_co53__p_fe53)*Y(jco53)*Y(jn)*state % rho - &
      screened_rates(k_p_co53__ni54)*Y(jco53)*Y(jp)*state % rho + &
      screened_rates(k_p_fe52__co53)*Y(jfe52)*Y(jp)*state % rho &
       )

    ydot_nuc(jco54) = ( &
      -screened_rates(k_co54__fe54__weak__wc12)*Y(jco54) - screened_rates(k_he4_co54__p_ni57)* &
      Y(jco54)*Y(jhe4)*state % rho + screened_rates(k_he4_fe51__p_co54)* &
      Y(jfe51)*Y(jhe4)*state % rho + screened_rates(k_he4_mn50__co54)*Y(jhe4) &
      *Y(jmn50)*state % rho + screened_rates(k_n_co53__co54)*Y(jco53)*Y(jn)* &
      state % rho - screened_rates(k_n_co54__co55)*Y(jco54)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__p_fe54)*Y(jco54)*Y(jn)*state % rho + &
      screened_rates(k_n_ni54__p_co54)*Y(jn)*Y(jni54)*state % rho + &
      screened_rates(k_ni54__co54__weak__wc12)*Y(jni54) - &
      screened_rates(k_p_co54__ni55)*Y(jco54)*Y(jp)*state % rho + &
      screened_rates(k_p_fe53__co54)*Y(jfe53)*Y(jp)*state % rho &
       )

    ydot_nuc(jco55) = ( &
      -screened_rates(k_co55__fe55__weak__wc12)*Y(jco55) - screened_rates(k_he4_co55__p_ni58)* &
      Y(jco55)*Y(jhe4)*state % rho + screened_rates(k_he4_fe52__p_co55)* &
      Y(jfe52)*Y(jhe4)*state % rho + screened_rates(k_he4_mn51__co55)*Y(jhe4) &
      *Y(jmn51)*state % rho + screened_rates(k_n_co54__co55)*Y(jco54)*Y(jn)* &
      state % rho - screened_rates(k_n_co55__co56)*Y(jco55)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*Y(jn)*state % rho + &
      screened_rates(k_n_ni55__p_co55)*Y(jn)*Y(jni55)*state % rho + &
      screened_rates(k_ni55__co55__weak__wc12)*Y(jni55) - &
      screened_rates(k_p_co55__ni56)*Y(jco55)*Y(jp)*state % rho + &
      screened_rates(k_p_fe54__co55)*Y(jfe54)*Y(jp)*state % rho &
       )

    ydot_nuc(jco56) = ( &
      -screened_rates(k_co56__fe56__weak__wc12)*Y(jco56) - screened_rates(k_he4_co56__p_ni59)* &
      Y(jco56)*Y(jhe4)*state % rho + screened_rates(k_he4_fe53__p_co56)* &
      Y(jfe53)*Y(jhe4)*state % rho + screened_rates(k_he4_mn52__co56)*Y(jhe4) &
      *Y(jmn52)*state % rho + screened_rates(k_n_co55__co56)*Y(jco55)*Y(jn)* &
      state % rho - screened_rates(k_n_co56__co57)*Y(jco56)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__he4_mn53)*Y(jco56)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__p_fe56)*Y(jco56)*Y(jn)*state % rho + &
      screened_rates(k_n_ni56__p_co56)*Y(jn)*Y(jni56)*state % rho + &
      screened_rates(k_ni56__co56__weak__wc12)*Y(jni56) - &
      screened_rates(k_p_co56__ni57)*Y(jco56)*Y(jp)*state % rho + &
      screened_rates(k_p_fe55__co56)*Y(jfe55)*Y(jp)*state % rho &
       )

    ydot_nuc(jco57) = ( &
      -screened_rates(k_he4_co57__p_ni60)*Y(jco57)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_mn53__co57)*Y(jhe4)*Y(jmn53)*state % rho + &
      screened_rates(k_n_co56__co57)*Y(jco56)*Y(jn)*state % rho - &
      screened_rates(k_n_co57__co58)*Y(jco57)*Y(jn)*state % rho - &
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*Y(jn)*state % rho + &
      screened_rates(k_n_ni57__p_co57)*Y(jn)*Y(jni57)*state % rho + &
      screened_rates(k_ni57__co57__weak__wc12)*Y(jni57) - &
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*Y(jp)*state % rho - &
      screened_rates(k_p_co57__ni58)*Y(jco57)*Y(jp)*state % rho + &
      screened_rates(k_p_fe56__co57)*Y(jfe56)*Y(jp)*state % rho &
       )

    ydot_nuc(jco58) = ( &
      -screened_rates(k_co58__ni58__weak__mo03)*Y(jco58) + screened_rates(k_he4_mn54__co58)* &
      Y(jhe4)*Y(jmn54)*state % rho + screened_rates(k_n_co57__co58)*Y(jco57)* &
      Y(jn)*state % rho - screened_rates(k_n_co58__he4_mn55)*Y(jco58)*Y(jn)* &
      state % rho + screened_rates(k_n_ni58__p_co58)*Y(jn)*Y(jni58)*state % rho - &
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*Y(jp)*state % rho - &
      screened_rates(k_p_co58__ni59)*Y(jco58)*Y(jp)*state % rho &
       )

    ydot_nuc(jni54) = ( &
      -screened_rates(k_n_ni54__he4_fe51)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni54__ni55)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni54__p_co54)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_ni54__co54__weak__wc12)*Y(jni54) + &
      screened_rates(k_p_co53__ni54)*Y(jco53)*Y(jp)*state % rho &
       )

    ydot_nuc(jni55) = ( &
      screened_rates(k_he4_fe51__ni55)*Y(jfe51)*Y(jhe4)*state % rho + &
      screened_rates(k_n_ni54__ni55)*Y(jn)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni55__he4_fe52)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__ni56)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__p_co55)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_ni55__co55__weak__wc12)*Y(jni55) + &
      screened_rates(k_p_co54__ni55)*Y(jco54)*Y(jp)*state % rho &
       )

    ydot_nuc(jni56) = ( &
      screened_rates(k_he4_co53__p_ni56)*Y(jco53)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe52__ni56)*Y(jfe52)*Y(jhe4)*state % rho + &
      screened_rates(k_n_ni55__ni56)*Y(jn)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni56__he4_fe53)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__ni57)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__p_co56)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_ni56__co56__weak__wc12)*Y(jni56) + &
      screened_rates(k_p_co55__ni56)*Y(jco55)*Y(jp)*state % rho &
       )

    ydot_nuc(jni57) = ( &
      screened_rates(k_he4_co54__p_ni57)*Y(jco54)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe53__ni57)*Y(jfe53)*Y(jhe4)*state % rho + &
      screened_rates(k_n_ni56__ni57)*Y(jn)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni57__he4_fe54)*Y(jn)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__ni58)*Y(jn)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__p_co57)*Y(jn)*Y(jni57)*state % rho - &
      screened_rates(k_ni57__co57__weak__wc12)*Y(jni57) + &
      screened_rates(k_p_co56__ni57)*Y(jco56)*Y(jp)*state % rho &
       )

    ydot_nuc(jni58) = ( &
      screened_rates(k_co58__ni58__weak__mo03)*Y(jco58) + screened_rates(k_he4_co55__p_ni58)* &
      Y(jco55)*Y(jhe4)*state % rho + screened_rates(k_he4_fe54__ni58)* &
      Y(jfe54)*Y(jhe4)*state % rho + screened_rates(k_n_ni57__ni58)*Y(jn)* &
      Y(jni57)*state % rho - screened_rates(k_n_ni58__he4_fe55)*Y(jn)* &
      Y(jni58)*state % rho - screened_rates(k_n_ni58__ni59)*Y(jn)*Y(jni58)* &
      state % rho - screened_rates(k_n_ni58__p_co58)*Y(jn)*Y(jni58)*state % rho + &
      screened_rates(k_p_co57__ni58)*Y(jco57)*Y(jp)*state % rho &
       )

    ydot_nuc(jni59) = ( &
      screened_rates(k_he4_co56__p_ni59)*Y(jco56)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe55__ni59)*Y(jfe55)*Y(jhe4)*state % rho + &
      screened_rates(k_n_ni58__ni59)*Y(jn)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni59__he4_fe56)*Y(jn)*Y(jni59)*state % rho - &
      screened_rates(k_n_ni59__ni60)*Y(jn)*Y(jni59)*state % rho + &
      screened_rates(k_p_co58__ni59)*Y(jco58)*Y(jp)*state % rho &
       )

    ydot_nuc(jni60) = ( &
      screened_rates(k_he4_co57__p_ni60)*Y(jco57)*Y(jhe4)*state % rho + &
      screened_rates(k_he4_fe56__ni60)*Y(jfe56)*Y(jhe4)*state % rho + &
      screened_rates(k_n_ni59__ni60)*Y(jn)*Y(jni59)*state % rho &
       )


  end subroutine rhs_nuc


  subroutine actual_jac(state, jac)

    !$acc routine seq

    use burn_type_module, only: net_itemp, net_ienuc, neqs, njrows, njcols
    use extern_probin_module, only: disable_thermal_neutrinos
    use sneut_module, only: sneut5
    use temperature_integration_module, only: temperature_jac
    use jacobian_sparsity_module, only: get_jac_entry, set_jac_entry, set_jac_zero

    implicit none
    
    type(burn_t), intent(in) :: state
    real(rt), intent(inout) :: jac(njrows, njcols)

    type(rate_eval_t) :: rate_eval
    real(rt) :: screened_rates_dt(nrates)
    real(rt) :: Y(nspec), yderivs(nspec)
    real(rt) :: ye, rhoy, b1, scratch
    real(rt) :: sneut, dsneutdt, dsneutdd, snuda, snudz
    integer  :: j, k

    !$gpu

    ! Set molar abundances
    Y(:) = state % xn(:) * aion_inv(:)
    
    call evaluate_rates(state, rate_eval)

    ! Zero out the Jacobian
    call set_jac_zero(jac)

    ! Species Jacobian elements with respect to other species
    call jac_nuc(state, jac, Y, rate_eval % screened_rates)

    ! Evaluate the species Jacobian elements with respect to temperature by
    ! calling the RHS using the temperature derivative of the screened rate
    screened_rates_dt = rate_eval % unscreened_rates(i_rate, :) * &
                        rate_eval % unscreened_rates(i_dscor_dt, :) + &
                        rate_eval % unscreened_rates(i_drate_dt, :) * &
                        rate_eval % unscreened_rates(i_scor, :)

    call rhs_nuc(state, yderivs, Y, screened_rates_dt)

    do k = 1, nspec
       call set_jac_entry(jac, k, net_itemp, yderivs(k))
    enddo

    ! Energy generation rate Jacobian elements with respect to species
    do j = 1, nspec
       do k = 1, nspec
          call get_jac_entry(jac, k, j, yderivs(k))
       enddo
       call ener_gener_rate(yderivs, scratch)
       call set_jac_entry(jac, net_ienuc, j, scratch)
    enddo

    ! Account for the thermal neutrino losses
    if (.not. disable_thermal_neutrinos) then
       call sneut5(state % T, state % rho, state % abar, state % zbar, sneut, dsneutdt, dsneutdd, snuda, snudz)

       do j = 1, nspec
          b1 = (-state % abar * state % abar * snuda + (zion(j) - state % zbar) * state % abar * snudz)
          call get_jac_entry(jac, net_ienuc, j, scratch)
          scratch = scratch - b1
          call set_jac_entry(jac, net_ienuc, j, scratch)
       enddo
    endif

    ! Energy generation rate Jacobian element with respect to temperature
    do k = 1, nspec
       call get_jac_entry(jac, k, net_itemp, yderivs(k))
    enddo
    call ener_gener_rate(yderivs, scratch)
    if (.not. disable_thermal_neutrinos) then
       scratch = scratch - dsneutdt
    endif
    call set_jac_entry(jac, net_ienuc, net_itemp, scratch)

    ! Temperature Jacobian elements
    call temperature_jac(state, jac)

  end subroutine actual_jac


  subroutine jac_nuc(state, jac, Y, screened_rates)

    !$acc routine seq

    use jacobian_sparsity_module, only: set_jac_entry

    implicit none

    type(burn_t), intent(in) :: state
    real(rt), intent(inout) :: jac(njrows, njcols)

    real(rt), intent(in)  :: Y(nspec)
    real(rt), intent(in)  :: screened_rates(nrates)
    real(rt) :: scratch


    !$gpu


    scratch = (&
      -screened_rates(k_n__p__weak__wc12) - screened_rates(k_n_al25__al26)*Y(jal25)*state % rho - &
      screened_rates(k_n_al25__he4_na22)*Y(jal25)*state % rho - &
      screened_rates(k_n_al25__p_mg25)*Y(jal25)*state % rho - &
      screened_rates(k_n_al26__al27)*Y(jal26)*state % rho - &
      screened_rates(k_n_al26__he4_na23)*Y(jal26)*state % rho - &
      screened_rates(k_n_al26__p_mg26)*Y(jal26)*state % rho - &
      screened_rates(k_n_al27__al28)*Y(jal27)*state % rho - screened_rates(k_n_al28__al29) &
      *Y(jal28)*state % rho - screened_rates(k_n_ar35__ar36)*Y(jar35)*state % rho &
      - screened_rates(k_n_ar35__he4_s32)*Y(jar35)*state % rho - &
      screened_rates(k_n_ar35__p_cl35)*Y(jar35)*state % rho - &
      screened_rates(k_n_ar36__ar37)*Y(jar36)*state % rho - &
      screened_rates(k_n_ar36__he4_s33)*Y(jar36)*state % rho - &
      screened_rates(k_n_ar36__p_cl36)*Y(jar36)*state % rho - &
      screened_rates(k_n_ar37__ar38)*Y(jar37)*state % rho - &
      screened_rates(k_n_ar37__p_cl37)*Y(jar37)*state % rho - &
      screened_rates(k_n_ar38__ar39)*Y(jar38)*state % rho - screened_rates(k_n_c12__c13)* &
      Y(jc12)*state % rho - screened_rates(k_n_c13__c14)*Y(jc13)*state % rho - &
      screened_rates(k_n_ca39__ca40)*Y(jca39)*state % rho - &
      screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*state % rho - &
      screened_rates(k_n_ca39__p_k39)*Y(jca39)*state % rho - &
      screened_rates(k_n_ca40__ca41)*Y(jca40)*state % rho - &
      screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*state % rho - &
      screened_rates(k_n_ca41__ca42)*Y(jca41)*state % rho - &
      screened_rates(k_n_ca41__he4_ar38)*Y(jca41)*state % rho - &
      screened_rates(k_n_ca42__ca43)*Y(jca42)*state % rho - &
      screened_rates(k_n_ca42__he4_ar39)*Y(jca42)*state % rho - &
      screened_rates(k_n_ca43__ca44)*Y(jca43)*state % rho - screened_rates(k_n_cl33__cl34) &
      *Y(jcl33)*state % rho - screened_rates(k_n_cl33__he4_p30)*Y(jcl33)* &
      state % rho - screened_rates(k_n_cl33__p_s33)*Y(jcl33)*state % rho - &
      screened_rates(k_n_cl34__cl35)*Y(jcl34)*state % rho - &
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*state % rho - &
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*state % rho - &
      screened_rates(k_n_cl35__cl36)*Y(jcl35)*state % rho - &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*state % rho - &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*state % rho - &
      screened_rates(k_n_cl36__cl37)*Y(jcl36)*state % rho - &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*state % rho - &
      screened_rates(k_n_co53__co54)*Y(jco53)*state % rho - &
      screened_rates(k_n_co53__he4_mn50)*Y(jco53)*state % rho - &
      screened_rates(k_n_co53__p_fe53)*Y(jco53)*state % rho - &
      screened_rates(k_n_co54__co55)*Y(jco54)*state % rho - &
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*state % rho - &
      screened_rates(k_n_co54__p_fe54)*Y(jco54)*state % rho - &
      screened_rates(k_n_co55__co56)*Y(jco55)*state % rho - &
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*state % rho - &
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*state % rho - &
      screened_rates(k_n_co56__co57)*Y(jco56)*state % rho - &
      screened_rates(k_n_co56__he4_mn53)*Y(jco56)*state % rho - &
      screened_rates(k_n_co56__p_fe56)*Y(jco56)*state % rho - &
      screened_rates(k_n_co57__co58)*Y(jco57)*state % rho - &
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*state % rho - &
      screened_rates(k_n_co58__he4_mn55)*Y(jco58)*state % rho - &
      screened_rates(k_n_cr47__cr48)*Y(jcr47)*state % rho - &
      screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*state % rho - &
      screened_rates(k_n_cr47__p_v47)*Y(jcr47)*state % rho - &
      screened_rates(k_n_cr48__cr49)*Y(jcr48)*state % rho - &
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*state % rho - &
      screened_rates(k_n_cr48__p_v48)*Y(jcr48)*state % rho - &
      screened_rates(k_n_cr49__cr50)*Y(jcr49)*state % rho - &
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*state % rho - &
      screened_rates(k_n_cr49__p_v49)*Y(jcr49)*state % rho - &
      screened_rates(k_n_cr50__cr51)*Y(jcr50)*state % rho - &
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*state % rho - &
      screened_rates(k_n_cr51__cr52)*Y(jcr51)*state % rho - &
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*state % rho - &
      screened_rates(k_n_cr51__p_v51)*Y(jcr51)*state % rho - screened_rates(k_n_d__t)* &
      Y(jd)*state % rho - screened_rates(k_n_f18__f19)*Y(jf18)*state % rho - &
      screened_rates(k_n_f18__he4_n15)*Y(jf18)*state % rho - &
      screened_rates(k_n_f18__p_o18)*Y(jf18)*state % rho - screened_rates(k_n_f19__f20)* &
      Y(jf19)*state % rho - screened_rates(k_n_f20__f21)*Y(jf20)*state % rho - &
      screened_rates(k_n_fe51__fe52)*Y(jfe51)*state % rho - &
      screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*state % rho - &
      screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*state % rho - &
      screened_rates(k_n_fe52__fe53)*Y(jfe52)*state % rho - &
      screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*state % rho - &
      screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*state % rho - &
      screened_rates(k_n_fe53__fe54)*Y(jfe53)*state % rho - &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*state % rho - &
      screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*state % rho - &
      screened_rates(k_n_fe54__fe55)*Y(jfe54)*state % rho - &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*state % rho - &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*state % rho - &
      screened_rates(k_n_fe55__fe56)*Y(jfe55)*state % rho - &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*state % rho - &
      screened_rates(k_n_fe55__p_mn55)*Y(jfe55)*state % rho - screened_rates(k_n_he3__he4) &
      *Y(jhe3)*state % rho - screened_rates(k_n_he3__p_t)*Y(jhe3)*state % rho - &
      screened_rates(k_n_k37__he4_cl34)*Y(jk37)*state % rho - screened_rates(k_n_k37__k38) &
      *Y(jk37)*state % rho - screened_rates(k_n_k37__p_ar37)*Y(jk37)*state % rho - &
      screened_rates(k_n_k38__he4_cl35)*Y(jk38)*state % rho - screened_rates(k_n_k38__k39) &
      *Y(jk38)*state % rho - screened_rates(k_n_k38__p_ar38)*Y(jk38)*state % rho - &
      screened_rates(k_n_k39__he4_cl36)*Y(jk39)*state % rho - screened_rates(k_n_k39__k40) &
      *Y(jk39)*state % rho - screened_rates(k_n_k39__p_ar39)*Y(jk39)*state % rho - &
      screened_rates(k_n_k40__he4_cl37)*Y(jk40)*state % rho - screened_rates(k_n_k40__k41) &
      *Y(jk40)*state % rho - screened_rates(k_n_k41__k42)*Y(jk41)*state % rho - &
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*state % rho - &
      screened_rates(k_n_mg22__mg23)*Y(jmg22)*state % rho - &
      screened_rates(k_n_mg22__p_na22)*Y(jmg22)*state % rho - &
      screened_rates(k_n_mg23__c12_c12)*Y(jmg23)*state % rho - &
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*state % rho - &
      screened_rates(k_n_mg23__mg24)*Y(jmg23)*state % rho - screened_rates(k_n_mg24__mg25) &
      *Y(jmg24)*state % rho - screened_rates(k_n_mg25__mg26)*Y(jmg25)*state % rho &
      - screened_rates(k_n_mg26__mg27)*Y(jmg26)*state % rho - &
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*state % rho - &
      screened_rates(k_n_mn49__mn50)*Y(jmn49)*state % rho - &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*state % rho - &
      screened_rates(k_n_mn50__he4_v47)*Y(jmn50)*state % rho - &
      screened_rates(k_n_mn50__mn51)*Y(jmn50)*state % rho - &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*state % rho - &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*state % rho - &
      screened_rates(k_n_mn51__mn52)*Y(jmn51)*state % rho - &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*state % rho - &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*state % rho - &
      screened_rates(k_n_mn52__mn53)*Y(jmn52)*state % rho - &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*state % rho - &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*state % rho - &
      screened_rates(k_n_mn53__mn54)*Y(jmn53)*state % rho - &
      screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*state % rho - &
      screened_rates(k_n_mn54__mn55)*Y(jmn54)*state % rho - screened_rates(k_n_n13__n14)* &
      Y(jn13)*state % rho - screened_rates(k_n_n14__n15)*Y(jn14)*state % rho - &
      screened_rates(k_n_n14__p_c14)*Y(jn14)*state % rho - screened_rates(k_n_na20__na21)* &
      Y(jna20)*state % rho - screened_rates(k_n_na20__p_ne20)*Y(jna20)*state % rho &
      - screened_rates(k_n_na21__he4_f18)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__na22)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__p_ne21)*Y(jna21)*state % rho - &
      screened_rates(k_n_na22__he4_f19)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__na23)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__p_ne22)*Y(jna22)*state % rho - &
      screened_rates(k_n_na23__na24)*Y(jna23)*state % rho - &
      screened_rates(k_n_ne19__he4_o16)*Y(jne19)*state % rho - &
      screened_rates(k_n_ne19__ne20)*Y(jne19)*state % rho - screened_rates(k_n_ne20__ne21) &
      *Y(jne20)*state % rho - screened_rates(k_n_ne21__ne22)*Y(jne21)*state % rho &
      - screened_rates(k_n_ne22__ne23)*Y(jne22)*state % rho - &
      screened_rates(k_n_ne23__ne24)*Y(jne23)*state % rho - &
      screened_rates(k_n_ni54__he4_fe51)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni54__ni55)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni54__p_co54)*Y(jni54)*state % rho - &
      screened_rates(k_n_ni55__he4_fe52)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__ni56)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__p_co55)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni56__he4_fe53)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__ni57)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__p_co56)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni57__he4_fe54)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__ni58)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__p_co57)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni58__he4_fe55)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni58__ni59)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni58__p_co58)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni59__he4_fe56)*Y(jni59)*state % rho - &
      screened_rates(k_n_ni59__ni60)*Y(jni59)*state % rho - screened_rates(k_n_o16__o17)* &
      Y(jo16)*state % rho - screened_rates(k_n_o17__he4_c14)*Y(jo17)*state % rho - &
      screened_rates(k_n_o17__o18)*Y(jo17)*state % rho - screened_rates(k_n_o18__o19)* &
      Y(jo18)*state % rho - screened_rates(k_n_p29__he4_al26)*Y(jp29)*state % rho &
      - screened_rates(k_n_p29__p30)*Y(jp29)*state % rho - screened_rates(k_n_p29__p_si29) &
      *Y(jp29)*state % rho - screened_rates(k_n_p30__p31)*Y(jp30)*state % rho - &
      screened_rates(k_n_p30__p_si30)*Y(jp30)*state % rho - screened_rates(k_n_p31__p32)* &
      Y(jp31)*state % rho - screened_rates(k_n_p32__p33)*Y(jp32)*state % rho - &
      screened_rates(k_n_p32__p_si32)*Y(jp32)*state % rho - screened_rates(k_n_p__d)* &
      Y(jp)*state % rho - 0.5e0_rt*screened_rates(k_n_p_p__p)*Y(jp)**2*state % rho**2 &
      - screened_rates(k_n_s30__he4_si27)*Y(js30)*state % rho - &
      screened_rates(k_n_s30__p_p30)*Y(js30)*state % rho - screened_rates(k_n_s30__s31)* &
      Y(js30)*state % rho - screened_rates(k_n_s31__he4_si28)*Y(js31)*state % rho &
      - screened_rates(k_n_s31__p_p31)*Y(js31)*state % rho - screened_rates(k_n_s31__s32)* &
      Y(js31)*state % rho - screened_rates(k_n_s32__he4_si29)*Y(js32)*state % rho &
      - screened_rates(k_n_s32__s33)*Y(js32)*state % rho - &
      screened_rates(k_n_s33__he4_si30)*Y(js33)*state % rho - &
      screened_rates(k_n_s33__p_p33)*Y(js33)*state % rho - screened_rates(k_n_s33__s34)* &
      Y(js33)*state % rho - screened_rates(k_n_s34__s35)*Y(js34)*state % rho - &
      screened_rates(k_n_s35__he4_si32)*Y(js35)*state % rho - &
      screened_rates(k_n_sc42__he4_k39)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__p_ca42)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc42__sc43)*Y(jsc42)*state % rho - &
      screened_rates(k_n_sc43__he4_k40)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc43__p_ca43)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc43__sc44)*Y(jsc43)*state % rho - &
      screened_rates(k_n_sc44__p_ca44)*Y(jsc44)*state % rho - &
      screened_rates(k_n_sc44__sc45)*Y(jsc44)*state % rho - screened_rates(k_n_sc45__sc46) &
      *Y(jsc45)*state % rho - screened_rates(k_n_si27__c12_o16)*Y(jsi27)* &
      state % rho - screened_rates(k_n_si27__he4_mg24)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__p_al27)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__si28)*Y(jsi27)*state % rho - screened_rates(k_n_si28__si29) &
      *Y(jsi28)*state % rho - screened_rates(k_n_si29__si30)*Y(jsi29)*state % rho &
      - screened_rates(k_n_si30__si31)*Y(jsi30)*state % rho - &
      screened_rates(k_n_si31__si32)*Y(jsi31)*state % rho - &
      screened_rates(k_n_ti43__he4_ca40)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__p_sc43)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti43__ti44)*Y(jti43)*state % rho - &
      screened_rates(k_n_ti44__he4_ca41)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__p_sc44)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__ti45)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti45__he4_ca42)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__p_sc45)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__ti46)*Y(jti45)*state % rho - screened_rates(k_n_ti46__ti47) &
      *Y(jti46)*state % rho - screened_rates(k_n_ti47__he4_ca44)*Y(jti47)* &
      state % rho - screened_rates(k_n_ti47__ti48)*Y(jti47)*state % rho - &
      screened_rates(k_n_ti48__ti49)*Y(jti48)*state % rho - &
      screened_rates(k_n_v46__he4_sc43)*Y(jv46)*state % rho - &
      screened_rates(k_n_v46__p_ti46)*Y(jv46)*state % rho - screened_rates(k_n_v46__v47)* &
      Y(jv46)*state % rho - screened_rates(k_n_v47__he4_sc44)*Y(jv47)*state % rho &
      - screened_rates(k_n_v47__p_ti47)*Y(jv47)*state % rho - screened_rates(k_n_v47__v48) &
      *Y(jv47)*state % rho - screened_rates(k_n_v48__he4_sc45)*Y(jv48)*state % rho &
      - screened_rates(k_n_v48__p_ti48)*Y(jv48)*state % rho - screened_rates(k_n_v48__v49) &
      *Y(jv48)*state % rho - screened_rates(k_n_v49__p_ti49)*Y(jv49)*state % rho - &
      screened_rates(k_n_v49__v50)*Y(jv49)*state % rho - screened_rates(k_n_v50__v51)* &
      Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jn, jn, scratch)

    scratch = (&
      -screened_rates(k_n_p__d)*Y(jn)*state % rho - 1.0e0_rt*screened_rates(k_n_p_p__p)*Y(jn)* &
      Y(jp)*state % rho**2 + screened_rates(k_p_al28__n_si28)*Y(jal28)*state % rho &
      + screened_rates(k_p_al29__n_si29)*Y(jal29)*state % rho + &
      screened_rates(k_p_c13__n_n13)*Y(jc13)*state % rho + screened_rates(k_p_f19__n_ne19) &
      *Y(jf19)*state % rho + screened_rates(k_p_f20__n_ne20)*Y(jf20)*state % rho + &
      screened_rates(k_p_f21__n_ne21)*Y(jf21)*state % rho + &
      screened_rates(k_p_k40__n_ca40)*Y(jk40)*state % rho + &
      screened_rates(k_p_k41__n_ca41)*Y(jk41)*state % rho + &
      screened_rates(k_p_k42__n_ca42)*Y(jk42)*state % rho + &
      screened_rates(k_p_mg27__n_al27)*Y(jmg27)*state % rho + &
      screened_rates(k_p_na23__n_mg23)*Y(jna23)*state % rho + &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*state % rho + &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*state % rho + &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*state % rho + &
      screened_rates(k_p_o19__n_f19)*Y(jo19)*state % rho + screened_rates(k_p_p32__n_s32)* &
      Y(jp32)*state % rho + screened_rates(k_p_sc46__n_ti46)*Y(jsc46)*state % rho &
      + screened_rates(k_p_si31__n_p31)*Y(jsi31)*state % rho + &
      screened_rates(k_p_v50__n_cr50)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jn, jp, scratch)

    scratch = (&
      screened_rates(k_d_c13__n_n14)*Y(jc13)*state % rho + screened_rates(k_d_c14__n_n15)* &
      Y(jc14)*state % rho + 1.0e0_rt*screened_rates(k_d_d__n_he3)*Y(jd)*state % rho + &
      screened_rates(k_d_t__n_he4)*Y(jt)*state % rho - screened_rates(k_n_d__t)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jn, jd, scratch)

    scratch = (&
      screened_rates(k_d_t__n_he4)*Y(jd)*state % rho + screened_rates(k_t_he3__n_p_he4)*Y(jhe3) &
      *state % rho + 2.0e0_rt*screened_rates(k_t_t__n_n_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jn, jt, scratch)

    scratch = (&
      -screened_rates(k_n_he3__he4)*Y(jn)*state % rho - screened_rates(k_n_he3__p_t)*Y(jn)* &
      state % rho + screened_rates(k_t_he3__n_p_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jn, jhe3, scratch)

    scratch = (&
      screened_rates(k_he4_al27__n_p30)*Y(jal27)*state % rho + screened_rates(k_he4_al28__n_p31)* &
      Y(jal28)*state % rho + screened_rates(k_he4_al29__n_p32)*Y(jal29)* &
      state % rho + screened_rates(k_he4_c13__n_o16)*Y(jc13)*state % rho + &
      screened_rates(k_he4_ca43__n_ti46)*Y(jca43)*state % rho + &
      screened_rates(k_he4_f20__n_na23)*Y(jf20)*state % rho + &
      screened_rates(k_he4_f21__n_na24)*Y(jf21)*state % rho + &
      screened_rates(k_he4_k41__n_sc44)*Y(jk41)*state % rho + &
      screened_rates(k_he4_k42__n_sc45)*Y(jk42)*state % rho + &
      screened_rates(k_he4_mg25__n_si28)*Y(jmg25)*state % rho + &
      screened_rates(k_he4_mg26__n_si29)*Y(jmg26)*state % rho + &
      screened_rates(k_he4_mg27__n_si30)*Y(jmg27)*state % rho + &
      screened_rates(k_he4_na24__n_al27)*Y(jna24)*state % rho + &
      screened_rates(k_he4_ne21__n_mg24)*Y(jne21)*state % rho + &
      screened_rates(k_he4_ne22__n_mg25)*Y(jne22)*state % rho + &
      screened_rates(k_he4_ne23__n_mg26)*Y(jne23)*state % rho + &
      screened_rates(k_he4_ne24__n_mg27)*Y(jne24)*state % rho + &
      screened_rates(k_he4_o17__n_ne20)*Y(jo17)*state % rho + &
      screened_rates(k_he4_o18__n_ne21)*Y(jo18)*state % rho + &
      screened_rates(k_he4_o19__n_ne22)*Y(jo19)*state % rho + &
      screened_rates(k_he4_s34__n_ar37)*Y(js34)*state % rho + &
      screened_rates(k_he4_s35__n_ar38)*Y(js35)*state % rho + &
      screened_rates(k_he4_sc46__n_v49)*Y(jsc46)*state % rho + &
      screened_rates(k_he4_si31__n_s34)*Y(jsi31)*state % rho + &
      screened_rates(k_he4_ti49__n_cr52)*Y(jti49)*state % rho &
       )
    call set_jac_entry(jac, jn, jhe4, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__n_s31)*Y(jne20)*state % rho - screened_rates(k_n_c12__c13)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jc12, scratch)

    scratch = (&
      screened_rates(k_d_c13__n_n14)*Y(jd)*state % rho + screened_rates(k_he4_c13__n_o16)* &
      Y(jhe4)*state % rho - screened_rates(k_n_c13__c14)*Y(jn)*state % rho + &
      screened_rates(k_p_c13__n_n13)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jc13, scratch)

    scratch = (&
      screened_rates(k_d_c14__n_n15)*Y(jd)*state % rho &
       )
    call set_jac_entry(jac, jn, jc14, scratch)

    scratch = (&
      -screened_rates(k_n_n13__n14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jn13, scratch)

    scratch = (&
      -screened_rates(k_n_n14__n15)*Y(jn)*state % rho - screened_rates(k_n_n14__p_c14)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jn, jn14, scratch)

    scratch = (&
      -screened_rates(k_n_o16__o17)*Y(jn)*state % rho + 1.0e0_rt*screened_rates(k_o16_o16__n_s31)* &
      Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jn, jo16, scratch)

    scratch = (&
      screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*state % rho - screened_rates(k_n_o17__he4_c14)* &
      Y(jn)*state % rho - screened_rates(k_n_o17__o18)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jo17, scratch)

    scratch = (&
      screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*state % rho - screened_rates(k_n_o18__o19)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jo18, scratch)

    scratch = (&
      screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*state % rho + screened_rates(k_p_o19__n_f19)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jo19, scratch)

    scratch = (&
      -screened_rates(k_n_f18__f19)*Y(jn)*state % rho - screened_rates(k_n_f18__he4_n15)*Y(jn)* &
      state % rho - screened_rates(k_n_f18__p_o18)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jf18, scratch)

    scratch = (&
      -screened_rates(k_n_f19__f20)*Y(jn)*state % rho + screened_rates(k_p_f19__n_ne19)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jn, jf19, scratch)

    scratch = (&
      screened_rates(k_he4_f20__n_na23)*Y(jhe4)*state % rho - screened_rates(k_n_f20__f21)* &
      Y(jn)*state % rho + screened_rates(k_p_f20__n_ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jf20, scratch)

    scratch = (&
      screened_rates(k_he4_f21__n_na24)*Y(jhe4)*state % rho + screened_rates(k_p_f21__n_ne21)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jf21, scratch)

    scratch = (&
      -screened_rates(k_n_ne19__he4_o16)*Y(jn)*state % rho - screened_rates(k_n_ne19__ne20)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jne19, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*state % rho - screened_rates(k_n_ne20__ne21)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jne20, scratch)

    scratch = (&
      screened_rates(k_he4_ne21__n_mg24)*Y(jhe4)*state % rho - screened_rates(k_n_ne21__ne22)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jne21, scratch)

    scratch = (&
      screened_rates(k_he4_ne22__n_mg25)*Y(jhe4)*state % rho - screened_rates(k_n_ne22__ne23)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jne22, scratch)

    scratch = (&
      screened_rates(k_he4_ne23__n_mg26)*Y(jhe4)*state % rho - screened_rates(k_n_ne23__ne24)* &
      Y(jn)*state % rho + screened_rates(k_p_ne23__n_na23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jne23, scratch)

    scratch = (&
      screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*state % rho + screened_rates(k_p_ne24__n_na24)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jne24, scratch)

    scratch = (&
      -screened_rates(k_n_na20__na21)*Y(jn)*state % rho - screened_rates(k_n_na20__p_ne20)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jna20, scratch)

    scratch = (&
      -screened_rates(k_n_na21__he4_f18)*Y(jn)*state % rho - screened_rates(k_n_na21__na22)* &
      Y(jn)*state % rho - screened_rates(k_n_na21__p_ne21)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jna21, scratch)

    scratch = (&
      -screened_rates(k_n_na22__he4_f19)*Y(jn)*state % rho - screened_rates(k_n_na22__na23)* &
      Y(jn)*state % rho - screened_rates(k_n_na22__p_ne22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jna22, scratch)

    scratch = (&
      -screened_rates(k_n_na23__na24)*Y(jn)*state % rho + screened_rates(k_p_na23__n_mg23)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jna23, scratch)

    scratch = (&
      screened_rates(k_he4_na24__n_al27)*Y(jhe4)*state % rho + screened_rates(k_p_na24__n_mg24)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jna24, scratch)

    scratch = (&
      -screened_rates(k_n_mg22__he4_ne19)*Y(jn)*state % rho - screened_rates(k_n_mg22__mg23)* &
      Y(jn)*state % rho - screened_rates(k_n_mg22__p_na22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmg22, scratch)

    scratch = (&
      -screened_rates(k_n_mg23__c12_c12)*Y(jn)*state % rho - screened_rates(k_n_mg23__he4_ne20)* &
      Y(jn)*state % rho - screened_rates(k_n_mg23__mg24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmg23, scratch)

    scratch = (&
      -screened_rates(k_n_mg24__mg25)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmg24, scratch)

    scratch = (&
      screened_rates(k_he4_mg25__n_si28)*Y(jhe4)*state % rho - screened_rates(k_n_mg25__mg26)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmg25, scratch)

    scratch = (&
      screened_rates(k_he4_mg26__n_si29)*Y(jhe4)*state % rho - screened_rates(k_n_mg26__mg27)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmg26, scratch)

    scratch = (&
      screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*state % rho + screened_rates(k_p_mg27__n_al27)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jmg27, scratch)

    scratch = (&
      -screened_rates(k_n_al25__al26)*Y(jn)*state % rho - screened_rates(k_n_al25__he4_na22)* &
      Y(jn)*state % rho - screened_rates(k_n_al25__p_mg25)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jal25, scratch)

    scratch = (&
      -screened_rates(k_n_al26__al27)*Y(jn)*state % rho - screened_rates(k_n_al26__he4_na23)* &
      Y(jn)*state % rho - screened_rates(k_n_al26__p_mg26)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jal26, scratch)

    scratch = (&
      screened_rates(k_he4_al27__n_p30)*Y(jhe4)*state % rho - screened_rates(k_n_al27__al28)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jal27, scratch)

    scratch = (&
      screened_rates(k_he4_al28__n_p31)*Y(jhe4)*state % rho - screened_rates(k_n_al28__al29)* &
      Y(jn)*state % rho + screened_rates(k_p_al28__n_si28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jal28, scratch)

    scratch = (&
      screened_rates(k_he4_al29__n_p32)*Y(jhe4)*state % rho + screened_rates(k_p_al29__n_si29)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jal29, scratch)

    scratch = (&
      -screened_rates(k_n_si27__c12_o16)*Y(jn)*state % rho - screened_rates(k_n_si27__he4_mg24)* &
      Y(jn)*state % rho - screened_rates(k_n_si27__p_al27)*Y(jn)*state % rho - &
      screened_rates(k_n_si27__si28)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsi27, scratch)

    scratch = (&
      -screened_rates(k_n_si28__si29)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsi28, scratch)

    scratch = (&
      -screened_rates(k_n_si29__si30)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsi29, scratch)

    scratch = (&
      -screened_rates(k_n_si30__si31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsi30, scratch)

    scratch = (&
      screened_rates(k_he4_si31__n_s34)*Y(jhe4)*state % rho - screened_rates(k_n_si31__si32)* &
      Y(jn)*state % rho + screened_rates(k_p_si31__n_p31)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jsi31, scratch)

    scratch = (&
      -screened_rates(k_n_p29__he4_al26)*Y(jn)*state % rho - screened_rates(k_n_p29__p30)*Y(jn) &
      *state % rho - screened_rates(k_n_p29__p_si29)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jp29, scratch)

    scratch = (&
      -screened_rates(k_n_p30__p31)*Y(jn)*state % rho - screened_rates(k_n_p30__p_si30)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jn, jp30, scratch)

    scratch = (&
      -screened_rates(k_n_p31__p32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jp31, scratch)

    scratch = (&
      -screened_rates(k_n_p32__p33)*Y(jn)*state % rho - screened_rates(k_n_p32__p_si32)*Y(jn)* &
      state % rho + screened_rates(k_p_p32__n_s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jp32, scratch)

    scratch = (&
      -screened_rates(k_n_s30__he4_si27)*Y(jn)*state % rho - screened_rates(k_n_s30__p_p30)* &
      Y(jn)*state % rho - screened_rates(k_n_s30__s31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, js30, scratch)

    scratch = (&
      -screened_rates(k_n_s31__he4_si28)*Y(jn)*state % rho - screened_rates(k_n_s31__p_p31)* &
      Y(jn)*state % rho - screened_rates(k_n_s31__s32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, js31, scratch)

    scratch = (&
      -screened_rates(k_n_s32__he4_si29)*Y(jn)*state % rho - screened_rates(k_n_s32__s33)*Y(jn) &
      *state % rho &
       )
    call set_jac_entry(jac, jn, js32, scratch)

    scratch = (&
      -screened_rates(k_n_s33__he4_si30)*Y(jn)*state % rho - screened_rates(k_n_s33__p_p33)* &
      Y(jn)*state % rho - screened_rates(k_n_s33__s34)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, js33, scratch)

    scratch = (&
      screened_rates(k_he4_s34__n_ar37)*Y(jhe4)*state % rho - screened_rates(k_n_s34__s35)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, js34, scratch)

    scratch = (&
      screened_rates(k_he4_s35__n_ar38)*Y(jhe4)*state % rho - screened_rates(k_n_s35__he4_si32)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, js35, scratch)

    scratch = (&
      -screened_rates(k_n_cl33__cl34)*Y(jn)*state % rho - screened_rates(k_n_cl33__he4_p30)* &
      Y(jn)*state % rho - screened_rates(k_n_cl33__p_s33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcl33, scratch)

    scratch = (&
      -screened_rates(k_n_cl34__cl35)*Y(jn)*state % rho - screened_rates(k_n_cl34__he4_p31)* &
      Y(jn)*state % rho - screened_rates(k_n_cl34__p_s34)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcl34, scratch)

    scratch = (&
      -screened_rates(k_n_cl35__cl36)*Y(jn)*state % rho - screened_rates(k_n_cl35__he4_p32)* &
      Y(jn)*state % rho - screened_rates(k_n_cl35__p_s35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcl35, scratch)

    scratch = (&
      -screened_rates(k_n_cl36__cl37)*Y(jn)*state % rho - screened_rates(k_n_cl36__he4_p33)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcl36, scratch)

    scratch = (&
      -screened_rates(k_n_ar35__ar36)*Y(jn)*state % rho - screened_rates(k_n_ar35__he4_s32)* &
      Y(jn)*state % rho - screened_rates(k_n_ar35__p_cl35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jar35, scratch)

    scratch = (&
      -screened_rates(k_n_ar36__ar37)*Y(jn)*state % rho - screened_rates(k_n_ar36__he4_s33)* &
      Y(jn)*state % rho - screened_rates(k_n_ar36__p_cl36)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jar36, scratch)

    scratch = (&
      -screened_rates(k_n_ar37__ar38)*Y(jn)*state % rho - screened_rates(k_n_ar37__p_cl37)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jar37, scratch)

    scratch = (&
      -screened_rates(k_n_ar38__ar39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jar38, scratch)

    scratch = (&
      -screened_rates(k_n_k37__he4_cl34)*Y(jn)*state % rho - screened_rates(k_n_k37__k38)*Y(jn) &
      *state % rho - screened_rates(k_n_k37__p_ar37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jk37, scratch)

    scratch = (&
      -screened_rates(k_n_k38__he4_cl35)*Y(jn)*state % rho - screened_rates(k_n_k38__k39)*Y(jn) &
      *state % rho - screened_rates(k_n_k38__p_ar38)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jk38, scratch)

    scratch = (&
      -screened_rates(k_n_k39__he4_cl36)*Y(jn)*state % rho - screened_rates(k_n_k39__k40)*Y(jn) &
      *state % rho - screened_rates(k_n_k39__p_ar39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jk39, scratch)

    scratch = (&
      -screened_rates(k_n_k40__he4_cl37)*Y(jn)*state % rho - screened_rates(k_n_k40__k41)*Y(jn) &
      *state % rho + screened_rates(k_p_k40__n_ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jk40, scratch)

    scratch = (&
      screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*state % rho - screened_rates(k_n_k41__k42)* &
      Y(jn)*state % rho + screened_rates(k_p_k41__n_ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jk41, scratch)

    scratch = (&
      screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*state % rho + screened_rates(k_p_k42__n_ca42)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jk42, scratch)

    scratch = (&
      -screened_rates(k_n_ca39__ca40)*Y(jn)*state % rho - screened_rates(k_n_ca39__he4_ar36)* &
      Y(jn)*state % rho - screened_rates(k_n_ca39__p_k39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jca39, scratch)

    scratch = (&
      -screened_rates(k_n_ca40__ca41)*Y(jn)*state % rho - screened_rates(k_n_ca40__he4_ar37)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jca40, scratch)

    scratch = (&
      -screened_rates(k_n_ca41__ca42)*Y(jn)*state % rho - screened_rates(k_n_ca41__he4_ar38)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jca41, scratch)

    scratch = (&
      -screened_rates(k_n_ca42__ca43)*Y(jn)*state % rho - screened_rates(k_n_ca42__he4_ar39)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jca42, scratch)

    scratch = (&
      screened_rates(k_he4_ca43__n_ti46)*Y(jhe4)*state % rho - screened_rates(k_n_ca43__ca44)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jca43, scratch)

    scratch = (&
      -screened_rates(k_n_sc42__he4_k39)*Y(jn)*state % rho - screened_rates(k_n_sc42__p_ca42)* &
      Y(jn)*state % rho - screened_rates(k_n_sc42__sc43)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsc42, scratch)

    scratch = (&
      -screened_rates(k_n_sc43__he4_k40)*Y(jn)*state % rho - screened_rates(k_n_sc43__p_ca43)* &
      Y(jn)*state % rho - screened_rates(k_n_sc43__sc44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsc43, scratch)

    scratch = (&
      -screened_rates(k_n_sc44__p_ca44)*Y(jn)*state % rho - screened_rates(k_n_sc44__sc45)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsc44, scratch)

    scratch = (&
      -screened_rates(k_n_sc45__sc46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jsc45, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*state % rho + screened_rates(k_p_sc46__n_ti46)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn, jsc46, scratch)

    scratch = (&
      -screened_rates(k_n_ti43__he4_ca40)*Y(jn)*state % rho - screened_rates(k_n_ti43__p_sc43)* &
      Y(jn)*state % rho - screened_rates(k_n_ti43__ti44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jti43, scratch)

    scratch = (&
      -screened_rates(k_n_ti44__he4_ca41)*Y(jn)*state % rho - screened_rates(k_n_ti44__p_sc44)* &
      Y(jn)*state % rho - screened_rates(k_n_ti44__ti45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jti44, scratch)

    scratch = (&
      -screened_rates(k_n_ti45__he4_ca42)*Y(jn)*state % rho - screened_rates(k_n_ti45__p_sc45)* &
      Y(jn)*state % rho - screened_rates(k_n_ti45__ti46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jti45, scratch)

    scratch = (&
      -screened_rates(k_n_ti46__ti47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jti46, scratch)

    scratch = (&
      -screened_rates(k_n_ti47__he4_ca44)*Y(jn)*state % rho - screened_rates(k_n_ti47__ti48)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jti47, scratch)

    scratch = (&
      -screened_rates(k_n_ti48__ti49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jti48, scratch)

    scratch = (&
      screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jn, jti49, scratch)

    scratch = (&
      -screened_rates(k_n_v46__he4_sc43)*Y(jn)*state % rho - screened_rates(k_n_v46__p_ti46)* &
      Y(jn)*state % rho - screened_rates(k_n_v46__v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jv46, scratch)

    scratch = (&
      -screened_rates(k_n_v47__he4_sc44)*Y(jn)*state % rho - screened_rates(k_n_v47__p_ti47)* &
      Y(jn)*state % rho - screened_rates(k_n_v47__v48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jv47, scratch)

    scratch = (&
      -screened_rates(k_n_v48__he4_sc45)*Y(jn)*state % rho - screened_rates(k_n_v48__p_ti48)* &
      Y(jn)*state % rho - screened_rates(k_n_v48__v49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jv48, scratch)

    scratch = (&
      -screened_rates(k_n_v49__p_ti49)*Y(jn)*state % rho - screened_rates(k_n_v49__v50)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jn, jv49, scratch)

    scratch = (&
      -screened_rates(k_n_v50__v51)*Y(jn)*state % rho + screened_rates(k_p_v50__n_cr50)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jn, jv50, scratch)

    scratch = (&
      -screened_rates(k_n_cr47__cr48)*Y(jn)*state % rho - screened_rates(k_n_cr47__he4_ti44)* &
      Y(jn)*state % rho - screened_rates(k_n_cr47__p_v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcr47, scratch)

    scratch = (&
      -screened_rates(k_n_cr48__cr49)*Y(jn)*state % rho - screened_rates(k_n_cr48__he4_ti45)* &
      Y(jn)*state % rho - screened_rates(k_n_cr48__p_v48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcr48, scratch)

    scratch = (&
      -screened_rates(k_n_cr49__cr50)*Y(jn)*state % rho - screened_rates(k_n_cr49__he4_ti46)* &
      Y(jn)*state % rho - screened_rates(k_n_cr49__p_v49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcr49, scratch)

    scratch = (&
      -screened_rates(k_n_cr50__cr51)*Y(jn)*state % rho - screened_rates(k_n_cr50__he4_ti47)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcr50, scratch)

    scratch = (&
      -screened_rates(k_n_cr51__cr52)*Y(jn)*state % rho - screened_rates(k_n_cr51__he4_ti48)* &
      Y(jn)*state % rho - screened_rates(k_n_cr51__p_v51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jcr51, scratch)

    scratch = (&
      -screened_rates(k_n_mn49__he4_v46)*Y(jn)*state % rho - screened_rates(k_n_mn49__mn50)* &
      Y(jn)*state % rho - screened_rates(k_n_mn49__p_cr49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmn49, scratch)

    scratch = (&
      -screened_rates(k_n_mn50__he4_v47)*Y(jn)*state % rho - screened_rates(k_n_mn50__mn51)* &
      Y(jn)*state % rho - screened_rates(k_n_mn50__p_cr50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmn50, scratch)

    scratch = (&
      -screened_rates(k_n_mn51__he4_v48)*Y(jn)*state % rho - screened_rates(k_n_mn51__mn52)* &
      Y(jn)*state % rho - screened_rates(k_n_mn51__p_cr51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmn51, scratch)

    scratch = (&
      -screened_rates(k_n_mn52__he4_v49)*Y(jn)*state % rho - screened_rates(k_n_mn52__mn53)* &
      Y(jn)*state % rho - screened_rates(k_n_mn52__p_cr52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmn52, scratch)

    scratch = (&
      -screened_rates(k_n_mn53__he4_v50)*Y(jn)*state % rho - screened_rates(k_n_mn53__mn54)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmn53, scratch)

    scratch = (&
      -screened_rates(k_n_mn54__he4_v51)*Y(jn)*state % rho - screened_rates(k_n_mn54__mn55)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jmn54, scratch)

    scratch = (&
      -screened_rates(k_n_fe51__fe52)*Y(jn)*state % rho - screened_rates(k_n_fe51__he4_cr48)* &
      Y(jn)*state % rho - screened_rates(k_n_fe51__p_mn51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jfe51, scratch)

    scratch = (&
      -screened_rates(k_n_fe52__fe53)*Y(jn)*state % rho - screened_rates(k_n_fe52__he4_cr49)* &
      Y(jn)*state % rho - screened_rates(k_n_fe52__p_mn52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jfe52, scratch)

    scratch = (&
      -screened_rates(k_n_fe53__fe54)*Y(jn)*state % rho - screened_rates(k_n_fe53__he4_cr50)* &
      Y(jn)*state % rho - screened_rates(k_n_fe53__p_mn53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jfe53, scratch)

    scratch = (&
      -screened_rates(k_n_fe54__fe55)*Y(jn)*state % rho - screened_rates(k_n_fe54__he4_cr51)* &
      Y(jn)*state % rho - screened_rates(k_n_fe54__p_mn54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jfe54, scratch)

    scratch = (&
      -screened_rates(k_n_fe55__fe56)*Y(jn)*state % rho - screened_rates(k_n_fe55__he4_cr52)* &
      Y(jn)*state % rho - screened_rates(k_n_fe55__p_mn55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jfe55, scratch)

    scratch = (&
      -screened_rates(k_n_co53__co54)*Y(jn)*state % rho - screened_rates(k_n_co53__he4_mn50)* &
      Y(jn)*state % rho - screened_rates(k_n_co53__p_fe53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jco53, scratch)

    scratch = (&
      -screened_rates(k_n_co54__co55)*Y(jn)*state % rho - screened_rates(k_n_co54__he4_mn51)* &
      Y(jn)*state % rho - screened_rates(k_n_co54__p_fe54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jco54, scratch)

    scratch = (&
      -screened_rates(k_n_co55__co56)*Y(jn)*state % rho - screened_rates(k_n_co55__he4_mn52)* &
      Y(jn)*state % rho - screened_rates(k_n_co55__p_fe55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jco55, scratch)

    scratch = (&
      -screened_rates(k_n_co56__co57)*Y(jn)*state % rho - screened_rates(k_n_co56__he4_mn53)* &
      Y(jn)*state % rho - screened_rates(k_n_co56__p_fe56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jco56, scratch)

    scratch = (&
      -screened_rates(k_n_co57__co58)*Y(jn)*state % rho - screened_rates(k_n_co57__he4_mn54)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jco57, scratch)

    scratch = (&
      -screened_rates(k_n_co58__he4_mn55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jco58, scratch)

    scratch = (&
      -screened_rates(k_n_ni54__he4_fe51)*Y(jn)*state % rho - screened_rates(k_n_ni54__ni55)* &
      Y(jn)*state % rho - screened_rates(k_n_ni54__p_co54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jni54, scratch)

    scratch = (&
      -screened_rates(k_n_ni55__he4_fe52)*Y(jn)*state % rho - screened_rates(k_n_ni55__ni56)* &
      Y(jn)*state % rho - screened_rates(k_n_ni55__p_co55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jni55, scratch)

    scratch = (&
      -screened_rates(k_n_ni56__he4_fe53)*Y(jn)*state % rho - screened_rates(k_n_ni56__ni57)* &
      Y(jn)*state % rho - screened_rates(k_n_ni56__p_co56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jni56, scratch)

    scratch = (&
      -screened_rates(k_n_ni57__he4_fe54)*Y(jn)*state % rho - screened_rates(k_n_ni57__ni58)* &
      Y(jn)*state % rho - screened_rates(k_n_ni57__p_co57)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jni57, scratch)

    scratch = (&
      -screened_rates(k_n_ni58__he4_fe55)*Y(jn)*state % rho - screened_rates(k_n_ni58__ni59)* &
      Y(jn)*state % rho - screened_rates(k_n_ni58__p_co58)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jni58, scratch)

    scratch = (&
      -screened_rates(k_n_ni59__he4_fe56)*Y(jn)*state % rho - screened_rates(k_n_ni59__ni60)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn, jni59, scratch)

    scratch = (&
      screened_rates(k_n__p__weak__wc12) + screened_rates(k_n_al25__p_mg25)*Y(jal25)*state % rho &
      + screened_rates(k_n_al26__p_mg26)*Y(jal26)*state % rho + &
      screened_rates(k_n_ar35__p_cl35)*Y(jar35)*state % rho + &
      screened_rates(k_n_ar36__p_cl36)*Y(jar36)*state % rho + &
      screened_rates(k_n_ar37__p_cl37)*Y(jar37)*state % rho + &
      screened_rates(k_n_ca39__p_k39)*Y(jca39)*state % rho + &
      screened_rates(k_n_cl33__p_s33)*Y(jcl33)*state % rho + &
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*state % rho + &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*state % rho + &
      screened_rates(k_n_co53__p_fe53)*Y(jco53)*state % rho + &
      screened_rates(k_n_co54__p_fe54)*Y(jco54)*state % rho + &
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*state % rho + &
      screened_rates(k_n_co56__p_fe56)*Y(jco56)*state % rho + &
      screened_rates(k_n_cr47__p_v47)*Y(jcr47)*state % rho + &
      screened_rates(k_n_cr48__p_v48)*Y(jcr48)*state % rho + &
      screened_rates(k_n_cr49__p_v49)*Y(jcr49)*state % rho + &
      screened_rates(k_n_cr51__p_v51)*Y(jcr51)*state % rho + &
      screened_rates(k_n_f18__p_o18)*Y(jf18)*state % rho + &
      screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*state % rho + &
      screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*state % rho + &
      screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*state % rho + &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*state % rho + &
      screened_rates(k_n_fe55__p_mn55)*Y(jfe55)*state % rho + screened_rates(k_n_he3__p_t) &
      *Y(jhe3)*state % rho + screened_rates(k_n_k37__p_ar37)*Y(jk37)*state % rho + &
      screened_rates(k_n_k38__p_ar38)*Y(jk38)*state % rho + &
      screened_rates(k_n_k39__p_ar39)*Y(jk39)*state % rho + &
      screened_rates(k_n_mg22__p_na22)*Y(jmg22)*state % rho + &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*state % rho + &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*state % rho + &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*state % rho + &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*state % rho + &
      screened_rates(k_n_n14__p_c14)*Y(jn14)*state % rho + &
      screened_rates(k_n_na20__p_ne20)*Y(jna20)*state % rho + &
      screened_rates(k_n_na21__p_ne21)*Y(jna21)*state % rho + &
      screened_rates(k_n_na22__p_ne22)*Y(jna22)*state % rho + &
      screened_rates(k_n_ni54__p_co54)*Y(jni54)*state % rho + &
      screened_rates(k_n_ni55__p_co55)*Y(jni55)*state % rho + &
      screened_rates(k_n_ni56__p_co56)*Y(jni56)*state % rho + &
      screened_rates(k_n_ni57__p_co57)*Y(jni57)*state % rho + &
      screened_rates(k_n_ni58__p_co58)*Y(jni58)*state % rho + &
      screened_rates(k_n_p29__p_si29)*Y(jp29)*state % rho + &
      screened_rates(k_n_p30__p_si30)*Y(jp30)*state % rho + &
      screened_rates(k_n_p32__p_si32)*Y(jp32)*state % rho - screened_rates(k_n_p__d)* &
      Y(jp)*state % rho - 1.0e0_rt*screened_rates(k_n_p_p__p)*Y(jp)**2*state % rho**2 &
      + screened_rates(k_n_s30__p_p30)*Y(js30)*state % rho + &
      screened_rates(k_n_s31__p_p31)*Y(js31)*state % rho + screened_rates(k_n_s33__p_p33)* &
      Y(js33)*state % rho + screened_rates(k_n_sc42__p_ca42)*Y(jsc42)*state % rho &
      + screened_rates(k_n_sc43__p_ca43)*Y(jsc43)*state % rho + &
      screened_rates(k_n_sc44__p_ca44)*Y(jsc44)*state % rho + &
      screened_rates(k_n_si27__p_al27)*Y(jsi27)*state % rho + &
      screened_rates(k_n_ti43__p_sc43)*Y(jti43)*state % rho + &
      screened_rates(k_n_ti44__p_sc44)*Y(jti44)*state % rho + &
      screened_rates(k_n_ti45__p_sc45)*Y(jti45)*state % rho + &
      screened_rates(k_n_v46__p_ti46)*Y(jv46)*state % rho + &
      screened_rates(k_n_v47__p_ti47)*Y(jv47)*state % rho + &
      screened_rates(k_n_v48__p_ti48)*Y(jv48)*state % rho + &
      screened_rates(k_n_v49__p_ti49)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jp, jn, scratch)

    scratch = (&
      -screened_rates(k_n_p__d)*Y(jn)*state % rho - 2.0e0_rt*screened_rates(k_n_p_p__p)*Y(jn)* &
      Y(jp)*state % rho**2 - screened_rates(k_p_al26__si27)*Y(jal26)*state % rho - &
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*state % rho - &
      screened_rates(k_p_al27__si28)*Y(jal27)*state % rho - &
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*state % rho - &
      screened_rates(k_p_al28__n_si28)*Y(jal28)*state % rho - &
      screened_rates(k_p_al28__si29)*Y(jal28)*state % rho - &
      screened_rates(k_p_al29__he4_mg26)*Y(jal29)*state % rho - &
      screened_rates(k_p_al29__n_si29)*Y(jal29)*state % rho - &
      screened_rates(k_p_al29__si30)*Y(jal29)*state % rho - screened_rates(k_p_ar36__k37)* &
      Y(jar36)*state % rho - screened_rates(k_p_ar37__k38)*Y(jar37)*state % rho - &
      screened_rates(k_p_ar38__k39)*Y(jar38)*state % rho - &
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*state % rho - &
      screened_rates(k_p_ar39__k40)*Y(jar39)*state % rho - screened_rates(k_p_c12__n13)* &
      Y(jc12)*state % rho - screened_rates(k_p_c13__n14)*Y(jc13)*state % rho - &
      screened_rates(k_p_c13__n_n13)*Y(jc13)*state % rho - screened_rates(k_p_c14__n15)* &
      Y(jc14)*state % rho - screened_rates(k_p_ca41__sc42)*Y(jca41)*state % rho - &
      screened_rates(k_p_ca42__he4_k39)*Y(jca42)*state % rho - &
      screened_rates(k_p_ca42__sc43)*Y(jca42)*state % rho - screened_rates(k_p_ca43__sc44) &
      *Y(jca43)*state % rho - screened_rates(k_p_ca44__sc45)*Y(jca44)*state % rho &
      - screened_rates(k_p_cl34__ar35)*Y(jcl34)*state % rho - &
      screened_rates(k_p_cl35__ar36)*Y(jcl35)*state % rho - &
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*state % rho - &
      screened_rates(k_p_cl36__ar37)*Y(jcl36)*state % rho - &
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*state % rho - &
      screened_rates(k_p_cl37__ar38)*Y(jcl37)*state % rho - screened_rates(k_p_co53__ni54) &
      *Y(jco53)*state % rho - screened_rates(k_p_co54__ni55)*Y(jco54)*state % rho &
      - screened_rates(k_p_co55__ni56)*Y(jco55)*state % rho - &
      screened_rates(k_p_co56__ni57)*Y(jco56)*state % rho - &
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*state % rho - &
      screened_rates(k_p_co57__ni58)*Y(jco57)*state % rho - &
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*state % rho - &
      screened_rates(k_p_co58__ni59)*Y(jco58)*state % rho - screened_rates(k_p_cr48__mn49) &
      *Y(jcr48)*state % rho - screened_rates(k_p_cr49__mn50)*Y(jcr49)*state % rho &
      - screened_rates(k_p_cr50__mn51)*Y(jcr50)*state % rho - &
      screened_rates(k_p_cr51__mn52)*Y(jcr51)*state % rho - screened_rates(k_p_cr52__mn53) &
      *Y(jcr52)*state % rho - screened_rates(k_p_d__he3)*Y(jd)*state % rho - &
      screened_rates(k_p_f18__ne19)*Y(jf18)*state % rho - screened_rates(k_p_f19__he4_o16) &
      *Y(jf19)*state % rho - screened_rates(k_p_f19__n_ne19)*Y(jf19)*state % rho - &
      screened_rates(k_p_f19__ne20)*Y(jf19)*state % rho - screened_rates(k_p_f20__he4_o17) &
      *Y(jf20)*state % rho - screened_rates(k_p_f20__n_ne20)*Y(jf20)*state % rho - &
      screened_rates(k_p_f20__ne21)*Y(jf20)*state % rho - screened_rates(k_p_f21__he4_o18) &
      *Y(jf21)*state % rho - screened_rates(k_p_f21__n_ne21)*Y(jf21)*state % rho - &
      screened_rates(k_p_f21__ne22)*Y(jf21)*state % rho - screened_rates(k_p_fe52__co53)* &
      Y(jfe52)*state % rho - screened_rates(k_p_fe53__co54)*Y(jfe53)*state % rho - &
      screened_rates(k_p_fe54__co55)*Y(jfe54)*state % rho - screened_rates(k_p_fe55__co56) &
      *Y(jfe55)*state % rho - screened_rates(k_p_fe56__co57)*Y(jfe56)*state % rho &
      - screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jhe3)*state % rho - &
      screened_rates(k_p_k38__ca39)*Y(jk38)*state % rho - screened_rates(k_p_k39__ca40)* &
      Y(jk39)*state % rho - screened_rates(k_p_k39__he4_ar36)*Y(jk39)*state % rho &
      - screened_rates(k_p_k40__ca41)*Y(jk40)*state % rho - &
      screened_rates(k_p_k40__he4_ar37)*Y(jk40)*state % rho - &
      screened_rates(k_p_k40__n_ca40)*Y(jk40)*state % rho - screened_rates(k_p_k41__ca42)* &
      Y(jk41)*state % rho - screened_rates(k_p_k41__he4_ar38)*Y(jk41)*state % rho &
      - screened_rates(k_p_k41__n_ca41)*Y(jk41)*state % rho - &
      screened_rates(k_p_k42__ca43)*Y(jk42)*state % rho - &
      screened_rates(k_p_k42__he4_ar39)*Y(jk42)*state % rho - &
      screened_rates(k_p_k42__n_ca42)*Y(jk42)*state % rho - screened_rates(k_p_mg24__al25) &
      *Y(jmg24)*state % rho - screened_rates(k_p_mg24__he4_na21)*Y(jmg24)* &
      state % rho - screened_rates(k_p_mg25__al26)*Y(jmg25)*state % rho - &
      screened_rates(k_p_mg26__al27)*Y(jmg26)*state % rho - screened_rates(k_p_mg27__al28) &
      *Y(jmg27)*state % rho - screened_rates(k_p_mg27__n_al27)*Y(jmg27)* &
      state % rho - screened_rates(k_p_mn50__fe51)*Y(jmn50)*state % rho - &
      screened_rates(k_p_mn51__fe52)*Y(jmn51)*state % rho - screened_rates(k_p_mn52__fe53) &
      *Y(jmn52)*state % rho - screened_rates(k_p_mn53__fe54)*Y(jmn53)*state % rho &
      - screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)*state % rho - &
      screened_rates(k_p_mn54__fe55)*Y(jmn54)*state % rho - &
      screened_rates(k_p_mn54__he4_cr51)*Y(jmn54)*state % rho - &
      screened_rates(k_p_mn55__fe56)*Y(jmn55)*state % rho - &
      screened_rates(k_p_mn55__he4_cr52)*Y(jmn55)*state % rho - &
      screened_rates(k_p_n15__he4_c12)*Y(jn15)*state % rho - screened_rates(k_p_n15__o16)* &
      Y(jn15)*state % rho - screened_rates(k_p_na21__mg22)*Y(jna21)*state % rho - &
      screened_rates(k_p_na22__mg23)*Y(jna22)*state % rho - &
      screened_rates(k_p_na23__he4_ne20)*Y(jna23)*state % rho - &
      screened_rates(k_p_na23__mg24)*Y(jna23)*state % rho - &
      screened_rates(k_p_na23__n_mg23)*Y(jna23)*state % rho - &
      screened_rates(k_p_na24__he4_ne21)*Y(jna24)*state % rho - &
      screened_rates(k_p_na24__mg25)*Y(jna24)*state % rho - &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*state % rho - &
      screened_rates(k_p_ne19__na20)*Y(jne19)*state % rho - screened_rates(k_p_ne20__na21) &
      *Y(jne20)*state % rho - screened_rates(k_p_ne21__na22)*Y(jne21)*state % rho &
      - screened_rates(k_p_ne22__na23)*Y(jne22)*state % rho - &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*state % rho - &
      screened_rates(k_p_ne23__na24)*Y(jne23)*state % rho - &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*state % rho - screened_rates(k_p_o17__f18) &
      *Y(jo17)*state % rho - screened_rates(k_p_o17__he4_n14)*Y(jo17)*state % rho &
      - screened_rates(k_p_o18__f19)*Y(jo18)*state % rho - &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*state % rho - screened_rates(k_p_o19__f20)* &
      Y(jo19)*state % rho - screened_rates(k_p_o19__n_f19)*Y(jo19)*state % rho - &
      screened_rates(k_p_p29__s30)*Y(jp29)*state % rho - screened_rates(k_p_p30__s31)* &
      Y(jp30)*state % rho - screened_rates(k_p_p31__he4_si28)*Y(jp31)*state % rho &
      - screened_rates(k_p_p31__s32)*Y(jp31)*state % rho - &
      screened_rates(k_p_p32__he4_si29)*Y(jp32)*state % rho - &
      screened_rates(k_p_p32__n_s32)*Y(jp32)*state % rho - screened_rates(k_p_p32__s33)* &
      Y(jp32)*state % rho - screened_rates(k_p_p33__he4_si30)*Y(jp33)*state % rho &
      - screened_rates(k_p_p33__s34)*Y(jp33)*state % rho - 2.0e0_rt* &
      screened_rates(k_p_p__d__weak__bet_pos_)*Y(jp)*state % rho - 2.0e0_rt* &
      screened_rates(k_p_p__d__weak__electron_capture)*Y(jp)*state % rho**2* &
      state % y_e - screened_rates(k_p_s32__cl33)*Y(js32)*state % rho - &
      screened_rates(k_p_s33__cl34)*Y(js33)*state % rho - screened_rates(k_p_s34__cl35)* &
      Y(js34)*state % rho - screened_rates(k_p_s35__cl36)*Y(js35)*state % rho - &
      screened_rates(k_p_s35__he4_p32)*Y(js35)*state % rho - &
      screened_rates(k_p_sc42__he4_ca39)*Y(jsc42)*state % rho - &
      screened_rates(k_p_sc42__ti43)*Y(jsc42)*state % rho - &
      screened_rates(k_p_sc43__he4_ca40)*Y(jsc43)*state % rho - &
      screened_rates(k_p_sc43__ti44)*Y(jsc43)*state % rho - &
      screened_rates(k_p_sc44__he4_ca41)*Y(jsc44)*state % rho - &
      screened_rates(k_p_sc44__ti45)*Y(jsc44)*state % rho - &
      screened_rates(k_p_sc45__he4_ca42)*Y(jsc45)*state % rho - &
      screened_rates(k_p_sc45__ti46)*Y(jsc45)*state % rho - &
      screened_rates(k_p_sc46__he4_ca43)*Y(jsc46)*state % rho - &
      screened_rates(k_p_sc46__n_ti46)*Y(jsc46)*state % rho - &
      screened_rates(k_p_sc46__ti47)*Y(jsc46)*state % rho - screened_rates(k_p_si28__p29)* &
      Y(jsi28)*state % rho - screened_rates(k_p_si29__p30)*Y(jsi29)*state % rho - &
      screened_rates(k_p_si30__p31)*Y(jsi30)*state % rho - screened_rates(k_p_si31__n_p31) &
      *Y(jsi31)*state % rho - screened_rates(k_p_si31__p32)*Y(jsi31)*state % rho - &
      screened_rates(k_p_si32__p33)*Y(jsi32)*state % rho - screened_rates(k_p_t__he4)* &
      Y(jt)*state % rho - screened_rates(k_p_ti45__v46)*Y(jti45)*state % rho - &
      screened_rates(k_p_ti46__v47)*Y(jti46)*state % rho - screened_rates(k_p_ti47__v48)* &
      Y(jti47)*state % rho - screened_rates(k_p_ti48__v49)*Y(jti48)*state % rho - &
      screened_rates(k_p_ti49__v50)*Y(jti49)*state % rho - screened_rates(k_p_v46__cr47)* &
      Y(jv46)*state % rho - screened_rates(k_p_v47__cr48)*Y(jv47)*state % rho - &
      screened_rates(k_p_v48__cr49)*Y(jv48)*state % rho - screened_rates(k_p_v49__cr50)* &
      Y(jv49)*state % rho - screened_rates(k_p_v49__he4_ti46)*Y(jv49)*state % rho &
      - screened_rates(k_p_v50__cr51)*Y(jv50)*state % rho - &
      screened_rates(k_p_v50__he4_ti47)*Y(jv50)*state % rho - &
      screened_rates(k_p_v50__n_cr50)*Y(jv50)*state % rho - screened_rates(k_p_v51__cr52)* &
      Y(jv51)*state % rho - screened_rates(k_p_v51__he4_ti48)*Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jp, jp, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_d_d__p_t)*Y(jd)*state % rho + screened_rates(k_d_he3__p_he4)* &
      Y(jhe3)*state % rho - screened_rates(k_p_d__he3)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jd, scratch)

    scratch = (&
      -screened_rates(k_p_t__he4)*Y(jp)*state % rho + screened_rates(k_t_he3__n_p_he4)*Y(jhe3)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jt, scratch)

    scratch = (&
      screened_rates(k_d_he3__p_he4)*Y(jd)*state % rho + 2.0e0_rt*screened_rates(k_he3_he3__p_p_he4) &
      *Y(jhe3)*state % rho + screened_rates(k_n_he3__p_t)*Y(jn)*state % rho - &
      screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jp)*state % rho + &
      screened_rates(k_t_he3__n_p_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jp, jhe3, scratch)

    scratch = (&
      screened_rates(k_he4_al25__p_si28)*Y(jal25)*state % rho + &
      screened_rates(k_he4_al26__p_si29)*Y(jal26)*state % rho + &
      screened_rates(k_he4_al27__p_si30)*Y(jal27)*state % rho + &
      screened_rates(k_he4_al28__p_si31)*Y(jal28)*state % rho + &
      screened_rates(k_he4_al29__p_si32)*Y(jal29)*state % rho + &
      screened_rates(k_he4_ar35__p_k38)*Y(jar35)*state % rho + &
      screened_rates(k_he4_cl33__p_ar36)*Y(jcl33)*state % rho + &
      screened_rates(k_he4_cl34__p_ar37)*Y(jcl34)*state % rho + &
      screened_rates(k_he4_cl35__p_ar38)*Y(jcl35)*state % rho + &
      screened_rates(k_he4_co53__p_ni56)*Y(jco53)*state % rho + &
      screened_rates(k_he4_co54__p_ni57)*Y(jco54)*state % rho + &
      screened_rates(k_he4_co55__p_ni58)*Y(jco55)*state % rho + &
      screened_rates(k_he4_co56__p_ni59)*Y(jco56)*state % rho + &
      screened_rates(k_he4_co57__p_ni60)*Y(jco57)*state % rho + &
      screened_rates(k_he4_cr47__p_mn50)*Y(jcr47)*state % rho + &
      screened_rates(k_he4_cr48__p_mn51)*Y(jcr48)*state % rho + &
      screened_rates(k_he4_cr49__p_mn52)*Y(jcr49)*state % rho + &
      screened_rates(k_he4_f18__p_ne21)*Y(jf18)*state % rho + &
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*state % rho + &
      screened_rates(k_he4_f20__p_ne23)*Y(jf20)*state % rho + &
      screened_rates(k_he4_f21__p_ne24)*Y(jf21)*state % rho + &
      screened_rates(k_he4_fe51__p_co54)*Y(jfe51)*state % rho + &
      screened_rates(k_he4_fe52__p_co55)*Y(jfe52)*state % rho + &
      screened_rates(k_he4_fe53__p_co56)*Y(jfe53)*state % rho + &
      screened_rates(k_he4_k37__p_ca40)*Y(jk37)*state % rho + &
      screened_rates(k_he4_k38__p_ca41)*Y(jk38)*state % rho + &
      screened_rates(k_he4_k40__p_ca43)*Y(jk40)*state % rho + &
      screened_rates(k_he4_k41__p_ca44)*Y(jk41)*state % rho + &
      screened_rates(k_he4_mg22__p_al25)*Y(jmg22)*state % rho + &
      screened_rates(k_he4_mg23__p_al26)*Y(jmg23)*state % rho + &
      screened_rates(k_he4_mn49__p_fe52)*Y(jmn49)*state % rho + &
      screened_rates(k_he4_mn50__p_fe53)*Y(jmn50)*state % rho + &
      screened_rates(k_he4_mn51__p_fe54)*Y(jmn51)*state % rho + &
      screened_rates(k_he4_mn52__p_fe55)*Y(jmn52)*state % rho + &
      screened_rates(k_he4_mn53__p_fe56)*Y(jmn53)*state % rho + &
      screened_rates(k_he4_n13__p_o16)*Y(jn13)*state % rho + &
      screened_rates(k_he4_na20__p_mg23)*Y(jna20)*state % rho + &
      screened_rates(k_he4_na22__p_mg25)*Y(jna22)*state % rho + &
      screened_rates(k_he4_na23__p_mg26)*Y(jna23)*state % rho + &
      screened_rates(k_he4_na24__p_mg27)*Y(jna24)*state % rho + &
      screened_rates(k_he4_ne19__p_na22)*Y(jne19)*state % rho + &
      screened_rates(k_he4_p29__p_s32)*Y(jp29)*state % rho + &
      screened_rates(k_he4_p30__p_s33)*Y(jp30)*state % rho + &
      screened_rates(k_he4_p31__p_s34)*Y(jp31)*state % rho + &
      screened_rates(k_he4_s30__p_cl33)*Y(js30)*state % rho + &
      screened_rates(k_he4_s31__p_cl34)*Y(js31)*state % rho + &
      screened_rates(k_he4_s34__p_cl37)*Y(js34)*state % rho + &
      screened_rates(k_he4_sc42__p_ti45)*Y(jsc42)*state % rho + &
      screened_rates(k_he4_sc43__p_ti46)*Y(jsc43)*state % rho + &
      screened_rates(k_he4_sc44__p_ti47)*Y(jsc44)*state % rho + &
      screened_rates(k_he4_sc45__p_ti48)*Y(jsc45)*state % rho + &
      screened_rates(k_he4_sc46__p_ti49)*Y(jsc46)*state % rho + &
      screened_rates(k_he4_si27__p_p30)*Y(jsi27)*state % rho + &
      screened_rates(k_he4_ti43__p_v46)*Y(jti43)*state % rho + &
      screened_rates(k_he4_ti44__p_v47)*Y(jti44)*state % rho + &
      screened_rates(k_he4_ti45__p_v48)*Y(jti45)*state % rho + &
      screened_rates(k_he4_v46__p_cr49)*Y(jv46)*state % rho + &
      screened_rates(k_he4_v47__p_cr50)*Y(jv47)*state % rho + &
      screened_rates(k_he4_v48__p_cr51)*Y(jv48)*state % rho + &
      screened_rates(k_he4_v49__p_cr52)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jp, jhe4, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_c12_c12__p_na23)*Y(jc12)*state % rho + &
      screened_rates(k_c12_ne20__p_p31)*Y(jne20)*state % rho + &
      screened_rates(k_c12_o16__p_al27)*Y(jo16)*state % rho - screened_rates(k_p_c12__n13) &
      *Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jc12, scratch)

    scratch = (&
      -screened_rates(k_p_c13__n14)*Y(jp)*state % rho - screened_rates(k_p_c13__n_n13)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jc13, scratch)

    scratch = (&
      -screened_rates(k_p_c14__n15)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jc14, scratch)

    scratch = (&
      screened_rates(k_he4_n13__p_o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp, jn13, scratch)

    scratch = (&
      screened_rates(k_n_n14__p_c14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jn14, scratch)

    scratch = (&
      -screened_rates(k_p_n15__he4_c12)*Y(jp)*state % rho - screened_rates(k_p_n15__o16)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jn15, scratch)

    scratch = (&
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*state % rho + 1.0e0_rt* &
      screened_rates(k_o16_o16__p_p31)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jp, jo16, scratch)

    scratch = (&
      -screened_rates(k_p_o17__f18)*Y(jp)*state % rho - screened_rates(k_p_o17__he4_n14)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jo17, scratch)

    scratch = (&
      -screened_rates(k_p_o18__f19)*Y(jp)*state % rho - screened_rates(k_p_o18__he4_n15)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jo18, scratch)

    scratch = (&
      -screened_rates(k_p_o19__f20)*Y(jp)*state % rho - screened_rates(k_p_o19__n_f19)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jo19, scratch)

    scratch = (&
      screened_rates(k_he4_f18__p_ne21)*Y(jhe4)*state % rho + screened_rates(k_n_f18__p_o18)* &
      Y(jn)*state % rho - screened_rates(k_p_f18__ne19)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jf18, scratch)

    scratch = (&
      screened_rates(k_he4_f19__p_ne22)*Y(jhe4)*state % rho - screened_rates(k_p_f19__he4_o16)* &
      Y(jp)*state % rho - screened_rates(k_p_f19__n_ne19)*Y(jp)*state % rho - &
      screened_rates(k_p_f19__ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jf19, scratch)

    scratch = (&
      screened_rates(k_he4_f20__p_ne23)*Y(jhe4)*state % rho - screened_rates(k_p_f20__he4_o17)* &
      Y(jp)*state % rho - screened_rates(k_p_f20__n_ne20)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__ne21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jf20, scratch)

    scratch = (&
      screened_rates(k_he4_f21__p_ne24)*Y(jhe4)*state % rho - screened_rates(k_p_f21__he4_o18)* &
      Y(jp)*state % rho - screened_rates(k_p_f21__n_ne21)*Y(jp)*state % rho - &
      screened_rates(k_p_f21__ne22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jf21, scratch)

    scratch = (&
      screened_rates(k_he4_ne19__p_na22)*Y(jhe4)*state % rho - screened_rates(k_p_ne19__na20)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jne19, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*state % rho - screened_rates(k_p_ne20__na21)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jne20, scratch)

    scratch = (&
      -screened_rates(k_p_ne21__na22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jne21, scratch)

    scratch = (&
      -screened_rates(k_p_ne22__na23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jne22, scratch)

    scratch = (&
      -screened_rates(k_p_ne23__n_na23)*Y(jp)*state % rho - screened_rates(k_p_ne23__na24)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jne23, scratch)

    scratch = (&
      -screened_rates(k_p_ne24__n_na24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jne24, scratch)

    scratch = (&
      screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*state % rho + screened_rates(k_n_na20__p_ne20)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jna20, scratch)

    scratch = (&
      screened_rates(k_n_na21__p_ne21)*Y(jn)*state % rho - screened_rates(k_p_na21__mg22)*Y(jp) &
      *state % rho &
       )
    call set_jac_entry(jac, jp, jna21, scratch)

    scratch = (&
      screened_rates(k_he4_na22__p_mg25)*Y(jhe4)*state % rho + screened_rates(k_n_na22__p_ne22)* &
      Y(jn)*state % rho - screened_rates(k_p_na22__mg23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jna22, scratch)

    scratch = (&
      screened_rates(k_he4_na23__p_mg26)*Y(jhe4)*state % rho - screened_rates(k_p_na23__he4_ne20) &
      *Y(jp)*state % rho - screened_rates(k_p_na23__mg24)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__n_mg23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jna23, scratch)

    scratch = (&
      screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*state % rho - screened_rates(k_p_na24__he4_ne21) &
      *Y(jp)*state % rho - screened_rates(k_p_na24__mg25)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__n_mg24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jna24, scratch)

    scratch = (&
      screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*state % rho + screened_rates(k_n_mg22__p_na22)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jmg22, scratch)

    scratch = (&
      screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp, jmg23, scratch)

    scratch = (&
      -screened_rates(k_p_mg24__al25)*Y(jp)*state % rho - screened_rates(k_p_mg24__he4_na21)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmg24, scratch)

    scratch = (&
      -screened_rates(k_p_mg25__al26)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmg25, scratch)

    scratch = (&
      -screened_rates(k_p_mg26__al27)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmg26, scratch)

    scratch = (&
      -screened_rates(k_p_mg27__al28)*Y(jp)*state % rho - screened_rates(k_p_mg27__n_al27)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmg27, scratch)

    scratch = (&
      screened_rates(k_he4_al25__p_si28)*Y(jhe4)*state % rho + screened_rates(k_n_al25__p_mg25)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jal25, scratch)

    scratch = (&
      screened_rates(k_he4_al26__p_si29)*Y(jhe4)*state % rho + screened_rates(k_n_al26__p_mg26)* &
      Y(jn)*state % rho - screened_rates(k_p_al26__si27)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jal26, scratch)

    scratch = (&
      screened_rates(k_he4_al27__p_si30)*Y(jhe4)*state % rho - screened_rates(k_p_al27__he4_mg24) &
      *Y(jp)*state % rho - screened_rates(k_p_al27__si28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jal27, scratch)

    scratch = (&
      screened_rates(k_he4_al28__p_si31)*Y(jhe4)*state % rho - screened_rates(k_p_al28__he4_mg25) &
      *Y(jp)*state % rho - screened_rates(k_p_al28__n_si28)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__si29)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jal28, scratch)

    scratch = (&
      screened_rates(k_he4_al29__p_si32)*Y(jhe4)*state % rho - screened_rates(k_p_al29__he4_mg26) &
      *Y(jp)*state % rho - screened_rates(k_p_al29__n_si29)*Y(jp)*state % rho - &
      screened_rates(k_p_al29__si30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jal29, scratch)

    scratch = (&
      screened_rates(k_he4_si27__p_p30)*Y(jhe4)*state % rho + screened_rates(k_n_si27__p_al27)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jsi27, scratch)

    scratch = (&
      -screened_rates(k_p_si28__p29)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsi28, scratch)

    scratch = (&
      -screened_rates(k_p_si29__p30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsi29, scratch)

    scratch = (&
      -screened_rates(k_p_si30__p31)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsi30, scratch)

    scratch = (&
      -screened_rates(k_p_si31__n_p31)*Y(jp)*state % rho - screened_rates(k_p_si31__p32)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jsi31, scratch)

    scratch = (&
      -screened_rates(k_p_si32__p33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsi32, scratch)

    scratch = (&
      screened_rates(k_he4_p29__p_s32)*Y(jhe4)*state % rho + screened_rates(k_n_p29__p_si29)* &
      Y(jn)*state % rho - screened_rates(k_p_p29__s30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jp29, scratch)

    scratch = (&
      screened_rates(k_he4_p30__p_s33)*Y(jhe4)*state % rho + screened_rates(k_n_p30__p_si30)* &
      Y(jn)*state % rho - screened_rates(k_p_p30__s31)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jp30, scratch)

    scratch = (&
      screened_rates(k_he4_p31__p_s34)*Y(jhe4)*state % rho - screened_rates(k_p_p31__he4_si28)* &
      Y(jp)*state % rho - screened_rates(k_p_p31__s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jp31, scratch)

    scratch = (&
      screened_rates(k_n_p32__p_si32)*Y(jn)*state % rho - screened_rates(k_p_p32__he4_si29)* &
      Y(jp)*state % rho - screened_rates(k_p_p32__n_s32)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__s33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jp32, scratch)

    scratch = (&
      -screened_rates(k_p_p33__he4_si30)*Y(jp)*state % rho - screened_rates(k_p_p33__s34)*Y(jp) &
      *state % rho &
       )
    call set_jac_entry(jac, jp, jp33, scratch)

    scratch = (&
      screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*state % rho + screened_rates(k_n_s30__p_p30)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, js30, scratch)

    scratch = (&
      screened_rates(k_he4_s31__p_cl34)*Y(jhe4)*state % rho + screened_rates(k_n_s31__p_p31)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, js31, scratch)

    scratch = (&
      -screened_rates(k_p_s32__cl33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, js32, scratch)

    scratch = (&
      screened_rates(k_n_s33__p_p33)*Y(jn)*state % rho - screened_rates(k_p_s33__cl34)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, js33, scratch)

    scratch = (&
      screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*state % rho - screened_rates(k_p_s34__cl35)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, js34, scratch)

    scratch = (&
      -screened_rates(k_p_s35__cl36)*Y(jp)*state % rho - screened_rates(k_p_s35__he4_p32)*Y(jp) &
      *state % rho &
       )
    call set_jac_entry(jac, jp, js35, scratch)

    scratch = (&
      screened_rates(k_he4_cl33__p_ar36)*Y(jhe4)*state % rho + screened_rates(k_n_cl33__p_s33)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jcl33, scratch)

    scratch = (&
      screened_rates(k_he4_cl34__p_ar37)*Y(jhe4)*state % rho + screened_rates(k_n_cl34__p_s34)* &
      Y(jn)*state % rho - screened_rates(k_p_cl34__ar35)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcl34, scratch)

    scratch = (&
      screened_rates(k_he4_cl35__p_ar38)*Y(jhe4)*state % rho + screened_rates(k_n_cl35__p_s35)* &
      Y(jn)*state % rho - screened_rates(k_p_cl35__ar36)*Y(jp)*state % rho - &
      screened_rates(k_p_cl35__he4_s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcl35, scratch)

    scratch = (&
      -screened_rates(k_p_cl36__ar37)*Y(jp)*state % rho - screened_rates(k_p_cl36__he4_s33)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcl36, scratch)

    scratch = (&
      -screened_rates(k_p_cl37__ar38)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcl37, scratch)

    scratch = (&
      screened_rates(k_he4_ar35__p_k38)*Y(jhe4)*state % rho + screened_rates(k_n_ar35__p_cl35)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jar35, scratch)

    scratch = (&
      screened_rates(k_n_ar36__p_cl36)*Y(jn)*state % rho - screened_rates(k_p_ar36__k37)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jar36, scratch)

    scratch = (&
      screened_rates(k_n_ar37__p_cl37)*Y(jn)*state % rho - screened_rates(k_p_ar37__k38)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jar37, scratch)

    scratch = (&
      -screened_rates(k_p_ar38__k39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jar38, scratch)

    scratch = (&
      -screened_rates(k_p_ar39__he4_cl36)*Y(jp)*state % rho - screened_rates(k_p_ar39__k40)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jar39, scratch)

    scratch = (&
      screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*state % rho + screened_rates(k_n_k37__p_ar37)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jk37, scratch)

    scratch = (&
      screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*state % rho + screened_rates(k_n_k38__p_ar38)* &
      Y(jn)*state % rho - screened_rates(k_p_k38__ca39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jk38, scratch)

    scratch = (&
      screened_rates(k_n_k39__p_ar39)*Y(jn)*state % rho - screened_rates(k_p_k39__ca40)*Y(jp)* &
      state % rho - screened_rates(k_p_k39__he4_ar36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jk39, scratch)

    scratch = (&
      screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*state % rho - screened_rates(k_p_k40__ca41)* &
      Y(jp)*state % rho - screened_rates(k_p_k40__he4_ar37)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__n_ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jk40, scratch)

    scratch = (&
      screened_rates(k_he4_k41__p_ca44)*Y(jhe4)*state % rho - screened_rates(k_p_k41__ca42)* &
      Y(jp)*state % rho - screened_rates(k_p_k41__he4_ar38)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__n_ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jk41, scratch)

    scratch = (&
      -screened_rates(k_p_k42__ca43)*Y(jp)*state % rho - screened_rates(k_p_k42__he4_ar39)* &
      Y(jp)*state % rho - screened_rates(k_p_k42__n_ca42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jk42, scratch)

    scratch = (&
      screened_rates(k_n_ca39__p_k39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jca39, scratch)

    scratch = (&
      -screened_rates(k_p_ca41__sc42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jca41, scratch)

    scratch = (&
      -screened_rates(k_p_ca42__he4_k39)*Y(jp)*state % rho - screened_rates(k_p_ca42__sc43)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jca42, scratch)

    scratch = (&
      -screened_rates(k_p_ca43__sc44)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jca43, scratch)

    scratch = (&
      -screened_rates(k_p_ca44__sc45)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jca44, scratch)

    scratch = (&
      screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*state % rho + screened_rates(k_n_sc42__p_ca42)* &
      Y(jn)*state % rho - screened_rates(k_p_sc42__he4_ca39)*Y(jp)*state % rho - &
      screened_rates(k_p_sc42__ti43)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsc42, scratch)

    scratch = (&
      screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*state % rho + screened_rates(k_n_sc43__p_ca43)* &
      Y(jn)*state % rho - screened_rates(k_p_sc43__he4_ca40)*Y(jp)*state % rho - &
      screened_rates(k_p_sc43__ti44)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsc43, scratch)

    scratch = (&
      screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*state % rho + screened_rates(k_n_sc44__p_ca44)* &
      Y(jn)*state % rho - screened_rates(k_p_sc44__he4_ca41)*Y(jp)*state % rho - &
      screened_rates(k_p_sc44__ti45)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsc44, scratch)

    scratch = (&
      screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*state % rho - screened_rates(k_p_sc45__he4_ca42) &
      *Y(jp)*state % rho - screened_rates(k_p_sc45__ti46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsc45, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__p_ti49)*Y(jhe4)*state % rho - screened_rates(k_p_sc46__he4_ca43) &
      *Y(jp)*state % rho - screened_rates(k_p_sc46__n_ti46)*Y(jp)*state % rho - &
      screened_rates(k_p_sc46__ti47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jsc46, scratch)

    scratch = (&
      screened_rates(k_he4_ti43__p_v46)*Y(jhe4)*state % rho + screened_rates(k_n_ti43__p_sc43)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jti43, scratch)

    scratch = (&
      screened_rates(k_he4_ti44__p_v47)*Y(jhe4)*state % rho + screened_rates(k_n_ti44__p_sc44)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jti44, scratch)

    scratch = (&
      screened_rates(k_he4_ti45__p_v48)*Y(jhe4)*state % rho + screened_rates(k_n_ti45__p_sc45)* &
      Y(jn)*state % rho - screened_rates(k_p_ti45__v46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jti45, scratch)

    scratch = (&
      -screened_rates(k_p_ti46__v47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jti46, scratch)

    scratch = (&
      -screened_rates(k_p_ti47__v48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jti47, scratch)

    scratch = (&
      -screened_rates(k_p_ti48__v49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jti48, scratch)

    scratch = (&
      -screened_rates(k_p_ti49__v50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jti49, scratch)

    scratch = (&
      screened_rates(k_he4_v46__p_cr49)*Y(jhe4)*state % rho + screened_rates(k_n_v46__p_ti46)* &
      Y(jn)*state % rho - screened_rates(k_p_v46__cr47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jv46, scratch)

    scratch = (&
      screened_rates(k_he4_v47__p_cr50)*Y(jhe4)*state % rho + screened_rates(k_n_v47__p_ti47)* &
      Y(jn)*state % rho - screened_rates(k_p_v47__cr48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jv47, scratch)

    scratch = (&
      screened_rates(k_he4_v48__p_cr51)*Y(jhe4)*state % rho + screened_rates(k_n_v48__p_ti48)* &
      Y(jn)*state % rho - screened_rates(k_p_v48__cr49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jv48, scratch)

    scratch = (&
      screened_rates(k_he4_v49__p_cr52)*Y(jhe4)*state % rho + screened_rates(k_n_v49__p_ti49)* &
      Y(jn)*state % rho - screened_rates(k_p_v49__cr50)*Y(jp)*state % rho - &
      screened_rates(k_p_v49__he4_ti46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jv49, scratch)

    scratch = (&
      -screened_rates(k_p_v50__cr51)*Y(jp)*state % rho - screened_rates(k_p_v50__he4_ti47)* &
      Y(jp)*state % rho - screened_rates(k_p_v50__n_cr50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jv50, scratch)

    scratch = (&
      -screened_rates(k_p_v51__cr52)*Y(jp)*state % rho - screened_rates(k_p_v51__he4_ti48)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jv51, scratch)

    scratch = (&
      screened_rates(k_he4_cr47__p_mn50)*Y(jhe4)*state % rho + screened_rates(k_n_cr47__p_v47)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jcr47, scratch)

    scratch = (&
      screened_rates(k_he4_cr48__p_mn51)*Y(jhe4)*state % rho + screened_rates(k_n_cr48__p_v48)* &
      Y(jn)*state % rho - screened_rates(k_p_cr48__mn49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcr48, scratch)

    scratch = (&
      screened_rates(k_he4_cr49__p_mn52)*Y(jhe4)*state % rho + screened_rates(k_n_cr49__p_v49)* &
      Y(jn)*state % rho - screened_rates(k_p_cr49__mn50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcr49, scratch)

    scratch = (&
      -screened_rates(k_p_cr50__mn51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcr50, scratch)

    scratch = (&
      screened_rates(k_n_cr51__p_v51)*Y(jn)*state % rho - screened_rates(k_p_cr51__mn52)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp, jcr51, scratch)

    scratch = (&
      -screened_rates(k_p_cr52__mn53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jcr52, scratch)

    scratch = (&
      screened_rates(k_he4_mn49__p_fe52)*Y(jhe4)*state % rho + screened_rates(k_n_mn49__p_cr49)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn49, scratch)

    scratch = (&
      screened_rates(k_he4_mn50__p_fe53)*Y(jhe4)*state % rho + screened_rates(k_n_mn50__p_cr50)* &
      Y(jn)*state % rho - screened_rates(k_p_mn50__fe51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn50, scratch)

    scratch = (&
      screened_rates(k_he4_mn51__p_fe54)*Y(jhe4)*state % rho + screened_rates(k_n_mn51__p_cr51)* &
      Y(jn)*state % rho - screened_rates(k_p_mn51__fe52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn51, scratch)

    scratch = (&
      screened_rates(k_he4_mn52__p_fe55)*Y(jhe4)*state % rho + screened_rates(k_n_mn52__p_cr52)* &
      Y(jn)*state % rho - screened_rates(k_p_mn52__fe53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn52, scratch)

    scratch = (&
      screened_rates(k_he4_mn53__p_fe56)*Y(jhe4)*state % rho - screened_rates(k_p_mn53__fe54)* &
      Y(jp)*state % rho - screened_rates(k_p_mn53__he4_cr50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn53, scratch)

    scratch = (&
      -screened_rates(k_p_mn54__fe55)*Y(jp)*state % rho - screened_rates(k_p_mn54__he4_cr51)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn54, scratch)

    scratch = (&
      -screened_rates(k_p_mn55__fe56)*Y(jp)*state % rho - screened_rates(k_p_mn55__he4_cr52)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jmn55, scratch)

    scratch = (&
      screened_rates(k_he4_fe51__p_co54)*Y(jhe4)*state % rho + screened_rates(k_n_fe51__p_mn51)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jfe51, scratch)

    scratch = (&
      screened_rates(k_he4_fe52__p_co55)*Y(jhe4)*state % rho + screened_rates(k_n_fe52__p_mn52)* &
      Y(jn)*state % rho - screened_rates(k_p_fe52__co53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jfe52, scratch)

    scratch = (&
      screened_rates(k_he4_fe53__p_co56)*Y(jhe4)*state % rho + screened_rates(k_n_fe53__p_mn53)* &
      Y(jn)*state % rho - screened_rates(k_p_fe53__co54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jfe53, scratch)

    scratch = (&
      screened_rates(k_n_fe54__p_mn54)*Y(jn)*state % rho - screened_rates(k_p_fe54__co55)*Y(jp) &
      *state % rho &
       )
    call set_jac_entry(jac, jp, jfe54, scratch)

    scratch = (&
      screened_rates(k_n_fe55__p_mn55)*Y(jn)*state % rho - screened_rates(k_p_fe55__co56)*Y(jp) &
      *state % rho &
       )
    call set_jac_entry(jac, jp, jfe55, scratch)

    scratch = (&
      -screened_rates(k_p_fe56__co57)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jfe56, scratch)

    scratch = (&
      screened_rates(k_he4_co53__p_ni56)*Y(jhe4)*state % rho + screened_rates(k_n_co53__p_fe53)* &
      Y(jn)*state % rho - screened_rates(k_p_co53__ni54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jco53, scratch)

    scratch = (&
      screened_rates(k_he4_co54__p_ni57)*Y(jhe4)*state % rho + screened_rates(k_n_co54__p_fe54)* &
      Y(jn)*state % rho - screened_rates(k_p_co54__ni55)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jco54, scratch)

    scratch = (&
      screened_rates(k_he4_co55__p_ni58)*Y(jhe4)*state % rho + screened_rates(k_n_co55__p_fe55)* &
      Y(jn)*state % rho - screened_rates(k_p_co55__ni56)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jco55, scratch)

    scratch = (&
      screened_rates(k_he4_co56__p_ni59)*Y(jhe4)*state % rho + screened_rates(k_n_co56__p_fe56)* &
      Y(jn)*state % rho - screened_rates(k_p_co56__ni57)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jco56, scratch)

    scratch = (&
      screened_rates(k_he4_co57__p_ni60)*Y(jhe4)*state % rho - screened_rates(k_p_co57__he4_fe54) &
      *Y(jp)*state % rho - screened_rates(k_p_co57__ni58)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jco57, scratch)

    scratch = (&
      -screened_rates(k_p_co58__he4_fe55)*Y(jp)*state % rho - screened_rates(k_p_co58__ni59)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp, jco58, scratch)

    scratch = (&
      screened_rates(k_n_ni54__p_co54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jni54, scratch)

    scratch = (&
      screened_rates(k_n_ni55__p_co55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jni55, scratch)

    scratch = (&
      screened_rates(k_n_ni56__p_co56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jni56, scratch)

    scratch = (&
      screened_rates(k_n_ni57__p_co57)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jni57, scratch)

    scratch = (&
      screened_rates(k_n_ni58__p_co58)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp, jni58, scratch)

    scratch = (&
      -screened_rates(k_n_d__t)*Y(jd)*state % rho + screened_rates(k_n_p__d)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jd, jn, scratch)

    scratch = (&
      screened_rates(k_n_p__d)*Y(jn)*state % rho - screened_rates(k_p_d__he3)*Y(jd)*state % rho + &
      1.0e0_rt*screened_rates(k_p_p__d__weak__bet_pos_)*Y(jp)*state % rho + 1.0e0_rt* &
      screened_rates(k_p_p__d__weak__electron_capture)*Y(jp)*state % rho**2* &
      state % y_e &
       )
    call set_jac_entry(jac, jd, jp, scratch)

    scratch = (&
      -screened_rates(k_d_c13__n_n14)*Y(jc13)*state % rho - screened_rates(k_d_c14__n_n15)* &
      Y(jc14)*state % rho - 2.0e0_rt*screened_rates(k_d_d__he4)*Y(jd)*state % rho - &
      2.0e0_rt*screened_rates(k_d_d__n_he3)*Y(jd)*state % rho - 2.0e0_rt* &
      screened_rates(k_d_d__p_t)*Y(jd)*state % rho - screened_rates(k_d_he3__p_he4)* &
      Y(jhe3)*state % rho - screened_rates(k_d_t__n_he4)*Y(jt)*state % rho - &
      screened_rates(k_n_d__t)*Y(jn)*state % rho - screened_rates(k_p_d__he3)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jd, jd, scratch)

    scratch = (&
      -screened_rates(k_d_t__n_he4)*Y(jd)*state % rho + screened_rates(k_t_he3__d_he4)*Y(jhe3)* &
      state % rho &
       )
    call set_jac_entry(jac, jd, jt, scratch)

    scratch = (&
      -screened_rates(k_d_he3__p_he4)*Y(jd)*state % rho + screened_rates(k_t_he3__d_he4)*Y(jt)* &
      state % rho &
       )
    call set_jac_entry(jac, jd, jhe3, scratch)

    scratch = (&
      -screened_rates(k_d_c13__n_n14)*Y(jd)*state % rho &
       )
    call set_jac_entry(jac, jd, jc13, scratch)

    scratch = (&
      -screened_rates(k_d_c14__n_n15)*Y(jd)*state % rho &
       )
    call set_jac_entry(jac, jd, jc14, scratch)

    scratch = (&
      screened_rates(k_n_d__t)*Y(jd)*state % rho + screened_rates(k_n_he3__p_t)*Y(jhe3)* &
      state % rho &
       )
    call set_jac_entry(jac, jt, jn, scratch)

    scratch = (&
      -screened_rates(k_p_t__he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jt, jp, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_d_d__p_t)*Y(jd)*state % rho - screened_rates(k_d_t__n_he4)*Y(jt)* &
      state % rho + screened_rates(k_n_d__t)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jt, jd, scratch)

    scratch = (&
      -screened_rates(k_d_t__n_he4)*Y(jd)*state % rho - screened_rates(k_p_t__he4)*Y(jp)* &
      state % rho - screened_rates(k_t__he3__weak__wc12) - screened_rates(k_t_he3__d_he4)* &
      Y(jhe3)*state % rho - screened_rates(k_t_he3__n_p_he4)*Y(jhe3)*state % rho - &
      2.0e0_rt*screened_rates(k_t_t__n_n_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jt, jt, scratch)

    scratch = (&
      screened_rates(k_he3__t__weak__electron_capture)*state % rho*state % y_e + &
      screened_rates(k_n_he3__p_t)*Y(jn)*state % rho - screened_rates(k_t_he3__d_he4)* &
      Y(jt)*state % rho - screened_rates(k_t_he3__n_p_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jt, jhe3, scratch)

    scratch = (&
      -screened_rates(k_n_he3__he4)*Y(jhe3)*state % rho - screened_rates(k_n_he3__p_t)*Y(jhe3)* &
      state % rho &
       )
    call set_jac_entry(jac, jhe3, jn, scratch)

    scratch = (&
      screened_rates(k_p_d__he3)*Y(jd)*state % rho - screened_rates(k_p_he3__he4__weak__bet_pos_) &
      *Y(jhe3)*state % rho &
       )
    call set_jac_entry(jac, jhe3, jp, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_d_d__n_he3)*Y(jd)*state % rho - screened_rates(k_d_he3__p_he4)* &
      Y(jhe3)*state % rho + screened_rates(k_p_d__he3)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe3, jd, scratch)

    scratch = (&
      screened_rates(k_t__he3__weak__wc12) - screened_rates(k_t_he3__d_he4)*Y(jhe3)*state % rho - &
      screened_rates(k_t_he3__n_p_he4)*Y(jhe3)*state % rho &
       )
    call set_jac_entry(jac, jhe3, jt, scratch)

    scratch = (&
      -screened_rates(k_d_he3__p_he4)*Y(jd)*state % rho - &
      screened_rates(k_he3__t__weak__electron_capture)*state % rho*state % y_e - 2.0e0_rt* &
      screened_rates(k_he3_he3__p_p_he4)*Y(jhe3)*state % rho - &
      screened_rates(k_n_he3__he4)*Y(jn)*state % rho - screened_rates(k_n_he3__p_t)* &
      Y(jn)*state % rho - screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jp)* &
      state % rho - screened_rates(k_t_he3__d_he4)*Y(jt)*state % rho - &
      screened_rates(k_t_he3__n_p_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jhe3, jhe3, scratch)

    scratch = (&
      screened_rates(k_n_al25__he4_na22)*Y(jal25)*state % rho + &
      screened_rates(k_n_al26__he4_na23)*Y(jal26)*state % rho + &
      screened_rates(k_n_ar35__he4_s32)*Y(jar35)*state % rho + &
      screened_rates(k_n_ar36__he4_s33)*Y(jar36)*state % rho + &
      screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*state % rho + &
      screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*state % rho + &
      screened_rates(k_n_ca41__he4_ar38)*Y(jca41)*state % rho + &
      screened_rates(k_n_ca42__he4_ar39)*Y(jca42)*state % rho + &
      screened_rates(k_n_cl33__he4_p30)*Y(jcl33)*state % rho + &
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*state % rho + &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*state % rho + &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*state % rho + &
      screened_rates(k_n_co53__he4_mn50)*Y(jco53)*state % rho + &
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*state % rho + &
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*state % rho + &
      screened_rates(k_n_co56__he4_mn53)*Y(jco56)*state % rho + &
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*state % rho + &
      screened_rates(k_n_co58__he4_mn55)*Y(jco58)*state % rho + &
      screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*state % rho + &
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*state % rho + &
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*state % rho + &
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*state % rho + &
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*state % rho + &
      screened_rates(k_n_f18__he4_n15)*Y(jf18)*state % rho + &
      screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*state % rho + &
      screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*state % rho + &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*state % rho + &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*state % rho + &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*state % rho + &
      screened_rates(k_n_he3__he4)*Y(jhe3)*state % rho + screened_rates(k_n_k37__he4_cl34) &
      *Y(jk37)*state % rho + screened_rates(k_n_k38__he4_cl35)*Y(jk38)*state % rho &
      + screened_rates(k_n_k39__he4_cl36)*Y(jk39)*state % rho + &
      screened_rates(k_n_k40__he4_cl37)*Y(jk40)*state % rho + &
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*state % rho + &
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*state % rho + &
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*state % rho + &
      screened_rates(k_n_mn50__he4_v47)*Y(jmn50)*state % rho + &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*state % rho + &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*state % rho + &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*state % rho + &
      screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*state % rho + &
      screened_rates(k_n_na21__he4_f18)*Y(jna21)*state % rho + &
      screened_rates(k_n_na22__he4_f19)*Y(jna22)*state % rho + &
      screened_rates(k_n_ne19__he4_o16)*Y(jne19)*state % rho + &
      screened_rates(k_n_ni54__he4_fe51)*Y(jni54)*state % rho + &
      screened_rates(k_n_ni55__he4_fe52)*Y(jni55)*state % rho + &
      screened_rates(k_n_ni56__he4_fe53)*Y(jni56)*state % rho + &
      screened_rates(k_n_ni57__he4_fe54)*Y(jni57)*state % rho + &
      screened_rates(k_n_ni58__he4_fe55)*Y(jni58)*state % rho + &
      screened_rates(k_n_ni59__he4_fe56)*Y(jni59)*state % rho + &
      screened_rates(k_n_o17__he4_c14)*Y(jo17)*state % rho + &
      screened_rates(k_n_p29__he4_al26)*Y(jp29)*state % rho + &
      screened_rates(k_n_s30__he4_si27)*Y(js30)*state % rho + &
      screened_rates(k_n_s31__he4_si28)*Y(js31)*state % rho + &
      screened_rates(k_n_s32__he4_si29)*Y(js32)*state % rho + &
      screened_rates(k_n_s33__he4_si30)*Y(js33)*state % rho + &
      screened_rates(k_n_s35__he4_si32)*Y(js35)*state % rho + &
      screened_rates(k_n_sc42__he4_k39)*Y(jsc42)*state % rho + &
      screened_rates(k_n_sc43__he4_k40)*Y(jsc43)*state % rho + &
      screened_rates(k_n_si27__he4_mg24)*Y(jsi27)*state % rho + &
      screened_rates(k_n_ti43__he4_ca40)*Y(jti43)*state % rho + &
      screened_rates(k_n_ti44__he4_ca41)*Y(jti44)*state % rho + &
      screened_rates(k_n_ti45__he4_ca42)*Y(jti45)*state % rho + &
      screened_rates(k_n_ti47__he4_ca44)*Y(jti47)*state % rho + &
      screened_rates(k_n_v46__he4_sc43)*Y(jv46)*state % rho + &
      screened_rates(k_n_v47__he4_sc44)*Y(jv47)*state % rho + &
      screened_rates(k_n_v48__he4_sc45)*Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jn, scratch)

    scratch = (&
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*state % rho + &
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*state % rho + &
      screened_rates(k_p_al29__he4_mg26)*Y(jal29)*state % rho + &
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*state % rho + &
      screened_rates(k_p_ca42__he4_k39)*Y(jca42)*state % rho + &
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*state % rho + &
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*state % rho + &
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*state % rho + &
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*state % rho + &
      screened_rates(k_p_f19__he4_o16)*Y(jf19)*state % rho + &
      screened_rates(k_p_f20__he4_o17)*Y(jf20)*state % rho + &
      screened_rates(k_p_f21__he4_o18)*Y(jf21)*state % rho + &
      screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jhe3)*state % rho + &
      screened_rates(k_p_k39__he4_ar36)*Y(jk39)*state % rho + &
      screened_rates(k_p_k40__he4_ar37)*Y(jk40)*state % rho + &
      screened_rates(k_p_k41__he4_ar38)*Y(jk41)*state % rho + &
      screened_rates(k_p_k42__he4_ar39)*Y(jk42)*state % rho + &
      screened_rates(k_p_mg24__he4_na21)*Y(jmg24)*state % rho + &
      screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)*state % rho + &
      screened_rates(k_p_mn54__he4_cr51)*Y(jmn54)*state % rho + &
      screened_rates(k_p_mn55__he4_cr52)*Y(jmn55)*state % rho + &
      screened_rates(k_p_n15__he4_c12)*Y(jn15)*state % rho + &
      screened_rates(k_p_na23__he4_ne20)*Y(jna23)*state % rho + &
      screened_rates(k_p_na24__he4_ne21)*Y(jna24)*state % rho + &
      screened_rates(k_p_o17__he4_n14)*Y(jo17)*state % rho + &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*state % rho + &
      screened_rates(k_p_p31__he4_si28)*Y(jp31)*state % rho + &
      screened_rates(k_p_p32__he4_si29)*Y(jp32)*state % rho + &
      screened_rates(k_p_p33__he4_si30)*Y(jp33)*state % rho + &
      screened_rates(k_p_s35__he4_p32)*Y(js35)*state % rho + &
      screened_rates(k_p_sc42__he4_ca39)*Y(jsc42)*state % rho + &
      screened_rates(k_p_sc43__he4_ca40)*Y(jsc43)*state % rho + &
      screened_rates(k_p_sc44__he4_ca41)*Y(jsc44)*state % rho + &
      screened_rates(k_p_sc45__he4_ca42)*Y(jsc45)*state % rho + &
      screened_rates(k_p_sc46__he4_ca43)*Y(jsc46)*state % rho + screened_rates(k_p_t__he4) &
      *Y(jt)*state % rho + screened_rates(k_p_v49__he4_ti46)*Y(jv49)*state % rho + &
      screened_rates(k_p_v50__he4_ti47)*Y(jv50)*state % rho + &
      screened_rates(k_p_v51__he4_ti48)*Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jp, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_d_d__he4)*Y(jd)*state % rho + screened_rates(k_d_he3__p_he4)* &
      Y(jhe3)*state % rho + screened_rates(k_d_t__n_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jd, scratch)

    scratch = (&
      screened_rates(k_d_t__n_he4)*Y(jd)*state % rho + screened_rates(k_p_t__he4)*Y(jp)* &
      state % rho + screened_rates(k_t_he3__d_he4)*Y(jhe3)*state % rho + &
      screened_rates(k_t_he3__n_p_he4)*Y(jhe3)*state % rho + 1.0e0_rt* &
      screened_rates(k_t_t__n_n_he4)*Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jt, scratch)

    scratch = (&
      screened_rates(k_d_he3__p_he4)*Y(jd)*state % rho + 1.0e0_rt*screened_rates(k_he3_he3__p_p_he4) &
      *Y(jhe3)*state % rho + screened_rates(k_n_he3__he4)*Y(jn)*state % rho + &
      screened_rates(k_p_he3__he4__weak__bet_pos_)*Y(jp)*state % rho + &
      screened_rates(k_t_he3__d_he4)*Y(jt)*state % rho + screened_rates(k_t_he3__n_p_he4)* &
      Y(jt)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jhe3, scratch)

    scratch = (&
      -screened_rates(k_he4_al25__p29)*Y(jal25)*state % rho - screened_rates(k_he4_al25__p_si28)* &
      Y(jal25)*state % rho - screened_rates(k_he4_al26__p30)*Y(jal26)*state % rho &
      - screened_rates(k_he4_al26__p_si29)*Y(jal26)*state % rho - &
      screened_rates(k_he4_al27__n_p30)*Y(jal27)*state % rho - &
      screened_rates(k_he4_al27__p31)*Y(jal27)*state % rho - &
      screened_rates(k_he4_al27__p_si30)*Y(jal27)*state % rho - &
      screened_rates(k_he4_al28__n_p31)*Y(jal28)*state % rho - &
      screened_rates(k_he4_al28__p32)*Y(jal28)*state % rho - &
      screened_rates(k_he4_al28__p_si31)*Y(jal28)*state % rho - &
      screened_rates(k_he4_al29__n_p32)*Y(jal29)*state % rho - &
      screened_rates(k_he4_al29__p33)*Y(jal29)*state % rho - &
      screened_rates(k_he4_al29__p_si32)*Y(jal29)*state % rho - &
      screened_rates(k_he4_ar35__ca39)*Y(jar35)*state % rho - &
      screened_rates(k_he4_ar35__p_k38)*Y(jar35)*state % rho - &
      screened_rates(k_he4_ar36__ca40)*Y(jar36)*state % rho - &
      screened_rates(k_he4_ar37__ca41)*Y(jar37)*state % rho - &
      screened_rates(k_he4_ar38__ca42)*Y(jar38)*state % rho - &
      screened_rates(k_he4_ar39__ca43)*Y(jar39)*state % rho - &
      screened_rates(k_he4_c12__o16)*Y(jc12)*state % rho - &
      screened_rates(k_he4_c13__n_o16)*Y(jc13)*state % rho - &
      screened_rates(k_he4_c14__o18)*Y(jc14)*state % rho - &
      screened_rates(k_he4_ca39__ti43)*Y(jca39)*state % rho - &
      screened_rates(k_he4_ca40__ti44)*Y(jca40)*state % rho - &
      screened_rates(k_he4_ca41__ti45)*Y(jca41)*state % rho - &
      screened_rates(k_he4_ca42__ti46)*Y(jca42)*state % rho - &
      screened_rates(k_he4_ca43__n_ti46)*Y(jca43)*state % rho - &
      screened_rates(k_he4_ca43__ti47)*Y(jca43)*state % rho - &
      screened_rates(k_he4_ca44__ti48)*Y(jca44)*state % rho - &
      screened_rates(k_he4_cl33__k37)*Y(jcl33)*state % rho - &
      screened_rates(k_he4_cl33__p_ar36)*Y(jcl33)*state % rho - &
      screened_rates(k_he4_cl34__k38)*Y(jcl34)*state % rho - &
      screened_rates(k_he4_cl34__p_ar37)*Y(jcl34)*state % rho - &
      screened_rates(k_he4_cl35__k39)*Y(jcl35)*state % rho - &
      screened_rates(k_he4_cl35__p_ar38)*Y(jcl35)*state % rho - &
      screened_rates(k_he4_cl36__k40)*Y(jcl36)*state % rho - &
      screened_rates(k_he4_cl37__k41)*Y(jcl37)*state % rho - &
      screened_rates(k_he4_co53__p_ni56)*Y(jco53)*state % rho - &
      screened_rates(k_he4_co54__p_ni57)*Y(jco54)*state % rho - &
      screened_rates(k_he4_co55__p_ni58)*Y(jco55)*state % rho - &
      screened_rates(k_he4_co56__p_ni59)*Y(jco56)*state % rho - &
      screened_rates(k_he4_co57__p_ni60)*Y(jco57)*state % rho - &
      screened_rates(k_he4_cr47__fe51)*Y(jcr47)*state % rho - &
      screened_rates(k_he4_cr47__p_mn50)*Y(jcr47)*state % rho - &
      screened_rates(k_he4_cr48__fe52)*Y(jcr48)*state % rho - &
      screened_rates(k_he4_cr48__p_mn51)*Y(jcr48)*state % rho - &
      screened_rates(k_he4_cr49__fe53)*Y(jcr49)*state % rho - &
      screened_rates(k_he4_cr49__p_mn52)*Y(jcr49)*state % rho - &
      screened_rates(k_he4_cr50__fe54)*Y(jcr50)*state % rho - &
      screened_rates(k_he4_cr51__fe55)*Y(jcr51)*state % rho - &
      screened_rates(k_he4_cr52__fe56)*Y(jcr52)*state % rho - &
      screened_rates(k_he4_f18__na22)*Y(jf18)*state % rho - &
      screened_rates(k_he4_f18__p_ne21)*Y(jf18)*state % rho - &
      screened_rates(k_he4_f19__na23)*Y(jf19)*state % rho - &
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*state % rho - &
      screened_rates(k_he4_f20__n_na23)*Y(jf20)*state % rho - &
      screened_rates(k_he4_f20__na24)*Y(jf20)*state % rho - &
      screened_rates(k_he4_f20__p_ne23)*Y(jf20)*state % rho - &
      screened_rates(k_he4_f21__n_na24)*Y(jf21)*state % rho - &
      screened_rates(k_he4_f21__p_ne24)*Y(jf21)*state % rho - &
      screened_rates(k_he4_fe51__ni55)*Y(jfe51)*state % rho - &
      screened_rates(k_he4_fe51__p_co54)*Y(jfe51)*state % rho - &
      screened_rates(k_he4_fe52__ni56)*Y(jfe52)*state % rho - &
      screened_rates(k_he4_fe52__p_co55)*Y(jfe52)*state % rho - &
      screened_rates(k_he4_fe53__ni57)*Y(jfe53)*state % rho - &
      screened_rates(k_he4_fe53__p_co56)*Y(jfe53)*state % rho - &
      screened_rates(k_he4_fe54__ni58)*Y(jfe54)*state % rho - &
      screened_rates(k_he4_fe55__ni59)*Y(jfe55)*state % rho - &
      screened_rates(k_he4_fe56__ni60)*Y(jfe56)*state % rho - 1.5e0_rt* &
      screened_rates(k_he4_he4_he4__c12)*Y(jhe4)**2*state % rho**2 - &
      screened_rates(k_he4_k37__p_ca40)*Y(jk37)*state % rho - &
      screened_rates(k_he4_k38__p_ca41)*Y(jk38)*state % rho - &
      screened_rates(k_he4_k38__sc42)*Y(jk38)*state % rho - &
      screened_rates(k_he4_k39__sc43)*Y(jk39)*state % rho - &
      screened_rates(k_he4_k40__p_ca43)*Y(jk40)*state % rho - &
      screened_rates(k_he4_k40__sc44)*Y(jk40)*state % rho - &
      screened_rates(k_he4_k41__n_sc44)*Y(jk41)*state % rho - &
      screened_rates(k_he4_k41__p_ca44)*Y(jk41)*state % rho - &
      screened_rates(k_he4_k41__sc45)*Y(jk41)*state % rho - &
      screened_rates(k_he4_k42__n_sc45)*Y(jk42)*state % rho - &
      screened_rates(k_he4_k42__sc46)*Y(jk42)*state % rho - &
      screened_rates(k_he4_mg22__p_al25)*Y(jmg22)*state % rho - &
      screened_rates(k_he4_mg23__p_al26)*Y(jmg23)*state % rho - &
      screened_rates(k_he4_mg23__si27)*Y(jmg23)*state % rho - &
      screened_rates(k_he4_mg24__si28)*Y(jmg24)*state % rho - &
      screened_rates(k_he4_mg25__n_si28)*Y(jmg25)*state % rho - &
      screened_rates(k_he4_mg25__si29)*Y(jmg25)*state % rho - &
      screened_rates(k_he4_mg26__n_si29)*Y(jmg26)*state % rho - &
      screened_rates(k_he4_mg26__si30)*Y(jmg26)*state % rho - &
      screened_rates(k_he4_mg27__n_si30)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_mg27__si31)*Y(jmg27)*state % rho - &
      screened_rates(k_he4_mn49__co53)*Y(jmn49)*state % rho - &
      screened_rates(k_he4_mn49__p_fe52)*Y(jmn49)*state % rho - &
      screened_rates(k_he4_mn50__co54)*Y(jmn50)*state % rho - &
      screened_rates(k_he4_mn50__p_fe53)*Y(jmn50)*state % rho - &
      screened_rates(k_he4_mn51__co55)*Y(jmn51)*state % rho - &
      screened_rates(k_he4_mn51__p_fe54)*Y(jmn51)*state % rho - &
      screened_rates(k_he4_mn52__co56)*Y(jmn52)*state % rho - &
      screened_rates(k_he4_mn52__p_fe55)*Y(jmn52)*state % rho - &
      screened_rates(k_he4_mn53__co57)*Y(jmn53)*state % rho - &
      screened_rates(k_he4_mn53__p_fe56)*Y(jmn53)*state % rho - &
      screened_rates(k_he4_mn54__co58)*Y(jmn54)*state % rho - &
      screened_rates(k_he4_n13__p_o16)*Y(jn13)*state % rho - &
      screened_rates(k_he4_n14__f18)*Y(jn14)*state % rho - screened_rates(k_he4_n15__f19)* &
      Y(jn15)*state % rho - screened_rates(k_he4_na20__p_mg23)*Y(jna20)* &
      state % rho - screened_rates(k_he4_na21__al25)*Y(jna21)*state % rho - &
      screened_rates(k_he4_na22__al26)*Y(jna22)*state % rho - &
      screened_rates(k_he4_na22__p_mg25)*Y(jna22)*state % rho - &
      screened_rates(k_he4_na23__al27)*Y(jna23)*state % rho - &
      screened_rates(k_he4_na23__p_mg26)*Y(jna23)*state % rho - &
      screened_rates(k_he4_na24__al28)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__n_al27)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__p_mg27)*Y(jna24)*state % rho - &
      screened_rates(k_he4_ne19__mg23)*Y(jne19)*state % rho - &
      screened_rates(k_he4_ne19__p_na22)*Y(jne19)*state % rho - &
      screened_rates(k_he4_ne20__mg24)*Y(jne20)*state % rho - &
      screened_rates(k_he4_ne21__mg25)*Y(jne21)*state % rho - &
      screened_rates(k_he4_ne21__n_mg24)*Y(jne21)*state % rho - &
      screened_rates(k_he4_ne22__mg26)*Y(jne22)*state % rho - &
      screened_rates(k_he4_ne22__n_mg25)*Y(jne22)*state % rho - &
      screened_rates(k_he4_ne23__mg27)*Y(jne23)*state % rho - &
      screened_rates(k_he4_ne23__n_mg26)*Y(jne23)*state % rho - &
      screened_rates(k_he4_ne24__n_mg27)*Y(jne24)*state % rho - &
      screened_rates(k_he4_o16__ne20)*Y(jo16)*state % rho - &
      screened_rates(k_he4_o17__n_ne20)*Y(jo17)*state % rho - &
      screened_rates(k_he4_o17__ne21)*Y(jo17)*state % rho - &
      screened_rates(k_he4_o18__n_ne21)*Y(jo18)*state % rho - &
      screened_rates(k_he4_o18__ne22)*Y(jo18)*state % rho - &
      screened_rates(k_he4_o19__n_ne22)*Y(jo19)*state % rho - &
      screened_rates(k_he4_o19__ne23)*Y(jo19)*state % rho - &
      screened_rates(k_he4_p29__cl33)*Y(jp29)*state % rho - &
      screened_rates(k_he4_p29__p_s32)*Y(jp29)*state % rho - &
      screened_rates(k_he4_p30__cl34)*Y(jp30)*state % rho - &
      screened_rates(k_he4_p30__p_s33)*Y(jp30)*state % rho - &
      screened_rates(k_he4_p31__cl35)*Y(jp31)*state % rho - &
      screened_rates(k_he4_p31__p_s34)*Y(jp31)*state % rho - &
      screened_rates(k_he4_p32__cl36)*Y(jp32)*state % rho - &
      screened_rates(k_he4_p33__cl37)*Y(jp33)*state % rho - &
      screened_rates(k_he4_s30__p_cl33)*Y(js30)*state % rho - &
      screened_rates(k_he4_s31__ar35)*Y(js31)*state % rho - &
      screened_rates(k_he4_s31__p_cl34)*Y(js31)*state % rho - &
      screened_rates(k_he4_s32__ar36)*Y(js32)*state % rho - &
      screened_rates(k_he4_s33__ar37)*Y(js33)*state % rho - &
      screened_rates(k_he4_s34__ar38)*Y(js34)*state % rho - &
      screened_rates(k_he4_s34__n_ar37)*Y(js34)*state % rho - &
      screened_rates(k_he4_s34__p_cl37)*Y(js34)*state % rho - &
      screened_rates(k_he4_s35__ar39)*Y(js35)*state % rho - &
      screened_rates(k_he4_s35__n_ar38)*Y(js35)*state % rho - &
      screened_rates(k_he4_sc42__p_ti45)*Y(jsc42)*state % rho - &
      screened_rates(k_he4_sc42__v46)*Y(jsc42)*state % rho - &
      screened_rates(k_he4_sc43__p_ti46)*Y(jsc43)*state % rho - &
      screened_rates(k_he4_sc43__v47)*Y(jsc43)*state % rho - &
      screened_rates(k_he4_sc44__p_ti47)*Y(jsc44)*state % rho - &
      screened_rates(k_he4_sc44__v48)*Y(jsc44)*state % rho - &
      screened_rates(k_he4_sc45__p_ti48)*Y(jsc45)*state % rho - &
      screened_rates(k_he4_sc45__v49)*Y(jsc45)*state % rho - &
      screened_rates(k_he4_sc46__n_v49)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_sc46__p_ti49)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_sc46__v50)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_si27__p_p30)*Y(jsi27)*state % rho - &
      screened_rates(k_he4_si27__s31)*Y(jsi27)*state % rho - &
      screened_rates(k_he4_si28__s32)*Y(jsi28)*state % rho - &
      screened_rates(k_he4_si29__s33)*Y(jsi29)*state % rho - &
      screened_rates(k_he4_si30__s34)*Y(jsi30)*state % rho - &
      screened_rates(k_he4_si31__n_s34)*Y(jsi31)*state % rho - &
      screened_rates(k_he4_si31__s35)*Y(jsi31)*state % rho - &
      screened_rates(k_he4_ti43__cr47)*Y(jti43)*state % rho - &
      screened_rates(k_he4_ti43__p_v46)*Y(jti43)*state % rho - &
      screened_rates(k_he4_ti44__cr48)*Y(jti44)*state % rho - &
      screened_rates(k_he4_ti44__p_v47)*Y(jti44)*state % rho - &
      screened_rates(k_he4_ti45__cr49)*Y(jti45)*state % rho - &
      screened_rates(k_he4_ti45__p_v48)*Y(jti45)*state % rho - &
      screened_rates(k_he4_ti46__cr50)*Y(jti46)*state % rho - &
      screened_rates(k_he4_ti47__cr51)*Y(jti47)*state % rho - &
      screened_rates(k_he4_ti48__cr52)*Y(jti48)*state % rho - &
      screened_rates(k_he4_ti49__n_cr52)*Y(jti49)*state % rho - &
      screened_rates(k_he4_v46__mn50)*Y(jv46)*state % rho - &
      screened_rates(k_he4_v46__p_cr49)*Y(jv46)*state % rho - &
      screened_rates(k_he4_v47__mn51)*Y(jv47)*state % rho - &
      screened_rates(k_he4_v47__p_cr50)*Y(jv47)*state % rho - &
      screened_rates(k_he4_v48__mn52)*Y(jv48)*state % rho - &
      screened_rates(k_he4_v48__p_cr51)*Y(jv48)*state % rho - &
      screened_rates(k_he4_v49__mn53)*Y(jv49)*state % rho - &
      screened_rates(k_he4_v49__p_cr52)*Y(jv49)*state % rho - &
      screened_rates(k_he4_v50__mn54)*Y(jv50)*state % rho - &
      screened_rates(k_he4_v51__mn55)*Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jhe4, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_c12_c12__he4_ne20)*Y(jc12)*state % rho + &
      screened_rates(k_c12_ne20__he4_si28)*Y(jne20)*state % rho + &
      screened_rates(k_c12_o16__he4_mg24)*Y(jo16)*state % rho - &
      screened_rates(k_he4_c12__o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jc12, scratch)

    scratch = (&
      -screened_rates(k_he4_c13__n_o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jc13, scratch)

    scratch = (&
      -screened_rates(k_he4_c14__o18)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jc14, scratch)

    scratch = (&
      -screened_rates(k_he4_n13__p_o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jn13, scratch)

    scratch = (&
      -screened_rates(k_he4_n14__f18)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jn14, scratch)

    scratch = (&
      -screened_rates(k_he4_n15__f19)*Y(jhe4)*state % rho + screened_rates(k_p_n15__he4_c12)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jn15, scratch)

    scratch = (&
      screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*state % rho - screened_rates(k_he4_o16__ne20)* &
      Y(jhe4)*state % rho + 1.0e0_rt*screened_rates(k_o16_o16__he4_si28)*Y(jo16)* &
      state % rho &
       )
    call set_jac_entry(jac, jhe4, jo16, scratch)

    scratch = (&
      -screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*state % rho - screened_rates(k_he4_o17__ne21)* &
      Y(jhe4)*state % rho + screened_rates(k_n_o17__he4_c14)*Y(jn)*state % rho + &
      screened_rates(k_p_o17__he4_n14)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jo17, scratch)

    scratch = (&
      -screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*state % rho - screened_rates(k_he4_o18__ne22)* &
      Y(jhe4)*state % rho + screened_rates(k_p_o18__he4_n15)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jo18, scratch)

    scratch = (&
      -screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*state % rho - screened_rates(k_he4_o19__ne23)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jo19, scratch)

    scratch = (&
      -screened_rates(k_he4_f18__na22)*Y(jhe4)*state % rho - screened_rates(k_he4_f18__p_ne21)* &
      Y(jhe4)*state % rho + screened_rates(k_n_f18__he4_n15)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jf18, scratch)

    scratch = (&
      -screened_rates(k_he4_f19__na23)*Y(jhe4)*state % rho - screened_rates(k_he4_f19__p_ne22)* &
      Y(jhe4)*state % rho + screened_rates(k_p_f19__he4_o16)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jf19, scratch)

    scratch = (&
      -screened_rates(k_he4_f20__n_na23)*Y(jhe4)*state % rho - screened_rates(k_he4_f20__na24)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_f20__p_ne23)*Y(jhe4)*state % rho &
      + screened_rates(k_p_f20__he4_o17)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jf20, scratch)

    scratch = (&
      -screened_rates(k_he4_f21__n_na24)*Y(jhe4)*state % rho - screened_rates(k_he4_f21__p_ne24)* &
      Y(jhe4)*state % rho + screened_rates(k_p_f21__he4_o18)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jf21, scratch)

    scratch = (&
      -screened_rates(k_he4_ne19__mg23)*Y(jhe4)*state % rho - screened_rates(k_he4_ne19__p_na22)* &
      Y(jhe4)*state % rho + screened_rates(k_n_ne19__he4_o16)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jne19, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*state % rho - screened_rates(k_he4_ne20__mg24) &
      *Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jne20, scratch)

    scratch = (&
      -screened_rates(k_he4_ne21__mg25)*Y(jhe4)*state % rho - screened_rates(k_he4_ne21__n_mg24)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jne21, scratch)

    scratch = (&
      -screened_rates(k_he4_ne22__mg26)*Y(jhe4)*state % rho - screened_rates(k_he4_ne22__n_mg25)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jne22, scratch)

    scratch = (&
      -screened_rates(k_he4_ne23__mg27)*Y(jhe4)*state % rho - screened_rates(k_he4_ne23__n_mg26)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jne23, scratch)

    scratch = (&
      -screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jne24, scratch)

    scratch = (&
      -screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*state % rho + &
      screened_rates(k_na20__he4_o16__weak__wc12) &
       )
    call set_jac_entry(jac, jhe4, jna20, scratch)

    scratch = (&
      -screened_rates(k_he4_na21__al25)*Y(jhe4)*state % rho + screened_rates(k_n_na21__he4_f18)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jna21, scratch)

    scratch = (&
      -screened_rates(k_he4_na22__al26)*Y(jhe4)*state % rho - screened_rates(k_he4_na22__p_mg25)* &
      Y(jhe4)*state % rho + screened_rates(k_n_na22__he4_f19)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jna22, scratch)

    scratch = (&
      -screened_rates(k_he4_na23__al27)*Y(jhe4)*state % rho - screened_rates(k_he4_na23__p_mg26)* &
      Y(jhe4)*state % rho + screened_rates(k_p_na23__he4_ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jna23, scratch)

    scratch = (&
      -screened_rates(k_he4_na24__al28)*Y(jhe4)*state % rho - screened_rates(k_he4_na24__n_al27)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*state % rho &
      + screened_rates(k_p_na24__he4_ne21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jna24, scratch)

    scratch = (&
      -screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*state % rho + &
      screened_rates(k_n_mg22__he4_ne19)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmg22, scratch)

    scratch = (&
      -screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*state % rho - screened_rates(k_he4_mg23__si27)* &
      Y(jhe4)*state % rho + screened_rates(k_n_mg23__he4_ne20)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmg23, scratch)

    scratch = (&
      -screened_rates(k_he4_mg24__si28)*Y(jhe4)*state % rho + screened_rates(k_p_mg24__he4_na21)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmg24, scratch)

    scratch = (&
      -screened_rates(k_he4_mg25__n_si28)*Y(jhe4)*state % rho - screened_rates(k_he4_mg25__si29)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmg25, scratch)

    scratch = (&
      -screened_rates(k_he4_mg26__n_si29)*Y(jhe4)*state % rho - screened_rates(k_he4_mg26__si30)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmg26, scratch)

    scratch = (&
      -screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*state % rho - screened_rates(k_he4_mg27__si31)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmg27, scratch)

    scratch = (&
      -screened_rates(k_he4_al25__p29)*Y(jhe4)*state % rho - screened_rates(k_he4_al25__p_si28)* &
      Y(jhe4)*state % rho + screened_rates(k_n_al25__he4_na22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jal25, scratch)

    scratch = (&
      -screened_rates(k_he4_al26__p30)*Y(jhe4)*state % rho - screened_rates(k_he4_al26__p_si29)* &
      Y(jhe4)*state % rho + screened_rates(k_n_al26__he4_na23)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jal26, scratch)

    scratch = (&
      -screened_rates(k_he4_al27__n_p30)*Y(jhe4)*state % rho - screened_rates(k_he4_al27__p31)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_al27__p_si30)*Y(jhe4)*state % rho &
      + screened_rates(k_p_al27__he4_mg24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jal27, scratch)

    scratch = (&
      -screened_rates(k_he4_al28__n_p31)*Y(jhe4)*state % rho - screened_rates(k_he4_al28__p32)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_al28__p_si31)*Y(jhe4)*state % rho &
      + screened_rates(k_p_al28__he4_mg25)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jal28, scratch)

    scratch = (&
      -screened_rates(k_he4_al29__n_p32)*Y(jhe4)*state % rho - screened_rates(k_he4_al29__p33)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_al29__p_si32)*Y(jhe4)*state % rho &
      + screened_rates(k_p_al29__he4_mg26)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jal29, scratch)

    scratch = (&
      -screened_rates(k_he4_si27__p_p30)*Y(jhe4)*state % rho - screened_rates(k_he4_si27__s31)* &
      Y(jhe4)*state % rho + screened_rates(k_n_si27__he4_mg24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsi27, scratch)

    scratch = (&
      -screened_rates(k_he4_si28__s32)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsi28, scratch)

    scratch = (&
      -screened_rates(k_he4_si29__s33)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsi29, scratch)

    scratch = (&
      -screened_rates(k_he4_si30__s34)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsi30, scratch)

    scratch = (&
      -screened_rates(k_he4_si31__n_s34)*Y(jhe4)*state % rho - screened_rates(k_he4_si31__s35)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsi31, scratch)

    scratch = (&
      -screened_rates(k_he4_p29__cl33)*Y(jhe4)*state % rho - screened_rates(k_he4_p29__p_s32)* &
      Y(jhe4)*state % rho + screened_rates(k_n_p29__he4_al26)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jp29, scratch)

    scratch = (&
      -screened_rates(k_he4_p30__cl34)*Y(jhe4)*state % rho - screened_rates(k_he4_p30__p_s33)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jp30, scratch)

    scratch = (&
      -screened_rates(k_he4_p31__cl35)*Y(jhe4)*state % rho - screened_rates(k_he4_p31__p_s34)* &
      Y(jhe4)*state % rho + screened_rates(k_p_p31__he4_si28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jp31, scratch)

    scratch = (&
      -screened_rates(k_he4_p32__cl36)*Y(jhe4)*state % rho + screened_rates(k_p_p32__he4_si29)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jp32, scratch)

    scratch = (&
      -screened_rates(k_he4_p33__cl37)*Y(jhe4)*state % rho + screened_rates(k_p_p33__he4_si30)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jp33, scratch)

    scratch = (&
      -screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*state % rho + screened_rates(k_n_s30__he4_si27)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, js30, scratch)

    scratch = (&
      -screened_rates(k_he4_s31__ar35)*Y(jhe4)*state % rho - screened_rates(k_he4_s31__p_cl34)* &
      Y(jhe4)*state % rho + screened_rates(k_n_s31__he4_si28)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, js31, scratch)

    scratch = (&
      -screened_rates(k_he4_s32__ar36)*Y(jhe4)*state % rho + screened_rates(k_n_s32__he4_si29)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, js32, scratch)

    scratch = (&
      -screened_rates(k_he4_s33__ar37)*Y(jhe4)*state % rho + screened_rates(k_n_s33__he4_si30)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, js33, scratch)

    scratch = (&
      -screened_rates(k_he4_s34__ar38)*Y(jhe4)*state % rho - screened_rates(k_he4_s34__n_ar37)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, js34, scratch)

    scratch = (&
      -screened_rates(k_he4_s35__ar39)*Y(jhe4)*state % rho - screened_rates(k_he4_s35__n_ar38)* &
      Y(jhe4)*state % rho + screened_rates(k_n_s35__he4_si32)*Y(jn)*state % rho + &
      screened_rates(k_p_s35__he4_p32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, js35, scratch)

    scratch = (&
      -screened_rates(k_he4_cl33__k37)*Y(jhe4)*state % rho - screened_rates(k_he4_cl33__p_ar36)* &
      Y(jhe4)*state % rho + screened_rates(k_n_cl33__he4_p30)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcl33, scratch)

    scratch = (&
      -screened_rates(k_he4_cl34__k38)*Y(jhe4)*state % rho - screened_rates(k_he4_cl34__p_ar37)* &
      Y(jhe4)*state % rho + screened_rates(k_n_cl34__he4_p31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcl34, scratch)

    scratch = (&
      -screened_rates(k_he4_cl35__k39)*Y(jhe4)*state % rho - screened_rates(k_he4_cl35__p_ar38)* &
      Y(jhe4)*state % rho + screened_rates(k_n_cl35__he4_p32)*Y(jn)*state % rho + &
      screened_rates(k_p_cl35__he4_s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcl35, scratch)

    scratch = (&
      -screened_rates(k_he4_cl36__k40)*Y(jhe4)*state % rho + screened_rates(k_n_cl36__he4_p33)* &
      Y(jn)*state % rho + screened_rates(k_p_cl36__he4_s33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcl36, scratch)

    scratch = (&
      -screened_rates(k_he4_cl37__k41)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcl37, scratch)

    scratch = (&
      -screened_rates(k_he4_ar35__ca39)*Y(jhe4)*state % rho - screened_rates(k_he4_ar35__p_k38)* &
      Y(jhe4)*state % rho + screened_rates(k_n_ar35__he4_s32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jar35, scratch)

    scratch = (&
      -screened_rates(k_he4_ar36__ca40)*Y(jhe4)*state % rho + screened_rates(k_n_ar36__he4_s33)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jar36, scratch)

    scratch = (&
      -screened_rates(k_he4_ar37__ca41)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jar37, scratch)

    scratch = (&
      -screened_rates(k_he4_ar38__ca42)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jar38, scratch)

    scratch = (&
      -screened_rates(k_he4_ar39__ca43)*Y(jhe4)*state % rho + screened_rates(k_p_ar39__he4_cl36)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jar39, scratch)

    scratch = (&
      -screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*state % rho + screened_rates(k_n_k37__he4_cl34)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jk37, scratch)

    scratch = (&
      -screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*state % rho - screened_rates(k_he4_k38__sc42)* &
      Y(jhe4)*state % rho + screened_rates(k_n_k38__he4_cl35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jk38, scratch)

    scratch = (&
      -screened_rates(k_he4_k39__sc43)*Y(jhe4)*state % rho + screened_rates(k_n_k39__he4_cl36)* &
      Y(jn)*state % rho + screened_rates(k_p_k39__he4_ar36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jk39, scratch)

    scratch = (&
      -screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*state % rho - screened_rates(k_he4_k40__sc44)* &
      Y(jhe4)*state % rho + screened_rates(k_n_k40__he4_cl37)*Y(jn)*state % rho + &
      screened_rates(k_p_k40__he4_ar37)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jk40, scratch)

    scratch = (&
      -screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*state % rho - screened_rates(k_he4_k41__p_ca44)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_k41__sc45)*Y(jhe4)*state % rho + &
      screened_rates(k_p_k41__he4_ar38)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jk41, scratch)

    scratch = (&
      -screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*state % rho - screened_rates(k_he4_k42__sc46)* &
      Y(jhe4)*state % rho + screened_rates(k_p_k42__he4_ar39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jk42, scratch)

    scratch = (&
      -screened_rates(k_he4_ca39__ti43)*Y(jhe4)*state % rho + screened_rates(k_n_ca39__he4_ar36)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jca39, scratch)

    scratch = (&
      -screened_rates(k_he4_ca40__ti44)*Y(jhe4)*state % rho + screened_rates(k_n_ca40__he4_ar37)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jca40, scratch)

    scratch = (&
      -screened_rates(k_he4_ca41__ti45)*Y(jhe4)*state % rho + screened_rates(k_n_ca41__he4_ar38)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jca41, scratch)

    scratch = (&
      -screened_rates(k_he4_ca42__ti46)*Y(jhe4)*state % rho + screened_rates(k_n_ca42__he4_ar39)* &
      Y(jn)*state % rho + screened_rates(k_p_ca42__he4_k39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jca42, scratch)

    scratch = (&
      -screened_rates(k_he4_ca43__n_ti46)*Y(jhe4)*state % rho - screened_rates(k_he4_ca43__ti47)* &
      Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jca43, scratch)

    scratch = (&
      -screened_rates(k_he4_ca44__ti48)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jca44, scratch)

    scratch = (&
      -screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*state % rho - screened_rates(k_he4_sc42__v46)* &
      Y(jhe4)*state % rho + screened_rates(k_n_sc42__he4_k39)*Y(jn)*state % rho + &
      screened_rates(k_p_sc42__he4_ca39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsc42, scratch)

    scratch = (&
      -screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*state % rho - screened_rates(k_he4_sc43__v47)* &
      Y(jhe4)*state % rho + screened_rates(k_n_sc43__he4_k40)*Y(jn)*state % rho + &
      screened_rates(k_p_sc43__he4_ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsc43, scratch)

    scratch = (&
      -screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*state % rho - screened_rates(k_he4_sc44__v48)* &
      Y(jhe4)*state % rho + screened_rates(k_p_sc44__he4_ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsc44, scratch)

    scratch = (&
      -screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*state % rho - screened_rates(k_he4_sc45__v49)* &
      Y(jhe4)*state % rho + screened_rates(k_p_sc45__he4_ca42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsc45, scratch)

    scratch = (&
      -screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*state % rho - screened_rates(k_he4_sc46__p_ti49) &
      *Y(jhe4)*state % rho - screened_rates(k_he4_sc46__v50)*Y(jhe4)*state % rho + &
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jsc46, scratch)

    scratch = (&
      -screened_rates(k_he4_ti43__cr47)*Y(jhe4)*state % rho - screened_rates(k_he4_ti43__p_v46)* &
      Y(jhe4)*state % rho + screened_rates(k_n_ti43__he4_ca40)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti43, scratch)

    scratch = (&
      -screened_rates(k_he4_ti44__cr48)*Y(jhe4)*state % rho - screened_rates(k_he4_ti44__p_v47)* &
      Y(jhe4)*state % rho + screened_rates(k_n_ti44__he4_ca41)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti44, scratch)

    scratch = (&
      -screened_rates(k_he4_ti45__cr49)*Y(jhe4)*state % rho - screened_rates(k_he4_ti45__p_v48)* &
      Y(jhe4)*state % rho + screened_rates(k_n_ti45__he4_ca42)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti45, scratch)

    scratch = (&
      -screened_rates(k_he4_ti46__cr50)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti46, scratch)

    scratch = (&
      -screened_rates(k_he4_ti47__cr51)*Y(jhe4)*state % rho + screened_rates(k_n_ti47__he4_ca44)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti47, scratch)

    scratch = (&
      -screened_rates(k_he4_ti48__cr52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti48, scratch)

    scratch = (&
      -screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jti49, scratch)

    scratch = (&
      -screened_rates(k_he4_v46__mn50)*Y(jhe4)*state % rho - screened_rates(k_he4_v46__p_cr49)* &
      Y(jhe4)*state % rho + screened_rates(k_n_v46__he4_sc43)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jv46, scratch)

    scratch = (&
      -screened_rates(k_he4_v47__mn51)*Y(jhe4)*state % rho - screened_rates(k_he4_v47__p_cr50)* &
      Y(jhe4)*state % rho + screened_rates(k_n_v47__he4_sc44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jv47, scratch)

    scratch = (&
      -screened_rates(k_he4_v48__mn52)*Y(jhe4)*state % rho - screened_rates(k_he4_v48__p_cr51)* &
      Y(jhe4)*state % rho + screened_rates(k_n_v48__he4_sc45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jv48, scratch)

    scratch = (&
      -screened_rates(k_he4_v49__mn53)*Y(jhe4)*state % rho - screened_rates(k_he4_v49__p_cr52)* &
      Y(jhe4)*state % rho + screened_rates(k_p_v49__he4_ti46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jv49, scratch)

    scratch = (&
      -screened_rates(k_he4_v50__mn54)*Y(jhe4)*state % rho + screened_rates(k_p_v50__he4_ti47)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jv50, scratch)

    scratch = (&
      -screened_rates(k_he4_v51__mn55)*Y(jhe4)*state % rho + screened_rates(k_p_v51__he4_ti48)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jv51, scratch)

    scratch = (&
      -screened_rates(k_he4_cr47__fe51)*Y(jhe4)*state % rho - screened_rates(k_he4_cr47__p_mn50)* &
      Y(jhe4)*state % rho + screened_rates(k_n_cr47__he4_ti44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcr47, scratch)

    scratch = (&
      -screened_rates(k_he4_cr48__fe52)*Y(jhe4)*state % rho - screened_rates(k_he4_cr48__p_mn51)* &
      Y(jhe4)*state % rho + screened_rates(k_n_cr48__he4_ti45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcr48, scratch)

    scratch = (&
      -screened_rates(k_he4_cr49__fe53)*Y(jhe4)*state % rho - screened_rates(k_he4_cr49__p_mn52)* &
      Y(jhe4)*state % rho + screened_rates(k_n_cr49__he4_ti46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcr49, scratch)

    scratch = (&
      -screened_rates(k_he4_cr50__fe54)*Y(jhe4)*state % rho + screened_rates(k_n_cr50__he4_ti47)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcr50, scratch)

    scratch = (&
      -screened_rates(k_he4_cr51__fe55)*Y(jhe4)*state % rho + screened_rates(k_n_cr51__he4_ti48)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcr51, scratch)

    scratch = (&
      -screened_rates(k_he4_cr52__fe56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jcr52, scratch)

    scratch = (&
      -screened_rates(k_he4_mn49__co53)*Y(jhe4)*state % rho - screened_rates(k_he4_mn49__p_fe52)* &
      Y(jhe4)*state % rho + screened_rates(k_n_mn49__he4_v46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn49, scratch)

    scratch = (&
      -screened_rates(k_he4_mn50__co54)*Y(jhe4)*state % rho - screened_rates(k_he4_mn50__p_fe53)* &
      Y(jhe4)*state % rho + screened_rates(k_n_mn50__he4_v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn50, scratch)

    scratch = (&
      -screened_rates(k_he4_mn51__co55)*Y(jhe4)*state % rho - screened_rates(k_he4_mn51__p_fe54)* &
      Y(jhe4)*state % rho + screened_rates(k_n_mn51__he4_v48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn51, scratch)

    scratch = (&
      -screened_rates(k_he4_mn52__co56)*Y(jhe4)*state % rho - screened_rates(k_he4_mn52__p_fe55)* &
      Y(jhe4)*state % rho + screened_rates(k_n_mn52__he4_v49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn52, scratch)

    scratch = (&
      -screened_rates(k_he4_mn53__co57)*Y(jhe4)*state % rho - screened_rates(k_he4_mn53__p_fe56)* &
      Y(jhe4)*state % rho + screened_rates(k_n_mn53__he4_v50)*Y(jn)*state % rho + &
      screened_rates(k_p_mn53__he4_cr50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn53, scratch)

    scratch = (&
      -screened_rates(k_he4_mn54__co58)*Y(jhe4)*state % rho + screened_rates(k_n_mn54__he4_v51)* &
      Y(jn)*state % rho + screened_rates(k_p_mn54__he4_cr51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn54, scratch)

    scratch = (&
      screened_rates(k_p_mn55__he4_cr52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jmn55, scratch)

    scratch = (&
      -screened_rates(k_he4_fe51__ni55)*Y(jhe4)*state % rho - screened_rates(k_he4_fe51__p_co54)* &
      Y(jhe4)*state % rho + screened_rates(k_n_fe51__he4_cr48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jfe51, scratch)

    scratch = (&
      -screened_rates(k_he4_fe52__ni56)*Y(jhe4)*state % rho - screened_rates(k_he4_fe52__p_co55)* &
      Y(jhe4)*state % rho + screened_rates(k_n_fe52__he4_cr49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jfe52, scratch)

    scratch = (&
      -screened_rates(k_he4_fe53__ni57)*Y(jhe4)*state % rho - screened_rates(k_he4_fe53__p_co56)* &
      Y(jhe4)*state % rho + screened_rates(k_n_fe53__he4_cr50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jfe53, scratch)

    scratch = (&
      -screened_rates(k_he4_fe54__ni58)*Y(jhe4)*state % rho + screened_rates(k_n_fe54__he4_cr51)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jfe54, scratch)

    scratch = (&
      -screened_rates(k_he4_fe55__ni59)*Y(jhe4)*state % rho + screened_rates(k_n_fe55__he4_cr52)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jfe55, scratch)

    scratch = (&
      -screened_rates(k_he4_fe56__ni60)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jfe56, scratch)

    scratch = (&
      -screened_rates(k_he4_co53__p_ni56)*Y(jhe4)*state % rho + &
      screened_rates(k_n_co53__he4_mn50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jco53, scratch)

    scratch = (&
      -screened_rates(k_he4_co54__p_ni57)*Y(jhe4)*state % rho + &
      screened_rates(k_n_co54__he4_mn51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jco54, scratch)

    scratch = (&
      -screened_rates(k_he4_co55__p_ni58)*Y(jhe4)*state % rho + &
      screened_rates(k_n_co55__he4_mn52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jco55, scratch)

    scratch = (&
      -screened_rates(k_he4_co56__p_ni59)*Y(jhe4)*state % rho + &
      screened_rates(k_n_co56__he4_mn53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jco56, scratch)

    scratch = (&
      -screened_rates(k_he4_co57__p_ni60)*Y(jhe4)*state % rho + &
      screened_rates(k_n_co57__he4_mn54)*Y(jn)*state % rho + &
      screened_rates(k_p_co57__he4_fe54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jco57, scratch)

    scratch = (&
      screened_rates(k_n_co58__he4_mn55)*Y(jn)*state % rho + screened_rates(k_p_co58__he4_fe55)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jco58, scratch)

    scratch = (&
      screened_rates(k_n_ni54__he4_fe51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jni54, scratch)

    scratch = (&
      screened_rates(k_n_ni55__he4_fe52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jni55, scratch)

    scratch = (&
      screened_rates(k_n_ni56__he4_fe53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jni56, scratch)

    scratch = (&
      screened_rates(k_n_ni57__he4_fe54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jni57, scratch)

    scratch = (&
      screened_rates(k_n_ni58__he4_fe55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jni58, scratch)

    scratch = (&
      screened_rates(k_n_ni59__he4_fe56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jhe4, jni59, scratch)

    scratch = (&
      -screened_rates(k_n_c12__c13)*Y(jc12)*state % rho + 2.0e0_rt*screened_rates(k_n_mg23__c12_c12) &
      *Y(jmg23)*state % rho + screened_rates(k_n_si27__c12_o16)*Y(jsi27)* &
      state % rho &
       )
    call set_jac_entry(jac, jc12, jn, scratch)

    scratch = (&
      -screened_rates(k_p_c12__n13)*Y(jc12)*state % rho + screened_rates(k_p_n15__he4_c12)* &
      Y(jn15)*state % rho &
       )
    call set_jac_entry(jac, jc12, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_c12__o16)*Y(jc12)*state % rho + 0.5e0_rt* &
      screened_rates(k_he4_he4_he4__c12)*Y(jhe4)**2*state % rho**2 &
       )
    call set_jac_entry(jac, jc12, jhe4, scratch)

    scratch = (&
      -2.0e0_rt*screened_rates(k_c12_c12__he4_ne20)*Y(jc12)*state % rho - 2.0e0_rt* &
      screened_rates(k_c12_c12__p_na23)*Y(jc12)*state % rho - &
      screened_rates(k_c12_ne20__he4_si28)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__n_s31)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__p_p31)*Y(jne20)*state % rho - &
      screened_rates(k_c12_o16__he4_mg24)*Y(jo16)*state % rho - &
      screened_rates(k_c12_o16__p_al27)*Y(jo16)*state % rho - &
      screened_rates(k_he4_c12__o16)*Y(jhe4)*state % rho - screened_rates(k_n_c12__c13)* &
      Y(jn)*state % rho - screened_rates(k_p_c12__n13)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jc12, jc12, scratch)

    scratch = (&
      screened_rates(k_p_n15__he4_c12)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jc12, jn15, scratch)

    scratch = (&
      -screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*state % rho - &
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jc12, jo16, scratch)

    scratch = (&
      -screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*state % rho - &
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*state % rho - &
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jc12, jne20, scratch)

    scratch = (&
      2.0e0_rt*screened_rates(k_n_mg23__c12_c12)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jc12, jmg23, scratch)

    scratch = (&
      screened_rates(k_n_si27__c12_o16)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jc12, jsi27, scratch)

    scratch = (&
      screened_rates(k_n_c12__c13)*Y(jc12)*state % rho - screened_rates(k_n_c13__c14)*Y(jc13)* &
      state % rho &
       )
    call set_jac_entry(jac, jc13, jn, scratch)

    scratch = (&
      -screened_rates(k_p_c13__n14)*Y(jc13)*state % rho - screened_rates(k_p_c13__n_n13)* &
      Y(jc13)*state % rho &
       )
    call set_jac_entry(jac, jc13, jp, scratch)

    scratch = (&
      -screened_rates(k_d_c13__n_n14)*Y(jc13)*state % rho &
       )
    call set_jac_entry(jac, jc13, jd, scratch)

    scratch = (&
      -screened_rates(k_he4_c13__n_o16)*Y(jc13)*state % rho &
       )
    call set_jac_entry(jac, jc13, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_c12__c13)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jc13, jc12, scratch)

    scratch = (&
      -screened_rates(k_d_c13__n_n14)*Y(jd)*state % rho - screened_rates(k_he4_c13__n_o16)* &
      Y(jhe4)*state % rho - screened_rates(k_n_c13__c14)*Y(jn)*state % rho - &
      screened_rates(k_p_c13__n14)*Y(jp)*state % rho - screened_rates(k_p_c13__n_n13)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jc13, jc13, scratch)

    scratch = (&
      screened_rates(k_n13__c13__weak__wc12) &
       )
    call set_jac_entry(jac, jc13, jn13, scratch)

    scratch = (&
      screened_rates(k_n_c13__c14)*Y(jc13)*state % rho + screened_rates(k_n_n14__p_c14)*Y(jn14) &
      *state % rho + screened_rates(k_n_o17__he4_c14)*Y(jo17)*state % rho &
       )
    call set_jac_entry(jac, jc14, jn, scratch)

    scratch = (&
      -screened_rates(k_p_c14__n15)*Y(jc14)*state % rho &
       )
    call set_jac_entry(jac, jc14, jp, scratch)

    scratch = (&
      -screened_rates(k_d_c14__n_n15)*Y(jc14)*state % rho &
       )
    call set_jac_entry(jac, jc14, jd, scratch)

    scratch = (&
      -screened_rates(k_he4_c14__o18)*Y(jc14)*state % rho &
       )
    call set_jac_entry(jac, jc14, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_c13__c14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jc14, jc13, scratch)

    scratch = (&
      -screened_rates(k_c14__n14__weak__wc12) - screened_rates(k_d_c14__n_n15)*Y(jd)*state % rho &
      - screened_rates(k_he4_c14__o18)*Y(jhe4)*state % rho - screened_rates(k_p_c14__n15)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jc14, jc14, scratch)

    scratch = (&
      screened_rates(k_n_n14__p_c14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jc14, jn14, scratch)

    scratch = (&
      screened_rates(k_n_o17__he4_c14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jc14, jo17, scratch)

    scratch = (&
      -screened_rates(k_n_n13__n14)*Y(jn13)*state % rho &
       )
    call set_jac_entry(jac, jn13, jn, scratch)

    scratch = (&
      screened_rates(k_p_c12__n13)*Y(jc12)*state % rho + screened_rates(k_p_c13__n_n13)*Y(jc13) &
      *state % rho &
       )
    call set_jac_entry(jac, jn13, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_n13__p_o16)*Y(jn13)*state % rho &
       )
    call set_jac_entry(jac, jn13, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_c12__n13)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn13, jc12, scratch)

    scratch = (&
      screened_rates(k_p_c13__n_n13)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn13, jc13, scratch)

    scratch = (&
      -screened_rates(k_he4_n13__p_o16)*Y(jhe4)*state % rho - &
      screened_rates(k_n13__c13__weak__wc12) - screened_rates(k_n_n13__n14)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jn13, jn13, scratch)

    scratch = (&
      screened_rates(k_n_n13__n14)*Y(jn13)*state % rho - screened_rates(k_n_n14__n15)*Y(jn14)* &
      state % rho - screened_rates(k_n_n14__p_c14)*Y(jn14)*state % rho &
       )
    call set_jac_entry(jac, jn14, jn, scratch)

    scratch = (&
      screened_rates(k_p_c13__n14)*Y(jc13)*state % rho + screened_rates(k_p_o17__he4_n14)* &
      Y(jo17)*state % rho &
       )
    call set_jac_entry(jac, jn14, jp, scratch)

    scratch = (&
      screened_rates(k_d_c13__n_n14)*Y(jc13)*state % rho &
       )
    call set_jac_entry(jac, jn14, jd, scratch)

    scratch = (&
      -screened_rates(k_he4_n14__f18)*Y(jn14)*state % rho &
       )
    call set_jac_entry(jac, jn14, jhe4, scratch)

    scratch = (&
      screened_rates(k_d_c13__n_n14)*Y(jd)*state % rho + screened_rates(k_p_c13__n14)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jn14, jc13, scratch)

    scratch = (&
      screened_rates(k_c14__n14__weak__wc12) &
       )
    call set_jac_entry(jac, jn14, jc14, scratch)

    scratch = (&
      screened_rates(k_n_n13__n14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn14, jn13, scratch)

    scratch = (&
      -screened_rates(k_he4_n14__f18)*Y(jhe4)*state % rho - screened_rates(k_n_n14__n15)*Y(jn)* &
      state % rho - screened_rates(k_n_n14__p_c14)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn14, jn14, scratch)

    scratch = (&
      screened_rates(k_p_o17__he4_n14)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn14, jo17, scratch)

    scratch = (&
      screened_rates(k_n_f18__he4_n15)*Y(jf18)*state % rho + screened_rates(k_n_n14__n15)* &
      Y(jn14)*state % rho &
       )
    call set_jac_entry(jac, jn15, jn, scratch)

    scratch = (&
      screened_rates(k_p_c14__n15)*Y(jc14)*state % rho - screened_rates(k_p_n15__he4_c12)* &
      Y(jn15)*state % rho - screened_rates(k_p_n15__o16)*Y(jn15)*state % rho + &
      screened_rates(k_p_o18__he4_n15)*Y(jo18)*state % rho &
       )
    call set_jac_entry(jac, jn15, jp, scratch)

    scratch = (&
      screened_rates(k_d_c14__n_n15)*Y(jc14)*state % rho &
       )
    call set_jac_entry(jac, jn15, jd, scratch)

    scratch = (&
      -screened_rates(k_he4_n15__f19)*Y(jn15)*state % rho &
       )
    call set_jac_entry(jac, jn15, jhe4, scratch)

    scratch = (&
      screened_rates(k_d_c14__n_n15)*Y(jd)*state % rho + screened_rates(k_p_c14__n15)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jn15, jc14, scratch)

    scratch = (&
      screened_rates(k_n_n14__n15)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn15, jn14, scratch)

    scratch = (&
      -screened_rates(k_he4_n15__f19)*Y(jhe4)*state % rho - screened_rates(k_p_n15__he4_c12)* &
      Y(jp)*state % rho - screened_rates(k_p_n15__o16)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn15, jn15, scratch)

    scratch = (&
      screened_rates(k_p_o18__he4_n15)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jn15, jo18, scratch)

    scratch = (&
      screened_rates(k_n_f18__he4_n15)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jn15, jf18, scratch)

    scratch = (&
      screened_rates(k_n_ne19__he4_o16)*Y(jne19)*state % rho - screened_rates(k_n_o16__o17)* &
      Y(jo16)*state % rho + screened_rates(k_n_si27__c12_o16)*Y(jsi27)*state % rho &
       )
    call set_jac_entry(jac, jo16, jn, scratch)

    scratch = (&
      screened_rates(k_p_f19__he4_o16)*Y(jf19)*state % rho + screened_rates(k_p_n15__o16)* &
      Y(jn15)*state % rho &
       )
    call set_jac_entry(jac, jo16, jp, scratch)

    scratch = (&
      screened_rates(k_he4_c12__o16)*Y(jc12)*state % rho + screened_rates(k_he4_c13__n_o16)* &
      Y(jc13)*state % rho + screened_rates(k_he4_n13__p_o16)*Y(jn13)*state % rho - &
      screened_rates(k_he4_o16__ne20)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jo16, jhe4, scratch)

    scratch = (&
      -screened_rates(k_c12_o16__he4_mg24)*Y(jo16)*state % rho - &
      screened_rates(k_c12_o16__p_al27)*Y(jo16)*state % rho + &
      screened_rates(k_he4_c12__o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jo16, jc12, scratch)

    scratch = (&
      screened_rates(k_he4_c13__n_o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jo16, jc13, scratch)

    scratch = (&
      screened_rates(k_he4_n13__p_o16)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jo16, jn13, scratch)

    scratch = (&
      screened_rates(k_p_n15__o16)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo16, jn15, scratch)

    scratch = (&
      -screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*state % rho - &
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*state % rho - &
      screened_rates(k_he4_o16__ne20)*Y(jhe4)*state % rho - screened_rates(k_n_o16__o17)* &
      Y(jn)*state % rho - 2.0e0_rt*screened_rates(k_o16_o16__he4_si28)*Y(jo16)* &
      state % rho - 2.0e0_rt*screened_rates(k_o16_o16__n_s31)*Y(jo16)*state % rho - &
      2.0e0_rt*screened_rates(k_o16_o16__p_p31)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jo16, jo16, scratch)

    scratch = (&
      screened_rates(k_p_f19__he4_o16)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo16, jf19, scratch)

    scratch = (&
      screened_rates(k_n_ne19__he4_o16)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jo16, jne19, scratch)

    scratch = (&
      screened_rates(k_na20__he4_o16__weak__wc12) &
       )
    call set_jac_entry(jac, jo16, jna20, scratch)

    scratch = (&
      screened_rates(k_n_si27__c12_o16)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jo16, jsi27, scratch)

    scratch = (&
      screened_rates(k_n_o16__o17)*Y(jo16)*state % rho - screened_rates(k_n_o17__he4_c14)* &
      Y(jo17)*state % rho - screened_rates(k_n_o17__o18)*Y(jo17)*state % rho &
       )
    call set_jac_entry(jac, jo17, jn, scratch)

    scratch = (&
      screened_rates(k_p_f20__he4_o17)*Y(jf20)*state % rho - screened_rates(k_p_o17__f18)* &
      Y(jo17)*state % rho - screened_rates(k_p_o17__he4_n14)*Y(jo17)*state % rho &
       )
    call set_jac_entry(jac, jo17, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_o17__n_ne20)*Y(jo17)*state % rho - screened_rates(k_he4_o17__ne21)* &
      Y(jo17)*state % rho &
       )
    call set_jac_entry(jac, jo17, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_o16__o17)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jo17, jo16, scratch)

    scratch = (&
      -screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*state % rho - screened_rates(k_he4_o17__ne21)* &
      Y(jhe4)*state % rho - screened_rates(k_n_o17__he4_c14)*Y(jn)*state % rho - &
      screened_rates(k_n_o17__o18)*Y(jn)*state % rho - screened_rates(k_p_o17__f18)* &
      Y(jp)*state % rho - screened_rates(k_p_o17__he4_n14)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo17, jo17, scratch)

    scratch = (&
      screened_rates(k_p_f20__he4_o17)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo17, jf20, scratch)

    scratch = (&
      screened_rates(k_n_f18__p_o18)*Y(jf18)*state % rho + screened_rates(k_n_o17__o18)*Y(jo17) &
      *state % rho - screened_rates(k_n_o18__o19)*Y(jo18)*state % rho &
       )
    call set_jac_entry(jac, jo18, jn, scratch)

    scratch = (&
      screened_rates(k_p_f21__he4_o18)*Y(jf21)*state % rho - screened_rates(k_p_o18__f19)* &
      Y(jo18)*state % rho - screened_rates(k_p_o18__he4_n15)*Y(jo18)*state % rho &
       )
    call set_jac_entry(jac, jo18, jp, scratch)

    scratch = (&
      screened_rates(k_he4_c14__o18)*Y(jc14)*state % rho - screened_rates(k_he4_o18__n_ne21)* &
      Y(jo18)*state % rho - screened_rates(k_he4_o18__ne22)*Y(jo18)*state % rho &
       )
    call set_jac_entry(jac, jo18, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_c14__o18)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jo18, jc14, scratch)

    scratch = (&
      screened_rates(k_n_o17__o18)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jo18, jo17, scratch)

    scratch = (&
      -screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*state % rho - screened_rates(k_he4_o18__ne22)* &
      Y(jhe4)*state % rho - screened_rates(k_n_o18__o19)*Y(jn)*state % rho - &
      screened_rates(k_p_o18__f19)*Y(jp)*state % rho - screened_rates(k_p_o18__he4_n15)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo18, jo18, scratch)

    scratch = (&
      screened_rates(k_f18__o18__weak__wc12) + screened_rates(k_n_f18__p_o18)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jo18, jf18, scratch)

    scratch = (&
      screened_rates(k_p_f21__he4_o18)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo18, jf21, scratch)

    scratch = (&
      screened_rates(k_n_o18__o19)*Y(jo18)*state % rho &
       )
    call set_jac_entry(jac, jo19, jn, scratch)

    scratch = (&
      -screened_rates(k_p_o19__f20)*Y(jo19)*state % rho - screened_rates(k_p_o19__n_f19)* &
      Y(jo19)*state % rho &
       )
    call set_jac_entry(jac, jo19, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_o19__n_ne22)*Y(jo19)*state % rho - screened_rates(k_he4_o19__ne23)* &
      Y(jo19)*state % rho &
       )
    call set_jac_entry(jac, jo19, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_o18__o19)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jo19, jo18, scratch)

    scratch = (&
      -screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*state % rho - screened_rates(k_he4_o19__ne23)* &
      Y(jhe4)*state % rho - screened_rates(k_o19__f19__weak__wc12) - &
      screened_rates(k_p_o19__f20)*Y(jp)*state % rho - screened_rates(k_p_o19__n_f19)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jo19, jo19, scratch)

    scratch = (&
      -screened_rates(k_n_f18__f19)*Y(jf18)*state % rho - screened_rates(k_n_f18__he4_n15)* &
      Y(jf18)*state % rho - screened_rates(k_n_f18__p_o18)*Y(jf18)*state % rho + &
      screened_rates(k_n_na21__he4_f18)*Y(jna21)*state % rho &
       )
    call set_jac_entry(jac, jf18, jn, scratch)

    scratch = (&
      -screened_rates(k_p_f18__ne19)*Y(jf18)*state % rho + screened_rates(k_p_o17__f18)*Y(jo17) &
      *state % rho &
       )
    call set_jac_entry(jac, jf18, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_f18__na22)*Y(jf18)*state % rho - screened_rates(k_he4_f18__p_ne21)* &
      Y(jf18)*state % rho + screened_rates(k_he4_n14__f18)*Y(jn14)*state % rho &
       )
    call set_jac_entry(jac, jf18, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_n14__f18)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jf18, jn14, scratch)

    scratch = (&
      screened_rates(k_p_o17__f18)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf18, jo17, scratch)

    scratch = (&
      -screened_rates(k_f18__o18__weak__wc12) - screened_rates(k_he4_f18__na22)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_f18__p_ne21)*Y(jhe4)*state % rho - &
      screened_rates(k_n_f18__f19)*Y(jn)*state % rho - screened_rates(k_n_f18__he4_n15)* &
      Y(jn)*state % rho - screened_rates(k_n_f18__p_o18)*Y(jn)*state % rho - &
      screened_rates(k_p_f18__ne19)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf18, jf18, scratch)

    scratch = (&
      screened_rates(k_n_na21__he4_f18)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jf18, jna21, scratch)

    scratch = (&
      screened_rates(k_n_f18__f19)*Y(jf18)*state % rho - screened_rates(k_n_f19__f20)*Y(jf19)* &
      state % rho + screened_rates(k_n_na22__he4_f19)*Y(jna22)*state % rho &
       )
    call set_jac_entry(jac, jf19, jn, scratch)

    scratch = (&
      -screened_rates(k_p_f19__he4_o16)*Y(jf19)*state % rho - screened_rates(k_p_f19__n_ne19)* &
      Y(jf19)*state % rho - screened_rates(k_p_f19__ne20)*Y(jf19)*state % rho + &
      screened_rates(k_p_o18__f19)*Y(jo18)*state % rho + screened_rates(k_p_o19__n_f19)* &
      Y(jo19)*state % rho &
       )
    call set_jac_entry(jac, jf19, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_f19__na23)*Y(jf19)*state % rho - screened_rates(k_he4_f19__p_ne22)* &
      Y(jf19)*state % rho + screened_rates(k_he4_n15__f19)*Y(jn15)*state % rho &
       )
    call set_jac_entry(jac, jf19, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_n15__f19)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jf19, jn15, scratch)

    scratch = (&
      screened_rates(k_p_o18__f19)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf19, jo18, scratch)

    scratch = (&
      screened_rates(k_o19__f19__weak__wc12) + screened_rates(k_p_o19__n_f19)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf19, jo19, scratch)

    scratch = (&
      screened_rates(k_n_f18__f19)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jf19, jf18, scratch)

    scratch = (&
      -screened_rates(k_he4_f19__na23)*Y(jhe4)*state % rho - screened_rates(k_he4_f19__p_ne22)* &
      Y(jhe4)*state % rho - screened_rates(k_n_f19__f20)*Y(jn)*state % rho - &
      screened_rates(k_p_f19__he4_o16)*Y(jp)*state % rho - screened_rates(k_p_f19__n_ne19) &
      *Y(jp)*state % rho - screened_rates(k_p_f19__ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf19, jf19, scratch)

    scratch = (&
      screened_rates(k_ne19__f19__weak__wc12) &
       )
    call set_jac_entry(jac, jf19, jne19, scratch)

    scratch = (&
      screened_rates(k_n_na22__he4_f19)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jf19, jna22, scratch)

    scratch = (&
      screened_rates(k_n_f19__f20)*Y(jf19)*state % rho - screened_rates(k_n_f20__f21)*Y(jf20)* &
      state % rho &
       )
    call set_jac_entry(jac, jf20, jn, scratch)

    scratch = (&
      -screened_rates(k_p_f20__he4_o17)*Y(jf20)*state % rho - screened_rates(k_p_f20__n_ne20)* &
      Y(jf20)*state % rho - screened_rates(k_p_f20__ne21)*Y(jf20)*state % rho + &
      screened_rates(k_p_o19__f20)*Y(jo19)*state % rho &
       )
    call set_jac_entry(jac, jf20, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_f20__n_na23)*Y(jf20)*state % rho - screened_rates(k_he4_f20__na24)* &
      Y(jf20)*state % rho - screened_rates(k_he4_f20__p_ne23)*Y(jf20)*state % rho &
       )
    call set_jac_entry(jac, jf20, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_o19__f20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf20, jo19, scratch)

    scratch = (&
      screened_rates(k_n_f19__f20)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jf20, jf19, scratch)

    scratch = (&
      -screened_rates(k_f20__ne20__weak__wc12) - screened_rates(k_he4_f20__n_na23)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_f20__na24)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_f20__p_ne23)*Y(jhe4)*state % rho - screened_rates(k_n_f20__f21) &
      *Y(jn)*state % rho - screened_rates(k_p_f20__he4_o17)*Y(jp)*state % rho - &
      screened_rates(k_p_f20__n_ne20)*Y(jp)*state % rho - screened_rates(k_p_f20__ne21)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf20, jf20, scratch)

    scratch = (&
      screened_rates(k_n_f20__f21)*Y(jf20)*state % rho &
       )
    call set_jac_entry(jac, jf21, jn, scratch)

    scratch = (&
      -screened_rates(k_p_f21__he4_o18)*Y(jf21)*state % rho - screened_rates(k_p_f21__n_ne21)* &
      Y(jf21)*state % rho - screened_rates(k_p_f21__ne22)*Y(jf21)*state % rho &
       )
    call set_jac_entry(jac, jf21, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_f21__n_na24)*Y(jf21)*state % rho - screened_rates(k_he4_f21__p_ne24)* &
      Y(jf21)*state % rho &
       )
    call set_jac_entry(jac, jf21, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_f20__f21)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jf21, jf20, scratch)

    scratch = (&
      -screened_rates(k_f21__ne21__weak__wc12) - screened_rates(k_he4_f21__n_na24)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_f21__p_ne24)*Y(jhe4)*state % rho - &
      screened_rates(k_p_f21__he4_o18)*Y(jp)*state % rho - screened_rates(k_p_f21__n_ne21) &
      *Y(jp)*state % rho - screened_rates(k_p_f21__ne22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jf21, jf21, scratch)

    scratch = (&
      screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*state % rho - screened_rates(k_n_ne19__he4_o16) &
      *Y(jne19)*state % rho - screened_rates(k_n_ne19__ne20)*Y(jne19)*state % rho &
       )
    call set_jac_entry(jac, jne19, jn, scratch)

    scratch = (&
      screened_rates(k_p_f18__ne19)*Y(jf18)*state % rho + screened_rates(k_p_f19__n_ne19)* &
      Y(jf19)*state % rho - screened_rates(k_p_ne19__na20)*Y(jne19)*state % rho &
       )
    call set_jac_entry(jac, jne19, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ne19__mg23)*Y(jne19)*state % rho - screened_rates(k_he4_ne19__p_na22) &
      *Y(jne19)*state % rho &
       )
    call set_jac_entry(jac, jne19, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_f18__ne19)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne19, jf18, scratch)

    scratch = (&
      screened_rates(k_p_f19__n_ne19)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne19, jf19, scratch)

    scratch = (&
      -screened_rates(k_he4_ne19__mg23)*Y(jhe4)*state % rho - screened_rates(k_he4_ne19__p_na22)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ne19__he4_o16)*Y(jn)*state % rho - &
      screened_rates(k_n_ne19__ne20)*Y(jn)*state % rho - &
      screened_rates(k_ne19__f19__weak__wc12) - screened_rates(k_p_ne19__na20)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jne19, jne19, scratch)

    scratch = (&
      screened_rates(k_n_mg22__he4_ne19)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne19, jmg22, scratch)

    scratch = (&
      screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)*state % rho + screened_rates(k_n_na20__p_ne20)* &
      Y(jna20)*state % rho + screened_rates(k_n_ne19__ne20)*Y(jne19)*state % rho - &
      screened_rates(k_n_ne20__ne21)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, jne20, jn, scratch)

    scratch = (&
      screened_rates(k_p_f19__ne20)*Y(jf19)*state % rho + screened_rates(k_p_f20__n_ne20)* &
      Y(jf20)*state % rho + screened_rates(k_p_na23__he4_ne20)*Y(jna23)* &
      state % rho - screened_rates(k_p_ne20__na21)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, jne20, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ne20__mg24)*Y(jne20)*state % rho + screened_rates(k_he4_o16__ne20)* &
      Y(jo16)*state % rho + screened_rates(k_he4_o17__n_ne20)*Y(jo17)*state % rho &
       )
    call set_jac_entry(jac, jne20, jhe4, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_c12_c12__he4_ne20)*Y(jc12)*state % rho - &
      screened_rates(k_c12_ne20__he4_si28)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__n_s31)*Y(jne20)*state % rho - &
      screened_rates(k_c12_ne20__p_p31)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, jne20, jc12, scratch)

    scratch = (&
      screened_rates(k_he4_o16__ne20)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne20, jo16, scratch)

    scratch = (&
      screened_rates(k_he4_o17__n_ne20)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne20, jo17, scratch)

    scratch = (&
      screened_rates(k_p_f19__ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne20, jf19, scratch)

    scratch = (&
      screened_rates(k_f20__ne20__weak__wc12) + screened_rates(k_p_f20__n_ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne20, jf20, scratch)

    scratch = (&
      screened_rates(k_n_ne19__ne20)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne20, jne19, scratch)

    scratch = (&
      -screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*state % rho - &
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*state % rho - &
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*state % rho - &
      screened_rates(k_he4_ne20__mg24)*Y(jhe4)*state % rho - &
      screened_rates(k_n_ne20__ne21)*Y(jn)*state % rho - screened_rates(k_p_ne20__na21)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne20, jne20, scratch)

    scratch = (&
      screened_rates(k_n_na20__p_ne20)*Y(jn)*state % rho + &
      screened_rates(k_na20__ne20__weak__wc12) &
       )
    call set_jac_entry(jac, jne20, jna20, scratch)

    scratch = (&
      screened_rates(k_p_na23__he4_ne20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne20, jna23, scratch)

    scratch = (&
      screened_rates(k_n_mg23__he4_ne20)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne20, jmg23, scratch)

    scratch = (&
      screened_rates(k_n_na21__p_ne21)*Y(jna21)*state % rho + screened_rates(k_n_ne20__ne21)* &
      Y(jne20)*state % rho - screened_rates(k_n_ne21__ne22)*Y(jne21)*state % rho &
       )
    call set_jac_entry(jac, jne21, jn, scratch)

    scratch = (&
      screened_rates(k_p_f20__ne21)*Y(jf20)*state % rho + screened_rates(k_p_f21__n_ne21)* &
      Y(jf21)*state % rho + screened_rates(k_p_na24__he4_ne21)*Y(jna24)* &
      state % rho - screened_rates(k_p_ne21__na22)*Y(jne21)*state % rho &
       )
    call set_jac_entry(jac, jne21, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f18__p_ne21)*Y(jf18)*state % rho - screened_rates(k_he4_ne21__mg25)* &
      Y(jne21)*state % rho - screened_rates(k_he4_ne21__n_mg24)*Y(jne21)* &
      state % rho + screened_rates(k_he4_o17__ne21)*Y(jo17)*state % rho + &
      screened_rates(k_he4_o18__n_ne21)*Y(jo18)*state % rho &
       )
    call set_jac_entry(jac, jne21, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_o17__ne21)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne21, jo17, scratch)

    scratch = (&
      screened_rates(k_he4_o18__n_ne21)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne21, jo18, scratch)

    scratch = (&
      screened_rates(k_he4_f18__p_ne21)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne21, jf18, scratch)

    scratch = (&
      screened_rates(k_p_f20__ne21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne21, jf20, scratch)

    scratch = (&
      screened_rates(k_f21__ne21__weak__wc12) + screened_rates(k_p_f21__n_ne21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne21, jf21, scratch)

    scratch = (&
      screened_rates(k_n_ne20__ne21)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne21, jne20, scratch)

    scratch = (&
      -screened_rates(k_he4_ne21__mg25)*Y(jhe4)*state % rho - screened_rates(k_he4_ne21__n_mg24)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ne21__ne22)*Y(jn)*state % rho - &
      screened_rates(k_p_ne21__na22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne21, jne21, scratch)

    scratch = (&
      screened_rates(k_n_na21__p_ne21)*Y(jn)*state % rho + &
      screened_rates(k_na21__ne21__weak__wc12) &
       )
    call set_jac_entry(jac, jne21, jna21, scratch)

    scratch = (&
      screened_rates(k_p_na24__he4_ne21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne21, jna24, scratch)

    scratch = (&
      screened_rates(k_n_na22__p_ne22)*Y(jna22)*state % rho + screened_rates(k_n_ne21__ne22)* &
      Y(jne21)*state % rho - screened_rates(k_n_ne22__ne23)*Y(jne22)*state % rho &
       )
    call set_jac_entry(jac, jne22, jn, scratch)

    scratch = (&
      screened_rates(k_p_f21__ne22)*Y(jf21)*state % rho - screened_rates(k_p_ne22__na23)* &
      Y(jne22)*state % rho &
       )
    call set_jac_entry(jac, jne22, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f19__p_ne22)*Y(jf19)*state % rho - screened_rates(k_he4_ne22__mg26)* &
      Y(jne22)*state % rho - screened_rates(k_he4_ne22__n_mg25)*Y(jne22)* &
      state % rho + screened_rates(k_he4_o18__ne22)*Y(jo18)*state % rho + &
      screened_rates(k_he4_o19__n_ne22)*Y(jo19)*state % rho &
       )
    call set_jac_entry(jac, jne22, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_o18__ne22)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne22, jo18, scratch)

    scratch = (&
      screened_rates(k_he4_o19__n_ne22)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne22, jo19, scratch)

    scratch = (&
      screened_rates(k_he4_f19__p_ne22)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne22, jf19, scratch)

    scratch = (&
      screened_rates(k_p_f21__ne22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne22, jf21, scratch)

    scratch = (&
      screened_rates(k_n_ne21__ne22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne22, jne21, scratch)

    scratch = (&
      -screened_rates(k_he4_ne22__mg26)*Y(jhe4)*state % rho - screened_rates(k_he4_ne22__n_mg25)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ne22__ne23)*Y(jn)*state % rho - &
      screened_rates(k_p_ne22__na23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne22, jne22, scratch)

    scratch = (&
      screened_rates(k_n_na22__p_ne22)*Y(jn)*state % rho + &
      screened_rates(k_na22__ne22__weak__wc12) &
       )
    call set_jac_entry(jac, jne22, jna22, scratch)

    scratch = (&
      screened_rates(k_n_ne22__ne23)*Y(jne22)*state % rho - screened_rates(k_n_ne23__ne24)* &
      Y(jne23)*state % rho &
       )
    call set_jac_entry(jac, jne23, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ne23__n_na23)*Y(jne23)*state % rho - screened_rates(k_p_ne23__na24)* &
      Y(jne23)*state % rho &
       )
    call set_jac_entry(jac, jne23, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f20__p_ne23)*Y(jf20)*state % rho - screened_rates(k_he4_ne23__mg27)* &
      Y(jne23)*state % rho - screened_rates(k_he4_ne23__n_mg26)*Y(jne23)* &
      state % rho + screened_rates(k_he4_o19__ne23)*Y(jo19)*state % rho &
       )
    call set_jac_entry(jac, jne23, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_o19__ne23)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne23, jo19, scratch)

    scratch = (&
      screened_rates(k_he4_f20__p_ne23)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne23, jf20, scratch)

    scratch = (&
      screened_rates(k_n_ne22__ne23)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne23, jne22, scratch)

    scratch = (&
      -screened_rates(k_he4_ne23__mg27)*Y(jhe4)*state % rho - screened_rates(k_he4_ne23__n_mg26)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ne23__ne24)*Y(jn)*state % rho - &
      screened_rates(k_ne23__na23__weak__wc12) - screened_rates(k_p_ne23__n_na23)*Y(jp) &
      *state % rho - screened_rates(k_p_ne23__na24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jne23, jne23, scratch)

    scratch = (&
      screened_rates(k_n_ne23__ne24)*Y(jne23)*state % rho &
       )
    call set_jac_entry(jac, jne24, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ne24__n_na24)*Y(jne24)*state % rho &
       )
    call set_jac_entry(jac, jne24, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f21__p_ne24)*Y(jf21)*state % rho - screened_rates(k_he4_ne24__n_mg27)* &
      Y(jne24)*state % rho &
       )
    call set_jac_entry(jac, jne24, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_f21__p_ne24)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jne24, jf21, scratch)

    scratch = (&
      screened_rates(k_n_ne23__ne24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jne24, jne23, scratch)

    scratch = (&
      -screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*state % rho - &
      screened_rates(k_ne24__na24__weak__wc12) - screened_rates(k_p_ne24__n_na24)*Y(jp) &
      *state % rho &
       )
    call set_jac_entry(jac, jne24, jne24, scratch)

    scratch = (&
      -screened_rates(k_n_na20__na21)*Y(jna20)*state % rho - screened_rates(k_n_na20__p_ne20)* &
      Y(jna20)*state % rho &
       )
    call set_jac_entry(jac, jna20, jn, scratch)

    scratch = (&
      screened_rates(k_p_ne19__na20)*Y(jne19)*state % rho &
       )
    call set_jac_entry(jac, jna20, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_na20__p_mg23)*Y(jna20)*state % rho &
       )
    call set_jac_entry(jac, jna20, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_ne19__na20)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna20, jne19, scratch)

    scratch = (&
      -screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*state % rho - screened_rates(k_n_na20__na21)* &
      Y(jn)*state % rho - screened_rates(k_n_na20__p_ne20)*Y(jn)*state % rho - &
      screened_rates(k_na20__he4_o16__weak__wc12) - &
      screened_rates(k_na20__ne20__weak__wc12) &
       )
    call set_jac_entry(jac, jna20, jna20, scratch)

    scratch = (&
      screened_rates(k_n_na20__na21)*Y(jna20)*state % rho - screened_rates(k_n_na21__he4_f18)* &
      Y(jna21)*state % rho - screened_rates(k_n_na21__na22)*Y(jna21)*state % rho - &
      screened_rates(k_n_na21__p_ne21)*Y(jna21)*state % rho &
       )
    call set_jac_entry(jac, jna21, jn, scratch)

    scratch = (&
      screened_rates(k_p_mg24__he4_na21)*Y(jmg24)*state % rho - screened_rates(k_p_na21__mg22)* &
      Y(jna21)*state % rho + screened_rates(k_p_ne20__na21)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, jna21, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_na21__al25)*Y(jna21)*state % rho &
       )
    call set_jac_entry(jac, jna21, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_ne20__na21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna21, jne20, scratch)

    scratch = (&
      screened_rates(k_n_na20__na21)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jna21, jna20, scratch)

    scratch = (&
      -screened_rates(k_he4_na21__al25)*Y(jhe4)*state % rho - screened_rates(k_n_na21__he4_f18)* &
      Y(jn)*state % rho - screened_rates(k_n_na21__na22)*Y(jn)*state % rho - &
      screened_rates(k_n_na21__p_ne21)*Y(jn)*state % rho - &
      screened_rates(k_na21__ne21__weak__wc12) - screened_rates(k_p_na21__mg22)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jna21, jna21, scratch)

    scratch = (&
      screened_rates(k_p_mg24__he4_na21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna21, jmg24, scratch)

    scratch = (&
      screened_rates(k_n_al25__he4_na22)*Y(jal25)*state % rho + screened_rates(k_n_mg22__p_na22)* &
      Y(jmg22)*state % rho + screened_rates(k_n_na21__na22)*Y(jna21)*state % rho - &
      screened_rates(k_n_na22__he4_f19)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__na23)*Y(jna22)*state % rho - &
      screened_rates(k_n_na22__p_ne22)*Y(jna22)*state % rho &
       )
    call set_jac_entry(jac, jna22, jn, scratch)

    scratch = (&
      -screened_rates(k_p_na22__mg23)*Y(jna22)*state % rho + screened_rates(k_p_ne21__na22)* &
      Y(jne21)*state % rho &
       )
    call set_jac_entry(jac, jna22, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f18__na22)*Y(jf18)*state % rho - screened_rates(k_he4_na22__al26)* &
      Y(jna22)*state % rho - screened_rates(k_he4_na22__p_mg25)*Y(jna22)* &
      state % rho + screened_rates(k_he4_ne19__p_na22)*Y(jne19)*state % rho &
       )
    call set_jac_entry(jac, jna22, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_f18__na22)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jna22, jf18, scratch)

    scratch = (&
      screened_rates(k_he4_ne19__p_na22)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jna22, jne19, scratch)

    scratch = (&
      screened_rates(k_p_ne21__na22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna22, jne21, scratch)

    scratch = (&
      screened_rates(k_n_na21__na22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jna22, jna21, scratch)

    scratch = (&
      -screened_rates(k_he4_na22__al26)*Y(jhe4)*state % rho - screened_rates(k_he4_na22__p_mg25)* &
      Y(jhe4)*state % rho - screened_rates(k_n_na22__he4_f19)*Y(jn)*state % rho - &
      screened_rates(k_n_na22__na23)*Y(jn)*state % rho - screened_rates(k_n_na22__p_ne22)* &
      Y(jn)*state % rho - screened_rates(k_na22__ne22__weak__wc12) - &
      screened_rates(k_p_na22__mg23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna22, jna22, scratch)

    scratch = (&
      screened_rates(k_mg22__na22__weak__wc12) + screened_rates(k_n_mg22__p_na22)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jna22, jmg22, scratch)

    scratch = (&
      screened_rates(k_n_al25__he4_na22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jna22, jal25, scratch)

    scratch = (&
      screened_rates(k_n_al26__he4_na23)*Y(jal26)*state % rho + screened_rates(k_n_na22__na23)* &
      Y(jna22)*state % rho - screened_rates(k_n_na23__na24)*Y(jna23)*state % rho &
       )
    call set_jac_entry(jac, jna23, jn, scratch)

    scratch = (&
      -screened_rates(k_p_na23__he4_ne20)*Y(jna23)*state % rho - screened_rates(k_p_na23__mg24)* &
      Y(jna23)*state % rho - screened_rates(k_p_na23__n_mg23)*Y(jna23)*state % rho &
      + screened_rates(k_p_ne22__na23)*Y(jne22)*state % rho + &
      screened_rates(k_p_ne23__n_na23)*Y(jne23)*state % rho &
       )
    call set_jac_entry(jac, jna23, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f19__na23)*Y(jf19)*state % rho + screened_rates(k_he4_f20__n_na23)* &
      Y(jf20)*state % rho - screened_rates(k_he4_na23__al27)*Y(jna23)*state % rho &
      - screened_rates(k_he4_na23__p_mg26)*Y(jna23)*state % rho &
       )
    call set_jac_entry(jac, jna23, jhe4, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_c12_c12__p_na23)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jna23, jc12, scratch)

    scratch = (&
      screened_rates(k_he4_f19__na23)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jna23, jf19, scratch)

    scratch = (&
      screened_rates(k_he4_f20__n_na23)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jna23, jf20, scratch)

    scratch = (&
      screened_rates(k_p_ne22__na23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna23, jne22, scratch)

    scratch = (&
      screened_rates(k_ne23__na23__weak__wc12) + screened_rates(k_p_ne23__n_na23)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jna23, jne23, scratch)

    scratch = (&
      screened_rates(k_n_na22__na23)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jna23, jna22, scratch)

    scratch = (&
      -screened_rates(k_he4_na23__al27)*Y(jhe4)*state % rho - screened_rates(k_he4_na23__p_mg26)* &
      Y(jhe4)*state % rho - screened_rates(k_n_na23__na24)*Y(jn)*state % rho - &
      screened_rates(k_p_na23__he4_ne20)*Y(jp)*state % rho - &
      screened_rates(k_p_na23__mg24)*Y(jp)*state % rho - screened_rates(k_p_na23__n_mg23)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna23, jna23, scratch)

    scratch = (&
      screened_rates(k_mg23__na23__weak__wc12) &
       )
    call set_jac_entry(jac, jna23, jmg23, scratch)

    scratch = (&
      screened_rates(k_n_al26__he4_na23)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jna23, jal26, scratch)

    scratch = (&
      screened_rates(k_n_na23__na24)*Y(jna23)*state % rho &
       )
    call set_jac_entry(jac, jna24, jn, scratch)

    scratch = (&
      -screened_rates(k_p_na24__he4_ne21)*Y(jna24)*state % rho - screened_rates(k_p_na24__mg25)* &
      Y(jna24)*state % rho - screened_rates(k_p_na24__n_mg24)*Y(jna24)*state % rho &
      + screened_rates(k_p_ne23__na24)*Y(jne23)*state % rho + &
      screened_rates(k_p_ne24__n_na24)*Y(jne24)*state % rho &
       )
    call set_jac_entry(jac, jna24, jp, scratch)

    scratch = (&
      screened_rates(k_he4_f20__na24)*Y(jf20)*state % rho + screened_rates(k_he4_f21__n_na24)* &
      Y(jf21)*state % rho - screened_rates(k_he4_na24__al28)*Y(jna24)*state % rho &
      - screened_rates(k_he4_na24__n_al27)*Y(jna24)*state % rho - &
      screened_rates(k_he4_na24__p_mg27)*Y(jna24)*state % rho &
       )
    call set_jac_entry(jac, jna24, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_f20__na24)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jna24, jf20, scratch)

    scratch = (&
      screened_rates(k_he4_f21__n_na24)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jna24, jf21, scratch)

    scratch = (&
      screened_rates(k_p_ne23__na24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna24, jne23, scratch)

    scratch = (&
      screened_rates(k_ne24__na24__weak__wc12) + screened_rates(k_p_ne24__n_na24)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jna24, jne24, scratch)

    scratch = (&
      screened_rates(k_n_na23__na24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jna24, jna23, scratch)

    scratch = (&
      -screened_rates(k_he4_na24__al28)*Y(jhe4)*state % rho - screened_rates(k_he4_na24__n_al27)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*state % rho &
      - screened_rates(k_na24__mg24__weak__wc12) - screened_rates(k_p_na24__he4_ne21)* &
      Y(jp)*state % rho - screened_rates(k_p_na24__mg25)*Y(jp)*state % rho - &
      screened_rates(k_p_na24__n_mg24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jna24, jna24, scratch)

    scratch = (&
      -screened_rates(k_n_mg22__he4_ne19)*Y(jmg22)*state % rho - screened_rates(k_n_mg22__mg23)* &
      Y(jmg22)*state % rho - screened_rates(k_n_mg22__p_na22)*Y(jmg22)*state % rho &
       )
    call set_jac_entry(jac, jmg22, jn, scratch)

    scratch = (&
      screened_rates(k_p_na21__mg22)*Y(jna21)*state % rho &
       )
    call set_jac_entry(jac, jmg22, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mg22__p_al25)*Y(jmg22)*state % rho &
       )
    call set_jac_entry(jac, jmg22, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_na21__mg22)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg22, jna21, scratch)

    scratch = (&
      -screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*state % rho - &
      screened_rates(k_mg22__na22__weak__wc12) - screened_rates(k_n_mg22__he4_ne19)* &
      Y(jn)*state % rho - screened_rates(k_n_mg22__mg23)*Y(jn)*state % rho - &
      screened_rates(k_n_mg22__p_na22)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg22, jmg22, scratch)

    scratch = (&
      screened_rates(k_n_mg22__mg23)*Y(jmg22)*state % rho - screened_rates(k_n_mg23__c12_c12)* &
      Y(jmg23)*state % rho - screened_rates(k_n_mg23__he4_ne20)*Y(jmg23)* &
      state % rho - screened_rates(k_n_mg23__mg24)*Y(jmg23)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jn, scratch)

    scratch = (&
      screened_rates(k_p_na22__mg23)*Y(jna22)*state % rho + screened_rates(k_p_na23__n_mg23)* &
      Y(jna23)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mg23__p_al26)*Y(jmg23)*state % rho - screened_rates(k_he4_mg23__si27) &
      *Y(jmg23)*state % rho + screened_rates(k_he4_na20__p_mg23)*Y(jna20)* &
      state % rho + screened_rates(k_he4_ne19__mg23)*Y(jne19)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ne19__mg23)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jne19, scratch)

    scratch = (&
      screened_rates(k_he4_na20__p_mg23)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jna20, scratch)

    scratch = (&
      screened_rates(k_p_na22__mg23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jna22, scratch)

    scratch = (&
      screened_rates(k_p_na23__n_mg23)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jna23, scratch)

    scratch = (&
      screened_rates(k_n_mg22__mg23)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jmg22, scratch)

    scratch = (&
      -screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*state % rho - screened_rates(k_he4_mg23__si27)* &
      Y(jhe4)*state % rho - screened_rates(k_mg23__na23__weak__wc12) - &
      screened_rates(k_n_mg23__c12_c12)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__he4_ne20)*Y(jn)*state % rho - &
      screened_rates(k_n_mg23__mg24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg23, jmg23, scratch)

    scratch = (&
      screened_rates(k_n_mg23__mg24)*Y(jmg23)*state % rho - screened_rates(k_n_mg24__mg25)* &
      Y(jmg24)*state % rho + screened_rates(k_n_si27__he4_mg24)*Y(jsi27)* &
      state % rho &
       )
    call set_jac_entry(jac, jmg24, jn, scratch)

    scratch = (&
      screened_rates(k_p_al27__he4_mg24)*Y(jal27)*state % rho - screened_rates(k_p_mg24__al25)* &
      Y(jmg24)*state % rho - screened_rates(k_p_mg24__he4_na21)*Y(jmg24)* &
      state % rho + screened_rates(k_p_na23__mg24)*Y(jna23)*state % rho + &
      screened_rates(k_p_na24__n_mg24)*Y(jna24)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mg24__si28)*Y(jmg24)*state % rho + screened_rates(k_he4_ne20__mg24)* &
      Y(jne20)*state % rho + screened_rates(k_he4_ne21__n_mg24)*Y(jne21)* &
      state % rho &
       )
    call set_jac_entry(jac, jmg24, jhe4, scratch)

    scratch = (&
      screened_rates(k_c12_o16__he4_mg24)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jc12, scratch)

    scratch = (&
      screened_rates(k_c12_o16__he4_mg24)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jo16, scratch)

    scratch = (&
      screened_rates(k_he4_ne20__mg24)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jne20, scratch)

    scratch = (&
      screened_rates(k_he4_ne21__n_mg24)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jne21, scratch)

    scratch = (&
      screened_rates(k_p_na23__mg24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jna23, scratch)

    scratch = (&
      screened_rates(k_na24__mg24__weak__wc12) + screened_rates(k_p_na24__n_mg24)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jmg24, jna24, scratch)

    scratch = (&
      screened_rates(k_n_mg23__mg24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jmg23, scratch)

    scratch = (&
      -screened_rates(k_he4_mg24__si28)*Y(jhe4)*state % rho - screened_rates(k_n_mg24__mg25)* &
      Y(jn)*state % rho - screened_rates(k_p_mg24__al25)*Y(jp)*state % rho - &
      screened_rates(k_p_mg24__he4_na21)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jmg24, scratch)

    scratch = (&
      screened_rates(k_p_al27__he4_mg24)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jal27, scratch)

    scratch = (&
      screened_rates(k_n_si27__he4_mg24)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg24, jsi27, scratch)

    scratch = (&
      screened_rates(k_n_al25__p_mg25)*Y(jal25)*state % rho + screened_rates(k_n_mg24__mg25)* &
      Y(jmg24)*state % rho - screened_rates(k_n_mg25__mg26)*Y(jmg25)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jn, scratch)

    scratch = (&
      screened_rates(k_p_al28__he4_mg25)*Y(jal28)*state % rho - screened_rates(k_p_mg25__al26)* &
      Y(jmg25)*state % rho + screened_rates(k_p_na24__mg25)*Y(jna24)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mg25__n_si28)*Y(jmg25)*state % rho - screened_rates(k_he4_mg25__si29) &
      *Y(jmg25)*state % rho + screened_rates(k_he4_na22__p_mg25)*Y(jna22)* &
      state % rho + screened_rates(k_he4_ne21__mg25)*Y(jne21)*state % rho + &
      screened_rates(k_he4_ne22__n_mg25)*Y(jne22)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ne21__mg25)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jne21, scratch)

    scratch = (&
      screened_rates(k_he4_ne22__n_mg25)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jne22, scratch)

    scratch = (&
      screened_rates(k_he4_na22__p_mg25)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jna22, scratch)

    scratch = (&
      screened_rates(k_p_na24__mg25)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jna24, scratch)

    scratch = (&
      screened_rates(k_n_mg24__mg25)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jmg24, scratch)

    scratch = (&
      -screened_rates(k_he4_mg25__n_si28)*Y(jhe4)*state % rho - screened_rates(k_he4_mg25__si29)* &
      Y(jhe4)*state % rho - screened_rates(k_n_mg25__mg26)*Y(jn)*state % rho - &
      screened_rates(k_p_mg25__al26)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jmg25, scratch)

    scratch = (&
      screened_rates(k_al25__mg25__weak__wc12) + screened_rates(k_n_al25__p_mg25)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jmg25, jal25, scratch)

    scratch = (&
      screened_rates(k_p_al28__he4_mg25)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg25, jal28, scratch)

    scratch = (&
      screened_rates(k_n_al26__p_mg26)*Y(jal26)*state % rho + screened_rates(k_n_mg25__mg26)* &
      Y(jmg25)*state % rho - screened_rates(k_n_mg26__mg27)*Y(jmg26)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jn, scratch)

    scratch = (&
      screened_rates(k_p_al29__he4_mg26)*Y(jal29)*state % rho - screened_rates(k_p_mg26__al27)* &
      Y(jmg26)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mg26__n_si29)*Y(jmg26)*state % rho - screened_rates(k_he4_mg26__si30) &
      *Y(jmg26)*state % rho + screened_rates(k_he4_na23__p_mg26)*Y(jna23)* &
      state % rho + screened_rates(k_he4_ne22__mg26)*Y(jne22)*state % rho + &
      screened_rates(k_he4_ne23__n_mg26)*Y(jne23)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ne22__mg26)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jne22, scratch)

    scratch = (&
      screened_rates(k_he4_ne23__n_mg26)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jne23, scratch)

    scratch = (&
      screened_rates(k_he4_na23__p_mg26)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jna23, scratch)

    scratch = (&
      screened_rates(k_n_mg25__mg26)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jmg25, scratch)

    scratch = (&
      -screened_rates(k_he4_mg26__n_si29)*Y(jhe4)*state % rho - screened_rates(k_he4_mg26__si30)* &
      Y(jhe4)*state % rho - screened_rates(k_n_mg26__mg27)*Y(jn)*state % rho - &
      screened_rates(k_p_mg26__al27)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jmg26, scratch)

    scratch = (&
      screened_rates(k_al26__mg26__weak__wc12) + screened_rates(k_n_al26__p_mg26)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jmg26, jal26, scratch)

    scratch = (&
      screened_rates(k_p_al29__he4_mg26)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg26, jal29, scratch)

    scratch = (&
      screened_rates(k_n_mg26__mg27)*Y(jmg26)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jn, scratch)

    scratch = (&
      -screened_rates(k_p_mg27__al28)*Y(jmg27)*state % rho - screened_rates(k_p_mg27__n_al27)* &
      Y(jmg27)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mg27__n_si30)*Y(jmg27)*state % rho - screened_rates(k_he4_mg27__si31) &
      *Y(jmg27)*state % rho + screened_rates(k_he4_na24__p_mg27)*Y(jna24)* &
      state % rho + screened_rates(k_he4_ne23__mg27)*Y(jne23)*state % rho + &
      screened_rates(k_he4_ne24__n_mg27)*Y(jne24)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ne23__mg27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jne23, scratch)

    scratch = (&
      screened_rates(k_he4_ne24__n_mg27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jne24, scratch)

    scratch = (&
      screened_rates(k_he4_na24__p_mg27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jna24, scratch)

    scratch = (&
      screened_rates(k_n_mg26__mg27)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jmg26, scratch)

    scratch = (&
      -screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*state % rho - screened_rates(k_he4_mg27__si31)* &
      Y(jhe4)*state % rho - screened_rates(k_mg27__al27__weak__wc12) - &
      screened_rates(k_p_mg27__al28)*Y(jp)*state % rho - screened_rates(k_p_mg27__n_al27)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmg27, jmg27, scratch)

    scratch = (&
      -screened_rates(k_n_al25__al26)*Y(jal25)*state % rho - screened_rates(k_n_al25__he4_na22)* &
      Y(jal25)*state % rho - screened_rates(k_n_al25__p_mg25)*Y(jal25)*state % rho &
       )
    call set_jac_entry(jac, jal25, jn, scratch)

    scratch = (&
      screened_rates(k_p_mg24__al25)*Y(jmg24)*state % rho &
       )
    call set_jac_entry(jac, jal25, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_al25__p29)*Y(jal25)*state % rho - screened_rates(k_he4_al25__p_si28)* &
      Y(jal25)*state % rho + screened_rates(k_he4_mg22__p_al25)*Y(jmg22)* &
      state % rho + screened_rates(k_he4_na21__al25)*Y(jna21)*state % rho &
       )
    call set_jac_entry(jac, jal25, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_na21__al25)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal25, jna21, scratch)

    scratch = (&
      screened_rates(k_he4_mg22__p_al25)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal25, jmg22, scratch)

    scratch = (&
      screened_rates(k_p_mg24__al25)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal25, jmg24, scratch)

    scratch = (&
      -screened_rates(k_al25__mg25__weak__wc12) - screened_rates(k_he4_al25__p29)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_al25__p_si28)*Y(jhe4)*state % rho - &
      screened_rates(k_n_al25__al26)*Y(jn)*state % rho - &
      screened_rates(k_n_al25__he4_na22)*Y(jn)*state % rho - &
      screened_rates(k_n_al25__p_mg25)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jal25, jal25, scratch)

    scratch = (&
      screened_rates(k_n_al25__al26)*Y(jal25)*state % rho - screened_rates(k_n_al26__al27)* &
      Y(jal26)*state % rho - screened_rates(k_n_al26__he4_na23)*Y(jal26)* &
      state % rho - screened_rates(k_n_al26__p_mg26)*Y(jal26)*state % rho + &
      screened_rates(k_n_p29__he4_al26)*Y(jp29)*state % rho &
       )
    call set_jac_entry(jac, jal26, jn, scratch)

    scratch = (&
      -screened_rates(k_p_al26__si27)*Y(jal26)*state % rho + screened_rates(k_p_mg25__al26)* &
      Y(jmg25)*state % rho &
       )
    call set_jac_entry(jac, jal26, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_al26__p30)*Y(jal26)*state % rho - screened_rates(k_he4_al26__p_si29)* &
      Y(jal26)*state % rho + screened_rates(k_he4_mg23__p_al26)*Y(jmg23)* &
      state % rho + screened_rates(k_he4_na22__al26)*Y(jna22)*state % rho &
       )
    call set_jac_entry(jac, jal26, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_na22__al26)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal26, jna22, scratch)

    scratch = (&
      screened_rates(k_he4_mg23__p_al26)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal26, jmg23, scratch)

    scratch = (&
      screened_rates(k_p_mg25__al26)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal26, jmg25, scratch)

    scratch = (&
      screened_rates(k_n_al25__al26)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jal26, jal25, scratch)

    scratch = (&
      -screened_rates(k_al26__mg26__weak__wc12) - screened_rates(k_he4_al26__p30)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_al26__p_si29)*Y(jhe4)*state % rho - &
      screened_rates(k_n_al26__al27)*Y(jn)*state % rho - &
      screened_rates(k_n_al26__he4_na23)*Y(jn)*state % rho - &
      screened_rates(k_n_al26__p_mg26)*Y(jn)*state % rho - screened_rates(k_p_al26__si27)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal26, jal26, scratch)

    scratch = (&
      screened_rates(k_n_p29__he4_al26)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jal26, jp29, scratch)

    scratch = (&
      screened_rates(k_n_al26__al27)*Y(jal26)*state % rho - screened_rates(k_n_al27__al28)* &
      Y(jal27)*state % rho + screened_rates(k_n_si27__p_al27)*Y(jsi27)*state % rho &
       )
    call set_jac_entry(jac, jal27, jn, scratch)

    scratch = (&
      -screened_rates(k_p_al27__he4_mg24)*Y(jal27)*state % rho - screened_rates(k_p_al27__si28)* &
      Y(jal27)*state % rho + screened_rates(k_p_mg26__al27)*Y(jmg26)*state % rho + &
      screened_rates(k_p_mg27__n_al27)*Y(jmg27)*state % rho &
       )
    call set_jac_entry(jac, jal27, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_al27__n_p30)*Y(jal27)*state % rho - screened_rates(k_he4_al27__p31)* &
      Y(jal27)*state % rho - screened_rates(k_he4_al27__p_si30)*Y(jal27)* &
      state % rho + screened_rates(k_he4_na23__al27)*Y(jna23)*state % rho + &
      screened_rates(k_he4_na24__n_al27)*Y(jna24)*state % rho &
       )
    call set_jac_entry(jac, jal27, jhe4, scratch)

    scratch = (&
      screened_rates(k_c12_o16__p_al27)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jal27, jc12, scratch)

    scratch = (&
      screened_rates(k_c12_o16__p_al27)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jal27, jo16, scratch)

    scratch = (&
      screened_rates(k_he4_na23__al27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal27, jna23, scratch)

    scratch = (&
      screened_rates(k_he4_na24__n_al27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal27, jna24, scratch)

    scratch = (&
      screened_rates(k_p_mg26__al27)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal27, jmg26, scratch)

    scratch = (&
      screened_rates(k_mg27__al27__weak__wc12) + screened_rates(k_p_mg27__n_al27)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jal27, jmg27, scratch)

    scratch = (&
      screened_rates(k_n_al26__al27)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jal27, jal26, scratch)

    scratch = (&
      -screened_rates(k_he4_al27__n_p30)*Y(jhe4)*state % rho - screened_rates(k_he4_al27__p31)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_al27__p_si30)*Y(jhe4)*state % rho &
      - screened_rates(k_n_al27__al28)*Y(jn)*state % rho - &
      screened_rates(k_p_al27__he4_mg24)*Y(jp)*state % rho - &
      screened_rates(k_p_al27__si28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal27, jal27, scratch)

    scratch = (&
      screened_rates(k_n_si27__p_al27)*Y(jn)*state % rho + &
      screened_rates(k_si27__al27__weak__wc12) &
       )
    call set_jac_entry(jac, jal27, jsi27, scratch)

    scratch = (&
      screened_rates(k_n_al27__al28)*Y(jal27)*state % rho - screened_rates(k_n_al28__al29)* &
      Y(jal28)*state % rho &
       )
    call set_jac_entry(jac, jal28, jn, scratch)

    scratch = (&
      -screened_rates(k_p_al28__he4_mg25)*Y(jal28)*state % rho - screened_rates(k_p_al28__n_si28) &
      *Y(jal28)*state % rho - screened_rates(k_p_al28__si29)*Y(jal28)*state % rho &
      + screened_rates(k_p_mg27__al28)*Y(jmg27)*state % rho &
       )
    call set_jac_entry(jac, jal28, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_al28__n_p31)*Y(jal28)*state % rho - screened_rates(k_he4_al28__p32)* &
      Y(jal28)*state % rho - screened_rates(k_he4_al28__p_si31)*Y(jal28)* &
      state % rho + screened_rates(k_he4_na24__al28)*Y(jna24)*state % rho &
       )
    call set_jac_entry(jac, jal28, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_na24__al28)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jal28, jna24, scratch)

    scratch = (&
      screened_rates(k_p_mg27__al28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal28, jmg27, scratch)

    scratch = (&
      screened_rates(k_n_al27__al28)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jal28, jal27, scratch)

    scratch = (&
      -screened_rates(k_al28__si28__weak__wc12) - screened_rates(k_he4_al28__n_p31)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_al28__p32)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al28__p_si31)*Y(jhe4)*state % rho - &
      screened_rates(k_n_al28__al29)*Y(jn)*state % rho - &
      screened_rates(k_p_al28__he4_mg25)*Y(jp)*state % rho - &
      screened_rates(k_p_al28__n_si28)*Y(jp)*state % rho - screened_rates(k_p_al28__si29)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal28, jal28, scratch)

    scratch = (&
      screened_rates(k_n_al28__al29)*Y(jal28)*state % rho &
       )
    call set_jac_entry(jac, jal29, jn, scratch)

    scratch = (&
      -screened_rates(k_p_al29__he4_mg26)*Y(jal29)*state % rho - screened_rates(k_p_al29__n_si29) &
      *Y(jal29)*state % rho - screened_rates(k_p_al29__si30)*Y(jal29)*state % rho &
       )
    call set_jac_entry(jac, jal29, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_al29__n_p32)*Y(jal29)*state % rho - screened_rates(k_he4_al29__p33)* &
      Y(jal29)*state % rho - screened_rates(k_he4_al29__p_si32)*Y(jal29)* &
      state % rho &
       )
    call set_jac_entry(jac, jal29, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_al28__al29)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jal29, jal28, scratch)

    scratch = (&
      -screened_rates(k_al29__si29__weak__wc12) - screened_rates(k_he4_al29__n_p32)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_al29__p33)*Y(jhe4)*state % rho - &
      screened_rates(k_he4_al29__p_si32)*Y(jhe4)*state % rho - &
      screened_rates(k_p_al29__he4_mg26)*Y(jp)*state % rho - &
      screened_rates(k_p_al29__n_si29)*Y(jp)*state % rho - screened_rates(k_p_al29__si30)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jal29, jal29, scratch)

    scratch = (&
      screened_rates(k_n_s30__he4_si27)*Y(js30)*state % rho - screened_rates(k_n_si27__c12_o16)* &
      Y(jsi27)*state % rho - screened_rates(k_n_si27__he4_mg24)*Y(jsi27)* &
      state % rho - screened_rates(k_n_si27__p_al27)*Y(jsi27)*state % rho - &
      screened_rates(k_n_si27__si28)*Y(jsi27)*state % rho &
       )
    call set_jac_entry(jac, jsi27, jn, scratch)

    scratch = (&
      screened_rates(k_p_al26__si27)*Y(jal26)*state % rho &
       )
    call set_jac_entry(jac, jsi27, jp, scratch)

    scratch = (&
      screened_rates(k_he4_mg23__si27)*Y(jmg23)*state % rho - screened_rates(k_he4_si27__p_p30)* &
      Y(jsi27)*state % rho - screened_rates(k_he4_si27__s31)*Y(jsi27)*state % rho &
       )
    call set_jac_entry(jac, jsi27, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mg23__si27)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi27, jmg23, scratch)

    scratch = (&
      screened_rates(k_p_al26__si27)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi27, jal26, scratch)

    scratch = (&
      -screened_rates(k_he4_si27__p_p30)*Y(jhe4)*state % rho - screened_rates(k_he4_si27__s31)* &
      Y(jhe4)*state % rho - screened_rates(k_n_si27__c12_o16)*Y(jn)*state % rho - &
      screened_rates(k_n_si27__he4_mg24)*Y(jn)*state % rho - &
      screened_rates(k_n_si27__p_al27)*Y(jn)*state % rho - screened_rates(k_n_si27__si28)* &
      Y(jn)*state % rho - screened_rates(k_si27__al27__weak__wc12) &
       )
    call set_jac_entry(jac, jsi27, jsi27, scratch)

    scratch = (&
      screened_rates(k_n_s30__he4_si27)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi27, js30, scratch)

    scratch = (&
      screened_rates(k_n_s31__he4_si28)*Y(js31)*state % rho + screened_rates(k_n_si27__si28)* &
      Y(jsi27)*state % rho - screened_rates(k_n_si28__si29)*Y(jsi28)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jn, scratch)

    scratch = (&
      screened_rates(k_p_al27__si28)*Y(jal27)*state % rho + screened_rates(k_p_al28__n_si28)* &
      Y(jal28)*state % rho + screened_rates(k_p_p31__he4_si28)*Y(jp31)*state % rho &
      - screened_rates(k_p_si28__p29)*Y(jsi28)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al25__p_si28)*Y(jal25)*state % rho + screened_rates(k_he4_mg24__si28)* &
      Y(jmg24)*state % rho + screened_rates(k_he4_mg25__n_si28)*Y(jmg25)* &
      state % rho - screened_rates(k_he4_si28__s32)*Y(jsi28)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jhe4, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__he4_si28)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jc12, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_o16_o16__he4_si28)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jo16, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__he4_si28)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jne20, scratch)

    scratch = (&
      screened_rates(k_he4_mg24__si28)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jmg24, scratch)

    scratch = (&
      screened_rates(k_he4_mg25__n_si28)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jmg25, scratch)

    scratch = (&
      screened_rates(k_he4_al25__p_si28)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jal25, scratch)

    scratch = (&
      screened_rates(k_p_al27__si28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jal27, scratch)

    scratch = (&
      screened_rates(k_al28__si28__weak__wc12) + screened_rates(k_p_al28__n_si28)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jsi28, jal28, scratch)

    scratch = (&
      screened_rates(k_n_si27__si28)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jsi27, scratch)

    scratch = (&
      -screened_rates(k_he4_si28__s32)*Y(jhe4)*state % rho - screened_rates(k_n_si28__si29)* &
      Y(jn)*state % rho - screened_rates(k_p_si28__p29)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jsi28, scratch)

    scratch = (&
      screened_rates(k_p_p31__he4_si28)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi28, jp31, scratch)

    scratch = (&
      screened_rates(k_n_s31__he4_si28)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi28, js31, scratch)

    scratch = (&
      screened_rates(k_n_p29__p_si29)*Y(jp29)*state % rho + screened_rates(k_n_s32__he4_si29)* &
      Y(js32)*state % rho + screened_rates(k_n_si28__si29)*Y(jsi28)*state % rho - &
      screened_rates(k_n_si29__si30)*Y(jsi29)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jn, scratch)

    scratch = (&
      screened_rates(k_p_al28__si29)*Y(jal28)*state % rho + screened_rates(k_p_al29__n_si29)* &
      Y(jal29)*state % rho + screened_rates(k_p_p32__he4_si29)*Y(jp32)*state % rho &
      - screened_rates(k_p_si29__p30)*Y(jsi29)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al26__p_si29)*Y(jal26)*state % rho + screened_rates(k_he4_mg25__si29)* &
      Y(jmg25)*state % rho + screened_rates(k_he4_mg26__n_si29)*Y(jmg26)* &
      state % rho - screened_rates(k_he4_si29__s33)*Y(jsi29)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mg25__si29)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jmg25, scratch)

    scratch = (&
      screened_rates(k_he4_mg26__n_si29)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jmg26, scratch)

    scratch = (&
      screened_rates(k_he4_al26__p_si29)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jal26, scratch)

    scratch = (&
      screened_rates(k_p_al28__si29)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jal28, scratch)

    scratch = (&
      screened_rates(k_al29__si29__weak__wc12) + screened_rates(k_p_al29__n_si29)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jsi29, jal29, scratch)

    scratch = (&
      screened_rates(k_n_si28__si29)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jsi28, scratch)

    scratch = (&
      -screened_rates(k_he4_si29__s33)*Y(jhe4)*state % rho - screened_rates(k_n_si29__si30)* &
      Y(jn)*state % rho - screened_rates(k_p_si29__p30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jsi29, scratch)

    scratch = (&
      screened_rates(k_n_p29__p_si29)*Y(jn)*state % rho + screened_rates(k_p29__si29__weak__wc12) &
       )
    call set_jac_entry(jac, jsi29, jp29, scratch)

    scratch = (&
      screened_rates(k_p_p32__he4_si29)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi29, jp32, scratch)

    scratch = (&
      screened_rates(k_n_s32__he4_si29)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi29, js32, scratch)

    scratch = (&
      screened_rates(k_n_p30__p_si30)*Y(jp30)*state % rho + screened_rates(k_n_s33__he4_si30)* &
      Y(js33)*state % rho + screened_rates(k_n_si29__si30)*Y(jsi29)*state % rho - &
      screened_rates(k_n_si30__si31)*Y(jsi30)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jn, scratch)

    scratch = (&
      screened_rates(k_p_al29__si30)*Y(jal29)*state % rho + screened_rates(k_p_p33__he4_si30)* &
      Y(jp33)*state % rho - screened_rates(k_p_si30__p31)*Y(jsi30)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al27__p_si30)*Y(jal27)*state % rho + screened_rates(k_he4_mg26__si30)* &
      Y(jmg26)*state % rho + screened_rates(k_he4_mg27__n_si30)*Y(jmg27)* &
      state % rho - screened_rates(k_he4_si30__s34)*Y(jsi30)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mg26__si30)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jmg26, scratch)

    scratch = (&
      screened_rates(k_he4_mg27__n_si30)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jmg27, scratch)

    scratch = (&
      screened_rates(k_he4_al27__p_si30)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jal27, scratch)

    scratch = (&
      screened_rates(k_p_al29__si30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jal29, scratch)

    scratch = (&
      screened_rates(k_n_si29__si30)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jsi29, scratch)

    scratch = (&
      -screened_rates(k_he4_si30__s34)*Y(jhe4)*state % rho - screened_rates(k_n_si30__si31)* &
      Y(jn)*state % rho - screened_rates(k_p_si30__p31)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jsi30, scratch)

    scratch = (&
      screened_rates(k_n_p30__p_si30)*Y(jn)*state % rho + screened_rates(k_p30__si30__weak__wc12) &
       )
    call set_jac_entry(jac, jsi30, jp30, scratch)

    scratch = (&
      screened_rates(k_p_p33__he4_si30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsi30, jp33, scratch)

    scratch = (&
      screened_rates(k_n_s33__he4_si30)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi30, js33, scratch)

    scratch = (&
      screened_rates(k_n_si30__si31)*Y(jsi30)*state % rho - screened_rates(k_n_si31__si32)* &
      Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, jsi31, jn, scratch)

    scratch = (&
      -screened_rates(k_p_si31__n_p31)*Y(jsi31)*state % rho - screened_rates(k_p_si31__p32)* &
      Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, jsi31, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al28__p_si31)*Y(jal28)*state % rho + screened_rates(k_he4_mg27__si31)* &
      Y(jmg27)*state % rho - screened_rates(k_he4_si31__n_s34)*Y(jsi31)* &
      state % rho - screened_rates(k_he4_si31__s35)*Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, jsi31, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mg27__si31)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi31, jmg27, scratch)

    scratch = (&
      screened_rates(k_he4_al28__p_si31)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi31, jal28, scratch)

    scratch = (&
      screened_rates(k_n_si30__si31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi31, jsi30, scratch)

    scratch = (&
      -screened_rates(k_he4_si31__n_s34)*Y(jhe4)*state % rho - screened_rates(k_he4_si31__s35)* &
      Y(jhe4)*state % rho - screened_rates(k_n_si31__si32)*Y(jn)*state % rho - &
      screened_rates(k_p_si31__n_p31)*Y(jp)*state % rho - screened_rates(k_p_si31__p32)* &
      Y(jp)*state % rho - screened_rates(k_si31__p31__weak__wc12) &
       )
    call set_jac_entry(jac, jsi31, jsi31, scratch)

    scratch = (&
      screened_rates(k_n_p32__p_si32)*Y(jp32)*state % rho + screened_rates(k_n_s35__he4_si32)* &
      Y(js35)*state % rho + screened_rates(k_n_si31__si32)*Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, jsi32, jn, scratch)

    scratch = (&
      -screened_rates(k_p_si32__p33)*Y(jsi32)*state % rho &
       )
    call set_jac_entry(jac, jsi32, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al29__p_si32)*Y(jal29)*state % rho &
       )
    call set_jac_entry(jac, jsi32, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_al29__p_si32)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsi32, jal29, scratch)

    scratch = (&
      screened_rates(k_n_si31__si32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi32, jsi31, scratch)

    scratch = (&
      -screened_rates(k_p_si32__p33)*Y(jp)*state % rho - screened_rates(k_si32__p32__weak__wc12) &
       )
    call set_jac_entry(jac, jsi32, jsi32, scratch)

    scratch = (&
      screened_rates(k_n_p32__p_si32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi32, jp32, scratch)

    scratch = (&
      screened_rates(k_n_s35__he4_si32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsi32, js35, scratch)

    scratch = (&
      -screened_rates(k_n_p29__he4_al26)*Y(jp29)*state % rho - screened_rates(k_n_p29__p30)* &
      Y(jp29)*state % rho - screened_rates(k_n_p29__p_si29)*Y(jp29)*state % rho &
       )
    call set_jac_entry(jac, jp29, jn, scratch)

    scratch = (&
      -screened_rates(k_p_p29__s30)*Y(jp29)*state % rho + screened_rates(k_p_si28__p29)* &
      Y(jsi28)*state % rho &
       )
    call set_jac_entry(jac, jp29, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al25__p29)*Y(jal25)*state % rho - screened_rates(k_he4_p29__cl33)* &
      Y(jp29)*state % rho - screened_rates(k_he4_p29__p_s32)*Y(jp29)*state % rho &
       )
    call set_jac_entry(jac, jp29, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_al25__p29)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp29, jal25, scratch)

    scratch = (&
      screened_rates(k_p_si28__p29)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp29, jsi28, scratch)

    scratch = (&
      -screened_rates(k_he4_p29__cl33)*Y(jhe4)*state % rho - screened_rates(k_he4_p29__p_s32)* &
      Y(jhe4)*state % rho - screened_rates(k_n_p29__he4_al26)*Y(jn)*state % rho - &
      screened_rates(k_n_p29__p30)*Y(jn)*state % rho - screened_rates(k_n_p29__p_si29)* &
      Y(jn)*state % rho - screened_rates(k_p29__si29__weak__wc12) - &
      screened_rates(k_p_p29__s30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp29, jp29, scratch)

    scratch = (&
      screened_rates(k_n_cl33__he4_p30)*Y(jcl33)*state % rho + screened_rates(k_n_p29__p30)* &
      Y(jp29)*state % rho - screened_rates(k_n_p30__p31)*Y(jp30)*state % rho - &
      screened_rates(k_n_p30__p_si30)*Y(jp30)*state % rho + screened_rates(k_n_s30__p_p30) &
      *Y(js30)*state % rho &
       )
    call set_jac_entry(jac, jp30, jn, scratch)

    scratch = (&
      -screened_rates(k_p_p30__s31)*Y(jp30)*state % rho + screened_rates(k_p_si29__p30)* &
      Y(jsi29)*state % rho &
       )
    call set_jac_entry(jac, jp30, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al26__p30)*Y(jal26)*state % rho + screened_rates(k_he4_al27__n_p30)* &
      Y(jal27)*state % rho - screened_rates(k_he4_p30__cl34)*Y(jp30)*state % rho - &
      screened_rates(k_he4_p30__p_s33)*Y(jp30)*state % rho + &
      screened_rates(k_he4_si27__p_p30)*Y(jsi27)*state % rho &
       )
    call set_jac_entry(jac, jp30, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_al26__p30)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp30, jal26, scratch)

    scratch = (&
      screened_rates(k_he4_al27__n_p30)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp30, jal27, scratch)

    scratch = (&
      screened_rates(k_he4_si27__p_p30)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp30, jsi27, scratch)

    scratch = (&
      screened_rates(k_p_si29__p30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp30, jsi29, scratch)

    scratch = (&
      screened_rates(k_n_p29__p30)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp30, jp29, scratch)

    scratch = (&
      -screened_rates(k_he4_p30__cl34)*Y(jhe4)*state % rho - screened_rates(k_he4_p30__p_s33)* &
      Y(jhe4)*state % rho - screened_rates(k_n_p30__p31)*Y(jn)*state % rho - &
      screened_rates(k_n_p30__p_si30)*Y(jn)*state % rho - &
      screened_rates(k_p30__si30__weak__wc12) - screened_rates(k_p_p30__s31)*Y(jp)* &
      state % rho &
       )
    call set_jac_entry(jac, jp30, jp30, scratch)

    scratch = (&
      screened_rates(k_n_s30__p_p30)*Y(jn)*state % rho + screened_rates(k_s30__p30__weak__wc12) &
       )
    call set_jac_entry(jac, jp30, js30, scratch)

    scratch = (&
      screened_rates(k_n_cl33__he4_p30)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp30, jcl33, scratch)

    scratch = (&
      screened_rates(k_n_cl34__he4_p31)*Y(jcl34)*state % rho + screened_rates(k_n_p30__p31)* &
      Y(jp30)*state % rho - screened_rates(k_n_p31__p32)*Y(jp31)*state % rho + &
      screened_rates(k_n_s31__p_p31)*Y(js31)*state % rho &
       )
    call set_jac_entry(jac, jp31, jn, scratch)

    scratch = (&
      -screened_rates(k_p_p31__he4_si28)*Y(jp31)*state % rho - screened_rates(k_p_p31__s32)* &
      Y(jp31)*state % rho + screened_rates(k_p_si30__p31)*Y(jsi30)*state % rho + &
      screened_rates(k_p_si31__n_p31)*Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, jp31, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al27__p31)*Y(jal27)*state % rho + screened_rates(k_he4_al28__n_p31)* &
      Y(jal28)*state % rho - screened_rates(k_he4_p31__cl35)*Y(jp31)*state % rho - &
      screened_rates(k_he4_p31__p_s34)*Y(jp31)*state % rho &
       )
    call set_jac_entry(jac, jp31, jhe4, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__p_p31)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, jp31, jc12, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_o16_o16__p_p31)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, jp31, jo16, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__p_p31)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, jp31, jne20, scratch)

    scratch = (&
      screened_rates(k_he4_al27__p31)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp31, jal27, scratch)

    scratch = (&
      screened_rates(k_he4_al28__n_p31)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp31, jal28, scratch)

    scratch = (&
      screened_rates(k_p_si30__p31)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp31, jsi30, scratch)

    scratch = (&
      screened_rates(k_p_si31__n_p31)*Y(jp)*state % rho + screened_rates(k_si31__p31__weak__wc12) &
       )
    call set_jac_entry(jac, jp31, jsi31, scratch)

    scratch = (&
      screened_rates(k_n_p30__p31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp31, jp30, scratch)

    scratch = (&
      -screened_rates(k_he4_p31__cl35)*Y(jhe4)*state % rho - screened_rates(k_he4_p31__p_s34)* &
      Y(jhe4)*state % rho - screened_rates(k_n_p31__p32)*Y(jn)*state % rho - &
      screened_rates(k_p_p31__he4_si28)*Y(jp)*state % rho - screened_rates(k_p_p31__s32)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp31, jp31, scratch)

    scratch = (&
      screened_rates(k_n_s31__p_p31)*Y(jn)*state % rho + screened_rates(k_s31__p31__weak__wc12) &
       )
    call set_jac_entry(jac, jp31, js31, scratch)

    scratch = (&
      screened_rates(k_n_cl34__he4_p31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp31, jcl34, scratch)

    scratch = (&
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*state % rho + screened_rates(k_n_p31__p32)* &
      Y(jp31)*state % rho - screened_rates(k_n_p32__p33)*Y(jp32)*state % rho - &
      screened_rates(k_n_p32__p_si32)*Y(jp32)*state % rho &
       )
    call set_jac_entry(jac, jp32, jn, scratch)

    scratch = (&
      -screened_rates(k_p_p32__he4_si29)*Y(jp32)*state % rho - screened_rates(k_p_p32__n_s32)* &
      Y(jp32)*state % rho - screened_rates(k_p_p32__s33)*Y(jp32)*state % rho + &
      screened_rates(k_p_s35__he4_p32)*Y(js35)*state % rho + screened_rates(k_p_si31__p32) &
      *Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, jp32, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al28__p32)*Y(jal28)*state % rho + screened_rates(k_he4_al29__n_p32)* &
      Y(jal29)*state % rho - screened_rates(k_he4_p32__cl36)*Y(jp32)*state % rho &
       )
    call set_jac_entry(jac, jp32, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_al28__p32)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp32, jal28, scratch)

    scratch = (&
      screened_rates(k_he4_al29__n_p32)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp32, jal29, scratch)

    scratch = (&
      screened_rates(k_p_si31__p32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp32, jsi31, scratch)

    scratch = (&
      screened_rates(k_si32__p32__weak__wc12) &
       )
    call set_jac_entry(jac, jp32, jsi32, scratch)

    scratch = (&
      screened_rates(k_n_p31__p32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp32, jp31, scratch)

    scratch = (&
      -screened_rates(k_he4_p32__cl36)*Y(jhe4)*state % rho - screened_rates(k_n_p32__p33)*Y(jn) &
      *state % rho - screened_rates(k_n_p32__p_si32)*Y(jn)*state % rho - &
      screened_rates(k_p32__s32__weak__wc12) - screened_rates(k_p_p32__he4_si29)*Y(jp)* &
      state % rho - screened_rates(k_p_p32__n_s32)*Y(jp)*state % rho - &
      screened_rates(k_p_p32__s33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp32, jp32, scratch)

    scratch = (&
      screened_rates(k_p_s35__he4_p32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp32, js35, scratch)

    scratch = (&
      screened_rates(k_n_cl35__he4_p32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp32, jcl35, scratch)

    scratch = (&
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*state % rho + screened_rates(k_n_p32__p33)* &
      Y(jp32)*state % rho + screened_rates(k_n_s33__p_p33)*Y(js33)*state % rho &
       )
    call set_jac_entry(jac, jp33, jn, scratch)

    scratch = (&
      -screened_rates(k_p_p33__he4_si30)*Y(jp33)*state % rho - screened_rates(k_p_p33__s34)* &
      Y(jp33)*state % rho + screened_rates(k_p_si32__p33)*Y(jsi32)*state % rho &
       )
    call set_jac_entry(jac, jp33, jp, scratch)

    scratch = (&
      screened_rates(k_he4_al29__p33)*Y(jal29)*state % rho - screened_rates(k_he4_p33__cl37)* &
      Y(jp33)*state % rho &
       )
    call set_jac_entry(jac, jp33, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_al29__p33)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jp33, jal29, scratch)

    scratch = (&
      screened_rates(k_p_si32__p33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp33, jsi32, scratch)

    scratch = (&
      screened_rates(k_n_p32__p33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp33, jp32, scratch)

    scratch = (&
      -screened_rates(k_he4_p33__cl37)*Y(jhe4)*state % rho - &
      screened_rates(k_p33__s33__weak__wc12) - screened_rates(k_p_p33__he4_si30)*Y(jp)* &
      state % rho - screened_rates(k_p_p33__s34)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jp33, jp33, scratch)

    scratch = (&
      screened_rates(k_n_s33__p_p33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp33, js33, scratch)

    scratch = (&
      screened_rates(k_n_cl36__he4_p33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jp33, jcl36, scratch)

    scratch = (&
      -screened_rates(k_n_s30__he4_si27)*Y(js30)*state % rho - screened_rates(k_n_s30__p_p30)* &
      Y(js30)*state % rho - screened_rates(k_n_s30__s31)*Y(js30)*state % rho &
       )
    call set_jac_entry(jac, js30, jn, scratch)

    scratch = (&
      screened_rates(k_p_p29__s30)*Y(jp29)*state % rho &
       )
    call set_jac_entry(jac, js30, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_s30__p_cl33)*Y(js30)*state % rho &
       )
    call set_jac_entry(jac, js30, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_p29__s30)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js30, jp29, scratch)

    scratch = (&
      -screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*state % rho - screened_rates(k_n_s30__he4_si27)* &
      Y(jn)*state % rho - screened_rates(k_n_s30__p_p30)*Y(jn)*state % rho - &
      screened_rates(k_n_s30__s31)*Y(jn)*state % rho - &
      screened_rates(k_s30__p30__weak__wc12) &
       )
    call set_jac_entry(jac, js30, js30, scratch)

    scratch = (&
      screened_rates(k_n_s30__s31)*Y(js30)*state % rho - screened_rates(k_n_s31__he4_si28)* &
      Y(js31)*state % rho - screened_rates(k_n_s31__p_p31)*Y(js31)*state % rho - &
      screened_rates(k_n_s31__s32)*Y(js31)*state % rho &
       )
    call set_jac_entry(jac, js31, jn, scratch)

    scratch = (&
      screened_rates(k_p_p30__s31)*Y(jp30)*state % rho &
       )
    call set_jac_entry(jac, js31, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_s31__ar35)*Y(js31)*state % rho - screened_rates(k_he4_s31__p_cl34)* &
      Y(js31)*state % rho + screened_rates(k_he4_si27__s31)*Y(jsi27)*state % rho &
       )
    call set_jac_entry(jac, js31, jhe4, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__n_s31)*Y(jne20)*state % rho &
       )
    call set_jac_entry(jac, js31, jc12, scratch)

    scratch = (&
      1.0e0_rt*screened_rates(k_o16_o16__n_s31)*Y(jo16)*state % rho &
       )
    call set_jac_entry(jac, js31, jo16, scratch)

    scratch = (&
      screened_rates(k_c12_ne20__n_s31)*Y(jc12)*state % rho &
       )
    call set_jac_entry(jac, js31, jne20, scratch)

    scratch = (&
      screened_rates(k_he4_si27__s31)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js31, jsi27, scratch)

    scratch = (&
      screened_rates(k_p_p30__s31)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js31, jp30, scratch)

    scratch = (&
      screened_rates(k_n_s30__s31)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js31, js30, scratch)

    scratch = (&
      -screened_rates(k_he4_s31__ar35)*Y(jhe4)*state % rho - screened_rates(k_he4_s31__p_cl34)* &
      Y(jhe4)*state % rho - screened_rates(k_n_s31__he4_si28)*Y(jn)*state % rho - &
      screened_rates(k_n_s31__p_p31)*Y(jn)*state % rho - screened_rates(k_n_s31__s32)* &
      Y(jn)*state % rho - screened_rates(k_s31__p31__weak__wc12) &
       )
    call set_jac_entry(jac, js31, js31, scratch)

    scratch = (&
      screened_rates(k_n_ar35__he4_s32)*Y(jar35)*state % rho + screened_rates(k_n_s31__s32)* &
      Y(js31)*state % rho - screened_rates(k_n_s32__he4_si29)*Y(js32)*state % rho &
      - screened_rates(k_n_s32__s33)*Y(js32)*state % rho &
       )
    call set_jac_entry(jac, js32, jn, scratch)

    scratch = (&
      screened_rates(k_p_cl35__he4_s32)*Y(jcl35)*state % rho + screened_rates(k_p_p31__s32)* &
      Y(jp31)*state % rho + screened_rates(k_p_p32__n_s32)*Y(jp32)*state % rho - &
      screened_rates(k_p_s32__cl33)*Y(js32)*state % rho &
       )
    call set_jac_entry(jac, js32, jp, scratch)

    scratch = (&
      screened_rates(k_he4_p29__p_s32)*Y(jp29)*state % rho - screened_rates(k_he4_s32__ar36)* &
      Y(js32)*state % rho + screened_rates(k_he4_si28__s32)*Y(jsi28)*state % rho &
       )
    call set_jac_entry(jac, js32, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_si28__s32)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js32, jsi28, scratch)

    scratch = (&
      screened_rates(k_he4_p29__p_s32)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js32, jp29, scratch)

    scratch = (&
      screened_rates(k_p_p31__s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js32, jp31, scratch)

    scratch = (&
      screened_rates(k_p32__s32__weak__wc12) + screened_rates(k_p_p32__n_s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js32, jp32, scratch)

    scratch = (&
      screened_rates(k_n_s31__s32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js32, js31, scratch)

    scratch = (&
      -screened_rates(k_he4_s32__ar36)*Y(jhe4)*state % rho - screened_rates(k_n_s32__he4_si29)* &
      Y(jn)*state % rho - screened_rates(k_n_s32__s33)*Y(jn)*state % rho - &
      screened_rates(k_p_s32__cl33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js32, js32, scratch)

    scratch = (&
      screened_rates(k_p_cl35__he4_s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js32, jcl35, scratch)

    scratch = (&
      screened_rates(k_n_ar35__he4_s32)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js32, jar35, scratch)

    scratch = (&
      screened_rates(k_n_ar36__he4_s33)*Y(jar36)*state % rho + screened_rates(k_n_cl33__p_s33)* &
      Y(jcl33)*state % rho + screened_rates(k_n_s32__s33)*Y(js32)*state % rho - &
      screened_rates(k_n_s33__he4_si30)*Y(js33)*state % rho - &
      screened_rates(k_n_s33__p_p33)*Y(js33)*state % rho - screened_rates(k_n_s33__s34)* &
      Y(js33)*state % rho &
       )
    call set_jac_entry(jac, js33, jn, scratch)

    scratch = (&
      screened_rates(k_p_cl36__he4_s33)*Y(jcl36)*state % rho + screened_rates(k_p_p32__s33)* &
      Y(jp32)*state % rho - screened_rates(k_p_s33__cl34)*Y(js33)*state % rho &
       )
    call set_jac_entry(jac, js33, jp, scratch)

    scratch = (&
      screened_rates(k_he4_p30__p_s33)*Y(jp30)*state % rho - screened_rates(k_he4_s33__ar37)* &
      Y(js33)*state % rho + screened_rates(k_he4_si29__s33)*Y(jsi29)*state % rho &
       )
    call set_jac_entry(jac, js33, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_si29__s33)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js33, jsi29, scratch)

    scratch = (&
      screened_rates(k_he4_p30__p_s33)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js33, jp30, scratch)

    scratch = (&
      screened_rates(k_p_p32__s33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js33, jp32, scratch)

    scratch = (&
      screened_rates(k_p33__s33__weak__wc12) &
       )
    call set_jac_entry(jac, js33, jp33, scratch)

    scratch = (&
      screened_rates(k_n_s32__s33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js33, js32, scratch)

    scratch = (&
      -screened_rates(k_he4_s33__ar37)*Y(jhe4)*state % rho - screened_rates(k_n_s33__he4_si30)* &
      Y(jn)*state % rho - screened_rates(k_n_s33__p_p33)*Y(jn)*state % rho - &
      screened_rates(k_n_s33__s34)*Y(jn)*state % rho - screened_rates(k_p_s33__cl34)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js33, js33, scratch)

    scratch = (&
      screened_rates(k_cl33__s33__weak__wc12) + screened_rates(k_n_cl33__p_s33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js33, jcl33, scratch)

    scratch = (&
      screened_rates(k_p_cl36__he4_s33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js33, jcl36, scratch)

    scratch = (&
      screened_rates(k_n_ar36__he4_s33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js33, jar36, scratch)

    scratch = (&
      screened_rates(k_n_cl34__p_s34)*Y(jcl34)*state % rho + screened_rates(k_n_s33__s34)* &
      Y(js33)*state % rho - screened_rates(k_n_s34__s35)*Y(js34)*state % rho &
       )
    call set_jac_entry(jac, js34, jn, scratch)

    scratch = (&
      screened_rates(k_p_p33__s34)*Y(jp33)*state % rho - screened_rates(k_p_s34__cl35)*Y(js34)* &
      state % rho &
       )
    call set_jac_entry(jac, js34, jp, scratch)

    scratch = (&
      screened_rates(k_he4_p31__p_s34)*Y(jp31)*state % rho - screened_rates(k_he4_s34__ar38)* &
      Y(js34)*state % rho - screened_rates(k_he4_s34__n_ar37)*Y(js34)*state % rho &
      - screened_rates(k_he4_s34__p_cl37)*Y(js34)*state % rho + &
      screened_rates(k_he4_si30__s34)*Y(jsi30)*state % rho + &
      screened_rates(k_he4_si31__n_s34)*Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, js34, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_si30__s34)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js34, jsi30, scratch)

    scratch = (&
      screened_rates(k_he4_si31__n_s34)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js34, jsi31, scratch)

    scratch = (&
      screened_rates(k_he4_p31__p_s34)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js34, jp31, scratch)

    scratch = (&
      screened_rates(k_p_p33__s34)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js34, jp33, scratch)

    scratch = (&
      screened_rates(k_n_s33__s34)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js34, js33, scratch)

    scratch = (&
      -screened_rates(k_he4_s34__ar38)*Y(jhe4)*state % rho - screened_rates(k_he4_s34__n_ar37)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*state % rho &
      - screened_rates(k_n_s34__s35)*Y(jn)*state % rho - screened_rates(k_p_s34__cl35)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, js34, js34, scratch)

    scratch = (&
      screened_rates(k_cl34__s34__weak__wc12) + screened_rates(k_n_cl34__p_s34)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js34, jcl34, scratch)

    scratch = (&
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*state % rho + screened_rates(k_n_s34__s35)* &
      Y(js34)*state % rho - screened_rates(k_n_s35__he4_si32)*Y(js35)*state % rho &
       )
    call set_jac_entry(jac, js35, jn, scratch)

    scratch = (&
      -screened_rates(k_p_s35__cl36)*Y(js35)*state % rho - screened_rates(k_p_s35__he4_p32)* &
      Y(js35)*state % rho &
       )
    call set_jac_entry(jac, js35, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_s35__ar39)*Y(js35)*state % rho - screened_rates(k_he4_s35__n_ar38)* &
      Y(js35)*state % rho + screened_rates(k_he4_si31__s35)*Y(jsi31)*state % rho &
       )
    call set_jac_entry(jac, js35, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_si31__s35)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, js35, jsi31, scratch)

    scratch = (&
      screened_rates(k_n_s34__s35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js35, js34, scratch)

    scratch = (&
      -screened_rates(k_he4_s35__ar39)*Y(jhe4)*state % rho - screened_rates(k_he4_s35__n_ar38)* &
      Y(jhe4)*state % rho - screened_rates(k_n_s35__he4_si32)*Y(jn)*state % rho - &
      screened_rates(k_p_s35__cl36)*Y(jp)*state % rho - screened_rates(k_p_s35__he4_p32)* &
      Y(jp)*state % rho - screened_rates(k_s35__cl35__weak__wc12) &
       )
    call set_jac_entry(jac, js35, js35, scratch)

    scratch = (&
      screened_rates(k_n_cl35__p_s35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, js35, jcl35, scratch)

    scratch = (&
      -screened_rates(k_n_cl33__cl34)*Y(jcl33)*state % rho - screened_rates(k_n_cl33__he4_p30)* &
      Y(jcl33)*state % rho - screened_rates(k_n_cl33__p_s33)*Y(jcl33)*state % rho &
       )
    call set_jac_entry(jac, jcl33, jn, scratch)

    scratch = (&
      screened_rates(k_p_s32__cl33)*Y(js32)*state % rho &
       )
    call set_jac_entry(jac, jcl33, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cl33__k37)*Y(jcl33)*state % rho - screened_rates(k_he4_cl33__p_ar36)* &
      Y(jcl33)*state % rho + screened_rates(k_he4_p29__cl33)*Y(jp29)*state % rho + &
      screened_rates(k_he4_s30__p_cl33)*Y(js30)*state % rho &
       )
    call set_jac_entry(jac, jcl33, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_p29__cl33)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl33, jp29, scratch)

    scratch = (&
      screened_rates(k_he4_s30__p_cl33)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl33, js30, scratch)

    scratch = (&
      screened_rates(k_p_s32__cl33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl33, js32, scratch)

    scratch = (&
      -screened_rates(k_cl33__s33__weak__wc12) - screened_rates(k_he4_cl33__k37)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_cl33__p_ar36)*Y(jhe4)*state % rho - &
      screened_rates(k_n_cl33__cl34)*Y(jn)*state % rho - screened_rates(k_n_cl33__he4_p30) &
      *Y(jn)*state % rho - screened_rates(k_n_cl33__p_s33)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl33, jcl33, scratch)

    scratch = (&
      screened_rates(k_n_cl33__cl34)*Y(jcl33)*state % rho - screened_rates(k_n_cl34__cl35)* &
      Y(jcl34)*state % rho - screened_rates(k_n_cl34__he4_p31)*Y(jcl34)* &
      state % rho - screened_rates(k_n_cl34__p_s34)*Y(jcl34)*state % rho + &
      screened_rates(k_n_k37__he4_cl34)*Y(jk37)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cl34__ar35)*Y(jcl34)*state % rho + screened_rates(k_p_s33__cl34)* &
      Y(js33)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cl34__k38)*Y(jcl34)*state % rho - screened_rates(k_he4_cl34__p_ar37)* &
      Y(jcl34)*state % rho + screened_rates(k_he4_p30__cl34)*Y(jp30)*state % rho + &
      screened_rates(k_he4_s31__p_cl34)*Y(js31)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_p30__cl34)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jp30, scratch)

    scratch = (&
      screened_rates(k_he4_s31__p_cl34)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl34, js31, scratch)

    scratch = (&
      screened_rates(k_p_s33__cl34)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl34, js33, scratch)

    scratch = (&
      screened_rates(k_n_cl33__cl34)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jcl33, scratch)

    scratch = (&
      -screened_rates(k_cl34__s34__weak__wc12) - screened_rates(k_he4_cl34__k38)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_cl34__p_ar37)*Y(jhe4)*state % rho - &
      screened_rates(k_n_cl34__cl35)*Y(jn)*state % rho - screened_rates(k_n_cl34__he4_p31) &
      *Y(jn)*state % rho - screened_rates(k_n_cl34__p_s34)*Y(jn)*state % rho - &
      screened_rates(k_p_cl34__ar35)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jcl34, scratch)

    scratch = (&
      screened_rates(k_n_k37__he4_cl34)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl34, jk37, scratch)

    scratch = (&
      screened_rates(k_n_ar35__p_cl35)*Y(jar35)*state % rho + screened_rates(k_n_cl34__cl35)* &
      Y(jcl34)*state % rho - screened_rates(k_n_cl35__cl36)*Y(jcl35)*state % rho - &
      screened_rates(k_n_cl35__he4_p32)*Y(jcl35)*state % rho - &
      screened_rates(k_n_cl35__p_s35)*Y(jcl35)*state % rho + &
      screened_rates(k_n_k38__he4_cl35)*Y(jk38)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cl35__ar36)*Y(jcl35)*state % rho - screened_rates(k_p_cl35__he4_s32)* &
      Y(jcl35)*state % rho + screened_rates(k_p_s34__cl35)*Y(js34)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cl35__k39)*Y(jcl35)*state % rho - screened_rates(k_he4_cl35__p_ar38)* &
      Y(jcl35)*state % rho + screened_rates(k_he4_p31__cl35)*Y(jp31)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_p31__cl35)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jp31, scratch)

    scratch = (&
      screened_rates(k_p_s34__cl35)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl35, js34, scratch)

    scratch = (&
      screened_rates(k_s35__cl35__weak__wc12) &
       )
    call set_jac_entry(jac, jcl35, js35, scratch)

    scratch = (&
      screened_rates(k_n_cl34__cl35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jcl34, scratch)

    scratch = (&
      -screened_rates(k_he4_cl35__k39)*Y(jhe4)*state % rho - screened_rates(k_he4_cl35__p_ar38)* &
      Y(jhe4)*state % rho - screened_rates(k_n_cl35__cl36)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__he4_p32)*Y(jn)*state % rho - &
      screened_rates(k_n_cl35__p_s35)*Y(jn)*state % rho - screened_rates(k_p_cl35__ar36)* &
      Y(jp)*state % rho - screened_rates(k_p_cl35__he4_s32)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jcl35, scratch)

    scratch = (&
      screened_rates(k_ar35__cl35__weak__wc12) + screened_rates(k_n_ar35__p_cl35)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jcl35, jar35, scratch)

    scratch = (&
      screened_rates(k_n_k38__he4_cl35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl35, jk38, scratch)

    scratch = (&
      screened_rates(k_n_ar36__p_cl36)*Y(jar36)*state % rho + screened_rates(k_n_cl35__cl36)* &
      Y(jcl35)*state % rho - screened_rates(k_n_cl36__cl37)*Y(jcl36)*state % rho - &
      screened_rates(k_n_cl36__he4_p33)*Y(jcl36)*state % rho + &
      screened_rates(k_n_k39__he4_cl36)*Y(jk39)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jn, scratch)

    scratch = (&
      screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*state % rho - screened_rates(k_p_cl36__ar37)* &
      Y(jcl36)*state % rho - screened_rates(k_p_cl36__he4_s33)*Y(jcl36)* &
      state % rho + screened_rates(k_p_s35__cl36)*Y(js35)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cl36__k40)*Y(jcl36)*state % rho + screened_rates(k_he4_p32__cl36)* &
      Y(jp32)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_p32__cl36)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jp32, scratch)

    scratch = (&
      screened_rates(k_p_s35__cl36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl36, js35, scratch)

    scratch = (&
      screened_rates(k_n_cl35__cl36)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jcl35, scratch)

    scratch = (&
      -screened_rates(k_cl36__ar36__weak__wc12) - screened_rates(k_he4_cl36__k40)*Y(jhe4)* &
      state % rho - screened_rates(k_n_cl36__cl37)*Y(jn)*state % rho - &
      screened_rates(k_n_cl36__he4_p33)*Y(jn)*state % rho - screened_rates(k_p_cl36__ar37) &
      *Y(jp)*state % rho - screened_rates(k_p_cl36__he4_s33)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jcl36, scratch)

    scratch = (&
      screened_rates(k_n_ar36__p_cl36)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jar36, scratch)

    scratch = (&
      screened_rates(k_p_ar39__he4_cl36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jar39, scratch)

    scratch = (&
      screened_rates(k_n_k39__he4_cl36)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl36, jk39, scratch)

    scratch = (&
      screened_rates(k_n_ar37__p_cl37)*Y(jar37)*state % rho + screened_rates(k_n_cl36__cl37)* &
      Y(jcl36)*state % rho + screened_rates(k_n_k40__he4_cl37)*Y(jk40)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cl37__ar38)*Y(jcl37)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cl37__k41)*Y(jcl37)*state % rho + screened_rates(k_he4_p33__cl37)* &
      Y(jp33)*state % rho + screened_rates(k_he4_s34__p_cl37)*Y(js34)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_p33__cl37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jp33, scratch)

    scratch = (&
      screened_rates(k_he4_s34__p_cl37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcl37, js34, scratch)

    scratch = (&
      screened_rates(k_n_cl36__cl37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jcl36, scratch)

    scratch = (&
      -screened_rates(k_he4_cl37__k41)*Y(jhe4)*state % rho - screened_rates(k_p_cl37__ar38)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jcl37, scratch)

    scratch = (&
      screened_rates(k_ar37__cl37__weak__wc12) + screened_rates(k_n_ar37__p_cl37)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jcl37, jar37, scratch)

    scratch = (&
      screened_rates(k_n_k40__he4_cl37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcl37, jk40, scratch)

    scratch = (&
      -screened_rates(k_n_ar35__ar36)*Y(jar35)*state % rho - screened_rates(k_n_ar35__he4_s32)* &
      Y(jar35)*state % rho - screened_rates(k_n_ar35__p_cl35)*Y(jar35)*state % rho &
       )
    call set_jac_entry(jac, jar35, jn, scratch)

    scratch = (&
      screened_rates(k_p_cl34__ar35)*Y(jcl34)*state % rho &
       )
    call set_jac_entry(jac, jar35, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ar35__ca39)*Y(jar35)*state % rho - screened_rates(k_he4_ar35__p_k38)* &
      Y(jar35)*state % rho + screened_rates(k_he4_s31__ar35)*Y(js31)*state % rho &
       )
    call set_jac_entry(jac, jar35, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_s31__ar35)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar35, js31, scratch)

    scratch = (&
      screened_rates(k_p_cl34__ar35)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar35, jcl34, scratch)

    scratch = (&
      -screened_rates(k_ar35__cl35__weak__wc12) - screened_rates(k_he4_ar35__ca39)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_ar35__p_k38)*Y(jhe4)*state % rho - &
      screened_rates(k_n_ar35__ar36)*Y(jn)*state % rho - screened_rates(k_n_ar35__he4_s32) &
      *Y(jn)*state % rho - screened_rates(k_n_ar35__p_cl35)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar35, jar35, scratch)

    scratch = (&
      screened_rates(k_n_ar35__ar36)*Y(jar35)*state % rho - screened_rates(k_n_ar36__ar37)* &
      Y(jar36)*state % rho - screened_rates(k_n_ar36__he4_s33)*Y(jar36)* &
      state % rho - screened_rates(k_n_ar36__p_cl36)*Y(jar36)*state % rho + &
      screened_rates(k_n_ca39__he4_ar36)*Y(jca39)*state % rho &
       )
    call set_jac_entry(jac, jar36, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ar36__k37)*Y(jar36)*state % rho + screened_rates(k_p_cl35__ar36)* &
      Y(jcl35)*state % rho + screened_rates(k_p_k39__he4_ar36)*Y(jk39)*state % rho &
       )
    call set_jac_entry(jac, jar36, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ar36__ca40)*Y(jar36)*state % rho + screened_rates(k_he4_cl33__p_ar36) &
      *Y(jcl33)*state % rho + screened_rates(k_he4_s32__ar36)*Y(js32)*state % rho &
       )
    call set_jac_entry(jac, jar36, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_s32__ar36)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar36, js32, scratch)

    scratch = (&
      screened_rates(k_he4_cl33__p_ar36)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar36, jcl33, scratch)

    scratch = (&
      screened_rates(k_p_cl35__ar36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar36, jcl35, scratch)

    scratch = (&
      screened_rates(k_cl36__ar36__weak__wc12) &
       )
    call set_jac_entry(jac, jar36, jcl36, scratch)

    scratch = (&
      screened_rates(k_n_ar35__ar36)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar36, jar35, scratch)

    scratch = (&
      -screened_rates(k_he4_ar36__ca40)*Y(jhe4)*state % rho - screened_rates(k_n_ar36__ar37)* &
      Y(jn)*state % rho - screened_rates(k_n_ar36__he4_s33)*Y(jn)*state % rho - &
      screened_rates(k_n_ar36__p_cl36)*Y(jn)*state % rho - screened_rates(k_p_ar36__k37)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar36, jar36, scratch)

    scratch = (&
      screened_rates(k_p_k39__he4_ar36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar36, jk39, scratch)

    scratch = (&
      screened_rates(k_n_ca39__he4_ar36)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar36, jca39, scratch)

    scratch = (&
      screened_rates(k_n_ar36__ar37)*Y(jar36)*state % rho - screened_rates(k_n_ar37__ar38)* &
      Y(jar37)*state % rho - screened_rates(k_n_ar37__p_cl37)*Y(jar37)*state % rho &
      + screened_rates(k_n_ca40__he4_ar37)*Y(jca40)*state % rho + &
      screened_rates(k_n_k37__p_ar37)*Y(jk37)*state % rho &
       )
    call set_jac_entry(jac, jar37, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ar37__k38)*Y(jar37)*state % rho + screened_rates(k_p_cl36__ar37)* &
      Y(jcl36)*state % rho + screened_rates(k_p_k40__he4_ar37)*Y(jk40)*state % rho &
       )
    call set_jac_entry(jac, jar37, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ar37__ca41)*Y(jar37)*state % rho + screened_rates(k_he4_cl34__p_ar37) &
      *Y(jcl34)*state % rho + screened_rates(k_he4_s33__ar37)*Y(js33)*state % rho &
      + screened_rates(k_he4_s34__n_ar37)*Y(js34)*state % rho &
       )
    call set_jac_entry(jac, jar37, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_s33__ar37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar37, js33, scratch)

    scratch = (&
      screened_rates(k_he4_s34__n_ar37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar37, js34, scratch)

    scratch = (&
      screened_rates(k_he4_cl34__p_ar37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar37, jcl34, scratch)

    scratch = (&
      screened_rates(k_p_cl36__ar37)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar37, jcl36, scratch)

    scratch = (&
      screened_rates(k_n_ar36__ar37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar37, jar36, scratch)

    scratch = (&
      -screened_rates(k_ar37__cl37__weak__wc12) - screened_rates(k_he4_ar37__ca41)*Y(jhe4)* &
      state % rho - screened_rates(k_n_ar37__ar38)*Y(jn)*state % rho - &
      screened_rates(k_n_ar37__p_cl37)*Y(jn)*state % rho - screened_rates(k_p_ar37__k38)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar37, jar37, scratch)

    scratch = (&
      screened_rates(k_k37__ar37__weak__wc12) + screened_rates(k_n_k37__p_ar37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar37, jk37, scratch)

    scratch = (&
      screened_rates(k_p_k40__he4_ar37)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar37, jk40, scratch)

    scratch = (&
      screened_rates(k_n_ca40__he4_ar37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar37, jca40, scratch)

    scratch = (&
      screened_rates(k_n_ar37__ar38)*Y(jar37)*state % rho - screened_rates(k_n_ar38__ar39)* &
      Y(jar38)*state % rho + screened_rates(k_n_ca41__he4_ar38)*Y(jca41)* &
      state % rho + screened_rates(k_n_k38__p_ar38)*Y(jk38)*state % rho &
       )
    call set_jac_entry(jac, jar38, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ar38__k39)*Y(jar38)*state % rho + screened_rates(k_p_cl37__ar38)* &
      Y(jcl37)*state % rho + screened_rates(k_p_k41__he4_ar38)*Y(jk41)*state % rho &
       )
    call set_jac_entry(jac, jar38, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ar38__ca42)*Y(jar38)*state % rho + screened_rates(k_he4_cl35__p_ar38) &
      *Y(jcl35)*state % rho + screened_rates(k_he4_s34__ar38)*Y(js34)*state % rho &
      + screened_rates(k_he4_s35__n_ar38)*Y(js35)*state % rho &
       )
    call set_jac_entry(jac, jar38, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_s34__ar38)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar38, js34, scratch)

    scratch = (&
      screened_rates(k_he4_s35__n_ar38)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar38, js35, scratch)

    scratch = (&
      screened_rates(k_he4_cl35__p_ar38)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar38, jcl35, scratch)

    scratch = (&
      screened_rates(k_p_cl37__ar38)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar38, jcl37, scratch)

    scratch = (&
      screened_rates(k_n_ar37__ar38)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar38, jar37, scratch)

    scratch = (&
      -screened_rates(k_he4_ar38__ca42)*Y(jhe4)*state % rho - screened_rates(k_n_ar38__ar39)* &
      Y(jn)*state % rho - screened_rates(k_p_ar38__k39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar38, jar38, scratch)

    scratch = (&
      screened_rates(k_k38__ar38__weak__wc12) + screened_rates(k_n_k38__p_ar38)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar38, jk38, scratch)

    scratch = (&
      screened_rates(k_p_k41__he4_ar38)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar38, jk41, scratch)

    scratch = (&
      screened_rates(k_n_ca41__he4_ar38)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar38, jca41, scratch)

    scratch = (&
      screened_rates(k_n_ar38__ar39)*Y(jar38)*state % rho + screened_rates(k_n_ca42__he4_ar39)* &
      Y(jca42)*state % rho + screened_rates(k_n_k39__p_ar39)*Y(jk39)*state % rho &
       )
    call set_jac_entry(jac, jar39, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ar39__he4_cl36)*Y(jar39)*state % rho - screened_rates(k_p_ar39__k40)* &
      Y(jar39)*state % rho + screened_rates(k_p_k42__he4_ar39)*Y(jk42)*state % rho &
       )
    call set_jac_entry(jac, jar39, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ar39__ca43)*Y(jar39)*state % rho + screened_rates(k_he4_s35__ar39)* &
      Y(js35)*state % rho &
       )
    call set_jac_entry(jac, jar39, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_s35__ar39)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jar39, js35, scratch)

    scratch = (&
      screened_rates(k_n_ar38__ar39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar39, jar38, scratch)

    scratch = (&
      -screened_rates(k_ar39__k39__weak__wc12) - screened_rates(k_he4_ar39__ca43)*Y(jhe4)* &
      state % rho - screened_rates(k_p_ar39__he4_cl36)*Y(jp)*state % rho - &
      screened_rates(k_p_ar39__k40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar39, jar39, scratch)

    scratch = (&
      screened_rates(k_n_k39__p_ar39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar39, jk39, scratch)

    scratch = (&
      screened_rates(k_p_k42__he4_ar39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jar39, jk42, scratch)

    scratch = (&
      screened_rates(k_n_ca42__he4_ar39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jar39, jca42, scratch)

    scratch = (&
      -screened_rates(k_n_k37__he4_cl34)*Y(jk37)*state % rho - screened_rates(k_n_k37__k38)* &
      Y(jk37)*state % rho - screened_rates(k_n_k37__p_ar37)*Y(jk37)*state % rho &
       )
    call set_jac_entry(jac, jk37, jn, scratch)

    scratch = (&
      screened_rates(k_p_ar36__k37)*Y(jar36)*state % rho &
       )
    call set_jac_entry(jac, jk37, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cl33__k37)*Y(jcl33)*state % rho - screened_rates(k_he4_k37__p_ca40)* &
      Y(jk37)*state % rho &
       )
    call set_jac_entry(jac, jk37, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cl33__k37)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jk37, jcl33, scratch)

    scratch = (&
      screened_rates(k_p_ar36__k37)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk37, jar36, scratch)

    scratch = (&
      -screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*state % rho - &
      screened_rates(k_k37__ar37__weak__wc12) - screened_rates(k_n_k37__he4_cl34)*Y(jn) &
      *state % rho - screened_rates(k_n_k37__k38)*Y(jn)*state % rho - &
      screened_rates(k_n_k37__p_ar37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk37, jk37, scratch)

    scratch = (&
      screened_rates(k_n_k37__k38)*Y(jk37)*state % rho - screened_rates(k_n_k38__he4_cl35)* &
      Y(jk38)*state % rho - screened_rates(k_n_k38__k39)*Y(jk38)*state % rho - &
      screened_rates(k_n_k38__p_ar38)*Y(jk38)*state % rho &
       )
    call set_jac_entry(jac, jk38, jn, scratch)

    scratch = (&
      screened_rates(k_p_ar37__k38)*Y(jar37)*state % rho - screened_rates(k_p_k38__ca39)* &
      Y(jk38)*state % rho &
       )
    call set_jac_entry(jac, jk38, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ar35__p_k38)*Y(jar35)*state % rho + screened_rates(k_he4_cl34__k38)* &
      Y(jcl34)*state % rho - screened_rates(k_he4_k38__p_ca41)*Y(jk38)*state % rho &
      - screened_rates(k_he4_k38__sc42)*Y(jk38)*state % rho &
       )
    call set_jac_entry(jac, jk38, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cl34__k38)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jk38, jcl34, scratch)

    scratch = (&
      screened_rates(k_he4_ar35__p_k38)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jk38, jar35, scratch)

    scratch = (&
      screened_rates(k_p_ar37__k38)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk38, jar37, scratch)

    scratch = (&
      screened_rates(k_n_k37__k38)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk38, jk37, scratch)

    scratch = (&
      -screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*state % rho - screened_rates(k_he4_k38__sc42)* &
      Y(jhe4)*state % rho - screened_rates(k_k38__ar38__weak__wc12) - &
      screened_rates(k_n_k38__he4_cl35)*Y(jn)*state % rho - screened_rates(k_n_k38__k39)* &
      Y(jn)*state % rho - screened_rates(k_n_k38__p_ar38)*Y(jn)*state % rho - &
      screened_rates(k_p_k38__ca39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk38, jk38, scratch)

    scratch = (&
      screened_rates(k_n_ca39__p_k39)*Y(jca39)*state % rho + screened_rates(k_n_k38__k39)* &
      Y(jk38)*state % rho - screened_rates(k_n_k39__he4_cl36)*Y(jk39)*state % rho &
      - screened_rates(k_n_k39__k40)*Y(jk39)*state % rho - screened_rates(k_n_k39__p_ar39) &
      *Y(jk39)*state % rho + screened_rates(k_n_sc42__he4_k39)*Y(jsc42)* &
      state % rho &
       )
    call set_jac_entry(jac, jk39, jn, scratch)

    scratch = (&
      screened_rates(k_p_ar38__k39)*Y(jar38)*state % rho + screened_rates(k_p_ca42__he4_k39)* &
      Y(jca42)*state % rho - screened_rates(k_p_k39__ca40)*Y(jk39)*state % rho - &
      screened_rates(k_p_k39__he4_ar36)*Y(jk39)*state % rho &
       )
    call set_jac_entry(jac, jk39, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cl35__k39)*Y(jcl35)*state % rho - screened_rates(k_he4_k39__sc43)* &
      Y(jk39)*state % rho &
       )
    call set_jac_entry(jac, jk39, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cl35__k39)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jk39, jcl35, scratch)

    scratch = (&
      screened_rates(k_p_ar38__k39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk39, jar38, scratch)

    scratch = (&
      screened_rates(k_ar39__k39__weak__wc12) &
       )
    call set_jac_entry(jac, jk39, jar39, scratch)

    scratch = (&
      screened_rates(k_n_k38__k39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk39, jk38, scratch)

    scratch = (&
      -screened_rates(k_he4_k39__sc43)*Y(jhe4)*state % rho - screened_rates(k_n_k39__he4_cl36)* &
      Y(jn)*state % rho - screened_rates(k_n_k39__k40)*Y(jn)*state % rho - &
      screened_rates(k_n_k39__p_ar39)*Y(jn)*state % rho - screened_rates(k_p_k39__ca40)* &
      Y(jp)*state % rho - screened_rates(k_p_k39__he4_ar36)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk39, jk39, scratch)

    scratch = (&
      screened_rates(k_ca39__k39__weak__wc12) + screened_rates(k_n_ca39__p_k39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk39, jca39, scratch)

    scratch = (&
      screened_rates(k_p_ca42__he4_k39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk39, jca42, scratch)

    scratch = (&
      screened_rates(k_n_sc42__he4_k39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk39, jsc42, scratch)

    scratch = (&
      screened_rates(k_n_k39__k40)*Y(jk39)*state % rho - screened_rates(k_n_k40__he4_cl37)* &
      Y(jk40)*state % rho - screened_rates(k_n_k40__k41)*Y(jk40)*state % rho + &
      screened_rates(k_n_sc43__he4_k40)*Y(jsc43)*state % rho &
       )
    call set_jac_entry(jac, jk40, jn, scratch)

    scratch = (&
      screened_rates(k_p_ar39__k40)*Y(jar39)*state % rho - screened_rates(k_p_k40__ca41)* &
      Y(jk40)*state % rho - screened_rates(k_p_k40__he4_ar37)*Y(jk40)*state % rho &
      - screened_rates(k_p_k40__n_ca40)*Y(jk40)*state % rho &
       )
    call set_jac_entry(jac, jk40, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cl36__k40)*Y(jcl36)*state % rho - screened_rates(k_he4_k40__p_ca43)* &
      Y(jk40)*state % rho - screened_rates(k_he4_k40__sc44)*Y(jk40)*state % rho &
       )
    call set_jac_entry(jac, jk40, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cl36__k40)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jk40, jcl36, scratch)

    scratch = (&
      screened_rates(k_p_ar39__k40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk40, jar39, scratch)

    scratch = (&
      screened_rates(k_n_k39__k40)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk40, jk39, scratch)

    scratch = (&
      -screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*state % rho - screened_rates(k_he4_k40__sc44)* &
      Y(jhe4)*state % rho - screened_rates(k_k40__ca40__weak__wc12) - &
      screened_rates(k_n_k40__he4_cl37)*Y(jn)*state % rho - screened_rates(k_n_k40__k41)* &
      Y(jn)*state % rho - screened_rates(k_p_k40__ca41)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__he4_ar37)*Y(jp)*state % rho - &
      screened_rates(k_p_k40__n_ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk40, jk40, scratch)

    scratch = (&
      screened_rates(k_n_sc43__he4_k40)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk40, jsc43, scratch)

    scratch = (&
      screened_rates(k_n_k40__k41)*Y(jk40)*state % rho - screened_rates(k_n_k41__k42)*Y(jk41)* &
      state % rho &
       )
    call set_jac_entry(jac, jk41, jn, scratch)

    scratch = (&
      -screened_rates(k_p_k41__ca42)*Y(jk41)*state % rho - screened_rates(k_p_k41__he4_ar38)* &
      Y(jk41)*state % rho - screened_rates(k_p_k41__n_ca41)*Y(jk41)*state % rho &
       )
    call set_jac_entry(jac, jk41, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cl37__k41)*Y(jcl37)*state % rho - screened_rates(k_he4_k41__n_sc44)* &
      Y(jk41)*state % rho - screened_rates(k_he4_k41__p_ca44)*Y(jk41)*state % rho &
      - screened_rates(k_he4_k41__sc45)*Y(jk41)*state % rho &
       )
    call set_jac_entry(jac, jk41, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cl37__k41)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jk41, jcl37, scratch)

    scratch = (&
      screened_rates(k_n_k40__k41)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk41, jk40, scratch)

    scratch = (&
      -screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*state % rho - screened_rates(k_he4_k41__p_ca44)* &
      Y(jhe4)*state % rho - screened_rates(k_he4_k41__sc45)*Y(jhe4)*state % rho - &
      screened_rates(k_n_k41__k42)*Y(jn)*state % rho - screened_rates(k_p_k41__ca42)* &
      Y(jp)*state % rho - screened_rates(k_p_k41__he4_ar38)*Y(jp)*state % rho - &
      screened_rates(k_p_k41__n_ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk41, jk41, scratch)

    scratch = (&
      screened_rates(k_ca41__k41__weak__wc12) &
       )
    call set_jac_entry(jac, jk41, jca41, scratch)

    scratch = (&
      screened_rates(k_n_k41__k42)*Y(jk41)*state % rho &
       )
    call set_jac_entry(jac, jk42, jn, scratch)

    scratch = (&
      -screened_rates(k_p_k42__ca43)*Y(jk42)*state % rho - screened_rates(k_p_k42__he4_ar39)* &
      Y(jk42)*state % rho - screened_rates(k_p_k42__n_ca42)*Y(jk42)*state % rho &
       )
    call set_jac_entry(jac, jk42, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_k42__n_sc45)*Y(jk42)*state % rho - screened_rates(k_he4_k42__sc46)* &
      Y(jk42)*state % rho &
       )
    call set_jac_entry(jac, jk42, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_k41__k42)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jk42, jk41, scratch)

    scratch = (&
      -screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*state % rho - screened_rates(k_he4_k42__sc46)* &
      Y(jhe4)*state % rho - screened_rates(k_k42__ca42__weak__wc12) - &
      screened_rates(k_p_k42__ca43)*Y(jp)*state % rho - screened_rates(k_p_k42__he4_ar39)* &
      Y(jp)*state % rho - screened_rates(k_p_k42__n_ca42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jk42, jk42, scratch)

    scratch = (&
      -screened_rates(k_n_ca39__ca40)*Y(jca39)*state % rho - screened_rates(k_n_ca39__he4_ar36)* &
      Y(jca39)*state % rho - screened_rates(k_n_ca39__p_k39)*Y(jca39)*state % rho &
       )
    call set_jac_entry(jac, jca39, jn, scratch)

    scratch = (&
      screened_rates(k_p_k38__ca39)*Y(jk38)*state % rho + screened_rates(k_p_sc42__he4_ca39)* &
      Y(jsc42)*state % rho &
       )
    call set_jac_entry(jac, jca39, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ar35__ca39)*Y(jar35)*state % rho - screened_rates(k_he4_ca39__ti43)* &
      Y(jca39)*state % rho &
       )
    call set_jac_entry(jac, jca39, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ar35__ca39)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca39, jar35, scratch)

    scratch = (&
      screened_rates(k_p_k38__ca39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca39, jk38, scratch)

    scratch = (&
      -screened_rates(k_ca39__k39__weak__wc12) - screened_rates(k_he4_ca39__ti43)*Y(jhe4)* &
      state % rho - screened_rates(k_n_ca39__ca40)*Y(jn)*state % rho - &
      screened_rates(k_n_ca39__he4_ar36)*Y(jn)*state % rho - &
      screened_rates(k_n_ca39__p_k39)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca39, jca39, scratch)

    scratch = (&
      screened_rates(k_p_sc42__he4_ca39)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca39, jsc42, scratch)

    scratch = (&
      screened_rates(k_n_ca39__ca40)*Y(jca39)*state % rho - screened_rates(k_n_ca40__ca41)* &
      Y(jca40)*state % rho - screened_rates(k_n_ca40__he4_ar37)*Y(jca40)* &
      state % rho + screened_rates(k_n_ti43__he4_ca40)*Y(jti43)*state % rho &
       )
    call set_jac_entry(jac, jca40, jn, scratch)

    scratch = (&
      screened_rates(k_p_k39__ca40)*Y(jk39)*state % rho + screened_rates(k_p_k40__n_ca40)* &
      Y(jk40)*state % rho + screened_rates(k_p_sc43__he4_ca40)*Y(jsc43)* &
      state % rho &
       )
    call set_jac_entry(jac, jca40, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ar36__ca40)*Y(jar36)*state % rho - screened_rates(k_he4_ca40__ti44)* &
      Y(jca40)*state % rho + screened_rates(k_he4_k37__p_ca40)*Y(jk37)*state % rho &
       )
    call set_jac_entry(jac, jca40, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ar36__ca40)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca40, jar36, scratch)

    scratch = (&
      screened_rates(k_he4_k37__p_ca40)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca40, jk37, scratch)

    scratch = (&
      screened_rates(k_p_k39__ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca40, jk39, scratch)

    scratch = (&
      screened_rates(k_k40__ca40__weak__wc12) + screened_rates(k_p_k40__n_ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca40, jk40, scratch)

    scratch = (&
      screened_rates(k_n_ca39__ca40)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca40, jca39, scratch)

    scratch = (&
      -screened_rates(k_he4_ca40__ti44)*Y(jhe4)*state % rho - screened_rates(k_n_ca40__ca41)* &
      Y(jn)*state % rho - screened_rates(k_n_ca40__he4_ar37)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca40, jca40, scratch)

    scratch = (&
      screened_rates(k_p_sc43__he4_ca40)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca40, jsc43, scratch)

    scratch = (&
      screened_rates(k_n_ti43__he4_ca40)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca40, jti43, scratch)

    scratch = (&
      screened_rates(k_n_ca40__ca41)*Y(jca40)*state % rho - screened_rates(k_n_ca41__ca42)* &
      Y(jca41)*state % rho - screened_rates(k_n_ca41__he4_ar38)*Y(jca41)* &
      state % rho + screened_rates(k_n_ti44__he4_ca41)*Y(jti44)*state % rho &
       )
    call set_jac_entry(jac, jca41, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ca41__sc42)*Y(jca41)*state % rho + screened_rates(k_p_k40__ca41)* &
      Y(jk40)*state % rho + screened_rates(k_p_k41__n_ca41)*Y(jk41)*state % rho + &
      screened_rates(k_p_sc44__he4_ca41)*Y(jsc44)*state % rho &
       )
    call set_jac_entry(jac, jca41, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ar37__ca41)*Y(jar37)*state % rho - screened_rates(k_he4_ca41__ti45)* &
      Y(jca41)*state % rho + screened_rates(k_he4_k38__p_ca41)*Y(jk38)*state % rho &
       )
    call set_jac_entry(jac, jca41, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ar37__ca41)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca41, jar37, scratch)

    scratch = (&
      screened_rates(k_he4_k38__p_ca41)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca41, jk38, scratch)

    scratch = (&
      screened_rates(k_p_k40__ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca41, jk40, scratch)

    scratch = (&
      screened_rates(k_p_k41__n_ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca41, jk41, scratch)

    scratch = (&
      screened_rates(k_n_ca40__ca41)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca41, jca40, scratch)

    scratch = (&
      -screened_rates(k_ca41__k41__weak__wc12) - screened_rates(k_he4_ca41__ti45)*Y(jhe4)* &
      state % rho - screened_rates(k_n_ca41__ca42)*Y(jn)*state % rho - &
      screened_rates(k_n_ca41__he4_ar38)*Y(jn)*state % rho - &
      screened_rates(k_p_ca41__sc42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca41, jca41, scratch)

    scratch = (&
      screened_rates(k_p_sc44__he4_ca41)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca41, jsc44, scratch)

    scratch = (&
      screened_rates(k_n_ti44__he4_ca41)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca41, jti44, scratch)

    scratch = (&
      screened_rates(k_n_ca41__ca42)*Y(jca41)*state % rho - screened_rates(k_n_ca42__ca43)* &
      Y(jca42)*state % rho - screened_rates(k_n_ca42__he4_ar39)*Y(jca42)* &
      state % rho + screened_rates(k_n_sc42__p_ca42)*Y(jsc42)*state % rho + &
      screened_rates(k_n_ti45__he4_ca42)*Y(jti45)*state % rho &
       )
    call set_jac_entry(jac, jca42, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ca42__he4_k39)*Y(jca42)*state % rho - screened_rates(k_p_ca42__sc43)* &
      Y(jca42)*state % rho + screened_rates(k_p_k41__ca42)*Y(jk41)*state % rho + &
      screened_rates(k_p_k42__n_ca42)*Y(jk42)*state % rho + &
      screened_rates(k_p_sc45__he4_ca42)*Y(jsc45)*state % rho &
       )
    call set_jac_entry(jac, jca42, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ar38__ca42)*Y(jar38)*state % rho - screened_rates(k_he4_ca42__ti46)* &
      Y(jca42)*state % rho &
       )
    call set_jac_entry(jac, jca42, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ar38__ca42)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca42, jar38, scratch)

    scratch = (&
      screened_rates(k_p_k41__ca42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca42, jk41, scratch)

    scratch = (&
      screened_rates(k_k42__ca42__weak__wc12) + screened_rates(k_p_k42__n_ca42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca42, jk42, scratch)

    scratch = (&
      screened_rates(k_n_ca41__ca42)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca42, jca41, scratch)

    scratch = (&
      -screened_rates(k_he4_ca42__ti46)*Y(jhe4)*state % rho - screened_rates(k_n_ca42__ca43)* &
      Y(jn)*state % rho - screened_rates(k_n_ca42__he4_ar39)*Y(jn)*state % rho - &
      screened_rates(k_p_ca42__he4_k39)*Y(jp)*state % rho - screened_rates(k_p_ca42__sc43) &
      *Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca42, jca42, scratch)

    scratch = (&
      screened_rates(k_n_sc42__p_ca42)*Y(jn)*state % rho + &
      screened_rates(k_sc42__ca42__weak__wc12) &
       )
    call set_jac_entry(jac, jca42, jsc42, scratch)

    scratch = (&
      screened_rates(k_p_sc45__he4_ca42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca42, jsc45, scratch)

    scratch = (&
      screened_rates(k_n_ti45__he4_ca42)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca42, jti45, scratch)

    scratch = (&
      screened_rates(k_n_ca42__ca43)*Y(jca42)*state % rho - screened_rates(k_n_ca43__ca44)* &
      Y(jca43)*state % rho + screened_rates(k_n_sc43__p_ca43)*Y(jsc43)*state % rho &
       )
    call set_jac_entry(jac, jca43, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ca43__sc44)*Y(jca43)*state % rho + screened_rates(k_p_k42__ca43)* &
      Y(jk42)*state % rho + screened_rates(k_p_sc46__he4_ca43)*Y(jsc46)* &
      state % rho &
       )
    call set_jac_entry(jac, jca43, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ar39__ca43)*Y(jar39)*state % rho - screened_rates(k_he4_ca43__n_ti46)* &
      Y(jca43)*state % rho - screened_rates(k_he4_ca43__ti47)*Y(jca43)*state % rho &
      + screened_rates(k_he4_k40__p_ca43)*Y(jk40)*state % rho &
       )
    call set_jac_entry(jac, jca43, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ar39__ca43)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca43, jar39, scratch)

    scratch = (&
      screened_rates(k_he4_k40__p_ca43)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca43, jk40, scratch)

    scratch = (&
      screened_rates(k_p_k42__ca43)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca43, jk42, scratch)

    scratch = (&
      screened_rates(k_n_ca42__ca43)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca43, jca42, scratch)

    scratch = (&
      -screened_rates(k_he4_ca43__n_ti46)*Y(jhe4)*state % rho - screened_rates(k_he4_ca43__ti47)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ca43__ca44)*Y(jn)*state % rho - &
      screened_rates(k_p_ca43__sc44)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca43, jca43, scratch)

    scratch = (&
      screened_rates(k_n_sc43__p_ca43)*Y(jn)*state % rho + &
      screened_rates(k_sc43__ca43__weak__wc12) &
       )
    call set_jac_entry(jac, jca43, jsc43, scratch)

    scratch = (&
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca43, jsc46, scratch)

    scratch = (&
      screened_rates(k_n_ca43__ca44)*Y(jca43)*state % rho + screened_rates(k_n_sc44__p_ca44)* &
      Y(jsc44)*state % rho + screened_rates(k_n_ti47__he4_ca44)*Y(jti47)* &
      state % rho &
       )
    call set_jac_entry(jac, jca44, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ca44__sc45)*Y(jca44)*state % rho &
       )
    call set_jac_entry(jac, jca44, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_ca44__ti48)*Y(jca44)*state % rho + screened_rates(k_he4_k41__p_ca44)* &
      Y(jk41)*state % rho &
       )
    call set_jac_entry(jac, jca44, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_k41__p_ca44)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jca44, jk41, scratch)

    scratch = (&
      screened_rates(k_n_ca43__ca44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca44, jca43, scratch)

    scratch = (&
      -screened_rates(k_he4_ca44__ti48)*Y(jhe4)*state % rho - screened_rates(k_p_ca44__sc45)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jca44, jca44, scratch)

    scratch = (&
      screened_rates(k_n_sc44__p_ca44)*Y(jn)*state % rho + &
      screened_rates(k_sc44__ca44__weak__wc12) &
       )
    call set_jac_entry(jac, jca44, jsc44, scratch)

    scratch = (&
      screened_rates(k_n_ti47__he4_ca44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jca44, jti47, scratch)

    scratch = (&
      -screened_rates(k_n_sc42__he4_k39)*Y(jsc42)*state % rho - screened_rates(k_n_sc42__p_ca42)* &
      Y(jsc42)*state % rho - screened_rates(k_n_sc42__sc43)*Y(jsc42)*state % rho &
       )
    call set_jac_entry(jac, jsc42, jn, scratch)

    scratch = (&
      screened_rates(k_p_ca41__sc42)*Y(jca41)*state % rho - screened_rates(k_p_sc42__he4_ca39)* &
      Y(jsc42)*state % rho - screened_rates(k_p_sc42__ti43)*Y(jsc42)*state % rho &
       )
    call set_jac_entry(jac, jsc42, jp, scratch)

    scratch = (&
      screened_rates(k_he4_k38__sc42)*Y(jk38)*state % rho - screened_rates(k_he4_sc42__p_ti45)* &
      Y(jsc42)*state % rho - screened_rates(k_he4_sc42__v46)*Y(jsc42)*state % rho &
       )
    call set_jac_entry(jac, jsc42, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_k38__sc42)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc42, jk38, scratch)

    scratch = (&
      screened_rates(k_p_ca41__sc42)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsc42, jca41, scratch)

    scratch = (&
      -screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*state % rho - screened_rates(k_he4_sc42__v46)* &
      Y(jhe4)*state % rho - screened_rates(k_n_sc42__he4_k39)*Y(jn)*state % rho - &
      screened_rates(k_n_sc42__p_ca42)*Y(jn)*state % rho - screened_rates(k_n_sc42__sc43)* &
      Y(jn)*state % rho - screened_rates(k_p_sc42__he4_ca39)*Y(jp)*state % rho - &
      screened_rates(k_p_sc42__ti43)*Y(jp)*state % rho - &
      screened_rates(k_sc42__ca42__weak__wc12) &
       )
    call set_jac_entry(jac, jsc42, jsc42, scratch)

    scratch = (&
      screened_rates(k_n_sc42__sc43)*Y(jsc42)*state % rho - screened_rates(k_n_sc43__he4_k40)* &
      Y(jsc43)*state % rho - screened_rates(k_n_sc43__p_ca43)*Y(jsc43)*state % rho &
      - screened_rates(k_n_sc43__sc44)*Y(jsc43)*state % rho + &
      screened_rates(k_n_ti43__p_sc43)*Y(jti43)*state % rho + &
      screened_rates(k_n_v46__he4_sc43)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jn, scratch)

    scratch = (&
      screened_rates(k_p_ca42__sc43)*Y(jca42)*state % rho - screened_rates(k_p_sc43__he4_ca40)* &
      Y(jsc43)*state % rho - screened_rates(k_p_sc43__ti44)*Y(jsc43)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jp, scratch)

    scratch = (&
      screened_rates(k_he4_k39__sc43)*Y(jk39)*state % rho - screened_rates(k_he4_sc43__p_ti46)* &
      Y(jsc43)*state % rho - screened_rates(k_he4_sc43__v47)*Y(jsc43)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_k39__sc43)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jk39, scratch)

    scratch = (&
      screened_rates(k_p_ca42__sc43)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jca42, scratch)

    scratch = (&
      screened_rates(k_n_sc42__sc43)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jsc42, scratch)

    scratch = (&
      -screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*state % rho - screened_rates(k_he4_sc43__v47)* &
      Y(jhe4)*state % rho - screened_rates(k_n_sc43__he4_k40)*Y(jn)*state % rho - &
      screened_rates(k_n_sc43__p_ca43)*Y(jn)*state % rho - screened_rates(k_n_sc43__sc44)* &
      Y(jn)*state % rho - screened_rates(k_p_sc43__he4_ca40)*Y(jp)*state % rho - &
      screened_rates(k_p_sc43__ti44)*Y(jp)*state % rho - &
      screened_rates(k_sc43__ca43__weak__wc12) &
       )
    call set_jac_entry(jac, jsc43, jsc43, scratch)

    scratch = (&
      screened_rates(k_n_ti43__p_sc43)*Y(jn)*state % rho + &
      screened_rates(k_ti43__sc43__weak__wc12) &
       )
    call set_jac_entry(jac, jsc43, jti43, scratch)

    scratch = (&
      screened_rates(k_n_v46__he4_sc43)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc43, jv46, scratch)

    scratch = (&
      screened_rates(k_n_sc43__sc44)*Y(jsc43)*state % rho - screened_rates(k_n_sc44__p_ca44)* &
      Y(jsc44)*state % rho - screened_rates(k_n_sc44__sc45)*Y(jsc44)*state % rho + &
      screened_rates(k_n_ti44__p_sc44)*Y(jti44)*state % rho + &
      screened_rates(k_n_v47__he4_sc44)*Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jn, scratch)

    scratch = (&
      screened_rates(k_p_ca43__sc44)*Y(jca43)*state % rho - screened_rates(k_p_sc44__he4_ca41)* &
      Y(jsc44)*state % rho - screened_rates(k_p_sc44__ti45)*Y(jsc44)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jp, scratch)

    scratch = (&
      screened_rates(k_he4_k40__sc44)*Y(jk40)*state % rho + screened_rates(k_he4_k41__n_sc44)* &
      Y(jk41)*state % rho - screened_rates(k_he4_sc44__p_ti47)*Y(jsc44)* &
      state % rho - screened_rates(k_he4_sc44__v48)*Y(jsc44)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_k40__sc44)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jk40, scratch)

    scratch = (&
      screened_rates(k_he4_k41__n_sc44)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jk41, scratch)

    scratch = (&
      screened_rates(k_p_ca43__sc44)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jca43, scratch)

    scratch = (&
      screened_rates(k_n_sc43__sc44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jsc43, scratch)

    scratch = (&
      -screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*state % rho - screened_rates(k_he4_sc44__v48)* &
      Y(jhe4)*state % rho - screened_rates(k_n_sc44__p_ca44)*Y(jn)*state % rho - &
      screened_rates(k_n_sc44__sc45)*Y(jn)*state % rho - &
      screened_rates(k_p_sc44__he4_ca41)*Y(jp)*state % rho - &
      screened_rates(k_p_sc44__ti45)*Y(jp)*state % rho - &
      screened_rates(k_sc44__ca44__weak__wc12) &
       )
    call set_jac_entry(jac, jsc44, jsc44, scratch)

    scratch = (&
      screened_rates(k_n_ti44__p_sc44)*Y(jn)*state % rho + &
      screened_rates(k_ti44__sc44__weak__wc12) &
       )
    call set_jac_entry(jac, jsc44, jti44, scratch)

    scratch = (&
      screened_rates(k_n_v47__he4_sc44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc44, jv47, scratch)

    scratch = (&
      screened_rates(k_n_sc44__sc45)*Y(jsc44)*state % rho - screened_rates(k_n_sc45__sc46)* &
      Y(jsc45)*state % rho + screened_rates(k_n_ti45__p_sc45)*Y(jti45)*state % rho &
      + screened_rates(k_n_v48__he4_sc45)*Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jn, scratch)

    scratch = (&
      screened_rates(k_p_ca44__sc45)*Y(jca44)*state % rho - screened_rates(k_p_sc45__he4_ca42)* &
      Y(jsc45)*state % rho - screened_rates(k_p_sc45__ti46)*Y(jsc45)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jp, scratch)

    scratch = (&
      screened_rates(k_he4_k41__sc45)*Y(jk41)*state % rho + screened_rates(k_he4_k42__n_sc45)* &
      Y(jk42)*state % rho - screened_rates(k_he4_sc45__p_ti48)*Y(jsc45)* &
      state % rho - screened_rates(k_he4_sc45__v49)*Y(jsc45)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_k41__sc45)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jk41, scratch)

    scratch = (&
      screened_rates(k_he4_k42__n_sc45)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jk42, scratch)

    scratch = (&
      screened_rates(k_p_ca44__sc45)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jca44, scratch)

    scratch = (&
      screened_rates(k_n_sc44__sc45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jsc44, scratch)

    scratch = (&
      -screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*state % rho - screened_rates(k_he4_sc45__v49)* &
      Y(jhe4)*state % rho - screened_rates(k_n_sc45__sc46)*Y(jn)*state % rho - &
      screened_rates(k_p_sc45__he4_ca42)*Y(jp)*state % rho - &
      screened_rates(k_p_sc45__ti46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jsc45, scratch)

    scratch = (&
      screened_rates(k_n_ti45__p_sc45)*Y(jn)*state % rho + &
      screened_rates(k_ti45__sc45__weak__wc12) &
       )
    call set_jac_entry(jac, jsc45, jti45, scratch)

    scratch = (&
      screened_rates(k_n_v48__he4_sc45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc45, jv48, scratch)

    scratch = (&
      screened_rates(k_n_sc45__sc46)*Y(jsc45)*state % rho &
       )
    call set_jac_entry(jac, jsc46, jn, scratch)

    scratch = (&
      -screened_rates(k_p_sc46__he4_ca43)*Y(jsc46)*state % rho - screened_rates(k_p_sc46__n_ti46) &
      *Y(jsc46)*state % rho - screened_rates(k_p_sc46__ti47)*Y(jsc46)*state % rho &
       )
    call set_jac_entry(jac, jsc46, jp, scratch)

    scratch = (&
      screened_rates(k_he4_k42__sc46)*Y(jk42)*state % rho - screened_rates(k_he4_sc46__n_v49)* &
      Y(jsc46)*state % rho - screened_rates(k_he4_sc46__p_ti49)*Y(jsc46)* &
      state % rho - screened_rates(k_he4_sc46__v50)*Y(jsc46)*state % rho &
       )
    call set_jac_entry(jac, jsc46, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_k42__sc46)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jsc46, jk42, scratch)

    scratch = (&
      screened_rates(k_n_sc45__sc46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jsc46, jsc45, scratch)

    scratch = (&
      -screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*state % rho - screened_rates(k_he4_sc46__p_ti49) &
      *Y(jhe4)*state % rho - screened_rates(k_he4_sc46__v50)*Y(jhe4)*state % rho - &
      screened_rates(k_p_sc46__he4_ca43)*Y(jp)*state % rho - &
      screened_rates(k_p_sc46__n_ti46)*Y(jp)*state % rho - screened_rates(k_p_sc46__ti47)* &
      Y(jp)*state % rho - screened_rates(k_sc46__ti46__weak__wc12) &
       )
    call set_jac_entry(jac, jsc46, jsc46, scratch)

    scratch = (&
      -screened_rates(k_n_ti43__he4_ca40)*Y(jti43)*state % rho - screened_rates(k_n_ti43__p_sc43) &
      *Y(jti43)*state % rho - screened_rates(k_n_ti43__ti44)*Y(jti43)*state % rho &
       )
    call set_jac_entry(jac, jti43, jn, scratch)

    scratch = (&
      screened_rates(k_p_sc42__ti43)*Y(jsc42)*state % rho &
       )
    call set_jac_entry(jac, jti43, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ca39__ti43)*Y(jca39)*state % rho - screened_rates(k_he4_ti43__cr47)* &
      Y(jti43)*state % rho - screened_rates(k_he4_ti43__p_v46)*Y(jti43)* &
      state % rho &
       )
    call set_jac_entry(jac, jti43, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ca39__ti43)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti43, jca39, scratch)

    scratch = (&
      screened_rates(k_p_sc42__ti43)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti43, jsc42, scratch)

    scratch = (&
      -screened_rates(k_he4_ti43__cr47)*Y(jhe4)*state % rho - screened_rates(k_he4_ti43__p_v46)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ti43__he4_ca40)*Y(jn)*state % rho - &
      screened_rates(k_n_ti43__p_sc43)*Y(jn)*state % rho - screened_rates(k_n_ti43__ti44)* &
      Y(jn)*state % rho - screened_rates(k_ti43__sc43__weak__wc12) &
       )
    call set_jac_entry(jac, jti43, jti43, scratch)

    scratch = (&
      screened_rates(k_n_cr47__he4_ti44)*Y(jcr47)*state % rho + screened_rates(k_n_ti43__ti44)* &
      Y(jti43)*state % rho - screened_rates(k_n_ti44__he4_ca41)*Y(jti44)* &
      state % rho - screened_rates(k_n_ti44__p_sc44)*Y(jti44)*state % rho - &
      screened_rates(k_n_ti44__ti45)*Y(jti44)*state % rho &
       )
    call set_jac_entry(jac, jti44, jn, scratch)

    scratch = (&
      screened_rates(k_p_sc43__ti44)*Y(jsc43)*state % rho &
       )
    call set_jac_entry(jac, jti44, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ca40__ti44)*Y(jca40)*state % rho - screened_rates(k_he4_ti44__cr48)* &
      Y(jti44)*state % rho - screened_rates(k_he4_ti44__p_v47)*Y(jti44)* &
      state % rho &
       )
    call set_jac_entry(jac, jti44, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ca40__ti44)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti44, jca40, scratch)

    scratch = (&
      screened_rates(k_p_sc43__ti44)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti44, jsc43, scratch)

    scratch = (&
      screened_rates(k_n_ti43__ti44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti44, jti43, scratch)

    scratch = (&
      -screened_rates(k_he4_ti44__cr48)*Y(jhe4)*state % rho - screened_rates(k_he4_ti44__p_v47)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ti44__he4_ca41)*Y(jn)*state % rho - &
      screened_rates(k_n_ti44__p_sc44)*Y(jn)*state % rho - screened_rates(k_n_ti44__ti45)* &
      Y(jn)*state % rho - screened_rates(k_ti44__sc44__weak__wc12) &
       )
    call set_jac_entry(jac, jti44, jti44, scratch)

    scratch = (&
      screened_rates(k_n_cr47__he4_ti44)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti44, jcr47, scratch)

    scratch = (&
      screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)*state % rho + screened_rates(k_n_ti44__ti45)* &
      Y(jti44)*state % rho - screened_rates(k_n_ti45__he4_ca42)*Y(jti45)* &
      state % rho - screened_rates(k_n_ti45__p_sc45)*Y(jti45)*state % rho - &
      screened_rates(k_n_ti45__ti46)*Y(jti45)*state % rho &
       )
    call set_jac_entry(jac, jti45, jn, scratch)

    scratch = (&
      screened_rates(k_p_sc44__ti45)*Y(jsc44)*state % rho - screened_rates(k_p_ti45__v46)* &
      Y(jti45)*state % rho &
       )
    call set_jac_entry(jac, jti45, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ca41__ti45)*Y(jca41)*state % rho + screened_rates(k_he4_sc42__p_ti45)* &
      Y(jsc42)*state % rho - screened_rates(k_he4_ti45__cr49)*Y(jti45)*state % rho &
      - screened_rates(k_he4_ti45__p_v48)*Y(jti45)*state % rho &
       )
    call set_jac_entry(jac, jti45, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ca41__ti45)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti45, jca41, scratch)

    scratch = (&
      screened_rates(k_he4_sc42__p_ti45)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti45, jsc42, scratch)

    scratch = (&
      screened_rates(k_p_sc44__ti45)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti45, jsc44, scratch)

    scratch = (&
      screened_rates(k_n_ti44__ti45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti45, jti44, scratch)

    scratch = (&
      -screened_rates(k_he4_ti45__cr49)*Y(jhe4)*state % rho - screened_rates(k_he4_ti45__p_v48)* &
      Y(jhe4)*state % rho - screened_rates(k_n_ti45__he4_ca42)*Y(jn)*state % rho - &
      screened_rates(k_n_ti45__p_sc45)*Y(jn)*state % rho - screened_rates(k_n_ti45__ti46)* &
      Y(jn)*state % rho - screened_rates(k_p_ti45__v46)*Y(jp)*state % rho - &
      screened_rates(k_ti45__sc45__weak__wc12) &
       )
    call set_jac_entry(jac, jti45, jti45, scratch)

    scratch = (&
      screened_rates(k_n_cr48__he4_ti45)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti45, jcr48, scratch)

    scratch = (&
      screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)*state % rho + screened_rates(k_n_ti45__ti46)* &
      Y(jti45)*state % rho - screened_rates(k_n_ti46__ti47)*Y(jti46)*state % rho + &
      screened_rates(k_n_v46__p_ti46)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jti46, jn, scratch)

    scratch = (&
      screened_rates(k_p_sc45__ti46)*Y(jsc45)*state % rho + screened_rates(k_p_sc46__n_ti46)* &
      Y(jsc46)*state % rho - screened_rates(k_p_ti46__v47)*Y(jti46)*state % rho + &
      screened_rates(k_p_v49__he4_ti46)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jti46, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ca42__ti46)*Y(jca42)*state % rho + screened_rates(k_he4_ca43__n_ti46)* &
      Y(jca43)*state % rho + screened_rates(k_he4_sc43__p_ti46)*Y(jsc43)* &
      state % rho - screened_rates(k_he4_ti46__cr50)*Y(jti46)*state % rho &
       )
    call set_jac_entry(jac, jti46, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ca42__ti46)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti46, jca42, scratch)

    scratch = (&
      screened_rates(k_he4_ca43__n_ti46)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti46, jca43, scratch)

    scratch = (&
      screened_rates(k_he4_sc43__p_ti46)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti46, jsc43, scratch)

    scratch = (&
      screened_rates(k_p_sc45__ti46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti46, jsc45, scratch)

    scratch = (&
      screened_rates(k_p_sc46__n_ti46)*Y(jp)*state % rho + &
      screened_rates(k_sc46__ti46__weak__wc12) &
       )
    call set_jac_entry(jac, jti46, jsc46, scratch)

    scratch = (&
      screened_rates(k_n_ti45__ti46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti46, jti45, scratch)

    scratch = (&
      -screened_rates(k_he4_ti46__cr50)*Y(jhe4)*state % rho - screened_rates(k_n_ti46__ti47)* &
      Y(jn)*state % rho - screened_rates(k_p_ti46__v47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti46, jti46, scratch)

    scratch = (&
      screened_rates(k_n_v46__p_ti46)*Y(jn)*state % rho + screened_rates(k_v46__ti46__weak__wc12) &
       )
    call set_jac_entry(jac, jti46, jv46, scratch)

    scratch = (&
      screened_rates(k_p_v49__he4_ti46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti46, jv49, scratch)

    scratch = (&
      screened_rates(k_n_cr49__he4_ti46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti46, jcr49, scratch)

    scratch = (&
      screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)*state % rho + screened_rates(k_n_ti46__ti47)* &
      Y(jti46)*state % rho - screened_rates(k_n_ti47__he4_ca44)*Y(jti47)* &
      state % rho - screened_rates(k_n_ti47__ti48)*Y(jti47)*state % rho + &
      screened_rates(k_n_v47__p_ti47)*Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jti47, jn, scratch)

    scratch = (&
      screened_rates(k_p_sc46__ti47)*Y(jsc46)*state % rho - screened_rates(k_p_ti47__v48)* &
      Y(jti47)*state % rho + screened_rates(k_p_v50__he4_ti47)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jti47, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ca43__ti47)*Y(jca43)*state % rho + screened_rates(k_he4_sc44__p_ti47)* &
      Y(jsc44)*state % rho - screened_rates(k_he4_ti47__cr51)*Y(jti47)*state % rho &
       )
    call set_jac_entry(jac, jti47, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ca43__ti47)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti47, jca43, scratch)

    scratch = (&
      screened_rates(k_he4_sc44__p_ti47)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti47, jsc44, scratch)

    scratch = (&
      screened_rates(k_p_sc46__ti47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti47, jsc46, scratch)

    scratch = (&
      screened_rates(k_n_ti46__ti47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti47, jti46, scratch)

    scratch = (&
      -screened_rates(k_he4_ti47__cr51)*Y(jhe4)*state % rho - screened_rates(k_n_ti47__he4_ca44)* &
      Y(jn)*state % rho - screened_rates(k_n_ti47__ti48)*Y(jn)*state % rho - &
      screened_rates(k_p_ti47__v48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti47, jti47, scratch)

    scratch = (&
      screened_rates(k_n_v47__p_ti47)*Y(jn)*state % rho + screened_rates(k_v47__ti47__weak__wc12) &
       )
    call set_jac_entry(jac, jti47, jv47, scratch)

    scratch = (&
      screened_rates(k_p_v50__he4_ti47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti47, jv50, scratch)

    scratch = (&
      screened_rates(k_n_cr50__he4_ti47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti47, jcr50, scratch)

    scratch = (&
      screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)*state % rho + screened_rates(k_n_ti47__ti48)* &
      Y(jti47)*state % rho - screened_rates(k_n_ti48__ti49)*Y(jti48)*state % rho + &
      screened_rates(k_n_v48__p_ti48)*Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jti48, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ti48__v49)*Y(jti48)*state % rho + screened_rates(k_p_v51__he4_ti48)* &
      Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jti48, jp, scratch)

    scratch = (&
      screened_rates(k_he4_ca44__ti48)*Y(jca44)*state % rho + screened_rates(k_he4_sc45__p_ti48)* &
      Y(jsc45)*state % rho - screened_rates(k_he4_ti48__cr52)*Y(jti48)*state % rho &
       )
    call set_jac_entry(jac, jti48, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ca44__ti48)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti48, jca44, scratch)

    scratch = (&
      screened_rates(k_he4_sc45__p_ti48)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti48, jsc45, scratch)

    scratch = (&
      screened_rates(k_n_ti47__ti48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti48, jti47, scratch)

    scratch = (&
      -screened_rates(k_he4_ti48__cr52)*Y(jhe4)*state % rho - screened_rates(k_n_ti48__ti49)* &
      Y(jn)*state % rho - screened_rates(k_p_ti48__v49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti48, jti48, scratch)

    scratch = (&
      screened_rates(k_n_v48__p_ti48)*Y(jn)*state % rho + screened_rates(k_v48__ti48__weak__wc12) &
       )
    call set_jac_entry(jac, jti48, jv48, scratch)

    scratch = (&
      screened_rates(k_p_v51__he4_ti48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti48, jv51, scratch)

    scratch = (&
      screened_rates(k_n_cr51__he4_ti48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti48, jcr51, scratch)

    scratch = (&
      screened_rates(k_n_ti48__ti49)*Y(jti48)*state % rho + screened_rates(k_n_v49__p_ti49)* &
      Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jti49, jn, scratch)

    scratch = (&
      -screened_rates(k_p_ti49__v50)*Y(jti49)*state % rho &
       )
    call set_jac_entry(jac, jti49, jp, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__p_ti49)*Y(jsc46)*state % rho - &
      screened_rates(k_he4_ti49__n_cr52)*Y(jti49)*state % rho &
       )
    call set_jac_entry(jac, jti49, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__p_ti49)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jti49, jsc46, scratch)

    scratch = (&
      screened_rates(k_n_ti48__ti49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jti49, jti48, scratch)

    scratch = (&
      -screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*state % rho - screened_rates(k_p_ti49__v50)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jti49, jti49, scratch)

    scratch = (&
      screened_rates(k_n_v49__p_ti49)*Y(jn)*state % rho + screened_rates(k_v49__ti49__weak__wc12) &
       )
    call set_jac_entry(jac, jti49, jv49, scratch)

    scratch = (&
      screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*state % rho - screened_rates(k_n_v46__he4_sc43)* &
      Y(jv46)*state % rho - screened_rates(k_n_v46__p_ti46)*Y(jv46)*state % rho - &
      screened_rates(k_n_v46__v47)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jv46, jn, scratch)

    scratch = (&
      screened_rates(k_p_ti45__v46)*Y(jti45)*state % rho - screened_rates(k_p_v46__cr47)* &
      Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jv46, jp, scratch)

    scratch = (&
      screened_rates(k_he4_sc42__v46)*Y(jsc42)*state % rho + screened_rates(k_he4_ti43__p_v46)* &
      Y(jti43)*state % rho - screened_rates(k_he4_v46__mn50)*Y(jv46)*state % rho - &
      screened_rates(k_he4_v46__p_cr49)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jv46, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_sc42__v46)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv46, jsc42, scratch)

    scratch = (&
      screened_rates(k_he4_ti43__p_v46)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv46, jti43, scratch)

    scratch = (&
      screened_rates(k_p_ti45__v46)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jv46, jti45, scratch)

    scratch = (&
      -screened_rates(k_he4_v46__mn50)*Y(jhe4)*state % rho - screened_rates(k_he4_v46__p_cr49)* &
      Y(jhe4)*state % rho - screened_rates(k_n_v46__he4_sc43)*Y(jn)*state % rho - &
      screened_rates(k_n_v46__p_ti46)*Y(jn)*state % rho - screened_rates(k_n_v46__v47)* &
      Y(jn)*state % rho - screened_rates(k_p_v46__cr47)*Y(jp)*state % rho - &
      screened_rates(k_v46__ti46__weak__wc12) &
       )
    call set_jac_entry(jac, jv46, jv46, scratch)

    scratch = (&
      screened_rates(k_n_mn49__he4_v46)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv46, jmn49, scratch)

    scratch = (&
      screened_rates(k_n_cr47__p_v47)*Y(jcr47)*state % rho + screened_rates(k_n_mn50__he4_v47)* &
      Y(jmn50)*state % rho + screened_rates(k_n_v46__v47)*Y(jv46)*state % rho - &
      screened_rates(k_n_v47__he4_sc44)*Y(jv47)*state % rho - &
      screened_rates(k_n_v47__p_ti47)*Y(jv47)*state % rho - screened_rates(k_n_v47__v48)* &
      Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jv47, jn, scratch)

    scratch = (&
      screened_rates(k_p_ti46__v47)*Y(jti46)*state % rho - screened_rates(k_p_v47__cr48)* &
      Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jv47, jp, scratch)

    scratch = (&
      screened_rates(k_he4_sc43__v47)*Y(jsc43)*state % rho + screened_rates(k_he4_ti44__p_v47)* &
      Y(jti44)*state % rho - screened_rates(k_he4_v47__mn51)*Y(jv47)*state % rho - &
      screened_rates(k_he4_v47__p_cr50)*Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jv47, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_sc43__v47)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv47, jsc43, scratch)

    scratch = (&
      screened_rates(k_he4_ti44__p_v47)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv47, jti44, scratch)

    scratch = (&
      screened_rates(k_p_ti46__v47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jv47, jti46, scratch)

    scratch = (&
      screened_rates(k_n_v46__v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv47, jv46, scratch)

    scratch = (&
      -screened_rates(k_he4_v47__mn51)*Y(jhe4)*state % rho - screened_rates(k_he4_v47__p_cr50)* &
      Y(jhe4)*state % rho - screened_rates(k_n_v47__he4_sc44)*Y(jn)*state % rho - &
      screened_rates(k_n_v47__p_ti47)*Y(jn)*state % rho - screened_rates(k_n_v47__v48)* &
      Y(jn)*state % rho - screened_rates(k_p_v47__cr48)*Y(jp)*state % rho - &
      screened_rates(k_v47__ti47__weak__wc12) &
       )
    call set_jac_entry(jac, jv47, jv47, scratch)

    scratch = (&
      screened_rates(k_cr47__v47__weak__wc12) + screened_rates(k_n_cr47__p_v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv47, jcr47, scratch)

    scratch = (&
      screened_rates(k_n_mn50__he4_v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv47, jmn50, scratch)

    scratch = (&
      screened_rates(k_n_cr48__p_v48)*Y(jcr48)*state % rho + screened_rates(k_n_mn51__he4_v48)* &
      Y(jmn51)*state % rho + screened_rates(k_n_v47__v48)*Y(jv47)*state % rho - &
      screened_rates(k_n_v48__he4_sc45)*Y(jv48)*state % rho - &
      screened_rates(k_n_v48__p_ti48)*Y(jv48)*state % rho - screened_rates(k_n_v48__v49)* &
      Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jv48, jn, scratch)

    scratch = (&
      screened_rates(k_p_ti47__v48)*Y(jti47)*state % rho - screened_rates(k_p_v48__cr49)* &
      Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jv48, jp, scratch)

    scratch = (&
      screened_rates(k_he4_sc44__v48)*Y(jsc44)*state % rho + screened_rates(k_he4_ti45__p_v48)* &
      Y(jti45)*state % rho - screened_rates(k_he4_v48__mn52)*Y(jv48)*state % rho - &
      screened_rates(k_he4_v48__p_cr51)*Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jv48, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_sc44__v48)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv48, jsc44, scratch)

    scratch = (&
      screened_rates(k_he4_ti45__p_v48)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv48, jti45, scratch)

    scratch = (&
      screened_rates(k_p_ti47__v48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jv48, jti47, scratch)

    scratch = (&
      screened_rates(k_n_v47__v48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv48, jv47, scratch)

    scratch = (&
      -screened_rates(k_he4_v48__mn52)*Y(jhe4)*state % rho - screened_rates(k_he4_v48__p_cr51)* &
      Y(jhe4)*state % rho - screened_rates(k_n_v48__he4_sc45)*Y(jn)*state % rho - &
      screened_rates(k_n_v48__p_ti48)*Y(jn)*state % rho - screened_rates(k_n_v48__v49)* &
      Y(jn)*state % rho - screened_rates(k_p_v48__cr49)*Y(jp)*state % rho - &
      screened_rates(k_v48__ti48__weak__wc12) &
       )
    call set_jac_entry(jac, jv48, jv48, scratch)

    scratch = (&
      screened_rates(k_cr48__v48__weak__wc12) + screened_rates(k_n_cr48__p_v48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv48, jcr48, scratch)

    scratch = (&
      screened_rates(k_n_mn51__he4_v48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv48, jmn51, scratch)

    scratch = (&
      screened_rates(k_n_cr49__p_v49)*Y(jcr49)*state % rho + screened_rates(k_n_mn52__he4_v49)* &
      Y(jmn52)*state % rho + screened_rates(k_n_v48__v49)*Y(jv48)*state % rho - &
      screened_rates(k_n_v49__p_ti49)*Y(jv49)*state % rho - screened_rates(k_n_v49__v50)* &
      Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jv49, jn, scratch)

    scratch = (&
      screened_rates(k_p_ti48__v49)*Y(jti48)*state % rho - screened_rates(k_p_v49__cr50)* &
      Y(jv49)*state % rho - screened_rates(k_p_v49__he4_ti46)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jv49, jp, scratch)

    scratch = (&
      screened_rates(k_he4_sc45__v49)*Y(jsc45)*state % rho + screened_rates(k_he4_sc46__n_v49)* &
      Y(jsc46)*state % rho - screened_rates(k_he4_v49__mn53)*Y(jv49)*state % rho - &
      screened_rates(k_he4_v49__p_cr52)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jv49, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_sc45__v49)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv49, jsc45, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__n_v49)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv49, jsc46, scratch)

    scratch = (&
      screened_rates(k_p_ti48__v49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jv49, jti48, scratch)

    scratch = (&
      screened_rates(k_n_v48__v49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv49, jv48, scratch)

    scratch = (&
      -screened_rates(k_he4_v49__mn53)*Y(jhe4)*state % rho - screened_rates(k_he4_v49__p_cr52)* &
      Y(jhe4)*state % rho - screened_rates(k_n_v49__p_ti49)*Y(jn)*state % rho - &
      screened_rates(k_n_v49__v50)*Y(jn)*state % rho - screened_rates(k_p_v49__cr50)* &
      Y(jp)*state % rho - screened_rates(k_p_v49__he4_ti46)*Y(jp)*state % rho - &
      screened_rates(k_v49__ti49__weak__wc12) &
       )
    call set_jac_entry(jac, jv49, jv49, scratch)

    scratch = (&
      screened_rates(k_cr49__v49__weak__wc12) + screened_rates(k_n_cr49__p_v49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv49, jcr49, scratch)

    scratch = (&
      screened_rates(k_n_mn52__he4_v49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv49, jmn52, scratch)

    scratch = (&
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*state % rho + screened_rates(k_n_v49__v50)* &
      Y(jv49)*state % rho - screened_rates(k_n_v50__v51)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jv50, jn, scratch)

    scratch = (&
      screened_rates(k_p_ti49__v50)*Y(jti49)*state % rho - screened_rates(k_p_v50__cr51)* &
      Y(jv50)*state % rho - screened_rates(k_p_v50__he4_ti47)*Y(jv50)*state % rho &
      - screened_rates(k_p_v50__n_cr50)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jv50, jp, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__v50)*Y(jsc46)*state % rho - screened_rates(k_he4_v50__mn54)* &
      Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jv50, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_sc46__v50)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jv50, jsc46, scratch)

    scratch = (&
      screened_rates(k_p_ti49__v50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jv50, jti49, scratch)

    scratch = (&
      screened_rates(k_n_v49__v50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv50, jv49, scratch)

    scratch = (&
      -screened_rates(k_he4_v50__mn54)*Y(jhe4)*state % rho - screened_rates(k_n_v50__v51)*Y(jn) &
      *state % rho - screened_rates(k_p_v50__cr51)*Y(jp)*state % rho - &
      screened_rates(k_p_v50__he4_ti47)*Y(jp)*state % rho - &
      screened_rates(k_p_v50__n_cr50)*Y(jp)*state % rho - &
      screened_rates(k_v50__cr50__weak__wc12) &
       )
    call set_jac_entry(jac, jv50, jv50, scratch)

    scratch = (&
      screened_rates(k_n_mn53__he4_v50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv50, jmn53, scratch)

    scratch = (&
      screened_rates(k_n_cr51__p_v51)*Y(jcr51)*state % rho + screened_rates(k_n_mn54__he4_v51)* &
      Y(jmn54)*state % rho + screened_rates(k_n_v50__v51)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jv51, jn, scratch)

    scratch = (&
      -screened_rates(k_p_v51__cr52)*Y(jv51)*state % rho - screened_rates(k_p_v51__he4_ti48)* &
      Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jv51, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_v51__mn55)*Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jv51, jhe4, scratch)

    scratch = (&
      screened_rates(k_n_v50__v51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv51, jv50, scratch)

    scratch = (&
      -screened_rates(k_he4_v51__mn55)*Y(jhe4)*state % rho - screened_rates(k_p_v51__cr52)* &
      Y(jp)*state % rho - screened_rates(k_p_v51__he4_ti48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jv51, jv51, scratch)

    scratch = (&
      screened_rates(k_cr51__v51__weak__wc12) + screened_rates(k_n_cr51__p_v51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv51, jcr51, scratch)

    scratch = (&
      screened_rates(k_n_mn54__he4_v51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jv51, jmn54, scratch)

    scratch = (&
      -screened_rates(k_n_cr47__cr48)*Y(jcr47)*state % rho - screened_rates(k_n_cr47__he4_ti44)* &
      Y(jcr47)*state % rho - screened_rates(k_n_cr47__p_v47)*Y(jcr47)*state % rho &
       )
    call set_jac_entry(jac, jcr47, jn, scratch)

    scratch = (&
      screened_rates(k_p_v46__cr47)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jcr47, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cr47__fe51)*Y(jcr47)*state % rho - screened_rates(k_he4_cr47__p_mn50) &
      *Y(jcr47)*state % rho + screened_rates(k_he4_ti43__cr47)*Y(jti43)* &
      state % rho &
       )
    call set_jac_entry(jac, jcr47, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ti43__cr47)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr47, jti43, scratch)

    scratch = (&
      screened_rates(k_p_v46__cr47)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr47, jv46, scratch)

    scratch = (&
      -screened_rates(k_cr47__v47__weak__wc12) - screened_rates(k_he4_cr47__fe51)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_cr47__p_mn50)*Y(jhe4)*state % rho - &
      screened_rates(k_n_cr47__cr48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr47__he4_ti44)*Y(jn)*state % rho - &
      screened_rates(k_n_cr47__p_v47)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr47, jcr47, scratch)

    scratch = (&
      screened_rates(k_n_cr47__cr48)*Y(jcr47)*state % rho - screened_rates(k_n_cr48__cr49)* &
      Y(jcr48)*state % rho - screened_rates(k_n_cr48__he4_ti45)*Y(jcr48)* &
      state % rho - screened_rates(k_n_cr48__p_v48)*Y(jcr48)*state % rho + &
      screened_rates(k_n_fe51__he4_cr48)*Y(jfe51)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cr48__mn49)*Y(jcr48)*state % rho + screened_rates(k_p_v47__cr48)* &
      Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cr48__fe52)*Y(jcr48)*state % rho - screened_rates(k_he4_cr48__p_mn51) &
      *Y(jcr48)*state % rho + screened_rates(k_he4_ti44__cr48)*Y(jti44)* &
      state % rho &
       )
    call set_jac_entry(jac, jcr48, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ti44__cr48)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jti44, scratch)

    scratch = (&
      screened_rates(k_p_v47__cr48)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jv47, scratch)

    scratch = (&
      screened_rates(k_n_cr47__cr48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jcr47, scratch)

    scratch = (&
      -screened_rates(k_cr48__v48__weak__wc12) - screened_rates(k_he4_cr48__fe52)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_cr48__p_mn51)*Y(jhe4)*state % rho - &
      screened_rates(k_n_cr48__cr49)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__he4_ti45)*Y(jn)*state % rho - &
      screened_rates(k_n_cr48__p_v48)*Y(jn)*state % rho - screened_rates(k_p_cr48__mn49)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jcr48, scratch)

    scratch = (&
      screened_rates(k_n_fe51__he4_cr48)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr48, jfe51, scratch)

    scratch = (&
      screened_rates(k_n_cr48__cr49)*Y(jcr48)*state % rho - screened_rates(k_n_cr49__cr50)* &
      Y(jcr49)*state % rho - screened_rates(k_n_cr49__he4_ti46)*Y(jcr49)* &
      state % rho - screened_rates(k_n_cr49__p_v49)*Y(jcr49)*state % rho + &
      screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)*state % rho + &
      screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cr49__mn50)*Y(jcr49)*state % rho + screened_rates(k_p_v48__cr49)* &
      Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cr49__fe53)*Y(jcr49)*state % rho - screened_rates(k_he4_cr49__p_mn52) &
      *Y(jcr49)*state % rho + screened_rates(k_he4_ti45__cr49)*Y(jti45)* &
      state % rho + screened_rates(k_he4_v46__p_cr49)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ti45__cr49)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jti45, scratch)

    scratch = (&
      screened_rates(k_he4_v46__p_cr49)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jv46, scratch)

    scratch = (&
      screened_rates(k_p_v48__cr49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jv48, scratch)

    scratch = (&
      screened_rates(k_n_cr48__cr49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jcr48, scratch)

    scratch = (&
      -screened_rates(k_cr49__v49__weak__wc12) - screened_rates(k_he4_cr49__fe53)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_cr49__p_mn52)*Y(jhe4)*state % rho - &
      screened_rates(k_n_cr49__cr50)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__he4_ti46)*Y(jn)*state % rho - &
      screened_rates(k_n_cr49__p_v49)*Y(jn)*state % rho - screened_rates(k_p_cr49__mn50)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jcr49, scratch)

    scratch = (&
      screened_rates(k_mn49__cr49__weak__wc12) + screened_rates(k_n_mn49__p_cr49)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jcr49, jmn49, scratch)

    scratch = (&
      screened_rates(k_n_fe52__he4_cr49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr49, jfe52, scratch)

    scratch = (&
      screened_rates(k_n_cr49__cr50)*Y(jcr49)*state % rho - screened_rates(k_n_cr50__cr51)* &
      Y(jcr50)*state % rho - screened_rates(k_n_cr50__he4_ti47)*Y(jcr50)* &
      state % rho + screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*state % rho + &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cr50__mn51)*Y(jcr50)*state % rho + screened_rates(k_p_mn53__he4_cr50)* &
      Y(jmn53)*state % rho + screened_rates(k_p_v49__cr50)*Y(jv49)*state % rho + &
      screened_rates(k_p_v50__n_cr50)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cr50__fe54)*Y(jcr50)*state % rho + screened_rates(k_he4_ti46__cr50)* &
      Y(jti46)*state % rho + screened_rates(k_he4_v47__p_cr50)*Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ti46__cr50)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jti46, scratch)

    scratch = (&
      screened_rates(k_he4_v47__p_cr50)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jv47, scratch)

    scratch = (&
      screened_rates(k_p_v49__cr50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jv49, scratch)

    scratch = (&
      screened_rates(k_p_v50__n_cr50)*Y(jp)*state % rho + screened_rates(k_v50__cr50__weak__wc12) &
       )
    call set_jac_entry(jac, jcr50, jv50, scratch)

    scratch = (&
      screened_rates(k_n_cr49__cr50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jcr49, scratch)

    scratch = (&
      -screened_rates(k_he4_cr50__fe54)*Y(jhe4)*state % rho - screened_rates(k_n_cr50__cr51)* &
      Y(jn)*state % rho - screened_rates(k_n_cr50__he4_ti47)*Y(jn)*state % rho - &
      screened_rates(k_p_cr50__mn51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jcr50, scratch)

    scratch = (&
      screened_rates(k_mn50__cr50__weak__wc12) + screened_rates(k_n_mn50__p_cr50)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jcr50, jmn50, scratch)

    scratch = (&
      screened_rates(k_p_mn53__he4_cr50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jmn53, scratch)

    scratch = (&
      screened_rates(k_n_fe53__he4_cr50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr50, jfe53, scratch)

    scratch = (&
      screened_rates(k_n_cr50__cr51)*Y(jcr50)*state % rho - screened_rates(k_n_cr51__cr52)* &
      Y(jcr51)*state % rho - screened_rates(k_n_cr51__he4_ti48)*Y(jcr51)* &
      state % rho - screened_rates(k_n_cr51__p_v51)*Y(jcr51)*state % rho + &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*state % rho + &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cr51__mn52)*Y(jcr51)*state % rho + screened_rates(k_p_mn54__he4_cr51)* &
      Y(jmn54)*state % rho + screened_rates(k_p_v50__cr51)*Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cr51__fe55)*Y(jcr51)*state % rho + screened_rates(k_he4_ti47__cr51)* &
      Y(jti47)*state % rho + screened_rates(k_he4_v48__p_cr51)*Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ti47__cr51)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jti47, scratch)

    scratch = (&
      screened_rates(k_he4_v48__p_cr51)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jv48, scratch)

    scratch = (&
      screened_rates(k_p_v50__cr51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jv50, scratch)

    scratch = (&
      screened_rates(k_n_cr50__cr51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jcr50, scratch)

    scratch = (&
      -screened_rates(k_cr51__v51__weak__wc12) - screened_rates(k_he4_cr51__fe55)*Y(jhe4)* &
      state % rho - screened_rates(k_n_cr51__cr52)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__he4_ti48)*Y(jn)*state % rho - &
      screened_rates(k_n_cr51__p_v51)*Y(jn)*state % rho - screened_rates(k_p_cr51__mn52)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jcr51, scratch)

    scratch = (&
      screened_rates(k_mn51__cr51__weak__wc12) + screened_rates(k_n_mn51__p_cr51)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jcr51, jmn51, scratch)

    scratch = (&
      screened_rates(k_p_mn54__he4_cr51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jmn54, scratch)

    scratch = (&
      screened_rates(k_n_fe54__he4_cr51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr51, jfe54, scratch)

    scratch = (&
      screened_rates(k_n_cr51__cr52)*Y(jcr51)*state % rho + screened_rates(k_n_fe55__he4_cr52)* &
      Y(jfe55)*state % rho + screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jn, scratch)

    scratch = (&
      -screened_rates(k_p_cr52__mn53)*Y(jcr52)*state % rho + screened_rates(k_p_mn55__he4_cr52)* &
      Y(jmn55)*state % rho + screened_rates(k_p_v51__cr52)*Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_cr52__fe56)*Y(jcr52)*state % rho + screened_rates(k_he4_ti48__cr52)* &
      Y(jti48)*state % rho + screened_rates(k_he4_ti49__n_cr52)*Y(jti49)* &
      state % rho + screened_rates(k_he4_v49__p_cr52)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_ti48__cr52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jti48, scratch)

    scratch = (&
      screened_rates(k_he4_ti49__n_cr52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jti49, scratch)

    scratch = (&
      screened_rates(k_he4_v49__p_cr52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jv49, scratch)

    scratch = (&
      screened_rates(k_p_v51__cr52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jv51, scratch)

    scratch = (&
      screened_rates(k_n_cr51__cr52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jcr51, scratch)

    scratch = (&
      -screened_rates(k_he4_cr52__fe56)*Y(jhe4)*state % rho - screened_rates(k_p_cr52__mn53)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jcr52, scratch)

    scratch = (&
      screened_rates(k_mn52__cr52__weak__wc12) + screened_rates(k_n_mn52__p_cr52)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jcr52, jmn52, scratch)

    scratch = (&
      screened_rates(k_p_mn55__he4_cr52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jmn55, scratch)

    scratch = (&
      screened_rates(k_n_fe55__he4_cr52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jcr52, jfe55, scratch)

    scratch = (&
      -screened_rates(k_n_mn49__he4_v46)*Y(jmn49)*state % rho - screened_rates(k_n_mn49__mn50)* &
      Y(jmn49)*state % rho - screened_rates(k_n_mn49__p_cr49)*Y(jmn49)*state % rho &
       )
    call set_jac_entry(jac, jmn49, jn, scratch)

    scratch = (&
      screened_rates(k_p_cr48__mn49)*Y(jcr48)*state % rho &
       )
    call set_jac_entry(jac, jmn49, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mn49__co53)*Y(jmn49)*state % rho - screened_rates(k_he4_mn49__p_fe52) &
      *Y(jmn49)*state % rho &
       )
    call set_jac_entry(jac, jmn49, jhe4, scratch)

    scratch = (&
      screened_rates(k_p_cr48__mn49)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn49, jcr48, scratch)

    scratch = (&
      -screened_rates(k_he4_mn49__co53)*Y(jhe4)*state % rho - screened_rates(k_he4_mn49__p_fe52)* &
      Y(jhe4)*state % rho - screened_rates(k_mn49__cr49__weak__wc12) - &
      screened_rates(k_n_mn49__he4_v46)*Y(jn)*state % rho - screened_rates(k_n_mn49__mn50) &
      *Y(jn)*state % rho - screened_rates(k_n_mn49__p_cr49)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn49, jmn49, scratch)

    scratch = (&
      screened_rates(k_n_co53__he4_mn50)*Y(jco53)*state % rho + screened_rates(k_n_mn49__mn50)* &
      Y(jmn49)*state % rho - screened_rates(k_n_mn50__he4_v47)*Y(jmn50)* &
      state % rho - screened_rates(k_n_mn50__mn51)*Y(jmn50)*state % rho - &
      screened_rates(k_n_mn50__p_cr50)*Y(jmn50)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jn, scratch)

    scratch = (&
      screened_rates(k_p_cr49__mn50)*Y(jcr49)*state % rho - screened_rates(k_p_mn50__fe51)* &
      Y(jmn50)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr47__p_mn50)*Y(jcr47)*state % rho - screened_rates(k_he4_mn50__co54)* &
      Y(jmn50)*state % rho - screened_rates(k_he4_mn50__p_fe53)*Y(jmn50)* &
      state % rho + screened_rates(k_he4_v46__mn50)*Y(jv46)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_v46__mn50)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jv46, scratch)

    scratch = (&
      screened_rates(k_he4_cr47__p_mn50)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jcr47, scratch)

    scratch = (&
      screened_rates(k_p_cr49__mn50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jcr49, scratch)

    scratch = (&
      screened_rates(k_n_mn49__mn50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jmn49, scratch)

    scratch = (&
      -screened_rates(k_he4_mn50__co54)*Y(jhe4)*state % rho - screened_rates(k_he4_mn50__p_fe53)* &
      Y(jhe4)*state % rho - screened_rates(k_mn50__cr50__weak__wc12) - &
      screened_rates(k_n_mn50__he4_v47)*Y(jn)*state % rho - screened_rates(k_n_mn50__mn51) &
      *Y(jn)*state % rho - screened_rates(k_n_mn50__p_cr50)*Y(jn)*state % rho - &
      screened_rates(k_p_mn50__fe51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jmn50, scratch)

    scratch = (&
      screened_rates(k_n_co53__he4_mn50)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn50, jco53, scratch)

    scratch = (&
      screened_rates(k_n_co54__he4_mn51)*Y(jco54)*state % rho + screened_rates(k_n_fe51__p_mn51)* &
      Y(jfe51)*state % rho + screened_rates(k_n_mn50__mn51)*Y(jmn50)*state % rho - &
      screened_rates(k_n_mn51__he4_v48)*Y(jmn51)*state % rho - &
      screened_rates(k_n_mn51__mn52)*Y(jmn51)*state % rho - &
      screened_rates(k_n_mn51__p_cr51)*Y(jmn51)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jn, scratch)

    scratch = (&
      screened_rates(k_p_cr50__mn51)*Y(jcr50)*state % rho - screened_rates(k_p_mn51__fe52)* &
      Y(jmn51)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr48__p_mn51)*Y(jcr48)*state % rho - screened_rates(k_he4_mn51__co55)* &
      Y(jmn51)*state % rho - screened_rates(k_he4_mn51__p_fe54)*Y(jmn51)* &
      state % rho + screened_rates(k_he4_v47__mn51)*Y(jv47)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_v47__mn51)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jv47, scratch)

    scratch = (&
      screened_rates(k_he4_cr48__p_mn51)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jcr48, scratch)

    scratch = (&
      screened_rates(k_p_cr50__mn51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jcr50, scratch)

    scratch = (&
      screened_rates(k_n_mn50__mn51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jmn50, scratch)

    scratch = (&
      -screened_rates(k_he4_mn51__co55)*Y(jhe4)*state % rho - screened_rates(k_he4_mn51__p_fe54)* &
      Y(jhe4)*state % rho - screened_rates(k_mn51__cr51__weak__wc12) - &
      screened_rates(k_n_mn51__he4_v48)*Y(jn)*state % rho - screened_rates(k_n_mn51__mn52) &
      *Y(jn)*state % rho - screened_rates(k_n_mn51__p_cr51)*Y(jn)*state % rho - &
      screened_rates(k_p_mn51__fe52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jmn51, scratch)

    scratch = (&
      screened_rates(k_fe51__mn51__weak__wc12) + screened_rates(k_n_fe51__p_mn51)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jmn51, jfe51, scratch)

    scratch = (&
      screened_rates(k_n_co54__he4_mn51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn51, jco54, scratch)

    scratch = (&
      screened_rates(k_n_co55__he4_mn52)*Y(jco55)*state % rho + screened_rates(k_n_fe52__p_mn52)* &
      Y(jfe52)*state % rho + screened_rates(k_n_mn51__mn52)*Y(jmn51)*state % rho - &
      screened_rates(k_n_mn52__he4_v49)*Y(jmn52)*state % rho - &
      screened_rates(k_n_mn52__mn53)*Y(jmn52)*state % rho - &
      screened_rates(k_n_mn52__p_cr52)*Y(jmn52)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jn, scratch)

    scratch = (&
      screened_rates(k_p_cr51__mn52)*Y(jcr51)*state % rho - screened_rates(k_p_mn52__fe53)* &
      Y(jmn52)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr49__p_mn52)*Y(jcr49)*state % rho - screened_rates(k_he4_mn52__co56)* &
      Y(jmn52)*state % rho - screened_rates(k_he4_mn52__p_fe55)*Y(jmn52)* &
      state % rho + screened_rates(k_he4_v48__mn52)*Y(jv48)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_v48__mn52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jv48, scratch)

    scratch = (&
      screened_rates(k_he4_cr49__p_mn52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jcr49, scratch)

    scratch = (&
      screened_rates(k_p_cr51__mn52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jcr51, scratch)

    scratch = (&
      screened_rates(k_n_mn51__mn52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jmn51, scratch)

    scratch = (&
      -screened_rates(k_he4_mn52__co56)*Y(jhe4)*state % rho - screened_rates(k_he4_mn52__p_fe55)* &
      Y(jhe4)*state % rho - screened_rates(k_mn52__cr52__weak__wc12) - &
      screened_rates(k_n_mn52__he4_v49)*Y(jn)*state % rho - screened_rates(k_n_mn52__mn53) &
      *Y(jn)*state % rho - screened_rates(k_n_mn52__p_cr52)*Y(jn)*state % rho - &
      screened_rates(k_p_mn52__fe53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jmn52, scratch)

    scratch = (&
      screened_rates(k_fe52__mn52__weak__wc12) + screened_rates(k_n_fe52__p_mn52)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jmn52, jfe52, scratch)

    scratch = (&
      screened_rates(k_n_co55__he4_mn52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn52, jco55, scratch)

    scratch = (&
      screened_rates(k_n_co56__he4_mn53)*Y(jco56)*state % rho + screened_rates(k_n_fe53__p_mn53)* &
      Y(jfe53)*state % rho + screened_rates(k_n_mn52__mn53)*Y(jmn52)*state % rho - &
      screened_rates(k_n_mn53__he4_v50)*Y(jmn53)*state % rho - &
      screened_rates(k_n_mn53__mn54)*Y(jmn53)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jn, scratch)

    scratch = (&
      screened_rates(k_p_cr52__mn53)*Y(jcr52)*state % rho - screened_rates(k_p_mn53__fe54)* &
      Y(jmn53)*state % rho - screened_rates(k_p_mn53__he4_cr50)*Y(jmn53)* &
      state % rho &
       )
    call set_jac_entry(jac, jmn53, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mn53__co57)*Y(jmn53)*state % rho - screened_rates(k_he4_mn53__p_fe56) &
      *Y(jmn53)*state % rho + screened_rates(k_he4_v49__mn53)*Y(jv49)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_v49__mn53)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jv49, scratch)

    scratch = (&
      screened_rates(k_p_cr52__mn53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jcr52, scratch)

    scratch = (&
      screened_rates(k_n_mn52__mn53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jmn52, scratch)

    scratch = (&
      -screened_rates(k_he4_mn53__co57)*Y(jhe4)*state % rho - screened_rates(k_he4_mn53__p_fe56)* &
      Y(jhe4)*state % rho - screened_rates(k_n_mn53__he4_v50)*Y(jn)*state % rho - &
      screened_rates(k_n_mn53__mn54)*Y(jn)*state % rho - screened_rates(k_p_mn53__fe54)* &
      Y(jp)*state % rho - screened_rates(k_p_mn53__he4_cr50)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jmn53, scratch)

    scratch = (&
      screened_rates(k_fe53__mn53__weak__wc12) + screened_rates(k_n_fe53__p_mn53)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jmn53, jfe53, scratch)

    scratch = (&
      screened_rates(k_n_co56__he4_mn53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn53, jco56, scratch)

    scratch = (&
      screened_rates(k_n_co57__he4_mn54)*Y(jco57)*state % rho + screened_rates(k_n_fe54__p_mn54)* &
      Y(jfe54)*state % rho + screened_rates(k_n_mn53__mn54)*Y(jmn53)*state % rho - &
      screened_rates(k_n_mn54__he4_v51)*Y(jmn54)*state % rho - &
      screened_rates(k_n_mn54__mn55)*Y(jmn54)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jn, scratch)

    scratch = (&
      -screened_rates(k_p_mn54__fe55)*Y(jmn54)*state % rho - screened_rates(k_p_mn54__he4_cr51)* &
      Y(jmn54)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_mn54__co58)*Y(jmn54)*state % rho + screened_rates(k_he4_v50__mn54)* &
      Y(jv50)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_v50__mn54)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jv50, scratch)

    scratch = (&
      screened_rates(k_n_mn53__mn54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jmn53, scratch)

    scratch = (&
      -screened_rates(k_he4_mn54__co58)*Y(jhe4)*state % rho - &
      screened_rates(k_mn54__fe54__weak__wc12) - screened_rates(k_n_mn54__he4_v51)* &
      Y(jn)*state % rho - screened_rates(k_n_mn54__mn55)*Y(jn)*state % rho - &
      screened_rates(k_p_mn54__fe55)*Y(jp)*state % rho - &
      screened_rates(k_p_mn54__he4_cr51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jmn54, scratch)

    scratch = (&
      screened_rates(k_n_fe54__p_mn54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jfe54, scratch)

    scratch = (&
      screened_rates(k_n_co57__he4_mn54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn54, jco57, scratch)

    scratch = (&
      screened_rates(k_n_co58__he4_mn55)*Y(jco58)*state % rho + screened_rates(k_n_fe55__p_mn55)* &
      Y(jfe55)*state % rho + screened_rates(k_n_mn54__mn55)*Y(jmn54)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jn, scratch)

    scratch = (&
      -screened_rates(k_p_mn55__fe56)*Y(jmn55)*state % rho - screened_rates(k_p_mn55__he4_cr52)* &
      Y(jmn55)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jp, scratch)

    scratch = (&
      screened_rates(k_he4_v51__mn55)*Y(jv51)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_v51__mn55)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jv51, scratch)

    scratch = (&
      screened_rates(k_n_mn54__mn55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jmn54, scratch)

    scratch = (&
      -screened_rates(k_p_mn55__fe56)*Y(jp)*state % rho - screened_rates(k_p_mn55__he4_cr52)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jmn55, scratch)

    scratch = (&
      screened_rates(k_fe55__mn55__weak__wc12) + screened_rates(k_n_fe55__p_mn55)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jmn55, jfe55, scratch)

    scratch = (&
      screened_rates(k_n_co58__he4_mn55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jmn55, jco58, scratch)

    scratch = (&
      -screened_rates(k_n_fe51__fe52)*Y(jfe51)*state % rho - screened_rates(k_n_fe51__he4_cr48)* &
      Y(jfe51)*state % rho - screened_rates(k_n_fe51__p_mn51)*Y(jfe51)*state % rho &
      + screened_rates(k_n_ni54__he4_fe51)*Y(jni54)*state % rho &
       )
    call set_jac_entry(jac, jfe51, jn, scratch)

    scratch = (&
      screened_rates(k_p_mn50__fe51)*Y(jmn50)*state % rho &
       )
    call set_jac_entry(jac, jfe51, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr47__fe51)*Y(jcr47)*state % rho - screened_rates(k_he4_fe51__ni55)* &
      Y(jfe51)*state % rho - screened_rates(k_he4_fe51__p_co54)*Y(jfe51)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe51, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cr47__fe51)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe51, jcr47, scratch)

    scratch = (&
      screened_rates(k_p_mn50__fe51)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe51, jmn50, scratch)

    scratch = (&
      -screened_rates(k_fe51__mn51__weak__wc12) - screened_rates(k_he4_fe51__ni55)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_fe51__p_co54)*Y(jhe4)*state % rho - &
      screened_rates(k_n_fe51__fe52)*Y(jn)*state % rho - &
      screened_rates(k_n_fe51__he4_cr48)*Y(jn)*state % rho - &
      screened_rates(k_n_fe51__p_mn51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe51, jfe51, scratch)

    scratch = (&
      screened_rates(k_n_ni54__he4_fe51)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe51, jni54, scratch)

    scratch = (&
      screened_rates(k_n_fe51__fe52)*Y(jfe51)*state % rho - screened_rates(k_n_fe52__fe53)* &
      Y(jfe52)*state % rho - screened_rates(k_n_fe52__he4_cr49)*Y(jfe52)* &
      state % rho - screened_rates(k_n_fe52__p_mn52)*Y(jfe52)*state % rho + &
      screened_rates(k_n_ni55__he4_fe52)*Y(jni55)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jn, scratch)

    scratch = (&
      -screened_rates(k_p_fe52__co53)*Y(jfe52)*state % rho + screened_rates(k_p_mn51__fe52)* &
      Y(jmn51)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr48__fe52)*Y(jcr48)*state % rho - screened_rates(k_he4_fe52__ni56)* &
      Y(jfe52)*state % rho - screened_rates(k_he4_fe52__p_co55)*Y(jfe52)* &
      state % rho + screened_rates(k_he4_mn49__p_fe52)*Y(jmn49)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cr48__fe52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jcr48, scratch)

    scratch = (&
      screened_rates(k_he4_mn49__p_fe52)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jmn49, scratch)

    scratch = (&
      screened_rates(k_p_mn51__fe52)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jmn51, scratch)

    scratch = (&
      screened_rates(k_n_fe51__fe52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jfe51, scratch)

    scratch = (&
      -screened_rates(k_fe52__mn52__weak__wc12) - screened_rates(k_he4_fe52__ni56)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_fe52__p_co55)*Y(jhe4)*state % rho - &
      screened_rates(k_n_fe52__fe53)*Y(jn)*state % rho - &
      screened_rates(k_n_fe52__he4_cr49)*Y(jn)*state % rho - &
      screened_rates(k_n_fe52__p_mn52)*Y(jn)*state % rho - screened_rates(k_p_fe52__co53)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jfe52, scratch)

    scratch = (&
      screened_rates(k_n_ni55__he4_fe52)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe52, jni55, scratch)

    scratch = (&
      screened_rates(k_n_co53__p_fe53)*Y(jco53)*state % rho + screened_rates(k_n_fe52__fe53)* &
      Y(jfe52)*state % rho - screened_rates(k_n_fe53__fe54)*Y(jfe53)*state % rho - &
      screened_rates(k_n_fe53__he4_cr50)*Y(jfe53)*state % rho - &
      screened_rates(k_n_fe53__p_mn53)*Y(jfe53)*state % rho + &
      screened_rates(k_n_ni56__he4_fe53)*Y(jni56)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jn, scratch)

    scratch = (&
      -screened_rates(k_p_fe53__co54)*Y(jfe53)*state % rho + screened_rates(k_p_mn52__fe53)* &
      Y(jmn52)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr49__fe53)*Y(jcr49)*state % rho - screened_rates(k_he4_fe53__ni57)* &
      Y(jfe53)*state % rho - screened_rates(k_he4_fe53__p_co56)*Y(jfe53)* &
      state % rho + screened_rates(k_he4_mn50__p_fe53)*Y(jmn50)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cr49__fe53)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jcr49, scratch)

    scratch = (&
      screened_rates(k_he4_mn50__p_fe53)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jmn50, scratch)

    scratch = (&
      screened_rates(k_p_mn52__fe53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jmn52, scratch)

    scratch = (&
      screened_rates(k_n_fe52__fe53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jfe52, scratch)

    scratch = (&
      -screened_rates(k_fe53__mn53__weak__wc12) - screened_rates(k_he4_fe53__ni57)*Y(jhe4)* &
      state % rho - screened_rates(k_he4_fe53__p_co56)*Y(jhe4)*state % rho - &
      screened_rates(k_n_fe53__fe54)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__he4_cr50)*Y(jn)*state % rho - &
      screened_rates(k_n_fe53__p_mn53)*Y(jn)*state % rho - screened_rates(k_p_fe53__co54)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jfe53, scratch)

    scratch = (&
      screened_rates(k_co53__fe53__weak__wc12) + screened_rates(k_n_co53__p_fe53)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe53, jco53, scratch)

    scratch = (&
      screened_rates(k_n_ni56__he4_fe53)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe53, jni56, scratch)

    scratch = (&
      screened_rates(k_n_co54__p_fe54)*Y(jco54)*state % rho + screened_rates(k_n_fe53__fe54)* &
      Y(jfe53)*state % rho - screened_rates(k_n_fe54__fe55)*Y(jfe54)*state % rho - &
      screened_rates(k_n_fe54__he4_cr51)*Y(jfe54)*state % rho - &
      screened_rates(k_n_fe54__p_mn54)*Y(jfe54)*state % rho + &
      screened_rates(k_n_ni57__he4_fe54)*Y(jni57)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jn, scratch)

    scratch = (&
      screened_rates(k_p_co57__he4_fe54)*Y(jco57)*state % rho - screened_rates(k_p_fe54__co55)* &
      Y(jfe54)*state % rho + screened_rates(k_p_mn53__fe54)*Y(jmn53)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr50__fe54)*Y(jcr50)*state % rho - screened_rates(k_he4_fe54__ni58)* &
      Y(jfe54)*state % rho + screened_rates(k_he4_mn51__p_fe54)*Y(jmn51)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe54, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cr50__fe54)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jcr50, scratch)

    scratch = (&
      screened_rates(k_he4_mn51__p_fe54)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jmn51, scratch)

    scratch = (&
      screened_rates(k_p_mn53__fe54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jmn53, scratch)

    scratch = (&
      screened_rates(k_mn54__fe54__weak__wc12) &
       )
    call set_jac_entry(jac, jfe54, jmn54, scratch)

    scratch = (&
      screened_rates(k_n_fe53__fe54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jfe53, scratch)

    scratch = (&
      -screened_rates(k_he4_fe54__ni58)*Y(jhe4)*state % rho - screened_rates(k_n_fe54__fe55)* &
      Y(jn)*state % rho - screened_rates(k_n_fe54__he4_cr51)*Y(jn)*state % rho - &
      screened_rates(k_n_fe54__p_mn54)*Y(jn)*state % rho - screened_rates(k_p_fe54__co55)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jfe54, scratch)

    scratch = (&
      screened_rates(k_co54__fe54__weak__wc12) + screened_rates(k_n_co54__p_fe54)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe54, jco54, scratch)

    scratch = (&
      screened_rates(k_p_co57__he4_fe54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jco57, scratch)

    scratch = (&
      screened_rates(k_n_ni57__he4_fe54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe54, jni57, scratch)

    scratch = (&
      screened_rates(k_n_co55__p_fe55)*Y(jco55)*state % rho + screened_rates(k_n_fe54__fe55)* &
      Y(jfe54)*state % rho - screened_rates(k_n_fe55__fe56)*Y(jfe55)*state % rho - &
      screened_rates(k_n_fe55__he4_cr52)*Y(jfe55)*state % rho - &
      screened_rates(k_n_fe55__p_mn55)*Y(jfe55)*state % rho + &
      screened_rates(k_n_ni58__he4_fe55)*Y(jni58)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jn, scratch)

    scratch = (&
      screened_rates(k_p_co58__he4_fe55)*Y(jco58)*state % rho - screened_rates(k_p_fe55__co56)* &
      Y(jfe55)*state % rho + screened_rates(k_p_mn54__fe55)*Y(jmn54)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr51__fe55)*Y(jcr51)*state % rho - screened_rates(k_he4_fe55__ni59)* &
      Y(jfe55)*state % rho + screened_rates(k_he4_mn52__p_fe55)*Y(jmn52)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe55, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cr51__fe55)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jcr51, scratch)

    scratch = (&
      screened_rates(k_he4_mn52__p_fe55)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jmn52, scratch)

    scratch = (&
      screened_rates(k_p_mn54__fe55)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jmn54, scratch)

    scratch = (&
      screened_rates(k_n_fe54__fe55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jfe54, scratch)

    scratch = (&
      -screened_rates(k_fe55__mn55__weak__wc12) - screened_rates(k_he4_fe55__ni59)*Y(jhe4)* &
      state % rho - screened_rates(k_n_fe55__fe56)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__he4_cr52)*Y(jn)*state % rho - &
      screened_rates(k_n_fe55__p_mn55)*Y(jn)*state % rho - screened_rates(k_p_fe55__co56)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jfe55, scratch)

    scratch = (&
      screened_rates(k_co55__fe55__weak__wc12) + screened_rates(k_n_co55__p_fe55)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe55, jco55, scratch)

    scratch = (&
      screened_rates(k_p_co58__he4_fe55)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jco58, scratch)

    scratch = (&
      screened_rates(k_n_ni58__he4_fe55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe55, jni58, scratch)

    scratch = (&
      screened_rates(k_n_co56__p_fe56)*Y(jco56)*state % rho + screened_rates(k_n_fe55__fe56)* &
      Y(jfe55)*state % rho + screened_rates(k_n_ni59__he4_fe56)*Y(jni59)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe56, jn, scratch)

    scratch = (&
      -screened_rates(k_p_fe56__co57)*Y(jfe56)*state % rho + screened_rates(k_p_mn55__fe56)* &
      Y(jmn55)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jp, scratch)

    scratch = (&
      screened_rates(k_he4_cr52__fe56)*Y(jcr52)*state % rho - screened_rates(k_he4_fe56__ni60)* &
      Y(jfe56)*state % rho + screened_rates(k_he4_mn53__p_fe56)*Y(jmn53)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe56, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_cr52__fe56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jcr52, scratch)

    scratch = (&
      screened_rates(k_he4_mn53__p_fe56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jmn53, scratch)

    scratch = (&
      screened_rates(k_p_mn55__fe56)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jmn55, scratch)

    scratch = (&
      screened_rates(k_n_fe55__fe56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jfe55, scratch)

    scratch = (&
      -screened_rates(k_he4_fe56__ni60)*Y(jhe4)*state % rho - screened_rates(k_p_fe56__co57)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jfe56, scratch)

    scratch = (&
      screened_rates(k_co56__fe56__weak__wc12) + screened_rates(k_n_co56__p_fe56)*Y(jn)* &
      state % rho &
       )
    call set_jac_entry(jac, jfe56, jco56, scratch)

    scratch = (&
      screened_rates(k_n_ni59__he4_fe56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jfe56, jni59, scratch)

    scratch = (&
      -screened_rates(k_n_co53__co54)*Y(jco53)*state % rho - screened_rates(k_n_co53__he4_mn50)* &
      Y(jco53)*state % rho - screened_rates(k_n_co53__p_fe53)*Y(jco53)*state % rho &
       )
    call set_jac_entry(jac, jco53, jn, scratch)

    scratch = (&
      -screened_rates(k_p_co53__ni54)*Y(jco53)*state % rho + screened_rates(k_p_fe52__co53)* &
      Y(jfe52)*state % rho &
       )
    call set_jac_entry(jac, jco53, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_co53__p_ni56)*Y(jco53)*state % rho + screened_rates(k_he4_mn49__co53) &
      *Y(jmn49)*state % rho &
       )
    call set_jac_entry(jac, jco53, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mn49__co53)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco53, jmn49, scratch)

    scratch = (&
      screened_rates(k_p_fe52__co53)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco53, jfe52, scratch)

    scratch = (&
      -screened_rates(k_co53__fe53__weak__wc12) - screened_rates(k_he4_co53__p_ni56)*Y(jhe4)* &
      state % rho - screened_rates(k_n_co53__co54)*Y(jn)*state % rho - &
      screened_rates(k_n_co53__he4_mn50)*Y(jn)*state % rho - &
      screened_rates(k_n_co53__p_fe53)*Y(jn)*state % rho - screened_rates(k_p_co53__ni54)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco53, jco53, scratch)

    scratch = (&
      screened_rates(k_n_co53__co54)*Y(jco53)*state % rho - screened_rates(k_n_co54__co55)* &
      Y(jco54)*state % rho - screened_rates(k_n_co54__he4_mn51)*Y(jco54)* &
      state % rho - screened_rates(k_n_co54__p_fe54)*Y(jco54)*state % rho + &
      screened_rates(k_n_ni54__p_co54)*Y(jni54)*state % rho &
       )
    call set_jac_entry(jac, jco54, jn, scratch)

    scratch = (&
      -screened_rates(k_p_co54__ni55)*Y(jco54)*state % rho + screened_rates(k_p_fe53__co54)* &
      Y(jfe53)*state % rho &
       )
    call set_jac_entry(jac, jco54, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_co54__p_ni57)*Y(jco54)*state % rho + &
      screened_rates(k_he4_fe51__p_co54)*Y(jfe51)*state % rho + &
      screened_rates(k_he4_mn50__co54)*Y(jmn50)*state % rho &
       )
    call set_jac_entry(jac, jco54, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mn50__co54)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco54, jmn50, scratch)

    scratch = (&
      screened_rates(k_he4_fe51__p_co54)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco54, jfe51, scratch)

    scratch = (&
      screened_rates(k_p_fe53__co54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco54, jfe53, scratch)

    scratch = (&
      screened_rates(k_n_co53__co54)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jco54, jco53, scratch)

    scratch = (&
      -screened_rates(k_co54__fe54__weak__wc12) - screened_rates(k_he4_co54__p_ni57)*Y(jhe4)* &
      state % rho - screened_rates(k_n_co54__co55)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__he4_mn51)*Y(jn)*state % rho - &
      screened_rates(k_n_co54__p_fe54)*Y(jn)*state % rho - screened_rates(k_p_co54__ni55)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco54, jco54, scratch)

    scratch = (&
      screened_rates(k_n_ni54__p_co54)*Y(jn)*state % rho + &
      screened_rates(k_ni54__co54__weak__wc12) &
       )
    call set_jac_entry(jac, jco54, jni54, scratch)

    scratch = (&
      screened_rates(k_n_co54__co55)*Y(jco54)*state % rho - screened_rates(k_n_co55__co56)* &
      Y(jco55)*state % rho - screened_rates(k_n_co55__he4_mn52)*Y(jco55)* &
      state % rho - screened_rates(k_n_co55__p_fe55)*Y(jco55)*state % rho + &
      screened_rates(k_n_ni55__p_co55)*Y(jni55)*state % rho &
       )
    call set_jac_entry(jac, jco55, jn, scratch)

    scratch = (&
      -screened_rates(k_p_co55__ni56)*Y(jco55)*state % rho + screened_rates(k_p_fe54__co55)* &
      Y(jfe54)*state % rho &
       )
    call set_jac_entry(jac, jco55, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_co55__p_ni58)*Y(jco55)*state % rho + &
      screened_rates(k_he4_fe52__p_co55)*Y(jfe52)*state % rho + &
      screened_rates(k_he4_mn51__co55)*Y(jmn51)*state % rho &
       )
    call set_jac_entry(jac, jco55, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mn51__co55)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco55, jmn51, scratch)

    scratch = (&
      screened_rates(k_he4_fe52__p_co55)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco55, jfe52, scratch)

    scratch = (&
      screened_rates(k_p_fe54__co55)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco55, jfe54, scratch)

    scratch = (&
      screened_rates(k_n_co54__co55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jco55, jco54, scratch)

    scratch = (&
      -screened_rates(k_co55__fe55__weak__wc12) - screened_rates(k_he4_co55__p_ni58)*Y(jhe4)* &
      state % rho - screened_rates(k_n_co55__co56)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__he4_mn52)*Y(jn)*state % rho - &
      screened_rates(k_n_co55__p_fe55)*Y(jn)*state % rho - screened_rates(k_p_co55__ni56)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco55, jco55, scratch)

    scratch = (&
      screened_rates(k_n_ni55__p_co55)*Y(jn)*state % rho + &
      screened_rates(k_ni55__co55__weak__wc12) &
       )
    call set_jac_entry(jac, jco55, jni55, scratch)

    scratch = (&
      screened_rates(k_n_co55__co56)*Y(jco55)*state % rho - screened_rates(k_n_co56__co57)* &
      Y(jco56)*state % rho - screened_rates(k_n_co56__he4_mn53)*Y(jco56)* &
      state % rho - screened_rates(k_n_co56__p_fe56)*Y(jco56)*state % rho + &
      screened_rates(k_n_ni56__p_co56)*Y(jni56)*state % rho &
       )
    call set_jac_entry(jac, jco56, jn, scratch)

    scratch = (&
      -screened_rates(k_p_co56__ni57)*Y(jco56)*state % rho + screened_rates(k_p_fe55__co56)* &
      Y(jfe55)*state % rho &
       )
    call set_jac_entry(jac, jco56, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_co56__p_ni59)*Y(jco56)*state % rho + &
      screened_rates(k_he4_fe53__p_co56)*Y(jfe53)*state % rho + &
      screened_rates(k_he4_mn52__co56)*Y(jmn52)*state % rho &
       )
    call set_jac_entry(jac, jco56, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mn52__co56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco56, jmn52, scratch)

    scratch = (&
      screened_rates(k_he4_fe53__p_co56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco56, jfe53, scratch)

    scratch = (&
      screened_rates(k_p_fe55__co56)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco56, jfe55, scratch)

    scratch = (&
      screened_rates(k_n_co55__co56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jco56, jco55, scratch)

    scratch = (&
      -screened_rates(k_co56__fe56__weak__wc12) - screened_rates(k_he4_co56__p_ni59)*Y(jhe4)* &
      state % rho - screened_rates(k_n_co56__co57)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__he4_mn53)*Y(jn)*state % rho - &
      screened_rates(k_n_co56__p_fe56)*Y(jn)*state % rho - screened_rates(k_p_co56__ni57)* &
      Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco56, jco56, scratch)

    scratch = (&
      screened_rates(k_n_ni56__p_co56)*Y(jn)*state % rho + &
      screened_rates(k_ni56__co56__weak__wc12) &
       )
    call set_jac_entry(jac, jco56, jni56, scratch)

    scratch = (&
      screened_rates(k_n_co56__co57)*Y(jco56)*state % rho - screened_rates(k_n_co57__co58)* &
      Y(jco57)*state % rho - screened_rates(k_n_co57__he4_mn54)*Y(jco57)* &
      state % rho + screened_rates(k_n_ni57__p_co57)*Y(jni57)*state % rho &
       )
    call set_jac_entry(jac, jco57, jn, scratch)

    scratch = (&
      -screened_rates(k_p_co57__he4_fe54)*Y(jco57)*state % rho - screened_rates(k_p_co57__ni58)* &
      Y(jco57)*state % rho + screened_rates(k_p_fe56__co57)*Y(jfe56)*state % rho &
       )
    call set_jac_entry(jac, jco57, jp, scratch)

    scratch = (&
      -screened_rates(k_he4_co57__p_ni60)*Y(jco57)*state % rho + screened_rates(k_he4_mn53__co57) &
      *Y(jmn53)*state % rho &
       )
    call set_jac_entry(jac, jco57, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mn53__co57)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco57, jmn53, scratch)

    scratch = (&
      screened_rates(k_p_fe56__co57)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco57, jfe56, scratch)

    scratch = (&
      screened_rates(k_n_co56__co57)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jco57, jco56, scratch)

    scratch = (&
      -screened_rates(k_he4_co57__p_ni60)*Y(jhe4)*state % rho - screened_rates(k_n_co57__co58)* &
      Y(jn)*state % rho - screened_rates(k_n_co57__he4_mn54)*Y(jn)*state % rho - &
      screened_rates(k_p_co57__he4_fe54)*Y(jp)*state % rho - &
      screened_rates(k_p_co57__ni58)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco57, jco57, scratch)

    scratch = (&
      screened_rates(k_n_ni57__p_co57)*Y(jn)*state % rho + &
      screened_rates(k_ni57__co57__weak__wc12) &
       )
    call set_jac_entry(jac, jco57, jni57, scratch)

    scratch = (&
      screened_rates(k_n_co57__co58)*Y(jco57)*state % rho - screened_rates(k_n_co58__he4_mn55)* &
      Y(jco58)*state % rho + screened_rates(k_n_ni58__p_co58)*Y(jni58)*state % rho &
       )
    call set_jac_entry(jac, jco58, jn, scratch)

    scratch = (&
      -screened_rates(k_p_co58__he4_fe55)*Y(jco58)*state % rho - screened_rates(k_p_co58__ni59)* &
      Y(jco58)*state % rho &
       )
    call set_jac_entry(jac, jco58, jp, scratch)

    scratch = (&
      screened_rates(k_he4_mn54__co58)*Y(jmn54)*state % rho &
       )
    call set_jac_entry(jac, jco58, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_mn54__co58)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jco58, jmn54, scratch)

    scratch = (&
      screened_rates(k_n_co57__co58)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jco58, jco57, scratch)

    scratch = (&
      -screened_rates(k_co58__ni58__weak__mo03) - screened_rates(k_n_co58__he4_mn55)*Y(jn)* &
      state % rho - screened_rates(k_p_co58__he4_fe55)*Y(jp)*state % rho - &
      screened_rates(k_p_co58__ni59)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jco58, jco58, scratch)

    scratch = (&
      screened_rates(k_n_ni58__p_co58)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jco58, jni58, scratch)

    scratch = (&
      -screened_rates(k_n_ni54__he4_fe51)*Y(jni54)*state % rho - screened_rates(k_n_ni54__ni55)* &
      Y(jni54)*state % rho - screened_rates(k_n_ni54__p_co54)*Y(jni54)*state % rho &
       )
    call set_jac_entry(jac, jni54, jn, scratch)

    scratch = (&
      screened_rates(k_p_co53__ni54)*Y(jco53)*state % rho &
       )
    call set_jac_entry(jac, jni54, jp, scratch)

    scratch = (&
      screened_rates(k_p_co53__ni54)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jni54, jco53, scratch)

    scratch = (&
      -screened_rates(k_n_ni54__he4_fe51)*Y(jn)*state % rho - screened_rates(k_n_ni54__ni55)* &
      Y(jn)*state % rho - screened_rates(k_n_ni54__p_co54)*Y(jn)*state % rho - &
      screened_rates(k_ni54__co54__weak__wc12) &
       )
    call set_jac_entry(jac, jni54, jni54, scratch)

    scratch = (&
      screened_rates(k_n_ni54__ni55)*Y(jni54)*state % rho - screened_rates(k_n_ni55__he4_fe52)* &
      Y(jni55)*state % rho - screened_rates(k_n_ni55__ni56)*Y(jni55)*state % rho - &
      screened_rates(k_n_ni55__p_co55)*Y(jni55)*state % rho &
       )
    call set_jac_entry(jac, jni55, jn, scratch)

    scratch = (&
      screened_rates(k_p_co54__ni55)*Y(jco54)*state % rho &
       )
    call set_jac_entry(jac, jni55, jp, scratch)

    scratch = (&
      screened_rates(k_he4_fe51__ni55)*Y(jfe51)*state % rho &
       )
    call set_jac_entry(jac, jni55, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_fe51__ni55)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni55, jfe51, scratch)

    scratch = (&
      screened_rates(k_p_co54__ni55)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jni55, jco54, scratch)

    scratch = (&
      screened_rates(k_n_ni54__ni55)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni55, jni54, scratch)

    scratch = (&
      -screened_rates(k_n_ni55__he4_fe52)*Y(jn)*state % rho - screened_rates(k_n_ni55__ni56)* &
      Y(jn)*state % rho - screened_rates(k_n_ni55__p_co55)*Y(jn)*state % rho - &
      screened_rates(k_ni55__co55__weak__wc12) &
       )
    call set_jac_entry(jac, jni55, jni55, scratch)

    scratch = (&
      screened_rates(k_n_ni55__ni56)*Y(jni55)*state % rho - screened_rates(k_n_ni56__he4_fe53)* &
      Y(jni56)*state % rho - screened_rates(k_n_ni56__ni57)*Y(jni56)*state % rho - &
      screened_rates(k_n_ni56__p_co56)*Y(jni56)*state % rho &
       )
    call set_jac_entry(jac, jni56, jn, scratch)

    scratch = (&
      screened_rates(k_p_co55__ni56)*Y(jco55)*state % rho &
       )
    call set_jac_entry(jac, jni56, jp, scratch)

    scratch = (&
      screened_rates(k_he4_co53__p_ni56)*Y(jco53)*state % rho + screened_rates(k_he4_fe52__ni56)* &
      Y(jfe52)*state % rho &
       )
    call set_jac_entry(jac, jni56, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_fe52__ni56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni56, jfe52, scratch)

    scratch = (&
      screened_rates(k_he4_co53__p_ni56)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni56, jco53, scratch)

    scratch = (&
      screened_rates(k_p_co55__ni56)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jni56, jco55, scratch)

    scratch = (&
      screened_rates(k_n_ni55__ni56)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni56, jni55, scratch)

    scratch = (&
      -screened_rates(k_n_ni56__he4_fe53)*Y(jn)*state % rho - screened_rates(k_n_ni56__ni57)* &
      Y(jn)*state % rho - screened_rates(k_n_ni56__p_co56)*Y(jn)*state % rho - &
      screened_rates(k_ni56__co56__weak__wc12) &
       )
    call set_jac_entry(jac, jni56, jni56, scratch)

    scratch = (&
      screened_rates(k_n_ni56__ni57)*Y(jni56)*state % rho - screened_rates(k_n_ni57__he4_fe54)* &
      Y(jni57)*state % rho - screened_rates(k_n_ni57__ni58)*Y(jni57)*state % rho - &
      screened_rates(k_n_ni57__p_co57)*Y(jni57)*state % rho &
       )
    call set_jac_entry(jac, jni57, jn, scratch)

    scratch = (&
      screened_rates(k_p_co56__ni57)*Y(jco56)*state % rho &
       )
    call set_jac_entry(jac, jni57, jp, scratch)

    scratch = (&
      screened_rates(k_he4_co54__p_ni57)*Y(jco54)*state % rho + screened_rates(k_he4_fe53__ni57)* &
      Y(jfe53)*state % rho &
       )
    call set_jac_entry(jac, jni57, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_fe53__ni57)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni57, jfe53, scratch)

    scratch = (&
      screened_rates(k_he4_co54__p_ni57)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni57, jco54, scratch)

    scratch = (&
      screened_rates(k_p_co56__ni57)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jni57, jco56, scratch)

    scratch = (&
      screened_rates(k_n_ni56__ni57)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni57, jni56, scratch)

    scratch = (&
      -screened_rates(k_n_ni57__he4_fe54)*Y(jn)*state % rho - screened_rates(k_n_ni57__ni58)* &
      Y(jn)*state % rho - screened_rates(k_n_ni57__p_co57)*Y(jn)*state % rho - &
      screened_rates(k_ni57__co57__weak__wc12) &
       )
    call set_jac_entry(jac, jni57, jni57, scratch)

    scratch = (&
      screened_rates(k_n_ni57__ni58)*Y(jni57)*state % rho - screened_rates(k_n_ni58__he4_fe55)* &
      Y(jni58)*state % rho - screened_rates(k_n_ni58__ni59)*Y(jni58)*state % rho - &
      screened_rates(k_n_ni58__p_co58)*Y(jni58)*state % rho &
       )
    call set_jac_entry(jac, jni58, jn, scratch)

    scratch = (&
      screened_rates(k_p_co57__ni58)*Y(jco57)*state % rho &
       )
    call set_jac_entry(jac, jni58, jp, scratch)

    scratch = (&
      screened_rates(k_he4_co55__p_ni58)*Y(jco55)*state % rho + screened_rates(k_he4_fe54__ni58)* &
      Y(jfe54)*state % rho &
       )
    call set_jac_entry(jac, jni58, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_fe54__ni58)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni58, jfe54, scratch)

    scratch = (&
      screened_rates(k_he4_co55__p_ni58)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni58, jco55, scratch)

    scratch = (&
      screened_rates(k_p_co57__ni58)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jni58, jco57, scratch)

    scratch = (&
      screened_rates(k_co58__ni58__weak__mo03) &
       )
    call set_jac_entry(jac, jni58, jco58, scratch)

    scratch = (&
      screened_rates(k_n_ni57__ni58)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni58, jni57, scratch)

    scratch = (&
      -screened_rates(k_n_ni58__he4_fe55)*Y(jn)*state % rho - screened_rates(k_n_ni58__ni59)* &
      Y(jn)*state % rho - screened_rates(k_n_ni58__p_co58)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni58, jni58, scratch)

    scratch = (&
      screened_rates(k_n_ni58__ni59)*Y(jni58)*state % rho - screened_rates(k_n_ni59__he4_fe56)* &
      Y(jni59)*state % rho - screened_rates(k_n_ni59__ni60)*Y(jni59)*state % rho &
       )
    call set_jac_entry(jac, jni59, jn, scratch)

    scratch = (&
      screened_rates(k_p_co58__ni59)*Y(jco58)*state % rho &
       )
    call set_jac_entry(jac, jni59, jp, scratch)

    scratch = (&
      screened_rates(k_he4_co56__p_ni59)*Y(jco56)*state % rho + screened_rates(k_he4_fe55__ni59)* &
      Y(jfe55)*state % rho &
       )
    call set_jac_entry(jac, jni59, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_fe55__ni59)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni59, jfe55, scratch)

    scratch = (&
      screened_rates(k_he4_co56__p_ni59)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni59, jco56, scratch)

    scratch = (&
      screened_rates(k_p_co58__ni59)*Y(jp)*state % rho &
       )
    call set_jac_entry(jac, jni59, jco58, scratch)

    scratch = (&
      screened_rates(k_n_ni58__ni59)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni59, jni58, scratch)

    scratch = (&
      -screened_rates(k_n_ni59__he4_fe56)*Y(jn)*state % rho - screened_rates(k_n_ni59__ni60)* &
      Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni59, jni59, scratch)

    scratch = (&
      screened_rates(k_n_ni59__ni60)*Y(jni59)*state % rho &
       )
    call set_jac_entry(jac, jni60, jn, scratch)

    scratch = (&
      screened_rates(k_he4_co57__p_ni60)*Y(jco57)*state % rho + screened_rates(k_he4_fe56__ni60)* &
      Y(jfe56)*state % rho &
       )
    call set_jac_entry(jac, jni60, jhe4, scratch)

    scratch = (&
      screened_rates(k_he4_fe56__ni60)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni60, jfe56, scratch)

    scratch = (&
      screened_rates(k_he4_co57__p_ni60)*Y(jhe4)*state % rho &
       )
    call set_jac_entry(jac, jni60, jco57, scratch)

    scratch = (&
      screened_rates(k_n_ni59__ni60)*Y(jn)*state % rho &
       )
    call set_jac_entry(jac, jni60, jni59, scratch)


  end subroutine jac_nuc

end module actual_rhs_module
