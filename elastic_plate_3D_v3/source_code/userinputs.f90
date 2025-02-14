!***********************************************************************

	! State variables  
	! 1 to 6: sig11, sig22, sig33, sig12, sig13, sig23
	! 7 to 12: stran11, stran22, stran33, stran12, stran13, stran23
	! 11, AR11_stran13, AR11_stran13
	! 12, AR12_stran23, AR12_stran23
	! 13, AR13_eqplas, AR13_eqplas
	! 14, AR14_sig_H, AR14_sig_H
	! 15, AR15_sig_H_grad_X, AR15_sig_H_grad_X
	! 16, AR16_sig_H_grad_Y, AR16_sig_H_grad_Y
	! 17, AR17_sig_H_grad_Z, AR17_sig_H_grad_Z
	! 18, AR18_sig_vonMises, AR18_sig_vonMises
	! 19, AR19_sig_Tresca, AR19_sig_Tresca
	! 20, AR20_sig_P1, AR20_sig_P1
	! 21, AR21_sig_P2, AR21_sig_P2
	! 22, AR22_sig_P3, AR22_sig_P3
	! 23, AR23_triax, AR23_triax
	! 24, AR24_lode, AR24_lode
	! 25, AR25_C_mol, AR25_C_mol
	! 26, AR26_CL_mol, AR26_CL_mol
	! 27, AR27_CT_mol, AR27_CT_mol
	! 28, AR28_C_wtppm, AR28_C_wtppm
	! 29, AR29_CL_wtppm, AR29_CL_wtppm
	! 30, AR30_CT_wtppm, AR30_CT_wtppm
	! 31, AR31_thetaL, AR31_thetaL
	! 32, AR32_thetaT_dis, AR32_thetaT_dis
	! 33, AR33_mu, AR33_mu
	! 34, AR34_theta_coverage, AR34_theta_coverage
	! 35, AR35_k_HEHE, AR35_k_HEHE
	! 36, AR36_dCT_dCL, AR36_dCT_dCL
	! 37, AR37_dCT_deqplas, AR37_dCT_deqplas

module userinputs
	use precision
	implicit none
	! THESE TWO VALUES ARE HARD-CODED
	! YOU MUST CHANGE IT TO THE ACTUAL NUMBER OF ELEMENTS AND NODES IN .INP FILE
	! YOU CAN USE PYTHON SCRIPTING TO CHANGE VALUES AS WELL

	integer, parameter :: total_elems = 3026 ! Storing the actual number of elements
	integer, parameter :: total_nodes = 4836 ! Storing the actual number of nodes

	! Subset of SDVs indices that you want to output 
	integer, parameter :: uvar_indices(16) = (/13, 14, 15, 16, 17, 18, 19, 20, 23, 24, 25, 26, 27, 31, 32, 33/)

	! Index of statev in UMAT and UMATHT
	integer, parameter :: sig_start_idx = 1 ! Starting index of the stress component in statev
	integer, parameter :: sig_end_idx = 6 ! Ending index of the strain component in statev
	integer, parameter :: stran_start_idx = 7 ! Starting index of the total strain component in statev
	integer, parameter :: stran_end_idx = 12 ! Ending index of the total strain component in statev
	integer, parameter :: eqplas_idx = 13 ! Index of the eqplas in statev
	integer, parameter :: sig_H_idx = 14 ! Index of the sig_H in statev
	integer, parameter :: sig_H_grad_X_idx = 15 ! Index of the sig_H_grad_X in statev
	integer, parameter :: sig_H_grad_Y_idx = 16 ! Index of the sig_H_grad_Y in statev
	integer, parameter :: sig_H_grad_Z_idx = 17 ! Index of the sig_H_grad_Z in statev
	integer, parameter :: sig_vonMises_idx = 18 ! Index of the sig_vonMises in statev
	integer, parameter :: sig_Tresca_idx = 19 ! Index of the sig_Tresca in statev
	integer, parameter :: sig_P1_idx = 20 ! Index of the sig_P1 in statev
	integer, parameter :: sig_P2_idx = 21 ! Index of the sig_P2 in statev
	integer, parameter :: sig_P3_idx = 22 ! Index of the sig_P3 in statev
	integer, parameter :: triax_idx = 23 ! Index of the triax in statev
	integer, parameter :: lode_idx = 24 ! Index of the lode in statev
	integer, parameter :: C_mol_idx = 25 ! Index of the C_mol in statev
	integer, parameter :: CL_mol_idx = 26 ! Index of the CL_mol in statev
	integer, parameter :: CT_mol_idx = 27 ! Index of the CT_mol in statev
	integer, parameter :: C_wtppm_idx = 28 ! Index of the C_wtppm in statev
	integer, parameter :: CL_wtppm_idx = 29 ! Index of the CL_wtppm in statev
	integer, parameter :: CT_wtppm_idx = 30 ! Index of the CT_wtppm in statev
	integer, parameter :: thetaL_idx = 31 ! Index of the thetaL in statev
	integer, parameter :: thetaT_dis_idx = 32 ! Index of the thetaT_dis in statev
	integer, parameter :: mu_idx = 33 ! Index of the mu in statev
	integer, parameter :: theta_coverage_idx = 34 ! Index of the theta_coverage in statev
	integer, parameter :: k_HEHE_idx = 35 ! Index of the k_HEHE in statev
	integer, parameter :: dCT_dCL_idx = 36 ! Index of the dCT_dCL in statev
	integer, parameter :: dCT_deqplas_idx = 37 ! Index of the dCT_deqplas in statev

	integer, parameter :: before_flow_props_idx = 8 ! Index of the first flow curve data in props in UMAT

end module
