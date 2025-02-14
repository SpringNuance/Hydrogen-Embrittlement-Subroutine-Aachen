1

   Abaqus 2023.HF4                                  Date 14-Feb-2025   Time 17:21:23
   For use by AALTO UNIVERSITY under license from Dassault Systemes or its subsidiary.

                                                                                 
                                                                                 
 STEP    1     INCREMENT     1     STEP TIME    0.00    


                        S T E P       1     C O U P L E D - T E M P E R A T U R E - D I S P L A C E M E N T   A N A L Y S I S


                                                                                          

     AUTOMATIC TIME CONTROL WITH -
          A SUGGESTED INITIAL TIME INCREMENT OF                5.000E+06
          AND A TOTAL TIME PERIOD OF                           1.000E+10
          THE MINIMUM TIME INCREMENT ALLOWED IS                1.000E+06
          THE MAXIMUM TIME INCREMENT ALLOWED IS                1.000E+07

          CREEP AND SWELLING EFFECTS ARE OMITTED IN THIS STEP

     THE SIZE OF THE TIME INCREMENT IS CONTROLLED BY -
          THE TEMPERATURE CHANGE PER INCREMENT NOT EXCEEDING            10.0    
          THE RATE DEPENDENCY WILL BE CONSIDERED IN THIS STEP

          CREEP AND SWELLING EFFECTS WILL BE IGNORED.

 CONVERGENCE TOLERANCE PARAMETERS FOR FORCE    
     CRITERION FOR RESIDUAL FORCE     FOR A NONLINEAR PROBLEM          5.000E-03
     CRITERION FOR DISP.    CORRECTION IN A NONLINEAR PROBLEM          1.000E-02
     INITIAL VALUE OF TIME AVERAGE FORCE                               1.000E-02
     AVERAGE FORCE     IS TIME AVERAGE FORCE    
     ALTERNATE CRIT. FOR RESIDUAL FORCE     FOR A NONLINEAR PROBLEM    2.000E-02
     CRITERION FOR ZERO FORCE     RELATIVE TO TIME AVRG. FORCE         1.000E-05
     CRITERION FOR RESIDUAL FORCE     WHEN THERE IS ZERO FLUX          1.000E-05
     CRITERION FOR DISP.    CORRECTION WHEN THERE IS ZERO FLUX         1.000E-03
     CRITERION FOR RESIDUAL FORCE     FOR A LINEAR INCREMENT           1.000E-08
     FIELD CONVERSION RATIO                                             1.00    
     CRITERION FOR ZERO FORCE     REL. TO TIME AVRG. MAX. FORCE        1.000E-05
     CRITERION FOR ZERO DISP.    RELATIVE TO CHARACTERISTIC LENGTH     1.000E-08

 CONVERGENCE TOLERANCE PARAMETERS FOR HEAT FLUX
 *** CRITERION FOR RESIDUAL HEAT FLUX FOR A NONLINEAR PROBLEM          5.000E-12
 *** CRITERION FOR TEMP.    CORRECTION IN A NONLINEAR PROBLEM          1.000E-09
     INITIAL VALUE OF TIME AVERAGE HEAT FLUX                           1.000E-02
     AVERAGE HEAT FLUX IS TIME AVERAGE HEAT FLUX
     ALTERNATE CRIT. FOR RESIDUAL HEAT FLUX FOR A NONLINEAR PROBLEM    2.000E-02
 *** CRITERION FOR ZERO HEAT FLUX RELATIVE TO TIME AVRG. HEAT FLUX     1.000E-12
 *** CRITERION FOR RESIDUAL HEAT FLUX WHEN THERE IS ZERO FLUX          1.000E-12
     CRITERION FOR TEMP.    CORRECTION WHEN THERE IS ZERO FLUX         1.000E-03
     CRITERION FOR RESIDUAL HEAT FLUX FOR A LINEAR INCREMENT           1.000E-08
     FIELD CONVERSION RATIO                                             1.00    
 *** CRITERION FOR ZERO HEAT FLUX REL. TO TIME AVRG. MAX. HEAT FLUX    1.000E-12

     VOLUMETRIC STRAIN COMPATIBILITY TOLERANCE FOR HYBRID SOLIDS       1.000E-05
     AXIAL STRAIN COMPATIBILITY TOLERANCE FOR HYBRID BEAMS             1.000E-05
     TRANS. SHEAR STRAIN COMPATIBILITY TOLERANCE FOR HYBRID BEAMS      1.000E-05
     SOFT CONTACT CONSTRAINT COMPATIBILITY TOLERANCE FOR P>P0          5.000E-03
     SOFT CONTACT CONSTRAINT COMPATIBILITY TOLERANCE FOR P=0.0         0.100    
     CONTACT FORCE ERROR TOLERANCE FOR CONVERT SDI=YES                 1.00    
     DISPLACEMENT COMPATIBILITY TOLERANCE FOR DCOUP ELEMENTS           1.000E-05
     ROTATION COMPATIBILITY TOLERANCE FOR DCOUP ELEMENTS               1.000E-05

 *** INDICATES USE OF NON-DEFAULT CONVERGENCE TOLERANCE PARAMETER
     SELECTED WITH *CONTROLS

 EQUILIBRIUM WILL BE CHECKED FOR SEVERE DISCONTINUITY ITERATIONS

 TIME INCREMENTATION CONTROL PARAMETERS:
 *** FIRST EQUILIBRIUM ITERATION FOR CONSECUTIVE DIVERGENCE CHECK             16
     EQUILIBRIUM ITERATION AT WHICH LOG. CONVERGENCE RATE CHECK BEGINS         8
     EQUILIBRIUM ITERATION AFTER WHICH ALTERNATE RESIDUAL IS USED              9
     MAXIMUM EQUILIBRIUM ITERATIONS ALLOWED                                   16
     EQUILIBRIUM ITERATION COUNT FOR CUT-BACK IN NEXT INCREMENT               10
 *** MAXIMUM EQUILIB. ITERS IN TWO INCREMENTS FOR TIME INCREMENT INCREASE     16
     MAXIMUM ITERATIONS FOR SEVERE DISCONTINUITIES                            50
 *** MAXIMUM ATTEMPTS ALLOWED IN AN INCREMENT                                100
     MAXIMUM DISCON. ITERS IN TWO INCREMENTS FOR TIME INCREMENT INCREASE      50
     MAXIMUM CONTACT AUGMENTATIONS FOR *SURFACE BEHAVIOR,AUGMENTED LAGRANGE   50
     CONSECUTIVE INCS MEETING TIME INTEG. TOL. FOR TIME INC. INCREASE          3
     CUT-BACK FACTOR AFTER DIVERGENCE                                 0.2500    
     CUT-BACK FACTOR FOR TOO SLOW CONVERGENCE                         0.5000    
     CUT-BACK FACTOR AFTER TOO MANY EQUILIBRIUM ITERATIONS            0.7500    
     CUT-BACK FACTOR AFTER TOO MANY SEVERE DISCONTINUITY ITERATIONS   0.2500    
     CUT-BACK FACTOR AFTER PROBLEMS IN ELEMENT ASSEMBLY               0.2500    
     INCREASE FACTOR AFTER TWO INCREMENTS THAT CONVERGE QUICKLY        1.500    
     MAX. TIME INCREMENT INCREASE FACTOR ALLOWED                       1.500    
     MAX. TIME INCREMENT INCREASE FACTOR ALLOWED (DYNAMICS)            1.250    
     MAX. TIME INCREMENT INCREASE FACTOR ALLOWED (DIFFUSION)           2.000    
     MINIMUM TIME INCREMENT RATIO FOR EXTRAPOLATION TO OCCUR          0.1000    
     CUT-BACK FACTOR WHEN TIME INTEGRATION ACCURACY TOL. EXCEEDED     0.8500    
     MAX. RATIO OF TIME INTEG. MEASURE TO TOL. FOR TIME INC. INCREASE 0.7500    
     INCREASE FACTOR FOR TIME INCREMENT WHEN POSSIBLE                 0.8000    
     MIN. TIME INC. INCREASE FACTOR IN LINEAR TRANSIENT PROBLEMS      0.9500    
     MAX. RATIO OF TIME INCREMENT TO STABILITY LIMIT                   1.000    
     FRACTION OF STABILITY LIMIT FOR NEW TIME INCREMENT               0.9500    
     TIME INCREMENT INCREASE FACTOR BEFORE A TIME POINT                1.000    

 *** INDICATES USE OF NON-DEFAULT TIME INCREMENTATION CONTROL PARAMETER
     SELECTED WITH *CONTROLS
     GLOBAL STABILIZATION CONTROL IS NOT USED

          PRINT OF INCREMENT NUMBER, TIME, ETC., EVERY    1  INCREMENTS

 LINE SEARCH TOLERANCE PARAMETERS:
 *** MAXIMUM NUMBER OF ZERO-FINDING ITERATIONS                                10
     MAXIMUM CORRECTION SCALE FACTOR                                    1.00    
     MINIMUM CORRECTION SCALE FACTOR                                   1.000E-04
     RESIDUAL REDUCTION FACTOR TO END LINE SEARCHING                   0.250    
     RATIO OF NEW TO OLD SCALE FACTORS TO END LINE SEARCHING           0.100    

 *** INDICATES USE OF NON-DEFAULT LINE SEARCH TOLERANCE PARAMETER
     SELECTED WITH *CONTROLS

     THE MAXIMUM NUMBER OF INCREMENTS IN THIS STEP IS                 *******

          LARGE DISPLACEMENT THEORY WILL BE USED

          UNSYMMETRIC MATRIX STORAGE AND SOLUTION WILL BE USED

     LINEAR EXTRAPOLATION WILL BE USED

     CHARACTERISTIC ELEMENT LENGTH     1.142E-03

     DETAILS REGARDING ACTUAL SOLUTION WAVEFRONT REQUESTED

     DETAILED OUTPUT OF DIAGNOSTICS TO DATABASE REQUESTED

     PRINT OF INCREMENT NUMBER, TIME, ETC., TO THE MESSAGE FILE EVERY     1  INCREMENTS

     COLLECTING MODEL CONSTRAINT INFORMATION FOR OVERCONSTRAINT CHECKS

     COLLECTING STEP CONSTRAINT INFORMATION FOR OVERCONSTRAINT CHECKS


  INCREMENT     1 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E+06
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 871 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 873 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 872 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 939 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1003 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 938 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 937 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 877 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 879 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 878 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 943 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 944 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 945 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1075 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1077 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1076 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1011 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1009 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1010 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1006 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 874 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 875 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 876 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 940 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 941 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 942 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1544 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1401 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1528 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1637 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1574 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1382 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1135 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1639 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1468 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1638 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1071 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1137 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1136 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1074 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1004 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1005 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1007 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1008 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1069 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1070 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1072 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1073 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: SOLVER PROBLEM.  ZERO PIVOT WHEN PROCESSING NODE 1138 INSTANCE 
             ELASTIC-HOLD-PLATE-1 D.O.F. 11
  Typically a zero pivot is associated with an overconstraint in the model. 
 However, Abaqus was not able to identify any chains of constraints that might 
 lead to an overconstraint at this node. This suggests that the model might be 
 in fact insufficiently constrained in this particular degree of freedom. 
 Please check the constraints used in this model. 


 ***WARNING: Solver problem. Zero pivot when processing D.O.F. 11 of 49 nodes. 
             The nodes have been identified in node set 
             WarnNodeSolvProbZeroPiv_11_1_1_1_1.

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       14.9       TIME AVG. FORCE        14.9    
 LARGEST SCALED RESIDUAL FORCE      9.541E-02   AT NODE        197   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.541E-02
 LARGEST INCREMENT OF DISP.         2.658E-05   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.658E-05   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          FORCE     EQUILIBRIUM NOT ACHIEVED WITHIN TOLERANCE.

 AVERAGE HEAT FLUX                  6.052E-29   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.235E-27   AT NODE       2236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -4.250E-15   AT NODE       2134   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -4.250E-15   AT NODE       2134   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     2


 AVERAGE FORCE                       14.9       TIME AVG. FORCE        14.9    
 LARGEST SCALED RESIDUAL FORCE     -3.102E-05   AT NODE        131   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.102E-05
 LARGEST INCREMENT OF DISP.         2.659E-05   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.140E-08   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM EQUATIONS HAVE CONVERGED

 AVERAGE HEAT FLUX                  7.312E-29   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.806E-27   AT NODE       2212   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         5.272E-15   AT NODE       3856   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        4.856E-15   AT NODE        489   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   5.272E-15

 ITERATION SUMMARY FOR THE INCREMENT:   2 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  2 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E+06,  FRACTION OF STEP COMPLETED  5.000E-04
 STEP TIME COMPLETED       5.000E+06,  TOTAL TIME COMPLETED        5.000E+06


  INCREMENT     2 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E+06
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.3    
 LARGEST SCALED RESIDUAL FORCE      0.110       AT NODE       1267   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      0.110    
 LARGEST INCREMENT OF DISP.        -1.245E-08   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.661E-05   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          FORCE     EQUILIBRIUM NOT ACHIEVED WITHIN TOLERANCE.

 AVERAGE HEAT FLUX                  1.355E-17   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         1.240E-17   AT NODE       3424   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.          1.35       AT NODE       3764   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.         1.35       AT NODE       3764   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     2


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.3    
 LARGEST SCALED RESIDUAL FORCE     -3.587E-05   AT NODE       1267   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.587E-05
 LARGEST INCREMENT OF DISP.         1.299E-12   AT NODE       3417   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.245E-08   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          FORCE     EQUILIB. ACCEPTED BASED ON SMALL RESIDUAL AND SMALL MAGNITUDE
           OF THE INCREMENT OF DISPLACEMENT

 AVERAGE HEAT FLUX                  1.370E-17   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -8.757E-21   AT NODE       3420   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.          1.35       AT NODE       3764   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        5.986E-04   AT NODE       3487   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS    1.35    

 ITERATION SUMMARY FOR THE INCREMENT:   2 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  2 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E+06,  FRACTION OF STEP COMPLETED  1.000E-03
 STEP TIME COMPLETED       1.000E+07,  TOTAL TIME COMPLETED        1.000E+07


  INCREMENT     3 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E+06
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.4    
 LARGEST SCALED RESIDUAL FORCE      1.103E-08   AT NODE       1274   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.103E-08
 LARGEST INCREMENT OF DISP.        -4.696E-12   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.294E-12   AT NODE       1353   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  1.404E-18   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -1.712E-23   AT NODE       4238   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.156E-02   AT NODE       3716   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        -1.33       AT NODE       3764   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.156E-02
          TIME INCREMENT MAY NOW INCREASE TO    1.000E+07

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E+06,  FRACTION OF STEP COMPLETED  1.500E-03
 STEP TIME COMPLETED       1.500E+07,  TOTAL TIME COMPLETED        1.500E+07


  INCREMENT     4 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.5    
 LARGEST SCALED RESIDUAL FORCE     -1.849E-08   AT NODE       1269   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.849E-08
 LARGEST INCREMENT OF DISP.         3.815E-15   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        9.395E-12   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  3.729E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         9.012E-24   AT NODE       4172   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -1.024E-03   AT NODE       1300   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -2.282E-02   AT NODE       3716   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -1.024E-03

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.500E-03
 STEP TIME COMPLETED       2.500E+07,  TOTAL TIME COMPLETED        2.500E+07


  INCREMENT     5 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.6    
 LARGEST SCALED RESIDUAL FORCE      1.955E-11   AT NODE       4173   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.955E-11
 LARGEST INCREMENT OF DISP.        -2.422E-15   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -6.237E-15   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.429E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -1.256E-26   AT NODE       4238   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.813E-04   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        9.173E-04   AT NODE       1334   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.813E-04

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.500E-03
 STEP TIME COMPLETED       3.500E+07,  TOTAL TIME COMPLETED        3.500E+07


  INCREMENT     6 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.6    
 LARGEST SCALED RESIDUAL FORCE     -6.771E-12   AT NODE       4173   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -6.771E-12
 LARGEST INCREMENT OF DISP.         2.298E-18   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.425E-15   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.143E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         4.382E-27   AT NODE       4238   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         4.574E-05   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.356E-04   AT NODE       2005   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   4.574E-05

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.500E-03
 STEP TIME COMPLETED       4.500E+07,  TOTAL TIME COMPLETED        4.500E+07


  INCREMENT     7 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.6    
 LARGEST SCALED RESIDUAL FORCE     -1.023E-12   AT NODE        446   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.023E-12
 LARGEST INCREMENT OF DISP.        -6.409E-19   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.939E-18   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.073E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.490E-27   AT NODE        443   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.136E-05   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -3.438E-05   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.136E-05

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.500E-03
 STEP TIME COMPLETED       5.500E+07,  TOTAL TIME COMPLETED        5.500E+07


  INCREMENT     8 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.6    
 LARGEST SCALED RESIDUAL FORCE      8.669E-13   AT NODE        218   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.669E-13
 LARGEST INCREMENT OF DISP.        -1.700E-20   AT NODE         66   DOF  1
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.401E-19   AT NODE       1341   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.056E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.453E-27   AT NODE       2262   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         2.860E-06   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -8.502E-06   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   2.860E-06

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.500E-03
 STEP TIME COMPLETED       6.500E+07,  TOTAL TIME COMPLETED        6.500E+07


  INCREMENT     9 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  118ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.6    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE        512   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.        -5.618E-21   AT NODE         51   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.057E-20   AT NODE         66   DOF  1
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.051E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.981E-27   AT NODE        278   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.307E-07   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -2.129E-06   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.307E-07

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.500E-03
 STEP TIME COMPLETED       7.500E+07,  TOTAL TIME COMPLETED        7.500E+07


  INCREMENT    10 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.6    
 LARGEST SCALED RESIDUAL FORCE     -1.350E-13   AT NODE        200   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.350E-13
 LARGEST INCREMENT OF DISP.         2.233E-21   AT NODE        196   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        7.661E-21   AT NODE         51   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.051E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.178E-27   AT NODE       1758   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.890E-07   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -5.417E-07   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.890E-07

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.500E-03
 STEP TIME COMPLETED       8.500E+07,  TOTAL TIME COMPLETED        8.500E+07


  INCREMENT    11 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        647   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         2.937E-21   AT NODE       1403   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.620E-21   AT NODE       1254   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.116E-27   AT NODE       1743   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         4.931E-08   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.396E-07   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   4.931E-08

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.500E-03
 STEP TIME COMPLETED       9.500E+07,  TOTAL TIME COMPLETED        9.500E+07


  INCREMENT    12 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.066E-13   AT NODE       1280   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.066E-13
 LARGEST INCREMENT OF DISP.        -7.180E-22   AT NODE       1502   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.602E-21   AT NODE       1500   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.294E-27   AT NODE        332   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.296E-08   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -3.635E-08   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.296E-08

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.050E-02
 STEP TIME COMPLETED       1.050E+08,  TOTAL TIME COMPLETED        1.050E+08


  INCREMENT    13 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1115   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -2.640E-21   AT NODE       1435   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.142E-21   AT NODE       1329   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.810E-27   AT NODE       1696   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         3.427E-09   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -9.535E-09   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   3.427E-09

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.150E-02
 STEP TIME COMPLETED       1.150E+08,  TOTAL TIME COMPLETED        1.150E+08


  INCREMENT    14 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.066E-13   AT NODE       1682   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.066E-13
 LARGEST INCREMENT OF DISP.         2.282E-21   AT NODE       1403   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.817E-21   AT NODE       1686   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.829E-27   AT NODE       1750   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         9.101E-10   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -2.516E-09   AT NODE       2094   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   9.101E-10

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.250E-02
 STEP TIME COMPLETED       1.250E+08,  TOTAL TIME COMPLETED        1.250E+08


  INCREMENT    15 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE        467   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.         1.179E-21   AT NODE        285   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.819E-21   AT NODE          7   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.883E-27   AT NODE       2159   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         2.427E-10   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -6.674E-10   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   2.427E-10

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.350E-02
 STEP TIME COMPLETED       1.350E+08,  TOTAL TIME COMPLETED        1.350E+08


  INCREMENT    16 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        101   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.266E-21   AT NODE       1193   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.051E-21   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.797E-27   AT NODE       1630   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.494E-11   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.778E-10   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.494E-11

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.450E-02
 STEP TIME COMPLETED       1.450E+08,  TOTAL TIME COMPLETED        1.450E+08


  INCREMENT    17 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE       1772   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.         2.643E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.232E-21   AT NODE        858   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.968E-27   AT NODE       2258   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.742E-11   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -4.752E-11   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.742E-11

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.550E-02
 STEP TIME COMPLETED       1.550E+08,  TOTAL TIME COMPLETED        1.550E+08


  INCREMENT    18 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE        218   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         1.272E-21   AT NODE         64   DOF  1
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.614E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.220E-27   AT NODE       1727   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         4.692E-12   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.274E-11   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   4.692E-12

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.650E-02
 STEP TIME COMPLETED       1.650E+08,  TOTAL TIME COMPLETED        1.650E+08


  INCREMENT    19 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        590   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.344E-21   AT NODE        395   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.261E-21   AT NODE        448   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.944E-27   AT NODE        212   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.263E-12   AT NODE       2032   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -3.434E-12   AT NODE       2092   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.263E-12

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.750E-02
 STEP TIME COMPLETED       1.750E+08,  TOTAL TIME COMPLETED        1.750E+08


  INCREMENT    20 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.105E-14   AT NODE        113   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.105E-14
 LARGEST INCREMENT OF DISP.         3.400E-21   AT NODE         54   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.652E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.043E-27   AT NODE       2216   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         3.446E-13   AT NODE       1941   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -9.283E-13   AT NODE       2032   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   3.446E-13

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.850E-02
 STEP TIME COMPLETED       1.850E+08,  TOTAL TIME COMPLETED        1.850E+08


  INCREMENT    21 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        686   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -3.061E-21   AT NODE        131   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -6.345E-21   AT NODE         54   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.310E-27   AT NODE       2244   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         9.572E-14   AT NODE       2032   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -2.564E-13   AT NODE       1797   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   9.572E-14

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  1.950E-02
 STEP TIME COMPLETED       1.950E+08,  TOTAL TIME COMPLETED        1.950E+08


  INCREMENT    22 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.137E-13   AT NODE       1276   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.137E-13
 LARGEST INCREMENT OF DISP.         1.081E-21   AT NODE         21   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.877E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.317E-27   AT NODE       1692   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         2.985E-14   AT NODE       1994   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -7.478E-14   AT NODE       2020   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   2.985E-14

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.050E-02
 STEP TIME COMPLETED       2.050E+08,  TOTAL TIME COMPLETED        2.050E+08


  INCREMENT    23 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.776E-13   AT NODE       1277   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.776E-13
 LARGEST INCREMENT OF DISP.        -1.704E-21   AT NODE       1530   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.188E-21   AT NODE        528   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.301E-27   AT NODE       1697   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         1.253E-14   AT NODE       2012   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        2.680E-14   AT NODE       1448   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   1.253E-14

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.150E-02
 STEP TIME COMPLETED       2.150E+08,  TOTAL TIME COMPLETED        2.150E+08


  INCREMENT    24 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE       2246   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.         1.809E-21   AT NODE       1248   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.025E-21   AT NODE       1530   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.214E-27   AT NODE       1669   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         8.373E-15   AT NODE       2089   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.678E-14   AT NODE       2012   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   8.373E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.250E-02
 STEP TIME COMPLETED       2.250E+08,  TOTAL TIME COMPLETED        2.250E+08


  INCREMENT    25 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        143   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -2.785E-21   AT NODE         61   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.152E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.973E-27   AT NODE       1743   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.851E-15   AT NODE       3248   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.170E-14   AT NODE       1497   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.851E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.350E-02
 STEP TIME COMPLETED       2.350E+08,  TOTAL TIME COMPLETED        2.350E+08


  INCREMENT    26 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.137E-13   AT NODE       2246   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.137E-13
 LARGEST INCREMENT OF DISP.         3.868E-21   AT NODE         61   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.653E-21   AT NODE         61   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.026E-27   AT NODE        389   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.724E-15   AT NODE       3729   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.166E-14   AT NODE       1132   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.724E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.450E-02
 STEP TIME COMPLETED       2.450E+08,  TOTAL TIME COMPLETED        2.450E+08


  INCREMENT    27 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.137E-13   AT NODE       1276   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.137E-13
 LARGEST INCREMENT OF DISP.        -2.534E-21   AT NODE        261   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -6.082E-21   AT NODE        261   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.144E-27   AT NODE       1774   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.867E-15   AT NODE         64   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.235E-14   AT NODE         64   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.867E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.550E-02
 STEP TIME COMPLETED       2.550E+08,  TOTAL TIME COMPLETED        2.550E+08


  INCREMENT    28 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.066E-13   AT NODE       1682   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.066E-13
 LARGEST INCREMENT OF DISP.         1.926E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.370E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.953E-27   AT NODE       1643   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.541E-15   AT NODE       1835   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.179E-14   AT NODE          7   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.541E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.650E-02
 STEP TIME COMPLETED       2.650E+08,  TOTAL TIME COMPLETED        2.650E+08


  INCREMENT    29 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.350E-13   AT NODE       1276   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.350E-13
 LARGEST INCREMENT OF DISP.        -3.177E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.973E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.391E-27   AT NODE       2154   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.820E-15   AT NODE       1885   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.233E-14   AT NODE       1885   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.820E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.750E-02
 STEP TIME COMPLETED       2.750E+08,  TOTAL TIME COMPLETED        2.750E+08


  INCREMENT    30 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        770   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.         1.340E-21   AT NODE        658   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.073E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.865E-27   AT NODE       1701   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.745E-15   AT NODE       1946   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.322E-14   AT NODE       1946   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.745E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.850E-02
 STEP TIME COMPLETED       2.850E+08,  TOTAL TIME COMPLETED        2.850E+08


  INCREMENT    31 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  114ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.634E-13   AT NODE       1277   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.634E-13
 LARGEST INCREMENT OF DISP.         3.389E-21   AT NODE       1422   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.729E-21   AT NODE       1422   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.805E-27   AT NODE       1738   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.854E-15   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.343E-14   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.854E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  2.950E-02
 STEP TIME COMPLETED       2.950E+08,  TOTAL TIME COMPLETED        2.950E+08


  INCREMENT    32 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.208E-13   AT NODE       1276   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.208E-13
 LARGEST INCREMENT OF DISP.        -2.507E-21   AT NODE       1122   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.666E-21   AT NODE       1208   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.017E-27   AT NODE       1720   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.429E-15   AT NODE       3370   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.164E-14   AT NODE       2386   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.429E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.050E-02
 STEP TIME COMPLETED       3.050E+08,  TOTAL TIME COMPLETED        3.050E+08


  INCREMENT    33 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE       2187   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -2.698E-21   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -1.880E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.467E-27   AT NODE       2195   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.583E-15   AT NODE       3770   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.218E-14   AT NODE       3770   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.583E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.150E-02
 STEP TIME COMPLETED       3.150E+08,  TOTAL TIME COMPLETED        3.150E+08


  INCREMENT    34 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      5.507E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      5.507E-13
 LARGEST INCREMENT OF DISP.         2.711E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.504E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.955E-27   AT NODE       2244   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.679E-15   AT NODE       2124   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.203E-14   AT NODE       3772   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.679E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.250E-02
 STEP TIME COMPLETED       3.250E+08,  TOTAL TIME COMPLETED        3.250E+08


  INCREMENT    35 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.066E-13   AT NODE       1276   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.066E-13
 LARGEST INCREMENT OF DISP.         1.526E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.546E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.854E-27   AT NODE        824   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.161E-15   AT NODE       1485   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.299E-14   AT NODE       1485   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.161E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.350E-02
 STEP TIME COMPLETED       3.350E+08,  TOTAL TIME COMPLETED        3.350E+08


  INCREMENT    36 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        596   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.103E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.629E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.582E-27   AT NODE       1670   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.213E-15   AT NODE       2042   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.285E-14   AT NODE       2042   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.213E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.450E-02
 STEP TIME COMPLETED       3.450E+08,  TOTAL TIME COMPLETED        3.450E+08


  INCREMENT    37 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      6.111E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      6.111E-13
 LARGEST INCREMENT OF DISP.         1.939E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.758E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.980E-27   AT NODE       1697   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.697E-15   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.165E-14   AT NODE       1336   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.697E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.550E-02
 STEP TIME COMPLETED       3.550E+08,  TOTAL TIME COMPLETED        3.550E+08


  INCREMENT    38 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.066E-13   AT NODE        530   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.066E-13
 LARGEST INCREMENT OF DISP.         1.151E-21   AT NODE       1339   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.996E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.108E-27   AT NODE       1734   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.167E-15   AT NODE       1381   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.303E-14   AT NODE       1381   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.167E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.650E-02
 STEP TIME COMPLETED       3.650E+08,  TOTAL TIME COMPLETED        3.650E+08


  INCREMENT    39 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.563E-13   AT NODE       1277   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.563E-13
 LARGEST INCREMENT OF DISP.        -3.512E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.069E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.147E-27   AT NODE       2223   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.937E-15   AT NODE       1518   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.251E-14   AT NODE       1518   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.937E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.750E-02
 STEP TIME COMPLETED       3.750E+08,  TOTAL TIME COMPLETED        3.750E+08


  INCREMENT    40 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.208E-13   AT NODE       1277   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.208E-13
 LARGEST INCREMENT OF DISP.         3.158E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.670E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.915E-27   AT NODE        764   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.235E-15   AT NODE       3251   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.289E-14   AT NODE       3251   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.235E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.850E-02
 STEP TIME COMPLETED       3.850E+08,  TOTAL TIME COMPLETED        3.850E+08


  INCREMENT    41 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.563E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.563E-13
 LARGEST INCREMENT OF DISP.        -1.925E-21   AT NODE         81   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.246E-21   AT NODE         47   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.392E-27   AT NODE       1670   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.865E-15   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.229E-14   AT NODE       1416   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.865E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  3.950E-02
 STEP TIME COMPLETED       3.950E+08,  TOTAL TIME COMPLETED        3.950E+08


  INCREMENT    42 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       1651   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -1.611E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.082E-21   AT NODE        140   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.477E-27   AT NODE       1632   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.839E-15   AT NODE       1787   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.294E-14   AT NODE       1787   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.839E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.050E-02
 STEP TIME COMPLETED       4.050E+08,  TOTAL TIME COMPLETED        4.050E+08


  INCREMENT    43 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE       1278   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.        -8.571E-22   AT NODE       1124   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.885E-21   AT NODE         38   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.145E-27   AT NODE       2187   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.183E-15   AT NODE       1891   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.154E-14   AT NODE       1640   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.183E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.150E-02
 STEP TIME COMPLETED       4.150E+08,  TOTAL TIME COMPLETED        4.150E+08


  INCREMENT    44 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE       1157   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -2.592E-21   AT NODE       1418   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.991E-21   AT NODE       1418   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.760E-27   AT NODE       1758   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.140E-15   AT NODE        723   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.147E-14   AT NODE        723   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.140E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.250E-02
 STEP TIME COMPLETED       4.250E+08,  TOTAL TIME COMPLETED        4.250E+08


  INCREMENT    45 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE        773   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.         1.408E-21   AT NODE         37   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.623E-21   AT NODE       1352   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.727E-27   AT NODE        416   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.428E-15   AT NODE       1850   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.280E-14   AT NODE       1850   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.428E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.350E-02
 STEP TIME COMPLETED       4.350E+08,  TOTAL TIME COMPLETED        4.350E+08


  INCREMENT    46 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE        422   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.         2.263E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.362E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.504E-27   AT NODE       1704   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.364E-15   AT NODE       1574   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.211E-14   AT NODE       2122   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.364E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.450E-02
 STEP TIME COMPLETED       4.450E+08,  TOTAL TIME COMPLETED        4.450E+08


  INCREMENT    47 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       2182   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -2.334E-21   AT NODE         52   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.547E-21   AT NODE         59   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.239E-27   AT NODE       2239   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.292E-15   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.279E-14   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.292E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.550E-02
 STEP TIME COMPLETED       4.550E+08,  TOTAL TIME COMPLETED        4.550E+08


  INCREMENT    48 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1028   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         1.569E-21   AT NODE        832   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.638E-21   AT NODE         51   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.318E-27   AT NODE       1775   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.879E-15   AT NODE       1393   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.164E-14   AT NODE       2088   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.879E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.650E-02
 STEP TIME COMPLETED       4.650E+08,  TOTAL TIME COMPLETED        4.650E+08


  INCREMENT    49 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  114ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE       1175   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         1.699E-21   AT NODE         33   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.616E-21   AT NODE        832   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.142E-27   AT NODE        797   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.930E-15   AT NODE       1422   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.285E-14   AT NODE       1422   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.930E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.750E-02
 STEP TIME COMPLETED       4.750E+08,  TOTAL TIME COMPLETED        4.750E+08


  INCREMENT    50 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE       1326   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         1.986E-21   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.466E-21   AT NODE         33   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.029E-27   AT NODE        470   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.570E-15   AT NODE       2008   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.142E-14   AT NODE       1206   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.570E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.850E-02
 STEP TIME COMPLETED       4.850E+08,  TOTAL TIME COMPLETED        4.850E+08


  INCREMENT    51 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.457E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.457E-13
 LARGEST INCREMENT OF DISP.        -2.536E-21   AT NODE       1435   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.508E-21   AT NODE       1212   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.937E-27   AT NODE        602   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.640E-15   AT NODE        213   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.198E-14   AT NODE        213   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.640E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  4.950E-02
 STEP TIME COMPLETED       4.950E+08,  TOTAL TIME COMPLETED        4.950E+08


  INCREMENT    52 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      5.294E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      5.294E-13
 LARGEST INCREMENT OF DISP.         2.088E-21   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.264E-21   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.084E-27   AT NODE       1682   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.424E-15   AT NODE       2035   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.237E-14   AT NODE       1803   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.424E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.050E-02
 STEP TIME COMPLETED       5.050E+08,  TOTAL TIME COMPLETED        5.050E+08


  INCREMENT    53 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  123ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.421E-13   AT NODE       1276   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.421E-13
 LARGEST INCREMENT OF DISP.         1.395E-21   AT NODE        395   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.987E-21   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.189E-27   AT NODE       4731   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.013E-15   AT NODE       1398   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.236E-14   AT NODE       1398   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.013E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.150E-02
 STEP TIME COMPLETED       5.150E+08,  TOTAL TIME COMPLETED        5.150E+08


  INCREMENT    54 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  116ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.634E-13   AT NODE       1277   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.634E-13
 LARGEST INCREMENT OF DISP.        -2.435E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.518E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.460E-27   AT NODE       2154   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.671E-15   AT NODE       2013   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.231E-14   AT NODE       1975   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.671E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.250E-02
 STEP TIME COMPLETED       5.250E+08,  TOTAL TIME COMPLETED        5.250E+08


  INCREMENT    55 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.279E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.279E-13
 LARGEST INCREMENT OF DISP.        -2.729E-21   AT NODE       1351   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.898E-21   AT NODE       1351   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.197E-27   AT NODE       1635   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.924E-15   AT NODE       1418   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.232E-14   AT NODE       1418   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.924E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.350E-02
 STEP TIME COMPLETED       5.350E+08,  TOTAL TIME COMPLETED        5.350E+08


  INCREMENT    56 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  132ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.761E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.761E-13
 LARGEST INCREMENT OF DISP.         1.891E-21   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.178E-21   AT NODE       1352   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.267E-27   AT NODE        824   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -5.937E-15   AT NODE       1950   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.142E-14   AT NODE       1950   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -5.937E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.450E-02
 STEP TIME COMPLETED       5.450E+08,  TOTAL TIME COMPLETED        5.450E+08


  INCREMENT    57 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1756   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -3.711E-21   AT NODE         56   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.449E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.561E-27   AT NODE       1723   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.768E-15   AT NODE       2042   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.152E-14   AT NODE       2353   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.768E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.550E-02
 STEP TIME COMPLETED       5.550E+08,  TOTAL TIME COMPLETED        5.550E+08


  INCREMENT    58 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  110ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.619E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.619E-13
 LARGEST INCREMENT OF DISP.         1.824E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.619E-21   AT NODE         56   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.194E-27   AT NODE       4744   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.344E-15   AT NODE       1350   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.220E-14   AT NODE         61   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.344E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.650E-02
 STEP TIME COMPLETED       5.650E+08,  TOTAL TIME COMPLETED        5.650E+08


  INCREMENT    59 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.592E-14   AT NODE       1641   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.592E-14
 LARGEST INCREMENT OF DISP.         3.747E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.633E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.884E-27   AT NODE       4744   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.465E-15   AT NODE       3791   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.203E-14   AT NODE       3791   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.465E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.750E-02
 STEP TIME COMPLETED       5.750E+08,  TOTAL TIME COMPLETED        5.750E+08


  INCREMENT    60 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE        806   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.        -3.177E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -6.815E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.860E-27   AT NODE       1662   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.560E-15   AT NODE       3685   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.221E-14   AT NODE       1920   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.560E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.850E-02
 STEP TIME COMPLETED       5.850E+08,  TOTAL TIME COMPLETED        5.850E+08


  INCREMENT    61 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1277   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -1.735E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.870E-21   AT NODE         51   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.218E-27   AT NODE       1680   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.607E-15   AT NODE        693   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.246E-14   AT NODE       2106   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.607E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  5.950E-02
 STEP TIME COMPLETED       5.950E+08,  TOTAL TIME COMPLETED        5.950E+08


  INCREMENT    62 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        779   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         1.508E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.121E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.830E-27   AT NODE       1725   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.205E-15   AT NODE       2067   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.196E-14   AT NODE       2067   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.205E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.050E-02
 STEP TIME COMPLETED       6.050E+08,  TOTAL TIME COMPLETED        6.050E+08


  INCREMENT    63 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        326   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         1.750E-21   AT NODE       1577   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.480E-21   AT NODE       1444   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.973E-27   AT NODE       1689   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.413E-15   AT NODE       1312   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.213E-14   AT NODE       1434   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.413E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.150E-02
 STEP TIME COMPLETED       6.150E+08,  TOTAL TIME COMPLETED        6.150E+08


  INCREMENT    64 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  101ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -4.867E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -4.867E-13
 LARGEST INCREMENT OF DISP.        -2.518E-21   AT NODE       1186   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.118E-21   AT NODE       1186   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.163E-27   AT NODE       2195   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.605E-15   AT NODE       1254   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.201E-14   AT NODE       1254   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.605E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.250E-02
 STEP TIME COMPLETED       6.250E+08,  TOTAL TIME COMPLETED        6.250E+08


  INCREMENT    65 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.137E-13   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.137E-13
 LARGEST INCREMENT OF DISP.         2.378E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.672E-21   AT NODE        726   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.064E-27   AT NODE       2233   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.619E-15   AT NODE       3741   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.212E-14   AT NODE       3741   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.619E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.350E-02
 STEP TIME COMPLETED       6.350E+08,  TOTAL TIME COMPLETED        6.350E+08


  INCREMENT    66 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE        179   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.        -2.941E-21   AT NODE         54   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.300E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.073E-27   AT NODE       1145   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.117E-15   AT NODE       3578   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.200E-14   AT NODE       2029   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.117E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.450E-02
 STEP TIME COMPLETED       6.450E+08,  TOTAL TIME COMPLETED        6.450E+08


  INCREMENT    67 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        614   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.         1.990E-21   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.260E-21   AT NODE         54   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.848E-27   AT NODE       2193   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.380E-15   AT NODE       4016   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.185E-14   AT NODE       3593   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.380E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.550E-02
 STEP TIME COMPLETED       6.550E+08,  TOTAL TIME COMPLETED        6.550E+08


  INCREMENT    68 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        686   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -1.435E-21   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.425E-21   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.843E-27   AT NODE       1088   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.405E-15   AT NODE       2841   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.193E-14   AT NODE       2841   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.405E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.650E-02
 STEP TIME COMPLETED       6.650E+08,  TOTAL TIME COMPLETED        6.650E+08


  INCREMENT    69 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  101ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       1673   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.318E-21   AT NODE       1345   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.233E-21   AT NODE       1404   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.823E-27   AT NODE        854   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.209E-15   AT NODE       1855   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.161E-14   AT NODE       1855   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.209E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.750E-02
 STEP TIME COMPLETED       6.750E+08,  TOTAL TIME COMPLETED        6.750E+08


  INCREMENT    70 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  101ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        179   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.         1.350E-21   AT NODE       1404   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.542E-21   AT NODE       1250   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.789E-27   AT NODE       1720   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.462E-15   AT NODE        198   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.189E-14   AT NODE        198   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.462E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.850E-02
 STEP TIME COMPLETED       6.850E+08,  TOTAL TIME COMPLETED        6.850E+08


  INCREMENT    71 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE       1278   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.        -1.506E-21   AT NODE       1336   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.789E-21   AT NODE       1336   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.076E-27   AT NODE        677   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.309E-15   AT NODE       1899   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.293E-14   AT NODE       1899   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.309E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  6.950E-02
 STEP TIME COMPLETED       6.950E+08,  TOTAL TIME COMPLETED        6.950E+08


  INCREMENT    72 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE        860   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         1.601E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.390E-21   AT NODE        658   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.459E-27   AT NODE       2209   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.400E-15   AT NODE       1367   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.185E-14   AT NODE       1367   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.400E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.050E-02
 STEP TIME COMPLETED       7.050E+08,  TOTAL TIME COMPLETED        7.050E+08


  INCREMENT    73 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.903E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.903E-13
 LARGEST INCREMENT OF DISP.         2.689E-21   AT NODE       1227   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.641E-21   AT NODE       1320   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.134E-27   AT NODE       1148   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.467E-15   AT NODE        786   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.189E-14   AT NODE       1467   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.467E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.150E-02
 STEP TIME COMPLETED       7.150E+08,  TOTAL TIME COMPLETED        7.150E+08


  INCREMENT    74 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE       2251   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.         9.165E-22   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.718E-21   AT NODE       1311   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.752E-27   AT NODE        494   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.073E-15   AT NODE       2447   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.152E-14   AT NODE       2447   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.073E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.250E-02
 STEP TIME COMPLETED       7.250E+08,  TOTAL TIME COMPLETED        7.250E+08


  INCREMENT    75 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE        704   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -2.645E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.507E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.914E-27   AT NODE        782   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.962E-15   AT NODE       1828   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.233E-14   AT NODE       1828   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.962E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.350E-02
 STEP TIME COMPLETED       7.350E+08,  TOTAL TIME COMPLETED        7.350E+08


  INCREMENT    76 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        206   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.         1.436E-21   AT NODE         36   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.003E-21   AT NODE         36   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.135E-27   AT NODE       2201   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.733E-15   AT NODE       2077   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.160E-14   AT NODE       3506   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.733E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.450E-02
 STEP TIME COMPLETED       7.450E+08,  TOTAL TIME COMPLETED        7.450E+08


  INCREMENT    77 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE       2189   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.         1.490E-21   AT NODE       1430   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -1.573E-21   AT NODE         32   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.101E-27   AT NODE       1632   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.131E-15   AT NODE       1789   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.188E-14   AT NODE       1789   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.131E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.550E-02
 STEP TIME COMPLETED       7.550E+08,  TOTAL TIME COMPLETED        7.550E+08


  INCREMENT    78 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  100ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -4.867E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -4.867E-13
 LARGEST INCREMENT OF DISP.         4.574E-21   AT NODE         61   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.831E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.768E-27   AT NODE       1635   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.544E-15   AT NODE       3713   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.193E-14   AT NODE       3713   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.544E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.650E-02
 STEP TIME COMPLETED       7.650E+08,  TOTAL TIME COMPLETED        7.650E+08


  INCREMENT    79 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.725E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.725E-13
 LARGEST INCREMENT OF DISP.        -5.664E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -1.014E-20   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.394E-27   AT NODE       1768   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.665E-15   AT NODE       4171   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.240E-14   AT NODE       4171   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.665E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.750E-02
 STEP TIME COMPLETED       7.750E+08,  TOTAL TIME COMPLETED        7.750E+08


  INCREMENT    80 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1282   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         3.963E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        9.627E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.039E-27   AT NODE       2182   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.237E-15   AT NODE       1346   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.192E-14   AT NODE       1969   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.237E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.850E-02
 STEP TIME COMPLETED       7.850E+08,  TOTAL TIME COMPLETED        7.850E+08


  INCREMENT    81 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.137E-13   AT NODE        662   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.137E-13
 LARGEST INCREMENT OF DISP.         1.688E-21   AT NODE        644   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.364E-21   AT NODE         55   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.115E-27   AT NODE        299   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.585E-15   AT NODE       1396   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.173E-14   AT NODE       1973   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.585E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  7.950E-02
 STEP TIME COMPLETED       7.950E+08,  TOTAL TIME COMPLETED        7.950E+08


  INCREMENT    82 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        317   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.        -3.855E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.657E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.255E-27   AT NODE       2218   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.358E-15   AT NODE       2835   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.260E-14   AT NODE       1396   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.358E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.050E-02
 STEP TIME COMPLETED       8.050E+08,  TOTAL TIME COMPLETED        8.050E+08


  INCREMENT    83 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE        530   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         1.918E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.708E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.700E-27   AT NODE       1623   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.357E-15   AT NODE       1783   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.155E-14   AT NODE        982   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.357E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.150E-02
 STEP TIME COMPLETED       8.150E+08,  TOTAL TIME COMPLETED        8.150E+08


  INCREMENT    84 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  103ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        569   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -1.532E-21   AT NODE        522   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.172E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.384E-27   AT NODE       1758   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.726E-15   AT NODE       2106   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.244E-14   AT NODE       2106   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.726E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.250E-02
 STEP TIME COMPLETED       8.250E+08,  TOTAL TIME COMPLETED        8.250E+08


  INCREMENT    85 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        332   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -1.016E-21   AT NODE        915   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.776E-21   AT NODE       2157   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.019E-27   AT NODE       2235   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.211E-15   AT NODE       3716   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.274E-14   AT NODE       3716   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.211E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.350E-02
 STEP TIME COMPLETED       8.350E+08,  TOTAL TIME COMPLETED        8.350E+08


  INCREMENT    86 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.882E-14   AT NODE       1652   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.882E-14
 LARGEST INCREMENT OF DISP.         1.228E-21   AT NODE       1445   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.002E-21   AT NODE       1570   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.054E-27   AT NODE       2232   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.384E-15   AT NODE       1434   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.196E-14   AT NODE       1434   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.384E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.450E-02
 STEP TIME COMPLETED       8.450E+08,  TOTAL TIME COMPLETED        8.450E+08


  INCREMENT    87 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        611   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         1.532E-21   AT NODE         28   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.161E-21   AT NODE         27   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.031E-27   AT NODE        596   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.713E-15   AT NODE       1228   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.233E-14   AT NODE       1485   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.713E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.550E-02
 STEP TIME COMPLETED       8.550E+08,  TOTAL TIME COMPLETED        8.550E+08


  INCREMENT    88 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        200   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -2.376E-21   AT NODE       1379   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.274E-21   AT NODE       1379   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.916E-27   AT NODE       1726   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.982E-15   AT NODE        526   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.262E-14   AT NODE        526   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.982E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.650E-02
 STEP TIME COMPLETED       8.650E+08,  TOTAL TIME COMPLETED        8.650E+08


  INCREMENT    89 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE       4773   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         3.102E-21   AT NODE       1488   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.260E-21   AT NODE       1344   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.922E-27   AT NODE        140   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.557E-15   AT NODE       3725   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.260E-14   AT NODE       1862   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.557E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.750E-02
 STEP TIME COMPLETED       8.750E+08,  TOTAL TIME COMPLETED        8.750E+08


  INCREMENT    90 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       1702   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.895E-21   AT NODE       1518   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.910E-21   AT NODE       1488   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.847E-27   AT NODE       1728   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.325E-15   AT NODE       2020   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.289E-14   AT NODE       2020   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.325E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.850E-02
 STEP TIME COMPLETED       8.850E+08,  TOTAL TIME COMPLETED        8.850E+08


  INCREMENT    91 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.492E-13   AT NODE        786   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.492E-13
 LARGEST INCREMENT OF DISP.        -1.510E-21   AT NODE       1389   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.310E-21   AT NODE       1518   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.030E-27   AT NODE       2159   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.650E-15   AT NODE       1826   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.213E-14   AT NODE       1498   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.650E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  8.950E-02
 STEP TIME COMPLETED       8.950E+08,  TOTAL TIME COMPLETED        8.950E+08


  INCREMENT    92 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        200   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.         1.534E-21   AT NODE         49   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.322E-21   AT NODE       1389   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.166E-27   AT NODE        653   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.390E-15   AT NODE       1388   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.148E-14   AT NODE       1311   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.390E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.050E-02
 STEP TIME COMPLETED       9.050E+08,  TOTAL TIME COMPLETED        9.050E+08


  INCREMENT    93 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  110ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       2140   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         1.558E-21   AT NODE       1247   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.535E-21   AT NODE         57   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.860E-27   AT NODE       1172   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.102E-15   AT NODE       1914   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.155E-14   AT NODE       1388   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.102E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.150E-02
 STEP TIME COMPLETED       9.150E+08,  TOTAL TIME COMPLETED        9.150E+08


  INCREMENT    94 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        386   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -1.497E-21   AT NODE       1570   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.789E-21   AT NODE       1351   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.916E-27   AT NODE       1649   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.288E-15   AT NODE       1534   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.274E-14   AT NODE       1534   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.288E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.250E-02
 STEP TIME COMPLETED       9.250E+08,  TOTAL TIME COMPLETED        9.250E+08


  INCREMENT    95 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        875   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.         1.913E-21   AT NODE         55   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.947E-21   AT NODE       1311   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.545E-27   AT NODE        974   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.342E-15   AT NODE       3503   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.192E-14   AT NODE       3503   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.342E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.350E-02
 STEP TIME COMPLETED       9.350E+08,  TOTAL TIME COMPLETED        9.350E+08


  INCREMENT    96 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  118ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.553E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.553E-13
 LARGEST INCREMENT OF DISP.        -2.991E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.775E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.615E-27   AT NODE       1759   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.869E-15   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.204E-14   AT NODE       1538   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.869E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.450E-02
 STEP TIME COMPLETED       9.450E+08,  TOTAL TIME COMPLETED        9.450E+08


  INCREMENT    97 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1641   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         1.210E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.201E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.940E-27   AT NODE       2229   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.263E-15   AT NODE       1389   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.179E-14   AT NODE       2029   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.263E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.550E-02
 STEP TIME COMPLETED       9.550E+08,  TOTAL TIME COMPLETED        9.550E+08


  INCREMENT    98 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  102ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        905   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -2.900E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.110E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.749E-27   AT NODE       1276   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.173E-15   AT NODE       1290   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.173E-14   AT NODE       1862   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.173E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.650E-02
 STEP TIME COMPLETED       9.650E+08,  TOTAL TIME COMPLETED        9.650E+08


  INCREMENT    99 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      3.446E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      3.446E-13
 LARGEST INCREMENT OF DISP.         4.207E-21   AT NODE       1394   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.717E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.982E-27   AT NODE       2234   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.296E-15   AT NODE        166   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.201E-14   AT NODE       1353   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.296E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.750E-02
 STEP TIME COMPLETED       9.750E+08,  TOTAL TIME COMPLETED        9.750E+08


  INCREMENT   100 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      3.197E-13   AT NODE       1645   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      3.197E-13
 LARGEST INCREMENT OF DISP.        -4.037E-21   AT NODE       1230   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -7.823E-21   AT NODE       1311   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.898E-27   AT NODE       1725   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.438E-15   AT NODE       1347   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.208E-14   AT NODE       1347   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.438E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  9.850E-02
 STEP TIME COMPLETED       9.850E+08,  TOTAL TIME COMPLETED        9.850E+08


  INCREMENT   101 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  121ms
