1

   Abaqus 2023.HF4                                  Date 13-Feb-2025   Time 21:19:52
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
     CRITERION FOR RESIDUAL HEAT FLUX FOR A NONLINEAR PROBLEM          5.000E-03
     CRITERION FOR TEMP.    CORRECTION IN A NONLINEAR PROBLEM          1.000E-02
     INITIAL VALUE OF TIME AVERAGE HEAT FLUX                           1.000E-02
     AVERAGE HEAT FLUX IS TIME AVERAGE HEAT FLUX
     ALTERNATE CRIT. FOR RESIDUAL HEAT FLUX FOR A NONLINEAR PROBLEM    2.000E-02
     CRITERION FOR ZERO HEAT FLUX RELATIVE TO TIME AVRG. HEAT FLUX     1.000E-05
     CRITERION FOR RESIDUAL HEAT FLUX WHEN THERE IS ZERO FLUX          1.000E-05
     CRITERION FOR TEMP.    CORRECTION WHEN THERE IS ZERO FLUX         1.000E-03
     CRITERION FOR RESIDUAL HEAT FLUX FOR A LINEAR INCREMENT           1.000E-08
     FIELD CONVERSION RATIO                                             1.00    
     CRITERION FOR ZERO HEAT FLUX REL. TO TIME AVRG. MAX. HEAT FLUX    1.000E-05

     VOLUMETRIC STRAIN COMPATIBILITY TOLERANCE FOR HYBRID SOLIDS       1.000E-05
     AXIAL STRAIN COMPATIBILITY TOLERANCE FOR HYBRID BEAMS             1.000E-05
     TRANS. SHEAR STRAIN COMPATIBILITY TOLERANCE FOR HYBRID BEAMS      1.000E-05
     SOFT CONTACT CONSTRAINT COMPATIBILITY TOLERANCE FOR P>P0          5.000E-03
     SOFT CONTACT CONSTRAINT COMPATIBILITY TOLERANCE FOR P=0.0         0.100    
     CONTACT FORCE ERROR TOLERANCE FOR CONVERT SDI=YES                 1.00    
     DISPLACEMENT COMPATIBILITY TOLERANCE FOR DCOUP ELEMENTS           1.000E-05
     ROTATION COMPATIBILITY TOLERANCE FOR DCOUP ELEMENTS               1.000E-05

 EQUILIBRIUM WILL BE CHECKED FOR SEVERE DISCONTINUITY ITERATIONS

 TIME INCREMENTATION CONTROL PARAMETERS:
     FIRST EQUILIBRIUM ITERATION FOR CONSECUTIVE DIVERGENCE CHECK              4
     EQUILIBRIUM ITERATION AT WHICH LOG. CONVERGENCE RATE CHECK BEGINS         8
     EQUILIBRIUM ITERATION AFTER WHICH ALTERNATE RESIDUAL IS USED              9
     MAXIMUM EQUILIBRIUM ITERATIONS ALLOWED                                   16
     EQUILIBRIUM ITERATION COUNT FOR CUT-BACK IN NEXT INCREMENT               10
     MAXIMUM EQUILIB. ITERS IN TWO INCREMENTS FOR TIME INCREMENT INCREASE      4
     MAXIMUM ITERATIONS FOR SEVERE DISCONTINUITIES                            50
     MAXIMUM ATTEMPTS ALLOWED IN AN INCREMENT                                  5
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
     GLOBAL STABILIZATION CONTROL IS NOT USED

          PRINT OF INCREMENT NUMBER, TIME, ETC., EVERY    1  INCREMENTS

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
        SOLVER ELAPSED TIME:  130ms

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

 AVERAGE HEAT FLUX                  6.053E-29   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.235E-27   AT NODE       2236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -4.250E-15   AT NODE       2134   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -4.250E-15   AT NODE       2134   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.
 
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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  106ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  101ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  103ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  108ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  103ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  195ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  123ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  157ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  176ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  188ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  190ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  121ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  179ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  177ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  169ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  133ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  171ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  110ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  127ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  126ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  183ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  111ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  112ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  119ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  106ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  143ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  105ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  108ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  111ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  109ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  105ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  119ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  119ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  121ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  157ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  126ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  122ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  111ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  137ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  112ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  161ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  119ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  126ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  120ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  116ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  105ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  105ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  111ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  125ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  115ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  106ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  105ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  120ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  125ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  106ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  104ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  103ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  110ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  105ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  111ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  109ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  128ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  113ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  112ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  106ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  128ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  111ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  122ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  114ms

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
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

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
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.547E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.547E-13
 LARGEST INCREMENT OF DISP.         2.320E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.227E-21   AT NODE       1230   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.153E-27   AT NODE       2211   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.379E-15   AT NODE       1236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.279E-14   AT NODE       1236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.379E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.100    
 STEP TIME COMPLETED       9.950E+08,  TOTAL TIME COMPLETED        9.950E+08


  INCREMENT   102 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.233E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.233E-13
 LARGEST INCREMENT OF DISP.        -3.081E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.675E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.929E-27   AT NODE       1632   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.200E-15   AT NODE       1098   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.177E-14   AT NODE         66   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.200E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.101    
 STEP TIME COMPLETED       1.005E+09,  TOTAL TIME COMPLETED        1.005E+09


  INCREMENT   103 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  116ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.274E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.274E-13
 LARGEST INCREMENT OF DISP.         1.548E-21   AT NODE       1311   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.529E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.180E-27   AT NODE       2182   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.419E-15   AT NODE       1377   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.312E-14   AT NODE       1377   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.419E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.102    
 STEP TIME COMPLETED       1.015E+09,  TOTAL TIME COMPLETED        1.015E+09


  INCREMENT   104 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE        257   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.        -1.059E-21   AT NODE         58   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.331E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.333E-27   AT NODE       1637   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.421E-15   AT NODE       2043   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.147E-14   AT NODE       2394   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.421E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.102    
 STEP TIME COMPLETED       1.025E+09,  TOTAL TIME COMPLETED        1.025E+09


  INCREMENT   105 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  115ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE       2254   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -1.996E-21   AT NODE        526   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.137E-21   AT NODE        645   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.387E-27   AT NODE       2224   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.149E-15   AT NODE       2224   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.187E-14   AT NODE       1284   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.149E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.103    
 STEP TIME COMPLETED       1.035E+09,  TOTAL TIME COMPLETED        1.035E+09


  INCREMENT   106 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  138ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE       1630   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.        -2.432E-21   AT NODE       1400   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.590E-21   AT NODE       1400   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.899E-27   AT NODE        629   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.349E-15   AT NODE       3757   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.196E-14   AT NODE         27   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.349E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.104    
 STEP TIME COMPLETED       1.045E+09,  TOTAL TIME COMPLETED        1.045E+09


  INCREMENT   107 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  120ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        611   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -1.612E-21   AT NODE       1121   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.647E-21   AT NODE        393   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.939E-27   AT NODE       2250   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.050E-15   AT NODE       3576   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.241E-14   AT NODE       3576   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.050E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.105    
 STEP TIME COMPLETED       1.055E+09,  TOTAL TIME COMPLETED        1.055E+09


  INCREMENT   108 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  115ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      3.340E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      3.340E-13
 LARGEST INCREMENT OF DISP.         3.412E-21   AT NODE        132   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.543E-21   AT NODE        330   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.398E-27   AT NODE       1685   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.208E-15   AT NODE       3585   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.159E-14   AT NODE       1487   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.208E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.106    
 STEP TIME COMPLETED       1.065E+09,  TOTAL TIME COMPLETED        1.065E+09


  INCREMENT   109 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  126ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.197E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.197E-13
 LARGEST INCREMENT OF DISP.        -4.255E-21   AT NODE         62   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -7.653E-21   AT NODE         62   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.071E-27   AT NODE       2236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.395E-15   AT NODE       2705   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.178E-14   AT NODE       3274   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.395E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.107    
 STEP TIME COMPLETED       1.075E+09,  TOTAL TIME COMPLETED        1.075E+09


  INCREMENT   110 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.867E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.867E-13
 LARGEST INCREMENT OF DISP.         2.055E-21   AT NODE        717   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.244E-21   AT NODE        120   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.190E-27   AT NODE       4744   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.655E-15   AT NODE       1374   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.213E-14   AT NODE       1374   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.655E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.108    
 STEP TIME COMPLETED       1.085E+09,  TOTAL TIME COMPLETED        1.085E+09


  INCREMENT   111 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  125ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.105E-14   AT NODE       2203   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.105E-14
 LARGEST INCREMENT OF DISP.         2.829E-21   AT NODE       1518   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.424E-21   AT NODE       1515   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.196E-27   AT NODE       1004   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.114E-15   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.194E-14   AT NODE       1919   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.114E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.110    
 STEP TIME COMPLETED       1.095E+09,  TOTAL TIME COMPLETED        1.095E+09


  INCREMENT   112 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  113ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.730E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.730E-13
 LARGEST INCREMENT OF DISP.        -2.932E-21   AT NODE       1352   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.621E-21   AT NODE       1352   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.002E-27   AT NODE       2218   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.555E-15   AT NODE       1494   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.189E-14   AT NODE       3618   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.555E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.111    
 STEP TIME COMPLETED       1.105E+09,  TOTAL TIME COMPLETED        1.105E+09


  INCREMENT   113 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.274E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.274E-13
 LARGEST INCREMENT OF DISP.        -2.820E-21   AT NODE        129   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.409E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.769E-27   AT NODE       2263   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.748E-15   AT NODE       3304   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.212E-14   AT NODE       3304   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.748E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.112    
 STEP TIME COMPLETED       1.115E+09,  TOTAL TIME COMPLETED        1.115E+09


  INCREMENT   114 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        686   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         3.002E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.807E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.605E-27   AT NODE       1726   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.571E-15   AT NODE       1379   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.229E-14   AT NODE       1920   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.571E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.113    
 STEP TIME COMPLETED       1.125E+09,  TOTAL TIME COMPLETED        1.125E+09


  INCREMENT   115 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        572   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -1.408E-21   AT NODE        969   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.524E-21   AT NODE        837   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.890E-27   AT NODE       1726   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.218E-15   AT NODE       1920   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.180E-14   AT NODE       1920   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.218E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.114    
 STEP TIME COMPLETED       1.135E+09,  TOTAL TIME COMPLETED        1.135E+09


  INCREMENT   116 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.411E-13   AT NODE       1651   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.411E-13
 LARGEST INCREMENT OF DISP.         2.063E-21   AT NODE       1156   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.289E-21   AT NODE       1143   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.977E-27   AT NODE       1657   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -8.091E-15   AT NODE       1340   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.181E-14   AT NODE       2762   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -8.091E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.115    
 STEP TIME COMPLETED       1.145E+09,  TOTAL TIME COMPLETED        1.145E+09


  INCREMENT   117 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.340E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.340E-13
 LARGEST INCREMENT OF DISP.        -2.851E-21   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.881E-21   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.007E-27   AT NODE       1654   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.666E-15   AT NODE       3685   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.215E-14   AT NODE       3685   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.666E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.116    
 STEP TIME COMPLETED       1.155E+09,  TOTAL TIME COMPLETED        1.155E+09


  INCREMENT   118 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  114ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.487E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.487E-13
 LARGEST INCREMENT OF DISP.         3.255E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.961E-21   AT NODE         61   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.541E-27   AT NODE       2256   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.888E-15   AT NODE       3629   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.257E-14   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.888E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.117    
 STEP TIME COMPLETED       1.165E+09,  TOTAL TIME COMPLETED        1.165E+09


  INCREMENT   119 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  120ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.137E-13   AT NODE       1280   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.137E-13
 LARGEST INCREMENT OF DISP.         8.993E-22   AT NODE       3660   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.942E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.441E-27   AT NODE       1013   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.076E-15   AT NODE       2061   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.160E-14   AT NODE       1512   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.076E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.117    
 STEP TIME COMPLETED       1.175E+09,  TOTAL TIME COMPLETED        1.175E+09


  INCREMENT   120 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  133ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       1776   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -1.735E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.269E-21   AT NODE         60   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.669E-27   AT NODE       2182   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.451E-15   AT NODE         36   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.149E-14   AT NODE       3346   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.451E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.118    
 STEP TIME COMPLETED       1.185E+09,  TOTAL TIME COMPLETED        1.185E+09


  INCREMENT   121 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  157ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        770   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -9.241E-22   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.156E-21   AT NODE       1283   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.792E-27   AT NODE       2182   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.627E-15   AT NODE       1500   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.224E-14   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.627E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.119    
 STEP TIME COMPLETED       1.195E+09,  TOTAL TIME COMPLETED        1.195E+09


  INCREMENT   122 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  125ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE       1654   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.         1.512E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.784E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.919E-27   AT NODE       1754   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.436E-15   AT NODE       1121   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.199E-14   AT NODE       4191   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.436E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.120    
 STEP TIME COMPLETED       1.205E+09,  TOTAL TIME COMPLETED        1.205E+09


  INCREMENT   123 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        794   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -2.844E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.355E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.951E-27   AT NODE       2236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.909E-15   AT NODE       1334   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.259E-14   AT NODE       1334   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.909E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.121    
 STEP TIME COMPLETED       1.215E+09,  TOTAL TIME COMPLETED        1.215E+09


  INCREMENT   124 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  215ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        233   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         2.844E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.688E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.094E-27   AT NODE       2246   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -8.341E-15   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.202E-14   AT NODE       3622   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -8.341E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.122    
 STEP TIME COMPLETED       1.225E+09,  TOTAL TIME COMPLETED        1.225E+09


  INCREMENT   125 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1668   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -1.179E-21   AT NODE        330   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.794E-21   AT NODE        330   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.248E-27   AT NODE       1685   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.543E-15   AT NODE       2635   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.214E-14   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.543E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.123    
 STEP TIME COMPLETED       1.235E+09,  TOTAL TIME COMPLETED        1.235E+09


  INCREMENT   126 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  119ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE        407   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.         2.262E-21   AT NODE       1283   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.455E-21   AT NODE       1420   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.010E-27   AT NODE        848   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.291E-15   AT NODE       1141   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.158E-14   AT NODE       2066   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.291E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.124    
 STEP TIME COMPLETED       1.245E+09,  TOTAL TIME COMPLETED        1.245E+09


  INCREMENT   127 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        617   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.        -1.797E-21   AT NODE       1339   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.976E-21   AT NODE       1460   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.681E-27   AT NODE       1772   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.789E-15   AT NODE       2020   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.239E-14   AT NODE       3653   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.789E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.126    
 STEP TIME COMPLETED       1.255E+09,  TOTAL TIME COMPLETED        1.255E+09


  INCREMENT   128 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  110ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      3.375E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      3.375E-13
 LARGEST INCREMENT OF DISP.         2.427E-21   AT NODE       1352   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.176E-21   AT NODE       1352   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.305E-27   AT NODE       1686   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.301E-15   AT NODE       2137   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.220E-14   AT NODE       2137   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.301E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.127    
 STEP TIME COMPLETED       1.265E+09,  TOTAL TIME COMPLETED        1.265E+09


  INCREMENT   129 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  113ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.553E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.553E-13
 LARGEST INCREMENT OF DISP.        -3.278E-21   AT NODE        130   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.372E-21   AT NODE        789   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.968E-27   AT NODE        650   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.555E-15   AT NODE       1072   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.189E-14   AT NODE       1072   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.555E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.128    
 STEP TIME COMPLETED       1.275E+09,  TOTAL TIME COMPLETED        1.275E+09


  INCREMENT   130 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.619E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.619E-13
 LARGEST INCREMENT OF DISP.         3.405E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.277E-21   AT NODE        725   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.070E-27   AT NODE        116   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.731E-15   AT NODE       2958   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.242E-14   AT NODE       3553   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.731E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.129    
 STEP TIME COMPLETED       1.285E+09,  TOTAL TIME COMPLETED        1.285E+09


  INCREMENT   131 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  110ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.375E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.375E-13
 LARGEST INCREMENT OF DISP.        -3.318E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -6.558E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.462E-27   AT NODE       2160   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.402E-15   AT NODE       1614   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.184E-14   AT NODE       1614   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.402E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.130    
 STEP TIME COMPLETED       1.295E+09,  TOTAL TIME COMPLETED        1.295E+09


  INCREMENT   132 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  113ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      4.690E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      4.690E-13
 LARGEST INCREMENT OF DISP.         3.240E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.558E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.852E-27   AT NODE       1763   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.946E-15   AT NODE       1472   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.337E-14   AT NODE       1472   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.946E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.131    
 STEP TIME COMPLETED       1.305E+09,  TOTAL TIME COMPLETED        1.305E+09


  INCREMENT   133 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  160ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.482E-13   AT NODE       1670   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.482E-13
 LARGEST INCREMENT OF DISP.        -3.322E-21   AT NODE       1518   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -6.286E-21   AT NODE       1340   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.150E-27   AT NODE        716   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.154E-15   AT NODE       1420   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.179E-14   AT NODE       1420   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.154E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.132    
 STEP TIME COMPLETED       1.315E+09,  TOTAL TIME COMPLETED        1.315E+09


  INCREMENT   134 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  129ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.203E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.203E-13
 LARGEST INCREMENT OF DISP.        -3.143E-21   AT NODE        330   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.377E-21   AT NODE        330   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.056E-27   AT NODE       1691   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.015E-15   AT NODE       1471   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.208E-14   AT NODE       1420   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.015E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.133    
 STEP TIME COMPLETED       1.325E+09,  TOTAL TIME COMPLETED        1.325E+09


  INCREMENT   135 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE         68   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.         3.189E-21   AT NODE         63   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.842E-21   AT NODE        330   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.728E-27   AT NODE       2154   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.098E-15   AT NODE       1380   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.265E-14   AT NODE       1380   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.098E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.134    
 STEP TIME COMPLETED       1.335E+09,  TOTAL TIME COMPLETED        1.335E+09


  INCREMENT   136 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE        116   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.         1.377E-21   AT NODE       1493   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.907E-21   AT NODE         63   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.772E-27   AT NODE       1781   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         9.479E-15   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.219E-14   AT NODE       1840   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   9.479E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.135    
 STEP TIME COMPLETED       1.345E+09,  TOTAL TIME COMPLETED        1.345E+09


  INCREMENT   137 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  121ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE       1680   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -2.276E-21   AT NODE       1493   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.652E-21   AT NODE       1493   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.189E-27   AT NODE       1763   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.573E-15   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.705E-14   AT NODE       3482   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.573E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.136    
 STEP TIME COMPLETED       1.355E+09,  TOTAL TIME COMPLETED        1.355E+09


  INCREMENT   138 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  115ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.105E-14   AT NODE        884   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.105E-14
 LARGEST INCREMENT OF DISP.        -2.509E-21   AT NODE        660   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.363E-21   AT NODE       1396   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.804E-27   AT NODE        287   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.577E-15   AT NODE       1485   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.156E-14   AT NODE       1125   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.577E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.137    
 STEP TIME COMPLETED       1.365E+09,  TOTAL TIME COMPLETED        1.365E+09


  INCREMENT   139 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  115ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.137E-13   AT NODE        137   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.137E-13
 LARGEST INCREMENT OF DISP.         2.971E-21   AT NODE        660   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.481E-21   AT NODE        660   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.166E-27   AT NODE        353   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.376E-15   AT NODE         11   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.200E-14   AT NODE         11   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.376E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.138    
 STEP TIME COMPLETED       1.375E+09,  TOTAL TIME COMPLETED        1.375E+09


  INCREMENT   140 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  110ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.990E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.990E-13
 LARGEST INCREMENT OF DISP.        -3.676E-21   AT NODE       1393   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.395E-21   AT NODE        660   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.944E-27   AT NODE       2190   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.617E-15   AT NODE       2029   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.168E-14   AT NODE       2022   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.617E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.139    
 STEP TIME COMPLETED       1.385E+09,  TOTAL TIME COMPLETED        1.385E+09


  INCREMENT   141 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  115ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.279E-13   AT NODE       4773   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.279E-13
 LARGEST INCREMENT OF DISP.        -2.608E-21   AT NODE        196   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.044E-21   AT NODE       1393   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.058E-27   AT NODE       1172   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         5.951E-15   AT NODE       1691   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.152E-14   AT NODE       1691   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   5.951E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.140    
 STEP TIME COMPLETED       1.395E+09,  TOTAL TIME COMPLETED        1.395E+09


  INCREMENT   142 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  123ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE         68   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         2.516E-21   AT NODE         63   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.860E-21   AT NODE        178   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.829E-27   AT NODE        476   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.207E-15   AT NODE       2459   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.162E-14   AT NODE       3549   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.207E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.141    
 STEP TIME COMPLETED       1.405E+09,  TOTAL TIME COMPLETED        1.405E+09


  INCREMENT   143 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  111ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        473   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.         1.850E-21   AT NODE        988   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        1.810E-21   AT NODE       1620   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.331E-27   AT NODE       2159   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.471E-15   AT NODE       2597   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.184E-14   AT NODE       2597   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.471E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.141    
 STEP TIME COMPLETED       1.415E+09,  TOTAL TIME COMPLETED        1.415E+09


  INCREMENT   144 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  124ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        236   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.701E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.309E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.887E-27   AT NODE        662   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.703E-15   AT NODE       1949   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.238E-14   AT NODE       1949   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.703E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.142    
 STEP TIME COMPLETED       1.425E+09,  TOTAL TIME COMPLETED        1.425E+09


  INCREMENT   145 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  137ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       2186   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -1.205E-21   AT NODE        103   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.882E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.726E-27   AT NODE       2226   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.202E-15   AT NODE       3565   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.163E-14   AT NODE       2061   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.202E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.143    
 STEP TIME COMPLETED       1.435E+09,  TOTAL TIME COMPLETED        1.435E+09


  INCREMENT   146 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  135ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE        260   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         1.800E-21   AT NODE        165   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.925E-21   AT NODE        103   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.633E-27   AT NODE       1643   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.817E-15   AT NODE       1344   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.242E-14   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.817E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.144    
 STEP TIME COMPLETED       1.445E+09,  TOTAL TIME COMPLETED        1.445E+09


  INCREMENT   147 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE         68   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -2.972E-21   AT NODE         57   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.504E-21   AT NODE         49   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.208E-27   AT NODE       1661   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.381E-15   AT NODE       1840   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.243E-14   AT NODE       1344   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.381E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.145    
 STEP TIME COMPLETED       1.455E+09,  TOTAL TIME COMPLETED        1.455E+09


  INCREMENT   148 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  117ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.061E-13   AT NODE       1650   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.061E-13
 LARGEST INCREMENT OF DISP.         3.330E-21   AT NODE       1372   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.656E-21   AT NODE       1182   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.816E-27   AT NODE       1725   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.984E-15   AT NODE       1420   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.248E-14   AT NODE       1473   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.984E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.146    
 STEP TIME COMPLETED       1.465E+09,  TOTAL TIME COMPLETED        1.465E+09


  INCREMENT   149 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  121ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -4.512E-13   AT NODE       1650   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -4.512E-13
 LARGEST INCREMENT OF DISP.         3.489E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.741E-21   AT NODE       1479   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.980E-27   AT NODE       1642   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.250E-15   AT NODE       1125   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.202E-14   AT NODE       1125   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.250E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.147    
 STEP TIME COMPLETED       1.475E+09,  TOTAL TIME COMPLETED        1.475E+09


  INCREMENT   150 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  152ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       2253   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -1.733E-21   AT NODE       1504   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.009E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.688E-27   AT NODE       1723   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.502E-15   AT NODE       1815   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.204E-14   AT NODE       1815   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.502E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.148    
 STEP TIME COMPLETED       1.485E+09,  TOTAL TIME COMPLETED        1.485E+09


  INCREMENT   151 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  180ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE        491   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -4.729E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.868E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.905E-27   AT NODE       2175   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.862E-15   AT NODE       2089   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.178E-14   AT NODE       1202   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.862E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.149    
 STEP TIME COMPLETED       1.495E+09,  TOTAL TIME COMPLETED        1.495E+09


  INCREMENT   152 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  124ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.137E-13   AT NODE        155   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.137E-13
 LARGEST INCREMENT OF DISP.         2.153E-21   AT NODE         66   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.783E-21   AT NODE        198   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.551E-27   AT NODE       2214   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.123E-15   AT NODE        534   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.152E-14   AT NODE       1253   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.123E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.150    
 STEP TIME COMPLETED       1.505E+09,  TOTAL TIME COMPLETED        1.505E+09


  INCREMENT   153 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  180ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       4733   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -3.929E-21   AT NODE       1341   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.707E-21   AT NODE       1378   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.888E-27   AT NODE        719   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.491E-15   AT NODE       1336   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.125E-14   AT NODE       4408   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.491E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.151    
 STEP TIME COMPLETED       1.515E+09,  TOTAL TIME COMPLETED        1.515E+09


  INCREMENT   154 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  118ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.061E-13   AT NODE       1627   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.061E-13
 LARGEST INCREMENT OF DISP.         2.199E-21   AT NODE       1341   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        6.128E-21   AT NODE       1341   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.295E-27   AT NODE        752   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.405E-15   AT NODE       1930   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.205E-14   AT NODE       1930   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.405E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.152    
 STEP TIME COMPLETED       1.525E+09,  TOTAL TIME COMPLETED        1.525E+09


  INCREMENT   155 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.350E-13   AT NODE        596   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.350E-13
 LARGEST INCREMENT OF DISP.         2.428E-21   AT NODE       1393   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.831E-21   AT NODE          7   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.941E-27   AT NODE       1145   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.306E-15   AT NODE       3225   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.155E-14   AT NODE       2746   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.306E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.153    
 STEP TIME COMPLETED       1.535E+09,  TOTAL TIME COMPLETED        1.535E+09


  INCREMENT   156 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  108ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE       1019   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.        -9.100E-22   AT NODE       1433   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.270E-21   AT NODE       1393   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.425E-27   AT NODE       2234   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.452E-15   AT NODE       3665   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.205E-14   AT NODE       3665   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.452E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.154    
 STEP TIME COMPLETED       1.545E+09,  TOTAL TIME COMPLETED        1.545E+09


  INCREMENT   157 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  113ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        305   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.         2.414E-21   AT NODE         63   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.428E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.925E-27   AT NODE        692   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.289E-15   AT NODE         36   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.159E-14   AT NODE       3237   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.289E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.155    
 STEP TIME COMPLETED       1.555E+09,  TOTAL TIME COMPLETED        1.555E+09


  INCREMENT   158 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  105ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE        670   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -3.403E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.747E-21   AT NODE         64   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.495E-27   AT NODE        140   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.743E-15   AT NODE         76   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.214E-14   AT NODE         76   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.743E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.157    
 STEP TIME COMPLETED       1.565E+09,  TOTAL TIME COMPLETED        1.565E+09


  INCREMENT   159 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  125ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE       1655   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.        -2.508E-21   AT NODE       1311   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.052E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.194E-27   AT NODE       1697   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.545E-15   AT NODE       1270   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.190E-14   AT NODE       1270   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.545E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.158    
 STEP TIME COMPLETED       1.575E+09,  TOTAL TIME COMPLETED        1.575E+09


  INCREMENT   160 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      1.670E-13   AT NODE       1627   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      1.670E-13
 LARGEST INCREMENT OF DISP.         1.063E-21   AT NODE       1049   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.337E-21   AT NODE       1120   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.207E-27   AT NODE       1734   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.254E-15   AT NODE         61   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.181E-14   AT NODE       1973   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.254E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.159    
 STEP TIME COMPLETED       1.585E+09,  TOTAL TIME COMPLETED        1.585E+09


  INCREMENT   161 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  120ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE       1112   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         2.634E-21   AT NODE       1370   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.236E-21   AT NODE       1396   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.016E-27   AT NODE       2236   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.697E-15   AT NODE       2047   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.216E-14   AT NODE       2047   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.697E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.160    
 STEP TIME COMPLETED       1.595E+09,  TOTAL TIME COMPLETED        1.595E+09


  INCREMENT   162 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  116ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE       1639   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.         1.031E-21   AT NODE         20   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.874E-21   AT NODE       1484   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.212E-27   AT NODE       1636   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         8.316E-15   AT NODE       1477   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.390E-14   AT NODE       1477   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   8.316E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.161    
 STEP TIME COMPLETED       1.605E+09,  TOTAL TIME COMPLETED        1.605E+09


  INCREMENT   163 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  165ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.948E-14   AT NODE        770   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.948E-14
 LARGEST INCREMENT OF DISP.        -1.798E-21   AT NODE       3473   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.252E-21   AT NODE       1203   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.137E-27   AT NODE       1142   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.243E-15   AT NODE       1605   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.239E-14   AT NODE       1393   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.243E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.162    
 STEP TIME COMPLETED       1.615E+09,  TOTAL TIME COMPLETED        1.615E+09


  INCREMENT   164 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  192ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -1.066E-13   AT NODE       1640   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -1.066E-13
 LARGEST INCREMENT OF DISP.         2.221E-21   AT NODE       1221   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.892E-21   AT NODE       1514   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.804E-27   AT NODE       1781   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.444E-15   AT NODE       3677   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.203E-14   AT NODE       3744   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.444E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.163    
 STEP TIME COMPLETED       1.625E+09,  TOTAL TIME COMPLETED        1.625E+09


  INCREMENT   165 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -8.527E-14   AT NODE        398   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -8.527E-14
 LARGEST INCREMENT OF DISP.         2.146E-21   AT NODE         30   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.780E-21   AT NODE         93   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.863E-27   AT NODE        755   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.276E-15   AT NODE       2107   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.273E-14   AT NODE       2107   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.276E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.164    
 STEP TIME COMPLETED       1.635E+09,  TOTAL TIME COMPLETED        1.635E+09


  INCREMENT   166 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  106ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      2.274E-13   AT NODE       1627   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      2.274E-13
 LARGEST INCREMENT OF DISP.        -2.771E-21   AT NODE       1422   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -3.659E-21   AT NODE       1519   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.923E-27   AT NODE        797   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.683E-15   AT NODE       2056   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.225E-14   AT NODE       2056   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.683E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.165    
 STEP TIME COMPLETED       1.645E+09,  TOTAL TIME COMPLETED        1.645E+09


  INCREMENT   167 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  138ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      2.416E-13   AT NODE       1627   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      2.416E-13
 LARGEST INCREMENT OF DISP.         1.142E-21   AT NODE       1508   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.846E-21   AT NODE       1508   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.713E-27   AT NODE       1755   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -7.020E-15   AT NODE       1334   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.168E-14   AT NODE       1202   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -7.020E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.166    
 STEP TIME COMPLETED       1.655E+09,  TOTAL TIME COMPLETED        1.655E+09


  INCREMENT   168 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  181ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -2.416E-13   AT NODE       1627   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -2.416E-13
 LARGEST INCREMENT OF DISP.         2.597E-21   AT NODE        262   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        3.603E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.082E-27   AT NODE       1625   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.497E-15   AT NODE       1448   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.196E-14   AT NODE       1448   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.497E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.167    
 STEP TIME COMPLETED       1.665E+09,  TOTAL TIME COMPLETED        1.665E+09


  INCREMENT   169 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  155ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -3.695E-13   AT NODE       1627   DOF  3
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -3.695E-13
 LARGEST INCREMENT OF DISP.        -1.931E-21   AT NODE       1530   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -4.023E-21   AT NODE       1439   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.588E-27   AT NODE       1758   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.507E-15   AT NODE       4149   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.185E-14   AT NODE       4149   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.507E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.168    
 STEP TIME COMPLETED       1.675E+09,  TOTAL TIME COMPLETED        1.675E+09


  INCREMENT   170 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  144ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE       2268   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -3.467E-21   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.466E-21   AT NODE       1371   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.992E-27   AT NODE         71   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         7.196E-15   AT NODE       1476   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.259E-14   AT NODE       1476   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   7.196E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.169    
 STEP TIME COMPLETED       1.685E+09,  TOTAL TIME COMPLETED        1.685E+09


  INCREMENT   171 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  104ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE        377   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -1.542E-21   AT NODE       1431   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.651E-21   AT NODE       1478   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.144E-27   AT NODE       2250   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.716E-15   AT NODE        459   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.216E-14   AT NODE       2567   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.716E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.170    
 STEP TIME COMPLETED       1.695E+09,  TOTAL TIME COMPLETED        1.695E+09


  INCREMENT   172 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  119ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.948E-14   AT NODE       4773   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.948E-14
 LARGEST INCREMENT OF DISP.         2.898E-21   AT NODE       1289   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        4.413E-21   AT NODE       1289   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.054E-27   AT NODE        860   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.859E-15   AT NODE       1273   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.231E-14   AT NODE       1273   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.859E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.171    
 STEP TIME COMPLETED       1.705E+09,  TOTAL TIME COMPLETED        1.705E+09


  INCREMENT   173 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  113ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        782   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -4.338E-21   AT NODE         58   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -5.885E-21   AT NODE         58   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.963E-27   AT NODE       1643   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.272E-15   AT NODE       1976   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.173E-14   AT NODE       1976   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.272E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.172    
 STEP TIME COMPLETED       1.715E+09,  TOTAL TIME COMPLETED        1.715E+09


  INCREMENT   174 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  107ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      8.527E-14   AT NODE        512   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      8.527E-14
 LARGEST INCREMENT OF DISP.         1.085E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        5.299E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -2.872E-27   AT NODE       2226   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.413E-15   AT NODE       3576   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.175E-14   AT NODE       3905   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.413E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.172    
 STEP TIME COMPLETED       1.725E+09,  TOTAL TIME COMPLETED        1.725E+09


  INCREMENT   175 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  114ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE       1742   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.        -1.487E-21   AT NODE        823   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.148E-21   AT NODE        460   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.038E-27   AT NODE        824   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.953E-15   AT NODE       1376   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.231E-14   AT NODE       1376   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.953E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.173    
 STEP TIME COMPLETED       1.735E+09,  TOTAL TIME COMPLETED        1.735E+09


  INCREMENT   176 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  112ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      9.237E-14   AT NODE        440   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      9.237E-14
 LARGEST INCREMENT OF DISP.         1.624E-21   AT NODE        196   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.128E-21   AT NODE        457   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         2.989E-27   AT NODE       1001   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.309E-15   AT NODE       1554   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.192E-14   AT NODE       1125   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.309E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.174    
 STEP TIME COMPLETED       1.745E+09,  TOTAL TIME COMPLETED        1.745E+09


  INCREMENT   177 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  113ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -7.816E-14   AT NODE        506   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -7.816E-14
 LARGEST INCREMENT OF DISP.        -1.557E-21   AT NODE       1548   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -2.029E-21   AT NODE       1313   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.080E-27   AT NODE        854   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.687E-15   AT NODE       1346   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.218E-14   AT NODE       1346   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.687E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.175    
 STEP TIME COMPLETED       1.755E+09,  TOTAL TIME COMPLETED        1.755E+09


  INCREMENT   178 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  109ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE     -9.237E-14   AT NODE       1776   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE     -9.237E-14
 LARGEST INCREMENT OF DISP.        -1.189E-21   AT NODE         45   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.        2.530E-21   AT NODE       1548   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX         3.106E-27   AT NODE       1741   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.        -6.731E-15   AT NODE       2704   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.       -1.213E-14   AT NODE       2704   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS  -6.731E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.176    
 STEP TIME COMPLETED       1.765E+09,  TOTAL TIME COMPLETED        1.765E+09


  INCREMENT   179 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
        SOLVER ELAPSED TIME:  140ms

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       15.7       TIME AVG. FORCE        15.7    
 LARGEST SCALED RESIDUAL FORCE      7.816E-14   AT NODE       2252   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
  CORRESPONDING RESIDUAL FORCE      7.816E-14
 LARGEST INCREMENT OF DISP.        -1.576E-21   AT NODE         65   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO DISP.       -1.387E-21   AT NODE       1493   DOF  2
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE HEAT FLUX                  2.050E-19   TIME AVG. HEAT FLUX   1.000E-02
 LARGEST RESIDUAL HEAT FLUX        -3.863E-27   AT NODE       1781   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST INCREMENT OF TEMP.         6.705E-15   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
 LARGEST CORRECTION TO TEMP.        1.209E-14   AT NODE       1371   DOF 11
   INSTANCE: ELASTIC-HOLD-PLATE-1                                                            
          THERE IS ZERO HEAT FLUX EVERYWHERE
 

 ***WARNING: THERE IS ZERO HEAT FLUX EVERYWHERE IN THE MODEL BASED ON THE 
             DEFAULT CRITERION. PLEASE CHECK THE VALUE OF THE AVERAGE HEAT FLUX 
             DURING THE CURRENT ITERATION TO VERIFY THAT THE HEAT FLUX IS SMALL 
             ENOUGH TO BE TREATED AS ZERO. IF NOT, PLEASE USE THE SOLUTION 
             CONTROLS TO RESET THE CRITERION FOR ZERO HEAT FLUX.

 MAXIMUM NON-PRESCRIBED INCREMENT IN TEMPERATURE IS   6.705E-15

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E+07,  FRACTION OF STEP COMPLETED  0.177    
 STEP TIME COMPLETED       1.775E+09,  TOTAL TIME COMPLETED        1.775E+09


  INCREMENT   180 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E+07
 
	UNSYMMETRIC NEW DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST x 1 MPI RANK PER HOST x 1 THREAD PER RANK
        NUMBER OF EQUATIONS:  19344
        NUMBER OF RHS:        1
        NUMBER OF FLOPS:      2.940e+09
