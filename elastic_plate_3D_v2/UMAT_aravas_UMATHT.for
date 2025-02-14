C***********************************************************************      
C
      SUBROUTINE KNT(NT,dNT_deqplas,eqplas,PROPS,NPROPS)
C
C*** computes N_T and its derivative wrt eqplas
C
      INCLUDE 'ABA_PARAM.INC'
      DIMENSION PROPS(NPROPS)
C      
C*** TAHA & SOFRONIS FITTING (2001)
C
      C0 = PROPS(6)

      NT    = ( 10**(23.26D0 - 2.33D0*DEXP(-5.5D0*eqplas)) )/C0
      dNT_deqplas = 2.33D0*5.5D0*DLOG(10.D0)*DEXP(-5.5D0*eqplas)*NT
C
      END
C

C***********************************************************************      
C
      SUBROUTINE KCT(CT,CTOR,dCTdEP,dCTdCL,CTT,THETAL,THETAT,eqplas,CL,
     1 PROPS,NPROPS,DTIME)
C
C*** computes C_T, theta_L, theta_T and
C    its derivatives wrt eqplas and C_L
C
      INCLUDE 'ABA_PARAM.INC'
      DIMENSION PROPS(NPROPS)
C
C     read constants
      KT    = PROPS(9)
      ALPHA  = PROPS(10)
      NL    = PROPS(11)
      BETA   = PROPS(12)
      ALFREQ = PROPS(27)
C
      THETAL = CL/(BETA*NL)
      CALL KNT(NT,dNT_deqplas,eqplas,PROPS,NPROPS)
C
      AUX    = 1.D0 + (1.D0 + KT*THETAL)*ALFREQ*DTIME 
      CT     = (CTT + ALPHA*KT*THETAL*NT*ALFREQ*DTIME)/AUX
      IF (CT.LT.0.D0) CT=0.D0
      THETAT = CT/(ALPHA*NT)
C
      THETATOR = KT*THETAL/(1.D0 + KT*THETAL - THETAL) ! ORIANI (used simplify version of 1 - thetaL approx 1)
      CTOR     = ALPHA*THETATOR*NT                      ! ORIANI
C
C*** find dCTdCL and dCTdEP
      AUX1   = (KT*ALFREQ*DTIME)/(BETA*NL)
      AUX2   = 1.D0 + (1.D0 + KT*THETAL)*ALFREQ*DTIME
C
      dCTdCL = -AUX1*(CTT - ALPHA*NT*(1.D0 + ALFREQ*DTIME))/(AUX2*AUX2)
      dCTdEP = ((ALPHA*KT*THETAL*ALFREQ*DTIME)/AUX2)*dNT_deqplas      
C
      END
C

C***********************************************************************
C
      SUBROUTINE UMATHT(U,DUDT,DUDG,FLUX,DFDT,DFDG,
     1 STATEV,TEMP,DTEMP,DTEMDX,TIME,DTIME,PREDEF,DPRED,
     2 CMNAME,NTGRD,NSTATV,PROPS,NPROPS,COORDS,PNEWDT,
     3 NOEL,NPT,LAYER,KSPT,KSTEP,KINC)
C
C*** UMATHT is called AFTER UMAT (if a UMAT exists in the analysis)
C
      use ktransfer
      INCLUDE 'ABA_PARAM.INC'
      include 'SMAAspUserSubroutines.hdr'
C
      CHARACTER*80 CMNAME
      DIMENSION DUDG(NTGRD),FLUX(NTGRD),DFDT(NTGRD),
     1 DFDG(NTGRD,NTGRD),STATEV(NSTATV),DTEMDX(NTGRD),
     2 TIME(2),PREDEF(1),DPRED(1),PROPS(NPROPS),COORDS(3)
C
      eqplas  = STATEV(1)
      CT    = STATEV(4)
      RHO   = STATEV(23)
C
      D     = PROPS(7)
      VH    = PROPS(8)
      KT   = PROPS(9)
      ALPHA = PROPS(10)
      NL   = PROPS(11)
      BETA  = PROPS(12)
      SFD   = PROPS(28)


      CLT   = TEMP
      DCL   = DTEMP
      CL    = CLT + DCL
C
      IF (CL.LT.1.D-6)  THEN
        CL=1.D-6

      END IF
C

C*** NEED TO ACCOUNT FOR INITIAL CL   
C
      IF (KSTEP.EQ.1.AND.KINC.EQ.1) THEN
        CL0 = CLT
        STATEV(8) = CL0
      END IF
C
C*** CALCULATE INTERNAL ENERGY PER UNIT MASS  
C
      CL0 = STATEV(8)
      U = (CL - CL0)/RHO
C
C*** CALCULATE DERIVATIVES OF U
C
      DUDT = 1.D0/RHO
      DUDG = 0.D0
C

      DO I=1,NTGRD
        FLUX(I) = -D*DTEMDX(I) + D*VH*CL*GRADP(NOEL,NPT,I)
      ENDDO
C
      DO I=1,NTGRD
        DFDT(I) = D*VH*GRADP(NOEL,NPT,I)
      ENDDO

      DFDG = 0.D0
      DO I=1,NTGRD
        DFDG(I,I) = -D
      ENDDO
C
      CALL KNT(NT,dNT_deqplas,eqplas,PROPS,NPROPS)
      THETAL  = CL/(BETA*NL)
      THETAT  = CT/(ALPHA*NT)
C
C*** change order of magnitude of heat equation to avoid zero heat flux in solver
C*** default value of SFD = 1.D0 in cases where we have no zero heat flux in solver
      U    = U*SFD
      DUDT = DUDT*SFD
      DUDG = DUDG*SFD
      FLUX = FLUX*SFD
      DFDT = DFDT*SFD
      DFDG = DFDG*SFD
C
C*** update state variables
      STATEV(3) = CL
      STATEV(5) = GRADP(NOEL,NPT,1)
      STATEV(6) = GRADP(NOEL,NPT,2)
      STATEV(7) = CL + CT
      STATEV(21) = U
      STATEV(24) = THETAL
      STATEV(25) = THETAT
C
      END
C