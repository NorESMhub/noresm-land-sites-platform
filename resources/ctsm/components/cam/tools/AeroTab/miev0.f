
      SUBROUTINE  MIEV0 ( XX, CREFIN, PERFCT, MIMCUT, ANYANG,
     $                    NUMANG, XMU, NMOM, IPOLZN, MOMDIM, PRNT,
     $                    QEXT, QSCA, GQSC, PMOM, SFORW, SBACK, S1,
     $                    S2, TFORW, TBACK, SPIKE )

c Author:  Dr. Warren J. Wiscombe (wiscombe@climate.gsfc.nasa.gov)
c         NASA Goddard Space Flight Center
c         Code 913
c         Greenbelt, MD 20771
c
c REFERENCES:
c ----------
c   (1) Wiscombe, W., 1979: Mie Scattering Calculations--Advances
c          in Technique And Fast, Vector-Speed Computer Codes,
c          Ncar Tech Note Tn-140+Str, National Center For
c          Atmospheric Research, Boulder, Colorado (NCAR no
c          longer distributes this, so contact the author or
c          NTIS for a copy)
c   (2) Wiscombe, W., 1980: Improved Mie Scattering Algorithms,
c          Appl. Opt. 19, 1505-1509
c   (3) Dave, J.V., 1970a:  Coefficients of the Legendre and
c          Fourier Series for the Scattering Functions of
c          Spherical Particles, Appl. Opt. 9, 1888-1896
c   (4) Dave, J.V., 1970b:  Intensity and Polarization of the
c          Radiation Emerging from a Plane-Parallel Atmosphere
c          Containing Monodisperse Aerosols, Appl. Opt. 9, 2673-84
c   (5) Van De Hulst, 1957, 1982:  Light Scattering by Small
c          Particles, Dover Press, New York.
c   (6) Bohren, C. and D. Huffman, Absorption and Scattering of
c          Light by Small Particles, Wiley, New York.  (has a
c          Mie program in the back of the book)
c
c For more info, see MIEV-documentation.txt.
c
c **********************************************************************************
c     Modified for use in sizmie.f by Alf Kirkevåg.
c **********************************************************************************

cak   NB! jeg har endret MAXTRM for aa kunne bruke fordelinger med
cak   store radier (opp til 100 mikrometer).
cak   Paa grunn at de samme subrutinene blir brukt i straalingstransportmodellen,
cak   har jeg her skiftet navn paa disse:
cak   errmsg  til  errmsga (nytt navn)
cak   wrtbad       wrtbada
cak   wrtdim       wrtdima
cak   tstbad       tstbada

C    COMPUTES MIE SCATTERING AND EXTINCTION EFFICIENCIES; ASYMMETRY
C    FACTOR;  FORWARD- AND BACKSCATTER AMPLITUDE;  SCATTERING
C    AMPLITUDES VS. SCATTERING ANGLE FOR INCIDENT POLARIZATION PARALLEL
C    AND PERPENDICULAR TO THE PLANE OF SCATTERING;
C    COEFFICIENTS IN THE LEGENDRE POLYNOMIAL EXPANSIONS OF EITHER THE
C    UNPOLARIZED PHASE FUNCTION OR THE POLARIZED PHASE MATRIX;
C    SOME QUANTITIES NEEDED IN POLARIZED RADIATIVE TRANSFER;  AND
C    INFORMATION ABOUT WHETHER OR NOT A RESONANCE HAS BEEN HIT.
C
C    INPUT AND OUTPUT VARIABLES ARE DESCRIBED IN FILE MIEV.DOC
C
C    ROUTINES CALLED :  BIGA, CKINMI, SMALL1, SMALL2, TESTMI,
C                       MIPRNT, LPCOEF, ERRMSG
C
C      I N T E R N A L   V A R I A B L E S
C      -----------------------------------
C
C  AN,BN           MIE COEFFICIENTS  LITTLE-A-SUB-N, LITTLE-B-SUB-N
C                     ( REF. 1, EQ. 16 )
C  ANM1,BNM1       MIE COEFFICIENTS  LITTLE-A-SUB-(N-1),
C                     LITTLE-B-SUB-(N-1);  USED IN -GQSC- SUM
C  ANP             COEFFS. IN S+ EXPANSION ( REF. 2, P. 1507 )
C  BNP             COEFFS. IN S- EXPANSION ( REF. 2, P. 1507 )
C  ANPM            COEFFS. IN S+ EXPANSION ( REF. 2, P. 1507 )
C                     WHEN  MU  IS REPLACED BY  - MU
C  BNPM            COEFFS. IN S- EXPANSION ( REF. 2, P. 1507 )
C                     ( REF. 1, P. 11 FF. )
C  RBIGA(N)        BESSEL FUNCTION RATIO CAPITAL-A-SUB-N (REF. 2, EQ. 2)
C                     ( REAL VERSION, FOR WHEN IMAG REFRAC INDEX = 0 )
C  RIORIV          1 / MRE
C  RN              1 / N
C  RTMP            (REAL) TEMPORARY VARIABLE
C  SP(J)           S+  FOR J-TH ANGLE  ( REF. 2, P. 1507 )
C  SM(J)           S-  FOR J-TH ANGLE  ( REF. 2, P. 1507 )
C  SPS(J)          S+  FOR (NUMANG+1-J)-TH ANGLE ( ANYANG=FALSE )
C  SMS(J)          S-  FOR (NUMANG+1-J)-TH ANGLE ( ANYANG=FALSE )
C  TAUN            ANGULAR FUNCTION LITTLE-TAU-SUB-N ( REF. 2, EQ. 4 )
C                     AT J-TH ANGLE
C  TCOEF           N ( N+1 ) ( 2N+1 ) (FOR SUMMING TFORW,TBACK SERIES)
C  TWONP1          2N + 1
C  YESANG          TRUE IF SCATTERING AMPLITUDES ARE TO BE CALCULATED
C  ZETNM1          RICATTI-BESSEL FUNCTION  ZETA-SUB-(N-1) OF ARGUMENT
C                     -XX-  ( REF. 2, EQ. 17 )
C  ZETN            RICATTI-BESSEL FUNCTION  ZETA-SUB-N OF ARGUMENT -XX-
C
C ----------------------------------------------------------------------
C --------  I / O SPECIFICATIONS FOR SUBROUTINE MIEV0  -----------------
C ----------------------------------------------------------------------
      LOGICAL  ANYANG, PERFCT, PRNT(*)
      INTEGER  IPOLZN, MOMDIM, NUMANG, NMOM, i
      REAL     GQSC, MIMCUT, PMOM( 0:MOMDIM, * ), QEXT, QSCA, SPIKE,
     $         XMU(*), XX, radius, pi, lambda
      COMPLEX  CREFIN, SFORW, SBACK, S1(*), S2(*), TFORW(*), TBACK(*)
C ----------------------------------------------------------------------
C
      PARAMETER ( MAXANG = 501, MXANG2 = MAXANG/2 + 1 )
C
C                                  ** NOTE --  MAXTRM = 10100  IS NECES-
C                                  ** SARY TO DO SOME OF THE TEST PROBS,
C                                  ** BUT 1100 IS SUFFICIENT FOR MOST
C                                  ** CONCEIVABLE APPLICATIONS
      PARAMETER ( MAXTRM = 10100 )  ! A.K.
c      PARAMETER ( MAXTRM = 1100 )
      PARAMETER ( ONETHR = 1./3. )
C
      LOGICAL   CALCMO(4), NOABS, OK, PASS1, YESANG
      INTEGER   NPQUAN
      REAL      MIM, MRE, MM, NP1DN
      REAL      RBIGA( MAXTRM ), PIN( MAXANG ), PINM1( MAXANG )
      COMPLEX   AN, BN, ANM1, BNM1, ANP, BNP, ANPM, BNPM,
     $          CDENAN, CDENBN, CIOR, CIORIV, CTMP, ZET, ZETNM1, ZETN
      COMPLEX   CBIGA( MAXTRM ), LITA( MAXTRM ), LITB( MAXTRM ),
     $          SP( MAXANG ), SM( MAXANG ), SPS( MXANG2 ), SMS( MXANG2 )
      SAVE  PASS1
      SQ( CTMP ) = REAL( CTMP )**2 + AIMAG( CTMP )**2
      DATA  PASS1 / .TRUE. /
C
C
C                    ** SAVE SOME INPUT VARIABLES AND REPLACE THEM
C                    ** WITH VALUES NEEDED TO DO THE SELF-TEST
      IF ( PASS1 )
     $     CALL  TESTMI( .FALSE., XX, CREFIN, MIMCUT, PERFCT, ANYANG,
     $                  NMOM, IPOLZN, NUMANG, XMU, QEXT, QSCA, GQSC,
     $                  SFORW, SBACK, S1, S2, TFORW, TBACK, PMOM,
     $                  MOMDIM )
C                                        ** CHECK INPUT AND CALCULATE
C                                        ** CERTAIN VARIABLES FROM INPUT
C
   10 CALL  CKINMI( NUMANG, MAXANG, XX, PERFCT, CREFIN, MOMDIM,
     $              NMOM, IPOLZN, ANYANG, XMU, CALCMO, NPQUAN )
C
      IF ( PERFCT .AND. XX .LE. 0.1 )  THEN
C                                            ** USE TOTALLY-REFLECTING
C                                            ** SMALL-PARTICLE LIMIT
C
         CALL  SMALL1 ( XX, NUMANG, XMU, QEXT, QSCA, GQSC, SFORW,
     $                  SBACK, S1, S2, TFORW, TBACK, LITA, LITB )
         NTRM = 2
         GO TO 200
      END IF
C
      IF ( .NOT.PERFCT )  THEN
C
         CIOR = CREFIN
         IF ( AIMAG( CIOR ) .GT. 0.0 )  CIOR = CONJG( CIOR )
         MRE =     REAL( CIOR )
         MIM =  - AIMAG( CIOR )
         NOABS = MIM .LE. MIMCUT
         CIORIV = 1.0 / CIOR
         RIORIV = 1.0 / MRE
C
         IF ( XX * AMAX1( 1.0, CABS(CIOR) ) .LE. 0.1 ) THEN
C
C                                    ** USE GENERAL-REFRACTIVE-INDEX
C                                    ** SMALL-PARTICLE LIMIT
C                                    ** ( REF. 2, P. 1508 )
C
            CALL  SMALL2 ( XX, CIOR, .NOT.NOABS, NUMANG, XMU, QEXT,
     $                     QSCA, GQSC, SFORW, SBACK, S1, S2, TFORW,
     $                     TBACK, LITA, LITB )
            NTRM = 2
            GO TO 200
         END IF
C
      END IF
C
      NANGD2 = ( NUMANG + 1 ) / 2
      YESANG = NUMANG .GT. 0
C                              ** ESTIMATE NUMBER OF TERMS IN MIE SERIES
C                              ** ( REF. 2, P. 1508 )
      IF ( XX.LE.8.0 )  THEN
         NTRM = XX + 4. * XX**ONETHR + 1.
      ELSE IF ( XX.LT.4200. )  THEN
         NTRM = XX + 4.05 * XX**ONETHR + 2.
      ELSE
         NTRM = XX + 4. * XX**ONETHR + 2.
      END IF
      IF ( NTRM+1 .GT. MAXTRM )
     $     CALL ERRMSGA( 'MIEV0--PARAMETER MAXTRM TOO SMALL', .TRUE. )
C
C                            ** CALCULATE LOGARITHMIC DERIVATIVES OF
C                            ** J-BESSEL-FCN., BIG-A-SUB-(1 TO NTRM)
      IF ( .NOT.PERFCT )
     $     CALL  BIGA( CIOR, XX, NTRM, NOABS, YESANG, RBIGA, CBIGA )
C
C                            ** INITIALIZE RICATTI-BESSEL FUNCTIONS
C                            ** (PSI,CHI,ZETA)-SUB-(0,1) FOR UPWARD
C                            ** RECURRENCE ( REF. 1, EQ. 19 )
      XINV = 1.0 / XX
      PSINM1   = SIN( XX )
      CHINM1   = COS( XX )
      PSIN = PSINM1 * XINV - CHINM1
      CHIN = CHINM1 * XINV + PSINM1
      ZETNM1 = CMPLX( PSINM1, CHINM1 )
      ZETN   = CMPLX( PSIN, CHIN )
C                                     ** INITIALIZE PREVIOUS COEFFI-
C                                     ** CIENTS FOR -GQSC- SERIES
      ANM1 = ( 0.0, 0.0 )
      BNM1 = ( 0.0, 0.0 )
C                             ** INITIALIZE ANGULAR FUNCTION LITTLE-PI
C                             ** AND SUMS FOR S+, S- ( REF. 2, P. 1507 )
      IF ( ANYANG )  THEN
         DO  60  J = 1, NUMANG
            PINM1( J ) = 0.0
            PIN( J )   = 1.0
            SP ( J ) = ( 0.0, 0.0 )
            SM ( J ) = ( 0.0, 0.0 )
   60    CONTINUE
      ELSE
         DO  70  J = 1, NANGD2
            PINM1( J ) = 0.0
            PIN( J )   = 1.0
            SP ( J ) = ( 0.0, 0.0 )
            SM ( J ) = ( 0.0, 0.0 )
            SPS( J ) = ( 0.0, 0.0 )
            SMS( J ) = ( 0.0, 0.0 )
   70    CONTINUE
      END IF
C                         ** INITIALIZE MIE SUMS FOR EFFICIENCIES, ETC.
      QSCA = 0.0
      GQSC = 0.0
      SFORW      = ( 0., 0. )
      SBACK      = ( 0., 0. )
      TFORW( 1 ) = ( 0., 0. )
      TBACK( 1 ) = ( 0., 0. )
C
C
C ---------  LOOP TO SUM MIE SERIES  -----------------------------------
C
      MM = + 1.0
      SPIKE = 1.0
      DO  100  N = 1, NTRM
C                           ** COMPUTE VARIOUS NUMERICAL COEFFICIENTS
         FN     = N
         RN     = 1.0 / FN
         NP1DN  = 1.0 + RN
         TWONP1 = 2 * N + 1
         COEFF  = TWONP1 / ( FN * ( N + 1 ) )
         TCOEF  = TWONP1 * ( FN * ( N + 1 ) )
C
C                              ** CALCULATE MIE SERIES COEFFICIENTS
         IF ( PERFCT )  THEN
C                                   ** TOTALLY-REFLECTING CASE
C
            AN = ( ( FN*XINV ) * PSIN - PSINM1 ) /
     $           ( ( FN*XINV ) * ZETN - ZETNM1 )
            BN = PSIN / ZETN
C
         ELSE IF ( NOABS )  THEN
C                                      ** NO-ABSORPTION CASE
C
            CDENAN = ( RIORIV*RBIGA(N) + ( FN*XINV ) ) * ZETN - ZETNM1
            AN =  ( ( RIORIV*RBIGA(N) + ( FN*XINV ) ) * PSIN - PSINM1 )
     $            / CDENAN
            CDENBN = (  MRE * RBIGA(N) + ( FN*XINV ) ) * ZETN - ZETNM1
            BN =  ( (  MRE * RBIGA(N) + ( FN*XINV ) ) * PSIN - PSINM1 )
     $            / CDENBN
         ELSE
C                                       ** ABSORPTIVE CASE
C
            CDENAN = ( CIORIV * CBIGA(N) + ( FN*XINV ) ) * ZETN - ZETNM1
            CDENBN = (   CIOR * CBIGA(N) + ( FN*XINV ) ) * ZETN - ZETNM1
            AN = ( ( CIORIV * CBIGA(N) + ( FN*XINV ) ) * PSIN - PSINM1 )
     $           / CDENAN
            BN = ( (   CIOR * CBIGA(N) + ( FN*XINV ) ) * PSIN - PSINM1 )
     $           / CDENBN
            QSCA = QSCA + TWONP1 * ( SQ( AN ) + SQ( BN ) )
C
         END IF
C                       ** SAVE MIE COEFFICIENTS FOR *PMOM* CALCULATION
         LITA( N ) = AN
         LITB( N ) = BN
C
         IF ( .NOT.PERFCT .AND. N.GT.XX )  THEN
C                                               ** FLAG RESONANCE SPIKES
            DENAN = CABS( CDENAN )
            DENBN = CABS( CDENBN )
            RATIO = DENAN / DENBN
            IF( RATIO.LE.0.2 .OR. RATIO.GE.5.0 )
     $          SPIKE = AMIN1( SPIKE, DENAN, DENBN )
         END IF
C                                  ** INCREMENT MIE SUMS FOR NON-ANGLE-
C                                  ** DEPENDENT QUANTITIES
C
         SFORW      = SFORW      + TWONP1 * ( AN + BN )
         TFORW( 1 ) = TFORW( 1 ) + TCOEF  * ( AN - BN )
         SBACK      = SBACK      + ( MM * TWONP1 ) * ( AN - BN )
         TBACK( 1 ) = TBACK( 1 ) + ( MM * TCOEF )  * ( AN + BN )
         GQSC = GQSC + ( FN - RN ) * REAL( ANM1 * CONJG( AN )
     $                                   + BNM1 * CONJG( BN ) )
     $          + COEFF * REAL( AN * CONJG( BN ) )
C
         IF ( YESANG )  THEN
C                                      ** PUT MIE COEFFICIENTS IN FORM
C                                      ** NEEDED FOR COMPUTING S+, S-
C                                      ** ( REF. 2, P. 1507 )
            ANP = COEFF * ( AN + BN )
            BNP = COEFF * ( AN - BN )
C                                      ** INCREMENT MIE SUMS FOR S+, S-
C                                      ** WHILE UPWARD RECURSING
C                                      ** ANGULAR FUNCTIONS LITTLE PI
C                                      ** AND LITTLE TAU
            IF ( ANYANG )  THEN
C                                         ** ARBITRARY ANGLES
C
C                                              ** VECTORIZABLE LOOP
               DO  80  J = 1, NUMANG
                  RTMP = ( XMU( J ) * PIN( J ) ) - PINM1( J )
                  TAUN =  FN * RTMP - PINM1( J )
                  SP( J )  = SP( J ) + ANP * ( PIN( J ) + TAUN )
                  SM( J )  = SM( J ) + BNP * ( PIN( J ) - TAUN )
                  PINM1( J ) = PIN( J )
                  PIN( J ) = ( XMU( J ) * PIN( J ) ) + NP1DN * RTMP
   80          CONTINUE
C
            ELSE
C                                  ** ANGLES SYMMETRIC ABOUT 90 DEGREES
               ANPM = MM * ANP
               BNPM = MM * BNP
C                                          ** VECTORIZABLE LOOP
               DO  90  J = 1, NANGD2
                  RTMP = ( XMU( J ) * PIN( J ) ) - PINM1( J )
                  TAUN =  FN * RTMP - PINM1( J )
                  SP ( J ) = SP ( J ) +  ANP * ( PIN( J ) + TAUN )
                  SMS( J ) = SMS( J ) + BNPM * ( PIN( J ) + TAUN )
                  SM ( J ) = SM ( J ) +  BNP * ( PIN( J ) - TAUN )
                  SPS( J ) = SPS( J ) + ANPM * ( PIN( J ) - TAUN )
                  PINM1( J ) = PIN( J )
                  PIN( J ) = ( XMU( J ) * PIN( J ) ) + NP1DN * RTMP
   90          CONTINUE
C
            END IF
         END IF
C                          ** UPDATE RELEVANT QUANTITIES FOR NEXT
C                          ** PASS THROUGH LOOP
         MM   =  - MM
         ANM1 = AN
         BNM1 = BN
C                           ** UPWARD RECURRENCE FOR RICATTI-BESSEL
C                           ** FUNCTIONS ( REF. 1, EQ. 17 )
C
         ZET    = ( TWONP1 * XINV ) * ZETN - ZETNM1
         ZETNM1 = ZETN
         ZETN   = ZET
         PSINM1 = PSIN
         PSIN   = REAL( ZETN )
  100 CONTINUE
C
C ---------- END LOOP TO SUM MIE SERIES --------------------------------
C
C
      QEXT = 2. / XX**2 * REAL( SFORW )
      IF ( PERFCT .OR. NOABS )  THEN
         QSCA = QEXT
      ELSE
         QSCA = 2. / XX**2 * QSCA
      END IF
C
      GQSC = 4. / XX**2 * GQSC
      SFORW = 0.5 * SFORW
      SBACK = 0.5 * SBACK
      TFORW( 2 ) = 0.5 * (   SFORW + 0.25 * TFORW( 1 ) )
      TFORW( 1 ) = 0.5 * (   SFORW - 0.25 * TFORW( 1 ) )
      TBACK( 2 ) = 0.5 * (   SBACK + 0.25 * TBACK( 1 ) )
      TBACK( 1 ) = 0.5 * ( - SBACK + 0.25 * TBACK( 1 ) )
C
      IF ( YESANG )  THEN
C                                ** RECOVER SCATTERING AMPLITUDES
C                                ** FROM S+, S- ( REF. 1, EQ. 11 )
         IF ( ANYANG )  THEN
C                                         ** VECTORIZABLE LOOP
            DO  110  J = 1, NUMANG
               S1( J ) = 0.5 * ( SP( J ) + SM( J ) )
               S2( J ) = 0.5 * ( SP( J ) - SM( J ) )
  110       CONTINUE
C
         ELSE
C                                         ** VECTORIZABLE LOOP
            DO  120  J = 1, NANGD2
               S1( J ) = 0.5 * ( SP( J ) + SM( J ) )
               S2( J ) = 0.5 * ( SP( J ) - SM( J ) )
  120       CONTINUE
C                                         ** VECTORIZABLE LOOP
            DO  130  J = 1, NANGD2
               S1( NUMANG+1 - J ) = 0.5 * ( SPS( J ) + SMS( J ) )
               S2( NUMANG+1 - J ) = 0.5 * ( SPS( J ) - SMS( J ) )
  130       CONTINUE
         END IF
C
      END IF
C                                         ** CALCULATE LEGENDRE MOMENTS
  200 IF ( NMOM.GT.0 )
     $     CALL LPCOEF ( NTRM, NMOM, IPOLZN, MOMDIM, CALCMO, NPQUAN,
     $                   LITA, LITB, PMOM )
C
      IF ( AIMAG(CREFIN) .GT. 0.0 )  THEN
C                                         ** TAKE COMPLEX CONJUGATES
C                                         ** OF SCATTERING AMPLITUDES
         SFORW = CONJG( SFORW )
         SBACK = CONJG( SBACK )
         DO  210  I = 1, 2
            TFORW( I ) = CONJG( TFORW(I) )
            TBACK( I ) = CONJG( TBACK(I) )
  210    CONTINUE
C
         DO  220  J = 1, NUMANG
            S1( J ) = CONJG( S1(J) )
            S2( J ) = CONJG( S2(J) )
  220    CONTINUE
C
      END IF
C
      IF ( PASS1 )  THEN
C                           ** COMPARE TEST CASE RESULTS WITH
C                           ** CORRECT ANSWERS AND ABORT IF BAD;
C                           ** OTHERWISE RESTORE USER INPUT AND PROCEED
C
         CALL  TESTMI ( .TRUE., XX, CREFIN, MIMCUT, PERFCT, ANYANG,
     $                  NMOM, IPOLZN, NUMANG, XMU, QEXT, QSCA, GQSC,
     $                  SFORW, SBACK, S1, S2, TFORW, TBACK, PMOM,
     $                  MOMDIM )
         PASS1 = .FALSE.
         GO TO 10
C
      END IF
C
      IF ( PRNT(1) .OR. PRNT(2) )
     $   CALL  MIPRNT( PRNT, XX, PERFCT, CREFIN, NUMANG, XMU, QEXT,
     $                 QSCA, GQSC, NMOM, IPOLZN, MOMDIM, CALCMO,
     $                 PMOM, SFORW, SBACK, TFORW, TBACK, S1, S2 )
C
      RETURN
C
      END
      SUBROUTINE  CKINMI( NUMANG, MAXANG, XX, PERFCT, CREFIN, MOMDIM,
     $                    NMOM, IPOLZN, ANYANG, XMU, CALCMO, NPQUAN )
C
C        CHECK FOR BAD INPUT TO 'MIEV0' AND CALCULATE -CALCMO,NPQUAN-
C
C     ROUTINES CALLED :  ERRMSGA, WRTBADA
C
      LOGICAL  WRTBADA, WRTDIMA
      LOGICAL  PERFCT, ANYANG, CALCMO(*)
      INTEGER  NUMANG, MAXANG, MOMDIM, NMOM, IPOLZN, NPQUAN
      REAL     XX, XMU(*)
      COMPLEX  CREFIN
C
      CHARACTER*4  STRING
      LOGICAL  INPERR
C
C
      INPERR = .FALSE.
      IF ( NUMANG.GT.MAXANG )  INPERR =  WRTDIMA( 'MAXANG', NUMANG )
      IF ( NUMANG.LT.0 )  INPERR =  WRTBADA( 'NUMANG' )
      IF ( XX.LT.0. )     INPERR =  WRTBADA( 'XX' )
      IF ( .NOT.PERFCT .AND. REAL(CREFIN).LE.0. )
     $     INPERR = WRTBADA( 'CREFIN' )
      IF ( MOMDIM.LT.1 )  INPERR = WRTBADA( 'MOMDIM' )
C
      IF ( NMOM.NE.0 )  THEN
         IF ( NMOM.LT.0 .OR. NMOM.GT.MOMDIM ) INPERR = WRTBADA('NMOM')
         IF ( IABS(IPOLZN).GT.4444 )  INPERR =  WRTBADA( 'IPOLZN' )
         NPQUAN = 0
         DO 5  L = 1, 4
            CALCMO( L ) = .FALSE.
    5    CONTINUE
         IF ( IPOLZN.NE.0 )  THEN
C                                 ** PARSE OUT -IPOLZN- INTO ITS DIGITS
C                                 ** TO FIND WHICH PHASE QUANTITIES ARE
C                                 ** TO HAVE THEIR MOMENTS CALCULATED
C
            WRITE( STRING, '(I4)' )  IABS(IPOLZN)
            DO 10  J = 1, 4
               IP = ICHAR( STRING(J:J) ) - ICHAR( '0' )
               IF ( IP.GE.1 .AND. IP.LE.4 )  CALCMO( IP ) = .TRUE.
               IF ( IP.EQ.0 .OR. (IP.GE.5 .AND. IP.LE.9) )
     $              INPERR =  WRTBADA( 'IPOLZN' )
               NPQUAN = MAX0( NPQUAN, IP )
   10       CONTINUE
         END IF
      END IF
C
      IF ( ANYANG )  THEN
C                                ** ALLOW FOR SLIGHT IMPERFECTIONS IN
C                                ** COMPUTATION OF COSINE
          DO  20  I = 1, NUMANG
             IF ( XMU(I).LT.-1.00001 .OR. XMU(I).GT.1.00001 )
     $            INPERR = WRTBADA( 'XMU' )
   20     CONTINUE
      ELSE
          DO  22  I = 1, ( NUMANG + 1 ) / 2
             IF ( XMU(I).LT.-0.00001 .OR. XMU(I).GT.1.00001 )
     $            INPERR = WRTBADA( 'XMU' )
   22     CONTINUE
      END IF
C
      IF ( INPERR )
     $     CALL ERRMSGA( 'MIEV0--INPUT ERROR(S).  ABORTING...', .TRUE. )
C
      IF ( XX.GT.20000.0 .OR. REAL(CREFIN).GT.10.0 .OR.
     $     ABS( AIMAG(CREFIN) ).GT.10.0 )  CALL  ERRMSGA(
     $     'MIEV0--XX OR CREFIN OUTSIDE TESTED RANGE', .FALSE. )
C
      RETURN
      END
      SUBROUTINE  LPCOEF ( NTRM, NMOM, IPOLZN, MOMDIM, CALCMO, NPQUAN,
     $                     A, B, PMOM )
C
C         CALCULATE LEGENDRE POLYNOMIAL EXPANSION COEFFICIENTS (ALSO
C         CALLED MOMENTS) FOR PHASE QUANTITIES ( REF. 5 FORMULATION )
C
C     INPUT:  NTRM                    NUMBER TERMS IN MIE SERIES
C             NMOM, IPOLZN, MOMDIM    'MIEV0' ARGUMENTS
C             CALCMO                  FLAGS CALCULATED FROM -IPOLZN-
C             NPQUAN                  DEFINED IN 'MIEV0'
C             A, B                    MIE SERIES COEFFICIENTS
C
C     OUTPUT: PMOM                   LEGENDRE MOMENTS ('MIEV0' ARGUMENT)
C
C     ROUTINES CALLED :  ERRMSGA, LPCO1T, LPCO2T
C
C     *** NOTES ***
C
C         (1)  EQS. 2-5 ARE IN ERROR IN DAVE, APPL. OPT. 9,
C         1888 (1970).  EQ. 2 REFERS TO M1, NOT M2;  EQ. 3 REFERS TO
C         M2, NOT M1.  IN EQS. 4 AND 5, THE SUBSCRIPTS ON THE SECOND
C         TERM IN SQUARE BRACKETS SHOULD BE INTERCHANGED.
C
C         (2)  THE GENERAL-CASE LOGIC IN THIS SUBROUTINE WORKS CORRECTLY
C         IN THE TWO-TERM MIE SERIES CASE, BUT SUBROUTINE  'LPCO2T'
C         IS CALLED INSTEAD, FOR SPEED.
C
C         (3)  SUBROUTINE  'LPCO1T', TO DO THE ONE-TERM CASE, IS NEVER
C         CALLED WITHIN THE CONTEXT OF 'MIEV0', BUT IS INCLUDED FOR
C         COMPLETE GENERALITY.
C
C         (4)  SOME IMPROVEMENT IN SPEED IS OBTAINABLE BY COMBINING THE
C         310- AND 410-LOOPS, IF MOMENTS FOR BOTH THE THIRD AND FOURTH
C         PHASE QUANTITIES ARE DESIRED, BECAUSE THE THIRD PHASE QUANTITY
C         IS THE REAL PART OF A COMPLEX SERIES, WHILE THE FOURTH PHASE
C         QUANTITY IS THE IMAGINARY PART OF THAT VERY SAME SERIES.  BUT
C         MOST USERS ARE NOT INTERESTED IN THE FOURTH PHASE QUANTITY,
C         WHICH IS RELATED TO CIRCULAR POLARIZATION, SO THE PRESENT
C         SCHEME IS USUALLY MORE EFFICIENT.
C
      LOGICAL  CALCMO(*)
      INTEGER  IPOLZN, MOMDIM, NMOM, NTRM, NPQUAN
      REAL     PMOM( 0:MOMDIM, * )
      COMPLEX  A(*), B(*)
C
C           ** SPECIFICATION OF LOCAL VARIABLES
C
C      AM(M)       NUMERICAL COEFFICIENTS  A-SUB-M-SUPER-L
C                     IN DAVE, EQS. 1-15, AS SIMPLIFIED IN REF. 5.
C
C      BI(I)       NUMERICAL COEFFICIENTS  B-SUB-I-SUPER-L
C                     IN DAVE, EQS. 1-15, AS SIMPLIFIED IN REF. 5.
C
C      BIDEL(I)    1/2 BI(I) TIMES FACTOR CAPITAL-DEL IN DAVE
C
C      CM,DM()     ARRAYS C AND D IN DAVE, EQS. 16-17 (MUELLER FORM),
C                     CALCULATED USING RECURRENCE DERIVED IN REF. 5
C
C      CS,DS()     ARRAYS C AND D IN REF. 4, EQS. A5-A6 (SEKERA FORM),
C                     CALCULATED USING RECURRENCE DERIVED IN REF. 5
C
C      C,D()       EITHER -CM,DM- OR -CS,DS-, DEPENDING ON -IPOLZN-
C
C      EVENL       TRUE FOR EVEN-NUMBERED MOMENTS;  FALSE OTHERWISE
C
C      IDEL        1 + LITTLE-DEL  IN DAVE
C
C      MAXTRM      MAX. NO. OF TERMS IN MIE SERIES
C
C      MAXMOM      MAX. NO. OF NON-ZERO MOMENTS
C
C      NUMMOM      NUMBER OF NON-ZERO MOMENTS
C
C      RECIP(K)    1 / K
C
      PARAMETER  ( MAXTRM = 1102, MAXMOM = 2*MAXTRM, MXMOM2 = MAXMOM/2,
     $             MAXRCP = 4*MAXTRM + 2 )
      REAL       AM( 0:MAXTRM ), BI( 0:MXMOM2 ), BIDEL( 0:MXMOM2 ),
     $           RECIP( MAXRCP )
      COMPLEX    CM( MAXTRM ), DM( MAXTRM ), CS( MAXTRM ), DS( MAXTRM ),
     $           C( MAXTRM ), D( MAXTRM )
      EQUIVALENCE  ( C, CM ),  ( D, DM )
      LOGICAL    PASS1, EVENL
      SAVE  PASS1, RECIP
      DATA  PASS1 / .TRUE. /
C
C
      IF ( PASS1 )  THEN
C
         DO  1  K = 1, MAXRCP
            RECIP( K ) = 1.0 / K
    1    CONTINUE
         PASS1 = .FALSE.
C
      END IF
C
      DO  5  J = 1, MAX0( 1, NPQUAN )
         DO  5  L = 0, NMOM
            PMOM( L, J ) = 0.0
    5 CONTINUE
C
      IF ( NTRM.EQ.1 )  THEN
         CALL  LPCO1T ( NMOM, IPOLZN, MOMDIM, CALCMO, A, B, PMOM )
         RETURN
      ELSE IF ( NTRM.EQ.2 )  THEN
         CALL  LPCO2T ( NMOM, IPOLZN, MOMDIM, CALCMO, A, B, PMOM )
         RETURN
      END IF
C
      IF ( NTRM+2 .GT. MAXTRM )
     $     CALL ERRMSGA( 'LPCOEF--PARAMETER MAXTRM TOO SMALL', .TRUE. )
C
C                                     ** CALCULATE MUELLER C, D ARRAYS
      CM( NTRM+2 ) = ( 0., 0. )
      DM( NTRM+2 ) = ( 0., 0. )
      CM( NTRM+1 ) = ( 1. - RECIP( NTRM+1 ) ) * B( NTRM )
      DM( NTRM+1 ) = ( 1. - RECIP( NTRM+1 ) ) * A( NTRM )
      CM( NTRM ) = ( RECIP(NTRM) + RECIP(NTRM+1) ) * A( NTRM )
     $             + ( 1. - RECIP(NTRM) ) * B( NTRM-1 )
      DM( NTRM ) = ( RECIP(NTRM) + RECIP(NTRM+1) ) * B( NTRM )
     $             + ( 1. - RECIP(NTRM) ) * A( NTRM-1 )
C
      DO  10  K = NTRM-1, 2, -1
         CM( K ) = CM( K+2 ) - ( 1. + RECIP(K+1) ) * B( K+1 )
     $                       + ( RECIP(K) + RECIP(K+1) ) * A( K )
     $                       + ( 1. - RECIP(K) ) * B( K-1 )
         DM( K ) = DM( K+2 ) - ( 1. + RECIP(K+1) ) * A( K+1 )
     $                       + ( RECIP(K) + RECIP(K+1) ) * B( K )
     $                       + ( 1. - RECIP(K) ) * A( K-1 )
   10 CONTINUE
      CM( 1 ) = CM( 3 ) + 1.5 * ( A( 1 ) - B( 2 ) )
      DM( 1 ) = DM( 3 ) + 1.5 * ( B( 1 ) - A( 2 ) )
C
      IF ( IPOLZN.GE.0 )  THEN
C
         DO  20  K = 1, NTRM + 2
            C( K ) = ( 2*K - 1 ) * CM( K )
            D( K ) = ( 2*K - 1 ) * DM( K )
   20    CONTINUE
C
      ELSE
C                                    ** COMPUTE SEKERA C AND D ARRAYS
         CS( NTRM+2 ) = ( 0., 0. )
         DS( NTRM+2 ) = ( 0., 0. )
         CS( NTRM+1 ) = ( 0., 0. )
         DS( NTRM+1 ) = ( 0., 0. )
C
         DO  30  K = NTRM, 1, -1
            CS( K ) = CS( K+2 ) + ( 2*K + 1 ) * ( CM( K+1 ) - B( K ) )
            DS( K ) = DS( K+2 ) + ( 2*K + 1 ) * ( DM( K+1 ) - A( K ) )
   30    CONTINUE
C
         DO  40  K = 1, NTRM + 2
            C( K ) = ( 2*K - 1 ) * CS( K )
            D( K ) = ( 2*K - 1 ) * DS( K )
   40    CONTINUE
C
      END IF
C
C
      IF( IPOLZN.LT.0 )  NUMMOM = MIN0( NMOM, 2*NTRM - 2 )
      IF( IPOLZN.GE.0 )  NUMMOM = MIN0( NMOM, 2*NTRM )
      IF ( NUMMOM .GT. MAXMOM )
     $     CALL ERRMSGA( 'LPCOEF--PARAMETER MAXTRM TOO SMALL', .TRUE. )
C
C                               ** LOOP OVER MOMENTS
      DO  500  L = 0, NUMMOM
         LD2 = L / 2
         EVENL = MOD( L,2 ) .EQ. 0
C                                    ** CALCULATE NUMERICAL COEFFICIENTS
C                                    ** A-SUB-M AND B-SUB-I IN DAVE
C                                    ** DOUBLE-SUMS FOR MOMENTS
         IF( L.EQ.0 )  THEN
C
            IDEL = 1
            DO  60  M = 0, NTRM
               AM( M ) = 2.0 * RECIP( 2*M + 1 )
   60       CONTINUE
            BI( 0 ) = 1.0
C
         ELSE IF( EVENL )  THEN
C
            IDEL = 1
            DO  70  M = LD2, NTRM
               AM( M ) = ( 1. + RECIP( 2*M-L+1 ) ) * AM( M )
   70       CONTINUE
            DO  75  I = 0, LD2-1
               BI( I ) = ( 1. - RECIP( L-2*I ) ) * BI( I )
   75       CONTINUE
            BI( LD2 ) = ( 2. - RECIP( L ) ) * BI( LD2-1 )
C
         ELSE
C
            IDEL = 2
            DO  80  M = LD2, NTRM
               AM( M ) = ( 1. - RECIP( 2*M+L+2 ) ) * AM( M )
   80       CONTINUE
            DO  85  I = 0, LD2
               BI( I ) = ( 1. - RECIP( L+2*I+1 ) ) * BI( I )
   85       CONTINUE
C
         END IF
C                                     ** ESTABLISH UPPER LIMITS FOR SUMS
C                                     ** AND INCORPORATE FACTOR CAPITAL-
C                                     ** DEL INTO B-SUB-I
         MMAX = NTRM - IDEL
         IF( IPOLZN.GE.0 )  MMAX = MMAX + 1
         IMAX = MIN0( LD2, MMAX - LD2 )
         IF( IMAX.LT.0 )  GO TO 600
         DO  90  I = 0, IMAX
            BIDEL( I ) = BI( I )
   90    CONTINUE
         IF( EVENL )  BIDEL( 0 ) = 0.5 * BIDEL( 0 )
C
C                                    ** PERFORM DOUBLE SUMS JUST FOR
C                                    ** PHASE QUANTITIES DESIRED BY USER
         IF( IPOLZN.EQ.0 )  THEN
C
            DO  110  I = 0, IMAX
C                                           ** VECTORIZABLE LOOP (CRAY)
               SUM = 0.0
               DO  100  M = LD2, MMAX - I
                  SUM = SUM + AM( M ) *
     $                      ( REAL( C(M-I+1) * CONJG( C(M+I+IDEL) ) )
     $                      + REAL( D(M-I+1) * CONJG( D(M+I+IDEL) ) ) )
  100          CONTINUE
               PMOM( L,1 ) = PMOM( L,1 ) + BIDEL( I ) * SUM
  110       CONTINUE
            PMOM( L,1 ) = 0.5 * PMOM( L,1 )
            GO TO 500
C
         END IF
C
         IF ( CALCMO(1) )  THEN
            DO  160  I = 0, IMAX
C                                           ** VECTORIZABLE LOOP (CRAY)
               SUM = 0.0
               DO  150  M = LD2, MMAX - I
                  SUM = SUM + AM( M ) *
     $                        REAL( C(M-I+1) * CONJG( C(M+I+IDEL) ) )
  150          CONTINUE
               PMOM( L,1 ) = PMOM( L,1 ) + BIDEL( I ) * SUM
  160       CONTINUE
         END IF
C
C
         IF ( CALCMO(2) )  THEN
            DO  210  I = 0, IMAX
C                                           ** VECTORIZABLE LOOP (CRAY)
               SUM = 0.0
               DO  200  M = LD2, MMAX - I
                  SUM = SUM + AM( M ) *
     $                        REAL( D(M-I+1) * CONJG( D(M+I+IDEL) ) )
  200          CONTINUE
               PMOM( L,2 ) = PMOM( L,2 ) + BIDEL( I ) * SUM
  210       CONTINUE
         END IF
C
C
         IF ( CALCMO(3) )  THEN
            DO  310  I = 0, IMAX
C                                           ** VECTORIZABLE LOOP (CRAY)
               SUM = 0.0
               DO  300  M = LD2, MMAX - I
                  SUM = SUM + AM( M ) *
     $                      ( REAL( C(M-I+1) * CONJG( D(M+I+IDEL) ) )
     $                      + REAL( C(M+I+IDEL) * CONJG( D(M-I+1) ) ) )
  300          CONTINUE
               PMOM( L,3 ) = PMOM( L,3 ) + BIDEL( I ) * SUM
  310       CONTINUE
            PMOM( L,3 ) = 0.5 * PMOM( L,3 )
         END IF
C
C
         IF ( CALCMO(4) )  THEN
            DO  410  I = 0, IMAX
C                                           ** VECTORIZABLE LOOP (CRAY)
               SUM = 0.0
               DO  400  M = LD2, MMAX - I
                  SUM = SUM + AM( M ) *
     $                      ( AIMAG( C(M-I+1) * CONJG( D(M+I+IDEL) ) )
     $                      + AIMAG( C(M+I+IDEL) * CONJG( D(M-I+1) ) ) )
  400          CONTINUE
               PMOM( L,4 ) = PMOM( L,4 ) + BIDEL( I ) * SUM
  410       CONTINUE
            PMOM( L,4 ) = - 0.5 * PMOM( L,4 )
         END IF
C
  500 CONTINUE
C
C
  600 RETURN
      END
      SUBROUTINE  LPCO1T ( NMOM, IPOLZN, MOMDIM, CALCMO, A, B, PMOM )
C
C         CALCULATE LEGENDRE POLYNOMIAL EXPANSION COEFFICIENTS (ALSO
C         CALLED MOMENTS) FOR PHASE QUANTITIES IN SPECIAL CASE WHERE
C         NO. TERMS IN MIE SERIES = 1
C
C        INPUT:  NMOM, IPOLZN, MOMDIM     'MIEV0' ARGUMENTS
C                CALCMO                   FLAGS CALCULATED FROM -IPOLZN-
C                A(1), B(1)               MIE SERIES COEFFICIENTS
C
C        OUTPUT: PMOM                     LEGENDRE MOMENTS
C
      LOGICAL  CALCMO(*)
      INTEGER  IPOLZN, MOMDIM, NMOM
      REAL     PMOM( 0:MOMDIM, * )
      COMPLEX  A(*), B(*), CTMP, A1B1C
      SQ( CTMP ) = REAL( CTMP )**2 + AIMAG( CTMP )**2
C
C
      A1SQ = SQ( A(1) )
      B1SQ = SQ( B(1) )
      A1B1C = A(1) * CONJG( B(1) )
C
      IF( IPOLZN.LT.0 )  THEN
C
         IF( CALCMO(1) )  PMOM( 0,1 ) = 2.25 * B1SQ
         IF( CALCMO(2) )  PMOM( 0,2 ) = 2.25 * A1SQ
         IF( CALCMO(3) )  PMOM( 0,3 ) = 2.25 * REAL( A1B1C )
         IF( CALCMO(4) )  PMOM( 0,4 ) = 2.25 *AIMAG( A1B1C )
C
      ELSE
C
         NUMMOM = MIN0( NMOM, 2 )
C                                   ** LOOP OVER MOMENTS
         DO  100  L = 0, NUMMOM
C
            IF( IPOLZN.EQ.0 )  THEN
               IF( L.EQ.0 )  PMOM( L,1 ) = 1.5 * ( A1SQ + B1SQ )
               IF( L.EQ.1 )  PMOM( L,1 ) = 1.5 * REAL( A1B1C )
               IF( L.EQ.2 )  PMOM( L,1 ) = 0.15 * ( A1SQ + B1SQ )
               GO TO 100
            END IF
C
            IF( CALCMO(1) )  THEN
               IF( L.EQ.0 )  PMOM( L,1 ) = 2.25 * ( A1SQ + B1SQ / 3. )
               IF( L.EQ.1 )  PMOM( L,1 ) = 1.5 * REAL( A1B1C )
               IF( L.EQ.2 )  PMOM( L,1 ) = 0.3 * B1SQ
            END IF
C
            IF( CALCMO(2) )  THEN
               IF( L.EQ.0 )  PMOM( L,2 ) = 2.25 * ( B1SQ + A1SQ / 3. )
               IF( L.EQ.1 )  PMOM( L,2 ) = 1.5 * REAL( A1B1C )
               IF( L.EQ.2 )  PMOM( L,2 ) = 0.3 * A1SQ
            END IF
C
            IF( CALCMO(3) )  THEN
               IF( L.EQ.0 )  PMOM( L,3 ) = 3.0 * REAL( A1B1C )
               IF( L.EQ.1 )  PMOM( L,3 ) = 0.75 * ( A1SQ + B1SQ )
               IF( L.EQ.2 )  PMOM( L,3 ) = 0.3 * REAL( A1B1C )
            END IF
C
            IF( CALCMO(4) )  THEN
               IF( L.EQ.0 )  PMOM( L,4 ) = - 1.5 * AIMAG( A1B1C )
               IF( L.EQ.1 )  PMOM( L,4 ) = 0.0
               IF( L.EQ.2 )  PMOM( L,4 ) = 0.3 * AIMAG( A1B1C )
            END IF
C
  100    CONTINUE
C
      END IF
C
      RETURN
      END
      SUBROUTINE  LPCO2T ( NMOM, IPOLZN, MOMDIM, CALCMO, A, B, PMOM )
C
C         CALCULATE LEGENDRE POLYNOMIAL EXPANSION COEFFICIENTS (ALSO
C         CALLED MOMENTS) FOR PHASE QUANTITIES IN SPECIAL CASE WHERE
C         NO. TERMS IN MIE SERIES = 2
C
C        INPUT:  NMOM, IPOLZN, MOMDIM     'MIEV0' ARGUMENTS
C                CALCMO                   FLAGS CALCULATED FROM -IPOLZN-
C                A(1-2), B(1-2)           MIE SERIES COEFFICIENTS
C
C        OUTPUT: PMOM                     LEGENDRE MOMENTS
C
      LOGICAL  CALCMO(*)
      INTEGER  IPOLZN, MOMDIM, NMOM
      REAL     PMOM( 0:MOMDIM, * )
      COMPLEX  A(*), B(*)
      COMPLEX  A2C, B2C, CTMP, CA, CAC, CAT, CB, CBC, CBT, CG, CH
      SQ( CTMP ) = REAL( CTMP )**2 + AIMAG( CTMP )**2
C
C
      CA = 3. * A(1) - 5. * B(2)
      CAT= 3. * B(1) - 5. * A(2)
      CAC = CONJG( CA )
      A2SQ = SQ( A(2) )
      B2SQ = SQ( B(2) )
      A2C = CONJG( A(2) )
      B2C = CONJG( B(2) )
C
      IF( IPOLZN.LT.0 )  THEN
C                                   ** LOOP OVER SEKERA MOMENTS
         NUMMOM = MIN0( NMOM, 2 )
         DO  50  L = 0, NUMMOM
C
            IF( CALCMO(1) )  THEN
               IF( L.EQ.0 ) PMOM( L,1 ) = 0.25 * ( SQ(CAT) +
     $                                             (100./3.) * B2SQ )
               IF( L.EQ.1 ) PMOM( L,1 ) = (5./3.) * REAL( CAT * B2C )
               IF( L.EQ.2 ) PMOM( L,1 ) = (10./3.) * B2SQ
            END IF
C
            IF( CALCMO(2) )  THEN
               IF( L.EQ.0 ) PMOM( L,2 ) = 0.25 * ( SQ(CA) +
     $                                             (100./3.) * A2SQ )
               IF( L.EQ.1 ) PMOM( L,2 ) = (5./3.) * REAL( CA * A2C )
               IF( L.EQ.2 ) PMOM( L,2 ) = (10./3.) * A2SQ
            END IF
C
            IF( CALCMO(3) )  THEN
               IF( L.EQ.0 ) PMOM( L,3 ) = 0.25 * REAL( CAT*CAC +
     $                                           (100./3.)*B(2)*A2C )
               IF( L.EQ.1 ) PMOM( L,3 ) = 5./6. * REAL( B(2)*CAC +
     $                                                  CAT*A2C )
               IF( L.EQ.2 ) PMOM( L,3 ) = 10./3. * REAL( B(2) * A2C )
            END IF
C
            IF( CALCMO(4) )  THEN
               IF( L.EQ.0 ) PMOM( L,4 ) = -0.25 * AIMAG( CAT*CAC +
     $                                           (100./3.)*B(2)*A2C )
               IF( L.EQ.1 ) PMOM( L,4 ) = -5./6. * AIMAG( B(2)*CAC +
     $                                                  CAT*A2C )
               IF( L.EQ.2 ) PMOM( L,4 ) = -10./3. * AIMAG( B(2) * A2C )
            END IF
C
   50    CONTINUE
C
      ELSE
C
         CB = 3. * B(1) + 5. * A(2)
         CBT= 3. * A(1) + 5. * B(2)
         CBC = CONJG( CB )
         CG = ( CBC*CBT + 10.*( CAC*A(2) + B2C*CAT) ) / 3.
         CH = 2.*( CBC*A(2) + B2C*CBT )
C
C                                   ** LOOP OVER MUELLER MOMENTS
         NUMMOM = MIN0( NMOM, 4 )
         DO  100  L = 0, NUMMOM
C
            IF( IPOLZN.EQ.0 .OR. CALCMO(1) )  THEN
               IF( L.EQ.0 ) PM1 = 0.25 * SQ(CA) + SQ(CB) / 12.
     $                            + (5./3.) * REAL(CA*B2C) + 5.*B2SQ
               IF( L.EQ.1 ) PM1 = REAL( CB * ( CAC/6. + B2C ) )
               IF( L.EQ.2 ) PM1 = SQ(CB)/30. + (20./7.) * B2SQ
     $                            + (2./3.) * REAL( CA * B2C )
               IF( L.EQ.3 ) PM1 = (2./7.) * REAL( CB * B2C )
               IF( L.EQ.4 ) PM1 = (40./63.) * B2SQ
               IF ( CALCMO(1) )  PMOM( L,1 ) = PM1
            END IF
C
            IF( IPOLZN.EQ.0 .OR. CALCMO(2) )  THEN
               IF( L.EQ.0 ) PM2 = 0.25*SQ(CAT) + SQ(CBT) / 12.
     $                            + (5./3.) * REAL(CAT*A2C) + 5.*A2SQ
               IF( L.EQ.1 ) PM2 = REAL( CBT * ( CONJG(CAT)/6. + A2C) )
               IF( L.EQ.2 ) PM2 = SQ(CBT)/30. + (20./7.) * A2SQ
     $                            + (2./3.) * REAL( CAT * A2C )
               IF( L.EQ.3 ) PM2 = (2./7.) * REAL( CBT * A2C )
               IF( L.EQ.4 ) PM2 = (40./63.) * A2SQ
               IF ( CALCMO(2) )  PMOM( L,2 ) = PM2
            END IF
C
            IF( IPOLZN.EQ.0 )  THEN
               PMOM( L,1 ) = 0.5 * ( PM1 + PM2 )
               GO TO 100
            END IF
C
            IF( CALCMO(3) )  THEN
               IF( L.EQ.0 ) PMOM( L,3 ) = 0.25 * REAL( CAC*CAT + CG +
     $                                                 20.*B2C*A(2) )
               IF( L.EQ.1 ) PMOM( L,3 ) = REAL( CAC*CBT + CBC*CAT +
     $                                          3.*CH ) / 12.
               IF( L.EQ.2 ) PMOM( L,3 ) = 0.1 * REAL( CG + (200./7.) *
     $                                                B2C * A(2) )
               IF( L.EQ.3 ) PMOM( L,3 ) = REAL( CH ) / 14.
               IF( L.EQ.4 ) PMOM( L,3 ) = 40./63. * REAL( B2C * A(2) )
            END IF
C
            IF( CALCMO(4) )  THEN
               IF( L.EQ.0 ) PMOM( L,4 ) = 0.25 * AIMAG( CAC*CAT + CG +
     $                                                  20.*B2C*A(2) )
               IF( L.EQ.1 ) PMOM( L,4 ) = AIMAG( CAC*CBT + CBC*CAT +
     $                                           3.*CH ) / 12.
               IF( L.EQ.2 ) PMOM( L,4 ) = 0.1 * AIMAG( CG + (200./7.) *
     $                                                 B2C * A(2) )
               IF( L.EQ.3 ) PMOM( L,4 ) = AIMAG( CH ) / 14.
               IF( L.EQ.4 ) PMOM( L,4 ) = 40./63. * AIMAG( B2C * A(2) )
            END IF
C
  100    CONTINUE
C
      END IF
C
      RETURN
      END
      SUBROUTINE  BIGA( CIOR, XX, NTRM, NOABS, YESANG, RBIGA, CBIGA )
C
C        CALCULATE LOGARITHMIC DERIVATIVES OF J-BESSEL-FUNCTION
C
C     INPUT :  CIOR, XX, NTRM, NOABS, YESANG  (DEFINED IN 'MIEV0')
C
C    OUTPUT :  RBIGA OR CBIGA  (DEFINED IN 'MIEV0')
C
C    ROUTINES CALLED :  CONFRA
C
C    INTERNAL VARIABLES :
C
C       CONFRA     VALUE OF LENTZ CONTINUED FRACTION FOR -CBIGA(NTRM)-,
C                     USED TO INITIALIZE DOWNWARD RECURRENCE.
C       DOWN       = TRUE, USE DOWN-RECURRENCE.  FALSE, DO NOT.
C       F1,F2,F3   ARITHMETIC STATEMENT FUNCTIONS USED IN DETERMINING
C                     WHETHER TO USE UP-  OR DOWN-RECURRENCE
C                     ( REF. 2, EQS. 6-8 )
C       MRE        REAL REFRACTIVE INDEX
C       MIM        IMAGINARY REFRACTIVE INDEX
C       REZINV     1 / ( MRE * XX ); TEMPORARY VARIABLE FOR RECURRENCE
C       ZINV       1 / ( CIOR * XX ); TEMPORARY VARIABLE FOR RECURRENCE
C
      LOGICAL  DOWN, NOABS, YESANG
      INTEGER  NTRM
      REAL     MRE, MIM, RBIGA(*), XX, REZINV, RTMP
      COMPLEX  CIOR, CTMP, CONFRA, CBIGA(*), ZINV
      F1( MRE ) =  - 8.0 + MRE**2 * ( 26.22 + MRE * ( - 0.4474
     $             + MRE**3 * ( 0.00204 - 0.000175 * MRE ) ) )
      F2( MRE ) = 3.9 + MRE * ( - 10.8 + 13.78 * MRE )
      F3( MRE ) =  - 15.04 + MRE * ( 8.42 + 16.35 * MRE )
C
C                                  ** DECIDE WHETHER 'BIGA' CAN BE
C                                  ** CALCULATED BY UP-RECURRENCE
      MRE =  REAL( CIOR )
      MIM =  ABS( AIMAG( CIOR ) )
      IF ( MRE.LT.1.0 .OR. MRE.GT.10.0 .OR. MIM.GT.10.0 )  THEN
         DOWN = .TRUE.
      ELSE IF ( YESANG )  THEN
         DOWN = .TRUE.
         IF ( MIM*XX .LT. F2( MRE ) )  DOWN = .FALSE.
      ELSE
         DOWN = .TRUE.
         IF ( MIM*XX .LT. F1( MRE ) )  DOWN = .FALSE.
      END IF
C
      ZINV  = 1.0 / ( CIOR * XX )
      REZINV = 1.0 / ( MRE * XX )
C
      IF ( DOWN )  THEN
C                          ** COMPUTE INITIAL HIGH-ORDER 'BIGA' USING
C                          ** LENTZ METHOD ( REF. 1, PP. 17-20 )
C
         CTMP = CONFRA( NTRM, ZINV )
C
C                                   *** DOWNWARD RECURRENCE FOR 'BIGA'
C                                   *** ( REF. 1, EQ. 22 )
         IF ( NOABS )  THEN
C                                            ** NO-ABSORPTION CASE
            RBIGA( NTRM ) = REAL( CTMP )
            DO  25  N = NTRM, 2, - 1
               RBIGA( N-1 ) = (N*REZINV)
     $                         - 1.0 / ( (N*REZINV) + RBIGA( N ) )
   25       CONTINUE
C
         ELSE
C                                            ** ABSORPTIVE CASE
            CBIGA( NTRM ) = CTMP
            DO  30  N = NTRM, 2, - 1
               CBIGA( N-1 ) = (N*ZINV) - 1.0 / ( (N*ZINV) + CBIGA( N ) )
   30       CONTINUE
C
         END IF
C
      ELSE
C                              *** UPWARD RECURRENCE FOR 'BIGA'
C                              *** ( REF. 1, EQS. 20-21 )
         IF ( NOABS )  THEN
C                                            ** NO-ABSORPTION CASE
            RTMP = SIN( MRE*XX )
            RBIGA( 1 ) =  - REZINV
     $                     + RTMP / ( RTMP*REZINV - COS( MRE*XX ) )
            DO  40  N = 2, NTRM
               RBIGA( N ) = - ( N*REZINV )
     $                       + 1.0 / ( ( N*REZINV ) - RBIGA( N-1 ) )
   40       CONTINUE
C
         ELSE
C                                                ** ABSORPTIVE CASE
            CTMP = CEXP( - (0.,2.) * CIOR * XX )
            CBIGA( 1 ) = - ZINV + (1.-CTMP) /
     $                          ( ZINV * (1.-CTMP) - (0.,1.)*(1.+CTMP) )
            DO  50  N = 2, NTRM
               CBIGA( N ) = - (N*ZINV) + 1.0 / ((N*ZINV) - CBIGA( N-1 ))
   50       CONTINUE
         END IF
C
      END IF
C
      RETURN
      END
      COMPLEX FUNCTION  CONFRA( N, ZINV )
C
C         COMPUTE BESSEL FUNCTION RATIO CAPITAL-A-SUB-N FROM ITS
C         CONTINUED FRACTION USING LENTZ METHOD ( REF. 1, PP. 17-20 )
C
C         ZINV = RECIPROCAL OF ARGUMENT OF CAPITAL-A
C
C    I N T E R N A L    V A R I A B L E S
C    ------------------------------------
C
C    CAK      TERM IN CONTINUED FRACTION EXPANSION OF CAPITAL-A
C                ( REF. 1, EQ. 25 )
C    CAPT     FACTOR USED IN LENTZ ITERATION FOR CAPITAL-A
C                ( REF. 1, EQ. 27 )
C    CDENOM   DENOMINATOR IN -CAPT-  ( REF. 1, EQ. 28B )
C    CNUMER   NUMERATOR   IN -CAPT-  ( REF. 1, EQ. 28A )
C    CDTD     PRODUCT OF TWO SUCCESSIVE DENOMINATORS OF -CAPT-
C                FACTORS  ( REF. 1, EQ. 34C )
C    CNTN     PRODUCT OF TWO SUCCESSIVE NUMERATORS OF -CAPT-
C                FACTORS  ( REF. 1, EQ. 34B )
C    EPS1     ILL-CONDITIONING CRITERION
C    EPS2     CONVERGENCE CRITERION
C    KK       SUBSCRIPT K OF -CAK-  ( REF. 1, EQ. 25B )
C    KOUNT    ITERATION COUNTER ( USED ONLY TO PREVENT RUNAWAY )
C    MAXIT    MAX. ALLOWED NO. OF ITERATIONS
C    MM       + 1  AND - 1, ALTERNATELY
C
      INTEGER   N
      COMPLEX   ZINV
      COMPLEX   CAK, CAPT, CDENOM, CDTD, CNUMER, CNTN
      DATA  EPS1 / 1.E - 2 /, EPS2 / 1.E - 8 /
      DATA  MAXIT / 10000 /
C
C                                      *** REF. 1, EQS. 25A, 27
      CONFRA = ( N + 1 ) * ZINV
      MM     =  - 1
      KK     = 2 * N + 3
      CAK    = ( MM * KK ) * ZINV
      CDENOM = CAK
      CNUMER = CDENOM + 1.0 / CONFRA
      KOUNT  = 1
C
   20 KOUNT = KOUNT + 1
      IF ( KOUNT.GT.MAXIT )
     $  CALL ERRMSGA( 'CONFRA--ITERATION FAILED TO CONVERGE$', .TRUE.)
C
C                                         *** REF. 2, EQ. 25B
      MM  =  - MM
      KK  = KK + 2
      CAK = ( MM * KK ) * ZINV
C                                         *** REF. 2, EQ. 32
      IF (      CABS( CNUMER/CAK ).LE.EPS1
     $     .OR. CABS( CDENOM/CAK ).LE.EPS1 )  THEN
C
C                                  ** ILL-CONDITIONED CASE -- STRIDE
C                                  ** TWO TERMS INSTEAD OF ONE
C
C                                         *** REF. 2, EQS. 34
         CNTN   = CAK * CNUMER + 1.0
         CDTD   = CAK * CDENOM + 1.0
         CONFRA = ( CNTN / CDTD ) * CONFRA
C                                             *** REF. 2, EQ. 25B
         MM  =  - MM
         KK  = KK + 2
         CAK = ( MM * KK ) * ZINV
C                                         *** REF. 2, EQS. 35
         CNUMER = CAK + CNUMER / CNTN
         CDENOM = CAK + CDENOM / CDTD
         KOUNT  = KOUNT + 1
         GO TO 20
C
      ELSE
C                                ** WELL-CONDITIONED CASE
C
C                                        *** REF. 2, EQS. 26, 27
         CAPT   = CNUMER / CDENOM
         CONFRA = CAPT * CONFRA
C                                    ** CHECK FOR CONVERGENCE
C                                    ** ( REF. 2, EQ. 31 )
C
         IF (      ABS( REAL (CAPT) - 1.0 ).GE.EPS2
     $        .OR. ABS( AIMAG(CAPT) )      .GE.EPS2 )  THEN
C
C                                        *** REF. 2, EQS. 30A-B
            CNUMER = CAK + 1.0 / CNUMER
            CDENOM = CAK + 1.0 / CDENOM
            GO TO 20
         END IF
      END IF
C
      RETURN
C
      END
      SUBROUTINE  MIPRNT( PRNT, XX, PERFCT, CREFIN, NUMANG, XMU,
     $                    QEXT, QSCA, GQSC, NMOM, IPOLZN, MOMDIM,
     $                    CALCMO, PMOM, SFORW, SBACK, TFORW, TBACK,
     $                    S1, S2 )
C
C         PRINT SCATTERING QUANTITIES OF A SINGLE PARTICLE
C
      LOGICAL  PERFCT, PRNT(*), CALCMO(*)
      INTEGER  IPOLZN, MOMDIM, NMOM, NUMANG
      REAL     GQSC, PMOM( 0:MOMDIM, * ), QEXT, QSCA, XX, XMU(*)
      COMPLEX  CREFIN, SFORW, SBACK, TFORW(*), TBACK(*), S1(*), S2(*)
      CHARACTER*22  FMT
C
C
      IF ( PERFCT )  WRITE ( *, '(''1'',10X,A,1P,E11.4)' )
     $                'PERFECTLY CONDUCTING CASE, SIZE PARAMETER =', XX
cak      IF ( .NOT.PERFCT )  WRITE ( *, '(''1'',10X,3(A,1P,E11.4))' )
cak     $                  'REFRACTIVE INDEX:  REAL ', REAL(CREFIN),
cak     $             '  IMAG ', AIMAG(CREFIN), ',   SIZE PARAMETER =', XX
C
      IF ( PRNT(1) .AND. NUMANG.GT.0 )  THEN
C
         WRITE ( *, '(/,A)' )
     $    '    COS(ANGLE)  ------- S1 ---------  ------- S2 ---------'//
     $    '  --- S1*CONJG(S2) ---   I1=S1**2   I2=S2**2  (I1+I2)/2'//
     $    '  DEG POLZN'
         DO  10  I = 1, NUMANG
            FI1 = REAL( S1(I) ) **2 + AIMAG( S1(I) )**2
            FI2 = REAL( S2(I) ) **2 + AIMAG( S2(I) )**2
            WRITE( *, '( I4, F10.6, 1P,10E11.3 )'   )
     $              I, XMU(I), S1(I), S2(I), S1(I)*CONJG(S2(I)),
     $              FI1, FI2, 0.5*(FI1+FI2), (FI2-FI1)/(FI2+FI1)
   10    CONTINUE
C
      END IF
C
C
      IF ( PRNT(2) )  THEN
C
cak         WRITE ( *, '(/,A,9X,A,17X,A,17X,A,/,(0P,F7.2, 1P,6E12.3) )' )
cak     $           '  ANGLE', 'S-SUB-1', 'T-SUB-1', 'T-SUB-2',
cak     $               0.0,     SFORW,    TFORW(1),  TFORW(2),
cak     $              180.,     SBACK,    TBACK(1),  TBACK(2)
cak         WRITE ( *, '(/,4(A,1P,E11.4))' )
cak     $           ' EFFICIENCY FACTORS,  EXTINCTION:', QEXT,
cak     $                              '   SCATTERING:', QSCA,
cak     $                              '   ABSORPTION:', QEXT-QSCA,
cak     $                           '   RAD. PRESSURE:', QEXT-GQSC
C
         IF ( NMOM.GT.0 )  THEN
C
            WRITE( *, '(/,A)' )  ' NORMALIZED MOMENTS OF :'
            IF ( IPOLZN.EQ.0 )  WRITE ( *, '(''+'',27X,A)' ) 'PHASE FCN'
            IF ( IPOLZN.GT.0 )  WRITE ( *, '(''+'',33X,A)' )
     $         'M1           M2          S21          D21'
            IF ( IPOLZN.LT.0 )  WRITE ( *, '(''+'',33X,A)' )
     $         'R1           R2           R3           R4'
C
            FNORM = 4. / ( XX**2 * QSCA )
            DO  20  M = 0, NMOM
               WRITE ( *, '(A,I4)' )  '      MOMENT NO.', M
               DO 20  J = 1, 4
                  IF( CALCMO(J) )  THEN
                     WRITE( FMT, 98 )  24 + (J-1)*13
                     WRITE ( *,FMT )  FNORM * PMOM(M,J)
                  END IF
   20       CONTINUE
         END IF
C
      END IF
C
      RETURN
C
   98 FORMAT( '( ''+'', T', I2, ', 1P,E13.4 )' )
      END
      SUBROUTINE  SMALL1 ( XX, NUMANG, XMU, QEXT, QSCA, GQSC, SFORW,
     $                     SBACK, S1, S2, TFORW, TBACK, A, B )
C
C       SMALL-PARTICLE LIMIT OF MIE QUANTITIES IN TOTALLY REFLECTING
C       LIMIT ( MIE SERIES TRUNCATED AFTER 2 TERMS )
C
C        A,B       FIRST TWO MIE COEFFICIENTS, WITH NUMERATOR AND
C                  DENOMINATOR EXPANDED IN POWERS OF -XX- ( A FACTOR
C                  OF XX**3 IS MISSING BUT IS RESTORED BEFORE RETURN
C                  TO CALLING PROGRAM )  ( REF. 2, P. 1508 )
C
      INTEGER  NUMANG
      REAL     GQSC, QEXT, QSCA, XX, XMU(*)
      COMPLEX  A(*), B(*), SFORW, SBACK, S1(*), S2(*),
     $         TFORW(*), TBACK(*)
C
      PARAMETER  ( TWOTHR = 2./3., FIVTHR = 5./3., FIVNIN = 5./9. )
      COMPLEX    CTMP
      SQ( CTMP ) = REAL( CTMP )**2 + AIMAG( CTMP )**2
C
C
      A( 1 ) = CMPLX ( 0., TWOTHR * ( 1. - 0.2 * XX**2 ) )
     $       / CMPLX ( 1. - 0.5 * XX**2, TWOTHR * XX**3 )
C
      B( 1 ) = CMPLX ( 0., - ( 1. - 0.1 * XX**2 ) / 3. )
     $       / CMPLX ( 1. + 0.5 * XX**2, - XX**3 / 3. )
C
      A( 2 ) = CMPLX ( 0.,   XX**2 / 30. )
      B( 2 ) = CMPLX ( 0., - XX**2 / 45. )
C
      QSCA = 6. * XX**4 * ( SQ( A(1) ) + SQ( B(1) )
     $                      + FIVTHR * ( SQ( A(2) ) + SQ( B(2) ) ) )
      QEXT = QSCA
      GQSC = 6. * XX**4 * REAL( A(1) * CONJG( A(2) + B(1) )
     $                    + ( B(1) + FIVNIN * A(2) ) * CONJG( B(2) ) )
C
      RTMP = 1.5 * XX**3
      SFORW      = RTMP * ( A(1) + B(1) + FIVTHR * ( A(2) + B(2) ) )
      SBACK      = RTMP * ( A(1) - B(1) - FIVTHR * ( A(2) - B(2) ) )
      TFORW( 1 ) = RTMP * ( B(1) + FIVTHR * ( 2.*B(2) - A(2) ) )
      TFORW( 2 ) = RTMP * ( A(1) + FIVTHR * ( 2.*A(2) - B(2) ) )
      TBACK( 1 ) = RTMP * ( B(1) - FIVTHR * ( 2.*B(2) + A(2) ) )
      TBACK( 2 ) = RTMP * ( A(1) - FIVTHR * ( 2.*A(2) + B(2) ) )
C
      DO  10  J = 1, NUMANG
         S1( J ) = RTMP * ( A(1) + B(1) * XMU(J) + FIVTHR *
     $              ( A(2) * XMU(J) + B(2) * ( 2.*XMU(J)**2 - 1. )) )
         S2( J ) = RTMP * ( B(1) + A(1) * XMU(J) + FIVTHR *
     $              ( B(2) * XMU(J) + A(2) * ( 2.*XMU(J)**2 - 1. )) )
   10 CONTINUE
C                                     ** RECOVER ACTUAL MIE COEFFICIENTS
      A( 1 ) = XX**3 * A( 1 )
      A( 2 ) = XX**3 * A( 2 )
      B( 1 ) = XX**3 * B( 1 )
      B( 2 ) = XX**3 * B( 2 )
C
      RETURN
      END
      SUBROUTINE  SMALL2 ( XX, CIOR, CALCQE, NUMANG, XMU, QEXT, QSCA,
     $                     GQSC, SFORW, SBACK, S1, S2, TFORW, TBACK,
     $                     A, B )
C
C       SMALL-PARTICLE LIMIT OF MIE QUANTITIES FOR GENERAL REFRACTIVE
C       INDEX ( MIE SERIES TRUNCATED AFTER 2 TERMS )
C
C        A,B       FIRST TWO MIE COEFFICIENTS, WITH NUMERATOR AND
C                  DENOMINATOR EXPANDED IN POWERS OF -XX- ( A FACTOR
C                  OF XX**3 IS MISSING BUT IS RESTORED BEFORE RETURN
C                  TO CALLING PROGRAM )  ( REF. 2, P. 1508 )
C
C        CIORSQ    SQUARE OF REFRACTIVE INDEX
C
      LOGICAL  CALCQE
      INTEGER  NUMANG
      REAL     GQSC, QEXT, QSCA, XX, XMU(*)
      COMPLEX  A(*), B(*), CIOR, SFORW, SBACK, S1(*), S2(*),
     $         TFORW(*), TBACK(*)
C
      PARAMETER  ( TWOTHR = 2./3., FIVTHR = 5./3. )
      COMPLEX  CTMP, CIORSQ
      SQ( CTMP ) = REAL( CTMP )**2 + AIMAG( CTMP )**2
C
C
      CIORSQ = CIOR**2
      CTMP = CMPLX( 0., TWOTHR ) * ( CIORSQ - 1.0 )
      A(1) = CTMP * ( 1.0 - 0.1 * XX**2 + (CIORSQ/350. + 1./280.)*XX**4)
     $       / ( CIORSQ + 2.0 + ( 1.0 - 0.7 * CIORSQ ) * XX**2
     $           - ( CIORSQ**2/175. - 0.275 * CIORSQ + 0.25 ) * XX**4
     $           + XX**3 * CTMP * ( 1.0 - 0.1 * XX**2 ) )
C
      B(1) = (XX**2/30.) * CTMP * ( 1.0 + (CIORSQ/35. - 1./14.) *XX**2 )
     $       / ( 1.0 - ( CIORSQ/15. - 1./6. ) * XX**2 )
C
      A(2) = ( 0.1 * XX**2 ) * CTMP * ( 1.0 - XX**2 / 14. )
     $       / ( 2. * CIORSQ + 3. - ( CIORSQ/7. - 0.5 ) * XX**2 )
C
      QSCA = 6. * XX**4 * ( SQ(A(1)) + SQ(B(1)) + FIVTHR * SQ(A(2)) )
      GQSC = 6. * XX**4 * REAL( A(1) * CONJG( A(2) + B(1) ) )
      QEXT = QSCA
      IF ( CALCQE ) QEXT = 6. * XX * REAL( A(1) + B(1) + FIVTHR * A(2) )
C
      RTMP = 1.5 * XX**3
      SFORW      = RTMP * ( A(1) + B(1) + FIVTHR * A(2) )
      SBACK      = RTMP * ( A(1) - B(1) - FIVTHR * A(2) )
      TFORW( 1 ) = RTMP * ( B(1) - FIVTHR * A(2) )
      TFORW( 2 ) = RTMP * ( A(1) + 2. * FIVTHR * A(2) )
      TBACK( 1 ) = TFORW( 1 )
      TBACK( 2 ) = RTMP * ( A(1) - 2. * FIVTHR * A(2) )
C
      DO  10  J = 1, NUMANG
         S1( J ) = RTMP * ( A(1) + ( B(1) + FIVTHR * A(2) ) * XMU(J) )
         S2( J ) = RTMP * ( B(1) + A(1) * XMU(J) + FIVTHR * A(2)
     $                      * ( 2. * XMU(J)**2 - 1. ) )
   10 CONTINUE
C                                     ** RECOVER ACTUAL MIE COEFFICIENTS
      A( 1 ) = XX**3 * A( 1 )
      A( 2 ) = XX**3 * A( 2 )
      B( 1 ) = XX**3 * B( 1 )
      B( 2 ) = ( 0., 0. )
C
      RETURN
      END
      SUBROUTINE  TESTMI ( COMPAR, XX, CREFIN, MIMCUT, PERFCT, ANYANG,
     $                     NMOM, IPOLZN, NUMANG, XMU, QEXT, QSCA, GQSC,
     $                     SFORW, SBACK, S1, S2, TFORW, TBACK, PMOM,
     $                     MOMDIM )
C
C         SET UP TO RUN TEST CASE WHEN  COMPAR = FALSE;  WHEN  = TRUE,
C         COMPARE MIE CODE TEST CASE RESULTS WITH CORRECT ANSWERS
C         AND ABORT IF EVEN ONE RESULT IS INACCURATE.
C
C         THE TEST CASE IS :  MIE SIZE PARAMETER = 10
C                             REFRACTIVE INDEX   = 1.5 - 0.1 I
C                             SCATTERING ANGLE = 140 DEGREES
C                             1 SEKERA MOMENT
C
C         RESULTS FOR THIS CASE MAY BE FOUND AMONG THE TEST CASES
C         AT THE END OF REFERENCE (1).
C
C         *** NOTE *** WHEN RUNNING ON SOME COMPUTERS, ESP. IN SINGLE
C         PRECISION, THE 'ACCUR' CRITERION BELOW MAY HAVE TO BE RELAXED.
C         HOWEVER, IF 'ACCUR' MUST BE SET LARGER THAN 10**-3 FOR SOME
C         SIZE PARAMETERS, YOUR COMPUTER IS PROBABLY NOT ACCURATE
C         ENOUGH TO DO MIE COMPUTATIONS FOR THOSE SIZE PARAMETERS.
C
C     ROUTINES CALLED :  ERRMSGA, MIPRNT, TSTBADA
C
      LOGICAL  TSTBADA
      LOGICAL  COMPAR, ANYANG, PERFCT
      INTEGER  IPOLZN, MOMDIM, NUMANG, NMOM
      REAL     MIMCUT, QEXT, QSCA, GQSC, PMOM( 0:MOMDIM, * ), XMU(*), XX
      COMPLEX  CREFIN, SFORW, SBACK, S1(*), S2(*), TFORW(*), TBACK(*)
C
      LOGICAL  ANYSAV, PERSAV, CALCMO(4), PRNT(2), OK, WRONG
      REAL     ACCUR, MIMSAV, TESTQE, TESTQS, TESTGQ, TESTPM( 0:1 )
      COMPLEX  CRESAV, TESTSF, TESTSB, TESTS1, TESTS2, TESTTF( 2 ),
     $         TESTTB( 2 )
      SAVE     XXSAV, CRESAV, MIMSAV, PERSAV, ANYSAV, NMOSAV, IPOSAV,
     $         NUMSAV, XMUSAV
      DATA   TESTQE / 2.459791 /,  TESTQS / 1.235144 /,
     $       TESTGQ / 1.139235 /,  TESTSF / ( 61.49476, -3.177994 ) /,
     $       TESTSB / ( 1.493434, 0.2963657 ) /,
     $       TESTS1 / ( -0.1548380, -1.128972) /,
     $       TESTS2 / ( 0.05669755, 0.5425681) /,
     $       TESTTF / ( 12.95238, -136.6436 ), ( 48.54238, 133.4656 ) /,
     $       TESTTB / ( 41.88414, -15.57833 ), ( 43.37758, -15.28196 )/,
     $       TESTPM / 227.1975, 183.6898 /
      DATA   ACCUR / 1.E-4 /
      WRONG( CALC, EXACT ) = ABS( (CALC - EXACT) / EXACT ) .GT. ACCUR
C
C
      IF  ( .NOT.COMPAR )  THEN
C                                   ** SAVE CERTAIN USER INPUT VALUES
         XXSAV  = XX
         CRESAV = CREFIN
         MIMSAV = MIMCUT
         PERSAV = PERFCT
         ANYSAV = ANYANG
         NMOSAV = NMOM
         IPOSAV = IPOLZN
         NUMSAV = NUMANG
         XMUSAV = XMU(1)
C                                   ** RESET INPUT VALUES FOR TEST CASE
         XX      = 10.0
         CREFIN  = ( 1.5, - 0.1 )
         MIMCUT  = 0.0
         PERFCT  = .FALSE.
         ANYANG  = .TRUE.
         NMOM    = 1
         IPOLZN  = - 1
         NUMANG  = 1
         XMU(1)  = - 0.7660444
C
      ELSE
C                                    ** COMPARE TEST CASE RESULTS WITH
C                                    ** CORRECT ANSWERS AND ABORT IF BAD
        OK = .TRUE.
        IF ( WRONG( QEXT,TESTQE ) )
     $       OK =  TSTBADA( 'QEXT', ABS((QEXT - TESTQE) / TESTQE) )
        IF ( WRONG( QSCA,TESTQS ) )
     $       OK =  TSTBADA( 'QSCA', ABS((QSCA - TESTQS) / TESTQS) )
        IF ( WRONG( GQSC,TESTGQ ) )
     $       OK =  TSTBADA( 'GQSC', ABS((GQSC - TESTGQ) / TESTGQ) )
C
        IF ( WRONG(  REAL(SFORW),  REAL(TESTSF) ) .OR.
     $       WRONG( AIMAG(SFORW), AIMAG(TESTSF) ) )
     $       OK =  TSTBADA( 'SFORW', CABS((SFORW - TESTSF) / TESTSF) )
C
        IF ( WRONG(  REAL(SBACK),  REAL(TESTSB) ) .OR.
     $       WRONG( AIMAG(SBACK), AIMAG(TESTSB) ) )
     $       OK =  TSTBADA( 'SBACK', CABS((SBACK - TESTSB) / TESTSB) )
C
        IF ( WRONG(  REAL(S1(1)),  REAL(TESTS1) ) .OR.
     $       WRONG( AIMAG(S1(1)), AIMAG(TESTS1) ) )
     $       OK =  TSTBADA( 'S1', CABS((S1(1) - TESTS1) / TESTS1) )
C
        IF ( WRONG(  REAL(S2(1)),  REAL(TESTS2) ) .OR.
     $       WRONG( AIMAG(S2(1)), AIMAG(TESTS2) ) )
     $       OK =  TSTBADA( 'S2', CABS((S2(1) - TESTS2) / TESTS2) )
C
        DO  20  N = 1, 2
           IF ( WRONG(  REAL(TFORW(N)),  REAL(TESTTF(N)) ) .OR.
     $          WRONG( AIMAG(TFORW(N)), AIMAG(TESTTF(N)) ) )
     $          OK =  TSTBADA( 'TFORW', CABS( (TFORW(N) - TESTTF(N)) /
     $                                       TESTTF(N) ) )
           IF ( WRONG(  REAL(TBACK(N)),  REAL(TESTTB(N)) ) .OR.
     $          WRONG( AIMAG(TBACK(N)), AIMAG(TESTTB(N)) ) )
     $          OK =  TSTBADA( 'TBACK', CABS( (TBACK(N) - TESTTB(N)) /
     $                                        TESTTB(N) ) )
   20    CONTINUE
C
         DO  30  M = 0, 1
            IF ( WRONG( PMOM(M,1), TESTPM(M) ) )
     $           OK =  TSTBADA( 'PMOM', ABS( (PMOM(M,1)-TESTPM(M)) /
     $                                      TESTPM(M) ) )
   30    CONTINUE
C
         IF ( .NOT. OK )  THEN
            PRNT(1) = .TRUE.
            PRNT(2) = .TRUE.
            CALCMO(1) = .TRUE.
            CALCMO(2) = .FALSE.
            CALCMO(3) = .FALSE.
            CALCMO(4) = .FALSE.
            CALL  MIPRNT( PRNT, XX, PERFCT, CREFIN, NUMANG, XMU, QEXT,
     $                    QSCA, GQSC, NMOM, IPOLZN, MOMDIM, CALCMO,
     $                    PMOM, SFORW, SBACK, TFORW, TBACK, S1, S2 )
            CALL ERRMSGA( 'MIEV0 -- SELF-TEST FAILED', .TRUE. )
         END IF
C                                       ** RESTORE USER INPUT VALUES
         XX     = XXSAV
         CREFIN = CRESAV
         MIMCUT = MIMSAV
         PERFCT = PERSAV
         ANYANG = ANYSAV
         NMOM   = NMOSAV
         IPOLZN = IPOSAV
         NUMANG = NUMSAV
         XMU(1) = XMUSAV
C
      END IF
C
      RETURN
      END
      LOGICAL FUNCTION  WRTBADA ( VARNAM )
C
C          WRITE NAMES OF ERRONEOUS VARIABLES AND RETURN 'TRUE'
C
C      INPUT :   VARNAM = NAME OF ERRONEOUS VARIABLE TO BE WRITTEN
C                         ( CHARACTER, ANY LENGTH )
C ----------------------------------------------------------------------
      CHARACTER*(*)  VARNAM
      INTEGER        MAXMSG, NUMMSG
      SAVE  NUMMSG, MAXMSG
      DATA  NUMMSG / 0 /,  MAXMSG / 50 /
C
C
      WRTBADA = .TRUE.
      NUMMSG = NUMMSG + 1
      WRITE ( *, '(3A)' )  ' ****  INPUT VARIABLE  ', VARNAM,
     $                     '  IN ERROR  ****'
      IF ( NUMMSG.EQ.MAXMSG )
     $ CALL ERRMSGA ( 'TOO MANY INPUT ERRORS.  ABORTING...$', .TRUE. )
      RETURN
      END
      LOGICAL FUNCTION  WRTDIMA ( DIMNAM, MINVAL )
C
C          WRITE NAME OF TOO-SMALL SYMBOLIC DIMENSION AND
C          THE VALUE IT SHOULD BE INCREASED TO;  RETURN 'TRUE'
C
C      INPUT :  DIMNAM = NAME OF SYMBOLIC DIMENSION WHICH IS TOO SMALL
C                        ( CHARACTER, ANY LENGTH )
C               MINVAL = VALUE TO WHICH THAT DIMENSION SHOULD BE
C                        INCREASED (AT LEAST)
C ----------------------------------------------------------------------
      CHARACTER*(*)  DIMNAM
      INTEGER        MINVAL
C
C
      WRITE ( *, '(3A,I7)' )  ' ****  SYMBOLIC DIMENSION  ', DIMNAM,
     $                     '  SHOULD BE INCREASED TO AT LEAST ', MINVAL
      WRTDIMA = .TRUE.
      RETURN
      END
      LOGICAL FUNCTION  TSTBADA( VARNAM, RELERR )
C
C       WRITE NAME (-VARNAM-) OF VARIABLE FAILING SELF-TEST AND ITS
C       PERCENT ERROR FROM THE CORRECT VALUE;  RETURN  'FALSE'.
C
      CHARACTER*(*)  VARNAM
      REAL           RELERR
C
C
      TSTBADA = .FALSE.
      WRITE( *, '(/,3A,1P,E11.2,A)' )
     $       ' OUTPUT VARIABLE  ', VARNAM,'  DIFFERED BY', 100.*RELERR,
     $       '  PER CENT FROM CORRECT VALUE.  SELF-TEST FAILED.'
      RETURN
      END
      SUBROUTINE  ERRMSGA( MESSAG, FATAL )
C
C        PRINT OUT A WARNING OR ERROR MESSAGE;  ABORT IF ERROR
C
      LOGICAL       FATAL, ONCE
      CHARACTER*(*) MESSAG
      INTEGER       MAXMSG, NUMMSG
      SAVE          MAXMSG, NUMMSG, ONCE
      DATA NUMMSG / 0 /,  MAXMSG / 100 /,  ONCE / .FALSE. /
C
C
      IF ( FATAL )  THEN
         WRITE ( *, '(2A)' )  ' ******* ERROR >>>>>>  ', MESSAG
         STOP
      END IF
C
      NUMMSG = NUMMSG + 1
      IF ( NUMMSG.GT.MAXMSG )  THEN
         IF ( .NOT.ONCE )  WRITE ( *,99 )
         ONCE = .TRUE.
      ELSE
         WRITE ( *, '(2A)' )  ' ******* WARNING >>>>>>  ', MESSAG
      ENDIF
C
      RETURN
C
   99 FORMAT( ///,' >>>>>>  TOO MANY WARNING MESSAGES --  ',
     $   'THEY WILL NO LONGER BE PRINTED  <<<<<<<', /// )
      END



