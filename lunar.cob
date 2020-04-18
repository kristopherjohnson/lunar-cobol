*> Translation of
*> <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
*> by Jim Storer from FOCAL to COBOL.

IDENTIFICATION DIVISION.
PROGRAM-ID. LunarLander.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> Constants

01 FeetPerMile                PIC 9(4)  VALUE 5280  USAGE COMP.
01 SecPerHour                 PIC 9(4)  VALUE 3600  USAGE COMP.
01 BlankLine                  PIC X     VALUE SPACE.

*> Global variables from original FOCAL code:
*>
*> A - Altitude (miles)
*> G - Gravity
*> I - Intermediate altitude (miles)
*> J - Intermediate velocity (miles/sec)
*> K - Fuel rate (lbs/sec)
*> L - Elapsed time (sec)
*> M - Total weight (lbs)
*> N - Empty weight (lbs, Note: M - N is remaining fuel weight)
*> Q - Temporary working variable
*> S - Time elapsed in current 10-second turn (sec)
*> T - Time remaining in current 10-second turn (sec)
*> V - Downward speed (miles/sec)
*> W - Temporary working variable
*> Z - Thrust per pound of fuel burned

01 Altitude                   PIC S9(6)V9(10)  USAGE COMP.
01 Gravity                    PIC S9(6)V9(10)  USAGE COMP.
01 I                          PIC S9(6)V9(10)  USAGE COMP.
01 J                          PIC S9(6)V9(10)  USAGE COMP.
01 FuelRate                   PIC S9(6)V9(10)  USAGE COMP.
01 ElapsedTime                PIC S9(6)V9(10)  USAGE COMP.
01 Weight                     PIC S9(6)V9(10)  USAGE COMP.
01 EmptyWeight                PIC S9(6)V9(10)  USAGE COMP.
01 Q                          PIC S9(6)V9(10)  USAGE COMP.
01 S                          PIC S9(6)V9(10)  USAGE COMP.
01 T                          PIC S9(6)V9(10)  USAGE COMP.
01 Velocity                   PIC S9(6)V9(10)  USAGE COMP.
01 W                          PIC S9(6)V9(10)  USAGE COMP.
01 Z                          PIC S9(6)V9(10)  USAGE COMP.

*> Variables used by Simulate and related paragraphs.

01 GameOverFlag               PIC 9.
    88 GameIsNotOver          VALUE 0.
    88 GameIsOver             VALUE 1.

01 Q2                         PIC S9(6)V9(10)  USAGE COMP.
01 Q3                         PIC S9(6)V9(10)  USAGE COMP.
01 Q4                         PIC S9(6)V9(10)  USAGE COMP.
01 Q5                         PIC S9(6)V9(10)  USAGE COMP.

*> Output Formatting

01 StatusRowHeadings.
    02 FILLER                 PIC X(12)  VALUE "TIME,SECS".
    02 FILLER                 PIC X(22)  VALUE "ALTITUDE,MILES+FEET".
    02 FILLER                 PIC X(15)  VALUE "VELOCITY,MPH".
    02 FILLER                 PIC X(11)  VALUE "FUEL,LBS".
    02 FILLER                 PIC X(9)   VALUE "FUEL RATE".

01 StatusRowData.
    02 ElapsedTimeDisplay     PIC -(6)9.
    02 AltitudeMilesDisplay   PIC -(15)9.
    02 AltitudeFeetDisplay    PIC -(6)9.
    02 VelocityMphDisplay     PIC -(11)9.99.
    02 FuelRemainingDisplay   PIC -(9)9.9.
    02 FILLER                 PIC X(6)  VALUE SPACES.

01 NotPossibleMessage.
    02 FILLER                 PIC X(12)  VALUE "NOT POSSIBLE".
    02 FILLER                 PIC X(51)  VALUE ALL '.'.

01 FuelOutTimeDisplay         PIC -(4)9.99.
01 ContactTimeDisplay         PIC -(4)9.99.
01 ImpactVelocityDisplay      PIC -(4)9.99.
01 FuelLeftDisplay            PIC -(4)9.99.
01 LunarCraterDisplay         PIC -(4)9.99.

*> User Input

01 FuelRateAnswer             PIC 999.
    88 IsValidFuelRate        VALUE 0, 8 THRU 200.

01 TryAgainAnswer             PIC X.
    88 TryAgain               VALUE "Y", "y".
    88 DontTryAgain           VALUE "N", "n".

PROCEDURE DIVISION.

*> (01.04 in original FOCAL code)
Begin.
    DISPLAY "CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY"
    DISPLAY "YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE"
    DISPLAY "BETWEEN 8 & 200 LBS/SEC. YOU'VE 16000 LBS FUEL. ESTIMATED"
    DISPLAY "FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS"
    DISPLAY BlankLine
    DISPLAY BlankLine

    PERFORM WITH TEST AFTER UNTIL DontTryAgain
        PERFORM PlayGame

        DISPLAY BlankLine
        DISPLAY BlankLine
        DISPLAY BlankLine
        DISPLAY "TRY AGAIN?"

        PERFORM WITH TEST AFTER UNTIL TryAgain OR DontTryAgain
            DISPLAY "(ANS. YES OR NO):" NO ADVANCING
            ACCEPT TryAgainAnswer
            *>TEST:DISPLAY TryAgainAnswer
        END-PERFORM
    END-PERFORM

    DISPLAY "CONTROL OUT"
    DISPLAY BlankLine
    DISPLAY BlankLine

    STOP RUN.

*> Play game until landing or crash.
PlayGame.
    DISPLAY "FIRST RADAR CHECK COMING UP"
    DISPLAY BlankLine
    DISPLAY BlankLine
    DISPLAY "COMMENCE LANDING PROCEDURE"
    DISPLAY StatusRowHeadings

    MOVE 120   TO Altitude
    MOVE 1     TO Velocity
    MOVE 32500 TO Weight
    MOVE 16500 TO EmptyWeight
    MOVE 0.001 TO Gravity
    MOVE 1.8   TO Z
    INITIALIZE ElapsedTime

    SET GameIsNotOver TO TRUE

    PERFORM UNTIL GameIsOver
        PERFORM GetFuelRate
        MOVE 10 TO T
        PERFORM Simulate
    END-PERFORM

    EXIT.

*> Display current status and prompt for new Fuel-Rate value until
*> valid answer is given.
GetFuelRate.
    COMPUTE ElapsedTimeDisplay ROUNDED = ElapsedTime
    MOVE FUNCTION INTEGER-PART(Altitude) TO AltitudeMilesDisplay
    COMPUTE AltitudeFeetDisplay ROUNDED =
        (Altitude - FUNCTION INTEGER-PART(Altitude)) * FeetPerMile
    MULTIPLY Velocity BY SecPerHour GIVING VelocityMphDisplay ROUNDED
    SUBTRACT EmptyWeight FROM Weight GIVING FuelRemainingDisplay ROUNDED

    DISPLAY StatusRowData NO ADVANCING

    PERFORM WITH TEST AFTER UNTIL IsValidFuelRate
        DISPLAY "K=:" NO ADVANCING
        ACCEPT FuelRateAnswer
        *>TEST:DISPLAY FuelRateAnswer
        IF IsValidFuelRate THEN
            MOVE FuelRateAnswer TO FuelRate
        ELSE
            DISPLAY NotPossibleMessage NO ADVANCING
        END-IF
    END-PERFORM

    EXIT.

*> Simulate T seconds using current fuel rate.
*> If out of fuel, continue until contact with surface.
*> On contact with surface, determine outcome and display score.
*> (03.10 in original FOCAL code)
Simulate.
    PERFORM UNTIL GameIsOver OR T < 0.001
        IF (Weight - EmptyWeight) < 0.001 THEN
            PERFORM FuelOut
        ELSE
            MOVE T TO S
            IF (S * FuelRate) > (Weight - EmptyWeight) THEN
                COMPUTE S ROUNDED = (Weight - EmptyWeight) / FuelRate
            END-IF
            PERFORM ApplyThrust
            IF I <= 0 THEN
                PERFORM UpdateUntilContact
            ELSE
                IF Velocity > 0 AND J < 0 THEN
                    PERFORM ApplyThrustLoop
                ELSE
                    PERFORM UpdateLanderState
                END-IF
            END-IF
        END-IF
    END-PERFORM
    EXIT.

*> (04.10 in original FOCAL code)
FuelOut.
    COMPUTE FuelOutTimeDisplay ROUNDED = ElapsedTime
    DISPLAY "FUEL OUT AT " FuelOutTimeDisplay " SECS"
    COMPUTE S ROUNDED =
        (FUNCTION SQRT(Velocity**2 + 2 * Altitude * Gravity) - Velocity)
        / Gravity
    COMPUTE Velocity ROUNDED = Velocity + Gravity * S
    ADD S to ElapsedTime ROUNDED
    PERFORM Contact
    EXIT.

*> Handle touchdown/crash
*> (05.10 in original FOCAL code)
Contact.
    COMPUTE ContactTimeDisplay ROUNDED = ElapsedTime
    DISPLAY "ON THE MOON AT " ContactTimeDisplay " SECS"

    *> W is velocity in miles-per-hour
    MULTIPLY SecPerHour BY Velocity GIVING W ROUNDED

    COMPUTE ImpactVelocityDisplay ROUNDED = W
    DISPLAY "IMPACT VELOCITY OF " ImpactVelocityDisplay " M.P.H."

    SUBTRACT EmptyWeight FROM Weight GIVING FuelLeftDisplay ROUNDED
    DISPLAY "FUEL LEFT: " FuelLeftDisplay " LBS"

    EVALUATE W
        WHEN <=  1 DISPLAY "PERFECT LANDING !-(LUCKY)"
        WHEN <= 10 DISPLAY "GOOD LANDING-(COULD BE BETTER)"
        WHEN <= 22 DISPLAY "CONGRATULATIONS ON A POOR LANDING"
        WHEN <= 40 DISPLAY "CRAFT DAMAGE. GOOD LUCK"
        WHEN <= 60 DISPLAY "CRASH LANDING-YOU'VE 5 HRS OXYGEN"
        WHEN OTHER
            PERFORM
                DISPLAY "SORRY,BUT THERE WERE NO SURVIVORS-YOU BLEW IT!"
                COMPUTE LunarCraterDisplay ROUNDED = W * 0.277777
                DISPLAY
                    "IN FACT YOU BLASTED A NEW LUNAR CRATER "
                    LunarCraterDisplay " FT. DEEP"
            END-PERFORM
    END-EVALUATE

    SET GameIsOver TO TRUE
    EXIT.

*> (06.10 in original FOCAL code)
UpdateLanderState.
    ADD S TO ElapsedTime
    SUBTRACT S FROM T
    COMPUTE Weight ROUNDED = Weight - (S * FuelRate)
    MOVE I TO Altitude
    MOVE J TO Velocity
    EXIT.

*> (07.10 in original FOCAL code)
UpdateUntilContact.
    PERFORM UNTIL S < 0.005
        COMPUTE S ROUNDED =
            2 * Altitude
            / (Velocity
                + FUNCTION SQRT(
                    Velocity**2 + 2 * Altitude
                    * (Gravity - Z * FuelRate / Weight)))
        PERFORM ApplyThrust
        PERFORM UpdateLanderState
    END-PERFORM
    PERFORM Contact
    EXIT.

*> (08.10 in original FOCAL code)
ApplyThrustLoop.
    PERFORM WITH TEST AFTER UNTIL (I <= 0) OR (-J < 0) OR (Velocity <= 0)
        COMPUTE W ROUNDED = (1 - Weight * Gravity / (Z * FuelRate)) / 2
        COMPUTE S ROUNDED =
            Weight * Velocity
            / (Z * FuelRate
                * (W + FUNCTION SQRT(W**2 + Velocity / Z)))
            + 0.5
        PERFORM ApplyThrust
        IF I <= 0 THEN
            PERFORM UpdateUntilContact
        ELSE
            PERFORM UpdateLanderState
        END-IF
    END-PERFORM
    EXIT.

*> (09.10 in original FOCAL code)
ApplyThrust.
    COMPUTE Q ROUNDED = S * FuelRate / Weight
    COMPUTE Q2 ROUNDED = Q ** 2
    COMPUTE Q3 ROUNDED = Q ** 3
    COMPUTE Q4 ROUNDED = Q ** 4
    COMPUTE Q5 ROUNDED = Q ** 5
    COMPUTE J ROUNDED =
        Velocity
        + Gravity * S
        + Z * (-Q - Q2/2 - Q3/3 - Q4/4 - Q5/5)
    COMPUTE I ROUNDED =
        Altitude
        - Gravity * S * S / 2
        - Velocity * S
        + Z * S * (Q/2 + Q2/6 + Q3/12 + Q4/20 + Q5/30)
    EXIT.

