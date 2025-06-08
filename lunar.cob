*> Translation of
*> <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
*> by Jim Storer from FOCAL to COBOL.

IDENTIFICATION DIVISION.
PROGRAM-ID. LunarLander.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> Physical and conversion constants
77 C-FEET-PER-MILE              PIC 9(4) VALUE 5280 COMP.
77 C-SECONDS-PER-HOUR           PIC 9(4) VALUE 3600 COMP.
77 C-BLANK-LINE                 PIC X VALUE SPACE.

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

01 WS-ALTITUDE                PIC S9(6)V9(10)  USAGE COMP.
01 WS-GRAVITY                 PIC S9(6)V9(10)  USAGE COMP.
01 WS-I                       PIC S9(6)V9(10)  USAGE COMP.
01 WS-J                       PIC S9(6)V9(10)  USAGE COMP.
01 WS-FUEL-RATE               PIC S9(6)V9(10)  USAGE COMP.
01 WS-ELAPSED-TIME            PIC S9(6)V9(10)  USAGE COMP.
01 WS-WEIGHT                  PIC S9(6)V9(10)  USAGE COMP.
01 WS-EMPTY-WEIGHT            PIC S9(6)V9(10)  USAGE COMP.
01 WS-Q                       PIC S9(6)V9(10)  USAGE COMP.
01 WS-S                       PIC S9(6)V9(10)  USAGE COMP.
01 WS-T                       PIC S9(6)V9(10)  USAGE COMP.
01 WS-VELOCITY                PIC S9(6)V9(10)  USAGE COMP.
01 WS-W                       PIC S9(6)V9(10)  USAGE COMP.
01 WS-Z                       PIC S9(6)V9(10)  USAGE COMP.

*> Variables used by Simulate and related paragraphs.

01 WS-GAME-OVER-FLAG          PIC 9.
    88 GAME-IS-NOT-OVER       VALUE 0.
    88 GAME-IS-OVER           VALUE 1.

01 WS-Q2                      PIC S9(6)V9(10)  USAGE COMP.
01 WS-Q3                      PIC S9(6)V9(10)  USAGE COMP.
01 WS-Q4                      PIC S9(6)V9(10)  USAGE COMP.
01 WS-Q5                      PIC S9(6)V9(10)  USAGE COMP.

*> Display formatting structures
01 WS-STATUS-DISPLAY.
   05 WS-STATUS-HEADINGS.
      10 FILLER                 PIC X(12) VALUE "TIME,SECS".
      10 FILLER                 PIC X(22) VALUE "ALTITUDE,MILES+FEET".
      10 FILLER                 PIC X(15) VALUE "VELOCITY,MPH".
      10 FILLER                 PIC X(11) VALUE "FUEL,LBS".
      10 FILLER                 PIC X(9) VALUE "FUEL RATE".
   05 WS-STATUS-DATA.
      10 WS-TIME-DISPLAY        PIC -(6)9.
      10 WS-ALT-MILES-DISPLAY   PIC -(15)9.
      10 WS-ALT-FEET-DISPLAY    PIC -(6)9.
      10 WS-VELOCITY-DISPLAY    PIC -(11)9.99.
      10 WS-FUEL-DISPLAY        PIC -(9)9.9.
      10 FILLER                 PIC X(6) VALUE SPACES.

01 WS-NOT-POSSIBLE-MSG.
   05 FILLER                    PIC X(12) VALUE "NOT POSSIBLE".
   05 FILLER                    PIC X(51) VALUE ALL '.'.

01 WS-FUEL-OUT-TIME            PIC -(4)9.99.
01 WS-CONTACT-TIME             PIC -(4)9.99.
01 WS-IMPACT-VELOCITY          PIC -(4)9.99.
01 WS-FUEL-LEFT                PIC -(4)9.99.
01 WS-CRATER-DEPTH             PIC -(4)9.99.

*> User Input

01 WS-FUEL-RATE-ANSWER        PIC 999.
    88 IS-VALID-FUEL-RATE     VALUE 0, 8 THRU 200.

01 WS-TRY-AGAIN-ANSWER        PIC X.
    88 TRY-AGAIN              VALUE "Y", "y".
    88 DONT-TRY-AGAIN         VALUE "N", "n".

PROCEDURE DIVISION.

*> (01.04 in original FOCAL code)
0000-MAIN-LOGIC.
    DISPLAY "CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY"
    DISPLAY "YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE"
    DISPLAY "BETWEEN 8 & 200 LBS/SEC. YOU'VE 16000 LBS FUEL. ESTIMATED"
    DISPLAY "FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS"
    DISPLAY C-BLANK-LINE
    DISPLAY C-BLANK-LINE

    PERFORM WITH TEST AFTER UNTIL DONT-TRY-AGAIN
        PERFORM 1000-PLAY-GAME

        DISPLAY C-BLANK-LINE
        DISPLAY C-BLANK-LINE
        DISPLAY C-BLANK-LINE
        DISPLAY "TRY AGAIN?"

        PERFORM WITH TEST AFTER UNTIL TRY-AGAIN OR DONT-TRY-AGAIN
            DISPLAY "(ANS. YES OR NO):" NO ADVANCING
            ACCEPT WS-TRY-AGAIN-ANSWER
            *>TEST:DISPLAY WS-TRY-AGAIN-ANSWER
        END-PERFORM
    END-PERFORM

    DISPLAY "CONTROL OUT"
    DISPLAY C-BLANK-LINE
    DISPLAY C-BLANK-LINE

    STOP RUN.

*> Play game until landing or crash.
1000-PLAY-GAME.
    DISPLAY "FIRST RADAR CHECK COMING UP"
    DISPLAY C-BLANK-LINE
    DISPLAY C-BLANK-LINE
    DISPLAY "COMMENCE LANDING PROCEDURE"
    DISPLAY WS-STATUS-HEADINGS

    MOVE 120   TO WS-ALTITUDE
    MOVE 1     TO WS-VELOCITY
    MOVE 32500 TO WS-WEIGHT
    MOVE 16500 TO WS-EMPTY-WEIGHT
    MOVE 0.001 TO WS-GRAVITY
    MOVE 1.8   TO WS-Z
    INITIALIZE WS-ELAPSED-TIME

    SET GAME-IS-NOT-OVER TO TRUE

    PERFORM UNTIL GAME-IS-OVER
        PERFORM 2000-GET-FUEL-RATE
        MOVE 10 TO WS-T
        PERFORM 3000-SIMULATE
    END-PERFORM

    EXIT.

*> Display current status and prompt for new Fuel-Rate value until
*> valid answer is given.
2000-GET-FUEL-RATE.
    COMPUTE
        WS-TIME-DISPLAY ROUNDED = WS-ELAPSED-TIME
    END-COMPUTE

    MOVE FUNCTION INTEGER-PART(WS-ALTITUDE) TO WS-ALT-MILES-DISPLAY

    COMPUTE
        WS-ALT-FEET-DISPLAY ROUNDED =
            (WS-ALTITUDE - FUNCTION INTEGER-PART(WS-ALTITUDE)) * C-FEET-PER-MILE
    END-COMPUTE

    MULTIPLY
        WS-VELOCITY BY C-SECONDS-PER-HOUR
        GIVING WS-VELOCITY-DISPLAY ROUNDED
    END-MULTIPLY

    SUBTRACT
        WS-EMPTY-WEIGHT FROM WS-WEIGHT
        GIVING WS-FUEL-DISPLAY ROUNDED
    END-SUBTRACT

    DISPLAY WS-STATUS-DATA NO ADVANCING

    PERFORM WITH TEST AFTER UNTIL IS-VALID-FUEL-RATE
        DISPLAY "K=:" NO ADVANCING
        ACCEPT WS-FUEL-RATE-ANSWER
        *>TEST:DISPLAY WS-FUEL-RATE-ANSWER
        IF IS-VALID-FUEL-RATE THEN
            MOVE WS-FUEL-RATE-ANSWER TO WS-FUEL-RATE
        ELSE
            DISPLAY WS-NOT-POSSIBLE-MSG NO ADVANCING
        END-IF
    END-PERFORM

    EXIT.

*> Simulate T seconds using current fuel rate.
*> If out of fuel, continue until contact with surface.
*> On contact with surface, determine outcome and display score.
*> (03.10 in original FOCAL code)
3000-SIMULATE.
    PERFORM UNTIL GAME-IS-OVER OR WS-T < 0.001
        IF (WS-WEIGHT - WS-EMPTY-WEIGHT) < 0.001 THEN
            PERFORM 4000-FUEL-OUT
        ELSE
            MOVE WS-T TO WS-S
            IF (WS-S * WS-FUEL-RATE) > (WS-WEIGHT - WS-EMPTY-WEIGHT) THEN
                COMPUTE
                    WS-S ROUNDED = (WS-WEIGHT - WS-EMPTY-WEIGHT) / WS-FUEL-RATE
                END-COMPUTE
            END-IF
            PERFORM 6000-APPLY-THRUST
            IF WS-I <= 0 THEN
                PERFORM 7000-UPDATE-UNTIL-CONTACT
            ELSE
                IF WS-VELOCITY > 0 AND WS-J < 0 THEN
                    PERFORM 8000-APPLY-THRUST-LOOP
                ELSE
                    PERFORM 5000-UPDATE-LANDER-STATE
                END-IF
            END-IF
        END-IF
    END-PERFORM
    EXIT.

*> (04.10 in original FOCAL code)
4000-FUEL-OUT.
    COMPUTE
        WS-FUEL-OUT-TIME ROUNDED = WS-ELAPSED-TIME
    END-COMPUTE
    DISPLAY "FUEL OUT AT " WS-FUEL-OUT-TIME " SECS"

    COMPUTE
        WS-S ROUNDED =
            (FUNCTION SQRT(WS-VELOCITY**2 + 2 * WS-ALTITUDE * WS-GRAVITY)
                - WS-VELOCITY)
            / WS-GRAVITY
    END-COMPUTE

    COMPUTE
        WS-VELOCITY ROUNDED = WS-VELOCITY + WS-GRAVITY * WS-S
    END-COMPUTE

    ADD
        WS-S to WS-ELAPSED-TIME ROUNDED
    END-ADD

    PERFORM 4100-CONTACT
    EXIT.

*> Handle touchdown/crash
*> (05.10 in original FOCAL code)
4100-CONTACT.
    COMPUTE
        WS-CONTACT-TIME ROUNDED = WS-ELAPSED-TIME
    END-COMPUTE
    DISPLAY "ON THE MOON AT " WS-CONTACT-TIME " SECS"

    *> W is velocity in miles-per-hour
    MULTIPLY
        C-SECONDS-PER-HOUR BY WS-VELOCITY
        GIVING WS-W ROUNDED
    END-MULTIPLY

    COMPUTE
        WS-IMPACT-VELOCITY ROUNDED = WS-W
    END-COMPUTE
    DISPLAY "IMPACT VELOCITY OF " WS-IMPACT-VELOCITY " M.P.H."

    SUBTRACT
        WS-EMPTY-WEIGHT FROM WS-WEIGHT
        GIVING WS-FUEL-LEFT ROUNDED
    END-SUBTRACT
    DISPLAY "FUEL LEFT: " WS-FUEL-LEFT " LBS"

    EVALUATE WS-W
        WHEN <=  1 DISPLAY "PERFECT LANDING !-(LUCKY)"
        WHEN <= 10 DISPLAY "GOOD LANDING-(COULD BE BETTER)"
        WHEN <= 22 DISPLAY "CONGRATULATIONS ON A POOR LANDING"
        WHEN <= 40 DISPLAY "CRAFT DAMAGE. GOOD LUCK"
        WHEN <= 60 DISPLAY "CRASH LANDING-YOU'VE 5 HRS OXYGEN"
        WHEN OTHER
            PERFORM
                DISPLAY "SORRY,BUT THERE WERE NO SURVIVORS-YOU BLEW IT!"

                COMPUTE
                    WS-CRATER-DEPTH ROUNDED = WS-W * 0.277777
                END-COMPUTE
                DISPLAY
                    "IN FACT YOU BLASTED A NEW LUNAR CRATER "
                    WS-CRATER-DEPTH " FT. DEEP"
            END-PERFORM
    END-EVALUATE

    SET GAME-IS-OVER TO TRUE
    EXIT.

*> (06.10 in original FOCAL code)
5000-UPDATE-LANDER-STATE.
    ADD
        WS-S TO WS-ELAPSED-TIME
    END-ADD

    SUBTRACT
        WS-S FROM WS-T
    END-SUBTRACT

    COMPUTE
        WS-WEIGHT ROUNDED = WS-WEIGHT - (WS-S * WS-FUEL-RATE)
    END-COMPUTE

    MOVE WS-I TO WS-ALTITUDE
    MOVE WS-J TO WS-VELOCITY
    EXIT.

*> (07.10 in original FOCAL code)
7000-UPDATE-UNTIL-CONTACT.
    PERFORM UNTIL WS-S < 0.005
        COMPUTE
            WS-S ROUNDED =
                (2 * WS-ALTITUDE)
                / (WS-VELOCITY
                    + FUNCTION SQRT(
                        WS-VELOCITY**2 + 2 * WS-ALTITUDE
                        * (WS-GRAVITY - WS-Z * WS-FUEL-RATE / WS-WEIGHT)))
        END-COMPUTE
        PERFORM 6000-APPLY-THRUST
        PERFORM 5000-UPDATE-LANDER-STATE
    END-PERFORM
    PERFORM 4100-CONTACT
    EXIT.

*> (08.10 in original FOCAL code)
8000-APPLY-THRUST-LOOP.
    PERFORM WITH TEST AFTER UNTIL (WS-I <= 0) OR (-WS-J < 0) OR (WS-VELOCITY <= 0)
        COMPUTE
            WS-W ROUNDED = (1 - WS-WEIGHT * WS-GRAVITY / (WS-Z * WS-FUEL-RATE)) / 2
        END-COMPUTE

        COMPUTE
            WS-S ROUNDED =
                WS-WEIGHT * WS-VELOCITY
                / (WS-Z * WS-FUEL-RATE
                    * (WS-W + FUNCTION SQRT(WS-W**2 + WS-VELOCITY / WS-Z)))
                + 0.05
        END-COMPUTE

        PERFORM 6000-APPLY-THRUST

        IF WS-I <= 0 THEN
            PERFORM 7000-UPDATE-UNTIL-CONTACT
        ELSE
            PERFORM 5000-UPDATE-LANDER-STATE
        END-IF
    END-PERFORM
    EXIT.

*> (09.10 in original FOCAL code)
6000-APPLY-THRUST.
    COMPUTE
        WS-Q ROUNDED = WS-S * WS-FUEL-RATE / WS-WEIGHT
    END-COMPUTE

    COMPUTE WS-Q2 ROUNDED = WS-Q ** 2 END-COMPUTE
    COMPUTE WS-Q3 ROUNDED = WS-Q ** 3 END-COMPUTE
    COMPUTE WS-Q4 ROUNDED = WS-Q ** 4 END-COMPUTE
    COMPUTE WS-Q5 ROUNDED = WS-Q ** 5 END-COMPUTE

    COMPUTE
        WS-J ROUNDED =
            WS-VELOCITY
            + WS-GRAVITY * WS-S
            + WS-Z * (-WS-Q - WS-Q2/2 - WS-Q3/3 - WS-Q4/4 - WS-Q5/5)
    END-COMPUTE

    COMPUTE
        WS-I ROUNDED =
            WS-ALTITUDE
            - WS-GRAVITY * WS-S * WS-S / 2
            - WS-VELOCITY * WS-S
            + WS-Z * WS-S * (WS-Q/2 + WS-Q2/6 + WS-Q3/12 + WS-Q4/20 + WS-Q5/30)
    END-COMPUTE
    EXIT.
