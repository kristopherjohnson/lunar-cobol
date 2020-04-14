*> Translation of
*> <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
*> by Jim Storer from FOCAL to COBOL.

IDENTIFICATION DIVISION.
PROGRAM-ID. Lunar-Lander.

DATA DIVISION.
WORKING-STORAGE SECTION.

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
*> S - Time elapsed in current 10-second turn (sec)
*> T - Time remaining in current 10-second turn (sec)
*> V - Downward speed (miles/sec)
*> W - Temporary working variable
*> Z - Thrust per pound of fuel burned

01 Altitude                   PIC S9(8)V9(4)  USAGE COMP.
01 Gravity                    PIC S9(8)V9(4)  USAGE COMP.
01 I                          PIC S9(8)V9(4)  USAGE COMP.
01 J                          PIC S9(8)V9(4)  USAGE COMP.
01 Fuel-Rate                  PIC S9(8)V9(4)  USAGE COMP.
01 Elapsed                    PIC S9(8)V9(4)  USAGE COMP.
01 Weight                     PIC S9(8)V9(4)  USAGE COMP.
01 Empty-Weight               PIC S9(8)V9(4)  USAGE COMP.
01 S                          PIC S9(8)V9(4)  USAGE COMP.
01 T                          PIC S9(8)V9(4)  USAGE COMP.
01 Velocity                   PIC S9(8)V9(4)  USAGE COMP.
01 W                          PIC S9(8)V9(4)  USAGE COMP.
01 Impulse                    PIC S9(8)V9(4)  USAGE COMP.

*> Output Formatting

01 Status-Row-Headings.
    02 FILLER                 PIC X(12)  VALUE "TIME,SECS".
    02 FILLER                 PIC X(22)  VALUE "ALTITUDE,MILES+FEET".
    02 FILLER                 PIC X(15)  VALUE "VELOCITY,MPH".
    02 FILLER                 PIC X(11)  VALUE "FUEL,LBS".
    02 FILLER                 PIC X(9)   VALUE "FUEL RATE".

01 Status-Row-Data.
    02 Elapsed-Display        PIC Z(6)9.
    02 Altitude-Miles-Display PIC Z(15)9.
    02 Altitude-Feet-Display  PIC Z(6)9.
    02 Velocity-MPH-Display   PIC Z(11)9.99.
    02 Fuel-Remaining-Display PIC Z(9)9.9.
    02 FILLER                 PIC X(6)  VALUE SPACES.

01 Not-Possible-Message.
    02 FILLER                 PIC X(12)  VALUE "NOT POSSIBLE".
    02 FILLER                 PIC X(51)  VALUE ALL '.'.

*> ACCEPT Destinations

01 Fuel-Rate-Answer       PIC 999.
    88 Is-Valid-Fuel-Rate VALUE 0, 8 THRU 200.

01 Try-Again-Answer       PIC X(3).
    88 Try-Again          VALUE "y", "Y", "yes", "YES".
    88 Dont-Try-Again     VALUE "n", "N", "no", "NO".

PROCEDURE DIVISION.
Begin.
    DISPLAY "CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY"
    DISPLAY "YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE"
    DISPLAY "BETWEEN 8 & 200 LBS/SEC. YOU'VE 16000 LBS FUEL. ESTIMATED"
    DISPLAY "FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS"
    DISPLAY " "
    DISPLAY " "

    PERFORM WITH TEST AFTER UNTIL Dont-Try-Again

        PERFORM Attempt-Landing

        DISPLAY " "
        DISPLAY " "
        DISPLAY "TRY AGAIN?"
        PERFORM WITH TEST AFTER UNTIL Try-Again OR Dont-Try-Again
            DISPLAY "(ANS. YES OR NO):" NO ADVANCING
            ACCEPT Try-Again-Answer
        END-PERFORM
    END-PERFORM

    DISPLAY "CONTROL OUT"
    DISPLAY " "
    DISPLAY " "
    STOP RUN.

Attempt-Landing.
    DISPLAY "FIRST RADAR CHECK COMING UP"
    DISPLAY " "
    DISPLAY " "
    DISPLAY "COMMENCE LANDING PROCEDURE"
    DISPLAY Status-Row-Headings

    MOVE 120   TO Altitude
    MOVE 1     TO Velocity
    MOVE 32500 TO Weight
    MOVE 16500 TO Empty-Weight
    MOVE 0.001 TO Gravity
    MOVE 1.8   TO Impulse
    MOVE 0     TO Elapsed

    PERFORM Get-Fuel-Rate

    EXIT.

*> 02.10 in original FOCAL code
Get-Fuel-Rate.
    MOVE Elapsed to Elapsed-Display
    MOVE 0 to Altitude-Miles-Display
    MOVE 0 to Altitude-Feet-Display
    COMPUTE Velocity-MPH-Display = Velocity * 3600
    COMPUTE Fuel-Remaining-Display = Weight - Empty-Weight

    DISPLAY Status-Row-Data NO ADVANCING

    PERFORM WITH TEST AFTER UNTIL Is-Valid-Fuel-Rate
        DISPLAY "K=:" NO ADVANCING
        ACCEPT Fuel-Rate-Answer
        IF Is-Valid-Fuel-Rate THEN
            MOVE Fuel-Rate-Answer TO Fuel-Rate
        ELSE
            DISPLAY Not-Possible-Message NO ADVANCING
        END-IF
    END-PERFORM

    EXIT.
