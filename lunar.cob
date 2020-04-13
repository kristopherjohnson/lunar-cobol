*> Translation of
*> <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
*> by Jim Storer from FOCAL to COBOL.

IDENTIFICATION DIVISION.
PROGRAM-ID. LUNAR.

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

01 Altitude               PIC 9(8)V9(4) USAGE COMP.
01 Gravity                PIC 9(8)V9(4) USAGE COMP.
01 I                      PIC 9(8)V9(4) USAGE COMP.
01 J                      PIC 9(8)V9(4) USAGE COMP.
01 FuelRate               PIC 9(8)V9(4) USAGE COMP.
01 ElapsedSec             PIC 9(8)V9(4) USAGE COMP.
01 Weight                 PIC 9(8)V9(4) USAGE COMP.
01 EmptyWeight            PIC 9(8)V9(4) USAGE COMP.
01 S                      PIC 9(8)V9(4) USAGE COMP.
01 T                      PIC 9(8)V9(4) USAGE COMP.
01 Velocity               PIC 9(8)V9(4) USAGE COMP.
01 W                      PIC 9(8)V9(4) USAGE COMP.
01 Impulse                PIC 9(8)V9(4) USAGE COMP.

01 FuelRateAnswer         PIC 999.
    88 IsValidFuelRate    VALUE 0, 8 THRU 100.

01 TryAgainAnswer         PIC X(3).
    88 TryAgain           VALUE "y", "Y", "yes", "YES".
    88 DontTryAgain       VALUE "n", "N", "no", "NO".

PROCEDURE DIVISION.
Begin.
    DISPLAY "CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY"
    DISPLAY "YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE"
    DISPLAY "BETWEEN 8 & 200 LBS/SEC. YOU'VE 16000 LBS FUEL. ESTIMATED"
    DISPLAY "FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS"
    DISPLAY " "
    DISPLAY " "

    PERFORM WITH TEST AFTER UNTIL DontTryAgain

        DISPLAY "(TODO: Attempt landing)"

        DISPLAY " "
        DISPLAY " "
        DISPLAY "TRY AGAIN?"
        PERFORM WITH TEST AFTER UNTIL TryAgain OR DontTryAgain
            DISPLAY "(ANS. YES OR NO):" NO ADVANCING
            ACCEPT TryAgainAnswer
        END-PERFORM
    END-PERFORM

    DISPLAY "CONTROL OUT"
    DISPLAY " "
    DISPLAY " "
    STOP RUN.
