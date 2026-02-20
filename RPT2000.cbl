       IDENTIFICATION DIVISION.                                         00010001
                                                                        00020001
       PROGRAM-ID. RPT2000.                                             00030006
                                                                        00040001
      *   Programmers.: Violet French                                   00050001
      *   Date........: 2026.02.19                                      00060001
      *   Github URL..: https://github.com/Pirategirl9000/RPT2000       00070001
      *   Description.: This program produces a sales report based on   00090001
      *   values acquired from the CUSTMAST dataset                     00100001
       ENVIRONMENT DIVISION.                                            00110001
                                                                        00120001
       INPUT-OUTPUT SECTION.                                            00130001
                                                                        00140001
       FILE-CONTROL.                                                    00150001
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00160001
           SELECT ORPT2000 ASSIGN TO RPT2000.                           00170001
                                                                        00180001
       DATA DIVISION.                                                   00190001
                                                                        00200001
       FILE SECTION.                                                    00210001
                                                                        00220001
      **************************************************************    00221010
      * INPUT FILE                                                 *    00222010
      **************************************************************    00224010
       FD  CUSTMAST                                                     00230001
           RECORDING MODE IS F                                          00240001
           LABEL RECORDS ARE STANDARD                                   00250001
           RECORD CONTAINS 130 CHARACTERS                               00260001
           BLOCK CONTAINS 130 CHARACTERS.                               00270001
       01  CUSTOMER-MASTER-RECORD.                                      00280001
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00290001
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00300001
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00310001
           05  CM-CUSTOMER-NAME        PIC X(20).                       00320001
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00330001
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00340001
           05  FILLER                  PIC X(87).                       00350001
                                                                        00360001
      **************************************************************    00361010
      * OUTPUT FILE                                                *    00362010
      **************************************************************    00363010
       FD  ORPT2000                                                     00370001
           RECORDING MODE IS F                                          00380001
           LABEL RECORDS ARE STANDARD                                   00390001
           RECORD CONTAINS 130 CHARACTERS                               00400001
           BLOCK CONTAINS 130 CHARACTERS.                               00410001
       01  PRINT-AREA      PIC X(130).                                  00420001
                                                                        00430001
       WORKING-STORAGE SECTION.                                         00440001
                                                                        00450001
      *------------------------------------------------------------*    00450129
      *                        WORKING FIELDS                      *    00450229
      *============================================================*    00450329
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00450429
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00450529
      *------------------------------------------------------------*    00450629
                                                                        00450729
      **************************************************************    00451010
      * SWITCH FOR END OF FILE                                     *    00452010
      **************************************************************    00453010
       01  SWITCHES.                                                    00460001
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00470001
                                                                        00480001
      **************************************************************    00481010
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00482010
      **************************************************************    00483010
       01  PRINT-FIELDS.                                                00490001
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00500001
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00510001
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00520001
                                                                        00540001
      **************************************************************    00541010
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00542010
      **************************************************************    00543010
       01  TOTAL-FIELDS.                                                00550001
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00560001
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00570001
                                                                        00580001
      **************************************************************    00581010
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    00582019
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    00582119
      **************************************************************    00583010
       01  CURRENT-DATE-AND-TIME.                                       00590001
           05  CD-YEAR         PIC 9999.                                00600001
           05  CD-MONTH        PIC 99.                                  00610001
           05  CD-DAY          PIC 99.                                  00620001
           05  CD-HOURS        PIC 99.                                  00630001
           05  CD-MINUTES      PIC 99.                                  00640001
           05  FILLER          PIC X(9).                                00650001
                                                                        00660001
      **************************************************************    00660114
      * STORES FIELDS WITH VALUES CALCULATED PER CUSTOMER         *     00660229
      **************************************************************    00660314
       01  CALCULATED-FIELDS.                                           00660414
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            00660514
                                                                        00660614
      *------------------------------------------------------------*    00660729
      *                       OUTPUT FIELDS                        *    00660829
      *============================================================*    00660929
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    00661029
      *                      THE OUTPUT FILE                       *    00661129
      *------------------------------------------------------------*    00661229
                                                                        00661329
      **************************************************************    00661410
      * STORES THE FIRST HEADER LINE INFORMATION                   *    00662029
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    00662119
      **************************************************************    00663010
       01  HEADING-LINE-1.                                              00670001
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             00680001
           05  HL1-MONTH       PIC 9(2).                                00690001
           05  FILLER          PIC X(1)    VALUE "/".                   00700001
           05  HL1-DAY         PIC 9(2).                                00710001
           05  FILLER          PIC X(1)    VALUE "/".                   00720001
           05  HL1-YEAR        PIC 9(4).                                00730001
           05  FILLER          PIC X(11)   VALUE SPACE.                 00740001
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".00750001
           05  FILLER          PIC X(20)   VALUE "EPORT               ".00760017
           05  FILLER          PIC X(10)   VALUE SPACE.                 00761023
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            00770001
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                00780017
           05  FILLER          PIC X(39)   VALUE SPACE.                 00790023
                                                                        00800001
      **************************************************************    00801010
      * STORES THE SECOND HEADER LINE INFORMATION                  *    00802029
      * HOLDS THE TIME AND THE PROGRAM ID                          *    00802129
      **************************************************************    00803010
       01  HEADING-LINE-2.                                              00810001
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             00820001
           05  HL2-HOURS       PIC 9(2).                                00830001
           05  FILLER          PIC X(1)    VALUE ":".                   00840001
           05  HL2-MINUTES     PIC 9(2).                                00850001
           05  FILLER          PIC X(68)   VALUE SPACE.                 00860023
           05  FILLER          PIC X(10)   VALUE "RPT2000".             00870017
           05  FILLER          PIC X(39)   VALUE SPACE.                 00880023
                                                                        00890001
      **************************************************************    00890117
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    00890229
      **************************************************************    00890317
       01  HEADING-LINE-3.                                              00890417
           05 FILLER               PIC X(130)   VALUE SPACE.            00890517
                                                                        00890617
      **************************************************************    00891010
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    00892029
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    00892119
      * THE NEXT HEADER LINE                                       *    00892219
      **************************************************************    00893010
       01  HEADING-LINE-4.                                              00900017
           05  FILLER      PIC X(7)   VALUE "BRANCH ".                  00901022
           05  FILLER      PIC X(6)   VALUE "SALES ".                   00902022
           05  FILLER      PIC X(20)   VALUE "CUST                ".    00910023
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    00920001
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    00930001
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    00931013
           05  FILLER      PIC X(37)   VALUE SPACE.                     00940022
                                                                        00950001
      **************************************************************    00951010
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    00952029
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    00952119
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    00952219
      **************************************************************    00953010
       01  HEADING-LINE-5.                                              00960017
           05  FILLER      PIC X(8)    VALUE " NUM    ".                00961022
           05  FILLER      PIC X(5)    VALUE "REP  ".                   00962022
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    00970023
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    00980001
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    00990001
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    00991014
           05  FILLER      PIC X(37)   VALUE SPACE.                     01000022
                                                                        01010001
      **************************************************************    01010120
      * STORES THE SIXTH HEADER LINE INFORMATION                   *    01010229
      * DISPLAYS COLUMN DIVIDERS FOR THE REPORT                    *    01010320
      **************************************************************    01010520
       01  HEADING-LINE-6.                                              01010620
           05  FILLER      PIC X(6)   VALUE ALL '-'. *> BRANCH NUM      01010720
           05  FILLER      PIC X      VALUE SPACE.                      01010820
           05  FILLER      PIC X(5)   VALUE ALL '-'. *> SALES REP       01010920
           05  FILLER      PIC X      VALUE SPACE.                      01011020
           05  FILLER      PIC X(5)   VALUE ALL '-'. *> CUST NUM        01011120
           05  FILLER      PIC X(2)   VALUE SPACE.                      01011223
           05  FILLER      PIC X(20)  VALUE ALL '-'. *> CUST NAME       01011320
           05  FILLER      PIC X(3)   VALUE SPACE.                      01011420
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> SALES THIS      01011520
           05  FILLER      PIC X(4)   VALUE SPACE.                      01011623
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> SALES LAST      01011720
           05  FILLER      PIC X(4)   VALUE SPACE.                      01011823
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> CHANGE AMNT     01011920
           05  FILLER      PIC X(3)   VALUE SPACE.                      01012020
           05  FILLER      PIC X(6)   VALUE ALL '-'. *> CHANGE PERC     01012120
           05  FILLER      PIC X(40)  VALUE SPACE.                      01012323
                                                                        01012420
      **************************************************************    01012510
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01012629
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    01012721
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    01012821
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    01012921
      * DIFFERENCE IN PERCENT.                                     *    01013021
      **************************************************************    01014010
       01  CUSTOMER-LINE.                                               01020001
           05  FILLER              PIC X(2)     VALUE SPACE.            01020121
           05  CL-BRANCH-NUMBER    PIC X(2).                            01021021
           05  FILLER              PIC X(4)     VALUE SPACE.            01021121
           05  CL-SALESREP-NUMBER  PIC X(2).                            01022021
           05  FILLER              PIC X(3)     VALUE SPACE.            01023021
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            01030001
           05  FILLER              PIC X(2)     VALUE SPACE.            01040001
           05  CL-CUSTOMER-NAME    PIC X(20).                           01050001
           05  FILLER              PIC X(3)     VALUE SPACE.            01060001
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      01070001
           05  FILLER              PIC X(4)     VALUE SPACE.            01080001
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      01090001
           05  FILLER              PIC X(4)     VALUE SPACE.            01091014
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      01092014
           05  FILLER              PIC X(3)     VALUE SPACE.            01093014
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          01094014
           05  FILLER              PIC X(40)    VALUE SPACE.            01100021
                                                                        01110001
      **************************************************************    01110124
      * STORES THE FIRST GRAND TOTAL LINE                          *    01110229
      * DISPLAYS COLUMN DIVIDERS FOR THE GRAND TOTALS              *    01110324
      **************************************************************    01110424
       01  GRAND-TOTAL-LINE1.                                           01110724
           05  FILLER              PIC X(40)    VALUE SPACE.            01110824
           05  FILLER              PIC X(13)    VALUE ALL '='.          01110924
           05  FILLER              PIC X        VALUE SPACE.            01111024
           05  FILLER              PIC X(13)    VALUE ALL '='.          01111124
           05  FILLER              PIC X        VALUE SPACE.            01111224
           05  FILLER              PIC X(13)    VALUE ALL '='.          01111324
           05  FILLER              PIC X(3)     VALUE SPACES.           01111424
           05  FILLER              PIC X(6)     VALUE ALL '='.          01111524
           05  FILLER              PIC X(40)    VALUE SPACES.           01111624
      **************************************************************    01111710
      * STORES THE SECOND GRAND TOTAL LINE                         *    01112029
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    01112119
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    01112219
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    01112319
      **************************************************************    01113010
       01  GRAND-TOTAL-LINE2.                                           01120024
           05  FILLER              PIC X(40)    VALUE SPACE.            01130023
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   01140001
           05  FILLER              PIC X(1)     VALUE SPACE.            01150001
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   01160001
           05  FILLER              PIC X        VALUE SPACE.            01161014
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   01162014
           05  FILLER              PIC X(3)     VALUE SPACE.            01162114
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          01163014
           05  FILLER              PIC X(40)    VALUE SPACE.            01170023
                                                                        01180001
       PROCEDURE DIVISION.                                              01190001
                                                                        01200001
      **************************************************************    01201011
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    01202011
      * READING AND WRITING TO AND FROM THEM                       *    01202111
      **************************************************************    01203011
       000-PREPARE-SALES-REPORT.                                        01210001
                                                                        01220001
           OPEN INPUT  CUSTMAST                                         01230001
                OUTPUT ORPT2000.                                        01240001
                                                                        01241019
           *> GRABS THE DATE AND TIME INFORMATION FOR                   01242019
           *> THE HEADER LINES                                          01243019
           PERFORM 100-FORMAT-REPORT-HEADING.                           01250001
                                                                        01251019
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     01252019
           *> THE END OF THE INPUT FILE                                 01253019
           PERFORM 200-PREPARE-SALES-LINES                              01260001
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         01270001
                                                                        01271019
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                01272019
           PERFORM 300-PRINT-GRAND-TOTALS.                              01280001
                                                                        01281019
           CLOSE CUSTMAST                                               01290001
                 ORPT2000.                                              01300001
           STOP RUN.                                                    01310001
                                                                        01320001
      **************************************************************    01321011
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    01322011
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    01323011
      **************************************************************    01324011
       100-FORMAT-REPORT-HEADING.                                       01330001
                                                                        01340001
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         01350001
                                                                        01351019
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          01352019
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         01353019
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           01354019
           MOVE CD-MONTH   TO HL1-MONTH.                                01360001
           MOVE CD-DAY     TO HL1-DAY.                                  01370001
           MOVE CD-YEAR    TO HL1-YEAR.                                 01380001
           MOVE CD-HOURS   TO HL2-HOURS.                                01390001
           MOVE CD-MINUTES TO HL2-MINUTES.                              01400001
                                                                        01410001
      **************************************************************    01411011
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    01412011
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    01413011
      * TERMINATING LINE OF THE FILE                               *    01413111
      **************************************************************    01414011
       200-PREPARE-SALES-LINES.                                         01420001
                                                                        01430001
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               01431019
           PERFORM 210-READ-CUSTOMER-RECORD.                            01440001
                                                                        01441019
           *> IF THE LINE WE READ WASN'T BLANK THEN                     01442026
           *> WE WILL OUTPUT THAT CUSTOMER'S SALES TO THE OUTPUT        01444019
           *> NOTE: WE DON'T OUTPUT THE LAST LINE BECAUSE IT'S BLANK    01445019
           IF CUSTMAST-EOF-SWITCH = "N"                                 01450001
                   PERFORM 220-PRINT-CUSTOMER-LINE.                     01460018
                                                                        01470001
      **************************************************************    01471011
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    01472011
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    01473011
      **************************************************************    01474011
       210-READ-CUSTOMER-RECORD.                                        01480001
                                                                        01490001
           READ CUSTMAST                                                01500001
               AT END                                                   01510001
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     01520001
                                                                        01530001
      **************************************************************    01531011
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    01532011
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    01533011
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    01533111
      **************************************************************    01534011
       220-PRINT-CUSTOMER-LINE.                                         01540001
                                                                        01550001
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     01551019
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   01552019
           IF LINE-COUNT >= LINES-ON-PAGE                               01560001
               PERFORM 230-PRINT-HEADING-LINES.                         01570001
                                                                        01571018
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         01572018
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     01573018
           MOVE CM-BRANCH-NUMBER    TO CL-BRANCH-NUMBER.                01574021
           MOVE CM-SALESREP-NUMBER  TO CL-SALESREP-NUMBER.              01575021
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              01580001
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                01590001
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               01600001
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               01610001
                                                                        01610118
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    01610219
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      01610319
           COMPUTE CHANGE-AMOUNT =                                      01611018
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   01612018
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      01613018
                                                                        01613119
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     01613219
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        01613319
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        01613419
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          01613519
           IF CM-SALES-LAST-YTD = ZERO                                  01614018
               MOVE 999.9 TO CL-CHANGE-PERCENT                          01615018
           ELSE                                                         01616018
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      01617018
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              01618018
                   ON SIZE ERROR                                        01619018
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 01619118
                                                                        01619218
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       01619318
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            01620001
           WRITE PRINT-AREA.                                            01630001
           ADD 1 TO LINE-COUNT.                                         01640001
                                                                        01641018
           *> ADD THIS CUSTOMERS SALES TO THE GRAND TOTALS              01642018
           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD.               01650001
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD.               01660001
                                                                        01680001
      **************************************************************    01681011
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    01682011
      * FOR EVERY PAGE                                             *    01683011
      **************************************************************    01684011
       230-PRINT-HEADING-LINES.                                         01690001
                                                                        01700001
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             01701020
           *> SO WE INCREASE THE PAGE COUNT HERE                        01702020
           ADD 1 TO PAGE-COUNT.                                         01710001
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      01720001
                                                                        01721020
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 01722020
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           01730001
           WRITE PRINT-AREA.                                            01740001
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           01750001
           WRITE PRINT-AREA.                                            01760001
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           01770001
           WRITE PRINT-AREA.                                            01780001
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           01790001
           WRITE PRINT-AREA.                                            01800001
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           01801017
           WRITE PRINT-AREA.                                            01802017
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           01802120
           WRITE PRINT-AREA.                                            01803020
                                                                        01803120
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    01804019
           *> OF A NEW PAGE                                             01805028
           MOVE ZERO TO LINE-COUNT.                                     01810001
                                                                        01830001
      **************************************************************    01831011
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    01832011
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    01833011
      * BEEN PRINTED                                               *    01833111
      **************************************************************    01834011
       300-PRINT-GRAND-TOTALS.                                          01840001
                                                                        01850001
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                01851018
           *> OUTPUT LINE FOR GRAND TOTALS                              01852018
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             01860001
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             01870001
                                                                        01870118
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             01870218
           COMPUTE CHANGE-AMOUNT =                                      01871018
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             01872018
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     01873018
                                                                        01873120
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             01873220
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   01873320
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           01873420
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          01873520
           IF GRAND-TOTAL-LAST-YTD = ZERO                               01874018
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         01875018
           ELSE                                                         01876018
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     01877018
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           01878018
                   ON SIZE ERROR                                        01879018
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                01879118
                                                                        01879218
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  01879318
           MOVE GRAND-TOTAL-LINE1    TO PRINT-AREA.                     01880024
           WRITE PRINT-AREA.                                            01890001
           MOVE GRAND-TOTAL-LINE2    TO PRINT-AREA.                     01900024
           WRITE PRINT-AREA.                                            01910024
