       IDENTIFICATION DIVISION.                                         00010001
                                                                        00020001
       PROGRAM-ID. RPT2000.                                             00030001
                                                                        00040001
      *   Programmers.: Violet French                                   00050001
      *   Date........: 2026.02.19                                      00060001
      *   Github URL..:                                                 00070001
      *   Description: This program uses hard coded values to calculate 00090001
      *   investments and doubles them twice.                           00100001
       ENVIRONMENT DIVISION.                                            00110001
                                                                        00120001
       INPUT-OUTPUT SECTION.                                            00130001
                                                                        00140001
       FILE-CONTROL.                                                    00150001
           SELECT CUSTMAST ASSIGN TO CUSTMAS.                           00160001
           SELECT SALESRPT ASSIGN TO SALESRPT.                          00170001
                                                                        00180001
       DATA DIVISION.                                                   00190001
                                                                        00200001
       FILE SECTION.                                                    00210001
                                                                        00220001
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
       FD  SALESRPT                                                     00370001
           RECORDING MODE IS F                                          00380001
           LABEL RECORDS ARE STANDARD                                   00390001
           RECORD CONTAINS 130 CHARACTERS                               00400001
           BLOCK CONTAINS 130 CHARACTERS.                               00410001
       01  PRINT-AREA      PIC X(130).                                  00420001
                                                                        00430001
       WORKING-STORAGE SECTION.                                         00440001
                                                                        00450001
       01  SWITCHES.                                                    00460001
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00470001
                                                                        00480001
       01  PRINT-FIELDS.                                                00490001
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00500001
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00510001
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00520001
           05  SPACE-CONTROL   PIC S9.                                  00530001
                                                                        00540001
       01  TOTAL-FIELDS.                                                00550001
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00560001
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00570001
                                                                        00580001
       01  CURRENT-DATE-AND-TIME.                                       00590001
           05  CD-YEAR         PIC 9999.                                00600001
           05  CD-MONTH        PIC 99.                                  00610001
           05  CD-DAY          PIC 99.                                  00620001
           05  CD-HOURS        PIC 99.                                  00630001
           05  CD-MINUTES      PIC 99.                                  00640001
           05  FILLER          PIC X(9).                                00650001
                                                                        00660001
       01  HEADING-LINE-1.                                              00670001
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             00680001
           05  HL1-MONTH       PIC 9(2).                                00690001
           05  FILLER          PIC X(1)    VALUE "/".                   00700001
           05  HL1-DAY         PIC 9(2).                                00710001
           05  FILLER          PIC X(1)    VALUE "/".                   00720001
           05  HL1-YEAR        PIC 9(4).                                00730001
           05  FILLER          PIC X(11)   VALUE SPACE.                 00740001
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".00750001
           05  FILLER          PIC X(20)   VALUE "EPORT               ".00760001
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            00770001
           05  Hl1-PAGE-NUMBER PIC ZZZ9.                                00780001
           05  FILLER          PIC X(49)   VALUE SPACE.                 00790001
                                                                        00800001
       01  HEADING-LINE-2.                                              00810001
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             00820001
           05  HL2-HOURS       PIC 9(2).                                00830001
           05  FILLER          PIC X(1)    VALUE ":".                   00840001
           05  HL2-MINUTES     PIC 9(2).                                00850001
           05  FILLER          PIC X(58)   VALUE SPACE.                 00860001
           05  FILLER          PIC X(10)   VALUE "RPT1000".             00870001
           05  FILLER          PIC X(49)   VALUE SPACE.                 00880001
                                                                        00890001
       01  HEADING-LINE-3.                                              00900001
           05  FILLER      PIC X(20)   VALUE "CUST                ".    00910001
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    00920001
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    00930001
           05  FILLER      PIC X(69)   VALUE SPACE.                     00940001
                                                                        00950001
       01  HEADING-LINE-4.                                              00960001
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    00970001
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    00980001
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    00990001
           05  FILLER      PIC X(69)   VALUE SPACE.                     01000001
                                                                        01010001
       01  CUSTOMER-LINE.                                               01020001
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            01030001
           05  FILLER              PIC X(2)     VALUE SPACE.            01040001
           05  CL-CUSTOMER-NAME    PIC X(20).                           01050001
           05  FILLER              PIC X(3)     VALUE SPACE.            01060001
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      01070001
           05  FILLER              PIC X(4)     VALUE SPACE.            01080001
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      01090001
           05  FILLER              PIC X(69)    VALUE SPACE.            01100001
                                                                        01110001
       01  GRAND-TOTAL-LINE.                                            01120001
           05  FILLER              PIC X(27)    VALUE SPACE.            01130001
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   01140001
           05  FILLER              PIC X(1)     VALUE SPACE.            01150001
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   01160001
           05  FILLER              PIC X(69)    VALUE SPACE.            01170001
                                                                        01180001
       PROCEDURE DIVISION.                                              01190001
                                                                        01200001
       000-PREPARE-SALES-REPORT.                                        01210001
                                                                        01220001
           OPEN INPUT  CUSTMAST                                         01230001
                OUTPUT SALESRPT.                                        01240001
           PERFORM 100-FORMAT-REPORT-HEADING.                           01250001
           PERFORM 200-PREPARE-SALES-LINES                              01260001
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         01270001
           PERFORM 300-PRINT-GRAND-TOTALS.                              01280001
           CLOSE CUSTMAST                                               01290001
                 SALESRPT.                                              01300001
           STOP RUN.                                                    01310001
                                                                        01320001
       100-FORMAT-REPORT-HEADING.                                       01330001
                                                                        01340001
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         01350001
           MOVE CD-MONTH   TO HL1-MONTH.                                01360001
           MOVE CD-DAY     TO HL1-DAY.                                  01370001
           MOVE CD-YEAR    TO HL1-YEAR.                                 01380001
           MOVE CD-HOURS   TO HL2-HOURS.                                01390001
           MOVE CD-MINUTES TO HL2-MINUTES.                              01400001
                                                                        01410001
       200-PREPARE-SALES-LINES.                                         01420001
                                                                        01430001
           PERFORM 210-READ-CUSTOMER-RECORD.                            01440001
           IF CUSTMAST-EOF-SWITCH = "N"                                 01450001
               PERFORM 220-PRINT-CUSTOMER-LINE.                         01460001
                                                                        01470001
       210-READ-CUSTOMER-RECORD.                                        01480001
                                                                        01490001
           READ CUSTMAST                                                01500001
               AT END                                                   01510001
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     01520001
                                                                        01530001
       220-PRINT-CUSTOMER-LINE.                                         01540001
                                                                        01550001
           IF LINE-COUNT >= LINES-ON-PAGE                               01560001
               PERFORM 230-PRINT-HEADING-LINES.                         01570001
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              01580001
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                01590001
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               01600001
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               01610001
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            01620001
           WRITE PRINT-AREA.                                            01630001
           ADD 1 TO LINE-COUNT.                                         01640001
           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD.               01650001
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD.               01660001
           MOVE 1 TO SPACE-CONTROL.                                     01670001
                                                                        01680001
       230-PRINT-HEADING-LINES.                                         01690001
                                                                        01700001
           ADD 1 TO PAGE-COUNT.                                         01710001
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      01720001
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           01730001
           WRITE PRINT-AREA.                                            01740001
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           01750001
           WRITE PRINT-AREA.                                            01760001
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           01770001
           WRITE PRINT-AREA.                                            01780001
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           01790001
           WRITE PRINT-AREA.                                            01800001
           MOVE ZERO TO LINE-COUNT.                                     01810001
           MOVE 2 TO SPACE-CONTROL.                                     01820001
                                                                        01830001
       300-PRINT-GRAND-TOTALS.                                          01840001
                                                                        01850001
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             01860001
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             01870001
           MOVE GRAND-TOTAL-LINE     TO PRINT-AREA.                     01880001
           WRITE PRINT-AREA.                                            01890001
