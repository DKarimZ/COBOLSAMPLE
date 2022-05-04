       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALESWITHCOMMISSION.
       AUTHOR.     PEGGY FISHER.
      ***************************************************************
      *  This program reads a file containing sales person yearly   *
      *   sales information and prints a report.                    *
      ***************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
       OBJECT-COMPUTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT SALESFILE ASSIGN TO "SALES.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO "SALESREPORT.DAT".


       DATA DIVISION.
       FILE SECTION.
       FD SALESFILE.

       01 SALESDETAILS.
            88 ENDOFSALES VALUE HIGH-VALUES.
            05 SALESPERSON-ID       PIC 9(5).
            05 SALESPERSON-NAME.
                10 LASTNAME         PIC X(20).
                10 FIRSTNAME        PIC X(20).
            05 REGION               PIC X(5).
            05 YEARLYSALES          PIC 9(6).
            05 GENDER               PIC X.

        FD PRINT-FILE.

        01  PRINT-LINE             PIC X(132).

        WORKING-STORAGE SECTION.
        01  WS-CURRENT-DATE-DATA.
           05 WS-CURRENT-DATE.
              10 WS-CURRENT-YEAR               PIC 9(04).
              10 WS-CURRENT-MONTH              PIC 9(02).
              10 WS-CURRENT-DAY                PIC 9(02).
           05 WS-CURRENT-TIME.
              10 WS-CURRENT-HOURS              PIC 9(02).   
              10 WS-CURRENT-MINUTES            PIC 9(02).
              10 WS-CURRENT-SECONDS            PIC 9(02).
              10 WS-CURRENT-HMILLISECONDS      PIC 9(02).

        01  WS-FIELDS.
            05 WS-TOTAL-SALES      PIC 9(10) COMP-3 VALUE ZEROES.
            05 WS-COMMISSION-RATE  PIC V99 VALUE .05.
            05 WS-COMMISSION-AMT   PIC 9(10) COMP-3.
            05 WS-TOTAL-COMM       PIC 9(8) COMP-3 VALUE ZEROES.

        01  WS-REGION-SALES.
            05 WS-EAST             PIC 9(7) VALUE ZEROES.
            05 WS-WEST             PIC 9(7) VALUE ZEROES.
            05 WS-NORTH            PIC 9(7) VALUE ZEROES.
            05 WS-SOUTH            PIC 9(7) VALUE ZEROES.

        01  HEADING-LINE.
            05 FILLER              PIC X(5) VALUE SPACES.
            05 FILLER              PIC X(16) VALUE 'SALESPERSON NAME'.
            05 FILLER              PIC X(29) VALUE SPACES.
            05 FILLER              PIC X(6)  VALUE 'REGION'.
            05 FILLER              PIC X(9) VALUE SPACES.
            05 FILLER              PIC X(12) VALUE 'COMMISSIONS'.
            05 FILLER              PIC X(10) VALUE SPACES.
            05 HEAD-DATE.
              10 HEAD-MONTH        PIC X(2).
              10 FILLER            PIC X VALUE '/'.
              10 HEAD-DAY          PIC X(2).
              10 FILLER            PIC X VALUE '/'.
              10 HEAD-YEAR         PIC X(4).
              10 FILLER            PIC X VALUE '/'.

        01  HEADING-LINE-2.
           05 FILLER              PIC X(5) VALUE SPACES.
           05 FILLER              PIC X(16) VALUE '----------------'.
           05 FILLER              PIC X(29) VALUE SPACES.
           05 FILLER              PIC X(6)  VALUE '------'.
           05 FILLER              PIC X(9) VALUE SPACES.
           05 FILLER              PIC X(12) VALUE '------------'.
           05 FILLER              PIC X(10) VALUE SPACES.   

        01  DETAIL-LINE.
            05 FILLER               PIC X(5)  VALUE SPACES.
            05 DET-SALESPERSON-NAME PIC X(40).
            05 FILLER               PIC X(5)  VALUE SPACES.
            05 DET-REGION           PIC X(5).
            05 FILLER               PIC X(1)  VALUE SPACES.
            05 DET-COMMISSIONAMT    PIC $$$,$$$,$$$,$$9.
            05 FILLER               PIC X(10)  VALUE SPACES.


        01  TOTAL-LINE.
             05 FILLER               PIC X(5)   VALUE SPACES.
             05 FILLER               PIC X(16)  VALUE SPACES.
             05 FILLER               PIC X(10)  VALUE SPACES.
             05 FILLER               PIC X(10)   VALUE SPACES.
             05 FILLER               PIC X(15)  VALUE 'GRAND  TOTAL'.
             05 TOTAL-YRLY-COMM      PIC $**,***,***,**9.
             05 FILLER               PIC X(43)  VALUE SPACES.
                      
        

        PROCEDURE DIVISION.

        0050-OPEN-FILE.
           OPEN INPUT SALESFILE.
           OPEN OUTPUT PRINT-FILE.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0200-STOP-RUN.

        0100-PROCESS-RECORDS.

           PERFORM 0110-WRITE-HEADING-LINE.
           READ SALESFILE
                AT END SET ENDOFSALES TO TRUE
           END-READ.
           PERFORM UNTIL ENDOFSALES
            ADD YEARLYSALES TO WS-TOTAL-SALES
            MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME
            MOVE REGION TO DET-REGION
            COMPUTE WS-COMMISSION-AMT = WS-COMMISSION-RATE *
              YEARLYSALES
            MOVE WS-COMMISSION-AMT TO DET-COMMISSIONAMT 
            COMPUTE  WS-TOTAL-COMM = WS-TOTAL-COMM + WS-COMMISSION-AMT
            PERFORM 0120-WRITE-DETAIL-LINE

            READ SALESFILE
            AT END SET ENDOFSALES TO TRUE
            END-READ
           END-PERFORM.
           PERFORM 0130-WRITE-TOTAL-LINE.

        0110-WRITE-HEADING-LINE.
            MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
            MOVE WS-CURRENT-DAY TO HEAD-DAY. 
            MOVE WS-CURRENT-MONTH TO HEAD-MONTH. 
            MOVE WS-CURRENT-YEAR TO HEAD-YEAR. 
            MOVE HEADING-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
            MOVE HEADING-LINE-2 TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

        0120-WRITE-DETAIL-LINE.
            MOVE DETAIL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

        0130-WRITE-TOTAL-LINE.
            MOVE WS-TOTAL-COMM TO TOTAL-YRLY-COMM .
            MOVE TOTAL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

        0200-STOP-RUN.
           CLOSE SALESFILE.
           CLOSE PRINT-FILE.
           STOP RUN.

          END PROGRAM SALESWITHCOMMISSION.
