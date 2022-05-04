       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CREATEINDEXFILE.

      * Création d'un fichier indéxé à partir d'un fichier séquentiel
      **************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 

           SELECT BASEBALLFILESEQ ASSIGN TO "BASEBALL2016.NEW"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BASEBALLFILEIDX ASSIGN TO "BaseBallIdx.DAT"    
           FILE STATUS IS FILE-CHECK-KEY
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS IDTIDX
           ALTERNATE RECORD KEY IS YEARIDX
              WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.


       FD BASEBALLFILEIDX.
         01 BBRECORDIDX.
           05 IDTIDX               PIC X(36).
           05 FILLER               PIC X(4).
           05 YEARIDX              PIC 9(4).
           05 FILLER               PIC X(18).
           05 HOMETEAMIDX          PIC X(12).
           05 AWAYTEAMIDX          PIC X(12).
           05 FILLER               PIC X(34).

       FD BASEBALLFILESEQ.
         01 BBRECORDSEQ.
           88 ENDOFFILE         VALUE HIGH-VALUES.
           02 IN-RECORD            PIC X(127).
           02 IN-DETAIL-RECORD REDEFINES IN-RECORD.
               05 IN-CODESEQ         PIC X(36).
               05 IN-YR              PIC 9(4).
               05 IN-DATESEQ.
                 10 IN-YEAR         PIC 9999.
                 10 FILLER          PIC X.
                 10 IN-MONTH        PIC 99.
                 10 FILLER          PIC X.
                 10 IN-DAY          PIC 99.
              05 FILLER             PIC X.
              05 IN-START-TIME      PIC X(8).
              05 FILLER             PIC X.
              05 IN-TIMEZONE        PIC X(3).
              05 IN-ATTENDANCE      PIC 9(5).
              05 IN-HOME-TEAM       PIC X(12).
              05 IN-AWAY-TEAM       PIC X(12).
              05 IN-VENUE           PIC X(20).
              05 IN-STATE           PIC X(2).
              05 IN-HOME-SCORE      PIC 9(2).
              05 IN-AWAY-SCORE      PIC 9(2).
              05 IN-INNING          PIC 9(2).
              05 IN-STATUS          PIC X(6).


       WORKING-STORAGE SECTION. 
         01 WS-WORKING-STORAGE.
           05 FILLER               PIC X(27)
              VALUE 'WORKING STORAGE STARTS HERE'.

         01 WS-WORK-AREAS.
           05 FILE-CHECK-KEY       PIC X(2).

       PROCEDURE DIVISION.
       0100-READ-BBGAMES.

           OPEN INPUT BASEBALLFILESEQ.
           OPEN OUTPUT BASEBALLFILEIDX.
           READ BASEBALLFILESEQ
           AT END SET ENDOFFILE TO TRUE 
           END-READ.

           PERFORM 0200-PERFORM-FILE UNTIL ENDOFFILE.
           PERFORM 0900-END-PROGRAM.

       0200-PERFORM-FILE.
           WRITE BBRECORDIDX FROM BBRECORDSEQ
              INVALID KEY DISPLAY
              "BASEBALL STATUS = " FILE-CHECK-KEY 
           END-WRITE.
           READ BASEBALLFILESEQ 
           AT END SET ENDOFFILE TO TRUE
           END-READ.

       0900-END-PROGRAM.
           CLOSE BASEBALLFILESEQ BASEBALLFILEIDX.
           STOP RUN.

       END PROGRAM CREATEINDEXFILE.
