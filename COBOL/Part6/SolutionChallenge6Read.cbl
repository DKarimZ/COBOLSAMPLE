       IDENTIFICATION DIVISION.
       PROGRAM-ID. READINDEXFILE.

      *Ce programme lit le fichier indexé précédemment créé"
      *********************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

           SELECT BBFILE ASSIGN TO "BaseBallIdx.DAT"
           FilE STATUS IS FILE-CHECK-KEY
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS IDTIDX
           ALTERNATE RECORD KEY IS YEARIDX
              WITH DUPLICATES.

       DATA DIVISION. 
       FILE SECTION. 
       FD BBFILE.
          01 BBRECORD.
           88 ENDOFFILE         VALUE HIGH-VALUES.
           05 IDTIDX               PIC X(36).
           05 YEARIDX              PIC 9(4).
           05 YYYY-MM-DDIDX        PIC X(10).
           05 TIMERIDX             PIC x(13).
           05 ATTENDANCEIDX        PIC X(5).
           05 HOMETEAMIDX          PIC X(12).
           05 AWAYTEAMIDX          PIC X(12).
           05 MOREINFOIDX          PIC X(35).
        
       WORKING-STORAGE SECTION.

         01  WS-WORKING-STORAGE.
           05 FILLER               PIC X(27) VALUE 
              'WORKING STORAGE STARTS HERE'.

         01  WS-WORK-AREAS.
           05 FILE-CHECK-KEY       PIC X(2).
           88 RECORDFOUND          VALUE '00'.

           05 READTYPE.
              88 IDTIDXKEY         VALUE 1.
              88 YEARIDXKEY        value 2.
        
           05 PRINTRECORD.
              10 PRTBBIDTIDXKEY    PIC X(36).
              10 PRTBBYEARIDXKEY   PIC 9(4).
              10 PRTBBATTENDANCEKEY   PIC 9(4).
              10 PRTBBHOMETEAMKEY   PIC 9(4).
              10 PRTBBAWAYTEAMKEY   PIC 9(4).
              10 PRTBBMOREINFOKEY   PIC 9(4).

       PROCEDURE DIVISION.

       0100-START.

           OPEN INPUT BBFILE.
           DISPLAY " SELECTIONNER SELON L'ID du match, ENTRER 1".
           DISPLAY " SELECTIONNER SELON L'année du match, ENTRER 2".

           ACCEPT READTYPE.

           IF IDTIDXKEY
              DISPLAY " Entrer le numéro du match ( 36 chiffres ) "
                 WITH NO ADVANCING.  
              ACCEPT IDTIDX
              READ BBFILE
                 KEY IS IDTIDX
                 INVALID KEY DISPLAY "BASEBALL STATUS: ",
                 FILE-CHECK-KEY
              END-READ
           END-IF.

           IF YEARIDXKEY 
              DISPLAY " Entrer l'année' du match ( 4 chiffres ) "
                 WITH NO ADVANCING.  
              ACCEPT YEARIDX
              READ BBFILE
                 KEY IS YEARIDX
                 INVALID KEY DISPLAY "BASEBALL STATUS: ",
                 FILE-CHECK-KEY
              END-READ
           END-IF.

           IF RECORDFOUND
              MOVE IDTIDX TO PRTBBIDTIDXKEY
              MOVE YEARIDX TO PRTBBYEARIDXKEY
              MOVE ATTENDANCEIDX TO PRTBBATTENDANCEKEY
              MOVE HOMETEAMIDX TO PRTBBHOMETEAMKEY
              MOVE AWAYTEAMIDX TO PRTBBTEAMKEY
              MOVE MOREINFOIDX TO PRTBBMOREINFOKEY
              DISPLAY PRINTRECORD
           END-IF.
              