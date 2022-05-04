       IDENTIFICATION DIVISION. 
       PROGRAM-ID. SEARCHWEATHER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT WEATHERDATA ASSIGN TO "weather2020.dat"
           FILE STATUS IS FILE-CHECK-KEY
              ORGANIZATION IS LINE SEQUENTIAL.
             

       
       DATA DIVISION.
       FILE SECTION.
       FD WEATHERDATA.
       01 WEATHERRECORD.
           88 ENDOFFILE       VALUE HIGH-VALUES.
           05 NEWSTATION         PIC 9(6).
           05 NEWMONTH           PIC 9(2).
           05 NEWDAY             PIC 9(2).
           05 NEWYEAR            PIC 9(4).
           05 NEWMOY-TEMP        PIC 999.9.
           05 NEWFOG             PIC 9.
           05 NEWRAIN            PIC 9.
           05 NEWSNOW            PIC 9.
           05 NEWHAIL            PIC 9.
           05 NEWTHUNDER         PIC 9.
           05 NEWSTORM           PIC 99999.

       WORKING-STORAGE SECTION.   
       01 WS-WEATHER-RECORD-TABLE OCCURS 100 TIMES ASCENDING
           KEY IS TABLE-STATION
           INDEXED BY STATION-INDEX.

           05 TABLE-STATION       PIC 9(6).
           05 TABLE-MONTH         PIC 9(2).
           05 TABLE-DAY           PIC 9(2).
           05 TABLE-YEAR          PIC 9(4).
           05 TABLE-MOYTEMP       PIC 999.9.
           05 TABLE-FOG           PIC 9.
           05 TABLE-RAIN          PIC 9.
           05 TABLE-SNOW          PIC 9.
           05 TABLE-HAIL          PIC 9.
           05 TABLE-THUNDER       PIC 9.
           05 TABLE-STORM         PIC 9. 
       
       01 WS-UTILS.
           05 WS-NUMBEROFSTAT    PIC 999.
           05 WS-SUBSCRIPT       PIC 999.
           05 SUBSCRIPT          PIC 999.
           05 FILE-CHECK-KEY     PIC 99.

       01 WS-USER-CHOICE.
          05 WS-ALLINTABLE       PIC 9.
          05 WS-SELECTSTATION    PIC 9.
          05 SEARCH-STATION-KEY  PIC 9(6).
          05 SEARCH-MESSAGE      PIC X(50).

       01 DET-LINE OCCURS 100 TIMES.
           05 FILLER             PIC X(10).
           05 DET-STATION        PIC 9(6).
           05 FILLER             PIC X(5).
           05 DET-MONTH          PIC 9(2).
           05 FILLER             PIC X(5).
           05 DET-DAY            PIC 9(2).
           05 FILLER             PIC X(5).
           05 DET-YEAR           PIC 9(4).
           05 FILLER             PIC X(5).
           05 DET-MOYTEMP        PIC 999.9.
           05 FILLER             PIC X(5).
           05 DET-FOG            PIC 9.
           05 FILLER             PIC X(5).
           05 DET-RAIN           PIC 9.
           05 FILLER             PIC X(5).
           05 DET-SNOW           PIC 9.
           05 FILLER             PIC X(5).
           05 DET-HAIL           PIC 9.
           05 FILLER             PIC X(5).
           05 DET-THUNDER        PIC 9.  
           05 FILLER             PIC X(5).
           05 DET-STORM          PIC 99999.

       01 HEADER-LINE-1.
           05 FILLER            PIC X(10).
           05 FILLER            PIC X(7)  VALUE 'STATION'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE 'MONTH'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(3)  VALUE 'DAY'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(4)  VALUE 'YEAR'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(3)  VALUE 'MOY'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(3)  VALUE 'FOG'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(4)  VALUE 'RAIN'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(4)  VALUE 'SNOW'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE 'HAIL'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE 'THUND'.  
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE 'STORM'.

       01 HEADER-LINE-2.
           05 FILLER            PIC X(10).
           05 FILLER            PIC X(7)  VALUE '-------'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE '-----'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(3)  VALUE '---'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(4)  VALUE  '----'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(3)  VALUE '---'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(3)  VALUE '---'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(4)  VALUE '----'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(4)  VALUE '----'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE '-----'.
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE '-----'.  
           05 FILLER            PIC X(5).
           05 FILLER            PIC X(5)  VALUE '-----'.

       PROCEDURE DIVISION.

       0100-START.
           OPEN INPUT WEATHERDATA.
           IF FILE-CHECK-KEY NOT= '00'
              DISPLAY 'FILE STATUS', FILE-CHECK-KEY 
              STOP RUN.
           READ WEATHERDATA 
              AT END SET ENDOFFILE TO TRUE
           END-READ.
           COMPUTE WS-NUMBEROFSTAT = 1.
           COMPUTE WS-SUBSCRIPT = 1.
           COMPUTE SUBSCRIPT = 1.
           PERFORM 0200-PROCESS-FILE UNTIL ENDOFFILE.
           DISPLAY 'Afficher toutes les stations en tapant 1'.
           DISPLAY 'Afficher une station specifique en tapant 2'.
           ACCEPT WS-USER-CHOICE.
           IF WS-USER-CHOICE = 2
              DISPLAY HEADER-LINE-1
              DISPLAY HEADER-LINE-2
              PERFORM 0220-PROCESS-SEARCHONE 
           ELSE IF WS-USER-CHOICE = 1
              DISPLAY HEADER-LINE-1
              DISPLAY HEADER-LINE-2
              PERFORM 0210-DISPLAY-DETAILS 
           END-IF.
           PERFORM 0900-STOP-RUN.


       0200-PROCESS-FILE.
           ADD 1 TO WS-NUMBEROFSTAT.
           MOVE NEWSTATION   TO TABLE-STATION (Ws-SUBSCRIPT).
           MOVE NEWMONTH     TO TABLE-MONTH   (Ws-SUBSCRIPT).
           MOVE NEWDAY       TO TABLE-DAY     (Ws-SUBSCRIPT).
           MOVE NEWYEAR      TO TABLE-YEAR    (Ws-SUBSCRIPT).
           MOVE NEWMOY-TEMP  TO TABLE-MOYTEMP (Ws-SUBSCRIPT).
           MOVE NEWFOG       TO TABLE-FOG     (Ws-SUBSCRIPT).
           MOVE NEWRAIN      TO TABLE-RAIN     (Ws-SUBSCRIPT).
           MOVE NEWSNOW      TO TABLE-SNOW    (Ws-SUBSCRIPT).
           MOVE NEWHAIL      TO TABLE-HAIl    (Ws-SUBSCRIPT).
           MOVE NEWTHUNDER   TO TABLE-THUNDER (Ws-SUBSCRIPT).
           MOVE NEWSTORM     TO TABLE-STORM   (Ws-SUBSCRIPT).
           READ WEATHERDATA 
             AT END SET ENDOFFILE TO TRUE
           END-READ.
           ADD 1 TO WS-SUBSCRIPT.


       0210-DISPLAY-DETAILS.
           PERFORM VARYING SUBSCRIPT FROM 1 BY 1
           UNTIL SUBSCRIPT > WS-NUMBEROFSTAT
           MOVE TABLE-STATION(SUBSCRIPT) TO DET-STATION(SUBSCRIPT)
           MOVE TABLE-MONTH  (SUBSCRIPT) TO DET-MONTH  (SUBSCRIPT)
           MOVE TABLE-DAY    (SUBSCRIPT) TO DET-DAY    (SUBSCRIPT)
           MOVE TABLE-YEAR   (SUBSCRIPT) TO DET-YEAR   (SUBSCRIPT)
           MOVE TABLE-MOYTEMP(SUBSCRIPT) TO DET-MOYTEMP(SUBSCRIPT) 
           MOVE TABLE-FOG    (SUBSCRIPT) TO DET-FOG    (SUBSCRIPT)
           MOVE TABLE-RAIN   (SUBSCRIPT) TO DET-RAIN   (SUBSCRIPT)
           MOVE TABLE-SNOW   (SUBSCRIPT) TO DET-SNOW   (SUBSCRIPT)
           MOVE TABLE-HAIL   (SUBSCRIPT) TO DET-HAIL   (SUBSCRIPT)
           MOVE TABLE-THUNDER(SUBSCRIPT) TO DET-THUNDER(SUBSCRIPT)
           MOVE TABLE-STORM  (SUBSCRIPT) TO DET-STORM  (SUBSCRIPT)
           DISPLAY DET-LINE (SUBSCRIPT)
           END-PERFORM.


       0220-PROCESS-SEARCHONE.  
           DISPLAY "Entrer un numéro de station".
           ACCEPT SEARCH-STATION-KEY.
           SEARCH  WS-WEATHER-RECORD-TABLE 
              AT END
                 MOVE 'NOT FOUND' TO SEARCH-MESSAGE
              WHEN TABLE-STATION (STATION-INDEX) = SEARCH-STATION-KEY 
               MOVE TABLE-STATION(STATION-INDEX) 
                 TO DET-STATION(STATION-INDEX)
               MOVE TABLE-MONTH  (STATION-INDEX) 
                 TO DET-MONTH  (STATION-INDEX)
               MOVE TABLE-DAY    (STATION-INDEX) 
                 TO DET-DAY    (STATION-INDEX)
               MOVE TABLE-YEAR   (STATION-INDEX) 
                 TO DET-YEAR   (STATION-INDEX)
               MOVE TABLE-MOYTEMP(STATION-INDEX) 
                 TO DET-MOYTEMP(STATION-INDEX)
               MOVE TABLE-FOG    (STATION-INDEX) 
                 TO DET-FOG    (STATION-INDEX)
               MOVE TABLE-RAIN   (STATION-INDEX) 
                 TO DET-RAIN   (STATION-INDEX)
               MOVE TABLE-SNOW   (STATION-INDEX) 
                 TO DET-SNOW   (STATION-INDEX)
               MOVE TABLE-HAIL   (STATION-INDEX) 
                 TO DET-HAIL   (STATION-INDEX)
               MOVE TABLE-THUNDER(STATION-INDEX) 
                  TO DET-THUNDER(STATION-INDEX)
               MOVE TABLE-STORM  (STATION-INDEX) 
                 TO DET-STORM  (STATION-INDEX)
           END-SEARCH.
           DISPLAY DET-LINE (STATION-INDEX).
           PERFORM 0900-STOP-RUN.

              

       0900-STOP-RUN.
           CLOSE WEATHERDATA.
           STOP RUN.

       END PROGRAM SEARCHWEATHER.
