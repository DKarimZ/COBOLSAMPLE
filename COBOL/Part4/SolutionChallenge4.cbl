       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEERAISE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	     SELECT EMPLOYEEFILE ASSIGN TO "EMPFILE.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.    
       	     SELECT NEWEMPFILE ASSIGN TO "NEWEMPFILE.DAT"
          	ORGANIZATION IS LINE SEQUENTIAL.      
               
       DATA DIVISION.
       FILE SECTION.
	FD EMPLOYEEFILE.
	     01 EMPDETAILS.
		88 ENDOFFILE VALUE HIGH-VALUES.
            	02 EMPDATA              	PIC X(38).
            	02 EMPINFO REDEFINES EMPDATA.
		   04 EMPLOYEEID  	 	PIC 9(7).
		   04 EMPLOYEENAME.
			05 LASTNAME	    	PIC X(10).
			05 FIRSTNAME     	PIC X(10).
		   04 STARTDATE.
		        05 START-YEAR		PIC 9(4).
		        05 START-MONTH		PIC 9(2).
		        05 START-DAY	  	PIC 9(2).
		   04 HOURSWORKED       	PIC 9(3).
            	02 HOURLYRATE        		PIC 9(4)V99.    
            	02 DEPARTMENT          		PIC X(30).   
		02 GENDER               	PIC X.

        FD NEWEMPFILE.
       	    01 NEWEMPLOYEE.
		04 NEW-EMP-DATA  	    	PIC 9(38).
		04 NEW-HOURLY-RATE     		PIC 9(4)V99.
           	04 NEW-DEPARTMENT       	PIC X(30).
            	04 NEW-GENDER           	PIC X.
            	04 NEW-SALARY         		PIC 9(5)V99.

       WORKING-STORAGE SECTION.
		   
	   01  WS-WORK-AREAS.
		05  FILE-CHECK-KEY    		PIC X(2).
           	05  WS-EMPLOYEE-COUNT 		PIC 9(5).

       PROCEDURE DIVISION.
       
       0100-READ-EMPLOYEES.
		OPEN INPUT EMPLOYEEFILE. 
           	OPEN OUTPUT NEWEMPFILE.
           	INITIALIZE WS-EMPLOYEE-COUNT.
		READ EMPLOYEEFILE
		    AT END SET ENDOFFILE TO TRUE
		END-READ.
		PERFORM 0200-PROCESS-EMPLOYEES UNTIL ENDOFFILE.
		PERFORM 9000-END-PROGRAM.
	0100-END.

	0200-PROCESS-EMPLOYEES.
		 MOVE EMPINFO TO NEW-EMP-DATA.
		 MOVE DEPARTMENT TO NEW-DEPARTMENT .
		 MOVE GENDER  TO NEW-GENDER.
		 COMPUTE NEW-HOURLY-RATE = HOURLYRATE * 1.03.
		 COMPUTE NEW-SALARY = NEW-HOURLY-RATE * HOURSWORKED.
		 WRITE NEWEMPLOYEE AFTER ADVANCING 1 LINE
		 ADD 1 TO WS-EMPLOYEE-COUNT
	         READ EMPLOYEEFILE 
		    AT END SET ENDOFFILE TO TRUE
		 END-READ.  
	 0200-END. 
	   
	   
	 9000-END-PROGRAM.	
		 CLOSE EMPLOYEEFILE, NEWEMPFILE. 
		 DISPLAY "Number of employees processed: ", WS-EMPLOYEE-COUNT.	
		 STOP RUN.
           
          END PROGRAM EMPLOYEERAISE.
