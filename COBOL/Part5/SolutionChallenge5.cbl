       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGEFILES.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	          SELECT ACME ASSIGN TO "ACME.DAT"
           FILE STATUS IS ACME-FILE-CHECK-KEY
		          ORGANIZATION IS LINE SEQUENTIAL.
      
	           SELECT FUSESINC ASSIGN TO "FUSESINC.DAT"
           FILE STATUS IS FUSESINC-FILE-CHECK-KEY
	             ORGANIZATION IS LINE SEQUENTIAL.
    
           SELECT SORTEDFILE ASSIGN TO "FUSION.NEW"
           ORGANIZATION IS LINE SEQUENTIAL.
		
            SELECT WORKFILE ASSIGN TO "WORK.TMP".
	               
       DATA DIVISION.
       FILE SECTION.
	    FD ACME.
	    01 STUDENTDETAILS     PIC X(40).
	
       FD FUSESINC .
	    01 FUSESINCDETAILS  PIC X(40).
				
       FD SORTEDFILE.
	    01 SORTDETAILS        PIC X(40).	
				
       SD WORKFILE.
       01 WORKREC.
          	02 WSSECURITYNUMBER   PIC 9(9).
		    	02 WSTUDENTLNAME PIC X(10).
		    	02 WSTUDENTFNAME PIC X(10).
            02 HIREDATE      PIC X(8).
            02 SALARY        PIC 9(9).		  
            02 GENDER        PIC X.	   
   
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER      PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.   
   
	      01  WS-WORK-AREAS.
	         05  ACME-FILE-CHECK-KEY   PIC X(2).
            05  FUSESINC-FILE-CHECK-KEY    PIC X(2).
		 

       PROCEDURE DIVISION.
       0050-START.
                
            PERFORM 0100-READ-FILES.
            PERFORM 9000-END-PROGRAM.
    

       0100-READ-FILES.

		         OPEN INPUT ACME, FUSESINC.
		   		IF ACME-FILE-CHECK-KEY NOT = "00"
                    DISPLAY "ERR: PB WITH OPEN FILE ACME ",
                   ACME-FILE-CHECK-KEY
                  GO TO 9000-END-PROGRAM
              END-IF.
              IF FUSESINC-FILE-CHECK-KEY NOT = "00"
                 DISPLAY "ERR: PB WITH OPEN FILE FUSESINC ",
                 FUSESINC-FILE-CHECK-KEY
                  GO TO 9000-END-PROGRAM
              END-IF.
                		
		         MERGE WORKFILE ON ASCENDING KEY 
		      WSSECURITYNUMBER
		      USING ACME 
			    FUSESINC
			     GIVING SORTEDFILE.
		   
	   
	      9000-END-PROGRAM.
           CLOSE ACME, FUSESINC.    	 
                
           STOP RUN.         
          END PROGRAM MERGEFILES.
