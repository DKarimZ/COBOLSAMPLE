              IDENTIFICATION DIVISION.
       PROGRAM-ID. PETSTORECHALLENGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT PETSALESFILE ASSIGN TO "PETSTORESALES.DAT"
		       ORGANIZATION IS LINE SEQUENTIAL.
              SELECT PETSALEREPORT ASSIGN TO "PETSALESREPORT.DAT"
             ORGANIZATION IS LINE SEQUENTIAL. 
       DATA DIVISION.
	     FILE SECTION.
       FD PETSALESFILE.

		 
	    01 SALESDETAILS.
			  88 ENDOFSALESFILE VALUE HIGH-VALUES.
           02 CUSTOMER-ID      PIC 9(7).
			  02 CUSTOMERNAME.
			      05  LASTNAME     PIC X(15).
			      05  FIRSTNAME    PIC X(15).
           02 PETITEM OCCURS 3 TIMES.
			      05 DESCRIPTION      PIC X(20).
			      05 PRICE            PIC 999999V99.
               05 QUANTITY         PIC 99999.
       
       FD PETSALEREPORT.
           01 PRINT-LINE     PIC X(100).

       WORKING-STORAGE SECTION.

	    01  WS-FIELDS.
		     05  WS-TOTAL-QUANT   PIC 999.
		     05  WS-ITEM-TOTAL    PIC 9999V99.
		     05  WS-TOTAL-SALE    PIC 99999V99.
           05  WS-INDEX         PIC 999.

       01   WS-SUBTOTAL-FIELDS.
           05  WS-NAME          PIC X(20).
           05  WS-SUB-QTY       PIC 999.
           05  WS-SUB-SUBTOTAL  PIC 99999V99.
      

       01  WS-DATE.
           05  WS-YEAR PIC 99.
           05  WS-MONTH PIC 99.
           05  WS-DAY   PIC 99.	   
		   
       01  HEADING-LINE.
            05 FILLER	        PIC X(16) VALUE 'ITEM DESCRIPTION'.
            05 FILLER	        PIC X(20) VALUE SPACES.
            05 FILLER	        PIC X(11)  VALUE 'PRICE'.
            05 FILLER	        PIC X(2) VALUE SPACES.
            05 FILLER	        PIC X(11)  VALUE 'QUANTITY'.
            05 FILLER	        PIC X(2) VALUE SPACES.
            05 FILLER	        PIC X(11)  VALUE 'TOTAL'.

       01  DETAIL-LINE.
			   05 FILLER           PIC X(5)  VALUE SPACES.
			   05 DET-DESCRIPTION  PIC X(20).
			   05 FILLER           PIC X(9)  VALUE SPACES.
			   05 DET-PRICE        PIC $,$$9.99.
			   05 FILLER           PIC X(8)  VALUE SPACES.
			   05 DET-QUANTITY     PIC Z9.
			   05 FILLER           PIC X(7)  VALUE SPACES.
			   05 DET-ITEM-TOTAL   PIC $$,$$9.99.

       01  DETAIL-SUBTOTAL-LINE.
            05 FILLER           PIC X(10) VALUE SPACES.
            05 DET-NAME         PIC X(20).
            05 FILLER           PIC X(6) VALUE SPACES.
            05 FILLER           PIC X(9) VALUE 'QUANTITY'.
            05 DET-SUB-QTY      PIC 999.
            05 FILLER           PIC X(6) VALUE SPACES.
            05 FILLER           PIC X(9) VALUE 'SUBTOTAL'.
            05 DET-SUBTOTAL     PIC $$,$$9.99.

      
       01 SUPER-SPACING.
            05 SPACING          PIC X(100) VALUE
             '--------------------------------------------------------'. 
          
		 01  DETAIL-TOTAL-LINE.
            05 FILLER           PIC X(7) VALUE SPACES.
			   05 FILLER           PIC X(19)  VALUE 
			       "    TOTAL QUANITY: ".
			   05 DET-TOTAL-QUANT  PIC 999. 
			   05 FILLER           PIC XX.
			   05 FILLER           PIC X(23)  VALUE 
			       "TOTAL AMOUNT: ".
		      05 FILLER           PIC X(1)  VALUE SPACES.
			   05 DET-TOT-SALES     PIC $$,$$$,$$9.99.
			   05 FILLER           PIC X(3)  VALUE SPACES.

       PROCEDURE DIVISION.
       0100-START.
           OPEN INPUT PETSALESFILE
           OPEN OUTPUT PETSALEREPORT . 
            READ PETSALESFILE
			    AT END SET ENDOFSALESFILE TO TRUE
			    END-READ.
            MOVE "Welcome to Pet Supplies and More" TO PRINT-LINE.
            WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
            MOVE HEADING-LINE TO PRINT-LINE 
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
		   
		     PERFORM 0200-PROCESS-ITEMS UNTIL ENDOFSALESFILE
		     PERFORM 0290-PRINT-TOTAL.
		     PERFORM 0300-STOP-RUN.
	     0100-END.	
		   
       0200-PROCESS-ITEMS.
		     MOVE 1 TO WS-INDEX.
        	  MOVE 0 TO WS-SUB-QTY.
             MOVE 0 TO WS-SUB-SUBTOTAL
           PERFORM 3 TIMES   
		         MOVE DESCRIPTION(WS-INDEX ) TO DET-DESCRIPTION
		         MOVE PRICE(WS-INDEX ) TO DET-PRICE
		         MOVE QUANTITY(WS-INDEX ) TO DET-QUANTITY
              ADD QUANTITY(WS-INDEX) TO WS-SUB-QTY
	          COMPUTE WS-ITEM-TOTAL = PRICE(WS-INDEX ) 
              *    QUANTITY(WS-INDEX )
              ADD WS-ITEM-TOTAL  TO WS-SUB-SUBTOTAL
		       COMPUTE WS-TOTAL-SALE = WS-TOTAL-SALE + 
			    WS-ITEM-TOTAL
		         COMPUTE WS-TOTAL-QUANT = WS-TOTAL-QUANT + 
			   QUANTITY(WS-INDEX )
		         
		         MOVE WS-ITEM-TOTAL TO DET-ITEM-TOTAL
               MOVE DETAIL-LINE TO PRINT-LINE
               WRITE PRINT-LINE AFTER ADVANCING 1 LINE
           ADD 1 TO WS-INDEX 
           END-PERFORM.
           MOVE LASTNAME TO WS-NAME
           MOVE SPACING TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE 
           PERFORM 0280-PRINT-SUBTOTAL
           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE 
	         READ PETSALESFILE
			     AT END SET ENDOFSALESFILE TO TRUE
			     END-READ.
			   
       0200-END.

       0280-PRINT-SUBTOTAL.
           MOVE WS-NAME TO DET-NAME.
           MOVE WS-SUB-QTY TO DET-SUB-QTY.
           MOVE WS-SUB-SUBTOTAL TO DET-SUBTOTAL.

           MOVE DETAIL-SUBTOTAL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
	   
       0290-PRINT-TOTAL. 			
		     
		     MOVE WS-TOTAL-QUANT TO DET-TOTAL-QUANT.
		     MOVE WS-TOTAL-SALE TO DET-TOT-SALES.
		 
           MOVE DETAIL-TOTAL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
		   		   
	    0290-END.
		
       0300-STOP-RUN.
	        CLOSE PETSALESFILE PETSALEREPORT.
           STOP RUN.
          END PROGRAM PETSTORECHALLENGE.
