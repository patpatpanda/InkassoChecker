       IDENTIFICATION DIVISION.
       PROGRAM-ID. The_Heart IS RECURSIVE.


       ENVIRONMENT DIVISION.
       FILE-CONTROL.
             
       CONFIGURATION SECTION.
            

       DATA DIVISION.
       FILE SECTION.
           

       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       COPY "P_W255.CPY".
       COPY "P_W666.CPY".

      
       01 WS-LOGTEXT PIC X(100).

      
       01 WS-TIMESTAMP PIC X(20).
       01 WS-ÅR PIC X(4).
       01 WS-MÅNAD PIC X(2).
       01 WS-DAG PIC X(2).

       01 ws-count PIC 9(4) VALUE 0.
          
       PROCEDURE DIVISION.

       A-MAIN SECTION.
          
           MOVE "Försöker INSERTA: " TO WS-LOGTEXT
          
           DISPLAY WS-LOGTEXT

           PERFORM B-CONNECT-TO-DB
           PERFORM CHECK-OBETALDA-FAKTUROR
           PERFORM REMOVE-BETALDA
           PERFORM FLAGGA-FOR-INKASSO
           PERFORM BYGG-DATUM-PAMINNELSE
           DISPLAY "Inkasso batch klar!"
           EXEC SQL
       COMMIT
           END-EXEC

           GOBACK.

       B-CONNECT-TO-DB SECTION.

           EXEC SQL
               CONNECT TO 'redwarriordb'
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY "DB CONNECTION FAILED. SQLCODE = " SQLCODE
               STOP RUN
           END-IF.
       CHECK-OBETALDA-FAKTUROR SECTION.

           EXEC SQL
               DECLARE OBETALDA_CURSOR CURSOR WITH HOLD FOR
               SELECT
                   upgnr,
                   kundnr,
                   lopnr,
                   faktdat,
                   forfdat,
                   attbet,
                   belopp_bet,
                   betaldat
               FROM REDWARRIOR.dbo.faktura
               WHERE forfdat < CAST(GETDATE() AS DATE)
                 AND belopp_bet = CAST(0.00 AS DECIMAL(11,2))
           END-EXEC.

           EXEC SQL
               OPEN OBETALDA_CURSOR
           END-EXEC.

           PERFORM UNTIL SQLCODE = 100

               EXEC SQL
                   FETCH OBETALDA_CURSOR INTO
                       :faktura-upgnr,
                       :faktura-kundnr,
                       :faktura-lopnr,
                       :faktura-faktdat,
                       :faktura-forfdat,
                       :faktura-attbet,
                       :faktura-belopp-bet,
                       :faktura-betaldat :faktura-betaldat-null
               END-EXEC

               IF SQLCODE = 0

                   
                   EXEC SQL
                       SELECT COUNT(*) INTO :ws-count
                       FROM REDWARRIOR.dbo.paminnelser
                       WHERE lopnr = :faktura-lopnr
                   END-EXEC

                   IF ws-count = 0

                       PERFORM BYGG-DATUM-PAMINNELSE

                       MOVE faktura-upgnr TO paminnelser-upgnr
                       MOVE faktura-kundnr TO paminnelser-kundnr
                       MOVE faktura-lopnr TO paminnelser-lopnr

                       MOVE "NEJ" TO paminnelser-inkasso-status
                       DISPLAY "Försöker INSERTA:"
                       DISPLAY "UPGNR: " paminnelser-upgnr
                       DISPLAY "KUNDNR: " paminnelser-kundnr
                       DISPLAY "LOPNR: " paminnelser-lopnr
                       DISPLAY "DATUM: " paminnelser-paminnelse-datum
                       DISPLAY "FORFALLO: " paminnelser-forfallo-datum
                       DISPLAY "STATUS: " paminnelser-inkasso-status

                       EXEC SQL
                           INSERT INTO REDWARRIOR.dbo.paminnelser
           (upgnr, kundnr, lopnr, paminnelse_datum, forfallo_datum,
                  inkasso_status
           )                    VALUES
           (:paminnelser-upgnr, :paminnelser-kundnr, :paminnelser-lopnr,
             :paminnelser-paminnelse-datum, :paminnelser-forfallo-datum,
                            :paminnelser-inkasso-status)
                       END-EXEC

                       IF SQLCODE = 0
                           DISPLAY "Ny påminnelse skapad för LOPNR: "
                             faktura-lopnr
                       ELSE
                           DISPLAY "FEL VID INSERT: " SQLCODE
                       END-IF
                   END-IF

               ELSE
                   IF SQLCODE NOT = 100
                       DISPLAY "SQL FEL VID FETCH: " SQLCODE
                   END-IF
               END-IF

           END-PERFORM.

           EXEC SQL
               CLOSE OBETALDA_CURSOR
           END-EXEC.

       BYGG-DATUM-PAMINNELSE SECTION.

           *> Hämta dagens datum i ISO-format: YYYYMMDDhhmmss...
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP

           *> Plocka ut år, månad och dag från strängen
           MOVE WS-TIMESTAMP(1:4) TO WS-ÅR
           MOVE WS-TIMESTAMP(5:2) TO WS-MÅNAD
           MOVE WS-TIMESTAMP(7:2) TO WS-DAG

           *> Bygg ett datumsträng: 'YYYY-MM-DD'
           STRING
             WS-ÅR "-" WS-MÅNAD "-" WS-DAG
             DELIMITED BY SIZE
             INTO paminnelser-paminnelse-datum
           END-STRING

           *> Sätt förfallodatum till 10 dagar från idag
           EXEC SQL
              SELECT CONVERT(CHAR(10), DATEADD(DAY, 10, GETDATE()), 120)
               INTO :paminnelser-forfallo-datum
           END-EXEC.

    REMOVE-BETALDA SECTION.

    EXEC SQL
        DELETE FROM paminnelser
        WHERE EXISTS (
            SELECT 1
            FROM faktura
            WHERE faktura.lopnr = paminnelser.lopnr
              AND belopp_bet > 0
        )
    END-EXEC

           END-EXEC

           IF SQLCODE = 0
               DISPLAY "Betalda fakturor borttagna från paminnelser."
           ELSE
               IF SQLCODE = 100
                   DISPLAY
                     "Inga betalda fakturor hittades i paminnelser."
               ELSE
                   DISPLAY "FEL I REMOVE-BETALDA. SQLCODE = " SQLCODE
               END-IF
           END-IF.

       FLAGGA-FOR-INKASSO SECTION.

           EXEC SQL
               UPDATE REDWARRIOR.dbo.paminnelser
               SET inkasso_status = 'JA'
               WHERE forfallo_datum < CAST(GETDATE() AS DATE)
                 AND inkasso_status = 'NEJ'
           END-EXEC

           IF SQLCODE = 0
               DISPLAY "Skickar påminnelse till kund: " faktura-kundnr
              

           ELSE
               IF SQLCODE = 100
                   DISPLAY "Inga påminnelser att flagga för inkasso."
               ELSE
                   DISPLAY "FEL VID FLAGGA-FOR-INKASSO. SQLCODE = "
                     SQLCODE
               END-IF
           END-IF.

