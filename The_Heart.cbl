       IDENTIFICATION DIVISION.
       PROGRAM-ID. INKASSO-BATCH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       COPY "P_W255.CPY".
       COPY "P_W666.CPY".

       01 WS-LOGTEXT                     PIC X(100).
       01 WS-TIMESTAMP                   PIC X(20).
       01 WS-ÅR                          PIC X(4).
       01 WS-MÅNAD                       PIC X(2).
       01 WS-DAG                         PIC X(2).
       01 WS-COUNT                       PIC 9(4) VALUE 0.
       01 WS-ANTAL-INSATTA               PIC 9(5) VALUE 0.
       01 WS-ANTAL-BORTTAGNA            PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

       A-MAIN SECTION.
           DISPLAY "====== STARTAR INKASSO-BATCH ======"

           PERFORM B-CONNECT-TO-DB
           PERFORM CHECK-OBETALDA-FAKTUROR
           PERFORM REMOVE-BETALDA
           PERFORM FLAGGA-FOR-INKASSO
           DISPLAY "====== INKASSO-BATCH KLAR ======"
           DISPLAY "Totalt insatta påminnelser: " WS-ANTAL-INSATTA
           DISPLAY "Totalt borttagna påminnelser: " WS-ANTAL-BORTTAGNA

           EXEC SQL COMMIT END-EXEC

           GOBACK.

       B-CONNECT-TO-DB SECTION.
           DISPLAY "== Ansluter till databas =="

           EXEC SQL
               CONNECT TO 'redwarriordb'
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY "🚨 DB CONNECTION FAILED. SQLCODE = " SQLCODE
               STOP RUN
           END-IF.

       CHECK-OBETALDA-FAKTUROR SECTION.
           DISPLAY "== Hämtar obetalda fakturor =="

           EXEC SQL
               DECLARE OBETALDA_CURSOR CURSOR WITH HOLD FOR
               SELECT upgnr, kundnr, lopnr, faktdat, forfdat, attbet,
                      belopp_bet, betaldat
               FROM REDWARRIOR.dbo.faktura
               WHERE forfdat < CAST(GETDATE() AS DATE)
                 AND belopp_bet = 0.00
           END-EXEC.

           EXEC SQL OPEN OBETALDA_CURSOR END-EXEC.

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH OBETALDA_CURSOR INTO
                       :faktura-upgnr, :faktura-kundnr, :faktura-lopnr,
                       :faktura-faktdat, :faktura-forfdat,
                       :faktura-attbet, :faktura-belopp-bet,
                       :faktura-betaldat :faktura-betaldat-null
               END-EXEC

               IF SQLCODE = 0
                   EXEC SQL
                       SELECT COUNT(*) INTO :WS-COUNT
                       FROM REDWARRIOR.dbo.paminnelser
                       WHERE lopnr = :faktura-lopnr
                   END-EXEC

                   IF WS-COUNT = 0
                       PERFORM BYGG-DATUM-PAMINNELSE

                       MOVE faktura-upgnr TO paminnelser-upgnr
                       MOVE faktura-kundnr TO paminnelser-kundnr
                       MOVE faktura-lopnr TO paminnelser-lopnr
                       MOVE "NEJ" TO paminnelser-inkasso-status

                       DISPLAY "-- Skapar ny påminnelse --"
                       DISPLAY "LOPNR: " paminnelser-lopnr

                       EXEC SQL
                           INSERT INTO REDWARRIOR.dbo.paminnelser
                           (upgnr, kundnr, lopnr, paminnelse_datum,
                            forfallo_datum, inkasso_status)
                           VALUES
                           (:paminnelser-upgnr, :paminnelser-kundnr,
                            :paminnelser-lopnr,
                            :paminnelser-paminnelse-datum,
                            :paminnelser-forfallo-datum,
                            :paminnelser-inkasso-status)
                       END-EXEC

                       IF SQLCODE = 0
                           ADD 1 TO WS-ANTAL-INSATTA
                           DISPLAY "✅ Påminnelse skapad för LOPNR: "
                               faktura-lopnr
                       ELSE
                           DISPLAY "🚨 FEL VID INSERT. SQLCODE: " SQLCODE
                       END-IF
                   END-IF
               ELSE
                   IF SQLCODE NOT = 100
                       DISPLAY "🚨 SQL FEL VID FETCH: " SQLCODE
                   END-IF
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE OBETALDA_CURSOR END-EXEC.

       BYGG-DATUM-PAMINNELSE SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP(1:4) TO WS-ÅR
           MOVE WS-TIMESTAMP(5:2) TO WS-MÅNAD
           MOVE WS-TIMESTAMP(7:2) TO WS-DAG

           STRING
               WS-ÅR "-" WS-MÅNAD "-" WS-DAG
               DELIMITED BY SIZE
               INTO paminnelser-paminnelse-datum
           END-STRING

           EXEC SQL
               SELECT CONVERT(CHAR(10), DATEADD(DAY, 10, GETDATE()), 120)
               INTO :paminnelser-forfallo-datum
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY "🚨 FEL VID DATUMBERÄKNING. SQLCODE: " SQLCODE
           END-IF.

       REMOVE-BETALDA SECTION.
           DISPLAY "== Rensar betalda fakturor från paminnelser =="

           EXEC SQL
               DELETE FROM paminnelser
               WHERE EXISTS (
                   SELECT 1
                   FROM faktura
                   WHERE faktura.lopnr = paminnelser.lopnr
                     AND belopp_bet > 0
               )
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-ANTAL-BORTTAGNA
               DISPLAY "🗑️  Betalda fakturor borttagna."
           ELSE
               IF SQLCODE = 100
                   DISPLAY "🔍 Inga betalda fakturor hittades."
               ELSE
                   DISPLAY "🚨 FEL I REMOVE-BETALDA. SQLCODE = " SQLCODE
               END-IF
           END-IF.

      FLAGGA-FOR-INKASSO SECTION.
    DISPLAY "== Flaggar gamla påminnelser för inkasso =="

    EXEC SQL
        UPDATE REDWARRIOR.dbo.paminnelser
        SET inkasso_status = 'JA'
        WHERE forfallo_datum <= DATEADD(DAY, -10, CAST(GETDATE() AS DATE))
          AND inkasso_status = 'NEJ'
    END-EXEC

    EVALUATE SQLCODE
        WHEN 0
            DISPLAY "✅ Påminnelser flaggade för inkasso."
        WHEN 100
            DISPLAY "🔍 Inga påminnelser att flagga."
        WHEN OTHER
            DISPLAY "🚨 FEL VID FLAGGA-FOR-INKASSO. SQLCODE = " SQLCODE
    END-EVALUATE.

