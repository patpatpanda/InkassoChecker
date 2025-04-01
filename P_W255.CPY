      *> -------------------------------------------
      *> DECLARE TABLE for faktura
      *> -------------------------------------------
           EXEC SQL DECLARE faktura TABLE 
           ( upgnr                decimal(7)   NOT NULL
           , kundnr               decimal(10)  NOT NULL
           , lopnr                decimal(10)  NOT NULL
           , faktdat              date         NOT NULL
           , forfdat              date         NOT NULL
           , attbet               decimal(11,2) NOT NULL
           , belopp_bet           decimal(11,2) NOT NULL
           , betaldat             date
           ) END-EXEC.
      *> -------------------------------------------
      *> COBOL HOST VARIABLES FOR TABLE faktura
      *> -------------------------------------------
       01  DCLfaktura.
           03 faktura-upgnr                   PIC S9(7)  COMP-3.
           03 faktura-kundnr                  PIC S9(10)  COMP-3.
           03 faktura-lopnr                   PIC S9(10)  COMP-3.
           03 faktura-faktdat                 PIC X(10).
           03 faktura-forfdat                 PIC X(10).
           03 faktura-attbet                  PIC S9(9)V9(2)  COMP-3.
           03 faktura-belopp-bet              PIC S9(9)V9(2)  COMP-3.
           03 faktura-betaldat                PIC X(10).
      *> -------------------------------------------
      *> COBOL INDICATOR VARIABLES FOR TABLE faktura
      *> -------------------------------------------
       01  DCLfaktura-NULL.
           03 faktura-betaldat-NULL           PIC S9(04)  COMP-5.
