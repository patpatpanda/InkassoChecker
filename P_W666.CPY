      *> -------------------------------------------
      *> DECLARE TABLE for paminnelser
      *> -------------------------------------------
           EXEC SQL DECLARE paminnelser TABLE 
           ( paminnelse_id        int
           , upgnr                decimal(7)   NOT NULL
           , kundnr               decimal(10)  NOT NULL
           , lopnr                decimal(10)  NOT NULL
           , paminnelse_datum     date         NOT NULL
           , forfallo_datum       date         NOT NULL
           , inkasso_status       varchar(10)  NOT NULL
           ) END-EXEC.
      *> -------------------------------------------
      *> COBOL HOST VARIABLES FOR TABLE paminnelser
      *> -------------------------------------------
       01  DCLpaminnelser.
           03 paminnelser-paminnelse-id       PIC S9(09)  COMP-5.
           03 paminnelser-upgnr               PIC S9(7)  COMP-3.
           03 paminnelser-kundnr              PIC S9(10)  COMP-3.
           03 paminnelser-lopnr               PIC S9(10)  COMP-3.
           03 paminnelser-paminnelse-datum    PIC X(10).
           03 paminnelser-forfallo-datum      PIC X(10).
           03 paminnelser-inkasso-status      PIC X(10).
