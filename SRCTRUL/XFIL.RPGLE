000100000000      ****************************************************************
000200930204      *   RITORNO IL CODICE DELLA FILIALE IN CUI SONO                *
000300020730      ****************************************************************
000400000000      *                                                              *
000500020730      *   OUTPUT: - CODICE FILIALE                                   *
000600020730      *           - TIPO FILIALE ( S=SEDE , 1=1°LIV , 2=2° LIV )     *
000700020730      *           - RAGIONE SOCIALE AZIENDA                          *
000800020730      *           - CODICE UTENTE                                    *
000900930204      *                                                              *
001000930204      ****************************************************************
001100930204     D DSFIL         E DS
001200930204     D CNCR80        E DS
001300930204     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400930204      ****************************************************************
001500020730      * PARAMETRI IN INPUT ALLA ROUTINE
001600000000     C     *ENTRY        PLIST
001700930204     C                   PARM                    DSFIL
001800020730      *
001900020730      * RECUPERO IL CODICE DELL'AZIENDA
002000930204     C                   Z-ADD     1             CODUT
002100930204     C                   CALL      'X§PARUT'
002200930204     C                   PARM                    UT§DSE
002300020730      *
002400930204     C                   MOVEL     RAGUT         DSFIRS
002500930204     C                   Z-ADD     CODUT         DSFICD
002600930204     C                   MOVEL     REC80         CNCR80
002700020730      *
002800020513     C                   MOVEL     SIMTPP        DSFITI
002900020730      * SE TPP = BLANKS SIGNIFICA CHE NON E' STATO TROVATO IL TER-PART
003000020730      * DEL P.O. UTENTE --> LO CONSIDERO UN SECONDO LIVELLO
003100020513     C     SIMTPP        IFEQ      *BLANKS
003200020513     C                   MOVEL     '2'           DSFITI
003300020513     C                   ENDIF
003400020730      * CONTROLLO SE SONO IN SEDE. In questo caso forzo tipo p.o. = "S"
003500020730     C                   SELECT
003600020730     C                   WHEN      SIMFEL = *ZEROS                              --- 1 -->
003700020730     C                   eval      DSFITI = 'S'
003800020730     C                   clear                   DSFIFI
003900020730     C                   WHEN      DSFITI = '2'                                 --- 1 -->
004000020513     C                   Z-ADD     SIMPOU        DSFIFI
004100020730     c                   OTHER
004200930204     C                   Z-ADD     SIMFEL        DSFIFI
004300020730     C                   ENDSL                                                  <-- 1 ---
004400020730      *
004500000000     C                   RETURN
