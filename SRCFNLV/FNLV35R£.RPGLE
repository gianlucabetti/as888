000100900914     H DECEDIT('0,') DATEDIT(*DMY.)
000200010115      * ------------------------------------------------------------------
000300010115      * ---------  STAMPA  RIEPILOGO  GIACENZE  --------------------------
000400010409      * --------- elabora i tipi record "T" e "Z" ------------------------
000500010115      * ------------------------------------------------------------------
000600010115     FFIAVE00F  IF   E             DISK    RENAME(AV00BOLL:FIAVE000)
000700900810     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800010115      *-------------------------------
000900900810     D KPJBA         E DS
001000020925     D ut§dse0f      E DS
001100020925     D CNCR80        E DS
001200010115     D RCDAVE_T        DS           400
001300030324     D  Articolo               2     10
001400010115     D  QtaInizio             11     30  3
001500010115     D  QtaCarico             31     50  3
001600010115     D  QtaScarico            51     70  3
001700010115     D  QtaFinale             71     90  3
001800010409     D RCDAVE_Z        DS           400
001900010409     D  Riga_Z                 2    133
002000010115      * -----------------------------------
002100010115     D  SQtaInizio     S             10  3
002200010115     D  SQtaCarico     S             10  3
002300010115     D  SQtaScarico    S             10  3
002400010115     D  SQtaFinale     S             10  3
002500010115      * ------------------------------------------------------------------
002600900810     C     *ENTRY        PLIST
002700900810     C                   PARM                    KPJBA
002800020925      *
002900020925     C                   Z-ADD     1             CODUT
003000020925     C                   CALL      'X§PARUT'
003100020925     C                   PARM                    ut§dse0f
003200020925     C                   MOVEL     REC80         CNCR80
003300020925      *
003400020925      * ESEGUO PROGRAMMA SOLO PER P.O =007
003500020925     C                   IF        SIMFEL = 007
003600010115      *
003700010115     c                   eval      *inOF = *on
003800010115      *
003900010115     C                   READ      FIAVE000
004000010115      *
004100010115     C                   DOW       NOT %EOF
004200010115      *
004300010409     c                   SELECT
004400010409      *
004500010409     C                   WHEN      %SUBST(AV00BOLL:1:1) = 'T'
004600010115     c                   movel     AV00BOLL      RCDAVE_T
004700010115     C                   z-add     QtaInizio     SQtaInizio
004800010115     C                   z-add     QtaCarico     SQtaCarico
004900010115     C                   z-add     QtaScarico    SQtaScarico
005000010115     C                   z-add     QtaFinale     SQtaFinale
005100010115     C   OF              except    TESTA
005200010115     C   OF              eval      *inOF = *OFF
005300010115     C                   except    DETTA
005400010409      *
005500010409     C                   WHEN      %SUBST(AV00BOLL:1:1) = 'Z'
005600010409     c                   movel     AV00BOLL      RCDAVE_Z
005700030324     c                   IF        RIGA_Z <> *blanks
005800010418     C   OF              except    TESTA
005900010418     C   OF              eval      *inOF = *OFF
006000010409     C                   except    DETTAz
006100030324     c                   ENDIF
006200010409      *
006300010409     c                   ENDSL
006400010115      *
006500010115     C                   READ      FIAVE000
006600010115      *
006700010115     C                   ENDDO
006800010115      *
006900020925     C                   ENDIF
007000010115     C                   EVAL      *inlr = *on
007100010115      * ------------------------------------------------------------------
007200900810     OQSYSPRT   E            TESTA          2 01
007300010115     O                                           75 'RIEPILOGO AVERNA - DEPOSIT-
007400010115     O                                              O DI BARI'
007500030324     O                       UDATE         Y    120
007600030324     O                                          128 'Pag.'
007700030324     O                       PAGE1         z    132
007800010115     O          E            TESTA          2
007900010115     O                                           12 'ARTICOLO'
008000010115     O                                           34 'Quantita Iniziale'
008100010115     O                                           52 'Quantita Carico'
008200010115     O                                           73 'Quantita Scarico'
008300010125     O                                           92 'Quantita Finale'
008400010418      *
008500010115     O          E            DETTA          1
008600010115     O                       Articolo            12
008700010115     O                       sQtaInizio    2     34
008800010115     O                       sQtaCarico    2     52
008900010115     O                       sQtaScarico   2     73
009000010125     O                       sQtaFinale    2     92
009100010418      *
009200030324     O          E            DETTAZ      1  1
009300010409     O                       Riga_z             132
