000100900914     H DECEDIT('0,') DATEDIT(*DMY.)
000200010115      * ------------------------------------------------------------------
000300010115      * ---------  LISTA PRELIEVI  PER  LINEA  DI  ARRIVO  ---------------
000400010115      * ------------------------------------------------------------------
000500010115     FFIAVP00F  IF   E           K DISK
000600900912     FAZORG01L  IF   E           K DISK
000700900912     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800010115      * ------------------------------------------------------------------
000900010115     d RSUT            S             20
001000010115     d DesLna          S                   like(ORGDES)
001100900912     D KPJBA         E DS
001200010115     D ut§dse0f      E DS
001300020925     D CNCR80        E DS
001400010115      * ------------------------------------------------------------------
001500900912     C     *ENTRY        PLIST
001600900912     C                   PARM                    KPJBA
001700010115      *
001800900912     C                   Z-ADD     1             CODUT
001900900912     C                   CALL      'X§PARUT'
002000010115     C                   PARM                    ut§dse0f
002100020925     C                   MOVEL     REC80         CNCR80
002200010115      * ------------------------------------------------
002300010115     C                   CLEAR                   SAVLNA            3 0
002400010115     C                   CLEAR                   SAVCAR            9
002500010115     C                   clear                   SAVDAR           40
002600010115      *
002700020925     C                   IF        SIMFEL = 007
002800010115     C     *loval        setll     FIAVP000
002900010115     C                   READ      FIAVP000
003000010115     C                   DOW       NOT %EOF
003100010115      *
003200010115      * LA PRIMA VOLTA STAMPO TESTATA
003300010115     C                   if        *in99 = *off
003400900912     C                   EXCEPT    TESTA
003500900912     C                   Z-ADD     AVPLNA        SAVLNA
003600010115     C                   movel     AVPCAR        SAVCAR
003700900912     C                   MOVEL     AVPDAR        SAVDAR
003800010115     C                   eval      *in99 = *on
003900900912     C                   END
004000010115      *
004100010115      * CAMBIO LINEA ARRIVO: DECODIFICO                                ---
004200010115     C                   if        SAVLNA <> AVPLNA
004300010115      *
004400010115     C     SAVLNA        CHAIN     AZORG
004500010115     c                   if        %found
004600010115     C                   eval      DESLNA = ORGDES
004700010115     c                   else
004800010115     C                   clear                   DESLNA
004900010115     c                   endif
005000010115      *
005100010115      *     STAMPA ULTIMO DETTAGLIO ARTICOLI
005200900912     C                   EXCEPT    DET
005300010115      *
005400010115      *     STAMPA TOTALI
005500900912     C                   EXCEPT    TOT
005600010115      *     SALVO NUOVA LINEA
005700900912     C                   Z-ADD     AVPLNA        SAVLNA
005800010115      *     RISTAMPO NUOVA TESTATA
005900900912     C                   EXCEPT    TESTA
006000010115      *
006100900912     C                   ELSE
006200010115      *
006300010115      * ALTRIMENTI CONTROLLO SE E' CAMBIATO L'ARTICOLO
006400010115     C                   if        SAVCAR <> AVPCAR
006500010115      *     STAMPA ULTIMO DETTAGLIO ARTICOLI
006600900912     C                   EXCEPT    DET
006700010115     C                   movel     AVPCAR        SAVCAR
006800900914     C                   MOVEL     AVPDAR        SAVDAR
006900010115     c                   endif
007000010115     c                   endif
007100010115      * CODICE ARTICOLO
007200010115     C     AVPCAR        COMP      *blanks                                66
007300010115     C   66              ADD       AVPPKG        GPESO             7 2
007400900912     C                   ADD       AVPQTA        TQTA              7 0
007500900912     C                   ADD       AVPNCL        TCOLLI            5 0
007600900912     C                   ADD       AVPNCL        GCOLLI            5 0
007700010115      *
007800010115     C                   READ      FIAVP000
007900010115     c                   ENDDO
008000010115      *
008100010115      * STAMPO SOLO SE IL FILE NON ERA VUOTO
008200010115     C                   if        SAVLNA <> *zeros
008300010115      * DECODIFICA ULTIMA LINEA
008400010115     C     SAVLNA        CHAIN     AZORG
008500010115     c                   if        %found
008600010115     C                   eval      DESLNA = ORGDES
008700010115     c                   else
008800010115     C                   clear                   DESLNA
008900010115     c                   endif
009000010115      * STAMPA ULTIMO DETTAGLIO ARTICOLI
009100900912     C                   EXCEPT    DET
009200010115      * STAMPA ULTIMO TOTALE
009300900912     C                   EXCEPT    TOT
009400010115     C                   endif
009500010115      *
009600020925     C                   ENDIF
009700010115     C                   eval      *inlr =*on
009800900912     C* ------------------------------------------------------------------
009900900912     OQSYSPRT   E            TESTA          2 01
010000900912     O                       RSUT                20
010100010115     O                                           49 '** PRELIEVO ARTICOLI AVERN-
010200010115     O                                              A **  '
010300900912     O                       UDATE         Y     80
010400900912     O          E            TESTA          2
010500900912     O                                           13 'ARTICOLO'
010600010115     O                                           66 'QUANTITA'
010700010115     O                                           73 'COLLI'
010800010115     O                                           83 'PESO'
010900900912     O          E      N66   DET            1
011000010115     O                       SAVCAR              13
011100010115     O                       SAVDAR           +   2
011200010115     O                       TQTA          2B +   2
011300010115     O                       TCOLLI        2B +   1
011400900912     O          E            TOT         1
011500010115     O                                              'T O T A L I  LINEA'
011600010115     O                       SAVLNA           +   1
011700010115     O                       DESLNA           +   2
011800010115     O                       GCOLLI        2B    73
011900010115     O                       GPESO         2B    83
012000900912     O          E            TOT         0  1
012100010116     O                                              'T O T A L I  LINEA'
012200010115     O                       SAVLNA           +   1
012300010115     O                       DESLNA           +   2
012400010115     O                       GCOLLI        2B    73
012500010115     O                       GPESO         2B    83
