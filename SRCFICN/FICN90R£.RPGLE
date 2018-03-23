000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400011031      *  Nome programma:  ficn90R
000500000000      *  Descrizione   :  Riorganizzazione tariffe padroncini
000600000000      *                   Programma di esecuzione
000700000000      *
000800000000      *****************************************************************
000900011031     Ffifgt00F  UF   E             DISK
001000011031     Ffifgt01L  IF   E           K DISK
001100011031     F                                     RENAME(fifgt000:fifgtL)
001200011031     F                                     INFDS($fifgt)
001300011031     Ffifpt01L  UF   E           K DISK
001400011031     Ffifpd01L  UF   E           K DISK
001500020528     Ffifgts0F  o    E             DISK
001600020729     FTABEL00F  IF   E           K DISK
001700000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800000000     F                                     USROPN
001900000000      *
002000000000     D TE              S             71    DIM(1) CTDATA PERRCD(1)              RICHE DI STAMP
002100000000      *
002200000000      * Numero relativo di record per il file testate tariffe
002300011031     D $fifgt          DS
002400000000     D  $FGTR                397    400B 0
002500000000     D PARAM           DS
002600011031     D  cn90D1                 1      8  0
002700011031     D  cn90D2                 9     16  0
002800011031     D  cn90ST                17     17
002900000000     D WLBDAT          DS
003000020729     D  G02DAT                 1      8  0
003100020729     D  G02INV                 9     16  0
003200020729     D  G02ERR                17     17
003300020729     D  G02TGI                18     22  0
003400020729     D WGIDAT          DS
003500020729     D  GIODAT                 1      8  0
003600020729     D  GIOINV                 9     16  0
003700020729     D  GIOTGI                17     21  0
003800020729      *
003900020729     D DS5A          E DS
004000000000      * Predispongo una DS che conterra' la chiave del record
004100000000     D                 DS
004200000000     D  FGTPDR                 1      7  0
004300000000     D  FGTSML                 8      8
004400000000     D  FGTTSR                 9      9
004500940926     D  FGTCTR                10     12  0
004600940926     D  KEYFGT                 1     12
004700940926     D*
004800940926     D                 DS
004900940926     D  AA                     1      4  0
005000940926     D  MM                     5      6  0
005100940926     D  GG                     7      8  0
005200940926     D  DATA                   1      8  0
005300940926     D*
005400000000     D KPJBA         E DS
005500000000     D CNCR80        E DS
005600000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700000000      *****************************************************************
005800000000      * RIEPILOGO INDICATORI
005900000000      *****************************************************************
006000000000      * 40    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
006100000000      *         IL RECORD E' ALLOCATO (CIOE' *IN40 ACCESO) SALTO LA
006200000000      *         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
006300000000      * 30/34 - DI COMODO
006400000000      *****************************************************************
006500000000     C     *ENTRY        PLIST
006600000000     C                   PARM                    KPJBA
006700020729     C*                  MOVEL     KPJBU         PARAM
006800000000     C                   Z-ADD     1             CODUT
006900000000     C                   CALL      'X§PARUT'                            99
007000000000     C                   PARM                    UT§DSE
007100000000     C                   MOVEL     RAGUT         RSUT             20
007200000000     C                   MOVEL     REC80         CNCR80
007300020729     c                   exsr      limiti
007400000000      *
007500011031     C     fifgdK        KLIST                                                  fifgt01L
007600000000     C                   KFLD                    FGTPDR                         CD.PADRONCINO
007700000000     C                   KFLD                    FGTSML                         SIMULAZIONE
007800000000     C                   KFLD                    FGTTSR                         TIPO PRESTAZION
007900000000     C                   KFLD                    FGTCTR                         CD.TARIFFA
008000000000     C                   KFLD                    FGTPRG                         PROGR.TARIFFA
008100000000      *
008200000000      *****************************************************************
008300000000      *               M A I N     L I N E
008400000000      *****************************************************************
008500000000      *
008600011031     C     cn90ST        IFEQ      'S'
008700000000     C                   OPEN      QSYSPRT
008800000000     C                   EXCEPT    TESTA                                        TESTATA
008900000000     C                   ENDIF
009000000000     C                   SETOFF                                       40        NESSUNO
009100000000      *
009200940926     C                   MOVE      *BLANKS       $FGTPR           12            PRECEDENTE
009300000000      *
009400000000      * MI POSIZIONO A FINE FILE, QUINDI LEGGO ALL'INDIETRO
009500011031     C     *HIVAL        SETLL     fifgt01L
009600000000     C                   DO        *HIVAL
009700011031     C                   READP     fifgt01L                               30
009800000000      *
009900000000      * CONTROLLO SE E' L'ULTIMO PROGRESSIVO
010000011128     C*mN30KEYFGT        IFEQ      $FGTPR                                       =AL PRECEDENTE
010100000000      *
010200000000      * CONTROLLO SE DTA SCADENZA E' NEL LIMITE
010300011031     C     FGTDST        IFLE      cn90D1                                       SCADUTA
010400000000     C     FGTATB        ANDEQ     *BLANK
010500000000      *
010600011031     C     $FGTR         CHAIN     fifgt00F                           3140
010700000000      *
010800000000      * 40 ON - RECORD ALLOCATO PER CUI SALTO LA TARIFFA
010900000000     C  N31
011000000000     CANN40              DO
011100000000      *
011200000000      * ANNULLO DETTAGLIO E TESTATA TARIFFA PARTICOLARE
011300000000     C                   EXSR      ANNULL
011400000000      *
011500000000      * ANNULLO TESTATA TARIFFA
011600000000     C                   MOVE      'A'           FGTATB                         'A'=ANNULLATO
011700000000     C*                    MOVE *BLANKS   FGTFTR           FLAG TRASMISS.
011800000000     C*                    Z-ADD0         FGTDTR           NR.TRASMISSIONE
011900020528     c                   write     fifgts00
012000000000     C                   EXCEPT    TESTAR
012100000000      *
012200000000      * STAMPO PROGRESSIVO
012300011031     C     cn90ST        IFEQ      'S'
012400000000     C                   EXSR      STAMPA
012500000000     C                   ENDIF
012600000000     C                   SETON                                        40        ALMENO UNO
012700000000      *
012800000000     C                   ENDDO
012900000000     C                   ENDIF
013000000000      *
013100011128     C*m                 ELSE
013200000000      *
013300000000      * Solo se non e' annullato
013400000000     C     FGTATB        IFEQ      *BLANK
013500000000     C                   MOVE      KEYFGT        $FGTPR                         PRECEDENTE
013600000000     C                   ENDIF
013700000000      *
013800011128     C*m                 ENDIF
013900000000     C  N30              ENDDO
014000000000      *
014100011031     C     cn90ST        IFEQ      'S'
014200000000     C  N40              EXCEPT    NOELEM                                       NESSUNO
014300000000     C                   EXCEPT    FINEST                                       FINE STAMPA
014400000000     C                   ENDIF
014500000000     C                   SETON                                        LR
014600000000      *****************************************************************
014700000000      *    ANNULLO TESTATA E DETTAGLIO TARIFFA PARTICOLARE
014800000000      *****************************************************************
014900000000     C     ANNULL        BEGSR
015000000000      *
015100000000      * ANNULLO DETTAGLIO
015200011031     C     fifgdK        SETLL     fifpd01L
015300011031     C     fifgdK        READE     fifpd01L                               33
015400000000     C     *IN33         DOWEQ     '0'
015500000000     C                   MOVE      'A'           FPDATB                         'A'=ANNULLATO
015600000000     C*                    MOVE *BLANK    FPDFTR           FLAG TRASMISS.
015700000000     C*                    Z-ADD*ZERO     FPDDTR           NR.TRASMISSIONE
015800000000     C                   EXCEPT    DETPAR
015900000000      *
016000011031     C     fifgdK        READE     fifpd01L                               33
016100000000     C                   END
016200000000      *
016300000000      * ANNULLO TESTATA
016400011031     C     fifgdK        SETLL     fifpt01L
016500011031     C     fifgdK        READE     fifpt01L                               34
016600000000     C     *IN34         DOWEQ     '0'
016700000000     C                   MOVE      'A'           FPTATB                         'A'=ANNULLATO
016800000000     C*                    MOVE *BLANKS   TPTFTR           FLAG TRASMISS.
016900000000     C*                    Z-ADD0         FPTDTR           NR.TRASMISSIONE
017000000000     C                   EXCEPT    TESPAR
017100000000      *
017200011031     C     fifgdK        READE     fifpt01L                               34
017300000000     C                   END
017400000000      *
017500000000     C                   ENDSR
017600000000      *****************************************************************
017700000000      *    STAMPA PROGRESSIVO ANNULLATO
017800000000      *****************************************************************
017900000000     C     STAMPA        BEGSR
018000000000      *
018100000000      * GIRO DATA DECORRENZA
018200940926     C                   Z-ADD     FGTDDT        DATA
018300940926     C                   Z-ADD     MM            GGMM              4 0
018400940926     C                   MOVEL     GG            GGMM
018500940926     C                   Z-ADD     AA            STADDT            8 0
018600940926     C                   MOVEL     GGMM          STADDT
018700000000      *
018800000000      * GIRO DATA SCADENZA
018900940926     C                   Z-ADD     FGTDST        DATA
019000940926     C                   Z-ADD     MM            GGMM
019100940926     C                   MOVEL     GG            GGMM
019200940926     C                   Z-ADD     AA            STADST            8 0
019300940926     C                   MOVEL     GGMM          STADST
019400940926     C*
019500000000      * STAMPO DETTAGLIO
019600000000     C                   EXCEPT    DET
019700000000      *
019800000000     C                   ENDSR
019900020729      *-------------------------------------------------
020000020729     C     limiti        begsr
020100020729      *-------------------------------------------------
020200020729     C     KTAB          KLIST
020300020729     C                   KFLD                    CODUT
020400020729     C                   KFLD                    COD               2
020500020729     C                   KFLD                    KEY               8
020600020729     C* LA DATA DEL GIORNO LA PRENDO DA TIME
020700020729     C                   TIME                    W0120            14 0
020800020729     C                   MOVE      W0120         WDAT              8 0
020900020729     C*
021000020729     c                   clear                   wlbdat
021100020729     C                   Z-ADD     WDAT          G02DAT
021200020729     C                   MOVEL     *BLANK        G02ERR
021300020729     C                   CALL      'XSRDA8'
021400020729     C                   PARM                    WLBDAT
021500020729      * DATE PULIZIE: REPERISCO GIORNI DA DS5A
021600020729     C                   MOVEL     '5A'          COD
021700020729     C                   MOVEL     '1       '    KEY
021800020729     C     KTAB          CHAIN     TABEL                              43
021900020729     C  N43              MOVEL     TBLUNI        DS5A
022000020729     C   43              MOVEL     *ZEROS        DS5A
022100020729      *
022200020729      * SOTTRAGGO §5AFGT AI GIORNI
022300020729     C     G02TGI        SUB       §5AFGT        GIOTGI
022400020729      *
022500020729     C                   CALL      'XSRGI8'
022600020729     C                   PARM                    WGIDAT
022700020729     C                   Z-ADD     GIOinv        cn90d1
022800020729     C                   Z-ADD     GIOdat        cn90d2
022900020729     c                   move      'S'           cn90st
023000020729     C                   endsr
023100000000      *****************************************************************
023200011031     Ofifgt000  E            TESTAR
023300000000     O                       FGTATB
023400011031     Ofifpt000  E            TESPAR
023500000000     O                       FPTATB
023600011031     Ofifpd000  E            DETPAR
023700000000     O                       FPDATB
023800000000      *****************************************************************
023900000000     OQSYSPRT   E            TESTA            02
024000000000     O         OR    OF
024100000000     O                       RSUT                20
024200000000     O                       TE(1)              101
024300011031     O                       cn90D2              89 '  /  /    '
024400000000     O          E            TESTA            04 05
024500000000     O         OR    OF
024600000000     O                                           17 'CODICE PADRONCINO'
024700000000     O                                           30 'SIMULAZIONE'
024800000000     O                                           43 'PRESTAZIONE'
024900000000     O                                           65 'CD.TARIFFA'
025000000000     O                                           78 'PROGRESSIVO'
025100000000     O                                           90 'DECORRENZA'
025200000000     O                                          100 'SCADENZA'
025300000000     O          EF           DET            1
025400000000     O                       FGTPDR              12
025500000000     O                       FGTSML              23
025600000000     O                       FGTTSR              38
025700000000     O                       FGTCTR              62
025800000000     O                       FGTPRG        Z     73
025900940926     O                       STADDT              89 '  /  /    '
026000940926     O                       STADST             100 '  /  /    '
026100000000     O          E            NOELEM      1
026200940926     O                                           57 '( Nessun elemento annull'
026300940926     O                                         +  0 'ato )'
026400000000     O          E            FINEST      3
026500000000     O                                           59 '***   FINE  STAMPA  ***'
026600940926**
026700940926 **  PROGRESSIVI TARIFFA ANNULLATI FINO ALLA DATA  XX/XX/XXXX  **
