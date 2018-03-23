000100931015     H DECEDIT('0,') DATEDIT(*DMY.)
000200931015?     *--------------------------------------------------------------*
000300941104      * TNSC24R                                                      *
000400931015      *       *-----------------------------------------*            *
000500931015      *        STAMPE CONTRASSEGNI: RICHIESTA PARAMETRI              *
000600931015      *       *-----------------------------------------*            *
000700931015      *                                                              *
000800931015?     *--------------------------------------------------------------*
000900931015      * 22 -DATA ERRATA (DATA DI SPEDIZIONE DA)                      *
001000931015      * 23 -DATE INCONGRUENTI  (DATA DI SPEDIZIONE DA .... A ...)    *
001100931015      * 24 -DATA ERRATA (DATA DI SPEDIZIONE A)                       *
001200931015      * 25 -PARZIALIZZAZIONE NON COMPATIBILE COL TIPO DI STAMPA      *
001300931015      *     (PARZIALIZZAZIONE X CODICE E STAMPA SCOPERTI IN ARRIVO)  *
001400931015      * 26 -IMPOSSIBILE SELEZIONARE SIA CODICE CHE RAGIONE SOCIALE   *
001500931015      * 27 -PARZIALIZZAZIONE NON COMPATIBILE COL TIPO DI STAMPA      *
001600931015      *     (PARZIALIZZAZIONE X RAG.SOCIALE E STP SCOPERTI IN ARRIVO)*
001700931015      * 28 -LINEA NON IN GESTIONE ALLA FILIALE                       *
001800931015      * 29 -IMPOSSIBILE SELEZIONARE ENTRAMBE LE STAMPE               *
001900931015      * 30 -EFFETTUARE UNA SCELTA                                    *
002000931015      * 99 -ERRORE GENERICO                                          *
002100931015?     *--------------------------------------------------------------*
002200931015      * 03 -FINE LAVORO                                              *
002300931015      * 06 -CONFERMA                                                 *
002400931015?     *--------------------------------------------------------------*
002500980112     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002600980112     F                                     USROPN
002700980112?    F*---------
002800931015     FTABEL00F  IF   E           K DISK    USROPN
002900931015?     *---------
003000941104     FTNSC24D   CF   E             WORKSTN
003100931015?     *--------------------------------------------------------------*
003200931015     D FIL             S              3  0 DIM(30)
003300931015     D FIL1            S              3    DIM(30)
003400980112     D FL1             S              1    DIM(30)
003500961216     D ERR             S             70    DIM(11) CTDATA PERRCD(1)
003600931015?     *--------------------------------------------------------------*
003700931015?     *  DS                                                          *
003800931015?     *--------------------------------------------------------------*
003900931015     D KPJBA         E DS
004000931015      *
004100941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
004200931015      *
004300931015     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400931015     D CNCR80        E DS
004500931015      *
004600941104     D WLBDA8          DS
004700941104     D  G02DAT                 1      8  0
004800941104     D  G02INV                 9     16  0
004900941104     D  G02ERR                17     17
005000941104     D  G02TGI                18     22  0
005100931015     D                SDS
005200931015     D  NOMPG                  1     10
005300980112?     *--------------------------------------------------------------*
005400980112?     *  Ridefinizione interna campi azorg                           *
005500980112?     *--------------------------------------------------------------*
005600980112     IAZORG01L  AA
005700980112     I                             P    4    5 0ORGFIL
005800980112     I                                 11   11  ORGFL1
005900931015      *---------------------------------------------------------------*
006000931015      *   C I C L O      P R I N C I P A L E                          *
006100931015      *---------------------------------------------------------------*
006200931015     C*  INIZIALIZZAZIONE VIDEATA
006300931015     C                   EXSR      INZVID
006400931015     C*
006500931015     C*  LOOP
006600931015     C     £FINE         DOUEQ     'S'
006700931015     C*
006800931015     C     TIPVID        CASEQ     '1'           GESVD1
006900931015     C                   END
007000931015     C*
007100931015     C                   END
007200931015     C*
007300931015     C                   SETON                                        LR
007400931015      *---------------------------------------------------------------*
007500931015      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
007600931015      *---------------------------------------------------------------*
007700931015     C     INZVID        BEGSR
007800931015     C*
007900931015     C                   MOVE      *BLANKS       VSEL1
008000931015     C                   MOVE      *BLANKS       VSEL2
008100931015     C                   MOVE      *BLANKS       VORD
008200931015     C                   MOVE      'SI'          VDET
008300931015     C                   Z-ADD     0             VDASPE
008400931015     C                   Z-ADD     0             VASPE
008500931015     C                   Z-ADD     0             VCDI
008600931015     C                   Z-ADD     0             VLIN
008700931015     C                   MOVE      *BLANKS       VRAG
008800961216     C                   MOVEL     'I'           V1CTPI
008900010108     C                   MOVE      *BLANKS       VPARZ
009000931015     C*
009100931015     C                   ENDSR
009200931015      *---------------------------------------------------------------*
009300931015      *  GESVD1: GESTIONE PRIMA VIDEATA RICHIESTA PARAMETRI           *
009400931015      *---------------------------------------------------------------*
009500931015     C     GESVD1        BEGSR
009600931015     C*
009700941104     C                   EXFMT     SC24D01
009800961216     C                   SETOFF                                       404142
009900961216     C                   SETOFF                                       434445
010000961216     C                   SETOFF                                       464748
010100961216     C                   SETOFF                                       28
010200931015     C*  Fine elaborazione
010300931015     C     *IN03         IFEQ      '1'
010400931015     C                   MOVEL     'S'           £FINE
010500931015     C                   GOTO      FINVD1
010600931015     C                   END
010700931015     C*  Controlli
010800931015     C                   EXSR      CNTVD1
010900961216     C   28              GOTO      FINVD1
011000931015     C*  Sottomissione Batch
011100931015     C     *IN06         IFEQ      '1'
011200931015     C                   EXSR      SBMP25
011300931018     C                   EXSR      INZVID
011400931015     C                   END
011500931015     C*
011600931015     C     FINVD1        ENDSR
011700931015      *---------------------------------------------------------------*
011800931015      *  CNTVD1: CONTROLLI PRIMA VIDEATA                              *
011900931015      *---------------------------------------------------------------*
012000931015     C     CNTVD1        BEGSR
012100931015     C*----------------------------------------------------*
012200931015     C*   Effettuare una sola selezione
012300931018     C     VSEL1         IFNE      ' '
012400931018     C     VSEL2         ANDNE     ' '
012500961216     C                   SETON                                        284041
012600961216     C                   MOVEL     ERR(2)        $MSG
012700931015     C                   GOTO      FINCT1
012800931015     C                   END
012900931015     C*
013000931018     C     VSEL1         IFEQ      ' '
013100931018     C     VSEL2         ANDEQ     ' '
013200961216     C                   SETON                                        284041
013300961216     C                   MOVEL     ERR(1)        $MSG
013400931015     C                   GOTO      FINCT1
013500931015     C                   END
013600980112     C*----------------------------------------------------*
013700980112     C*  Controllo richiesta dati x una sola linea
013800980112     C     VLIN          IFNE      0
013900980112     C                   Z-ADD     1             Y
014000980112     C     VLIN          LOOKUP    FIL(Y)                                 32
014100980112     C     *IN32         IFEQ      '0'
014200980112     C                   SETON                                        2848
014300980112     C                   MOVEL     ERR(9)        $MSG
014400980112     C                   GOTO      FINCT1
014500980112     C                   END
014600980112     C                   END
014700931015     C*----------------------------------------------------*
014800961216     C*   Se tipo stampa x spedizioni estero obbligatorio
014900961216     C*   indicare stampa scoperti in arrivo ordinati per
015000961216     C*   lna
015100961216     C     V1CTPI        IFEQ      'E'
015200961216     C     VSEL2         IFEQ      ' '
015300961216     C                   SETON                                        284143
015400961216     C                   MOVEL     ERR(3)        $MSG
015500961216     C                   GOTO      FINCT1
015600961216     C                   END
015700961216     C     VORD          IFEQ      'P'
015800961216     C                   SETON                                        284243
015900961216     C                   MOVEL     ERR(4)        $MSG
016000961216     C                   GOTO      FINCT1
016100961216     C                   END
016200980112     C*  se estero filiale arrivo no Estero do errore
016300980112     C     FL1(Y)        IFNE      'E'
016400961217     C     VLIN          ANDGT     0
016500961216     C                   SETON                                        284348
016600961217     C                   MOVEL     ERR(11)       $MSG
016700961216     C                   GOTO      FINCT1
016800961216     C                   END
016900961216     C                   ELSE
017000980112     C*  se italia filiale arrivo si Estero do errore
017100980112     C     FL1(Y)        IFEQ      'E'
017200980128     C     VLIN          ANDGT     0
017300961216     C                   SETON                                        284348
017400961216     C                   MOVEL     ERR(10)       $MSG
017500961216     C                   GOTO      FINCT1
017600961216     C                   END
017700961216     C                   END
017800961216     C*----------------------------------------------------*
017900931015     C*   Controllo le date di spedizione
018000931015     C     VDASPE        IFNE      0
018100931015     C                   MOVE      VDASPE        G02DAT
018200931015     C                   MOVEL     *BLANK        G02ERR
018300941104     C                   CALL      'XSRDA8'
018400941104     C                   PARM                    WLBDA8
018500931015     C     G02ERR        IFEQ      '1'
018600961216     C                   SETON                                        4428
018700961216     C                   MOVEL     ERR(5)        $MSG
018800931015     C                   GOTO      FINCT1
018900931015     C                   ELSE
019000931015     C                   Z-ADD     G02INV        SDDAT
019100941104     C                   Z-ADD     G02DAT        VDASPE
019200931015     C                   END
019300931015     C                   END
019400931015     C*---
019500931015     C     VDASPE        IFNE      0
019600931015     C     VASPE         ANDEQ     0
019700931015     C                   MOVE      VDASPE        VASPE
019800931015     C                   END
019900931015     C*---
020000931015     C     VASPE         IFNE      0
020100931015     C                   MOVE      VASPE         G02DAT
020200931015     C                   MOVEL     *BLANK        G02ERR
020300941104     C                   CALL      'XSRDA8'
020400941104     C                   PARM                    WLBDA8
020500931015     C     G02ERR        IFEQ      '1'
020600961216     C                   SETON                                        4528
020700961216     C                   MOVEL     ERR(5)        $MSG
020800931015     C                   GOTO      FINCT1
020900931015     C                   ELSE
021000931015     C                   Z-ADD     G02INV        SADAT
021100941104     C                   Z-ADD     G02DAT        VASPE
021200931015     C                   END
021300931015     C                   ELSE
021400961022     C                   Z-ADD     99999999      SADAT
021500931015     C                   END
021600931015     C*---
021700931015     C     SDDAT         IFGT      SADAT
021800961216     C                   SETON                                        444528
021900961216     C                   MOVEL     ERR(6)        $MSG
022000961216     C                   GOTO      FINCT1
022100931015     C                   END
022200931015     C*----------------------------------------------------*
022300931015     C*  Controllo parzializzazioni su cliente
022400931015     C     VCDI          IFNE      0
022500941104     C     VSEL2         ANDEQ     '1'
022600961216     C                   SETON                                        414628
022700961216     C                   MOVEL     ERR(7)        $MSG
022800931015     C                   GOTO      FINCT1
022900931015     C                   END
023000931015     C*---
023100931015     C     VRAG          IFNE      *BLANKS
023200941104     C     VSEL2         ANDEQ     '1'
023300961216     C                   SETON                                        414728
023400961216     C                   MOVEL     ERR(7)        $MSG
023500931015     C                   GOTO      FINCT1
023600931015     C                   END
023700931015     C*---
023800931015     C     VCDI          IFNE      0
023900931015     C     VRAG          ANDNE     *BLANKS
024000961216     C                   SETON                                        464728
024100961216     C                   MOVEL     ERR(8)        $MSG
024200931015     C                   GOTO      FINCT1
024300931015     C                   END
024400931015     C*---
024500931015     C     FINCT1        ENDSR
024600931015      *---------------------------------------------------------------*
024700931015      *  SBMP25: SOTTOMISSIONE AZIONE BATCH DI STAMPA                 *
024800931015      *---------------------------------------------------------------*
024900931015     C     SBMP25        BEGSR
025000931015     C*----------------------------------------------------*
025100941104     C                   MOVE      VSEL1         D24SCP
025200941104     C                   MOVE      VSEL2         D24SCA
025300941104     C                   MOVE      VORD          D24ORD
025400941104     C                   MOVE      VDET          D24DET
025500941104     C                   MOVE      SDDAT         D24SDD
025600941104     C                   MOVE      SADAT         D24SDA
025700941104     C                   MOVE      VCDI          D24CDI
025800941104     C                   MOVE      VRAG          D24RAG
025900941104     C                   MOVE      VLIN          D24LIN
026000961216     C                   MOVE      V1CTPI        D24TPI
026100010108     C                   MOVE      VPARZ         D24PAR
026200931015     C*---
026300941104     C                   MOVE      'SC25'        KCOAZ
026400941104     C                   MOVEL     TNSC24        KPJBU
026500931015     C                   CALL      'BCH10'
026600931015     C                   PARM                    KPJBA
026700931015     C*---
026800931015     C                   ENDSR
026900931015      *---------------------------------------------------------------*
027000941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
027100931015      *---------------------------------------------------------------*
027200941104     C     *INZSR        BEGSR
027300931015      *---------------------------------------------------------------*
027400931015     C     *ENTRY        PLIST
027500931015     C                   PARM                    KPJBA
027600931015      *
027700931015     C                   Z-ADD     1             CODUT
027800931015     C                   CALL      'X§PARUT'
027900931015     C                   PARM                    UT§DSE
028000931015     C                   MOVEL     RAGUT         RSUT             20
028100931015     C                   MOVEL     REC80         CNCR80
028200931015     C                   MOVEL     KNMUS         §SEI              6
028300931015     C*----------------------------------------------------*
028400931015     C*  Chiave x reperimento dati '£3'                    *
028500931015     C     KEYTAB        KLIST
028600931015     C                   KFLD                    CODUT
028700931015     C                   KFLD                    TBLCOD
028800931015     C                   KFLD                    TBLKEY
028900931015     C*----------------------------------------------------*
029000931015     C*  Reperimanto codici filiali in gestione sull'AS
029100961216     C                   SETOFF                                       05
029200931015     C                   OPEN      TABEL00F
029300980112     C                   OPEN      AZORG01L
029400931019     C                   MOVEL     '£3'          TBLCOD
029500931015     C                   MOVEL     SIMFEL        TBLKEY
029600931015     C     KEYTAB        CHAIN     TABEL                              21
029700931015     C     *IN21         IFEQ      '0'
029800931015     C                   MOVEA     TBLUNI        FIL1
029900931015     C                   Z-ADD     0             Y                 2 0
030000931015     C                   DO        30            X                 2 0
030100931015     C     FIL1(X)       IFGT      *ZEROS
030200931015     C                   ADD       1             Y
030300931015     C                   MOVE      FIL1(X)       FIL(Y)
030400980112     C* Controllo se filiale Italia o filiale estera
030500980112     C                   Z-ADD     FIL(Y)        ORGFIL
030600980112     C                   MOVEL     *BLANKS       ORGFL1
030700980112     C     ORGFIL        CHAIN     AZORG01L                           22
030800980112     C                   MOVE      ORGFL1        FL1(Y)
030900980112     C     FL1(Y)        IFEQ      'E'
031000980112     C                   SETON                                        05
031100980112     C                   END
031200931015     C                   END
031300931015     C                   END
031400931015     C                   ELSE
031500931015     C                   MOVE      SIMFEL        FIL(1)
031600931015     C                   END
031700931015     C                   CLOSE     TABEL00F
031800980112     C                   CLOSE     AZORG01L
031900931015     C*----------------------------------------------------*
032000931015     C*  Definizione variabili
032100941104     C     *LIKE         DEFINE    D24SDD        SDDAT
032200941104     C     *LIKE         DEFINE    D24SDA        SADAT
032300931015     C*----------------------------------------------------*
032400931015     C                   MOVEL     'N'           £FINE             1
032500931015     C                   MOVEL     '1'           TIPVID            1
032600931018     C                   MOVEL     NOMPG         NOMPGM
032700931015     C*----------------------------------------------------*
032800931015     C                   ENDSR
032900980112**
033000961216Effettuare una scelta                                                  01
033100961216Impossibile scegliere entrambe                                         02
033200961216Se selez. stampa per estero obbligatoria stampa scoperti in arrivo     03
033300961216Se selez. stampa per estero obbligatoria ordinamento per P.O. arrivo   04
033400961216Data errata                                                            05
033500961216Data iniziale maggiore della data finale                               06
033600961216Parzializzazione non compatibile con tipo stampa                       07
033700961216Impossibile selezionare sia codice che ragione sociale                 08
033800961216Linea non in gestione al punto operativo                               09
033900980112Se indicata linea di arrivo NO ITALIA obbligatoria stampa estero       10
034000980112Se indicata linea di arrivo ITALIA impossibile selez. stampa estero    11
