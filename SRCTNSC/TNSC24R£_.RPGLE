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
002800020506     F*ABEL00F  IF   E           K DISK    USROPN
002900931015?     *---------
003000941104     FTNSC24D   CF   E             WORKSTN
003100931015?     *--------------------------------------------------------------*
003200931015     D FIL             S              3  0 DIM(30)
003300020506     D*FIL1            S              3    DIM(30)
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
004400020729     D OG143         E DS
004500931015     D CNCR80        E DS
004600020506     d trul06ds      e ds
004700020506     d  lin                    1     90  0 dim(30)
004800931015      *
004900941104     D WLBDA8          DS
005000941104     D  G02DAT                 1      8  0
005100941104     D  G02INV                 9     16  0
005200941104     D  G02ERR                17     17
005300941104     D  G02TGI                18     22  0
005400931015     D                SDS
005500931015     D  NOMPG                  1     10
005600980112?     *--------------------------------------------------------------*
005700980112?     *  Ridefinizione interna campi azorg                           *
005800980112?     *--------------------------------------------------------------*
005900980112     IAZORG01L  AA
006000980112     I                             P    4    5 0ORGFIL
006100980112     I                                 11   11  ORGFL1
006200020729     I                               3776 3800  ORGDE3
006300931015      *---------------------------------------------------------------*
006400931015      *   C I C L O      P R I N C I P A L E                          *
006500931015      *---------------------------------------------------------------*
006600931015     C*  INIZIALIZZAZIONE VIDEATA
006700931015     C                   EXSR      INZVID
006800931015     C*
006900931015     C*  LOOP
007000931015     C     £FINE         DOUEQ     'S'
007100931015     C*
007200931015     C     TIPVID        CASEQ     '1'           GESVD1
007300931015     C                   END
007400931015     C*
007500931015     C                   END
007600931015     C*
007700931015     C                   SETON                                        LR
007800931015      *---------------------------------------------------------------*
007900931015      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
008000931015      *---------------------------------------------------------------*
008100931015     C     INZVID        BEGSR
008200931015     C*
008300931015     C                   MOVE      *BLANKS       VSEL1
008400931015     C                   MOVE      *BLANKS       VSEL2
008500931015     C                   MOVE      *BLANKS       VORD
008600931015     C                   MOVE      'SI'          VDET
008700931015     C                   Z-ADD     0             VDASPE
008800931015     C                   Z-ADD     0             VASPE
008900931015     C                   Z-ADD     0             VCDI
009000931015     C                   Z-ADD     0             VLIN
009100931015     C                   MOVE      *BLANKS       VRAG
009200961216     C                   MOVEL     'I'           V1CTPI
009300010108     C                   MOVE      *BLANKS       VPARZ
009301070702     C                   MOVE      'N'           Vxca
009400931015     C*
009500931015     C                   ENDSR
009600931015      *---------------------------------------------------------------*
009700931015      *  GESVD1: GESTIONE PRIMA VIDEATA RICHIESTA PARAMETRI           *
009800931015      *---------------------------------------------------------------*
009900931015     C     GESVD1        BEGSR
010000931015     C*
010100941104     C                   EXFMT     SC24D01
010200961216     C                   SETOFF                                       404142
010300961216     C                   SETOFF                                       434445
010400961216     C                   SETOFF                                       464748
010500961216     C                   SETOFF                                       28
010600931015     C*  Fine elaborazione
010700931015     C     *IN03         IFEQ      '1'
010800931015     C                   MOVEL     'S'           £FINE
010900931015     C                   GOTO      FINVD1
011000931015     C                   END
011100931015     C*  Controlli
011200931015     C                   EXSR      CNTVD1
011300961216     C   28              GOTO      FINVD1
011400931015     C*  Sottomissione Batch
011500931015     C     *IN06         IFEQ      '1'
011600931015     C                   EXSR      SBMP25
011700931018     C                   EXSR      INZVID
011800931015     C                   END
011900931015     C*
012000931015     C     FINVD1        ENDSR
012100931015      *---------------------------------------------------------------*
012200931015      *  CNTVD1: CONTROLLI PRIMA VIDEATA                              *
012300931015      *---------------------------------------------------------------*
012400931015     C     CNTVD1        BEGSR
012500931015     C*----------------------------------------------------*
012600931015     C*   Effettuare una sola selezione
012700931018     C     VSEL1         IFNE      ' '
012800931018     C     VSEL2         ANDNE     ' '
012900961216     C                   SETON                                        284041
013000961216     C                   MOVEL     ERR(2)        $MSG
013100931015     C                   GOTO      FINCT1
013200931015     C                   END
013300931015     C*
013400931018     C     VSEL1         IFEQ      ' '
013500931018     C     VSEL2         ANDEQ     ' '
013600961216     C                   SETON                                        284041
013700961216     C                   MOVEL     ERR(1)        $MSG
013800931015     C                   GOTO      FINCT1
013900931015     C                   END
014000980112     C*----------------------------------------------------*
014100980112     C*  Controllo richiesta dati x una sola linea
014200980112     C     VLIN          IFNE      0
014300020506     C                   Z-ADD     1             Y                 3 0
014400980112     C     VLIN          LOOKUP    FIL(Y)                                 32
014500980112     C     *IN32         IFEQ      '0'
014600980112     C                   SETON                                        2848
014700980112     C                   MOVEL     ERR(9)        $MSG
014800980112     C                   GOTO      FINCT1
014900980112     C                   END
015000980112     C                   END
015100931015     C*----------------------------------------------------*
015200961216     C*   Se tipo stampa x spedizioni estero obbligatorio
015300961216     C*   indicare stampa scoperti in arrivo ordinati per
015400961216     C*   lna
015500961216     C     V1CTPI        IFEQ      'E'
015600961216     C     VSEL2         IFEQ      ' '
015700961216     C                   SETON                                        284143
015800961216     C                   MOVEL     ERR(3)        $MSG
015900961216     C                   GOTO      FINCT1
016000961216     C                   END
016100961216     C     VORD          IFEQ      'P'
016200961216     C                   SETON                                        284243
016300961216     C                   MOVEL     ERR(4)        $MSG
016400961216     C                   GOTO      FINCT1
016500961216     C                   END
016600980112     C*  se estero filiale arrivo no Estero do errore
016700980112     C     FL1(Y)        IFNE      'E'
016800961217     C     VLIN          ANDGT     0
016900961216     C                   SETON                                        284348
017000961217     C                   MOVEL     ERR(11)       $MSG
017100961216     C                   GOTO      FINCT1
017200961216     C                   END
017300961216     C                   ELSE
017400980112     C*  se italia filiale arrivo si Estero do errore
017500980112     C     FL1(Y)        IFEQ      'E'
017600980128     C     VLIN          ANDGT     0
017700961216     C                   SETON                                        284348
017800961216     C                   MOVEL     ERR(10)       $MSG
017900961216     C                   GOTO      FINCT1
018000961216     C                   END
018100961216     C                   END
018200961216     C*----------------------------------------------------*
018300931015     C*   Controllo le date di spedizione
018400931015     C     VDASPE        IFNE      0
018500931015     C                   MOVE      VDASPE        G02DAT
018600931015     C                   MOVEL     *BLANK        G02ERR
018700941104     C                   CALL      'XSRDA8'
018800941104     C                   PARM                    WLBDA8
018900931015     C     G02ERR        IFEQ      '1'
019000961216     C                   SETON                                        4428
019100961216     C                   MOVEL     ERR(5)        $MSG
019200931015     C                   GOTO      FINCT1
019300931015     C                   ELSE
019400931015     C                   Z-ADD     G02INV        SDDAT
019500941104     C                   Z-ADD     G02DAT        VDASPE
019600931015     C                   END
019700931015     C                   END
019800931015     C*---
019900931015     C     VDASPE        IFNE      0
020000931015     C     VASPE         ANDEQ     0
020100931015     C                   MOVE      VDASPE        VASPE
020200931015     C                   END
020300931015     C*---
020400931015     C     VASPE         IFNE      0
020500931015     C                   MOVE      VASPE         G02DAT
020600931015     C                   MOVEL     *BLANK        G02ERR
020700941104     C                   CALL      'XSRDA8'
020800941104     C                   PARM                    WLBDA8
020900931015     C     G02ERR        IFEQ      '1'
021000961216     C                   SETON                                        4528
021100961216     C                   MOVEL     ERR(5)        $MSG
021200931015     C                   GOTO      FINCT1
021300931015     C                   ELSE
021400931015     C                   Z-ADD     G02INV        SADAT
021500941104     C                   Z-ADD     G02DAT        VASPE
021600931015     C                   END
021700931015     C                   ELSE
021800961022     C                   Z-ADD     99999999      SADAT
021900931015     C                   END
022000931015     C*---
022100931015     C     SDDAT         IFGT      SADAT
022200961216     C                   SETON                                        444528
022300961216     C                   MOVEL     ERR(6)        $MSG
022400961216     C                   GOTO      FINCT1
022500931015     C                   END
022600931015     C*----------------------------------------------------*
022700931015     C*  Controllo parzializzazioni su cliente
022800931015     C     VCDI          IFNE      0
022900941104     C     VSEL2         ANDEQ     '1'
023000961216     C                   SETON                                        414628
023100961216     C                   MOVEL     ERR(7)        $MSG
023200931015     C                   GOTO      FINCT1
023300931015     C                   END
023400931015     C*---
023500931015     C     VRAG          IFNE      *BLANKS
023600941104     C     VSEL2         ANDEQ     '1'
023700961216     C                   SETON                                        414728
023800961216     C                   MOVEL     ERR(7)        $MSG
023900931015     C                   GOTO      FINCT1
024000931015     C                   END
024100931015     C*---
024200931015     C     VCDI          IFNE      0
024300931015     C     VRAG          ANDNE     *BLANKS
024400961216     C                   SETON                                        464728
024500961216     C                   MOVEL     ERR(8)        $MSG
024600931015     C                   GOTO      FINCT1
024700931015     C                   END
024800931015     C*---
024900931015     C     FINCT1        ENDSR
025000931015      *---------------------------------------------------------------*
025100931015      *  SBMP25: SOTTOMISSIONE AZIONE BATCH DI STAMPA                 *
025200931015      *---------------------------------------------------------------*
025300931015     C     SBMP25        BEGSR
025400931015     C*----------------------------------------------------*
025500941104     C                   MOVE      VSEL1         D24SCP
025600941104     C                   MOVE      VSEL2         D24SCA
025700941104     C                   MOVE      VORD          D24ORD
025800941104     C                   MOVE      VDET          D24DET
025801070629     C                   MOVE      Vxca          D24xca
025900941104     C                   MOVE      SDDAT         D24SDD
026000941104     C                   MOVE      SADAT         D24SDA
026100941104     C                   MOVE      VCDI          D24CDI
026200941104     C                   MOVE      VRAG          D24RAG
026300941104     C                   MOVE      VLIN          D24LIN
026400961216     C                   MOVE      V1CTPI        D24TPI
026500010108     C                   MOVE      VPARZ         D24PAR
026600931015     C*---
026700941104     C                   MOVE      'SC25'        KCOAZ
026800941104     C                   MOVEL     TNSC24        KPJBU
026900931015     C                   CALL      'BCH10'
027000931015     C                   PARM                    KPJBA
027100931015     C*---
027200931015     C                   ENDSR
027300931015      *---------------------------------------------------------------*
027400941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
027500931015      *---------------------------------------------------------------*
027600941104     C     *INZSR        BEGSR
027700931015      *---------------------------------------------------------------*
027800931015     C     *ENTRY        PLIST
027900931015     C                   PARM                    KPJBA
028000931015      *
028100931015     C                   Z-ADD     1             CODUT
028200931015     C                   CALL      'X§PARUT'
028300931015     C                   PARM                    UT§DSE
028400931015     C                   MOVEL     RAGUT         RSUT             20
028500931015     C                   MOVEL     REC80         CNCR80
028600931015     C                   MOVEL     KNMUS         §SEI              6
028700931015     C*----------------------------------------------------*
028800931015     C*  Chiave x reperimento dati '£3'                    *
028900020506     C*    KEYTAB        KLIST
029000020506     C*                  KFLD                    CODUT
029100020506     C*                  KFLD                    TBLCOD
029200020506     C*                  KFLD                    TBLKEY
029300931015     C*----------------------------------------------------*
029400931015     C*  Reperimanto codici filiali in gestione sull'AS
029500961216     C                   SETOFF                                       05
029600020506     C*                  OPEN      TABEL00F
029700980112     C                   OPEN      AZORG01L
029800020506     c                   clear                   trul06ds
029900020506     c                   eval      d06cod = '£1'
030000020506     c                   movel     simfel        d06key
030100020506     c                   movel     trul06ds      kpjbu
030200020506     c                   call      'TRUL06R'
030300020506     c                   parm                    kpjba
030400020506     c                   movel     kpjbu         trul06ds
030500020506     c                   movea     lin           fil
030600020506     C*                  MOVEL     '£3'          TBLCOD
030700020506     C*                  MOVEL     SIMFEL        TBLKEY
030800020506     C*    KEYTAB        CHAIN     TABEL                              21
030900020506     C*    *IN21         IFEQ      '0'
031000020506     C*                  MOVEA     TBLUNI        FIL1
031100020506     C                   Z-ADD     1             Y
031200020506     C                   DO        30            X                 2 0
031300020506     C     FIL(X)        IFGT      *ZEROS
031400020506     C*                  ADD       1             Y
031500020506     C*                  MOVE      FIL1(X)       FIL(Y)
031600980112     C* Controllo se filiale Italia o filiale estera
031700020506     C                   Z-ADD     FIL(X)        ORGFIL
031800020506     C                   MOVEL     *BLANKS       ORGFL1
031900020506     C     ORGFIL        CHAIN     AZORG01L                           22
032000020729     C                   clear                   OG143
032100020729     C  n22              movel     ORGDE3        OG143
032200020729     C                   movel     *blank        flgest            1
032300020729     C     §OgNTW        ifeq      'FED'
032400020729     C     §OgNTW        oreq      'EEX'
032500020729     C     §OgNTW        oreq      'EUP'
032600020729     C                   MOVEL     'E'           flgest
032700020729     c                   end
032800020729     C****               MOVE      ORGFL1        FL1(x)
032900020729     C                   MOVE      flgest        FL1(x)
033000020506     C     FL1(x)        IFEQ      'E'
033100980112     C                   SETON                                        05
033200980112     C                   END
033300931015     C                   END
033400931015     C                   END
033500020506     C*                  ELSE
033600020506     C*                  MOVE      SIMFEL        FIL(1)
033700020506     C*                  END
033800020506     C*                  CLOSE     TABEL00F
033900980112     C                   CLOSE     AZORG01L
034000931015     C*----------------------------------------------------*
034100931015     C*  Definizione variabili
034200941104     C     *LIKE         DEFINE    D24SDD        SDDAT
034300941104     C     *LIKE         DEFINE    D24SDA        SADAT
034400931015     C*----------------------------------------------------*
034500931015     C                   MOVEL     'N'           £FINE             1
034600931015     C                   MOVEL     '1'           TIPVID            1
034700931018     C                   MOVEL     NOMPG         NOMPGM
034800931015     C*----------------------------------------------------*
034900931015     C                   ENDSR
035000980112**
035100961216Effettuare una scelta                                                  01
035200961216Impossibile scegliere entrambe                                         02
035300961216Se selez. stampa per estero obbligatoria stampa scoperti in arrivo     03
035400961216Se selez. stampa per estero obbligatoria ordinamento per P.O. arrivo   04
035500961216Data errata                                                            05
035600961216Data iniziale maggiore della data finale                               06
035700961216Parzializzazione non compatibile con tipo stampa                       07
035800961216Impossibile selezionare sia codice che ragione sociale                 08
035900961216Linea non in gestione al punto operativo                               09
036000980112Se indicata linea di arrivo NO ITALIA obbligatoria stampa estero       10
036100980112Se indicata linea di arrivo ITALIA impossibile selez. stampa estero    11
