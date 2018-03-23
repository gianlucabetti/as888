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
003200080813     D FIL             S              3  0 DIM(60)
003300020506     D*FIL1            S              3    DIM(30)
003400080813     D FL1             S              1    DIM(60)
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
004600080813     D TRUL72DS      E DS
004700080813     D  Lin                           3s 0 DESCEND
004800080813     D                                     DIM(60)
004900080813     D                                     OVERLAY(O72Lia)
005000931015      *
005100941104     D WLBDA8          DS
005200941104     D  G02DAT                 1      8  0
005300941104     D  G02INV                 9     16  0
005400941104     D  G02ERR                17     17
005500941104     D  G02TGI                18     22  0
005600931015     D                SDS
005700931015     D  NOMPG                  1     10
005800980112?     *--------------------------------------------------------------*
005900980112?     *  Ridefinizione interna campi azorg                           *
006000980112?     *--------------------------------------------------------------*
006100980112     IAZORG01L  AA
006200980112     I                             P    4    5 0ORGFIL
006300980112     I                                 11   11  ORGFL1
006400020729     I                               3776 3800  ORGDE3
006500931015      *---------------------------------------------------------------*
006600931015      *   C I C L O      P R I N C I P A L E                          *
006700931015      *---------------------------------------------------------------*
006800931015     C*  INIZIALIZZAZIONE VIDEATA
006900931015     C                   EXSR      INZVID
007000931015     C*
007100931015     C*  LOOP
007200931015     C     £FINE         DOUEQ     'S'
007300931015     C*
007400931015     C     TIPVID        CASEQ     '1'           GESVD1
007500931015     C                   END
007600931015     C*
007700931015     C                   END
007800931015     C*
007900931015     C                   SETON                                        LR
008000931015      *---------------------------------------------------------------*
008100931015      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
008200931015      *---------------------------------------------------------------*
008300931015     C     INZVID        BEGSR
008400931015     C*
008500931015     C                   MOVE      *BLANKS       VSEL1
008600931015     C                   MOVE      *BLANKS       VSEL2
008700931015     C                   MOVE      *BLANKS       VORD
008800931015     C                   MOVE      'SI'          VDET
008900931015     C                   Z-ADD     0             VDASPE
009000931015     C                   Z-ADD     0             VASPE
009100931015     C                   Z-ADD     0             VCDI
009200931015     C                   Z-ADD     0             VLIN
009300931015     C                   MOVE      *BLANKS       VRAG
009400961216     C                   MOVEL     'I'           V1CTPI
009500010108     C                   MOVE      *BLANKS       VPARZ
009600070702     C                   MOVE      'N'           Vxca
009700931015     C*
009800931015     C                   ENDSR
009900931015      *---------------------------------------------------------------*
010000931015      *  GESVD1: GESTIONE PRIMA VIDEATA RICHIESTA PARAMETRI           *
010100931015      *---------------------------------------------------------------*
010200931015     C     GESVD1        BEGSR
010300931015     C*
010400941104     C                   EXFMT     SC24D01
010500961216     C                   SETOFF                                       404142
010600961216     C                   SETOFF                                       434445
010700961216     C                   SETOFF                                       464748
010800961216     C                   SETOFF                                       28
010900931015     C*  Fine elaborazione
011000931015     C     *IN03         IFEQ      '1'
011100931015     C                   MOVEL     'S'           £FINE
011200931015     C                   GOTO      FINVD1
011300931015     C                   END
011400931015     C*  Controlli
011500931015     C                   EXSR      CNTVD1
011600961216     C   28              GOTO      FINVD1
011700931015     C*  Sottomissione Batch
011800931015     C     *IN06         IFEQ      '1'
011900931015     C                   EXSR      SBMP25
012000931018     C                   EXSR      INZVID
012100931015     C                   END
012200931015     C*
012300931015     C     FINVD1        ENDSR
012400931015      *---------------------------------------------------------------*
012500931015      *  CNTVD1: CONTROLLI PRIMA VIDEATA                              *
012600931015      *---------------------------------------------------------------*
012700931015     C     CNTVD1        BEGSR
012800931015     C*----------------------------------------------------*
012900931015     C*   Effettuare una sola selezione
013000931018     C     VSEL1         IFNE      ' '
013100931018     C     VSEL2         ANDNE     ' '
013200961216     C                   SETON                                        284041
013300961216     C                   MOVEL     ERR(2)        $MSG
013400931015     C                   GOTO      FINCT1
013500931015     C                   END
013600931015     C*
013700931018     C     VSEL1         IFEQ      ' '
013800931018     C     VSEL2         ANDEQ     ' '
013900961216     C                   SETON                                        284041
014000961216     C                   MOVEL     ERR(1)        $MSG
014100931015     C                   GOTO      FINCT1
014200931015     C                   END
014300980112     C*----------------------------------------------------*
014400980112     C*  Controllo richiesta dati x una sola linea
014500980112     C     VLIN          IFNE      0
014600020506     C                   Z-ADD     1             Y                 3 0
014700980112     C     VLIN          LOOKUP    FIL(Y)                                 32
014800980112     C     *IN32         IFEQ      '0'
014900980112     C                   SETON                                        2848
015000980112     C                   MOVEL     ERR(9)        $MSG
015100980112     C                   GOTO      FINCT1
015200980112     C                   END
015300980112     C                   END
015400931015     C*----------------------------------------------------*
015500961216     C*   Se tipo stampa x spedizioni estero obbligatorio
015600961216     C*   indicare stampa scoperti in arrivo ordinati per
015700961216     C*   lna
015800961216     C     V1CTPI        IFEQ      'E'
015900961216     C     VSEL2         IFEQ      ' '
016000961216     C                   SETON                                        284143
016100961216     C                   MOVEL     ERR(3)        $MSG
016200961216     C                   GOTO      FINCT1
016300961216     C                   END
016400961216     C     VORD          IFEQ      'P'
016500961216     C                   SETON                                        284243
016600961216     C                   MOVEL     ERR(4)        $MSG
016700961216     C                   GOTO      FINCT1
016800961216     C                   END
016900980112     C*  se estero filiale arrivo no Estero do errore
017000980112     C     FL1(Y)        IFNE      'E'
017100961217     C     VLIN          ANDGT     0
017200961216     C                   SETON                                        284348
017300961217     C                   MOVEL     ERR(11)       $MSG
017400961216     C                   GOTO      FINCT1
017500961216     C                   END
017600961216     C                   ELSE
017700980112     C*  se italia filiale arrivo si Estero do errore
017800980112     C     FL1(Y)        IFEQ      'E'
017900980128     C     VLIN          ANDGT     0
018000961216     C                   SETON                                        284348
018100961216     C                   MOVEL     ERR(10)       $MSG
018200961216     C                   GOTO      FINCT1
018300961216     C                   END
018400961216     C                   END
018500961216     C*----------------------------------------------------*
018600931015     C*   Controllo le date di spedizione
018700931015     C     VDASPE        IFNE      0
018800931015     C                   MOVE      VDASPE        G02DAT
018900931015     C                   MOVEL     *BLANK        G02ERR
019000941104     C                   CALL      'XSRDA8'
019100941104     C                   PARM                    WLBDA8
019200931015     C     G02ERR        IFEQ      '1'
019300961216     C                   SETON                                        4428
019400961216     C                   MOVEL     ERR(5)        $MSG
019500931015     C                   GOTO      FINCT1
019600931015     C                   ELSE
019700931015     C                   Z-ADD     G02INV        SDDAT
019800941104     C                   Z-ADD     G02DAT        VDASPE
019900931015     C                   END
020000931015     C                   END
020100931015     C*---
020200931015     C     VDASPE        IFNE      0
020300931015     C     VASPE         ANDEQ     0
020400931015     C                   MOVE      VDASPE        VASPE
020500931015     C                   END
020600931015     C*---
020700931015     C     VASPE         IFNE      0
020800931015     C                   MOVE      VASPE         G02DAT
020900931015     C                   MOVEL     *BLANK        G02ERR
021000941104     C                   CALL      'XSRDA8'
021100941104     C                   PARM                    WLBDA8
021200931015     C     G02ERR        IFEQ      '1'
021300961216     C                   SETON                                        4528
021400961216     C                   MOVEL     ERR(5)        $MSG
021500931015     C                   GOTO      FINCT1
021600931015     C                   ELSE
021700931015     C                   Z-ADD     G02INV        SADAT
021800941104     C                   Z-ADD     G02DAT        VASPE
021900931015     C                   END
022000931015     C                   ELSE
022100961022     C                   Z-ADD     99999999      SADAT
022200931015     C                   END
022300931015     C*---
022400931015     C     SDDAT         IFGT      SADAT
022500961216     C                   SETON                                        444528
022600961216     C                   MOVEL     ERR(6)        $MSG
022700961216     C                   GOTO      FINCT1
022800931015     C                   END
022900931015     C*----------------------------------------------------*
023000931015     C*  Controllo parzializzazioni su cliente
023100931015     C     VCDI          IFNE      0
023200941104     C     VSEL2         ANDEQ     '1'
023300961216     C                   SETON                                        414628
023400961216     C                   MOVEL     ERR(7)        $MSG
023500931015     C                   GOTO      FINCT1
023600931015     C                   END
023700931015     C*---
023800931015     C     VRAG          IFNE      *BLANKS
023900941104     C     VSEL2         ANDEQ     '1'
024000961216     C                   SETON                                        414728
024100961216     C                   MOVEL     ERR(7)        $MSG
024200931015     C                   GOTO      FINCT1
024300931015     C                   END
024400931015     C*---
024500931015     C     VCDI          IFNE      0
024600931015     C     VRAG          ANDNE     *BLANKS
024700961216     C                   SETON                                        464728
024800961216     C                   MOVEL     ERR(8)        $MSG
024900931015     C                   GOTO      FINCT1
025000931015     C                   END
025100931015     C*---
025200931015     C     FINCT1        ENDSR
025300931015      *---------------------------------------------------------------*
025400931015      *  SBMP25: SOTTOMISSIONE AZIONE BATCH DI STAMPA                 *
025500931015      *---------------------------------------------------------------*
025600931015     C     SBMP25        BEGSR
025700931015     C*----------------------------------------------------*
025800941104     C                   MOVE      VSEL1         D24SCP
025900941104     C                   MOVE      VSEL2         D24SCA
026000941104     C                   MOVE      VORD          D24ORD
026100941104     C                   MOVE      VDET          D24DET
026200070629     C                   MOVE      Vxca          D24xca
026300941104     C                   MOVE      SDDAT         D24SDD
026400941104     C                   MOVE      SADAT         D24SDA
026500941104     C                   MOVE      VCDI          D24CDI
026600941104     C                   MOVE      VRAG          D24RAG
026700941104     C                   MOVE      VLIN          D24LIN
026800961216     C                   MOVE      V1CTPI        D24TPI
026900010108     C                   MOVE      VPARZ         D24PAR
027000931015     C*---
027100941104     C                   MOVE      'SC25'        KCOAZ
027200941104     C                   MOVEL     TNSC24        KPJBU
027300931015     C                   CALL      'BCH10'
027400931015     C                   PARM                    KPJBA
027500931015     C*---
027600931015     C                   ENDSR
027700931015      *---------------------------------------------------------------*
027800941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
027900931015      *---------------------------------------------------------------*
028000941104     C     *INZSR        BEGSR
028100931015      *---------------------------------------------------------------*
028200931015     C     *ENTRY        PLIST
028300931015     C                   PARM                    KPJBA
028400931015      *
028500931015     C                   Z-ADD     1             CODUT
028600931015     C                   CALL      'X§PARUT'
028700931015     C                   PARM                    UT§DSE
028800931015     C                   MOVEL     RAGUT         RSUT             20
028900931015     C                   MOVEL     REC80         CNCR80
029000931015     C                   MOVEL     KNMUS         §SEI              6
029100931015     C*----------------------------------------------------*
029200931015     C*  Chiave x reperimento dati '£3'                    *
029300020506     C*    KEYTAB        KLIST
029400020506     C*                  KFLD                    CODUT
029500020506     C*                  KFLD                    TBLCOD
029600020506     C*                  KFLD                    TBLKEY
029700931015     C*----------------------------------------------------*
029800931015     C*  Reperimanto codici filiali in gestione sull'AS
029900961216     C                   SETOFF                                       05
030000020506     C*                  OPEN      TABEL00F
030100980112     C                   OPEN      AZORG01L
030200080813      *
030300080813     c                   clear                   trul72ds
030400080813     c                   eval      i72tla = 'L'
030500080813     c                   eval      I72POU = simfel
030600080813     c                   eval      kpjbu = trul72ds
030700080813     C                   CALL      'TRUL72R'
030800080813     C                   PARM                    KPJBA
030900080813     c                   movel     kpjbu         trul72ds
031000080813     c                   movea     lin           fil
031100080813      *
031200020506     C*                  MOVEL     '£3'          TBLCOD
031300020506     C*                  MOVEL     SIMFEL        TBLKEY
031400020506     C*    KEYTAB        CHAIN     TABEL                              21
031500020506     C*    *IN21         IFEQ      '0'
031600020506     C*                  MOVEA     TBLUNI        FIL1
031700020506     C                   Z-ADD     1             Y
031800080813     C                   DO        60            X                 2 0
031900020506     C     FIL(X)        IFGT      *ZEROS
032000020506     C*                  ADD       1             Y
032100020506     C*                  MOVE      FIL1(X)       FIL(Y)
032200980112     C* Controllo se filiale Italia o filiale estera
032300020506     C                   Z-ADD     FIL(X)        ORGFIL
032400020506     C                   MOVEL     *BLANKS       ORGFL1
032500020506     C     ORGFIL        CHAIN     AZORG01L                           22
032600020729     C                   clear                   OG143
032700020729     C  n22              movel     ORGDE3        OG143
032800020729     C                   movel     *blank        flgest            1
032900020729     C     §OgNTW        ifeq      'FED'
033000020729     C     §OgNTW        oreq      'EEX'
033100020729     C     §OgNTW        oreq      'EUP'
033200020729     C                   MOVEL     'E'           flgest
033300020729     c                   end
033400020729     C****               MOVE      ORGFL1        FL1(x)
033500020729     C                   MOVE      flgest        FL1(x)
033600020506     C     FL1(x)        IFEQ      'E'
033700980112     C                   SETON                                        05
033800980112     C                   END
033900931015     C                   END
034000931015     C                   END
034100020506     C*                  ELSE
034200020506     C*                  MOVE      SIMFEL        FIL(1)
034300020506     C*                  END
034400020506     C*                  CLOSE     TABEL00F
034500980112     C                   CLOSE     AZORG01L
034600931015     C*----------------------------------------------------*
034700931015     C*  Definizione variabili
034800941104     C     *LIKE         DEFINE    D24SDD        SDDAT
034900941104     C     *LIKE         DEFINE    D24SDA        SADAT
035000931015     C*----------------------------------------------------*
035100931015     C                   MOVEL     'N'           £FINE             1
035200931015     C                   MOVEL     '1'           TIPVID            1
035300931018     C                   MOVEL     NOMPG         NOMPGM
035400931015     C*----------------------------------------------------*
035500931015     C                   ENDSR
035600980112**
035700961216Effettuare una scelta                                                  01
035800961216Impossibile scegliere entrambe                                         02
035900961216Se selez. stampa per estero obbligatoria stampa scoperti in arrivo     03
036000961216Se selez. stampa per estero obbligatoria ordinamento per P.O. arrivo   04
036100961216Data errata                                                            05
036200961216Data iniziale maggiore della data finale                               06
036300961216Parzializzazione non compatibile con tipo stampa                       07
036400961216Impossibile selezionare sia codice che ragione sociale                 08
036500961216Linea non in gestione al punto operativo                               09
036600980112Se indicata linea di arrivo NO ITALIA obbligatoria stampa estero       10
036700980112Se indicata linea di arrivo ITALIA impossibile selez. stampa estero    11
