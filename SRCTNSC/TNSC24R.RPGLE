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
002900140120     Ftntbe01L  IF   E           K DISK
003000931015?     *---------
003100941104     FTNSC24D   CF   E             WORKSTN
003200931015?     *--------------------------------------------------------------*
003300140120     D**** FIL             S              3  0 DIM(60)
003400140120     D FIL             S              3  0 DIM(250)
003500020506     D*FIL1            S              3    DIM(30)
003600140120     D*** FL1             S              1    DIM(60)
003700140120     D FL1             S              1    DIM(250)
003800961216     D ERR             S             70    DIM(11) CTDATA PERRCD(1)
003900931015?     *--------------------------------------------------------------*
004000931015?     *  DS                                                          *
004100931015?     *--------------------------------------------------------------*
004200931015     D KPJBA         E DS
004300140120     D DNTW          E DS
004400931015      *
004500941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
004600931015      *
004700931015     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800020729     D OG143         E DS
004900931015     D CNCR80        E DS
005000140120      ****-?
005100140120     D**** TRUL72DS      E DS
005200140120     D****  Lin                           3s 0 DESCEND
005300140120     D****                                     DIM(60)
005400140120     D****                                     OVERLAY(O72Lia)
005500140120      ****-?Caricamento filiali contabili
005600140120     d TRUL31ds      e ds                  inz
005700140120     d $POg            s              3    inz   dim(250)
005800140120      ****-?
005900931015      *
006000941104     D WLBDA8          DS
006100941104     D  G02DAT                 1      8  0
006200941104     D  G02INV                 9     16  0
006300941104     D  G02ERR                17     17
006400941104     D  G02TGI                18     22  0
006500931015     D                SDS
006600931015     D  NOMPG                  1     10
006700980112?     *--------------------------------------------------------------*
006800980112?     *  Ridefinizione interna campi azorg                           *
006900980112?     *--------------------------------------------------------------*
007000980112     IAZORG01L  AA
007100980112     I                             P    4    5 0ORGFIL
007200980112     I                                 11   11  ORGFL1
007300020729     I                               3776 3800  ORGDE3
007400931015      *---------------------------------------------------------------*
007500931015      *   C I C L O      P R I N C I P A L E                          *
007600931015      *---------------------------------------------------------------*
007700931015     C*  INIZIALIZZAZIONE VIDEATA
007800931015     C                   EXSR      INZVID
007900931015     C*
008000931015     C*  LOOP
008100931015     C     £FINE         DOUEQ     'S'
008200931015     C*
008300931015     C     TIPVID        CASEQ     '1'           GESVD1
008400931015     C                   END
008500931015     C*
008600931015     C                   END
008700931015     C*
008800931015     C                   SETON                                        LR
008900931015      *---------------------------------------------------------------*
009000931015      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
009100931015      *---------------------------------------------------------------*
009200931015     C     INZVID        BEGSR
009300931015     C*
009400931015     C                   MOVE      *BLANKS       VSEL1
009500931015     C                   MOVE      *BLANKS       VSEL2
009600931015     C                   MOVE      *BLANKS       VORD
009700931015     C                   MOVE      'SI'          VDET
009800931015     C                   Z-ADD     0             VDASPE
009900931015     C                   Z-ADD     0             VASPE
010000931015     C                   Z-ADD     0             VCDI
010100931015     C                   Z-ADD     0             VLIN
010200931015     C                   MOVE      *BLANKS       VRAG
010300961216     C                   MOVEL     'I'           V1CTPI
010400010108     C                   MOVE      *BLANKS       VPARZ
010500070702     C                   MOVE      'N'           Vxca
010600931015     C*
010700931015     C                   ENDSR
010800931015      *---------------------------------------------------------------*
010900931015      *  GESVD1: GESTIONE PRIMA VIDEATA RICHIESTA PARAMETRI           *
011000931015      *---------------------------------------------------------------*
011100931015     C     GESVD1        BEGSR
011200931015     C*
011300941104     C                   EXFMT     SC24D01
011400961216     C                   SETOFF                                       404142
011500961216     C                   SETOFF                                       434445
011600961216     C                   SETOFF                                       464748
011700961216     C                   SETOFF                                       28
011800931015     C*  Fine elaborazione
011900931015     C     *IN03         IFEQ      '1'
012000931015     C                   MOVEL     'S'           £FINE
012100931015     C                   GOTO      FINVD1
012200931015     C                   END
012300931015     C*  Controlli
012400931015     C                   EXSR      CNTVD1
012500961216     C   28              GOTO      FINVD1
012600931015     C*  Sottomissione Batch
012700931015     C     *IN06         IFEQ      '1'
012800931015     C                   EXSR      SBMP25
012900931018     C                   EXSR      INZVID
013000931015     C                   END
013100931015     C*
013200931015     C     FINVD1        ENDSR
013300931015      *---------------------------------------------------------------*
013400931015      *  CNTVD1: CONTROLLI PRIMA VIDEATA                              *
013500931015      *---------------------------------------------------------------*
013600931015     C     CNTVD1        BEGSR
013700931015     C*----------------------------------------------------*
013800931015     C*   Effettuare una sola selezione
013900931018     C     VSEL1         IFNE      ' '
014000931018     C     VSEL2         ANDNE     ' '
014100961216     C                   SETON                                        284041
014200961216     C                   MOVEL     ERR(2)        $MSG
014300931015     C                   GOTO      FINCT1
014400931015     C                   END
014500931015     C*
014600931018     C     VSEL1         IFEQ      ' '
014700931018     C     VSEL2         ANDEQ     ' '
014800961216     C                   SETON                                        284041
014900961216     C                   MOVEL     ERR(1)        $MSG
015000931015     C                   GOTO      FINCT1
015100931015     C                   END
015200980112     C*----------------------------------------------------*
015300980112     C*  Controllo richiesta dati x una sola linea
015400980112     C     VLIN          IFNE      0
015500020506     C                   Z-ADD     1             Y                 3 0
015600980112     C     VLIN          LOOKUP    FIL(Y)                                 32
015700980112     C     *IN32         IFEQ      '0'
015800980112     C                   SETON                                        2848
015900980112     C                   MOVEL     ERR(9)        $MSG
016000980112     C                   GOTO      FINCT1
016100980112     C                   END
016200980112     C                   END
016300931015     C*----------------------------------------------------*
016400961216     C*   Se tipo stampa x spedizioni estero obbligatorio
016500961216     C*   indicare stampa scoperti in arrivo ordinati per
016600961216     C*   lna
016700961216     C     V1CTPI        IFEQ      'E'
016800961216     C     VSEL2         IFEQ      ' '
016900961216     C                   SETON                                        284143
017000961216     C                   MOVEL     ERR(3)        $MSG
017100961216     C                   GOTO      FINCT1
017200961216     C                   END
017300961216     C     VORD          IFEQ      'P'
017400961216     C                   SETON                                        284243
017500961216     C                   MOVEL     ERR(4)        $MSG
017600961216     C                   GOTO      FINCT1
017700961216     C                   END
017800980112     C*  se estero filiale arrivo no Estero do errore
017900980112     C     FL1(Y)        IFNE      'E'
018000961217     C     VLIN          ANDGT     0
018100961216     C                   SETON                                        284348
018200961217     C                   MOVEL     ERR(11)       $MSG
018300961216     C                   GOTO      FINCT1
018400961216     C                   END
018500961216     C                   ELSE
018600980112     C*  se italia filiale arrivo si Estero do errore
018700980112     C     FL1(Y)        IFEQ      'E'
018800980128     C     VLIN          ANDGT     0
018900961216     C                   SETON                                        284348
019000961216     C                   MOVEL     ERR(10)       $MSG
019100961216     C                   GOTO      FINCT1
019200961216     C                   END
019300961216     C                   END
019400961216     C*----------------------------------------------------*
019500931015     C*   Controllo le date di spedizione
019600931015     C     VDASPE        IFNE      0
019700931015     C                   MOVE      VDASPE        G02DAT
019800931015     C                   MOVEL     *BLANK        G02ERR
019900941104     C                   CALL      'XSRDA8'
020000941104     C                   PARM                    WLBDA8
020100931015     C     G02ERR        IFEQ      '1'
020200961216     C                   SETON                                        4428
020300961216     C                   MOVEL     ERR(5)        $MSG
020400931015     C                   GOTO      FINCT1
020500931015     C                   ELSE
020600931015     C                   Z-ADD     G02INV        SDDAT
020700941104     C                   Z-ADD     G02DAT        VDASPE
020800931015     C                   END
020900931015     C                   END
021000931015     C*---
021100931015     C     VDASPE        IFNE      0
021200931015     C     VASPE         ANDEQ     0
021300931015     C                   MOVE      VDASPE        VASPE
021400931015     C                   END
021500931015     C*---
021600931015     C     VASPE         IFNE      0
021700931015     C                   MOVE      VASPE         G02DAT
021800931015     C                   MOVEL     *BLANK        G02ERR
021900941104     C                   CALL      'XSRDA8'
022000941104     C                   PARM                    WLBDA8
022100931015     C     G02ERR        IFEQ      '1'
022200961216     C                   SETON                                        4528
022300961216     C                   MOVEL     ERR(5)        $MSG
022400931015     C                   GOTO      FINCT1
022500931015     C                   ELSE
022600931015     C                   Z-ADD     G02INV        SADAT
022700941104     C                   Z-ADD     G02DAT        VASPE
022800931015     C                   END
022900931015     C                   ELSE
023000961022     C                   Z-ADD     99999999      SADAT
023100931015     C                   END
023200931015     C*---
023300931015     C     SDDAT         IFGT      SADAT
023400961216     C                   SETON                                        444528
023500961216     C                   MOVEL     ERR(6)        $MSG
023600961216     C                   GOTO      FINCT1
023700931015     C                   END
023800931015     C*----------------------------------------------------*
023900931015     C*  Controllo parzializzazioni su cliente
024000931015     C     VCDI          IFNE      0
024100941104     C     VSEL2         ANDEQ     '1'
024200961216     C                   SETON                                        414628
024300961216     C                   MOVEL     ERR(7)        $MSG
024400931015     C                   GOTO      FINCT1
024500931015     C                   END
024600931015     C*---
024700931015     C     VRAG          IFNE      *BLANKS
024800941104     C     VSEL2         ANDEQ     '1'
024900961216     C                   SETON                                        414728
025000961216     C                   MOVEL     ERR(7)        $MSG
025100931015     C                   GOTO      FINCT1
025200931015     C                   END
025300931015     C*---
025400931015     C     VCDI          IFNE      0
025500931015     C     VRAG          ANDNE     *BLANKS
025600961216     C                   SETON                                        464728
025700961216     C                   MOVEL     ERR(8)        $MSG
025800931015     C                   GOTO      FINCT1
025900931015     C                   END
026000931015     C*---
026100931015     C     FINCT1        ENDSR
026200931015      *---------------------------------------------------------------*
026300931015      *  SBMP25: SOTTOMISSIONE AZIONE BATCH DI STAMPA                 *
026400931015      *---------------------------------------------------------------*
026500931015     C     SBMP25        BEGSR
026600931015     C*----------------------------------------------------*
026700941104     C                   MOVE      VSEL1         D24SCP
026800941104     C                   MOVE      VSEL2         D24SCA
026900941104     C                   MOVE      VORD          D24ORD
027000941104     C                   MOVE      VDET          D24DET
027100070629     C                   MOVE      Vxca          D24xca
027200941104     C                   MOVE      SDDAT         D24SDD
027300941104     C                   MOVE      SADAT         D24SDA
027400941104     C                   MOVE      VCDI          D24CDI
027500941104     C                   MOVE      VRAG          D24RAG
027600941104     C                   MOVE      VLIN          D24LIN
027700961216     C                   MOVE      V1CTPI        D24TPI
027800010108     C                   MOVE      VPARZ         D24PAR
027900931015     C*---
028000941104     C                   MOVE      'SC25'        KCOAZ
028100941104     C                   MOVEL     TNSC24        KPJBU
028200931015     C                   CALL      'BCH10'
028300931015     C                   PARM                    KPJBA
028400931015     C*---
028500931015     C                   ENDSR
028600931015      *---------------------------------------------------------------*
028700941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
028800931015      *---------------------------------------------------------------*
028900941104     C     *INZSR        BEGSR
029000931015      *---------------------------------------------------------------*
029100931015     C     *ENTRY        PLIST
029200931015     C                   PARM                    KPJBA
029300931015      *
029400931015     C                   Z-ADD     1             CODUT
029500931015     C                   CALL      'X§PARUT'
029600931015     C                   PARM                    UT§DSE
029700931015     C                   MOVEL     RAGUT         RSUT             20
029800931015     C                   MOVEL     REC80         CNCR80
029900931015     C                   MOVEL     KNMUS         §SEI              6
030000931015     C*----------------------------------------------------*
030100931015     C*  Chiave x reperimento dati '£3'                    *
030200020506     C*    KEYTAB        KLIST
030300020506     C*                  KFLD                    CODUT
030400020506     C*                  KFLD                    TBLCOD
030500020506     C*                  KFLD                    TBLKEY
030600140120     C*
030700140120     C     KTBE          KLIST
030800140120     C                   KFLD                    TBECOD
030900140120     C                   KFLD                    TBEKE1
031000931015     C*----------------------------------------------------*
031100931015     C*  Reperimanto codici filiali in gestione sull'AS
031200961216     C                   SETOFF                                       05
031300020506     C*                  OPEN      TABEL00F
031400980112     C                   OPEN      AZORG01L
031500140120      *********
031600140120     c*********          clear                   trul72ds
031700140120     c*********          eval      i72tla = 'L'
031800140120     c*********          eval      I72POU = simfel
031900140120     c*********          eval      kpjbu = trul72ds
032000140120     C*********          CALL      'TRUL72R'
032100140120     C*********          PARM                    KPJBA
032200140120     c*********          movel     kpjbu         trul72ds
032300140120     c*********          movea     lin           fil
032400140120     C***
032500140120      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
032600140120     c                   clear                   TRUL31DS
032700140120     c                   eval      I31abi = 'AC'
032800140120     c                   eval      I31cpo = simfel
032900140120     c                   call      'TRUL31R'
033000140120     c                   parm                    KPJBA
033100140120     c                   parm                    TRUL31DS
033200140120     c                   if        o31pog > *zeros
033300140120     c                   movea     o31pog        $pog
033400140120     c                   endif
033500140120      *
033600140120     C                   DO        250           X
033700140120     c                   if           $pog(X) <> *blank
033800140120     c                   move      $pog(X)       fil(X)
033900140120     c                   else
034000140120     c                   move      '000'         fil(X)
034100140120     c                   endif
034200140120     c                   endDO
034300080813      *
034400020506     C*                  MOVEL     '£3'          TBLCOD
034500020506     C*                  MOVEL     SIMFEL        TBLKEY
034600020506     C*    KEYTAB        CHAIN     TABEL                              21
034700020506     C*    *IN21         IFEQ      '0'
034800020506     C*                  MOVEA     TBLUNI        FIL1
034900020506     C                   Z-ADD     1             Y
035000140120     C                   DO        250           X                 3 0
035100020506     C     FIL(X)        IFGT      *ZEROS
035200020506     C*                  ADD       1             Y
035300020506     C*                  MOVE      FIL1(X)       FIL(Y)
035400980112     C* Controllo se filiale Italia o filiale estera
035500020506     C                   Z-ADD     FIL(X)        ORGFIL
035600020506     C                   MOVEL     *BLANKS       ORGFL1
035700020506     C     ORGFIL        CHAIN     AZORG01L                           22
035800020729     C                   clear                   OG143
035900020729     C  n22              movel     ORGDE3        OG143
036000020729     C                   movel     *blank        flgest            1
036100140120      *-- ITALIA/ESTERO
036200140120     c                   movel     'NTW'         tbeCOD
036300140120     c                   movel(p)  §OgNTW        tbeKE1
036400140120     c     kTBE          chain     tnTBE01L
036500140120     c                   if        %Found(tnTBE01L)
036600140120     c                   eval      dNTW = tbeUNI
036700140120     c                   eval      flgest = §NTWFIE
036800140120     c                   end
036900140120     C**** §OgNTW        ifeq      'FED'
037000140120     C**** §OgNTW        oreq      'EEX'
037100140120     C**** §OgNTW        oreq      'EUP'
037200140120     C*******            MOVEL     'E'           flgest
037300140120     c**********         end
037400020729     C****               MOVE      ORGFL1        FL1(x)
037500020729     C                   MOVE      flgest        FL1(x)
037600020506     C     FL1(x)        IFEQ      'E'
037700980112     C                   SETON                                        05
037800980112     C                   END
037900931015     C                   END
038000931015     C                   END
038100020506     C*                  ELSE
038200020506     C*                  MOVE      SIMFEL        FIL(1)
038300020506     C*                  END
038400020506     C*                  CLOSE     TABEL00F
038500980112     C                   CLOSE     AZORG01L
038600931015     C*----------------------------------------------------*
038700931015     C*  Definizione variabili
038800941104     C     *LIKE         DEFINE    D24SDD        SDDAT
038900941104     C     *LIKE         DEFINE    D24SDA        SADAT
039000931015     C*----------------------------------------------------*
039100931015     C                   MOVEL     'N'           £FINE             1
039200931015     C                   MOVEL     '1'           TIPVID            1
039300931018     C                   MOVEL     NOMPG         NOMPGM
039400931015     C*----------------------------------------------------*
039500931015     C                   ENDSR
039600980112**
039700961216Effettuare una scelta                                                  01
039800961216Impossibile scegliere entrambe                                         02
039900961216Se selez. stampa per estero obbligatoria stampa scoperti in arrivo     03
040000961216Se selez. stampa per estero obbligatoria ordinamento per P.O. arrivo   04
040100961216Data errata                                                            05
040200961216Data iniziale maggiore della data finale                               06
040300961216Parzializzazione non compatibile con tipo stampa                       07
040400961216Impossibile selezionare sia codice che ragione sociale                 08
040500961216Linea non in gestione al punto operativo                               09
040600980112Se indicata linea di arrivo NO ITALIA obbligatoria stampa estero       10
040700980112Se indicata linea di arrivo ITALIA impossibile selez. stampa estero    11
