000100020717     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300010214     H* STATISTICA CONSEGNE RICHIESTE - PARAMETRI                              *
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNSD87D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(SD87DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     D*------------------------------------------------------------------------*
001200000000     D* SCHIERE
001300000000     D*------------------------------------------------------------------------*
001400000000     D*-------------------
001500000000     D* SCHIERE VIDEO
001600000000     D*-------------------
001700000000     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
001800000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900000000     D ANN             S              2  0 DIM(12)                              *ANNI ELABORAZIONE
002000000000     D MES             S              2  0 DIM(12)                              *MESI ELABORAZIONE
002100000000     D*-------------------
002200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300000000     D*-------------------
002400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700000000     D*-------------------
002800000000     D* TABELLA DESCRIZIONI A VIDEO
002900000000     D*-------------------
003000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100000000     D*-------------------
003200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003300000000     D*-------------------
003400000000     D KEYS            S              1    DIM(40)
003500000000     D*-------------------
003600000000     D* FILIALI IN GESTIONE
003700000000     D*-------------------
003800020415     D L1              S              3  0 DIM(30)                              FILIALI IN GESTIONE
003900000000     D L6              S              3  0 DIM(30)                              FILIALI IN GEST. ARR
004000000000     D*------------------------------------------------------------------------*
004100000000     D* INPUT
004200000000     D*------------------------------------------------------------------------*
004300000000     D*-------------------
004400000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004500000000     D*-------------------
004600000000     D DSFUN         E DS
004700000000     D  FUNRIG                 1     40
004800000000     D*-------------------
004900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005000000000     D*-------------------
005100000000     D DSMSG         E DS
005200000000     D*-------------------
005300000000     D* DS PER GESTIONE TASTI FUNZIONALI
005400000000     D*-------------------
005500000000     D DSKEYS        E DS
005600000000     D  KEYRIG                 1     40
005700000000     D*-------------------
005800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005900000000     D*-------------------
006000000000     D                SDS
006100000000     D  USER                 254    263
006200000000     D*-------------------
006300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006400000000     D*-------------------
006500000000     D DEVDS1          DS
006600000000     D  KEY                  369    369
006700000000     D  CURSOR               370    371
006800000000     D*-------------------
006900000000     D* ARCHITETTURA
007000000000     D*-------------------
007100000000     D KPJBA         E DS
007200020717
007300020717     d DdatiUte      e ds
007400020717     d AzuteDs       e ds                  ExtName(Azute00f)
007500020717     d Tibs34Ds      e ds                  Inz
007600020717
007700000000     D*------------------
007800000000     D* PARAMETRI DI LANCIO
007900000000     D*------------------
008000000000     D PARAM           DS                  INZ
008100970916     D  PARLNA                 1      3  0
008200970916     D  PARDTI                 4     11  0
008300970916     D  PARDTF                12     19  0
008400970916     D  PARDET                20     20
008500970916     D  PARCD1                21     21
008600970916     D  PARCD2                22     22
008700970916     D  PARCD3                23     23
008800970916     D  PARCD4                24     24
008900970916     D  PARCD5                25     25
009000970916     D  PARCD6                26     26
009100970916     D  PARCD7                27     27
009200970916     D  PARCD8                28     28
009300970916     D  PARCD9                29     29
009400970916     D  PARCD0                30     30
009500970916     D  PARCDA                31     31
009600970916     D  PARCDB                32     32
009700970916     D  PARCDC                33     33
009800970916     D  PARCDD                34     34
009900970916     D  PARCDE                35     35
010000970916     D  PARCDF                36     36
010100970916     D  PARCDG                37     37
010200970916     D  PARCDH                38     38
010300970916     D  PARCDI                39     39
010400970916     D  PARCDL                40     40
010500000000     D*------------------
010600000000     D* DS CONTROLLA DATA (8)
010700000000     D*------------------
010800000000     D WLBDA8          DS                  INZ
010900000000     D  G08DAT                 1      8  0
011000000000     D  G08INV                 9     16  0
011100000000     D  G08ERR                17     17
011200000000     D  G08TGI                18     22  0
011300000000     D*-------------------
011400000000     D* RICALCOLA DATA (8)
011500000000     D*-------------------
011600000000     D WGIDA8          DS
011700000000     D  GI8DAT                 1      8  0
011800000000     D  GI8INV                 9     16  0
011900000000     D  GI8TGI                17     21  0
012000000000     D*------------------
012100000000     D* DS PER TRUL06R - CARICAMENTO TABELLE "£X"
012200000000     D*------------------
012300000000     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012400000000     D  LIN                    1     90  0
012500000000     D                                     DIM(30)                              FILIALI DI COMODO
012600000000     C*------------------------------------------------------------------------*
012700000000     C* MAIN LINE
012800000000     C*------------------------------------------------------------------------*
012900000000     C*
013000000000     C* CICLO FINO A CHE:
013100000000DO  1C     $FINE         DOUEQ     '1'
013200000000     C*
013300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013400000000     C*
013500000000     C* CONTROLLA GESTIONE VIDEO
013600000000IF  2C     $GES          IFEQ      'V1'
013700000000     C                   EXSR      GESV1                                        *PARAMETRI
013800000000E   2C                   ENDIF
013900000000     C*
014000000000E   1C                   ENDDO
014100000000     C*
014200000000     C                   SETON                                        LR
014300000000     C*------------------------------------------------------------------------*
014400000000     C* GESV1 - GESTIONE PRIMO VIDEO
014500000000     C*------------------------------------------------------------------------*
014600000000     C     GESV1         BEGSR
014700000000     C*
014800000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
014900000000     C                   EXSR      CARV1                                        *CARICA VIDEO
015000000000E   1C                   ENDIF
015100000000     C*
015200000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
015300000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
015400000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
015500000000E   1C                   ENDIF
015600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
015700000000     C*
015800000000     C                   EXFMT     SD87DV1                                      *PARAMETRI
015900000000     C*
016000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
016100020717
016200020717      * Se Profilo non OK devo uscire dal pgm
016300020717     c                   If        *In76
016400020717     c                   Eval      $err = '1'
016500020717     c                   Eval      $fine = '1'
016600020717     c                   EndIf
016700000000     C*
016800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
016900000000SL  2C                   SELECT
017000000000     C     KEY           WHENEQ    ENTER
017100000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
017200000000     C     KEY           WHENEQ    F6
017300000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
017400000000     C                   OTHER
017500000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
017600000000E   2C                   ENDSL
017700000000E   1C                   ENDIF
017800000000     C*
017900000000     C                   ENDSR
018000000000     C*------------------------------------------------------------------------*
018100000000     C* CARV1 - CARICA PRIMO VIDEO
018200000000     C*------------------------------------------------------------------------*
018300000000     C     CARV1         BEGSR
018400000000     C*
018500000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
018600000000     C*
018700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
018800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
018900000000     C*
019000000000     C                   WRITE     SD87DVT                                      *TESTATA
019100000000     C                   WRITE     SD87DVF                                      *FUNZIONI
019200000000     C*
019300000000     C                   ENDSR
019400000000     C*------------------------------------------------------------------------*
019500000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
019600000000     C*------------------------------------------------------------------------*
019700000000     C     ENTV1         BEGSR
019800000000     C*
019900000000     C* CONTROLLA I CAMPI IMMESSI
020000000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
020100000000     C*
020200000000     C                   ENDSR
020300000000     C*------------------------------------------------------------------------*
020400000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
020500000000     C*------------------------------------------------------------------------*
020600000000     C     F06GES        BEGSR
020700000000     C*
020800000000     C* CONTROLLA I CAMPI IMMESSI
020900000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
021000000000     C*
021100000000     C* LANCIA IL PROGRAMMA
021200000000IF  1C     $ERR          IFEQ      '0'                                          *NESSUN ERRORE
021300000000     C                   Z-ADD     V1CLNA        PARLNA                          *LINEA DI ARRIVO
021400000000     C                   Z-ADD     DTIV1C        PARDTI                          *DATA INIZIO (A/M/G
021500000000     C                   Z-ADD     DTFV1C        PARDTF                          *DATA FINE (A/M/G)
021600000000     C                   MOVEL     V1CDET        PARDET                          *DETTAGLIO (S/N)
021700970916     C                   MOVEL     V1CCD1        PARCD1                          *CAUSALI DETTAGLIO
021800970916     C                   MOVEL     V1CCD2        PARCD2
021900970916     C                   MOVEL     V1CCD3        PARCD3
022000970916     C                   MOVEL     V1CCD4        PARCD4
022100970916     C                   MOVEL     V1CCD5        PARCD5
022200970916     C                   MOVEL     V1CCD6        PARCD6
022300970916     C                   MOVEL     V1CCD7        PARCD7
022400970916     C                   MOVEL     V1CCD8        PARCD8
022500970916     C                   MOVEL     V1CCD9        PARCD9
022600970916     C                   MOVEL     V1CCD0        PARCD0
022700970916     C                   MOVEL     V1CCDA        PARCDA
022800970916     C                   MOVEL     V1CCDB        PARCDB
022900970916     C                   MOVEL     V1CCDC        PARCDC
023000970916     C                   MOVEL     V1CCDD        PARCDD
023100970916     C                   MOVEL     V1CCDE        PARCDE
023200970916     C                   MOVEL     V1CCDF        PARCDF
023300970916     C                   MOVEL     V1CCDG        PARCDG
023400970916     C                   MOVEL     V1CCDH        PARCDH
023500970916     C                   MOVEL     V1CCDI        PARCDI
023600970916     C                   MOVEL     V1CCDL        PARCDL
023700000000     C                   MOVEL     'SD88'        KCOAZ
023800000000     C                   MOVEL     PARAM         KPJBU
023900020910     C                   CALL      'BCH09'
024000020910     C                   PARM                    KPJBA
024100020910     C                   CALL      'BCH10'
024200000000     C                   PARM                    KPJBA
024300980430     C                   MOVEL     '1'           $FINE
024400000000E   1C                   ENDIF
024500000000     C*
024600000000     C                   ENDSR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024900000000     C*------------------------------------------------------------------------*
025000000000     C     DECODI        BEGSR
025100000000     C*
025200000000     C* REIMPOSTA INDICATORI DI ERRORE SUI CAMPI
025300000000     C                   SETOFF                                       606162
025400000000     C                   SETOFF                                       63
025500000000     C*--------------------
025600000000     C* LINEA DI ARRIVO
025700000000     C*--------------------
025800000000     C                   MOVEL     *BLANKS       V1DLNA
025900000000IF  1C     $ERR          IFEQ      '0'
026000000000IF  2C     V1CLNA        IFNE      *ZEROS
026100000000     C                   Z-ADD     V1CLNA        WLNA
026200000000     C                   MOVEL     *BLANKS       WDLNA
026300000000     C                   EXSR      CHKLNA
026400000000     C                   Z-ADD     WLNA          V1CLNA
026500000000     C                   MOVEL     WDLNA         V1DLNA
026600000000IF  3C     $ERR          IFEQ      '1'
026700000000     C                   MOVEL     '1'           $ERR
026800000000     C                   MOVEL     MSG(7)        $MSG
026900000000     C                   EXSR      CARMSG
027000000000     C                   SETON                                        60
027100000000E   3C                   ENDIF
027200000000E   2C                   ENDIF
027300000000E   1C                   ENDIF
027400000000     C*--------------------
027500000000     C* DATA INIZIALE
027600000000     C*--------------------
027700000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C                         *DATA ROVESCIATA
027800000000IF  1C     $ERR          IFEQ      '0'
027900000000     C                   Z-ADD     V1CDTI        WDA8
028000000000     C                   Z-ADD     *ZEROS        DA8W
028100000000     C                   EXSR      CHKDA8
028200000000     C                   Z-ADD     WDA8          V1CDTI
028300000000     C                   Z-ADD     DA8W          DTIV1C
028400000000IF  2C     $ERR          IFEQ      '1'
028500000000     C                   MOVEL     '1'           $ERR
028600000000     C                   MOVEL     MSG(8)        $MSG
028700000000     C                   EXSR      CARMSG
028800000000     C                   SETON                                        61
028900000000E   2C                   ENDIF
029000000000E   1C                   ENDIF
029100000000     C*--------------------
029200000000     C* DATA FINALE
029300000000     C*--------------------
029400000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C                         *DATA ROVESCIATA
029500000000IF  1C     $ERR          IFEQ      '0'
029600000000     C                   Z-ADD     V1CDTF        WDA8
029700000000     C                   Z-ADD     *ZEROS        DA8W
029800000000     C                   EXSR      CHKDA8
029900000000     C                   Z-ADD     WDA8          V1CDTF
030000000000     C                   Z-ADD     DA8W          DTFV1C
030100000000IF  2C     $ERR          IFEQ      '1'
030200000000     C                   MOVEL     '1'           $ERR
030300000000     C                   MOVEL     MSG(8)        $MSG
030400000000     C                   EXSR      CARMSG
030500000000     C                   SETON                                        62
030600000000E   2C                   ENDIF
030700000000E   1C                   ENDIF
030800000000     C***
030900000000     C* DATA FINALE MAGGIORE DATA INIZIALE
031000000000     C***
031100000000IF  1C     $ERR          IFEQ      '0'
031200000000IF  2C     DTIV1C        IFGT      DTFV1C
031300000000     C                   MOVEL     '1'           $ERR
031400000000     C                   MOVEL     MSG(9)        $MSG
031500000000     C                   EXSR      CARMSG
031600000000     C                   SETON                                        6162
031700000000E   2C                   ENDIF
031800000000E   1C                   ENDIF
031900000000     C*--------------------
032000000000     C* DETTAGLIO BOLLE
032100000000     C*--------------------
032200000000IF  1C     $ERR          IFEQ      '0'
032300000000IF  2C     V1CDET        IFEQ      'S'
032400000000     C     V1CDET        OREQ      'N'
032500000000X   2C                   ELSE
032600000000     C                   MOVEL     '1'           $ERR
032700000000     C                   MOVEL     MSG(10)       $MSG
032800000000     C                   EXSR      CARMSG
032900000000     C                   SETON                                        63
033000000000E   2C                   ENDIF
033100000000E   1C                   ENDIF
033200000000     C*
033300000000     C                   ENDSR
033400000000     C*------------------------------------------------------------------------*
033500000000     C* CHKLNA - CONTROLLA LA LINEA DI ARRIVO
033600000000     C*        . SE E' FILIALE DI 1° LIVELLO PUO' ELABORARE I DATI DI SE' STESSA
033700020415     C*          E DELLE SUE 2° LIVELLO ("£1");
033800000000     C*        . SE E' FILIALE DI 2° LIVELLO PUO' ELABORARE SOLO I DATI DI SE'
033900000000     C*          STESSA PIU' QUELLI DELLE FILIALI CHE GESTISCE IN ARRIVO ("£6")
034000000000     C*------------------------------------------------------------------------*
034100000000     C     CHKLNA        BEGSR
034200000000     C*
034300000000     C     *LIKE         DEFINE    V1CLNA        WLNA
034400000000     C     *LIKE         DEFINE    V1DLNA        WDLNA
034500000000     C*
034600020415     C* FIL. 1° LIVELLO --> DEVE ESSERE PRESENTE FRA QUELLE CARICATE DALLA "£1"
034700000000     C* FIL. 2° LIVELLO --> DEVE ESSERE PRESENTE FRA QUELLE CARICATE DALLA "£6"
034800000000     C                   SETOFF                                           28
034900020717     c                   If        DutLpo = '1'
035000020415     C     WLNA          LOOKUP    L1                                     28
035100000000X   1C                   ELSE                                                   *2° LIVELLO
035200000000     C     WLNA          LOOKUP    L6                                     28
035300000000E   1C                   ENDIF
035400000000     C*
035500000000IF  1C     *IN28         IFEQ      *ON                                          *TROVATA
035600000000     C                   Z-ADD     WLNA          KORFIL
035700000000     C     KEYORG        CHAIN     AZORG01L                           99
035800000000IF  2C     *IN99         IFEQ      *OFF
035900000000     C                   MOVEL     ORGDES        WDLNA
036000000000E   2C                   ENDIF
036100000000X   1C                   ELSE                                                   *NON TROVATA
036200000000     C                   MOVEL     '1'           $ERR
036300000000E   1C                   ENDIF
036400000000     C*
036500000000     C                   ENDSR
036600000000     C*------------------------------------------------------------------------*
036700000000     C* CHKDA8 - CONTROLLA UN DATA LUNGA 8 E LA RIBALTA IN A/M/G
036800000000     C*------------------------------------------------------------------------*
036900000000     C     CHKDA8        BEGSR
037000000000     C*
037100000000     C     *LIKE         DEFINE    V1CDTI        WDA8
037200000000     C     *LIKE         DEFINE    V1CDTI        DA8W
037300000000     C*
037400000000     C                   Z-ADD     WDA8          G08DAT
037500000000     C                   Z-ADD     *ZEROS        G08INV
037600000000     C                   MOVEL     '0'           G08ERR
037700000000     C                   CALL      'XSRDA8'
037800000000     C                   PARM                    WLBDA8
037900000000     C                   Z-ADD     G08DAT        WDA8
038000000000     C                   Z-ADD     G08INV        DA8W
038100000000IF  1C     G08ERR        IFEQ      '1'
038200000000     C                   MOVEL     '1'           $ERR
038300000000E   1C                   ENDIF
038400000000     C*
038500000000     C                   ENDSR
038600000000     C*------------------------------------------------------------------------*
038700000000     C* IMPVID - IMPOSTAZIONI VIDEO
038800000000     C*------------------------------------------------------------------------*
038900000000     C     IMPVID        BEGSR
039000000000     C*
039100020717     c                   Z-Add     DutPou        V1cLna                         Linea arrivo
039200000000     C                   MOVEL     'N'           V1CDET                         *DETTAGLIO BOLLE
039300970916     C                   MOVEL     '1'           V1CCD1                         *CAUSALI DETTAGLIO
039400970916     C                   MOVEL     '2'           V1CCD2
039500970916     C                   MOVEL     'F'           V1CCD3
039600000000     C*--------------------
039700010124     C* DATA INIZIO = DATA CORRENTE - 7gg
039800000000     C* DATA FINE   = DATA CORRENTE
039900000000     C*--------------------
040000991223     C* REPERISCE LA DATA CORRENTE
040100991223     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
040200991223     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
040300991223     C                   Z-ADD     N8            V1CDTF                         D A T A   F I N E
040400991223     C                   Z-ADD     N8            G08DAT
040500991223     C                   Z-ADD     *ZEROS        G08INV
040600991223     C                   MOVEL     '0'           G08ERR
040700991223     C                   Z-ADD     *ZEROS        G08TGI
040800991223     C                   CALL      'XSRDA8'
040900991223     C                   PARM                    WLBDA8
041000991223     C                   Z-ADD     G08INV        DATCOR            8 0
041100991223     C*
041200010124     C* TRASFORMA LA DATA IN GIORNI
041300991223     C                   Z-ADD     DATCOR        G08INV
041400000000     C                   Z-ADD     *ZEROS        G08DAT
041500000000     C                   MOVEL     '3'           G08ERR
041600000000     C                   Z-ADD     *ZEROS        G08TGI
041700000000     C                   CALL      'XSRDA8'
041800991223     C                   PARM                    WLBDA8
041900991223     C*
042000010124     C* TOGLIE 7 GIORNI ALLA DATA CORRENTE E LA RITRASFORMA IN DATA
042100010124     C     G08TGI        SUB       7             G08TGI
042200000000     C                   Z-ADD     *ZEROS        GI8DAT
042300000000     C                   Z-ADD     *ZEROS        GI8INV
042400000000     C                   Z-ADD     G08TGI        GI8TGI
042500000000     C                   CALL      'XSRGI8'
042600000000     C                   PARM                    WGIDA8
042700000000     C                   Z-ADD     GI8DAT        V1CDTI                         D A T A   I N I Z I
042800991223     C*
042900000000     C                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100000000     C* CARTAB - CARICAMENTO TABELLE
043200000000     C*------------------------------------------------------------------------*
043300000000     C     CARTAB        BEGSR
043400000000     C***
043500020415     C* "L1" FILIALI IN GESTIONE
043600000000     C***
043700000000     C                   CLEAR                   DSUL06
043800000000     C                   CLEAR                   LIN
043900020415     C                   MOVE      '£1'          D06COD                         *CODICE TABELLA
044000020717     c                   Movel     DutPou        D06Key                         Fil.elaboratore
044100000000     C                   MOVEL     'L'           D06TLA                         *"L"=CHIUDE CON LR
044200000000     C                   MOVEL     DSUL06        KPJBU
044300000000     C                   CALL      'TRUL06R'
044400000000     C                   PARM                    KPJBA
044500000000     C                   MOVEL     KPJBU         DSUL06
044600020415     C                   MOVEA     LIN           L1
044700000000     C***
044800000000     C* "L6" FILIALI IN GESTIONE IN ARRIVO
044900000000     C***
045000000000     C                   CLEAR                   DSUL06
045100000000     C                   CLEAR                   LIN
045200000000     C                   MOVE      '£6'          D06COD                         *CODICE TABELLA
045300020717     c                   Movel     DutPou        D06Key                         Fil.elaboratore
045400000000     C                   MOVEL     'L'           D06TLA                         *"L"=CHIUDE CON LR
045500000000     C                   MOVEL     DSUL06        KPJBU
045600000000     C                   CALL      'TRUL06R'
045700000000     C                   PARM                    KPJBA
045800000000     C                   MOVEL     KPJBU         DSUL06
045900000000     C                   MOVEA     LIN           L6
046000000000     C*
046100000000     C                   ENDSR
046200000000     C*------------------------------------------------------------------------*
046300000000     C* *INZSR - ROUTINE INIZIALE
046400000000     C*------------------------------------------------------------------------*
046500000000     C     *INZSR        BEGSR
046600000000     C*--------------------
046700000000     C* RICEVIMENTO PARAMETRI
046800000000     C*--------------------
046900000000     C     *ENTRY        PLIST
047000000000     C                   PARM                    KPJBA
047100000000     C*--------------------
047200000000     C* CAMPI RIFERITI AL DATA BASE
047300000000     C*--------------------
047400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
047500000000     C*--------------------
047600000000     C* CHIAVI DI LETTURA
047700000000     C*--------------------
047800000000     C* LETTURA AZORG01L - COMPLETA
047900000000     C     KEYORG        KLIST
048000000000     C                   KFLD                    KORFIL                         *FILIALE
048100000000     C*
048200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
048300000000     C                   EXSR      INZSTD
048400000000     C*
048500000000     C* CARICA LE TABELLE OCCORRENTI
048600000000     C                   EXSR      CARTAB
048700000000     C*
048800000000     C* IMPOSTAZIONI INIZIALI VIDEO
048900000000     C                   EXSR      IMPVID
049000000000     C*
049100000000     C* EMETTE FORMATO VIDEO
049200000000     C                   EXSR      CARV1
049300000000     C*
049400000000     C                   ENDSR
049500000000     C*------------------------------------------------------------------------*
049600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
049900000000     C*    STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
050000000000     C*    NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
050100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050300000000     C*------------------------------------------------------------------------*
050400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
050500000000     C*------------------------------------------------------------------------*
050600000000     C     INZSTD        BEGSR
050700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
050800000000     C                   CALL      'XKEYS'
050900000000     C                   PARM                    DSKEYS
051000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
051100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
051200000000     C     XMSGPA        PLIST
051300000000     C                   PARM                    DSMSMS
051400000000     C                   PARM                    DSMSPG
051500000000     C                   PARM                    DSMSKE
051600000000     C                   PARM                    DSMSER
051700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
051800000000     C                   MOVEL     'TNSD87R'     DSMSPG                         *NOME PGM CHIAMANTE
051900000000     C                   MOVEL     'TNSD87R'     VMCQUE                         *CODA MESSAGGI DESTI
052000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
052100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
052200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
052300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
052400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
052500000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
052600000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
052700000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
052800000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
052900000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
053000000000     C* LANCIO ROUTINE PER VERIFICARE SE
053100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
053200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
053300020717
053400020717     c     *dtaara       define    §azute        azuteds
053500020717     c     *dtaara       define    §datiute      ddatiute
053600020717
053700020717     c                   in(E)     *dtaara
053800020717     c                   if        %Error or rsut = *blanks
053900020717     c                   clear                   tibs34ds
054000020717     c                   call      'TIBS34R'
054100020717     c                   parm                    Tibs34Ds
054200020717     c                   in        *dtaara
054300020717     c                   endif
054400020717
054500020717      * Controllo il profilo utente
054600020717     c                   ExSr      CtrUte
054700020717
054800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
054900000000     C                   BITOFF    '01234567'    POSMI1            1
055000000000     C                   BITOFF    '01234567'    POSMI2            1
055100000000     C                   BITOFF    '01234567'    POSMA1            1
055200000000     C                   BITOFF    '01234567'    POSMA2            1
055300000000     C                   BITON     '34'          POSMI1            1            *18
055400000000     C                   BITON     '7'           POSMI2            1            *01
055500000000     C                   BITON     '34'          POSMA1            1            *18
055600000000     C                   BITON     '0'           POSMA2            1            *80
055700000000     C                   MOVE      POSMI2        POSMIN            2
055800000000     C                   MOVEL     POSMI1        POSMIN
055900000000     C                   MOVE      POSMA2        POSMAX            2
056000000000     C                   MOVEL     POSMA1        POSMAX
056100000000     C                   ENDSR
056200020717      *------------------------------------------------------------------------*
056300020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
056400020717      *------------------------------------------------------------------------*
056500020717     c     CtrUte        BegSr
056600020717
056700020717     c                   Eval      *In76 = *Off
056800020717
056900020717      * Controllo se il profilo è ok da tibs34
057000020717     c                   If        DutErr = 'E'
057100020717     c                   eval      $Msg = DutMsg
057200020717     c                   ExSr      CarMsg
057300020717     c                   eval      *In76 = *On
057400020717     c                   endIf
057500020717
057600020717     c                   endSr
057700000000     C*---------------------------------------------------------------*
057800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
057900000000     C*---------------------------------------------------------------*
058000000000     C     FUNSTD        BEGSR
058100000000     C                   SELECT
058200000000     C     KEY           WHENEQ    F3
058300000000     C                   EXSR      F03STD
058400000000     C     KEY           WHENEQ    F12
058500000000     C                   EXSR      F12STD
058600000000     C     KEY           WHENEQ    F24
058700000000     C                   EXSR      F24STD
058800000000     C                   OTHER
058900000000     C                   MOVEL     MSG(2)        $MSG
059000000000     C                   EXSR      CARMSG
059100000000     C                   ENDSL
059200000000     C                   ENDSR
059300000000     C*---------------------------------------------------------------*
059400000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
059500000000     C*---------------------------------------------------------------*
059600000000     C     F03STD        BEGSR
059700000000     C*
059800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
059900000000     C                   MOVEL     '1'           $FINE
060000000000     C*
060100000000     C                   ENDSR
060200000000     C*---------------------------------------------------------------*
060300000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
060400000000     C*---------------------------------------------------------------*
060500000000     C     F12STD        BEGSR
060600000000     C*
060700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
060800000000     C                   MOVEL     '1'           $FINE
060900000000     C*
061000000000     C                   ENDSR
061100000000     C*---------------------------------------------------------------*
061200000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
061300000000     C*---------------------------------------------------------------*
061400000000     C     F24STD        BEGSR
061500000000     C*
061600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
061700000000     C                   EXSR      OUTFUN
061800000000     C*
061900000000     C                   ENDSR
062000000000     C*---------------------------------------------------------------*
062100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
062200000000     C*          CODA MESSAGGI DEL PGM
062300000000     C*---------------------------------------------------------------*
062400000000     C     CARMSG        BEGSR
062500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
062600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
062700000000     C     MSGULT        IFEQ      0
062800000000     C                   MOVEL     '1'           *IN40
062900000000     C                   WRITE     SD87DVK
063000000000     C                   MOVEL     '0'           *IN40
063100000000     C                   ENDIF
063200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
063300000000     C                   MOVEL     $MSG          DSMSMS
063400000000     C                   CALL      'XMSG'        XMSGPA
063500000000     C     DSMSER        IFEQ      0
063600000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
063700000000     C                   ENDIF
063800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
063900000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
064000000000     C                   WRITE     SD87DVM                                      *SFL MESS
064100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
064200000000     C                   MOVEL     *BLANKS       $MSG
064300000000     C                   ENDSR
064400000000     C*---------------------------------------------------------------*
064500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
064600000000     C*---------------------------------------------------------------*
064700000000     C     OUTMSG        BEGSR
064800000000     C* VISUALIZZAZIONE MESSAGGIO
064900000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
065000000000     C                   WRITE     SD87DVK                                      *CONTR MESS
065100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
065200000000     C                   Z-ADD     *ZEROS        MSGULT
065300000000     C                   ENDSR
065400000000     C*---------------------------------------------------------------*
065500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
065600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
065700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
065800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
065900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
066000000000     C*---------------------------------------------------------------*
066100000000     C     CARFUN        BEGSR
066200000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
066300000000     C     1             DO        31            I                 5 0          --- 1 -->
066400000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
066500000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
066600000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
066700020717     c                   If        DutLpo = 'S'
066800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
066900000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
067000000000     C                   ELSE                                                   *INTERROG.
067100000000     C                   MOVEL     FUNASI        FAT(I)
067200000000     C                   ENDIF
067300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
067400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
067500000000     C                   MOVEL     FUNAFG        FAT(I)
067600000000     C                   ELSE                                                   *INTERROG.
067700000000     C                   MOVEL     FUNAFI        FAT(I)
067800000000     C                   ENDIF
067900000000     C                   ENDIF                                                  <-- 2 ---
068000000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
068100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
068200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
068300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
068400000000     C                   ELSE
068500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
068600000000     C                   ENDIF
068700000000     C                   ENDIF
068800000000     C                   ENDDO                                                  <-- 1 ---
068900000000     C                   ENDSR
069000000000     C*---------------------------------------------------------------*
069100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
069200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
069300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
069400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
069500000000     C*---------------------------------------------------------------*
069600000000     C     OUTFUN        BEGSR
069700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
069800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
069900000000     C                   ENDDO
070000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
070100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
070200000000     C     1             DO        31            I
070300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
070400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
070500000000     C                   ADD       1             FUNNAT
070600000000     C                   ENDIF
070700000000     C                   ENDDO
070800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
070900000000     C     FUNNAT        IFGT      8
071000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
071100000000     C                   ENDIF
071200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
071300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
071400000000     C                   Z-ADD     8             FUNVIS            5 0
071500000000     C     FAT(23)       IFEQ      'S'
071600000000     C                   SUB       1             FUNVIS
071700000000     C                   ENDIF
071800000000     C     FAT(24)       IFEQ      'S'
071900000000     C                   SUB       1             FUNVIS
072000000000     C                   ENDIF
072100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
072200000000     C                   Z-ADD     *ZEROS        W                 5 0
072300000000     C                   ADD       1             FUNULT
072400000000     C     FUNULT        IFGT      31
072500000000     C                   Z-ADD     1             FUNULT
072600000000     C                   ENDIF
072700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
072800000000     C     FUNULT        DO        31            I                              --- 1 -->
072900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
073000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
073100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
073200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
073300000000     C     FDE(I)        ANDNE     *BLANKS
073400000000     C     I             IFEQ      23                                           *TOLGO F23
073500000000     C     I             OREQ      24                                           *E F24
073600000000     C                   ELSE
073700000000     C                   ADD       1             W
073800000000     C                   MOVEL     FDE(I)        FUV(W)
073900000000     C                   ENDIF
074000000000     C                   ENDIF                                                  <-- 3 ---
074100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
074200000000     C                   Z-ADD     I             FUNULT
074300000000     C                   ENDIF                                                  <-- 2 ---
074400000000     C                   ENDDO                                                  <-- 1 ---
074500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
074600000000     C                   MOVEL     FUV(1)        VFD001
074700000000     C                   MOVEL     FUV(2)        VFD002
074800000000     C                   MOVEL     FUV(3)        VFD003
074900000000     C                   MOVEL     FUV(4)        VFD004
075000000000     C                   MOVEL     FUV(5)        VFD005
075100000000     C                   MOVEL     FUV(6)        VFD006
075200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
075300000000     C     FAT(24)       ANDEQ     'N'
075400000000     C                   MOVEL     FUV(7)        VFD007
075500000000     C                   MOVEL     FUV(8)        VFD008
075600000000     C                   ELSE                                                   <-- 1 -->
075700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
075800000000     C     FAT(24)       ANDEQ     'S'
075900000000     C                   MOVEL     FDE(23)       VFD007
076000000000     C                   MOVEL     FDE(24)       VFD008
076100000000     C                   ELSE                                                   <-- 2 -->
076200000000     C                   MOVEL     FUV(7)        VFD007
076300000000     C     FAT(23)       IFEQ      'S'
076400000000     C                   MOVEL     FDE(23)       VFD008
076500000000     C                   ELSE
076600000000     C                   MOVEL     FDE(24)       VFD008
076700000000     C                   ENDIF
076800000000     C                   ENDIF                                                  <-- 2 ---
076900000000     C                   ENDIF                                                  <-- 1 ---
077000000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
077100000000     C                   ENDSR
077200000000     C*---------------------------------------------------------------*
077300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
077400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
077500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
077600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
077700000000     C*---------------------------------------------------------------*
077800000000     C     CONFUN        BEGSR
077900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
078000000000     C     1             DO        31            I                              --- 1 -->
078100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
078200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
078300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
078400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
078500000000     C                   EXSR      CARMSG
078600000000     C                   MOVEL     '1'           $ERR
078700000000     C                   ENDIF                                                  <-- 3 ---
078800000000     C                   ENDIF                                                  <-- 2 ---
078900000000     C                   ENDDO                                                  <-- 1 ---
079000000000     C                   ENDSR
079100000000**   SCHIERA: MSG  (MESSAGGI)
079200000000TNSD87R- Tasto funzionale non valido.                                          1
079300000000TNSD87R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
079400000000TNSD87R- Opzione non valida.                                                   3
079500000000TNSD87R- Opzione abilitata ma non gestita. Avvertire CED.                      4
079600000000TNSD87R- Inizio elenco.                                                        5
079700000000TNSD87R- Fine elenco.                                                          6
079800000000TNSD87R- Punto operativo errato o non in gestione                              7
079900000000TNSD87R- Data errata                                                           8
080000000000TNSD87R- Data iniziale maggiore di quella finale                               9
080100000000TNSD87R- Immettere 'S' per stampare il dettaglio bolle, 'N' per i soli totali 10
080200000000TNSD87R- 11
080300000000TNSD87R- 12
080400000000TNSD87R- 13
080500000000TNSD87R- 14
080600000000TNSD87R- 15
080700000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
080800000000F1    -                   -    NN     NN
080900000000F2    -                   -    NN     NN
081000000000F3    -F3=Fine            -    SS     SS
081100000000F4    -                   -    NN     NN
081200000000F5    -                   -    NN     NN
081300000000F6    -F6=Lancio          -    SS     SS
081400000000F7    -                   -    NN     NN
081500000000F8    -                   -    NN     NN
081600000000F9    -                   -    NN     NN
081700000000F10   -                   -    NN     NN
081800000000F11   -                   -    NN     NN
081900000000F12   -F12=Ritorno        -    SS     SS
082000000000F13   -                   -    NN     NN
082100000000F14   -                   -    NN     NN
082200000000F15   -                   -    NN     NN
082300000000F16   -                   -    NN     NN
082400000000F17   -                   -    NN     NN
082500000000F18   -                   -    NN     NN
082600000000F19   -                   -    NN     NN
082700000000F20   -                   -    NN     NN
082800000000F21   -                   -    NN     NN
082900000000F22   -                   -    NN     NN
083000000000F23   -                   -    NN     NN
083100000000F24   -                   -    NN     NN
083200000000ENTER -                   -    SS     SS
083300000000ROLLUP-                   -    NN     NN
083400000000ROLLDN-                   -    NN     NN
083500000000HELP  -                   -    NN     NN
083600000000PRINT -                   -    NN     NN
083700000000BSPACE-                   -    NN     NN
083800000000CLEAR -                   -    NN     NN
