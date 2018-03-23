000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
000500940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000600940620      *  03           PROTECT TUTTI I CAMPI
000700030123      *  09           PROTECT dei campi voce C/E
000800940223      *  21           GENERICO OPERAZIONI I/O
000900940223      *  22           GENERICO ERRORE OPERAZIONI I/O
001000940223      *  30           SFLDSP
001100940223      * N31           SFLCLR
001200940128      *  31           SFLDSPCTL
001300940128      *  32           SFLNXTCHG
001400940128      *  33           SFLEND
001500940128      *  39           OF PRTF
001600940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940315      *  50 <---> 98  ERRORI SU VIDEO
001900940317    > *  Specificare l'uso dei singoli indicatori
002000940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002300940128      *  99           INDIC. GENERALE DI ERRORE
002400940128     F*----------------------------------------------------*
002500040930$001 FTRTB87D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700030113     FTABEL00F  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900040930$003 D EXTDS2F       E DS                  EXTNAME(DS2F) INZ
003000030113      *-------------
003100940127     D* Passaggio Parametri
003200940127     D KPJBA         E DS
003300030113      *-------------
003400030113     D* Parametri in ricezione
003500030113     D  TABDS          DS
003600030113     D  XTAOPZ                 1      2
003700030113     D  XTARET                 3      3
003800030113     D  XTAOPR                 4      4
003900030113     D  XTAERR                 5      5
004000030113     D  XTAKEY                 6     13
004100940201     D*-------------
004200030204     D  S_TASTO        S                   like($Tasto)
004300940201     D DSFMT           DS
004400940506     D  $TASTO               369    369
004500940201     D  NRG                  370    370
004600940201     D  NCL                  371    371
004700940201     D*-------------
004800940201     D* posizione cursore
004900940201     D CURSOR          DS
005000940223     D  RIRI                   1      2B 0 INZ
005100940201     D  R$$                    2      2
005200940223     D  COCO                   3      4B 0 INZ
005300940201     D  C$$                    4      4
005400940207     D*-------------
005500940207     D* Reperimento nome PGM
005600940207     D STATUS         SDS           333
005700940207     D  DSPGM            *PROC
005800940225     D*-------------
005900940225     D* COSTANTI
006000940225     D*-------------
006100940506     D* Tasti di funzione
006200940506     D F01             C                   CONST(X'31')
006300940506     D F02             C                   CONST(X'32')
006400940506     D F03             C                   CONST(X'33')
006500940506     D F04             C                   CONST(X'34')
006600940506     D F05             C                   CONST(X'35')
006700940506     D F06             C                   CONST(X'36')
006800940506     D F07             C                   CONST(X'37')
006900940506     D F08             C                   CONST(X'38')
007000940506     D F09             C                   CONST(X'39')
007100940506     D F10             C                   CONST(X'3A')
007200940506     D F11             C                   CONST(X'3B')
007300940506     D F12             C                   CONST(X'3C')
007400940506     D F13             C                   CONST(X'B1')
007500940506     D F14             C                   CONST(X'B2')
007600940506     D F15             C                   CONST(X'B3')
007700940506     D F16             C                   CONST(X'B4')
007800940506     D F17             C                   CONST(X'B5')
007900940506     D F18             C                   CONST(X'B6')
008000940506     D F19             C                   CONST(X'B7')
008100940506     D F20             C                   CONST(X'B8')
008200940506     D F21             C                   CONST(X'B9')
008300940506     D F22             C                   CONST(X'BA')
008400940506     D F23             C                   CONST(X'BB')
008500940506     D F24             C                   CONST(X'BC')
008600940506     D ENTER           C                   CONST(X'F1')
008700940506     D ROLDWN          C                   CONST(X'F4')
008800940506     D ROLLUP          C                   CONST(X'F5')
008900940506     D*-------------
009000941108     D DATA            C                   CONST('0001-01-01')
009100030114     D Annull          C                   CONST('  ANNULLATO  ')
009200030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009300030114     D OPz_Var         C                   CONST('VARIAZIONE')
009400030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009500030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009600940127     C*----------------------------------------------------*
009700940127     C*                MAIN LINE PROGRAM
009800940127     C*----------------------------------------------------*
009900940223     C*
010000940223     C     $FINE         DOWEQ     *OFF
010100940202     C     $GEST         CASEQ     'D1'          GESD1
010200940117     C                   END
010300940117     C                   END
010400940325     C* fine programma
010500030113     c                   movel     tabds         kpjbu
010600030113     C                   Seton                                        LR
010700940131     C************************************************************
010800940510     C* GESTIONE VIDEO RECORD D1
010900940131     C************************************************************
011000940127     C     GESD1         BEGSR
011100030113      *
011200940225     C* inizializzazione videata
011300940223     C     $INZD1        IFEQ      *ON
011400940127     C                   EXSR      INZD1
011500940223     C                   MOVE      *OFF          $INZD1
011600940117     C                   END
011700030113      *
011800940225     C* emissione piede videata se proveniente da altra
011900940225     C* salvataggio valore formato di provenienza
012000940207     C     $LASTV        IFNE      'D1'
012100940614     C                   WRITE     T1
012200940202     C                   MOVE      $LASTV        $LASTG
012300940202     C                   MOVE      'D1'          $LASTV
012400940202     C                   END
012500940131     C*
012600940203     C                   SELECT
012700940203     C* Annullamento
012800030114     C* Visualizzazione
012900030114    >C     xtaopz        WHENEQ    '01'
013000030114     c                   movel     Opz_Imm       D1Fun
013100030114    >C     xtaopz        WHENEQ    '05'
013200030114     c                   movel     Opz_Vis       D1Fun
013300030114     C                   SETON                                        020306
013400030114    >C     xtaopz        WHENEQ    '04'
013500030114     c                   movel     Opz_Del       D1Fun
013600030114     C                   SETON                                        0203
013700030114     C                   SETOFF                                         06
013800940620     C* Manutenzione
013900030113    >C     xtaopz        WHENEQ    '02'
014000030114     c                   movel     Opz_Var       D1Fun
014100030114     C                   SETON                                        02
014200030114     C                   SETOFF                                       0306
014300940620     C*
014400940620     C                   OTHER
014500940620     C*
014600030114     C                   SETOFF                                       020306
014700940203     C                   ENDSL
014800030114     C*
014900030114     c                   SETOFF                                         99
015000940620     C* Immissione/Modifica/Copia
015100030113     C*               *----------------*
015200940620     C                   EXFMT     D1
015300030113     C*               *----------------*
015400030114     C*
015500030114     C     *IN99         IFEQ      *OFF
015600030114     C*
0157000301141    C                   SELECT
015800030114     C* F3=Fine
015900030114     C     $TASTO        WHENEQ    F03
016000030114     C                   EXSR      F03D1
016100030114     C* F12=Ritorno
016200030114     C     $TASTO        WHENEQ    F12
016300030114     C                   EXSR      F12D1
016400030114     C*
0165000301141O   C                   OTHER
016600030114     C*
016700030114     C                   EXSR      CTRD1
016800030113     C*
016900940616     C     *IN99         IFEQ      *OFF
017000940616     C* F6
0171000302052    C     $TASTO        ifeq      F06
0172000301152    C     $TASTO        oreq      F23
017300030204     C* A chi Trasmette la Tabella ?
017400030204     c                   Exsr      ACHI
017500940224     C* eseguo aggiornamento
017600940315    >C                   EXSR      AGGANA
017700940224     C* eseguo operazioni del dopo-aggiornamento
017800030114     C  N99              EXSR      GESAGG
017900030205     c                   end
018000030114     C*
018100030114     C                   ENDIF
018200030114     C*
0183000301142-   C                   ENDSL
018400030114     C*
0185000301142-   C                   ENDIF
018600030114     C*
018700940117     C                   ENDSR
018800940117      ************************************************************
018900940207      * INIZIALIZZAZIONE VIDEATA DATI
019000940117      ************************************************************
019100940127     C     INZD1         BEGSR
019200940131     C*
019300940615     C                   CLEAR                   D1
019400040930     C                   move      '2F'          TBLcod
019500030113     C                   MOVEl     xtakey        D1COD
019600030113     C                   MOVEl     xtakey        TBLkey
019700940207     C*
019800940224     C* valorizzo la chiave univoca per reperire i dati
019900940224     C* se non immissione
0200000301141   >C     xtaopz        IFNE      '01'
020100030204      *
020200940315    >C*-------------------------------------------------------------
020300940315    >C* impostare i campi chiave ricevuti
020400940315    >C*-------------------------------------------------------------
020500040930     C                   move      '2F'          TBLcod
020600030204     C     KTBL          chain(N)  TABEL00F
020700940614     C*
020800030113     C                   IF        %Found(Tabel00F)
020900040930     C                   MOVEL     TBLuni        EXTDS2F
021000030114     c                   if        TblFLG = '*'
021100030115     c                   seton                                        08
021200030114     C                   MOVEl     Annull        D1Ann
021300030114     C                   end
021400030113     C*
021500940224     C* riempio la videata
021600940224     C                   EXSR      RIED1
021700030113      *
021800940224     C* richiamo routine dei ctrl per decodificarla
021900940614    >C                   EXSR      CTRD1
022000030113      *
022100940224     C                   MOVE      *ALL'0'       IN5098
022200940224     C                   MOVEA     IN5098        *IN(50)
022300030113      *
022400940224     C* definizione indicatori su video
022500940224     C                   SETOFF                                       02
0226009402243    C                   SELECT
022700030114    >C     xtaopz        WHENEQ    '02'
022800940224     C                   SETON                                        02
022900030114    >C     xtaopz        WHENEQ    '05'
0230009402243-   C                   ENDSL
023100940224     C*
0232009402242-   C                   ENDIF
023300030114     C*
0234000301141   >C                   Else
023500030114     C* Se in Immissione nuovo record
023600030114     c                   exsr      NewD1
023700940223     C*
0238009402241-   C                   ENDIF
023900940224     C*
024000940117     C                   ENDSR
024100940207     C/EJECT
024200940207     C************************************************************
024300940510     C* RIEMPIMENTO VIDEATA  D1
024400940207     C************************************************************
024500030114     C     NEWD1         BEGSR
024600940207     C*
024700030114     c                   clear                   d1
024800040930     c                   move      'N'           D1ftm
024900040930     c                   move      *blanks       D1ftp
025000040930     c                   move      'N'           D1frp
025100040930     c                   move      'N'           D1gc1
025200040930     c                   move      *blanks       D1tcd
025300040930     c                   move      *blanks       D1txt
025400040930     c                   move      *blanks       D1vag
025500040930     c                   move      *blanks       D1ris
025501050511     c                   move      *blanks       D1sta
025600030114     C*
025700030114     C                   ENDSR
025800030114     C************************************************************
025900030114     C* RIEMPIMENTO VIDEATA  D1
026000030114     C************************************************************
026100030114     C     RIED1         BEGSR
026200030114     C*
026300030113     C* descrizione
026400040930     c                   eval      d1DES = §2FDES
026500040930     c                   eval      D1ftm = §2Fftm
026600040930     c                   eval      D1ftp = §2Fftp
026700040930     c                   eval      D1frp = §2Ffrp
026800040930     c                   eval      D1gc1 = §2Fgc1
026900040930     c                   eval      D1tcd = §2Ftcd
027000040930     c                   eval      D1txt = §2Ftxt
027100040930     c                   eval      D1vag = §2Fvag
027200040930     c                   eval      D1ris = §2Fris
027201050511     c                   eval      D1sta = §2Fsta
027300030204     C*
027400940207     C                   ENDSR
027500940309     C************************************************************
027600940309     C* GESTIONE F03 VIDEO D1
027700940309     C************************************************************
027800940309     C     F03D1         BEGSR
027900940309     C*
028000940309     C                   MOVE      *ON           $FINE
028100030113    >C                   MOVE      '1'           xtaret
028200940325     C*
028300940325     C                   ENDSR
028400940309     C************************************************************
028500940309     C* GESTIONE F12 VIDEO D1
028600940309     C************************************************************
028700940309     C     F12D1         BEGSR
028800940309     C*
028900940309     C                   MOVE      *ON           $FINE
029000940309     C*
029100940309     C                   ENDSR
029200940131     C************************************************************
029300940207     C* CONTROLLO VIDEATA
029400940131     C************************************************************
029500940127     C     CTRD1         BEGSR
029600940131     C*
029700940127     C                   SETOFF                                       99
029800030123      *
029900940127     C* non imposto pos. del cursore
030000940127     C                   Z-ADD     0             H1RIGA
030100940127     C                   Z-ADD     0             H1COLO
030200940621     C*
030300940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0304000301141   >C     xtaopz        IFNE      '04'
030500971110     C*
030600971110     C* controllo codice vuoto
030700971110$017 C     D1COD         IFEQ      *ALL' '
030800971110     C                   SETON                                        5099
030900971110     C                   ENDIF
031000030114     C*
031100030114     C* controllo codice vuoto
031200030114$017 C     D1DES         IFEQ      *ALL' '
031300030114     C                   SETON                                        5199
031400030114     C                   ENDIF
031401050511     C*
031402050511     C* E' possibile stampare la comunicazione di apertura giacenza in
031403050614     c* automatico solo se si stampa una giacenza per pagina
031404050511$017 C     D1sta         IFEQ      'S'
031405050511$017 C     D1GC1         ifeq      'N'
031407050511     C                   SETON                                        5399
031408050511     C                   ENDIF
031409050511     C                   ENDIF
031500940207     C*
031600940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
031700030114    >C     xtaopz        IFEQ      '01'
031800030114    >C     xtaopz        oreq      '03'
031900030114     c                   movel(p)  d1cod         tblkey
032000030114     C     KTBL          chain(N)  TABEL00F
032100030114     c                   if        %Found(TABEL00F)
032200030114     C                   SETON                                        5299
032300030114     C                   EndIF
032400030114     C                   ENDIF
032500030114      *
0326009402241-   C                   END
032700940131     C*
032800940302     C     *IN99         IFEQ      *ON
032900940510     C     $GEST         ANDEQ     'D1'
033000940117     C                   MOVEA     *IN           WIN
033100940128     C                   MOVE      *ALL'0'       IN5098           49
033200940128     C                   MOVEA     IN5098        *IN(50)
033300940614     C                   WRITE     D1
033400940117     C                   MOVEA     WIN           *IN
033500940302     C                   ENDIF
033600940127     C*
033700940117     C                   ENDSR
033800960305     C/EJECT
033900030204     C************************************************************
034000030204     C* A chi deve trasmettere
034100030204     C************************************************************
034200030204     C     ACHI          BEGSR
034300030204     C*
034400030204     c                   eval      S_Tasto = $Tasto
034500030204     C*
034600030204     c                   move      '1'           t1trs
034700030204     c                   clear                   t1fil
034800030204     c     su_win        tag
034900030204     C                   Exfmt     W1Tras
035000030204     c                   if        t1trs = '2' and
035100030204     c                             t1fil = 0
035200030204     c                   seton                                        77
035300030204     c                   goto      su_win
035400030204     c                   end
035500030204     C*
035600030204     c                   eval      $Tasto = S_Tasto
035700030204     C*
035800030204     C                   ENDSR
035900030204     C/EJECT
036000030204     C************************************************************
036100030204    >C* AGGIORNAMENTO ANAGRAFICA
036200030204     C************************************************************
036300030204    >C     FLDAGG        BEGSR
036400030204     C*
036500030204     C*  campi chiave da scrivere e aggiornare
036600030204$007 C                   MOVE      '1'           tblKUT
036700030204     C                   movel     *blanks       tblKEY
036800030204     C                   MOVEl     D1COD         tblKEY
036900030204     c                   movel     T1trs         tblFTT
037000030204     c                   move      T1fil         tblFLT
037100030204     c                   move      *zeros        tblDTR
037200030204     c                   move      *blanks       tblFTR
037300030204     c                   move      ' '           TBLFLG
037400030204     C*
037500030204     C                   ENDSR
037600940207     C/EJECT
037700940224     C************************************************************
037800940315    >C* AGGIORNAMENTO ANAGRAFICA
037900940224     C************************************************************
038000940315    >C     AGGANA        BEGSR
038100940207     C*
038200030204     C                   exsr      FLDAGG
038300030204     C*
038400940207     C                   SELECT
038500030204     C*
038600030114    >C     xtaopz        WHENEQ    '01'
038700030114    >C     xtaopz        OREQ      '03'
038800030204     C*
038900040930     C                   move      '2F'          TBLcod
039000040930    >C                   EXSR      RIEANA_2F
039100030114     c                   write     TABEL                                59
039200940318     C* se rcd non scrivibile attivo errore
039300030114    >C   59              SETON                                          99
039400030204      *
039500030114    >C     xtaopz        WHENEQ    '02'
039600030204      *
039700040930     C                   move      '2F'          TBLcod
039800030204     C     KTBL          chain     TABEL00F
039900030204     C                   exsr      FLDAGG
040000040930    >C                   EXSR      RIEANA_2F
040100030114     c                   update    TABEL                                59
040200030114    >C   59              SETON                                          99
040300030204      *
040400030114    >C     xtaopz        WHENEQ    '04'
040500040930     C                   move      '2F'          TBLcod
040600030204     C     KTBL          chain     TABEL00F
040700030114     c                   move      '*'           TBLFLG
040800030115     c                   if        $tasto = F23
040900030115     c                   delete    TABEL                                59
041000030115     c                   else
041100030114     c                   update    TABEL                                59
041200030115     c                   end
041300030114    >C   59              SETON                                          99
041400940621     C*
041500940207     C                   ENDSL
041600940203     C*
041700940203     C                   ENDSR
041800940224     C************************************************************
041900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
042000940224     C************************************************************
042100040930     C     RIEANA_2F     BEGSR
042200030114     C*
042300030114     C                   CLEAR                   tblUNI
042400030114     C* descrizione
042500040930     c                   movel     d1DES         §2FDES
042600040930     c                   movel     D1ftm         §2Fftm
042700040930     c                   movel     D1ftp         §2Fftp
042800040930     c                   movel     D1frp         §2Ffrp
042900040930     c                   movel     D1gc1         §2Fgc1
043000040930     c                   movel     D1tcd         §2Ftcd
043100040930     c                   movel     D1txt         §2Ftxt
043200040930     c                   movel     D1vag         §2Fvag
043300040930     c                   movel     D1ris         §2Fris
043301050511     c                   movel     D1sta         §2Fsta
043400030114     C*
043500040930     C                   MOVEL     EXTDS2F       TBLUNI
043600030114     C*
043700940207     C                   ENDSR
043800940224     C************************************************************
043900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
044000940224     C************************************************************
044100940224     C     GESAGG        BEGSR
044200940224     C*
044300940224     C* segnalo al pgm chiamante l'avvenuta conferma
044400030114    >C                   MOVE      *ON           xtaopr
044500940224     C* segnalo al pgm chiamante l'aver premuto l'F6
044600030114    >C                   MOVE      '0'           xtaret
044700940224     C*
0448009402241    C                   SELECT
044900940224     C* nel caso di immissione, ripristino la videata iniziale
045000030114    >C     xtaopz        WHENEQ    '01'
045100940224     C                   MOVE      *ON           $INZD1
045200030114     C                   MOVE      *ON           $FINE
045300940224     C* altrimenti ritorno al pgm chiamante
045400940224     C                   OTHER
045500940224     C                   MOVE      *ON           $FINE
0456009402241-   C                   ENDSL
045700940224     C*
045800940224     C                   ENDSR
045900940131     C************************************************************
046000940131     C* OPERAZIONI INIZIALI
046100940131     C************************************************************
046200940131     C     *INZSR        BEGSR
046300940131     C*
046400940127     C* Reperimento parametri
046500940127     C*
046600940117     C     *ENTRY        PLIST
046700940117     C                   PARM                    KPJBA
046800030113     C                   movel     kpjbu         tabds
046900030115     c                   setoff                                       08
047000030115     C*
047100940223     C* Variabili per gestione videate
047200940223     C                   MOVE      'D1'          $GEST
047300940223     C                   MOVE      *OFF          $FINE
047400940223     C                   MOVE      *ON           $INZD1
047500940224     C                   MOVE      *BLANK        $LASTG
047600940224     C                   MOVE      *BLANK        $LASTV
047700940506     C                   Z-ADD     0             $ULKD1            3 0
047800940127     C*
047900940223     C* Variabili appoggio
048000940224     C                   Z-ADD     0             CURR
048100940224     C                   Z-ADD     0             CURC
048200940207     C*
048300940207     C* Valorizzazione campi univoci testate
048400940614     C                   CLEAR                   T1
048500940614     C                   MOVEL     KNSIF         NOMSIF
048600030113     C******             MOVEL     XSCDSI        NOMDIT
048700941027     C                   MOVEL     DSPGM         NOMPGM
048800030113     C*
048900030113     C                   z-add     1             TBLkut
049000040930     C                   move      '2F'          TBLcod
049100030114     C                   movel     xtakey        TBLkey
049200940224     C*
049300940224     C* Inizializzazione/Decodifica prima videata
049400940224     C                   EXSR      INZD1
049500940224     C                   MOVE      *OFF          $INZD1
049600940127     C*
049700940117     C                   ENDSR
049800940128     C************************************************************
049900940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
050000940223     C************************************************************
050100940223     C     DEFCAM        BEGSR
050200940223     C*
050300940223     C* klist
050400030113     C     KTBL          KLIST
050500030113     C                   KFLD                    TBLkut
050600030113     C                   KFLD                    TBlCOD
050700030113     C                   KFLD                    TBlKEY
050800940207     C*
050900940223     C* Variabili per gestione videate
051000940223     C*
051100940223     C                   MOVE      *BLANK        $GEST             2
051200940223     C                   MOVE      *BLANK        $FINE             1
051300940223     C                   MOVE      *BLANK        $INZD1            1
051400940223     C                   MOVE      *BLANK        $LASTG            2
051500940223     C                   MOVE      *BLANK        $LASTV            2
051600940223     C*
051700940223     C* Variabili appoggio
051800940223     C                   Z-ADD     0             CURR              2 0
051900940223     C                   Z-ADD     0             CURC              2 0
052000940223     C                   MOVE      *ZEROS        WIN              99
052100940224     C                   MOVE      *BLANK        WTBERR            1
052200940223     C*
052300940223     C* Indici
052400940223     C                   Z-ADD     0             X                 3 0
052500940223     C                   Z-ADD     0             Y                 3 0
052600940127     C*
052700940117     C                   ENDSR
052800940128     C************************************************************
