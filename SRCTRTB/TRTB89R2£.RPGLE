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
002500070628$001 FTRTB89D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700030113     FTABEL00F  UF A E           K DISK
002800940128     D*----------------------------------------------------*
003100070628$003 D EXTDSCC       E DS                  EXTNAME(DSCC) INZ
003101070628$003 D EXTDSCA       E DS                  EXTNAME(DSCA) INZ
003200030113      *-------------
003300940127     D* Passaggio Parametri
003400940127     D KPJBA         E DS
003500030113      *-------------
003600030113     D* Parametri in ricezione
003700030113     D  TABDS          DS
003800030113     D  XTAOPZ                 1      2
003900030113     D  XTARET                 3      3
004000030113     D  XTAOPR                 4      4
004100030113     D  XTAERR                 5      5
004200030113     D  XTAKEY                 6     13
004300940201     D*-------------
004400030204     D  S_TASTO        S                   like($Tasto)
004500940201     D DSFMT           DS
004600940506     D  $TASTO               369    369
004700940201     D  NRG                  370    370
004800940201     D  NCL                  371    371
004900940201     D*-------------
005000940201     D* posizione cursore
005100940201     D CURSOR          DS
005200940223     D  RIRI                   1      2B 0 INZ
005300940201     D  R$$                    2      2
005400940223     D  COCO                   3      4B 0 INZ
005500940201     D  C$$                    4      4
005600940207     D*-------------
005700940207     D* Reperimento nome PGM
005800940207     D STATUS         SDS           333
005900940207     D  DSPGM            *PROC
006000940225     D*-------------
006100940225     D* COSTANTI
006200940225     D*-------------
006300940506     D* Tasti di funzione
006400940506     D F01             C                   CONST(X'31')
006500940506     D F02             C                   CONST(X'32')
006600940506     D F03             C                   CONST(X'33')
006700940506     D F04             C                   CONST(X'34')
006800940506     D F05             C                   CONST(X'35')
006900940506     D F06             C                   CONST(X'36')
007000940506     D F07             C                   CONST(X'37')
007100940506     D F08             C                   CONST(X'38')
007200940506     D F09             C                   CONST(X'39')
007300940506     D F10             C                   CONST(X'3A')
007400940506     D F11             C                   CONST(X'3B')
007500940506     D F12             C                   CONST(X'3C')
007600940506     D F13             C                   CONST(X'B1')
007700940506     D F14             C                   CONST(X'B2')
007800940506     D F15             C                   CONST(X'B3')
007900940506     D F16             C                   CONST(X'B4')
008000940506     D F17             C                   CONST(X'B5')
008100940506     D F18             C                   CONST(X'B6')
008200940506     D F19             C                   CONST(X'B7')
008300940506     D F20             C                   CONST(X'B8')
008400940506     D F21             C                   CONST(X'B9')
008500940506     D F22             C                   CONST(X'BA')
008600940506     D F23             C                   CONST(X'BB')
008700940506     D F24             C                   CONST(X'BC')
008800940506     D ENTER           C                   CONST(X'F1')
008900940506     D ROLDWN          C                   CONST(X'F4')
009000940506     D ROLLUP          C                   CONST(X'F5')
009100940506     D*-------------
009200941108     D DATA            C                   CONST('0001-01-01')
009300030114     D Annull          C                   CONST('  ANNULLATO  ')
009400030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009500030114     D OPz_Var         C                   CONST('VARIAZIONE')
009600030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009700030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009800940127     C*----------------------------------------------------*
009900940127     C*                MAIN LINE PROGRAM
010000940127     C*----------------------------------------------------*
010100940223     C*
010200940223     C     $FINE         DOWEQ     *OFF
010300940202     C     $GEST         CASEQ     'D1'          GESD1
010400940117     C                   END
010500940117     C                   END
010600940325     C* fine programma
010700030113     c                   movel     tabds         kpjbu
010800030113     C                   Seton                                        LR
010900940131     C************************************************************
011000940510     C* GESTIONE VIDEO RECORD D1
011100940131     C************************************************************
011200940127     C     GESD1         BEGSR
011300030113      *
011400940225     C* inizializzazione videata
011500940223     C     $INZD1        IFEQ      *ON
011600940127     C                   EXSR      INZD1
011700940223     C                   MOVE      *OFF          $INZD1
011800940117     C                   END
011900030113      *
012000940225     C* emissione piede videata se proveniente da altra
012100940225     C* salvataggio valore formato di provenienza
012200940207     C     $LASTV        IFNE      'D1'
012300940614     C                   WRITE     T1
012400940202     C                   MOVE      $LASTV        $LASTG
012500940202     C                   MOVE      'D1'          $LASTV
012600940202     C                   END
012700940131     C*
012800940203     C                   SELECT
012900940203     C* Annullamento
013000030114     C* Visualizzazione
013100030114    >C     xtaopz        WHENEQ    '01'
013200030114     c                   movel     Opz_Imm       D1Fun
013300030114    >C     xtaopz        WHENEQ    '05'
013400030114     c                   movel     Opz_Vis       D1Fun
013500030114     C                   SETON                                        020306
013600030114    >C     xtaopz        WHENEQ    '04'
013700030114     c                   movel     Opz_Del       D1Fun
013800030114     C                   SETON                                        0203
013900030114     C                   SETOFF                                         06
014000940620     C* Manutenzione
014100030113    >C     xtaopz        WHENEQ    '02'
014200030114     c                   movel     Opz_Var       D1Fun
014300030114     C                   SETON                                        02
014400030114     C                   SETOFF                                       0306
014500940620     C*
014600940620     C                   OTHER
014700940620     C*
014800030114     C                   SETOFF                                       020306
014900940203     C                   ENDSL
015200030113     C*
015300030114     c                   SETOFF                                         99
015400940620     C* Immissione/Modifica/Copia
015500030113     C*               *----------------*
015600940620     C                   EXFMT     D1
015700030113     C*               *----------------*
015800030114     C*
015900030114     C     *IN99         IFEQ      *OFF
016000030114     C*
0161000301141    C                   SELECT
016200030114     C* F3=Fine
016300030114     C     $TASTO        WHENEQ    F03
016400030114     C                   EXSR      F03D1
016500030114     C* F12=Ritorno
016600030114     C     $TASTO        WHENEQ    F12
016700030114     C                   EXSR      F12D1
016800030114     C*
0169000301141O   C                   OTHER
017000030114     C*
017100030114     C                   EXSR      CTRD1
019200030113     C*
019300940616     C     *IN99         IFEQ      *OFF
019400940616     C* F6
0195000302052    C     $TASTO        ifeq      F06
0196000301152    C     $TASTO        oreq      F23
019700030204     C* A chi Trasmette la Tabella ?
019800030204     c                   Exsr      ACHI
019900940224     C* eseguo aggiornamento
020000940315    >C                   EXSR      AGGANA
020100940224     C* eseguo operazioni del dopo-aggiornamento
020200030114     C  N99              EXSR      GESAGG
020300030205     c                   end
020400030114     C*
020500030114     C                   ENDIF
020600030114     C*
0211000301142-   C                   ENDSL
021200030114     C*
0213000301142-   C                   END
021400030114     C*
021500940117     C                   ENDSR
021600940117      ************************************************************
021700940207      * INIZIALIZZAZIONE VIDEATA DATI
021800940117      ************************************************************
021900940127     C     INZD1         BEGSR
022000940131     C*
022100940615     C                   CLEAR                   D1
022300070628     C                   move      'CA'          TBLcod
022400030113     C                   MOVEl     xtakey        D1COD
022600030113     C                   MOVEl     xtakey        TBLkey
022700940207     C*
022800940224     C* valorizzo la chiave univoca per reperire i dati
022900940224     C* se non immissione
0230000301141   >C     xtaopz        IFNE      '01'
023100030204      *
023200940315    >C*-------------------------------------------------------------
023300940315    >C* impostare i campi chiave ricevuti
023400940315    >C*-------------------------------------------------------------
023500070628     C                   move      'CA'          TBLcod
023600030204     C     KTBL          chain(N)  TABEL00F
023700940614     C*
023800030113     C                   IF        %Found(Tabel00F)
023900070628     C                   MOVEL     TBLuni        EXTDSCA
024000030114     c                   if        TblFLG = '*'
024100030115     c                   seton                                        08
024200030114     C                   MOVEl     Annull        D1Ann
024400030114     C                   end
026000940224     C* riempio la videata
026100940224     C                   EXSR      RIED1
026200030113      *
026300940224     C* richiamo routine dei ctrl per decodificarla
026400940614    >C                   EXSR      CTRD1
026500030113      *
026600960228     C                   MOVEL     *IN99         SAV99             1
027000030113      *
027100960305    >C     *IN99         IFEQ      *ON
027200960305    >C                   MOVEL     *IN99         SAV99             1
027300960305    >C                   ENDIF
027400030113      *
027500940224     C                   MOVE      *ALL'0'       IN5098
027600940224     C                   MOVEA     IN5098        *IN(50)
027700030113      *
027800940224     C* definizione indicatori su video
027900940224     C                   SETOFF                                       02
0280009402243    C                   SELECT
028100030114    >C     xtaopz        WHENEQ    '02'
028200940224     C                   SETON                                        02
028300030114    >C     xtaopz        WHENEQ    '05'
0284009402243-   C                   ENDSL
028500940224     C*
0286009402242-   C                   ENDIF
028700030114     C*
0288000301141   >C                   Else
028900030114     C* Se in Immissione nuovo record
029000030114     c                   exsr      NewD1
029100940223     C*
0292009402241-   C                   ENDIF
029300940224     C*
029400940117     C                   ENDSR
029500940207     C/EJECT
029600940207     C************************************************************
029700940510     C* RIEMPIMENTO VIDEATA  D1
029800940207     C************************************************************
029900030114     C     NEWD1         BEGSR
030000940207     C*
030100030114     c                   clear                   d1
030101070628     c                   z-add     999           d1ord
030102070628     c                   move      'N'           d1stc
030103070628     c                   movel     'Nuova'       d1des
031701070628     C*
031900030114     C                   ENDSR
032000030114     C************************************************************
032100030114     C* RIEMPIMENTO VIDEATA  D1
032200030114     C************************************************************
032300030114     C     RIED1         BEGSR
034700070626      *
036200070628     c                   eval      d1STC = 低ASTC
036300070628     c                   eval      d1DES = 低ADSC
036400070628     c                   eval      d1ORD = 低AORD
036401070628     C*
036402070628     C*  Aggancia la tabella correlata: (CC)
036403070628     C                   move      'CC'          TBLcod
036404070628     C     KTBL          chain(N)  TABEL00F
036405070628     C                   IF        %Found(Tabel00F)
036406070628     C                   MOVEL     TBLuni        EXTDSCC
036407070628     c                   eval      d1DCC = 低Cdes
036408070628     C                   else
036409070628     c                   eval      d1DCC = 'Non presente VARIA su tab.CC '+
036410070628     c                             'ma solo x statistica'
036411070628     C                   end
036500070626     C*
036600940207     C                   ENDSR
036700940309     C************************************************************
036800940309     C* GESTIONE F03 VIDEO D1
036900940309     C************************************************************
037000940309     C     F03D1         BEGSR
037100940309     C*
037200940309     C                   MOVE      *ON           $FINE
037300030113    >C                   MOVE      '1'           xtaret
037400940325     C*
037500940325     C                   ENDSR
037600940309     C************************************************************
037700940309     C* GESTIONE F12 VIDEO D1
037800940309     C************************************************************
037900940309     C     F12D1         BEGSR
038000940309     C*
038100940309     C                   MOVE      *ON           $FINE
038200940309     C*
038300940309     C                   ENDSR
038400940131     C************************************************************
038500940207     C* CONTROLLO VIDEATA
038600940131     C************************************************************
038700940127     C     CTRD1         BEGSR
038800940131     C*
038900940127     C                   SETOFF                                       99
039000030123      *
039100940127     C* non imposto pos. del cursore
039200940127     C                   Z-ADD     0             H1RIGA
039300940127     C                   Z-ADD     0             H1COLO
039400940621     C*
039500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0396000301141   >C     xtaopz        IFNE      '04'
039700971110     C*
039800971110     C* controllo codice vuoto
039900971110$017 C     D1COD         IFEQ      *ALL' '
040000971110     C                   SETON                                        5099
040300971110     C                   ENDIF
040400030114     C*
040500030114     C* controllo codice vuoto
040600030114$017 C     D1DES         IFEQ      *ALL' '
040700030114     C                   SETON                                        5199
041000030114     C                   ENDIF
041100940207     C*
041200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
041300030114    >C     xtaopz        IFEQ      '01'
041400030114    >C     xtaopz        oreq      '03'
041500030114     c                   movel(p)  d1cod         tblkey
041600030114     C     KTBL          chain(N)  TABEL00F
041700030114     c                   if        %Found(TABEL00F)
041800030114     C                   SETON                                        5299
041900030114     C                   EndIF
042000030114     C                   ENDIF
042100030114     C*
042200070626     C* -------------------------------
042300070626      *?  Inizio controlli sui campi
042400070626     C* controllo campi vuoti (quellli asteriscati non sono obbligatori)
042401070628     C* controllo codice vuoto ordinamento
042402070628     C*
042403070628$017 C     D1STC         ifEQ      *blank
042404070628$017 C     D1ORD         ifGt      *zero
042405070628$017 C     D1Des         orNE      *blanks
042406070628$017 C                   SETON                                        6699
042407070628$017 C                   End
042408070628$017 C                   End
042409070628      *
042410070628$017 C     D1STC         ifNE      *blank
042411070628$017 C     D1Des         andEQ     *blank
042412070628$017 C                   SETON                                        6499
042413070628$017 C                   End
042414070628     C*
042415070628$017 C     D1STC         ifEQ      'S'
042416070628$017 C     D1ORD         andEQ     *zero
042417070628$017 C                   SETON                                        6599
042418070628$017 C                   End
045200070626      *
045300070626      *?  Fine controlli sui campi
0455009402241-   C                   END
045600940131     C*
045700940302     C     *IN99         IFEQ      *ON
045800940510     C     $GEST         ANDEQ     'D1'
045900940117     C                   MOVEA     *IN           WIN
046000940128     C                   MOVE      *ALL'0'       IN5098           49
046100940128     C                   MOVEA     IN5098        *IN(50)
046200940614     C                   WRITE     D1
046300940117     C                   MOVEA     WIN           *IN
046400940302     C                   ENDIF
046500940127     C*
046600940117     C                   ENDSR
050300030204     C/EJECT
050400030204     C************************************************************
050500030204     C* A chi deve trasmettere
050600030204     C************************************************************
050700030204     C     ACHI          BEGSR
050800030204     C*
050900030204     c                   eval      S_Tasto = $Tasto
051000030204     C*
051100030204     c                   move      '1'           t1trs
051200030204     c                   clear                   t1fil
051300030204     c     su_win        tag
051400030204     C                   Exfmt     W1Tras
051500030204     c                   if        t1trs = '2' and
051600030204     c                             t1fil = 0
051700030204     c                   seton                                        77
051800030204     c                   goto      su_win
051900030204     c                   end
052000030204     C*
052100030204     c                   eval      $Tasto = S_Tasto
052200030204     C*
052300030204     C                   ENDSR
052400030204     C/EJECT
052500030204     C************************************************************
052600030204    >C* AGGIORNAMENTO ANAGRAFICA
052700030204     C************************************************************
052800030204    >C     FLDAGG        BEGSR
052900030204     C*
053000030204     C*  campi chiave da scrivere e aggiornare
053100030204$007 C                   MOVE      '1'           tblKUT
053200030204     C                   movel     *blanks       tblKEY
053300030204     C                   MOVEl     D1COD         tblKEY
053400030204     c                   movel     T1trs         tblFTT
053500030204     c                   move      T1fil         tblFLT
053600030204     c                   move      *zeros        tblDTR
053700030204     c                   move      *blanks       tblFTR
053800030204     c                   move      ' '           TBLFLG
053900030204     C*
054000030204     C                   ENDSR
054100940207     C/EJECT
054200940224     C************************************************************
054300940315    >C* AGGIORNAMENTO ANAGRAFICA
054400940224     C************************************************************
054500940315    >C     AGGANA        BEGSR
054600940207     C*
054700030204     C                   exsr      FLDAGG
054800030204     C*
054900940207     C                   SELECT
055000030204     C*
055100030114    >C     xtaopz        WHENEQ    '01'
055200030114    >C     xtaopz        OREQ      '03'
055300030204     C*
055400070628     C                   move      'CA'          TBLcod
055500070628    >C                   EXSR      RIEANA_CA
055600030114     c                   write     TABEL                                59
055700940318     C* se rcd non scrivibile attivo errore
055800030114    >C   59              SETON                                          99
057800070626      *
057900030114    >C     xtaopz        WHENEQ    '02'
058000030204      *
058100070628     C                   move      'CA'          TBLcod
058200030204     C     KTBL          chain     TABEL00F
058300030204     C                   exsr      FLDAGG
058400070628    >C                   EXSR      RIEANA_CA
058500030114     c                   update    TABEL                                59
058600030114    >C   59              SETON                                          99
062100030204      *
062200030114    >C     xtaopz        WHENEQ    '04'
062300070626      *
062400070628     C                   move      'CA'          TBLcod
062500030204     C     KTBL          chain     TABEL00F
062600030114     c                   move      '*'           TBLFLG
062700030115     c                   if        $tasto = F23
062800030115     c                   delete    TABEL                                59
062900030115     c                   else
063000030114     c                   update    TABEL                                59
063100030115     c                   end
063200030114    >C   59              SETON                                          99
068800070626     C*
068900940207     C                   ENDSL
069000940203     C*
069100940203     C                   ENDSR
075000070626     C************************************************************
075100070626     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
075200070626     C************************************************************
075300070626     C     RIEANA_CA     BEGSR
075400070626     C*
075500070626     C                   CLEAR                   tblUNI
075600070626     C*
075700070626     C*  Tabella CA  correlata
075800070628     c                   eval      低aSTC  =  d1STC
075900070628     c                   eval      低aDSC  =  d1DES
076000070628     c                   eval      低aORD  =  d1ORD
076100070626     C*
076200070626     C                   MOVEL     EXTDSCA       TBLUNI
076300070626     C*
076400070626     C                   ENDSR
076500940224     C************************************************************
076600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
076700940224     C************************************************************
076800940224     C     GESAGG        BEGSR
076900940224     C*
077000940224     C* segnalo al pgm chiamante l'avvenuta conferma
077100030114    >C                   MOVE      *ON           xtaopr
077200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
077300030114    >C                   MOVE      '0'           xtaret
077400940224     C*
0775009402241    C                   SELECT
077600940224     C* nel caso di immissione, ripristino la videata iniziale
077700030114    >C     xtaopz        WHENEQ    '01'
077800940224     C                   MOVE      *ON           $INZD1
077900030114     C                   MOVE      *ON           $FINE
078000940224     C* altrimenti ritorno al pgm chiamante
078100940224     C                   OTHER
078200940224     C                   MOVE      *ON           $FINE
0783009402241-   C                   ENDSL
078400940224     C*
078500940224     C                   ENDSR
078600940131     C************************************************************
078700940131     C* OPERAZIONI INIZIALI
078800940131     C************************************************************
078900940131     C     *INZSR        BEGSR
079000940131     C*
079100940127     C* Reperimento parametri
079200940127     C*
079300940117     C     *ENTRY        PLIST
079400940117     C                   PARM                    KPJBA
079500030113     C                   movel     kpjbu         tabds
079600030115     c                   setoff                                       08
079700030115     C*
079800940223     C* Variabili per gestione videate
079900940223     C                   MOVE      'D1'          $GEST
080000940223     C                   MOVE      *OFF          $FINE
080100940223     C                   MOVE      *ON           $INZD1
080200940224     C                   MOVE      *BLANK        $LASTG
080300940224     C                   MOVE      *BLANK        $LASTV
080400940506     C                   Z-ADD     0             $ULKD1            3 0
080500940127     C*
080600940223     C* Variabili appoggio
080700940224     C                   Z-ADD     0             CURR
080800940224     C                   Z-ADD     0             CURC
080900940207     C*
081000940207     C* Valorizzazione campi univoci testate
081100940614     C                   CLEAR                   T1
081200940614     C                   MOVEL     KNSIF         NOMSIF
081300030113     C******             MOVEL     XSCDSI        NOMDIT
081400941027     C                   MOVEL     DSPGM         NOMPGM
081500030113     C*
081600030113     C                   z-add     1             TBLkut
081700070628     C                   move      'CA'          TBLcod
081800030114     C                   movel     xtakey        TBLkey
081900940224     C*
082000940224     C* Inizializzazione/Decodifica prima videata
082100940224     C                   EXSR      INZD1
082200940224     C                   MOVE      *OFF          $INZD1
082300940127     C*
082400940117     C                   ENDSR
082500940128     C************************************************************
082600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
082700940223     C************************************************************
082800940223     C     DEFCAM        BEGSR
082900940223     C*
083000940223     C* klist
083100030113     C     KTBL          KLIST
083200030113     C                   KFLD                    TBLkut
083300030113     C                   KFLD                    TBlCOD
083400030113     C                   KFLD                    TBlKEY
083500940207     C*
083600940223     C* Variabili per gestione videate
083700940223     C*
083800940223     C                   MOVE      *BLANK        $GEST             2
083900940223     C                   MOVE      *BLANK        $FINE             1
084000940223     C                   MOVE      *BLANK        $INZD1            1
084100940223     C                   MOVE      *BLANK        $LASTG            2
084200940223     C                   MOVE      *BLANK        $LASTV            2
084300940223     C*
084400940223     C* Variabili appoggio
084500940223     C                   Z-ADD     0             CURR              2 0
084600940223     C                   Z-ADD     0             CURC              2 0
084700940223     C                   MOVE      *ZEROS        WIN              99
084800940224     C                   MOVE      *BLANK        WTBERR            1
084900940223     C*
085000940223     C* Indici
085100940223     C                   Z-ADD     0             X                 3 0
085200940223     C                   Z-ADD     0             Y                 3 0
085300940127     C*
085400940117     C                   ENDSR
085500940128     C************************************************************
