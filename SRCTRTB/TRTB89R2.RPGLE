000100141127     H*
000200141127     H*
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
002900070628$003 D EXTDSCC       E DS                  EXTNAME(DSCC) INZ
003000070628$003 D EXTDSCA       E DS                  EXTNAME(DSCA) INZ
003100030113      *-------------
003200940127     D* Passaggio Parametri
003300940127     D KPJBA         E DS
003400030113      *-------------
003500030113     D* Parametri in ricezione
003600030113     D  TABDS          DS
003700030113     D  XTAOPZ                 1      2
003800030113     D  XTARET                 3      3
003900030113     D  XTAOPR                 4      4
004000030113     D  XTAERR                 5      5
004100030113     D  XTAKEY                 6     13
004200940201     D*-------------
004300030204     D  S_TASTO        S                   like($Tasto)
004400940201     D DSFMT           DS
004500940506     D  $TASTO               369    369
004600940201     D  NRG                  370    370
004700940201     D  NCL                  371    371
004800940201     D*-------------
004900940201     D* posizione cursore
005000940201     D CURSOR          DS
005100940223     D  RIRI                   1      2B 0 INZ
005200940201     D  R$$                    2      2
005300940223     D  COCO                   3      4B 0 INZ
005400940201     D  C$$                    4      4
005500940207     D*-------------
005600940207     D* Reperimento nome PGM
005700940207     D STATUS         SDS           333
005800940207     D  DSPGM            *PROC
005900940225     D*-------------
006000940225     D* COSTANTI
006100940225     D*-------------
006200940506     D* Tasti di funzione
006300940506     D F01             C                   CONST(X'31')
006400940506     D F02             C                   CONST(X'32')
006500940506     D F03             C                   CONST(X'33')
006600940506     D F04             C                   CONST(X'34')
006700940506     D F05             C                   CONST(X'35')
006800940506     D F06             C                   CONST(X'36')
006900940506     D F07             C                   CONST(X'37')
007000940506     D F08             C                   CONST(X'38')
007100940506     D F09             C                   CONST(X'39')
007200940506     D F10             C                   CONST(X'3A')
007300940506     D F11             C                   CONST(X'3B')
007400940506     D F12             C                   CONST(X'3C')
007500940506     D F13             C                   CONST(X'B1')
007600940506     D F14             C                   CONST(X'B2')
007700940506     D F15             C                   CONST(X'B3')
007800940506     D F16             C                   CONST(X'B4')
007900940506     D F17             C                   CONST(X'B5')
008000940506     D F18             C                   CONST(X'B6')
008100940506     D F19             C                   CONST(X'B7')
008200940506     D F20             C                   CONST(X'B8')
008300940506     D F21             C                   CONST(X'B9')
008400940506     D F22             C                   CONST(X'BA')
008500940506     D F23             C                   CONST(X'BB')
008600940506     D F24             C                   CONST(X'BC')
008700940506     D ENTER           C                   CONST(X'F1')
008800940506     D ROLDWN          C                   CONST(X'F4')
008900940506     D ROLLUP          C                   CONST(X'F5')
009000940506     D*-------------
009100941108     D DATA            C                   CONST('0001-01-01')
009200030114     D Annull          C                   CONST('  ANNULLATO  ')
009300030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009400030114     D OPz_Var         C                   CONST('VARIAZIONE')
009500030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009600030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009700940127     C*----------------------------------------------------*
009800940127     C*                MAIN LINE PROGRAM
009900940127     C*----------------------------------------------------*
010000940223     C*
010100940223     C     $FINE         DOWEQ     *OFF
010200940202     C     $GEST         CASEQ     'D1'          GESD1
010300940117     C                   END
010400940117     C                   END
010500940325     C* fine programma
010600030113     c                   movel     tabds         kpjbu
010700030113     C                   Seton                                        LR
010800940131     C************************************************************
010900940510     C* GESTIONE VIDEO RECORD D1
011000940131     C************************************************************
011100940127     C     GESD1         BEGSR
011200030113      *
011300940225     C* inizializzazione videata
011400940223     C     $INZD1        IFEQ      *ON
011500940127     C                   EXSR      INZD1
011600940223     C                   MOVE      *OFF          $INZD1
011700940117     C                   END
011800030113      *
011900940225     C* emissione piede videata se proveniente da altra
012000940225     C* salvataggio valore formato di provenienza
012100940207     C     $LASTV        IFNE      'D1'
012200940614     C                   WRITE     T1
012300940202     C                   MOVE      $LASTV        $LASTG
012400940202     C                   MOVE      'D1'          $LASTV
012500940202     C                   END
012600940131     C*
012700940203     C                   SELECT
012800940203     C* Annullamento
012900030114     C* Visualizzazione
013000030114    >C     xtaopz        WHENEQ    '01'
013100030114     c                   movel     Opz_Imm       D1Fun
013200030114    >C     xtaopz        WHENEQ    '05'
013300030114     c                   movel     Opz_Vis       D1Fun
013400030114     C                   SETON                                        020306
013500030114    >C     xtaopz        WHENEQ    '04'
013600030114     c                   movel     Opz_Del       D1Fun
013700030114     C                   SETON                                        0203
013800030114     C                   SETOFF                                         06
013900940620     C* Manutenzione
014000030113    >C     xtaopz        WHENEQ    '02'
014100030114     c                   movel     Opz_Var       D1Fun
014200030114     C                   SETON                                        02
014300030114     C                   SETOFF                                       0306
014400940620     C*
014500940620     C                   OTHER
014600940620     C*
014700030114     C                   SETOFF                                       020306
014800940203     C                   ENDSL
014900030113     C*
015000030114     c                   SETOFF                                         99
015100940620     C* Immissione/Modifica/Copia
015200030113     C*               *----------------*
015300940620     C                   EXFMT     D1
015400030113     C*               *----------------*
015500030114     C*
015600030114     C     *IN99         IFEQ      *OFF
015700030114     C*
0158000301141    C                   SELECT
015900030114     C* F3=Fine
016000030114     C     $TASTO        WHENEQ    F03
016100030114     C                   EXSR      F03D1
016200030114     C* F12=Ritorno
016300030114     C     $TASTO        WHENEQ    F12
016400030114     C                   EXSR      F12D1
016500030114     C*
0166000301141O   C                   OTHER
016700030114     C*
016800030114     C                   EXSR      CTRD1
016900030113     C*
017000940616     C     *IN99         IFEQ      *OFF
017100940616     C* F6
0172000302052    C     $TASTO        ifeq      F06
0173000301152    C     $TASTO        oreq      F23
017400030204     C* A chi Trasmette la Tabella ?
017500030204     c                   Exsr      ACHI
017600940224     C* eseguo aggiornamento
017700940315    >C                   EXSR      AGGANA
017800940224     C* eseguo operazioni del dopo-aggiornamento
017900030114     C  N99              EXSR      GESAGG
018000030205     c                   end
018100030114     C*
018200030114     C                   ENDIF
018300030114     C*
0184000301142-   C                   ENDSL
018500030114     C*
0186000301142-   C                   END
018700030114     C*
018800940117     C                   ENDSR
018900940117      ************************************************************
019000940207      * INIZIALIZZAZIONE VIDEATA DATI
019100940117      ************************************************************
019200940127     C     INZD1         BEGSR
019300940131     C*
019400940615     C                   CLEAR                   D1
019500070628     C                   move      'CA'          TBLcod
019600030113     C                   MOVEl     xtakey        D1COD
019700030113     C                   MOVEl     xtakey        TBLkey
019800940207     C*
019900940224     C* valorizzo la chiave univoca per reperire i dati
020000940224     C* se non immissione
0201000301141   >C     xtaopz        IFNE      '01'
020200030204      *
020300940315    >C*-------------------------------------------------------------
020400940315    >C* impostare i campi chiave ricevuti
020500940315    >C*-------------------------------------------------------------
020600070628     C                   move      'CA'          TBLcod
020700030204     C     KTBL          chain(N)  TABEL00F
020800940614     C*
020900030113     C                   IF        %Found(Tabel00F)
021000070628     C                   MOVEL     TBLuni        EXTDSCA
021100030114     c                   if        TblFLG = '*'
021200030115     c                   seton                                        08
021300030114     C                   MOVEl     Annull        D1Ann
021400030114     C                   end
021500940224     C* riempio la videata
021600940224     C                   EXSR      RIED1
021700030113      *
021800940224     C* richiamo routine dei ctrl per decodificarla
021900940614    >C                   EXSR      CTRD1
022000030113      *
022100960228     C                   MOVEL     *IN99         SAV99             1
022200030113      *
022300960305    >C     *IN99         IFEQ      *ON
022400960305    >C                   MOVEL     *IN99         SAV99             1
022500960305    >C                   ENDIF
022600030113      *
022700940224     C                   MOVE      *ALL'0'       IN5098
022800940224     C                   MOVEA     IN5098        *IN(50)
022900030113      *
023000940224     C* definizione indicatori su video
023100940224     C                   SETOFF                                       02
0232009402243    C                   SELECT
023300030114    >C     xtaopz        WHENEQ    '02'
023400940224     C                   SETON                                        02
023500030114    >C     xtaopz        WHENEQ    '05'
0236009402243-   C                   ENDSL
023700940224     C*
0238009402242-   C                   ENDIF
023900030114     C*
0240000301141   >C                   Else
024100030114     C* Se in Immissione nuovo record
024200030114     c                   exsr      NewD1
024300940223     C*
0244009402241-   C                   ENDIF
024500940224     C*
024600940117     C                   ENDSR
024700940207     C/EJECT
024800940207     C************************************************************
024900940510     C* RIEMPIMENTO VIDEATA  D1
025000940207     C************************************************************
025100030114     C     NEWD1         BEGSR
025200940207     C*
025300030114     c                   clear                   d1
025400070628     c                   z-add     999           d1ord
025500070628     c                   move      'N'           d1stc
025600070628     c                   movel     'Nuova'       d1des
025700070628     C*
025800030114     C                   ENDSR
025900030114     C************************************************************
026000030114     C* RIEMPIMENTO VIDEATA  D1
026100030114     C************************************************************
026200030114     C     RIED1         BEGSR
026300070626      *
026400070628     c                   eval      d1STC = 低ASTC
026500070628     c                   eval      d1DES = 低ADSC
026600070628     c                   eval      d1ORD = 低AORD
026700070628     C*
026800070628     C*  Aggancia la tabella correlata: (CC)
026900070628     C                   move      'CC'          TBLcod
027000070628     C     KTBL          chain(N)  TABEL00F
027100070628     C                   IF        %Found(Tabel00F)
027200070628     C                   MOVEL     TBLuni        EXTDSCC
027300070628     c                   eval      d1DCC = 低Cdes
027400070628     C                   else
027500070628     c                   eval      d1DCC = 'Non presente VARIA su tab.CC '+
027600070628     c                             'ma solo x statistica'
027700070628     C                   end
027800070626     C*
027900940207     C                   ENDSR
028000940309     C************************************************************
028100940309     C* GESTIONE F03 VIDEO D1
028200940309     C************************************************************
028300940309     C     F03D1         BEGSR
028400940309     C*
028500940309     C                   MOVE      *ON           $FINE
028600030113    >C                   MOVE      '1'           xtaret
028700940325     C*
028800940325     C                   ENDSR
028900940309     C************************************************************
029000940309     C* GESTIONE F12 VIDEO D1
029100940309     C************************************************************
029200940309     C     F12D1         BEGSR
029300940309     C*
029400940309     C                   MOVE      *ON           $FINE
029500940309     C*
029600940309     C                   ENDSR
029700940131     C************************************************************
029800940207     C* CONTROLLO VIDEATA
029900940131     C************************************************************
030000940127     C     CTRD1         BEGSR
030100940131     C*
030200940127     C                   SETOFF                                       99
030300030123      *
030400940127     C* non imposto pos. del cursore
030500940127     C                   Z-ADD     0             H1RIGA
030600940127     C                   Z-ADD     0             H1COLO
030700940621     C*
030800940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0309000301141   >C     xtaopz        IFNE      '04'
031000971110     C*
031100971110     C* controllo codice vuoto
031200971110$017 C     D1COD         IFEQ      *ALL' '
031300971110     C                   SETON                                        5099
031400971110     C                   ENDIF
031500030114     C*
031600030114     C* controllo codice vuoto
031700030114$017 C     D1DES         IFEQ      *ALL' '
031800030114     C                   SETON                                        5199
031900030114     C                   ENDIF
032000940207     C*
032100940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
032200030114    >C     xtaopz        IFEQ      '01'
032300030114    >C     xtaopz        oreq      '03'
032400030114     c                   movel(p)  d1cod         tblkey
032500030114     C     KTBL          chain(N)  TABEL00F
032600030114     c                   if        %Found(TABEL00F)
032700030114     C                   SETON                                        5299
032800030114     C                   EndIF
032900030114     C                   ENDIF
033000030114     C*
033100070626     C* -------------------------------
033200070626      *?  Inizio controlli sui campi
033300070626     C* controllo campi vuoti (quellli asteriscati non sono obbligatori)
033400070628     C* controllo codice vuoto ordinamento
033500070628     C*
033600070628$017 C     D1STC         ifEQ      *blank
033700070628$017 C     D1ORD         ifGt      *zero
033800070628$017 C     D1Des         orNE      *blanks
033900070628$017 C                   SETON                                        6699
034000070628$017 C                   End
034100070628$017 C                   End
034200070628      *
034300070628$017 C     D1STC         ifNE      *blank
034400070628$017 C     D1Des         andEQ     *blank
034500070628$017 C                   SETON                                        6499
034600070628$017 C                   End
034700070628     C*
034800070628$017 C     D1STC         ifEQ      'S'
034900070628$017 C     D1ORD         andEQ     *zero
035000070628$017 C                   SETON                                        6599
035100070628$017 C                   End
035200070626      *
035300070626      *?  Fine controlli sui campi
0354009402241-   C                   END
035500940131     C*
035600940302     C     *IN99         IFEQ      *ON
035700940510     C     $GEST         ANDEQ     'D1'
035800940117     C                   MOVEA     *IN           WIN
035900940128     C                   MOVE      *ALL'0'       IN5098           49
036000940128     C                   MOVEA     IN5098        *IN(50)
036100940614     C                   WRITE     D1
036200940117     C                   MOVEA     WIN           *IN
036300940302     C                   ENDIF
036400940127     C*
036500940117     C                   ENDSR
036600030204     C/EJECT
036700030204     C************************************************************
036800030204     C* A chi deve trasmettere
036900030204     C************************************************************
037000030204     C     ACHI          BEGSR
037100030204     C*
037200030204     c                   eval      S_Tasto = $Tasto
037300030204     C*
037400030204     c                   move      '1'           t1trs
037500030204     c                   clear                   t1fil
037600030204     c     su_win        tag
037700030204     C                   Exfmt     W1Tras
037800030204     c                   if        t1trs = '2' and
037900030204     c                             t1fil = 0
038000030204     c                   seton                                        77
038100030204     c                   goto      su_win
038200030204     c                   end
038300030204     C*
038400030204     c                   eval      $Tasto = S_Tasto
038500030204     C*
038600030204     C                   ENDSR
038700030204     C/EJECT
038800030204     C************************************************************
038900030204    >C* AGGIORNAMENTO ANAGRAFICA
039000030204     C************************************************************
039100030204    >C     FLDAGG        BEGSR
039200030204     C*
039300030204     C*  campi chiave da scrivere e aggiornare
039400030204$007 C                   MOVE      '1'           tblKUT
039500030204     C                   movel     *blanks       tblKEY
039600030204     C                   MOVEl     D1COD         tblKEY
039700030204     c                   movel     T1trs         tblFTT
039800030204     c                   move      T1fil         tblFLT
039900030204     c                   move      *zeros        tblDTR
040000030204     c                   move      *blanks       tblFTR
040100030204     c                   move      ' '           TBLFLG
040200030204     C*
040300030204     C                   ENDSR
040400940207     C/EJECT
040500940224     C************************************************************
040600940315    >C* AGGIORNAMENTO ANAGRAFICA
040700940224     C************************************************************
040800940315    >C     AGGANA        BEGSR
040900940207     C*
041000030204     C                   exsr      FLDAGG
041100030204     C*
041200940207     C                   SELECT
041300030204     C*
041400030114    >C     xtaopz        WHENEQ    '01'
041500030114    >C     xtaopz        OREQ      '03'
041600030204     C*
041700070628     C                   move      'CA'          TBLcod
041800070628    >C                   EXSR      RIEANA_CA
041900030114     c                   write     TABEL                                59
042000940318     C* se rcd non scrivibile attivo errore
042100030114    >C   59              SETON                                          99
042200070626      *
042300030114    >C     xtaopz        WHENEQ    '02'
042400030204      *
042500070628     C                   move      'CA'          TBLcod
042600030204     C     KTBL          chain     TABEL00F
042700030204     C                   exsr      FLDAGG
042800070628    >C                   EXSR      RIEANA_CA
042900030114     c                   update    TABEL                                59
043000030114    >C   59              SETON                                          99
043100030204      *
043200030114    >C     xtaopz        WHENEQ    '04'
043300070626      *
043400070628     C                   move      'CA'          TBLcod
043500030204     C     KTBL          chain     TABEL00F
043600030114     c                   move      '*'           TBLFLG
043700030115     c                   if        $tasto = F23
043800030115     c                   delete    TABEL                                59
043900030115     c                   else
044000030114     c                   update    TABEL                                59
044100030115     c                   end
044200030114    >C   59              SETON                                          99
044300070626     C*
044400940207     C                   ENDSL
044500940203     C*
044600940203     C                   ENDSR
044700070626     C************************************************************
044800070626     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
044900070626     C************************************************************
045000070626     C     RIEANA_CA     BEGSR
045100070626     C*
045200070626     C                   CLEAR                   tblUNI
045300070626     C*
045400070626     C*  Tabella CA  correlata
045500070628     c                   eval      低aSTC  =  d1STC
045600070628     c                   eval      低aDSC  =  d1DES
045700070628     c                   eval      低aORD  =  d1ORD
045800070626     C*
045900070626     C                   MOVEL     EXTDSCA       TBLUNI
046000070626     C*
046100070626     C                   ENDSR
046200940224     C************************************************************
046300940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
046400940224     C************************************************************
046500940224     C     GESAGG        BEGSR
046600940224     C*
046700940224     C* segnalo al pgm chiamante l'avvenuta conferma
046800030114    >C                   MOVE      *ON           xtaopr
046900940224     C* segnalo al pgm chiamante l'aver premuto l'F6
047000030114    >C                   MOVE      '0'           xtaret
047100940224     C*
0472009402241    C                   SELECT
047300940224     C* nel caso di immissione, ripristino la videata iniziale
047400030114    >C     xtaopz        WHENEQ    '01'
047500940224     C                   MOVE      *ON           $INZD1
047600030114     C                   MOVE      *ON           $FINE
047700940224     C* altrimenti ritorno al pgm chiamante
047800940224     C                   OTHER
047900940224     C                   MOVE      *ON           $FINE
0480009402241-   C                   ENDSL
048100940224     C*
048200940224     C                   ENDSR
048300940131     C************************************************************
048400940131     C* OPERAZIONI INIZIALI
048500940131     C************************************************************
048600940131     C     *INZSR        BEGSR
048700940131     C*
048800940127     C* Reperimento parametri
048900940127     C*
049000940117     C     *ENTRY        PLIST
049100940117     C                   PARM                    KPJBA
049200030113     C                   movel     kpjbu         tabds
049300030115     c                   setoff                                       08
049400030115     C*
049500940223     C* Variabili per gestione videate
049600940223     C                   MOVE      'D1'          $GEST
049700940223     C                   MOVE      *OFF          $FINE
049800940223     C                   MOVE      *ON           $INZD1
049900940224     C                   MOVE      *BLANK        $LASTG
050000940224     C                   MOVE      *BLANK        $LASTV
050100940506     C                   Z-ADD     0             $ULKD1            3 0
050200940127     C*
050300940223     C* Variabili appoggio
050400940224     C                   Z-ADD     0             CURR
050500940224     C                   Z-ADD     0             CURC
050600940207     C*
050700940207     C* Valorizzazione campi univoci testate
050800940614     C                   CLEAR                   T1
050900940614     C                   MOVEL     KNSIF         NOMSIF
051000030113     C******             MOVEL     XSCDSI        NOMDIT
051100941027     C                   MOVEL     DSPGM         NOMPGM
051200030113     C*
051300030113     C                   z-add     1             TBLkut
051400070628     C                   move      'CA'          TBLcod
051500030114     C                   movel     xtakey        TBLkey
051600940224     C*
051700940224     C* Inizializzazione/Decodifica prima videata
051800940224     C                   EXSR      INZD1
051900940224     C                   MOVE      *OFF          $INZD1
052000940127     C*
052100940117     C                   ENDSR
052200940128     C************************************************************
052300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
052400940223     C************************************************************
052500940223     C     DEFCAM        BEGSR
052600940223     C*
052700940223     C* klist
052800030113     C     KTBL          KLIST
052900030113     C                   KFLD                    TBLkut
053000030113     C                   KFLD                    TBlCOD
053100030113     C                   KFLD                    TBlKEY
053200940207     C*
053300940223     C* Variabili per gestione videate
053400940223     C*
053500940223     C                   MOVE      *BLANK        $GEST             2
053600940223     C                   MOVE      *BLANK        $FINE             1
053700940223     C                   MOVE      *BLANK        $INZD1            1
053800940223     C                   MOVE      *BLANK        $LASTG            2
053900940223     C                   MOVE      *BLANK        $LASTV            2
054000940223     C*
054100940223     C* Variabili appoggio
054200940223     C                   Z-ADD     0             CURR              2 0
054300940223     C                   Z-ADD     0             CURC              2 0
054400940223     C                   MOVE      *ZEROS        WIN              99
054500940224     C                   MOVE      *BLANK        WTBERR            1
054600940223     C*
054700940223     C* Indici
054800940223     C                   Z-ADD     0             X                 3 0
054900940223     C                   Z-ADD     0             Y                 3 0
055000940127     C*
055100940117     C                   ENDSR
055200940128     C************************************************************
