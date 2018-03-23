000100970214     H DECEDIT('0,') DATEDIT(*DMY.)
000200940223      *
000300940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000400940620      *  03           PROTECT TUTTI I CAMPI
000500030123      *  09           PROTECT dei campi voce C/E
000600940223      *  21           GENERICO OPERAZIONI I/O
000700940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940223      *  30           SFLDSP
000900940223      * N31           SFLCLR
001000940128      *  31           SFLDSPCTL
001100940128      *  32           SFLNXTCHG
001200940128      *  33           SFLEND
001300940128      *  39           OF PRTF
001400940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940317    > *  Specificare l'uso dei singoli indicatori
001600940315      *  50 <---> 98  ERRORI SU VIDEO
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
001900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002100940128      *  99           INDIC. GENERALE DI ERRORE
002200940128     F*----------------------------------------------------*
002300070626$001 FTRTB88D2  CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500030113     FTABEL00F  UF A E           K DISK
002600940128     D*----------------------------------------------------*
002700070626$003 D EXTDSCC       E DS                  EXTNAME(DSCC) INZ
002800070626$003 D EXTDSCB       E DS                  EXTNAME(DSCB) INZ
002900070626$003 D EXTDSCA       E DS                  EXTNAME(DSCA) INZ
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
014900030114     c                   movel     D1Fun         D2Fun
015000030113     C*
015100030114     c                   SETOFF                                         99
015200940620     C* Immissione/Modifica/Copia
015300030113     C*               *----------------*
015400940620     C                   EXFMT     D1
015500030113     C*               *----------------*
015600030114     C*
015700030114     C     *IN99         IFEQ      *OFF
015800030114     C*
0159000301141    C                   SELECT
016000030114     C* F3=Fine
016100030114     C     $TASTO        WHENEQ    F03
016200030114     C                   EXSR      F03D1
016300030114     C* F12=Ritorno
016400030114     C     $TASTO        WHENEQ    F12
016500030114     C                   EXSR      F12D1
016600030114     C*
0167000301141O   C                   OTHER
016800030114     C*
016900030114     C                   EXSR      CTRD1
017000030114     C* Video 2
017100940506     C     *IN99         IFEQ      *OFF
017200030204     c     Video2        tag
017300030205     c                   setoff                                       99
017400030114     C*               *----------------*
017500030114     C                   EXFMT     D2
017600030114     C*               *----------------*
017700940131     C*
0178009402031    C                   SELECT
017900940131     C* F3=Fine
018000940506     C     $TASTO        WHENEQ    F03
018100940309     C                   EXSR      F03D1
018200940131     C* F12=Ritorno
018300940506     C     $TASTO        WHENEQ    F12
018400940309     C                   EXSR      F12D1
018500940131     C*
0186009402031O   C                   OTHER
018700030113     C*
018800030114     C                   EXSR      CTRD2
018900030205     c   99              goto      video2
019000030113     C*
019100940616     C     *IN99         IFEQ      *OFF
019200940616     C* F6
0193000302052    C     $TASTO        ifeq      F06
0194000301152    C     $TASTO        oreq      F23
019500030204     C* A chi Trasmette la Tabella ?
019600030204     c                   Exsr      ACHI
019700940224     C* eseguo aggiornamento
019800940315    >C                   EXSR      AGGANA
019900940224     C* eseguo operazioni del dopo-aggiornamento
020000030114     C  N99              EXSR      GESAGG
020100030205     c                   end
020200030114     C*
020300030114     C                   ENDIF
020400030114     C*
0205000301142-   C                   ENDSL
020600030114     C*
0207000301142-   C                   ENDIF
020800940224     C*
0209000301142-   C                   ENDSL
021000030114     C*
0211000301142-   C                   END
021200030114     C*
021300940117     C                   ENDSR
021400940117      ************************************************************
021500940207      * INIZIALIZZAZIONE VIDEATA DATI
021600940117      ************************************************************
021700940127     C     INZD1         BEGSR
021800940131     C*
021900940615     C                   CLEAR                   D1
022000030113     C                   CLEAR                   D2
022100070626     C                   move      'CC'          TBLcod
022200030113     C                   MOVEl     xtakey        D1COD
022300030113     C                   MOVEl     xtakey        D2COD
022400030113     C                   MOVEl     xtakey        TBLkey
022500940207     C*
022600940224     C* valorizzo la chiave univoca per reperire i dati
022700940224     C* se non immissione
0228000301141   >C     xtaopz        IFNE      '01'
022900030204      *
023000940315    >C*-------------------------------------------------------------
023100940315    >C* impostare i campi chiave ricevuti
023200940315    >C*-------------------------------------------------------------
023300070626     C                   move      'CC'          TBLcod
023400030204     C     KTBL          chain(N)  TABEL00F
023500940614     C*
023600030113     C                   IF        %Found(Tabel00F)
023700070626     C                   MOVEL     TBLuni        EXTDSCC
023800030114     c                   if        TblFLG = '*'
023900030115     c                   seton                                        08
024000030114     C                   MOVEl     Annull        D1Ann
024100030114     C                   MOVEl     D1Ann         D2Ann
024200030114     C                   end
024300030204     C*
024400070626     C*  Aggancia la tabella correlata: (CB)
024500070626     C                   move      'CB'          TBLcod
024600030204     C     KTBL          chain(N)  TABEL00F
024700030204     C                   IF        %Found(Tabel00F)
024800070626     C                   MOVEL     TBLuni        EXTDSCB
024900030204     C                   end
025000070626     C*
025100070626     C*  Aggancia la tabella correlata: (CA)
025200070626     C                   move      'CA'          TBLcod
025300070626     C     KTBL          chain(N)  TABEL00F
025400070626     C                   IF        %Found(Tabel00F)
025500070626     C                   MOVEL     TBLuni        EXTDSCA
025600070626     C                   end
025700030113     C*
025800940224     C* riempio la videata
025900940224     C                   EXSR      RIED1
026000030113      *
026100940224     C* richiamo routine dei ctrl per decodificarla
026200940614    >C                   EXSR      CTRD1
026300030113      *
026400960228     C                   MOVEL     *IN99         SAV99             1
026500030113      *
026600960228     C* Se la gestione è più videate
026700960305    >C                   EXSR      CTRD2
026800030113      *
026900960305    >C     *IN99         IFEQ      *ON
027000960305    >C                   MOVEL     *IN99         SAV99             1
027100960305    >C                   ENDIF
027200030113      *
027300940224     C                   MOVE      *ALL'0'       IN5098
027400940224     C                   MOVEA     IN5098        *IN(50)
027500030113      *
027600940224     C* definizione indicatori su video
027700940224     C                   SETOFF                                       02
0278009402243    C                   SELECT
027900030114    >C     xtaopz        WHENEQ    '02'
028000940224     C                   SETON                                        02
028100030114    >C     xtaopz        WHENEQ    '05'
0282009402243-   C                   ENDSL
028300940224     C*
0284009402242-   C                   ENDIF
028500030114     C*
0286000301141   >C                   Else
028700030114     C* Se in Immissione nuovo record
028800030114     c                   exsr      NewD1
028900940223     C*
0290009402241-   C                   ENDIF
029100940224     C*
029200940117     C                   ENDSR
029300940207     C/EJECT
029400940207     C************************************************************
029500940510     C* RIEMPIMENTO VIDEATA  D1
029600940207     C************************************************************
029700030114     C     NEWD1         BEGSR
029800940207     C*
029900030114     c                   clear                   d1
030000030114     c                   clear                   d2
030100070626     c                   move      *zeros        D1JCI
030200070626     c                   move      *zeros        D1JSI
030300070626     c                   movel     '0000'        D1JVI
030400070626     c                   move      *zeros        D1JCE
030500070626     c                   move      *zeros        D1JSE
030600070626     c                   movel     '0000'        D1JVE
030700080605     c*******            move      'N'           D1FLI
030800070626     c                   move      'N'           D1FLP
030900070626     c                   move      'N'           D1FL1
031000070626     c                   move      'N'           D1FL2
031100070626     c                   move      'N'           D1FL3
031200070626     c                   move      'N'           D1FL4
031300070626     c                   move      'N'           D1FL5
031400070626     c                   move      'N'           D1FL6
031500070626     c                   move      *zeros        D1CMP
031600030114     C*
031700030114     C                   ENDSR
031800030114     C************************************************************
031900030114     C* RIEMPIMENTO VIDEATA  D1
032000030114     C************************************************************
032100030114     C     RIED1         BEGSR
032200030114     C*
032300070626     c                   eval      d1DES = §CCDES
032400070626      *
032500080605     c*******            if        §CCFLI = 1
032600080605     c*******            eval      d1FLI = 'S'
032700080605     c*******            else
032800080605     c*******            eval      d1FLI = 'N'
032900080605     c*******            end
033000070626      *
033100070626     c                   eval      d1JEI = §CCJEI
033200070626     c                   eval      d1JCI = §CCJCI
033300070626     c                   eval      d1JSI = §CCJSI
033400070626     c                   eval      d1JVI = §CCJVI
033500070626     c                   eval      d1JCE = §CCJCE
033600070626     c                   eval      d1JSE = §CCJSE
033700070626     c                   eval      d1JVE = §CCJVE
033800070626     c                   eval      d1FL0 = §CCFL0
033900070626      *
034000070626     c                   if        §CCFLP = 1
034100070626     c                   eval      d1FLP = 'S'
034200070626     c                   else
034300070626     c                   eval      d1FLP = 'N'
034400070626     c                   end
034500070626      *
034600070626     c                   eval      d1CEI = §CCCEI
034700070626     c                   eval      d1FL1 = §CCFL1
034800070626     c                   eval      d1FL2 = §CCFL2
034900070626     c                   eval      d1FL3 = §CCFL3
035000070626     c                   eval      d1FL4 = §CCFL4
035100070626     c                   eval      d1FL5 = §CCFL5
035200070626     c                   eval      d1FL6 = §CCFL6
035300160121     c                   eval      d1RDL = §CCrdl
035400070626     c                   eval      d1CMP = §CCCMP
035500070626     c                   eval      d1UTI = §CCUTI
035600070626     c                   eval      d1DFE = §CCDFE
035700070626     c                   eval      d1SV0 = §CCSV0
035800030204     C*
035900070626     c                   eval      d1DSF = §CBDSF
036000070626     C*
036100070626     c                   eval      d2STC = §CASTC
036200070626     c                   eval      d2DSC = §CADSC
036300070626     c                   eval      d2ORD = §CAORD
036400070626     C*
036500940207     C                   ENDSR
036600940309     C************************************************************
036700940309     C* GESTIONE F03 VIDEO D1
036800940309     C************************************************************
036900940309     C     F03D1         BEGSR
037000940309     C*
037100940309     C                   MOVE      *ON           $FINE
037200030113    >C                   MOVE      '1'           xtaret
037300940325     C*
037400940325     C                   ENDSR
037500940309     C************************************************************
037600940309     C* GESTIONE F12 VIDEO D1
037700940309     C************************************************************
037800940309     C     F12D1         BEGSR
037900940309     C*
038000940309     C                   MOVE      *ON           $FINE
038100940309     C*
038200940309     C                   ENDSR
038300940131     C************************************************************
038400940207     C* CONTROLLO VIDEATA
038500940131     C************************************************************
038600940127     C     CTRD1         BEGSR
038700940131     C*
038800940127     C                   SETOFF                                       99
038900030123      *
039000940127     C* non imposto pos. del cursore
039100940127     C                   Z-ADD     0             H1RIGA
039200940127     C                   Z-ADD     0             H1COLO
039300940621     C*
039400940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0395000301141   >C     xtaopz        IFNE      '04'
039600971110     C*
039700971110     C* controllo codice vuoto
039800971110$017 C     D1COD         IFEQ      *ALL' '
039900971110     C                   SETON                                        5099
040000030114     C                   Else
040100030114     c                   eval      d2cod = d1cod
040200971110     C                   ENDIF
040300030114     C*
040400030114     C* controllo codice vuoto
040500030114$017 C     D1DES         IFEQ      *ALL' '
040600030114     C                   SETON                                        5199
040700030114     C                   Else
040800030114     c                   eval      d2des = d1des
040900030114     C                   ENDIF
041000940207     C*
041100940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
041200030114    >C     xtaopz        IFEQ      '01'
041300030114    >C     xtaopz        oreq      '03'
041400030114     c                   movel(p)  d1cod         tblkey
041500030114     C     KTBL          chain(N)  TABEL00F
041600030114     c                   if        %Found(TABEL00F)
041700030114     C                   SETON                                        5299
041800030114     C                   EndIF
041900030114     C                   ENDIF
042000030114     C*
042100070626     C* -------------------------------
042200070626      *?  Inizio controlli sui campi
042300070626     C* controllo campi vuoti (quellli asteriscati non sono obbligatori)
042400070626$017 C     D1JEI         IFEQ      *blank
042500070626$017 C*******            SETON                                        5399
042600070626$017 C                   End
042700030114      *
042800070626$017 C     D1JCI         IFEQ      *blank
042900070626$017 C                   SETON                                        5499
043000070626$017 C                   End
043100070626      *
043200070626$017 C     D1JSI         IFEQ      *blank
043300070626$017 C                   SETON                                        5599
043400070626$017 C                   End
043500070626      *
043600070626$017 C     D1JVI         IFEQ      *blank
043700070626$017 C                   SETON                                        5699
043800070626$017 C                   End
043900070626      *
044000070626$017 C     D1JCE         IFEQ      *blank
044100070626$017 C                   SETON                                        5799
044200070626$017 C                   End
044300070626      *
044400070626$017 C     D1JSE         IFEQ      *blank
044500070626$017 C                   SETON                                        5899
044600070626$017 C                   End
044700070626      *
044800070626$017 C     D1JVE         IFEQ      *blank
044900070626$017 C                   SETON                                        6099
045000070626$017 C                   End
045100070626      *
045200070626      *?  Fine controlli sui campi
045300070626      *
0454009402241-   C                   END
045500940131     C*
045600940302     C     *IN99         IFEQ      *ON
045700940510     C     $GEST         ANDEQ     'D1'
045800940117     C                   MOVEA     *IN           WIN
045900940128     C                   MOVE      *ALL'0'       IN5098           49
046000940128     C                   MOVEA     IN5098        *IN(50)
046100940614     C                   WRITE     D1
046200940117     C                   MOVEA     WIN           *IN
046300940302     C                   ENDIF
046400940127     C*
046500940117     C                   ENDSR
046600960305     C/EJECT
046700960305     C************************************************************
046800960305     C* CONTROLLO VIDEATA D2
046900960305     C************************************************************
047000960305     C     CTRD2         BEGSR
047100030205     C*
047200070627      * FEDEX
047300070627$017 C     D1DSF         IFne      *blank
047400070627$017 C     D1DFE         andEQ     *blank
047500070627$017 C                   SETON                                        6199
047600070627$017 C                   End
047700070627      *
047800070627$017 C     D1DSF         IFEQ      *blank
047900070627$017 C     D1DFE         andeq     'S'
048000070627$017 C                   SETON                                        6299
048100070627$017 C                   End
048200070626     C* controllo codice vuoto ordinamento
048300070626     C*
048400070626$017 C     D2STC         ifEQ      *blank
048500070626$017 C     D2ORD         ifGt      *zero
048600070626$017 C     D2DSC         orNE      *blanks
048700070626$017 C                   SETON                                        6699
048800070626$017 C                   End
048900070626$017 C                   End
049000070626      *
049100070626$017 C     D2STC         ifNE      *blank
049200070626$017 C     D2DSC         andEQ     *blank
049300070626$017 C                   SETON                                        6499
049400070626$017 C                   End
049500030205     C*
049600070626$017 C     D2STC         ifEQ      'S'
049700070626$017 C     D2ORD         andEQ     *zero
049800070626$017 C                   SETON                                        6599
049900070626$017 C                   End
050000070626     C*
050100960305     C                   ENDSR
050200030204     C/EJECT
050300030204     C************************************************************
050400030204     C* A chi deve trasmettere
050500030204     C************************************************************
050600030204     C     ACHI          BEGSR
050700030204     C*
050800030204     c                   eval      S_Tasto = $Tasto
050900030204     C*
051000030204     c                   move      '1'           t1trs
051100030204     c                   clear                   t1fil
051200030204     c     su_win        tag
051300030204     C                   Exfmt     W1Tras
051400030204     c                   if        t1trs = '2' and
051500030204     c                             t1fil = 0
051600030204     c                   seton                                        77
051700030204     c                   goto      su_win
051800030204     c                   end
051900030204     C*
052000030204     c                   eval      $Tasto = S_Tasto
052100030204     C*
052200030204     C                   ENDSR
052300030204     C/EJECT
052400030204     C************************************************************
052500030204    >C* AGGIORNAMENTO ANAGRAFICA
052600030204     C************************************************************
052700030204    >C     FLDAGG        BEGSR
052800030204     C*
052900030204     C*  campi chiave da scrivere e aggiornare
053000030204$007 C                   MOVE      '1'           tblKUT
053100030204     C                   movel     *blanks       tblKEY
053200030204     C                   MOVEl     D1COD         tblKEY
053300030204     c                   movel     T1trs         tblFTT
053400030204     c                   move      T1fil         tblFLT
053500030204     c                   move      *zeros        tblDTR
053600030204     c                   move      *blanks       tblFTR
053700030204     c                   move      ' '           TBLFLG
053800030204     C*
053900030204     C                   ENDSR
054000940207     C/EJECT
054100940224     C************************************************************
054200940315    >C* AGGIORNAMENTO ANAGRAFICA
054300940224     C************************************************************
054400940315    >C     AGGANA        BEGSR
054500940207     C*
054600030204     C                   exsr      FLDAGG
054700030204     C*
054800940207     C                   SELECT
054900030204     C*
055000030114    >C     xtaopz        WHENEQ    '01'
055100030114    >C     xtaopz        OREQ      '03'
055200030204     C*
055300070626     C                   move      'CC'          TBLcod
055400070626    >C                   EXSR      RIEANA_CC
055500030114     c                   write     TABEL                                59
055600940318     C* se rcd non scrivibile attivo errore
055700030114    >C   59              SETON                                          99
055800070626     C*   correlata CB
055900070626     c                   if        *in99 = *Off   and
056000070626     c                             d1DFE = 'S' and d1DSF <> ' '
056100070626     C                   move      'CB'          TBLcod
056200070626    >C                   EXSR      RIEANA_CB
056300030204     c                   write     TABEL                                59
056400030204     C* se rcd non scrivibile attivo errore
056500030204    >C   59              SETON                                          99
056600030204     c                   end
056700030204      *
056800070626     C*   correlata CA x stat.Clienti
056900070626     c                   if        *in99 = *Off and
057000070626     c                             d2STC <>' ' and d2DSC <> ' '
057100070626     C                   move      'CA'          TBLcod
057200070626    >C                   EXSR      RIEANA_CA
057300070626     c                   write     TABEL                                59
057400070626     C* se rcd non scrivibile attivo errore
057500070626    >C   59              SETON                                          99
057600070626     c                   end
057700070626      *
057800030114    >C     xtaopz        WHENEQ    '02'
057900030204      *
058000070626     C                   move      'CC'          TBLcod
058100030204     C     KTBL          chain     TABEL00F
058200030204     C                   exsr      FLDAGG
058300070626    >C                   EXSR      RIEANA_CC
058400030114     c                   update    TABEL                                59
058500030114    >C   59              SETON                                          99
058600030204      *
058700030204     c                   if        *in99 = *Off
058800070626     C                   move      'CB'          TBLcod
058900030204     C     KTBL          chain     TABEL00F
059000030204     c                   if        %Found(TABEL00F)
059100030204     C                   exsr      FLDAGG
059200070626    >C                   EXSR      RIEANA_CB
059300030204     c                   update    TABEL                                59
059400030204    >C   59              SETON                                          99
059500030204     c                   else
059600070626     c                   if        d1DFE = 'S' and d1DSF <> ' '
059700070626    >C                   EXSR      RIEANA_CB
059800030204     c                   write     TABEL                                59
059900030204    >C   59              SETON                                          99
060000070626     c                   end
060100030204     c                   end
060200030204     c                   end
060300070626      *
060400070626     c                   if        *in99 = *Off
060500070626     C                   move      'CA'          TBLcod
060600070626     C     KTBL          chain     TABEL00F
060700070626     c                   if        %Found(TABEL00F)
060800070626     C                   exsr      FLDAGG
060900070626    >C                   EXSR      RIEANA_CA
061000070626     c                   update    TABEL                                59
061100070626    >C   59              SETON                                          99
061200070626     c                   else
061300070626     c                   if        d2STC <>' ' and d2DSC <> ' '
061400070626    >C                   EXSR      RIEANA_CA
061500070626     c                   write     TABEL                                59
061600070626    >C   59              SETON                                          99
061700070626     c                   end
061800070626     c                   end
061900070626     c                   end
062000030204      *
062100030114    >C     xtaopz        WHENEQ    '04'
062200070626      *
062300070626     C                   move      'CC'          TBLcod
062400030204     C     KTBL          chain     TABEL00F
062500030114     c                   move      '*'           TBLFLG
062600030115     c                   if        $tasto = F23
062700030115     c                   delete    TABEL                                59
062800030115     c                   else
062900030114     c                   update    TABEL                                59
063000030115     c                   end
063100030114    >C   59              SETON                                          99
063200030204      *
063300030204     c                   if        *in99 = *Off
063400070626      *
063500070626     C                   move      'CB'          TBLcod
063600030204     C     KTBL          chain     TABEL00F
063700030204     c                   if        %Found(TABEL00F)
063800030204     c                   move      '*'           TBLFLG
063900030204     c                   if        $tasto = F23
064000030204     c                   delete    TABEL                                59
064100030204     c                   else
064200030204     c                   update    TABEL                                59
064300030204     c                   end
064400030204    >C   59              SETON                                          99
064500030204     c                   end
064600070626      *
064700030204     c                   else
064800070626      *
064900070626     c                   if        $tasto = F06    and
065000070626     c                             d1DFE = 'S' and d1DSF <> ' '
065100030204     c                   move      '*'           TBLFLG
065200030204     C                   exsr      FLDAGG
065300070626    >C                   EXSR      RIEANA_CB
065400030204     c                   write     TABEL                                59
065500030204    >C   59              SETON                                          99
065600030204     c                   end
065700070626      *
065800030204     c                   end
065900940621     C*
066000070626      *
066100070626     c                   if        *in99 = *Off
066200070626      *
066300070626     C                   move      'CA'          TBLcod
066400070626     C     KTBL          chain     TABEL00F
066500070626     c                   if        %Found(TABEL00F)
066600070626     c                   move      '*'           TBLFLG
066700070626     c                   if        $tasto = F23
066800070626     c                   delete    TABEL                                59
066900070626     c                   else
067000070626     c                   update    TABEL                                59
067100070626     c                   end
067200070626    >C   59              SETON                                          99
067300070626     c                   end
067400070626      *
067500070626     c                   else
067600070626      *
067700070626     c                   if        $tasto = F06    and
067800070626     c                             d2STC <>' ' and d2DSC <> ' '
067900070626     c                   move      '*'           TBLFLG
068000070626     C                   exsr      FLDAGG
068100070626    >C                   EXSR      RIEANA_CA
068200070626     c                   write     TABEL                                59
068300070626    >C   59              SETON                                          99
068400070626     c                   end
068500070626      *
068600070626     c                   end
068700070626     C*
068800940207     C                   ENDSL
068900940203     C*
069000940203     C                   ENDSR
069100940224     C************************************************************
069200940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
069300940224     C************************************************************
069400070626     C     RIEANA_CC     BEGSR
069500030114     C*
069600030114     C                   CLEAR                   tblUNI
069700030114     C* descrizione
069800070626     c                   movel     d1DES         §CCDES
069900080605      *******
070000080605     c*******            if        d1FLI = 'S'
070100080605     c*******            z-add     1             §CCFLI
070200080605     c*******            else
070300080605     c*******            z-add     0             §CCFLI
070400080605     c*******            end
070500070626      *
070600070626     c                   movel     d1JEI         §CCJEI
070700070626     c                   movel     d1JCI         §CCJCI
070800070626     c                   movel     d1JSI         §CCJSI
070900070626     c                   movel     d1JVI         §CCJVI
071000070626     c                   movel     d1JCE         §CCJCE
071100070626     c                   movel     d1JSE         §CCJSE
071200070626     c                   movel     d1JVE         §CCJVE
071300070626     c                   movel     d1FL0         §CCFL0
071400070626      *
071500070626     c                   if        d1FLP = 'S'
071600070626     c                   z-add     1             §CCFLP
071700070626     c                   else
071800070626     c                   z-add     0             §CCFLP
071900070626     c                   end
072000070626      *
072100070626     c                   movel     d1CEI         §CCCEI
072200070626     c                   movel     d1FL1         §CCFL1
072300070626     c                   movel     d1FL2         §CCFL2
072400070626     c                   movel     d1FL3         §CCFL3
072500070626     c                   movel     d1FL4         §CCFL4
072600070626     c                   movel     d1FL5         §CCFL5
072700070626     c                   movel     d1FL6         §CCFL6
072800160121     c                   z-add     d1RDL         §CCrdl
072900070626     c                   z-add     d1CMP         §CCCMP
073000070626     c                   movel     d1UTI         §CCUTI
073100070626     c                   movel     d1DFE         §CCDFE
073200070626     c                   movel     d1SV0         §CCSV0
073300030114     C*
073400070626     C                   MOVEL     EXTDSCC       TBLUNI
073500030114     C*
073600940207     C                   ENDSR
073700030204     C************************************************************
073800030204     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
073900030204     C************************************************************
074000070626     C     RIEANA_CB     BEGSR
074100030204     C*
074200030204     C                   CLEAR                   tblUNI
074300030204     C*
074400070626     C*  Tabella CB  correlata
074500070626     c                   eval      §CBDSF  =  d1DSF
074600030204     C*
074700070626     C                   MOVEL     EXTDSCB       TBLUNI
074800030204     C*
074900030204     C                   ENDSR
075000070626     C************************************************************
075100070626     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
075200070626     C************************************************************
075300070626     C     RIEANA_CA     BEGSR
075400070626     C*
075500070626     C                   CLEAR                   tblUNI
075600070626     C*
075700070626     C*  Tabella CA  correlata
075800070626     c                   eval      §CASTC  =  d2STC
075900070626     c                   eval      §CADSC  =  d2DSC
076000070626     c                   eval      §CAORD  =  d2ORD
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
081700070626     C                   move      'CC'          TBLcod
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
