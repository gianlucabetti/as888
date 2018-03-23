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
002500070626$001 FTRTB88D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700030113     FTABEL00F  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900070626$003 D EXTDSCC       E DS                  EXTNAME(DSCC) INZ
003000070626$003 D EXTDSCB       E DS                  EXTNAME(DSCB) INZ
003100070626$003 D EXTDSCA       E DS                  EXTNAME(DSCA) INZ
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
015000030114     C*
015100030114     c                   movel     D1Fun         D2Fun
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
017200030114     C* Video 2
017300940506     C     *IN99         IFEQ      *OFF
017400030204     c     Video2        tag
017500030205     c                   setoff                                       99
017600030114     C*               *----------------*
017700030114     C                   EXFMT     D2
017800030114     C*               *----------------*
017900940131     C*
0180009402031    C                   SELECT
018100940131     C* F3=Fine
018200940506     C     $TASTO        WHENEQ    F03
018300940309     C                   EXSR      F03D1
018400940131     C* F12=Ritorno
018500940506     C     $TASTO        WHENEQ    F12
018600940309     C                   EXSR      F12D1
018700940131     C*
0188009402031O   C                   OTHER
018900030113     C*
019000030114     C                   EXSR      CTRD2
019100030205     c   99              goto      video2
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
0207000301142-   C                   ENDSL
020800030114     C*
0209000301142-   C                   ENDIF
021000940224     C*
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
022200030113     C                   CLEAR                   D2
022300070626     C                   move      'CC'          TBLcod
022400030113     C                   MOVEl     xtakey        D1COD
022500030113     C                   MOVEl     xtakey        D2COD
022600030113     C                   MOVEl     xtakey        TBLkey
022700940207     C*
022800940224     C* valorizzo la chiave univoca per reperire i dati
022900940224     C* se non immissione
0230000301141   >C     xtaopz        IFNE      '01'
023100030204      *
023200940315    >C*-------------------------------------------------------------
023300940315    >C* impostare i campi chiave ricevuti
023400940315    >C*-------------------------------------------------------------
023500070626     C                   move      'CC'          TBLcod
023600030204     C     KTBL          chain(N)  TABEL00F
023700940614     C*
023800030113     C                   IF        %Found(Tabel00F)
023900070626     C                   MOVEL     TBLuni        EXTDSCC
024000030114     c                   if        TblFLG = '*'
024100030115     c                   seton                                        08
024200030114     C                   MOVEl     Annull        D1Ann
024300030114     C                   MOVEl     D1Ann         D2Ann
024400030114     C                   end
024500030204     C*
024600070626     C*  Aggancia la tabella correlata: (CB)
024700070626     C                   move      'CB'          TBLcod
024800030204     C     KTBL          chain(N)  TABEL00F
024900030204     C                   IF        %Found(Tabel00F)
025000070626     C                   MOVEL     TBLuni        EXTDSCB
025100030204     C                   end
025200070626     C*
025300070626     C*  Aggancia la tabella correlata: (CA)
025400070626     C                   move      'CA'          TBLcod
025500070626     C     KTBL          chain(N)  TABEL00F
025600070626     C                   IF        %Found(Tabel00F)
025700070626     C                   MOVEL     TBLuni        EXTDSCA
025800070626     C                   end
025900030113     C*
026000940224     C* riempio la videata
026100940224     C                   EXSR      RIED1
026200030113      *
026300940224     C* richiamo routine dei ctrl per decodificarla
026400940614    >C                   EXSR      CTRD1
026500030113      *
026600960228     C                   MOVEL     *IN99         SAV99             1
026700030113      *
026800960228     C* Se la gestione è più videate
026900960305    >C                   EXSR      CTRD2
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
030200030114     c                   clear                   d2
030300070626     c                   move      *zeros        D1JCI
030400070626     c                   move      *zeros        D1JSI
030500070626     c                   movel     '0000'        D1JVI
030600070626     c                   move      *zeros        D1JCE
030700070626     c                   move      *zeros        D1JSE
030800070626     c                   movel     '0000'        D1JVE
030900080605     c*******            move      'N'           D1FLI
031000070626     c                   move      'N'           D1FLP
031100070626     c                   move      'N'           D1FL1
031200070626     c                   move      'N'           D1FL2
031300070626     c                   move      'N'           D1FL3
031400070626     c                   move      'N'           D1FL4
031500070626     c                   move      'N'           D1FL5
031600070626     c                   move      'N'           D1FL6
031700070626     c                   move      *zeros        D1CMP
031800030114     C*
031900030114     C                   ENDSR
032000030114     C************************************************************
032100030114     C* RIEMPIMENTO VIDEATA  D1
032200030114     C************************************************************
032300030114     C     RIED1         BEGSR
032400030114     C*
032500070626     c                   eval      d1DES = §CCDES
032600070626      *
032700080605     c*******            if        §CCFLI = 1
032800080605     c*******            eval      d1FLI = 'S'
032900080605     c*******            else
033000080605     c*******            eval      d1FLI = 'N'
033100080605     c*******            end
033200070626      *
033300070626     c                   eval      d1JEI = §CCJEI
033400070626     c                   eval      d1JCI = §CCJCI
033500070626     c                   eval      d1JSI = §CCJSI
033600070626     c                   eval      d1JVI = §CCJVI
033700070626     c                   eval      d1JCE = §CCJCE
033800070626     c                   eval      d1JSE = §CCJSE
033900070626     c                   eval      d1JVE = §CCJVE
034000070626     c                   eval      d1FL0 = §CCFL0
034100070626      *
034200070626     c                   if        §CCFLP = 1
034300070626     c                   eval      d1FLP = 'S'
034400070626     c                   else
034500070626     c                   eval      d1FLP = 'N'
034600070626     c                   end
034700070626      *
034800070626     c                   eval      d1CEI = §CCCEI
034900070626     c                   eval      d1FL1 = §CCFL1
035000070626     c                   eval      d1FL2 = §CCFL2
035100070626     c                   eval      d1FL3 = §CCFL3
035200070626     c                   eval      d1FL4 = §CCFL4
035300070626     c                   eval      d1FL5 = §CCFL5
035400070626     c                   eval      d1FL6 = §CCFL6
035500070626     c                   eval      d1CMP = §CCCMP
035600070626     c                   eval      d1UTI = §CCUTI
035700070626     c                   eval      d1DFE = §CCDFE
035800070626     c                   eval      d1SV0 = §CCSV0
035900030204     C*
036000070626     c                   eval      d1DSF = §CBDSF
036100070626     C*
036200070626     c                   eval      d2STC = §CASTC
036300070626     c                   eval      d2DSC = §CADSC
036400070626     c                   eval      d2ORD = §CAORD
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
040100030114     C                   Else
040200030114     c                   eval      d2cod = d1cod
040300971110     C                   ENDIF
040400030114     C*
040500030114     C* controllo codice vuoto
040600030114$017 C     D1DES         IFEQ      *ALL' '
040700030114     C                   SETON                                        5199
040800030114     C                   Else
040900030114     c                   eval      d2des = d1des
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
042500070626$017 C     D1JEI         IFEQ      *blank
042600070626$017 C*******            SETON                                        5399
042700070626$017 C                   End
042800030114      *
042900070626$017 C     D1JCI         IFEQ      *blank
043000070626$017 C                   SETON                                        5499
043100070626$017 C                   End
043200070626      *
043300070626$017 C     D1JSI         IFEQ      *blank
043400070626$017 C                   SETON                                        5599
043500070626$017 C                   End
043600070626      *
043700070626$017 C     D1JVI         IFEQ      *blank
043800070626$017 C                   SETON                                        5699
043900070626$017 C                   End
044000070626      *
044100070626$017 C     D1JCE         IFEQ      *blank
044200070626$017 C                   SETON                                        5799
044300070626$017 C                   End
044400070626      *
044500070626$017 C     D1JSE         IFEQ      *blank
044600070626$017 C                   SETON                                        5899
044700070626$017 C                   End
044800070626      *
044900070626$017 C     D1JVE         IFEQ      *blank
045000070626$017 C                   SETON                                        6099
045100070626$017 C                   End
045200070626      *
045300070626      *?  Fine controlli sui campi
045400070626      *
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
046700960305     C/EJECT
046800960305     C************************************************************
046900960305     C* CONTROLLO VIDEATA D2
047000960305     C************************************************************
047100960305     C     CTRD2         BEGSR
047200030205     C*
047300070627      * FEDEX
047400070627$017 C     D1DSF         IFne      *blank
047500070627$017 C     D1DFE         andEQ     *blank
047600070627$017 C                   SETON                                        6199
047700070627$017 C                   End
047800070627      *
047900070627$017 C     D1DSF         IFEQ      *blank
048000070627$017 C     D1DFE         andeq     'S'
048100070627$017 C                   SETON                                        6299
048200070627$017 C                   End
048300070626     C* controllo codice vuoto ordinamento
048400070626     C*
048500070626$017 C     D2STC         ifEQ      *blank
048600070626$017 C     D2ORD         ifGt      *zero
048700070626$017 C     D2DSC         orNE      *blanks
048800070626$017 C                   SETON                                        6699
048900070626$017 C                   End
049000070626$017 C                   End
049100070626      *
049200070626$017 C     D2STC         ifNE      *blank
049300070626$017 C     D2DSC         andEQ     *blank
049400070626$017 C                   SETON                                        6499
049500070626$017 C                   End
049600030205     C*
049700070626$017 C     D2STC         ifEQ      'S'
049800070626$017 C     D2ORD         andEQ     *zero
049900070626$017 C                   SETON                                        6599
050000070626$017 C                   End
050100070626     C*
050200960305     C                   ENDSR
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
055400070626     C                   move      'CC'          TBLcod
055500070626    >C                   EXSR      RIEANA_CC
055600030114     c                   write     TABEL                                59
055700940318     C* se rcd non scrivibile attivo errore
055800030114    >C   59              SETON                                          99
055900070626     C*   correlata CB
056000070626     c                   if        *in99 = *Off   and
056100070626     c                             d1DFE = 'S' and d1DSF <> ' '
056200070626     C                   move      'CB'          TBLcod
056300070626    >C                   EXSR      RIEANA_CB
056400030204     c                   write     TABEL                                59
056500030204     C* se rcd non scrivibile attivo errore
056600030204    >C   59              SETON                                          99
056700030204     c                   end
056800030204      *
056900070626     C*   correlata CA x stat.Clienti
057000070626     c                   if        *in99 = *Off and
057100070626     c                             d2STC <>' ' and d2DSC <> ' '
057200070626     C                   move      'CA'          TBLcod
057300070626    >C                   EXSR      RIEANA_CA
057400070626     c                   write     TABEL                                59
057500070626     C* se rcd non scrivibile attivo errore
057600070626    >C   59              SETON                                          99
057700070626     c                   end
057800070626      *
057900030114    >C     xtaopz        WHENEQ    '02'
058000030204      *
058100070626     C                   move      'CC'          TBLcod
058200030204     C     KTBL          chain     TABEL00F
058300030204     C                   exsr      FLDAGG
058400070626    >C                   EXSR      RIEANA_CC
058500030114     c                   update    TABEL                                59
058600030114    >C   59              SETON                                          99
058700030204      *
058800030204     c                   if        *in99 = *Off
058900070626     C                   move      'CB'          TBLcod
059000030204     C     KTBL          chain     TABEL00F
059100030204     c                   if        %Found(TABEL00F)
059200030204     C                   exsr      FLDAGG
059300070626    >C                   EXSR      RIEANA_CB
059400030204     c                   update    TABEL                                59
059500030204    >C   59              SETON                                          99
059600030204     c                   else
059700070626     c                   if        d1DFE = 'S' and d1DSF <> ' '
059800070626    >C                   EXSR      RIEANA_CB
059900030204     c                   write     TABEL                                59
060000030204    >C   59              SETON                                          99
060100070626     c                   end
060200030204     c                   end
060300030204     c                   end
060400070626      *
060500070626     c                   if        *in99 = *Off
060600070626     C                   move      'CA'          TBLcod
060700070626     C     KTBL          chain     TABEL00F
060800070626     c                   if        %Found(TABEL00F)
060900070626     C                   exsr      FLDAGG
061000070626    >C                   EXSR      RIEANA_CA
061100070626     c                   update    TABEL                                59
061200070626    >C   59              SETON                                          99
061300070626     c                   else
061400070626     c                   if        d2STC <>' ' and d2DSC <> ' '
061500070626    >C                   EXSR      RIEANA_CA
061600070626     c                   write     TABEL                                59
061700070626    >C   59              SETON                                          99
061800070626     c                   end
061900070626     c                   end
062000070626     c                   end
062100030204      *
062200030114    >C     xtaopz        WHENEQ    '04'
062300070626      *
062400070626     C                   move      'CC'          TBLcod
062500030204     C     KTBL          chain     TABEL00F
062600030114     c                   move      '*'           TBLFLG
062700030115     c                   if        $tasto = F23
062800030115     c                   delete    TABEL                                59
062900030115     c                   else
063000030114     c                   update    TABEL                                59
063100030115     c                   end
063200030114    >C   59              SETON                                          99
063300030204      *
063400030204     c                   if        *in99 = *Off
063500070626      *
063600070626     C                   move      'CB'          TBLcod
063700030204     C     KTBL          chain     TABEL00F
063800030204     c                   if        %Found(TABEL00F)
063900030204     c                   move      '*'           TBLFLG
064000030204     c                   if        $tasto = F23
064100030204     c                   delete    TABEL                                59
064200030204     c                   else
064300030204     c                   update    TABEL                                59
064400030204     c                   end
064500030204    >C   59              SETON                                          99
064600030204     c                   end
064700070626      *
064800030204     c                   else
064900070626      *
065000070626     c                   if        $tasto = F06    and
065100070626     c                             d1DFE = 'S' and d1DSF <> ' '
065200030204     c                   move      '*'           TBLFLG
065300030204     C                   exsr      FLDAGG
065400070626    >C                   EXSR      RIEANA_CB
065500030204     c                   write     TABEL                                59
065600030204    >C   59              SETON                                          99
065700030204     c                   end
065800070626      *
065900030204     c                   end
066000940621     C*
066100070626      *
066200070626     c                   if        *in99 = *Off
066300070626      *
066400070626     C                   move      'CA'          TBLcod
066500070626     C     KTBL          chain     TABEL00F
066600070626     c                   if        %Found(TABEL00F)
066700070626     c                   move      '*'           TBLFLG
066800070626     c                   if        $tasto = F23
066900070626     c                   delete    TABEL                                59
067000070626     c                   else
067100070626     c                   update    TABEL                                59
067200070626     c                   end
067300070626    >C   59              SETON                                          99
067400070626     c                   end
067500070626      *
067600070626     c                   else
067700070626      *
067800070626     c                   if        $tasto = F06    and
067900070626     c                             d2STC <>' ' and d2DSC <> ' '
068000070626     c                   move      '*'           TBLFLG
068100070626     C                   exsr      FLDAGG
068200070626    >C                   EXSR      RIEANA_CA
068300070626     c                   write     TABEL                                59
068400070626    >C   59              SETON                                          99
068500070626     c                   end
068600070626      *
068700070626     c                   end
068800070626     C*
068900940207     C                   ENDSL
069000940203     C*
069100940203     C                   ENDSR
069200940224     C************************************************************
069300940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
069400940224     C************************************************************
069500070626     C     RIEANA_CC     BEGSR
069600030114     C*
069700030114     C                   CLEAR                   tblUNI
069800030114     C* descrizione
069900070626     c                   movel     d1DES         §CCDES
070000080605      *******
070100080605     c*******            if        d1FLI = 'S'
070200080605     c*******            z-add     1             §CCFLI
070300080605     c*******            else
070400080605     c*******            z-add     0             §CCFLI
070500080605     c*******            end
070600070626      *
070700070626     c                   movel     d1JEI         §CCJEI
070800070626     c                   movel     d1JCI         §CCJCI
070900070626     c                   movel     d1JSI         §CCJSI
071000070626     c                   movel     d1JVI         §CCJVI
071100070626     c                   movel     d1JCE         §CCJCE
071200070626     c                   movel     d1JSE         §CCJSE
071300070626     c                   movel     d1JVE         §CCJVE
071400070626     c                   movel     d1FL0         §CCFL0
071500070626      *
071600070626     c                   if        d1FLP = 'S'
071700070626     c                   z-add     1             §CCFLP
071800070626     c                   else
071900070626     c                   z-add     0             §CCFLP
072000070626     c                   end
072100070626      *
072200070626     c                   movel     d1CEI         §CCCEI
072300070626     c                   movel     d1FL1         §CCFL1
072400070626     c                   movel     d1FL2         §CCFL2
072500070626     c                   movel     d1FL3         §CCFL3
072600070626     c                   movel     d1FL4         §CCFL4
072700070626     c                   movel     d1FL5         §CCFL5
072800070626     c                   movel     d1FL6         §CCFL6
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
