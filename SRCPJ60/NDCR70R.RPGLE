000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951122     H*PARMS CVTOPT(*DATETIME)
000400951122     H DECEDIT('0,') DATEDIT(*DMY.)
000500951122     F*---------------------------------------------------------------*
000600951122     F*            LANCIO ESTRAZIONE MOVIMENTI RITENUTE               *
000700951122     F*                        PER MOD. 770                           *
000701161020R235 F*
000702161020R235 F* Modificata dicitura pgm,
000703161020R235 F*  da V5R0M1_Lancio estrazione mov. ritenute mod.770
000704161020R235 F*   a Estrazione percipienti per CU
000800000000     F*---------------------------------------------------------------*
000900951122     Fndcr70d   CF   E             WORKSTN
001000000328D0495F                                     INFDS(DSFMT)
001100020502     Fantri01L  IF   E           K DISK
001200951124     D*---------------------------------------------------------------*
001300000000     D KPJBA         E DS
001400951122     D*-----------
001500951122     D ndcr70ds      E DS
001600951122     D*-------------
001700000328D0495D DSFMT           DS
001800000328  "  D  $TASTO               369    369
001900000328  "  D  NRG                  370    370
002000000328  "  D  NCL                  371    371
002100000328  "  D*-------------
002200000328  "  D* posizione cursore
002300000328  "  D CURSOR          DS
002400000328  "  D  RIRI                   1      2B 0 INZ
002500000328  "  D  R$$                    2      2
002600000328  "  D  COCO                   3      4B 0 INZ
002700000328  "  D  C$$                    4      4
002800000328  "  D*-------------
002900000328  "  D* Values
003000000328  "  D X07ValDs      E DS
003100000328  "  D*-------------
003200000328  "  D* Values
003300000328  " >D Ana964Ds      E DS
003400000328D0495D*-------------
003500020502      * Passaggio Parametri per ricerca tributi
003600020502     D ANA095DS      E DS
003700020502      *-------------
003800951122     D* Dati di ambiente ottenuti da XSOC
003900951122    >D SOC001        E DS                  EXTNAME(XSOC001DS)
004000951122     D*-------------
004100951122     D* DS Interna per dati di output di XSOC
004200951122     D XSOCDS          DS          1000
004300951122     D*-------------
004400951122     D* Reperimento nome PGM
004500951122     D STATUS         SDS           333
004600951122     D  DSPGM            *PROC
004700951122     D  PARMS            *PARMS
004800020502     D*-------------
004900020502     D Win             S             99A
005000000000     C*---------------------------------------------------------------*
005100951122     C                   EXSR      INZVAR
005200951122     C*
005300000000     C     INIZIO        TAG
005400951122     C                   Z-ADD     0             d1anno
005500971023     C                   Move      XScOn         D1On1
005600971023     C                   Move      XScOff        D1Off1
005700020415     C                   Move      XScOn         D1On2
005800020415     C                   Move      XScOff        D1Off2
005900030625C1667C                   Move      XScOn         D1On3
006000030625C1667C                   Move      XScOff        D1Off3
006100971023     C                   Move      XScOff        D1RaggRGT
006200000328D0495C                   Move      '1'           D1Indir
006300020502     C                   Move      *Blanks       D1EscTri
006400030917C1667C                   Move      *Blanks       D1EscPrg
006500030625C1667C                   Move      XScOff        D1EscItc
006600030625C1667C                   Move      XScOff        D1EscImn
006700951122     C*
006800000000     C     VIDEO         TAG
006900951122     C*
007000951122     C     *IN99         IFEQ      '1'
007100951122     C                   SETOFF                                       99
007200020502     C                   MOVEA     *IN           WIN
007300020502     C                   MOVE      *ALL'0'       IN5098           49
007400020502     C                   MOVEA     IN5098        *IN(50)
007500951122     C                   WRITE     CR70D1
007600020502     C                   MOVEA     WIN           *IN
007700951122     C                   SETON                                        99
007800951122     C                   ENDIF
007900951122     C*
008000000000     C                   EXFMT     CR70D1
008100951122     C*
008200900411     C   KC              GOTO      FINE
008300951129     C   KN              GOTO      FINE
008400000328D0495C*
008500010404D0495C   kd              Exsr      Search
008600951122     C*
008700951122     C* F21 = Parametri batch
008800951122     C     *INKV         IFEQ      '1'
008900951129     C                   MOVE      'CR71'        KCOAZ
009000951122     C                   CALL      'BCH09'
009100951122     C                   PARM                    KPJBA
009200951122     C                   GOTO      VIDEO
009300951122     C                   ENDIF
009400951122     C*
009500951122     C                   setoff                                       99
009600951122     C     d1anno        ifeq      0
009700951122     C                   seton                                        5099
009800951122     C                   endif
009900951122     C*
010000971023     C                   If        D1RaggRGT <> XScOn and
010100971023     C                             D1RaggRGT <> XScOff
010200971023     C                   SetOn                                        5199
010300971023     C                   EndIf
010400000328D0495C*
010500000328  "  C* Provenienza indirizzo
010600000328  "  C                   Clear                   D1IndirD
010700000328  "  C                   RESET                   X07ValDS
010800000328  "  C                   MOVE      '1'           X07RIC
010900000328  "  C                   MOVEL     'ANDIZ000'    X07TRC
011000000328  "  C                   MOVEL     'DSPINDIR'    X07CAM
011100000328  "  C                   MOVEL     D1Indir       X07VAL
011200000328  "  C                   CALLb     'X07VALR'
011300000328  "  C                   PARM                    X07ValDS
011400000328  "  C     X07ERR        IFEQ      '0'
011500000328  "  C                   MOVEL     X07DES        D1Indird
011600000328  "  C                   ELSE
011700000328  "  C                   SETON                                        9952
011800000328D0495c                   ENDIF
011900030625C1667C*
012000030625  "  C* controllo flag di esclusione da Ammontare lordo
012100030625  "  C                   If        D1EscItc  <> XScOn and
012200030625  "  C                             D1EscItc  <> XScOff
012300030625  "  C                   SetOn                                        5799
012400030625  "  C                   EndIf
012500030625  "  C*
012600030625  "  C* controllo flag di esclusione da Imponibile non soggetto
012700030625  "  C                   If        D1EscImn  <> XScOn and
012800030625  "  C                             D1EscImn  <> XScOff
012900030625  "  C                   SetOn                                        5899
013000030625  "  C                   EndIf
013100041005C1667C*
013200041005C1788C* se richiesta esclusione, il tributo è obbligatorio
013300041005  "  C*                  If        D1EscItc  = XScOn Or
013400041005  "  C*                            D1EscImn  = XScOn
013500041005  "  C*                  If        D1EscTri  = *Blanks
013600041005  "  C*                  SetOn                                        5499
013700041005  "  C*                  EndIf
013800041005C1788C*                  EndIf
013900041005C1667C*
014000030625  "  C* se non richiesta esclusione, il tributo non va inserito
014100030625  "  C                   If        D1EscItc = XScOff And
014200030625  "  C                             D1EscImn = XScOff
014300030917  "  C                   If        D1EscTri  <> *Blanks   or
014400030917  "  C                             D1EscPrg  <> *Blanks
014500030625  "  C                   SetOn                                        5599
014600030625  "  C                   EndIf
014700030625C1667C                   EndIf
014800020502     C*
014900020502     C* controllo esistenza tributo
015000020502     C                   Clear                   D1EscTriD
015100020502     C     D1EscTri      IfNE      *Blanks
015200020502     C                   Movel     XScSoc        TriSocieta
015300020502     C                   Movel     D1EscTri      TriTributo
015400030917C1667C                   Movel     D1EscPRG      TriTriProg
015500030917  "  C**** K02Tri01      Chain     AnTri01L                           21
015600030917C1667C     K03Tri01      Chain     AnTri01L                           21
015700020502     C                   If        *In21 = *On
015800020502     C                   SetOn                                        5699
015900020502     C                   Else
016000020502     C                   Movel     TriDescr      D1EscTriD
016100020502     C                   EndIf
016200020502     C                   EndIf
016300020415     C*
016400020415     C  NKF
016500020415     COR 99              GOTO      VIDEO
016600020415     C*
016700951123     C                   move      xscsoc        socR70
016800951123     C                   z-add     d1anno        annoR70
016900951123     C                   move      *on           flagDR70
017000951123     C                   move      *on           flagD1R70
017100971007     C                   move      *on           flagDBR70
017200971023     C                   If        D1RaggRGT = XScOn
017300971023     C                   Move      *On           RaggRGTR70
017400971023     C                   Else
017500971023     C                   Move      *Off          RaggRGTR70
017600971023     C                   EndIf
017700000328D0495C                   move      D1Indir       IndiRR70
017800030625C1667C                   If        D1EscItc  = XScOn
017900030625  "  C                   Move      *On           EscItcR70
018000030625  "  C                   Else
018100030625  "  C                   Move      *Off          EscItcR70
018200030625  "  C                   EndIf
018300030625  "  C                   If        D1EscImn  = XScOn
018400030625  "  C                   Move      *On           EscImnR70
018500030625  "  C                   Else
018600030625  "  C                   Move      *Off          EscImnR70
018700030625C1667C                   EndIf
018800020502     C                   Move      D1EscTri      EscTriR70
018900030917C1667C                   Move      D1EscPrg      EscPrgR70
019000020415     C*
019100020415     C                   MOVEL     ndcr70ds      KPJBU
019200020415     C                   MOVE      'CR71'        KCOAZ
019201160830R235 C                   Z-Add     0             Debug             1 0
019202160809 "   C                   if        Debug       = 1
019203160809 "   C                   CALL      'NDCR71C'
019204160809 "   C                   PARM                    KPJBA
019205160809R235 C                   else
019300020415     C                   CALL      'BCH10'
019400000000     C                   PARM                    KPJBA
019404160808R235 C                   endif
019500951122     C*
019600000000     C     FINE          TAG
019700000000     C                   SETON                                        LR
019800951124     C*----------------------------------------------------*
019900951124     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
020000951124     C*----------------------------------------------------*
020100951122     C     INZVAR        BEGSR
020200951124     C*
020300951124     C     *ENTRY        PLIST                                                  *
020400951124     C                   PARM                    KPJBA                          *
020500951122     C*
020600951122     C* Reperimento dati società
020700951122     C*
020800951122    >C                   MOVEL     'SOC001'      TIPXSC
020900951122     C                   MOVEL     *BLANK        SOCXSC
021000951124     C                   CALL      'XSOC'
021100951124     C                   PARM                    TIPXSC            6
021200951124     C                   PARM                    SOCXSC            3
021300951124     C                   PARM                    CDSXSC            9 0
021400951124     C                   PARM                    MODXSC            3
021500951124     C                   PARM      *BLANKS       RTNXSC            1
021600951124     C                   PARM                    XSOCDS
021700951124     C                   PARM                    KPJBA
021800951122     C     RTNXSC        IFNE      '1'
021900951122     C                   MOVEL     XSOCDS        SOC001
022000951122     C                   ELSE
022100951124     C                   GOTO      FINE
022200951122     C                   ENDIF
022300951122     C*
022400951122     C* Valorizzazione campi univoci testate
022500951122     C*
022600951122     C                   MOVEL     KNSIF         NOMSIF
022700951122     C                   MOVEL     XSCDSI        NOMDIT
022800951122     C                   MOVEL     DSPGM         NOMPGM
022900951122     C*
023000000328D0495C                   MOVE      *BLANK        $LASTV            2
023100000328  "  C*
023200000328  "  C* Variabili appoggio
023300000328  "  C                   Z-ADD     0             CURR              2 0
023400000328  "  C                   Z-ADD     0             CURC              2 0
023500000328D0495C*
023600951122     C                   ENDSR
023700000328D0495C************************************************************
023800000328     C* RICERCHE
023900000328     C************************************************************
024000000328     C     SEARCH        BEGSR
024100000328     C*
024200000328     C* determino Riga/Colonna del cursore
024300000328     C                   MOVE      NRG           R$$
024400000328     C                   MOVE      NCL           C$$
024500000328     C                   Z-ADD     RIRI          CURR
024600000328     C                   Z-ADD     COCO          CURC
024700000328     C*
024800000328     C                   Select
024900000328     C*
025000000328     C* FMTD2 - Calcolo valuta media
025100000328     C     H1NMFL        WHENEQ    'D1INDIR'
025200000328     C                   Reset                   Ana964Ds
025300000328     C                   Movel     '01'          Opz964
025400000328     C                   MoveL     'ANDIZ000'    TRc964
025500000328     C                   Movel     'DSPINDIR'    Cam964
025600000328     C                   Movel     Ana964DS      KPJBU
025700000328     C                   Call      'ANA964R'
025800000328     C                   Parm                    KPJBA
025900000328     C                   Movel     KPJBU         Ana964DS
026000000328     C*
026100000328     C     VAL964        IFNE      *BLANKS
026200000328     C                   Movel     VAL964        D1Indir
026300000328     C                   ENDIF
026400020502     C*
026500020502     C* FMTD1  - Tributo
026600020502     C     H1NMFL        WHENEQ    'D1ESCTRI'
026700020502     C                   RESET                   Ana095ds
026800020502     C                   MOVEL     '01'          OPZ095
026900020502     C                   MOVEL     Ana095ds      KPJBU
027000020502     C                   CALL      'ANA095R'
027100020502     C                   PARM                    KPJBA
027200020502     C                   MOVEL     KPJBU         Ana095ds
027300020502     C*
027400020502     C     Tributo095    IfNE      *Blanks
027500020502     C                   Movel     Tributo095    D1EscTri
027600030917C1667C                   Movel     TriProg095    D1EscPrg
027700020502     C                   EndIf
027800000328     C*
027900000328     C* ricerca non abilitata in questa posizione
028000000328     C                   OTHER
028100000328     C                   SETON                                        9899
028200000328     C*
028300000328     C                   ENDSL
028400000328     C*
028500000328     C* imposto pos. del cursore
028600000328     C                   Z-ADD     CURR          H1RIGA
028700000328     C                   Z-ADD     CURC          H1COLO
028800000328     C*
028900000328     C                   MOVE      *HIVAL        $LASTV
029000000328     C*
029100000328     C                   ENDSR
029200000328D0495C/EJECT
029300020502      ************************************************************
029400020502      * DEFINIZIONE KLIST                                        *
029500020502      ************************************************************
029600020502     C     DefKlist      BEGSR
029700020502      *
029800020502      * klist
029900020502      *
030000020502      * ANTRI01L
030100030917C1667C**** K02Tri01      KLIST
030200030917 "   C****               KFLD                    TriSocieta
030300030917 "   C****               KFLD                    TriTributo
030400030917 "   C*
030500030917 "   C     K03Tri01      KLIST
030600030917 "   C                   KFLD                    TriSocieta
030700030917 "   C                   KFLD                    TriTributo
030800030917 "   C                   KFLD                    TriTriProg
030900030917C1667C*
031000020502     C*
031100020502     C                   ENDSR
031200020502     C/EJECT
