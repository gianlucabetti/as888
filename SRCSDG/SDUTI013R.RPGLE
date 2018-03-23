000100151002     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO)
000200151002     H*PARMS BNDDIR(PJXBND PJCBND)
000300151002     H*PARMS ACTGRP(QILE)
000400151006     H*****************************************************************
000500151002     H DECEDIT('0,') DATEDIT(*DMY.)
000600151006     H*****************************************************************
000700000000     H*
000800000000     H*               -------------------------------
000900151008     H* La procedura deve scrivere la natura dell'operazione e cioè
001000151008     H* se si tratta di beni o di servizi, nelle regole della estrazione
001100151008     H* spesometro (SDGSPR00F).
001200151008     H* Leggo prima la tabella CB3 dove per causale è indicata la
001300151008     H* natura della operazione. Scrivo le regole con solo la causale.
001400151008     H* Successivamente leggo il file della procedura che andiamo a sostituire
001500151008     H* (BlBOS00F) che indica sempre la natura dell'operazione per contropartita
001600151009     H* e voce. Scrivo poi il file delle regole di estrazione.
001700151118     H* Dalla lettura di BlBos00f reperisco anche l'esclusione per
001800151118     H* contropartita e voce.
001900000000     F*----------------------------------------------------*
002000151013     FBlBOS01L  IF A E           K DISK
002100151007     F*------------
002200151007     FSDGSPR01L IF   E           K DISK    RENAME(SDGSPR000:SDGSPR001)
002300151007     F*------------
002400151007     FSDGSPR03L UF A E           K DISK
002500000000     D*-------------
002600151013     F* Printer elenco causali escluse inserite
002700151013     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002800151013     D*-------------
002900000000     D* Reperimento nome PGM
003000000000     D STATUS         SDS           333
003100000000     D  DSPGM            *PROC
003200000000     D*------------------
003300000000     D* DS Esterna gestione messaggi
003400000000     D SDGMSGDS      E DS
003500000000     D  STS          E                     EXTFLD(MSGSTS)
003600000000     D                                     DIM(333)
003700000000     D  JBA          E                     EXTFLD(MSGJBA)
003800000000     D                                     DIM(502)
003900151002     D*-------------
004000151002     D* Passaggio Parametri
004100151002     D KPJBA         E DS
004200000000     D*------------------
004300151001     D* Dati di ambiente ottenuti da SDGSOCDR
004400151001     D SOC001        E DS                  EXTNAME(SDGSOCDS )
004500151001     D*-------------
004600151001     D* DS Interna per dati di output di SDGSOCDR
004700151001     D XSOCDS          DS          1000
004800151006     D*-------------
004900151008     D* Dati del file delle regole di estrazione
005000151008     D SDGSPRDS      E DS                  EXTNAME(SDGSPR00F)
005100151008     D*-------------
005200151007     D* Richiamo a SDGTABDR
005300151007     D SDGTABDS      E DS                  Inz
005400000000     D*------------------
005500151008     D* Ds Tabella Cb3
005600151008     D ANGCB3DS      E DS                  Inz
005700151013     D*-----------
005800151013     D* Definizioni parametri standard chiamate moduli
005900151013     D Esito           S              1
006000151013     D GesMsg          S              1
006100151013     D LenOut          S              9B 0
006200151013     D OutPut          S           4000
006300151013     D StrutturaO      S             10
006400151013     D Operazione      S              2
006500151013     D Societa         S              3
006600151013     D*------------------
006700151013     D* DS esterna per decodifica conto
006800151013     D ND002DS       E DS
006900151013     D*-------------
007000151013     D* Ds per reperimento dati su causali operative contabili
007100151013     D ANOPE         E DS                  ExtName(ANOPE00F) Inz
007200151013     D*-------------
007300151013     D*-------------
007400151013     D* Controlli validità legende
007500151013     D X07ValDS      E DS
007600151008     D*------------------
007700151008     D $Tipreg         S                   like(SPRTipReg) INZ
007800151013     D $TipregD        S             20    INZ
007900151008     D $StpReg         S                   like(SPRStpReg) INZ
008000151013     D $StpRegD        S             20    INZ
008100151008     D $Causale        S                   like(SPRCausale)INZ
008200151013     D $CausaleD       S             20    INZ
008300151008     D $kcc            S                   like(SPRKcc)    INZ
008400151008     D $ksc            S                   like(SPRKsc)    INZ
008500151013     D $kscD           S             20    INZ
008600151008     D $Voce           S                   like(SPRVoce)   INZ
008700151008     D $NrRiga         S                   like(SPRNrRiga) INZ
008800151008     D $NrRigaBen      S                   like(SPRNrRiga) INZ
008900151008     D $NrRigaSer      S                   like(SPRNrRiga) INZ
009000151008     D $NrRigaEsc      S                   like(SPRNrRiga) INZ
009100151008     D*----------------
009200151008     D $Efile          S              1A
009300151008     D $RcdOK          S              1A
009400151013     D*----------------
009500151013     D ZdateIso        s               D
009600151008     D*----------------
009700151007     C*----------------------------------------------------*
009800151007     C*                MAIN LINE PROGRAM
009900151007     C*----------------------------------------------------*
010000151007     C                   EXSR      INZVAR
010100151007     C*
010200151008     C                   EXSR      ELABORACB3
010300151008     C*
010400151008     C                   EXSR      ELABORABOS
010500151007     C*
010600151007     C                   EXSR      ENDPGM
010700151007     C/EJECT
010800151007     C************************************************************
010900151008     C* Elaborazione file BLBOS
011000151007     C************************************************************
011100151008     C     ElaboraBos    BEGSR
011200151007     C*
011300151007     C*
011400151013     C     XscSoc        reade     BlBOS01l
011500151007     C*
011600151008     C                   Do        *hival
011700151007     C*
011800151013     C     XscSoc        Reade     BlBOS01l
011900151007     C*
012000151008     C                   If        %eof(BlBos01l)
012100151007     C                   Leave
012200151007     C                   Endif
012300151007     C*
012400151008     C                   Select
012500151008     C*
012600151008     C                   When      BosBos ='1' or BosBos='2'
012700151008     C*
012800151008     C                   Eval      $TipReg    = '2'
012900151008     C                   Eval      $StpReg    = BosBos
013000151008     C                   Clear                   $Causale
013100151008     C                   Eval      $kcc  = BosKcc
013200151008     C                   Eval      $ksc  = BosKsc
013300151008     C                   Eval      $Voce = BosVoc
013400151007     C                   Exsr      Aggiorna
013500151008     C*
013600151008     C*
013700151008     C                   When      BosBos ='0' and BosVoc=*blanks
013800151008     C*
013900151008     C                   Eval      $TipReg    = '5'
014000151008     C                   Eval      $StpReg    = '2'
014100151008     C                   Clear                   $Causale
014200151008     C                   Eval      $kcc  = BosKcc
014300151008     C                   Eval      $ksc  = BosKsc
014400151008     C                   Eval      $Voce = BosVoc
014500151008     C                   Exsr      Aggiorna
014600151008     C*
014700151008     C                   EndSL
014800151007     C*
014900151007     C                   Enddo
015000151007     C
015100151007     C                   ENDSR
015200151002     C/EJECT
015300151006     C************************************************************
015400151007     C* Reperisco i dati della tabella CB3
015500151006     C************************************************************
015600151008     C     ElaboraCB3    BEGSR
015700151006     C*
015800151007     C                   Reset                   SDGTABDS
015900151008     C                   Clear                   $Efile
016000151008     C                   Clear                   $RcdOk
016100151008     C*
016200151008     C                   Move      Xscsoc        XTBAZI
016300151008     C                   MOVE      XscLin        XTBLIN
016400151008     C                   MOVE      '9'           XTBRIC
016500151008     C                   MOVE      '0'           XTBALC
016600151007     C                   Move      'CB3'         XTBCOD
016700151008     C                   MOVE      XscCut        XTBUTE
016800151008     C                   MOVE      '0'           XTBERR
016900151007     C*
017000151007     C                   CALL      'SDGTABDR'
017100151007     C                   PARM                    SDGTABDS
017200151008     C*
017300151008     C     XTBERR        IFNE      '0'
017400151008     C                   MOVEL     *ON           $EFILE
017500151008     C                   ENDIF
017600151008     C*
017700151008     C                   MOVEL     *OFF          $EFILE
017800151008     C                   MOVEL     *OFF          $RCDOK
017900151008     C*
018000151008     C     $EFILE        DOUEq     *ON
018100151008     C     $RCDOK        OrEq      *ON
018200151008     C*
018300151008     C                   MOVE      '3'           XTBRIC
018400151008     C                   MOVE      '0'           XTBERR
018500151008     C                   CALL      'SDGTABDR'
018600151008     C                   PARM                    SDGTABDS
018700151008     C*
018800151008     C     XTBERR        IFNE      '0'
018900151008     C                   MOVEL     *ON           $EFILE
019000151008     C                   ENDIF
019100151008     C*
019200151008     C     $EFILE        IfEq      *OFF
019300151008     C*
019400151008     C                   MoveL     XtbUni        AngCB3DS
019500151008     C*
019600151008     C                   If        BenSerCB3 = '1' or
019700151008     C                             BenSerCB3 = '2'
019800151008     C*
019900151008     C                   Eval      $TipReg    = '2'
020000151008     C                   Eval      $StpReg    = BenSerCb3
020100151008     C                   Move      XTBKey        $Causale
020200151008     C                   Clear                   $kcc
020300151008     C                   Clear                   $ksc
020400151008     C                   Clear                   $Voce
020500151008     C*
020600151008     C                   Exsr      Aggiorna
020700151008     C*
020800151008     C                   Endif
020900151008     C*
021000151008     C                   EndIf
021100151008     C*
021200151008     C                   EndDo
021300151008     C*
021400151007     C*
021500151006     C                   ENDSR
021600151007     C/EJECT
021700151007     C************************************************************
021800151007     C* Aggiornamento regola se non esiste già
021900151007     C************************************************************
022000151008     C     Aggiorna      BEGSR
022100151008     C*
022200151008     C                   Clear                   SDGSPRDS
022300151008     C*
022400151008     C                   Eval      SPRSocieta = XscSoc
022500151008     C                   Eval      SPRTipReg  = $TipReg
022600151008     C                   Eval      SPRStpReg  = $StpReg
022700151008     C                   Eval      SPRCausale = $Causale
022800151008     C                   Eval      SPRKcc     = $Kcc
022900151008     C                   Eval      SPRKsc     = $Ksc
023000151008     C                   Eval      SPRVoce    = $Voce
023100151007     C*
023200151008     C     K07SPR03      Chain(n)  SDGSPR03L
023300151007     C*
023400151007     C                   If        Not %found(SDGSPR03L)
023500151007     C*
023600151007     C                   Exsr      RieAna
023700151007     C*
023800151007     C                   Write     SdgSpr000                            22
023900151007     C*
024000151007     C     *IN22         IfEq      *ON
024100151007     C                   EXSR      MSGPGM
024200151013     C                   EXSR      EndPgm
024300151013     C                   Else
024400151013     C                   Exsr      Stampa
024500151013     C                   EndIf
024600151007     C*
024700151007     C                   EndIf
024800151007     C*
024900151007     C                   ENDSR
025000151007     C/EJECT
025100151007     C************************************************************
025200151007     C* Riempimento anagrafica
025300151007     C************************************************************
025400151007     C     RieAna        BEGSR
025500151007     C*
025600151008     C*
025700151007     C                   Eval      SPRSocieta = XscSoc
025800151008     C                   Eval      SPRTipReg  = $TipReg
025900151009     C                   Eval      SPRStpReg  = $StpReg
026000151008     C                   Eval      SPRCausale = $Causale
026100151008     C                   Eval      SPRKcc     = $Kcc
026200151008     C                   Eval      SPRKsc     = $Ksc
026300151009     C                   Eval      SPRVoce    = $Voce
026400160302     C*
026500160302     C                   Select
026600160302     C*
026700160302     C                   When      SprTipReg = '2' and
026800160302     C                             SprStpReg = '1'
026900160302     C                   Add       1             $NrRigaBen
027000160302     C                   Z-add     $NrRigaBen    SPRNrRiga
027100160302     C                   When      SprTipReg = '2' and
027200160302     C                             SprStpreg = '2'
027300160302     C                   Add       1             $NrRigaSer
027400160302     C                   Z-add     $NrRigaSer    SPRNrRiga
027500160302     C                   When      SprTipReg = '5' and
027600160302     C                             SprStpreg = '2'
027700160302     C                   Add       1             $NrRigaEsc
027800160302     C                   Z-add     $NrRigaEsc    SPRNrRiga
027900160302     C*
028000160302     C                   EndSl
028100160302     C*
028200151007     c                   Move      XScCUt        SPRUteImm
028300151007     C     *Eur          Move      *Date         SPRDtImm
028400151007     c                   Move      XScCUt        SPRUteUlMo
028500151007     C     *Eur          Move      *Date         SPRDtUlMo
028600151007     C*
028700151007     C                   ENDSR
028800151013     C/EJECT
028900151013     C************************************************************
029000151013     C* Stampa
029100151013     C************************************************************
029200151013     C     Stampa        BEGSR
029300151013     C*
029400151013     C                   Exsr      DecTipRegB
029500151013     C                   Exsr      DecStpReg
029600151013     C                   Exsr      CallMVC003
029700151013     C                   Exsr      CallMVC002
029800151013     C*
029900151013     C                   If        *inOf = *on
030000151013     C                   Except    Testa
030100151013     C                   Setoff                                       OF
030200151013     C                   Endif
030300151013     C*
030400151013     C                   Except    Riga
030500151013     C*
030600151013     C                   Endsr
030700151013     C/EJECT
030800151013     C************************************************************
030900151013     C* DECODIFICA CONTO
031000151013     C************************************************************
031100151013     C     CallMVC002    BEGSR
031200151013     C*
031300151013     C                   Clear                   $kscD
031400151013     C                   Clear                   ND002DS
031500151013     C                   EVAL      LenOut = %Size(ND002DS)
031600151013     C                   CallB     'NDMVC002'
031700151013     C                   PARM                    XScSoc
031800151013     C                   PARM                    $Kcc
031900151013     C                   PARM                    $Ksc
032000151013     C                   PARM                    *Omit
032100151013     C                   PARM      *OFF          GesMsg
032200151013     C                   PARM      *OFF          Esito
032300151013     C                   PARM      'ND002DS'     StrutturaO
032400151013     C                   PARM                    ND002DS
032500151013     C                   PARM                    LenOut
032600151013     C* se nessun errore
032700151013     C                   If        Esito      <>*On
032800151013     C                   MoveL     DesBrev002    $KSCD
032900151013     C                   EndIf
033000151013     C*
033100151013     C                   ENDSR
033200151013     C/EJECT
033300151013     C************************************************************
033400151013     C* Rerperimento dati di causale operativa
033500151013     C************************************************************
033600151013     C     CallMVC003    BEGSR
033700151013     C*
033800151013     C                   Clear                   $CausaleD
033900151013     C                   Clear                   ANOPE
034000151013     C*
034100151013     C                   Eval      LenOut     = %Size(ANOPE)
034200151013     C                   CallB     'NDMVC003'
034300151013     C                   Parm                    XScSoc
034400151013     C                   Parm                    $Causale
034500151013     C                   Parm                    ZDateIso
034600151013     C                   Parm      '0'           Uso               1
034700151013     C                   Parm      *OFF          GesMsg
034800151013     C                   Parm      *ON           Esito
034900151013     C                   Parm      'ANOPE     '  StrutturaO
035000151013     C                   Parm                    ANOPE
035100151013     C                   Parm                    LenOut
035200151013     C*
035300151013     C                   If        Esito <> *on
035400151013     C                   Movel     OpeDesBrev    $CausaleD
035500151013     C                   EndIf
035600151013     C*
035700151013     C                   ENDSR
035800000000     C/EJECT
035900151013     C***********************************************************
036000151013     C* Decodifico Tipo Regola per Black List
036100151013     C***********************************************************
036200151013     C     DecTipRegB    BEGSR
036300151013     C*
036400151013     C                   Clear                   $TipRegD
036500151013     C*
036600151013     C                   Clear                   X07ValDs
036700151013     C                   Move      '1'           X07Ric
036800151013     C                   Move      *Off          X07All
036900151013     C                   Movel     'ANDIZ000 '   X07TRc
037000151013     C                   Movel     'SDGTIPREGB'  X07Cam
037100151013     C                   Movel     $TipReg       X07VAL
037200151013     C                   Callb     'X07VALR'
037300151013     C                   Parm                    X07ValDs
037400151013     C*
037500151013     C     X07ERR        IFEQ      *Off
037600151013     C                   MOVEL     X07DES        $TipRegD
037700151013     C                   endif
037800151013     C*
037900151013     C                   ENDSR
038000151013     C/EJECT
038100151013     C***********************************************************
038200151013     C* Decodifica Sottotipo Regola
038300151013     C***********************************************************
038400151013     C     DecStpReg     BEGSR
038500151013     C*
038600151013     C                   Clear                   $StpRegD
038700151013     C*
038800151013     C                   Clear                   X07ValDs
038900151013     C*
039000151013     C                   Move      '1'           X07Ric
039100151013     C                   Move      *Off          X07All
039200151013     C                   Movel     'ANDIZ000'    X07TRc
039300151013     C                   select
039400151013     C                   when      $TipReg = '1'
039500151013     C                   Movel     'SDGSTPREG1'  X07Cam
039600151013     C                   when      $TipReg = '2'
039700151013     C                   Movel     'SDGSTPREG2'  X07Cam
039800151013     C                   when      $TipReg = '3'
039900151013     C                   Movel     'SDGSTPREG3'  X07Cam
040000151013     C                   when      $TipReg = '4'
040100151013     C                   Movel     'SDGSTPREG4'  X07Cam
040200151013     C                   when      $TipReg = '5'
040300151013     C                   Movel     'SDGSTPREG5'  X07Cam
040400151013     C                   endsl
040500151013     C                   Movel     $StpReg       X07VAL
040600151013     C                   Callb     'X07VALR'
040700151013     C                   Parm                    X07ValDs
040800151013     C*
040900151013     C     X07ERR        IFEQ      *Off
041000151013     C                   MOVEL     X07DES        $stpregd
041100151013     C                   endif
041200151013     C*
041300151013     C                   ENDSR
041400151013     C/EJECT
041500000000     C************************************************************
041600000000     C* Messaggio errore
041700000000     C************************************************************
041800000000     C     MsgPgm        BEGSR
041900000000     C*
042000000000     C                   Reset                   SDGMSGDS
042100000000     C*
042200000000     C                   Movel     'N'           MsgRsp
042300000000     C                   Movel     *Blanks       MsgTpR
042400000000     C                   Z-Add     0             MsgMsg
042500000000     C                   Movel     'S'           MsgEmV
042600000000     C                   Z-Add     10            MsgRgp
042700000000     C                   Movel     'N'           MsgLck
042800000000     C                   Movea     Status        Sts
042900000000     C                   Movel     'N'           MsgCnl
043000000000     C                   Movel     'N'           MsgVid
043100000000     C                   Movel     'N'           MsgOpe
043200000000     C                   Movel     'N'           MsgStp
043300000000     C*
043400000000     C                   Call      'SDGMSGR'
043500000000     C                   Parm                    SDGMSGDS
043600000000     C                   Parm                    XMSGTxt         135
043700000000     C                   Parm                    XMSGInz           1
043800000000     c* codici dei 3 messaggi possibili da emettere a video
043900000000     C* "Si sono verificati errori che hanno impedito l'esecuzione"
044000000000     C                   PARM      'COS0377'     MSGCd1            7
044100000000     C                   PARM                    MSGCd2            7
044200000000     C                   PARM                    MSGCd3            7
044300000000     c* codice del messaggio da mandare alla coda dell'operatore
044400000000     C                   PARM                    MSGSn1            7
044500000000     c* codici(lunghi 7  ciascuno) dei 9 bottoni
044600000000     C                   PARM                    MSGCdTxt         63
044700000000     c* codici(lunghi 7  ciascuno) dei 2 titoli (titolo e sottotitolo)
044800000000     c* per stampa e video
044900000000     C                   PARM                    MSGCdTit         14
045000000000     c* flag per chiusura prtf
045100000000     C                   PARM                    MSGClPrtf         1
045200000000     C* variabili per messaggi
045300000000     C                   PARM                    VarMsg1         100
045400000000     C                   PARM                    VarMsg2         100
045500000000     C                   PARM                    VarMsg3         100
045600000000     C*
045700000000     C                   ENDSR
045800000000     C/EJECT
045900000000     C************************************************************
046000000000     C* OPERAZIONI INIZIALI
046100000000     C************************************************************
046200151007     C     INZVAR        BEGSR
046300151007     C*
046400151006     C* Reperimento dati società
046500151006    >C                   MOVEL     'SOC001'      TIPXSC
046600151006     C                   MOVEL     *BLANK        SOCXSC
046700151001     C                   Exsr      RepSoc
046800151006     C     RTNXSC        IFNE      '1'
046900151006     C                   MOVEL     XSOCDS        SOC001
047000151006     C                   ELSE
047100151006     C                   RETURN
047200151006     C                   ENDIF
047300151008     C* Reperisco ultimo numero riga scritto per natura operazione Beni.
047400151007     C                   Eval      SPRSocieta = XscSoc
047500151008     C                   Eval      SPRTipReg  = '2'
047600151007     C                   Eval      SPRStpReg  = '1'
047700151008     C*
047800151008     C     K03SPR01      SETGT     SDGSPR01l
047900151007     C     K03SPR01      READPE    SDGSPR01l
048000151007     C                   If        not %eof(SDGSPR01L)
048100151008     C                   Z-add     SPRNrRiga     $NrRigaBen
048200151007     C                   Else
048300151008     C                   Z-add     0             $NrRigaBen
048400151007     C                   Endif
048500151008     C* Reperisco ultimo numero riga scritto per natura operazione Servizi.
048600151008     C                   Eval      SPRSocieta = XscSoc
048700151008     C                   Eval      SPRTipReg  = '2'
048800151008     C                   Eval      SPRStpReg  = '2'
048900151008     C*
049000151008     C     K03SPR01      SETGT     SDGSPR01l
049100151008     C     K03SPR01      READPE    SDGSPR01l
049200151008     C                   If        not %eof(SDGSPR01L)
049300151008     C                   Z-add     SPRNrRiga     $NrRigaSer
049400151008     C                   Else
049500151008     C                   Z-add     0             $NrRigaSer
049600151008     C                   Endif
049700151008     C* Reperisco ultimo numero riga scritto per esclusione contropartita .
049800151008     C                   Eval      SPRSocieta = XscSoc
049900151008     C                   Eval      SPRTipReg  = '5'
050000151008     C                   Eval      SPRStpReg  = '2'
050100151008     C*
050200151008     C     K03SPR01      SETGT     SDGSPR01l
050300151008     C     K03SPR01      READPE    SDGSPR01l
050400151008     C                   If        not %eof(SDGSPR01L)
050500151008     C                   Z-add     SPRNrRiga     $NrRigaEsc
050600151008     C                   Else
050700151008     C                   Z-add     0             $NrRigaEsc
050800151008     C                   Endif
050900151007     C*
051000151013     C     *DMY          Move      Udate         ZDateISO
051100151013     C                   Seton                                        OF
051200151013     C*
051300151002     C                   ENDSR
051400151002     C/EJECT
051500151002     C************************************************************
051600151002     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
051700151002     C************************************************************
051800151002     C     DEFCAM        BEGSR
051900151002     C*
052000151002     C* klist
052100151006     C*
052200151007     C* SdgSPR03l
052300151008     C     K07SPR03      KLIST
052400151007     C                   KFLD                    SPRSocieta
052500151007     C                   KFLD                    SPRTipReg
052600151007     C                   KFLD                    SPRStpReg
052700151007     C                   KFLD                    SPRCausale
052800151008     C                   KFLD                    SPRKcc
052900151008     C                   KFLD                    SPRKsc
053000151008     C                   KFLD                    SPRVoce
053100151006     C*
053200151007     C* SdgSPR01l
053300151007     C     K04SPR01      KLIST
053400151007     C                   KFLD                    SPRSocieta
053500151007     C                   KFLD                    SPRTipReg
053600151007     C                   KFLD                    SPRStpReg
053700151007     C                   KFLD                    SPRNrRiga
053800151007     C* SdgSPR01l
053900151007     C     K03SPR01      KLIST
054000151007     C                   KFLD                    SPRSocieta
054100151007     C                   KFLD                    SPRTipReg
054200151007     C                   KFLD                    SPRStpReg
054300151002     C                   ENDSR
054400151002     C/EJECT
054500151001     C************************************************************
054600151001     C* REPERIMENTO DATI SOCIETA'
054700151001     C************************************************************
054800151001     C     REPSOC        BEGSR
054900151001     C*
055000151001     C                   CALL      'SDGSOCDR'
055100151001     C                   PARM                    TIPXSC            6
055200151001     C                   PARM                    SOCXSC            3
055300151001     C                   PARM                    CDSXSC            9 0
055400151001     C                   PARM                    MODXSC            3
055500151001     C                   PARM      *BLANKS       RTNXSC            1
055600151001     C                   PARM                    XSOCDS
055700151001     C                   PARM                    KPJBA
055800151001     C*
055900151001     C                   ENDSR
056000151001     C/EJECT
056100000000     C************************************************************
056200000000     C* Fine programma
056300000000     C************************************************************
056400000000     C     ENDPGM        BEGSR
056500000000     C*
056600000000     C                   SETON                                        LR
056700000000     C                   Return
056800000000     C*
056900000000     C                   ENDSR
057000000000     C/EJECT
057100151006     C************************************************************
057200151013     OQSYSPRT   E            TESTA            02
057300151013     O                                           35 '***---'
057400151013     O                                         +  0 '------------------------'
057500151013     O                                         +  0 '------------------------'
057600151013     O                                         +  0 '--***'
057700151013     O          E            TESTA            03
057800151013     O                                           35 '***-- '
057900151013     O                                         +  0 '        Black list: elen'
058000151013     O                                         +  0 'co regole inserite      '
058100151013     O                                         +  0 '--***'
058200151013     O          E            TESTA            04
058300151013     O                                           35 '***---'
058400151013     O                                         +  0 '------------------------'
058500151013     O                                         +  0 '------------------------'
058600151013     O                                         +  0 '--***'
058700151013     O*
058800151013     O          E            TESTA            06
058900151013     O                                            3 'Soc'
059000151013     O                                           15 'Tipo regola'
059100151013     O                                           44 'Sottotipo  regola'
059200151013     O                                           57 'Causale'
059300151013     O                                           81 'Conto'
059400151013     O                                          117 'Voce'
059500151013     O*
059600151013     O          E            Riga        1
059700151013     O*
059800151013     O                       Xscsoc               3
059900151013     O                       $TipReg              5
060000151013     O                       $TipRegD            26
060100151013     O                       $StpReg             28
060200151013     O                       $StpRegD            49
060300151013     O                       $Causale            54
060400151013     O                       $CausaleD           75
060500151013     O                       $Kcc                82
060600151013     O                       $Ksc                91
060700151013     O                       $KscD              112
060800151013     O                       $Voce              121
060900151013     O*
