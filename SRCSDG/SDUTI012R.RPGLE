000301151002     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO)
000302151002     H*PARMS BNDDIR(PJXBND PJCBND)
000303151002     H*PARMS ACTGRP(QILE)
000304151006     H*****************************************************************
000305151002     H DECEDIT('0,') DATEDIT(*DMY.)
000306151006     H*****************************************************************
000400000000     H*
000500000000     H*               -------------------------------
000501151009     H* La procedura deve scrivere le causali escluse, nel file
000502151007     H* delle regole di estrazione comune allo spesometro (SDGSPR00F).
000503151009     H* Le causali incluse si trovano nella tabella CB3 per la procedura
000504160226     H* Solgenia e nella tabella HD2 per la procedura GPA.
000505151007     H* Procede leggendo tutte le causali contabili di tipo iva,
000506151007     H* scrivendo nel file quelle che non si trovano sulla tabella CB3
000507160226     H* o sulla tabella HD2
000900000000     F*----------------------------------------------------*
000901151007     FANOPE04L  IF A E           K DISK
000902151007     F*------------
000903151007     FSDGSPR01L IF   E           K DISK    RENAME(SDGSPR000:SDGSPR001)
000909151007     F*------------
000910151007     FSDGSPR03L UF A E           K DISK
000911151013     D*-------------
000912151013     F* Printer elenco causali escluse inserite
000913151013     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400000000     D*-------------
001500000000     D* Reperimento nome PGM
001600000000     D STATUS         SDS           333
001700000000     D  DSPGM            *PROC
001800000000     D*------------------
001900000000     D* DS Esterna gestione messaggi
002000000000     D SDGMSGDS      E DS
002100000000     D  STS          E                     EXTFLD(MSGSTS)
002200000000     D                                     DIM(333)
002300000000     D  JBA          E                     EXTFLD(MSGJBA)
002400000000     D                                     DIM(502)
002401151002     D*-------------
002402151002     D* Passaggio Parametri
002403151002     D KPJBA         E DS
002700000000     D*------------------
002702151001     D* Dati di ambiente ottenuti da SDGSOCDR
002703151001     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002704151001     D*-------------
002705151001     D* DS Interna per dati di output di SDGSOCDR
002706151001     D XSOCDS          DS          1000
002707151008     D*-------------
002708151008     D* Dati del file delle regole di estrazione
002709151008     D SDGSPRDS      E DS                  EXTNAME(SDGSPR00F)
002710151006     D*-------------
002711151007     D* Richiamo a SDGTABDR
002712151007     D SDGTABDS      E DS                  Inz
002713151013     D*-------------
002714151013     D* Controlli validità legende
002715151013     D X07ValDS      E DS
002900000000     D*------------------
002904151007     D $NrRiga         S                   like(SPRNrRiga) INZ
002905151013     D $Causale        S                   like(OpeCausale)
002906151013     D $Causaled       S                   like(OpeDescr)
002907151013     D $TipReg         S                   like(SPRTipReg)
002908151013     D $TipRegD        S                   like(OpeDesBrev)
002909151013     D $StpReg         S                   like(SPRStpReg)
002910151013     D $StpRegD        S                   like(OpeDesBrev)
002915151007     C*
002916151007     C*----------------------------------------------------*
002917151007     C*                MAIN LINE PROGRAM
002918151007     C*----------------------------------------------------*
002919151007     C                   EXSR      INZVAR
002920151007     C*
002921151007     C                   EXSR      ELABORA
002922151007     C*
002923151007     C                   EXSR      ENDPGM
002924151007     C/EJECT
002925151007     C************************************************************
002926151007     C* Lettura causali contabili di tipo iva
002927151007     C************************************************************
002928151007     C     Elabora       BEGSR
002929151007     C*
002930151007     C                   Eval      Opesocieta = Xscsoc
002931151007     C                   Eval      OpeTpc     = '1'
002932151007     C*
002933151007     C     K02OPE04      setll     AnOPE04l
002934151007     C*
002935151007     C                   Do        *hival
002936151007     C*
002937151007     C     K02OPE04      Reade     AnOPE04l
002938151007     C*
002939151007     C                   If        %eof(AnOPE04l)
002940151007     C                   Leave
002941151007     C                   Endif
002942151007     C*
002943151009     C* Se non esiste sulla tabella CB3 verifico che non esista neppure
002944160226     C* sulla tabella HD2.
002945151009     C* Se non esiste su emtrambe scrivo il file delle regole,
002946151009     C* verificando che il record non esista già.
002947151007     C*
002948151007     C                   Exsr      RepTabCB3
002949151009     C*
002950151009     C                   If        XTbErr <> '0'
002951151009     C*
002952160226     C                   Exsr      RepTabHD2
002953151009     C*
002954151009     C                   Endif
002955151007     C* per non trovato
002956151007     C                   If        XTbErr <> '0'
002957151007     C                   Exsr      Aggiorna
002958151007     C                   Endif
002963151007     C*
002964151007     C                   Enddo
002965151007     C
002966151007     C                   ENDSR
019645151002     C/EJECT
019646151006     C************************************************************
019647151007     C* Reperisco i dati della tabella CB3
019648151006     C************************************************************
019649151007     C     RepTabCB3     BEGSR
019653151006     C*
019654151007     C                   Reset                   SDGTABDS
019655151007     C*
019656151006     C                   Move      '1'           XTBRIC
019657151006     C                   Move      Xscsoc        XTBAZI
019658151006     C                   Move      *ZERO         XTBKEY
019659151007     C                   Move      'CB3'         XTBCOD
019660151007     C                   Move      OPECausale    XTBKEY
019661151007     C*
019662151007     C                   CALL      'SDGTABDR'
019663151007     C                   PARM                    SDGTABDS
019673151006     C*
019674151006     C                   ENDSR
019675151009     C/EJECT
019676151009     C************************************************************
019677160226     C* Reperisco i dati della tabella HD2
019678151009     C************************************************************
019679160226     C     RepTabHD2     BEGSR
019680151009     C*
019681151009     C                   Reset                   SDGTABDS
019682151009     C*
019683151009     C                   Move      '1'           XTBRIC
019684151009     C                   Move      Xscsoc        XTBAZI
019685151009     C                   Move      *ZERO         XTBKEY
019686160226     C                   Move      'HD2'         XTBCOD
019687151009     C                   Move      OPECausale    XTBKEY
019688151009     C*
019689151009     C                   CALL      'SDGTABDR'
019690151009     C                   PARM                    SDGTABDS
019692151009     C*
019693151009     C                   ENDSR
019694151007     C/EJECT
019695151007     C************************************************************
019696151007     C* Aggiornamento regola se non esiste già
019697151007     C************************************************************
019698151007     C     Aggiorna      BEGSR
019699151008     C*
019700151008     C                   Clear                   SDGSPRDS
019701151007     C*
019702151007     C                   Eval      SPRSocieta = XscSoc
019703151007     C                   Eval      SPRTipReg  = '5'
019704151007     C                   Eval      SPRStpReg  = '1'
019705151007     C                   Eval      SPRCausale = OPECausale
019706151007     C*
019707151007     C     K04SPR03      Chain(n)  SDGSPR03L
019708151007     C*
019709151007     C                   If        Not %found(SDGSPR03L)
019710151007     C*
019711151007     C                   Exsr      RieAna
019712151007     C*
019713151007     C                   Write     SdgSpr000                            22
019714151007     C*
019715151007     C     *IN22         IfEq      *ON
019716151007     C                   EXSR      MSGPGM
019717151013     C                   Exsr      EndPgm
019718151013     C                   Else
019719151013     C                   Exsr      Stampa
019720151013     C                   Endif
019721151007     C*
019722151007     C                   EndIf
019723151007     C*
019724151007     C                   ENDSR
019725151007     C/EJECT
019726151007     C************************************************************
019727151007     C* Riempimento anagrafica
019728151007     C************************************************************
019729151007     C     RieAna        BEGSR
019730151007     C*
019731151007     C                   Eval      SPRSocieta = XscSoc
019732151007     C                   Eval      SPRTipReg  = '5'
019733151007     C                   Eval      SPRStpReg  = '1'
019734151007     C                   Eval      SPRCausale = OPECausale
019735151007     C                   Add       1             $NrRiga
019736151007     C                   Z-add     $NrRiga       SPRNrRiga
019737151007     C                   Eval      SPRSocieta = XscSoc
019738151007     c                   Move      XScCUt        SPRUteImm
019739151007     C     *Eur          Move      *Date         SPRDtImm
019740151007     c                   Move      XScCUt        SPRUteUlMo
019741151007     C     *Eur          Move      *Date         SPRDtUlMo
019742151007     C*
019743151007     C                   ENDSR
019744151013     C/EJECT
019746151013     C************************************************************
019747151013     C* Stampa
019748151013     C************************************************************
019749151013     C     Stampa        BEGSR
019750151013     C*
019751151013     C                   Eval      $TipReg   = SPRTipReg
019752151013     C                   Exsr      DecTipRegB
019753151013     C                   Eval      $TipReg   = SPRStpReg
019754151013     C                   Exsr      DecStpReg
019755151013     C                   Eval      $Causale  = OpeCausale
019756151013     C                   Eval      $CausaleD = OpeDescr
019757151013     C*
019758151013     C                   If        *inOf = *on
019759151013     C                   Except    Testa
019760151013     C                   Setoff                                       OF
019761151013     C                   Endif
019762151013     C*
019763151013     C                   Except    Riga
019764151013     C*
019765151013     C                   Endsr
019766151013     C/EJECT
019767151013     C***********************************************************
019768151013     C* Decodifico Tipo Regola per Black List
019769151013     C***********************************************************
019770151013     C     DecTipRegB    BEGSR
019771151013     C*
019772151013     C                   Clear                   $TipRegD
019773151013     C*
019774151013     C                   Clear                   X07ValDs
019775151013     C                   Move      '1'           X07Ric
019776151013     C                   Move      *Off          X07All
019777151013     C                   Movel     'ANDIZ000 '   X07TRc
019778151013     C                   Movel     'SDGTIPREGB'  X07Cam
019779151013     C                   Movel     $TipReg       X07VAL
019780151013     C                   Callb     'X07VALR'
019781151013     C                   Parm                    X07ValDs
019782151013     C*
019783151013     C     X07ERR        IFEQ      *Off
019784151013     C                   MOVEL     X07DES        $TipRegD
019785151013     C                   endif
019786151013     C*
019787151013     C                   ENDSR
019788151013     C/EJECT
019789151013     C***********************************************************
019790151013     C* Decodifica Sottotipo Regola
019791151013     C***********************************************************
019792151013     C     DecStpReg     BEGSR
019793151013     C*
019794151013     C                   Clear                   $StpRegD
019795151013     C*
019796151013     C                   Clear                   X07ValDs
019797151013     C*
019798151013     C                   Move      '1'           X07Ric
019799151013     C                   Move      *Off          X07All
019800151013     C                   Movel     'ANDIZ000'    X07TRc
019801151013     C                   select
019802151013     C                   when      $TipReg = '1'
019803151013     C                   Movel     'SDGSTPREG1'  X07Cam
019804151013     C                   when      $TipReg = '2'
019805151013     C                   Movel     'SDGSTPREG2'  X07Cam
019806151013     C                   when      $TipReg = '3'
019807151013     C                   Movel     'SDGSTPREG3'  X07Cam
019808151013     C                   when      $TipReg = '4'
019809151013     C                   Movel     'SDGSTPREG4'  X07Cam
019810151013     C                   when      $TipReg = '5'
019811151013     C                   Movel     'SDGSTPREG5'  X07Cam
019812151013     C                   endsl
019813151013     C                   Movel     $StpReg       X07VAL
019814151013     C                   Callb     'X07VALR'
019815151013     C                   Parm                    X07ValDs
019816151013     C*
019817151013     C     X07ERR        IFEQ      *Off
019818151013     C                   MOVEL     X07DES        $stpregd
019819151013     C                   endif
019820151013     C*
019821151013     C                   ENDSR
019822000000     C/EJECT
019823000000     C************************************************************
019900000000     C* Messaggio errore
020000000000     C************************************************************
020100000000     C     MsgPgm        BEGSR
020200000000     C*
020300000000     C                   Reset                   SDGMSGDS
020400000000     C*
020500000000     C                   Movel     'N'           MsgRsp
020600000000     C                   Movel     *Blanks       MsgTpR
020700000000     C                   Z-Add     0             MsgMsg
020800000000     C                   Movel     'S'           MsgEmV
020900000000     C                   Z-Add     10            MsgRgp
021000000000     C                   Movel     'N'           MsgLck
021100000000     C                   Movea     Status        Sts
021200000000     C                   Movel     'N'           MsgCnl
021300000000     C                   Movel     'N'           MsgVid
021400000000     C                   Movel     'N'           MsgOpe
021500000000     C                   Movel     'N'           MsgStp
021600000000     C*
021700000000     C                   Call      'SDGMSGR'
021800000000     C                   Parm                    SDGMSGDS
021900000000     C                   Parm                    XMSGTxt         135
022000000000     C                   Parm                    XMSGInz           1
022100000000     c* codici dei 3 messaggi possibili da emettere a video
022200000000     C* "Si sono verificati errori che hanno impedito l'esecuzione"
022300000000     C                   PARM      'COS0377'     MSGCd1            7
022400000000     C                   PARM                    MSGCd2            7
022500000000     C                   PARM                    MSGCd3            7
022600000000     c* codice del messaggio da mandare alla coda dell'operatore
022700000000     C                   PARM                    MSGSn1            7
022800000000     c* codici(lunghi 7  ciascuno) dei 9 bottoni
022900000000     C                   PARM                    MSGCdTxt         63
023000000000     c* codici(lunghi 7  ciascuno) dei 2 titoli (titolo e sottotitolo)
023100000000     c* per stampa e video
023200000000     C                   PARM                    MSGCdTit         14
023300000000     c* flag per chiusura prtf
023400000000     C                   PARM                    MSGClPrtf         1
023500000000     C* variabili per messaggi
023600000000     C                   PARM                    VarMsg1         100
023700000000     C                   PARM                    VarMsg2         100
023800000000     C                   PARM                    VarMsg3         100
023900000000     C*
024000000000     C                   ENDSR
024100000000     C/EJECT
024200000000     C************************************************************
024300000000     C* OPERAZIONI INIZIALI
024400000000     C************************************************************
024500151007     C     INZVAR        BEGSR
024501151007     C*
024604151006     C* Reperimento dati società
024605151006    >C                   MOVEL     'SOC001'      TIPXSC
024606151006     C                   MOVEL     *BLANK        SOCXSC
024607151001     C                   Exsr      RepSoc
024608151006     C     RTNXSC        IFNE      '1'
024609151006     C                   MOVEL     XSOCDS        SOC001
024610151006     C                   ELSE
024611151006     C                   RETURN
024612151006     C                   ENDIF
024614151007     C* Reperisco ultimo numero riga scritto per tipologia Esclusione,
024615151007     C* e sottotipologia causale.
024616151007     C                   Eval      SPRSocieta = XscSoc
024617151007     C                   Eval      SPRTipReg  = '5'
024618151007     C                   Eval      SPRStpReg  = '1'
024826151002     C*
024827151007     C     K03SPR01      SETGT     SDGSPR01l
024828151007     C     K03SPR01      READPE    SDGSPR01l
024829151007     C                   If        not %eof(SDGSPR01L)
024830151007     C                   Z-add     SPRNrRiga     $NrRiga
024831151007     C                   Else
024832151007     C                   Z-add     0             $NrRiga
024833151007     C                   Endif
024834151013     C*
024835151013     C                   Seton                                        OF
024836151007     C*
024837151002     C                   ENDSR
024838151002     C/EJECT
024839151002     C************************************************************
024840151002     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
024841151002     C************************************************************
024842151002     C     DEFCAM        BEGSR
024843151002     C*
024844151002     C* klist
024845151002     C*
024846151007     C* AnOPE04l
024847151007     C     K02OPE04      KLIST
024848151007     C                   KFLD                    OPESocieta
024849151007     C                   KFLD                    OPETpc
024850151006     C*
024851151007     C* SdgSPR03l
024852151007     C     K04SPR03      KLIST
024853151007     C                   KFLD                    SPRSocieta
024854151007     C                   KFLD                    SPRTipReg
024855151007     C                   KFLD                    SPRStpReg
024856151007     C                   KFLD                    SPRCausale
024857151006     C*
024859151007     C* SdgSPR01l
024860151007     C     K04SPR01      KLIST
024861151007     C                   KFLD                    SPRSocieta
024862151007     C                   KFLD                    SPRTipReg
024863151007     C                   KFLD                    SPRStpReg
024864151007     C                   KFLD                    SPRNrRiga
024865151007     C* SdgSPR01l
024866151007     C     K03SPR01      KLIST
024867151007     C                   KFLD                    SPRSocieta
024868151007     C                   KFLD                    SPRTipReg
024869151007     C                   KFLD                    SPRStpReg
024870151002     C                   ENDSR
024871151002     C/EJECT
024872151001     C************************************************************
024873151001     C* REPERIMENTO DATI SOCIETA'
024874151001     C************************************************************
024875151001     C     REPSOC        BEGSR
024876151001     C*
024877151001     C                   CALL      'SDGSOCDR'
024878151001     C                   PARM                    TIPXSC            6
024879151001     C                   PARM                    SOCXSC            3
024880151001     C                   PARM                    CDSXSC            9 0
024881151001     C                   PARM                    MODXSC            3
024882151001     C                   PARM      *BLANKS       RTNXSC            1
024883151001     C                   PARM                    XSOCDS
024884151001     C                   PARM                    KPJBA
024885151001     C*
024886151001     C                   ENDSR
024887151001     C/EJECT
024900000000     C************************************************************
025000000000     C* Fine programma
025100000000     C************************************************************
025200000000     C     ENDPGM        BEGSR
025300000000     C*
025400000000     C                   SETON                                        LR
025500000000     C                   Return
025600000000     C*
025700000000     C                   ENDSR
025800000000     C/EJECT
025900151006     C************************************************************
026100151013     OQSYSPRT   E            TESTA            02
026200151013     O                                           35 '***---'
026300151013     O                                         +  0 '------------------------'
026400151013     O                                         +  0 '------------------------'
026500151013     O                                         +  0 '--***'
026600151013     O          E            TESTA            03
026700151013     O                                           35 '***-- '
026800151013     O                                         +  0 'Black list: elenco caual'
026900151013     O                                         +  0 'i da escludere inserite '
027000151013     O                                         +  0 '--***'
027100151013     O          E            TESTA            04
027200151013     O                                           35 '***---'
027300151013     O                                         +  0 '------------------------'
027400151013     O                                         +  0 '------------------------'
027500151013     O                                         +  0 '--***'
027600151013     O*
027700151013     O          E            TESTA            06
027800151013     O                                            3 'Soc'
027801151013     O                                           15 'Tipo regola'
027802151013     O                                           44 'Sottotipo  regola'
027900151013     O                                           70 'Causale da escludere'
028700151013     O*
028800151013     O          E            Riga        1
028801151013     O*
028900151013     O                       Xscsoc               3
028901151013     O                       $TipReg              5
028902151013     O                       $TipRegD            26
028903151013     O                       $StpReg             28
028904151013     O                       $StpRegD            49
028905151013     O                       $Causale            54
028906151013     O                       $CausaleD           99
029900151013     O*
