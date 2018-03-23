000100000712     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300960909     H*PARMS CVTOPT(*DATETIME)
000400000712     H DECEDIT('0,') DATEDIT(*DMY/)
000500921221     H*----------------------------------------------------------------
000600960909     H* NDCK31R - CONTROLLI FORMALI DATI INSOLUTI
000700921221     H*----------------------------------------------------------------
000800921221     H* IL CONTROLLO AGGIORNA LA TESTATA COL SEGUENTE CRITERIO:
000900960909     H*   1) SE ERA RICHIESTO SOLO IL CONTROLLO (RTKCONV='C')
001000960909     H*      - RTKCONV='O' SE CONTROLLO "OK"
001100960909     H*      -    "   ='E' SE ESISTONO ERRORI NEL DETTAGLIO
001200960909     H*   2) SE ERA RICHIESTA ANCHE LA CONTABILIZZAZIONE (RTKCONV='P')
001300960909     H*      - RTKCONV='P' SE CONTROLLO "OK"
001400960909     H*      -    "   ='E' SE ESISTONO ERRORI NEL DETTAGLIO
001500960909     H* SOLO SE RTKCONV='P' VIENE LANCIATA LA CONTABILIZZAZIONE
001600921221     H*----------------------------------------------------------------
001700960909     FNDRTK01L  UF   E           K DISK
001800960909     FNDRDK01L  IF   E           K DISK
001900960909     FANOPE01L  IF   E           K DISK
002000960909     FNDEFA02L  IF   E           K DISK
002100921215     FQSYSPRT   O    F  132        PRINTER
002200921215     F                                     INFDS(CTRDS)
002300960909     D*----------
002400921215     D KPJBA         E DS
002500960909     D*----------
002600960909     D XATBDS        E DS
002700960918     D TABI07        E DS                  EXTNAME(ANGI07DS)
002800960909     D*----------
002900960909     D SOC001        E DS                  EXTNAME(XSOC001DS)
003000960909     D XSOCDS          DS          1000
003100960909     D*----------
003200000103     D* Parametri Insoriba
003300000103     D XPAOUT          DS          2000
003400000103    >D NDCK37DS      E DS                  INZ
003500000103     D*-------------
003600960909     D ND002DS       E DS
003700960918     D $Esito          S              1
003800960918     D $GesMsg         S              1
003900960918     D $LenOut         S              9B 0
004000960918     D $Struttura      S             10
004100960918     D $Kcc            S              6
004200960918     D $ksc            S              8
004300960918     D $Soc            S              3
004400960909     D*-------------
004500960918     D ANBRADS       E DS                  EXTNAME(ANBRA00F) INZ
004600960918     D*----------
004700960930     D $SrcVar         S             30S 0
004800960930     D $SrcVarPr       S              9B 0 Inz(30)
004900960930     D $SrcVarDe       S              9B 0
005000960930     D $EdtCode        S              1
005100960930     D $LenFld         S              3S 0
005200960930     D $RcvrVar        S             50
005300960930     D $Err            S              1
005400960930     D*-------------
005500960909     D STATUS         SDS           333
005600960909     D  DSPGM            *PROC
005700960909     D  PARMS            *PARMS
005800960909     D*-------------
005900921215     D CTRDS           DS
006000921215     D  CTR                  367    368B 0
006100960909     D*----------
006200960917     D XMSGDS        E DS
006300960917     D $COS            S             27    DIM(2) CTDATA PERRCD(1)              Costanti pgm
006400960917     D*----------
006500960917     D SK              S             66    DIM(16) CTDATA PERRCD(1)             STAMPA
006600960917     D*----------
006700960909     D DatMin          C                   CONST('0001-01-01')
006800000114C1309D ZDateISO        S               D
006900960917     D*----------
007000921215     C*****************************************************
007100921221     C*          FLUSSO PRINCIPALE
007200921215     C*****************************************************
007300960909     C*
007400981116     C     XscSoc        Setll     NDRTK000
007500921215     C                   DO        *HIVAL
007600960909     C*
007700921215     C     M010          TAG
007800960909     C     XscSoc        READE     NDRTK000                               50
007900960909     C     *IN50         CABEQ     *ON           FINE
008000960911     C* NO CONTABILIZZATI/ANNULLATI
008100960909     C     RTKATTI       CABEQ     'A'           M010
008200921215     C*
008300921215     C* ELABORO SOLO SE E' UNA TESTATA SELEZIONATA
008400921215     C*
008500960909     C     RTKCONV       IFEQ      'C'
008600960909     C     RTKCONV       OREQ      'P'
008700960909     C*
008800960912     C* CONTROLLI TESTATA
008900921215     C*
009000960919     C                   SetOff                                       5163
009100960912     C* Conto banca c/c di accredito
009200960918     C                   Move      RTKBKCC       $Kcc
009300960918     C                   Move      RTKBKSC       $Ksc
009400960912     C                   EXSR      CALLMVC002
009500960912     C                   If        $Esito <> *Off
009600960919     C                             or RTKBKSC = *Blank
009700960912     C                   SETON                                        51
009800960912     C                   EndIf
009900000103     C* Conto effetti in sofferenza
010000000103     C* (se no impostato reg. banca unita a prec., nel qual caso non serve)
010100000103     C                   If        TpRegzBK37 <> 'U'
010200960918     C                   Move      RTKEKCC       $Kcc
010300960918     C                   Move      RTKEKSC       $Ksc
010400960912     C                   EXSR      CALLMVC002
010500960912     C                   If        $Esito <> *Off
010600960919     C                             or RTKEKSC = *Blank
010700960912     C                   SETON                                        63
010800960912     C                   EndIf
010900000103     C                   EndIf
011000960919     C*
011100960919     C                   If        *In51 = *On or *In63 = *On
011200960919     C                   EXSR      STAMPA                                       STAMPA ERRORI
011300960919     C                   Move      RtkConv       TipEla
011400960919     C                   MOVEL     'E'           RTKCONV
011500960919     C                   UPDATE    NDRTK000
011600960919     C                   Exsr      WinErr
011700960919     C                   GOTO      M010
011800960919     C                   EndIf
011900960909     C*
012000960909     C* CONTROLLI DETTAGLIO
012100960909     C*
012200921215     C     SUB1          TAG
012300960909     C                   MOVE      *BLANK        ERRORE
012400960909     C*
012500960909     C     k02rdk01      SETLL     NDRDK000
012600921215     C                   DO        *HIVAL
012700960909     C     k02rdk01      READE     NDRDK000                               20
012800960909     C     *IN20         IFEQ      *OFF
012900921215     C*
013000960909     C* ELABORO SOLO I RECORD NON CONTABILIZZATI/ANNULLATI (RDKBA14<>'A')
013100921215     C*
013200960909     C     RDKBA14       IFNE      'A'
013300921215     C                   MOVEA     *ALL'0'       *IN(52)
013400960909     C*
013500960909     C* REPERIMENTO E CONTROLLO CAUSALE INSOLUTO
013600921215     C*
013700960909     C                   MOVE      XSCCUT        XTBUTE
013800960909     C                   MOVE      '1'           XTBRIC
013900960909     C                   MOVE      XSCSOC        XTBAZI
014000960909     C                   MOVE      *ZERO         XTBKEY
014100960909     C                   MOVE      'I07'         XTBCOD
014200960909     C                   MOVE      RDKCAUS       XTBKEY
014300960923     C                   CALLb     'XATB'
014400960909     C                   PARM                    XATBDS
014500960909     C     XTBERR        IFEQ      *ON
014600960909     C                   SETON                                        59
014700960909     C                   ELSE
014800960909     C                   MOVEL     XTBUNI        TABI07
014900960909     C                   Move      CauI07        OpeCausale
015000960909     C     k02ope01      CHAIN     ANOPE01L                           60
015100960909     C     *IN60         IFEQ      *OFF
015200960909     C     OpeNa0        IFNE      '0'
015300960909     C     OpeNa0        ANDNE     '1'
015400960909     C                   SETON                                        62
015500960909     C                   ENDIF
015600960909     C                   ENDIF
015700960909     C                   ENDIF
015800921215     C*
015900960909     C* CONTROLLO EFFETTO
016000921215     C*
016100960909     C                   MOVE      'CG'          EFACTB
016200960909     C                   Z-ADD     RDKDSOR       EFANREFF
016300960909     C     K03EFA02      CHAIN     NDEFA02L                           52
016400960909     C     *IN52         IFEQ      *OFF
016500960909     C*
016600960909     C* CONTROLLO CHE EFFETTO NON SIA ANNULLATO
016700960909     C     EfaEsito      CABEQ     '1'           SUB2                     53
016800921215     C*
016900960917     C* CONTROLLO CHE EFFETTO NON SIA INSOLUTO
017000960917     C     EfaEsito      CABEQ     '4'                                    54
017100921215     C*
017200960909     C* CONTROLLO CHE LO STATO EFFETTO SIA PRESENTATO
017300960912     C     EfaStatus     CABNE     '4'                                5555
017400921215     C*
017500960909     C* CONTROLLO CHE LA DATA SCADENZA SUL FILE DETTAGLIO COINCIDA CON
017600960909     C* QUELLA SUL FILE PORTAFOGLI
017700960909     C     EfaDtSca      CABNE     RDKDSCA                            5656
017800921215     C*
017900921215     C* CONTROLLO CHE GLI IMPORTI SUL FILE DETTAGLIO E SUL
018000921215     C* FILE PORTAFOGLI CORRISPONDANO
018100981109     C     EfaImportD    CABNE     RDKIMPO                            5757
018200921215     C*
018300960918     C* CONTROLLO CHE LA BANCA DELL'EFFETTO SIA UGUALE A QUELLA
018400960918     C* DELLA TESTATA (...NON DEVE ESSERE ERRORE BLOCCANTE!!!)
018500960918     C                   Eval      $Struttura = 'ANBRA'
018600960918     C                   Eval      $LenOut = %Size(ANBRADS)
018700960918     C                   Callb     'NDDVABRA'
018800960918     C                   PARM      '01'          $Operazion        2
018900960918     C                   PARM                    XScSoc
019000960918     C                   PARM                    *Omit
019100960918     C                   PARM                    *Omit
019200960918     C                   PARM                    RtkBKcc
019300960918     C                   PARM                    RtkBKsc
019400960918     C                   PARM                    $Struttura
019500960918     C                   PARM                    $LenOut
019600960918     C                   PARM                    $Esito
019700960918     C                   PARM                    ANBRADS
019800960918     C                   If        $Esito = *Off
019900960918     C                   If        EfaBanca <> BraBanca
020000960918     C                   Seton                                        58
020100960918     C                   EndIf
020200960918     C                   Else
020300960918     C                   Seton                                        58
020400960918     C                   EndIf
020500960909     C*
020600960909     C                   ENDIF
020700921215     C*
020800921215     C     SUB2          TAG
020900921215     C     '1'           LOOKUP    *IN(52)                                25
021000960909     C     *IN25         IFEQ      *ON
021100960909     C                   EXSR      STAMPA                                       STAMPA ERRORI
021200960918     C* Se solo il 58 acceso non è errore bloccante
021300041012>>>>>C**                 SetOff                                       58
021400960918     C     '1'           LOOKUP    *IN(52)                                25
021500960918     C     *IN25         IFEQ      *ON
021600960909     C                   MOVE      '1'           ERRORE
021700960909     C                   ENDIF
021800960918     C                   ENDIF
021900921215     C*
022000960909     C                   ENDIF
022100960909     C                   ENDIF
022200960909     C  N20              ENDDO
022300960909     C*
022400960909     C* AGGIORNO TESTATA
022500921221     C*
022600960917     C                   Move      RtkConv       TipEla            1
022700921215     C     ERRORE        IFNE      *BLANK
022800960909     C                   MOVEL     'E'           RTKCONV
022900921215     C                   ELSE
023000960909     C     RTKCONV       IFEQ      'C'
023100960909     C                   MOVEL     'O'           RTKCONV
023200960909     C                   ENDIF
023300960909     C                   ENDIF
023400960909     C                   UPDATE    NDRTK000
023500960917     C*
023600960917     C* ERRORE --> Emissione finestra segnalazione
023700960917     C*
023800960917     C                   If        RtkConv = 'E'
023900960917     C                   Exsr      WinErr
024000960917     C                   EndIf
024100960917     C*
024200960917     C* CONTABILIZZAZIONE (solo se richiesta e se controlli OK)
024300921215     C*
024400960909     C     RTKCONV       IFEQ      'P'
024500960909     C                   MOVEL     RTKCARN       KPJBU
024600960909     C                   CALL      'NDCK32R'
024700921215     C                   PARM                    KPJBA
024800960909     C                   ENDIF                                                  RTCONV=P
024900960909     C*
025000960909     C                   ENDIF                                                  RTCONV='C''P'
025100960909     C                   ENDDO                                                  *HIVAL
025200921215     C*
025300921215     C     FINE          TAG
025400921215     C                   SETON                                        LR
025500921215     C*****************************************************
025600921215     C* SUBROUTINE DI STAMPA
025700921215     C*****************************************************
025800960909     C     STAMPA        BEGSR
025900960918     C*
026000921215     C* RAGIONE SOCIALE
026100960912     C     RDKAKSC       IFNE      *BLANK
026200960918     C                   Move      RDKAKCC       $Kcc
026300960918     C                   Move      RDKAKSC       $Ksc
026400960909     C                   EXSR      CALLMVC002
026500960909     C                   If        $Esito = *Off
026600960909     C                   Movel     desbrev002    RAGSOC           20
026700960909     C                   Else
026800960909     C                   Movel     *blank        RAGSOC
026900960909     C                   EndIf
027000960918     C                   ENDIF
027100960909     C*
027200960909     C* DATA SCADENZA
027300960909     C     RdkDSca       IfNe      DatMin
027400960909     C                   CALLb     'XDT4000'
027500960909     C                   PARM      RdkDSca       XDTAMG           10
027600960909     C     DatSca        PARM                    XDTGMA            6 0
027700960909     C                   PARM      2             XDTSTA            1 0
027800960909     C                   Else
027900960909     C                   Z-Add     0             DatSca            6 0
028000960909     C                   EndIf
028100960909     C*
028200960909     C* IMPORTO DISPOSIZIONE
028300981106     C                   Z-Add     RdkImpo       Importo          15 3
028400921215     C*
028500921215     C* DATA RIFERIMENTO PARTITA
028600960909     C   52              Z-Add     0             DATRIF
028700960909     C   52              Z-Add     0             EfaNrPar
028800960909     C  N52EfaDtPar      IfNe      DatMin
028900960909     C                   CALLb     'XDT4000'
029000960909     C                   PARM      EfaDtPar      XDTAMG           10
029100960909     C     DatRif        PARM                    XDTGMA            6 0
029200960909     C                   PARM      2             XDTSTA            1 0
029300960909     C                   Else
029400960909     C                   Z-Add     0             DatRif            6 0
029500960909     C                   EndIf
029600921215     C*
029700960909     C* STAMPA ERRORE
029800921215     C     CTR           IFGT      55
029900921215     C                   EXCEPT    TESTA
030000960909     C                   ENDIF
030100921215     C                   EXCEPT    ERRST
030200921215     C*
030300921215     C                   ENDSR
030400921215     C*
030500921215     C*****************************************************
030600960909     C* SUBROUTINE INIZIALE
030700921215     C*****************************************************
030800921221     C     *INZSR        BEGSR
030900921215     C*
031000000114C1309C     *DMY          Move      Udate         ZDateISO
031100000114C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
031200000114     C*
031300921215     C     *ENTRY        PLIST
031400921215     C                   PARM                    KPJBA
031500960909     C*
031600960909     C     k02rdk01      KLIST
031700960909     C                   KFLD                    XScSoc
031800960909     C                   KFLD                    RtkCarN
031900921215     C*
032000960909     C     k02ope01      KLIST
032100960909     C                   KFLD                    XScSoc
032200960909     C                   KFLD                    OpeCausale
032300960909     C*
032400960909     C     k03efa02      KLIST
032500960909     C                   KFLD                    XScSoc
032600960909     C                   KFLD                    EfaCtb
032700960909     C                   KFLD                    EfaNrEff
032800960909     C*
032900960909     C* Reperimento dati società
033000960909     C*
033100960909    >C                   MOVEL     'SOC001'      TIPXSC
033200960909     C                   MOVEL     *BLANK        SOCXSC
033300960909     C                   EXSR      REPSOC
033400960909     C     RTNXSC        IFNE      '1'
033500960909     C                   MOVEL     XSOCDS        SOC001
033600960909     C                   ELSE
033700960909     C                   GOTO      FINE
033800960909     C                   ENDIF
033900000103     C*
034000000103     C* Reperimento parametri di base InsoRiBa
034100000103     C*
034200000103     C                   CALLB     'XPAR'
034300000103     C                   parm                    xscsoc
034400000103     C                   parm      'HBINRIBA'    xpapar            8
034500000103     C                   parm                    xpaout
034600000103     C                   parm      *off          xpaerr            1
034700000103     C                   parm      *blank        xparic            1
034800000103     C     xpaerr        ifne      *on
034900000103     C                   movel     xpaout        ndck37ds
035000000103     C                   endif
035100960909     C*
035200960909     C* Valorizzazione campi univoci testate
035300960909     C*
035400960909     C                   MOVEL     KNSIF         NOMSIF           10
035500960909     C                   MOVEL     XSCDSI        NOMDIT           20
035600960909     C                   MOVEL     DSPGM         NOMPGM           10
035700960916     C* Reperisco nome sistema informatico
035800960916     C                   CALL      'XNETA'                              21
035900960916     C                   PARM                    NOMSYS            8
036000960910     C                   TIME                    ORA               6 0
036100960909     C*
036200960909     C                   MOVE      ' '           ERRORE            1
036300921215     C                   Z-ADD     66            CTR
036400921215     C*
036500921215     C                   ENDSR
036600960917     C******************************************************
036700960917     C* EMISSIONE MESSAGGIO ERRORE                         *
036800960917     C******************************************************
036900960917     C     WINERR        BEGSR
037000960917     C*
037100960917     C                   CLEAR                   XMSGDS
037200960917     C*
037300960917     C                   Move      *Blank        XMsgTesti
037400960917     C                   If        XMsgInz = *Blank
037500960917     C                   Move      *On           XMsgInz
037600960917     C                   Else
037700960917     C                   Move      *Off          XMsgInz
037800960917     C                   EndIf
037900960917     C*
038000960930     C                   CallB     'X23EDITR'
038100960930     C                   Parm      RtkCarN       $SrcVar
038200960930     C                   Parm                    $SrcVarPr
038300960930     C                   Parm      0             $SrcVarDe
038400960930     C                   Parm      'Z'           $EdtCode
038500960930     C                   Parm      9             $LenFld
038600960930     C                   Parm      *Blank        $RcvrVar
038700960930     C                   Parm                    $Err
038800960930     C                   If        $Err = *On
038900960930     C                   Movel     RtkCarN       CarNum            9
039000960930     C                   Else
039100960930     C                   Movel     $RcvrVar      CarNum
039200960930     C                   EndIf
039300960930     C*
039400960917     C                   MOVEL     $COS(1)       IDMSG
039500960917     C                   CALLB     'XRTVM'
039600960917     C                   PARM                    IDMSG            27
039700960917     C                   PARM                    TXTMSG          100
039800960917     C                   PARM                    ERRMSG            1
039900960917     C                   PARM      *ON           CTRMSG            1
040000960917     C                   PARM      60            LENMSG            3 0
040100960917     C     ERRMSG        IFNE      *ON
040200960917     C                   MOVEL     TXTMSG        MSGMS1
040300960917     C                   CAT       CarNum:1      MSGMS1
040400960917     C                   ELSE
040500960917     C                   MOVEL     *ALL'?'       MSGMS1
040600960917     C                   ENDIF
040700960917     C*
040800960917     C                   If        TipEla = 'P'
040900960917     C                   MOVEL     $COS(2)       IDMSG
041000960917     C                   CALLB     'XRTVM'
041100960917     C                   PARM                    IDMSG            27
041200960917     C                   PARM                    TXTMSG          100
041300960917     C                   PARM                    ERRMSG            1
041400960917     C                   PARM      *ON           CTRMSG            1
041500960917     C                   PARM      70            LENMSG            3 0
041600960917     C     ERRMSG        IFNE      *ON
041700960917     C                   MOVEL     TXTMSG        MSGMS3
041800960917     C                   ELSE
041900960917     C                   MOVEL     *ALL'?'       MSGMS3
042000960917     C                   ENDIF
042100960917     C                   EndIf
042200960917     C*
042300960917     C                   MOVEL     'N'           MSGRSP
042400960917     C                   MOVEL     'S'           MSGEMV
042500960917     C                   Z-ADD     10            MSGRGP
042600960917     C                   MOVEL     'N'           MSGCNL
042700960917     C                   MOVEL     'N'           MSGVID
042800960917     C                   MOVEL     'N'           MSGOPE
042900960917     C                   MOVEL     'N'           MSGSTP
043000960917     C                   CALL      'XMSGR'
043100960917     C                   PARM                    XMSGDS
043200960917     C                   PARM                    XMSGTESTI       135
043300960917     C                   PARM                    XMSGINZ           1
043400960917     C*
043500960917     C                   ENDSR
043600960917     C************************************************************
043700960917     C* DECODIFICA CONTO
043800960917     C************************************************************
043900960909     C     CALLMVC002    BEGSR
044000960909     C*
044100960909     C                   Clear                   ND002DS
044200960909     C                   EVAL      $LenOut = %Size(ND002DS)
044300960909     C                   CallB     'NDMVC002'
044400960909     C                   PARM      XScSoc        $soc
044500960909     C                   PARM                    $kcc
044600960909     C                   PARM                    $ksc
044700960909     C                   PARM                    *omit
044800960909     C                   PARM      *OFF          $GesMsg
044900960909     C                   PARM      *OFF          $Esito
045000960909     C                   PARM      'ND002DS'     $Struttura
045100960909     C                   PARM                    ND002DS
045200960909     C                   PARM                    $LenOut
045300960909     C*
045400960909     C                   ENDSR
045500960909     C************************************************************
045600960909     C* REPERIMENTO DATI SOCIETA'
045700960909     C************************************************************
045800960909     C     REPSOC        BEGSR
045900960909     C*
046000960909     C                   CALLB     'XSOC'
046100960909     C                   PARM                    TIPXSC            6
046200960909     C                   PARM                    SOCXSC            3
046300960909     C                   PARM                    CDSXSC            9 0
046400960909     C                   PARM                    MODXSC            3
046500960909     C                   PARM      *BLANKS       RTNXSC            1
046600960909     C                   PARM                    XSOCDS
046700960909     C                   PARM                    KPJBA
046800960909     C*
046900960909     C                   ENDSR
047000921221     C*----------------------------------------------------------------
047100921215     OQSYSPRT   E            TESTA            01
047200960909     O                       NOMDIT              21
047300960909     O                       KNMUS              110
047400000712C1309X*****                  UDATE         Y    120
047500000114C1309O                       ZDate         Y    120
047600960909     O                                          126 'Pag.'
047700960909     O                       PAGE          Z    131
047800921215     O          E            TESTA            02
047900960916     O                       NOMSYS               9                             000010
048000960916     O                       NOMSIF              20                             000010
048100960918     O                                           47 '**'
048200960918     O                                           61 'LISTA ERRORI'
048300960918     O                                           75 'ESITI EFFETTI'
048400960918     O                                           79 '**'
048500960909     O                       NOMPGM             110
048600960909     O                       ORA                120 '  :  :  '
048700921215     O          E            TESTA            03
048800960909     O                                          110 'gruppo PRO'
048900921215     O          E            TESTA            05
049000921215     O                                           18 'BANCA   EMISSIONE'
049100921215     O                                           43 'CLIENTE   RAG. SOCIALE'
049200921215     O                                           80 'IMPORTO RICEVUTA'
049300960918     O                                          103 'SCADENZA   DATA RIF.'
049400960918     O                                          117 'NR.RIF.'
049500960918     O                                          130 'NR.CARICO'
049600960918     O          E            ERRST       2  1
049700960909     O                       RTKOABI              6
049800960909     O                       RDKDSOR       Z     18
049900960909     O                       RDKAKSC             28
050000960918     O                       RAGSOC              51
050100000712D0638X*****                  IMPORTO       2     80
050200000712  "  O                       IMPORTO       2     75
050300000712D0638O                       EFADivisa           80
050400960909     O                       DATSCA              91 '  /  /  '
050500960909     O                       DATRIF             102 '  /  /  '
050600960918     O                       EFANRPAR      Z    115
050700960918     O                       RTKCARN       Z    129
050800921215     O          E    51      ERRST          1
050900921215     O                       SK(1)               66
051000921215     O          E    52      ERRST          1
051100921215     O                       SK(3)               66
051200921215     O          E    53      ERRST          1
051300921215     O                       SK(4)               66
051400921215     O          E    54      ERRST          1
051500921215     O                       SK(5)               66
051600921215     O          E    55      ERRST          1
051700921215     O                       SK(6)               66
051800921215     O          E    56      ERRST          1
051900921215     O                       SK(7)               66
052000921215     O                       SK(8)              132
052100921215     O          E    57      ERRST          1
052200921215     O                       SK(9)               66
052300921215     O                       SK(10)             132
052400921215     O          E    58      ERRST          1
052500921215     O                       SK(11)              66
052600921215     O          E    59      ERRST          1
052700921215     O                       SK(12)              66
052800921215     O          E    60      ERRST          1
052900921215     O                       SK(13)              66
053000921215     O                       SK(14)             132
053100921215     O          E    61      ERRST          1
053200921215     O                       SK(15)              66
053300921215     O          E    62      ERRST          1
053400921215     O                       SK(16)              66
053500960912     O          E    63      ERRST          1
053600960912     O                       SK(2)               66
053700921215     O*
053800960917**  Costanti x XMSGR
053900960917PROMSG    *LIBL     COS0183    Errore controllo carico
054000960917PROMSG    *LIBL     COS0184    Contabilizz. non eseguita
054100960909** SCHIERA MESSAGGI DI ERRORE
054200960912ERRORE. Conto banca C/C di addebito non presente in anagrafica.
054300960912ERRORE. Conto effetti in sofferenza non presente in anagrafica.
054400960916ERRORE. Non trovato effetto sul file portafoglio attivo.
054500921215ERRORE. Effetto annullato.
054600921215ERRORE. Effetto già insoluto.
054700921215ERRORE. Effetto non presentato.
054800960916ERRORE. La data scadenza sul file portafoglio non corrisponde a qu
054900930114ella sul file dettagli esiti effetti.
055000960916ERRORE. L'importo sul file portafoglio non corrisponde a quello su
055100960916l file dettagli esiti effetti.
055200960918AVVERTENZA. Effetto presentato su un altro istituto.
055300921215ERRORE. Non reperita in tabella la causale insoluto.
055400960916ERRORE. La causale insoluto reperita non ha riferimenti sull'anagr
055500960916afica delle causali contabili.
055600921215ERRORE. La causale insoluto è stata annullata.
055700921215ERRORE. Causale insoluto errata.
