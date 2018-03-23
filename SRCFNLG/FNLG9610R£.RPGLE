000100050928     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE')
000200050928
000300050928     Fcnaco00f  IF   E           K DISK
000400050930     F                                     USROPN
000500050928     Fazorg01l  IF   E           K DISK
000600050930     F                                     USROPN
000700050928     Ffnlg9610d CF   E             WORKSTN
000800050930     F                                     USROPN
000900050928     F                                     INDDS(indDspF)
001000050928     F                                     INFDS(infDspF)
001100050928     F                                     PREFIX(dsp)
001200050928
001300050928     ***************************************************************************
001400050928     **?
001500050928     **?Definizione costanti.
001600050928     **?
001700050928     ***************************************************************************
001800050928     D NonVisionate    C                   'N'
001900050928     D Scartate        C                   'S'
002000050929     D Partenza        C                   'P'
002100050929     D Livello2        C                   '2'
002200050928     D Enter           C                   X'F1'
002300050928     D F3              C                   X'33'
002400050929     D F5              C                   X'35'
002500050928     D F7              C                   X'37'
002600050929     D F12             C                   X'3C'
002700050928     D F14             C                   X'B2'
002800050928     D F18             C                   X'B6'
002900050928
003000050928     ***************************************************************************
003100050928     **?
003200050928     **?Definizione procedure.
003300050928     **?
003400050928     ***************************************************************************
003500050928     D atoi            PR            10I 0
003600050928     D                                     EXTPROC('atoi')
003700050928     D  a                              *
003800050928     D                                     VALUE
003900050928     D                                     OPTIONS(*STRING)
004000050928
004100050928     D xalfa1          PR
004200050928     D                                     EXTPGM('XALFA1')
004300050928     D  a                            30
004400050928     D  p                             1  0
004500050928     D  a                            48
004600050928     D  p                             4  0
004700050928     D  p                             7  0
004800050928     D  p                             1  0
004900050928     D  a                            90
005000050928
005100050928     D tibs34r         PR
005200050928     D                                     EXTPGM('TIBS34R')
005300050928     D  a
005400050928     D                                     LIKE(tibs34ds)
005500050929
005600050929     D fnls04r         PR
005700050929     D                                     EXTPGM('FNLS04R')
005800050929     D  a
005900050929     D                                     LIKE(kpjba)
006000050929     D  a
006100050929     D                                     LIKE(fidna1ds)
006200050929
006300050929     D fnls05r         PR
006400050929     D                                     EXTPGM('FNLS05R')
006500050929     D  a
006600050929     D                                     LIKE(kpjba)
006700050929     D  a
006800050929     D                                     LIKE(fidna1ds)
006900050929
007000050929     D fnlg30r         PR
007100050929     D                                     EXTPGM('FNLG30R')
007200050929     D  a
007300050929     D                                     LIKE(kpjba)
007400050929
007500050929     D fnlg31r         PR
007600050929     D                                     EXTPGM('FNLG31R')
007700050929     D  a
007800050929     D                                     LIKE(kpjba)
007900050929
008000050929     D trul06r         PR
008100050929     D                                     EXTPGM('TRUL06R')
008200050929     D  a
008300050929     D                                     LIKE(kpjba)
008400050928
008500050928     ***************************************************************************
008600050928     **?
008700050928     **?Definizione strutture dati.
008800050928     **?
008900050928     ***************************************************************************
009000050929     D psds           SDS
009100050929     D  procName         *PROC
009200050928     D indDspF         DS
009300050928     D  erroreCdCliente...
009400050928     D                         1      1N
009500050928     D  erroreDtRicezioneDal...
009600050928     D                         2      2N
009700050928     D  erroreDtRicezioneAl...
009800050928     D                         3      3N
009900050928     D  erroreSequenzaDate...
010000050928     D                         4      4N
010100050929     D  poProtetto...
010200050929     D                         5      5N
010300050929     D  errorePo...
010400050929     D                         6      6N
010500050929     D  f18attivo...
010600050929     D                        18     18N
010700050928     D  erroreGenerico...
010800050928     D                        99     99N
010900050928     D infDspF         DS
011000050928     D  dsp_aid              369    369
011100050928     D  act_row              382    382
011200050928     D  act_col              383    383
011300050928     D                 DS
011400050928     D  rowInt                 1      2I 0
011500050928     D                                     INZ
011600050928     D  rowChr                 2      2
011700050928     D  colInt                 3      4I 0
011800050928     D                                     INZ
011900050928     D  colChr                 4      4
012000050929     D cnDizion      E DS
012100050929     D                                     BASED(nullPtr)
012200050929     D                                     PREFIX(diz)
012300050929     D kpjba         E DS
012400050929     D fnlg9610s     E DS
012500050929     D                                     INZ
012600050929     D  presaVisio   E
012700050929     D                                     INZ(NonVisionate)
012800050929     D  esitoRicez   E
012900050929     D                                     INZ(Scartate)
013000050929     D  drda         E
013100050929     D                                     INZ(*HIVAL)
013200050929     D dDatiUte      E DS
013300050929     D                                     INZ
013400050929     D azute00f      E DS
013500050929     D                                     INZ
013600050929     D tibs34ds      E DS
013700050929     D                                     INZ
013800050929     D fidna1ds      E DS
013900050929     D                                     INZ
014000050929     D fnlg30ds      E DS
014100050929     D                                     INZ
014200050929     D  d30poa       E
014300050929     D                                     INZ(Partenza)
014400050929     D trul06ds      E DS
014500050929     D                                     INZ
014600050929     D  d06cod       E
014700050929     D                                     INZ('£1')
014800050929     D  lia                           3S 0
014900050929     D                                     DIM(30)
015000050929     D                                     OVERLAY(d06lia)
015100050929     D param3          DS
015200050929     D                                     INZ
015300050929     D  pa3aas                 1      4  0
015400050929     D  pa3lnp                 5      7  0
015500050929     D  pa3nrs                 8      9  0
015600050929     D  pa3nsp                10     16  0
015700050929     D  pa3flg                31     31
015800050929     D                                     INZ('1')
015900050929     D  pa3cli               143    143
016000050929     D  calPgm               201    210
016100050929     D  parAzi               211    211
016200050929     D  parF19               212    212
016300050929     D  parF3                213    213
016400050929     D param0          DS
016500050929     D                                     INZ
016600050929     D  pa0aas                 1      4  0
016700050929     D  pa0lnp                 5      7  0
016800050929     D  pa0nrs                 8      9  0
016900050929     D  pa0nsp                10     16  0
017000050929     D  pa0f03                17     17
017100050929     D  pa0flg                18     18
017200050929     D  pa0a9                 19     19
017300050929     D  pa0B5                 20     20
017400050929     D  pa075                 21     21
017500050929     D  pa098                 22     22
017600050929     D  pa0rsu                23     42
017700050929     D  pa0cml                43     43
017800050929     D  pa0sim                44     46  0
017900050929     D  pa0cli                47     47
018000050929     D  pa0gia                48     48
018100050928
018200050928     ***************************************************************************
018300050928     **?
018400050929     **?Definizione variabili.
018500050928     **?
018600050928     ***************************************************************************
018700050928     D nullPtr         S               *
018800050928     D wrkOpCode       S             10
018900050928     D                                     INZ('INZFILTRO')
019000050928     D parSta          S              1  0
019100050928     D                                     INZ(9)
019200050928     D parDut          S             30
019300050928     D p               S              5I 0
019400050928
019500050928     ***************************************************************************
019600050928     **?
019700050928     **?Definizione chiavi.
019800050928     **?
019900050928     ***************************************************************************
020000050928     C     k03aco00      KLIST
020100050928     C                   KFLD                    acoKut
020200050928     C                   KFLD                    acoKcc
020300050928     C                   KFLD                    acoKsc
020400050928
020500050928     ***************************************************************************
020600050928     **?
020700050928     **?Definizione parametri procedura.
020800050928     **?
020900050928     ***************************************************************************
021000050928     C     *ENTRY        PLIST
021100050928     C                   PARM                    kpjba
021200050928
021300050928     ***************************************************************************
021400050928     **?
021500050928     **?Definizione aree dati.
021600050928     **?
021700050928     ***************************************************************************
021800050928     C     *DTAARA       DEFINE    §azute        azute00f
021900050928     C     *DTAARA       DEFINE    §datiUte      dDatiUte
022000050928
022100050928     ***************************************************************************
022200050928     **?
022300050928     **?Main
022400050928     **?
022500050928     ***************************************************************************
022600050929     C                   EXSR      chkParm
022700050930     C                   EXSR      inzProc
022800050928
022900050928     C                   DO        *HIVAL
023000050928     C                   SELECT
023100050928     C                   WHEN      wrkOpCode = 'INZFILTRO'
023200050928     C                   EXSR      inzFiltro
023300050928     C                   WHEN      wrkOpCode = 'DSPFILTRO'
023400050928     C                   EXSR      dspFiltro
023500050928     C                   WHEN      wrkOpCode = 'RICERCA'
023600050928     C                   EXSR      runRicerca
023700050928     C                   WHEN      wrkOpCode = 'CHKFILTRO'
023800050928     C                   EXSR      chkFiltro
023900050929     C                   WHEN      wrkOpCode = 'SETPARM'
024000050929     C                   EXSR      setParm
024100050929     C                   WHEN      wrkOpCode = 'VISBOLLE'
024200050929     C                   EXSR      visBolle
024300050929     C                   WHEN      wrkOpCode = 'VISGIAC'
024400050929     C                   EXSR      visGiacenze
024500050929     C                   WHEN      wrkOpCode = 'CAMBIOPO'
024600050929     C                   EXSR      cambioPO
024700050928     C                   OTHER
024800050928     C                   LEAVE
024900050928     C                   ENDSL
025000050928     C                   ENDDO
025100050928
025200050928     C                   EXSR      uscita
025300050928
025400050928     ***************************************************************************
025500050928     **?
025600050928     **?Operazioni iniziali.
025700050928     **?
025800050928     ***************************************************************************
025900050928     C     *INZSR        BEGSR
026000050930
026100050930     C                   IN(E)     *DTAARA
026200050930     C                   IF        %ERROR OR rsut = *BLANK
026300050930     C                   CALLP     tibs34r(tibs34ds)
026400050930     C                   IN        *DTAARA
026500050930     C                   ENDIF
026600050928
026700050928     C                   ENDSR
026800050929
026900050929     ***************************************************************************
027000050929     **?
027100050929     **?Ricezione e controllo parametri.
027200050929     **?
027300050929     ***************************************************************************
027400050929     C     chkParm       BEGSR
027500050929
027600050929     C                   IF        kpjbu <> *BLANK
027700050929     C                   EVAL      fnlg9610s = kpjbu
027800050929     C                   ELSE
027900050929     C                   RESET                   fnlg9610s
028000050929     C                   ENDIF
028100050929
028200050929     C                   ENDSR
028300050928
028400050928     ***************************************************************************
028500050928     **?
028600050928     **?Inizializzazione programma.
028700050928     **?
028800050928     ***************************************************************************
028900050930     C     inzProc       BEGSR
029000050928
029100050929     C                   IF        po = 0
029200050929     C                   EVAL      po = dutPou
029300050929     C                   ENDIF
029400050929
029500050929     C                   EVAL      f18attivo = (dutLpo <> Livello2)
029600050929     C                   EVAL      poProtetto = *ON
029700050929
029800050929     **?Reperisco i PO dell' area dell'utente.
029900050929     C                   RESET                   trul06ds
030000050929     C                   EVAL      d06key = %EDITC(dutPou:'X')
030100050929     C                   EVAL      kpjbu = trul06ds
030200050929     C                   CALLP     trul06r(kpjba)
030300050929     C                   EVAL      trul06ds = kpjbu
030400050929
030500050929     C                   RESET                   wrkOpCode
030600050930
030700050930     C                   OPEN(E)   fnlg9610d
030800050930     C                   OPEN(E)   cnaco00f
030900050930     C                   OPEN(E)   azorg01l
031000050928
031100050928     C                   ENDSR
031200050928
031300050928     ***************************************************************************
031400050928     **?
031500050928     **?Inizializzazione finestra di filtro.
031600050928     **?
031700050928     ***************************************************************************
031800050928     C     inzFiltro     BEGSR
031900050928
032000050929     C                   EVAL      dspPO = po
032100050929     C                   IF        ksc = 0
032200050929     C                   CLEAR                   dspKsc
032300050929     C                   ELSE
032400050929     C                   EVAL      dspKsc = %EDITC(ksc:'X')
032500050929     C                   ENDIF
032600050929     C                   IF        drdd = *LOVAL
032700050929     C                   CLEAR                   dspDrdd
032800050929     C                   ELSE
032900050929     C     *JOBRUN       MOVE      drdd          dspDrdd
033000050929     C                   ENDIF
033100050929     C                   IF        drda = *HIVAL
033200050929     C                   EVAL      dspDrda = *HIVAL
033300050929     C                   ELSE
033400050929     C     *JOBRUN       MOVE      drda          dspDrda
033500050929     C                   ENDIF
033600050929     C                   EVAL      dspAgc = agc
033700050929     C                   EVAL      dspFgc = fgc
033800050929     C                   EVAL      dspNgc = ngc
033900050929     C                   EVAL      dspAas = aas
034000050929     C                   EVAL      dspLnp = lnp
034100050929     C                   EVAL      dspNrs = nrs
034200050929     C                   EVAL      dspNsp = nsp
034300050929     C                   EVAL      dspPresaVisio = presaVisio
034400050929     C                   EVAL      dspEsitoRicez = esitoRicez
034500050929     C                   CLEAR                   dspOrgDes
034600050929     C                   CLEAR                   dspAcoRag
034700050928
034800050929     C                   EXSR      chkFiltro
034900050928     C                   EVAL      wrkOpCode = 'DSPFILTRO'
035000050928
035100050928     C                   ENDSR
035200050928
035300050928     ***************************************************************************
035400050928     **?
035500050928     **?Emissione finestra di filtro.
035600050928     **?
035700050928     ***************************************************************************
035800050928     C     dspFiltro     BEGSR
035900050928
036000050928     **?Visualizzo campi di output in presenza di errori.
036100050928     C                   IF        erroreGenerico
036200050928     C                   EVAL      erroreGenerico = *OFF
036300050928     C                   WRITE     w01
036400050928     C                   EVAL      erroreGenerico = *ON
036500050928     C                   ENDIF
036600050928
036700050928     C                   EXFMT     w01
036800050928     C                   EVAL      tasto = dsp_aid
036900050928
037000050928     C                   SELECT
037100050929     C                   WHEN      tasto = F3 OR tasto = F12
037200050928     C                   EVAL      wrkOpCode = 'FINE'
037300050929     C                   WHEN      tasto = F5
037400050929     C                   EVAL      wrkOpCode = 'INZFILTRO'
037500050928     C                   WHEN      tasto = F7
037600050929     C                   EVAL      wrkOpCode = 'VISBOLLE'
037700050928     C                   WHEN      tasto = F14
037800050929     C                   EVAL      wrkOpCode = 'VISGIAC'
037900050928     C                   WHEN      tasto = F18
038000050928     C                   EVAL      wrkOpCode = 'CAMBIOPO'
038100050928     C                   WHEN      tasto = Enter
038200050928     C                   EVAL      wrkOpCode = 'RICERCA'
038300050928     C                   ENDSL
038400050928
038500050928     C                   ENDSR
038600050928
038700050928     ***************************************************************************
038800050928     **?
038900050928     **?Ricerche.
039000050928     **?
039100050928     ***************************************************************************
039200050928     C     runRicerca    BEGSR
039300050928
039400050928     C                   EVAL      wrkOpCode = 'CHKFILTRO'
039500050928
039600050928     **?Ricerca e scelta cliente.
039700050928     C                   EVAL      p = %SCAN('?':dspKsc)
039800050928     C                   IF        p > 0
039900050928     C                   EVAL      parDut = rsut
040000050928     C                   CLEAR                   acoKsc
040100050928     C                   RESET                   parSta
040200050928     C                   IF        p = 1
040300050928     C                   CLEAR                   acoRag
040400050928     C                   ELSE
040500050928     C                   EVAL      acoRag = %SUBST(dspKsc:1:p-1)
040600050928     C                   ENDIF
040700050928     C                   EVAL      acoKut = 1
040800050928     C                   EVAL      acoKcc = dutKci
040900050928     C                   CALLP     xalfa1(parDut:acoKut:acoRag:acoKcc:acoKsc:
041000050929     C                             parSta:d06lia)
041100050928     C                   IF        parSta = 0
041200050928     C                   EVAL      dspKsc = %EDITC(acoKsc:'X')
041300050930     C                   EVAL      dspAcoRag = acoRag
041400050928     C                   ENDIF
041500050928     C                   EVAL      wrkOpCode = 'DSPFILTRO'
041600050928     C                   ENDIF
041700050928
041800050928     C                   ENDSR
041900050928
042000050928     ***************************************************************************
042100050928     **?
042200050928     **?Controllo finestra di filtro.
042300050928     **?
042400050928     ***************************************************************************
042500050928     C     chkFiltro     BEGSR
042600050928
042700050928     C                   EVAL      erroreGenerico = *OFF
042800050929
042900050929     **?Controllo autorizzazione al PO.
043000050929     C     dspPO         LOOKUP    lia                                    98
043100050929     C                   IF        NOT %EQUAL
043200050929     C                   EVAL      errorePO = *ON
043300050929     C                   EVAL      erroreGenerico = *ON
043400050929     C                   ELSE
043500050929     C                   EVAL      poProtetto = *ON
043600050929     C                   ENDIF
043700050929
043800050929     **?Decodifica PO.
043900050929     C     dspPO         CHAIN     azorg01l
044000050929     C                   IF        %FOUND
044100050929     C                   EVAL      dspOrgDes = orgDes
044200050929     C                   ELSE
044300050929     C                   EVAL      errorePO = *ON
044400050929     C                   EVAL      erroreGenerico = *ON
044500050929     C                   ENDIF
044600050928
044700050928     **?Controllo e decodifica cliente.
044800050928     C                   IF        dspKsc <> *BLANK AND dspKsc <> *ZERO
044900050928     C                   EVAL      acoKut = 1
045000050928     C                   EVAL      acoKcc = dutKci
045100050928     C                   EVAL      acoKsc = atoi(dspKsc)
045200050928     C     k03aco00      CHAIN     cnaco00f
045300050928     C                   IF        NOT %FOUND OR acoFlg <> *BLANK
045400050928     C                   EVAL      erroreCdCliente = *ON
045500050928     C                   EVAL      erroreGenerico = *ON
045600050928     C                   ELSE
045700050930     C                   EVAL      dspAcoRag = acoRag
045800050928     C                   ENDIF
045900050928     C                   EVAL      dspKsc = %EDITC(acoKsc:'X')
046000050928     C                   ENDIF
046100050928
046200050929     **?Controllo data ricezione iniziale.
046300050929     C                   RESET                   drdd
046400050929     C                   IF        dspDrdd > 0
046500050929     C     *JOBRUN       TEST(DE)                dspDrdd
046600050929     C                   IF        NOT %ERROR
046700050929     C     *JOBRUN       MOVE      dspDrdd       drdd
046800050929     C                   ELSE
046900050929     C                   EVAL      erroreDtRicezioneDal = *ON
047000050929     C                   EVAL      erroreGenerico = *ON
047100050929     C                   ENDIF
047200050929     C                   ENDIF
047300050929
047400050929     **?Controllo data ricezione finale.
047500050929     C                   RESET                   drda
047600051010     C                   SELECT
047700051010     C                   WHEN      dspDrda = 0
047800051010     C                   EVAL      dspDrda = *HIVAL
047900051010     C                   WHEN      dspDrda < *HIVAL
048000050929     C     *JOBRUN       TEST(DE)                dspDrda
048100050929     C                   IF        NOT %ERROR
048200050929     C     *JOBRUN       MOVE      dspDrda       drda
048300050929     C                   ELSE
048400050929     C                   EVAL      erroreDtRicezioneAl = *ON
048500050929     C                   EVAL      erroreGenerico = *ON
048600050929     C                   ENDIF
048700051010     C                   ENDSL
048800050929
048900050929     **?Controllo sequenza date.
049000050929     C                   IF        drdd > drda
049100050929     C                   EVAL      erroreSequenzaDate = *ON
049200050929     C                   EVAL      erroreGenerico = *ON
049300050929     C                   ENDIF
049400050929
049500050928     C                   IF        erroreGenerico
049600050928     C                   EVAL      wrkOpCode = 'DSPFILTRO'
049700050928     C                   ELSE
049800050929     C                   EVAL      wrkOpCode = 'SETPARM'
049900050928     C                   ENDIF
050000050928
050100050928     C                   ENDSR
050200050929
050300050929     ***************************************************************************
050400050929     **?
050500050929     **?Interrogazione bolle partenza.
050600050929     **?
050700050929     ***************************************************************************
050800050929     C     visBolle      BEGSR
050900050929
051000050929     C                   RESET                   fidna1ds
051100050929
051200050929     C                   IF        dspAas = 0 OR dspLnp = 0 OR dspNsp = 0
051300050929     C                   RESET                   param3
051400050929     C                   EVAL      calPgm = procName
051500050929     C                   EVAL      kpjbu = param3
051600050929     C                   CALLP     fnls04r(kpjba:fidna1ds)
051700050929     C                   EVAL      param3 = kpjbu
051800050929     C                   IF        pa3nsp > 0
051900050929     C                   EVAL      dspAas = pa3aas
052000050929     C                   EVAL      dspLnp = pa3lnp
052100050929     C                   EVAL      dspNrs = pa3nrs
052200050929     C                   EVAL      dspNsp = pa3nsp
052300050929     C                   ENDIF
052400050929     C                   ELSE
052500051005     C                   RESET                   param0
052600050929     C                   EVAL      pa0aas = dspAas
052700050929     C                   EVAL      pa0lnp = dspLnp
052800050929     C                   EVAL      pa0nrs = dspNrs
052900050929     C                   EVAL      pa0nsp = dspNsp
053000050929     C                   EVAL      kpjbu = param0
053100050929     C                   CALLP     fnls05r(kpjba:fidna1ds)
053200050929     C                   EVAL      param0 = kpjbu
053300050929     C                   ENDIF
053400050929
053500050929     C                   EVAL      wrkOpCode = 'DSPFILTRO'
053600050929
053700050929     C                   ENDSR
053800050929
053900050929     ***************************************************************************
054000050929     **?
054100050929     **?Interrogazione giaceze partenza.
054200050929     **?
054300050929     ***************************************************************************
054400050929     C     visGiacenze   BEGSR
054500050929
054600050929     C                   RESET                   fnlg30ds
054700050929     C                   EVAL      d30agc = dspAgc
054800050929     C                   EVAL      d30fgc = dspFgc
054900050929     C                   EVAL      d30ngc = dspNgc
055000050929     C                   EVAL      d30aas = dspAas
055100050929     C                   EVAL      d30lnp = dspLnp
055200050929     C                   EVAL      d30nrs = dspNrs
055300050929     C                   EVAL      d30nsp = dspNsp
055400050929     C                   EVAL      kpjbu = fnlg30ds
055500050929     C                   IF        d30ngc = 0 AND d30nsp = 0
055600050929     C                   CALLP     fnlg30r(kpjba)
055700050929     C                   ELSE
055800050929     C                   CALLP     fnlg31r(kpjba)
055900050929     C                   ENDIF
056000050929     C                   EVAL      fnlg30ds = kpjbu
056100050929
056200050929     C                   EVAL      wrkOpCode = 'DSPFILTRO'
056300050929
056400050929     C                   ENDSR
056500050929
056600050929     ***************************************************************************
056700050929     **?
056800050929     **?Impostazione parametri di output.
056900050929     **?
057000050929     ***************************************************************************
057100050929     C     setParm       BEGSR
057200050929
057300050929     C                   EVAL      po = dspPo
057400050929     C                   EVAL      ksc = atoi(dspKsc)
057500050929     C                   IF        dspDrdd = 0
057600050929     C                   CLEAR                   drdd
057700050929     C                   ELSE
057800050929     C     *JOBRUN       MOVE      dspDrdd       drdd
057900050929     C                   ENDIF
058000050929     C                   IF        dspDrda = *HIVAL
058100050929     C                   EVAL      drda = *HIVAL
058200050929     C                   ELSE
058300050929     C     *JOBRUN       MOVE      dspDrda       drda
058400050929     C                   ENDIF
058500050929     C                   EVAL      agc = dspAgc
058600050929     C                   EVAL      fgc = dspFgc
058700050929     C                   EVAL      ngc = dspNgc
058800050929     C                   EVAL      aas = dspAas
058900050929     C                   EVAL      lnp = dspLnp
059000050929     C                   EVAL      nrs = dspNrs
059100050929     C                   EVAL      nsp = dspNsp
059200050929     C                   EVAL      presaVisio = dspPresaVisio
059300050929     C                   EVAL      esitoRicez = dspEsitoRicez
059400050929
059500050929     C                   EVAL      wrkOpCode = 'FINE'
059600050929
059700050929     C                   ENDSR
059800050929
059900050929     ***************************************************************************
060000050929     **?
060100050929     **?Cambio PO.
060200050929     **?
060300050929     ***************************************************************************
060400050929     C     cambioPO      BEGSR
060500050929
060600050929     C                   EVAL      poProtetto = *OFF
060700050929     C                   EVAL      wrkOpCode = 'DSPFILTRO'
060800050929
060900050929     C                   ENDSR
061000050928
061100050928     ***************************************************************************
061200050928     **?
061300050928     **?Operazioni finali.
061400050928     **?
061500050928     ***************************************************************************
061600050928     C     uscita        BEGSR
061700050928
061800050930     C                   CLOSE     fnlg9610d
061900050930     C                   CLOSE     cnaco00f
062000050930     C                   CLOSE     azorg01l
062100050928     C                   EVAL      kpjbu = fnlg9610s
062200050928     C                   RETURN
062300050928
062400050928     C                   ENDSR
