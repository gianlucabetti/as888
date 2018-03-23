000100060612     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'QC2LE') OPTION(*NODEBUGIO)
000200050601
000300050525     ***********************************************************************************************
000400050525     **?
000500050525     **?Questo programma riceve un riferimento mittente numerico e un codice cliente padre
000600050525     **?e restituisce una lista di spedizioni.
000700050525     **?
000800050525     ***********************************************************************************************
000900050519
001000050519     Ftitas32c  IF   E           K DISK
001100050519     F                                     USROPN
001200050520     Ftitaa30c  IF   E           K DISK
001300050520     F                                     USROPN
001400050525     Ftita430c  IF   E           K DISK
001500050525     F                                     USROPN
001600050520     Ffnlbl01l  IF   E           K DISK
001700050520     F                                     USROPN
001800050520     Fcnaco00f  IF   E           K DISK
001900050520     F                                     USROPN
002000050520     Ftivss02l  IF   E           K DISK
002100050520     F                                     USROPN
002200050519
002300050524     ***********************************************************************************************
002400050524     **?
002500050524     **?Definizione parametri procedura.
002600050524     **?
002700050524     ***********************************************************************************************
002800050519     D tis7711r        PR
002900050524     D  a                          9999
003000050524     D  a                             1
003100050524     D  s                             5S 0
003200050524     D  a                          9999
003300050524     D  s                             5S 0
003400050524     D  a                             1
003500050519
003600050519     D tis7711r        PI
003700050524     D  rqsdta                     9999                                         Dati richiesta
003800050524     D  operazione                    1                                         Operazione
003900050524     D  righeric                      5S 0                                      Righe richieste
004000050524     D  rpydta                     9999                                         Dati risposta
004100050524     D  righeout                      5S 0                                      Righe restituite
004200050524     D  esito                         1                                         Esito
004300050519
004400050524     ***********************************************************************************************
004500050524     **?
004600050524     **?Definizione prototipi procedure chiamate.
004700050524     **?
004800050524     ***********************************************************************************************
004900050519     D tibs10r         PR
005000050519     D                                     EXTPGM('TIBS10R')
005100050519     D  a
005200050519     D                                     LIKE(tibs10ds)
005300050519     D  a                             1
005400050519     D                                     CONST
005500050520
005600050729     D atoll           PR            20I 0
005700050729     D                                     EXTPROC('atoll')
005800050520     D  a                              *
005900050520     D                                     VALUE
006000050520     D                                     OPTIONS(*STRING)
006100050524
006200060612     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
006300060612     D  msgId                         7A   CONST
006400060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
006500060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
006600060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
006700060612     D                                     VARYING
006800060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
006900060612     D                                     VARYING
007000060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
007100060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
007200060612
007300090828     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
007400090828
007500050524     ***********************************************************************************************
007600050524     **?
007700050524     **?Definizione costanti.
007800050524     **?
007900050524     ***********************************************************************************************
008000050524     D NuovaRichiesta  C                   '1'
008100050524     D Continua        C                   '2'
008200050524     D EOF             C                   '9'
008300050524     D Mittente        C                   'M'
008400050524     D CapoContoClienti...
008500050524     D                 C                   151
008600050524     D FranchiVari     C                   '8888'
008700050524     D TrackTrace      C                   'TT'
008800050525     D RifMitAlfabetico...
008900050525     D                 C                   'A'
009000050519
009100050524     ***********************************************************************************************
009200050524     **?
009300050524     **?Definizione strutture dati.
009400050524     **?
009500050524     ***********************************************************************************************
009600050524     D tis172dsi     E DS
009700050519     D                                     PREFIX(rqs)
009800050519     D tis173dso     E DS
009900050519     D                                     PREFIX(rpy)
010000050519     D tibs10ds      E DS
010100050519     D                                     INZ
010200050520     D  d10paf       E
010300050520     D                                     INZ('F')                             Cerca i figli
010400050520     D  skc
010500050520     D                                     LIKE(wrkfiglio)
010600050520     D                                     DIM(500)
010700050520     D                                     OVERLAY(d10skc)
010800050520     D                                     ASCEND
010900090828     D tis7700i0     E DS                  QUALIFIED
011000090828     D                                     INZ(*EXTDFT)
011100090828     D tis7700o0     E DS                  QUALIFIED
011200090828
011300050524     ***********************************************************************************************
011400050524     **?
011500050524     **?Definizione variabili e schiere.
011600050524     **?
011700050524     ***********************************************************************************************
011800050520     D righe           S
011900050520     D                                     LIKE(tis173dso)
012000050520     D                                     DIM(20)
012100050520     D i               S              5I 0
012200050526     D f               S              5I 0
012300050520     D wrkdsp          S              8  0                                      Data spedizione
012400050520     D wrkdspin        S              8  0                                      Data sped.ne iniz.
012500050520     D wrkdspfi        S              8  0                                      Data sped.ne finale
012600050520     D                                     INZ(*HIVAL)
012700050519     D wrkdteur        S               D
012800050519     D                                     DATFMT(*EUR)
012900050526     D wrk1annofa      S               D
013000050520     D wrkbollaok      S              1
013100050520     D                                     INZ(*ON)
013200050524     D wrkfiglio       S             11S 0
013300050524     D wrknksu         S                                                        KSU numerico
013400050524     D                                     LIKE(tasksc)
013500050729     D i20             S             20I 0
013600050527     D tblo            S                                                        Tp bolla da omettere
013700050527     D                                     LIKE(tastbl)
013800050527     D                                     DIM(5)
013900050527     D                                     PERRCD(5)
014000050527     D                                     CTDATA
014100050527     D                                     ASCEND
014200090828     D rpyEsito        S             10I 0
014300060608
014400050524     ***********************************************************************************************
014500050524     **?
014600050524     **?Definizione chiavi data base.
014700050524     **?
014800050524     ***********************************************************************************************
014900050526     C     k02tas32      KLIST
015000050526     C                   KFLD                    tasrmn
015100050526     C                   KFLD                    tasccm
015200050526
015300050520     C     k03aco00      KLIST
015400050520     C                   KFLD                    acokut
015500050520     C                   KFLD                    acokcc
015600050520     C                   KFLD                    tasccm
015700050520
015800050520     C     k05taa30      KLIST
015900050520     C                   KFLD                    tasaas
016000050520     C                   KFLD                    taslnp
016100050520     C                   KFLD                    tasnrs
016200050520     C                   KFLD                    tasnsp
016300050520     C                   KFLD                    taatrc
016400050525
016500050525     C     k05ta430      KLIST
016600050525     C                   KFLD                    tasaas
016700050525     C                   KFLD                    taslnp
016800050525     C                   KFLD                    tasnrs
016900050525     C                   KFLD                    tasnsp
017000050525     C                   KFLD                    ta4trc
017100050520
017200050520     C     k04lbl01      KLIST
017300050520     C                   KFLD                    tasaas
017400050520     C                   KFLD                    taslnp
017500050520     C                   KFLD                    tasnrs
017600050520     C                   KFLD                    tasnsp
017700050520
017800050520     C     k03vss02      KLIST
017900050524     C                   KFLD                    rqsksci72
018000050520     C                   KFLD                    vssisv
018100050520     C                   KFLD                    vsssun
018200050616
018300050615     C     k02vss02      KLIST
018400050615     C                   KFLD                    rqsksci72
018500050615     C                   KFLD                    vssisv
018600050519
018700050519     ***********************************************************************************************
018800050519     **?
018900050519     **?Main.
019000050519     **?
019100050519     ***********************************************************************************************
019200050524     C                   EXSR      chkParm
019300050519
019400050520     C                   IF        operazione = NuovaRichiesta
019500050524     C                   EXSR      inzRichiesta
019600050520     C                   ENDIF
019700050520
019800050520     C                   IF        operazione = Continua
019900050524     C                   EXSR      getBolle
020000050520     C                   ENDIF
020100050520
020200050520     C                   IF        operazione = EOF
020300050524     C                   EXSR      endRichiesta
020400050520     C                   ENDIF
020500050519
020600050520     C                   EXSR      uscita
020700050519
020800050519     ***********************************************************************************************
020900050519     **?
021000050519     **?Controllo parametri ricevuti.
021100050519     **?
021200050519     ***********************************************************************************************
021300050520     C     chkParm       BEGSR
021400050519
021500050520     C                   CLEAR                   rpydta
021600050520     C                   CLEAR                   righeout
021700050519
021800050520     C                   IF        operazione <> NuovaRichiesta AND
021900050520     C                             operazione <> Continua AND
022000050520     C                             operazione <> EOF
022100050520     C                   EVAL      operazione = EOF
022200050520     C                   EVAL      esito = *ON
022300050526     C                   LEAVESR
022400050520     C                   ENDIF
022500050519
022600050520     C                   ENDSR
022700050519
022800050519     ***********************************************************************************************
022900050519     **?
023000050519     **?Nuova richiesta.
023100050519     **?
023200050519     ***********************************************************************************************
023300050520     C     inzRichiesta  BEGSR
023400050519
023500050524     C                   EVAL      tis172dsi  = rqsdta
023600050519
023700050524     C                   IF        rqsrifi72 = *BLANK
023800050520     C                   EVAL      operazione = EOF
023900050520     C                   EVAL      esito = *ON
024000050526     C                   LEAVESR
024100050520     C                   ENDIF
024200050524
024300050526     **?Controllo se con il riferimento mittente numerico ricevuto esiste almeno una bolla.
024400050524     C                   IF        NOT %OPEN(titas32c)
024500050524     C                   OPEN      titas32c
024600050524     C                   ENDIF
024700050524     C
024800050729     C                   EVAL      i20 = atoll(rqsrifi72)
024900050729     C                   Z-ADD     i20           tasrmn
025000050524     C     tasrmn        SETLL     titas32c
025100050524     C
025200050524     C                   IF        NOT %EQUAL
025300050524     C                   EVAL      operazione = EOF
025400050524     C                   LEAVESR
025500050524     C                   ENDIF
025600050520
025700050524     **?Reperisco la famiglia.
025800050729     C                   EVAL      i20 = atoll(rqsksci72)
025900050729     C                   Z-ADD     i20           wrknksu
026000050524     C                   EXSR      getFamiglia
026100050526     C                   IF        operazione = EOF
026200050526     C                   LEAVESR
026300050526     C                   ENDIF
026400050526
026500050526     **?Normalizzazione variabili.
026600050526     C                   IF        righeric <= 0 OR righeric > %ELEM(righe)
026700050526     C                   EVAL      righeric = %ELEM(righe)
026800050526     C                   ENDIF
026900050526
027000050526     **?Data spedizione iniziale.
027100050526     C                   TIME                    wrk1annofa
027200050526     C                   SUBDUR    1:*Y          wrk1annofa
027300050526     C     *ISO          MOVE      wrk1annofa    wrkdspin
027400050526     C     *EUR          TEST(DE)                rqsdini72
027500050526     C                   IF        NOT %ERROR
027600050526     C     *EUR          MOVE      rqsdini72     wrkdteur
027700050526     C                   IF        wrkdteur > wrk1annofa
027800050526     C     *ISO          MOVE      wrkdteur      wrkdspin
027900050526     C                   ENDIF
028000050526     C                   ENDIF
028100050526
028200050526     **?Data spedizione finale.
028300050526     C     *EUR          TEST(DE)                rqsdfii72
028400050526     C                   IF        %ERROR
028500050526     C                   RESET                   wrkdspfi
028600050526     C                   ELSE
028700050526     C     *EUR          MOVE      rqsdfii72     wrkdteur
028800050526     C     *ISO          MOVE      wrkdteur      wrkdspfi
028900050526     C                   ENDIF
029000050526
029100050526     **?Apertura files.
029200050526     C                   IF        NOT %OPEN(cnaco00f)
029300050526     C                   OPEN      cnaco00f
029400050526     C                   ENDIF
029500050526     C                   IF        NOT %OPEN(fnlbl01l)
029600050526     C                   OPEN      fnlbl01l
029700050526     C                   ENDIF
029800050526     C                   IF        NOT %OPEN(titaa30c)
029900050526     C                   OPEN      titaa30c
030000050526     C                   ENDIF
030100050526     C                   IF        NOT %OPEN(tita430c)
030200050526     C                   OPEN      tita430c
030300050526     C                   ENDIF
030400050526
030500050526     **?Tutto bene, si può continuare.
030600050520     C                   EVAL      operazione = Continua
030700050526     C                   RESET                   f
030800050526     C                   CLEAR                   tasccm
030900050519
031000050520     C                   ENDSR
031100050519
031200050519     ***********************************************************************************************
031300050519     **?
031400050519     **?Reperisce bolle.
031500050519     **?
031600050519     ***********************************************************************************************
031700050520     C     getBolle      BEGSR
031800050526
031900050526     C                   RESET                   i
032000050526     C                   RESET                   righe
032100050526
032200050526     **?Se TITAS32C è chiuso significa che la sequenza delle operazioni non è corretta.
032300050526     C                   IF        NOT %OPEN(titas32c)
032400050526     C                   EVAL      operazione = EOF
032500050526     C                   EVAL      esito = *ON
032600050526     C                   LEAVESR
032700050526     C                   ENDIF
032800050526
032900050526     **?Ciclo di scorrimento della schiera SKC codici cliente.
033000050526     C                   DO        *HIVAL
033100050519
033200050526     **?Ciclo di lettura delle bolle.
033300050520     C                   DO        *HIVAL
033400050525     C
033500050526     C     k02tas32      READE     titas32c
033600050525     C
033700050520     C                   IF        %EOF
033800050520     C                   LEAVE
033900050520     C                   ENDIF
034000050525     C
034100050524     C                   EXSR      selBolla
034200050524     C                   EXSR      setRiga
034300050526     **?La pagina è piena.
034400050520     C                   IF        i = righeric
034500050520     C                   LEAVE
034600050520     C                   ENDIF
034700050525     C
034800050520     C                   ENDDO
034900050526
035000050526     **?Posizionamento con codice cliente successivo.
035100050526     C                   IF        i < righeric
035200050526     C                   EXSR      stlBolleCli
035300050526     C                   IF        operazione = EOF
035400050526     C                   LEAVE
035500050526     C                   ENDIF
035600050526     C                   ELSE
035700050526     C                   LEAVE
035800050526     C                   ENDIF
035900050520
036000050526     C                   ENDDO
036100050526
036200050520     C                   MOVEA     righe         rpydta
036300050520     C                   EVAL      righeout = i
036400050526
036500050520     C                   ENDSR
036600050520
036700050520     ***********************************************************************************************
036800050520     **?
036900050520     **?Selezione bolle.
037000050520     **?
037100050520     ***********************************************************************************************
037200050520     C     selBolla      BEGSR
037300050520
037400050520     C                   RESET                   wrkbollaok
037500050520
037600050520     **?Scarto alcuni tipi bolla.
037700050527     C     tastbl        LOOKUP    tblo                               01  01
037800050527     C                   IF        %EQUAL
037900050520     C                   EVAL      wrkbollaok = *OFF
038000050520     C                   LEAVESR
038100050601     C                   ENDIF
038200050520
038300050520     **?Scarto le bolle fuori dal periodo richiesto.
038400050520     C                   EVAL      wrkdsp = (tasaas * 10000) + tasmgs
038500050520     C                   IF        wrkdsp < wrkdspin OR wrkdsp > wrkdspfi
038600050520     C                   EVAL      wrkbollaok = *OFF
038700050520     C                   LEAVESR
038800050520     C                   ENDIF
038900050520
039000050520     **?Scarto le bolle figlie.
039100050520     C     k04lbl01      CHAIN     fnlbl01l
039200050526     C                   IF        %FOUND
039300050520     C                   EVAL      wrkbollaok = *OFF
039400050520     C                   LEAVESR
039500050526     C                   ENDIF
039600050520
039700090828     ** Eseguo test appartenenza bolla a subunificante.
039800090828     C                   RESET                   tis7700i0
039900090828     C                   EVAL      tis7700i0.aas = tasAas
040000090828     C                   EVAL      tis7700i0.lnp = tasLnp
040100090828     C                   EVAL      tis7700i0.nrs = tasNrs
040200090828     C                   EVAL      tis7700i0.nsp = tasNsp
040300090828     C                   EVAL      tis7700i0.tbl = tasTbl
040400090828     C                   EVAL      tis7700i0.ksu = rqsKscI72
040500090828     C                   EVAL      tis7700i0.sun = %EDITC(rqsRqsCidI72 : 'X')
040600090828     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
040700090828     C                             : rpyEsito
040800090828     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
040900090828     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
041000090828     C                             )
041100090828     C                   IF        rpyEsito >= *ZERO AND
041200090828     C                             tis7700o0.bollValida = *OFF
041300090828     C                   EVAL      wrkBollaOk = *OFF
041400090828     C                   LEAVESR
041500090828     C                   ENDIF
041600090828
041700050520     C                   ENDSR
041800050519
041900050519     ***********************************************************************************************
042000050519     **?
042100050520     **?Impostazione riga.
042200050519     **?
042300050519     ***********************************************************************************************
042400050520     C     setRiga       BEGSR
042500050519
042600050520     C                   IF        wrkbollaok = *ON
042700050520
042800050526     **?Riferimenti mittente.
042900050526     C                   EVAL      rpyrifo73 = %TRIML(%EDITC(tasrmn:'Z'))
043000050525     C     k05ta430      CHAIN     tita430c
043100050526     C                   IF        %FOUND
043200050525     C                   EVAL      rpyrifo73 = %TRIMR(rpyrifo73) + ' ' +
043300050525     C                             %TRIML(ta4not)
043400050526     C                   ENDIF
043500050525
043600050520     C     *ISO          MOVE      wrkdsp        wrkdteur
043700050520     C                   MOVE      wrkdteur      rpydato73                      Data spedizione
043800050520     C                   EVAL      rpydeso73 = %TRIMR(taslod) + ' ' + tasprd    Destinazione
043900050520     C                   EVAL      rpydsto73 = tasrsd                           Destinatario
044000050520     C                   EVAL      rpyspeo73 = %EDITC(taslnp:'X') +             Id spedizione
044100050520     C                             %EDITC(tasnrs:'X') + %EDITC(tasnsp:'X')
044200050520
044300050520     C                   IF        tasdcm > 0
044400060608     C*                  EVAL      rpystao73 = 'Consegnata'
044500060612     C                   EVAL      rpystao73 = rtvMsgLang('TIS0091':rqsLangI72)
044600060608     C                   ELSE
044700060608     C*                  EVAL      rpystao73 = 'Da consegnare'
044800060612     C                   EVAL      rpystao73 = rtvMsgLang('TIS0103':rqsLangI72)
044900050520     C                   ENDIF
045000050520
045100050520     C                   EXSR      setMittente
045200050520
045300050520     C                   EVAL      i = i + 1
045400050520     C                   EVAL      righe(i) = tis173dso
045500050520
045600050520     C                   ENDIF
045700050519
045800050520     C                   ENDSR
045900050520
046000050520     ***********************************************************************************************
046100050520     **?
046200050520     **?Ragione sociale del cliente mittente
046300050520     **?
046400050520     ***********************************************************************************************
046500050520     C     setMittente   BEGSR
046600050520
046700050526     C                   IF        %SUBST(%EDITC(tasccm:'X'):4:4) <> FranchiVari
046800050520     C     k03aco00      CHAIN     cnaco00f
046900050526     C                   IF        %FOUND
047000050520     C                   EVAL      rpymito73 = acorag
047100050526     C                   ENDIF
047200050526     C                   ELSE
047300050520     C     k05taa30      CHAIN     titaa30c
047400050526     C                   IF        %FOUND
047500050520     C                   EVAL      rpymito73 = taarsc
047600050526     C                   ENDIF
047700050526     C                   ENDIF
047800050520
047900050520     C                   ENDSR
048000050519
048100050519     ***********************************************************************************************
048200050519     **?
048300050519     **?Fine richiesta.
048400050519     **?
048500050519     ***********************************************************************************************
048600050520     C     endRichiesta  BEGSR
048700050519
048800050520     C                   IF        %OPEN(titas32c)
048900050520     C                   CLOSE     titas32c
049000050520     C                   ENDIF
049100050520     C                   IF        %OPEN(titaa30c)
049200050520     C                   CLOSE     titaa30c
049300050520     C                   ENDIF
049400050525     C                   IF        %OPEN(tita430c)
049500050525     C                   CLOSE     tita430c
049600050525     C                   ENDIF
049700050520     C                   IF        %OPEN(fnlbl01l)
049800050520     C                   CLOSE     fnlbl01l
049900050520     C                   ENDIF
050000050520     C                   IF        %OPEN(cnaco00f)
050100050520     C                   CLOSE     cnaco00f
050200050520     C                   ENDIF
050300050519
050400050520     C                   ENDSR
050500050526
050600050526     ***********************************************************************************************
050700050526     **?
050800050526     **?Posizionamento bolle codice cliente.
050900050526     **?
051000050526     ***********************************************************************************************
051100050526     C     stlBolleCli   BEGSR
051200050526
051300050526     C                   DO        *HIVAL
051400050526     C                   EVAL      f = f + 1
051500050526     C                   IF        f > %ELEM(skc) OR skc(f) = 0
051600050526     C                   EVAL      operazione = EOF
051700050526     C                   LEAVE
051800050526     C                   ELSE
051900050526     C                   EVAL      tasccm = skc(f)
052000050526     C     k02tas32      SETLL     titas32c
052100050526     C                   IF        %EQUAL
052200050526     C                   LEAVE
052300050526     C                   ENDIF
052400050526     C                   ENDIF
052500050526     C                   ENDDO
052600050526
052700050526     C                   ENDSR
052800050520
052900050520     ***********************************************************************************************
053000050520     **?
053100050520     **?Reperisco la famiglia.
053200050520     **?
053300050520     ***********************************************************************************************
053400050520     C     getFamiglia   BEGSR
053500050526
053600050526     **?Reperisco abilitazione al servizio.
053700050520     C                   EXSR      getAbiServizi
053800050526     C                   IF        operazione = EOF
053900050526     C                   LEAVESR
054000050526     C                   ENDIF
054100050520
054200050520     **?Cerco la famiglia solo se esiste il tipo legame.
054300050526     C                   RESET                   tibs10ds
054400050520     C                   IF        vsstle <> *BLANK
054500050520     C                   EVAL      d10tle = vsstle
054600050524     C                   EVAL      d10cod = wrknksu
054700050520     C                   CALLP     tibs10r(tibs10ds:' ')
054800050526     C                   IF        d10err <> *BLANK
054900050526     C                   EVAL      operazione = EOF
055000050526     C                   EVAL      esito = *ON
055100050520     C                   ENDIF
055200050520     C                   ELSE
055300050524     C                   EVAL      skc(1) = wrknksu
055400050520     C                   ENDIF
055500050520
055600050520     C                   ENDSR
055700050520
055800050520     ***********************************************************************************************
055900050520     **?
056000050520     **?Reperisco abilitazione al servizio.
056100050520     **?
056200050520     ***********************************************************************************************
056300050520     C     getAbiServizi BEGSR
056400050520
056500050520     C                   IF        NOT %OPEN(tivss02l)
056600050520     C                   OPEN      tivss02l
056700050520     C                   ENDIF
056800050520
056900050615     C                   IF        rqsrqscidi72 <> 0
057000050524     C                   EVAL      vsssun = %EDITC(rqsrqscidi72:'X')
057100050520     C     k03vss02      CHAIN     tivss02l
057200050615     C                   ELSE
057300050615     C     k02vss02      CHAIN     tivss02l
057400050615     C                   ENDIF
057500050615
057600050520     C                   CLOSE     tivss02l
057700050520
057800050520     C                   IF        NOT %FOUND
057900050520     C                   EVAL      operazione = EOF
058000050520     C                   EVAL      esito = *ON
058100050520     C                   ENDIF
058200050520
058300050520     C                   ENDSR
058400060608
058500050520     ***********************************************************************************************
058600050520     **?
058700050524     **?Operazioni iniziali.
058800050520     **?
058900050520     ***********************************************************************************************
059000050520     C     *INZSR        BEGSR
059100050520
059200050520     C                   EVAL      acokut = 1
059300050520     C                   EVAL      acokcc = CapoContoClienti
059400050520     C                   EVAL      taatrc = Mittente
059500050525     C                   EVAL      ta4trc = RifMitAlfabetico
059600050520     C                   EVAL      vssisv = TrackTrace
059700050520
059800050520     C                   ENDSR
059900050519
060000050519     ***********************************************************************************************
060100050519     **?
060200050519     **?Uscita.
060300050519     **?
060400050519     ***********************************************************************************************
060500050520     C     uscita        BEGSR
060600050519
060700050520     C                   IF        esito = *BLANK
060800050520     C                   EVAL      esito = *OFF
060900050520     C                   ENDIF
061000050519
061100050520     C                   RETURN
061200050519
061300050520     C                   ENDSR
061400050527
061500050527**CTDATA tblo
061600050527APARA3FCF3
