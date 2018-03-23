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
006501060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
006800060612     D                                     VARYING
006900060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
007000060612     D                                     VARYING
007100060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
007200060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
007300060612
007400050524     ***********************************************************************************************
007500050524     **?
007600050524     **?Definizione costanti.
007700050524     **?
007800050524     ***********************************************************************************************
007900050524     D NuovaRichiesta  C                   '1'
008000050524     D Continua        C                   '2'
008100050524     D EOF             C                   '9'
008200050524     D Mittente        C                   'M'
008300050524     D CapoContoClienti...
008400050524     D                 C                   151
008500050524     D FranchiVari     C                   '8888'
008600050524     D TrackTrace      C                   'TT'
008700050525     D RifMitAlfabetico...
008800050525     D                 C                   'A'
008900050519
009000050524     ***********************************************************************************************
009100050524     **?
009200050524     **?Definizione strutture dati.
009300050524     **?
009400050524     ***********************************************************************************************
009500050524     D tis172dsi     E DS
009600050519     D                                     PREFIX(rqs)
009700050519     D tis173dso     E DS
009800050519     D                                     PREFIX(rpy)
009900050519     D tibs10ds      E DS
010000050519     D                                     INZ
010100050520     D  d10paf       E
010200050520     D                                     INZ('F')                             Cerca i figli
010300050520     D  skc
010400050520     D                                     LIKE(wrkfiglio)
010500050520     D                                     DIM(500)
010600050520     D                                     OVERLAY(d10skc)
010700050520     D                                     ASCEND
010800050519
010900050524     ***********************************************************************************************
011000050524     **?
011100050524     **?Definizione variabili e schiere.
011200050524     **?
011300050524     ***********************************************************************************************
011400050520     D righe           S
011500050520     D                                     LIKE(tis173dso)
011600050520     D                                     DIM(20)
011700050520     D i               S              5I 0
011800050526     D f               S              5I 0
011900050520     D wrkdsp          S              8  0                                      Data spedizione
012000050520     D wrkdspin        S              8  0                                      Data sped.ne iniz.
012100050520     D wrkdspfi        S              8  0                                      Data sped.ne finale
012200050520     D                                     INZ(*HIVAL)
012300050519     D wrkdteur        S               D
012400050519     D                                     DATFMT(*EUR)
012500050526     D wrk1annofa      S               D
012600050520     D wrkbollaok      S              1
012700050520     D                                     INZ(*ON)
012800050524     D wrkfiglio       S             11S 0
012900050524     D wrknksu         S                                                        KSU numerico
013000050524     D                                     LIKE(tasksc)
013100050729     D i20             S             20I 0
013200050527     D tblo            S                                                        Tp bolla da omettere
013300050527     D                                     LIKE(tastbl)
013400050527     D                                     DIM(5)
013500050527     D                                     PERRCD(5)
013600050527     D                                     CTDATA
013700050527     D                                     ASCEND
013800060608
013900050524     ***********************************************************************************************
014000050524     **?
014100050524     **?Definizione chiavi data base.
014200050524     **?
014300050524     ***********************************************************************************************
014400050526     C     k02tas32      KLIST
014500050526     C                   KFLD                    tasrmn
014600050526     C                   KFLD                    tasccm
014700050526
014800050520     C     k03aco00      KLIST
014900050520     C                   KFLD                    acokut
015000050520     C                   KFLD                    acokcc
015100050520     C                   KFLD                    tasccm
015200050520
015300050520     C     k05taa30      KLIST
015400050520     C                   KFLD                    tasaas
015500050520     C                   KFLD                    taslnp
015600050520     C                   KFLD                    tasnrs
015700050520     C                   KFLD                    tasnsp
015800050520     C                   KFLD                    taatrc
015900050525
016000050525     C     k05ta430      KLIST
016100050525     C                   KFLD                    tasaas
016200050525     C                   KFLD                    taslnp
016300050525     C                   KFLD                    tasnrs
016400050525     C                   KFLD                    tasnsp
016500050525     C                   KFLD                    ta4trc
016600050520
016700050520     C     k04lbl01      KLIST
016800050520     C                   KFLD                    tasaas
016900050520     C                   KFLD                    taslnp
017000050520     C                   KFLD                    tasnrs
017100050520     C                   KFLD                    tasnsp
017200050520
017300050520     C     k03vss02      KLIST
017400050524     C                   KFLD                    rqsksci72
017500050520     C                   KFLD                    vssisv
017600050520     C                   KFLD                    vsssun
017700050616
017800050615     C     k02vss02      KLIST
017900050615     C                   KFLD                    rqsksci72
018000050615     C                   KFLD                    vssisv
018100050519
018200050519     ***********************************************************************************************
018300050519     **?
018400050519     **?Main.
018500050519     **?
018600050519     ***********************************************************************************************
018700050524     C                   EXSR      chkParm
018800050519
018900050520     C                   IF        operazione = NuovaRichiesta
019000050524     C                   EXSR      inzRichiesta
019100050520     C                   ENDIF
019200050520
019300050520     C                   IF        operazione = Continua
019400050524     C                   EXSR      getBolle
019500050520     C                   ENDIF
019600050520
019700050520     C                   IF        operazione = EOF
019800050524     C                   EXSR      endRichiesta
019900050520     C                   ENDIF
020000050519
020100050520     C                   EXSR      uscita
020200050519
020300050519     ***********************************************************************************************
020400050519     **?
020500050519     **?Controllo parametri ricevuti.
020600050519     **?
020700050519     ***********************************************************************************************
020800050520     C     chkParm       BEGSR
020900050519
021000050520     C                   CLEAR                   rpydta
021100050520     C                   CLEAR                   righeout
021200050519
021300050520     C                   IF        operazione <> NuovaRichiesta AND
021400050520     C                             operazione <> Continua AND
021500050520     C                             operazione <> EOF
021600050520     C                   EVAL      operazione = EOF
021700050520     C                   EVAL      esito = *ON
021800050526     C                   LEAVESR
021900050520     C                   ENDIF
022000050519
022100050520     C                   ENDSR
022200050519
022300050519     ***********************************************************************************************
022400050519     **?
022500050519     **?Nuova richiesta.
022600050519     **?
022700050519     ***********************************************************************************************
022800050520     C     inzRichiesta  BEGSR
022900050519
023000050524     C                   EVAL      tis172dsi  = rqsdta
023100050519
023200050524     C                   IF        rqsrifi72 = *BLANK
023300050520     C                   EVAL      operazione = EOF
023400050520     C                   EVAL      esito = *ON
023500050526     C                   LEAVESR
023600050520     C                   ENDIF
023700050524
023800050526     **?Controllo se con il riferimento mittente numerico ricevuto esiste almeno una bolla.
023900050524     C                   IF        NOT %OPEN(titas32c)
024000050524     C                   OPEN      titas32c
024100050524     C                   ENDIF
024200050524     C
024300050729     C                   EVAL      i20 = atoll(rqsrifi72)
024400050729     C                   Z-ADD     i20           tasrmn
024500050524     C     tasrmn        SETLL     titas32c
024600050524     C
024700050524     C                   IF        NOT %EQUAL
024800050524     C                   EVAL      operazione = EOF
024900050524     C                   LEAVESR
025000050524     C                   ENDIF
025100050520
025200050524     **?Reperisco la famiglia.
025300050729     C                   EVAL      i20 = atoll(rqsksci72)
025400050729     C                   Z-ADD     i20           wrknksu
025500050524     C                   EXSR      getFamiglia
025600050526     C                   IF        operazione = EOF
025700050526     C                   LEAVESR
025800050526     C                   ENDIF
025900050526
026000050526     **?Normalizzazione variabili.
026100050526     C                   IF        righeric <= 0 OR righeric > %ELEM(righe)
026200050526     C                   EVAL      righeric = %ELEM(righe)
026300050526     C                   ENDIF
026400050526
026500050526     **?Data spedizione iniziale.
026600050526     C                   TIME                    wrk1annofa
026700050526     C                   SUBDUR    1:*Y          wrk1annofa
026800050526     C     *ISO          MOVE      wrk1annofa    wrkdspin
026900050526     C     *EUR          TEST(DE)                rqsdini72
027000050526     C                   IF        NOT %ERROR
027100050526     C     *EUR          MOVE      rqsdini72     wrkdteur
027200050526     C                   IF        wrkdteur > wrk1annofa
027300050526     C     *ISO          MOVE      wrkdteur      wrkdspin
027400050526     C                   ENDIF
027500050526     C                   ENDIF
027600050526
027700050526     **?Data spedizione finale.
027800050526     C     *EUR          TEST(DE)                rqsdfii72
027900050526     C                   IF        %ERROR
028000050526     C                   RESET                   wrkdspfi
028100050526     C                   ELSE
028200050526     C     *EUR          MOVE      rqsdfii72     wrkdteur
028300050526     C     *ISO          MOVE      wrkdteur      wrkdspfi
028400050526     C                   ENDIF
028500050526
028600050526     **?Apertura files.
028700050526     C                   IF        NOT %OPEN(cnaco00f)
028800050526     C                   OPEN      cnaco00f
028900050526     C                   ENDIF
029000050526     C                   IF        NOT %OPEN(fnlbl01l)
029100050526     C                   OPEN      fnlbl01l
029200050526     C                   ENDIF
029300050526     C                   IF        NOT %OPEN(titaa30c)
029400050526     C                   OPEN      titaa30c
029500050526     C                   ENDIF
029600050526     C                   IF        NOT %OPEN(tita430c)
029700050526     C                   OPEN      tita430c
029800050526     C                   ENDIF
029900050526
030000050526     **?Tutto bene, si può continuare.
030100050520     C                   EVAL      operazione = Continua
030200050526     C                   RESET                   f
030300050526     C                   CLEAR                   tasccm
030400050519
030500050520     C                   ENDSR
030600050519
030700050519     ***********************************************************************************************
030800050519     **?
030900050519     **?Reperisce bolle.
031000050519     **?
031100050519     ***********************************************************************************************
031200050520     C     getBolle      BEGSR
031300050526
031400050526     C                   RESET                   i
031500050526     C                   RESET                   righe
031600050526
031700050526     **?Se TITAS32C è chiuso significa che la sequenza delle operazioni non è corretta.
031800050526     C                   IF        NOT %OPEN(titas32c)
031900050526     C                   EVAL      operazione = EOF
032000050526     C                   EVAL      esito = *ON
032100050526     C                   LEAVESR
032200050526     C                   ENDIF
032300050526
032400050526     **?Ciclo di scorrimento della schiera SKC codici cliente.
032500050526     C                   DO        *HIVAL
032600050519
032700050526     **?Ciclo di lettura delle bolle.
032800050520     C                   DO        *HIVAL
032900050525     C
033000050526     C     k02tas32      READE     titas32c
033100050525     C
033200050520     C                   IF        %EOF
033300050520     C                   LEAVE
033400050520     C                   ENDIF
033500050525     C
033600050524     C                   EXSR      selBolla
033700050524     C                   EXSR      setRiga
033800050526     **?La pagina è piena.
033900050520     C                   IF        i = righeric
034000050520     C                   LEAVE
034100050520     C                   ENDIF
034200050525     C
034300050520     C                   ENDDO
034400050526
034500050526     **?Posizionamento con codice cliente successivo.
034600050526     C                   IF        i < righeric
034700050526     C                   EXSR      stlBolleCli
034800050526     C                   IF        operazione = EOF
034900050526     C                   LEAVE
035000050526     C                   ENDIF
035100050526     C                   ELSE
035200050526     C                   LEAVE
035300050526     C                   ENDIF
035400050520
035500050526     C                   ENDDO
035600050526
035700050520     C                   MOVEA     righe         rpydta
035800050520     C                   EVAL      righeout = i
035900050526
036000050520     C                   ENDSR
036100050520
036200050520     ***********************************************************************************************
036300050520     **?
036400050520     **?Selezione bolle.
036500050520     **?
036600050520     ***********************************************************************************************
036700050520     C     selBolla      BEGSR
036800050520
036900050520     C                   RESET                   wrkbollaok
037000050520
037100050520     **?Scarto alcuni tipi bolla.
037200050527     C     tastbl        LOOKUP    tblo                               01  01
037300050527     C                   IF        %EQUAL
037400050520     C                   EVAL      wrkbollaok = *OFF
037500050520     C                   LEAVESR
037600050601     C                   ENDIF
037700050520
037800050520     **?Scarto le bolle fuori dal periodo richiesto.
037900050520     C                   EVAL      wrkdsp = (tasaas * 10000) + tasmgs
038000050520     C                   IF        wrkdsp < wrkdspin OR wrkdsp > wrkdspfi
038100050520     C                   EVAL      wrkbollaok = *OFF
038200050520     C                   LEAVESR
038300050520     C                   ENDIF
038400050520
038500050520     **?Scarto le bolle figlie.
038600050520     C     k04lbl01      CHAIN     fnlbl01l
038700050526     C                   IF        %FOUND
038800050520     C                   EVAL      wrkbollaok = *OFF
038900050520     C                   LEAVESR
039000050526     C                   ENDIF
039100050520
039200050520     C                   ENDSR
039300050519
039400050519     ***********************************************************************************************
039500050519     **?
039600050520     **?Impostazione riga.
039700050519     **?
039800050519     ***********************************************************************************************
039900050520     C     setRiga       BEGSR
040000050519
040100050520     C                   IF        wrkbollaok = *ON
040200050520
040300050526     **?Riferimenti mittente.
040400050526     C                   EVAL      rpyrifo73 = %TRIML(%EDITC(tasrmn:'Z'))
040500050525     C     k05ta430      CHAIN     tita430c
040600050526     C                   IF        %FOUND
040700050525     C                   EVAL      rpyrifo73 = %TRIMR(rpyrifo73) + ' ' +
040800050525     C                             %TRIML(ta4not)
040900050526     C                   ENDIF
041000050525
041100050520     C     *ISO          MOVE      wrkdsp        wrkdteur
041200050520     C                   MOVE      wrkdteur      rpydato73                      Data spedizione
041300050520     C                   EVAL      rpydeso73 = %TRIMR(taslod) + ' ' + tasprd    Destinazione
041400050520     C                   EVAL      rpydsto73 = tasrsd                           Destinatario
041500050520     C                   EVAL      rpyspeo73 = %EDITC(taslnp:'X') +             Id spedizione
041600050520     C                             %EDITC(tasnrs:'X') + %EDITC(tasnsp:'X')
041700050520
041800050520     C                   IF        tasdcm > 0
041900060608     C*                  EVAL      rpystao73 = 'Consegnata'
042000060612     C                   EVAL      rpystao73 = rtvMsgLang('TIS0091':rqsLangI72)
042100060608     C                   ELSE
042200060608     C*                  EVAL      rpystao73 = 'Da consegnare'
042300060612     C                   EVAL      rpystao73 = rtvMsgLang('TIS0103':rqsLangI72)
042400050520     C                   ENDIF
042500050520
042600050520     C                   EXSR      setMittente
042700050520
042800050520     C                   EVAL      i = i + 1
042900050520     C                   EVAL      righe(i) = tis173dso
043000050520
043100050520     C                   ENDIF
043200050519
043300050520     C                   ENDSR
043400050520
043500050520     ***********************************************************************************************
043600050520     **?
043700050520     **?Ragione sociale del cliente mittente
043800050520     **?
043900050520     ***********************************************************************************************
044000050520     C     setMittente   BEGSR
044100050520
044200050526     C                   IF        %SUBST(%EDITC(tasccm:'X'):4:4) <> FranchiVari
044300050520     C     k03aco00      CHAIN     cnaco00f
044400050526     C                   IF        %FOUND
044500050520     C                   EVAL      rpymito73 = acorag
044600050526     C                   ENDIF
044700050526     C                   ELSE
044800050520     C     k05taa30      CHAIN     titaa30c
044900050526     C                   IF        %FOUND
045000050520     C                   EVAL      rpymito73 = taarsc
045100050526     C                   ENDIF
045200050526     C                   ENDIF
045300050520
045400050520     C                   ENDSR
045500050519
045600050519     ***********************************************************************************************
045700050519     **?
045800050519     **?Fine richiesta.
045900050519     **?
046000050519     ***********************************************************************************************
046100050520     C     endRichiesta  BEGSR
046200050519
046300050520     C                   IF        %OPEN(titas32c)
046400050520     C                   CLOSE     titas32c
046500050520     C                   ENDIF
046600050520     C                   IF        %OPEN(titaa30c)
046700050520     C                   CLOSE     titaa30c
046800050520     C                   ENDIF
046900050525     C                   IF        %OPEN(tita430c)
047000050525     C                   CLOSE     tita430c
047100050525     C                   ENDIF
047200050520     C                   IF        %OPEN(fnlbl01l)
047300050520     C                   CLOSE     fnlbl01l
047400050520     C                   ENDIF
047500050520     C                   IF        %OPEN(cnaco00f)
047600050520     C                   CLOSE     cnaco00f
047700050520     C                   ENDIF
047800050519
047900050520     C                   ENDSR
048000050526
048100050526     ***********************************************************************************************
048200050526     **?
048300050526     **?Posizionamento bolle codice cliente.
048400050526     **?
048500050526     ***********************************************************************************************
048600050526     C     stlBolleCli   BEGSR
048700050526
048800050526     C                   DO        *HIVAL
048900050526     C                   EVAL      f = f + 1
049000050526     C                   IF        f > %ELEM(skc) OR skc(f) = 0
049100050526     C                   EVAL      operazione = EOF
049200050526     C                   LEAVE
049300050526     C                   ELSE
049400050526     C                   EVAL      tasccm = skc(f)
049500050526     C     k02tas32      SETLL     titas32c
049600050526     C                   IF        %EQUAL
049700050526     C                   LEAVE
049800050526     C                   ENDIF
049900050526     C                   ENDIF
050000050526     C                   ENDDO
050100050526
050200050526     C                   ENDSR
050300050520
050400050520     ***********************************************************************************************
050500050520     **?
050600050520     **?Reperisco la famiglia.
050700050520     **?
050800050520     ***********************************************************************************************
050900050520     C     getFamiglia   BEGSR
051000050526
051100050526     **?Reperisco abilitazione al servizio.
051200050520     C                   EXSR      getAbiServizi
051300050526     C                   IF        operazione = EOF
051400050526     C                   LEAVESR
051500050526     C                   ENDIF
051600050520
051700050520     **?Cerco la famiglia solo se esiste il tipo legame.
051800050526     C                   RESET                   tibs10ds
051900050520     C                   IF        vsstle <> *BLANK
052000050520     C                   EVAL      d10tle = vsstle
052100050524     C                   EVAL      d10cod = wrknksu
052200050520     C                   CALLP     tibs10r(tibs10ds:' ')
052300050526     C                   IF        d10err <> *BLANK
052400050526     C                   EVAL      operazione = EOF
052500050526     C                   EVAL      esito = *ON
052600050520     C                   ENDIF
052700050520     C                   ELSE
052800050524     C                   EVAL      skc(1) = wrknksu
052900050520     C                   ENDIF
053000050520
053100050520     C                   ENDSR
053200050520
053300050520     ***********************************************************************************************
053400050520     **?
053500050520     **?Reperisco abilitazione al servizio.
053600050520     **?
053700050520     ***********************************************************************************************
053800050520     C     getAbiServizi BEGSR
053900050520
054000050520     C                   IF        NOT %OPEN(tivss02l)
054100050520     C                   OPEN      tivss02l
054200050520     C                   ENDIF
054300050520
054400050615     C                   IF        rqsrqscidi72 <> 0
054500050524     C                   EVAL      vsssun = %EDITC(rqsrqscidi72:'X')
054600050520     C     k03vss02      CHAIN     tivss02l
054700050615     C                   ELSE
054800050615     C     k02vss02      CHAIN     tivss02l
054900050615     C                   ENDIF
055000050615
055100050520     C                   CLOSE     tivss02l
055200050520
055300050520     C                   IF        NOT %FOUND
055400050520     C                   EVAL      operazione = EOF
055500050520     C                   EVAL      esito = *ON
055600050520     C                   ENDIF
055700050520
055800050520     C                   ENDSR
055900060608
056000050520     ***********************************************************************************************
056100050520     **?
056200050524     **?Operazioni iniziali.
056300050520     **?
056400050520     ***********************************************************************************************
056500050520     C     *INZSR        BEGSR
056600050520
056700050520     C                   EVAL      acokut = 1
056800050520     C                   EVAL      acokcc = CapoContoClienti
056900050520     C                   EVAL      taatrc = Mittente
057000050525     C                   EVAL      ta4trc = RifMitAlfabetico
057100050520     C                   EVAL      vssisv = TrackTrace
057200050520
057300050520     C                   ENDSR
057400050519
057500050519     ***********************************************************************************************
057600050519     **?
057700050519     **?Uscita.
057800050519     **?
057900050519     ***********************************************************************************************
058000050520     C     uscita        BEGSR
058100050519
058200050520     C                   IF        esito = *BLANK
058300050520     C                   EVAL      esito = *OFF
058400050520     C                   ENDIF
058500050519
058600050520     C                   RETURN
058700050519
058800050520     C                   ENDSR
058900050527
059000050527**CTDATA tblo
059100050527APARA3FCF3
