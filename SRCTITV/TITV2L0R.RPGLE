000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200130531     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000300000313     F*
000400040723     Ffnacr01l  if   e           k disk
000500090618     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700040708     FFnvaowwr  O    E             DISK    usropn
000800040723     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000000313     D*
001100040709     D*------------
001200040709     D* SCHIERE A PROGRAMMA
001300040709     D*------------
001400040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001500010330     D*----------------------------------------------------
001600010330     D* DICHIARAZIOINE VARIABILI DI WRK
001700010330     D*----------------------------------------------------
001800010330     D dscmz         e ds                  inz
001900010330     D psds           sds
002000010330     D  procname         *PROC
002100010330     D tivlrds       e ds                  extname(tivlr00f)
002200050826     D fnvapds       e ds                  extname(fnvap00f)
002300040723     D dorm01        e ds
002400040723     D dfar          e ds
002500090618     d trul33ds      e ds
002600090618     d kpjba         e ds
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D rrnum           s              6  0 INZ(*zeros)
003200040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003300060306     D Cli_Abil        s              1    inz(*off)
003400060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003500090618     d §numori         s              7  0 inz
003600030715     D*------------------
003700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003800030715     D*------------------
003900030715     D SkSplitFLD      S             10    DIM(100)
004000030715     D SkSplitCSV      S            256    DIM(100)
004100030715     D CharCSV         S              1
004200030715     D CharTXT         S              1
004300030715     D CharNUM         S              1
004400030715     D posDa           S              3  0 INZ(*zeros)
004500030715     D posA            S              3  0 INZ(*zeros)
004600030715     D i               s              3  0 INZ(1)
004700030715     D wGiro           s              1  0 INZ(*zeros)
004800030822     D*------------------
004900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005000030822     D*------------------
005100030822     D posDaDft        S              3  0 INZ(*zeros)
005200030822     D posADft         S              3  0 INZ(*zeros)
005300030822     D j               s              3  0 INZ(1)
005400030822     D wGiroDft        s              1  0 INZ(*zeros)
005500040723     D*------------------
005600040723     D* DS REPERIMENTO DATI CLIENTE
005700040723     D*-------------------
005800040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
005900040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006000040723     D INDDS         E DS                  EXTNAME(CNIND00F)
006100040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006200040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006300080702     D*------------------
006400080702
006500080702     D*------------------
006600080702     D* LINKING A DEFINIZIONI ESTERNE
006700080702     D*------------------
006800090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006900090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000091026
007100091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007200091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
007300091026
007400080702
007500031201     D*------------------
007600031201     D* Costanti
007700031201     D*------------------
007800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000010330
008100010330
008200010330
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600010601     C*
008700010601     C                   exsr      opeini
008800041011     C                   exsr      PREELA
008900040708     C                   exsr      rwvao
009000010601     C*
009100010601     C                   seton                                        lr
009200010601
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200041011     C                   if        flgGiro = '0'
010300010601     C*
010400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010500010601     C                   eval      flgGiro = '1'
010600041011     C*
010700041011     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700000616     C*
011800010601     C                   ENDSR
011900010601     C***
012000010601
012100010601
012200010330     C*--------------------------------------------------------
012300040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
012400010330     C*--------------------------------------------------------
012500040708     C     RWVAO         BEGSR
012600010330     C*
012700010330     C                   if        not %open(tivin00r)
012800010330     C                   open      tivin00r
012900010330     C                   endif
013000040708     C                   if        not %open(fnvaowwr)
013100040708     C                   open      fnvaowwr
013200010330     C                   endif
013300010330     C*
013400010604     C                   clear                   §CTROK
013500010604     C                   clear                   §CTRMO
013600010604     C                   clear                   §CTRNO
013700010330     C*
013800030822     C                   do        *HIVAL
013900010330     C*
014000030822     C                   read      tivin00r                               70
014100010618     C*
014200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014300010618     C                   if        vinflg = '1'
014400010618     C                   eval      flgOk = '1'
014500010618     C                   endif
014600010618     C*
014700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014800040510     C                   z-add     *zeros        wLenVINDTA        4 0
014900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015300040510     C                   z-add     1             wFlgVINDTA
015400040510     C                   leave
015500040510     C                   endif
015600040510     C                   enddo
015700040510     C*
015800040218     C                   if        vindta > *blanks AND
015900040510     C                             wFlgVINDTA = 1
016000010330     C                   add       1             rrnum
016100010330     C*
016200010601     C                   if        *in70 = *off and
016300010330     C                             (vinflg = *blanks
016400010330     C                              or vinflg = '0'
016500010330     C                              or vinflg = '2')
016600010330     C*
016700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016800010711     C                   if        vinflg = *blanks or vinflg = '0'
016900010711     C                   clear                   vinmsg
017000010711     C                   endif
017100010601     C*
017200010330     C                   exsr      inzvar
017300040708     C                   exsr      impvao
017400010601     C*
017500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017600040708     C                   eval      depfil = VAOPOE
017700010601     C                   exsr      repfil
017800010601     C                   if        depfil = invfil
017900021025     C                   if        vlrpoi = 999
018000040708     C                   move(P)   invfil        VAOPOE
018100021025     C                   else
018200040708     C                   move(P)   vlrpoi        VAOPOE
018300021025     C                   endif
018400010601     C*
018500010604     C* Ebbene...
018600010604     C*
018700030822     C  N31              add       1             §CTROK            7 0
018800030822     C   32              add       1             §CTRMO            7 0
018900030822     C   31              add       1             §CTRNO            7 0
019000030822     C                   if        wGiro = 2
019100040723     C  N31              EXSR      CHKCOR
019200040723     C  N31              EXSR      WRIORI
019300050826     C  N31              EXSR      WRIVGD
019400040723     C  N31              write     FNVAO000
019500030822     C                   endif
019600010604     C*
019700010604     C                   if        *in31 = *off and
019800010604     C                             *in32 = *off
019900010604     C                   eval      vinflg = '1'
020000010604     C                   else
020100010604     C                   eval      vinflg = '2'
020200010604     C                   endif
020300010604     C                   endif
020400010601     C*
020500010604     C                   endif
020600010604     C*
020700010330     C                   else
020800010330     C                   eval      vinflg = '1'
020900010330     C                   endif
021000010601     C*
021100010601     C  N70              update    tivin000
021200010330     C*
021300030822     C  N70              enddo
021400010601     C
021500010601     C*
021600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021700010601     C                   if        cntNonEl = *zeros or
021800010601     C                             flgMulti = '0'
021900010330     C* Se non ci sono record con errori ...
022000010601     C                   if        §ctrno = 0 and
022100010604     C                             §ctrmo = 0 and
022200010601     C                             flgStato <> '2'
022300010330     C* ... restituisco esito OK.
022400010330     C                   eval      wrkesito = '0'
022500010330     C                   else
022600010330     C                   if        §ctrok > 0
022700010330     C                   eval      wrkesito = '1'
022800010330     C                   else
022900010615     C                   if        flgOk = '0'
023000010615     C                   eval      wrkesito = '2'
023100010615     C                   else
023200010615     C                   eval      wrkesito = '6'
023300010615     C                   endif
023400010330     C                   endif
023500010330     C                   endif
023600010601     C                   else
023700010601     C                   eval      wrkesito = '9'
023800010601     C                   endif
023900010330     C*
024000010330     C                   if        %open(tivin00r)
024100010330     C                   close     tivin00r
024200010330     C                   endif
024300040708     C                   if        %open(fnvaowwr)
024400040708     C                   close     fnvaowwr
024500010330     C                   endif
024600010601     C*
024700010601     C                   if        vlrpoi <> 999
024800010601     C                   eval      invfil = vlrpoi
024900010601     C                   endif
025000010330     C*
025100010330     C                   if        §ctrok > 0
025200010601     C                             and invfil > *zeros
025300010330     C                   exsr      invio
025400010330     C                   endif
025500010601     C*
025600010618     C                   if        flgGiro = '1'
025700010601     C                   exsr      endela
025800010618     C                   endif
025900010330     C*
026000010330     C                   ENDSR
026100010330     C***
026200040723
026300040723
026400040723     C*------------------------------------------------------------------------*
026500040723     C* CHKCOR - Considerazioni su VAOCOR
026600040723     C*------------------------------------------------------------------------*
026700040723     C     CHKCOR        BEGSR
026800040723     C*
026900040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
027000040723     C                   if        vaoTCO = 'F'
027100040723     C* Quindi verifico se il codice ordinante non è valorizzato
027200040723     C                   if        vaoCOR = *zeros  AND
027300040723     C                             vaoRSO = *blanks AND
027400040723     C                             vaoINO = *blanks AND
027500040723     C                             vaoCAO = *blanks AND
027600040723     C                             vaoLOO = *blanks AND
027700040723     C                             vaoPRO = *blanks AND
027800040723     C                             vaoNAO = *blanks
027900040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
028000040723     C                   movel     '0'           wFlgAcr           1
028100040723     C                   move      vlrKSC        wCli              7 0
028200040723     C                   move(p)   wCli          acrCRO
028300040723     C     acrCRO        setll     fnacr01l
028400040723     C                   if        %found(fnacr01l)
028500040723     C                   read      fnacr01l
028600040723     C                   dow       not %eof(fnacr01l)
028700040723     C                   movel     acrCRO        wCliAcr           7 0
028800040723     C                   if        wCliAcr = wCli
028900040723     C                   eval      vaoCOR = acrCRO
029000040723     C                   movel     '1'           wFlgAcr
029100040723     C                   leave
029200040723     C                   else
029300040723     C                   leave
029400040723     C                   endif
029500040723     C                   read      fnacr01l
029600040723     C                   enddo
029700040723     C                   endif
029800040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
029900040723     C* dati anagrafici dell'ordinante dal piano dei conti
030000040723     C                   if        wFlgAcr = '0'
030100040723     C                   clear                   BS69DS
030200040723     C                   clear                   ACODS
030300040723     C                   clear                   INDDS
030400040723     C                   clear                   CLPDS
030500040723     C                   clear                   CLSDS
030600040723     C                   move(P)   vlrKSC        I69KAC
030700040723     C                   move(P)   vlrKSC        I69KIN
030800040723     C                   call      'TIBS69R'
030900040723     C                   parm                    BS69DS
031000040723     C                   parm                    ACODS
031100040723     C                   parm                    INDDS
031200040723     C                   parm                    CLPDS
031300040723     C                   parm                    CLSDS
031400040723     C     O69ERR        ifne      '1'
031500040723     C                   eval      vaoRSO = ACORAG
031600040723     C                   eval      vaoINO = INDVIA
031700040723     C                   movel(P)  INDCAP        vaoCAO
031800040723     C                   eval      vaoLOO = INDCIT
031900040723     C                   eval      vaoPRO = INDPRV
032000040723     C                   eval      vaoNAO = INDSTA
032100040723     C                   endif
032200040723     C                   endif
032300040723     C                   endif
032400040723     C                   endif
032500040723     C*
032600040723     C                   ENDSR
032700040723
032800040723
032900040723     C*------------------------------------------------------------------------*
033000040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
033100040723     C*------------------------------------------------------------------------*
033200040723     C     WRIORI        BEGSR
033300040723     C*
033400090618     C* Come prima cosa stacco un numeratore da AZNUM
033500090618     C                   clear                   trul33ds
033600090618     C                   eval      I33OPE = *zeros
033700090618     C                   eval      I33CNU = 600
033800090618     C                   eval      I33NUM = 1
033900090618     C                   movel     TRUL33DS      KPJBU
034000090618     C                   call      'TRUL33R'
034100090618     C                   parm                    KPJBA
034200090618     C                   movel     KPJBU         TRUL33DS
034300090618     C                   if        O33ERR = *zeros
034400090618     C                   z-add     O33NRF        §numori
034500090618     c                   else
034600090618     c                   eval      §numori = *all'9'
034700090618     C                   endif
034800040723     C*
034900040723     C* Quindi imposto i campi "extra-VAO"
035000040723     C                   if        vlrKSC = '0ORM0000'
035100040723     C*
035200040723     C                   if        vaoCOR > *zeros
035300040723     C                   movel     vaoCOR        wrkCOR7           7 0
035400040723     C*
035500040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
035600040723     C* codice padre. Se è un padre assegno già il codice ordinante.
035700040723     C                   clear                   tibs10ds
035800040723     C                   eval      d10drf = datcor                              *data riferimento
035900040723     C                   eval      d10paf = 'P'                                 *cerca il padre
036000040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
036100091026     C*
036200091026     C* Preparazione chiamata al TIBS10R1
036300091026     C                   clear                   tibs10r1ds
036400091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
036500091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
036600091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
036700091026     C*
036800091026     C                   call      'TIBS10R1'
036900091026     C                   parm                    tibs10r1ds
037000091026     C*
037100091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
037200040723     C*
037300040723     C                   if        d10err <> *blanks or                         *No unif. o padre
037400040723     C                             d10cop =  *zeros  or
037500040723     C                             d10cop =  d10cod
037600040723     C                   move      d10cod        oriIDC
037700040723     C                   else                                                   *Figlio
037800040723     C                   move      d10cop        oriIDC
037900040723     C                   endif
038000040723     C*
038100040723     C                   else
038200040723     C                   eval      oriIDC = *zeros
038300040723     C                   endif
038400040723     C*
038500040723     C                   else
038600040723     C                   eval      oriIDC = vlrKSC
038700040723     C                   endif
038800040723     C*
038900040723     C                   eval      oriPRG = §numori
039000040723     C                   eval      oriDER = datcor
039100040723     C                   eval      oriOER = wn6
039200040723     C                   eval      oriPOE = vaoPOE
039300040723     C                   eval      oriNSR = vaoNSR
039400040723     C                   eval      oriNOR = vaoNOR
039500040723     C                   eval      oriNRV = vaoNRV
039600040723     C                   eval      oriFLO = *blanks
039700040723     C*
039800040723     C* Infine valorizzo la chiave esterna sul file FNVAO
039900040723     C                   eval      dorm01 = vaoFLO
040000040723     C                   movel     §numori       §ormpg
040100050412     C*
040200050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
040300050412     C                   movel     'S'           §ormfr
040400120430     C                   movel     'S'           §ormks
040500120430     C                   movel     'S'           §ormfd
040600120430     C                   if        wVAOFLO = 'S'
040700120430     C                   movel     'S'           §orcomc
040800120430     C                   else
040900120430     C                   movel     'N'           §orcomc
041000120430     C                   endif
041100050412     C*
041200050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
041300040723     C                   eval      vaoFLO = dorm01
041400040723     C*
041500040723     C* ...quindi scrivo il file TIORI00F
041600040723     C                   WRITE     TIORI000
041700040723     C*
041800040723     C                   ENDSR
041900050826
042000050826
042100050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
042200050826     C     wriVGD        BEGSR
042300050826     C*
042400050826     C* Reperisco la descrizione della fase da tabella
042500050826     C                   movel(p)  'FAR'         KteCOD
042600050826     C                   movel(p)  '000'         KteKE1
042700050826     C     KEYtbe        chain     tntbe01l
042800050826     C                   if        %found(tntbe01l)
042900050826     C                   movel(P)  tbeuni        DFAR
043000050826     C                   else
043100050826     C                   clear                   DFAR
043200050826     C                   endif
043300050826     C*
043400050826     C* Valorizzo buffer record
043500050826     C                   clear                   FNVAPDS
043600050826     C                   eval      vapIDC = oriIDC
043700050826     C                   eval      vapPOE = oriPOE
043800050826     C                   eval      vapRFA = vaoRFA
043900050826     C                   eval      vapPOG = oriPOE
044000050826     C                   eval      vapDAE = oriDER
044100050826     C                   eval      vapORE = oriOER
044200050826     C                   if        d§fardva <> *blanks
044300050826     C                   eval      vapDFA = d§fardva
044400050826     C                   else
044500050826     C                   eval      vapDFA = d§fardes
044600050826     C                   endif
044700050826     C*
044800050826     C                   clear                   tivgd000
044900080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
045000050826     C                   eval      vgdTIP = 'VP'
045100050826     C                   eval      vgdKSU = vapIDC
045200050826     C                   eval      vgdTSC = 'WW'
045300050826     C                   eval      vgdDAT = datcor
045400120430     C                   eval      vgdPGM = 'TITV2L0R'
045500060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
045600060306     C                   exsr      sr_Control
045700060306     C                   if        Cli_Abil = *on
045800060306     C                   eval      vgdKSU = woriIDC
045900050826     C                   write     tivgd000
046000060306     C                   endif
046100050826     C*
046200050826     C                   ENDSR
046300050826     C*------------------------------------------------------------------------*
046400010601
046500010601
046600010330     C*----------------------------------------------------*
046700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046800010330     C*----------------------------------------------------*
046900010330     C     INZVAR        BEGSR
047000010330     C*
047100030822     C* Inizializzo variabili di wrk
047200020204     C                   Z-ADD     *zeros        Num5_0            5 0
047300030822     C*
047400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047500040708     C                   CLEAR                   FNVAO000
047600040723     C                   CLEAR                   TIORI000
047700030822     C                   CLEAR                   SkSplitCSV
047800030822     C                   EVAL      i = 1
047900030822     C                   EVAL      posDa = *zeros
048000030822     C                   EVAL      posA  = *zeros
048100030822     C*
048200030822     C* Reimposto i valori di default
048300030822     C                   EXSR      DEFCAM
048400030822     C*
048500010330     C                   ENDSR
048600010330     C*----------------------------------------------------*
048700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048800010330     C*----------------------------------------------------*
048900010330     C     DEFCAM        BEGSR
049000030822     C*
049100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049200030715     C* e delimitatore testo.
049300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049600030822     C*
049700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049900030715     C                   SETON                                        50
050000030715     C                   ELSE
050100030715     C                   SETOFF                                       50
050200030715     C                   ENDIF
050300040723     C*
050400080707     C* Imposto i valori d default d alcuni campi
050500080708     C*
050600080708     C                   CLEAR                   DORM01
050700091103     C                   EVAL      VAOFLO = DORM01
050800080707     C*
050900091103     C                   EVAL      VAOTCO = 'F'
051000091103     C                   EVAL      VAOTOR = 'S'
051100091127     C*
051200091204     C* IMPOSTO I CAMPI PERSONALIZZATI
051300100416     C
051400030822     C*
051500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051600030822     C                   EVAL      posDaDft = 1
051700030822     C                   EVAL      posADft  = 0
051800030822     C                   EVAL      wGiroDft = 0
051900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052000030822     C                             posDaDft > 0
052100030822     C*
052200030822     C* Gestisco il 1° giro
052300030822     C                   IF        wGiroDft = 0
052400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052600030822     C* Incremento il contatore dei "giri"
052700030822     C                   EVAL      wGiroDft = 1
052800030822     C                   ELSE
052900030822     C                   EVAL      posDaDft = posADft
053000030822     C                   ENDIF
053100030822     C* Eseguo lo scan x trovare la fine del campo corrente
053200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053300030822     C*
053400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053600030822     C                   IF        posDaDft > 0 AND
053700030822     C                             posADft  > 0
053800040708     C* POE
053900040708     C                   IF        %subst(
054000040708     C                             %subst(vlrppt:posDaDft+1:
054100040708     C                             posADft-posDaDft-1):1:3)
054200040708     C                             = 'POE'
054300040708     C                   EVAL      PiStr=%trim(%subst(
054400040708     C                             %subst(vlrppt:posDaDft+1:
054500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600040708     C                   EXSR      CHKNUM
054700040708     C                   IF        PiInt=*on
054800040708     C                   Z-ADD     PiVal         VAOPOE
054900040708     C                   ENDIF
055000040708     C                   ENDIF
055100040708     C* TOR
055200040708     C                   IF        %subst(
055300040708     C                             %subst(vlrppt:posDaDft+1:
055400040708     C                             posADft-posDaDft-1):1:3)
055500040708     C                             = 'TOR'
055600040708     C                   EVAL      VAOTOR=%trim(%subst(
055700040708     C                             %subst(vlrppt:posDaDft+1:
055800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900040708     C                   ENDIF
056000040708     C* TCO
056100040708     C                   IF        %subst(
056200040708     C                             %subst(vlrppt:posDaDft+1:
056300040708     C                             posADft-posDaDft-1):1:3)
056400040708     C                             = 'TCO'
056500040708     C                   EVAL      VAOTCO=%trim(%subst(
056600040708     C                             %subst(vlrppt:posDaDft+1:
056700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800040708     C                   ENDIF
056900040707     C* COR
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300040707     C                             = 'COR'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900040708     C                   Z-ADD     PiVal         VAOCOR
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200040708     C* CRC
058300040708     C                   IF        %subst(
058400040708     C                             %subst(vlrppt:posDaDft+1:
058500040708     C                             posADft-posDaDft-1):1:3)
058600040708     C                             = 'CRC'
058700040708     C                   EVAL      PiStr=%trim(%subst(
058800040708     C                             %subst(vlrppt:posDaDft+1:
058900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000040708     C                   EXSR      CHKNUM
059100040708     C                   IF        PiInt=*on
059200040708     C                   Z-ADD     PiVal         VAOCRC
059300040708     C                   ENDIF
059400040708     C                   ENDIF
059500040708     C* FFD
059600040708     C                   IF        %subst(
059700040708     C                             %subst(vlrppt:posDaDft+1:
059800040708     C                             posADft-posDaDft-1):1:3)
059900040708     C                             = 'FFD'
060000040708     C                   EVAL      VAOFFD=%trim(%subst(
060100040708     C                             %subst(vlrppt:posDaDft+1:
060200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300040708     C                   ENDIF
060400040708     C* ORR
060500040708     C                   IF        %subst(
060600040708     C                             %subst(vlrppt:posDaDft+1:
060700040708     C                             posADft-posDaDft-1):1:3)
060800040708     C                             = 'ORR'
060900040708     C                   EVAL      PiStr=%trim(%subst(
061000040708     C                             %subst(vlrppt:posDaDft+1:
061100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200040708     C                   EXSR      CHKNUM
061300040708     C                   IF        PiInt=*on
061400040708     C                   Z-ADD     PiVal         VAOORR
061500040708     C                   ENDIF
061600040708     C                   ENDIF
061700040712     C* NCL
061800040712     C                   IF        %subst(
061900040712     C                             %subst(vlrppt:posDaDft+1:
062000040712     C                             posADft-posDaDft-1):1:3)
062100040712     C                             = 'NCL'
062200040712     C                   EVAL      PiStr=%trim(%subst(
062300040712     C                             %subst(vlrppt:posDaDft+1:
062400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500040712     C                   EXSR      CHKNUM
062600040712     C                   IF        PiInt=*on
062700040712     C                   Z-ADD     PiVal         VAONCL
062800040712     C                   ENDIF
062900040712     C                   ENDIF
063000040712     C* PKG
063100040712     C                   IF        %subst(
063200040712     C                             %subst(vlrppt:posDaDft+1:
063300040712     C                             posADft-posDaDft-1):1:3)
063400040712     C                             = 'PKG'
063500040712     C                   EVAL      PiStr=%trim(%subst(
063600040712     C                             %subst(vlrppt:posDaDft+1:
063700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800040712     C                   EXSR      CHKNUM
063900040712     C                   IF        PiNum=*on
064000040712     C                   Z-ADD     PiVal         VAOPKG
064100040712     C                   ENDIF
064200040712     C                   ENDIF
064300040712     C* VLM
064400040712     C                   IF        %subst(
064500040712     C                             %subst(vlrppt:posDaDft+1:
064600040712     C                             posADft-posDaDft-1):1:3)
064700040712     C                             = 'VLM'
064800040712     C                   EVAL      PiStr=%trim(%subst(
064900040712     C                             %subst(vlrppt:posDaDft+1:
065000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100040712     C                   EXSR      CHKNUM
065200040712     C                   IF        PiNum=*on
065300040712     C                   Z-ADD     PiVal         VAOVLM
065400040712     C                   ENDIF
065500040712     C                   ENDIF
065600040708     C* NAM
065700040708     C                   IF        %subst(
065800040708     C                             %subst(vlrppt:posDaDft+1:
065900040708     C                             posADft-posDaDft-1):1:3)
066000040708     C                             = 'NAM'
066100040708     C                   EVAL      VAONAM=%trim(%subst(
066200040708     C                             %subst(vlrppt:posDaDft+1:
066300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400040708     C                   ENDIF
066500040708     C* PAG
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900040708     C                             = 'PAG'
067000040708     C                   EVAL      VAOPAG=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400040708     C* KSC
067500040708     C                   IF        %subst(
067600040708     C                             %subst(vlrppt:posDaDft+1:
067700040708     C                             posADft-posDaDft-1):1:3)
067800040708     C                             = 'KSC'
067900040708     C                   EVAL      PiStr=%trim(%subst(
068000040708     C                             %subst(vlrppt:posDaDft+1:
068100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200040708     C                   EXSR      CHKNUM
068300040708     C                   IF        PiInt=*on
068400040708     C                   Z-ADD     PiVal         VAOKSC
068500040708     C                   ENDIF
068600040708     C                   ENDIF
068700040708     C* CTR
068800040708     C                   IF        %subst(
068900040708     C                             %subst(vlrppt:posDaDft+1:
069000040708     C                             posADft-posDaDft-1):1:3)
069100040708     C                             = 'CTR'
069200040708     C                   EVAL      VAOCTR=%trim(%subst(
069300040708     C                             %subst(vlrppt:posDaDft+1:
069400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500040708     C                   ENDIF
069600040708     C* POC
069700040708     C                   IF        %subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):1:3)
070000040708     C                             = 'POC'
070100040708     C                   EVAL      PiStr=%trim(%subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400040708     C                   EXSR      CHKNUM
070500040708     C                   IF        PiInt=*on
070600040708     C                   Z-ADD     PiVal         VAOPOC
070700040708     C                   ENDIF
070800040708     C                   ENDIF
070900040708     C* NO1
071000040708     C                   IF        %subst(
071100040708     C                             %subst(vlrppt:posDaDft+1:
071200040708     C                             posADft-posDaDft-1):1:3)
071300040708     C                             = 'NO1'
071400040708     C                   EVAL      VAONO1=%trim(%subst(
071500040708     C                             %subst(vlrppt:posDaDft+1:
071600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700040708     C                   ENDIF
071800040708     C* DDT
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200040708     C                             = 'DDT'
072300040708     C                   EVAL      VAODDT=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700040708     C* RFA
072800040708     C                   IF        %subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):1:3)
073100040708     C                             = 'RFA'
073200040708     C                   EVAL      VAORFA=%trim(%subst(
073300040708     C                             %subst(vlrppt:posDaDft+1:
073400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500040708     C                   ENDIF
073600040708     C* FLO
073700040708     C                   IF        %subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):1:3)
074000040708     C                             = 'FLO'
074100040708     C                   EVAL      VAOFLO=%trim(%subst(
074200040708     C                             %subst(vlrppt:posDaDft+1:
074300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400091103     C*
074500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
074600080702     C* e posizione 7 del VAOFLO
074700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
074800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
074900050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
075000040708     C                   ENDIF
075100030822     C* ...
075200030822     C                   ENDIF
075300030822     C                   ENDDO
075400020204     C*
075500010330     C                   ENDSR
075600010607     C*----------------------------------------------------*
075700040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
075800010607     C*----------------------------------------------------*
075900040708     C     IMPVAO        BEGSR
076000010607     C*
076100010607     C                   SETOFF                                       3132
076200030515     C*
076300040708     C                   MOVEL     datcor        VAODAO
076400040708     C                   MOVEL     wn14          VAOOAO
076500050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076600050314     C     minu:maiu     XLATE     vindta        vindta
076700010607     C*
076800030715     C**********
076900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077000030715     C**********
077100030715     C                   IF        wGiro = *zeros
077200040709     C*
077300040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
077400040709     C                   EVAL      depvinDTA = vinDTA
077500040709     C*
077600040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077700040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077800040709     C                                      %trim(intcol(3))
077900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078000050314     C**!!!minu:maiu     XLATE     vindta        vindta
078100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078200040324     C                   DOW       posDa <= %len(%trimr(vindta))
078300030715     C*
078400030715     C* Gestisco il 1° campo
078500030715     C                   IF        i = 1
078600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078700030715     C                   EVAL      posDa = 1
078800030715     C* Eseguo lo scan x trovare la fine del primo campo
078900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079000030715     C* A questo punto "estraggo" il campo corrente
079100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079200030715     C                                                    (posA-posDa))
079300030715     C* X i campi successivi al 1°
079400030715     C                   ELSE
079500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079600030715     C                   EVAL      posDa = posA + 1
079700030715     C* Eseguo lo scan x trovare la fine del campo corrente
079800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079900030715     C* Gestisco l'ultimo campo
080000030715     C                   IF        posA = *zeros
080100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
080200030715     C* A questo punto "estraggo" il campo corrente
080300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080400030715     C                                                    (posA-posDa))
080500030715     C                   LEAVE
080600030715     C                   ELSE
080700030715     C* A questo punto "estraggo" il campo corrente
080800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080900030715     C                                                    (posA-posDa))
081000030715     C                   ENDIF
081100030715     C                   ENDIF
081200030715     C* Incremento il contatore d campo
081300030715     C                   EVAL      i = i +1
081400030715     C                   ENDDO
081500030715     C*
081600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081700030715     C* delimitazione testo
081800030715     C                   EVAL      i = 1
081900030715     C                   DOW       i < %elem(SkSplitFLD)
082000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082200030715     C                   EVAL      i = i + 1
082300030715     C                   ENDDO
082400030715     C                   EVAL      wGiro = 1
082500040709     C* Ripristino la 1° riga originale
082600040709     C                   EVAL      vinDTA = depvinDTA
082700030715     C                   ELSE
082800030715     C                   EVAL      wGiro = 2
082900031204     C**********
083000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083100031204     C* d separatore campo
083200031204     C**********
083300040709     C                   EVAL      vindta = %trim(vindta)
083400040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083500040709     C                   EVAL      vindta = %subst(vindta:2)
083600040709     C                   ENDDO
083700040709     C*
083800040709     C                   Z-ADD     *zeros        lunghInput        4 0
083900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
084000040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084100040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084200040709     C                   EVAL      lunghInput = %len(%trim(vindta))
084300040709     C                   ENDDO
084400030715     C**********
084500030715     C* Effettuo lo split del campo dati d input
084600030715     C**********
084700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084800040324     C                   DOW       posDa <= %len(%trimr(vindta))
084900030715     C*
085000030715     C* Gestisco il 1° campo
085100030715     C                   IF        i = 1
085200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085300030715     C                   EVAL      posDa = 1
085400030715     C* Eseguo lo scan x trovare la fine del primo campo
085500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085600030715     C* A questo punto "estraggo" il campo corrente
085700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085800030715     C                                                    (posA-posDa))
085900030715     C* X i campi successivi al 1°
086000030715     C                   ELSE
086100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086200030715     C                   EVAL      posDa = posA + 1
086300031204     C* Verifico che nn vi sia il campo nn valorizzato
086400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086500031204     C* Se campo nn valorizzato skippo al prossimo
086600031204     C                   EVAL      SkSplitCSV(i) = *blanks
086700031204     C                   EVAL      posA  = posA + 1
086800031204     C                   ELSE
086900030715     C* Eseguo lo scan x trovare la fine del campo corrente
087000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087100030715     C* Gestisco l'ultimo campo
087200030715     C                   IF        posA = *zeros
087300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
087400030715     C* A questo punto "estraggo" il campo corrente
087500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087600030715     C                                                    (posA-posDa))
087700030715     C                   LEAVE
087800030715     C                   ELSE
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C                   ENDIF
088300031204     C                   ENDIF
088400030715     C                   ENDIF
088500030715     C* Incremento il contatore d campo
088600030715     C                   EVAL      i = i +1
088700030715     C                   ENDDO
088800030715     C*
088900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089000030715     C* delimitazione testo
089100030715     C                   EVAL      i = 1
089200030715     C                   DOW       i < %elem(SkSplitCSV)
089300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089500030715     C                   EVAL      i = i + 1
089600030715     C                   ENDDO
089700030715     C*
089800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089900030715     C                   EVAL      i = 1
090000030715     C                   DOW       i < %elem(SkSplitFLD)
090100030715     C*
090200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090400030715     C                             SkSplitCSV(i) <> *blanks
090500030721     C*** ATB
090600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
090700040708     C                   EVAL      VAOATB = SkSplitCSV(i)
090800030721     C                   ENDIF
090900040708     C*** TOR
091000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
091100040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
091200040708     C                   ENDIF
091300040708     C*** TCO
091400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
091500040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
091600040708     C                   ENDIF
091700040708     C*** RSO
091800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
091900040708     C                   EVAL      VAORSO = SkSplitCSV(i)
092000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092100040708     C     '@':'A'       XLATE     VAORSO        VAORSO
092200030715     C* ==
092300030715     C                   ENDIF
092400040708     C*** INO
092500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
092600040708     C                   EVAL      VAOINO = SkSplitCSV(i)
092700030715     C                   ENDIF
092800040708     C*** LOO
092900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
093000040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
093100030715     C                   ENDIF
093200040708     C*** PRO
093300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
093400040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
093500030715     C                   ENDIF
093600040708     C*** NAO
093700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
093800040708     C                   EVAL      VAONAO = SkSplitCSV(i)
093900030721     C                   ENDIF
094000040708     C*** RSR
094100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
094200040708     C                   EVAL      VAORSR = SkSplitCSV(i)
094300040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094400040708     C     '@':'A'       XLATE     VAORSR        VAORSR
094500040708     C* ==
094600040708     C                   IF        VAORSR = *blanks
094700040708     C                   SETON                                        31
094800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
094900040708     C                             + ' ' + 'VAORSR'
095000040708     C                   ENDIF
095100040708     C                   ENDIF
095200040708     C*** INR
095300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
095400040708     C                   EVAL      VAOINR = SkSplitCSV(i)
095500040708     C                   IF        VAOINR = *blanks
095600040708     C                   SETON                                        31
095700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
095800040708     C                             + ' ' + 'VAOINR'
095900040708     C                   ENDIF
096000040708     C                   ENDIF
096100040708     C*** LOR
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
096300040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
096400040708     C                   IF        VAOLOR = *blanks
096500040708     C                   SETON                                        31
096600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
096700040708     C                             + ' ' + 'VAOLOR'
096800040708     C                   ENDIF
096900040708     C                   ENDIF
097000040708     C*** PRR
097100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
097200040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
097300040708     C                   IF        VAOPRR = *blanks
097400040708     C                   SETON                                        31
097500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097600040708     C                             + ' ' + 'VAOPRR'
097700040708     C                   ENDIF
097800040708     C                   ENDIF
097900040708     C*** NAR
098000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
098100040708     C                   EVAL      VAONAR = SkSplitCSV(i)
098200040708     C                   ENDIF
098300040708     C*** RER
098400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
098500040708     C                   EVAL      VAORER = SkSplitCSV(i)
098600040708     C                   ENDIF
098700040708     C*** TER
098800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
098900040708     C                   EVAL      VAOTER = SkSplitCSV(i)
099000040708     C                   ENDIF
099100040708     C*** RSC
099200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
099300040708     C                   EVAL      VAORSC = SkSplitCSV(i)
099400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099500040708     C     '@':'A'       XLATE     VAORSC        VAORSC
099600040708     C* ==
099700040708     C                   ENDIF
099800040708     C*** INC
099900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
100000040708     C                   EVAL      VAOINC = SkSplitCSV(i)
100100040708     C                   ENDIF
100200040708     C*** LOC
100300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
100400040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
100500040708     C                   ENDIF
100600040708     C*** PRC
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
100800040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
100900040708     C                   ENDIF
101000040708     C*** NAC
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
101200040708     C                   EVAL      VAONAC = SkSplitCSV(i)
101300040708     C                   ENDIF
101400040708     C*** FFD
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
101600040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
101700040708     C                   ENDIF
101800040708     C*** RMP
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
102000040708     C                   EVAL      VAORMP = SkSplitCSV(i)
102100040708     C                   ENDIF
102200040708     C*** NAM
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
102400040708     C                   EVAL      VAONAM = SkSplitCSV(i)
102500040708     C                   ENDIF
102600040708     C*** PAG
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
102800040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
102900040708     C                   ENDIF
103000040708     C*** CTR
103100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
103200040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
103300040708     C                   ENDIF
103400040708     C*** ZOR
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
103600040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
103700040708     C                   ENDIF
103800040708     C*** NO1
103900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
104000040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
104100030715     C                   ENDIF
104200040708     C*** NO2
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
104400040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
104500030715     C                   ENDIF
104600040708     C*** DDT
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
104800040708     C                   EVAL      VAODDT = SkSplitCSV(i)
104900030715     C                   ENDIF
105000040708     C*** RFA
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
105200040708     C                   EVAL      VAORFA = SkSplitCSV(i)
105300030715     C                   ENDIF
105400040708     C*** SPI
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
105600040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
105700030715     C                   ENDIF
105800040708     C*** FLO
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
106000040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
106100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
106200080708     C* e la posizione 7 del VAOFLO
106300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
106400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
106500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
106600030715     C                   ENDIF
106700091103     C
106800091103     C*** DAR
106900050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
107000050412     C                   EVAL      PiStr=SkSplitCSV(i)
107100050412     C                   EXSR      CHKNUM
107200050412     C                   IF        PiInt=*on
107300050412     C                   Z-ADD     PiVal         VAODAR
107400050412     C                   ELSE
107500050412     C                   SETON                                        32
107600050412     C                   EVAL      VAODAR = *zeros
107700050412     C                   EVAL      vinmsg = %trimr(vinmsg)
107800050412     C                             + ' ' + 'VAODAR'
107900050412     C                   ENDIF
108000050314     C                   ENDIF
108100030715     C
108200030721     C*
108300030721     C* Reperisco quindi i campi numerici...
108400040708     C*** POE
108500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
108600040723     C                   EVAL      PiStr=SkSplitCSV(i)
108700040723     C                   EXSR      CHKNUM
108800040723     C                   IF        PiInt=*on    AND
108900040723     C                             PiVal<=999   AND
109000040723     C                             PiVal>*zeros
109100040723     C                   Z-ADD     PiVal         VAOPOE
109200040723     C                   ELSE
109300040723     C                   SETON                                        32
109400040723     C                   EVAL      VAOPOE = *zeros
109500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
109600040723     C                             + ' ' + 'VAOPOE'
109700040723     C                   ENDIF
109800040723     C                   ENDIF
109900040708     C*** COR
110000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
110100040708     C                   EVAL      PiStr=SkSplitCSV(i)
110200040708     C                   EXSR      CHKNUM
110300040708     C                   IF        PiInt=*on
110400040708     C                   Z-ADD     PiVal         VAOCOR
110500040708     C                   ELSE
110600040708     C                   SETON                                        31
110700040708     C                   EVAL      VAOCOR = *zeros
110800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110900040708     C                             + ' ' + 'VAOCOR'
111000040708     C                   ENDIF
111100040708     C                   ENDIF
111200040708     C*** CAO
111300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on
111700030721     C                   Z-ADD     PiVal         Num5_0
111800040708     C                   MOVEL(p)  Num5_0        VAOCAO
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100040708     C                   EVAL      VAOCAO = *zeros
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300040708     C                             + ' ' + 'VAOCAO'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600040708     C*** CRA
112700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on
113100040708     C                   Z-ADD     PiVal         VAOCRA
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400040708     C                   EVAL      VAOCRA = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040708     C                             + ' ' + 'VAOCRA'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900040708     C*** CAR
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
114100040708     C                   EVAL      PiStr=SkSplitCSV(i)
114200040708     C                   EXSR      CHKNUM
114300040708     C                   IF        PiInt=*on
114400040708     C                   Z-ADD     PiVal         Num5_0
114500040708     C                   MOVEL(p)  Num5_0        VAOCAR
114600040708     C                   ELSE
114700040708     C                   SETON                                        31
114800040708     C                   EVAL      VAOCAR = *zeros
114900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
115000040708     C                             + ' ' + 'VAOCAR'
115100040708     C                   ENDIF
115200040708     C                   ENDIF
115300040708     C*** CRC
115400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
115500040708     C                   EVAL      PiStr=SkSplitCSV(i)
115600040708     C                   EXSR      CHKNUM
115700040708     C                   IF        PiInt=*on
115800040708     C                   Z-ADD     PiVal         VAOCRC
115900040708     C                   ELSE
116000040708     C                   SETON                                        32
116100040708     C                   EVAL      VAOCRC = *zeros
116200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
116300040708     C                             + ' ' + 'VAOCRC'
116400040708     C                   ENDIF
116500040708     C                   ENDIF
116600040708     C*** CAC
116700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
116800040708     C                   EVAL      PiStr=SkSplitCSV(i)
116900040708     C                   EXSR      CHKNUM
117000040708     C                   IF        PiInt=*on
117100040708     C                   Z-ADD     PiVal         Num5_0
117200040708     C                   MOVEL(p)  Num5_0        VAOCAC
117300040708     C                   ELSE
117400040708     C                   SETON                                        32
117500040708     C                   EVAL      VAOCAC = *zeros
117600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117700040708     C                             + ' ' + 'VAOCAC'
117800040708     C                   ENDIF
117900040708     C                   ENDIF
118000040708     C*** DAR
118100090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
118200090428     C                   EVAL      PiStr=SkSplitCSV(i)
118300090428     C                   EXSR      CHKNUM
118400090428     C                   IF        PiInt=*on
118500090428     C                   Z-ADD     PiVal         VAODAR
118600090428     C                   ELSE
118700090428     C                   SETON                                        32
118800090428     C                   EVAL      VAODAR = *zeros
118900090428     C                   EVAL      vinmsg = %trimr(vinmsg)
119000090428     C                             + ' ' + 'VAODAR'
119100090428     C                   ENDIF
119200090428     C                   ENDIF
119300040708     C*** ORR
119400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
119500040708     C                   EVAL      PiStr=SkSplitCSV(i)
119600040708     C                   EXSR      CHKNUM
119700040708     C                   IF        PiInt=*on
119800040708     C                   Z-ADD     PiVal         VAOORR
119900040708     C                   ELSE
120000040708     C                   SETON                                        32
120100040708     C                   EVAL      VAOORR = *zeros
120200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120300040708     C                             + ' ' + 'VAOORR'
120400040708     C                   ENDIF
120500040708     C                   ENDIF
120600030721     C*** NCL
120700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on
121100040723     C                   IF        PiVal > *zeros
121200040708     C                   Z-ADD     PiVal         VAONCL
121300040723     C                   ENDIF
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600040708     C                   EVAL      VAONCL = *zeros
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800040708     C                             + ' ' + 'VAONCL'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100040708     C*** PKG
122200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiNum=*on
122600040723     C                   IF        PiVal > *zeros
122700040708     C                   Z-ADD     PiVal         VAOPKG
122800040723     C                   ENDIF
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100040708     C                   EVAL      VAOPKG = *zeros
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300040708     C                             + ' ' + 'VAOPKG'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600040708     C*** VLM
123700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
123800040708     C                   EVAL      PiStr=SkSplitCSV(i)
123900040708     C                   EXSR      CHKNUM
124000040708     C                   IF        PiNum=*on
124100040708     C                   Z-ADD     PiVal         VAOVLM
124200040708     C                   ELSE
124300040708     C                   SETON                                        32
124400040708     C                   EVAL      VAOVLM = *zeros
124500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124600040708     C                             + ' ' + 'VAOVLM'
124700040708     C                   ENDIF
124800040708     C                   ENDIF
124900040708     C*** BNC
125000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
125100040708     C                   EVAL      PiStr=SkSplitCSV(i)
125200040708     C                   EXSR      CHKNUM
125300040708     C                   IF        PiInt=*on
125400040708     C                   Z-ADD     PiVal         VAOBNC
125500040708     C                   ELSE
125600040708     C                   SETON                                        32
125700040708     C                   EVAL      VAOBNC = *zeros
125800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125900040708     C                             + ' ' + 'VAOBNC'
126000040708     C                   ENDIF
126100040708     C                   ENDIF
126200040708     C*** BLC
126300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
126400040708     C                   EVAL      PiStr=SkSplitCSV(i)
126500040708     C                   EXSR      CHKNUM
126600040708     C                   IF        PiInt=*on
126700040708     C                   Z-ADD     PiVal         VAOBLC
126800040708     C                   ELSE
126900040708     C                   SETON                                        32
127000040708     C                   EVAL      VAOBLC = *zeros
127100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127200040708     C                             + ' ' + 'VAOBLC'
127300040708     C                   ENDIF
127400040708     C                   ENDIF
127500040708     C*** ATT
127600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
127700040708     C                   EVAL      PiStr=SkSplitCSV(i)
127800040708     C                   EXSR      CHKNUM
127900040708     C                   IF        PiInt=*on
128000040708     C                   Z-ADD     PiVal         VAOATT
128100040708     C                   ELSE
128200040708     C                   SETON                                        32
128300040708     C                   EVAL      VAOATT = *zeros
128400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128500040708     C                             + ' ' + 'VAOATT'
128600040708     C                   ENDIF
128700040708     C                   ENDIF
128800040708     C*** MTC
128900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
129000040708     C                   EVAL      PiStr=SkSplitCSV(i)
129100040708     C                   EXSR      CHKNUM
129200040708     C                   IF        PiInt=*on
129300040708     C                   Z-ADD     PiVal         VAOMTC
129400040708     C                   ELSE
129500040708     C                   SETON                                        32
129600040708     C                   EVAL      VAOMTC = *zeros
129700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129800040708     C                             + ' ' + 'VAOMTC'
129900040708     C                   ENDIF
130000040708     C                   ENDIF
130100040708     C*** KSC
130200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999 AND
130700030721     C                             PiVal>*zeros
130800040708     C                   Z-ADD     PiVal         VAOKSC
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100040708     C                   EVAL      VAOKSC = *zeros
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300040708     C                             + ' ' + 'VAOKSC'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600040708     C*** POR
131700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on    AND
132100030721     C                             PiVal<=999   AND
132200030721     C                             PiVal>*zeros
132300040708     C                   Z-ADD     PiVal         VAOPOR
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600040708     C                   EVAL      VAOPOR = *zeros
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800040708     C                             + ' ' + 'VAOPOR'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100040708     C*** POC
133200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
133300040708     C                   EVAL      PiStr=SkSplitCSV(i)
133400040708     C                   EXSR      CHKNUM
133500040708     C                   IF        PiInt=*on    AND
133600040708     C                             PiVal<=999   AND
133700040708     C                             PiVal>*zeros
133800040708     C                   Z-ADD     PiVal         VAOPOC
133900040708     C                   ELSE
134000040708     C                   SETON                                        32
134100040708     C                   EVAL      VAOPOC = *zeros
134200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134300040708     C                             + ' ' + 'VAOPOC'
134400040708     C                   ENDIF
134500040708     C                   ENDIF
134600040708     C*** STO
134700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
134800040708     C                   EVAL      PiStr=SkSplitCSV(i)
134900040708     C                   EXSR      CHKNUM
135000040708     C                   IF        PiInt=*on
135100040708     C                   Z-ADD     PiVal         VAOSTO
135200040708     C                   ELSE
135300040708     C                   SETON                                        32
135400040708     C                   EVAL      VAOSTO = *zeros
135500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135600040708     C                             + ' ' + 'VAOSTO'
135700040708     C                   ENDIF
135800040708     C                   ENDIF
135900090428     C*
136000091103     C********** GESTIONE CAMPI PERSONALIZZATI
136100090428     C*
136200120430     C*** W_VAOFLO
136300120430     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOFLO'
136400120430     C                   MOVEL     *blanks       wVAOFLO          20
136500120430     C                   EVAL      wVAOFLO = SkSplitCSV(i)
136600120430     C                   ENDIF
136700090428     C*
136800090428     C****************************************
136900090428     C*
137000030715     C*
137100030715     C                   ENDIF
137200030715     C                   EVAL      i = i + 1
137300030715     C                   ENDDO
137400040723     C*
137500030715     C                   ENDIF
137600020204     C*
137700010607     C                   ENDSR
137800010607     C*----------------------------------------------------*
137900080702
138000080702
138100080702     C*----------------------------------------------------*
138200080702     C*  CONTROLLO NUMERICITA' CAMPI
138300080702     C*----------------------------------------------------*
138400080702     C     CHKNUM        BEGSR
138500080702     C*
138600080702     C                   IF        PiDecChr = *blanks
138700080702     C                   EVAL      PiDecChr = ','
138800080702     C                   ENDIF
138900080702     C*
139000080702     C                   callp     UBISNUM_Check(PiStr
139100080702     C                                          :PiDecChr
139200080702     C                                          :PiVal
139300080702     C                                          :PiNum
139400080702     C                                          :PiInt)
139500080702     C*
139600080702     C                   ENDSR
139700080702     C***
139800080702
139900010601
140000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
140100010601     C     repfil        BEGSR
140200010601     C*
140300010601     C                   if        invfil = *zeros and
140400010601     C                             depfil > *zeros and
140500010629     C                             (vinflg = *blanks or
140600010629     C                              vinflg = *zeros)
140700010601     C
140800010601     C                   eval      invfil = depfil
140900010601     C                   endif
141000010601     C*
141100010601     C                   if        depfil <> invfil and
141200010601     C                             invfil > *zeros
141300010601     C                   eval      flgMulti = '1'
141400010601     C                   if        vinflg = *blanks
141500010601     C                   add       1             cntNonEl
141600010601     C                   endif
141700010601     C                   endif
141800010601     C*
141900010601     C                   if        vinflg = '2'
142000010601     C                   eval      flgStato = '2'
142100010601     C                   endif
142200010601     C*
142300010601     C                   ENDSR
142400010601     C***
142500010330
142600010330
142700990920      /TITLE Invio dei dati al punto operativo.
142800000613     C     invio         BEGSR
142900990920     C*
143000990920     C                   reset                   dscmz
143100010601     C                   move      invfil        cmzdst
143200990920     C                   eval      cmzfld = vlrfou
143300990920     C                   eval      cmzmbd = vlrhdl
143400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
143500000710     C                   if        prmfir = *blanks
143600040708     C                   eval      cmzfla = 'FNVAO00F'
143700040708     C                   eval      cmzmba = 'FNVAO00F'
143800000710     C                   else
143900000710     C                   eval      cmzfla = prmfir
144000000710     C                   eval      cmzmba = prmfir
144100000710     C                   endif
144200990920     C                   eval      cmznrr = *zeros
144300990920     C                   move      §ctrok        cmznrr
144400021018     C                   eval      cmzlba = vlrfl1
144500990920     C                   call(e)   'TIS711C'
144600990920     C                   parm                    dscmz
144700990921     C                   parm      *blanks       esito
144800990920     C                   if        %error
144900990920     C                             or cmzerr = '1'
145000990921     C                             or esito  = '1'
145100000710     C                   eval      wrkesito = '3'
145200990920     C                   endif
145300990920     C*
145400000613     C                   ENDSR
145500010601
145600010601
145700010601      /TITLE Invio dei dati al punto operativo.
145800010601     C     opeini        BEGSR
145900010601     C*
146000010601     C* Inizializzo flag e contatori operativi
146100010601     C                   movel     '0'           flgGiro           1
146200010601     C                   movel     '0'           flgMulti          1
146300010601     C                   movel     '1'           flgStato          1
146400010615     C                   movel     '0'           flgOk             1
146500010601     C                   z-add     *zeros        cntNonEl         10 0
146600010601     C                   z-add     *zeros        depfil            3 0
146700010601     C                   z-add     *zeros        invfil            3 0
146800010601     C*
146900010601     C                   ENDSR
147000010601     C***
147100050421
147200050421
147300091130     C     *pssr         BEGSR
147400050421     C*
147500091130     C                   eval      wrkesito = '2'
147600050421     C*
147700091130     C                   ENDSR     '*CANCL'
147800050421     C***
147900060306
148000060306
148100060306     C*-------------------------------------------------------------------------*
148200060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
148300060306     C*-------------------------------------------------------------------------*
148400060306     C     sr_Control    begsr
148500060306     C*
148600060306     C* inizializzo a ok il flag d controllo abilitazione
148700060306     C                   eval      Cli_Abil = *on
148800060306     C
148900060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
149000060306     C                   clear                   woriIDC
149100060306     C                   eval      kteCOD = 'VAP'
149200060306     C                   eval      kteKE1 = oriIDC
149300060306     C     keytbe        chain     TNTBE01L
149400060306     C                   if        %found(Tntbe01l)
149500060306     C                   eval      woriIDC = TbeKe2
149600060306     C                   else
149700060306     C                   eval      Cli_Abil = *off
149800060306     C                   endif
149900060306     C*
150000060306     C                   endsr
150100010330
150200010330
150300000613     C     *inzsr        BEGSR
150400990910     C*
150500990910     C     *entry        plist
150600990920     C                   parm                    tivlrds
150700990921     C                   parm      wrkesito      esito
150800000724     C                   parm                    prmlit
150900000710     C                   parm                    prmfir
151000010330     C*
151100010330     C* CALCOLA LA DATA CORRENTE
151200091127     C                   z-add     *zeros        datcor            8 0
151300091127     C                   time                    wn14             14 0
151400091127     C                   movel     wn14          wn6               6 0          *ORA (6)
151500091127     C                   eval      datcor = %dec(%date() : *ISO)
151600040723     C*
151700040723     C* Campi riferito al database
151800040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
151900040723     C     *like         define    tbeke1        kteKE1
152000040723     C*
152100040723     C* Definizione chiavi di lettura
152200040723     C* TNTBE01L - parziale
152300040723     C     keytbe        klist
152400040723     C                   kfld                    kteCOD                         *UTENTE
152500040723     C                   kfld                    kteKE1                         *CODICE
152600000613     C*
152700000613     C                   ENDSR
152800000613     C***
152900040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
153000120613VAOTOR;VAOORR;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAORER;VAOTER;
153100120430VAOBNC;VAONCL;VAOPKG;VAORFA;VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;VAOPAG;
153200150112VAOCOR;W_VAOFLO;VAONO1;VAONO2;VAODAR;VAONAM;
