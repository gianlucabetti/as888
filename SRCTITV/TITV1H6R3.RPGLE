000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400040723     Ffnacr01l  if   e           k disk
000500090617     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700040708     FFnvaowwr  O    E             DISK    usropn
000800040723     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000000313     D*
001100040709     D*------------
001200040709     D* SCHIERE A PROGRAMMA
001300040709     D*------------
001400110616     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
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
002500080707     D trul13ds      e ds                  inz
002600090617     d trul33ds      e ds
002700090617     d kpjba         e ds
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003400060306     D Cli_Abil        s              1    inz(*off)
003500060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003600090617     d §numori         s              7  0 inz
003700030715     D*------------------
003800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003900030715     D*------------------
004000030715     D SkSplitFLD      S             10    DIM(100)
004100030715     D SkSplitCSV      S            256    DIM(100)
004200030715     D CharCSV         S              1
004300030715     D CharTXT         S              1
004400030715     D CharNUM         S              1
004500030715     D posDa           S              3  0 INZ(*zeros)
004600030715     D posA            S              3  0 INZ(*zeros)
004700030715     D i               s              3  0 INZ(1)
004800030715     D wGiro           s              1  0 INZ(*zeros)
004900030822     D*------------------
005000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005100030822     D*------------------
005200030822     D posDaDft        S              3  0 INZ(*zeros)
005300030822     D posADft         S              3  0 INZ(*zeros)
005400030822     D j               s              3  0 INZ(1)
005500030822     D wGiroDft        s              1  0 INZ(*zeros)
005600010330     D*------------------
005700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800010330     D*------------------
005900010330     D WLBDA8          DS                  INZ
006000010330     D  G08DAT                 1      8  0
006100010330     D  G08INV                 9     16  0
006200010330     D  G08ERR                17     17
006300010330     D  G08TGI                18     22  0
006400040723     D*------------------
006500040723     D* DS REPERIMENTO DATI CLIENTE
006600040723     D*-------------------
006700040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006800040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006900040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007000040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007100040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007200080702     D*------------------
007300080702     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
007400080702     D*------------------
007500080702     D PiStr           S            100A   INZ
007600080702     D PiDecChr        S              1A   INZ
007700080702     D PiVal           S             63S30 INZ
007800080702     D PiInt           S               N   INZ
007900080702     D PiNum           S               N   INZ
008000080702
008100080702     D*------------------
008200080702     D* LINKING A DEFINIZIONI ESTERNE
008300080702     D*------------------
008400080702     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008500091026
008600091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008700091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008800091026
008900120605     D wRER_1          s                   like(VAORER)
009000120605     D wRER_2          s                   like(VAORER)
009100080702
009200031201     D*------------------
009300031201     D* Costanti
009400031201     D*------------------
009500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700010330
009800010330
009900010330
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500041011     C                   exsr      PREELA
010600040708     C                   exsr      rwvao
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011500010601     C*--------------------------------------------------------
011600010601     C     PREELA        BEGSR
011700010601     C*
011800010601     C* SE OCCORRE SPEDIRE IN FILIALE
011900041011     C                   if        flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300041011     C*
012400041011     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601
013300010601
013400010601
013500010601     C*--------------------------------------------------------
013600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013700010601     C*--------------------------------------------------------
013800010601     C     ENDELA        BEGSR
013900080707     C*
014000080707     C* Chiusura TRUL13R
014100080707     C                   clear                   TRUL13DS
014200080707     C                   eval      I13TLA = 'C'
014300080707     C                   call      'TRUL13R'
014400080707     C                   parm                    TRUL13DS
014500000616     C*
014600010601     C                   ENDSR
014700010601     C***
014800000613
014900010330
015000010601
015100010601
015200010601
015300010330     C*--------------------------------------------------------
015400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
015500010330     C*--------------------------------------------------------
015600040708     C     RWVAO         BEGSR
015700010330     C*
015800010330     C                   if        not %open(tivin00r)
015900010330     C                   open      tivin00r
016000010330     C                   endif
016100040708     C                   if        not %open(fnvaowwr)
016200040708     C                   open      fnvaowwr
016300010330     C                   endif
016400010330     C*
016500010604     C                   clear                   §CTROK
016600010604     C                   clear                   §CTRMO
016700010604     C                   clear                   §CTRNO
016800010330     C*
016900030822     C                   do        *HIVAL
017000010330     C*
017100030822     C                   read      tivin00r                               70
017200010618     C*
017300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017400010618     C                   if        vinflg = '1'
017500010618     C                   eval      flgOk = '1'
017600010618     C                   endif
017700010618     C*
017800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017900040510     C                   z-add     *zeros        wLenVINDTA        4 0
018000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018400040510     C                   z-add     1             wFlgVINDTA
018500040510     C                   leave
018600040510     C                   endif
018700040510     C                   enddo
018800040510     C*
018900040218     C                   if        vindta > *blanks AND
019000040510     C                             wFlgVINDTA = 1
019100010330     C                   add       1             rrnum
019200010330     C*
019300010601     C                   if        *in70 = *off and
019400010330     C                             (vinflg = *blanks
019500010330     C                              or vinflg = '0'
019600010330     C                              or vinflg = '2')
019700010330     C*
019800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019900010711     C                   if        vinflg = *blanks or vinflg = '0'
020000010711     C                   clear                   vinmsg
020100010711     C                   endif
020200010601     C*
020300010330     C                   exsr      inzvar
020400040708     C                   exsr      impvao
020500100512     C                   exsr      exeforz
020600010601     C*
020700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020800040708     C                   eval      depfil = VAOPOE
020900010601     C                   exsr      repfil
021000010601     C                   if        depfil = invfil
021100021025     C                   if        vlrpoi = 999
021200040708     C                   move(P)   invfil        VAOPOE
021300021025     C                   else
021400040708     C                   move(P)   vlrpoi        VAOPOE
021500021025     C                   endif
021600010601     C*
021700010604     C* Ebbene...
021800010604     C*
021900030822     C  N31              add       1             §CTROK            7 0
022000030822     C   32              add       1             §CTRMO            7 0
022100030822     C   31              add       1             §CTRNO            7 0
022200030822     C                   if        wGiro = 2
022300040723     C  N31              EXSR      CHKCOR
022400040723     C  N31              EXSR      WRIORI
022500050826     C  N31              EXSR      WRIVGD
022600040723     C  N31              write     FNVAO000
022700030822     C                   endif
022800010604     C*
022900010604     C                   if        *in31 = *off and
023000010604     C                             *in32 = *off
023100010604     C                   eval      vinflg = '1'
023200010604     C                   else
023300010604     C                   eval      vinflg = '2'
023400010604     C                   endif
023500010604     C                   endif
023600010601     C*
023700010604     C                   endif
023800010604     C*
023900010330     C                   else
024000010330     C                   eval      vinflg = '1'
024100010330     C                   endif
024200010601     C*
024300010601     C  N70              update    tivin000
024400010330     C*
024500030822     C  N70              enddo
024600010601     C
024700010601     C*
024800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024900010601     C                   if        cntNonEl = *zeros or
025000010601     C                             flgMulti = '0'
025100010330     C* Se non ci sono record con errori ...
025200010601     C                   if        §ctrno = 0 and
025300010604     C                             §ctrmo = 0 and
025400010601     C                             flgStato <> '2'
025500010330     C* ... restituisco esito OK.
025600010330     C                   eval      wrkesito = '0'
025700010330     C                   else
025800010330     C                   if        §ctrok > 0
025900010330     C                   eval      wrkesito = '1'
026000010330     C                   else
026100010615     C                   if        flgOk = '0'
026200010615     C                   eval      wrkesito = '2'
026300010615     C                   else
026400010615     C                   eval      wrkesito = '6'
026500010615     C                   endif
026600010330     C                   endif
026700010330     C                   endif
026800010601     C                   else
026900010601     C                   eval      wrkesito = '9'
027000010601     C                   endif
027100010330     C*
027200010330     C                   if        %open(tivin00r)
027300010330     C                   close     tivin00r
027400010330     C                   endif
027500040708     C                   if        %open(fnvaowwr)
027600040708     C                   close     fnvaowwr
027700010330     C                   endif
027800010601     C*
027900010601     C                   if        vlrpoi <> 999
028000010601     C                   eval      invfil = vlrpoi
028100010601     C                   endif
028200010330     C*
028300010330     C                   if        §ctrok > 0
028400010601     C                             and invfil > *zeros
028500010330     C                   exsr      invio
028600010330     C                   endif
028700010601     C*
028800010618     C                   if        flgGiro = '1'
028900010601     C                   exsr      endela
029000010618     C                   endif
029100010330     C*
029200010330     C                   ENDSR
029300010330     C***
029400040723
029500040723
029600040723     C*------------------------------------------------------------------------*
029700040723     C* CHKCOR - Considerazioni su VAOCOR
029800040723     C*------------------------------------------------------------------------*
029900040723     C     CHKCOR        BEGSR
030000040723     C*
030100040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
030200040723     C                   if        vaoTCO = 'F'
030300040723     C* Quindi verifico se il codice ordinante non è valorizzato
030400040723     C                   if        vaoCOR = *zeros  AND
030500040723     C                             vaoRSO = *blanks AND
030600040723     C                             vaoINO = *blanks AND
030700040723     C                             vaoCAO = *blanks AND
030800040723     C                             vaoLOO = *blanks AND
030900040723     C                             vaoPRO = *blanks AND
031000040723     C                             vaoNAO = *blanks
031100040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
031200040723     C                   movel     '0'           wFlgAcr           1
031300040723     C                   move      vlrKSC        wCli              7 0
031400040723     C                   move(p)   wCli          acrCRO
031500040723     C     acrCRO        setll     fnacr01l
031600040723     C                   if        %found(fnacr01l)
031700040723     C                   read      fnacr01l
031800040723     C                   dow       not %eof(fnacr01l)
031900040723     C                   movel     acrCRO        wCliAcr           7 0
032000040723     C                   if        wCliAcr = wCli
032100040723     C                   eval      vaoCOR = acrCRO
032200040723     C                   movel     '1'           wFlgAcr
032300040723     C                   leave
032400040723     C                   else
032500040723     C                   leave
032600040723     C                   endif
032700040723     C                   read      fnacr01l
032800040723     C                   enddo
032900040723     C                   endif
033000040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
033100040723     C* dati anagrafici dell'ordinante dal piano dei conti
033200040723     C                   if        wFlgAcr = '0'
033300040723     C                   clear                   BS69DS
033400040723     C                   clear                   ACODS
033500040723     C                   clear                   INDDS
033600040723     C                   clear                   CLPDS
033700040723     C                   clear                   CLSDS
033800040723     C                   move(P)   vlrKSC        I69KAC
033900040723     C                   move(P)   vlrKSC        I69KIN
034000040723     C                   call      'TIBS69R'
034100040723     C                   parm                    BS69DS
034200040723     C                   parm                    ACODS
034300040723     C                   parm                    INDDS
034400040723     C                   parm                    CLPDS
034500040723     C                   parm                    CLSDS
034600040723     C     O69ERR        ifne      '1'
034700040723     C                   eval      vaoRSO = ACORAG
034800040723     C                   eval      vaoINO = INDVIA
034900040723     C                   movel(P)  INDCAP        vaoCAO
035000040723     C                   eval      vaoLOO = INDCIT
035100040723     C                   eval      vaoPRO = INDPRV
035200040723     C                   eval      vaoNAO = INDSTA
035300040723     C                   endif
035400040723     C                   endif
035500040723     C                   endif
035600040723     C                   endif
035700040723     C*
035800040723     C                   ENDSR
035900040723
036000040723
036100040723
036200040723     C*------------------------------------------------------------------------*
036300040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
036400040723     C*------------------------------------------------------------------------*
036500040723     C     WRIORI        BEGSR
036600040723     C*
036700090617     C* Come prima cosa stacco un numeratore da AZNUM
036800090617     C                   clear                   trul33ds
036900090617     C                   eval      I33OPE = *zeros
037000090617     C                   eval      I33CNU = 600
037100090617     C                   eval      I33NUM = 1
037200090617     C                   movel     TRUL33DS      KPJBU
037300090617     C                   call      'TRUL33R'
037400090617     C                   parm                    KPJBA
037500090617     C                   movel     KPJBU         TRUL33DS
037600090617     C                   if        O33ERR = *zeros
037700090617     C                   z-add     O33NRF        §numori
037800090618     c                   else
037900090618     c                   eval      §numori = *all'9'
038000090617     C                   endif
038100040723     C*
038200040723     C* Quindi imposto i campi "extra-VAO"
038300040723     C                   if        vlrKSC = '0ORM0000'
038400040723     C*
038500040723     C                   if        vaoCOR > *zeros
038600040723     C                   movel     vaoCOR        wrkCOR7           7 0
038700040723     C*
038800040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
038900040723     C* codice padre. Se è un padre assegno già il codice ordinante.
039000040723     C                   clear                   tibs10ds
039100040723     C                   eval      d10drf = datcor                              *data riferimento
039200040723     C                   eval      d10paf = 'P'                                 *cerca il padre
039300040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
039400091026     C*
039500091026     C* Preparazione chiamata al TIBS10R1
039600091026     C                   clear                   tibs10r1ds
039700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
039800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
039900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
040000091026     C*
040100091026     C                   call      'TIBS10R1'
040200091026     C                   parm                    tibs10r1ds
040300091026     C*
040400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
040500040723     C*
040600040723     C                   if        d10err <> *blanks or                         *No unif. o padre
040700040723     C                             d10cop =  *zeros  or
040800040723     C                             d10cop =  d10cod
040900040723     C                   move      d10cod        oriIDC
041000040723     C                   else                                                   *Figlio
041100040723     C                   move      d10cop        oriIDC
041200040723     C                   endif
041300040723     C*
041400040723     C                   else
041500040723     C                   eval      oriIDC = *zeros
041600040723     C                   endif
041700040723     C*
041800040723     C                   else
041900040723     C                   eval      oriIDC = vlrKSC
042000040723     C                   endif
042100040723     C*
042200040723     C                   eval      oriPRG = §numori
042300040723     C                   eval      oriDER = datcor
042400040723     C                   eval      oriOER = wn6
042500040723     C                   eval      oriPOE = vaoPOE
042600040723     C                   eval      oriNSR = vaoNSR
042700040723     C                   eval      oriNOR = vaoNOR
042800040723     C                   eval      oriNRV = vaoNRV
042900040723     C                   eval      oriFLO = *blanks
043000040723     C*
043100040723     C* Infine valorizzo la chiave esterna sul file FNVAO
043200040723     C                   eval      dorm01 = vaoFLO
043300040723     C                   movel     §numori       §ormpg
043400050412     C*
043500050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
043600050412     C                   movel     'S'           §ormfr
043700050412     C*
043800050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
043900040723     C                   eval      vaoFLO = dorm01
044000040723     C*
044100040723     C* ...quindi scrivo il file TIORI00F
044200040723     C                   WRITE     TIORI000
044300040723     C*
044400040723     C                   ENDSR
044500050826
044600050826
044700050826
044800050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
044900050826     C     wriVGD        BEGSR
045000050826     C*
045100050826     C* Reperisco la descrizione della fase da tabella
045200050826     C                   movel(p)  'FAR'         KteCOD
045300050826     C                   movel(p)  '000'         KteKE1
045400050826     C     KEYtbe        chain     tntbe01l
045500050826     C                   if        %found(tntbe01l)
045600050826     C                   movel(P)  tbeuni        DFAR
045700050826     C                   else
045800050826     C                   clear                   DFAR
045900050826     C                   endif
046000050826     C*
046100050826     C* Valorizzo buffer record
046200050826     C                   clear                   FNVAPDS
046300050826     C                   eval      vapIDC = oriIDC
046400050826     C                   eval      vapPOE = oriPOE
046500050826     C                   eval      vapRFA = vaoRFA
046600050826     C                   eval      vapPOG = oriPOE
046700050826     C                   eval      vapDAE = oriDER
046800050826     C                   eval      vapORE = oriOER
046900050826     C                   if        d§fardva <> *blanks
047000050826     C                   eval      vapDFA = d§fardva
047100050826     C                   else
047200050826     C                   eval      vapDFA = d§fardes
047300050826     C                   endif
047400050826     C*
047500050826     C                   clear                   tivgd000
047600080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
047700050826     C                   eval      vgdTIP = 'VP'
047800050826     C                   eval      vgdKSU = vapIDC
047900050826     C                   eval      vgdTSC = 'WW'
048000050826     C                   eval      vgdDAT = datcor
048100080702     C                   eval      vgdPGM = 'TITV1H6R'
048200060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
048300060306     C                   exsr      sr_Control
048400060306     C                   if        Cli_Abil = *on
048500060306     C                   eval      vgdKSU = woriIDC
048600050826     C                   write     tivgd000
048700060306     C                   endif
048800050826     C*
048900050826     C                   ENDSR
049000050826     C*------------------------------------------------------------------------*
049100010601
049200010601
049300010601
049400010330     C*----------------------------------------------------*
049500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049600010330     C*----------------------------------------------------*
049700010330     C     INZVAR        BEGSR
049800010330     C*
049900030822     C* Inizializzo variabili di wrk
050000020204     C                   Z-ADD     *zeros        Num5_0            5 0
050100030822     C*
050200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050300040708     C                   CLEAR                   FNVAO000
050400040723     C                   CLEAR                   TIORI000
050500030822     C                   CLEAR                   SkSplitCSV
050600030822     C                   EVAL      i = 1
050700030822     C                   EVAL      posDa = *zeros
050800030822     C                   EVAL      posA  = *zeros
050900030822     C*
051000030822     C* Reimposto i valori di default
051100030822     C                   EXSR      DEFCAM
051200030822     C*
051300010330     C                   ENDSR
051400010330     C*----------------------------------------------------*
051500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051600010330     C*----------------------------------------------------*
051700010330     C     DEFCAM        BEGSR
051800030822     C*
051900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052000030715     C* e delimitatore testo.
052100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052400030822     C*
052500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052600130115     C* NON PIU' GESTITO
052700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052800030715     C                   SETON                                        50
052900030715     C                   ELSE
053000030715     C                   SETOFF                                       50
053100030715     C                   ENDIF
053200040723     C*
053300080707     C* Imposto i valori d default d alcuni campi
053400111109     C***                EVAL      VAOCOR = 0594068102
053500111109     C***                EVAL      VAOCRC = 0594068102
053600111109     C***                EVAL      VAOCTR = '100'
053700111109     C***                EVAL      VAOORR = 1600
053800111109     C***                EVAL      VAOPOE = 059
053900080708     C*
054000080708     C                   CLEAR                   DORM01
054100080708     C                   EVAL      §ORCOMC = 'S'
054200080708     C                   EVAL      VAOFLO = DORM01
054300100226     C***                EVAL      VAODAR = %dec(%date() + %Days(1) : *ISO)
054400110621     C***                EVAL      VAODAR = datcor
054500080707     C*
054600110628     C                   EVAL      VAOPAG = 'O'
054700111114     C***                EVAL      VAODDT = 'S'
054800080707     C                   EVAL      VAOTCO = 'F'
054900080707     C                   EVAL      VAOTOR = 'S'
055000030822     C*
055100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055200030822     C                   EVAL      posDaDft = 1
055300030822     C                   EVAL      posADft  = 0
055400030822     C                   EVAL      wGiroDft = 0
055500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055600030822     C                             posDaDft > 0
055700030822     C*
055800030822     C* Gestisco il 1° giro
055900030822     C                   IF        wGiroDft = 0
056000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056200030822     C* Incremento il contatore dei "giri"
056300030822     C                   EVAL      wGiroDft = 1
056400030822     C                   ELSE
056500030822     C                   EVAL      posDaDft = posADft
056600030822     C                   ENDIF
056700030822     C* Eseguo lo scan x trovare la fine del campo corrente
056800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056900030822     C*
057000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057200030822     C                   IF        posDaDft > 0 AND
057300030822     C                             posADft  > 0
057400040708     C* POE
057500040708     C                   IF        %subst(
057600040708     C                             %subst(vlrppt:posDaDft+1:
057700040708     C                             posADft-posDaDft-1):1:3)
057800040708     C                             = 'POE'
057900040708     C                   EVAL      PiStr=%trim(%subst(
058000040708     C                             %subst(vlrppt:posDaDft+1:
058100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200040708     C                   EXSR      CHKNUM
058300040708     C                   IF        PiInt=*on
058400040708     C                   Z-ADD     PiVal         VAOPOE
058500040708     C                   ENDIF
058600040708     C                   ENDIF
058700040708     C* TOR
058800040708     C                   IF        %subst(
058900040708     C                             %subst(vlrppt:posDaDft+1:
059000040708     C                             posADft-posDaDft-1):1:3)
059100040708     C                             = 'TOR'
059200040708     C                   EVAL      VAOTOR=%trim(%subst(
059300040708     C                             %subst(vlrppt:posDaDft+1:
059400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500040708     C                   ENDIF
059600040708     C* TCO
059700040708     C                   IF        %subst(
059800040708     C                             %subst(vlrppt:posDaDft+1:
059900040708     C                             posADft-posDaDft-1):1:3)
060000040708     C                             = 'TCO'
060100040708     C                   EVAL      VAOTCO=%trim(%subst(
060200040708     C                             %subst(vlrppt:posDaDft+1:
060300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400040708     C                   ENDIF
060500040707     C* COR
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900040707     C                             = 'COR'
061000030822     C                   EVAL      PiStr=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   EXSR      CHKNUM
061400030822     C                   IF        PiInt=*on
061500040708     C                   Z-ADD     PiVal         VAOCOR
061600030822     C                   ENDIF
061700030822     C                   ENDIF
061800040708     C* CRC
061900040708     C                   IF        %subst(
062000040708     C                             %subst(vlrppt:posDaDft+1:
062100040708     C                             posADft-posDaDft-1):1:3)
062200040708     C                             = 'CRC'
062300040708     C                   EVAL      PiStr=%trim(%subst(
062400040708     C                             %subst(vlrppt:posDaDft+1:
062500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600040708     C                   EXSR      CHKNUM
062700040708     C                   IF        PiInt=*on
062800040708     C                   Z-ADD     PiVal         VAOCRC
062900040708     C                   ENDIF
063000040708     C                   ENDIF
063100040708     C* FFD
063200040708     C                   IF        %subst(
063300040708     C                             %subst(vlrppt:posDaDft+1:
063400040708     C                             posADft-posDaDft-1):1:3)
063500040708     C                             = 'FFD'
063600040708     C                   EVAL      VAOFFD=%trim(%subst(
063700040708     C                             %subst(vlrppt:posDaDft+1:
063800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900040708     C                   ENDIF
064000040708     C* ORR
064100040708     C                   IF        %subst(
064200040708     C                             %subst(vlrppt:posDaDft+1:
064300040708     C                             posADft-posDaDft-1):1:3)
064400040708     C                             = 'ORR'
064500040708     C                   EVAL      PiStr=%trim(%subst(
064600040708     C                             %subst(vlrppt:posDaDft+1:
064700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800040708     C                   EXSR      CHKNUM
064900040708     C                   IF        PiInt=*on
065000040708     C                   Z-ADD     PiVal         VAOORR
065100040708     C                   ENDIF
065200040708     C                   ENDIF
065300040712     C* NCL
065400040712     C                   IF        %subst(
065500040712     C                             %subst(vlrppt:posDaDft+1:
065600040712     C                             posADft-posDaDft-1):1:3)
065700040712     C                             = 'NCL'
065800040712     C                   EVAL      PiStr=%trim(%subst(
065900040712     C                             %subst(vlrppt:posDaDft+1:
066000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100040712     C                   EXSR      CHKNUM
066200040712     C                   IF        PiInt=*on
066300040712     C                   Z-ADD     PiVal         VAONCL
066400040712     C                   ENDIF
066500040712     C                   ENDIF
066600040712     C* PKG
066700040712     C                   IF        %subst(
066800040712     C                             %subst(vlrppt:posDaDft+1:
066900040712     C                             posADft-posDaDft-1):1:3)
067000040712     C                             = 'PKG'
067100040712     C                   EVAL      PiStr=%trim(%subst(
067200040712     C                             %subst(vlrppt:posDaDft+1:
067300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400040712     C                   EXSR      CHKNUM
067500040712     C                   IF        PiNum=*on
067600040712     C                   Z-ADD     PiVal         VAOPKG
067700040712     C                   ENDIF
067800040712     C                   ENDIF
067900040712     C* VLM
068000040712     C                   IF        %subst(
068100040712     C                             %subst(vlrppt:posDaDft+1:
068200040712     C                             posADft-posDaDft-1):1:3)
068300040712     C                             = 'VLM'
068400040712     C                   EVAL      PiStr=%trim(%subst(
068500040712     C                             %subst(vlrppt:posDaDft+1:
068600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700040712     C                   EXSR      CHKNUM
068800040712     C                   IF        PiNum=*on
068900040712     C                   Z-ADD     PiVal         VAOVLM
069000040712     C                   ENDIF
069100040712     C                   ENDIF
069200040708     C* NAM
069300040708     C                   IF        %subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):1:3)
069600040708     C                             = 'NAM'
069700040708     C                   EVAL      VAONAM=%trim(%subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000040708     C                   ENDIF
070100040708     C* PAG
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500040708     C                             = 'PAG'
070600040708     C                   EVAL      VAOPAG=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000040708     C* KSC
071100040708     C                   IF        %subst(
071200040708     C                             %subst(vlrppt:posDaDft+1:
071300040708     C                             posADft-posDaDft-1):1:3)
071400040708     C                             = 'KSC'
071500040708     C                   EVAL      PiStr=%trim(%subst(
071600040708     C                             %subst(vlrppt:posDaDft+1:
071700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800040708     C                   EXSR      CHKNUM
071900040708     C                   IF        PiInt=*on
072000040708     C                   Z-ADD     PiVal         VAOKSC
072100040708     C                   ENDIF
072200040708     C                   ENDIF
072300040708     C* CTR
072400040708     C                   IF        %subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):1:3)
072700040708     C                             = 'CTR'
072800040708     C                   EVAL      VAOCTR=%trim(%subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040708     C                   ENDIF
073200040708     C* POC
073300040708     C                   IF        %subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):1:3)
073600040708     C                             = 'POC'
073700040708     C                   EVAL      PiStr=%trim(%subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000040708     C                   EXSR      CHKNUM
074100040708     C                   IF        PiInt=*on
074200040708     C                   Z-ADD     PiVal         VAOPOC
074300040708     C                   ENDIF
074400040708     C                   ENDIF
074500040708     C* NO1
074600040708     C                   IF        %subst(
074700040708     C                             %subst(vlrppt:posDaDft+1:
074800040708     C                             posADft-posDaDft-1):1:3)
074900040708     C                             = 'NO1'
075000040708     C                   EVAL      VAONO1=%trim(%subst(
075100040708     C                             %subst(vlrppt:posDaDft+1:
075200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300040708     C                   ENDIF
075400040708     C* DDT
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800040708     C                             = 'DDT'
075900040708     C                   EVAL      VAODDT=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300040708     C* RFA
076400040708     C                   IF        %subst(
076500040708     C                             %subst(vlrppt:posDaDft+1:
076600040708     C                             posADft-posDaDft-1):1:3)
076700040708     C                             = 'RFA'
076800040708     C                   EVAL      VAORFA=%trim(%subst(
076900040708     C                             %subst(vlrppt:posDaDft+1:
077000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100040708     C                   ENDIF
077200040708     C* FLO
077300040708     C                   IF        %subst(
077400040708     C                             %subst(vlrppt:posDaDft+1:
077500040708     C                             posADft-posDaDft-1):1:3)
077600040708     C                             = 'FLO'
077700040708     C                   EVAL      VAOFLO=%trim(%subst(
077800040708     C                             %subst(vlrppt:posDaDft+1:
077900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
078100080702     C* e posizione 7 del VAOFLO
078200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
078300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
078400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
078500040708     C                   ENDIF
078600030822     C* ...
078700030822     C                   ENDIF
078800030822     C                   ENDDO
078900020204     C*
079000010330     C                   ENDSR
079100010607     C*----------------------------------------------------*
079200040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
079300010607     C*----------------------------------------------------*
079400040708     C     IMPVAO        BEGSR
079500010607     C*
079600010607     C                   SETOFF                                       3132
079700030515     C*
079800100223     C                   MOVEL     datcor        VAODAO
079900040708     C                   MOVEL     wn14          VAOOAO
080000050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080100050314     C     minu:maiu     XLATE     vindta        vindta
080200010607     C*
080300030715     C**********
080400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080500030715     C**********
080600030715     C                   IF        wGiro = *zeros
080700040709     C*
080800040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080900040709     C                   EVAL      depvinDTA = vinDTA
081000040709     C*
081100040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
081200040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081300110616     C                                      %trim(intcol(3))+%trim(intcol(4))+
081400110616     C                                      %trim(intcol(5))
081500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081600050314     C**!!!minu:maiu     XLATE     vindta        vindta
081700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081800040324     C                   DOW       posDa <= %len(%trimr(vindta))
081900030715     C*
082000030715     C* Gestisco il 1° campo
082100030715     C                   IF        i = 1
082200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082300030715     C                   EVAL      posDa = 1
082400030715     C* Eseguo lo scan x trovare la fine del primo campo
082500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082600030715     C* A questo punto "estraggo" il campo corrente
082700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082800030715     C                                                    (posA-posDa))
082900030715     C* X i campi successivi al 1°
083000030715     C                   ELSE
083100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083200030715     C                   EVAL      posDa = posA + 1
083300030715     C* Eseguo lo scan x trovare la fine del campo corrente
083400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083500030715     C* Gestisco l'ultimo campo
083600030715     C                   IF        posA = *zeros
083700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
083800030715     C* A questo punto "estraggo" il campo corrente
083900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084000030715     C                                                    (posA-posDa))
084100030715     C                   LEAVE
084200030715     C                   ELSE
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   ENDIF
084700030715     C                   ENDIF
084800030715     C* Incremento il contatore d campo
084900030715     C                   EVAL      i = i +1
085000030715     C                   ENDDO
085100030715     C*
085200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085300030715     C* delimitazione testo
085400030715     C                   EVAL      i = 1
085500030715     C                   DOW       i < %elem(SkSplitFLD)
085600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085800030715     C                   EVAL      i = i + 1
085900030715     C                   ENDDO
086000030715     C                   EVAL      wGiro = 1
086100040709     C* Ripristino la 1° riga originale
086200040709     C                   EVAL      vinDTA = depvinDTA
086300030715     C                   ELSE
086400030715     C                   EVAL      wGiro = 2
086500031204     C**********
086600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086700031204     C* d separatore campo
086800031204     C**********
086900040709     C                   EVAL      vindta = %trim(vindta)
087000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087100040709     C                   EVAL      vindta = %subst(vindta:2)
087200040709     C                   ENDDO
087300040709     C*
087400040709     C                   Z-ADD     *zeros        lunghInput        4 0
087500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087900040709     C                   ENDDO
088000030715     C**********
088100030715     C* Effettuo lo split del campo dati d input
088200030715     C**********
088300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088400040324     C                   DOW       posDa <= %len(%trimr(vindta))
088500030715     C*
088600030715     C* Gestisco il 1° campo
088700030715     C                   IF        i = 1
088800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088900030715     C                   EVAL      posDa = 1
089000030715     C* Eseguo lo scan x trovare la fine del primo campo
089100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089200030715     C* A questo punto "estraggo" il campo corrente
089300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089400030715     C                                                    (posA-posDa))
089500030715     C* X i campi successivi al 1°
089600030715     C                   ELSE
089700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089800030715     C                   EVAL      posDa = posA + 1
089900031204     C* Verifico che nn vi sia il campo nn valorizzato
090000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090100031204     C* Se campo nn valorizzato skippo al prossimo
090200031204     C                   EVAL      SkSplitCSV(i) = *blanks
090300031204     C                   EVAL      posA  = posA + 1
090400031204     C                   ELSE
090500030715     C* Eseguo lo scan x trovare la fine del campo corrente
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* Gestisco l'ultimo campo
090800030715     C                   IF        posA = *zeros
090900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
091000030715     C* A questo punto "estraggo" il campo corrente
091100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091200030715     C                                                    (posA-posDa))
091300030715     C                   LEAVE
091400030715     C                   ELSE
091500030715     C* A questo punto "estraggo" il campo corrente
091600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091700030715     C                                                    (posA-posDa))
091800030715     C                   ENDIF
091900031204     C                   ENDIF
092000030715     C                   ENDIF
092100030715     C* Incremento il contatore d campo
092200030715     C                   EVAL      i = i +1
092300030715     C                   ENDDO
092400030715     C*
092500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092600030715     C* delimitazione testo
092700030715     C                   EVAL      i = 1
092800030715     C                   DOW       i < %elem(SkSplitCSV)
092900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093100030715     C                   EVAL      i = i + 1
093200030715     C                   ENDDO
093300030715     C*
093400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitFLD)
093700030715     C*
093800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094000030715     C                             SkSplitCSV(i) <> *blanks
094100030721     C*** ATB
094200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
094300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
094400030721     C                   ENDIF
094500040708     C*** TOR
094600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
094700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
094800040708     C                   ENDIF
094900040708     C*** TCO
095000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
095100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
095200040708     C                   ENDIF
095300040708     C*** RSO
095400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
095500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
095600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
095800030715     C* ==
095900030715     C                   ENDIF
096000040708     C*** INO
096100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
096200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
096300030715     C                   ENDIF
096400040708     C*** LOO
096500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
096600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
096700030715     C                   ENDIF
096800040708     C*** PRO
096900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
097000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
097100030715     C                   ENDIF
097200040708     C*** NAO
097300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
097400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
097500030721     C                   ENDIF
097600040708     C*** RSR
097700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
097800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
097900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
098100040708     C* ==
098200040708     C                   IF        VAORSR = *blanks
098300040708     C                   SETON                                        31
098400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098500040708     C                             + ' ' + 'VAORSR'
098600040708     C                   ENDIF
098700040708     C                   ENDIF
098800040708     C*** INR
098900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
099000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
099100040708     C                   IF        VAOINR = *blanks
099200040708     C                   SETON                                        31
099300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099400040708     C                             + ' ' + 'VAOINR'
099500040708     C                   ENDIF
099600040708     C                   ENDIF
099700040708     C*** LOR
099800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
099900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
100000040708     C                   IF        VAOLOR = *blanks
100100040708     C                   SETON                                        31
100200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100300040708     C                             + ' ' + 'VAOLOR'
100400040708     C                   ENDIF
100500040708     C                   ENDIF
100600040708     C*** PRR
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
100800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
100900040708     C                   IF        VAOPRR = *blanks
101000040708     C                   SETON                                        31
101100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
101200040708     C                             + ' ' + 'VAOPRR'
101300040708     C                   ENDIF
101400040708     C                   ENDIF
101500040708     C*** NAR
101600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
101700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
101800040708     C                   ENDIF
101900040708     C*** RER
102000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
102100040708     C                   EVAL      VAORER = SkSplitCSV(i)
102200040708     C                   ENDIF
102300040708     C*** TER
102400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
102500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
102600040708     C                   ENDIF
102700040708     C*** RSC
102800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
102900040708     C                   EVAL      VAORSC = SkSplitCSV(i)
103000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103100040708     C     '@':'A'       XLATE     VAORSC        VAORSC
103200040708     C* ==
103300040708     C                   ENDIF
103400040708     C*** INC
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
103600040708     C                   EVAL      VAOINC = SkSplitCSV(i)
103700040708     C                   ENDIF
103800040708     C*** LOC
103900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
104000040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
104100040708     C                   ENDIF
104200040708     C*** PRC
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
104400040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
104500040708     C                   ENDIF
104600040708     C*** NAC
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
104800040708     C                   EVAL      VAONAC = SkSplitCSV(i)
104900040708     C                   ENDIF
105000040708     C*** FFD
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
105200040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
105300040708     C                   ENDIF
105400040708     C*** RMP
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
105600040708     C                   EVAL      VAORMP = SkSplitCSV(i)
105700040708     C                   ENDIF
105800040708     C*** NAM
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
106000040708     C                   EVAL      VAONAM = SkSplitCSV(i)
106100040708     C                   ENDIF
106200040708     C*** PAG
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
106400040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
106500040708     C                   ENDIF
106600040708     C*** CTR
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
106800040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
106900040708     C                   ENDIF
107000040708     C*** ZOR
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
107200040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
107300040708     C                   ENDIF
107400040708     C*** NO1
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
107600040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
107700030715     C                   ENDIF
107800040708     C*** NO2
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
108000040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
108100030715     C                   ENDIF
108200040708     C*** DDT
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
108400040708     C                   EVAL      VAODDT = SkSplitCSV(i)
108500030715     C                   ENDIF
108600040708     C*** RFA
108700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
108800040708     C                   EVAL      VAORFA = SkSplitCSV(i)
108900030715     C                   ENDIF
109000040708     C*** SPI
109100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
109200040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
109300030715     C                   ENDIF
109400040708     C*** FLO
109500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
109600040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
109700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
109800080708     C* e la posizione 7 del VAOFLO
109900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
110000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
110100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
110200030715     C                   ENDIF
110300050314
110400050412      *** DAR
110500050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
110600050412     C                   EVAL      PiStr=SkSplitCSV(i)
110700050412     C                   EXSR      CHKNUM
110800050412     C                   IF        PiInt=*on
110900050412     C                   Z-ADD     PiVal         VAODAR
111000050412     C                   ELSE
111100050412     C                   SETON                                        32
111200050412     C                   EVAL      VAODAR = *zeros
111300050412     C                   EVAL      vinmsg = %trimr(vinmsg)
111400050412     C                             + ' ' + 'VAODAR'
111500050412     C                   ENDIF
111600050314     C                   ENDIF
111700030715     C
111800030721     C*
111900030721     C* Reperisco quindi i campi numerici...
112000040708     C*** POE
112100040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
112200040723     C                   EVAL      PiStr=SkSplitCSV(i)
112300040723     C                   EXSR      CHKNUM
112400040723     C                   IF        PiInt=*on    AND
112500040723     C                             PiVal<=999   AND
112600040723     C                             PiVal>*zeros
112700040723     C                   Z-ADD     PiVal         VAOPOE
112800040723     C                   ELSE
112900040723     C                   SETON                                        32
113000040723     C                   EVAL      VAOPOE = *zeros
113100040723     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040723     C                             + ' ' + 'VAOPOE'
113300040723     C                   ENDIF
113400040723     C                   ENDIF
113500040708     C*** COR
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
113700040708     C                   EVAL      PiStr=SkSplitCSV(i)
113800040708     C                   EXSR      CHKNUM
113900040708     C                   IF        PiInt=*on
114000040708     C                   Z-ADD     PiVal         VAOCOR
114100040708     C                   ELSE
114200040708     C                   SETON                                        31
114300040708     C                   EVAL      VAOCOR = *zeros
114400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040708     C                             + ' ' + 'VAOCOR'
114600040708     C                   ENDIF
114700040708     C                   ENDIF
114800040708     C*** CAO
114900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on
115300030721     C                   Z-ADD     PiVal         Num5_0
115400040708     C                   MOVEL(p)  Num5_0        VAOCAO
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700040708     C                   EVAL      VAOCAO = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900040708     C                             + ' ' + 'VAOCAO'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200040708     C*** CRA
116300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on
116700040708     C                   Z-ADD     PiVal         VAOCRA
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000040708     C                   EVAL      VAOCRA = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040708     C                             + ' ' + 'VAOCRA'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500040708     C*** CAR
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
117700040708     C                   EVAL      PiStr=SkSplitCSV(i)
117800040708     C                   EXSR      CHKNUM
117900040708     C                   IF        PiInt=*on
118000040708     C                   Z-ADD     PiVal         Num5_0
118100040708     C                   MOVEL(p)  Num5_0        VAOCAR
118200040708     C                   ELSE
118300040708     C                   SETON                                        31
118400040708     C                   EVAL      VAOCAR = *zeros
118500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118600040708     C                             + ' ' + 'VAOCAR'
118700040708     C                   ENDIF
118800040708     C                   ENDIF
118900040708     C*** CRC
119000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
119100040708     C                   EVAL      PiStr=SkSplitCSV(i)
119200040708     C                   EXSR      CHKNUM
119300040708     C                   IF        PiInt=*on
119400040708     C                   Z-ADD     PiVal         VAOCRC
119500040708     C                   ELSE
119600040708     C                   SETON                                        32
119700040708     C                   EVAL      VAOCRC = *zeros
119800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040708     C                             + ' ' + 'VAOCRC'
120000040708     C                   ENDIF
120100040708     C                   ENDIF
120200040708     C*** CAC
120300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
120400040708     C                   EVAL      PiStr=SkSplitCSV(i)
120500040708     C                   EXSR      CHKNUM
120600040708     C                   IF        PiInt=*on
120700040708     C                   Z-ADD     PiVal         Num5_0
120800040708     C                   MOVEL(p)  Num5_0        VAOCAC
120900040708     C                   ELSE
121000040708     C                   SETON                                        32
121100040708     C                   EVAL      VAOCAC = *zeros
121200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040708     C                             + ' ' + 'VAOCAC'
121400040708     C                   ENDIF
121500040708     C                   ENDIF
121600040708     C*** DAR
121700050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
121800050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
121900050314     C**!!!              EXSR      CHKNUM
122000050314     C**!!!              IF        PiInt=*on
122100050314     C**!!!              Z-ADD     PiVal         VAODAR
122200050314     C**!!!              ELSE
122300050314     C**!!!              SETON                                        32
122400050314     C**!!!              EVAL      VAODAR = *zeros
122500050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
122600050314     C**!!!                        + ' ' + 'VAODAR'
122700050314     C**!!!              ENDIF
122800050314     C**!!!              ENDIF
122900040708     C*** ORR
123000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
123100040708     C                   EVAL      PiStr=SkSplitCSV(i)
123200040708     C                   EXSR      CHKNUM
123300040708     C                   IF        PiInt=*on
123400040708     C                   Z-ADD     PiVal         VAOORR
123500040708     C                   ELSE
123600040708     C                   SETON                                        32
123700040708     C                   EVAL      VAOORR = *zeros
123800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123900040708     C                             + ' ' + 'VAOORR'
124000040708     C                   ENDIF
124100040708     C                   ENDIF
124200030721     C*** NCL
124300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on
124700040723     C                   IF        PiVal > *zeros
124800040708     C                   Z-ADD     PiVal         VAONCL
124900040723     C                   ENDIF
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200040708     C                   EVAL      VAONCL = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400040708     C                             + ' ' + 'VAONCL'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700040708     C*** PKG
125800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiNum=*on
126200040723     C                   IF        PiVal > *zeros
126300040708     C                   Z-ADD     PiVal         VAOPKG
126400040723     C                   ENDIF
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700040708     C                   EVAL      VAOPKG = *zeros
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900040708     C                             + ' ' + 'VAOPKG'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200040708     C*** VLM
127300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
127400040708     C                   EVAL      PiStr=SkSplitCSV(i)
127500040708     C                   EXSR      CHKNUM
127600040708     C                   IF        PiNum=*on
127700040708     C                   Z-ADD     PiVal         VAOVLM
127800040708     C                   ELSE
127900040708     C                   SETON                                        32
128000040708     C                   EVAL      VAOVLM = *zeros
128100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128200040708     C                             + ' ' + 'VAOVLM'
128300040708     C                   ENDIF
128400040708     C                   ENDIF
128500040708     C*** BNC
128600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
128700040708     C                   EVAL      PiStr=SkSplitCSV(i)
128800040708     C                   EXSR      CHKNUM
128900040708     C                   IF        PiInt=*on
129000040708     C                   Z-ADD     PiVal         VAOBNC
129100040708     C                   ELSE
129200040708     C                   SETON                                        32
129300040708     C                   EVAL      VAOBNC = *zeros
129400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129500040708     C                             + ' ' + 'VAOBNC'
129600040708     C                   ENDIF
129700040708     C                   ENDIF
129800040708     C*** BLC
129900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
130000040708     C                   EVAL      PiStr=SkSplitCSV(i)
130100040708     C                   EXSR      CHKNUM
130200040708     C                   IF        PiInt=*on
130300040708     C                   Z-ADD     PiVal         VAOBLC
130400040708     C                   ELSE
130500040708     C                   SETON                                        32
130600040708     C                   EVAL      VAOBLC = *zeros
130700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130800040708     C                             + ' ' + 'VAOBLC'
130900040708     C                   ENDIF
131000040708     C                   ENDIF
131100040708     C*** ATT
131200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
131300040708     C                   EVAL      PiStr=SkSplitCSV(i)
131400040708     C                   EXSR      CHKNUM
131500040708     C                   IF        PiInt=*on
131600040708     C                   Z-ADD     PiVal         VAOATT
131700040708     C                   ELSE
131800040708     C                   SETON                                        32
131900040708     C                   EVAL      VAOATT = *zeros
132000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132100040708     C                             + ' ' + 'VAOATT'
132200040708     C                   ENDIF
132300040708     C                   ENDIF
132400040708     C*** MTC
132500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
132600040708     C                   EVAL      PiStr=SkSplitCSV(i)
132700040708     C                   EXSR      CHKNUM
132800040708     C                   IF        PiInt=*on
132900040708     C                   Z-ADD     PiVal         VAOMTC
133000040708     C                   ELSE
133100040708     C                   SETON                                        32
133200040708     C                   EVAL      VAOMTC = *zeros
133300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133400040708     C                             + ' ' + 'VAOMTC'
133500040708     C                   ENDIF
133600040708     C                   ENDIF
133700040708     C*** KSC
133800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on      AND
134200030721     C                             PiVal<=9999999 AND
134300030721     C                             PiVal>*zeros
134400040708     C                   Z-ADD     PiVal         VAOKSC
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700040708     C                   EVAL      VAOKSC = *zeros
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900040708     C                             + ' ' + 'VAOKSC'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200040708     C*** POR
135300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on    AND
135700030721     C                             PiVal<=999   AND
135800030721     C                             PiVal>*zeros
135900040708     C                   Z-ADD     PiVal         VAOPOR
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200040708     C                   EVAL      VAOPOR = *zeros
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400040708     C                             + ' ' + 'VAOPOR'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700040708     C*** POC
136800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
136900040708     C                   EVAL      PiStr=SkSplitCSV(i)
137000040708     C                   EXSR      CHKNUM
137100040708     C                   IF        PiInt=*on    AND
137200040708     C                             PiVal<=999   AND
137300040708     C                             PiVal>*zeros
137400040708     C                   Z-ADD     PiVal         VAOPOC
137500040708     C                   ELSE
137600040708     C                   SETON                                        32
137700040708     C                   EVAL      VAOPOC = *zeros
137800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137900040708     C                             + ' ' + 'VAOPOC'
138000040708     C                   ENDIF
138100040708     C                   ENDIF
138200040708     C*** STO
138300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
138400040708     C                   EVAL      PiStr=SkSplitCSV(i)
138500040708     C                   EXSR      CHKNUM
138600040708     C                   IF        PiInt=*on
138700040708     C                   Z-ADD     PiVal         VAOSTO
138800040708     C                   ELSE
138900040708     C                   SETON                                        32
139000040708     C                   EVAL      VAOSTO = *zeros
139100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139200040708     C                             + ' ' + 'VAOSTO'
139300040708     C                   ENDIF
139400040708     C                   ENDIF
139500110526     C*
139600110526     C*********  Gestione particolarità
139700110526     C*
139800110616     C*** DAR
139900110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
140000110630     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
140100110630     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
140200110630     C                   ENDIF
140300110616     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
140400110616     C                                   %subst(SkSplitCSV(i):3:2)+
140500110616     C                                   %subst(SkSplitCSV(i):1:2)
140600110616     C                   EXSR      CHKNUM
140700110616     C                   IF        PiInt=*on
140800110616     C                   Z-ADD     PiVal         VAODAR
140900110616     C                   ELSE
141000110616     C                   SETON                                        32
141100110616     C                   EVAL      VAODAR = *zeros
141200110616     C                   EVAL      vinmsg = %trimr(vinmsg)
141300110616     C                             + ' ' + 'VAODAR'
141400110616     C                   ENDIF
141500110616     C                   ENDIF
141600110616     C*** ORR
141700110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOORR'
141800110630     C***                EVAL      PiStr=%subst(SkSplitCSV(i):1:2)+
141900110630     C***                                %subst(SkSplitCSV(i):4:2)
142000110630     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
142100110616     C                   EXSR      CHKNUM
142200110616     C                   IF        PiInt=*on
142300110616     C                   Z-ADD     PiVal         VAOORR
142400110616     C                   ELSE
142500110616     C                   SETON                                        32
142600110616     C                   EVAL      VAOORR = *zeros
142700110616     C                   EVAL      vinmsg = %trimr(vinmsg)
142800110616     C                             + ' ' + 'VAOORR'
142900110616     C                   ENDIF
143000110616     C                   ENDIF
143100120605     C*
143200120605     C*** RER 1 e 2
143300120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORER_1'
143400120605     C                   EVAL      wRER_1=%trim(SkSplitCSV(i))
143500120605     C                   ENDIF
143600120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORER_2'
143700120605     C                   EVAL      wRER_2=%trim(SkSplitCSV(i))
143800120605     C                   ENDIF
143900110526     C*
144000110526     C*********
144100030715     C*
144200030715     C                   ENDIF
144300030715     C                   EVAL      i = i + 1
144400030715     C                   ENDDO
144500120605     C*
144600120605     C***  ===>  Gestione campi "particolari" fuori ciclo
144700120605     C*
144800120605     C                   EVAL      VAORER=%trim(wRER_1) + %trim(wRER_2)
144900040723     C*
145000030715     C                   ENDIF
145100020204     C*
145200010607     C                   ENDSR
145300010607     C*----------------------------------------------------*
145400010330
145500100512
145600100512
145700100512     C*------------------------------------------------------------------------*
145800100512     C* EXEFORZ - Considerazioni finali
145900100512     C*------------------------------------------------------------------------*
146000100512     C     EXEFORZ       BEGSR
146100100512     C*
146200100512     C                   if        VAOKSC = *zeros
146300100512     C                   select
146400100512     C                   when      VAOPAG = 'O'
146500100512     C                   movel     VAOCOR        VAOKSC
146600100512     C                   when      VAOPAG = 'M'
146700100512     C                   movel     VAOCRA        VAOKSC
146800100512     C                   when      VAOPAG = 'D'
146900100512     C                   movel     VAOCRC        VAOKSC
147000100512     C                   endsl
147100100512     C                   endif
147200100512     C*
147300100512     C                   ENDSR
147400080702
147500080702
147600080702     C*----------------------------------------------------*
147700080702     C*  CONTROLLO NUMERICITA' CAMPI
147800080702     C*----------------------------------------------------*
147900080702     C     CHKNUM        BEGSR
148000080702     C*
148100080702     C                   IF        PiDecChr = *blanks
148200080702     C                   EVAL      PiDecChr = ','
148300080702     C                   ENDIF
148400080702     C*
148500080702     C                   callp     UBISNUM_Check(PiStr
148600080702     C                                          :PiDecChr
148700080702     C                                          :PiVal
148800080702     C                                          :PiNum
148900080702     C                                          :PiInt)
149000080702     C*
149100080702     C                   ENDSR
149200080702     C***
149300010601
149400080702
149500010601
149600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149700010601     C     repfil        BEGSR
149800010601     C*
149900010601     C                   if        invfil = *zeros and
150000010601     C                             depfil > *zeros and
150100010629     C                             (vinflg = *blanks or
150200010629     C                              vinflg = *zeros)
150300010601     C
150400010601     C                   eval      invfil = depfil
150500010601     C                   endif
150600010601     C*
150700010601     C                   if        depfil <> invfil and
150800010601     C                             invfil > *zeros
150900010601     C                   eval      flgMulti = '1'
151000010601     C                   if        vinflg = *blanks
151100010601     C                   add       1             cntNonEl
151200010601     C                   endif
151300010601     C                   endif
151400010601     C*
151500010601     C                   if        vinflg = '2'
151600010601     C                   eval      flgStato = '2'
151700010601     C                   endif
151800010601     C*
151900010601     C                   ENDSR
152000010601     C***
152100010601
152200010601
152300010601
152400010330
152500010330
152600010330
152700990920      /TITLE Invio dei dati al punto operativo.
152800000613     C     invio         BEGSR
152900990920     C*
153000990920     C                   reset                   dscmz
153100010601     C                   move      invfil        cmzdst
153200990920     C                   eval      cmzfld = vlrfou
153300990920     C                   eval      cmzmbd = vlrhdl
153400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
153500000710     C                   if        prmfir = *blanks
153600040708     C                   eval      cmzfla = 'FNVAO00F'
153700040708     C                   eval      cmzmba = 'FNVAO00F'
153800000710     C                   else
153900000710     C                   eval      cmzfla = prmfir
154000000710     C                   eval      cmzmba = prmfir
154100000710     C                   endif
154200990920     C                   eval      cmznrr = *zeros
154300990920     C                   move      §ctrok        cmznrr
154400021018     C                   eval      cmzlba = vlrfl1
154500990920     C                   call(e)   'TIS711C'
154600990920     C                   parm                    dscmz
154700990921     C                   parm      *blanks       esito
154800990920     C                   if        %error
154900990920     C                             or cmzerr = '1'
155000990921     C                             or esito  = '1'
155100000710     C                   eval      wrkesito = '3'
155200990920     C                   endif
155300990920     C*
155400000613     C                   ENDSR
155500990910
155600010601
155700010601
155800010601
155900010601
156000010601      /TITLE Invio dei dati al punto operativo.
156100010601     C     opeini        BEGSR
156200010601     C*
156300010601     C* Inizializzo flag e contatori operativi
156400010601     C                   movel     '0'           flgGiro           1
156500010601     C                   movel     '0'           flgMulti          1
156600010601     C                   movel     '1'           flgStato          1
156700010615     C                   movel     '0'           flgOk             1
156800010601     C                   z-add     *zeros        cntNonEl         10 0
156900010601     C                   z-add     *zeros        depfil            3 0
157000010601     C                   z-add     *zeros        invfil            3 0
157100010601     C*
157200010601     C                   ENDSR
157300010601     C***
157400010601
157500010601
157600050421
157700050421
157800050421     C     *pssr         BEGSR
157900050421     C*
158000050421     C                   eval      wrkesito = '2'
158100050421     C*
158200050421     C                   ENDSR     '*CANCL'
158300050421     C***
158400060306
158500060306
158600060306     C*-------------------------------------------------------------------------*
158700060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
158800060306     C*-------------------------------------------------------------------------*
158900060306     C     sr_Control    begsr
159000060306     C*
159100060306     C* inizializzo a ok il flag d controllo abilitazione
159200060306     C                   eval      Cli_Abil = *on
159300060306     C
159400060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
159500060306     C                   clear                   woriIDC
159600060306     C                   eval      kteCOD = 'VAP'
159700060306     C                   eval      kteKE1 = oriIDC
159800060306     C     keytbe        chain     TNTBE01L
159900060306     C                   if        %found(Tntbe01l)
160000060306     C                   eval      woriIDC = TbeKe2
160100060306     C                   else
160200060306     C                   eval      Cli_Abil = *off
160300060306     C                   endif
160400060306     C*
160500060306     C                   endsr
160600050421
160700010601
160800010330
160900010330
161000000613     C     *inzsr        BEGSR
161100990910     C*
161200990910     C     *entry        plist
161300990920     C                   parm                    tivlrds
161400990921     C                   parm      wrkesito      esito
161500000724     C                   parm                    prmlit
161600000710     C                   parm                    prmfir
161700010330     C*
161800010330     C* CALCOLA LA DATA CORRENTE
161900100223     C                   time                    wn14             14 0
162000100223     C                   movel     wn14          wn6               6 0          *ORA (6)
162100100223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
162200100223     C                   eval      datcor = %dec(%date() : *ISO)
162300040723     C*
162400040723     C* Campi riferito al database
162500040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
162600040723     C     *like         define    tbeke1        kteKE1
162700040723     C*
162800040723     C* Definizione chiavi di lettura
162900040723     C* TNTBE01L - parziale
163000040723     C     keytbe        klist
163100040723     C                   kfld                    kteCOD                         *UTENTE
163200040723     C                   kfld                    kteKE1                         *CODICE
163300000613     C*
163400000613     C                   ENDSR
163500000613     C***
163600990908
163700040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
163800080702FILL01;FILL02;VAORSR;FILL03;VAOINR;VAOLOR;VAOCAR;VAOPRR;FILL04;VAOTER;
163900120605W_VAORER_1;FILL05;VAONAM;VAONCL;W_VAODAR;W_VAOORR;VAORSC;VAOINC;VAOLOC;
164000120605VAOCAC;VAOPRC;FILXXX;VAONO1;VAONO2;VAORFA;W_VAORER_2;
164100111109
164200111109
