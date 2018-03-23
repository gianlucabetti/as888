000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200080702     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
005600040723     D*------------------
005700040723     D* DS REPERIMENTO DATI CLIENTE
005800040723     D*-------------------
005900040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006000040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006100040723     D INDDS         E DS                  EXTNAME(CNIND00F)
006200040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006300040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006400080702     D*------------------
006500080702     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006600080702     D*------------------
006700080702     D PiStr           S            100A   INZ
006800080702     D PiDecChr        S              1A   INZ
006900080702     D PiVal           S             63S30 INZ
007000080702     D PiInt           S               N   INZ
007100080702     D PiNum           S               N   INZ
007200080702
007300080702     D*------------------
007400080702     D* LINKING A DEFINIZIONI ESTERNE
007500080702     D*------------------
007600080702     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700091026
007800091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007900091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008000091026
008100080702
008200031201     D*------------------
008300031201     D* Costanti
008400031201     D*------------------
008500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700010330
008800010330
008900010330
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300010601     C*
009400010601     C                   exsr      opeini
009500041011     C                   exsr      PREELA
009600040708     C                   exsr      rwvao
009700010601     C*
009800010601     C                   seton                                        lr
009900010601
010000010601
010100010601
010200010601
010300010601     C*--------------------------------------------------------
010400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010500010601     C*--------------------------------------------------------
010600010601     C     PREELA        BEGSR
010700010601     C*
010800010601     C* SE OCCORRE SPEDIRE IN FILIALE
010900041011     C                   if        flgGiro = '0'
011000010601     C*
011100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011200010601     C                   eval      flgGiro = '1'
011300010601     C*
011400120306     C                   endif
011500120306     C*
011600010601     C                   ENDSR
011700010601     C***
011800010601
011900010601
012000010601
012100010601
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010601     C*--------------------------------------------------------
012800010601     C     ENDELA        BEGSR
012900080707     C*
013000080707     C* Chiusura TRUL13R
013100080707     C                   clear                   TRUL13DS
013200080707     C                   eval      I13TLA = 'C'
013300080707     C                   call      'TRUL13R'
013400080707     C                   parm                    TRUL13DS
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800000613
013900010330
014000010601
014100010601
014200010601
014300010330     C*--------------------------------------------------------
014400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
014500010330     C*--------------------------------------------------------
014600040708     C     RWVAO         BEGSR
014700010330     C*
014800010330     C                   if        not %open(tivin00r)
014900010330     C                   open      tivin00r
015000010330     C                   endif
015100040708     C                   if        not %open(fnvaowwr)
015200040708     C                   open      fnvaowwr
015300010330     C                   endif
015400010330     C*
015500010604     C                   clear                   §CTROK
015600010604     C                   clear                   §CTRMO
015700010604     C                   clear                   §CTRNO
015800010330     C*
015900030822     C                   do        *HIVAL
016000010330     C*
016100030822     C                   read      tivin00r                               70
016200010618     C*
016300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016400010618     C                   if        vinflg = '1'
016500010618     C                   eval      flgOk = '1'
016600010618     C                   endif
016700010618     C*
016800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016900040510     C                   z-add     *zeros        wLenVINDTA        4 0
017000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017400040510     C                   z-add     1             wFlgVINDTA
017500040510     C                   leave
017600040510     C                   endif
017700040510     C                   enddo
017800040510     C*
017900040218     C                   if        vindta > *blanks AND
018000040510     C                             wFlgVINDTA = 1
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200010601     C*
019300010330     C                   exsr      inzvar
019400040708     C                   exsr      impvao
019500100512     C                   exsr      exeforz
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800040708     C                   eval      depfil = VAOPOE
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200040708     C                   move(P)   invfil        VAOPOE
020300021025     C                   else
020400040708     C                   move(P)   vlrpoi        VAOPOE
020500021025     C                   endif
020600010601     C*
020700010604     C* Ebbene...
020800010604     C*
020900030822     C  N31              add       1             §CTROK            7 0
021000030822     C   32              add       1             §CTRMO            7 0
021100030822     C   31              add       1             §CTRNO            7 0
021200030822     C                   if        wGiro = 2
021300040723     C  N31              EXSR      CHKCOR
021400161221     C                   if        not *IN50
021500040723     C  N31              EXSR      WRIORI
021600050826     C  N31              EXSR      WRIVGD
021700161221     C                   endif
021800040723     C  N31              write     FNVAO000
021900030822     C                   endif
022000010604     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700030822     C  N70              enddo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700040708     C                   if        %open(fnvaowwr)
026800040708     C                   close     fnvaowwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600040723
028700040723
028800040723     C*------------------------------------------------------------------------*
028900040723     C* CHKCOR - Considerazioni su VAOCOR
029000040723     C*------------------------------------------------------------------------*
029100040723     C     CHKCOR        BEGSR
029200040723     C*
029300040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
029400040723     C                   if        vaoTCO = 'F'
029500040723     C* Quindi verifico se il codice ordinante non è valorizzato
029600040723     C                   if        vaoCOR = *zeros  AND
029700040723     C                             vaoRSO = *blanks AND
029800040723     C                             vaoINO = *blanks AND
029900040723     C                             vaoCAO = *blanks AND
030000040723     C                             vaoLOO = *blanks AND
030100040723     C                             vaoPRO = *blanks AND
030200040723     C                             vaoNAO = *blanks
030300040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
030400040723     C                   movel     '0'           wFlgAcr           1
030500040723     C                   move      vlrKSC        wCli              7 0
030600040723     C                   move(p)   wCli          acrCRO
030700040723     C     acrCRO        setll     fnacr01l
030800040723     C                   if        %found(fnacr01l)
030900040723     C                   read      fnacr01l
031000040723     C                   dow       not %eof(fnacr01l)
031100040723     C                   movel     acrCRO        wCliAcr           7 0
031200040723     C                   if        wCliAcr = wCli
031300040723     C                   eval      vaoCOR = acrCRO
031400040723     C                   movel     '1'           wFlgAcr
031500040723     C                   leave
031600040723     C                   else
031700040723     C                   leave
031800040723     C                   endif
031900040723     C                   read      fnacr01l
032000040723     C                   enddo
032100040723     C                   endif
032200040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
032300040723     C* dati anagrafici dell'ordinante dal piano dei conti
032400040723     C                   if        wFlgAcr = '0'
032500040723     C                   clear                   BS69DS
032600040723     C                   clear                   ACODS
032700040723     C                   clear                   INDDS
032800040723     C                   clear                   CLPDS
032900040723     C                   clear                   CLSDS
033000040723     C                   move(P)   vlrKSC        I69KAC
033100040723     C                   move(P)   vlrKSC        I69KIN
033200040723     C                   call      'TIBS69R'
033300040723     C                   parm                    BS69DS
033400040723     C                   parm                    ACODS
033500040723     C                   parm                    INDDS
033600040723     C                   parm                    CLPDS
033700040723     C                   parm                    CLSDS
033800040723     C     O69ERR        ifne      '1'
033900040723     C                   eval      vaoRSO = ACORAG
034000040723     C                   eval      vaoINO = INDVIA
034100040723     C                   movel(P)  INDCAP        vaoCAO
034200040723     C                   eval      vaoLOO = INDCIT
034300040723     C                   eval      vaoPRO = INDPRV
034400040723     C                   eval      vaoNAO = INDSTA
034500040723     C                   endif
034600040723     C                   endif
034700040723     C                   endif
034800040723     C                   endif
034900040723     C*
035000040723     C                   ENDSR
035100040723
035200040723
035300040723
035400040723     C*------------------------------------------------------------------------*
035500040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
035600040723     C*------------------------------------------------------------------------*
035700040723     C     WRIORI        BEGSR
035800040723     C*
035900090617     C* Come prima cosa stacco un numeratore da AZNUM
036000090617     C                   clear                   trul33ds
036100090617     C                   eval      I33OPE = *zeros
036200090617     C                   eval      I33CNU = 600
036300090617     C                   eval      I33NUM = 1
036400090617     C                   movel     TRUL33DS      KPJBU
036500090617     C                   call      'TRUL33R'
036600090617     C                   parm                    KPJBA
036700090617     C                   movel     KPJBU         TRUL33DS
036800090617     C                   if        O33ERR = *zeros
036900090617     C                   z-add     O33NRF        §numori
037000090618     c                   else
037100090618     c                   eval      §numori = *all'9'
037200090617     C                   endif
037300040723     C*
037400040723     C* Quindi imposto i campi "extra-VAO"
037500040723     C                   if        vlrKSC = '0ORM0000'
037600040723     C*
037700040723     C                   if        vaoCOR > *zeros
037800040723     C                   movel     vaoCOR        wrkCOR7           7 0
037900040723     C*
038000040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
038100040723     C* codice padre. Se è un padre assegno già il codice ordinante.
038200040723     C                   clear                   tibs10ds
038300040723     C                   eval      d10drf = datcor                              *data riferimento
038400040723     C                   eval      d10paf = 'P'                                 *cerca il padre
038500040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
038600091026     C*
038700091026     C* Preparazione chiamata al TIBS10R1
038800091026     C                   clear                   tibs10r1ds
038900091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
039000091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
039100091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
039200091026     C*
039300091026     C                   call      'TIBS10R1'
039400091026     C                   parm                    tibs10r1ds
039500091026     C*
039600091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
039700040723     C*
039800040723     C                   if        d10err <> *blanks or                         *No unif. o padre
039900040723     C                             d10cop =  *zeros  or
040000040723     C                             d10cop =  d10cod
040100040723     C                   move      d10cod        oriIDC
040200040723     C                   else                                                   *Figlio
040300040723     C                   move      d10cop        oriIDC
040400040723     C                   endif
040500040723     C*
040600040723     C                   else
040700040723     C                   eval      oriIDC = *zeros
040800040723     C                   endif
040900040723     C*
041000040723     C                   else
041100040723     C                   eval      oriIDC = vlrKSC
041200040723     C                   endif
041300040723     C*
041400040723     C                   eval      oriPRG = §numori
041500040723     C                   eval      oriDER = datcor
041600040723     C                   eval      oriOER = wn6
041700040723     C                   eval      oriPOE = vaoPOE
041800040723     C                   eval      oriNSR = vaoNSR
041900040723     C                   eval      oriNOR = vaoNOR
042000040723     C                   eval      oriNRV = vaoNRV
042100040723     C                   eval      oriFLO = *blanks
042200040723     C*
042300040723     C* Infine valorizzo la chiave esterna sul file FNVAO
042400040723     C                   eval      dorm01 = vaoFLO
042500040723     C                   movel     §numori       §ormpg
042600050412     C*
042700050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
042800050412     C                   movel     'S'           §ormfr
042900050412     C*
043000050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
043100040723     C                   eval      vaoFLO = dorm01
043200040723     C*
043300040723     C* ...quindi scrivo il file TIORI00F
043400040723     C                   WRITE     TIORI000
043500040723     C*
043600040723     C                   ENDSR
043700050826
043800050826
043900050826
044000050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
044100050826     C     wriVGD        BEGSR
044200050826     C*
044300050826     C* Reperisco la descrizione della fase da tabella
044400050826     C                   movel(p)  'FAR'         KteCOD
044500050826     C                   movel(p)  '000'         KteKE1
044600050826     C     KEYtbe        chain     tntbe01l
044700050826     C                   if        %found(tntbe01l)
044800050826     C                   movel(P)  tbeuni        DFAR
044900050826     C                   else
045000050826     C                   clear                   DFAR
045100050826     C                   endif
045200050826     C*
045300050826     C* Valorizzo buffer record
045400050826     C                   clear                   FNVAPDS
045500050826     C                   eval      vapIDC = oriIDC
045600050826     C                   eval      vapPOE = oriPOE
045700050826     C                   eval      vapRFA = vaoRFA
045800050826     C                   eval      vapPOG = oriPOE
045900050826     C                   eval      vapDAE = oriDER
046000050826     C                   eval      vapORE = oriOER
046100050826     C                   if        d§fardva <> *blanks
046200050826     C                   eval      vapDFA = d§fardva
046300050826     C                   else
046400050826     C                   eval      vapDFA = d§fardes
046500050826     C                   endif
046600050826     C*
046700050826     C                   clear                   tivgd000
046800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
046900050826     C                   eval      vgdTIP = 'VP'
047000050826     C                   eval      vgdKSU = vapIDC
047100050826     C                   eval      vgdTSC = 'WW'
047200050826     C                   eval      vgdDAT = datcor
047300161221     C                   eval      vgdPGM = 'TITV2W9R'
047400060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
047500060306     C                   exsr      sr_Control
047600060306     C                   if        Cli_Abil = *on
047700060306     C                   eval      vgdKSU = woriIDC
047800050826     C                   write     tivgd000
047900060306     C                   endif
048000050826     C*
048100050826     C                   ENDSR
048200050826     C*------------------------------------------------------------------------*
048300010601
048400010601
048500010601
048600010330     C*----------------------------------------------------*
048700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048800010330     C*----------------------------------------------------*
048900010330     C     INZVAR        BEGSR
049000010330     C*
049100030822     C* Inizializzo variabili di wrk
049200020204     C                   Z-ADD     *zeros        Num5_0            5 0
049300030822     C*
049400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049500040708     C                   CLEAR                   FNVAO000
049600040723     C                   CLEAR                   TIORI000
049700030822     C                   CLEAR                   SkSplitCSV
049800030822     C                   EVAL      i = 1
049900030822     C                   EVAL      posDa = *zeros
050000030822     C                   EVAL      posA  = *zeros
050100030822     C*
050200030822     C* Reimposto i valori di default
050300030822     C                   EXSR      DEFCAM
050400030822     C*
050500010330     C                   ENDSR
050600010330     C*----------------------------------------------------*
050700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050800010330     C*----------------------------------------------------*
050900010330     C     DEFCAM        BEGSR
051000030822     C*
051100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051200030715     C* e delimitatore testo.
051300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051600030822     C*
051700161221     C* Reperisco il flag che mi indica se lavorare in "TEST"
051800161221     C                   IF        %subst(vlrppt:1:1) = 'T'
051900030715     C                   SETON                                        50
052000030715     C                   ELSE
052100030715     C                   SETOFF                                       50
052200030715     C                   ENDIF
052300040723     C*
052400080707     C* Imposto i valori d default d alcuni campi
052500161221     C                   EVAL      VAOCOR = 0058494000
052600161221     C                   EVAL      VAOKSC = 0058494
052700161221     C                   EVAL      VAOCTR = '301'
052800161221     C                   EVAL      VAOPOE = 005
052900161221     C*
053000161221     C                   EVAL      VAOTCO = 'F'
053100161221     C                   EVAL      VAOTOR = 'S'
053200161221     C                   EVAL      VAOPAG = 'O'
053300161221     C                   EVAL      VAOFLO = 'NSSSNNNS'
053400030822     C*
053500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053600030822     C                   EVAL      posDaDft = 1
053700030822     C                   EVAL      posADft  = 0
053800030822     C                   EVAL      wGiroDft = 0
053900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054000030822     C                             posDaDft > 0
054100030822     C*
054200030822     C* Gestisco il 1° giro
054300030822     C                   IF        wGiroDft = 0
054400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054600030822     C* Incremento il contatore dei "giri"
054700030822     C                   EVAL      wGiroDft = 1
054800030822     C                   ELSE
054900030822     C                   EVAL      posDaDft = posADft
055000030822     C                   ENDIF
055100030822     C* Eseguo lo scan x trovare la fine del campo corrente
055200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055300030822     C*
055400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055600030822     C                   IF        posDaDft > 0 AND
055700030822     C                             posADft  > 0
055800040708     C* POE
055900040708     C                   IF        %subst(
056000040708     C                             %subst(vlrppt:posDaDft+1:
056100040708     C                             posADft-posDaDft-1):1:3)
056200040708     C                             = 'POE'
056300040708     C                   EVAL      PiStr=%trim(%subst(
056400040708     C                             %subst(vlrppt:posDaDft+1:
056500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600040708     C                   EXSR      CHKNUM
056700040708     C                   IF        PiInt=*on
056800040708     C                   Z-ADD     PiVal         VAOPOE
056900040708     C                   ENDIF
057000040708     C                   ENDIF
057100040708     C* TOR
057200040708     C                   IF        %subst(
057300040708     C                             %subst(vlrppt:posDaDft+1:
057400040708     C                             posADft-posDaDft-1):1:3)
057500040708     C                             = 'TOR'
057600040708     C                   EVAL      VAOTOR=%trim(%subst(
057700040708     C                             %subst(vlrppt:posDaDft+1:
057800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900040708     C                   ENDIF
058000040708     C* TCO
058100040708     C                   IF        %subst(
058200040708     C                             %subst(vlrppt:posDaDft+1:
058300040708     C                             posADft-posDaDft-1):1:3)
058400040708     C                             = 'TCO'
058500040708     C                   EVAL      VAOTCO=%trim(%subst(
058600040708     C                             %subst(vlrppt:posDaDft+1:
058700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800040708     C                   ENDIF
058900040707     C* COR
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300040707     C                             = 'COR'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900040708     C                   Z-ADD     PiVal         VAOCOR
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200040708     C* CRC
060300040708     C                   IF        %subst(
060400040708     C                             %subst(vlrppt:posDaDft+1:
060500040708     C                             posADft-posDaDft-1):1:3)
060600040708     C                             = 'CRC'
060700040708     C                   EVAL      PiStr=%trim(%subst(
060800040708     C                             %subst(vlrppt:posDaDft+1:
060900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000040708     C                   EXSR      CHKNUM
061100040708     C                   IF        PiInt=*on
061200040708     C                   Z-ADD     PiVal         VAOCRC
061300040708     C                   ENDIF
061400040708     C                   ENDIF
061500040708     C* FFD
061600040708     C                   IF        %subst(
061700040708     C                             %subst(vlrppt:posDaDft+1:
061800040708     C                             posADft-posDaDft-1):1:3)
061900040708     C                             = 'FFD'
062000040708     C                   EVAL      VAOFFD=%trim(%subst(
062100040708     C                             %subst(vlrppt:posDaDft+1:
062200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300040708     C                   ENDIF
062400040708     C* ORR
062500040708     C                   IF        %subst(
062600040708     C                             %subst(vlrppt:posDaDft+1:
062700040708     C                             posADft-posDaDft-1):1:3)
062800040708     C                             = 'ORR'
062900040708     C                   EVAL      PiStr=%trim(%subst(
063000040708     C                             %subst(vlrppt:posDaDft+1:
063100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200040708     C                   EXSR      CHKNUM
063300040708     C                   IF        PiInt=*on
063400040708     C                   Z-ADD     PiVal         VAOORR
063500040708     C                   ENDIF
063600040708     C                   ENDIF
063700040712     C* NCL
063800040712     C                   IF        %subst(
063900040712     C                             %subst(vlrppt:posDaDft+1:
064000040712     C                             posADft-posDaDft-1):1:3)
064100040712     C                             = 'NCL'
064200040712     C                   EVAL      PiStr=%trim(%subst(
064300040712     C                             %subst(vlrppt:posDaDft+1:
064400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500040712     C                   EXSR      CHKNUM
064600040712     C                   IF        PiInt=*on
064700040712     C                   Z-ADD     PiVal         VAONCL
064800040712     C                   ENDIF
064900040712     C                   ENDIF
065000040712     C* PKG
065100040712     C                   IF        %subst(
065200040712     C                             %subst(vlrppt:posDaDft+1:
065300040712     C                             posADft-posDaDft-1):1:3)
065400040712     C                             = 'PKG'
065500040712     C                   EVAL      PiStr=%trim(%subst(
065600040712     C                             %subst(vlrppt:posDaDft+1:
065700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800040712     C                   EXSR      CHKNUM
065900040712     C                   IF        PiNum=*on
066000040712     C                   Z-ADD     PiVal         VAOPKG
066100040712     C                   ENDIF
066200040712     C                   ENDIF
066300040712     C* VLM
066400040712     C                   IF        %subst(
066500040712     C                             %subst(vlrppt:posDaDft+1:
066600040712     C                             posADft-posDaDft-1):1:3)
066700040712     C                             = 'VLM'
066800040712     C                   EVAL      PiStr=%trim(%subst(
066900040712     C                             %subst(vlrppt:posDaDft+1:
067000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100040712     C                   EXSR      CHKNUM
067200040712     C                   IF        PiNum=*on
067300040712     C                   Z-ADD     PiVal         VAOVLM
067400040712     C                   ENDIF
067500040712     C                   ENDIF
067600040708     C* NAM
067700040708     C                   IF        %subst(
067800040708     C                             %subst(vlrppt:posDaDft+1:
067900040708     C                             posADft-posDaDft-1):1:3)
068000040708     C                             = 'NAM'
068100040708     C                   EVAL      VAONAM=%trim(%subst(
068200040708     C                             %subst(vlrppt:posDaDft+1:
068300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400040708     C                   ENDIF
068500040708     C* PAG
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900040708     C                             = 'PAG'
069000040708     C                   EVAL      VAOPAG=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400040708     C* KSC
069500040708     C                   IF        %subst(
069600040708     C                             %subst(vlrppt:posDaDft+1:
069700040708     C                             posADft-posDaDft-1):1:3)
069800040708     C                             = 'KSC'
069900040708     C                   EVAL      PiStr=%trim(%subst(
070000040708     C                             %subst(vlrppt:posDaDft+1:
070100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200040708     C                   EXSR      CHKNUM
070300040708     C                   IF        PiInt=*on
070400040708     C                   Z-ADD     PiVal         VAOKSC
070500040708     C                   ENDIF
070600040708     C                   ENDIF
070700040708     C* CTR
070800040708     C                   IF        %subst(
070900040708     C                             %subst(vlrppt:posDaDft+1:
071000040708     C                             posADft-posDaDft-1):1:3)
071100040708     C                             = 'CTR'
071200040708     C                   EVAL      VAOCTR=%trim(%subst(
071300040708     C                             %subst(vlrppt:posDaDft+1:
071400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500040708     C                   ENDIF
071600040708     C* POC
071700040708     C                   IF        %subst(
071800040708     C                             %subst(vlrppt:posDaDft+1:
071900040708     C                             posADft-posDaDft-1):1:3)
072000040708     C                             = 'POC'
072100040708     C                   EVAL      PiStr=%trim(%subst(
072200040708     C                             %subst(vlrppt:posDaDft+1:
072300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400040708     C                   EXSR      CHKNUM
072500040708     C                   IF        PiInt=*on
072600040708     C                   Z-ADD     PiVal         VAOPOC
072700040708     C                   ENDIF
072800040708     C                   ENDIF
072900040708     C* NO1
073000040708     C                   IF        %subst(
073100040708     C                             %subst(vlrppt:posDaDft+1:
073200040708     C                             posADft-posDaDft-1):1:3)
073300040708     C                             = 'NO1'
073400040708     C                   EVAL      VAONO1=%trim(%subst(
073500040708     C                             %subst(vlrppt:posDaDft+1:
073600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700040708     C                   ENDIF
073800040708     C* DDT
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200040708     C                             = 'DDT'
074300040708     C                   EVAL      VAODDT=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700040708     C* RFA
074800040708     C                   IF        %subst(
074900040708     C                             %subst(vlrppt:posDaDft+1:
075000040708     C                             posADft-posDaDft-1):1:3)
075100040708     C                             = 'RFA'
075200040708     C                   EVAL      VAORFA=%trim(%subst(
075300040708     C                             %subst(vlrppt:posDaDft+1:
075400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040708     C                   ENDIF
075600040708     C* FLO
075700040708     C                   IF        %subst(
075800040708     C                             %subst(vlrppt:posDaDft+1:
075900040708     C                             posADft-posDaDft-1):1:3)
076000040708     C                             = 'FLO'
076100040708     C                   EVAL      VAOFLO=%trim(%subst(
076200040708     C                             %subst(vlrppt:posDaDft+1:
076300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
076500080702     C* e posizione 7 del VAOFLO
076600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
076700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
076800050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
076900040708     C                   ENDIF
077000030822     C* ...
077100030822     C                   ENDIF
077200030822     C                   ENDDO
077300020204     C*
077400010330     C                   ENDSR
077500010607     C*----------------------------------------------------*
077600040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
077700010607     C*----------------------------------------------------*
077800040708     C     IMPVAO        BEGSR
077900010607     C*
078000010607     C                   SETOFF                                       3132
078100030515     C*
078200100223     C                   MOVEL     datcor        VAODAO
078300040708     C                   MOVEL     wn14          VAOOAO
078400050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078500050314     C     minu:maiu     XLATE     vindta        vindta
078600010607     C*
078700030715     C**********
078800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078900030715     C**********
079000030715     C                   IF        wGiro = *zeros
079100040709     C*
079200040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
079300040709     C                   EVAL      depvinDTA = vinDTA
079400040709     C*
079500040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
079600040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079700110616     C                                      %trim(intcol(3))+%trim(intcol(4))+
079800110616     C                                      %trim(intcol(5))
079900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080000050314     C**!!!minu:maiu     XLATE     vindta        vindta
080100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080200040324     C                   DOW       posDa <= %len(%trimr(vindta))
080300030715     C*
080400030715     C* Gestisco il 1° campo
080500030715     C                   IF        i = 1
080600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080700030715     C                   EVAL      posDa = 1
080800030715     C* Eseguo lo scan x trovare la fine del primo campo
080900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C* X i campi successivi al 1°
081400030715     C                   ELSE
081500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081600030715     C                   EVAL      posDa = posA + 1
081700030715     C* Eseguo lo scan x trovare la fine del campo corrente
081800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* Gestisco l'ultimo campo
082000030715     C                   IF        posA = *zeros
082100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C                   LEAVE
082600030715     C                   ELSE
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   ENDIF
083100030715     C                   ENDIF
083200030715     C* Incremento il contatore d campo
083300030715     C                   EVAL      i = i +1
083400030715     C                   ENDDO
083500030715     C*
083600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083700030715     C* delimitazione testo
083800030715     C                   EVAL      i = 1
083900030715     C                   DOW       i < %elem(SkSplitFLD)
084000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084200030715     C                   EVAL      i = i + 1
084300030715     C                   ENDDO
084400030715     C                   EVAL      wGiro = 1
084500040709     C* Ripristino la 1° riga originale
084600040709     C                   EVAL      vinDTA = depvinDTA
084700030715     C                   ELSE
084800030715     C                   EVAL      wGiro = 2
084900031204     C**********
085000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085100031204     C* d separatore campo
085200031204     C**********
085300040709     C                   EVAL      vindta = %trim(vindta)
085400040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085500040709     C                   EVAL      vindta = %subst(vindta:2)
085600040709     C                   ENDDO
085700040709     C*
085800040709     C                   Z-ADD     *zeros        lunghInput        4 0
085900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086000040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086100040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086200040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086300040709     C                   ENDDO
086400030715     C**********
086500030715     C* Effettuo lo split del campo dati d input
086600030715     C**********
086700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086800040324     C                   DOW       posDa <= %len(%trimr(vindta))
086900030715     C*
087000030715     C* Gestisco il 1° campo
087100030715     C                   IF        i = 1
087200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087300030715     C                   EVAL      posDa = 1
087400030715     C* Eseguo lo scan x trovare la fine del primo campo
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C* X i campi successivi al 1°
088000030715     C                   ELSE
088100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088200030715     C                   EVAL      posDa = posA + 1
088300031204     C* Verifico che nn vi sia il campo nn valorizzato
088400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088500031204     C* Se campo nn valorizzato skippo al prossimo
088600031204     C                   EVAL      SkSplitCSV(i) = *blanks
088700031204     C                   EVAL      posA  = posA + 1
088800031204     C                   ELSE
088900030715     C* Eseguo lo scan x trovare la fine del campo corrente
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* Gestisco l'ultimo campo
089200030715     C                   IF        posA = *zeros
089300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C                   LEAVE
089800030715     C                   ELSE
089900030715     C* A questo punto "estraggo" il campo corrente
090000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090100030715     C                                                    (posA-posDa))
090200030715     C                   ENDIF
090300031204     C                   ENDIF
090400030715     C                   ENDIF
090500030715     C* Incremento il contatore d campo
090600030715     C                   EVAL      i = i +1
090700030715     C                   ENDDO
090800030715     C*
090900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091000030715     C* delimitazione testo
091100030715     C                   EVAL      i = 1
091200030715     C                   DOW       i < %elem(SkSplitCSV)
091300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091500030715     C                   EVAL      i = i + 1
091600030715     C                   ENDDO
091700030715     C*
091800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091900030715     C                   EVAL      i = 1
092000030715     C                   DOW       i < %elem(SkSplitFLD)
092100030715     C*
092200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092400030715     C                             SkSplitCSV(i) <> *blanks
092500030721     C*** ATB
092600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
092700040708     C                   EVAL      VAOATB = SkSplitCSV(i)
092800030721     C                   ENDIF
092900040708     C*** TOR
093000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
093100040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
093200040708     C                   ENDIF
093300040708     C*** TCO
093400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
093500040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
093600040708     C                   ENDIF
093700040708     C*** RSO
093800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
093900040708     C                   EVAL      VAORSO = SkSplitCSV(i)
094000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094100040708     C     '@':'A'       XLATE     VAORSO        VAORSO
094200030715     C* ==
094300030715     C                   ENDIF
094400040708     C*** INO
094500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
094600040708     C                   EVAL      VAOINO = SkSplitCSV(i)
094700030715     C                   ENDIF
094800040708     C*** LOO
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
095000040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
095100030715     C                   ENDIF
095200040708     C*** PRO
095300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
095400040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
095500030715     C                   ENDIF
095600040708     C*** NAO
095700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
095800040708     C                   EVAL      VAONAO = SkSplitCSV(i)
095900030721     C                   ENDIF
096000040708     C*** RSR
096100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
096200040708     C                   EVAL      VAORSR = SkSplitCSV(i)
096300040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096400040708     C     '@':'A'       XLATE     VAORSR        VAORSR
096500040708     C* ==
096600040708     C                   IF        VAORSR = *blanks
096700040708     C                   SETON                                        31
096800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
096900040708     C                             + ' ' + 'VAORSR'
097000040708     C                   ENDIF
097100040708     C                   ENDIF
097200040708     C*** INR
097300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
097400040708     C                   EVAL      VAOINR = SkSplitCSV(i)
097500040708     C                   IF        VAOINR = *blanks
097600040708     C                   SETON                                        31
097700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097800040708     C                             + ' ' + 'VAOINR'
097900040708     C                   ENDIF
098000040708     C                   ENDIF
098100040708     C*** LOR
098200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
098300040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
098400040708     C                   IF        VAOLOR = *blanks
098500040708     C                   SETON                                        31
098600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098700040708     C                             + ' ' + 'VAOLOR'
098800040708     C                   ENDIF
098900040708     C                   ENDIF
099000040708     C*** PRR
099100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
099200040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
099300040708     C                   IF        VAOPRR = *blanks
099400040708     C                   SETON                                        31
099500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099600040708     C                             + ' ' + 'VAOPRR'
099700040708     C                   ENDIF
099800040708     C                   ENDIF
099900040708     C*** NAR
100000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
100100040708     C                   EVAL      VAONAR = SkSplitCSV(i)
100200040708     C                   ENDIF
100300040708     C*** RER
100400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
100500040708     C                   EVAL      VAORER = SkSplitCSV(i)
100600040708     C                   ENDIF
100700040708     C*** TER
100800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
100900040708     C                   EVAL      VAOTER = SkSplitCSV(i)
101000040708     C                   ENDIF
101100040708     C*** RSC
101200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
101300040708     C                   EVAL      VAORSC = SkSplitCSV(i)
101400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101500040708     C     '@':'A'       XLATE     VAORSC        VAORSC
101600040708     C* ==
101700040708     C                   ENDIF
101800040708     C*** INC
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
102000040708     C                   EVAL      VAOINC = SkSplitCSV(i)
102100040708     C                   ENDIF
102200040708     C*** LOC
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
102400040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
102500040708     C                   ENDIF
102600040708     C*** PRC
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
102800040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
102900040708     C                   ENDIF
103000040708     C*** NAC
103100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
103200040708     C                   EVAL      VAONAC = SkSplitCSV(i)
103300040708     C                   ENDIF
103400040708     C*** FFD
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
103600040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
103700040708     C                   ENDIF
103800040708     C*** RMP
103900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
104000040708     C                   EVAL      VAORMP = SkSplitCSV(i)
104100040708     C                   ENDIF
104200040708     C*** NAM
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
104400040708     C                   EVAL      VAONAM = SkSplitCSV(i)
104500040708     C                   ENDIF
104600040708     C*** PAG
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
104800040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
104900040708     C                   ENDIF
105000040708     C*** CTR
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
105200040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
105300040708     C                   ENDIF
105400040708     C*** ZOR
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
105600040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
105700040708     C                   ENDIF
105800040708     C*** NO1
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
106000040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
106100030715     C                   ENDIF
106200040708     C*** NO2
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
106400040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
106500030715     C                   ENDIF
106600040708     C*** DDT
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
106800040708     C                   EVAL      VAODDT = SkSplitCSV(i)
106900030715     C                   ENDIF
107000040708     C*** RFA
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
107200040708     C                   EVAL      VAORFA = SkSplitCSV(i)
107300030715     C                   ENDIF
107400040708     C*** SPI
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
107600040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
107700030715     C                   ENDIF
107800040708     C*** FLO
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
108000040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
108100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
108200080708     C* e la posizione 7 del VAOFLO
108300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
108400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
108500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
108600030715     C                   ENDIF
108700050314
108800050412      *** DAR
108900050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
109000050412     C                   EVAL      PiStr=SkSplitCSV(i)
109100050412     C                   EXSR      CHKNUM
109200050412     C                   IF        PiInt=*on
109300050412     C                   Z-ADD     PiVal         VAODAR
109400050412     C                   ELSE
109500050412     C                   SETON                                        32
109600050412     C                   EVAL      VAODAR = *zeros
109700050412     C                   EVAL      vinmsg = %trimr(vinmsg)
109800050412     C                             + ' ' + 'VAODAR'
109900050412     C                   ENDIF
110000050314     C                   ENDIF
110100030715     C
110200030721     C*
110300030721     C* Reperisco quindi i campi numerici...
110400040708     C*** POE
110500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
110600040723     C                   EVAL      PiStr=SkSplitCSV(i)
110700040723     C                   EXSR      CHKNUM
110800040723     C                   IF        PiInt=*on    AND
110900040723     C                             PiVal<=999   AND
111000040723     C                             PiVal>*zeros
111100040723     C                   Z-ADD     PiVal         VAOPOE
111200040723     C                   ELSE
111300040723     C                   SETON                                        32
111400040723     C                   EVAL      VAOPOE = *zeros
111500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040723     C                             + ' ' + 'VAOPOE'
111700040723     C                   ENDIF
111800040723     C                   ENDIF
111900040708     C*** COR
112000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
112100040708     C                   EVAL      PiStr=SkSplitCSV(i)
112200040708     C                   EXSR      CHKNUM
112300040708     C                   IF        PiInt=*on
112400040708     C                   Z-ADD     PiVal         VAOCOR
112500040708     C                   ELSE
112600040708     C                   SETON                                        31
112700040708     C                   EVAL      VAOCOR = *zeros
112800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040708     C                             + ' ' + 'VAOCOR'
113000040708     C                   ENDIF
113100040708     C                   ENDIF
113200040708     C*** CAO
113300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700030721     C                   Z-ADD     PiVal         Num5_0
113800040708     C                   MOVEL(p)  Num5_0        VAOCAO
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100040708     C                   EVAL      VAOCAO = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040708     C                             + ' ' + 'VAOCAO'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600040708     C*** CRA
114700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on
115100040708     C                   Z-ADD     PiVal         VAOCRA
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400040708     C                   EVAL      VAOCRA = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040708     C                             + ' ' + 'VAOCRA'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900040708     C*** CAR
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
116100040708     C                   EVAL      PiStr=SkSplitCSV(i)
116200040708     C                   EXSR      CHKNUM
116300040708     C                   IF        PiInt=*on
116400040708     C                   Z-ADD     PiVal         Num5_0
116500040708     C                   MOVEL(p)  Num5_0        VAOCAR
116600040708     C                   ELSE
116700040708     C                   SETON                                        31
116800040708     C                   EVAL      VAOCAR = *zeros
116900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117000040708     C                             + ' ' + 'VAOCAR'
117100040708     C                   ENDIF
117200040708     C                   ENDIF
117300040708     C*** CRC
117400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
117500040708     C                   EVAL      PiStr=SkSplitCSV(i)
117600040708     C                   EXSR      CHKNUM
117700040708     C                   IF        PiInt=*on
117800040708     C                   Z-ADD     PiVal         VAOCRC
117900040708     C                   ELSE
118000040708     C                   SETON                                        32
118100040708     C                   EVAL      VAOCRC = *zeros
118200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040708     C                             + ' ' + 'VAOCRC'
118400040708     C                   ENDIF
118500040708     C                   ENDIF
118600040708     C*** CAC
118700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
118800040708     C                   EVAL      PiStr=SkSplitCSV(i)
118900040708     C                   EXSR      CHKNUM
119000040708     C                   IF        PiInt=*on
119100040708     C                   Z-ADD     PiVal         Num5_0
119200040708     C                   MOVEL(p)  Num5_0        VAOCAC
119300040708     C                   ELSE
119400040708     C                   SETON                                        32
119500040708     C                   EVAL      VAOCAC = *zeros
119600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119700040708     C                             + ' ' + 'VAOCAC'
119800040708     C                   ENDIF
119900040708     C                   ENDIF
120000040708     C*** DAR
120100050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
120200050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
120300050314     C**!!!              EXSR      CHKNUM
120400050314     C**!!!              IF        PiInt=*on
120500050314     C**!!!              Z-ADD     PiVal         VAODAR
120600050314     C**!!!              ELSE
120700050314     C**!!!              SETON                                        32
120800050314     C**!!!              EVAL      VAODAR = *zeros
120900050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
121000050314     C**!!!                        + ' ' + 'VAODAR'
121100050314     C**!!!              ENDIF
121200050314     C**!!!              ENDIF
121300040708     C*** ORR
121400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
121500040708     C                   EVAL      PiStr=SkSplitCSV(i)
121600040708     C                   EXSR      CHKNUM
121700040708     C                   IF        PiInt=*on
121800040708     C                   Z-ADD     PiVal         VAOORR
121900040708     C                   ELSE
122000040708     C                   SETON                                        32
122100040708     C                   EVAL      VAOORR = *zeros
122200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122300040708     C                             + ' ' + 'VAOORR'
122400040708     C                   ENDIF
122500040708     C                   ENDIF
122600030721     C*** NCL
122700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on
123100040723     C                   IF        PiVal > *zeros
123200040708     C                   Z-ADD     PiVal         VAONCL
123300040723     C                   ENDIF
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600040708     C                   EVAL      VAONCL = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800040708     C                             + ' ' + 'VAONCL'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100040708     C*** PKG
124200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiNum=*on
124600040723     C                   IF        PiVal > *zeros
124700040708     C                   Z-ADD     PiVal         VAOPKG
124800040723     C                   ENDIF
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100040708     C                   EVAL      VAOPKG = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040708     C                             + ' ' + 'VAOPKG'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600040708     C*** VLM
125700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
125800040708     C                   EVAL      PiStr=SkSplitCSV(i)
125900040708     C                   EXSR      CHKNUM
126000040708     C                   IF        PiNum=*on
126100040708     C                   Z-ADD     PiVal         VAOVLM
126200040708     C                   ELSE
126300040708     C                   SETON                                        32
126400040708     C                   EVAL      VAOVLM = *zeros
126500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126600040708     C                             + ' ' + 'VAOVLM'
126700040708     C                   ENDIF
126800040708     C                   ENDIF
126900040708     C*** BNC
127000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
127100040708     C                   EVAL      PiStr=SkSplitCSV(i)
127200040708     C                   EXSR      CHKNUM
127300040708     C                   IF        PiInt=*on
127400040708     C                   Z-ADD     PiVal         VAOBNC
127500040708     C                   ELSE
127600040708     C                   SETON                                        32
127700040708     C                   EVAL      VAOBNC = *zeros
127800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127900040708     C                             + ' ' + 'VAOBNC'
128000040708     C                   ENDIF
128100040708     C                   ENDIF
128200040708     C*** BLC
128300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
128400040708     C                   EVAL      PiStr=SkSplitCSV(i)
128500040708     C                   EXSR      CHKNUM
128600040708     C                   IF        PiInt=*on
128700040708     C                   Z-ADD     PiVal         VAOBLC
128800040708     C                   ELSE
128900040708     C                   SETON                                        32
129000040708     C                   EVAL      VAOBLC = *zeros
129100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129200040708     C                             + ' ' + 'VAOBLC'
129300040708     C                   ENDIF
129400040708     C                   ENDIF
129500040708     C*** ATT
129600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
129700040708     C                   EVAL      PiStr=SkSplitCSV(i)
129800040708     C                   EXSR      CHKNUM
129900040708     C                   IF        PiInt=*on
130000040708     C                   Z-ADD     PiVal         VAOATT
130100040708     C                   ELSE
130200040708     C                   SETON                                        32
130300040708     C                   EVAL      VAOATT = *zeros
130400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130500040708     C                             + ' ' + 'VAOATT'
130600040708     C                   ENDIF
130700040708     C                   ENDIF
130800040708     C*** MTC
130900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
131000040708     C                   EVAL      PiStr=SkSplitCSV(i)
131100040708     C                   EXSR      CHKNUM
131200040708     C                   IF        PiInt=*on
131300040708     C                   Z-ADD     PiVal         VAOMTC
131400040708     C                   ELSE
131500040708     C                   SETON                                        32
131600040708     C                   EVAL      VAOMTC = *zeros
131700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131800040708     C                             + ' ' + 'VAOMTC'
131900040708     C                   ENDIF
132000040708     C                   ENDIF
132100040708     C*** KSC
132200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on      AND
132600030721     C                             PiVal<=9999999 AND
132700030721     C                             PiVal>*zeros
132800040708     C                   Z-ADD     PiVal         VAOKSC
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100040708     C                   EVAL      VAOKSC = *zeros
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300040708     C                             + ' ' + 'VAOKSC'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600040708     C*** POR
133700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on    AND
134100030721     C                             PiVal<=999   AND
134200030721     C                             PiVal>*zeros
134300040708     C                   Z-ADD     PiVal         VAOPOR
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600040708     C                   EVAL      VAOPOR = *zeros
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800040708     C                             + ' ' + 'VAOPOR'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100040708     C*** POC
135200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
135300040708     C                   EVAL      PiStr=SkSplitCSV(i)
135400040708     C                   EXSR      CHKNUM
135500040708     C                   IF        PiInt=*on    AND
135600040708     C                             PiVal<=999   AND
135700040708     C                             PiVal>*zeros
135800040708     C                   Z-ADD     PiVal         VAOPOC
135900040708     C                   ELSE
136000040708     C                   SETON                                        32
136100040708     C                   EVAL      VAOPOC = *zeros
136200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136300040708     C                             + ' ' + 'VAOPOC'
136400040708     C                   ENDIF
136500040708     C                   ENDIF
136600040708     C*** STO
136700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
136800040708     C                   EVAL      PiStr=SkSplitCSV(i)
136900040708     C                   EXSR      CHKNUM
137000040708     C                   IF        PiInt=*on
137100040708     C                   Z-ADD     PiVal         VAOSTO
137200040708     C                   ELSE
137300040708     C                   SETON                                        32
137400040708     C                   EVAL      VAOSTO = *zeros
137500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137600040708     C                             + ' ' + 'VAOSTO'
137700040708     C                   ENDIF
137800040708     C                   ENDIF
137900110526     C*
138000110526     C*********  Gestione particolarità
138100110526     C*
138200110526     C*** RSC_1
138300110526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSC_1'
138400110526     C                   EVAL      VAORSC = SkSplitCSV(i)
138500110526     C                   EVAL      VAOCRC = *zeros
138600110526     C                   ENDIF
138700110526     C*** RSC_2
138800110526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSC_2'
138900110526     C                   EVAL      VAORSC = %trim(VAORSC) + ' ' + SkSplitCSV(i)
139000110526     C                   ENDIF
139100110526     C*** NO1
139200110526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO1'
139300110526     C                   EVAL      VAONO1 = 'TEL DEST. ' + SkSplitCSV(i)
139400110526     C                   ENDIF
139500110616     C*** DAR
139600110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
139700110705     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
139800110705     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
139900110705     C                   ENDIF
140000110616     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
140100110616     C                                   %subst(SkSplitCSV(i):3:2)+
140200110616     C                                   %subst(SkSplitCSV(i):1:2)
140300110616     C                   EXSR      CHKNUM
140400110616     C                   IF        PiInt=*on
140500110616     C                   Z-ADD     PiVal         VAODAR
140600110616     C                   ELSE
140700110616     C                   SETON                                        32
140800110616     C                   EVAL      VAODAR = *zeros
140900110616     C                   EVAL      vinmsg = %trimr(vinmsg)
141000110616     C                             + ' ' + 'VAODAR'
141100110616     C                   ENDIF
141200110616     C                   ENDIF
141300110616     C*** ORR
141400110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOORR'
141500110621     C***                EVAL      PiStr=%subst(SkSplitCSV(i):1:2)+
141600110621     C***                                %subst(SkSplitCSV(i):4:2)
141700110621     C                   EVAL      PiStr=SkSplitCSV(i)
141800110616     C                   EXSR      CHKNUM
141900110616     C                   IF        PiInt=*on
142000110616     C                   Z-ADD     PiVal         VAOORR
142100110616     C                   ELSE
142200110616     C                   SETON                                        32
142300110616     C                   EVAL      VAOORR = *zeros
142400110616     C                   EVAL      vinmsg = %trimr(vinmsg)
142500110616     C                             + ' ' + 'VAOORR'
142600110616     C                   ENDIF
142700110616     C                   ENDIF
142800110526     C*
142900110526     C*********
143000030715     C*
143100030715     C                   ENDIF
143200030715     C                   EVAL      i = i + 1
143300030715     C                   ENDDO
143400040723     C*
143500030715     C                   ENDIF
143600020204     C*
143700010607     C                   ENDSR
143800010607     C*----------------------------------------------------*
143900010330
144000100512
144100100512
144200100512     C*------------------------------------------------------------------------*
144300100512     C* EXEFORZ - Considerazioni finali
144400100512     C*------------------------------------------------------------------------*
144500100512     C     EXEFORZ       BEGSR
144600100512     C*
144700100512     C                   if        VAOKSC = *zeros
144800100512     C                   select
144900100512     C                   when      VAOPAG = 'O'
145000100512     C                   movel     VAOCOR        VAOKSC
145100100512     C                   when      VAOPAG = 'M'
145200100512     C                   movel     VAOCRA        VAOKSC
145300100512     C                   when      VAOPAG = 'D'
145400100512     C                   movel     VAOCRC        VAOKSC
145500100512     C                   endsl
145600100512     C                   endif
145700100512     C*
145800100512     C                   ENDSR
145900080702
146000080702
146100080702     C*----------------------------------------------------*
146200080702     C*  CONTROLLO NUMERICITA' CAMPI
146300080702     C*----------------------------------------------------*
146400080702     C     CHKNUM        BEGSR
146500080702     C*
146600080702     C                   IF        PiDecChr = *blanks
146700080702     C                   EVAL      PiDecChr = ','
146800080702     C                   ENDIF
146900080702     C*
147000080702     C                   callp     UBISNUM_Check(PiStr
147100080702     C                                          :PiDecChr
147200080702     C                                          :PiVal
147300080702     C                                          :PiNum
147400080702     C                                          :PiInt)
147500080702     C*
147600080702     C                   ENDSR
147700080702     C***
147800010601
147900080702
148000010601
148100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
148200010601     C     repfil        BEGSR
148300010601     C*
148400010601     C                   if        invfil = *zeros and
148500010601     C                             depfil > *zeros and
148600010629     C                             (vinflg = *blanks or
148700010629     C                              vinflg = *zeros)
148800010601     C
148900010601     C                   eval      invfil = depfil
149000010601     C                   endif
149100010601     C*
149200010601     C                   if        depfil <> invfil and
149300010601     C                             invfil > *zeros
149400010601     C                   eval      flgMulti = '1'
149500010601     C                   if        vinflg = *blanks
149600010601     C                   add       1             cntNonEl
149700010601     C                   endif
149800010601     C                   endif
149900010601     C*
150000010601     C                   if        vinflg = '2'
150100010601     C                   eval      flgStato = '2'
150200010601     C                   endif
150300010601     C*
150400010601     C                   ENDSR
150500010601     C***
150600010601
150700010601
150800010601
150900010330
151000010330
151100010330
151200990920      /TITLE Invio dei dati al punto operativo.
151300000613     C     invio         BEGSR
151400990920     C*
151500990920     C                   reset                   dscmz
151600010601     C                   move      invfil        cmzdst
151700990920     C                   eval      cmzfld = vlrfou
151800990920     C                   eval      cmzmbd = vlrhdl
151900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
152000000710     C                   if        prmfir = *blanks
152100040708     C                   eval      cmzfla = 'FNVAO00F'
152200040708     C                   eval      cmzmba = 'FNVAO00F'
152300000710     C                   else
152400000710     C                   eval      cmzfla = prmfir
152500000710     C                   eval      cmzmba = prmfir
152600000710     C                   endif
152700990920     C                   eval      cmznrr = *zeros
152800990920     C                   move      §ctrok        cmznrr
152900021018     C                   eval      cmzlba = vlrfl1
153000990920     C                   call(e)   'TIS711C'
153100990920     C                   parm                    dscmz
153200990921     C                   parm      *blanks       esito
153300990920     C                   if        %error
153400990920     C                             or cmzerr = '1'
153500990921     C                             or esito  = '1'
153600000710     C                   eval      wrkesito = '3'
153700990920     C                   endif
153800990920     C*
153900000613     C                   ENDSR
154000990910
154100010601
154200010601
154300010601
154400010601
154500010601      /TITLE Invio dei dati al punto operativo.
154600010601     C     opeini        BEGSR
154700010601     C*
154800010601     C* Inizializzo flag e contatori operativi
154900010601     C                   movel     '0'           flgGiro           1
155000010601     C                   movel     '0'           flgMulti          1
155100010601     C                   movel     '1'           flgStato          1
155200010615     C                   movel     '0'           flgOk             1
155300010601     C                   z-add     *zeros        cntNonEl         10 0
155400010601     C                   z-add     *zeros        depfil            3 0
155500010601     C                   z-add     *zeros        invfil            3 0
155600010601     C*
155700010601     C                   ENDSR
155800010601     C***
155900010601
156000010601
156100050421
156200050421
156300050421     C     *pssr         BEGSR
156400050421     C*
156500050421     C                   eval      wrkesito = '2'
156600050421     C*
156700050421     C                   ENDSR     '*CANCL'
156800050421     C***
156900060306
157000060306
157100060306     C*-------------------------------------------------------------------------*
157200060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
157300060306     C*-------------------------------------------------------------------------*
157400060306     C     sr_Control    begsr
157500060306     C*
157600060306     C* inizializzo a ok il flag d controllo abilitazione
157700060306     C                   eval      Cli_Abil = *on
157800060306     C
157900060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
158000060306     C                   clear                   woriIDC
158100060306     C                   eval      kteCOD = 'VAP'
158200060306     C                   eval      kteKE1 = oriIDC
158300060306     C     keytbe        chain     TNTBE01L
158400060306     C                   if        %found(Tntbe01l)
158500060306     C                   eval      woriIDC = TbeKe2
158600060306     C                   else
158700060306     C                   eval      Cli_Abil = *off
158800060306     C                   endif
158900060306     C*
159000060306     C                   endsr
159100050421
159200010601
159300010330
159400010330
159500000613     C     *inzsr        BEGSR
159600990910     C*
159700990910     C     *entry        plist
159800990920     C                   parm                    tivlrds
159900990921     C                   parm      wrkesito      esito
160000000724     C                   parm                    prmlit
160100000710     C                   parm                    prmfir
160200010330     C*
160300010330     C* CALCOLA LA DATA CORRENTE
160400100223     C                   time                    wn14             14 0
160500100223     C                   movel     wn14          wn6               6 0          *ORA (6)
160600100223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
160700100223     C                   eval      datcor = %dec(%date() : *ISO)
160800040723     C*
160900040723     C* Campi riferito al database
161000040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
161100040723     C     *like         define    tbeke1        kteKE1
161200040723     C*
161300040723     C* Definizione chiavi di lettura
161400040723     C* TNTBE01L - parziale
161500040723     C     keytbe        klist
161600040723     C                   kfld                    kteCOD                         *UTENTE
161700040723     C                   kfld                    kteKE1                         *CODICE
161800000613     C*
161900000613     C                   ENDSR
162000000613     C***
162100040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
162200161221VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAONAR;VAORER;VAOTER;VAONCL;VAOPKG;
162300161221VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;VAONAC;VAONO1;VAONO2;VAODAR;VAOORR;
162400161221VAORFA;
162500110616
162600110621
