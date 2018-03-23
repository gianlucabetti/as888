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
002800130326     D***
002900130326     D WLBDAT          DS                  INZ
003000130326     D  G02DAT                 1      8  0
003100130326     D  G02INV                 9     16  0
003200130326     D  G02ERR                17     17
003300130326     D  G02TGI                18     22  0
003400010330     D esito           s              1
003500010330     D prmlit          s             10
003600010330     D prmfir          s             10
003700010330     D wrkesito        s                   like(esito)
003800010330     D rrnum           s              6  0 INZ(*zeros)
003900040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004000060306     D Cli_Abil        s              1    inz(*off)
004100060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004200090617     d §numori         s              7  0 inz
004300030715     D*------------------
004400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004500030715     D*------------------
004600030715     D SkSplitFLD      S             10    DIM(100)
004700030715     D SkSplitCSV      S            256    DIM(100)
004800030715     D CharCSV         S              1
004900030715     D CharTXT         S              1
005000030715     D CharNUM         S              1
005100030715     D posDa           S              3  0 INZ(*zeros)
005200030715     D posA            S              3  0 INZ(*zeros)
005300030715     D i               s              3  0 INZ(1)
005400030715     D wGiro           s              1  0 INZ(*zeros)
005500030822     D*------------------
005600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005700030822     D*------------------
005800030822     D posDaDft        S              3  0 INZ(*zeros)
005900030822     D posADft         S              3  0 INZ(*zeros)
006000030822     D j               s              3  0 INZ(1)
006100030822     D wGiroDft        s              1  0 INZ(*zeros)
006200010330     D*------------------
006300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400010330     D*------------------
006500010330     D WLBDA8          DS                  INZ
006600010330     D  G08DAT                 1      8  0
006700010330     D  G08INV                 9     16  0
006800010330     D  G08ERR                17     17
006900010330     D  G08TGI                18     22  0
007000040723     D*------------------
007100040723     D* DS REPERIMENTO DATI CLIENTE
007200040723     D*-------------------
007300040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007400040723     D ACODS         E DS                  EXTNAME(CNACO00F)
007500040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007600040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007700040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007800080702     D*------------------
007900080702     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
008000080702     D*------------------
008100080702     D PiStr           S            100A   INZ
008200080702     D PiDecChr        S              1A   INZ
008300080702     D PiVal           S             63S30 INZ
008400080702     D PiInt           S               N   INZ
008500080702     D PiNum           S               N   INZ
008600080702
008700080702     D*------------------
008800080702     D* LINKING A DEFINIZIONI ESTERNE
008900080702     D*------------------
009000080702     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100091026
009200091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
009300091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
009400091026
009500120605     D wRER_1          s                   like(VAORER)
009600120605     D wRER_2          s                   like(VAORER)
009700130416     D wTER_1          s                   like(VAOTER)
009800130416     D wTER_2          s                   like(VAOTER)
009900080702
010000031201     D*------------------
010100031201     D* Costanti
010200031201     D*------------------
010300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010500010330
010600010330
010700010330
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300041011     C                   exsr      PREELA
011400040708     C                   exsr      rwvao
011500010601     C*
011600010601     C                   seton                                        lr
011700010601
011800010601
011900010601
012000010601
012100010601     C*--------------------------------------------------------
012200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012300010601     C*--------------------------------------------------------
012400010601     C     PREELA        BEGSR
012500010601     C*
012600010601     C* SE OCCORRE SPEDIRE IN FILIALE
012700041011     C                   if        flgGiro = '0'
012800010601     C*
012900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013000010601     C                   eval      flgGiro = '1'
013100041011     C*
013200041011     C                   endif
013300010601     C*
013400010601     C                   ENDSR
013500010601     C***
013600010601
013700010601
013800010601
013900010601
014000010601
014100010601
014200010601
014300010601     C*--------------------------------------------------------
014400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014500010601     C*--------------------------------------------------------
014600010601     C     ENDELA        BEGSR
014700080707     C*
014800080707     C* Chiusura TRUL13R
014900080707     C                   clear                   TRUL13DS
015000080707     C                   eval      I13TLA = 'C'
015100080707     C                   call      'TRUL13R'
015200080707     C                   parm                    TRUL13DS
015300000616     C*
015400010601     C                   ENDSR
015500010601     C***
015600000613
015700010330
015800010601
015900010601
016000010601
016100010330     C*--------------------------------------------------------
016200040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
016300010330     C*--------------------------------------------------------
016400040708     C     RWVAO         BEGSR
016500010330     C*
016600010330     C                   if        not %open(tivin00r)
016700010330     C                   open      tivin00r
016800010330     C                   endif
016900040708     C                   if        not %open(fnvaowwr)
017000040708     C                   open      fnvaowwr
017100010330     C                   endif
017200010330     C*
017300010604     C                   clear                   §CTROK
017400010604     C                   clear                   §CTRMO
017500010604     C                   clear                   §CTRNO
017600010330     C*
017700030822     C                   do        *HIVAL
017800010330     C*
017900030822     C                   read      tivin00r                               70
018000010618     C*
018100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018200010618     C                   if        vinflg = '1'
018300010618     C                   eval      flgOk = '1'
018400010618     C                   endif
018500010618     C*
018600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018700040510     C                   z-add     *zeros        wLenVINDTA        4 0
018800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019200040510     C                   z-add     1             wFlgVINDTA
019300040510     C                   leave
019400040510     C                   endif
019500040510     C                   enddo
019600040510     C*
019700040218     C                   if        vindta > *blanks AND
019800040510     C                             wFlgVINDTA = 1
019900010330     C                   add       1             rrnum
020000010330     C*
020100010601     C                   if        *in70 = *off and
020200010330     C                             (vinflg = *blanks
020300010330     C                              or vinflg = '0'
020400010330     C                              or vinflg = '2')
020500010330     C*
020600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020700010711     C                   if        vinflg = *blanks or vinflg = '0'
020800010711     C                   clear                   vinmsg
020900010711     C                   endif
021000010601     C*
021100010330     C                   exsr      inzvar
021200040708     C                   exsr      impvao
021300100512     C                   exsr      exeforz
021400010601     C*
021500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021600040708     C                   eval      depfil = VAOPOE
021700010601     C                   exsr      repfil
021800010601     C                   if        depfil = invfil
021900021025     C                   if        vlrpoi = 999
022000040708     C                   move(P)   invfil        VAOPOE
022100021025     C                   else
022200040708     C                   move(P)   vlrpoi        VAOPOE
022300021025     C                   endif
022400010601     C*
022500010604     C* Ebbene...
022600010604     C*
022700030822     C  N31              add       1             §CTROK            7 0
022800030822     C   32              add       1             §CTRMO            7 0
022900030822     C   31              add       1             §CTRNO            7 0
023000030822     C                   if        wGiro = 2
023100040723     C  N31              EXSR      CHKCOR
023200040723     C  N31              EXSR      WRIORI
023300050826     C  N31              EXSR      WRIVGD
023400040723     C  N31              write     FNVAO000
023500030822     C                   endif
023600010604     C*
023700010604     C                   if        *in31 = *off and
023800010604     C                             *in32 = *off
023900010604     C                   eval      vinflg = '1'
024000010604     C                   else
024100010604     C                   eval      vinflg = '2'
024200010604     C                   endif
024300010604     C                   endif
024400010601     C*
024500010604     C                   endif
024600010604     C*
024700010330     C                   else
024800010330     C                   eval      vinflg = '1'
024900010330     C                   endif
025000010601     C*
025100010601     C  N70              update    tivin000
025200010330     C*
025300030822     C  N70              enddo
025400010601     C
025500010601     C*
025600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025700010601     C                   if        cntNonEl = *zeros or
025800010601     C                             flgMulti = '0'
025900010330     C* Se non ci sono record con errori ...
026000010601     C                   if        §ctrno = 0 and
026100010604     C                             §ctrmo = 0 and
026200010601     C                             flgStato <> '2'
026300010330     C* ... restituisco esito OK.
026400010330     C                   eval      wrkesito = '0'
026500010330     C                   else
026600010330     C                   if        §ctrok > 0
026700010330     C                   eval      wrkesito = '1'
026800010330     C                   else
026900010615     C                   if        flgOk = '0'
027000010615     C                   eval      wrkesito = '2'
027100010615     C                   else
027200010615     C                   eval      wrkesito = '6'
027300010615     C                   endif
027400010330     C                   endif
027500010330     C                   endif
027600010601     C                   else
027700010601     C                   eval      wrkesito = '9'
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        %open(tivin00r)
028100010330     C                   close     tivin00r
028200010330     C                   endif
028300040708     C                   if        %open(fnvaowwr)
028400040708     C                   close     fnvaowwr
028500010330     C                   endif
028600010601     C*
028700010601     C                   if        vlrpoi <> 999
028800010601     C                   eval      invfil = vlrpoi
028900010601     C                   endif
029000010330     C*
029100010330     C                   if        §ctrok > 0
029200010601     C                             and invfil > *zeros
029300010330     C                   exsr      invio
029400010330     C                   endif
029500010601     C*
029600010618     C                   if        flgGiro = '1'
029700010601     C                   exsr      endela
029800010618     C                   endif
029900010330     C*
030000010330     C                   ENDSR
030100010330     C***
030200040723
030300040723
030400040723     C*------------------------------------------------------------------------*
030500040723     C* CHKCOR - Considerazioni su VAOCOR
030600040723     C*------------------------------------------------------------------------*
030700040723     C     CHKCOR        BEGSR
030800040723     C*
030900040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
031000040723     C                   if        vaoTCO = 'F'
031100040723     C* Quindi verifico se il codice ordinante non è valorizzato
031200040723     C                   if        vaoCOR = *zeros  AND
031300040723     C                             vaoRSO = *blanks AND
031400040723     C                             vaoINO = *blanks AND
031500040723     C                             vaoCAO = *blanks AND
031600040723     C                             vaoLOO = *blanks AND
031700040723     C                             vaoPRO = *blanks AND
031800040723     C                             vaoNAO = *blanks
031900040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
032000040723     C                   movel     '0'           wFlgAcr           1
032100040723     C                   move      vlrKSC        wCli              7 0
032200040723     C                   move(p)   wCli          acrCRO
032300040723     C     acrCRO        setll     fnacr01l
032400040723     C                   if        %found(fnacr01l)
032500040723     C                   read      fnacr01l
032600040723     C                   dow       not %eof(fnacr01l)
032700040723     C                   movel     acrCRO        wCliAcr           7 0
032800040723     C                   if        wCliAcr = wCli
032900040723     C                   eval      vaoCOR = acrCRO
033000040723     C                   movel     '1'           wFlgAcr
033100040723     C                   leave
033200040723     C                   else
033300040723     C                   leave
033400040723     C                   endif
033500040723     C                   read      fnacr01l
033600040723     C                   enddo
033700040723     C                   endif
033800040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
033900040723     C* dati anagrafici dell'ordinante dal piano dei conti
034000040723     C                   if        wFlgAcr = '0'
034100040723     C                   clear                   BS69DS
034200040723     C                   clear                   ACODS
034300040723     C                   clear                   INDDS
034400040723     C                   clear                   CLPDS
034500040723     C                   clear                   CLSDS
034600040723     C                   move(P)   vlrKSC        I69KAC
034700040723     C                   move(P)   vlrKSC        I69KIN
034800040723     C                   call      'TIBS69R'
034900040723     C                   parm                    BS69DS
035000040723     C                   parm                    ACODS
035100040723     C                   parm                    INDDS
035200040723     C                   parm                    CLPDS
035300040723     C                   parm                    CLSDS
035400040723     C     O69ERR        ifne      '1'
035500040723     C                   eval      vaoRSO = ACORAG
035600040723     C                   eval      vaoINO = INDVIA
035700040723     C                   movel(P)  INDCAP        vaoCAO
035800040723     C                   eval      vaoLOO = INDCIT
035900040723     C                   eval      vaoPRO = INDPRV
036000040723     C                   eval      vaoNAO = INDSTA
036100040723     C                   endif
036200040723     C                   endif
036300040723     C                   endif
036400040723     C                   endif
036500040723     C*
036600040723     C                   ENDSR
036700040723
036800040723
036900040723
037000040723     C*------------------------------------------------------------------------*
037100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
037200040723     C*------------------------------------------------------------------------*
037300040723     C     WRIORI        BEGSR
037400040723     C*
037500090617     C* Come prima cosa stacco un numeratore da AZNUM
037600090617     C                   clear                   trul33ds
037700090617     C                   eval      I33OPE = *zeros
037800090617     C                   eval      I33CNU = 600
037900090617     C                   eval      I33NUM = 1
038000090617     C                   movel     TRUL33DS      KPJBU
038100090617     C                   call      'TRUL33R'
038200090617     C                   parm                    KPJBA
038300090617     C                   movel     KPJBU         TRUL33DS
038400090617     C                   if        O33ERR = *zeros
038500090617     C                   z-add     O33NRF        §numori
038600090618     c                   else
038700090618     c                   eval      §numori = *all'9'
038800090617     C                   endif
038900040723     C*
039000040723     C* Quindi imposto i campi "extra-VAO"
039100040723     C                   if        vlrKSC = '0ORM0000'
039200040723     C*
039300040723     C                   if        vaoCOR > *zeros
039400040723     C                   movel     vaoCOR        wrkCOR7           7 0
039500040723     C*
039600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
039700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
039800040723     C                   clear                   tibs10ds
039900040723     C                   eval      d10drf = datcor                              *data riferimento
040000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
040100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
040200091026     C*
040300091026     C* Preparazione chiamata al TIBS10R1
040400091026     C                   clear                   tibs10r1ds
040500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
040600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
040700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
040800091026     C*
040900091026     C                   call      'TIBS10R1'
041000091026     C                   parm                    tibs10r1ds
041100091026     C*
041200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
041300040723     C*
041400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
041500040723     C                             d10cop =  *zeros  or
041600040723     C                             d10cop =  d10cod
041700040723     C                   move      d10cod        oriIDC
041800040723     C                   else                                                   *Figlio
041900040723     C                   move      d10cop        oriIDC
042000040723     C                   endif
042100040723     C*
042200040723     C                   else
042300040723     C                   eval      oriIDC = *zeros
042400040723     C                   endif
042500040723     C*
042600040723     C                   else
042700040723     C                   eval      oriIDC = vlrKSC
042800040723     C                   endif
042900040723     C*
043000040723     C                   eval      oriPRG = §numori
043100040723     C                   eval      oriDER = datcor
043200040723     C                   eval      oriOER = wn6
043300040723     C                   eval      oriPOE = vaoPOE
043400040723     C                   eval      oriNSR = vaoNSR
043500040723     C                   eval      oriNOR = vaoNOR
043600040723     C                   eval      oriNRV = vaoNRV
043700040723     C                   eval      oriFLO = *blanks
043800040723     C*
043900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
044000040723     C                   eval      dorm01 = vaoFLO
044100040723     C                   movel     §numori       §ormpg
044200050412     C*
044300050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
044400050412     C                   movel     'S'           §ormfr
044500130604 xxx C                   eval      §ORMFR = 'S'
044600130604     C                   eval      §ORMKS = 'S'
044700130604     C                   eval      §ORMFD = 'S'
044800050412     C*
044900050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
045000040723     C                   eval      vaoFLO = dorm01
045100040723     C*
045200040723     C* ...quindi scrivo il file TIORI00F
045300150123     C  N55              WRITE     TIORI000
045400040723     C*
045500040723     C                   ENDSR
045600050826
045700050826
045800050826
045900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
046000050826     C     wriVGD        BEGSR
046100050826     C*
046200050826     C* Reperisco la descrizione della fase da tabella
046300050826     C                   movel(p)  'FAR'         KteCOD
046400050826     C                   movel(p)  '000'         KteKE1
046500050826     C     KEYtbe        chain     tntbe01l
046600050826     C                   if        %found(tntbe01l)
046700050826     C                   movel(P)  tbeuni        DFAR
046800050826     C                   else
046900050826     C                   clear                   DFAR
047000050826     C                   endif
047100050826     C*
047200050826     C* Valorizzo buffer record
047300050826     C                   clear                   FNVAPDS
047400050826     C                   eval      vapIDC = oriIDC
047500050826     C                   eval      vapPOE = oriPOE
047600050826     C                   eval      vapRFA = vaoRFA
047700050826     C                   eval      vapPOG = oriPOE
047800050826     C                   eval      vapDAE = oriDER
047900050826     C                   eval      vapORE = oriOER
048000050826     C                   if        d§fardva <> *blanks
048100050826     C                   eval      vapDFA = d§fardva
048200050826     C                   else
048300050826     C                   eval      vapDFA = d§fardes
048400050826     C                   endif
048500050826     C*
048600050826     C                   clear                   tivgd000
048700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
048800050826     C                   eval      vgdTIP = 'VP'
048900050826     C                   eval      vgdKSU = vapIDC
049000050826     C                   eval      vgdTSC = 'WW'
049100050826     C                   eval      vgdDAT = datcor
049200080702     C                   eval      vgdPGM = 'TITV1H6R'
049300060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
049400060306     C                   exsr      sr_Control
049500060306     C                   if        Cli_Abil = *on
049600060306     C                   eval      vgdKSU = woriIDC
049700150123     C  N55              write     tivgd000
049800060306     C                   endif
049900050826     C*
050000050826     C                   ENDSR
050100050826     C*------------------------------------------------------------------------*
050200010601
050300010601
050400010601
050500010330     C*----------------------------------------------------*
050600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050700010330     C*----------------------------------------------------*
050800010330     C     INZVAR        BEGSR
050900010330     C*
051000030822     C* Inizializzo variabili di wrk
051100020204     C                   Z-ADD     *zeros        Num5_0            5 0
051200030822     C*
051300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051400040708     C                   CLEAR                   FNVAO000
051500040723     C                   CLEAR                   TIORI000
051600030822     C                   CLEAR                   SkSplitCSV
051700030822     C                   EVAL      i = 1
051800030822     C                   EVAL      posDa = *zeros
051900030822     C                   EVAL      posA  = *zeros
052000030822     C*
052100030822     C* Reimposto i valori di default
052200030822     C                   EXSR      DEFCAM
052300030822     C*
052400010330     C                   ENDSR
052500010330     C*----------------------------------------------------*
052600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052700010330     C*----------------------------------------------------*
052800010330     C     DEFCAM        BEGSR
052900030822     C*
053000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053100030715     C* e delimitatore testo.
053200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053500030822     C*
053600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053700130115     C* NON PIU' GESTITO
053800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053900030715     C                   SETON                                        50
054000030715     C                   ELSE
054100030715     C                   SETOFF                                       50
054200030715     C                   ENDIF
054300150123
054400150123     C* se il 1° char dei parametri è T=Test
054500150123     C                   SETOFF                                       55
054600150123     C                   IF        %subst(vlrppt:1:1) = 'T'
054700150123     C* inibisco la scrittura dei file TIORI e TIVGD
054800150123     C                   SETON                                        55
054900150123     C* imposto il PO di emissione come filiale fittizia
055000150123     C                   EVAL      VAOPOE = 999
055100150123     C                   ENDIF
055200040723     C*
055300080707     C* Imposto i valori d default d alcuni campi
055400111109     C***                EVAL      VAOCOR = 0594068102
055500111109     C***                EVAL      VAOCRC = 0594068102
055600111109     C***                EVAL      VAOCTR = '100'
055700111109     C***                EVAL      VAOORR = 1600
055800111109     C***                EVAL      VAOPOE = 059
055900080708     C*
056000080708     C                   CLEAR                   DORM01
056100160107     C                   EVAL      §ORCOMC = 'N'
056200080708     C                   EVAL      VAOFLO = DORM01
056300100226     C***                EVAL      VAODAR = %dec(%date() + %Days(1) : *ISO)
056400110621     C***                EVAL      VAODAR = datcor
056500080707     C*
056600110628     C                   EVAL      VAOPAG = 'O'
056700111114     C***                EVAL      VAODDT = 'S'
056800080707     C                   EVAL      VAOTCO = 'F'
056900080707     C                   EVAL      VAOTOR = 'S'
057000030822     C*
057100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057200030822     C                   EVAL      posDaDft = 1
057300030822     C                   EVAL      posADft  = 0
057400030822     C                   EVAL      wGiroDft = 0
057500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057600030822     C                             posDaDft > 0
057700030822     C*
057800030822     C* Gestisco il 1° giro
057900030822     C                   IF        wGiroDft = 0
058000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058200030822     C* Incremento il contatore dei "giri"
058300030822     C                   EVAL      wGiroDft = 1
058400030822     C                   ELSE
058500030822     C                   EVAL      posDaDft = posADft
058600030822     C                   ENDIF
058700030822     C* Eseguo lo scan x trovare la fine del campo corrente
058800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058900030822     C*
059000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059200030822     C                   IF        posDaDft > 0 AND
059300030822     C                             posADft  > 0
059400040708     C* POE
059500040708     C                   IF        %subst(
059600040708     C                             %subst(vlrppt:posDaDft+1:
059700040708     C                             posADft-posDaDft-1):1:3)
059800040708     C                             = 'POE'
059900040708     C                   EVAL      PiStr=%trim(%subst(
060000040708     C                             %subst(vlrppt:posDaDft+1:
060100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200040708     C                   EXSR      CHKNUM
060300040708     C                   IF        PiInt=*on
060400040708     C                   Z-ADD     PiVal         VAOPOE
060500040708     C                   ENDIF
060600040708     C                   ENDIF
060700040708     C* TOR
060800040708     C                   IF        %subst(
060900040708     C                             %subst(vlrppt:posDaDft+1:
061000040708     C                             posADft-posDaDft-1):1:3)
061100040708     C                             = 'TOR'
061200040708     C                   EVAL      VAOTOR=%trim(%subst(
061300040708     C                             %subst(vlrppt:posDaDft+1:
061400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500040708     C                   ENDIF
061600040708     C* TCO
061700040708     C                   IF        %subst(
061800040708     C                             %subst(vlrppt:posDaDft+1:
061900040708     C                             posADft-posDaDft-1):1:3)
062000040708     C                             = 'TCO'
062100040708     C                   EVAL      VAOTCO=%trim(%subst(
062200040708     C                             %subst(vlrppt:posDaDft+1:
062300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400040708     C                   ENDIF
062500040707     C* COR
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900040707     C                             = 'COR'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiInt=*on
063500040708     C                   Z-ADD     PiVal         VAOCOR
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800040708     C* CRC
063900040708     C                   IF        %subst(
064000040708     C                             %subst(vlrppt:posDaDft+1:
064100040708     C                             posADft-posDaDft-1):1:3)
064200040708     C                             = 'CRC'
064300040708     C                   EVAL      PiStr=%trim(%subst(
064400040708     C                             %subst(vlrppt:posDaDft+1:
064500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600040708     C                   EXSR      CHKNUM
064700040708     C                   IF        PiInt=*on
064800040708     C                   Z-ADD     PiVal         VAOCRC
064900040708     C                   ENDIF
065000040708     C                   ENDIF
065100040708     C* FFD
065200040708     C                   IF        %subst(
065300040708     C                             %subst(vlrppt:posDaDft+1:
065400040708     C                             posADft-posDaDft-1):1:3)
065500040708     C                             = 'FFD'
065600040708     C                   EVAL      VAOFFD=%trim(%subst(
065700040708     C                             %subst(vlrppt:posDaDft+1:
065800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900040708     C                   ENDIF
066000040708     C* ORR
066100040708     C                   IF        %subst(
066200040708     C                             %subst(vlrppt:posDaDft+1:
066300040708     C                             posADft-posDaDft-1):1:3)
066400040708     C                             = 'ORR'
066500040708     C                   EVAL      PiStr=%trim(%subst(
066600040708     C                             %subst(vlrppt:posDaDft+1:
066700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800040708     C                   EXSR      CHKNUM
066900040708     C                   IF        PiInt=*on
067000040708     C                   Z-ADD     PiVal         VAOORR
067100040708     C                   ENDIF
067200040708     C                   ENDIF
067300040712     C* NCL
067400040712     C                   IF        %subst(
067500040712     C                             %subst(vlrppt:posDaDft+1:
067600040712     C                             posADft-posDaDft-1):1:3)
067700040712     C                             = 'NCL'
067800040712     C                   EVAL      PiStr=%trim(%subst(
067900040712     C                             %subst(vlrppt:posDaDft+1:
068000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100040712     C                   EXSR      CHKNUM
068200040712     C                   IF        PiInt=*on
068300040712     C                   Z-ADD     PiVal         VAONCL
068400040712     C                   ENDIF
068500040712     C                   ENDIF
068600040712     C* PKG
068700040712     C                   IF        %subst(
068800040712     C                             %subst(vlrppt:posDaDft+1:
068900040712     C                             posADft-posDaDft-1):1:3)
069000040712     C                             = 'PKG'
069100040712     C                   EVAL      PiStr=%trim(%subst(
069200040712     C                             %subst(vlrppt:posDaDft+1:
069300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400040712     C                   EXSR      CHKNUM
069500040712     C                   IF        PiNum=*on
069600040712     C                   Z-ADD     PiVal         VAOPKG
069700040712     C                   ENDIF
069800040712     C                   ENDIF
069900040712     C* VLM
070000040712     C                   IF        %subst(
070100040712     C                             %subst(vlrppt:posDaDft+1:
070200040712     C                             posADft-posDaDft-1):1:3)
070300040712     C                             = 'VLM'
070400040712     C                   EVAL      PiStr=%trim(%subst(
070500040712     C                             %subst(vlrppt:posDaDft+1:
070600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700040712     C                   EXSR      CHKNUM
070800040712     C                   IF        PiNum=*on
070900040712     C                   Z-ADD     PiVal         VAOVLM
071000040712     C                   ENDIF
071100040712     C                   ENDIF
071200040708     C* NAM
071300040708     C                   IF        %subst(
071400040708     C                             %subst(vlrppt:posDaDft+1:
071500040708     C                             posADft-posDaDft-1):1:3)
071600040708     C                             = 'NAM'
071700040708     C                   EVAL      VAONAM=%trim(%subst(
071800040708     C                             %subst(vlrppt:posDaDft+1:
071900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000040708     C                   ENDIF
072100040708     C* PAG
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500040708     C                             = 'PAG'
072600040708     C                   EVAL      VAOPAG=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000040708     C* KSC
073100040708     C                   IF        %subst(
073200040708     C                             %subst(vlrppt:posDaDft+1:
073300040708     C                             posADft-posDaDft-1):1:3)
073400040708     C                             = 'KSC'
073500040708     C                   EVAL      PiStr=%trim(%subst(
073600040708     C                             %subst(vlrppt:posDaDft+1:
073700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800040708     C                   EXSR      CHKNUM
073900040708     C                   IF        PiInt=*on
074000040708     C                   Z-ADD     PiVal         VAOKSC
074100040708     C                   ENDIF
074200040708     C                   ENDIF
074300040708     C* CTR
074400040708     C                   IF        %subst(
074500040708     C                             %subst(vlrppt:posDaDft+1:
074600040708     C                             posADft-posDaDft-1):1:3)
074700040708     C                             = 'CTR'
074800040708     C                   EVAL      VAOCTR=%trim(%subst(
074900040708     C                             %subst(vlrppt:posDaDft+1:
075000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100040708     C                   ENDIF
075200040708     C* POC
075300040708     C                   IF        %subst(
075400040708     C                             %subst(vlrppt:posDaDft+1:
075500040708     C                             posADft-posDaDft-1):1:3)
075600040708     C                             = 'POC'
075700040708     C                   EVAL      PiStr=%trim(%subst(
075800040708     C                             %subst(vlrppt:posDaDft+1:
075900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000040708     C                   EXSR      CHKNUM
076100040708     C                   IF        PiInt=*on
076200040708     C                   Z-ADD     PiVal         VAOPOC
076300040708     C                   ENDIF
076400040708     C                   ENDIF
076500040708     C* NO1
076600040708     C                   IF        %subst(
076700040708     C                             %subst(vlrppt:posDaDft+1:
076800040708     C                             posADft-posDaDft-1):1:3)
076900040708     C                             = 'NO1'
077000040708     C                   EVAL      VAONO1=%trim(%subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300040708     C                   ENDIF
077400040708     C* DDT
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800040708     C                             = 'DDT'
077900040708     C                   EVAL      VAODDT=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300040708     C* RFA
078400040708     C                   IF        %subst(
078500040708     C                             %subst(vlrppt:posDaDft+1:
078600040708     C                             posADft-posDaDft-1):1:3)
078700040708     C                             = 'RFA'
078800040708     C                   EVAL      VAORFA=%trim(%subst(
078900040708     C                             %subst(vlrppt:posDaDft+1:
079000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100040708     C                   ENDIF
079200040708     C* FLO
079300040708     C                   IF        %subst(
079400040708     C                             %subst(vlrppt:posDaDft+1:
079500040708     C                             posADft-posDaDft-1):1:3)
079600040708     C                             = 'FLO'
079700040708     C                   EVAL      VAOFLO=%trim(%subst(
079800040708     C                             %subst(vlrppt:posDaDft+1:
079900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
080100080702     C* e posizione 7 del VAOFLO
080200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
080300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
080400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
080500040708     C                   ENDIF
080600030822     C* ...
080700030822     C                   ENDIF
080800030822     C                   ENDDO
080900020204     C*
081000010330     C                   ENDSR
081100010607     C*----------------------------------------------------*
081200040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
081300010607     C*----------------------------------------------------*
081400040708     C     IMPVAO        BEGSR
081500010607     C*
081600010607     C                   SETOFF                                       3132
081700030515     C*
081800100223     C                   MOVEL     datcor        VAODAO
081900040708     C                   MOVEL     wn14          VAOOAO
082000050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082100050314     C     minu:maiu     XLATE     vindta        vindta
082200010607     C*
082300030715     C**********
082400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082500030715     C**********
082600030715     C                   IF        wGiro = *zeros
082700040709     C*
082800040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
082900040709     C                   EVAL      depvinDTA = vinDTA
083000040709     C*
083100040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
083200040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083300110616     C                                      %trim(intcol(3))+%trim(intcol(4))+
083400110616     C                                      %trim(intcol(5))
083500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083600050314     C**!!!minu:maiu     XLATE     vindta        vindta
083700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083800040324     C                   DOW       posDa <= %len(%trimr(vindta))
083900030715     C*
084000030715     C* Gestisco il 1° campo
084100030715     C                   IF        i = 1
084200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084300030715     C                   EVAL      posDa = 1
084400030715     C* Eseguo lo scan x trovare la fine del primo campo
084500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C* X i campi successivi al 1°
085000030715     C                   ELSE
085100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085200030715     C                   EVAL      posDa = posA + 1
085300030715     C* Eseguo lo scan x trovare la fine del campo corrente
085400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085500030715     C* Gestisco l'ultimo campo
085600030715     C                   IF        posA = *zeros
085700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
085800030715     C* A questo punto "estraggo" il campo corrente
085900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086000030715     C                                                    (posA-posDa))
086100030715     C                   LEAVE
086200030715     C                   ELSE
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C                   ENDIF
086700030715     C                   ENDIF
086800030715     C* Incremento il contatore d campo
086900030715     C                   EVAL      i = i +1
087000030715     C                   ENDDO
087100030715     C*
087200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087300030715     C* delimitazione testo
087400030715     C                   EVAL      i = 1
087500030715     C                   DOW       i < %elem(SkSplitFLD)
087600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087800030715     C                   EVAL      i = i + 1
087900030715     C                   ENDDO
088000030715     C                   EVAL      wGiro = 1
088100040709     C* Ripristino la 1° riga originale
088200040709     C                   EVAL      vinDTA = depvinDTA
088300030715     C                   ELSE
088400030715     C                   EVAL      wGiro = 2
088500031204     C**********
088600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088700031204     C* d separatore campo
088800031204     C**********
088900040709     C                   EVAL      vindta = %trim(vindta)
089000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089100040709     C                   EVAL      vindta = %subst(vindta:2)
089200040709     C                   ENDDO
089300040709     C*
089400040709     C                   Z-ADD     *zeros        lunghInput        4 0
089500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
089600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
089900040709     C                   ENDDO
090000030715     C**********
090100030715     C* Effettuo lo split del campo dati d input
090200030715     C**********
090300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090400040324     C                   DOW       posDa <= %len(%trimr(vindta))
090500030715     C*
090600030715     C* Gestisco il 1° campo
090700030715     C                   IF        i = 1
090800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090900030715     C                   EVAL      posDa = 1
091000030715     C* Eseguo lo scan x trovare la fine del primo campo
091100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C* X i campi successivi al 1°
091600030715     C                   ELSE
091700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091800030715     C                   EVAL      posDa = posA + 1
091900031204     C* Verifico che nn vi sia il campo nn valorizzato
092000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092100031204     C* Se campo nn valorizzato skippo al prossimo
092200031204     C                   EVAL      SkSplitCSV(i) = *blanks
092300031204     C                   EVAL      posA  = posA + 1
092400031204     C                   ELSE
092500030715     C* Eseguo lo scan x trovare la fine del campo corrente
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* Gestisco l'ultimo campo
092800030715     C                   IF        posA = *zeros
092900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   LEAVE
093400030715     C                   ELSE
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C                   ENDIF
093900031204     C                   ENDIF
094000030715     C                   ENDIF
094100030715     C* Incremento il contatore d campo
094200030715     C                   EVAL      i = i +1
094300030715     C                   ENDDO
094400030715     C*
094500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094600030715     C* delimitazione testo
094700030715     C                   EVAL      i = 1
094800030715     C                   DOW       i < %elem(SkSplitCSV)
094900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095100030715     C                   EVAL      i = i + 1
095200030715     C                   ENDDO
095300030715     C*
095400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitFLD)
095700030715     C*
095800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096000030715     C                             SkSplitCSV(i) <> *blanks
096100030721     C*** ATB
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
096300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
096400030721     C                   ENDIF
096500040708     C*** TOR
096600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
096700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
096800040708     C                   ENDIF
096900040708     C*** TCO
097000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
097100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
097200040708     C                   ENDIF
097300040708     C*** RSO
097400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
097500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
097600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
097800030715     C* ==
097900030715     C                   ENDIF
098000040708     C*** INO
098100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
098200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
098300030715     C                   ENDIF
098400040708     C*** LOO
098500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
098600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
098700030715     C                   ENDIF
098800040708     C*** PRO
098900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
099000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
099100030715     C                   ENDIF
099200040708     C*** NAO
099300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
099400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
099500030721     C                   ENDIF
099600040708     C*** RSR
099700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
099800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
099900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
100100040708     C* ==
100200040708     C                   IF        VAORSR = *blanks
100300040708     C                   SETON                                        31
100400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100500040708     C                             + ' ' + 'VAORSR'
100600040708     C                   ENDIF
100700040708     C                   ENDIF
100800040708     C*** INR
100900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
101000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
101100040708     C                   IF        VAOINR = *blanks
101200040708     C                   SETON                                        31
101300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
101400040708     C                             + ' ' + 'VAOINR'
101500040708     C                   ENDIF
101600040708     C                   ENDIF
101700040708     C*** LOR
101800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
101900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
102000040708     C                   IF        VAOLOR = *blanks
102100040708     C                   SETON                                        31
102200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
102300040708     C                             + ' ' + 'VAOLOR'
102400040708     C                   ENDIF
102500040708     C                   ENDIF
102600040708     C*** PRR
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
102800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
102900040708     C                   IF        VAOPRR = *blanks
103000040708     C                   SETON                                        31
103100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103200040708     C                             + ' ' + 'VAOPRR'
103300040708     C                   ENDIF
103400040708     C                   ENDIF
103500040708     C*** NAR
103600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
103700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
103800040708     C                   ENDIF
103900040708     C*** RER
104000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
104100040708     C                   EVAL      VAORER = SkSplitCSV(i)
104200040708     C                   ENDIF
104300040708     C*** TER
104400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
104500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
104600040708     C                   ENDIF
104700040708     C*** RSC
104800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
104900040708     C                   EVAL      VAORSC = SkSplitCSV(i)
105000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
105100040708     C     '@':'A'       XLATE     VAORSC        VAORSC
105200040708     C* ==
105300040708     C                   ENDIF
105400040708     C*** INC
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
105600040708     C                   EVAL      VAOINC = SkSplitCSV(i)
105700040708     C                   ENDIF
105800040708     C*** LOC
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
106000040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
106100040708     C                   ENDIF
106200040708     C*** PRC
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
106400040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
106500040708     C                   ENDIF
106600040708     C*** NAC
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
106800040708     C                   EVAL      VAONAC = SkSplitCSV(i)
106900040708     C                   ENDIF
107000040708     C*** FFD
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
107200040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
107300040708     C                   ENDIF
107400040708     C*** RMP
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
107600040708     C                   EVAL      VAORMP = SkSplitCSV(i)
107700040708     C                   ENDIF
107800040708     C*** NAM
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
108000040708     C                   EVAL      VAONAM = SkSplitCSV(i)
108100040708     C                   ENDIF
108200040708     C*** PAG
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
108400040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
108500040708     C                   ENDIF
108600040708     C*** CTR
108700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
108800040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
108900040708     C                   ENDIF
109000040708     C*** ZOR
109100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
109200040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
109300040708     C                   ENDIF
109400040708     C*** NO1
109500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
109600040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
109700030715     C                   ENDIF
109800040708     C*** NO2
109900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
110000040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
110100030715     C                   ENDIF
110200040708     C*** DDT
110300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
110400040708     C                   EVAL      VAODDT = SkSplitCSV(i)
110500030715     C                   ENDIF
110600040708     C*** RFA
110700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
110800040708     C                   EVAL      VAORFA = SkSplitCSV(i)
110900030715     C                   ENDIF
111000040708     C*** SPI
111100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
111200040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
111300030715     C                   ENDIF
111400040708     C*** FLO
111500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
111600040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
111700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
111800080708     C* e la posizione 7 del VAOFLO
111900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
112000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
112100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
112200030715     C                   ENDIF
112300050314
112400050412      *** DAR
112500050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
112600050412     C                   EVAL      PiStr=SkSplitCSV(i)
112700050412     C                   EXSR      CHKNUM
112800050412     C                   IF        PiInt=*on
112900050412     C                   Z-ADD     PiVal         VAODAR
113000050412     C                   ELSE
113100050412     C                   SETON                                        32
113200050412     C                   EVAL      VAODAR = *zeros
113300050412     C                   EVAL      vinmsg = %trimr(vinmsg)
113400050412     C                             + ' ' + 'VAODAR'
113500050412     C                   ENDIF
113600050314     C                   ENDIF
113700030715     C
113800030721     C*
113900030721     C* Reperisco quindi i campi numerici...
114000040708     C*** POE
114100040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
114200040723     C                   EVAL      PiStr=SkSplitCSV(i)
114300040723     C                   EXSR      CHKNUM
114400040723     C                   IF        PiInt=*on    AND
114500040723     C                             PiVal<=999   AND
114600040723     C                             PiVal>*zeros
114700040723     C                   Z-ADD     PiVal         VAOPOE
114800040723     C                   ELSE
114900040723     C                   SETON                                        32
115000040723     C                   EVAL      VAOPOE = *zeros
115100040723     C                   EVAL      vinmsg = %trimr(vinmsg)
115200040723     C                             + ' ' + 'VAOPOE'
115300040723     C                   ENDIF
115400040723     C                   ENDIF
115500040708     C*** COR
115600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
115700040708     C                   EVAL      PiStr=SkSplitCSV(i)
115800040708     C                   EXSR      CHKNUM
115900040708     C                   IF        PiInt=*on
116000040708     C                   Z-ADD     PiVal         VAOCOR
116100040708     C                   ELSE
116200040708     C                   SETON                                        31
116300040708     C                   EVAL      VAOCOR = *zeros
116400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
116500040708     C                             + ' ' + 'VAOCOR'
116600040708     C                   ENDIF
116700040708     C                   ENDIF
116800040708     C*** CAO
116900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on
117300030721     C                   Z-ADD     PiVal         Num5_0
117400040708     C                   MOVEL(p)  Num5_0        VAOCAO
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700040708     C                   EVAL      VAOCAO = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900040708     C                             + ' ' + 'VAOCAO'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200040708     C*** CRA
118300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on
118700040708     C                   Z-ADD     PiVal         VAOCRA
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000040708     C                   EVAL      VAOCRA = *zeros
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200040708     C                             + ' ' + 'VAOCRA'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500040708     C*** CAR
119600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
119700040708     C                   EVAL      PiStr=SkSplitCSV(i)
119800040708     C                   EXSR      CHKNUM
119900040708     C                   IF        PiInt=*on
120000040708     C                   Z-ADD     PiVal         Num5_0
120100040708     C                   MOVEL(p)  Num5_0        VAOCAR
120200040708     C                   ELSE
120300040708     C                   SETON                                        31
120400040708     C                   EVAL      VAOCAR = *zeros
120500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120600040708     C                             + ' ' + 'VAOCAR'
120700040708     C                   ENDIF
120800040708     C                   ENDIF
120900040708     C*** CRC
121000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
121100040708     C                   EVAL      PiStr=SkSplitCSV(i)
121200040708     C                   EXSR      CHKNUM
121300040708     C                   IF        PiInt=*on
121400040708     C                   Z-ADD     PiVal         VAOCRC
121500040708     C                   ELSE
121600040708     C                   SETON                                        32
121700040708     C                   EVAL      VAOCRC = *zeros
121800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121900040708     C                             + ' ' + 'VAOCRC'
122000040708     C                   ENDIF
122100040708     C                   ENDIF
122200040708     C*** CAC
122300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
122400040708     C                   EVAL      PiStr=SkSplitCSV(i)
122500040708     C                   EXSR      CHKNUM
122600040708     C                   IF        PiInt=*on
122700040708     C                   Z-ADD     PiVal         Num5_0
122800040708     C                   MOVEL(p)  Num5_0        VAOCAC
122900040708     C                   ELSE
123000040708     C                   SETON                                        32
123100040708     C                   EVAL      VAOCAC = *zeros
123200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123300040708     C                             + ' ' + 'VAOCAC'
123400040708     C                   ENDIF
123500040708     C                   ENDIF
123600040708     C*** DAR
123700050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
123800050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
123900050314     C**!!!              EXSR      CHKNUM
124000050314     C**!!!              IF        PiInt=*on
124100050314     C**!!!              Z-ADD     PiVal         VAODAR
124200050314     C**!!!              ELSE
124300050314     C**!!!              SETON                                        32
124400050314     C**!!!              EVAL      VAODAR = *zeros
124500050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
124600050314     C**!!!                        + ' ' + 'VAODAR'
124700050314     C**!!!              ENDIF
124800050314     C**!!!              ENDIF
124900040708     C*** ORR
125000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
125100040708     C                   EVAL      PiStr=SkSplitCSV(i)
125200040708     C                   EXSR      CHKNUM
125300040708     C                   IF        PiInt=*on
125400040708     C                   Z-ADD     PiVal         VAOORR
125500040708     C                   ELSE
125600040708     C                   SETON                                        32
125700040708     C                   EVAL      VAOORR = *zeros
125800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125900040708     C                             + ' ' + 'VAOORR'
126000040708     C                   ENDIF
126100040708     C                   ENDIF
126200030721     C*** NCL
126300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on
126700040723     C                   IF        PiVal > *zeros
126800040708     C                   Z-ADD     PiVal         VAONCL
126900040723     C                   ENDIF
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200040708     C                   EVAL      VAONCL = *zeros
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400040708     C                             + ' ' + 'VAONCL'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700040708     C*** PKG
127800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiNum=*on
128200040723     C                   IF        PiVal > *zeros
128300040708     C                   Z-ADD     PiVal         VAOPKG
128400040723     C                   ENDIF
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700040708     C                   EVAL      VAOPKG = *zeros
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900040708     C                             + ' ' + 'VAOPKG'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200040708     C*** VLM
129300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
129400040708     C                   EVAL      PiStr=SkSplitCSV(i)
129500040708     C                   EXSR      CHKNUM
129600040708     C                   IF        PiNum=*on
129700040708     C                   Z-ADD     PiVal         VAOVLM
129800040708     C                   ELSE
129900040708     C                   SETON                                        32
130000040708     C                   EVAL      VAOVLM = *zeros
130100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130200040708     C                             + ' ' + 'VAOVLM'
130300040708     C                   ENDIF
130400040708     C                   ENDIF
130500040708     C*** BNC
130600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
130700040708     C                   EVAL      PiStr=SkSplitCSV(i)
130800040708     C                   EXSR      CHKNUM
130900040708     C                   IF        PiInt=*on
131000040708     C                   Z-ADD     PiVal         VAOBNC
131100040708     C                   ELSE
131200040708     C                   SETON                                        32
131300040708     C                   EVAL      VAOBNC = *zeros
131400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131500040708     C                             + ' ' + 'VAOBNC'
131600040708     C                   ENDIF
131700040708     C                   ENDIF
131800040708     C*** BLC
131900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
132000040708     C                   EVAL      PiStr=SkSplitCSV(i)
132100040708     C                   EXSR      CHKNUM
132200040708     C                   IF        PiInt=*on
132300040708     C                   Z-ADD     PiVal         VAOBLC
132400040708     C                   ELSE
132500040708     C                   SETON                                        32
132600040708     C                   EVAL      VAOBLC = *zeros
132700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132800040708     C                             + ' ' + 'VAOBLC'
132900040708     C                   ENDIF
133000040708     C                   ENDIF
133100040708     C*** ATT
133200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
133300040708     C                   EVAL      PiStr=SkSplitCSV(i)
133400040708     C                   EXSR      CHKNUM
133500040708     C                   IF        PiInt=*on
133600040708     C                   Z-ADD     PiVal         VAOATT
133700040708     C                   ELSE
133800040708     C                   SETON                                        32
133900040708     C                   EVAL      VAOATT = *zeros
134000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134100040708     C                             + ' ' + 'VAOATT'
134200040708     C                   ENDIF
134300040708     C                   ENDIF
134400040708     C*** MTC
134500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
134600040708     C                   EVAL      PiStr=SkSplitCSV(i)
134700040708     C                   EXSR      CHKNUM
134800040708     C                   IF        PiInt=*on
134900040708     C                   Z-ADD     PiVal         VAOMTC
135000040708     C                   ELSE
135100040708     C                   SETON                                        32
135200040708     C                   EVAL      VAOMTC = *zeros
135300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135400040708     C                             + ' ' + 'VAOMTC'
135500040708     C                   ENDIF
135600040708     C                   ENDIF
135700040708     C*** KSC
135800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on      AND
136200030721     C                             PiVal<=9999999 AND
136300030721     C                             PiVal>*zeros
136400040708     C                   Z-ADD     PiVal         VAOKSC
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700040708     C                   EVAL      VAOKSC = *zeros
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900040708     C                             + ' ' + 'VAOKSC'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200040708     C*** POR
137300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiInt=*on    AND
137700030721     C                             PiVal<=999   AND
137800030721     C                             PiVal>*zeros
137900040708     C                   Z-ADD     PiVal         VAOPOR
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200040708     C                   EVAL      VAOPOR = *zeros
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400040708     C                             + ' ' + 'VAOPOR'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700040708     C*** POC
138800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
138900040708     C                   EVAL      PiStr=SkSplitCSV(i)
139000040708     C                   EXSR      CHKNUM
139100040708     C                   IF        PiInt=*on    AND
139200040708     C                             PiVal<=999   AND
139300040708     C                             PiVal>*zeros
139400040708     C                   Z-ADD     PiVal         VAOPOC
139500040708     C                   ELSE
139600040708     C                   SETON                                        32
139700040708     C                   EVAL      VAOPOC = *zeros
139800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139900040708     C                             + ' ' + 'VAOPOC'
140000040708     C                   ENDIF
140100040708     C                   ENDIF
140200040708     C*** STO
140300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
140400040708     C                   EVAL      PiStr=SkSplitCSV(i)
140500040708     C                   EXSR      CHKNUM
140600040708     C                   IF        PiInt=*on
140700040708     C                   Z-ADD     PiVal         VAOSTO
140800040708     C                   ELSE
140900040708     C                   SETON                                        32
141000040708     C                   EVAL      VAOSTO = *zeros
141100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141200040708     C                             + ' ' + 'VAOSTO'
141300040708     C                   ENDIF
141400040708     C                   ENDIF
141500110526     C*
141600110526     C*********  Gestione particolarità
141700110526     C*
141800110616     C*** DAR
141900110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
142000150126     C* prevedo la possibilità che la data si in formato ddMMyyyy oppure dd/mm/yyyy
142100150126     C                   SELECT
142200150126     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 7
142300150126     C                             OR %len(%trim(SkSplitCSV(i))) = 8
142400150126     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
142500110630     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
142600110630     C                   ENDIF
142700130417     C* la data è in formato ddMMyyyy
142800130415     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
142900130415     C                                   %subst(SkSplitCSV(i):3:2)+
143000130415     C                                   %subst(SkSplitCSV(i):1:2)
143100150126
143200150126     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 9
143300150126     C                             OR %len(%trim(SkSplitCSV(i))) = 10
143400150126     C                   IF        %len(%trim(SkSplitCSV(i))) = 9
143500150126     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
143600150126     C                   ENDIF
143700150126     C* la data è in formato dd/MM/yyyy
143800150126     C                   EVAL      PiStr=%subst(SkSplitCSV(i):7:4)+
143900150126     C                                   %subst(SkSplitCSV(i):4:2)+
144000150126     C                                   %subst(SkSplitCSV(i):1:2)
144100150126     C                   ENDSL
144200150126
144300110616     C                   EXSR      CHKNUM
144400110616     C                   IF        PiInt=*on
144500130405     C                   Z-ADD     PiVal         VAODAR
144600110616     C                   ELSE
144700110616     C                   SETON                                        32
144800110616     C                   EVAL      VAODAR = *zeros
144900110616     C                   EVAL      vinmsg = %trimr(vinmsg)
145000110616     C                             + ' ' + 'VAODAR'
145100110616     C                   ENDIF
145200110616     C                   ENDIF
145300110616     C*** ORR
145400110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOORR'
145500130405     C                   EVAL      PiStr=%subst(SkSplitCSV(i):1:2)+
145600130405     C                                   %subst(SkSplitCSV(i):4:2)
145700110630     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
145800110616     C                   EXSR      CHKNUM
145900110616     C                   IF        PiInt=*on
146000110616     C                   Z-ADD     PiVal         VAOORR
146100110616     C                   ELSE
146200110616     C                   SETON                                        32
146300110616     C                   EVAL      VAOORR = *zeros
146400110616     C                   EVAL      vinmsg = %trimr(vinmsg)
146500110616     C                             + ' ' + 'VAOORR'
146600110616     C                   ENDIF
146700110616     C                   ENDIF
146800120605     C*
146900120605     C*** RER 1 e 2
147000120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORER_1'
147100120605     C                   EVAL      wRER_1=%trim(SkSplitCSV(i))
147200120605     C                   ENDIF
147300120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORER_2'
147400120605     C                   EVAL      wRER_2=%trim(SkSplitCSV(i))
147500120605     C                   ENDIF
147600130416     C*
147700130416     C*** TER 1 e 2
147800130416     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOTER_1'
147900130416     C                   EVAL      wTER_1=%trim(SkSplitCSV(i))
148000130416     C                   ENDIF
148100130416     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOTER_2'
148200130416     C                   EVAL      wTER_2=%trim(SkSplitCSV(i))
148300130416     C                   ENDIF
148400110526     C*
148500110526     C*********
148600030715     C*
148700030715     C                   ENDIF
148800030715     C                   EVAL      i = i + 1
148900030715     C                   ENDDO
149000120605     C*
149100120605     C***  ===>  Gestione campi "particolari" fuori ciclo
149200120605     C*
149300130522     C***                EVAL      VAORER=%trim(wRER_1) + %trim(wRER_2)
149400130522     C***                EVAL      VAOTER=%trim(wTER_1) + %trim(wTER_2)
149500040723     C*
149600030715     C                   ENDIF
149700020204     C*
149800010607     C                   ENDSR
149900010607     C*----------------------------------------------------*
150000010330
150100100512
150200100512
150300100512     C*------------------------------------------------------------------------*
150400100512     C* EXEFORZ - Considerazioni finali
150500100512     C*------------------------------------------------------------------------*
150600100512     C     EXEFORZ       BEGSR
150700100512     C*
150800100512     C                   if        VAOKSC = *zeros
150900100512     C                   select
151000100512     C                   when      VAOPAG = 'O'
151100100512     C                   movel     VAOCOR        VAOKSC
151200100512     C                   when      VAOPAG = 'M'
151300100512     C                   movel     VAOCRA        VAOKSC
151400100512     C                   when      VAOPAG = 'D'
151500100512     C                   movel     VAOCRC        VAOKSC
151600100512     C                   endsl
151700100512     C                   endif
151800100512     C*
151900100512     C                   ENDSR
152000080702
152100080702
152200080702     C*----------------------------------------------------*
152300080702     C*  CONTROLLO NUMERICITA' CAMPI
152400080702     C*----------------------------------------------------*
152500080702     C     CHKNUM        BEGSR
152600080702     C*
152700080702     C                   IF        PiDecChr = *blanks
152800080702     C                   EVAL      PiDecChr = ','
152900080702     C                   ENDIF
153000080702     C*
153100080702     C                   callp     UBISNUM_Check(PiStr
153200080702     C                                          :PiDecChr
153300080702     C                                          :PiVal
153400080702     C                                          :PiNum
153500080702     C                                          :PiInt)
153600080702     C*
153700080702     C                   ENDSR
153800080702     C***
153900010601
154000080702
154100010601
154200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
154300010601     C     repfil        BEGSR
154400010601     C*
154500010601     C                   if        invfil = *zeros and
154600010601     C                             depfil > *zeros and
154700010629     C                             (vinflg = *blanks or
154800010629     C                              vinflg = *zeros)
154900010601     C
155000010601     C                   eval      invfil = depfil
155100010601     C                   endif
155200010601     C*
155300010601     C                   if        depfil <> invfil and
155400010601     C                             invfil > *zeros
155500010601     C                   eval      flgMulti = '1'
155600010601     C                   if        vinflg = *blanks
155700010601     C                   add       1             cntNonEl
155800010601     C                   endif
155900010601     C                   endif
156000010601     C*
156100010601     C                   if        vinflg = '2'
156200010601     C                   eval      flgStato = '2'
156300010601     C                   endif
156400010601     C*
156500010601     C                   ENDSR
156600010601     C***
156700010601
156800010601
156900010601
157000010330
157100010330
157200010330
157300990920      /TITLE Invio dei dati al punto operativo.
157400000613     C     invio         BEGSR
157500990920     C*
157600990920     C                   reset                   dscmz
157700010601     C                   move      invfil        cmzdst
157800990920     C                   eval      cmzfld = vlrfou
157900990920     C                   eval      cmzmbd = vlrhdl
158000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
158100000710     C                   if        prmfir = *blanks
158200040708     C                   eval      cmzfla = 'FNVAO00F'
158300040708     C                   eval      cmzmba = 'FNVAO00F'
158400000710     C                   else
158500000710     C                   eval      cmzfla = prmfir
158600000710     C                   eval      cmzmba = prmfir
158700000710     C                   endif
158800990920     C                   eval      cmznrr = *zeros
158900990920     C                   move      §ctrok        cmznrr
159000021018     C                   eval      cmzlba = vlrfl1
159100990920     C                   call(e)   'TIS711C'
159200990920     C                   parm                    dscmz
159300990921     C                   parm      *blanks       esito
159400990920     C                   if        %error
159500990920     C                             or cmzerr = '1'
159600990921     C                             or esito  = '1'
159700000710     C                   eval      wrkesito = '3'
159800990920     C                   endif
159900990920     C*
160000000613     C                   ENDSR
160100990910
160200010601
160300010601
160400010601
160500010601
160600010601      /TITLE Invio dei dati al punto operativo.
160700010601     C     opeini        BEGSR
160800010601     C*
160900010601     C* Inizializzo flag e contatori operativi
161000010601     C                   movel     '0'           flgGiro           1
161100010601     C                   movel     '0'           flgMulti          1
161200010601     C                   movel     '1'           flgStato          1
161300010615     C                   movel     '0'           flgOk             1
161400010601     C                   z-add     *zeros        cntNonEl         10 0
161500010601     C                   z-add     *zeros        depfil            3 0
161600010601     C                   z-add     *zeros        invfil            3 0
161700010601     C*
161800010601     C                   ENDSR
161900010601     C***
162000010601
162100010601
162200050421
162300050421
162400050421     C     *pssr         BEGSR
162500050421     C*
162600050421     C                   eval      wrkesito = '2'
162700050421     C*
162800050421     C                   ENDSR     '*CANCL'
162900050421     C***
163000060306
163100060306
163200060306     C*-------------------------------------------------------------------------*
163300060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
163400060306     C*-------------------------------------------------------------------------*
163500060306     C     sr_Control    begsr
163600060306     C*
163700060306     C* inizializzo a ok il flag d controllo abilitazione
163800060306     C                   eval      Cli_Abil = *on
163900060306     C
164000060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
164100060306     C                   clear                   woriIDC
164200060306     C                   eval      kteCOD = 'VAP'
164300060306     C                   eval      kteKE1 = oriIDC
164400060306     C     keytbe        chain     TNTBE01L
164500060306     C                   if        %found(Tntbe01l)
164600060306     C                   eval      woriIDC = TbeKe2
164700060306     C                   else
164800060306     C                   eval      Cli_Abil = *off
164900060306     C                   endif
165000060306     C*
165100060306     C                   endsr
165200050421
165300010601
165400010330
165500010330
165600000613     C     *inzsr        BEGSR
165700990910     C*
165800990910     C     *entry        plist
165900990920     C                   parm                    tivlrds
166000990921     C                   parm      wrkesito      esito
166100000724     C                   parm                    prmlit
166200000710     C                   parm                    prmfir
166300010330     C*
166400010330     C* CALCOLA LA DATA CORRENTE
166500100223     C                   time                    wn14             14 0
166600100223     C                   movel     wn14          wn6               6 0          *ORA (6)
166700100223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
166800100223     C                   eval      datcor = %dec(%date() : *ISO)
166900040723     C*
167000040723     C* Campi riferito al database
167100040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
167200040723     C     *like         define    tbeke1        kteKE1
167300040723     C*
167400040723     C* Definizione chiavi di lettura
167500040723     C* TNTBE01L - parziale
167600040723     C     keytbe        klist
167700040723     C                   kfld                    kteCOD                         *UTENTE
167800040723     C                   kfld                    kteKE1                         *CODICE
167900000613     C*
168000000613     C                   ENDSR
168100000613     C***
168200990908
168300040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168400130522X;X;X;VAORSR;X;VAOINR;VAOLOR;VAOCAR;VAOPRR;X;VAOTER;
168500150123VAORER;X;VAORFA;VAOPKG;VAONCL;W_VAODAR;W_VAOORR;X;VAORSC;X;
168600130415VAOINC;VAOLOC;VAOCAC;VAOPRC;X;X;X;X;VAONO1;
168700111109
168800111109
