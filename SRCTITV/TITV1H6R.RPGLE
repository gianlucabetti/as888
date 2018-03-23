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
008900080702
009000031201     D*------------------
009100031201     D* Costanti
009200031201     D*------------------
009300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009500010330
009600010330
009700010330
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300041011     C                   exsr      PREELA
010400040708     C                   exsr      rwvao
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700041011     C                   if        flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100010601     C*
012200120306     C                   endif
012300120306     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601
012900010601
013000010601
013100010601
013200010601
013300010601     C*--------------------------------------------------------
013400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013500010601     C*--------------------------------------------------------
013600010601     C     ENDELA        BEGSR
013700080707     C*
013800080707     C* Chiusura TRUL13R
013900080707     C                   clear                   TRUL13DS
014000080707     C                   eval      I13TLA = 'C'
014100080707     C                   call      'TRUL13R'
014200080707     C                   parm                    TRUL13DS
014300000616     C*
014400010601     C                   ENDSR
014500010601     C***
014600000613
014700010330
014800010601
014900010601
015000010601
015100010330     C*--------------------------------------------------------
015200040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
015300010330     C*--------------------------------------------------------
015400040708     C     RWVAO         BEGSR
015500010330     C*
015600010330     C                   if        not %open(tivin00r)
015700010330     C                   open      tivin00r
015800010330     C                   endif
015900040708     C                   if        not %open(fnvaowwr)
016000040708     C                   open      fnvaowwr
016100010330     C                   endif
016200010330     C*
016300010604     C                   clear                   §CTROK
016400010604     C                   clear                   §CTRMO
016500010604     C                   clear                   §CTRNO
016600010330     C*
016700030822     C                   do        *HIVAL
016800010330     C*
016900030822     C                   read      tivin00r                               70
017000010618     C*
017100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017200010618     C                   if        vinflg = '1'
017300010618     C                   eval      flgOk = '1'
017400010618     C                   endif
017500010618     C*
017600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017700040510     C                   z-add     *zeros        wLenVINDTA        4 0
017800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018200040510     C                   z-add     1             wFlgVINDTA
018300040510     C                   leave
018400040510     C                   endif
018500040510     C                   enddo
018600040510     C*
018700040218     C                   if        vindta > *blanks AND
018800040510     C                             wFlgVINDTA = 1
018900010330     C                   add       1             rrnum
019000010330     C*
019100010601     C                   if        *in70 = *off and
019200010330     C                             (vinflg = *blanks
019300010330     C                              or vinflg = '0'
019400010330     C                              or vinflg = '2')
019500010330     C*
019600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019700010711     C                   if        vinflg = *blanks or vinflg = '0'
019800010711     C                   clear                   vinmsg
019900010711     C                   endif
020000010601     C*
020100010330     C                   exsr      inzvar
020200040708     C                   exsr      impvao
020300100512     C                   exsr      exeforz
020400010601     C*
020500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020600040708     C                   eval      depfil = VAOPOE
020700010601     C                   exsr      repfil
020800010601     C                   if        depfil = invfil
020900021025     C                   if        vlrpoi = 999
021000040708     C                   move(P)   invfil        VAOPOE
021100021025     C                   else
021200040708     C                   move(P)   vlrpoi        VAOPOE
021300021025     C                   endif
021400010601     C*
021500010604     C* Ebbene...
021600010604     C*
021700030822     C  N31              add       1             §CTROK            7 0
021800030822     C   32              add       1             §CTRMO            7 0
021900030822     C   31              add       1             §CTRNO            7 0
022000030822     C                   if        wGiro = 2
022100040723     C  N31              EXSR      CHKCOR
022200040723     C  N31              EXSR      WRIORI
022300050826     C  N31              EXSR      WRIVGD
022400040723     C  N31              write     FNVAO000
022500030822     C                   endif
022600010604     C*
022700010604     C                   if        *in31 = *off and
022800010604     C                             *in32 = *off
022900010604     C                   eval      vinflg = '1'
023000010604     C                   else
023100010604     C                   eval      vinflg = '2'
023200010604     C                   endif
023300010604     C                   endif
023400010601     C*
023500010604     C                   endif
023600010604     C*
023700010330     C                   else
023800010330     C                   eval      vinflg = '1'
023900010330     C                   endif
024000010601     C*
024100010601     C  N70              update    tivin000
024200010330     C*
024300030822     C  N70              enddo
024400010601     C
024500010601     C*
024600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024700010601     C                   if        cntNonEl = *zeros or
024800010601     C                             flgMulti = '0'
024900010330     C* Se non ci sono record con errori ...
025000010601     C                   if        §ctrno = 0 and
025100010604     C                             §ctrmo = 0 and
025200010601     C                             flgStato <> '2'
025300010330     C* ... restituisco esito OK.
025400010330     C                   eval      wrkesito = '0'
025500010330     C                   else
025600010330     C                   if        §ctrok > 0
025700010330     C                   eval      wrkesito = '1'
025800010330     C                   else
025900010615     C                   if        flgOk = '0'
026000010615     C                   eval      wrkesito = '2'
026100010615     C                   else
026200010615     C                   eval      wrkesito = '6'
026300010615     C                   endif
026400010330     C                   endif
026500010330     C                   endif
026600010601     C                   else
026700010601     C                   eval      wrkesito = '9'
026800010601     C                   endif
026900010330     C*
027000010330     C                   if        %open(tivin00r)
027100010330     C                   close     tivin00r
027200010330     C                   endif
027300040708     C                   if        %open(fnvaowwr)
027400040708     C                   close     fnvaowwr
027500010330     C                   endif
027600010601     C*
027700010601     C                   if        vlrpoi <> 999
027800010601     C                   eval      invfil = vlrpoi
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        §ctrok > 0
028200010601     C                             and invfil > *zeros
028300010330     C                   exsr      invio
028400010330     C                   endif
028500010601     C*
028600010618     C                   if        flgGiro = '1'
028700010601     C                   exsr      endela
028800010618     C                   endif
028900010330     C*
029000010330     C                   ENDSR
029100010330     C***
029200040723
029300040723
029400040723     C*------------------------------------------------------------------------*
029500040723     C* CHKCOR - Considerazioni su VAOCOR
029600040723     C*------------------------------------------------------------------------*
029700040723     C     CHKCOR        BEGSR
029800040723     C*
029900040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
030000040723     C                   if        vaoTCO = 'F'
030100040723     C* Quindi verifico se il codice ordinante non è valorizzato
030200040723     C                   if        vaoCOR = *zeros  AND
030300040723     C                             vaoRSO = *blanks AND
030400040723     C                             vaoINO = *blanks AND
030500040723     C                             vaoCAO = *blanks AND
030600040723     C                             vaoLOO = *blanks AND
030700040723     C                             vaoPRO = *blanks AND
030800040723     C                             vaoNAO = *blanks
030900040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
031000040723     C                   movel     '0'           wFlgAcr           1
031100040723     C                   move      vlrKSC        wCli              7 0
031200040723     C                   move(p)   wCli          acrCRO
031300040723     C     acrCRO        setll     fnacr01l
031400040723     C                   if        %found(fnacr01l)
031500040723     C                   read      fnacr01l
031600040723     C                   dow       not %eof(fnacr01l)
031700040723     C                   movel     acrCRO        wCliAcr           7 0
031800040723     C                   if        wCliAcr = wCli
031900040723     C                   eval      vaoCOR = acrCRO
032000040723     C                   movel     '1'           wFlgAcr
032100040723     C                   leave
032200040723     C                   else
032300040723     C                   leave
032400040723     C                   endif
032500040723     C                   read      fnacr01l
032600040723     C                   enddo
032700040723     C                   endif
032800040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
032900040723     C* dati anagrafici dell'ordinante dal piano dei conti
033000040723     C                   if        wFlgAcr = '0'
033100040723     C                   clear                   BS69DS
033200040723     C                   clear                   ACODS
033300040723     C                   clear                   INDDS
033400040723     C                   clear                   CLPDS
033500040723     C                   clear                   CLSDS
033600040723     C                   move(P)   vlrKSC        I69KAC
033700040723     C                   move(P)   vlrKSC        I69KIN
033800040723     C                   call      'TIBS69R'
033900040723     C                   parm                    BS69DS
034000040723     C                   parm                    ACODS
034100040723     C                   parm                    INDDS
034200040723     C                   parm                    CLPDS
034300040723     C                   parm                    CLSDS
034400040723     C     O69ERR        ifne      '1'
034500040723     C                   eval      vaoRSO = ACORAG
034600040723     C                   eval      vaoINO = INDVIA
034700040723     C                   movel(P)  INDCAP        vaoCAO
034800040723     C                   eval      vaoLOO = INDCIT
034900040723     C                   eval      vaoPRO = INDPRV
035000040723     C                   eval      vaoNAO = INDSTA
035100040723     C                   endif
035200040723     C                   endif
035300040723     C                   endif
035400040723     C                   endif
035500040723     C*
035600040723     C                   ENDSR
035700040723
035800040723
035900040723
036000040723     C*------------------------------------------------------------------------*
036100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
036200040723     C*------------------------------------------------------------------------*
036300040723     C     WRIORI        BEGSR
036400040723     C*
036500090617     C* Come prima cosa stacco un numeratore da AZNUM
036600090617     C                   clear                   trul33ds
036700090617     C                   eval      I33OPE = *zeros
036800090617     C                   eval      I33CNU = 600
036900090617     C                   eval      I33NUM = 1
037000090617     C                   movel     TRUL33DS      KPJBU
037100090617     C                   call      'TRUL33R'
037200090617     C                   parm                    KPJBA
037300090617     C                   movel     KPJBU         TRUL33DS
037400090617     C                   if        O33ERR = *zeros
037500090617     C                   z-add     O33NRF        §numori
037600090618     c                   else
037700090618     c                   eval      §numori = *all'9'
037800090617     C                   endif
037900040723     C*
038000040723     C* Quindi imposto i campi "extra-VAO"
038100040723     C                   if        vlrKSC = '0ORM0000'
038200040723     C*
038300040723     C                   if        vaoCOR > *zeros
038400040723     C                   movel     vaoCOR        wrkCOR7           7 0
038500040723     C*
038600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
038700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
038800040723     C                   clear                   tibs10ds
038900040723     C                   eval      d10drf = datcor                              *data riferimento
039000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
039100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
039200091026     C*
039300091026     C* Preparazione chiamata al TIBS10R1
039400091026     C                   clear                   tibs10r1ds
039500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
039600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
039700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
039800091026     C*
039900091026     C                   call      'TIBS10R1'
040000091026     C                   parm                    tibs10r1ds
040100091026     C*
040200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
040300040723     C*
040400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
040500040723     C                             d10cop =  *zeros  or
040600040723     C                             d10cop =  d10cod
040700040723     C                   move      d10cod        oriIDC
040800040723     C                   else                                                   *Figlio
040900040723     C                   move      d10cop        oriIDC
041000040723     C                   endif
041100040723     C*
041200040723     C                   else
041300040723     C                   eval      oriIDC = *zeros
041400040723     C                   endif
041500040723     C*
041600040723     C                   else
041700040723     C                   eval      oriIDC = vlrKSC
041800040723     C                   endif
041900040723     C*
042000040723     C                   eval      oriPRG = §numori
042100040723     C                   eval      oriDER = datcor
042200040723     C                   eval      oriOER = wn6
042300040723     C                   eval      oriPOE = vaoPOE
042400040723     C                   eval      oriNSR = vaoNSR
042500040723     C                   eval      oriNOR = vaoNOR
042600040723     C                   eval      oriNRV = vaoNRV
042700040723     C                   eval      oriFLO = *blanks
042800040723     C*
042900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
043000040723     C                   eval      dorm01 = vaoFLO
043100040723     C                   movel     §numori       §ormpg
043200050412     C*
043300050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
043400050412     C                   movel     'S'           §ormfr
043500050412     C*
043600050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
043700040723     C                   eval      vaoFLO = dorm01
043800040723     C*
043900040723     C* ...quindi scrivo il file TIORI00F
044000040723     C                   WRITE     TIORI000
044100040723     C*
044200040723     C                   ENDSR
044300050826
044400050826
044500050826
044600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
044700050826     C     wriVGD        BEGSR
044800050826     C*
044900050826     C* Reperisco la descrizione della fase da tabella
045000050826     C                   movel(p)  'FAR'         KteCOD
045100050826     C                   movel(p)  '000'         KteKE1
045200050826     C     KEYtbe        chain     tntbe01l
045300050826     C                   if        %found(tntbe01l)
045400050826     C                   movel(P)  tbeuni        DFAR
045500050826     C                   else
045600050826     C                   clear                   DFAR
045700050826     C                   endif
045800050826     C*
045900050826     C* Valorizzo buffer record
046000050826     C                   clear                   FNVAPDS
046100050826     C                   eval      vapIDC = oriIDC
046200050826     C                   eval      vapPOE = oriPOE
046300050826     C                   eval      vapRFA = vaoRFA
046400050826     C                   eval      vapPOG = oriPOE
046500050826     C                   eval      vapDAE = oriDER
046600050826     C                   eval      vapORE = oriOER
046700050826     C                   if        d§fardva <> *blanks
046800050826     C                   eval      vapDFA = d§fardva
046900050826     C                   else
047000050826     C                   eval      vapDFA = d§fardes
047100050826     C                   endif
047200050826     C*
047300050826     C                   clear                   tivgd000
047400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
047500050826     C                   eval      vgdTIP = 'VP'
047600050826     C                   eval      vgdKSU = vapIDC
047700050826     C                   eval      vgdTSC = 'WW'
047800050826     C                   eval      vgdDAT = datcor
047900080702     C                   eval      vgdPGM = 'TITV1H6R'
048000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
048100060306     C                   exsr      sr_Control
048200060306     C                   if        Cli_Abil = *on
048300060306     C                   eval      vgdKSU = woriIDC
048400050826     C                   write     tivgd000
048500060306     C                   endif
048600050826     C*
048700050826     C                   ENDSR
048800050826     C*------------------------------------------------------------------------*
048900010601
049000010601
049100010601
049200010330     C*----------------------------------------------------*
049300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049400010330     C*----------------------------------------------------*
049500010330     C     INZVAR        BEGSR
049600010330     C*
049700030822     C* Inizializzo variabili di wrk
049800020204     C                   Z-ADD     *zeros        Num5_0            5 0
049900030822     C*
050000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050100040708     C                   CLEAR                   FNVAO000
050200040723     C                   CLEAR                   TIORI000
050300030822     C                   CLEAR                   SkSplitCSV
050400030822     C                   EVAL      i = 1
050500030822     C                   EVAL      posDa = *zeros
050600030822     C                   EVAL      posA  = *zeros
050700030822     C*
050800030822     C* Reimposto i valori di default
050900030822     C                   EXSR      DEFCAM
051000030822     C*
051100010330     C                   ENDSR
051200010330     C*----------------------------------------------------*
051300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051400010330     C*----------------------------------------------------*
051500010330     C     DEFCAM        BEGSR
051600030822     C*
051700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051800030715     C* e delimitatore testo.
051900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052200030822     C*
052300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052500030715     C                   SETON                                        50
052600030715     C                   ELSE
052700030715     C                   SETOFF                                       50
052800030715     C                   ENDIF
052900040723     C*
053000080707     C* Imposto i valori d default d alcuni campi
053100100225     C                   EVAL      VAOCOR = 0594068102
053200100225     C                   EVAL      VAOCRC = 0594068102
053300080704     C                   EVAL      VAOCTR = '100'
053400080704     C                   EVAL      VAOORR = 1600
053500080708     C                   EVAL      VAOPOE = 059
053600080708     C*
053700080708     C                   CLEAR                   DORM01
053800160107     C                   EVAL      §ORCOMC = 'N'
053900080708     C                   EVAL      VAOFLO = DORM01
054000100226     C***                EVAL      VAODAR = %dec(%date() + %Days(1) : *ISO)
054100110621     C***                EVAL      VAODAR = datcor
054200080707     C*
054300080707     C                   EVAL      VAOPAG = 'D'
054400080707     C                   EVAL      VAODDT = 'S'
054500080707     C                   EVAL      VAOTCO = 'F'
054600080707     C                   EVAL      VAOTOR = 'S'
054700030822     C*
054800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054900030822     C                   EVAL      posDaDft = 1
055000030822     C                   EVAL      posADft  = 0
055100030822     C                   EVAL      wGiroDft = 0
055200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055300030822     C                             posDaDft > 0
055400030822     C*
055500030822     C* Gestisco il 1° giro
055600030822     C                   IF        wGiroDft = 0
055700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055900030822     C* Incremento il contatore dei "giri"
056000030822     C                   EVAL      wGiroDft = 1
056100030822     C                   ELSE
056200030822     C                   EVAL      posDaDft = posADft
056300030822     C                   ENDIF
056400030822     C* Eseguo lo scan x trovare la fine del campo corrente
056500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056600030822     C*
056700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056900030822     C                   IF        posDaDft > 0 AND
057000030822     C                             posADft  > 0
057100040708     C* POE
057200040708     C                   IF        %subst(
057300040708     C                             %subst(vlrppt:posDaDft+1:
057400040708     C                             posADft-posDaDft-1):1:3)
057500040708     C                             = 'POE'
057600040708     C                   EVAL      PiStr=%trim(%subst(
057700040708     C                             %subst(vlrppt:posDaDft+1:
057800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900040708     C                   EXSR      CHKNUM
058000040708     C                   IF        PiInt=*on
058100040708     C                   Z-ADD     PiVal         VAOPOE
058200040708     C                   ENDIF
058300040708     C                   ENDIF
058400040708     C* TOR
058500040708     C                   IF        %subst(
058600040708     C                             %subst(vlrppt:posDaDft+1:
058700040708     C                             posADft-posDaDft-1):1:3)
058800040708     C                             = 'TOR'
058900040708     C                   EVAL      VAOTOR=%trim(%subst(
059000040708     C                             %subst(vlrppt:posDaDft+1:
059100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200040708     C                   ENDIF
059300040708     C* TCO
059400040708     C                   IF        %subst(
059500040708     C                             %subst(vlrppt:posDaDft+1:
059600040708     C                             posADft-posDaDft-1):1:3)
059700040708     C                             = 'TCO'
059800040708     C                   EVAL      VAOTCO=%trim(%subst(
059900040708     C                             %subst(vlrppt:posDaDft+1:
060000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100040708     C                   ENDIF
060200040707     C* COR
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600040707     C                             = 'COR'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200040708     C                   Z-ADD     PiVal         VAOCOR
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500040708     C* CRC
061600040708     C                   IF        %subst(
061700040708     C                             %subst(vlrppt:posDaDft+1:
061800040708     C                             posADft-posDaDft-1):1:3)
061900040708     C                             = 'CRC'
062000040708     C                   EVAL      PiStr=%trim(%subst(
062100040708     C                             %subst(vlrppt:posDaDft+1:
062200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300040708     C                   EXSR      CHKNUM
062400040708     C                   IF        PiInt=*on
062500040708     C                   Z-ADD     PiVal         VAOCRC
062600040708     C                   ENDIF
062700040708     C                   ENDIF
062800040708     C* FFD
062900040708     C                   IF        %subst(
063000040708     C                             %subst(vlrppt:posDaDft+1:
063100040708     C                             posADft-posDaDft-1):1:3)
063200040708     C                             = 'FFD'
063300040708     C                   EVAL      VAOFFD=%trim(%subst(
063400040708     C                             %subst(vlrppt:posDaDft+1:
063500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600040708     C                   ENDIF
063700040708     C* ORR
063800040708     C                   IF        %subst(
063900040708     C                             %subst(vlrppt:posDaDft+1:
064000040708     C                             posADft-posDaDft-1):1:3)
064100040708     C                             = 'ORR'
064200040708     C                   EVAL      PiStr=%trim(%subst(
064300040708     C                             %subst(vlrppt:posDaDft+1:
064400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500040708     C                   EXSR      CHKNUM
064600040708     C                   IF        PiInt=*on
064700040708     C                   Z-ADD     PiVal         VAOORR
064800040708     C                   ENDIF
064900040708     C                   ENDIF
065000040712     C* NCL
065100040712     C                   IF        %subst(
065200040712     C                             %subst(vlrppt:posDaDft+1:
065300040712     C                             posADft-posDaDft-1):1:3)
065400040712     C                             = 'NCL'
065500040712     C                   EVAL      PiStr=%trim(%subst(
065600040712     C                             %subst(vlrppt:posDaDft+1:
065700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800040712     C                   EXSR      CHKNUM
065900040712     C                   IF        PiInt=*on
066000040712     C                   Z-ADD     PiVal         VAONCL
066100040712     C                   ENDIF
066200040712     C                   ENDIF
066300040712     C* PKG
066400040712     C                   IF        %subst(
066500040712     C                             %subst(vlrppt:posDaDft+1:
066600040712     C                             posADft-posDaDft-1):1:3)
066700040712     C                             = 'PKG'
066800040712     C                   EVAL      PiStr=%trim(%subst(
066900040712     C                             %subst(vlrppt:posDaDft+1:
067000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100040712     C                   EXSR      CHKNUM
067200040712     C                   IF        PiNum=*on
067300040712     C                   Z-ADD     PiVal         VAOPKG
067400040712     C                   ENDIF
067500040712     C                   ENDIF
067600040712     C* VLM
067700040712     C                   IF        %subst(
067800040712     C                             %subst(vlrppt:posDaDft+1:
067900040712     C                             posADft-posDaDft-1):1:3)
068000040712     C                             = 'VLM'
068100040712     C                   EVAL      PiStr=%trim(%subst(
068200040712     C                             %subst(vlrppt:posDaDft+1:
068300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400040712     C                   EXSR      CHKNUM
068500040712     C                   IF        PiNum=*on
068600040712     C                   Z-ADD     PiVal         VAOVLM
068700040712     C                   ENDIF
068800040712     C                   ENDIF
068900040708     C* NAM
069000040708     C                   IF        %subst(
069100040708     C                             %subst(vlrppt:posDaDft+1:
069200040708     C                             posADft-posDaDft-1):1:3)
069300040708     C                             = 'NAM'
069400040708     C                   EVAL      VAONAM=%trim(%subst(
069500040708     C                             %subst(vlrppt:posDaDft+1:
069600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700040708     C                   ENDIF
069800040708     C* PAG
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200040708     C                             = 'PAG'
070300040708     C                   EVAL      VAOPAG=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700040708     C* KSC
070800040708     C                   IF        %subst(
070900040708     C                             %subst(vlrppt:posDaDft+1:
071000040708     C                             posADft-posDaDft-1):1:3)
071100040708     C                             = 'KSC'
071200040708     C                   EVAL      PiStr=%trim(%subst(
071300040708     C                             %subst(vlrppt:posDaDft+1:
071400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500040708     C                   EXSR      CHKNUM
071600040708     C                   IF        PiInt=*on
071700040708     C                   Z-ADD     PiVal         VAOKSC
071800040708     C                   ENDIF
071900040708     C                   ENDIF
072000040708     C* CTR
072100040708     C                   IF        %subst(
072200040708     C                             %subst(vlrppt:posDaDft+1:
072300040708     C                             posADft-posDaDft-1):1:3)
072400040708     C                             = 'CTR'
072500040708     C                   EVAL      VAOCTR=%trim(%subst(
072600040708     C                             %subst(vlrppt:posDaDft+1:
072700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800040708     C                   ENDIF
072900040708     C* POC
073000040708     C                   IF        %subst(
073100040708     C                             %subst(vlrppt:posDaDft+1:
073200040708     C                             posADft-posDaDft-1):1:3)
073300040708     C                             = 'POC'
073400040708     C                   EVAL      PiStr=%trim(%subst(
073500040708     C                             %subst(vlrppt:posDaDft+1:
073600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700040708     C                   EXSR      CHKNUM
073800040708     C                   IF        PiInt=*on
073900040708     C                   Z-ADD     PiVal         VAOPOC
074000040708     C                   ENDIF
074100040708     C                   ENDIF
074200040708     C* NO1
074300040708     C                   IF        %subst(
074400040708     C                             %subst(vlrppt:posDaDft+1:
074500040708     C                             posADft-posDaDft-1):1:3)
074600040708     C                             = 'NO1'
074700040708     C                   EVAL      VAONO1=%trim(%subst(
074800040708     C                             %subst(vlrppt:posDaDft+1:
074900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000040708     C                   ENDIF
075100040708     C* DDT
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500040708     C                             = 'DDT'
075600040708     C                   EVAL      VAODDT=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000040708     C* RFA
076100040708     C                   IF        %subst(
076200040708     C                             %subst(vlrppt:posDaDft+1:
076300040708     C                             posADft-posDaDft-1):1:3)
076400040708     C                             = 'RFA'
076500040708     C                   EVAL      VAORFA=%trim(%subst(
076600040708     C                             %subst(vlrppt:posDaDft+1:
076700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800040708     C                   ENDIF
076900040708     C* FLO
077000040708     C                   IF        %subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):1:3)
077300040708     C                             = 'FLO'
077400040708     C                   EVAL      VAOFLO=%trim(%subst(
077500040708     C                             %subst(vlrppt:posDaDft+1:
077600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
077800080702     C* e posizione 7 del VAOFLO
077900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
078000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
078100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
078200040708     C                   ENDIF
078300030822     C* ...
078400030822     C                   ENDIF
078500030822     C                   ENDDO
078600020204     C*
078700010330     C                   ENDSR
078800010607     C*----------------------------------------------------*
078900040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
079000010607     C*----------------------------------------------------*
079100040708     C     IMPVAO        BEGSR
079200010607     C*
079300010607     C                   SETOFF                                       3132
079400030515     C*
079500100223     C                   MOVEL     datcor        VAODAO
079600040708     C                   MOVEL     wn14          VAOOAO
079700050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079800050314     C     minu:maiu     XLATE     vindta        vindta
079900010607     C*
080000030715     C**********
080100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080200030715     C**********
080300030715     C                   IF        wGiro = *zeros
080400040709     C*
080500040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080600040709     C                   EVAL      depvinDTA = vinDTA
080700040709     C*
080800040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
080900040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081000110616     C                                      %trim(intcol(3))+%trim(intcol(4))+
081100110616     C                                      %trim(intcol(5))
081200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081300050314     C**!!!minu:maiu     XLATE     vindta        vindta
081400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081500040324     C                   DOW       posDa <= %len(%trimr(vindta))
081600030715     C*
081700030715     C* Gestisco il 1° campo
081800030715     C                   IF        i = 1
081900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082000030715     C                   EVAL      posDa = 1
082100030715     C* Eseguo lo scan x trovare la fine del primo campo
082200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C* X i campi successivi al 1°
082700030715     C                   ELSE
082800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082900030715     C                   EVAL      posDa = posA + 1
083000030715     C* Eseguo lo scan x trovare la fine del campo corrente
083100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200030715     C* Gestisco l'ultimo campo
083300030715     C                   IF        posA = *zeros
083400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
083500030715     C* A questo punto "estraggo" il campo corrente
083600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083700030715     C                                                    (posA-posDa))
083800030715     C                   LEAVE
083900030715     C                   ELSE
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C                   ENDIF
084400030715     C                   ENDIF
084500030715     C* Incremento il contatore d campo
084600030715     C                   EVAL      i = i +1
084700030715     C                   ENDDO
084800030715     C*
084900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085000030715     C* delimitazione testo
085100030715     C                   EVAL      i = 1
085200030715     C                   DOW       i < %elem(SkSplitFLD)
085300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085500030715     C                   EVAL      i = i + 1
085600030715     C                   ENDDO
085700030715     C                   EVAL      wGiro = 1
085800040709     C* Ripristino la 1° riga originale
085900040709     C                   EVAL      vinDTA = depvinDTA
086000030715     C                   ELSE
086100030715     C                   EVAL      wGiro = 2
086200031204     C**********
086300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086400031204     C* d separatore campo
086500031204     C**********
086600040709     C                   EVAL      vindta = %trim(vindta)
086700040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086800040709     C                   EVAL      vindta = %subst(vindta:2)
086900040709     C                   ENDDO
087000040709     C*
087100040709     C                   Z-ADD     *zeros        lunghInput        4 0
087200040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087300040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087400040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087600040709     C                   ENDDO
087700030715     C**********
087800030715     C* Effettuo lo split del campo dati d input
087900030715     C**********
088000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088100040324     C                   DOW       posDa <= %len(%trimr(vindta))
088200030715     C*
088300030715     C* Gestisco il 1° campo
088400030715     C                   IF        i = 1
088500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088600030715     C                   EVAL      posDa = 1
088700030715     C* Eseguo lo scan x trovare la fine del primo campo
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C* X i campi successivi al 1°
089300030715     C                   ELSE
089400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089500030715     C                   EVAL      posDa = posA + 1
089600031204     C* Verifico che nn vi sia il campo nn valorizzato
089700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089800031204     C* Se campo nn valorizzato skippo al prossimo
089900031204     C                   EVAL      SkSplitCSV(i) = *blanks
090000031204     C                   EVAL      posA  = posA + 1
090100031204     C                   ELSE
090200030715     C* Eseguo lo scan x trovare la fine del campo corrente
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* Gestisco l'ultimo campo
090500030715     C                   IF        posA = *zeros
090600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   LEAVE
091100030715     C                   ELSE
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   ENDIF
091600031204     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitCSV)
092600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000030715     C*
093100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093200030715     C                   EVAL      i = 1
093300030715     C                   DOW       i < %elem(SkSplitFLD)
093400030715     C*
093500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093700030715     C                             SkSplitCSV(i) <> *blanks
093800030721     C*** ATB
093900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
094000040708     C                   EVAL      VAOATB = SkSplitCSV(i)
094100030721     C                   ENDIF
094200040708     C*** TOR
094300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
094400040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
094500040708     C                   ENDIF
094600040708     C*** TCO
094700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
094800040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
094900040708     C                   ENDIF
095000040708     C*** RSO
095100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
095200040708     C                   EVAL      VAORSO = SkSplitCSV(i)
095300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095400040708     C     '@':'A'       XLATE     VAORSO        VAORSO
095500030715     C* ==
095600030715     C                   ENDIF
095700040708     C*** INO
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
095900040708     C                   EVAL      VAOINO = SkSplitCSV(i)
096000030715     C                   ENDIF
096100040708     C*** LOO
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
096300040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
096400030715     C                   ENDIF
096500040708     C*** PRO
096600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
096700040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
096800030715     C                   ENDIF
096900040708     C*** NAO
097000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
097100040708     C                   EVAL      VAONAO = SkSplitCSV(i)
097200030721     C                   ENDIF
097300040708     C*** RSR
097400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
097500040708     C                   EVAL      VAORSR = SkSplitCSV(i)
097600040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097700040708     C     '@':'A'       XLATE     VAORSR        VAORSR
097800040708     C* ==
097900040708     C                   IF        VAORSR = *blanks
098000040708     C                   SETON                                        31
098100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098200040708     C                             + ' ' + 'VAORSR'
098300040708     C                   ENDIF
098400040708     C                   ENDIF
098500040708     C*** INR
098600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
098700040708     C                   EVAL      VAOINR = SkSplitCSV(i)
098800040708     C                   IF        VAOINR = *blanks
098900040708     C                   SETON                                        31
099000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099100040708     C                             + ' ' + 'VAOINR'
099200040708     C                   ENDIF
099300040708     C                   ENDIF
099400040708     C*** LOR
099500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
099600040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
099700040708     C                   IF        VAOLOR = *blanks
099800040708     C                   SETON                                        31
099900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100000040708     C                             + ' ' + 'VAOLOR'
100100040708     C                   ENDIF
100200040708     C                   ENDIF
100300040708     C*** PRR
100400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
100500040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
100600040708     C                   IF        VAOPRR = *blanks
100700040708     C                   SETON                                        31
100800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100900040708     C                             + ' ' + 'VAOPRR'
101000040708     C                   ENDIF
101100040708     C                   ENDIF
101200040708     C*** NAR
101300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
101400040708     C                   EVAL      VAONAR = SkSplitCSV(i)
101500040708     C                   ENDIF
101600040708     C*** RER
101700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
101800040708     C                   EVAL      VAORER = SkSplitCSV(i)
101900040708     C                   ENDIF
102000040708     C*** TER
102100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
102200040708     C                   EVAL      VAOTER = SkSplitCSV(i)
102300040708     C                   ENDIF
102400040708     C*** RSC
102500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
102600040708     C                   EVAL      VAORSC = SkSplitCSV(i)
102700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102800040708     C     '@':'A'       XLATE     VAORSC        VAORSC
102900040708     C* ==
103000040708     C                   ENDIF
103100040708     C*** INC
103200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
103300040708     C                   EVAL      VAOINC = SkSplitCSV(i)
103400040708     C                   ENDIF
103500040708     C*** LOC
103600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
103700040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
103800040708     C                   ENDIF
103900040708     C*** PRC
104000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
104100040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
104200040708     C                   ENDIF
104300040708     C*** NAC
104400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
104500040708     C                   EVAL      VAONAC = SkSplitCSV(i)
104600040708     C                   ENDIF
104700040708     C*** FFD
104800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
104900040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
105000040708     C                   ENDIF
105100040708     C*** RMP
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
105300040708     C                   EVAL      VAORMP = SkSplitCSV(i)
105400040708     C                   ENDIF
105500040708     C*** NAM
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
105700040708     C                   EVAL      VAONAM = SkSplitCSV(i)
105800040708     C                   ENDIF
105900040708     C*** PAG
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
106100040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
106200040708     C                   ENDIF
106300040708     C*** CTR
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
106500040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
106600040708     C                   ENDIF
106700040708     C*** ZOR
106800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
106900040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
107000040708     C                   ENDIF
107100040708     C*** NO1
107200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
107300040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
107400030715     C                   ENDIF
107500040708     C*** NO2
107600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
107700040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
107800030715     C                   ENDIF
107900040708     C*** DDT
108000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
108100040708     C                   EVAL      VAODDT = SkSplitCSV(i)
108200030715     C                   ENDIF
108300040708     C*** RFA
108400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
108500040708     C                   EVAL      VAORFA = SkSplitCSV(i)
108600030715     C                   ENDIF
108700040708     C*** SPI
108800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
108900040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
109000030715     C                   ENDIF
109100040708     C*** FLO
109200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
109300040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
109400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
109500080708     C* e la posizione 7 del VAOFLO
109600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
109700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
109800050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
109900030715     C                   ENDIF
110000050314
110100050412      *** DAR
110200050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
110300050412     C                   EVAL      PiStr=SkSplitCSV(i)
110400050412     C                   EXSR      CHKNUM
110500050412     C                   IF        PiInt=*on
110600050412     C                   Z-ADD     PiVal         VAODAR
110700050412     C                   ELSE
110800050412     C                   SETON                                        32
110900050412     C                   EVAL      VAODAR = *zeros
111000050412     C                   EVAL      vinmsg = %trimr(vinmsg)
111100050412     C                             + ' ' + 'VAODAR'
111200050412     C                   ENDIF
111300050314     C                   ENDIF
111400030715     C
111500030721     C*
111600030721     C* Reperisco quindi i campi numerici...
111700040708     C*** POE
111800040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
111900040723     C                   EVAL      PiStr=SkSplitCSV(i)
112000040723     C                   EXSR      CHKNUM
112100040723     C                   IF        PiInt=*on    AND
112200040723     C                             PiVal<=999   AND
112300040723     C                             PiVal>*zeros
112400040723     C                   Z-ADD     PiVal         VAOPOE
112500040723     C                   ELSE
112600040723     C                   SETON                                        32
112700040723     C                   EVAL      VAOPOE = *zeros
112800040723     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040723     C                             + ' ' + 'VAOPOE'
113000040723     C                   ENDIF
113100040723     C                   ENDIF
113200040708     C*** COR
113300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
113400040708     C                   EVAL      PiStr=SkSplitCSV(i)
113500040708     C                   EXSR      CHKNUM
113600040708     C                   IF        PiInt=*on
113700040708     C                   Z-ADD     PiVal         VAOCOR
113800040708     C                   ELSE
113900040708     C                   SETON                                        31
114000040708     C                   EVAL      VAOCOR = *zeros
114100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
114200040708     C                             + ' ' + 'VAOCOR'
114300040708     C                   ENDIF
114400040708     C                   ENDIF
114500040708     C*** CAO
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on
115000030721     C                   Z-ADD     PiVal         Num5_0
115100040708     C                   MOVEL(p)  Num5_0        VAOCAO
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400040708     C                   EVAL      VAOCAO = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040708     C                             + ' ' + 'VAOCAO'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900040708     C*** CRA
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on
116400040708     C                   Z-ADD     PiVal         VAOCRA
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700040708     C                   EVAL      VAOCRA = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900040708     C                             + ' ' + 'VAOCRA'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200040708     C*** CAR
117300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
117400040708     C                   EVAL      PiStr=SkSplitCSV(i)
117500040708     C                   EXSR      CHKNUM
117600040708     C                   IF        PiInt=*on
117700040708     C                   Z-ADD     PiVal         Num5_0
117800040708     C                   MOVEL(p)  Num5_0        VAOCAR
117900040708     C                   ELSE
118000040708     C                   SETON                                        31
118100040708     C                   EVAL      VAOCAR = *zeros
118200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040708     C                             + ' ' + 'VAOCAR'
118400040708     C                   ENDIF
118500040708     C                   ENDIF
118600040708     C*** CRC
118700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
118800040708     C                   EVAL      PiStr=SkSplitCSV(i)
118900040708     C                   EXSR      CHKNUM
119000040708     C                   IF        PiInt=*on
119100040708     C                   Z-ADD     PiVal         VAOCRC
119200040708     C                   ELSE
119300040708     C                   SETON                                        32
119400040708     C                   EVAL      VAOCRC = *zeros
119500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040708     C                             + ' ' + 'VAOCRC'
119700040708     C                   ENDIF
119800040708     C                   ENDIF
119900040708     C*** CAC
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
120100040708     C                   EVAL      PiStr=SkSplitCSV(i)
120200040708     C                   EXSR      CHKNUM
120300040708     C                   IF        PiInt=*on
120400040708     C                   Z-ADD     PiVal         Num5_0
120500040708     C                   MOVEL(p)  Num5_0        VAOCAC
120600040708     C                   ELSE
120700040708     C                   SETON                                        32
120800040708     C                   EVAL      VAOCAC = *zeros
120900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121000040708     C                             + ' ' + 'VAOCAC'
121100040708     C                   ENDIF
121200040708     C                   ENDIF
121300040708     C*** DAR
121400050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
121500050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
121600050314     C**!!!              EXSR      CHKNUM
121700050314     C**!!!              IF        PiInt=*on
121800050314     C**!!!              Z-ADD     PiVal         VAODAR
121900050314     C**!!!              ELSE
122000050314     C**!!!              SETON                                        32
122100050314     C**!!!              EVAL      VAODAR = *zeros
122200050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
122300050314     C**!!!                        + ' ' + 'VAODAR'
122400050314     C**!!!              ENDIF
122500050314     C**!!!              ENDIF
122600040708     C*** ORR
122700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
122800040708     C                   EVAL      PiStr=SkSplitCSV(i)
122900040708     C                   EXSR      CHKNUM
123000040708     C                   IF        PiInt=*on
123100040708     C                   Z-ADD     PiVal         VAOORR
123200040708     C                   ELSE
123300040708     C                   SETON                                        32
123400040708     C                   EVAL      VAOORR = *zeros
123500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040708     C                             + ' ' + 'VAOORR'
123700040708     C                   ENDIF
123800040708     C                   ENDIF
123900030721     C*** NCL
124000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on
124400040723     C                   IF        PiVal > *zeros
124500040708     C                   Z-ADD     PiVal         VAONCL
124600040723     C                   ENDIF
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900040708     C                   EVAL      VAONCL = *zeros
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100040708     C                             + ' ' + 'VAONCL'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400040708     C*** PKG
125500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900040723     C                   IF        PiVal > *zeros
126000040708     C                   Z-ADD     PiVal         VAOPKG
126100040723     C                   ENDIF
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400040708     C                   EVAL      VAOPKG = *zeros
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600040708     C                             + ' ' + 'VAOPKG'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900040708     C*** VLM
127000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
127100040708     C                   EVAL      PiStr=SkSplitCSV(i)
127200040708     C                   EXSR      CHKNUM
127300040708     C                   IF        PiNum=*on
127400040708     C                   Z-ADD     PiVal         VAOVLM
127500040708     C                   ELSE
127600040708     C                   SETON                                        32
127700040708     C                   EVAL      VAOVLM = *zeros
127800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127900040708     C                             + ' ' + 'VAOVLM'
128000040708     C                   ENDIF
128100040708     C                   ENDIF
128200040708     C*** BNC
128300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
128400040708     C                   EVAL      PiStr=SkSplitCSV(i)
128500040708     C                   EXSR      CHKNUM
128600040708     C                   IF        PiInt=*on
128700040708     C                   Z-ADD     PiVal         VAOBNC
128800040708     C                   ELSE
128900040708     C                   SETON                                        32
129000040708     C                   EVAL      VAOBNC = *zeros
129100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129200040708     C                             + ' ' + 'VAOBNC'
129300040708     C                   ENDIF
129400040708     C                   ENDIF
129500040708     C*** BLC
129600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
129700040708     C                   EVAL      PiStr=SkSplitCSV(i)
129800040708     C                   EXSR      CHKNUM
129900040708     C                   IF        PiInt=*on
130000040708     C                   Z-ADD     PiVal         VAOBLC
130100040708     C                   ELSE
130200040708     C                   SETON                                        32
130300040708     C                   EVAL      VAOBLC = *zeros
130400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130500040708     C                             + ' ' + 'VAOBLC'
130600040708     C                   ENDIF
130700040708     C                   ENDIF
130800040708     C*** ATT
130900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
131000040708     C                   EVAL      PiStr=SkSplitCSV(i)
131100040708     C                   EXSR      CHKNUM
131200040708     C                   IF        PiInt=*on
131300040708     C                   Z-ADD     PiVal         VAOATT
131400040708     C                   ELSE
131500040708     C                   SETON                                        32
131600040708     C                   EVAL      VAOATT = *zeros
131700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131800040708     C                             + ' ' + 'VAOATT'
131900040708     C                   ENDIF
132000040708     C                   ENDIF
132100040708     C*** MTC
132200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
132300040708     C                   EVAL      PiStr=SkSplitCSV(i)
132400040708     C                   EXSR      CHKNUM
132500040708     C                   IF        PiInt=*on
132600040708     C                   Z-ADD     PiVal         VAOMTC
132700040708     C                   ELSE
132800040708     C                   SETON                                        32
132900040708     C                   EVAL      VAOMTC = *zeros
133000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133100040708     C                             + ' ' + 'VAOMTC'
133200040708     C                   ENDIF
133300040708     C                   ENDIF
133400040708     C*** KSC
133500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on      AND
133900030721     C                             PiVal<=9999999 AND
134000030721     C                             PiVal>*zeros
134100040708     C                   Z-ADD     PiVal         VAOKSC
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400040708     C                   EVAL      VAOKSC = *zeros
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600040708     C                             + ' ' + 'VAOKSC'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900040708     C*** POR
135000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on    AND
135400030721     C                             PiVal<=999   AND
135500030721     C                             PiVal>*zeros
135600040708     C                   Z-ADD     PiVal         VAOPOR
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900040708     C                   EVAL      VAOPOR = *zeros
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100040708     C                             + ' ' + 'VAOPOR'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400040708     C*** POC
136500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
136600040708     C                   EVAL      PiStr=SkSplitCSV(i)
136700040708     C                   EXSR      CHKNUM
136800040708     C                   IF        PiInt=*on    AND
136900040708     C                             PiVal<=999   AND
137000040708     C                             PiVal>*zeros
137100040708     C                   Z-ADD     PiVal         VAOPOC
137200040708     C                   ELSE
137300040708     C                   SETON                                        32
137400040708     C                   EVAL      VAOPOC = *zeros
137500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137600040708     C                             + ' ' + 'VAOPOC'
137700040708     C                   ENDIF
137800040708     C                   ENDIF
137900040708     C*** STO
138000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
138100040708     C                   EVAL      PiStr=SkSplitCSV(i)
138200040708     C                   EXSR      CHKNUM
138300040708     C                   IF        PiInt=*on
138400040708     C                   Z-ADD     PiVal         VAOSTO
138500040708     C                   ELSE
138600040708     C                   SETON                                        32
138700040708     C                   EVAL      VAOSTO = *zeros
138800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138900040708     C                             + ' ' + 'VAOSTO'
139000040708     C                   ENDIF
139100040708     C                   ENDIF
139200110526     C*
139300110526     C*********  Gestione particolarità
139400110526     C*
139500110526     C*** RSC_1
139600110526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSC_1'
139700110526     C                   EVAL      VAORSC = SkSplitCSV(i)
139800110526     C                   EVAL      VAOCRC = *zeros
139900110526     C                   ENDIF
140000110526     C*** RSC_2
140100110526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSC_2'
140200110526     C                   EVAL      VAORSC = %trim(VAORSC) + ' ' + SkSplitCSV(i)
140300110526     C                   ENDIF
140400110526     C*** NO1
140500110526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO1'
140600110526     C                   EVAL      VAONO1 = 'TEL DEST. ' + SkSplitCSV(i)
140700110526     C                   ENDIF
140800110616     C*** DAR
140900110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
141000110705     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
141100110705     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
141200110705     C                   ENDIF
141300110616     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
141400110616     C                                   %subst(SkSplitCSV(i):3:2)+
141500110616     C                                   %subst(SkSplitCSV(i):1:2)
141600110616     C                   EXSR      CHKNUM
141700110616     C                   IF        PiInt=*on
141800110616     C                   Z-ADD     PiVal         VAODAR
141900110616     C                   ELSE
142000110616     C                   SETON                                        32
142100110616     C                   EVAL      VAODAR = *zeros
142200110616     C                   EVAL      vinmsg = %trimr(vinmsg)
142300110616     C                             + ' ' + 'VAODAR'
142400110616     C                   ENDIF
142500110616     C                   ENDIF
142600110616     C*** ORR
142700110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOORR'
142800110621     C***                EVAL      PiStr=%subst(SkSplitCSV(i):1:2)+
142900110621     C***                                %subst(SkSplitCSV(i):4:2)
143000110621     C                   EVAL      PiStr=SkSplitCSV(i)
143100110616     C                   EXSR      CHKNUM
143200110616     C                   IF        PiInt=*on
143300110616     C                   Z-ADD     PiVal         VAOORR
143400110616     C                   ELSE
143500110616     C                   SETON                                        32
143600110616     C                   EVAL      VAOORR = *zeros
143700110616     C                   EVAL      vinmsg = %trimr(vinmsg)
143800110616     C                             + ' ' + 'VAOORR'
143900110616     C                   ENDIF
144000110616     C                   ENDIF
144100110526     C*
144200110526     C*********
144300030715     C*
144400030715     C                   ENDIF
144500030715     C                   EVAL      i = i + 1
144600030715     C                   ENDDO
144700040723     C*
144800030715     C                   ENDIF
144900020204     C*
145000010607     C                   ENDSR
145100010607     C*----------------------------------------------------*
145200010330
145300100512
145400100512
145500100512     C*------------------------------------------------------------------------*
145600100512     C* EXEFORZ - Considerazioni finali
145700100512     C*------------------------------------------------------------------------*
145800100512     C     EXEFORZ       BEGSR
145900100512     C*
146000100512     C                   if        VAOKSC = *zeros
146100100512     C                   select
146200100512     C                   when      VAOPAG = 'O'
146300100512     C                   movel     VAOCOR        VAOKSC
146400100512     C                   when      VAOPAG = 'M'
146500100512     C                   movel     VAOCRA        VAOKSC
146600100512     C                   when      VAOPAG = 'D'
146700100512     C                   movel     VAOCRC        VAOKSC
146800100512     C                   endsl
146900100512     C                   endif
147000100512     C*
147100100512     C                   ENDSR
147200080702
147300080702
147400080702     C*----------------------------------------------------*
147500080702     C*  CONTROLLO NUMERICITA' CAMPI
147600080702     C*----------------------------------------------------*
147700080702     C     CHKNUM        BEGSR
147800080702     C*
147900080702     C                   IF        PiDecChr = *blanks
148000080702     C                   EVAL      PiDecChr = ','
148100080702     C                   ENDIF
148200080702     C*
148300080702     C                   callp     UBISNUM_Check(PiStr
148400080702     C                                          :PiDecChr
148500080702     C                                          :PiVal
148600080702     C                                          :PiNum
148700080702     C                                          :PiInt)
148800080702     C*
148900080702     C                   ENDSR
149000080702     C***
149100010601
149200080702
149300010601
149400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149500010601     C     repfil        BEGSR
149600010601     C*
149700010601     C                   if        invfil = *zeros and
149800010601     C                             depfil > *zeros and
149900010629     C                             (vinflg = *blanks or
150000010629     C                              vinflg = *zeros)
150100010601     C
150200010601     C                   eval      invfil = depfil
150300010601     C                   endif
150400010601     C*
150500010601     C                   if        depfil <> invfil and
150600010601     C                             invfil > *zeros
150700010601     C                   eval      flgMulti = '1'
150800010601     C                   if        vinflg = *blanks
150900010601     C                   add       1             cntNonEl
151000010601     C                   endif
151100010601     C                   endif
151200010601     C*
151300010601     C                   if        vinflg = '2'
151400010601     C                   eval      flgStato = '2'
151500010601     C                   endif
151600010601     C*
151700010601     C                   ENDSR
151800010601     C***
151900010601
152000010601
152100010601
152200010330
152300010330
152400010330
152500990920      /TITLE Invio dei dati al punto operativo.
152600000613     C     invio         BEGSR
152700990920     C*
152800990920     C                   reset                   dscmz
152900010601     C                   move      invfil        cmzdst
153000990920     C                   eval      cmzfld = vlrfou
153100990920     C                   eval      cmzmbd = vlrhdl
153200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
153300000710     C                   if        prmfir = *blanks
153400040708     C                   eval      cmzfla = 'FNVAO00F'
153500040708     C                   eval      cmzmba = 'FNVAO00F'
153600000710     C                   else
153700000710     C                   eval      cmzfla = prmfir
153800000710     C                   eval      cmzmba = prmfir
153900000710     C                   endif
154000990920     C                   eval      cmznrr = *zeros
154100990920     C                   move      §ctrok        cmznrr
154200021018     C                   eval      cmzlba = vlrfl1
154300990920     C                   call(e)   'TIS711C'
154400990920     C                   parm                    dscmz
154500990921     C                   parm      *blanks       esito
154600990920     C                   if        %error
154700990920     C                             or cmzerr = '1'
154800990921     C                             or esito  = '1'
154900000710     C                   eval      wrkesito = '3'
155000990920     C                   endif
155100990920     C*
155200000613     C                   ENDSR
155300990910
155400010601
155500010601
155600010601
155700010601
155800010601      /TITLE Invio dei dati al punto operativo.
155900010601     C     opeini        BEGSR
156000010601     C*
156100010601     C* Inizializzo flag e contatori operativi
156200010601     C                   movel     '0'           flgGiro           1
156300010601     C                   movel     '0'           flgMulti          1
156400010601     C                   movel     '1'           flgStato          1
156500010615     C                   movel     '0'           flgOk             1
156600010601     C                   z-add     *zeros        cntNonEl         10 0
156700010601     C                   z-add     *zeros        depfil            3 0
156800010601     C                   z-add     *zeros        invfil            3 0
156900010601     C*
157000010601     C                   ENDSR
157100010601     C***
157200010601
157300010601
157400050421
157500050421
157600050421     C     *pssr         BEGSR
157700050421     C*
157800050421     C                   eval      wrkesito = '2'
157900050421     C*
158000050421     C                   ENDSR     '*CANCL'
158100050421     C***
158200060306
158300060306
158400060306     C*-------------------------------------------------------------------------*
158500060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
158600060306     C*-------------------------------------------------------------------------*
158700060306     C     sr_Control    begsr
158800060306     C*
158900060306     C* inizializzo a ok il flag d controllo abilitazione
159000060306     C                   eval      Cli_Abil = *on
159100060306     C
159200060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
159300060306     C                   clear                   woriIDC
159400060306     C                   eval      kteCOD = 'VAP'
159500060306     C                   eval      kteKE1 = oriIDC
159600060306     C     keytbe        chain     TNTBE01L
159700060306     C                   if        %found(Tntbe01l)
159800060306     C                   eval      woriIDC = TbeKe2
159900060306     C                   else
160000060306     C                   eval      Cli_Abil = *off
160100060306     C                   endif
160200060306     C*
160300060306     C                   endsr
160400050421
160500010601
160600010330
160700010330
160800000613     C     *inzsr        BEGSR
160900990910     C*
161000990910     C     *entry        plist
161100990920     C                   parm                    tivlrds
161200990921     C                   parm      wrkesito      esito
161300000724     C                   parm                    prmlit
161400000710     C                   parm                    prmfir
161500010330     C*
161600010330     C* CALCOLA LA DATA CORRENTE
161700100223     C                   time                    wn14             14 0
161800100223     C                   movel     wn14          wn6               6 0          *ORA (6)
161900100223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
162000100223     C                   eval      datcor = %dec(%date() : *ISO)
162100040723     C*
162200040723     C* Campi riferito al database
162300040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
162400040723     C     *like         define    tbeke1        kteKE1
162500040723     C*
162600040723     C* Definizione chiavi di lettura
162700040723     C* TNTBE01L - parziale
162800040723     C     keytbe        klist
162900040723     C                   kfld                    kteCOD                         *UTENTE
163000040723     C                   kfld                    kteKE1                         *CODICE
163100000613     C*
163200000613     C                   ENDSR
163300000613     C***
163400040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
163500110621FILLXX;FILLXX;VAORSR;VAORER;VAOINR;VAOLOR;VAOCAR;VAOPRR;FILLXX;VAOTER;
163600110621FILLXX;FILLXX;FILLXX;FILLXX;FILLXX;FILLXX;VAORFA;VAOPKG;VAONCL;
163700110616W_VAODAR;W_VAOORR;
163800110616
163900110621
