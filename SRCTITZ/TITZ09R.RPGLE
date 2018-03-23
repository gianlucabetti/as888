000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209     F*
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
011700990915     C*
011800000616     C*
011900010601     C                   ENDSR
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
019100120120     C**N31              EXSR      CHKCOR
019200120120     C**N31              EXSR      WRIORI
019300120120     C**N31              EXSR      WRIVGD
019400120120     C**N31              write     FNVAO000
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
021400120120
021500120120      * scrivo il file FNVAO SOLO a totale di lettura dei dati mandati dal cliente
021600120120     C                   if        wGiro = 2
021700120120     C  N31              EXSR      CHKCOR
021800120120     C  N31              EXSR      WRIORI
021900120120     C  N31              EXSR      WRIVGD
022000120120     C  N31              write     FNVAO000
022100120120     C                   endif
022200010601     C
022300010601     C*
022400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022500010601     C                   if        cntNonEl = *zeros or
022600010601     C                             flgMulti = '0'
022700010330     C* Se non ci sono record con errori ...
022800010601     C                   if        §ctrno = 0 and
022900010604     C                             §ctrmo = 0 and
023000010601     C                             flgStato <> '2'
023100010330     C* ... restituisco esito OK.
023200010330     C                   eval      wrkesito = '0'
023300010330     C                   else
023400010330     C                   if        §ctrok > 0
023500010330     C                   eval      wrkesito = '1'
023600010330     C                   else
023700010615     C                   if        flgOk = '0'
023800010615     C                   eval      wrkesito = '2'
023900010615     C                   else
024000010615     C                   eval      wrkesito = '6'
024100010615     C                   endif
024200010330     C                   endif
024300010330     C                   endif
024400010601     C                   else
024500010601     C                   eval      wrkesito = '9'
024600010601     C                   endif
024700010330     C*
024800010330     C                   if        %open(tivin00r)
024900010330     C                   close     tivin00r
025000010330     C                   endif
025100040708     C                   if        %open(fnvaowwr)
025200040708     C                   close     fnvaowwr
025300010330     C                   endif
025400010601     C*
025500010601     C                   if        vlrpoi <> 999
025600010601     C                   eval      invfil = vlrpoi
025700010601     C                   endif
025800010330     C*
025900010330     C                   if        §ctrok > 0
026000010601     C                             and invfil > *zeros
026100010330     C                   exsr      invio
026200010330     C                   endif
026300010601     C*
026400010618     C                   if        flgGiro = '1'
026500010601     C                   exsr      endela
026600010618     C                   endif
026700010330     C*
026800010330     C                   ENDSR
026900010330     C***
027000040723
027100040723
027200040723     C*------------------------------------------------------------------------*
027300040723     C* CHKCOR - Considerazioni su VAOCOR
027400040723     C*------------------------------------------------------------------------*
027500040723     C     CHKCOR        BEGSR
027600040723     C*
027700040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
027800040723     C                   if        vaoTCO = 'F'
027900040723     C* Quindi verifico se il codice ordinante non è valorizzato
028000040723     C                   if        vaoCOR = *zeros  AND
028100040723     C                             vaoRSO = *blanks AND
028200040723     C                             vaoINO = *blanks AND
028300040723     C                             vaoCAO = *blanks AND
028400040723     C                             vaoLOO = *blanks AND
028500040723     C                             vaoPRO = *blanks AND
028600040723     C                             vaoNAO = *blanks
028700040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
028800040723     C                   movel     '0'           wFlgAcr           1
028900040723     C                   move      vlrKSC        wCli              7 0
029000040723     C                   move(p)   wCli          acrCRO
029100040723     C     acrCRO        setll     fnacr01l
029200040723     C                   if        %found(fnacr01l)
029300040723     C                   read      fnacr01l
029400040723     C                   dow       not %eof(fnacr01l)
029500040723     C                   movel     acrCRO        wCliAcr           7 0
029600040723     C                   if        wCliAcr = wCli
029700040723     C                   eval      vaoCOR = acrCRO
029800040723     C                   movel     '1'           wFlgAcr
029900040723     C                   leave
030000040723     C                   else
030100040723     C                   leave
030200040723     C                   endif
030300040723     C                   read      fnacr01l
030400040723     C                   enddo
030500040723     C                   endif
030600040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
030700040723     C* dati anagrafici dell'ordinante dal piano dei conti
030800040723     C                   if        wFlgAcr = '0'
030900040723     C                   clear                   BS69DS
031000040723     C                   clear                   ACODS
031100040723     C                   clear                   INDDS
031200040723     C                   clear                   CLPDS
031300040723     C                   clear                   CLSDS
031400040723     C                   move(P)   vlrKSC        I69KAC
031500040723     C                   move(P)   vlrKSC        I69KIN
031600040723     C                   call      'TIBS69R'
031700040723     C                   parm                    BS69DS
031800040723     C                   parm                    ACODS
031900040723     C                   parm                    INDDS
032000040723     C                   parm                    CLPDS
032100040723     C                   parm                    CLSDS
032200040723     C     O69ERR        ifne      '1'
032300040723     C                   eval      vaoRSO = ACORAG
032400040723     C                   eval      vaoINO = INDVIA
032500040723     C                   movel(P)  INDCAP        vaoCAO
032600040723     C                   eval      vaoLOO = INDCIT
032700040723     C                   eval      vaoPRO = INDPRV
032800040723     C                   eval      vaoNAO = INDSTA
032900040723     C                   endif
033000040723     C                   endif
033100040723     C                   endif
033200040723     C                   endif
033300040723     C*
033400040723     C                   ENDSR
033500040723
033600040723
033700040723     C*------------------------------------------------------------------------*
033800040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
033900040723     C*------------------------------------------------------------------------*
034000040723     C     WRIORI        BEGSR
034100040723     C*
034200090618     C* Come prima cosa stacco un numeratore da AZNUM
034300090618     C                   clear                   trul33ds
034400090618     C                   eval      I33OPE = *zeros
034500090618     C                   eval      I33CNU = 600
034600090618     C                   eval      I33NUM = 1
034700090618     C                   movel     TRUL33DS      KPJBU
034800090618     C                   call      'TRUL33R'
034900090618     C                   parm                    KPJBA
035000090618     C                   movel     KPJBU         TRUL33DS
035100090618     C                   if        O33ERR = *zeros
035200090618     C                   z-add     O33NRF        §numori
035300090618     c                   else
035400090618     c                   eval      §numori = *all'9'
035500090618     C                   endif
035600040723     C*
035700040723     C* Quindi imposto i campi "extra-VAO"
035800040723     C                   if        vlrKSC = '0ORM0000'
035900040723     C*
036000040723     C                   if        vaoCOR > *zeros
036100040723     C                   movel     vaoCOR        wrkCOR7           7 0
036200040723     C*
036300040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
036400040723     C* codice padre. Se è un padre assegno già il codice ordinante.
036500040723     C                   clear                   tibs10ds
036600040723     C                   eval      d10drf = datcor                              *data riferimento
036700040723     C                   eval      d10paf = 'P'                                 *cerca il padre
036800040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
036900091026     C*
037000091026     C* Preparazione chiamata al TIBS10R1
037100091026     C                   clear                   tibs10r1ds
037200091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
037300091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
037400091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
037500091026     C*
037600091026     C                   call      'TIBS10R1'
037700091026     C                   parm                    tibs10r1ds
037800091026     C*
037900091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
038000040723     C*
038100040723     C                   if        d10err <> *blanks or                         *No unif. o padre
038200040723     C                             d10cop =  *zeros  or
038300040723     C                             d10cop =  d10cod
038400040723     C                   move      d10cod        oriIDC
038500040723     C                   else                                                   *Figlio
038600040723     C                   move      d10cop        oriIDC
038700040723     C                   endif
038800040723     C*
038900040723     C                   else
039000040723     C                   eval      oriIDC = *zeros
039100040723     C                   endif
039200040723     C*
039300040723     C                   else
039400040723     C                   eval      oriIDC = vlrKSC
039500040723     C                   endif
039600040723     C*
039700040723     C                   eval      oriPRG = §numori
039800040723     C                   eval      oriDER = datcor
039900040723     C                   eval      oriOER = wn6
040000040723     C                   eval      oriPOE = vaoPOE
040100040723     C                   eval      oriNSR = vaoNSR
040200040723     C                   eval      oriNOR = vaoNOR
040300040723     C                   eval      oriNRV = vaoNRV
040400040723     C                   eval      oriFLO = *blanks
040500040723     C*
040600040723     C* Infine valorizzo la chiave esterna sul file FNVAO
040700040723     C                   eval      dorm01 = vaoFLO
040800040723     C                   movel     §numori       §ormpg
040900050412     C*
041000050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
041100120120     C*******            movel     'S'           §ormfr
041200120120     C*******            movel     'S'           §ormfd
041300130306     C**********         movel     'N'           §orcomc
041400050412     C*
041500050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
041600040723     C                   eval      vaoFLO = dorm01
041700040723     C*
041800040723     C* ...quindi scrivo il file TIORI00F
041900040723     C                   WRITE     TIORI000
042000040723     C*
042100040723     C                   ENDSR
042200050826
042300050826
042400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
042500050826     C     wriVGD        BEGSR
042600050826     C*
042700050826     C* Reperisco la descrizione della fase da tabella
042800050826     C                   movel(p)  'FAR'         KteCOD
042900050826     C                   movel(p)  '000'         KteKE1
043000050826     C     KEYtbe        chain     tntbe01l
043100050826     C                   if        %found(tntbe01l)
043200050826     C                   movel(P)  tbeuni        DFAR
043300050826     C                   else
043400050826     C                   clear                   DFAR
043500050826     C                   endif
043600050826     C*
043700050826     C* Valorizzo buffer record
043800050826     C                   clear                   FNVAPDS
043900050826     C                   eval      vapIDC = oriIDC
044000050826     C                   eval      vapPOE = oriPOE
044100050826     C                   eval      vapRFA = vaoRFA
044200050826     C                   eval      vapPOG = oriPOE
044300050826     C                   eval      vapDAE = oriDER
044400050826     C                   eval      vapORE = oriOER
044500050826     C                   if        d§fardva <> *blanks
044600050826     C                   eval      vapDFA = d§fardva
044700050826     C                   else
044800050826     C                   eval      vapDFA = d§fardes
044900050826     C                   endif
045000050826     C*
045100050826     C                   clear                   tivgd000
045200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
045300050826     C                   eval      vgdTIP = 'VP'
045400050826     C                   eval      vgdKSU = vapIDC
045500050826     C                   eval      vgdTSC = 'WW'
045600050826     C                   eval      vgdDAT = datcor
045700151207     C                   eval      vgdPGM = procname
045800060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
045900060306     C                   exsr      sr_Control
046000060306     C                   if        Cli_Abil = *on
046100060306     C                   eval      vgdKSU = woriIDC
046200050826     C                   write     tivgd000
046300060306     C                   endif
046400050826     C*
046500050826     C                   ENDSR
046600050826     C*------------------------------------------------------------------------*
046700010601
046800010601
046900010330     C*----------------------------------------------------*
047000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047100010330     C*----------------------------------------------------*
047200010330     C     INZVAR        BEGSR
047300010330     C*
047400030822     C* Inizializzo variabili di wrk
047500020204     C                   Z-ADD     *zeros        Num5_0            5 0
047600030822     C*
047700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047800120120     C****               CLEAR                   FNVAO000
047900120120     C****               CLEAR                   TIORI000
048000030822     C                   CLEAR                   SkSplitCSV
048100030822     C                   EVAL      i = 1
048200030822     C                   EVAL      posDa = *zeros
048300030822     C                   EVAL      posA  = *zeros
048400030822     C*
048500030822     C* Reimposto i valori di default
048600030822     C                   EXSR      DEFCAM
048700030822     C*
048800010330     C                   ENDSR
048900010330     C*----------------------------------------------------*
049000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049100010330     C*----------------------------------------------------*
049200010330     C     DEFCAM        BEGSR
049300030822     C*
049400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049500030715     C* e delimitatore testo.
049600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049900040723     C*
050000080707     C* Imposto i valori d default d alcuni campi
050100080708     C*
050200080708     C                   CLEAR                   DORM01
050300091103     C                   EVAL      VAOFLO = DORM01
050400080707     C*
050500091103     C                   EVAL      VAOTCO = 'F'
050600120120     C                   EVAL      VAOTOR = 'M'
050700091127     C*
050800091204     C* IMPOSTO I CAMPI PERSONALIZZATI
050900100416     C
051000030822     C*
051100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051200030822     C                   EVAL      posDaDft = 1
051300030822     C                   EVAL      posADft  = 0
051400030822     C                   EVAL      wGiroDft = 0
051500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051600030822     C                             posDaDft > 0
051700030822     C*
051800030822     C* Gestisco il 1° giro
051900030822     C                   IF        wGiroDft = 0
052000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052200030822     C* Incremento il contatore dei "giri"
052300030822     C                   EVAL      wGiroDft = 1
052400030822     C                   ELSE
052500030822     C                   EVAL      posDaDft = posADft
052600030822     C                   ENDIF
052700030822     C* Eseguo lo scan x trovare la fine del campo corrente
052800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052900030822     C*
053000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053200030822     C                   IF        posDaDft > 0 AND
053300030822     C                             posADft  > 0
053400040708     C* POE
053500040708     C                   IF        %subst(
053600040708     C                             %subst(vlrppt:posDaDft+1:
053700040708     C                             posADft-posDaDft-1):1:3)
053800040708     C                             = 'POE'
053900040708     C                   EVAL      PiStr=%trim(%subst(
054000040708     C                             %subst(vlrppt:posDaDft+1:
054100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200040708     C                   EXSR      CHKNUM
054300040708     C                   IF        PiInt=*on
054400040708     C                   Z-ADD     PiVal         VAOPOE
054500040708     C                   ENDIF
054600040708     C                   ENDIF
054700040708     C* TOR
054800040708     C                   IF        %subst(
054900040708     C                             %subst(vlrppt:posDaDft+1:
055000040708     C                             posADft-posDaDft-1):1:3)
055100040708     C                             = 'TOR'
055200040708     C                   EVAL      VAOTOR=%trim(%subst(
055300040708     C                             %subst(vlrppt:posDaDft+1:
055400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500040708     C                   ENDIF
055600040708     C* TCO
055700040708     C                   IF        %subst(
055800040708     C                             %subst(vlrppt:posDaDft+1:
055900040708     C                             posADft-posDaDft-1):1:3)
056000040708     C                             = 'TCO'
056100040708     C                   EVAL      VAOTCO=%trim(%subst(
056200040708     C                             %subst(vlrppt:posDaDft+1:
056300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400040708     C                   ENDIF
056500040707     C* COR
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900040707     C                             = 'COR'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500040708     C                   Z-ADD     PiVal         VAOCOR
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800040708     C* CRC
057900040708     C                   IF        %subst(
058000040708     C                             %subst(vlrppt:posDaDft+1:
058100040708     C                             posADft-posDaDft-1):1:3)
058200040708     C                             = 'CRC'
058300040708     C                   EVAL      PiStr=%trim(%subst(
058400040708     C                             %subst(vlrppt:posDaDft+1:
058500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600040708     C                   EXSR      CHKNUM
058700040708     C                   IF        PiInt=*on
058800040708     C                   Z-ADD     PiVal         VAOCRC
058900040708     C                   ENDIF
059000040708     C                   ENDIF
059100040708     C* FFD
059200040708     C                   IF        %subst(
059300040708     C                             %subst(vlrppt:posDaDft+1:
059400040708     C                             posADft-posDaDft-1):1:3)
059500040708     C                             = 'FFD'
059600040708     C                   EVAL      VAOFFD=%trim(%subst(
059700040708     C                             %subst(vlrppt:posDaDft+1:
059800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900040708     C                   ENDIF
060000040708     C* ORR
060100040708     C                   IF        %subst(
060200040708     C                             %subst(vlrppt:posDaDft+1:
060300040708     C                             posADft-posDaDft-1):1:3)
060400040708     C                             = 'ORR'
060500040708     C                   EVAL      PiStr=%trim(%subst(
060600040708     C                             %subst(vlrppt:posDaDft+1:
060700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800040708     C                   EXSR      CHKNUM
060900040708     C                   IF        PiInt=*on
061000040708     C                   Z-ADD     PiVal         VAOORR
061100040708     C                   ENDIF
061200040708     C                   ENDIF
061300040712     C* NCL
061400040712     C                   IF        %subst(
061500040712     C                             %subst(vlrppt:posDaDft+1:
061600040712     C                             posADft-posDaDft-1):1:3)
061700040712     C                             = 'NCL'
061800040712     C                   EVAL      PiStr=%trim(%subst(
061900040712     C                             %subst(vlrppt:posDaDft+1:
062000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100040712     C                   EXSR      CHKNUM
062200040712     C                   IF        PiInt=*on
062300040712     C                   Z-ADD     PiVal         VAONCL
062400040712     C                   ENDIF
062500040712     C                   ENDIF
062600040712     C* PKG
062700040712     C                   IF        %subst(
062800040712     C                             %subst(vlrppt:posDaDft+1:
062900040712     C                             posADft-posDaDft-1):1:3)
063000040712     C                             = 'PKG'
063100040712     C                   EVAL      PiStr=%trim(%subst(
063200040712     C                             %subst(vlrppt:posDaDft+1:
063300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400040712     C                   EXSR      CHKNUM
063500040712     C                   IF        PiNum=*on
063600040712     C                   Z-ADD     PiVal         VAOPKG
063700040712     C                   ENDIF
063800040712     C                   ENDIF
063900040712     C* VLM
064000040712     C                   IF        %subst(
064100040712     C                             %subst(vlrppt:posDaDft+1:
064200040712     C                             posADft-posDaDft-1):1:3)
064300040712     C                             = 'VLM'
064400040712     C                   EVAL      PiStr=%trim(%subst(
064500040712     C                             %subst(vlrppt:posDaDft+1:
064600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700040712     C                   EXSR      CHKNUM
064800040712     C                   IF        PiNum=*on
064900040712     C                   Z-ADD     PiVal         VAOVLM
065000040712     C                   ENDIF
065100040712     C                   ENDIF
065200040708     C* NAM
065300040708     C                   IF        %subst(
065400040708     C                             %subst(vlrppt:posDaDft+1:
065500040708     C                             posADft-posDaDft-1):1:3)
065600040708     C                             = 'NAM'
065700040708     C                   EVAL      VAONAM=%trim(%subst(
065800040708     C                             %subst(vlrppt:posDaDft+1:
065900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000040708     C                   ENDIF
066100040708     C* PAG
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500040708     C                             = 'PAG'
066600040708     C                   EVAL      VAOPAG=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000040708     C* KSC
067100040708     C                   IF        %subst(
067200040708     C                             %subst(vlrppt:posDaDft+1:
067300040708     C                             posADft-posDaDft-1):1:3)
067400040708     C                             = 'KSC'
067500040708     C                   EVAL      PiStr=%trim(%subst(
067600040708     C                             %subst(vlrppt:posDaDft+1:
067700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800040708     C                   EXSR      CHKNUM
067900040708     C                   IF        PiInt=*on
068000040708     C                   Z-ADD     PiVal         VAOKSC
068100040708     C                   ENDIF
068200040708     C                   ENDIF
068300040708     C* CTR
068400040708     C                   IF        %subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):1:3)
068700040708     C                             = 'CTR'
068800040708     C                   EVAL      VAOCTR=%trim(%subst(
068900040708     C                             %subst(vlrppt:posDaDft+1:
069000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100040708     C                   ENDIF
069200040708     C* POC
069300040708     C                   IF        %subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):1:3)
069600040708     C                             = 'POC'
069700040708     C                   EVAL      PiStr=%trim(%subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000040708     C                   EXSR      CHKNUM
070100040708     C                   IF        PiInt=*on
070200040708     C                   Z-ADD     PiVal         VAOPOC
070300040708     C                   ENDIF
070400040708     C                   ENDIF
070500040708     C* NO1
070600040708     C                   IF        %subst(
070700040708     C                             %subst(vlrppt:posDaDft+1:
070800040708     C                             posADft-posDaDft-1):1:3)
070900040708     C                             = 'NO1'
071000040708     C                   EVAL      VAONO1=%trim(%subst(
071100040708     C                             %subst(vlrppt:posDaDft+1:
071200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300040708     C                   ENDIF
071400040708     C* DDT
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800040708     C                             = 'DDT'
071900040708     C                   EVAL      VAODDT=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300040708     C* RFA
072400040708     C                   IF        %subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):1:3)
072700040708     C                             = 'RFA'
072800040708     C                   EVAL      VAORFA=%trim(%subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040708     C                   ENDIF
073200040708     C* FLO
073300040708     C                   IF        %subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):1:3)
073600040708     C                             = 'FLO'
073700040708     C                   EVAL      VAOFLO=%trim(%subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000091103     C*
074100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
074200080702     C* e posizione 7 del VAOFLO
074300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
074400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
074500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
074600040708     C                   ENDIF
074700030822     C* ...
074800030822     C                   ENDIF
074900030822     C                   ENDDO
075000020204     C*
075100010330     C                   ENDSR
075200010607     C*----------------------------------------------------*
075300040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
075400010607     C*----------------------------------------------------*
075500040708     C     IMPVAO        BEGSR
075600010607     C*
075700010607     C                   SETOFF                                       3132
075800030515     C*
075900040708     C                   MOVEL     datcor        VAODAO
076000040708     C                   MOVEL     wn14          VAOOAO
076100050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076200050314     C     minu:maiu     XLATE     vindta        vindta
076300010607     C*
076400030715     C**********
076500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076600030715     C**********
076700030715     C                   IF        wGiro = *zeros
076800040709     C*
076900040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
077000040709     C                   EVAL      depvinDTA = vinDTA
077100040709     C*
077200040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077300040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077400040709     C                                      %trim(intcol(3))
077500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077600050314     C**!!!minu:maiu     XLATE     vindta        vindta
077700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077800040324     C                   DOW       posDa <= %len(%trimr(vindta))
077900030715     C*
078000030715     C* Gestisco il 1° campo
078100030715     C                   IF        i = 1
078200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078300030715     C                   EVAL      posDa = 1
078400030715     C* Eseguo lo scan x trovare la fine del primo campo
078500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078600030715     C* A questo punto "estraggo" il campo corrente
078700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078800030715     C                                                    (posA-posDa))
078900030715     C* X i campi successivi al 1°
079000030715     C                   ELSE
079100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079200030715     C                   EVAL      posDa = posA + 1
079300030715     C* Eseguo lo scan x trovare la fine del campo corrente
079400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079500030715     C* Gestisco l'ultimo campo
079600030715     C                   IF        posA = *zeros
079700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
079800030715     C* A questo punto "estraggo" il campo corrente
079900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080000030715     C                                                    (posA-posDa))
080100030715     C                   LEAVE
080200030715     C                   ELSE
080300030715     C* A questo punto "estraggo" il campo corrente
080400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080500030715     C                                                    (posA-posDa))
080600030715     C                   ENDIF
080700030715     C                   ENDIF
080800030715     C* Incremento il contatore d campo
080900030715     C                   EVAL      i = i +1
081000030715     C                   ENDDO
081100030715     C*
081200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081300030715     C* delimitazione testo
081400030715     C                   EVAL      i = 1
081500030715     C                   DOW       i < %elem(SkSplitFLD)
081600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081800030715     C                   EVAL      i = i + 1
081900030715     C                   ENDDO
082000030715     C                   EVAL      wGiro = 1
082100040709     C* Ripristino la 1° riga originale
082200040709     C                   EVAL      vinDTA = depvinDTA
082300030715     C                   ELSE
082400030715     C                   EVAL      wGiro = 2
082500031204     C**********
082600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082700031204     C* d separatore campo
082800031204     C**********
082900040709     C                   EVAL      vindta = %trim(vindta)
083000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083100040709     C                   EVAL      vindta = %subst(vindta:2)
083200040709     C                   ENDDO
083300040709     C*
083400040709     C                   Z-ADD     *zeros        lunghInput        4 0
083500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
083600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
083900040709     C                   ENDDO
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input
084200030715     C**********
084300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084400040324     C                   DOW       posDa <= %len(%trimr(vindta))
084500030715     C*
084600030715     C* Gestisco il 1° campo
084700030715     C                   IF        i = 1
084800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084900030715     C                   EVAL      posDa = 1
085000030715     C* Eseguo lo scan x trovare la fine del primo campo
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C* X i campi successivi al 1°
085600030715     C                   ELSE
085700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085800030715     C                   EVAL      posDa = posA + 1
085900031204     C* Verifico che nn vi sia il campo nn valorizzato
086000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086100031204     C* Se campo nn valorizzato skippo al prossimo
086200031204     C                   EVAL      SkSplitCSV(i) = *blanks
086300031204     C                   EVAL      posA  = posA + 1
086400031204     C                   ELSE
086500030715     C* Eseguo lo scan x trovare la fine del campo corrente
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* Gestisco l'ultimo campo
086800030715     C                   IF        posA = *zeros
086900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   LEAVE
087400030715     C                   ELSE
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   ENDIF
087900031204     C                   ENDIF
088000030715     C                   ENDIF
088100030715     C* Incremento il contatore d campo
088200030715     C                   EVAL      i = i +1
088300030715     C                   ENDDO
088400030715     C*
088500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088600030715     C* delimitazione testo
088700030715     C                   EVAL      i = 1
088800030715     C                   DOW       i < %elem(SkSplitCSV)
088900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089100030715     C                   EVAL      i = i + 1
089200030715     C                   ENDDO
089300030715     C*
089400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089500030715     C                   EVAL      i = 1
089600030715     C                   DOW       i < %elem(SkSplitFLD)
089700030715     C*
089800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090000030715     C                             SkSplitCSV(i) <> *blanks
090100030721     C*** ATB
090200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
090300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
090400030721     C                   ENDIF
090500040708     C*** TOR
090600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
090700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
090800040708     C                   ENDIF
090900040708     C*** TCO
091000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
091100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
091200040708     C                   ENDIF
091300040708     C*** RSO
091400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
091500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
091600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
091800030715     C* ==
091900030715     C                   ENDIF
092000040708     C*** INO
092100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
092200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
092300030715     C                   ENDIF
092400040708     C*** LOO
092500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
092600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
092700030715     C                   ENDIF
092800040708     C*** PRO
092900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
093000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
093100030715     C                   ENDIF
093200040708     C*** NAO
093300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
093400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
093500030721     C                   ENDIF
093600040708     C*** RSR
093700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
093800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
093900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
094100040708     C* ==
094200040708     C                   IF        VAORSR = *blanks
094300040708     C                   SETON                                        31
094400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
094500040708     C                             + ' ' + 'VAORSR'
094600040708     C                   ENDIF
094700040708     C                   ENDIF
094800040708     C*** INR
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
095000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
095100040708     C                   IF        VAOINR = *blanks
095200040708     C                   SETON                                        31
095300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
095400040708     C                             + ' ' + 'VAOINR'
095500040708     C                   ENDIF
095600040708     C                   ENDIF
095700040708     C*** LOR
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
095900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
096000040708     C                   IF        VAOLOR = *blanks
096100040708     C                   SETON                                        31
096200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
096300040708     C                             + ' ' + 'VAOLOR'
096400040708     C                   ENDIF
096500040708     C                   ENDIF
096600040708     C*** PRR
096700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
096800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
096900040708     C                   IF        VAOPRR = *blanks
097000040708     C                   SETON                                        31
097100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097200040708     C                             + ' ' + 'VAOPRR'
097300040708     C                   ENDIF
097400040708     C                   ENDIF
097500040708     C*** NAR
097600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
097700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
097800040708     C                   ENDIF
097900040708     C*** RER
098000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
098100040708     C                   EVAL      VAORER = SkSplitCSV(i)
098200040708     C                   ENDIF
098300040708     C*** TER
098400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
098500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
098600040708     C                   ENDIF
098700040708     C*** RSC
098800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
098900040708     C                   EVAL      VAORSC = SkSplitCSV(i)
099000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099100040708     C     '@':'A'       XLATE     VAORSC        VAORSC
099200040708     C* ==
099300040708     C                   ENDIF
099400040708     C*** INC
099500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
099600040708     C                   EVAL      VAOINC = SkSplitCSV(i)
099700040708     C                   ENDIF
099800040708     C*** LOC
099900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
100000040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
100100040708     C                   ENDIF
100200040708     C*** PRC
100300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
100400040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
100500040708     C                   ENDIF
100600040708     C*** NAC
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
100800040708     C                   EVAL      VAONAC = SkSplitCSV(i)
100900040708     C                   ENDIF
101000040708     C*** FFD
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
101200040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
101300040708     C                   ENDIF
101400040708     C*** RMP
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
101600040708     C                   EVAL      VAORMP = SkSplitCSV(i)
101700040708     C                   ENDIF
101800040708     C*** NAM
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
102000040708     C                   EVAL      VAONAM = SkSplitCSV(i)
102100040708     C                   ENDIF
102200040708     C*** PAG
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
102400040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
102500040708     C                   ENDIF
102600040708     C*** CTR
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
102800040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
102900040708     C                   ENDIF
103000040708     C*** ZOR
103100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
103200040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
103300040708     C                   ENDIF
103400040708     C*** NO1
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
103600040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
103700030715     C                   ENDIF
103800040708     C*** NO2
103900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
104000040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
104100030715     C                   ENDIF
104200040708     C*** DDT
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
104400040708     C                   EVAL      VAODDT = SkSplitCSV(i)
104500030715     C                   ENDIF
104600040708     C*** RFA
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
104800040708     C                   EVAL      VAORFA = SkSplitCSV(i)
104900030715     C                   ENDIF
105000040708     C*** SPI
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
105200040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
105300030715     C                   ENDIF
105400040708     C*** FLO
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
105600040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
105700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
105800080708     C* e la posizione 7 del VAOFLO
105900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
106000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
106100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
106200030715     C                   ENDIF
106300091103     C
106400091103     C*** DAR
106500050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
106600050412     C                   EVAL      PiStr=SkSplitCSV(i)
106700050412     C                   EXSR      CHKNUM
106800050412     C                   IF        PiInt=*on
106900050412     C                   Z-ADD     PiVal         VAODAR
107000050412     C                   ELSE
107100050412     C                   SETON                                        32
107200050412     C                   EVAL      VAODAR = *zeros
107300050412     C                   EVAL      vinmsg = %trimr(vinmsg)
107400050412     C                             + ' ' + 'VAODAR'
107500050412     C                   ENDIF
107600050314     C                   ENDIF
107700030715     C
107800030721     C*
107900030721     C* Reperisco quindi i campi numerici...
108000040708     C*** POE
108100040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
108200040723     C                   EVAL      PiStr=SkSplitCSV(i)
108300040723     C                   EXSR      CHKNUM
108400040723     C                   IF        PiInt=*on    AND
108500040723     C                             PiVal<=999   AND
108600040723     C                             PiVal>*zeros
108700040723     C                   Z-ADD     PiVal         VAOPOE
108800040723     C                   ELSE
108900040723     C                   SETON                                        32
109000040723     C                   EVAL      VAOPOE = *zeros
109100040723     C                   EVAL      vinmsg = %trimr(vinmsg)
109200040723     C                             + ' ' + 'VAOPOE'
109300040723     C                   ENDIF
109400040723     C                   ENDIF
109500040708     C*** COR
109600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
109700040708     C                   EVAL      PiStr=SkSplitCSV(i)
109800040708     C                   EXSR      CHKNUM
109900040708     C                   IF        PiInt=*on
110000040708     C                   Z-ADD     PiVal         VAOCOR
110100040708     C                   ELSE
110200040708     C                   SETON                                        31
110300040708     C                   EVAL      VAOCOR = *zeros
110400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110500040708     C                             + ' ' + 'VAOCOR'
110600040708     C                   ENDIF
110700040708     C                   ENDIF
110800040708     C*** CAO
110900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on
111300030721     C                   Z-ADD     PiVal         Num5_0
111400040708     C                   MOVEL(p)  Num5_0        VAOCAO
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700040708     C                   EVAL      VAOCAO = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900040708     C                             + ' ' + 'VAOCAO'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200040708     C*** CRA
112300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on
112700040708     C                   Z-ADD     PiVal         VAOCRA
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000040708     C                   EVAL      VAOCRA = *zeros
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040708     C                             + ' ' + 'VAOCRA'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500040708     C*** CAR
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
113700040708     C                   EVAL      PiStr=SkSplitCSV(i)
113800040708     C                   EXSR      CHKNUM
113900040708     C                   IF        PiInt=*on
114000040708     C                   Z-ADD     PiVal         Num5_0
114100040708     C                   MOVEL(p)  Num5_0        VAOCAR
114200040708     C                   ELSE
114300040708     C                   SETON                                        31
114400040708     C                   EVAL      VAOCAR = *zeros
114500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
114600040708     C                             + ' ' + 'VAOCAR'
114700040708     C                   ENDIF
114800040708     C                   ENDIF
114900040708     C*** CRC
115000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
115100040708     C                   EVAL      PiStr=SkSplitCSV(i)
115200040708     C                   EXSR      CHKNUM
115300040708     C                   IF        PiInt=*on
115400040708     C                   Z-ADD     PiVal         VAOCRC
115500040708     C                   ELSE
115600040708     C                   SETON                                        32
115700040708     C                   EVAL      VAOCRC = *zeros
115800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
115900040708     C                             + ' ' + 'VAOCRC'
116000040708     C                   ENDIF
116100040708     C                   ENDIF
116200040708     C*** CAC
116300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
116400040708     C                   EVAL      PiStr=SkSplitCSV(i)
116500040708     C                   EXSR      CHKNUM
116600040708     C                   IF        PiInt=*on
116700040708     C                   Z-ADD     PiVal         Num5_0
116800040708     C                   MOVEL(p)  Num5_0        VAOCAC
116900040708     C                   ELSE
117000040708     C                   SETON                                        32
117100040708     C                   EVAL      VAOCAC = *zeros
117200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040708     C                             + ' ' + 'VAOCAC'
117400040708     C                   ENDIF
117500040708     C                   ENDIF
117600040708     C*** DAR
117700090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
117800090428     C                   EVAL      PiStr=SkSplitCSV(i)
117900090428     C                   EXSR      CHKNUM
118000090428     C                   IF        PiInt=*on
118100090428     C                   Z-ADD     PiVal         VAODAR
118200090428     C                   ELSE
118300090428     C                   SETON                                        32
118400090428     C                   EVAL      VAODAR = *zeros
118500090428     C                   EVAL      vinmsg = %trimr(vinmsg)
118600090428     C                             + ' ' + 'VAODAR'
118700090428     C                   ENDIF
118800090428     C                   ENDIF
118900040708     C*** ORR
119000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
119100040708     C                   EVAL      PiStr=SkSplitCSV(i)
119200040708     C                   EXSR      CHKNUM
119300040708     C                   IF        PiInt=*on
119400040708     C                   Z-ADD     PiVal         VAOORR
119500040708     C                   ELSE
119600040708     C                   SETON                                        32
119700040708     C                   EVAL      VAOORR = *zeros
119800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040708     C                             + ' ' + 'VAOORR'
120000040708     C                   ENDIF
120100040708     C                   ENDIF
120200030721     C*** NCL
120300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on
120700040723     C                   IF        PiVal > *zeros
120800040708     C                   Z-ADD     PiVal         VAONCL
120900040723     C                   ENDIF
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200040708     C                   EVAL      VAONCL = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400040708     C                             + ' ' + 'VAONCL'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700040708     C*** PKG
121800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiNum=*on
122200040723     C                   IF        PiVal > *zeros
122300040708     C                   Z-ADD     PiVal         VAOPKG
122400040723     C                   ENDIF
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700040708     C                   EVAL      VAOPKG = *zeros
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900040708     C                             + ' ' + 'VAOPKG'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200040708     C*** VLM
123300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
123400040708     C                   EVAL      PiStr=SkSplitCSV(i)
123500040708     C                   EXSR      CHKNUM
123600040708     C                   IF        PiNum=*on
123700040708     C                   Z-ADD     PiVal         VAOVLM
123800040708     C                   ELSE
123900040708     C                   SETON                                        32
124000040708     C                   EVAL      VAOVLM = *zeros
124100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124200040708     C                             + ' ' + 'VAOVLM'
124300040708     C                   ENDIF
124400040708     C                   ENDIF
124500040708     C*** BNC
124600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
124700040708     C                   EVAL      PiStr=SkSplitCSV(i)
124800040708     C                   EXSR      CHKNUM
124900040708     C                   IF        PiInt=*on
125000040708     C                   Z-ADD     PiVal         VAOBNC
125100040708     C                   ELSE
125200040708     C                   SETON                                        32
125300040708     C                   EVAL      VAOBNC = *zeros
125400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125500040708     C                             + ' ' + 'VAOBNC'
125600040708     C                   ENDIF
125700040708     C                   ENDIF
125800040708     C*** BLC
125900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
126000040708     C                   EVAL      PiStr=SkSplitCSV(i)
126100040708     C                   EXSR      CHKNUM
126200040708     C                   IF        PiInt=*on
126300040708     C                   Z-ADD     PiVal         VAOBLC
126400040708     C                   ELSE
126500040708     C                   SETON                                        32
126600040708     C                   EVAL      VAOBLC = *zeros
126700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126800040708     C                             + ' ' + 'VAOBLC'
126900040708     C                   ENDIF
127000040708     C                   ENDIF
127100040708     C*** ATT
127200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
127300040708     C                   EVAL      PiStr=SkSplitCSV(i)
127400040708     C                   EXSR      CHKNUM
127500040708     C                   IF        PiInt=*on
127600040708     C                   Z-ADD     PiVal         VAOATT
127700040708     C                   ELSE
127800040708     C                   SETON                                        32
127900040708     C                   EVAL      VAOATT = *zeros
128000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128100040708     C                             + ' ' + 'VAOATT'
128200040708     C                   ENDIF
128300040708     C                   ENDIF
128400040708     C*** MTC
128500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
128600040708     C                   EVAL      PiStr=SkSplitCSV(i)
128700040708     C                   EXSR      CHKNUM
128800040708     C                   IF        PiInt=*on
128900040708     C                   Z-ADD     PiVal         VAOMTC
129000040708     C                   ELSE
129100040708     C                   SETON                                        32
129200040708     C                   EVAL      VAOMTC = *zeros
129300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129400040708     C                             + ' ' + 'VAOMTC'
129500040708     C                   ENDIF
129600040708     C                   ENDIF
129700040708     C*** KSC
129800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on      AND
130200030721     C                             PiVal<=9999999 AND
130300030721     C                             PiVal>*zeros
130400040708     C                   Z-ADD     PiVal         VAOKSC
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700040708     C                   EVAL      VAOKSC = *zeros
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900040708     C                             + ' ' + 'VAOKSC'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200040708     C*** POR
131300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on    AND
131700030721     C                             PiVal<=999   AND
131800030721     C                             PiVal>*zeros
131900040708     C                   Z-ADD     PiVal         VAOPOR
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200040708     C                   EVAL      VAOPOR = *zeros
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400040708     C                             + ' ' + 'VAOPOR'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700040708     C*** POC
132800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
132900040708     C                   EVAL      PiStr=SkSplitCSV(i)
133000040708     C                   EXSR      CHKNUM
133100040708     C                   IF        PiInt=*on    AND
133200040708     C                             PiVal<=999   AND
133300040708     C                             PiVal>*zeros
133400040708     C                   Z-ADD     PiVal         VAOPOC
133500040708     C                   ELSE
133600040708     C                   SETON                                        32
133700040708     C                   EVAL      VAOPOC = *zeros
133800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133900040708     C                             + ' ' + 'VAOPOC'
134000040708     C                   ENDIF
134100040708     C                   ENDIF
134200040708     C*** STO
134300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
134400040708     C                   EVAL      PiStr=SkSplitCSV(i)
134500040708     C                   EXSR      CHKNUM
134600040708     C                   IF        PiInt=*on
134700040708     C                   Z-ADD     PiVal         VAOSTO
134800040708     C                   ELSE
134900040708     C                   SETON                                        32
135000040708     C                   EVAL      VAOSTO = *zeros
135100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135200040708     C                             + ' ' + 'VAOSTO'
135300040708     C                   ENDIF
135400040708     C                   ENDIF
135500090428     C*
135600091103     C********** GESTIONE CAMPI PERSONALIZZATI
135700120120
135800120120     C*** CRA
135900120120     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOCRA'
136000120120     C                   EVAL      PiStr=SkSplitCSV(i)
136100120120     C                   EXSR      CHKNUM
136200120120     C                   IF        PiInt=*on
136300120120     C                   Z-ADD     PiVal         W_VAOCRA         10 0
136400120120     C                   EVAL      VAOCRA = W_VAOCRA * 1000 + 5
136500120120     C                   ELSE
136600120120     C                   SETON                                        32
136700120120     C                   EVAL      VAOCRA = *zeros
136800120120     C                   EVAL      vinmsg = %trimr(vinmsg)
136900120120     C                             + ' ' + 'VAOCRA'
137000120120     C                   ENDIF
137100120120     C                   ENDIF
137200120120     C*** NCL
137300120120     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONCL'
137400120120     C                   EVAL      PiStr=SkSplitCSV(i)
137500120120     C                   EXSR      CHKNUM
137600120120     C                   IF        PiInt=*on
137700120120     C                   IF        PiVal > *zeros
137800120120     C                   ADD       PiVal         VAONCL
137900120120     C                   ENDIF
138000120120     C                   ELSE
138100120120     C                   SETON                                        32
138200120120     C                   EVAL      VAONCL = *zeros
138300120120     C                   EVAL      vinmsg = %trimr(vinmsg)
138400120120     C                             + ' ' + 'VAONCL'
138500120120     C                   ENDIF
138600120120     C                   ENDIF
138700120120     C*** PKG
138800120120     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOPKG'
138900120120     C                   EVAL      PiStr=SkSplitCSV(i)
139000120120     C                   EXSR      CHKNUM
139100120120     C                   IF        PiNum=*on
139200120120     C                   IF        PiVal > *zeros
139300120120     C                   ADD       PiVal         VAOPKG
139400120120     C                   ENDIF
139500120120     C                   ELSE
139600120120     C                   SETON                                        32
139700120120     C                   EVAL      VAOPKG = *zeros
139800120120     C                   EVAL      vinmsg = %trimr(vinmsg)
139900120120     C                             + ' ' + 'VAOPKG'
140000120120     C                   ENDIF
140100120120     C                   ENDIF
140200120120     C*** VLM
140300120120     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOVLM'
140400120120     C                   EVAL      PiStr=SkSplitCSV(i)
140500120120     C                   EXSR      CHKNUM
140600120120     C                   IF        PiNum=*on
140700120120     C                   ADD       PiVal         VAOVLM
140800120120     C                   ELSE
140900120120     C                   SETON                                        32
141000120120     C                   EVAL      VAOVLM = *zeros
141100120120     C                   EVAL      vinmsg = %trimr(vinmsg)
141200120120     C                             + ' ' + 'VAOVLM'
141300120120     C                   ENDIF
141400120120     C                   ENDIF
141500090428     C*
141600090428     C*
141700090428     C****************************************
141800090428     C*
141900030715     C*
142000030715     C                   ENDIF
142100030715     C                   EVAL      i = i + 1
142200030715     C                   ENDDO
142300040723     C*
142400030715     C                   ENDIF
142500020204     C*
142600010607     C                   ENDSR
142700010607     C*----------------------------------------------------*
142800080702
142900080702
143000080702     C*----------------------------------------------------*
143100080702     C*  CONTROLLO NUMERICITA' CAMPI
143200080702     C*----------------------------------------------------*
143300080702     C     CHKNUM        BEGSR
143400080702     C*
143500080702     C                   IF        PiDecChr = *blanks
143600080702     C                   EVAL      PiDecChr = ','
143700080702     C                   ENDIF
143800080702     C*
143900080702     C                   callp     UBISNUM_Check(PiStr
144000080702     C                                          :PiDecChr
144100080702     C                                          :PiVal
144200080702     C                                          :PiNum
144300080702     C                                          :PiInt)
144400080702     C*
144500080702     C                   ENDSR
144600080702     C***
144700080702
144800010601
144900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145000010601     C     repfil        BEGSR
145100010601     C*
145200010601     C                   if        invfil = *zeros and
145300010601     C                             depfil > *zeros and
145400010629     C                             (vinflg = *blanks or
145500010629     C                              vinflg = *zeros)
145600010601     C
145700010601     C                   eval      invfil = depfil
145800010601     C                   endif
145900010601     C*
146000010601     C                   if        depfil <> invfil and
146100010601     C                             invfil > *zeros
146200010601     C                   eval      flgMulti = '1'
146300010601     C                   if        vinflg = *blanks
146400010601     C                   add       1             cntNonEl
146500010601     C                   endif
146600010601     C                   endif
146700010601     C*
146800010601     C                   if        vinflg = '2'
146900010601     C                   eval      flgStato = '2'
147000010601     C                   endif
147100010601     C*
147200010601     C                   ENDSR
147300010601     C***
147400010330
147500010330
147600990920      /TITLE Invio dei dati al punto operativo.
147700000613     C     invio         BEGSR
147800990920     C*
147900990920     C                   reset                   dscmz
148000010601     C                   move      invfil        cmzdst
148100990920     C                   eval      cmzfld = vlrfou
148200990920     C                   eval      cmzmbd = vlrhdl
148300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
148400000710     C                   if        prmfir = *blanks
148500040708     C                   eval      cmzfla = 'FNVAO00F'
148600040708     C                   eval      cmzmba = 'FNVAO00F'
148700000710     C                   else
148800000710     C                   eval      cmzfla = prmfir
148900000710     C                   eval      cmzmba = prmfir
149000000710     C                   endif
149100990920     C                   eval      cmznrr = *zeros
149200990920     C                   move      §ctrok        cmznrr
149300021018     C                   eval      cmzlba = vlrfl1
149400990920     C                   call(e)   'TIS711C'
149500990920     C                   parm                    dscmz
149600990921     C                   parm      *blanks       esito
149700990920     C                   if        %error
149800990920     C                             or cmzerr = '1'
149900990921     C                             or esito  = '1'
150000000710     C                   eval      wrkesito = '3'
150100990920     C                   endif
150200990920     C*
150300000613     C                   ENDSR
150400010601
150500010601
150600010601      /TITLE Invio dei dati al punto operativo.
150700010601     C     opeini        BEGSR
150800010601     C*
150900010601     C* Inizializzo flag e contatori operativi
151000010601     C                   movel     '0'           flgGiro           1
151100010601     C                   movel     '0'           flgMulti          1
151200010601     C                   movel     '1'           flgStato          1
151300010615     C                   movel     '0'           flgOk             1
151400010601     C                   z-add     *zeros        cntNonEl         10 0
151500010601     C                   z-add     *zeros        depfil            3 0
151600010601     C                   z-add     *zeros        invfil            3 0
151700010601     C*
151800010601     C                   ENDSR
151900010601     C***
152000050421
152100050421
152200091130     C     *pssr         BEGSR
152300050421     C*
152400091130     C                   eval      wrkesito = '2'
152500050421     C*
152600091130     C                   ENDSR     '*CANCL'
152700050421     C***
152800060306
152900060306
153000060306     C*-------------------------------------------------------------------------*
153100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
153200060306     C*-------------------------------------------------------------------------*
153300060306     C     sr_Control    begsr
153400060306     C*
153500060306     C* inizializzo a ok il flag d controllo abilitazione
153600060306     C                   eval      Cli_Abil = *on
153700060306     C
153800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
153900060306     C                   clear                   woriIDC
154000060306     C                   eval      kteCOD = 'VAP'
154100060306     C                   eval      kteKE1 = oriIDC
154200060306     C     keytbe        chain     TNTBE01L
154300060306     C                   if        %found(Tntbe01l)
154400060306     C                   eval      woriIDC = TbeKe2
154500060306     C                   else
154600060306     C                   eval      Cli_Abil = *off
154700060306     C                   endif
154800060306     C*
154900060306     C                   endsr
155000010330
155100010330
155200000613     C     *inzsr        BEGSR
155300990910     C*
155400990910     C     *entry        plist
155500990920     C                   parm                    tivlrds
155600990921     C                   parm      wrkesito      esito
155700000724     C                   parm                    prmlit
155800000710     C                   parm                    prmfir
155900010330     C*
156000010330     C* CALCOLA LA DATA CORRENTE
156100091127     C                   z-add     *zeros        datcor            8 0
156200091127     C                   time                    wn14             14 0
156300091127     C                   movel     wn14          wn6               6 0          *ORA (6)
156400091127     C                   eval      datcor = %dec(%date() : *ISO)
156500040723     C*
156600040723     C* Campi riferito al database
156700040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
156800040723     C     *like         define    tbeke1        kteKE1
156900040723     C*
157000040723     C* Definizione chiavi di lettura
157100040723     C* TNTBE01L - parziale
157200040723     C     keytbe        klist
157300040723     C                   kfld                    kteCOD                         *UTENTE
157400040723     C                   kfld                    kteKE1                         *CODICE
157500000613     C*
157600000613     C                   ENDSR
157700000613     C***
157800990908
157900040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
158000120120VAORFA;FIL001;W_VAONCL;W_VAOPKG;W_VAOVLM;X;X;X;X;X;X;X;X;X;X;X;X;
158100120120W_VAOCRA;X;X;X;X;X;
158200120120
