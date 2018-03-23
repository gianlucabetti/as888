000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121129      *
000300121129      * PARTICOLARITA':
000400121129      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600120209     F*
000700040723     Ffnacr01l  if   e           k disk
000800090618     Ftntbe01l  if   e           k disk
000900990910     Ftivin00r  uF   E             DISK    usropn
001000040708     FFnvaowwr  O    E             DISK    usropn
001100040723     Ftiori00f  O    E             DISK
001200050826     FTIVGD00F  O    E             DISK
001300000313     D*
001400040709     D*------------
001500040709     D* SCHIERE A PROGRAMMA
001600040709     D*------------
001700040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D dscmz         e ds                  inz
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500050826     D fnvapds       e ds                  extname(fnvap00f)
002600040723     D dorm01        e ds
002700040723     D dfar          e ds
002800090618     d trul33ds      e ds
002900090618     d kpjba         e ds
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003600060306     D Cli_Abil        s              1    inz(*off)
003700060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003800090618     d §numori         s              7  0 inz
003900120123     d wData_EUR       s               d   datfmt(*eur)
004000120123     d wData_ISO       s               d   datfmt(*iso)
004100120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004200120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004300120905     D Intestazioni    s               n   inz(*on)
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600030822     D*------------------
005700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005800030822     D*------------------
005900030822     D posDaDft        S              3  0 INZ(*zeros)
006000030822     D posADft         S              3  0 INZ(*zeros)
006100030822     D j               s              3  0 INZ(1)
006200030822     D wGiroDft        s              1  0 INZ(*zeros)
006300040723     D*------------------
006400040723     D* DS REPERIMENTO DATI CLIENTE
006500040723     D*-------------------
006600040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006700040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006800040723     D INDDS         E DS                  EXTNAME(CNIND00F)
006900040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007000040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007100121129     D*------------------
007200121129     D w70             s             70
007300080702
007400080702     D*------------------
007500080702     D* LINKING A DEFINIZIONI ESTERNE
007600080702     D*------------------
007700090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007900091026
008000091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008100091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008200091026
008300080702
008400031201     D*------------------
008500031201     D* Costanti
008600031201     D*------------------
008700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008900010330
009000010330
009100010330
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400990921     C                   reset                   wrkesito
009500010601     C*
009600010601     C                   exsr      opeini
009700041011     C                   exsr      PREELA
009800040708     C                   exsr      rwvao
009900010601     C*
010000010601     C                   seton                                        lr
010100010601
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010700010601     C*--------------------------------------------------------
010800010601     C     PREELA        BEGSR
010900010601     C*
011000010601     C* SE OCCORRE SPEDIRE IN FILIALE
011100041011     C                   if        flgGiro = '0'
011200010601     C*
011300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011400010601     C                   eval      flgGiro = '1'
011500041011     C*
011600041011     C                   endif
011700010601     C*
011800010601     C                   ENDSR
011900010601     C***
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012400010601     C*--------------------------------------------------------
012500010601     C     ENDELA        BEGSR
012600000616     C*
012700120209     C                   ENDSR
012800010601
012900010601
013000010330     C*--------------------------------------------------------
013100040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
013200010330     C*--------------------------------------------------------
013300040708     C     RWVAO         BEGSR
013400010330     C*
013500010330     C                   if        not %open(tivin00r)
013600010330     C                   open      tivin00r
013700010330     C                   endif
013800040708     C                   if        not %open(fnvaowwr)
013900040708     C                   open      fnvaowwr
014000010330     C                   endif
014100010330     C*
014200010604     C                   clear                   §CTROK
014300010604     C                   clear                   §CTRMO
014400010604     C                   clear                   §CTRNO
014500120123     C*
014600120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
014700120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
014800120905     C                   IF        Intestazioni = *off
014900120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
015000120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015100120123     C                   exsr      inzvar
015200120123     C                   exsr      impvao
015300120905     C                   ENDIF
015400010330     C*
015500030822     C                   do        *HIVAL
015600010330     C*
015700030822     C                   read      tivin00r                               70
015800010618     C*
015900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016000010618     C                   if        vinflg = '1'
016100010618     C                   eval      flgOk = '1'
016200010618     C                   endif
016300010618     C*
016400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016500040510     C                   z-add     *zeros        wLenVINDTA        4 0
016600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017000040510     C                   z-add     1             wFlgVINDTA
017100040510     C                   leave
017200040510     C                   endif
017300040510     C                   enddo
017400040510     C*
017500040218     C                   if        vindta > *blanks AND
017600040510     C                             wFlgVINDTA = 1
017700010330     C                   add       1             rrnum
017800010330     C*
017900010601     C                   if        *in70 = *off and
018000010330     C                             (vinflg = *blanks
018100010330     C                              or vinflg = '0'
018200010330     C                              or vinflg = '2')
018300010330     C*
018400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018500010711     C                   if        vinflg = *blanks or vinflg = '0'
018600010711     C                   clear                   vinmsg
018700010711     C                   endif
018800010601     C*
018900010330     C                   exsr      inzvar
019000040708     C                   exsr      impvao
019100010601     C*
019200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300040708     C                   eval      depfil = VAOPOE
019400010601     C                   exsr      repfil
019500010601     C                   if        depfil = invfil
019600021025     C                   if        vlrpoi = 999
019700040708     C                   move(P)   invfil        VAOPOE
019800021025     C                   else
019900040708     C                   move(P)   vlrpoi        VAOPOE
020000021025     C                   endif
020100010601     C*
020200010604     C* Ebbene...
020300010604     C*
020400030822     C  N31              add       1             §CTROK            7 0
020500030822     C   32              add       1             §CTRMO            7 0
020600030822     C   31              add       1             §CTRNO            7 0
020700030822     C                   if        wGiro = 2
020800120123     C  N31              EXSR      CHKCOR
020900120123     C  N31              EXSR      WRIORI
021000120123     C  N31              EXSR      WRIVGD
021100120123     C  N31              write     FNVAO000
021200030822     C                   endif
021300010604     C*
021400010604     C                   if        *in31 = *off and
021500010604     C                             *in32 = *off
021600010604     C                   eval      vinflg = '1'
021700010604     C                   else
021800010604     C                   eval      vinflg = '2'
021900010604     C                   endif
022000010604     C                   endif
022100010601     C*
022200010604     C                   endif
022300010604     C*
022400010330     C                   else
022500010330     C                   eval      vinflg = '1'
022600010330     C                   endif
022700010601     C*
022800010601     C  N70              update    tivin000
022900010330     C*
023000030822     C  N70              enddo
023100010601     C
023200010601     C*
023300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023400010601     C                   if        cntNonEl = *zeros or
023500010601     C                             flgMulti = '0'
023600010330     C* Se non ci sono record con errori ...
023700010601     C                   if        §ctrno = 0 and
023800010604     C                             §ctrmo = 0 and
023900010601     C                             flgStato <> '2'
024000010330     C* ... restituisco esito OK.
024100010330     C                   eval      wrkesito = '0'
024200010330     C                   else
024300010330     C                   if        §ctrok > 0
024400010330     C                   eval      wrkesito = '1'
024500010330     C                   else
024600010615     C                   if        flgOk = '0'
024700010615     C                   eval      wrkesito = '2'
024800010615     C                   else
024900010615     C                   eval      wrkesito = '6'
025000010615     C                   endif
025100010330     C                   endif
025200010330     C                   endif
025300010601     C                   else
025400010601     C                   eval      wrkesito = '9'
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        %open(tivin00r)
025800010330     C                   close     tivin00r
025900010330     C                   endif
026000040708     C                   if        %open(fnvaowwr)
026100040708     C                   close     fnvaowwr
026200010330     C                   endif
026300010601     C*
026400010601     C                   if        vlrpoi <> 999
026500010601     C                   eval      invfil = vlrpoi
026600010601     C                   endif
026700010330     C*
026800010330     C                   if        §ctrok > 0
026900010601     C                             and invfil > *zeros
027000010330     C                   exsr      invio
027100010330     C                   endif
027200010601     C*
027300010618     C                   if        flgGiro = '1'
027400010601     C                   exsr      endela
027500010618     C                   endif
027600010330     C*
027700010330     C                   ENDSR
027800010330     C***
027900040723
028000040723
028100040723     C*------------------------------------------------------------------------*
028200040723     C* CHKCOR - Considerazioni su VAOCOR
028300040723     C*------------------------------------------------------------------------*
028400040723     C     CHKCOR        BEGSR
028500040723     C*
028600040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
028700040723     C                   if        vaoTCO = 'F'
028800040723     C* Quindi verifico se il codice ordinante non è valorizzato
028900040723     C                   if        vaoCOR = *zeros  AND
029000040723     C                             vaoRSO = *blanks AND
029100040723     C                             vaoINO = *blanks AND
029200040723     C                             vaoCAO = *blanks AND
029300040723     C                             vaoLOO = *blanks AND
029400040723     C                             vaoPRO = *blanks AND
029500040723     C                             vaoNAO = *blanks
029600040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
029700040723     C                   movel     '0'           wFlgAcr           1
029800040723     C                   move      vlrKSC        wCli              7 0
029900040723     C                   move(p)   wCli          acrCRO
030000040723     C     acrCRO        setll     fnacr01l
030100040723     C                   if        %found(fnacr01l)
030200040723     C                   read      fnacr01l
030300040723     C                   dow       not %eof(fnacr01l)
030400040723     C                   movel     acrCRO        wCliAcr           7 0
030500040723     C                   if        wCliAcr = wCli
030600040723     C                   eval      vaoCOR = acrCRO
030700040723     C                   movel     '1'           wFlgAcr
030800040723     C                   leave
030900040723     C                   else
031000040723     C                   leave
031100040723     C                   endif
031200040723     C                   read      fnacr01l
031300040723     C                   enddo
031400040723     C                   endif
031500040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
031600040723     C* dati anagrafici dell'ordinante dal piano dei conti
031700040723     C                   if        wFlgAcr = '0'
031800040723     C                   clear                   BS69DS
031900040723     C                   clear                   ACODS
032000040723     C                   clear                   INDDS
032100040723     C                   clear                   CLPDS
032200040723     C                   clear                   CLSDS
032300040723     C                   move(P)   vlrKSC        I69KAC
032400040723     C                   move(P)   vlrKSC        I69KIN
032500040723     C                   call      'TIBS69R'
032600040723     C                   parm                    BS69DS
032700040723     C                   parm                    ACODS
032800040723     C                   parm                    INDDS
032900040723     C                   parm                    CLPDS
033000040723     C                   parm                    CLSDS
033100040723     C     O69ERR        ifne      '1'
033200040723     C                   eval      vaoRSO = ACORAG
033300040723     C                   eval      vaoINO = INDVIA
033400040723     C                   movel(P)  INDCAP        vaoCAO
033500040723     C                   eval      vaoLOO = INDCIT
033600040723     C                   eval      vaoPRO = INDPRV
033700040723     C                   eval      vaoNAO = INDSTA
033800040723     C                   endif
033900040723     C                   endif
034000040723     C                   endif
034100040723     C                   endif
034200040723     C*
034300040723     C                   ENDSR
034400040723
034500040723
034600040723     C*------------------------------------------------------------------------*
034700040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
034800040723     C*------------------------------------------------------------------------*
034900040723     C     WRIORI        BEGSR
035000040723     C*
035100090618     C* Come prima cosa stacco un numeratore da AZNUM
035200090618     C                   clear                   trul33ds
035300090618     C                   eval      I33OPE = *zeros
035400090618     C                   eval      I33CNU = 600
035500090618     C                   eval      I33NUM = 1
035600090618     C                   movel     TRUL33DS      KPJBU
035700090618     C                   call      'TRUL33R'
035800090618     C                   parm                    KPJBA
035900090618     C                   movel     KPJBU         TRUL33DS
036000090618     C                   if        O33ERR = *zeros
036100090618     C                   z-add     O33NRF        §numori
036200090618     c                   else
036300090618     c                   eval      §numori = *all'9'
036400090618     C                   endif
036500040723     C*
036600040723     C* Quindi imposto i campi "extra-VAO"
036700040723     C                   if        vlrKSC = '0ORM0000'
036800040723     C*
036900040723     C                   if        vaoCOR > *zeros
037000040723     C                   movel     vaoCOR        wrkCOR7           7 0
037100040723     C*
037200040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
037300040723     C* codice padre. Se è un padre assegno già il codice ordinante.
037400040723     C                   clear                   tibs10ds
037500040723     C                   eval      d10drf = datcor                              *data riferimento
037600040723     C                   eval      d10paf = 'P'                                 *cerca il padre
037700040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
037800091026     C*
037900091026     C* Preparazione chiamata al TIBS10R1
038000091026     C                   clear                   tibs10r1ds
038100091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
038200091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
038300091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
038400091026     C*
038500091026     C                   call      'TIBS10R1'
038600091026     C                   parm                    tibs10r1ds
038700091026     C*
038800091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
038900040723     C*
039000040723     C                   if        d10err <> *blanks or                         *No unif. o padre
039100040723     C                             d10cop =  *zeros  or
039200040723     C                             d10cop =  d10cod
039300040723     C                   move      d10cod        oriIDC
039400040723     C                   else                                                   *Figlio
039500040723     C                   move      d10cop        oriIDC
039600040723     C                   endif
039700040723     C*
039800040723     C                   else
039900040723     C                   eval      oriIDC = *zeros
040000040723     C                   endif
040100040723     C*
040200040723     C                   else
040300040723     C                   eval      oriIDC = vlrKSC
040400040723     C                   endif
040500040723     C*
040600040723     C                   eval      oriPRG = §numori
040700040723     C                   eval      oriDER = datcor
040800040723     C                   eval      oriOER = wn6
040900040723     C                   eval      oriPOE = vaoPOE
041000040723     C                   eval      oriNSR = vaoNSR
041100040723     C                   eval      oriNOR = vaoNOR
041200040723     C                   eval      oriNRV = vaoNRV
041300040723     C                   eval      oriFLO = *blanks
041400040723     C*
041500040723     C* Infine valorizzo la chiave esterna sul file FNVAO
041600040723     C                   eval      dorm01 = vaoFLO
041700040723     C                   movel     §numori       §ormpg
041800050412     C*
041900050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
042000130326     C                   movel     'S'           §ormfr
042100130326     C                   movel     'S'           §ormfd
042200130326     C                   movel     'N'           §orcomc
042300050412     C*
042400130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
042500040723     C                   eval      vaoFLO = dorm01
042600130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
042700130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
042800130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
042900040723     C*
043000040723     C* ...quindi scrivo il file TIORI00F
043100120905     C  N55              WRITE     TIORI000
043200040723     C*
043300040723     C                   ENDSR
043400050826
043500050826
043600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
043700050826     C     wriVGD        BEGSR
043800050826     C*
043900050826     C* Reperisco la descrizione della fase da tabella
044000050826     C                   movel(p)  'FAR'         KteCOD
044100050826     C                   movel(p)  '000'         KteKE1
044200050826     C     KEYtbe        chain     tntbe01l
044300050826     C                   if        %found(tntbe01l)
044400050826     C                   movel(P)  tbeuni        DFAR
044500050826     C                   else
044600050826     C                   clear                   DFAR
044700050826     C                   endif
044800050826     C*
044900050826     C* Valorizzo buffer record
045000050826     C                   clear                   FNVAPDS
045100050826     C                   eval      vapIDC = oriIDC
045200050826     C                   eval      vapPOE = oriPOE
045300050826     C                   eval      vapRFA = vaoRFA
045400050826     C                   eval      vapPOG = oriPOE
045500050826     C                   eval      vapDAE = oriDER
045600050826     C                   eval      vapORE = oriOER
045700050826     C                   if        d§fardva <> *blanks
045800050826     C                   eval      vapDFA = d§fardva
045900050826     C                   else
046000050826     C                   eval      vapDFA = d§fardes
046100050826     C                   endif
046200050826     C*
046300050826     C                   clear                   tivgd000
046400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
046500050826     C                   eval      vgdTIP = 'VP'
046600050826     C                   eval      vgdKSU = vapIDC
046700050826     C                   eval      vgdTSC = 'WW'
046800050826     C                   eval      vgdDAT = datcor
046900120123     C                   eval      vgdPGM = 'TITZ10R'
047000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
047100060306     C                   exsr      sr_Control
047200060306     C                   if        Cli_Abil = *on
047300060306     C                   eval      vgdKSU = woriIDC
047400120905     C  N55              write     tivgd000
047500060306     C                   endif
047600050826     C*
047700050826     C                   ENDSR
047800050826     C*------------------------------------------------------------------------*
047900010601
048000010601
048100010330     C*----------------------------------------------------*
048200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048300010330     C*----------------------------------------------------*
048400010330     C     INZVAR        BEGSR
048500010330     C*
048600030822     C* Inizializzo variabili di wrk
048700020204     C                   Z-ADD     *zeros        Num5_0            5 0
048800120124
048900030822     C*
049000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049100120123     C                   CLEAR                   FNVAO000
049200120123     C                   CLEAR                   TIORI000
049300030822     C                   CLEAR                   SkSplitCSV
049400030822     C                   EVAL      i = 1
049500030822     C                   EVAL      posDa = *zeros
049600030822     C                   EVAL      posA  = *zeros
049700030822     C*
049800030822     C* Reimposto i valori di default
049900030822     C                   EXSR      DEFCAM
050000030822     C*
050100010330     C                   ENDSR
050200010330     C*----------------------------------------------------*
050300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050400010330     C*----------------------------------------------------*
050500010330     C     DEFCAM        BEGSR
050600030822     C*
050700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050800030715     C* e delimitatore testo.
050900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051200040723     C*
051300080707     C* Imposto i valori d default d alcuni campi
051400080708     C*
051500080708     C                   CLEAR                   DORM01
051600120822     C                   EVAL      VAOFLO = DORM01
051700080707     C*
051800121129     C                   EVAL      VAOCOR = 1661796000
051900121129     C                   EVAL      VAOPOE = 166
052000121129     C                   EVAL      VAOTOR = 'S'
052100121129     C                   EVAL      VAOKSC = 1661796
052200121129     C                   EVAL      VAOCTR = '102'
052300121129     C                   EVAL      VAOPAG = 'O'
052400121129     C                   EVAL      VAODDT = 'S'
052500120905     C* se il 1° char dei parametri è T=Test
052600120905     C                   SETOFF                                       55
052700120905     C                   IF        %subst(vlrppt:1:1) = 'T'
052800120905     C* inibisco la scrittura dei file TIORI e TIVGD
052900120905     C                   SETON                                        55
053000120905     C* imposto il PO di emissione come filiale fittizia
053100120905     C                   EVAL      VAOPOE = 999
053200120905     C                   ENDIF
053300091127     C*
053400091204     C* IMPOSTO I CAMPI PERSONALIZZATI
053500100416     C
053600030822     C*
053700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053800030822     C                   EVAL      posDaDft = 1
053900030822     C                   EVAL      posADft  = 0
054000030822     C                   EVAL      wGiroDft = 0
054100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054200030822     C                             posDaDft > 0
054300030822     C*
054400030822     C* Gestisco il 1° giro
054500030822     C                   IF        wGiroDft = 0
054600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054800030822     C* Incremento il contatore dei "giri"
054900030822     C                   EVAL      wGiroDft = 1
055000030822     C                   ELSE
055100030822     C                   EVAL      posDaDft = posADft
055200030822     C                   ENDIF
055300030822     C* Eseguo lo scan x trovare la fine del campo corrente
055400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055500030822     C*
055600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055800030822     C                   IF        posDaDft > 0 AND
055900030822     C                             posADft  > 0
056000040708     C* POE
056100040708     C                   IF        %subst(
056200040708     C                             %subst(vlrppt:posDaDft+1:
056300040708     C                             posADft-posDaDft-1):1:3)
056400040708     C                             = 'POE'
056500040708     C                   EVAL      PiStr=%trim(%subst(
056600040708     C                             %subst(vlrppt:posDaDft+1:
056700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800040708     C                   EXSR      CHKNUM
056900040708     C                   IF        PiInt=*on
057000040708     C                   Z-ADD     PiVal         VAOPOE
057100040708     C                   ENDIF
057200040708     C                   ENDIF
057300040708     C* TOR
057400040708     C                   IF        %subst(
057500040708     C                             %subst(vlrppt:posDaDft+1:
057600040708     C                             posADft-posDaDft-1):1:3)
057700040708     C                             = 'TOR'
057800040708     C                   EVAL      VAOTOR=%trim(%subst(
057900040708     C                             %subst(vlrppt:posDaDft+1:
058000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100040708     C                   ENDIF
058200040708     C* TCO
058300040708     C                   IF        %subst(
058400040708     C                             %subst(vlrppt:posDaDft+1:
058500040708     C                             posADft-posDaDft-1):1:3)
058600040708     C                             = 'TCO'
058700040708     C                   EVAL      VAOTCO=%trim(%subst(
058800040708     C                             %subst(vlrppt:posDaDft+1:
058900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000040708     C                   ENDIF
059100040707     C* COR
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500040707     C                             = 'COR'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100040708     C                   Z-ADD     PiVal         VAOCOR
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400040708     C* CRC
060500040708     C                   IF        %subst(
060600040708     C                             %subst(vlrppt:posDaDft+1:
060700040708     C                             posADft-posDaDft-1):1:3)
060800040708     C                             = 'CRC'
060900040708     C                   EVAL      PiStr=%trim(%subst(
061000040708     C                             %subst(vlrppt:posDaDft+1:
061100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200040708     C                   EXSR      CHKNUM
061300040708     C                   IF        PiInt=*on
061400040708     C                   Z-ADD     PiVal         VAOCRC
061500040708     C                   ENDIF
061600040708     C                   ENDIF
061700040708     C* FFD
061800040708     C                   IF        %subst(
061900040708     C                             %subst(vlrppt:posDaDft+1:
062000040708     C                             posADft-posDaDft-1):1:3)
062100040708     C                             = 'FFD'
062200040708     C                   EVAL      VAOFFD=%trim(%subst(
062300040708     C                             %subst(vlrppt:posDaDft+1:
062400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500040708     C                   ENDIF
062600040708     C* ORR
062700040708     C                   IF        %subst(
062800040708     C                             %subst(vlrppt:posDaDft+1:
062900040708     C                             posADft-posDaDft-1):1:3)
063000040708     C                             = 'ORR'
063100040708     C                   EVAL      PiStr=%trim(%subst(
063200040708     C                             %subst(vlrppt:posDaDft+1:
063300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400040708     C                   EXSR      CHKNUM
063500040708     C                   IF        PiInt=*on
063600040708     C                   Z-ADD     PiVal         VAOORR
063700040708     C                   ENDIF
063800040708     C                   ENDIF
063900040712     C* NCL
064000040712     C                   IF        %subst(
064100040712     C                             %subst(vlrppt:posDaDft+1:
064200040712     C                             posADft-posDaDft-1):1:3)
064300040712     C                             = 'NCL'
064400040712     C                   EVAL      PiStr=%trim(%subst(
064500040712     C                             %subst(vlrppt:posDaDft+1:
064600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700040712     C                   EXSR      CHKNUM
064800040712     C                   IF        PiInt=*on
064900040712     C                   Z-ADD     PiVal         VAONCL
065000040712     C                   ENDIF
065100040712     C                   ENDIF
065200040712     C* PKG
065300040712     C                   IF        %subst(
065400040712     C                             %subst(vlrppt:posDaDft+1:
065500040712     C                             posADft-posDaDft-1):1:3)
065600040712     C                             = 'PKG'
065700040712     C                   EVAL      PiStr=%trim(%subst(
065800040712     C                             %subst(vlrppt:posDaDft+1:
065900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000040712     C                   EXSR      CHKNUM
066100040712     C                   IF        PiNum=*on
066200040712     C                   Z-ADD     PiVal         VAOPKG
066300040712     C                   ENDIF
066400040712     C                   ENDIF
066500040712     C* VLM
066600040712     C                   IF        %subst(
066700040712     C                             %subst(vlrppt:posDaDft+1:
066800040712     C                             posADft-posDaDft-1):1:3)
066900040712     C                             = 'VLM'
067000040712     C                   EVAL      PiStr=%trim(%subst(
067100040712     C                             %subst(vlrppt:posDaDft+1:
067200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300040712     C                   EXSR      CHKNUM
067400040712     C                   IF        PiNum=*on
067500040712     C                   Z-ADD     PiVal         VAOVLM
067600040712     C                   ENDIF
067700040712     C                   ENDIF
067800040708     C* NAM
067900040708     C                   IF        %subst(
068000040708     C                             %subst(vlrppt:posDaDft+1:
068100040708     C                             posADft-posDaDft-1):1:3)
068200040708     C                             = 'NAM'
068300040708     C                   EVAL      VAONAM=%trim(%subst(
068400040708     C                             %subst(vlrppt:posDaDft+1:
068500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600040708     C                   ENDIF
068700040708     C* PAG
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100040708     C                             = 'PAG'
069200040708     C                   EVAL      VAOPAG=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600040708     C* KSC
069700040708     C                   IF        %subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):1:3)
070000040708     C                             = 'KSC'
070100040708     C                   EVAL      PiStr=%trim(%subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400040708     C                   EXSR      CHKNUM
070500040708     C                   IF        PiInt=*on
070600040708     C                   Z-ADD     PiVal         VAOKSC
070700040708     C                   ENDIF
070800040708     C                   ENDIF
070900040708     C* CTR
071000040708     C                   IF        %subst(
071100040708     C                             %subst(vlrppt:posDaDft+1:
071200040708     C                             posADft-posDaDft-1):1:3)
071300040708     C                             = 'CTR'
071400040708     C                   EVAL      VAOCTR=%trim(%subst(
071500040708     C                             %subst(vlrppt:posDaDft+1:
071600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700040708     C                   ENDIF
071800040708     C* POC
071900040708     C                   IF        %subst(
072000040708     C                             %subst(vlrppt:posDaDft+1:
072100040708     C                             posADft-posDaDft-1):1:3)
072200040708     C                             = 'POC'
072300040708     C                   EVAL      PiStr=%trim(%subst(
072400040708     C                             %subst(vlrppt:posDaDft+1:
072500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600040708     C                   EXSR      CHKNUM
072700040708     C                   IF        PiInt=*on
072800040708     C                   Z-ADD     PiVal         VAOPOC
072900040708     C                   ENDIF
073000040708     C                   ENDIF
073100040708     C* NO1
073200040708     C                   IF        %subst(
073300040708     C                             %subst(vlrppt:posDaDft+1:
073400121129     C                             posADft-posDaDft-1):1:3)
073500040708     C                             = 'NO1'
073600040708     C                   EVAL      VAONO1=%trim(%subst(
073700040708     C                             %subst(vlrppt:posDaDft+1:
073800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900040708     C                   ENDIF
074000040708     C* DDT
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400040708     C                             = 'DDT'
074500040708     C                   EVAL      VAODDT=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900040708     C* RFA
075000040708     C                   IF        %subst(
075100040708     C                             %subst(vlrppt:posDaDft+1:
075200040708     C                             posADft-posDaDft-1):1:3)
075300040708     C                             = 'RFA'
075400040708     C                   EVAL      VAORFA=%trim(%subst(
075500040708     C                             %subst(vlrppt:posDaDft+1:
075600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700040708     C                   ENDIF
075800040708     C* FLO
075900040708     C                   IF        %subst(
076000040708     C                             %subst(vlrppt:posDaDft+1:
076100040708     C                             posADft-posDaDft-1):1:3)
076200040708     C                             = 'FLO'
076300040708     C                   EVAL      VAOFLO=%trim(%subst(
076400040708     C                             %subst(vlrppt:posDaDft+1:
076500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600091103     C*
076700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
076800080702     C* e posizione 7 del VAOFLO
076900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
077000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
077100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
077200040708     C                   ENDIF
077300030822     C* ...
077400030822     C                   ENDIF
077500030822     C                   ENDDO
077600020204     C*
077700010330     C                   ENDSR
077800010607     C*----------------------------------------------------*
077900040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
078000010607     C*----------------------------------------------------*
078100040708     C     IMPVAO        BEGSR
078200010607     C*
078300010607     C                   SETOFF                                       3132
078400120124
078500030515     C*
078600040708     C                   MOVEL     datcor        VAODAO
078700040708     C                   MOVEL     wn14          VAOOAO
078800050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078900050314     C     minu:maiu     XLATE     vindta        vindta
079000010607     C*
079100030715     C**********
079200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079300030715     C**********
079400030715     C                   IF        wGiro = *zeros
079500040709     C*
079600040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
079700040709     C                   EVAL      depvinDTA = vinDTA
079800040709     C*
079900040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
080000040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080100040709     C                                      %trim(intcol(3))
080200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080300050314     C**!!!minu:maiu     XLATE     vindta        vindta
080400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080500040324     C                   DOW       posDa <= %len(%trimr(vindta))
080600030715     C*
080700030715     C* Gestisco il 1° campo
080800030715     C                   IF        i = 1
080900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081000030715     C                   EVAL      posDa = 1
081100030715     C* Eseguo lo scan x trovare la fine del primo campo
081200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C* X i campi successivi al 1°
081700030715     C                   ELSE
081800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081900030715     C                   EVAL      posDa = posA + 1
082000030715     C* Eseguo lo scan x trovare la fine del campo corrente
082100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082200030715     C* Gestisco l'ultimo campo
082300030715     C                   IF        posA = *zeros
082400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C                   LEAVE
082900030715     C                   ELSE
083000030715     C* A questo punto "estraggo" il campo corrente
083100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083200030715     C                                                    (posA-posDa))
083300030715     C                   ENDIF
083400030715     C                   ENDIF
083500030715     C* Incremento il contatore d campo
083600030715     C                   EVAL      i = i +1
083700030715     C                   ENDDO
083800030715     C*
083900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084000030715     C* delimitazione testo
084100030715     C                   EVAL      i = 1
084200030715     C                   DOW       i < %elem(SkSplitFLD)
084300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084500030715     C                   EVAL      i = i + 1
084600030715     C                   ENDDO
084700030715     C                   EVAL      wGiro = 1
084800040709     C* Ripristino la 1° riga originale
084900040709     C                   EVAL      vinDTA = depvinDTA
085000030715     C                   ELSE
085100030715     C                   EVAL      wGiro = 2
085200031204     C**********
085300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085400031204     C* d separatore campo
085500031204     C**********
085600040709     C                   EVAL      vindta = %trim(vindta)
085700040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085800040709     C                   EVAL      vindta = %subst(vindta:2)
085900040709     C                   ENDDO
086000040709     C*
086100040709     C                   Z-ADD     *zeros        lunghInput        4 0
086200040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086300040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086400040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086600040709     C                   ENDDO
086700030715     C**********
086800030715     C* Effettuo lo split del campo dati d input
086900030715     C**********
087000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087100040324     C                   DOW       posDa <= %len(%trimr(vindta))
087200030715     C*
087300030715     C* Gestisco il 1° campo
087400030715     C                   IF        i = 1
087500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087600030715     C                   EVAL      posDa = 1
087700030715     C* Eseguo lo scan x trovare la fine del primo campo
087800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C* X i campi successivi al 1°
088300030715     C                   ELSE
088400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088500030715     C                   EVAL      posDa = posA + 1
088600031204     C* Verifico che nn vi sia il campo nn valorizzato
088700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088800031204     C* Se campo nn valorizzato skippo al prossimo
088900031204     C                   EVAL      SkSplitCSV(i) = *blanks
089000031204     C                   EVAL      posA  = posA + 1
089100031204     C                   ELSE
089200030715     C* Eseguo lo scan x trovare la fine del campo corrente
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* Gestisco l'ultimo campo
089500030715     C                   IF        posA = *zeros
089600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   LEAVE
090100030715     C                   ELSE
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C                   ENDIF
090600031204     C                   ENDIF
090700030715     C                   ENDIF
090800030715     C* Incremento il contatore d campo
090900030715     C                   EVAL      i = i +1
091000030715     C                   ENDDO
091100030715     C*
091200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091300030715     C* delimitazione testo
091400030715     C                   EVAL      i = 1
091500030715     C                   DOW       i < %elem(SkSplitCSV)
091600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091800030715     C                   EVAL      i = i + 1
091900030715     C                   ENDDO
092000030715     C*
092100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092200030715     C                   EVAL      i = 1
092300030715     C                   DOW       i < %elem(SkSplitFLD)
092400030715     C*
092500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092700030715     C                             SkSplitCSV(i) <> *blanks
092800030721     C*** ATB
092900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
093000040708     C                   EVAL      VAOATB = SkSplitCSV(i)
093100030721     C                   ENDIF
093200040708     C*** TOR
093300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
093400040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
093500040708     C                   ENDIF
093600040708     C*** TCO
093700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
093800040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
093900040708     C                   ENDIF
094000040708     C*** RSO
094100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
094200040708     C                   EVAL      VAORSO = SkSplitCSV(i)
094300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094400040708     C     '@':'A'       XLATE     VAORSO        VAORSO
094500030715     C* ==
094600030715     C                   ENDIF
094700040708     C*** INO
094800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
094900040708     C                   EVAL      VAOINO = SkSplitCSV(i)
095000030715     C                   ENDIF
095100040708     C*** LOO
095200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
095300040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
095400030715     C                   ENDIF
095500040708     C*** PRO
095600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
095700040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
095800030715     C                   ENDIF
095900040708     C*** NAO
096000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
096100040708     C                   EVAL      VAONAO = SkSplitCSV(i)
096200030721     C                   ENDIF
096300040708     C*** RSR
096400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
096500040708     C                   EVAL      VAORSR = SkSplitCSV(i)
096600040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096700040708     C     '@':'A'       XLATE     VAORSR        VAORSR
096800040708     C* ==
096900040708     C                   IF        VAORSR = *blanks
097000040708     C                   SETON                                        31
097100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097200040708     C                             + ' ' + 'VAORSR'
097300040708     C                   ENDIF
097400040708     C                   ENDIF
097500040708     C*** INR
097600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
097700040708     C                   EVAL      VAOINR = SkSplitCSV(i)
097800040708     C                   IF        VAOINR = *blanks
097900040708     C                   SETON                                        31
098000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098100040708     C                             + ' ' + 'VAOINR'
098200040708     C                   ENDIF
098300040708     C                   ENDIF
098400040708     C*** LOR
098500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
098600040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
098700040708     C                   IF        VAOLOR = *blanks
098800040708     C                   SETON                                        31
098900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099000040708     C                             + ' ' + 'VAOLOR'
099100040708     C                   ENDIF
099200040708     C                   ENDIF
099300040708     C*** PRR
099400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
099500040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
099600040708     C                   IF        VAOPRR = *blanks
099700040708     C                   SETON                                        31
099800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099900040708     C                             + ' ' + 'VAOPRR'
100000040708     C                   ENDIF
100100040708     C                   ENDIF
100200040708     C*** NAR
100300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
100400040708     C                   EVAL      VAONAR = SkSplitCSV(i)
100500040708     C                   ENDIF
100600040708     C*** RER
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
100800040708     C                   EVAL      VAORER = SkSplitCSV(i)
100900040708     C                   ENDIF
101000040708     C*** TER
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
101200040708     C                   EVAL      VAOTER = SkSplitCSV(i)
101300040708     C                   ENDIF
101400040708     C*** RSC
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
101600040708     C                   EVAL      VAORSC = SkSplitCSV(i)
101700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101800040708     C     '@':'A'       XLATE     VAORSC        VAORSC
101900040708     C* ==
102000040708     C                   ENDIF
102100040708     C*** INC
102200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
102300040708     C                   EVAL      VAOINC = SkSplitCSV(i)
102400040708     C                   ENDIF
102500040708     C*** LOC
102600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
102700040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
102800040708     C                   ENDIF
102900040708     C*** PRC
103000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
103100040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
103200040708     C                   ENDIF
103300040708     C*** NAC
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
103500040708     C                   EVAL      VAONAC = SkSplitCSV(i)
103600040708     C                   ENDIF
103700040708     C*** FFD
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
103900040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
104000040708     C                   ENDIF
104100040708     C*** RMP
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
104300040708     C                   EVAL      VAORMP = SkSplitCSV(i)
104400040708     C                   ENDIF
104500040708     C*** NAM
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
104700040708     C                   EVAL      VAONAM = SkSplitCSV(i)
104800040708     C                   ENDIF
104900040708     C*** PAG
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
105100040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
105200040708     C                   ENDIF
105300040708     C*** CTR
105400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
105500040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
105600040708     C                   ENDIF
105700040708     C*** ZOR
105800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
105900040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
106000040708     C                   ENDIF
106100040708     C*** NO1
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
106300040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
106400030715     C                   ENDIF
106500040708     C*** NO2
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
106700040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
106800030715     C                   ENDIF
106900040708     C*** DDT
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
107100040708     C                   EVAL      VAODDT = SkSplitCSV(i)
107200030715     C                   ENDIF
107300040708     C*** RFA
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
107500040708     C                   EVAL      VAORFA = SkSplitCSV(i)
107600030715     C                   ENDIF
107700040708     C*** SPI
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
107900040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
108000030715     C                   ENDIF
108100040708     C*** FLO
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
108300040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
108400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
108500080708     C* e la posizione 7 del VAOFLO
108600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
108700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
108800050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
108900030715     C                   ENDIF
109000091103     C
109100091103     C*** DAR
109200050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
109300050412     C                   EVAL      PiStr=SkSplitCSV(i)
109400050412     C                   EXSR      CHKNUM
109500050412     C                   IF        PiInt=*on
109600050412     C                   Z-ADD     PiVal         VAODAR
109700050412     C                   ELSE
109800050412     C                   SETON                                        32
109900050412     C                   EVAL      VAODAR = *zeros
110000050412     C                   EVAL      vinmsg = %trimr(vinmsg)
110100050412     C                             + ' ' + 'VAODAR'
110200050412     C                   ENDIF
110300050314     C                   ENDIF
110400030715     C
110500030721     C*
110600030721     C* Reperisco quindi i campi numerici...
110700040708     C*** POE
110800040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
110900040723     C                   EVAL      PiStr=SkSplitCSV(i)
111000040723     C                   EXSR      CHKNUM
111100040723     C                   IF        PiInt=*on    AND
111200040723     C                             PiVal<=999   AND
111300040723     C                             PiVal>*zeros
111400040723     C                   Z-ADD     PiVal         VAOPOE
111500040723     C                   ELSE
111600040723     C                   SETON                                        32
111700040723     C                   EVAL      VAOPOE = *zeros
111800040723     C                   EVAL      vinmsg = %trimr(vinmsg)
111900040723     C                             + ' ' + 'VAOPOE'
112000040723     C                   ENDIF
112100040723     C                   ENDIF
112200040708     C*** COR
112300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
112400040708     C                   EVAL      PiStr=SkSplitCSV(i)
112500040708     C                   EXSR      CHKNUM
112600040708     C                   IF        PiInt=*on
112700040708     C                   Z-ADD     PiVal         VAOCOR
112800040708     C                   ELSE
112900040708     C                   SETON                                        31
113000040708     C                   EVAL      VAOCOR = *zeros
113100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040708     C                             + ' ' + 'VAOCOR'
113300040708     C                   ENDIF
113400040708     C                   ENDIF
113500040708     C*** CAO
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on
114000030721     C                   Z-ADD     PiVal         Num5_0
114100040708     C                   MOVEL(p)  Num5_0        VAOCAO
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400040708     C                   EVAL      VAOCAO = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600040708     C                             + ' ' + 'VAOCAO'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900040708     C*** CRA
115000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiInt=*on
115400040708     C                   Z-ADD     PiVal         VAOCRA
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700040708     C                   EVAL      VAOCRA = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900040708     C                             + ' ' + 'VAOCRA'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200040708     C*** CAR
116300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
116400040708     C                   EVAL      PiStr=SkSplitCSV(i)
116500040708     C                   EXSR      CHKNUM
116600040708     C                   IF        PiInt=*on
116700040708     C                   Z-ADD     PiVal         Num5_0
116800040708     C                   MOVEL(p)  Num5_0        VAOCAR
116900040708     C                   ELSE
117000040708     C                   SETON                                        31
117100040708     C                   EVAL      VAOCAR = *zeros
117200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040708     C                             + ' ' + 'VAOCAR'
117400040708     C                   ENDIF
117500040708     C                   ENDIF
117600040708     C*** CRC
117700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
117800040708     C                   EVAL      PiStr=SkSplitCSV(i)
117900040708     C                   EXSR      CHKNUM
118000040708     C                   IF        PiInt=*on
118100040708     C                   Z-ADD     PiVal         VAOCRC
118200040708     C                   ELSE
118300040708     C                   SETON                                        32
118400040708     C                   EVAL      VAOCRC = *zeros
118500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118600040708     C                             + ' ' + 'VAOCRC'
118700040708     C                   ENDIF
118800040708     C                   ENDIF
118900040708     C*** CAC
119000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
119100040708     C                   EVAL      PiStr=SkSplitCSV(i)
119200040708     C                   EXSR      CHKNUM
119300040708     C                   IF        PiInt=*on
119400040708     C                   Z-ADD     PiVal         Num5_0
119500040708     C                   MOVEL(p)  Num5_0        VAOCAC
119600040708     C                   ELSE
119700040708     C                   SETON                                        32
119800040708     C                   EVAL      VAOCAC = *zeros
119900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120000040708     C                             + ' ' + 'VAOCAC'
120100040708     C                   ENDIF
120200040708     C                   ENDIF
120300040708     C*** DAR
120400090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
120500090428     C                   EVAL      PiStr=SkSplitCSV(i)
120600090428     C                   EXSR      CHKNUM
120700090428     C                   IF        PiInt=*on
120800090428     C                   Z-ADD     PiVal         VAODAR
120900090428     C                   ELSE
121000090428     C                   SETON                                        32
121100090428     C                   EVAL      VAODAR = *zeros
121200090428     C                   EVAL      vinmsg = %trimr(vinmsg)
121300090428     C                             + ' ' + 'VAODAR'
121400090428     C                   ENDIF
121500090428     C                   ENDIF
121600040708     C*** ORR
121700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
121800040708     C                   EVAL      PiStr=SkSplitCSV(i)
121900040708     C                   EXSR      CHKNUM
122000040708     C                   IF        PiInt=*on
122100040708     C                   Z-ADD     PiVal         VAOORR
122200040708     C                   ELSE
122300040708     C                   SETON                                        32
122400040708     C                   EVAL      VAOORR = *zeros
122500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040708     C                             + ' ' + 'VAOORR'
122700040708     C                   ENDIF
122800040708     C                   ENDIF
122900030721     C*** NCL
123000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on
123400040723     C                   IF        PiVal > *zeros
123500040708     C                   Z-ADD     PiVal         VAONCL
123600040723     C                   ENDIF
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900040708     C                   EVAL      VAONCL = *zeros
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100040708     C                             + ' ' + 'VAONCL'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400040708     C*** PKG
124500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiNum=*on
124900040723     C                   IF        PiVal > *zeros
125000040708     C                   Z-ADD     PiVal         VAOPKG
125100040723     C                   ENDIF
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400040708     C                   EVAL      VAOPKG = *zeros
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600040708     C                             + ' ' + 'VAOPKG'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900040708     C*** VLM
126000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
126100040708     C                   EVAL      PiStr=SkSplitCSV(i)
126200040708     C                   EXSR      CHKNUM
126300040708     C                   IF        PiNum=*on
126400040708     C                   Z-ADD     PiVal         VAOVLM
126500040708     C                   ELSE
126600040708     C                   SETON                                        32
126700040708     C                   EVAL      VAOVLM = *zeros
126800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126900040708     C                             + ' ' + 'VAOVLM'
127000040708     C                   ENDIF
127100040708     C                   ENDIF
127200040708     C*** BNC
127300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
127400040708     C                   EVAL      PiStr=SkSplitCSV(i)
127500040708     C                   EXSR      CHKNUM
127600040708     C                   IF        PiInt=*on
127700040708     C                   Z-ADD     PiVal         VAOBNC
127800040708     C                   ELSE
127900040708     C                   SETON                                        32
128000040708     C                   EVAL      VAOBNC = *zeros
128100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128200040708     C                             + ' ' + 'VAOBNC'
128300040708     C                   ENDIF
128400040708     C                   ENDIF
128500040708     C*** BLC
128600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
128700040708     C                   EVAL      PiStr=SkSplitCSV(i)
128800040708     C                   EXSR      CHKNUM
128900040708     C                   IF        PiInt=*on
129000040708     C                   Z-ADD     PiVal         VAOBLC
129100040708     C                   ELSE
129200040708     C                   SETON                                        32
129300040708     C                   EVAL      VAOBLC = *zeros
129400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129500040708     C                             + ' ' + 'VAOBLC'
129600040708     C                   ENDIF
129700040708     C                   ENDIF
129800040708     C*** ATT
129900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
130000040708     C                   EVAL      PiStr=SkSplitCSV(i)
130100040708     C                   EXSR      CHKNUM
130200040708     C                   IF        PiInt=*on
130300040708     C                   Z-ADD     PiVal         VAOATT
130400040708     C                   ELSE
130500040708     C                   SETON                                        32
130600040708     C                   EVAL      VAOATT = *zeros
130700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130800040708     C                             + ' ' + 'VAOATT'
130900040708     C                   ENDIF
131000040708     C                   ENDIF
131100040708     C*** MTC
131200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
131300040708     C                   EVAL      PiStr=SkSplitCSV(i)
131400040708     C                   EXSR      CHKNUM
131500040708     C                   IF        PiInt=*on
131600040708     C                   Z-ADD     PiVal         VAOMTC
131700040708     C                   ELSE
131800040708     C                   SETON                                        32
131900040708     C                   EVAL      VAOMTC = *zeros
132000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132100040708     C                             + ' ' + 'VAOMTC'
132200040708     C                   ENDIF
132300040708     C                   ENDIF
132400040708     C*** KSC
132500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on      AND
132900030721     C                             PiVal<=9999999 AND
133000030721     C                             PiVal>*zeros
133100040708     C                   Z-ADD     PiVal         VAOKSC
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400040708     C                   EVAL      VAOKSC = *zeros
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600040708     C                             + ' ' + 'VAOKSC'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900040708     C*** POR
134000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiInt=*on    AND
134400030721     C                             PiVal<=999   AND
134500030721     C                             PiVal>*zeros
134600040708     C                   Z-ADD     PiVal         VAOPOR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900040708     C                   EVAL      VAOPOR = *zeros
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100040708     C                             + ' ' + 'VAOPOR'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400040708     C*** POC
135500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
135600040708     C                   EVAL      PiStr=SkSplitCSV(i)
135700040708     C                   EXSR      CHKNUM
135800040708     C                   IF        PiInt=*on    AND
135900040708     C                             PiVal<=999   AND
136000040708     C                             PiVal>*zeros
136100040708     C                   Z-ADD     PiVal         VAOPOC
136200040708     C                   ELSE
136300040708     C                   SETON                                        32
136400040708     C                   EVAL      VAOPOC = *zeros
136500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136600040708     C                             + ' ' + 'VAOPOC'
136700040708     C                   ENDIF
136800040708     C                   ENDIF
136900040708     C*** STO
137000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
137100040708     C                   EVAL      PiStr=SkSplitCSV(i)
137200040708     C                   EXSR      CHKNUM
137300040708     C                   IF        PiInt=*on
137400040708     C                   Z-ADD     PiVal         VAOSTO
137500040708     C                   ELSE
137600040708     C                   SETON                                        32
137700040708     C                   EVAL      VAOSTO = *zeros
137800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137900040708     C                             + ' ' + 'VAOSTO'
138000040708     C                   ENDIF
138100040708     C                   ENDIF
138200090428     C*
138300091103     C********** GESTIONE CAMPI PERSONALIZZATI
138400120120
138500121129     C*** NO1 e NO2
138600121129     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO'
138700121129     C                   EVAL      w70 = *blank
138800121129     C                   EVAL      w70 = %trim(SkSplitCSV(i))
138900121129     C                   EVAL      VAONO1 = %subst(w70:1:35)
139000121129     C                   EVAL      VAONO2 = %subst(w70:36:35)
139100120920     C                   ENDIF
139200090428     C*
139300090428     C****************************************
139400090428     C*
139500030715     C*
139600030715     C                   ENDIF
139700030715     C                   EVAL      i = i + 1
139800030715     C                   ENDDO
139900040723     C*
140000030715     C                   ENDIF
140100020204     C*
140200010607     C                   ENDSR
140300010607     C*----------------------------------------------------*
140400080702
140500080702
140600080702     C*----------------------------------------------------*
140700080702     C*  CONTROLLO NUMERICITA' CAMPI
140800080702     C*----------------------------------------------------*
140900080702     C     CHKNUM        BEGSR
141000080702     C*
141100080702     C                   IF        PiDecChr = *blanks
141200080702     C                   EVAL      PiDecChr = ','
141300080702     C                   ENDIF
141400080702     C*
141500080702     C                   callp     UBISNUM_Check(PiStr
141600080702     C                                          :PiDecChr
141700080702     C                                          :PiVal
141800080702     C                                          :PiNum
141900080702     C                                          :PiInt)
142000080702     C*
142100080702     C                   ENDSR
142200080702     C***
142300080702
142400010601
142500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142600010601     C     repfil        BEGSR
142700010601     C*
142800010601     C                   if        invfil = *zeros and
142900010601     C                             depfil > *zeros and
143000010629     C                             (vinflg = *blanks or
143100010629     C                              vinflg = *zeros)
143200010601     C
143300010601     C                   eval      invfil = depfil
143400010601     C                   endif
143500010601     C*
143600010601     C                   if        depfil <> invfil and
143700010601     C                             invfil > *zeros
143800010601     C                   eval      flgMulti = '1'
143900010601     C                   if        vinflg = *blanks
144000010601     C                   add       1             cntNonEl
144100010601     C                   endif
144200010601     C                   endif
144300010601     C*
144400010601     C                   if        vinflg = '2'
144500010601     C                   eval      flgStato = '2'
144600010601     C                   endif
144700010601     C*
144800010601     C                   ENDSR
144900010601     C***
145000010330
145100010330
145200990920      /TITLE Invio dei dati al punto operativo.
145300000613     C     invio         BEGSR
145400990920     C*
145500990920     C                   reset                   dscmz
145600010601     C                   move      invfil        cmzdst
145700990920     C                   eval      cmzfld = vlrfou
145800990920     C                   eval      cmzmbd = vlrhdl
145900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
146000000710     C                   if        prmfir = *blanks
146100040708     C                   eval      cmzfla = 'FNVAO00F'
146200040708     C                   eval      cmzmba = 'FNVAO00F'
146300000710     C                   else
146400000710     C                   eval      cmzfla = prmfir
146500000710     C                   eval      cmzmba = prmfir
146600000710     C                   endif
146700990920     C                   eval      cmznrr = *zeros
146800990920     C                   move      §ctrok        cmznrr
146900021018     C                   eval      cmzlba = vlrfl1
147000990920     C                   call(e)   'TIS711C'
147100990920     C                   parm                    dscmz
147200990921     C                   parm      *blanks       esito
147300990920     C                   if        %error
147400990920     C                             or cmzerr = '1'
147500990921     C                             or esito  = '1'
147600000710     C                   eval      wrkesito = '3'
147700990920     C                   endif
147800990920     C*
147900000613     C                   ENDSR
148000010601
148100010601
148200010601      /TITLE Invio dei dati al punto operativo.
148300010601     C     opeini        BEGSR
148400010601     C*
148500010601     C* Inizializzo flag e contatori operativi
148600010601     C                   movel     '0'           flgGiro           1
148700010601     C                   movel     '0'           flgMulti          1
148800010601     C                   movel     '1'           flgStato          1
148900010615     C                   movel     '0'           flgOk             1
149000010601     C                   z-add     *zeros        cntNonEl         10 0
149100010601     C                   z-add     *zeros        depfil            3 0
149200010601     C                   z-add     *zeros        invfil            3 0
149300010601     C*
149400010601     C                   ENDSR
149500010601     C***
149600050421
149700050421
149800091130     C     *pssr         BEGSR
149900050421     C*
150000091130     C                   eval      wrkesito = '2'
150100050421     C*
150200091130     C                   ENDSR     '*CANCL'
150300050421     C***
150400060306
150500060306
150600060306     C*-------------------------------------------------------------------------*
150700060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
150800060306     C*-------------------------------------------------------------------------*
150900060306     C     sr_Control    begsr
151000060306     C*
151100060306     C* inizializzo a ok il flag d controllo abilitazione
151200060306     C                   eval      Cli_Abil = *on
151300060306     C
151400060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
151500060306     C                   clear                   woriIDC
151600060306     C                   eval      kteCOD = 'VAP'
151700060306     C                   eval      kteKE1 = oriIDC
151800060306     C     keytbe        chain     TNTBE01L
151900060306     C                   if        %found(Tntbe01l)
152000060306     C                   eval      woriIDC = TbeKe2
152100060306     C                   else
152200060306     C                   eval      Cli_Abil = *off
152300060306     C                   endif
152400060306     C*
152500060306     C                   endsr
152600010330
152700010330
152800000613     C     *inzsr        BEGSR
152900990910     C*
153000990910     C     *entry        plist
153100990920     C                   parm                    tivlrds
153200990921     C                   parm      wrkesito      esito
153300000724     C                   parm                    prmlit
153400000710     C                   parm                    prmfir
153500010330     C*
153600010330     C* CALCOLA LA DATA CORRENTE
153700091127     C                   z-add     *zeros        datcor            8 0
153800091127     C                   time                    wn14             14 0
153900091127     C                   movel     wn14          wn6               6 0          *ORA (6)
154000091127     C                   eval      datcor = %dec(%date() : *ISO)
154100040723     C*
154200040723     C* Campi riferito al database
154300040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
154400040723     C     *like         define    tbeke1        kteKE1
154500040723     C*
154600040723     C* Definizione chiavi di lettura
154700040723     C* TNTBE01L - parziale
154800040723     C     keytbe        klist
154900040723     C                   kfld                    kteCOD                         *UTENTE
155000040723     C                   kfld                    kteKE1                         *CODICE
155100000613     C*
155200000613     C                   ENDSR
155300000613     C***
155400990908
155500040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
155600121129VAORFA;VAORSR;VAOINR;VAOLOR;VAOCAR;VAOPRR;VAOTER;VAORER;VAORSC;VAOINC;VAOLOC;
155700121129VAOCAC;VAOPRC;VAONCL;VAOPKG;VAOORR;W_VAONO
155800120120
