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
003600120123     d wData_EUR       s               d   datfmt(*eur)
003700120123     d wData_ISO       s               d   datfmt(*iso)
003800120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003900120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004000120905     D Intestazioni    s               n   inz(*on)
004100030715     D*------------------
004200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004300030715     D*------------------
004400030715     D SkSplitFLD      S             10    DIM(100)
004500030715     D SkSplitCSV      S            256    DIM(100)
004600030715     D CharCSV         S              1
004700030715     D CharTXT         S              1
004800030715     D CharNUM         S              1
004900030715     D posDa           S              3  0 INZ(*zeros)
005000030715     D posA            S              3  0 INZ(*zeros)
005100030715     D i               s              3  0 INZ(1)
005200030715     D wGiro           s              1  0 INZ(*zeros)
005300030822     D*------------------
005400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005500030822     D*------------------
005600030822     D posDaDft        S              3  0 INZ(*zeros)
005700030822     D posADft         S              3  0 INZ(*zeros)
005800030822     D j               s              3  0 INZ(1)
005900030822     D wGiroDft        s              1  0 INZ(*zeros)
006000040723     D*------------------
006100040723     D* DS REPERIMENTO DATI CLIENTE
006200040723     D*-------------------
006300040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006400040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006500040723     D INDDS         E DS                  EXTNAME(CNIND00F)
006600040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006700040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006800080702     D*------------------
006900080702
007000080702     D*------------------
007100080702     D* LINKING A DEFINIZIONI ESTERNE
007200080702     D*------------------
007300090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007400090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007500091026
007600091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007700091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
007800091026
007900080702
008000031201     D*------------------
008100031201     D* Costanti
008200031201     D*------------------
008300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500010330
008600010330
008700010330
008800000913     C                   reset                   rrnum
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100010601     C*
009200010601     C                   exsr      opeini
009300041011     C                   exsr      PREELA
009400040708     C                   exsr      rwvao
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700041011     C                   if        flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100041011     C*
011200041011     C                   endif
011300010601     C*
011400010601     C                   ENDSR
011500010601     C***
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200000616     C*
012300120209     C                   ENDSR
012400010601
012500010601
012600010330     C*--------------------------------------------------------
012700040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
012800010330     C*--------------------------------------------------------
012900040708     C     RWVAO         BEGSR
013000010330     C*
013100010330     C                   if        not %open(tivin00r)
013200010330     C                   open      tivin00r
013300010330     C                   endif
013400040708     C                   if        not %open(fnvaowwr)
013500040708     C                   open      fnvaowwr
013600010330     C                   endif
013700010330     C*
013800010604     C                   clear                   §CTROK
013900010604     C                   clear                   §CTRMO
014000010604     C                   clear                   §CTRNO
014100120123     C*
014200120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
014300120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
014400120905     C                   IF        Intestazioni = *off
014500120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
014600120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014700120123     C                   exsr      inzvar
014800120123     C                   exsr      impvao
014900120905     C                   ENDIF
015000010330     C*
015100030822     C                   do        *HIVAL
015200010330     C*
015300030822     C                   read      tivin00r                               70
015400010618     C*
015500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015600010618     C                   if        vinflg = '1'
015700010618     C                   eval      flgOk = '1'
015800010618     C                   endif
015900010618     C*
016000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016100040510     C                   z-add     *zeros        wLenVINDTA        4 0
016200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016600040510     C                   z-add     1             wFlgVINDTA
016700040510     C                   leave
016800040510     C                   endif
016900040510     C                   enddo
017000040510     C*
017100040218     C                   if        vindta > *blanks AND
017200040510     C                             wFlgVINDTA = 1
017300010330     C                   add       1             rrnum
017400010330     C*
017500010601     C                   if        *in70 = *off and
017600010330     C                             (vinflg = *blanks
017700010330     C                              or vinflg = '0'
017800010330     C                              or vinflg = '2')
017900010330     C*
018000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018100010711     C                   if        vinflg = *blanks or vinflg = '0'
018200010711     C                   clear                   vinmsg
018300010711     C                   endif
018400010601     C*
018500010330     C                   exsr      inzvar
018600040708     C                   exsr      impvao
018700010601     C*
018800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018900040708     C                   eval      depfil = VAOPOE
019000010601     C                   exsr      repfil
019100010601     C                   if        depfil = invfil
019200021025     C                   if        vlrpoi = 999
019300040708     C                   move(P)   invfil        VAOPOE
019400021025     C                   else
019500040708     C                   move(P)   vlrpoi        VAOPOE
019600021025     C                   endif
019700010601     C*
019800010604     C* Ebbene...
019900010604     C*
020000030822     C  N31              add       1             §CTROK            7 0
020100030822     C   32              add       1             §CTRMO            7 0
020200030822     C   31              add       1             §CTRNO            7 0
020300030822     C                   if        wGiro = 2
020400120123     C  N31              EXSR      CHKCOR
020500120123     C  N31              EXSR      WRIORI
020600120123     C  N31              EXSR      WRIVGD
020700120123     C  N31              write     FNVAO000
020800030822     C                   endif
020900010604     C*
021000010604     C                   if        *in31 = *off and
021100010604     C                             *in32 = *off
021200010604     C                   eval      vinflg = '1'
021300010604     C                   else
021400010604     C                   eval      vinflg = '2'
021500010604     C                   endif
021600010604     C                   endif
021700010601     C*
021800010604     C                   endif
021900010604     C*
022000010330     C                   else
022100010330     C                   eval      vinflg = '1'
022200010330     C                   endif
022300010601     C*
022400010601     C  N70              update    tivin000
022500010330     C*
022600030822     C  N70              enddo
022700010601     C
022800010601     C*
022900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023000010601     C                   if        cntNonEl = *zeros or
023100010601     C                             flgMulti = '0'
023200010330     C* Se non ci sono record con errori ...
023300010601     C                   if        §ctrno = 0 and
023400010604     C                             §ctrmo = 0 and
023500010601     C                             flgStato <> '2'
023600010330     C* ... restituisco esito OK.
023700010330     C                   eval      wrkesito = '0'
023800010330     C                   else
023900010330     C                   if        §ctrok > 0
024000010330     C                   eval      wrkesito = '1'
024100010330     C                   else
024200010615     C                   if        flgOk = '0'
024300010615     C                   eval      wrkesito = '2'
024400010615     C                   else
024500010615     C                   eval      wrkesito = '6'
024600010615     C                   endif
024700010330     C                   endif
024800010330     C                   endif
024900010601     C                   else
025000010601     C                   eval      wrkesito = '9'
025100010601     C                   endif
025200010330     C*
025300010330     C                   if        %open(tivin00r)
025400010330     C                   close     tivin00r
025500010330     C                   endif
025600040708     C                   if        %open(fnvaowwr)
025700040708     C                   close     fnvaowwr
025800010330     C                   endif
025900010601     C*
026000010601     C                   if        vlrpoi <> 999
026100010601     C                   eval      invfil = vlrpoi
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        §ctrok > 0
026500010601     C                             and invfil > *zeros
026600010330     C                   exsr      invio
026700010330     C                   endif
026800010601     C*
026900010618     C                   if        flgGiro = '1'
027000010601     C                   exsr      endela
027100010618     C                   endif
027200010330     C*
027300010330     C                   ENDSR
027400010330     C***
027500040723
027600040723
027700040723     C*------------------------------------------------------------------------*
027800040723     C* CHKCOR - Considerazioni su VAOCOR
027900040723     C*------------------------------------------------------------------------*
028000040723     C     CHKCOR        BEGSR
028100040723     C*
028200040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
028300040723     C                   if        vaoTCO = 'F'
028400040723     C* Quindi verifico se il codice ordinante non è valorizzato
028500040723     C                   if        vaoCOR = *zeros  AND
028600040723     C                             vaoRSO = *blanks AND
028700040723     C                             vaoINO = *blanks AND
028800040723     C                             vaoCAO = *blanks AND
028900040723     C                             vaoLOO = *blanks AND
029000040723     C                             vaoPRO = *blanks AND
029100040723     C                             vaoNAO = *blanks
029200040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
029300040723     C                   movel     '0'           wFlgAcr           1
029400040723     C                   move      vlrKSC        wCli              7 0
029500040723     C                   move(p)   wCli          acrCRO
029600040723     C     acrCRO        setll     fnacr01l
029700040723     C                   if        %found(fnacr01l)
029800040723     C                   read      fnacr01l
029900040723     C                   dow       not %eof(fnacr01l)
030000040723     C                   movel     acrCRO        wCliAcr           7 0
030100040723     C                   if        wCliAcr = wCli
030200040723     C                   eval      vaoCOR = acrCRO
030300040723     C                   movel     '1'           wFlgAcr
030400040723     C                   leave
030500040723     C                   else
030600040723     C                   leave
030700040723     C                   endif
030800040723     C                   read      fnacr01l
030900040723     C                   enddo
031000040723     C                   endif
031100040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
031200040723     C* dati anagrafici dell'ordinante dal piano dei conti
031300040723     C                   if        wFlgAcr = '0'
031400040723     C                   clear                   BS69DS
031500040723     C                   clear                   ACODS
031600040723     C                   clear                   INDDS
031700040723     C                   clear                   CLPDS
031800040723     C                   clear                   CLSDS
031900040723     C                   move(P)   vlrKSC        I69KAC
032000040723     C                   move(P)   vlrKSC        I69KIN
032100040723     C                   call      'TIBS69R'
032200040723     C                   parm                    BS69DS
032300040723     C                   parm                    ACODS
032400040723     C                   parm                    INDDS
032500040723     C                   parm                    CLPDS
032600040723     C                   parm                    CLSDS
032700040723     C     O69ERR        ifne      '1'
032800040723     C                   eval      vaoRSO = ACORAG
032900040723     C                   eval      vaoINO = INDVIA
033000040723     C                   movel(P)  INDCAP        vaoCAO
033100040723     C                   eval      vaoLOO = INDCIT
033200040723     C                   eval      vaoPRO = INDPRV
033300040723     C                   eval      vaoNAO = INDSTA
033400040723     C                   endif
033500040723     C                   endif
033600040723     C                   endif
033700040723     C                   endif
033800040723     C*
033900040723     C                   ENDSR
034000040723
034100040723
034200040723     C*------------------------------------------------------------------------*
034300040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
034400040723     C*------------------------------------------------------------------------*
034500040723     C     WRIORI        BEGSR
034600040723     C*
034700090618     C* Come prima cosa stacco un numeratore da AZNUM
034800090618     C                   clear                   trul33ds
034900090618     C                   eval      I33OPE = *zeros
035000090618     C                   eval      I33CNU = 600
035100090618     C                   eval      I33NUM = 1
035200090618     C                   movel     TRUL33DS      KPJBU
035300090618     C                   call      'TRUL33R'
035400090618     C                   parm                    KPJBA
035500090618     C                   movel     KPJBU         TRUL33DS
035600090618     C                   if        O33ERR = *zeros
035700090618     C                   z-add     O33NRF        §numori
035800090618     c                   else
035900090618     c                   eval      §numori = *all'9'
036000090618     C                   endif
036100040723     C*
036200040723     C* Quindi imposto i campi "extra-VAO"
036300040723     C                   if        vlrKSC = '0ORM0000'
036400040723     C*
036500040723     C                   if        vaoCOR > *zeros
036600040723     C                   movel     vaoCOR        wrkCOR7           7 0
036700040723     C*
036800040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
036900040723     C* codice padre. Se è un padre assegno già il codice ordinante.
037000040723     C                   clear                   tibs10ds
037100040723     C                   eval      d10drf = datcor                              *data riferimento
037200040723     C                   eval      d10paf = 'P'                                 *cerca il padre
037300040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
037400091026     C*
037500091026     C* Preparazione chiamata al TIBS10R1
037600091026     C                   clear                   tibs10r1ds
037700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
037800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
037900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
038000091026     C*
038100091026     C                   call      'TIBS10R1'
038200091026     C                   parm                    tibs10r1ds
038300091026     C*
038400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
038500040723     C*
038600040723     C                   if        d10err <> *blanks or                         *No unif. o padre
038700040723     C                             d10cop =  *zeros  or
038800040723     C                             d10cop =  d10cod
038900040723     C                   move      d10cod        oriIDC
039000040723     C                   else                                                   *Figlio
039100040723     C                   move      d10cop        oriIDC
039200040723     C                   endif
039300040723     C*
039400040723     C                   else
039500040723     C                   eval      oriIDC = *zeros
039600040723     C                   endif
039700040723     C*
039800040723     C                   else
039900040723     C                   eval      oriIDC = vlrKSC
040000040723     C                   endif
040100040723     C*
040200040723     C                   eval      oriPRG = §numori
040300040723     C                   eval      oriDER = datcor
040400040723     C                   eval      oriOER = wn6
040500040723     C                   eval      oriPOE = vaoPOE
040600040723     C                   eval      oriNSR = vaoNSR
040700040723     C                   eval      oriNOR = vaoNOR
040800040723     C                   eval      oriNRV = vaoNRV
040900040723     C                   eval      oriFLO = *blanks
041000040723     C*
041100040723     C* Infine valorizzo la chiave esterna sul file FNVAO
041200040723     C                   eval      dorm01 = vaoFLO
041300040723     C                   movel     §numori       §ormpg
041400050412     C*
041500050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
041600130326     C                   movel     'N'           §ormcb
041700130326     C                   movel     'S'           §ormfr
041800130326     C                   movel     'S'           §ormks
041900130326     C                   movel     'S'           §ormfd
042000130326     C                   movel     'N'           §orcom
042100050412     C*
042200130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
042300040723     C                   eval      vaoFLO = dorm01
042400130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
042500130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
042600130326     C***                eval      %subst(VAOFLO:6:1) = *blanks
042700040723     C*
042800040723     C* ...quindi scrivo il file TIORI00F
042900120905     C  N55              WRITE     TIORI000
043000040723     C*
043100040723     C                   ENDSR
043200050826
043300050826
043400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
043500050826     C     wriVGD        BEGSR
043600050826     C*
043700050826     C* Reperisco la descrizione della fase da tabella
043800050826     C                   movel(p)  'FAR'         KteCOD
043900050826     C                   movel(p)  '000'         KteKE1
044000050826     C     KEYtbe        chain     tntbe01l
044100050826     C                   if        %found(tntbe01l)
044200050826     C                   movel(P)  tbeuni        DFAR
044300050826     C                   else
044400050826     C                   clear                   DFAR
044500050826     C                   endif
044600050826     C*
044700050826     C* Valorizzo buffer record
044800050826     C                   clear                   FNVAPDS
044900050826     C                   eval      vapIDC = oriIDC
045000050826     C                   eval      vapPOE = oriPOE
045100050826     C                   eval      vapRFA = vaoRFA
045200050826     C                   eval      vapPOG = oriPOE
045300050826     C                   eval      vapDAE = oriDER
045400050826     C                   eval      vapORE = oriOER
045500050826     C                   if        d§fardva <> *blanks
045600050826     C                   eval      vapDFA = d§fardva
045700050826     C                   else
045800050826     C                   eval      vapDFA = d§fardes
045900050826     C                   endif
046000050826     C*
046100050826     C                   clear                   tivgd000
046200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
046300050826     C                   eval      vgdTIP = 'VP'
046400050826     C                   eval      vgdKSU = vapIDC
046500050826     C                   eval      vgdTSC = 'WW'
046600050826     C                   eval      vgdDAT = datcor
046700151207     C                   eval      vgdPGM = procname
046800060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
046900060306     C                   exsr      sr_Control
047000060306     C                   if        Cli_Abil = *on
047100060306     C                   eval      vgdKSU = woriIDC
047200120905     C  N55              write     tivgd000
047300060306     C                   endif
047400050826     C*
047500050826     C                   ENDSR
047600050826     C*------------------------------------------------------------------------*
047700010601
047800010601
047900010330     C*----------------------------------------------------*
048000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048100010330     C*----------------------------------------------------*
048200010330     C     INZVAR        BEGSR
048300010330     C*
048400030822     C* Inizializzo variabili di wrk
048500020204     C                   Z-ADD     *zeros        Num5_0            5 0
048600120124
048700030822     C*
048800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048900120123     C                   CLEAR                   FNVAO000
049000120123     C                   CLEAR                   TIORI000
049100030822     C                   CLEAR                   SkSplitCSV
049200030822     C                   EVAL      i = 1
049300030822     C                   EVAL      posDa = *zeros
049400030822     C                   EVAL      posA  = *zeros
049500030822     C*
049600030822     C* Reimposto i valori di default
049700030822     C                   EXSR      DEFCAM
049800030822     C*
049900010330     C                   ENDSR
050000010330     C*----------------------------------------------------*
050100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050200010330     C*----------------------------------------------------*
050300010330     C     DEFCAM        BEGSR
050400030822     C*
050500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050600030715     C* e delimitatore testo.
050700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051000040723     C*
051100080707     C* Imposto i valori d default d alcuni campi
051200080708     C*
051300080708     C                   CLEAR                   DORM01
051400120822     C                   EVAL      VAOFLO = DORM01
051500080707     C*
051600120822     C                   EVAL      VAOCOR = 0054805000
051700120822     C                   EVAL      VAOCRC = 0054805000
051800120822     C                   EVAL      VAOPAG = 'O'
051900120822     C                   EVAL      VAOKSC = 0054805
052000120822     C                   EVAL      VAOCTR = '000'
052100120920     C                   EVAL      VAOTOR = 'S'
052200120920     C                   EVAL      VAOORR = 1400
052300120905     C* se il 1° char dei parametri è T=Test
052400120905     C                   SETOFF                                       55
052500120905     C                   IF        %subst(vlrppt:1:1) = 'T'
052600120905     C* inibisco la scrittura dei file TIORI e TIVGD
052700120905     C                   SETON                                        55
052800120905     C* imposto il PO di emissione come filiale fittizia
052900120905     C                   EVAL      VAOPOE = 999
053000120905     C                   ENDIF
053100091127     C*
053200091204     C* IMPOSTO I CAMPI PERSONALIZZATI
053300100416     C
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
076400091103     C*
076500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
076600080702     C* e posizione 7 del VAOFLO
076700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
076800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
076900050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
077000040708     C                   ENDIF
077100030822     C* ...
077200030822     C                   ENDIF
077300030822     C                   ENDDO
077400020204     C*
077500010330     C                   ENDSR
077600010607     C*----------------------------------------------------*
077700040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
077800010607     C*----------------------------------------------------*
077900040708     C     IMPVAO        BEGSR
078000010607     C*
078100010607     C                   SETOFF                                       3132
078200120124
078300030515     C*
078400040708     C                   MOVEL     datcor        VAODAO
078500040708     C                   MOVEL     wn14          VAOOAO
078600050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078700050314     C     minu:maiu     XLATE     vindta        vindta
078800010607     C*
078900030715     C**********
079000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079100030715     C**********
079200030715     C                   IF        wGiro = *zeros
079300040709     C*
079400040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
079500040709     C                   EVAL      depvinDTA = vinDTA
079600040709     C*
079700040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
079800040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079900040709     C                                      %trim(intcol(3))
080000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080100050314     C**!!!minu:maiu     XLATE     vindta        vindta
080200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080300040324     C                   DOW       posDa <= %len(%trimr(vindta))
080400030715     C*
080500030715     C* Gestisco il 1° campo
080600030715     C                   IF        i = 1
080700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080800030715     C                   EVAL      posDa = 1
080900030715     C* Eseguo lo scan x trovare la fine del primo campo
081000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081100030715     C* A questo punto "estraggo" il campo corrente
081200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081300030715     C                                                    (posA-posDa))
081400030715     C* X i campi successivi al 1°
081500030715     C                   ELSE
081600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081700030715     C                   EVAL      posDa = posA + 1
081800030715     C* Eseguo lo scan x trovare la fine del campo corrente
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* Gestisco l'ultimo campo
082100030715     C                   IF        posA = *zeros
082200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C                   LEAVE
082700030715     C                   ELSE
082800030715     C* A questo punto "estraggo" il campo corrente
082900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083000030715     C                                                    (posA-posDa))
083100030715     C                   ENDIF
083200030715     C                   ENDIF
083300030715     C* Incremento il contatore d campo
083400030715     C                   EVAL      i = i +1
083500030715     C                   ENDDO
083600030715     C*
083700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083800030715     C* delimitazione testo
083900030715     C                   EVAL      i = 1
084000030715     C                   DOW       i < %elem(SkSplitFLD)
084100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084300030715     C                   EVAL      i = i + 1
084400030715     C                   ENDDO
084500030715     C                   EVAL      wGiro = 1
084600040709     C* Ripristino la 1° riga originale
084700040709     C                   EVAL      vinDTA = depvinDTA
084800030715     C                   ELSE
084900030715     C                   EVAL      wGiro = 2
085000031204     C**********
085100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085200031204     C* d separatore campo
085300031204     C**********
085400040709     C                   EVAL      vindta = %trim(vindta)
085500040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085600040709     C                   EVAL      vindta = %subst(vindta:2)
085700040709     C                   ENDDO
085800040709     C*
085900040709     C                   Z-ADD     *zeros        lunghInput        4 0
086000040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086100040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086200040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086300040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086400040709     C                   ENDDO
086500030715     C**********
086600030715     C* Effettuo lo split del campo dati d input
086700030715     C**********
086800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086900040324     C                   DOW       posDa <= %len(%trimr(vindta))
087000030715     C*
087100030715     C* Gestisco il 1° campo
087200030715     C                   IF        i = 1
087300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087400030715     C                   EVAL      posDa = 1
087500030715     C* Eseguo lo scan x trovare la fine del primo campo
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C* X i campi successivi al 1°
088100030715     C                   ELSE
088200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088300030715     C                   EVAL      posDa = posA + 1
088400031204     C* Verifico che nn vi sia il campo nn valorizzato
088500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088600031204     C* Se campo nn valorizzato skippo al prossimo
088700031204     C                   EVAL      SkSplitCSV(i) = *blanks
088800031204     C                   EVAL      posA  = posA + 1
088900031204     C                   ELSE
089000030715     C* Eseguo lo scan x trovare la fine del campo corrente
089100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089200030715     C* Gestisco l'ultimo campo
089300030715     C                   IF        posA = *zeros
089400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C                   LEAVE
089900030715     C                   ELSE
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C                   ENDIF
090400031204     C                   ENDIF
090500030715     C                   ENDIF
090600030715     C* Incremento il contatore d campo
090700030715     C                   EVAL      i = i +1
090800030715     C                   ENDDO
090900030715     C*
091000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091100030715     C* delimitazione testo
091200030715     C                   EVAL      i = 1
091300030715     C                   DOW       i < %elem(SkSplitCSV)
091400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091600030715     C                   EVAL      i = i + 1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092000030715     C                   EVAL      i = 1
092100030715     C                   DOW       i < %elem(SkSplitFLD)
092200030715     C*
092300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092500030715     C                             SkSplitCSV(i) <> *blanks
092600030721     C*** ATB
092700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
092800040708     C                   EVAL      VAOATB = SkSplitCSV(i)
092900030721     C                   ENDIF
093000040708     C*** TOR
093100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
093200040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
093300040708     C                   ENDIF
093400040708     C*** TCO
093500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
093600040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
093700040708     C                   ENDIF
093800040708     C*** RSO
093900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
094000040708     C                   EVAL      VAORSO = SkSplitCSV(i)
094100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094200040708     C     '@':'A'       XLATE     VAORSO        VAORSO
094300030715     C* ==
094400030715     C                   ENDIF
094500040708     C*** INO
094600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
094700040708     C                   EVAL      VAOINO = SkSplitCSV(i)
094800030715     C                   ENDIF
094900040708     C*** LOO
095000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
095100040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
095200030715     C                   ENDIF
095300040708     C*** PRO
095400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
095500040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
095600030715     C                   ENDIF
095700040708     C*** NAO
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
095900040708     C                   EVAL      VAONAO = SkSplitCSV(i)
096000030721     C                   ENDIF
096100040708     C*** RSR
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
096300040708     C                   EVAL      VAORSR = SkSplitCSV(i)
096400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096500040708     C     '@':'A'       XLATE     VAORSR        VAORSR
096600040708     C* ==
096700040708     C                   IF        VAORSR = *blanks
096800040708     C                   SETON                                        31
096900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097000040708     C                             + ' ' + 'VAORSR'
097100040708     C                   ENDIF
097200040708     C                   ENDIF
097300040708     C*** INR
097400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
097500040708     C                   EVAL      VAOINR = SkSplitCSV(i)
097600040708     C                   IF        VAOINR = *blanks
097700040708     C                   SETON                                        31
097800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097900040708     C                             + ' ' + 'VAOINR'
098000040708     C                   ENDIF
098100040708     C                   ENDIF
098200040708     C*** LOR
098300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
098400040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
098500040708     C                   IF        VAOLOR = *blanks
098600040708     C                   SETON                                        31
098700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098800040708     C                             + ' ' + 'VAOLOR'
098900040708     C                   ENDIF
099000040708     C                   ENDIF
099100040708     C*** PRR
099200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
099300040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
099400040708     C                   IF        VAOPRR = *blanks
099500040708     C                   SETON                                        31
099600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099700040708     C                             + ' ' + 'VAOPRR'
099800040708     C                   ENDIF
099900040708     C                   ENDIF
100000040708     C*** NAR
100100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
100200040708     C                   EVAL      VAONAR = SkSplitCSV(i)
100300040708     C                   ENDIF
100400040708     C*** RER
100500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
100600040708     C                   EVAL      VAORER = SkSplitCSV(i)
100700040708     C                   ENDIF
100800040708     C*** TER
100900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
101000040708     C                   EVAL      VAOTER = SkSplitCSV(i)
101100040708     C                   ENDIF
101200040708     C*** RSC
101300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
101400040708     C                   EVAL      VAORSC = SkSplitCSV(i)
101500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101600040708     C     '@':'A'       XLATE     VAORSC        VAORSC
101700040708     C* ==
101800040708     C                   ENDIF
101900040708     C*** INC
102000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
102100040708     C                   EVAL      VAOINC = SkSplitCSV(i)
102200040708     C                   ENDIF
102300040708     C*** LOC
102400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
102500040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
102600040708     C                   ENDIF
102700040708     C*** PRC
102800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
102900040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
103000040708     C                   ENDIF
103100040708     C*** NAC
103200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
103300040708     C                   EVAL      VAONAC = SkSplitCSV(i)
103400040708     C                   ENDIF
103500040708     C*** FFD
103600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
103700040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
103800040708     C                   ENDIF
103900040708     C*** RMP
104000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
104100040708     C                   EVAL      VAORMP = SkSplitCSV(i)
104200040708     C                   ENDIF
104300040708     C*** NAM
104400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
104500040708     C                   EVAL      VAONAM = SkSplitCSV(i)
104600040708     C                   ENDIF
104700040708     C*** PAG
104800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
104900040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
105000040708     C                   ENDIF
105100040708     C*** CTR
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
105300040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
105400040708     C                   ENDIF
105500040708     C*** ZOR
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
105700040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
105800040708     C                   ENDIF
105900040708     C*** NO1
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
106100040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
106200030715     C                   ENDIF
106300040708     C*** NO2
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
106500040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
106600030715     C                   ENDIF
106700040708     C*** DDT
106800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
106900040708     C                   EVAL      VAODDT = SkSplitCSV(i)
107000030715     C                   ENDIF
107100040708     C*** RFA
107200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
107300040708     C                   EVAL      VAORFA = SkSplitCSV(i)
107400030715     C                   ENDIF
107500040708     C*** SPI
107600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
107700040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
107800030715     C                   ENDIF
107900040708     C*** FLO
108000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
108100040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
108200040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
108300080708     C* e la posizione 7 del VAOFLO
108400040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
108500040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
108600050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
108700030715     C                   ENDIF
108800091103     C
108900091103     C*** DAR
109000050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
109100050412     C                   EVAL      PiStr=SkSplitCSV(i)
109200050412     C                   EXSR      CHKNUM
109300050412     C                   IF        PiInt=*on
109400050412     C                   Z-ADD     PiVal         VAODAR
109500050412     C                   ELSE
109600050412     C                   SETON                                        32
109700050412     C                   EVAL      VAODAR = *zeros
109800050412     C                   EVAL      vinmsg = %trimr(vinmsg)
109900050412     C                             + ' ' + 'VAODAR'
110000050412     C                   ENDIF
110100050314     C                   ENDIF
110200030715     C
110300030721     C*
110400030721     C* Reperisco quindi i campi numerici...
110500040708     C*** POE
110600040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
110700040723     C                   EVAL      PiStr=SkSplitCSV(i)
110800040723     C                   EXSR      CHKNUM
110900040723     C                   IF        PiInt=*on    AND
111000040723     C                             PiVal<=999   AND
111100040723     C                             PiVal>*zeros
111200040723     C                   Z-ADD     PiVal         VAOPOE
111300040723     C                   ELSE
111400040723     C                   SETON                                        32
111500040723     C                   EVAL      VAOPOE = *zeros
111600040723     C                   EVAL      vinmsg = %trimr(vinmsg)
111700040723     C                             + ' ' + 'VAOPOE'
111800040723     C                   ENDIF
111900040723     C                   ENDIF
112000040708     C*** COR
112100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
112200040708     C                   EVAL      PiStr=SkSplitCSV(i)
112300040708     C                   EXSR      CHKNUM
112400040708     C                   IF        PiInt=*on
112500040708     C                   Z-ADD     PiVal         VAOCOR
112600040708     C                   ELSE
112700040708     C                   SETON                                        31
112800040708     C                   EVAL      VAOCOR = *zeros
112900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113000040708     C                             + ' ' + 'VAOCOR'
113100040708     C                   ENDIF
113200040708     C                   ENDIF
113300040708     C*** CAO
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiInt=*on
113800030721     C                   Z-ADD     PiVal         Num5_0
113900040708     C                   MOVEL(p)  Num5_0        VAOCAO
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200040708     C                   EVAL      VAOCAO = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400040708     C                             + ' ' + 'VAOCAO'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700040708     C*** CRA
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on
115200040708     C                   Z-ADD     PiVal         VAOCRA
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500040708     C                   EVAL      VAOCRA = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700040708     C                             + ' ' + 'VAOCRA'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000040708     C*** CAR
116100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
116200040708     C                   EVAL      PiStr=SkSplitCSV(i)
116300040708     C                   EXSR      CHKNUM
116400040708     C                   IF        PiInt=*on
116500040708     C                   Z-ADD     PiVal         Num5_0
116600040708     C                   MOVEL(p)  Num5_0        VAOCAR
116700040708     C                   ELSE
116800040708     C                   SETON                                        31
116900040708     C                   EVAL      VAOCAR = *zeros
117000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040708     C                             + ' ' + 'VAOCAR'
117200040708     C                   ENDIF
117300040708     C                   ENDIF
117400040708     C*** CRC
117500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
117600040708     C                   EVAL      PiStr=SkSplitCSV(i)
117700040708     C                   EXSR      CHKNUM
117800040708     C                   IF        PiInt=*on
117900040708     C                   Z-ADD     PiVal         VAOCRC
118000040708     C                   ELSE
118100040708     C                   SETON                                        32
118200040708     C                   EVAL      VAOCRC = *zeros
118300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040708     C                             + ' ' + 'VAOCRC'
118500040708     C                   ENDIF
118600040708     C                   ENDIF
118700040708     C*** CAC
118800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
118900040708     C                   EVAL      PiStr=SkSplitCSV(i)
119000040708     C                   EXSR      CHKNUM
119100040708     C                   IF        PiInt=*on
119200040708     C                   Z-ADD     PiVal         Num5_0
119300040708     C                   MOVEL(p)  Num5_0        VAOCAC
119400040708     C                   ELSE
119500040708     C                   SETON                                        32
119600040708     C                   EVAL      VAOCAC = *zeros
119700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119800040708     C                             + ' ' + 'VAOCAC'
119900040708     C                   ENDIF
120000040708     C                   ENDIF
120100040708     C*** DAR
120200090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
120300090428     C                   EVAL      PiStr=SkSplitCSV(i)
120400090428     C                   EXSR      CHKNUM
120500090428     C                   IF        PiInt=*on
120600090428     C                   Z-ADD     PiVal         VAODAR
120700090428     C                   ELSE
120800090428     C                   SETON                                        32
120900090428     C                   EVAL      VAODAR = *zeros
121000090428     C                   EVAL      vinmsg = %trimr(vinmsg)
121100090428     C                             + ' ' + 'VAODAR'
121200090428     C                   ENDIF
121300090428     C                   ENDIF
121400040708     C*** ORR
121500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
121600040708     C                   EVAL      PiStr=SkSplitCSV(i)
121700040708     C                   EXSR      CHKNUM
121800040708     C                   IF        PiInt=*on
121900040708     C                   Z-ADD     PiVal         VAOORR
122000040708     C                   ELSE
122100040708     C                   SETON                                        32
122200040708     C                   EVAL      VAOORR = *zeros
122300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122400040708     C                             + ' ' + 'VAOORR'
122500040708     C                   ENDIF
122600040708     C                   ENDIF
122700030721     C*** NCL
122800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on
123200040723     C                   IF        PiVal > *zeros
123300040708     C                   Z-ADD     PiVal         VAONCL
123400040723     C                   ENDIF
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700040708     C                   EVAL      VAONCL = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900040708     C                             + ' ' + 'VAONCL'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200040708     C*** PKG
124300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiNum=*on
124700040723     C                   IF        PiVal > *zeros
124800040708     C                   Z-ADD     PiVal         VAOPKG
124900040723     C                   ENDIF
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200040708     C                   EVAL      VAOPKG = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400040708     C                             + ' ' + 'VAOPKG'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700040708     C*** VLM
125800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
125900040708     C                   EVAL      PiStr=SkSplitCSV(i)
126000040708     C                   EXSR      CHKNUM
126100040708     C                   IF        PiNum=*on
126200040708     C                   Z-ADD     PiVal         VAOVLM
126300040708     C                   ELSE
126400040708     C                   SETON                                        32
126500040708     C                   EVAL      VAOVLM = *zeros
126600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126700040708     C                             + ' ' + 'VAOVLM'
126800040708     C                   ENDIF
126900040708     C                   ENDIF
127000040708     C*** BNC
127100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
127200040708     C                   EVAL      PiStr=SkSplitCSV(i)
127300040708     C                   EXSR      CHKNUM
127400040708     C                   IF        PiInt=*on
127500040708     C                   Z-ADD     PiVal         VAOBNC
127600040708     C                   ELSE
127700040708     C                   SETON                                        32
127800040708     C                   EVAL      VAOBNC = *zeros
127900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128000040708     C                             + ' ' + 'VAOBNC'
128100040708     C                   ENDIF
128200040708     C                   ENDIF
128300040708     C*** BLC
128400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
128500040708     C                   EVAL      PiStr=SkSplitCSV(i)
128600040708     C                   EXSR      CHKNUM
128700040708     C                   IF        PiInt=*on
128800040708     C                   Z-ADD     PiVal         VAOBLC
128900040708     C                   ELSE
129000040708     C                   SETON                                        32
129100040708     C                   EVAL      VAOBLC = *zeros
129200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129300040708     C                             + ' ' + 'VAOBLC'
129400040708     C                   ENDIF
129500040708     C                   ENDIF
129600040708     C*** ATT
129700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
129800040708     C                   EVAL      PiStr=SkSplitCSV(i)
129900040708     C                   EXSR      CHKNUM
130000040708     C                   IF        PiInt=*on
130100040708     C                   Z-ADD     PiVal         VAOATT
130200040708     C                   ELSE
130300040708     C                   SETON                                        32
130400040708     C                   EVAL      VAOATT = *zeros
130500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130600040708     C                             + ' ' + 'VAOATT'
130700040708     C                   ENDIF
130800040708     C                   ENDIF
130900040708     C*** MTC
131000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
131100040708     C                   EVAL      PiStr=SkSplitCSV(i)
131200040708     C                   EXSR      CHKNUM
131300040708     C                   IF        PiInt=*on
131400040708     C                   Z-ADD     PiVal         VAOMTC
131500040708     C                   ELSE
131600040708     C                   SETON                                        32
131700040708     C                   EVAL      VAOMTC = *zeros
131800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131900040708     C                             + ' ' + 'VAOMTC'
132000040708     C                   ENDIF
132100040708     C                   ENDIF
132200040708     C*** KSC
132300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on      AND
132700030721     C                             PiVal<=9999999 AND
132800030721     C                             PiVal>*zeros
132900040708     C                   Z-ADD     PiVal         VAOKSC
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200040708     C                   EVAL      VAOKSC = *zeros
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400040708     C                             + ' ' + 'VAOKSC'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700040708     C*** POR
133800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on    AND
134200030721     C                             PiVal<=999   AND
134300030721     C                             PiVal>*zeros
134400040708     C                   Z-ADD     PiVal         VAOPOR
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700040708     C                   EVAL      VAOPOR = *zeros
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900040708     C                             + ' ' + 'VAOPOR'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200040708     C*** POC
135300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
135400040708     C                   EVAL      PiStr=SkSplitCSV(i)
135500040708     C                   EXSR      CHKNUM
135600040708     C                   IF        PiInt=*on    AND
135700040708     C                             PiVal<=999   AND
135800040708     C                             PiVal>*zeros
135900040708     C                   Z-ADD     PiVal         VAOPOC
136000040708     C                   ELSE
136100040708     C                   SETON                                        32
136200040708     C                   EVAL      VAOPOC = *zeros
136300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136400040708     C                             + ' ' + 'VAOPOC'
136500040708     C                   ENDIF
136600040708     C                   ENDIF
136700040708     C*** STO
136800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
136900040708     C                   EVAL      PiStr=SkSplitCSV(i)
137000040708     C                   EXSR      CHKNUM
137100040708     C                   IF        PiInt=*on
137200040708     C                   Z-ADD     PiVal         VAOSTO
137300040708     C                   ELSE
137400040708     C                   SETON                                        32
137500040708     C                   EVAL      VAOSTO = *zeros
137600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137700040708     C                             + ' ' + 'VAOSTO'
137800040708     C                   ENDIF
137900040708     C                   ENDIF
138000090428     C*
138100091103     C********** GESTIONE CAMPI PERSONALIZZATI
138200120120
138300120123     C*** DAR
138400120123      * la data viene passata in formato ggmmaaaa
138500120123     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
138600120123     C                   EVAL      PiStr=SkSplitCSV(i)
138700120123     C                   EXSR      CHKNUM
138800120123     C                   IF        PiInt=*on
138900120123     C                   Z-ADD     PiVal         wdar              8 0
139000120123      * giro la data da ggmmaaaa a aaaammgg
139100120123     c                   eval      wData_EUR = %date(wdar:*eur)
139200120123     c                   eval      wData_ISO = wData_EUR
139300120123     c                   eval      VAODAR =  %dec(wData_ISO)
139400120123     C                   ELSE
139500120123     C                   SETON                                        32
139600120123     C                   EVAL      VAODAR = *zeros
139700120123     C                   EVAL      vinmsg = %trimr(vinmsg)
139800120123     C                             + ' ' + 'VAODAR'
139900120123     C                   ENDIF
140000120123     C                   ENDIF
140100120920     C*** RSR valorizza anche RER
140200120920     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSR'
140300120920     C                   EVAL      VAORER = SkSplitCSV(i)
140400120920     C                   EVAL      VAORSR = SkSplitCSV(i)
140500120920     C                   ENDIF
140600120920     C*
140700090428     C*
140800090428     C****************************************
140900090428     C*
141000030715     C*
141100030715     C                   ENDIF
141200030715     C                   EVAL      i = i + 1
141300030715     C                   ENDDO
141400040723     C*
141500030715     C                   ENDIF
141600020204     C*
141700010607     C                   ENDSR
141800010607     C*----------------------------------------------------*
141900080702
142000080702
142100080702     C*----------------------------------------------------*
142200080702     C*  CONTROLLO NUMERICITA' CAMPI
142300080702     C*----------------------------------------------------*
142400080702     C     CHKNUM        BEGSR
142500080702     C*
142600080702     C                   IF        PiDecChr = *blanks
142700080702     C                   EVAL      PiDecChr = ','
142800080702     C                   ENDIF
142900080702     C*
143000080702     C                   callp     UBISNUM_Check(PiStr
143100080702     C                                          :PiDecChr
143200080702     C                                          :PiVal
143300080702     C                                          :PiNum
143400080702     C                                          :PiInt)
143500080702     C*
143600080702     C                   ENDSR
143700080702     C***
143800080702
143900010601
144000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
144100010601     C     repfil        BEGSR
144200010601     C*
144300010601     C                   if        invfil = *zeros and
144400010601     C                             depfil > *zeros and
144500010629     C                             (vinflg = *blanks or
144600010629     C                              vinflg = *zeros)
144700010601     C
144800010601     C                   eval      invfil = depfil
144900010601     C                   endif
145000010601     C*
145100010601     C                   if        depfil <> invfil and
145200010601     C                             invfil > *zeros
145300010601     C                   eval      flgMulti = '1'
145400010601     C                   if        vinflg = *blanks
145500010601     C                   add       1             cntNonEl
145600010601     C                   endif
145700010601     C                   endif
145800010601     C*
145900010601     C                   if        vinflg = '2'
146000010601     C                   eval      flgStato = '2'
146100010601     C                   endif
146200010601     C*
146300010601     C                   ENDSR
146400010601     C***
146500010330
146600010330
146700990920      /TITLE Invio dei dati al punto operativo.
146800000613     C     invio         BEGSR
146900990920     C*
147000990920     C                   reset                   dscmz
147100010601     C                   move      invfil        cmzdst
147200990920     C                   eval      cmzfld = vlrfou
147300990920     C                   eval      cmzmbd = vlrhdl
147400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
147500000710     C                   if        prmfir = *blanks
147600040708     C                   eval      cmzfla = 'FNVAO00F'
147700040708     C                   eval      cmzmba = 'FNVAO00F'
147800000710     C                   else
147900000710     C                   eval      cmzfla = prmfir
148000000710     C                   eval      cmzmba = prmfir
148100000710     C                   endif
148200990920     C                   eval      cmznrr = *zeros
148300990920     C                   move      §ctrok        cmznrr
148400021018     C                   eval      cmzlba = vlrfl1
148500990920     C                   call(e)   'TIS711C'
148600990920     C                   parm                    dscmz
148700990921     C                   parm      *blanks       esito
148800990920     C                   if        %error
148900990920     C                             or cmzerr = '1'
149000990921     C                             or esito  = '1'
149100000710     C                   eval      wrkesito = '3'
149200990920     C                   endif
149300990920     C*
149400000613     C                   ENDSR
149500010601
149600010601
149700010601      /TITLE Invio dei dati al punto operativo.
149800010601     C     opeini        BEGSR
149900010601     C*
150000010601     C* Inizializzo flag e contatori operativi
150100010601     C                   movel     '0'           flgGiro           1
150200010601     C                   movel     '0'           flgMulti          1
150300010601     C                   movel     '1'           flgStato          1
150400010615     C                   movel     '0'           flgOk             1
150500010601     C                   z-add     *zeros        cntNonEl         10 0
150600010601     C                   z-add     *zeros        depfil            3 0
150700010601     C                   z-add     *zeros        invfil            3 0
150800010601     C*
150900010601     C                   ENDSR
151000010601     C***
151100050421
151200050421
151300091130     C     *pssr         BEGSR
151400050421     C*
151500091130     C                   eval      wrkesito = '2'
151600050421     C*
151700091130     C                   ENDSR     '*CANCL'
151800050421     C***
151900060306
152000060306
152100060306     C*-------------------------------------------------------------------------*
152200060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
152300060306     C*-------------------------------------------------------------------------*
152400060306     C     sr_Control    begsr
152500060306     C*
152600060306     C* inizializzo a ok il flag d controllo abilitazione
152700060306     C                   eval      Cli_Abil = *on
152800060306     C
152900060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
153000060306     C                   clear                   woriIDC
153100060306     C                   eval      kteCOD = 'VAP'
153200060306     C                   eval      kteKE1 = oriIDC
153300060306     C     keytbe        chain     TNTBE01L
153400060306     C                   if        %found(Tntbe01l)
153500060306     C                   eval      woriIDC = TbeKe2
153600060306     C                   else
153700060306     C                   eval      Cli_Abil = *off
153800060306     C                   endif
153900060306     C*
154000060306     C                   endsr
154100010330
154200010330
154300000613     C     *inzsr        BEGSR
154400990910     C*
154500990910     C     *entry        plist
154600990920     C                   parm                    tivlrds
154700990921     C                   parm      wrkesito      esito
154800000724     C                   parm                    prmlit
154900000710     C                   parm                    prmfir
155000010330     C*
155100010330     C* CALCOLA LA DATA CORRENTE
155200091127     C                   z-add     *zeros        datcor            8 0
155300091127     C                   time                    wn14             14 0
155400091127     C                   movel     wn14          wn6               6 0          *ORA (6)
155500091127     C                   eval      datcor = %dec(%date() : *ISO)
155600040723     C*
155700040723     C* Campi riferito al database
155800040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
155900040723     C     *like         define    tbeke1        kteKE1
156000040723     C*
156100040723     C* Definizione chiavi di lettura
156200040723     C* TNTBE01L - parziale
156300040723     C     keytbe        klist
156400040723     C                   kfld                    kteCOD                         *UTENTE
156500040723     C                   kfld                    kteKE1                         *CODICE
156600000613     C*
156700000613     C                   ENDSR
156800000613     C***
156900990908
157000040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
157100120920W_VAORSR;VAOINR;VAOLOR;VAOCAR;VAOPRR;VAONCL;VAOPKG;VAOVLM;X;X;X;VAOTER;
157200120905VAONO1;VAONO2;VAORFA;X;
157300120120
