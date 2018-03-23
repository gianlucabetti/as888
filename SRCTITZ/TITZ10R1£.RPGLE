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
003800030715     D*------------------
003900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004000030715     D*------------------
004100030715     D SkSplitFLD      S             10    DIM(100)
004200030715     D SkSplitCSV      S            256    DIM(100)
004300030715     D CharCSV         S              1
004400030715     D CharTXT         S              1
004500030715     D CharNUM         S              1
004600030715     D posDa           S              3  0 INZ(*zeros)
004700030715     D posA            S              3  0 INZ(*zeros)
004800030715     D i               s              3  0 INZ(1)
004900030715     D wGiro           s              1  0 INZ(*zeros)
005000030822     D*------------------
005100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005200030822     D*------------------
005300030822     D posDaDft        S              3  0 INZ(*zeros)
005400030822     D posADft         S              3  0 INZ(*zeros)
005500030822     D j               s              3  0 INZ(1)
005600030822     D wGiroDft        s              1  0 INZ(*zeros)
005700040723     D*------------------
005800040723     D* DS REPERIMENTO DATI CLIENTE
005900040723     D*-------------------
006000040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006100040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006200040723     D INDDS         E DS                  EXTNAME(CNIND00F)
006300040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006400040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006500080702     D*------------------
006600080702
006700080702     D*------------------
006800080702     D* LINKING A DEFINIZIONI ESTERNE
006900080702     D*------------------
007000090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007100090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007200091026
007300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
007500091026
007600080702
007700031201     D*------------------
007800031201     D* Costanti
007900031201     D*------------------
008000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008200010330
008300010330
008400010330
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000041011     C                   exsr      PREELA
009100040708     C                   exsr      rwvao
009200010601     C*
009300010601     C                   seton                                        lr
009400010601
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010000010601     C*--------------------------------------------------------
010100010601     C     PREELA        BEGSR
010200010601     C*
010300010601     C* SE OCCORRE SPEDIRE IN FILIALE
010400041011     C                   if        flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800041011     C*
010900041011     C                   endif
011000010601     C*
011100010601     C                   ENDSR
011200010601     C***
011300010601
011400010601
011500010601     C*--------------------------------------------------------
011600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011700010601     C*--------------------------------------------------------
011800010601     C     ENDELA        BEGSR
011900000616     C*
012000120209     C                   ENDSR
012100010601
012200010601
012300010330     C*--------------------------------------------------------
012400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
012500010330     C*--------------------------------------------------------
012600040708     C     RWVAO         BEGSR
012700010330     C*
012800010330     C                   if        not %open(tivin00r)
012900010330     C                   open      tivin00r
013000010330     C                   endif
013100040708     C                   if        not %open(fnvaowwr)
013200040708     C                   open      fnvaowwr
013300010330     C                   endif
013400010330     C*
013500010604     C                   clear                   §CTROK
013600010604     C                   clear                   §CTRMO
013700010604     C                   clear                   §CTRNO
013800120123     C*
013900120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
014000120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014100120123     C                   exsr      inzvar
014200120123     C                   exsr      impvao
014300010330     C*
014400030822     C                   do        *HIVAL
014500010330     C*
014600030822     C                   read      tivin00r                               70
014700010618     C*
014800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014900010618     C                   if        vinflg = '1'
015000010618     C                   eval      flgOk = '1'
015100010618     C                   endif
015200010618     C*
015300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015400040510     C                   z-add     *zeros        wLenVINDTA        4 0
015500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015900040510     C                   z-add     1             wFlgVINDTA
016000040510     C                   leave
016100040510     C                   endif
016200040510     C                   enddo
016300040510     C*
016400040218     C                   if        vindta > *blanks AND
016500040510     C                             wFlgVINDTA = 1
016600010330     C                   add       1             rrnum
016700010330     C*
016800010601     C                   if        *in70 = *off and
016900010330     C                             (vinflg = *blanks
017000010330     C                              or vinflg = '0'
017100010330     C                              or vinflg = '2')
017200010330     C*
017300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017400010711     C                   if        vinflg = *blanks or vinflg = '0'
017500010711     C                   clear                   vinmsg
017600010711     C                   endif
017700010601     C*
017800010330     C                   exsr      inzvar
017900040708     C                   exsr      impvao
018000010601     C*
018100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018200040708     C                   eval      depfil = VAOPOE
018300010601     C                   exsr      repfil
018400010601     C                   if        depfil = invfil
018500021025     C                   if        vlrpoi = 999
018600040708     C                   move(P)   invfil        VAOPOE
018700021025     C                   else
018800040708     C                   move(P)   vlrpoi        VAOPOE
018900021025     C                   endif
019000010601     C*
019100010604     C* Ebbene...
019200010604     C*
019300030822     C  N31              add       1             §CTROK            7 0
019400030822     C   32              add       1             §CTRMO            7 0
019500030822     C   31              add       1             §CTRNO            7 0
019600030822     C                   if        wGiro = 2
019700120123     C  N31              EXSR      CHKCOR
019800120123     C  N31              EXSR      WRIORI
019900120123     C  N31              EXSR      WRIVGD
020000120123     C  N31              write     FNVAO000
020100030822     C                   endif
020200010604     C*
020300010604     C                   if        *in31 = *off and
020400010604     C                             *in32 = *off
020500010604     C                   eval      vinflg = '1'
020600010604     C                   else
020700010604     C                   eval      vinflg = '2'
020800010604     C                   endif
020900010604     C                   endif
021000010601     C*
021100010604     C                   endif
021200010604     C*
021300010330     C                   else
021400010330     C                   eval      vinflg = '1'
021500010330     C                   endif
021600010601     C*
021700010601     C  N70              update    tivin000
021800010330     C*
021900030822     C  N70              enddo
022000010601     C
022100010601     C*
022200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022300010601     C                   if        cntNonEl = *zeros or
022400010601     C                             flgMulti = '0'
022500010330     C* Se non ci sono record con errori ...
022600010601     C                   if        §ctrno = 0 and
022700010604     C                             §ctrmo = 0 and
022800010601     C                             flgStato <> '2'
022900010330     C* ... restituisco esito OK.
023000010330     C                   eval      wrkesito = '0'
023100010330     C                   else
023200010330     C                   if        §ctrok > 0
023300010330     C                   eval      wrkesito = '1'
023400010330     C                   else
023500010615     C                   if        flgOk = '0'
023600010615     C                   eval      wrkesito = '2'
023700010615     C                   else
023800010615     C                   eval      wrkesito = '6'
023900010615     C                   endif
024000010330     C                   endif
024100010330     C                   endif
024200010601     C                   else
024300010601     C                   eval      wrkesito = '9'
024400010601     C                   endif
024500010330     C*
024600010330     C                   if        %open(tivin00r)
024700010330     C                   close     tivin00r
024800010330     C                   endif
024900040708     C                   if        %open(fnvaowwr)
025000040708     C                   close     fnvaowwr
025100010330     C                   endif
025200010601     C*
025300010601     C                   if        vlrpoi <> 999
025400010601     C                   eval      invfil = vlrpoi
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        §ctrok > 0
025800010601     C                             and invfil > *zeros
025900010330     C                   exsr      invio
026000010330     C                   endif
026100010601     C*
026200010618     C                   if        flgGiro = '1'
026300010601     C                   exsr      endela
026400010618     C                   endif
026500010330     C*
026600010330     C                   ENDSR
026700010330     C***
026800040723
026900040723
027000040723     C*------------------------------------------------------------------------*
027100040723     C* CHKCOR - Considerazioni su VAOCOR
027200040723     C*------------------------------------------------------------------------*
027300040723     C     CHKCOR        BEGSR
027400040723     C*
027500040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
027600040723     C                   if        vaoTCO = 'F'
027700040723     C* Quindi verifico se il codice ordinante non è valorizzato
027800040723     C                   if        vaoCOR = *zeros  AND
027900040723     C                             vaoRSO = *blanks AND
028000040723     C                             vaoINO = *blanks AND
028100040723     C                             vaoCAO = *blanks AND
028200040723     C                             vaoLOO = *blanks AND
028300040723     C                             vaoPRO = *blanks AND
028400040723     C                             vaoNAO = *blanks
028500040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
028600040723     C                   movel     '0'           wFlgAcr           1
028700040723     C                   move      vlrKSC        wCli              7 0
028800040723     C                   move(p)   wCli          acrCRO
028900040723     C     acrCRO        setll     fnacr01l
029000040723     C                   if        %found(fnacr01l)
029100040723     C                   read      fnacr01l
029200040723     C                   dow       not %eof(fnacr01l)
029300040723     C                   movel     acrCRO        wCliAcr           7 0
029400040723     C                   if        wCliAcr = wCli
029500040723     C                   eval      vaoCOR = acrCRO
029600040723     C                   movel     '1'           wFlgAcr
029700040723     C                   leave
029800040723     C                   else
029900040723     C                   leave
030000040723     C                   endif
030100040723     C                   read      fnacr01l
030200040723     C                   enddo
030300040723     C                   endif
030400040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
030500040723     C* dati anagrafici dell'ordinante dal piano dei conti
030600040723     C                   if        wFlgAcr = '0'
030700040723     C                   clear                   BS69DS
030800040723     C                   clear                   ACODS
030900040723     C                   clear                   INDDS
031000040723     C                   clear                   CLPDS
031100040723     C                   clear                   CLSDS
031200040723     C                   move(P)   vlrKSC        I69KAC
031300040723     C                   move(P)   vlrKSC        I69KIN
031400040723     C                   call      'TIBS69R'
031500040723     C                   parm                    BS69DS
031600040723     C                   parm                    ACODS
031700040723     C                   parm                    INDDS
031800040723     C                   parm                    CLPDS
031900040723     C                   parm                    CLSDS
032000040723     C     O69ERR        ifne      '1'
032100040723     C                   eval      vaoRSO = ACORAG
032200040723     C                   eval      vaoINO = INDVIA
032300040723     C                   movel(P)  INDCAP        vaoCAO
032400040723     C                   eval      vaoLOO = INDCIT
032500040723     C                   eval      vaoPRO = INDPRV
032600040723     C                   eval      vaoNAO = INDSTA
032700040723     C                   endif
032800040723     C                   endif
032900040723     C                   endif
033000040723     C                   endif
033100040723     C*
033200040723     C                   ENDSR
033300040723
033400040723
033500040723     C*------------------------------------------------------------------------*
033600040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
033700040723     C*------------------------------------------------------------------------*
033800040723     C     WRIORI        BEGSR
033900040723     C*
034000090618     C* Come prima cosa stacco un numeratore da AZNUM
034100090618     C                   clear                   trul33ds
034200090618     C                   eval      I33OPE = *zeros
034300090618     C                   eval      I33CNU = 600
034400090618     C                   eval      I33NUM = 1
034500090618     C                   movel     TRUL33DS      KPJBU
034600090618     C                   call      'TRUL33R'
034700090618     C                   parm                    KPJBA
034800090618     C                   movel     KPJBU         TRUL33DS
034900090618     C                   if        O33ERR = *zeros
035000090618     C                   z-add     O33NRF        §numori
035100090618     c                   else
035200090618     c                   eval      §numori = *all'9'
035300090618     C                   endif
035400040723     C*
035500040723     C* Quindi imposto i campi "extra-VAO"
035600040723     C                   if        vlrKSC = '0ORM0000'
035700040723     C*
035800040723     C                   if        vaoCOR > *zeros
035900040723     C                   movel     vaoCOR        wrkCOR7           7 0
036000040723     C*
036100040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
036200040723     C* codice padre. Se è un padre assegno già il codice ordinante.
036300040723     C                   clear                   tibs10ds
036400040723     C                   eval      d10drf = datcor                              *data riferimento
036500040723     C                   eval      d10paf = 'P'                                 *cerca il padre
036600040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
036700091026     C*
036800091026     C* Preparazione chiamata al TIBS10R1
036900091026     C                   clear                   tibs10r1ds
037000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
037100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
037200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
037300091026     C*
037400091026     C                   call      'TIBS10R1'
037500091026     C                   parm                    tibs10r1ds
037600091026     C*
037700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
037800040723     C*
037900040723     C                   if        d10err <> *blanks or                         *No unif. o padre
038000040723     C                             d10cop =  *zeros  or
038100040723     C                             d10cop =  d10cod
038200040723     C                   move      d10cod        oriIDC
038300040723     C                   else                                                   *Figlio
038400040723     C                   move      d10cop        oriIDC
038500040723     C                   endif
038600040723     C*
038700040723     C                   else
038800040723     C                   eval      oriIDC = *zeros
038900040723     C                   endif
039000040723     C*
039100040723     C                   else
039200040723     C                   eval      oriIDC = vlrKSC
039300040723     C                   endif
039400040723     C*
039500040723     C                   eval      oriPRG = §numori
039600040723     C                   eval      oriDER = datcor
039700040723     C                   eval      oriOER = wn6
039800040723     C                   eval      oriPOE = vaoPOE
039900040723     C                   eval      oriNSR = vaoNSR
040000040723     C                   eval      oriNOR = vaoNOR
040100040723     C                   eval      oriNRV = vaoNRV
040200040723     C                   eval      oriFLO = *blanks
040300040723     C*
040400040723     C* Infine valorizzo la chiave esterna sul file FNVAO
040500040723     C                   eval      dorm01 = vaoFLO
040600040723     C                   movel     §numori       §ormpg
040700050412     C*
040800050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
040900120123     C                   movel     'S'           §ormfr
041000120123     C                   movel     'S'           §ormks
041100120123     C                   movel     'S'           §ormfd
041200120123      * Imposto ORM commissionato in base a quanto passato dal cliente
041300120123     C                   IF        wORCOMC = *blanks
041400100416     C                   movel     'N'           §orcomc
041500120123     c                   ELSE
041600120123     c                   eval      §orcomc = wORCOMC
041700120123     c                   ENDIF
041800050412     C*
041900050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
042000040723     C                   eval      vaoFLO = dorm01
042100040723     C*
042200040723     C* ...quindi scrivo il file TIORI00F
042300040723     C                   WRITE     TIORI000
042400040723     C*
042500040723     C                   ENDSR
042600050826
042700050826
042800050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
042900050826     C     wriVGD        BEGSR
043000050826     C*
043100050826     C* Reperisco la descrizione della fase da tabella
043200050826     C                   movel(p)  'FAR'         KteCOD
043300050826     C                   movel(p)  '000'         KteKE1
043400050826     C     KEYtbe        chain     tntbe01l
043500050826     C                   if        %found(tntbe01l)
043600050826     C                   movel(P)  tbeuni        DFAR
043700050826     C                   else
043800050826     C                   clear                   DFAR
043900050826     C                   endif
044000050826     C*
044100050826     C* Valorizzo buffer record
044200050826     C                   clear                   FNVAPDS
044300050826     C                   eval      vapIDC = oriIDC
044400050826     C                   eval      vapPOE = oriPOE
044500050826     C                   eval      vapRFA = vaoRFA
044600050826     C                   eval      vapPOG = oriPOE
044700050826     C                   eval      vapDAE = oriDER
044800050826     C                   eval      vapORE = oriOER
044900050826     C                   if        d§fardva <> *blanks
045000050826     C                   eval      vapDFA = d§fardva
045100050826     C                   else
045200050826     C                   eval      vapDFA = d§fardes
045300050826     C                   endif
045400050826     C*
045500050826     C                   clear                   tivgd000
045600080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
045700050826     C                   eval      vgdTIP = 'VP'
045800050826     C                   eval      vgdKSU = vapIDC
045900050826     C                   eval      vgdTSC = 'WW'
046000050826     C                   eval      vgdDAT = datcor
046100120123     C                   eval      vgdPGM = 'TITZ10R'
046200060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
046300060306     C                   exsr      sr_Control
046400060306     C                   if        Cli_Abil = *on
046500060306     C                   eval      vgdKSU = woriIDC
046600050826     C                   write     tivgd000
046700060306     C                   endif
046800050826     C*
046900050826     C                   ENDSR
047000050826     C*------------------------------------------------------------------------*
047100010601
047200010601
047300010330     C*----------------------------------------------------*
047400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047500010330     C*----------------------------------------------------*
047600010330     C     INZVAR        BEGSR
047700010330     C*
047800030822     C* Inizializzo variabili di wrk
047900020204     C                   Z-ADD     *zeros        Num5_0            5 0
048000120124
048100120124      * pulisco campo di comodo per ORM COMMISSIONATO
048200120124     c                   clear                   wORCOMC           1
048300030822     C*
048400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048500120123     C                   CLEAR                   FNVAO000
048600120123     C                   CLEAR                   TIORI000
048700030822     C                   CLEAR                   SkSplitCSV
048800030822     C                   EVAL      i = 1
048900030822     C                   EVAL      posDa = *zeros
049000030822     C                   EVAL      posA  = *zeros
049100030822     C*
049200030822     C* Reimposto i valori di default
049300030822     C                   EXSR      DEFCAM
049400030822     C*
049500010330     C                   ENDSR
049600010330     C*----------------------------------------------------*
049700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049800010330     C*----------------------------------------------------*
049900010330     C     DEFCAM        BEGSR
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600040723     C*
050700080707     C* Imposto i valori d default d alcuni campi
050800080708     C*
050900080708     C                   CLEAR                   DORM01
051000091103     C                   EVAL      VAOFLO = DORM01
051100080707     C*
051200091103     C                   EVAL      VAOTCO = 'F'
051300120123     C                   EVAL      VAOTOR = 'S'
051400120123     C                   EVAL      VAODDT = 'S'
051500150727     C                   EVAL      VAOCRC = 1031812001
051600091127     C*
051700091204     C* IMPOSTO I CAMPI PERSONALIZZATI
051800100416     C
051900030822     C*
052000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052100030822     C                   EVAL      posDaDft = 1
052200030822     C                   EVAL      posADft  = 0
052300030822     C                   EVAL      wGiroDft = 0
052400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052500030822     C                             posDaDft > 0
052600030822     C*
052700030822     C* Gestisco il 1° giro
052800030822     C                   IF        wGiroDft = 0
052900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053100030822     C* Incremento il contatore dei "giri"
053200030822     C                   EVAL      wGiroDft = 1
053300030822     C                   ELSE
053400030822     C                   EVAL      posDaDft = posADft
053500030822     C                   ENDIF
053600030822     C* Eseguo lo scan x trovare la fine del campo corrente
053700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053800030822     C*
053900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054100030822     C                   IF        posDaDft > 0 AND
054200030822     C                             posADft  > 0
054300040708     C* POE
054400040708     C                   IF        %subst(
054500040708     C                             %subst(vlrppt:posDaDft+1:
054600040708     C                             posADft-posDaDft-1):1:3)
054700040708     C                             = 'POE'
054800040708     C                   EVAL      PiStr=%trim(%subst(
054900040708     C                             %subst(vlrppt:posDaDft+1:
055000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100040708     C                   EXSR      CHKNUM
055200040708     C                   IF        PiInt=*on
055300040708     C                   Z-ADD     PiVal         VAOPOE
055400040708     C                   ENDIF
055500040708     C                   ENDIF
055600040708     C* TOR
055700040708     C                   IF        %subst(
055800040708     C                             %subst(vlrppt:posDaDft+1:
055900040708     C                             posADft-posDaDft-1):1:3)
056000040708     C                             = 'TOR'
056100040708     C                   EVAL      VAOTOR=%trim(%subst(
056200040708     C                             %subst(vlrppt:posDaDft+1:
056300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400040708     C                   ENDIF
056500040708     C* TCO
056600040708     C                   IF        %subst(
056700040708     C                             %subst(vlrppt:posDaDft+1:
056800040708     C                             posADft-posDaDft-1):1:3)
056900040708     C                             = 'TCO'
057000040708     C                   EVAL      VAOTCO=%trim(%subst(
057100040708     C                             %subst(vlrppt:posDaDft+1:
057200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300040708     C                   ENDIF
057400040707     C* COR
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800040707     C                             = 'COR'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400040708     C                   Z-ADD     PiVal         VAOCOR
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700040708     C* CRC
058800040708     C                   IF        %subst(
058900040708     C                             %subst(vlrppt:posDaDft+1:
059000040708     C                             posADft-posDaDft-1):1:3)
059100040708     C                             = 'CRC'
059200040708     C                   EVAL      PiStr=%trim(%subst(
059300040708     C                             %subst(vlrppt:posDaDft+1:
059400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500040708     C                   EXSR      CHKNUM
059600040708     C                   IF        PiInt=*on
059700040708     C                   Z-ADD     PiVal         VAOCRC
059800040708     C                   ENDIF
059900040708     C                   ENDIF
060000040708     C* FFD
060100040708     C                   IF        %subst(
060200040708     C                             %subst(vlrppt:posDaDft+1:
060300040708     C                             posADft-posDaDft-1):1:3)
060400040708     C                             = 'FFD'
060500040708     C                   EVAL      VAOFFD=%trim(%subst(
060600040708     C                             %subst(vlrppt:posDaDft+1:
060700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800040708     C                   ENDIF
060900040708     C* ORR
061000040708     C                   IF        %subst(
061100040708     C                             %subst(vlrppt:posDaDft+1:
061200040708     C                             posADft-posDaDft-1):1:3)
061300040708     C                             = 'ORR'
061400040708     C                   EVAL      PiStr=%trim(%subst(
061500040708     C                             %subst(vlrppt:posDaDft+1:
061600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700040708     C                   EXSR      CHKNUM
061800040708     C                   IF        PiInt=*on
061900040708     C                   Z-ADD     PiVal         VAOORR
062000040708     C                   ENDIF
062100040708     C                   ENDIF
062200040712     C* NCL
062300040712     C                   IF        %subst(
062400040712     C                             %subst(vlrppt:posDaDft+1:
062500040712     C                             posADft-posDaDft-1):1:3)
062600040712     C                             = 'NCL'
062700040712     C                   EVAL      PiStr=%trim(%subst(
062800040712     C                             %subst(vlrppt:posDaDft+1:
062900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000040712     C                   EXSR      CHKNUM
063100040712     C                   IF        PiInt=*on
063200040712     C                   Z-ADD     PiVal         VAONCL
063300040712     C                   ENDIF
063400040712     C                   ENDIF
063500040712     C* PKG
063600040712     C                   IF        %subst(
063700040712     C                             %subst(vlrppt:posDaDft+1:
063800040712     C                             posADft-posDaDft-1):1:3)
063900040712     C                             = 'PKG'
064000040712     C                   EVAL      PiStr=%trim(%subst(
064100040712     C                             %subst(vlrppt:posDaDft+1:
064200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300040712     C                   EXSR      CHKNUM
064400040712     C                   IF        PiNum=*on
064500040712     C                   Z-ADD     PiVal         VAOPKG
064600040712     C                   ENDIF
064700040712     C                   ENDIF
064800040712     C* VLM
064900040712     C                   IF        %subst(
065000040712     C                             %subst(vlrppt:posDaDft+1:
065100040712     C                             posADft-posDaDft-1):1:3)
065200040712     C                             = 'VLM'
065300040712     C                   EVAL      PiStr=%trim(%subst(
065400040712     C                             %subst(vlrppt:posDaDft+1:
065500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600040712     C                   EXSR      CHKNUM
065700040712     C                   IF        PiNum=*on
065800040712     C                   Z-ADD     PiVal         VAOVLM
065900040712     C                   ENDIF
066000040712     C                   ENDIF
066100040708     C* NAM
066200040708     C                   IF        %subst(
066300040708     C                             %subst(vlrppt:posDaDft+1:
066400040708     C                             posADft-posDaDft-1):1:3)
066500040708     C                             = 'NAM'
066600040708     C                   EVAL      VAONAM=%trim(%subst(
066700040708     C                             %subst(vlrppt:posDaDft+1:
066800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900040708     C                   ENDIF
067000040708     C* PAG
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400040708     C                             = 'PAG'
067500040708     C                   EVAL      VAOPAG=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900040708     C* KSC
068000040708     C                   IF        %subst(
068100040708     C                             %subst(vlrppt:posDaDft+1:
068200040708     C                             posADft-posDaDft-1):1:3)
068300040708     C                             = 'KSC'
068400040708     C                   EVAL      PiStr=%trim(%subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700040708     C                   EXSR      CHKNUM
068800040708     C                   IF        PiInt=*on
068900040708     C                   Z-ADD     PiVal         VAOKSC
069000040708     C                   ENDIF
069100040708     C                   ENDIF
069200040708     C* CTR
069300040708     C                   IF        %subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):1:3)
069600040708     C                             = 'CTR'
069700040708     C                   EVAL      VAOCTR=%trim(%subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000040708     C                   ENDIF
070100040708     C* POC
070200040708     C                   IF        %subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):1:3)
070500040708     C                             = 'POC'
070600040708     C                   EVAL      PiStr=%trim(%subst(
070700040708     C                             %subst(vlrppt:posDaDft+1:
070800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900040708     C                   EXSR      CHKNUM
071000040708     C                   IF        PiInt=*on
071100040708     C                   Z-ADD     PiVal         VAOPOC
071200040708     C                   ENDIF
071300040708     C                   ENDIF
071400040708     C* NO1
071500040708     C                   IF        %subst(
071600040708     C                             %subst(vlrppt:posDaDft+1:
071700040708     C                             posADft-posDaDft-1):1:3)
071800040708     C                             = 'NO1'
071900040708     C                   EVAL      VAONO1=%trim(%subst(
072000040708     C                             %subst(vlrppt:posDaDft+1:
072100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200040708     C                   ENDIF
072300040708     C* DDT
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700040708     C                             = 'DDT'
072800040708     C                   EVAL      VAODDT=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200040708     C* RFA
073300040708     C                   IF        %subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):1:3)
073600040708     C                             = 'RFA'
073700040708     C                   EVAL      VAORFA=%trim(%subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000040708     C                   ENDIF
074100040708     C* FLO
074200040708     C                   IF        %subst(
074300040708     C                             %subst(vlrppt:posDaDft+1:
074400040708     C                             posADft-posDaDft-1):1:3)
074500040708     C                             = 'FLO'
074600040708     C                   EVAL      VAOFLO=%trim(%subst(
074700040708     C                             %subst(vlrppt:posDaDft+1:
074800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900091103     C*
075000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
075100080702     C* e posizione 7 del VAOFLO
075200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
075300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
075400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
075500040708     C                   ENDIF
075600030822     C* ...
075700030822     C                   ENDIF
075800030822     C                   ENDDO
075900020204     C*
076000010330     C                   ENDSR
076100010607     C*----------------------------------------------------*
076200040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
076300010607     C*----------------------------------------------------*
076400040708     C     IMPVAO        BEGSR
076500010607     C*
076600010607     C                   SETOFF                                       3132
076700120124
076800030515     C*
076900040708     C                   MOVEL     datcor        VAODAO
077000040708     C                   MOVEL     wn14          VAOOAO
077100050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077200050314     C     minu:maiu     XLATE     vindta        vindta
077300010607     C*
077400030715     C**********
077500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077600030715     C**********
077700030715     C                   IF        wGiro = *zeros
077800040709     C*
077900040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
078000040709     C                   EVAL      depvinDTA = vinDTA
078100040709     C*
078200040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
078300040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078400040709     C                                      %trim(intcol(3))
078500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078600050314     C**!!!minu:maiu     XLATE     vindta        vindta
078700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078800040324     C                   DOW       posDa <= %len(%trimr(vindta))
078900030715     C*
079000030715     C* Gestisco il 1° campo
079100030715     C                   IF        i = 1
079200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079300030715     C                   EVAL      posDa = 1
079400030715     C* Eseguo lo scan x trovare la fine del primo campo
079500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C* X i campi successivi al 1°
080000030715     C                   ELSE
080100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080200030715     C                   EVAL      posDa = posA + 1
080300030715     C* Eseguo lo scan x trovare la fine del campo corrente
080400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080500030715     C* Gestisco l'ultimo campo
080600030715     C                   IF        posA = *zeros
080700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
080800030715     C* A questo punto "estraggo" il campo corrente
080900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081000030715     C                                                    (posA-posDa))
081100030715     C                   LEAVE
081200030715     C                   ELSE
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C                   ENDIF
081700030715     C                   ENDIF
081800030715     C* Incremento il contatore d campo
081900030715     C                   EVAL      i = i +1
082000030715     C                   ENDDO
082100030715     C*
082200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082300030715     C* delimitazione testo
082400030715     C                   EVAL      i = 1
082500030715     C                   DOW       i < %elem(SkSplitFLD)
082600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082800030715     C                   EVAL      i = i + 1
082900030715     C                   ENDDO
083000030715     C                   EVAL      wGiro = 1
083100040709     C* Ripristino la 1° riga originale
083200040709     C                   EVAL      vinDTA = depvinDTA
083300030715     C                   ELSE
083400030715     C                   EVAL      wGiro = 2
083500031204     C**********
083600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083700031204     C* d separatore campo
083800031204     C**********
083900040709     C                   EVAL      vindta = %trim(vindta)
084000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084100040709     C                   EVAL      vindta = %subst(vindta:2)
084200040709     C                   ENDDO
084300040709     C*
084400040709     C                   Z-ADD     *zeros        lunghInput        4 0
084500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
084600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
084900040709     C                   ENDDO
085000030715     C**********
085100030715     C* Effettuo lo split del campo dati d input
085200030715     C**********
085300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085400040324     C                   DOW       posDa <= %len(%trimr(vindta))
085500030715     C*
085600030715     C* Gestisco il 1° campo
085700030715     C                   IF        i = 1
085800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085900030715     C                   EVAL      posDa = 1
086000030715     C* Eseguo lo scan x trovare la fine del primo campo
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C* X i campi successivi al 1°
086600030715     C                   ELSE
086700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086800030715     C                   EVAL      posDa = posA + 1
086900031204     C* Verifico che nn vi sia il campo nn valorizzato
087000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087100031204     C* Se campo nn valorizzato skippo al prossimo
087200031204     C                   EVAL      SkSplitCSV(i) = *blanks
087300031204     C                   EVAL      posA  = posA + 1
087400031204     C                   ELSE
087500030715     C* Eseguo lo scan x trovare la fine del campo corrente
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* Gestisco l'ultimo campo
087800030715     C                   IF        posA = *zeros
087900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   LEAVE
088400030715     C                   ELSE
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   ENDIF
088900031204     C                   ENDIF
089000030715     C                   ENDIF
089100030715     C* Incremento il contatore d campo
089200030715     C                   EVAL      i = i +1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089600030715     C* delimitazione testo
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitCSV)
089900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090100030715     C                   EVAL      i = i + 1
090200030715     C                   ENDDO
090300030715     C*
090400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090500030715     C                   EVAL      i = 1
090600030715     C                   DOW       i < %elem(SkSplitFLD)
090700030715     C*
090800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091000030715     C                             SkSplitCSV(i) <> *blanks
091100030721     C*** ATB
091200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
091300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
091400030721     C                   ENDIF
091500040708     C*** TOR
091600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
091700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
091800040708     C                   ENDIF
091900040708     C*** TCO
092000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
092100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
092200040708     C                   ENDIF
092300040708     C*** RSO
092400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
092500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
092600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
092800030715     C* ==
092900030715     C                   ENDIF
093000040708     C*** INO
093100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
093200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
093300030715     C                   ENDIF
093400040708     C*** LOO
093500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
093600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
093700030715     C                   ENDIF
093800040708     C*** PRO
093900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
094000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
094100030715     C                   ENDIF
094200040708     C*** NAO
094300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
094400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
094500030721     C                   ENDIF
094600040708     C*** RSR
094700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
094800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
094900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
095100040708     C* ==
095200040708     C                   IF        VAORSR = *blanks
095300040708     C                   SETON                                        31
095400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
095500040708     C                             + ' ' + 'VAORSR'
095600040708     C                   ENDIF
095700040708     C                   ENDIF
095800040708     C*** INR
095900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
096000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
096100040708     C                   IF        VAOINR = *blanks
096200040708     C                   SETON                                        31
096300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
096400040708     C                             + ' ' + 'VAOINR'
096500040708     C                   ENDIF
096600040708     C                   ENDIF
096700040708     C*** LOR
096800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
096900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
097000040708     C                   IF        VAOLOR = *blanks
097100040708     C                   SETON                                        31
097200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097300040708     C                             + ' ' + 'VAOLOR'
097400040708     C                   ENDIF
097500040708     C                   ENDIF
097600040708     C*** PRR
097700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
097800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
097900040708     C                   IF        VAOPRR = *blanks
098000040708     C                   SETON                                        31
098100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098200040708     C                             + ' ' + 'VAOPRR'
098300040708     C                   ENDIF
098400040708     C                   ENDIF
098500040708     C*** NAR
098600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
098700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
098800040708     C                   ENDIF
098900040708     C*** RER
099000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
099100040708     C                   EVAL      VAORER = SkSplitCSV(i)
099200040708     C                   ENDIF
099300040708     C*** TER
099400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
099500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
099600040708     C                   ENDIF
099700040708     C*** RSC
099800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
099900040708     C                   EVAL      VAORSC = SkSplitCSV(i)
100000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100100040708     C     '@':'A'       XLATE     VAORSC        VAORSC
100200040708     C* ==
100300040708     C                   ENDIF
100400040708     C*** INC
100500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
100600040708     C                   EVAL      VAOINC = SkSplitCSV(i)
100700040708     C                   ENDIF
100800040708     C*** LOC
100900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
101000040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
101100040708     C                   ENDIF
101200040708     C*** PRC
101300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
101400040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
101500040708     C                   ENDIF
101600040708     C*** NAC
101700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
101800040708     C                   EVAL      VAONAC = SkSplitCSV(i)
101900040708     C                   ENDIF
102000040708     C*** FFD
102100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
102200040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
102300040708     C                   ENDIF
102400040708     C*** RMP
102500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
102600040708     C                   EVAL      VAORMP = SkSplitCSV(i)
102700040708     C                   ENDIF
102800040708     C*** NAM
102900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
103000040708     C                   EVAL      VAONAM = SkSplitCSV(i)
103100040708     C                   ENDIF
103200040708     C*** PAG
103300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
103400040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
103500040708     C                   ENDIF
103600040708     C*** CTR
103700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
103800040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
103900040708     C                   ENDIF
104000040708     C*** ZOR
104100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
104200040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
104300040708     C                   ENDIF
104400040708     C*** NO1
104500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
104600040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
104700030715     C                   ENDIF
104800040708     C*** NO2
104900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
105000040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
105100030715     C                   ENDIF
105200040708     C*** DDT
105300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
105400040708     C                   EVAL      VAODDT = SkSplitCSV(i)
105500030715     C                   ENDIF
105600040708     C*** RFA
105700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
105800040708     C                   EVAL      VAORFA = SkSplitCSV(i)
105900030715     C                   ENDIF
106000040708     C*** SPI
106100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
106200040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
106300030715     C                   ENDIF
106400040708     C*** FLO
106500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
106600040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
106700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
106800080708     C* e la posizione 7 del VAOFLO
106900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
107000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
107100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
107200030715     C                   ENDIF
107300091103     C
107400091103     C*** DAR
107500050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
107600050412     C                   EVAL      PiStr=SkSplitCSV(i)
107700050412     C                   EXSR      CHKNUM
107800050412     C                   IF        PiInt=*on
107900050412     C                   Z-ADD     PiVal         VAODAR
108000050412     C                   ELSE
108100050412     C                   SETON                                        32
108200050412     C                   EVAL      VAODAR = *zeros
108300050412     C                   EVAL      vinmsg = %trimr(vinmsg)
108400050412     C                             + ' ' + 'VAODAR'
108500050412     C                   ENDIF
108600050314     C                   ENDIF
108700030715     C
108800030721     C*
108900030721     C* Reperisco quindi i campi numerici...
109000040708     C*** POE
109100040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
109200040723     C                   EVAL      PiStr=SkSplitCSV(i)
109300040723     C                   EXSR      CHKNUM
109400040723     C                   IF        PiInt=*on    AND
109500040723     C                             PiVal<=999   AND
109600040723     C                             PiVal>*zeros
109700040723     C                   Z-ADD     PiVal         VAOPOE
109800040723     C                   ELSE
109900040723     C                   SETON                                        32
110000040723     C                   EVAL      VAOPOE = *zeros
110100040723     C                   EVAL      vinmsg = %trimr(vinmsg)
110200040723     C                             + ' ' + 'VAOPOE'
110300040723     C                   ENDIF
110400040723     C                   ENDIF
110500040708     C*** COR
110600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
110700040708     C                   EVAL      PiStr=SkSplitCSV(i)
110800040708     C                   EXSR      CHKNUM
110900040708     C                   IF        PiInt=*on
111000040708     C                   Z-ADD     PiVal         VAOCOR
111100040708     C                   ELSE
111200040708     C                   SETON                                        31
111300040708     C                   EVAL      VAOCOR = *zeros
111400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040708     C                             + ' ' + 'VAOCOR'
111600040708     C                   ENDIF
111700040708     C                   ENDIF
111800040708     C*** CAO
111900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on
112300030721     C                   Z-ADD     PiVal         Num5_0
112400040708     C                   MOVEL(p)  Num5_0        VAOCAO
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700040708     C                   EVAL      VAOCAO = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040708     C                             + ' ' + 'VAOCAO'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200040708     C*** CRA
113300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700040708     C                   Z-ADD     PiVal         VAOCRA
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000040708     C                   EVAL      VAOCRA = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200040708     C                             + ' ' + 'VAOCRA'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500040708     C*** CAR
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
114700040708     C                   EVAL      PiStr=SkSplitCSV(i)
114800040708     C                   EXSR      CHKNUM
114900040708     C                   IF        PiInt=*on
115000040708     C                   Z-ADD     PiVal         Num5_0
115100040708     C                   MOVEL(p)  Num5_0        VAOCAR
115200040708     C                   ELSE
115300040708     C                   SETON                                        31
115400040708     C                   EVAL      VAOCAR = *zeros
115500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040708     C                             + ' ' + 'VAOCAR'
115700040708     C                   ENDIF
115800040708     C                   ENDIF
115900040708     C*** CRC
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
116100040708     C                   EVAL      PiStr=SkSplitCSV(i)
116200040708     C                   EXSR      CHKNUM
116300040708     C                   IF        PiInt=*on
116400040708     C                   Z-ADD     PiVal         VAOCRC
116500040708     C                   ELSE
116600040708     C                   SETON                                        32
116700040708     C                   EVAL      VAOCRC = *zeros
116800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
116900040708     C                             + ' ' + 'VAOCRC'
117000040708     C                   ENDIF
117100040708     C                   ENDIF
117200040708     C*** CAC
117300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
117400040708     C                   EVAL      PiStr=SkSplitCSV(i)
117500040708     C                   EXSR      CHKNUM
117600040708     C                   IF        PiInt=*on
117700040708     C                   Z-ADD     PiVal         Num5_0
117800040708     C                   MOVEL(p)  Num5_0        VAOCAC
117900040708     C                   ELSE
118000040708     C                   SETON                                        32
118100040708     C                   EVAL      VAOCAC = *zeros
118200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040708     C                             + ' ' + 'VAOCAC'
118400040708     C                   ENDIF
118500040708     C                   ENDIF
118600040708     C*** DAR
118700090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
118800090428     C                   EVAL      PiStr=SkSplitCSV(i)
118900090428     C                   EXSR      CHKNUM
119000090428     C                   IF        PiInt=*on
119100090428     C                   Z-ADD     PiVal         VAODAR
119200090428     C                   ELSE
119300090428     C                   SETON                                        32
119400090428     C                   EVAL      VAODAR = *zeros
119500090428     C                   EVAL      vinmsg = %trimr(vinmsg)
119600090428     C                             + ' ' + 'VAODAR'
119700090428     C                   ENDIF
119800090428     C                   ENDIF
119900040708     C*** ORR
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
120100040708     C                   EVAL      PiStr=SkSplitCSV(i)
120200040708     C                   EXSR      CHKNUM
120300040708     C                   IF        PiInt=*on
120400040708     C                   Z-ADD     PiVal         VAOORR
120500040708     C                   ELSE
120600040708     C                   SETON                                        32
120700040708     C                   EVAL      VAOORR = *zeros
120800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040708     C                             + ' ' + 'VAOORR'
121000040708     C                   ENDIF
121100040708     C                   ENDIF
121200030721     C*** NCL
121300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on
121700040723     C                   IF        PiVal > *zeros
121800040708     C                   Z-ADD     PiVal         VAONCL
121900040723     C                   ENDIF
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200040708     C                   EVAL      VAONCL = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400040708     C                             + ' ' + 'VAONCL'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700040708     C*** PKG
122800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200040723     C                   IF        PiVal > *zeros
123300040708     C                   Z-ADD     PiVal         VAOPKG
123400040723     C                   ENDIF
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700040708     C                   EVAL      VAOPKG = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900040708     C                             + ' ' + 'VAOPKG'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200040708     C*** VLM
124300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
124400040708     C                   EVAL      PiStr=SkSplitCSV(i)
124500040708     C                   EXSR      CHKNUM
124600040708     C                   IF        PiNum=*on
124700040708     C                   Z-ADD     PiVal         VAOVLM
124800040708     C                   ELSE
124900040708     C                   SETON                                        32
125000040708     C                   EVAL      VAOVLM = *zeros
125100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125200040708     C                             + ' ' + 'VAOVLM'
125300040708     C                   ENDIF
125400040708     C                   ENDIF
125500040708     C*** BNC
125600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
125700040708     C                   EVAL      PiStr=SkSplitCSV(i)
125800040708     C                   EXSR      CHKNUM
125900040708     C                   IF        PiInt=*on
126000040708     C                   Z-ADD     PiVal         VAOBNC
126100040708     C                   ELSE
126200040708     C                   SETON                                        32
126300040708     C                   EVAL      VAOBNC = *zeros
126400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126500040708     C                             + ' ' + 'VAOBNC'
126600040708     C                   ENDIF
126700040708     C                   ENDIF
126800040708     C*** BLC
126900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
127000040708     C                   EVAL      PiStr=SkSplitCSV(i)
127100040708     C                   EXSR      CHKNUM
127200040708     C                   IF        PiInt=*on
127300040708     C                   Z-ADD     PiVal         VAOBLC
127400040708     C                   ELSE
127500040708     C                   SETON                                        32
127600040708     C                   EVAL      VAOBLC = *zeros
127700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127800040708     C                             + ' ' + 'VAOBLC'
127900040708     C                   ENDIF
128000040708     C                   ENDIF
128100040708     C*** ATT
128200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
128300040708     C                   EVAL      PiStr=SkSplitCSV(i)
128400040708     C                   EXSR      CHKNUM
128500040708     C                   IF        PiInt=*on
128600040708     C                   Z-ADD     PiVal         VAOATT
128700040708     C                   ELSE
128800040708     C                   SETON                                        32
128900040708     C                   EVAL      VAOATT = *zeros
129000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129100040708     C                             + ' ' + 'VAOATT'
129200040708     C                   ENDIF
129300040708     C                   ENDIF
129400040708     C*** MTC
129500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
129600040708     C                   EVAL      PiStr=SkSplitCSV(i)
129700040708     C                   EXSR      CHKNUM
129800040708     C                   IF        PiInt=*on
129900040708     C                   Z-ADD     PiVal         VAOMTC
130000040708     C                   ELSE
130100040708     C                   SETON                                        32
130200040708     C                   EVAL      VAOMTC = *zeros
130300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130400040708     C                             + ' ' + 'VAOMTC'
130500040708     C                   ENDIF
130600040708     C                   ENDIF
130700040708     C*** KSC
130800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on      AND
131200030721     C                             PiVal<=9999999 AND
131300030721     C                             PiVal>*zeros
131400040708     C                   Z-ADD     PiVal         VAOKSC
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700040708     C                   EVAL      VAOKSC = *zeros
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900040708     C                             + ' ' + 'VAOKSC'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200040708     C*** POR
132300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on    AND
132700030721     C                             PiVal<=999   AND
132800030721     C                             PiVal>*zeros
132900040708     C                   Z-ADD     PiVal         VAOPOR
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200040708     C                   EVAL      VAOPOR = *zeros
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400040708     C                             + ' ' + 'VAOPOR'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700040708     C*** POC
133800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
133900040708     C                   EVAL      PiStr=SkSplitCSV(i)
134000040708     C                   EXSR      CHKNUM
134100040708     C                   IF        PiInt=*on    AND
134200040708     C                             PiVal<=999   AND
134300040708     C                             PiVal>*zeros
134400040708     C                   Z-ADD     PiVal         VAOPOC
134500040708     C                   ELSE
134600040708     C                   SETON                                        32
134700040708     C                   EVAL      VAOPOC = *zeros
134800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134900040708     C                             + ' ' + 'VAOPOC'
135000040708     C                   ENDIF
135100040708     C                   ENDIF
135200040708     C*** STO
135300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
135400040708     C                   EVAL      PiStr=SkSplitCSV(i)
135500040708     C                   EXSR      CHKNUM
135600040708     C                   IF        PiInt=*on
135700040708     C                   Z-ADD     PiVal         VAOSTO
135800040708     C                   ELSE
135900040708     C                   SETON                                        32
136000040708     C                   EVAL      VAOSTO = *zeros
136100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136200040708     C                             + ' ' + 'VAOSTO'
136300040708     C                   ENDIF
136400040708     C                   ENDIF
136500090428     C*
136600091103     C********** GESTIONE CAMPI PERSONALIZZATI
136700120120
136800120123     C*** DAR
136900120123      * la data viene passata in formato ggmmaaaa
137000120123     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
137100120123     C                   EVAL      PiStr=SkSplitCSV(i)
137200120123     C                   EXSR      CHKNUM
137300120123     C                   IF        PiInt=*on
137400120123     C                   Z-ADD     PiVal         wdar              8 0
137500120123      * giro la data da ggmmaaaa a aaaammgg
137600120123     c                   eval      wData_EUR = %date(wdar:*eur)
137700120123     c                   eval      wData_ISO = wData_EUR
137800120123     c                   eval      VAODAR =  %dec(wData_ISO)
137900120123     C                   ELSE
138000120123     C                   SETON                                        32
138100120123     C                   EVAL      VAODAR = *zeros
138200120123     C                   EVAL      vinmsg = %trimr(vinmsg)
138300120123     C                             + ' ' + 'VAODAR'
138400120123     C                   ENDIF
138500120123     C                   ENDIF
138600120123     C*** ORM COMMISSIONATO
138700120123     C                   IF        %trim(SkSplitFLD(i)) = 'W_ORCOMC'
138800120123     C                   EVAL      wORCOMC = SkSplitCSV(i)
138900120123     C                   ENDIF
139000090428     C*
139100090428     C*
139200090428     C****************************************
139300090428     C*
139400030715     C*
139500030715     C                   ENDIF
139600030715     C                   EVAL      i = i + 1
139700030715     C                   ENDDO
139800040723     C*
139900030715     C                   ENDIF
140000020204     C*
140100010607     C                   ENDSR
140200010607     C*----------------------------------------------------*
140300080702
140400080702
140500080702     C*----------------------------------------------------*
140600080702     C*  CONTROLLO NUMERICITA' CAMPI
140700080702     C*----------------------------------------------------*
140800080702     C     CHKNUM        BEGSR
140900080702     C*
141000080702     C                   IF        PiDecChr = *blanks
141100080702     C                   EVAL      PiDecChr = ','
141200080702     C                   ENDIF
141300080702     C*
141400080702     C                   callp     UBISNUM_Check(PiStr
141500080702     C                                          :PiDecChr
141600080702     C                                          :PiVal
141700080702     C                                          :PiNum
141800080702     C                                          :PiInt)
141900080702     C*
142000080702     C                   ENDSR
142100080702     C***
142200080702
142300010601
142400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142500010601     C     repfil        BEGSR
142600010601     C*
142700010601     C                   if        invfil = *zeros and
142800010601     C                             depfil > *zeros and
142900010629     C                             (vinflg = *blanks or
143000010629     C                              vinflg = *zeros)
143100010601     C
143200010601     C                   eval      invfil = depfil
143300010601     C                   endif
143400010601     C*
143500010601     C                   if        depfil <> invfil and
143600010601     C                             invfil > *zeros
143700010601     C                   eval      flgMulti = '1'
143800010601     C                   if        vinflg = *blanks
143900010601     C                   add       1             cntNonEl
144000010601     C                   endif
144100010601     C                   endif
144200010601     C*
144300010601     C                   if        vinflg = '2'
144400010601     C                   eval      flgStato = '2'
144500010601     C                   endif
144600010601     C*
144700010601     C                   ENDSR
144800010601     C***
144900010330
145000010330
145100990920      /TITLE Invio dei dati al punto operativo.
145200000613     C     invio         BEGSR
145300990920     C*
145400990920     C                   reset                   dscmz
145500010601     C                   move      invfil        cmzdst
145600990920     C                   eval      cmzfld = vlrfou
145700990920     C                   eval      cmzmbd = vlrhdl
145800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
145900000710     C                   if        prmfir = *blanks
146000040708     C                   eval      cmzfla = 'FNVAO00F'
146100040708     C                   eval      cmzmba = 'FNVAO00F'
146200000710     C                   else
146300000710     C                   eval      cmzfla = prmfir
146400000710     C                   eval      cmzmba = prmfir
146500000710     C                   endif
146600990920     C                   eval      cmznrr = *zeros
146700990920     C                   move      §ctrok        cmznrr
146800021018     C                   eval      cmzlba = vlrfl1
146900990920     C                   call(e)   'TIS711C'
147000990920     C                   parm                    dscmz
147100990921     C                   parm      *blanks       esito
147200990920     C                   if        %error
147300990920     C                             or cmzerr = '1'
147400990921     C                             or esito  = '1'
147500000710     C                   eval      wrkesito = '3'
147600990920     C                   endif
147700990920     C*
147800000613     C                   ENDSR
147900010601
148000010601
148100010601      /TITLE Invio dei dati al punto operativo.
148200010601     C     opeini        BEGSR
148300010601     C*
148400010601     C* Inizializzo flag e contatori operativi
148500010601     C                   movel     '0'           flgGiro           1
148600010601     C                   movel     '0'           flgMulti          1
148700010601     C                   movel     '1'           flgStato          1
148800010615     C                   movel     '0'           flgOk             1
148900010601     C                   z-add     *zeros        cntNonEl         10 0
149000010601     C                   z-add     *zeros        depfil            3 0
149100010601     C                   z-add     *zeros        invfil            3 0
149200010601     C*
149300010601     C                   ENDSR
149400010601     C***
149500050421
149600050421
149700091130     C     *pssr         BEGSR
149800050421     C*
149900091130     C                   eval      wrkesito = '2'
150000050421     C*
150100091130     C                   ENDSR     '*CANCL'
150200050421     C***
150300060306
150400060306
150500060306     C*-------------------------------------------------------------------------*
150600060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
150700060306     C*-------------------------------------------------------------------------*
150800060306     C     sr_Control    begsr
150900060306     C*
151000060306     C* inizializzo a ok il flag d controllo abilitazione
151100060306     C                   eval      Cli_Abil = *on
151200060306     C
151300060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
151400060306     C                   clear                   woriIDC
151500060306     C                   eval      kteCOD = 'VAP'
151600060306     C                   eval      kteKE1 = oriIDC
151700060306     C     keytbe        chain     TNTBE01L
151800060306     C                   if        %found(Tntbe01l)
151900060306     C                   eval      woriIDC = TbeKe2
152000060306     C                   else
152100060306     C                   eval      Cli_Abil = *off
152200060306     C                   endif
152300060306     C*
152400060306     C                   endsr
152500010330
152600010330
152700000613     C     *inzsr        BEGSR
152800990910     C*
152900990910     C     *entry        plist
153000990920     C                   parm                    tivlrds
153100990921     C                   parm      wrkesito      esito
153200000724     C                   parm                    prmlit
153300000710     C                   parm                    prmfir
153400010330     C*
153500010330     C* CALCOLA LA DATA CORRENTE
153600091127     C                   z-add     *zeros        datcor            8 0
153700091127     C                   time                    wn14             14 0
153800091127     C                   movel     wn14          wn6               6 0          *ORA (6)
153900091127     C                   eval      datcor = %dec(%date() : *ISO)
154000040723     C*
154100040723     C* Campi riferito al database
154200040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
154300040723     C     *like         define    tbeke1        kteKE1
154400040723     C*
154500040723     C* Definizione chiavi di lettura
154600040723     C* TNTBE01L - parziale
154700040723     C     keytbe        klist
154800040723     C                   kfld                    kteCOD                         *UTENTE
154900040723     C                   kfld                    kteKE1                         *CODICE
155000000613     C*
155100000613     C                   ENDSR
155200000613     C***
155300990908
155400040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
155500140204VAORFA;VAONCL;VAOPKG;X;VAORER;VAORSR;VAOINR;VAOCAR;VAOLOR;
155600120123VAOTER;VAOPRR;X;W_VAODAR;VAONO1;W_ORCOMC;
155700120120
