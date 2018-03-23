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
004300130222     D Intestazioni    s               n   inz(*off)
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
007200121207     D wRFA1           s              1
007300121207     D wRFA2           s              7
007400121207     D wRFA3           s              3
007500121207     D wRFA4           s              2
007600080702
007700080702     D*------------------
007800080702     D* LINKING A DEFINIZIONI ESTERNE
007900080702     D*------------------
008000090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008100090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200091026
008300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008500091026
008600080702
008700031201     D*------------------
008800031201     D* Costanti
008900031201     D*------------------
009000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200010330
009300010330
009400010330
009500000913     C                   reset                   rrnum
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800010601     C*
009900010601     C                   exsr      opeini
010000041011     C                   exsr      PREELA
010100040708     C                   exsr      rwvao
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400041011     C                   if        flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800041011     C*
011900041011     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010601     C*--------------------------------------------------------
012800010601     C     ENDELA        BEGSR
012900000616     C*
013000120209     C                   ENDSR
013100010601
013200010601
013300010330     C*--------------------------------------------------------
013400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
013500010330     C*--------------------------------------------------------
013600040708     C     RWVAO         BEGSR
013700010330     C*
013800010330     C                   if        not %open(tivin00r)
013900010330     C                   open      tivin00r
014000010330     C                   endif
014100040708     C                   if        not %open(fnvaowwr)
014200040708     C                   open      fnvaowwr
014300010330     C                   endif
014400010330     C*
014500010604     C                   clear                   §CTROK
014600010604     C                   clear                   §CTRMO
014700010604     C                   clear                   §CTRNO
014800120123     C*
014900120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
015000120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
015100120905     C                   IF        Intestazioni = *off
015200120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
015300120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015400120123     C                   exsr      inzvar
015500120123     C                   exsr      impvao
015600120905     C                   ENDIF
015700010330     C*
015800030822     C                   do        *HIVAL
015900010330     C*
016000030822     C                   read      tivin00r                               70
016100010618     C*
016200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016300010618     C                   if        vinflg = '1'
016400010618     C                   eval      flgOk = '1'
016500010618     C                   endif
016600010618     C*
016700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016800040510     C                   z-add     *zeros        wLenVINDTA        4 0
016900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017300040510     C                   z-add     1             wFlgVINDTA
017400040510     C                   leave
017500040510     C                   endif
017600040510     C                   enddo
017700040510     C*
017800040218     C                   if        vindta > *blanks AND
017900040510     C                             wFlgVINDTA = 1
018000010330     C                   add       1             rrnum
018100010330     C*
018200010601     C                   if        *in70 = *off and
018300010330     C                             (vinflg = *blanks
018400010330     C                              or vinflg = '0'
018500010330     C                              or vinflg = '2')
018600010330     C*
018700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018800010711     C                   if        vinflg = *blanks or vinflg = '0'
018900010711     C                   clear                   vinmsg
019000010711     C                   endif
019100010601     C*
019200010330     C                   exsr      inzvar
019300040708     C                   exsr      impvao
019400010601     C*
019500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019600040708     C                   eval      depfil = VAOPOE
019700010601     C                   exsr      repfil
019800010601     C                   if        depfil = invfil
019900021025     C                   if        vlrpoi = 999
020000040708     C                   move(P)   invfil        VAOPOE
020100021025     C                   else
020200040708     C                   move(P)   vlrpoi        VAOPOE
020300021025     C                   endif
020400010601     C*
020500010604     C* Ebbene...
020600010604     C*
020700030822     C  N31              add       1             §CTROK            7 0
020800030822     C   32              add       1             §CTRMO            7 0
020900030822     C   31              add       1             §CTRNO            7 0
021000030822     C                   if        wGiro = 2
021100120123     C  N31              EXSR      CHKCOR
021200120123     C  N31              EXSR      WRIORI
021300120123     C  N31              EXSR      WRIVGD
021400120123     C  N31              write     FNVAO000
021500030822     C                   endif
021600010604     C*
021700010604     C                   if        *in31 = *off and
021800010604     C                             *in32 = *off
021900010604     C                   eval      vinflg = '1'
022000010604     C                   else
022100010604     C                   eval      vinflg = '2'
022200010604     C                   endif
022300010604     C                   endif
022400010601     C*
022500010604     C                   endif
022600010604     C*
022700010330     C                   else
022800010330     C                   eval      vinflg = '1'
022900010330     C                   endif
023000010601     C*
023100010601     C  N70              update    tivin000
023200010330     C*
023300030822     C  N70              enddo
023400010601     C
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600010330     C                   if        §ctrok > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300040708     C                   if        %open(fnvaowwr)
026400040708     C                   close     fnvaowwr
026500010330     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        §ctrok > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200040723
028300040723
028400040723     C*------------------------------------------------------------------------*
028500040723     C* CHKCOR - Considerazioni su VAOCOR
028600040723     C*------------------------------------------------------------------------*
028700040723     C     CHKCOR        BEGSR
028800040723     C*
028900040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
029000040723     C                   if        vaoTCO = 'F'
029100040723     C* Quindi verifico se il codice ordinante non è valorizzato
029200040723     C                   if        vaoCOR = *zeros  AND
029300040723     C                             vaoRSO = *blanks AND
029400040723     C                             vaoINO = *blanks AND
029500040723     C                             vaoCAO = *blanks AND
029600040723     C                             vaoLOO = *blanks AND
029700040723     C                             vaoPRO = *blanks AND
029800040723     C                             vaoNAO = *blanks
029900040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
030000040723     C                   movel     '0'           wFlgAcr           1
030100040723     C                   move      vlrKSC        wCli              7 0
030200040723     C                   move(p)   wCli          acrCRO
030300040723     C     acrCRO        setll     fnacr01l
030400040723     C                   if        %found(fnacr01l)
030500040723     C                   read      fnacr01l
030600040723     C                   dow       not %eof(fnacr01l)
030700040723     C                   movel     acrCRO        wCliAcr           7 0
030800040723     C                   if        wCliAcr = wCli
030900040723     C                   eval      vaoCOR = acrCRO
031000040723     C                   movel     '1'           wFlgAcr
031100040723     C                   leave
031200040723     C                   else
031300040723     C                   leave
031400040723     C                   endif
031500040723     C                   read      fnacr01l
031600040723     C                   enddo
031700040723     C                   endif
031800040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
031900040723     C* dati anagrafici dell'ordinante dal piano dei conti
032000040723     C                   if        wFlgAcr = '0'
032100040723     C                   clear                   BS69DS
032200040723     C                   clear                   ACODS
032300040723     C                   clear                   INDDS
032400040723     C                   clear                   CLPDS
032500040723     C                   clear                   CLSDS
032600040723     C                   move(P)   vlrKSC        I69KAC
032700040723     C                   move(P)   vlrKSC        I69KIN
032800040723     C                   call      'TIBS69R'
032900040723     C                   parm                    BS69DS
033000040723     C                   parm                    ACODS
033100040723     C                   parm                    INDDS
033200040723     C                   parm                    CLPDS
033300040723     C                   parm                    CLSDS
033400040723     C     O69ERR        ifne      '1'
033500040723     C                   eval      vaoRSO = ACORAG
033600040723     C                   eval      vaoINO = INDVIA
033700040723     C                   movel(P)  INDCAP        vaoCAO
033800040723     C                   eval      vaoLOO = INDCIT
033900040723     C                   eval      vaoPRO = INDPRV
034000040723     C                   eval      vaoNAO = INDSTA
034100040723     C                   endif
034200040723     C                   endif
034300040723     C                   endif
034400040723     C                   endif
034500040723     C*
034600040723     C                   ENDSR
034700040723
034800040723
034900040723     C*------------------------------------------------------------------------*
035000040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
035100040723     C*------------------------------------------------------------------------*
035200040723     C     WRIORI        BEGSR
035300040723     C*
035400090618     C* Come prima cosa stacco un numeratore da AZNUM
035500090618     C                   clear                   trul33ds
035600090618     C                   eval      I33OPE = *zeros
035700090618     C                   eval      I33CNU = 600
035800090618     C                   eval      I33NUM = 1
035900090618     C                   movel     TRUL33DS      KPJBU
036000090618     C                   call      'TRUL33R'
036100090618     C                   parm                    KPJBA
036200090618     C                   movel     KPJBU         TRUL33DS
036300090618     C                   if        O33ERR = *zeros
036400090618     C                   z-add     O33NRF        §numori
036500090618     c                   else
036600090618     c                   eval      §numori = *all'9'
036700090618     C                   endif
036800040723     C*
036900040723     C* Quindi imposto i campi "extra-VAO"
037000040723     C                   if        vlrKSC = '0ORM0000'
037100040723     C*
037200040723     C                   if        vaoCOR > *zeros
037300040723     C                   movel     vaoCOR        wrkCOR7           7 0
037400040723     C*
037500040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
037600040723     C* codice padre. Se è un padre assegno già il codice ordinante.
037700040723     C                   clear                   tibs10ds
037800040723     C                   eval      d10drf = datcor                              *data riferimento
037900040723     C                   eval      d10paf = 'P'                                 *cerca il padre
038000040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
038100091026     C*
038200091026     C* Preparazione chiamata al TIBS10R1
038300091026     C                   clear                   tibs10r1ds
038400091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
038500091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
038600091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
038700091026     C*
038800091026     C                   call      'TIBS10R1'
038900091026     C                   parm                    tibs10r1ds
039000091026     C*
039100091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
039200040723     C*
039300040723     C                   if        d10err <> *blanks or                         *No unif. o padre
039400040723     C                             d10cop =  *zeros  or
039500040723     C                             d10cop =  d10cod
039600040723     C                   move      d10cod        oriIDC
039700040723     C                   else                                                   *Figlio
039800040723     C                   move      d10cop        oriIDC
039900040723     C                   endif
040000040723     C*
040100040723     C                   else
040200040723     C                   eval      oriIDC = *zeros
040300040723     C                   endif
040400040723     C*
040500040723     C                   else
040600040723     C                   eval      oriIDC = vlrKSC
040700040723     C                   endif
040800040723     C*
040900040723     C                   eval      oriPRG = §numori
041000040723     C                   eval      oriDER = datcor
041100040723     C                   eval      oriOER = wn6
041200040723     C                   eval      oriPOE = vaoPOE
041300040723     C                   eval      oriNSR = vaoNSR
041400040723     C                   eval      oriNOR = vaoNOR
041500040723     C                   eval      oriNRV = vaoNRV
041600040723     C                   eval      oriFLO = *blanks
041700040723     C*
041800040723     C* Infine valorizzo la chiave esterna sul file FNVAO
041900040723     C                   eval      dorm01 = vaoFLO
042000040723     C                   movel     §numori       §ormpg
042100050412     C*
042200050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
042300130326     C*                   in questo caso non ci sono default
042400050412     C*
042500130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
042600040723     C                   eval      vaoFLO = dorm01
042700130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
042800130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
042900130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
043000040723     C*
043100040723     C* ...quindi scrivo il file TIORI00F
043200120905     C  N55              WRITE     TIORI000
043300040723     C*
043400040723     C                   ENDSR
043500050826
043600050826
043700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
043800050826     C     wriVGD        BEGSR
043900050826     C*
044000050826     C* Reperisco la descrizione della fase da tabella
044100050826     C                   movel(p)  'FAR'         KteCOD
044200050826     C                   movel(p)  '000'         KteKE1
044300050826     C     KEYtbe        chain     tntbe01l
044400050826     C                   if        %found(tntbe01l)
044500050826     C                   movel(P)  tbeuni        DFAR
044600050826     C                   else
044700050826     C                   clear                   DFAR
044800050826     C                   endif
044900050826     C*
045000050826     C* Valorizzo buffer record
045100050826     C                   clear                   FNVAPDS
045200050826     C                   eval      vapIDC = oriIDC
045300050826     C                   eval      vapPOE = oriPOE
045400050826     C                   eval      vapRFA = vaoRFA
045500050826     C                   eval      vapPOG = oriPOE
045600050826     C                   eval      vapDAE = oriDER
045700050826     C                   eval      vapORE = oriOER
045800050826     C                   if        d§fardva <> *blanks
045900050826     C                   eval      vapDFA = d§fardva
046000050826     C                   else
046100050826     C                   eval      vapDFA = d§fardes
046200050826     C                   endif
046300050826     C*
046400050826     C                   clear                   tivgd000
046500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
046600050826     C                   eval      vgdTIP = 'VP'
046700050826     C                   eval      vgdKSU = vapIDC
046800050826     C                   eval      vgdTSC = 'WW'
046900050826     C                   eval      vgdDAT = datcor
047000120123     C                   eval      vgdPGM = 'TITZ10R'
047100060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
047200060306     C                   exsr      sr_Control
047300060306     C                   if        Cli_Abil = *on
047400060306     C                   eval      vgdKSU = woriIDC
047500120905     C  N55              write     tivgd000
047600060306     C                   endif
047700050826     C*
047800050826     C                   ENDSR
047900050826     C*------------------------------------------------------------------------*
048000010601
048100010601
048200010330     C*----------------------------------------------------*
048300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048400010330     C*----------------------------------------------------*
048500010330     C     INZVAR        BEGSR
048600010330     C*
048700030822     C* Inizializzo variabili di wrk
048800020204     C                   Z-ADD     *zeros        Num5_0            5 0
048900120124
049000030822     C*
049100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049200120123     C                   CLEAR                   FNVAO000
049300120123     C                   CLEAR                   TIORI000
049400030822     C                   CLEAR                   SkSplitCSV
049500030822     C                   EVAL      i = 1
049600030822     C                   EVAL      posDa = *zeros
049700030822     C                   EVAL      posA  = *zeros
049800030822     C*
049900030822     C* Reimposto i valori di default
050000030822     C                   EXSR      DEFCAM
050100030822     C*
050200010330     C                   ENDSR
050300010330     C*----------------------------------------------------*
050400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050500010330     C*----------------------------------------------------*
050600010330     C     DEFCAM        BEGSR
050700030822     C*
050800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050900030715     C* e delimitatore testo.
051000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051300040723     C*
051400080707     C* Imposto i valori d default d alcuni campi
051500080708     C*
051600080708     C                   CLEAR                   DORM01
051700120822     C                   EVAL      VAOFLO = DORM01
051800080707     C*
051900130222     C                   EVAL      VAOPOE = 005
052000130222     C                   EVAL      VAOTOR = 'S'
052100130222     C                   EVAL      VAOCOR = 0055041000
052200130226     C                   EVAL      VAOCRC = 0055041000
052300130222     C                   EVAL      VAOKSC = 0055041
052400130222     C                   EVAL      VAOPAG = 'O'
052500130222     C                   EVAL      VAOCTR = '001'
052600130222     C                   EVAL      VAOORR = 1400
052700120905     C* se il 1° char dei parametri è T=Test
052800120905     C                   SETOFF                                       55
052900120905     C                   IF        %subst(vlrppt:1:1) = 'T'
053000120905     C* inibisco la scrittura dei file TIORI e TIVGD
053100120905     C                   SETON                                        55
053200120905     C* imposto il PO di emissione come filiale fittizia
053300120905     C                   EVAL      VAOPOE = 999
053400120905     C                   ENDIF
053500091127     C*
053600091204     C* IMPOSTO I CAMPI PERSONALIZZATI
053700100416     C
053800030822     C*
053900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054000030822     C                   EVAL      posDaDft = 1
054100030822     C                   EVAL      posADft  = 0
054200030822     C                   EVAL      wGiroDft = 0
054300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054400030822     C                             posDaDft > 0
054500030822     C*
054600030822     C* Gestisco il 1° giro
054700030822     C                   IF        wGiroDft = 0
054800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055000030822     C* Incremento il contatore dei "giri"
055100030822     C                   EVAL      wGiroDft = 1
055200030822     C                   ELSE
055300030822     C                   EVAL      posDaDft = posADft
055400030822     C                   ENDIF
055500030822     C* Eseguo lo scan x trovare la fine del campo corrente
055600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055700030822     C*
055800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056000030822     C                   IF        posDaDft > 0 AND
056100030822     C                             posADft  > 0
056200040708     C* POE
056300040708     C                   IF        %subst(
056400040708     C                             %subst(vlrppt:posDaDft+1:
056500040708     C                             posADft-posDaDft-1):1:3)
056600040708     C                             = 'POE'
056700040708     C                   EVAL      PiStr=%trim(%subst(
056800040708     C                             %subst(vlrppt:posDaDft+1:
056900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000040708     C                   EXSR      CHKNUM
057100040708     C                   IF        PiInt=*on
057200040708     C                   Z-ADD     PiVal         VAOPOE
057300040708     C                   ENDIF
057400040708     C                   ENDIF
057500040708     C* TOR
057600040708     C                   IF        %subst(
057700040708     C                             %subst(vlrppt:posDaDft+1:
057800040708     C                             posADft-posDaDft-1):1:3)
057900040708     C                             = 'TOR'
058000040708     C                   EVAL      VAOTOR=%trim(%subst(
058100040708     C                             %subst(vlrppt:posDaDft+1:
058200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300040708     C                   ENDIF
058400040708     C* TCO
058500040708     C                   IF        %subst(
058600040708     C                             %subst(vlrppt:posDaDft+1:
058700040708     C                             posADft-posDaDft-1):1:3)
058800040708     C                             = 'TCO'
058900040708     C                   EVAL      VAOTCO=%trim(%subst(
059000040708     C                             %subst(vlrppt:posDaDft+1:
059100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200040708     C                   ENDIF
059300040707     C* COR
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700040707     C                             = 'COR'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300040708     C                   Z-ADD     PiVal         VAOCOR
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600040708     C* CRC
060700040708     C                   IF        %subst(
060800040708     C                             %subst(vlrppt:posDaDft+1:
060900040708     C                             posADft-posDaDft-1):1:3)
061000040708     C                             = 'CRC'
061100040708     C                   EVAL      PiStr=%trim(%subst(
061200040708     C                             %subst(vlrppt:posDaDft+1:
061300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400040708     C                   EXSR      CHKNUM
061500040708     C                   IF        PiInt=*on
061600040708     C                   Z-ADD     PiVal         VAOCRC
061700040708     C                   ENDIF
061800040708     C                   ENDIF
061900040708     C* FFD
062000040708     C                   IF        %subst(
062100040708     C                             %subst(vlrppt:posDaDft+1:
062200040708     C                             posADft-posDaDft-1):1:3)
062300040708     C                             = 'FFD'
062400040708     C                   EVAL      VAOFFD=%trim(%subst(
062500040708     C                             %subst(vlrppt:posDaDft+1:
062600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700040708     C                   ENDIF
062800040708     C* ORR
062900040708     C                   IF        %subst(
063000040708     C                             %subst(vlrppt:posDaDft+1:
063100040708     C                             posADft-posDaDft-1):1:3)
063200040708     C                             = 'ORR'
063300040708     C                   EVAL      PiStr=%trim(%subst(
063400040708     C                             %subst(vlrppt:posDaDft+1:
063500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600040708     C                   EXSR      CHKNUM
063700040708     C                   IF        PiInt=*on
063800040708     C                   Z-ADD     PiVal         VAOORR
063900040708     C                   ENDIF
064000040708     C                   ENDIF
064100040712     C* NCL
064200040712     C                   IF        %subst(
064300040712     C                             %subst(vlrppt:posDaDft+1:
064400040712     C                             posADft-posDaDft-1):1:3)
064500040712     C                             = 'NCL'
064600040712     C                   EVAL      PiStr=%trim(%subst(
064700040712     C                             %subst(vlrppt:posDaDft+1:
064800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900040712     C                   EXSR      CHKNUM
065000040712     C                   IF        PiInt=*on
065100040712     C                   Z-ADD     PiVal         VAONCL
065200040712     C                   ENDIF
065300040712     C                   ENDIF
065400040712     C* PKG
065500040712     C                   IF        %subst(
065600040712     C                             %subst(vlrppt:posDaDft+1:
065700040712     C                             posADft-posDaDft-1):1:3)
065800040712     C                             = 'PKG'
065900040712     C                   EVAL      PiStr=%trim(%subst(
066000040712     C                             %subst(vlrppt:posDaDft+1:
066100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200040712     C                   EXSR      CHKNUM
066300040712     C                   IF        PiNum=*on
066400040712     C                   Z-ADD     PiVal         VAOPKG
066500040712     C                   ENDIF
066600040712     C                   ENDIF
066700040712     C* VLM
066800040712     C                   IF        %subst(
066900040712     C                             %subst(vlrppt:posDaDft+1:
067000040712     C                             posADft-posDaDft-1):1:3)
067100040712     C                             = 'VLM'
067200040712     C                   EVAL      PiStr=%trim(%subst(
067300040712     C                             %subst(vlrppt:posDaDft+1:
067400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500040712     C                   EXSR      CHKNUM
067600040712     C                   IF        PiNum=*on
067700040712     C                   Z-ADD     PiVal         VAOVLM
067800040712     C                   ENDIF
067900040712     C                   ENDIF
068000040708     C* NAM
068100040708     C                   IF        %subst(
068200040708     C                             %subst(vlrppt:posDaDft+1:
068300040708     C                             posADft-posDaDft-1):1:3)
068400040708     C                             = 'NAM'
068500040708     C                   EVAL      VAONAM=%trim(%subst(
068600040708     C                             %subst(vlrppt:posDaDft+1:
068700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800040708     C                   ENDIF
068900040708     C* PAG
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300040708     C                             = 'PAG'
069400040708     C                   EVAL      VAOPAG=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800040708     C* KSC
069900040708     C                   IF        %subst(
070000040708     C                             %subst(vlrppt:posDaDft+1:
070100040708     C                             posADft-posDaDft-1):1:3)
070200040708     C                             = 'KSC'
070300040708     C                   EVAL      PiStr=%trim(%subst(
070400040708     C                             %subst(vlrppt:posDaDft+1:
070500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600040708     C                   EXSR      CHKNUM
070700040708     C                   IF        PiInt=*on
070800040708     C                   Z-ADD     PiVal         VAOKSC
070900040708     C                   ENDIF
071000040708     C                   ENDIF
071100040708     C* CTR
071200040708     C                   IF        %subst(
071300040708     C                             %subst(vlrppt:posDaDft+1:
071400040708     C                             posADft-posDaDft-1):1:3)
071500040708     C                             = 'CTR'
071600040708     C                   EVAL      VAOCTR=%trim(%subst(
071700040708     C                             %subst(vlrppt:posDaDft+1:
071800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900040708     C                   ENDIF
072000040708     C* POC
072100040708     C                   IF        %subst(
072200040708     C                             %subst(vlrppt:posDaDft+1:
072300040708     C                             posADft-posDaDft-1):1:3)
072400040708     C                             = 'POC'
072500040708     C                   EVAL      PiStr=%trim(%subst(
072600040708     C                             %subst(vlrppt:posDaDft+1:
072700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800040708     C                   EXSR      CHKNUM
072900040708     C                   IF        PiInt=*on
073000040708     C                   Z-ADD     PiVal         VAOPOC
073100040708     C                   ENDIF
073200040708     C                   ENDIF
073300040708     C* NO1
073400040708     C                   IF        %subst(
073500040708     C                             %subst(vlrppt:posDaDft+1:
073600121129     C                             posADft-posDaDft-1):1:3)
073700040708     C                             = 'NO1'
073800040708     C                   EVAL      VAONO1=%trim(%subst(
073900040708     C                             %subst(vlrppt:posDaDft+1:
074000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100040708     C                   ENDIF
074200040708     C* DDT
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600040708     C                             = 'DDT'
074700040708     C                   EVAL      VAODDT=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100040708     C* RFA
075200040708     C                   IF        %subst(
075300040708     C                             %subst(vlrppt:posDaDft+1:
075400040708     C                             posADft-posDaDft-1):1:3)
075500040708     C                             = 'RFA'
075600040708     C                   EVAL      VAORFA=%trim(%subst(
075700040708     C                             %subst(vlrppt:posDaDft+1:
075800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900040708     C                   ENDIF
076000040708     C* FLO
076100040708     C                   IF        %subst(
076200040708     C                             %subst(vlrppt:posDaDft+1:
076300040708     C                             posADft-posDaDft-1):1:3)
076400040708     C                             = 'FLO'
076500040708     C                   EVAL      VAOFLO=%trim(%subst(
076600040708     C                             %subst(vlrppt:posDaDft+1:
076700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800091103     C*
076900040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
077000080702     C* e posizione 7 del VAOFLO
077100040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
077200040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
077300050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
077400040708     C                   ENDIF
077500030822     C* ...
077600030822     C                   ENDIF
077700030822     C                   ENDDO
077800020204     C*
077900010330     C                   ENDSR
078000010607     C*----------------------------------------------------*
078100040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
078200010607     C*----------------------------------------------------*
078300040708     C     IMPVAO        BEGSR
078400010607     C*
078500010607     C                   SETOFF                                       3132
078600120124
078700030515     C*
078800040708     C                   MOVEL     datcor        VAODAO
078900040708     C                   MOVEL     wn14          VAOOAO
079000050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079100050314     C     minu:maiu     XLATE     vindta        vindta
079200010607     C*
079300030715     C**********
079400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079500030715     C**********
079600030715     C                   IF        wGiro = *zeros
079700040709     C*
079800040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
079900040709     C                   EVAL      depvinDTA = vinDTA
080000040709     C*
080100040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
080200040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080300040709     C                                      %trim(intcol(3))
080400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080500050314     C**!!!minu:maiu     XLATE     vindta        vindta
080600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080700040324     C                   DOW       posDa <= %len(%trimr(vindta))
080800030715     C*
080900030715     C* Gestisco il 1° campo
081000030715     C                   IF        i = 1
081100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081200030715     C                   EVAL      posDa = 1
081300030715     C* Eseguo lo scan x trovare la fine del primo campo
081400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C* X i campi successivi al 1°
081900030715     C                   ELSE
082000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082100030715     C                   EVAL      posDa = posA + 1
082200030715     C* Eseguo lo scan x trovare la fine del campo corrente
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* Gestisco l'ultimo campo
082500030715     C                   IF        posA = *zeros
082600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   LEAVE
083100030715     C                   ELSE
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   ENDIF
083600030715     C                   ENDIF
083700030715     C* Incremento il contatore d campo
083800030715     C                   EVAL      i = i +1
083900030715     C                   ENDDO
084000030715     C*
084100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084200030715     C* delimitazione testo
084300030715     C                   EVAL      i = 1
084400030715     C                   DOW       i < %elem(SkSplitFLD)
084500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084700030715     C                   EVAL      i = i + 1
084800030715     C                   ENDDO
084900030715     C                   EVAL      wGiro = 1
085000040709     C* Ripristino la 1° riga originale
085100040709     C                   EVAL      vinDTA = depvinDTA
085200030715     C                   ELSE
085300030715     C                   EVAL      wGiro = 2
085400031204     C**********
085500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085600031204     C* d separatore campo
085700031204     C**********
085800040709     C                   EVAL      vindta = %trim(vindta)
085900040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086000040709     C                   EVAL      vindta = %subst(vindta:2)
086100040709     C                   ENDDO
086200040709     C*
086300040709     C                   Z-ADD     *zeros        lunghInput        4 0
086400040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086500040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086600040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086700040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086800040709     C                   ENDDO
086900030715     C**********
087000030715     C* Effettuo lo split del campo dati d input
087100030715     C**********
087200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087300040324     C                   DOW       posDa <= %len(%trimr(vindta))
087400030715     C*
087500030715     C* Gestisco il 1° campo
087600030715     C                   IF        i = 1
087700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087800030715     C                   EVAL      posDa = 1
087900030715     C* Eseguo lo scan x trovare la fine del primo campo
088000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088100030715     C* A questo punto "estraggo" il campo corrente
088200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088300030715     C                                                    (posA-posDa))
088400030715     C* X i campi successivi al 1°
088500030715     C                   ELSE
088600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088700030715     C                   EVAL      posDa = posA + 1
088800031204     C* Verifico che nn vi sia il campo nn valorizzato
088900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089000031204     C* Se campo nn valorizzato skippo al prossimo
089100031204     C                   EVAL      SkSplitCSV(i) = *blanks
089200031204     C                   EVAL      posA  = posA + 1
089300031204     C                   ELSE
089400030715     C* Eseguo lo scan x trovare la fine del campo corrente
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* Gestisco l'ultimo campo
089700030715     C                   IF        posA = *zeros
089800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089900030715     C* A questo punto "estraggo" il campo corrente
090000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090100030715     C                                                    (posA-posDa))
090200030715     C                   LEAVE
090300030715     C                   ELSE
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C                   ENDIF
090800031204     C                   ENDIF
090900030715     C                   ENDIF
091000030715     C* Incremento il contatore d campo
091100030715     C                   EVAL      i = i +1
091200030715     C                   ENDDO
091300030715     C*
091400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091500030715     C* delimitazione testo
091600030715     C                   EVAL      i = 1
091700030715     C                   DOW       i < %elem(SkSplitCSV)
091800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092000030715     C                   EVAL      i = i + 1
092100030715     C                   ENDDO
092200030715     C*
092300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitFLD)
092600030715     C*
092700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092900030715     C                             SkSplitCSV(i) <> *blanks
093000030721     C*** ATB
093100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
093200040708     C                   EVAL      VAOATB = SkSplitCSV(i)
093300030721     C                   ENDIF
093400040708     C*** TOR
093500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
093600040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
093700040708     C                   ENDIF
093800040708     C*** TCO
093900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
094000040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
094100040708     C                   ENDIF
094200040708     C*** RSO
094300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
094400040708     C                   EVAL      VAORSO = SkSplitCSV(i)
094500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094600040708     C     '@':'A'       XLATE     VAORSO        VAORSO
094700030715     C* ==
094800030715     C                   ENDIF
094900040708     C*** INO
095000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
095100040708     C                   EVAL      VAOINO = SkSplitCSV(i)
095200030715     C                   ENDIF
095300040708     C*** LOO
095400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
095500040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
095600030715     C                   ENDIF
095700040708     C*** PRO
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
095900040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
096000030715     C                   ENDIF
096100040708     C*** NAO
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
096300040708     C                   EVAL      VAONAO = SkSplitCSV(i)
096400030721     C                   ENDIF
096500040708     C*** RSR
096600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
096700040708     C                   EVAL      VAORSR = SkSplitCSV(i)
096800040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096900040708     C     '@':'A'       XLATE     VAORSR        VAORSR
097000040708     C* ==
097100040708     C                   IF        VAORSR = *blanks
097200040708     C                   SETON                                        31
097300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097400040708     C                             + ' ' + 'VAORSR'
097500040708     C                   ENDIF
097600040708     C                   ENDIF
097700040708     C*** INR
097800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
097900040708     C                   EVAL      VAOINR = SkSplitCSV(i)
098000040708     C                   IF        VAOINR = *blanks
098100040708     C                   SETON                                        31
098200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098300040708     C                             + ' ' + 'VAOINR'
098400040708     C                   ENDIF
098500040708     C                   ENDIF
098600040708     C*** LOR
098700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
098800040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
098900040708     C                   IF        VAOLOR = *blanks
099000040708     C                   SETON                                        31
099100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099200040708     C                             + ' ' + 'VAOLOR'
099300040708     C                   ENDIF
099400040708     C                   ENDIF
099500040708     C*** PRR
099600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
099700040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
099800040708     C                   IF        VAOPRR = *blanks
099900040708     C                   SETON                                        31
100000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100100040708     C                             + ' ' + 'VAOPRR'
100200040708     C                   ENDIF
100300040708     C                   ENDIF
100400040708     C*** NAR
100500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
100600040708     C                   EVAL      VAONAR = SkSplitCSV(i)
100700040708     C                   ENDIF
100800040708     C*** RER
100900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
101000040708     C                   EVAL      VAORER = SkSplitCSV(i)
101100040708     C                   ENDIF
101200040708     C*** TER
101300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
101400040708     C                   EVAL      VAOTER = SkSplitCSV(i)
101500040708     C                   ENDIF
101600040708     C*** RSC
101700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
101800040708     C                   EVAL      VAORSC = SkSplitCSV(i)
101900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102000040708     C     '@':'A'       XLATE     VAORSC        VAORSC
102100040708     C* ==
102200040708     C                   ENDIF
102300040708     C*** INC
102400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
102500040708     C                   EVAL      VAOINC = SkSplitCSV(i)
102600040708     C                   ENDIF
102700040708     C*** LOC
102800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
102900040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
103000040708     C                   ENDIF
103100040708     C*** PRC
103200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
103300040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
103400040708     C                   ENDIF
103500040708     C*** NAC
103600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
103700040708     C                   EVAL      VAONAC = SkSplitCSV(i)
103800040708     C                   ENDIF
103900040708     C*** FFD
104000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
104100040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
104200040708     C                   ENDIF
104300040708     C*** RMP
104400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
104500040708     C                   EVAL      VAORMP = SkSplitCSV(i)
104600040708     C                   ENDIF
104700040708     C*** NAM
104800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
104900040708     C                   EVAL      VAONAM = SkSplitCSV(i)
105000040708     C                   ENDIF
105100040708     C*** PAG
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
105300040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
105400040708     C                   ENDIF
105500040708     C*** CTR
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
105700040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
105800040708     C                   ENDIF
105900040708     C*** ZOR
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
106100040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
106200040708     C                   ENDIF
106300040708     C*** NO1
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
106500040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
106600030715     C                   ENDIF
106700040708     C*** NO2
106800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
106900040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
107000030715     C                   ENDIF
107100040708     C*** DDT
107200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
107300040708     C                   EVAL      VAODDT = SkSplitCSV(i)
107400030715     C                   ENDIF
107500040708     C*** RFA
107600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
107700040708     C                   EVAL      VAORFA = SkSplitCSV(i)
107800030715     C                   ENDIF
107900040708     C*** SPI
108000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
108100040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
108200030715     C                   ENDIF
108300040708     C*** FLO
108400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
108500040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
108600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
108700080708     C* e la posizione 7 del VAOFLO
108800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
108900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
109000050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
109100030715     C                   ENDIF
109200091103     C
109300091103     C*** DAR
109400050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
109500050412     C                   EVAL      PiStr=SkSplitCSV(i)
109600050412     C                   EXSR      CHKNUM
109700050412     C                   IF        PiInt=*on
109800050412     C                   Z-ADD     PiVal         VAODAR
109900050412     C                   ELSE
110000050412     C                   SETON                                        32
110100050412     C                   EVAL      VAODAR = *zeros
110200050412     C                   EVAL      vinmsg = %trimr(vinmsg)
110300050412     C                             + ' ' + 'VAODAR'
110400050412     C                   ENDIF
110500050314     C                   ENDIF
110600030715     C
110700030721     C*
110800030721     C* Reperisco quindi i campi numerici...
110900040708     C*** POE
111000040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
111100040723     C                   EVAL      PiStr=SkSplitCSV(i)
111200040723     C                   EXSR      CHKNUM
111300040723     C                   IF        PiInt=*on    AND
111400040723     C                             PiVal<=999   AND
111500040723     C                             PiVal>*zeros
111600040723     C                   Z-ADD     PiVal         VAOPOE
111700040723     C                   ELSE
111800040723     C                   SETON                                        32
111900040723     C                   EVAL      VAOPOE = *zeros
112000040723     C                   EVAL      vinmsg = %trimr(vinmsg)
112100040723     C                             + ' ' + 'VAOPOE'
112200040723     C                   ENDIF
112300040723     C                   ENDIF
112400040708     C*** COR
112500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
112600040708     C                   EVAL      PiStr=SkSplitCSV(i)
112700040708     C                   EXSR      CHKNUM
112800040708     C                   IF        PiInt=*on
112900040708     C                   Z-ADD     PiVal         VAOCOR
113000040708     C                   ELSE
113100040708     C                   SETON                                        31
113200040708     C                   EVAL      VAOCOR = *zeros
113300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040708     C                             + ' ' + 'VAOCOR'
113500040708     C                   ENDIF
113600040708     C                   ENDIF
113700040708     C*** CAO
113800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiInt=*on
114200030721     C                   Z-ADD     PiVal         Num5_0
114300040708     C                   MOVEL(p)  Num5_0        VAOCAO
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600040708     C                   EVAL      VAOCAO = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800040708     C                             + ' ' + 'VAOCAO'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100040708     C*** CRA
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on
115600040708     C                   Z-ADD     PiVal         VAOCRA
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900040708     C                   EVAL      VAOCRA = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040708     C                             + ' ' + 'VAOCRA'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400040708     C*** CAR
116500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
116600040708     C                   EVAL      PiStr=SkSplitCSV(i)
116700040708     C                   EXSR      CHKNUM
116800040708     C                   IF        PiInt=*on
116900040708     C                   Z-ADD     PiVal         Num5_0
117000040708     C                   MOVEL(p)  Num5_0        VAOCAR
117100040708     C                   ELSE
117200040708     C                   SETON                                        31
117300040708     C                   EVAL      VAOCAR = *zeros
117400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117500040708     C                             + ' ' + 'VAOCAR'
117600040708     C                   ENDIF
117700040708     C                   ENDIF
117800040708     C*** CRC
117900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
118000040708     C                   EVAL      PiStr=SkSplitCSV(i)
118100040708     C                   EXSR      CHKNUM
118200040708     C                   IF        PiInt=*on
118300040708     C                   Z-ADD     PiVal         VAOCRC
118400040708     C                   ELSE
118500040708     C                   SETON                                        32
118600040708     C                   EVAL      VAOCRC = *zeros
118700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118800040708     C                             + ' ' + 'VAOCRC'
118900040708     C                   ENDIF
119000040708     C                   ENDIF
119100040708     C*** CAC
119200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
119300040708     C                   EVAL      PiStr=SkSplitCSV(i)
119400040708     C                   EXSR      CHKNUM
119500040708     C                   IF        PiInt=*on
119600040708     C                   Z-ADD     PiVal         Num5_0
119700040708     C                   MOVEL(p)  Num5_0        VAOCAC
119800040708     C                   ELSE
119900040708     C                   SETON                                        32
120000040708     C                   EVAL      VAOCAC = *zeros
120100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040708     C                             + ' ' + 'VAOCAC'
120300040708     C                   ENDIF
120400040708     C                   ENDIF
120500040708     C*** DAR
120600090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
120700090428     C                   EVAL      PiStr=SkSplitCSV(i)
120800090428     C                   EXSR      CHKNUM
120900090428     C                   IF        PiInt=*on
121000090428     C                   Z-ADD     PiVal         VAODAR
121100090428     C                   ELSE
121200090428     C                   SETON                                        32
121300090428     C                   EVAL      VAODAR = *zeros
121400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
121500090428     C                             + ' ' + 'VAODAR'
121600090428     C                   ENDIF
121700090428     C                   ENDIF
121800040708     C*** ORR
121900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
122000040708     C                   EVAL      PiStr=SkSplitCSV(i)
122100040708     C                   EXSR      CHKNUM
122200040708     C                   IF        PiInt=*on
122300040708     C                   Z-ADD     PiVal         VAOORR
122400040708     C                   ELSE
122500040708     C                   SETON                                        32
122600040708     C                   EVAL      VAOORR = *zeros
122700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122800040708     C                             + ' ' + 'VAOORR'
122900040708     C                   ENDIF
123000040708     C                   ENDIF
123100030721     C*** NCL
123200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on
123600040723     C                   IF        PiVal > *zeros
123700040708     C                   Z-ADD     PiVal         VAONCL
123800040723     C                   ENDIF
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100040708     C                   EVAL      VAONCL = *zeros
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300040708     C                             + ' ' + 'VAONCL'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600040708     C*** PKG
124700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiNum=*on
125100040723     C                   IF        PiVal > *zeros
125200040708     C                   Z-ADD     PiVal         VAOPKG
125300040723     C                   ENDIF
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600040708     C                   EVAL      VAOPKG = *zeros
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800040708     C                             + ' ' + 'VAOPKG'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100040708     C*** VLM
126200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
126300040708     C                   EVAL      PiStr=SkSplitCSV(i)
126400040708     C                   EXSR      CHKNUM
126500040708     C                   IF        PiNum=*on
126600040708     C                   Z-ADD     PiVal         VAOVLM
126700040708     C                   ELSE
126800040708     C                   SETON                                        32
126900040708     C                   EVAL      VAOVLM = *zeros
127000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127100040708     C                             + ' ' + 'VAOVLM'
127200040708     C                   ENDIF
127300040708     C                   ENDIF
127400040708     C*** BNC
127500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
127600040708     C                   EVAL      PiStr=SkSplitCSV(i)
127700040708     C                   EXSR      CHKNUM
127800040708     C                   IF        PiInt=*on
127900040708     C                   Z-ADD     PiVal         VAOBNC
128000040708     C                   ELSE
128100040708     C                   SETON                                        32
128200040708     C                   EVAL      VAOBNC = *zeros
128300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128400040708     C                             + ' ' + 'VAOBNC'
128500040708     C                   ENDIF
128600040708     C                   ENDIF
128700040708     C*** BLC
128800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
128900040708     C                   EVAL      PiStr=SkSplitCSV(i)
129000040708     C                   EXSR      CHKNUM
129100040708     C                   IF        PiInt=*on
129200040708     C                   Z-ADD     PiVal         VAOBLC
129300040708     C                   ELSE
129400040708     C                   SETON                                        32
129500040708     C                   EVAL      VAOBLC = *zeros
129600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129700040708     C                             + ' ' + 'VAOBLC'
129800040708     C                   ENDIF
129900040708     C                   ENDIF
130000040708     C*** ATT
130100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
130200040708     C                   EVAL      PiStr=SkSplitCSV(i)
130300040708     C                   EXSR      CHKNUM
130400040708     C                   IF        PiInt=*on
130500040708     C                   Z-ADD     PiVal         VAOATT
130600040708     C                   ELSE
130700040708     C                   SETON                                        32
130800040708     C                   EVAL      VAOATT = *zeros
130900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131000040708     C                             + ' ' + 'VAOATT'
131100040708     C                   ENDIF
131200040708     C                   ENDIF
131300040708     C*** MTC
131400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
131500040708     C                   EVAL      PiStr=SkSplitCSV(i)
131600040708     C                   EXSR      CHKNUM
131700040708     C                   IF        PiInt=*on
131800040708     C                   Z-ADD     PiVal         VAOMTC
131900040708     C                   ELSE
132000040708     C                   SETON                                        32
132100040708     C                   EVAL      VAOMTC = *zeros
132200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132300040708     C                             + ' ' + 'VAOMTC'
132400040708     C                   ENDIF
132500040708     C                   ENDIF
132600040708     C*** KSC
132700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiInt=*on      AND
133100030721     C                             PiVal<=9999999 AND
133200030721     C                             PiVal>*zeros
133300040708     C                   Z-ADD     PiVal         VAOKSC
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600040708     C                   EVAL      VAOKSC = *zeros
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800040708     C                             + ' ' + 'VAOKSC'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100040708     C*** POR
134200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on    AND
134600030721     C                             PiVal<=999   AND
134700030721     C                             PiVal>*zeros
134800040708     C                   Z-ADD     PiVal         VAOPOR
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100040708     C                   EVAL      VAOPOR = *zeros
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300040708     C                             + ' ' + 'VAOPOR'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600040708     C*** POC
135700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
135800040708     C                   EVAL      PiStr=SkSplitCSV(i)
135900040708     C                   EXSR      CHKNUM
136000040708     C                   IF        PiInt=*on    AND
136100040708     C                             PiVal<=999   AND
136200040708     C                             PiVal>*zeros
136300040708     C                   Z-ADD     PiVal         VAOPOC
136400040708     C                   ELSE
136500040708     C                   SETON                                        32
136600040708     C                   EVAL      VAOPOC = *zeros
136700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136800040708     C                             + ' ' + 'VAOPOC'
136900040708     C                   ENDIF
137000040708     C                   ENDIF
137100040708     C*** STO
137200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
137300040708     C                   EVAL      PiStr=SkSplitCSV(i)
137400040708     C                   EXSR      CHKNUM
137500040708     C                   IF        PiInt=*on
137600040708     C                   Z-ADD     PiVal         VAOSTO
137700040708     C                   ELSE
137800040708     C                   SETON                                        32
137900040708     C                   EVAL      VAOSTO = *zeros
138000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138100040708     C                             + ' ' + 'VAOSTO'
138200040708     C                   ENDIF
138300040708     C                   ENDIF
138400090428     C*
138500091103     C********** GESTIONE CAMPI PERSONALIZZATI
138600120120
138700121207
138800090428     C*
138900090428     C****************************************
139000090428     C*
139100030715     C*
139200030715     C                   ENDIF
139300030715     C                   EVAL      i = i + 1
139400030715     C                   ENDDO
139500040723     C*
139600121207     C***  ===>  Gestione campi "particolari" fuori ciclo
139700121207     C*
139800030715     C                   ENDIF
139900020204     C*
140000010607     C                   ENDSR
140100010607     C*----------------------------------------------------*
140200080702
140300080702
140400080702     C*----------------------------------------------------*
140500080702     C*  CONTROLLO NUMERICITA' CAMPI
140600080702     C*----------------------------------------------------*
140700080702     C     CHKNUM        BEGSR
140800080702     C*
140900080702     C                   IF        PiDecChr = *blanks
141000080702     C                   EVAL      PiDecChr = ','
141100080702     C                   ENDIF
141200080702     C*
141300080702     C                   callp     UBISNUM_Check(PiStr
141400080702     C                                          :PiDecChr
141500080702     C                                          :PiVal
141600080702     C                                          :PiNum
141700080702     C                                          :PiInt)
141800080702     C*
141900080702     C                   ENDSR
142000080702     C***
142100080702
142200010601
142300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142400010601     C     repfil        BEGSR
142500010601     C*
142600010601     C                   if        invfil = *zeros and
142700010601     C                             depfil > *zeros and
142800010629     C                             (vinflg = *blanks or
142900010629     C                              vinflg = *zeros)
143000010601     C
143100010601     C                   eval      invfil = depfil
143200010601     C                   endif
143300010601     C*
143400010601     C                   if        depfil <> invfil and
143500010601     C                             invfil > *zeros
143600010601     C                   eval      flgMulti = '1'
143700010601     C                   if        vinflg = *blanks
143800010601     C                   add       1             cntNonEl
143900010601     C                   endif
144000010601     C                   endif
144100010601     C*
144200010601     C                   if        vinflg = '2'
144300010601     C                   eval      flgStato = '2'
144400010601     C                   endif
144500010601     C*
144600010601     C                   ENDSR
144700010601     C***
144800010330
144900010330
145000990920      /TITLE Invio dei dati al punto operativo.
145100000613     C     invio         BEGSR
145200990920     C*
145300990920     C                   reset                   dscmz
145400010601     C                   move      invfil        cmzdst
145500990920     C                   eval      cmzfld = vlrfou
145600990920     C                   eval      cmzmbd = vlrhdl
145700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
145800000710     C                   if        prmfir = *blanks
145900040708     C                   eval      cmzfla = 'FNVAO00F'
146000040708     C                   eval      cmzmba = 'FNVAO00F'
146100000710     C                   else
146200000710     C                   eval      cmzfla = prmfir
146300000710     C                   eval      cmzmba = prmfir
146400000710     C                   endif
146500990920     C                   eval      cmznrr = *zeros
146600990920     C                   move      §ctrok        cmznrr
146700021018     C                   eval      cmzlba = vlrfl1
146800990920     C                   call(e)   'TIS711C'
146900990920     C                   parm                    dscmz
147000990921     C                   parm      *blanks       esito
147100990920     C                   if        %error
147200990920     C                             or cmzerr = '1'
147300990921     C                             or esito  = '1'
147400000710     C                   eval      wrkesito = '3'
147500990920     C                   endif
147600990920     C*
147700000613     C                   ENDSR
147800010601
147900010601
148000010601      /TITLE Invio dei dati al punto operativo.
148100010601     C     opeini        BEGSR
148200010601     C*
148300010601     C* Inizializzo flag e contatori operativi
148400010601     C                   movel     '0'           flgGiro           1
148500010601     C                   movel     '0'           flgMulti          1
148600010601     C                   movel     '1'           flgStato          1
148700010615     C                   movel     '0'           flgOk             1
148800010601     C                   z-add     *zeros        cntNonEl         10 0
148900010601     C                   z-add     *zeros        depfil            3 0
149000010601     C                   z-add     *zeros        invfil            3 0
149100010601     C*
149200010601     C                   ENDSR
149300010601     C***
149400050421
149500050421
149600091130     C     *pssr         BEGSR
149700050421     C*
149800091130     C                   eval      wrkesito = '2'
149900050421     C*
150000091130     C                   ENDSR     '*CANCL'
150100050421     C***
150200060306
150300060306
150400060306     C*-------------------------------------------------------------------------*
150500060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
150600060306     C*-------------------------------------------------------------------------*
150700060306     C     sr_Control    begsr
150800060306     C*
150900060306     C* inizializzo a ok il flag d controllo abilitazione
151000060306     C                   eval      Cli_Abil = *on
151100060306     C
151200060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
151300060306     C                   clear                   woriIDC
151400060306     C                   eval      kteCOD = 'VAP'
151500060306     C                   eval      kteKE1 = oriIDC
151600060306     C     keytbe        chain     TNTBE01L
151700060306     C                   if        %found(Tntbe01l)
151800060306     C                   eval      woriIDC = TbeKe2
151900060306     C                   else
152000060306     C                   eval      Cli_Abil = *off
152100060306     C                   endif
152200060306     C*
152300060306     C                   endsr
152400010330
152500010330
152600000613     C     *inzsr        BEGSR
152700990910     C*
152800990910     C     *entry        plist
152900990920     C                   parm                    tivlrds
153000990921     C                   parm      wrkesito      esito
153100000724     C                   parm                    prmlit
153200000710     C                   parm                    prmfir
153300010330     C*
153400010330     C* CALCOLA LA DATA CORRENTE
153500091127     C                   z-add     *zeros        datcor            8 0
153600091127     C                   time                    wn14             14 0
153700091127     C                   movel     wn14          wn6               6 0          *ORA (6)
153800091127     C                   eval      datcor = %dec(%date() : *ISO)
153900040723     C*
154000040723     C* Campi riferito al database
154100040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
154200040723     C     *like         define    tbeke1        kteKE1
154300040723     C*
154400040723     C* Definizione chiavi di lettura
154500040723     C* TNTBE01L - parziale
154600040723     C     keytbe        klist
154700040723     C                   kfld                    kteCOD                         *UTENTE
154800040723     C                   kfld                    kteKE1                         *CODICE
154900000613     C*
155000000613     C                   ENDSR
155100000613     C***
155200990908
155300040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
155400130222VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAORER;VAOTER;VAONO1;VAONO2;VAONCL;VAOPKG;VAONAM;VAORFA
155500130222
155600130222
