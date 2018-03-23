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
004300130513     D Intestazioni    s               n   inz(*on)
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
007200130513     D wRSR1           s             35
007300130513     D wRSR2           s             35
007400130513     D wINR1           s             35
007500130513     D wINR2           s             35
007600130513     D w10             s             10
007700080702
007800080702     D*------------------
007900080702     D* LINKING A DEFINIZIONI ESTERNE
008000080702     D*------------------
008100090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008200090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008300091026
008400091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008500091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008600091026
008700080702
008800031201     D*------------------
008900031201     D* Costanti
009000031201     D*------------------
009100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009300010330
009400010330
009500010330
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100041011     C                   exsr      PREELA
010200040708     C                   exsr      rwvao
010300010601     C*
010400010601     C                   seton                                        lr
010500010601
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011100010601     C*--------------------------------------------------------
011200010601     C     PREELA        BEGSR
011300010601     C*
011400010601     C* SE OCCORRE SPEDIRE IN FILIALE
011500041011     C                   if        flgGiro = '0'
011600010601     C*
011700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011800010601     C                   eval      flgGiro = '1'
011900041011     C*
012000041011     C                   endif
012100010601     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012800010601     C*--------------------------------------------------------
012900010601     C     ENDELA        BEGSR
013000000616     C*
013100120209     C                   ENDSR
013200010601
013300010601
013400010330     C*--------------------------------------------------------
013500040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
013600010330     C*--------------------------------------------------------
013700040708     C     RWVAO         BEGSR
013800010330     C*
013900010330     C                   if        not %open(tivin00r)
014000010330     C                   open      tivin00r
014100010330     C                   endif
014200040708     C                   if        not %open(fnvaowwr)
014300040708     C                   open      fnvaowwr
014400010330     C                   endif
014500010330     C*
014600010604     C                   clear                   §CTROK
014700010604     C                   clear                   §CTRMO
014800010604     C                   clear                   §CTRNO
014900120123     C*
015000120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
015100120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
015200120905     C                   IF        Intestazioni = *off
015300120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
015400120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015500120123     C                   exsr      inzvar
015600120123     C                   exsr      impvao
015700120905     C                   ENDIF
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
019500010601     C*
019600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019700040708     C                   eval      depfil = VAOPOE
019800010601     C                   exsr      repfil
019900010601     C                   if        depfil = invfil
020000021025     C                   if        vlrpoi = 999
020100040708     C                   move(P)   invfil        VAOPOE
020200021025     C                   else
020300040708     C                   move(P)   vlrpoi        VAOPOE
020400021025     C                   endif
020500010601     C*
020600010604     C* Ebbene...
020700010604     C*
020800030822     C  N31              add       1             §CTROK            7 0
020900030822     C   32              add       1             §CTRMO            7 0
021000030822     C   31              add       1             §CTRNO            7 0
021100030822     C                   if        wGiro = 2
021200120123     C  N31              EXSR      CHKCOR
021300120123     C  N31              EXSR      WRIORI
021400120123     C  N31              EXSR      WRIVGD
021500120123     C  N31              write     FNVAO000
021600030822     C                   endif
021700010604     C*
021800010604     C                   if        *in31 = *off and
021900010604     C                             *in32 = *off
022000010604     C                   eval      vinflg = '1'
022100010604     C                   else
022200010604     C                   eval      vinflg = '2'
022300010604     C                   endif
022400010604     C                   endif
022500010601     C*
022600010604     C                   endif
022700010604     C*
022800010330     C                   else
022900010330     C                   eval      vinflg = '1'
023000010330     C                   endif
023100010601     C*
023200010601     C  N70              update    tivin000
023300010330     C*
023400030822     C  N70              enddo
023500010601     C
023600010601     C*
023700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023800010601     C                   if        cntNonEl = *zeros or
023900010601     C                             flgMulti = '0'
024000010330     C* Se non ci sono record con errori ...
024100010601     C                   if        §ctrno = 0 and
024200010604     C                             §ctrmo = 0 and
024300010601     C                             flgStato <> '2'
024400010330     C* ... restituisco esito OK.
024500010330     C                   eval      wrkesito = '0'
024600010330     C                   else
024700010330     C                   if        §ctrok > 0
024800010330     C                   eval      wrkesito = '1'
024900010330     C                   else
025000010615     C                   if        flgOk = '0'
025100010615     C                   eval      wrkesito = '2'
025200010615     C                   else
025300010615     C                   eval      wrkesito = '6'
025400010615     C                   endif
025500010330     C                   endif
025600010330     C                   endif
025700010601     C                   else
025800010601     C                   eval      wrkesito = '9'
025900010601     C                   endif
026000010330     C*
026100010330     C                   if        %open(tivin00r)
026200010330     C                   close     tivin00r
026300010330     C                   endif
026400040708     C                   if        %open(fnvaowwr)
026500040708     C                   close     fnvaowwr
026600010330     C                   endif
026700010601     C*
026800010601     C                   if        vlrpoi <> 999
026900010601     C                   eval      invfil = vlrpoi
027000010601     C                   endif
027100010330     C*
027200010330     C                   if        §ctrok > 0
027300010601     C                             and invfil > *zeros
027400010330     C                   exsr      invio
027500010330     C                   endif
027600010601     C*
027700010618     C                   if        flgGiro = '1'
027800010601     C                   exsr      endela
027900010618     C                   endif
028000010330     C*
028100010330     C                   ENDSR
028200010330     C***
028300040723
028400040723
028500040723     C*------------------------------------------------------------------------*
028600040723     C* CHKCOR - Considerazioni su VAOCOR
028700040723     C*------------------------------------------------------------------------*
028800040723     C     CHKCOR        BEGSR
028900040723     C*
029000040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
029100040723     C                   if        vaoTCO = 'F'
029200040723     C* Quindi verifico se il codice ordinante non è valorizzato
029300040723     C                   if        vaoCOR = *zeros  AND
029400040723     C                             vaoRSO = *blanks AND
029500040723     C                             vaoINO = *blanks AND
029600040723     C                             vaoCAO = *blanks AND
029700040723     C                             vaoLOO = *blanks AND
029800040723     C                             vaoPRO = *blanks AND
029900040723     C                             vaoNAO = *blanks
030000040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
030100040723     C                   movel     '0'           wFlgAcr           1
030200040723     C                   move      vlrKSC        wCli              7 0
030300040723     C                   move(p)   wCli          acrCRO
030400040723     C     acrCRO        setll     fnacr01l
030500040723     C                   if        %found(fnacr01l)
030600040723     C                   read      fnacr01l
030700040723     C                   dow       not %eof(fnacr01l)
030800040723     C                   movel     acrCRO        wCliAcr           7 0
030900040723     C                   if        wCliAcr = wCli
031000040723     C                   eval      vaoCOR = acrCRO
031100040723     C                   movel     '1'           wFlgAcr
031200040723     C                   leave
031300040723     C                   else
031400040723     C                   leave
031500040723     C                   endif
031600040723     C                   read      fnacr01l
031700040723     C                   enddo
031800040723     C                   endif
031900040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
032000040723     C* dati anagrafici dell'ordinante dal piano dei conti
032100040723     C                   if        wFlgAcr = '0'
032200040723     C                   clear                   BS69DS
032300040723     C                   clear                   ACODS
032400040723     C                   clear                   INDDS
032500040723     C                   clear                   CLPDS
032600040723     C                   clear                   CLSDS
032700040723     C                   move(P)   vlrKSC        I69KAC
032800040723     C                   move(P)   vlrKSC        I69KIN
032900040723     C                   call      'TIBS69R'
033000040723     C                   parm                    BS69DS
033100040723     C                   parm                    ACODS
033200040723     C                   parm                    INDDS
033300040723     C                   parm                    CLPDS
033400040723     C                   parm                    CLSDS
033500040723     C     O69ERR        ifne      '1'
033600040723     C                   eval      vaoRSO = ACORAG
033700040723     C                   eval      vaoINO = INDVIA
033800040723     C                   movel(P)  INDCAP        vaoCAO
033900040723     C                   eval      vaoLOO = INDCIT
034000040723     C                   eval      vaoPRO = INDPRV
034100040723     C                   eval      vaoNAO = INDSTA
034200040723     C                   endif
034300040723     C                   endif
034400040723     C                   endif
034500040723     C                   endif
034600040723     C*
034700040723     C                   ENDSR
034800040723
034900040723
035000040723     C*------------------------------------------------------------------------*
035100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
035200040723     C*------------------------------------------------------------------------*
035300040723     C     WRIORI        BEGSR
035400040723     C*
035500090618     C* Come prima cosa stacco un numeratore da AZNUM
035600090618     C                   clear                   trul33ds
035700090618     C                   eval      I33OPE = *zeros
035800090618     C                   eval      I33CNU = 600
035900090618     C                   eval      I33NUM = 1
036000090618     C                   movel     TRUL33DS      KPJBU
036100090618     C                   call      'TRUL33R'
036200090618     C                   parm                    KPJBA
036300090618     C                   movel     KPJBU         TRUL33DS
036400090618     C                   if        O33ERR = *zeros
036500090618     C                   z-add     O33NRF        §numori
036600090618     c                   else
036700090618     c                   eval      §numori = *all'9'
036800090618     C                   endif
036900040723     C*
037000040723     C* Quindi imposto i campi "extra-VAO"
037100040723     C                   if        vlrKSC = '0ORM0000'
037200040723     C*
037300040723     C                   if        vaoCOR > *zeros
037400040723     C                   movel     vaoCOR        wrkCOR7           7 0
037500040723     C*
037600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
037700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
037800040723     C                   clear                   tibs10ds
037900040723     C                   eval      d10drf = datcor                              *data riferimento
038000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
038100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
038200091026     C*
038300091026     C* Preparazione chiamata al TIBS10R1
038400091026     C                   clear                   tibs10r1ds
038500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
038600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
038700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
038800091026     C*
038900091026     C                   call      'TIBS10R1'
039000091026     C                   parm                    tibs10r1ds
039100091026     C*
039200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
039300040723     C*
039400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
039500040723     C                             d10cop =  *zeros  or
039600040723     C                             d10cop =  d10cod
039700040723     C                   move      d10cod        oriIDC
039800040723     C                   else                                                   *Figlio
039900040723     C                   move      d10cop        oriIDC
040000040723     C                   endif
040100040723     C*
040200040723     C                   else
040300040723     C                   eval      oriIDC = *zeros
040400040723     C                   endif
040500040723     C*
040600040723     C                   else
040700040723     C                   eval      oriIDC = vlrKSC
040800040723     C                   endif
040900040723     C*
041000040723     C                   eval      oriPRG = §numori
041100040723     C                   eval      oriDER = datcor
041200040723     C                   eval      oriOER = wn6
041300040723     C                   eval      oriPOE = vaoPOE
041400040723     C                   eval      oriNSR = vaoNSR
041500040723     C                   eval      oriNOR = vaoNOR
041600040723     C                   eval      oriNRV = vaoNRV
041700040723     C                   eval      oriFLO = *blanks
041800040723     C*
041900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
042000040723     C                   eval      dorm01 = vaoFLO
042100040723     C                   movel     §numori       §ormpg
042200130603     C                   EVAL      §ORCOMC = 'N'
042300050412     C*
042400050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
042500130326     C*                   in questo caso non ci sono default
042600050412     C*
042700130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
042800040723     C                   eval      vaoFLO = dorm01
042900130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
043000130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
043100130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
043200040723     C*
043300040723     C* ...quindi scrivo il file TIORI00F
043400120905     C  N55              WRITE     TIORI000
043500040723     C*
043600040723     C                   ENDSR
043700050826
043800050826
043900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
044000050826     C     wriVGD        BEGSR
044100050826     C*
044200050826     C* Reperisco la descrizione della fase da tabella
044300050826     C                   movel(p)  'FAR'         KteCOD
044400050826     C                   movel(p)  '000'         KteKE1
044500050826     C     KEYtbe        chain     tntbe01l
044600050826     C                   if        %found(tntbe01l)
044700050826     C                   movel(P)  tbeuni        DFAR
044800050826     C                   else
044900050826     C                   clear                   DFAR
045000050826     C                   endif
045100050826     C*
045200050826     C* Valorizzo buffer record
045300050826     C                   clear                   FNVAPDS
045400050826     C                   eval      vapIDC = oriIDC
045500050826     C                   eval      vapPOE = oriPOE
045600050826     C                   eval      vapRFA = vaoRFA
045700050826     C                   eval      vapPOG = oriPOE
045800050826     C                   eval      vapDAE = oriDER
045900050826     C                   eval      vapORE = oriOER
046000050826     C                   if        d§fardva <> *blanks
046100050826     C                   eval      vapDFA = d§fardva
046200050826     C                   else
046300050826     C                   eval      vapDFA = d§fardes
046400050826     C                   endif
046500050826     C*
046600050826     C                   clear                   tivgd000
046700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
046800050826     C                   eval      vgdTIP = 'VP'
046900050826     C                   eval      vgdKSU = vapIDC
047000050826     C                   eval      vgdTSC = 'WW'
047100050826     C                   eval      vgdDAT = datcor
047200151207     C                   eval      vgdPGM = procname
047300060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
047400060306     C                   exsr      sr_Control
047500060306     C                   if        Cli_Abil = *on
047600060306     C                   eval      vgdKSU = woriIDC
047700120905     C  N55              write     tivgd000
047800060306     C                   endif
047900050826     C*
048000050826     C                   ENDSR
048100050826     C*------------------------------------------------------------------------*
048200010601
048300010601
048400010330     C*----------------------------------------------------*
048500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048600010330     C*----------------------------------------------------*
048700010330     C     INZVAR        BEGSR
048800010330     C*
048900030822     C* Inizializzo variabili di wrk
049000020204     C                   Z-ADD     *zeros        Num5_0            5 0
049100120124
049200030822     C*
049300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049400120123     C                   CLEAR                   FNVAO000
049500120123     C                   CLEAR                   TIORI000
049600030822     C                   CLEAR                   SkSplitCSV
049700030822     C                   EVAL      i = 1
049800030822     C                   EVAL      posDa = *zeros
049900030822     C                   EVAL      posA  = *zeros
050000030822     C*
050100030822     C* Reimposto i valori di default
050200030822     C                   EXSR      DEFCAM
050300030822     C*
050400010330     C                   ENDSR
050500010330     C*----------------------------------------------------*
050600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050700010330     C*----------------------------------------------------*
050800010330     C     DEFCAM        BEGSR
050900030822     C*
051000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051100030715     C* e delimitatore testo.
051200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051500040723     C*
051600080707     C* Imposto i valori d default d alcuni campi
051700080708     C*
051800080708     C                   CLEAR                   DORM01
051900120822     C                   EVAL      VAOFLO = DORM01
052000080707     C*
052100130603     C                   EVAL      VAOTOR = 'S'
052200160719     C                   EVAL      VAOCOR = 0898884000
052300130716     C                   EVAL      VAOPAG = 'O'
052400160719     C                   EVAL      VAOPOE = 089
052500160719     C                   EVAL      VAOKSC = 0898884
052600130716     C                   EVAL      VAOCTR = '000'
052700130603     C                   EVAL      VAOORR = 1400
052800130513
052900120905     C* se il 1° char dei parametri è T=Test
053000120905     C                   SETOFF                                       55
053100120905     C                   IF        %subst(vlrppt:1:1) = 'T'
053200120905     C* inibisco la scrittura dei file TIORI e TIVGD
053300120905     C                   SETON                                        55
053400120905     C* imposto il PO di emissione come filiale fittizia
053500120905     C                   EVAL      VAOPOE = 999
053600120905     C                   ENDIF
053700091127     C*
053800091204     C* IMPOSTO I CAMPI PERSONALIZZATI
053900100416     C
054000030822     C*
054100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054200030822     C                   EVAL      posDaDft = 1
054300030822     C                   EVAL      posADft  = 0
054400030822     C                   EVAL      wGiroDft = 0
054500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054600030822     C                             posDaDft > 0
054700030822     C*
054800030822     C* Gestisco il 1° giro
054900030822     C                   IF        wGiroDft = 0
055000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055200030822     C* Incremento il contatore dei "giri"
055300030822     C                   EVAL      wGiroDft = 1
055400030822     C                   ELSE
055500030822     C                   EVAL      posDaDft = posADft
055600030822     C                   ENDIF
055700030822     C* Eseguo lo scan x trovare la fine del campo corrente
055800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055900030822     C*
056000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056200030822     C                   IF        posDaDft > 0 AND
056300030822     C                             posADft  > 0
056400040708     C* POE
056500040708     C                   IF        %subst(
056600040708     C                             %subst(vlrppt:posDaDft+1:
056700040708     C                             posADft-posDaDft-1):1:3)
056800040708     C                             = 'POE'
056900040708     C                   EVAL      PiStr=%trim(%subst(
057000040708     C                             %subst(vlrppt:posDaDft+1:
057100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200040708     C                   EXSR      CHKNUM
057300040708     C                   IF        PiInt=*on
057400040708     C                   Z-ADD     PiVal         VAOPOE
057500040708     C                   ENDIF
057600040708     C                   ENDIF
057700040708     C* TOR
057800040708     C                   IF        %subst(
057900040708     C                             %subst(vlrppt:posDaDft+1:
058000040708     C                             posADft-posDaDft-1):1:3)
058100040708     C                             = 'TOR'
058200040708     C                   EVAL      VAOTOR=%trim(%subst(
058300040708     C                             %subst(vlrppt:posDaDft+1:
058400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500040708     C                   ENDIF
058600040708     C* TCO
058700040708     C                   IF        %subst(
058800040708     C                             %subst(vlrppt:posDaDft+1:
058900040708     C                             posADft-posDaDft-1):1:3)
059000040708     C                             = 'TCO'
059100040708     C                   EVAL      VAOTCO=%trim(%subst(
059200040708     C                             %subst(vlrppt:posDaDft+1:
059300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400040708     C                   ENDIF
059500040707     C* COR
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900040707     C                             = 'COR'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500040708     C                   Z-ADD     PiVal         VAOCOR
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800040708     C* CRC
060900040708     C                   IF        %subst(
061000040708     C                             %subst(vlrppt:posDaDft+1:
061100040708     C                             posADft-posDaDft-1):1:3)
061200040708     C                             = 'CRC'
061300040708     C                   EVAL      PiStr=%trim(%subst(
061400040708     C                             %subst(vlrppt:posDaDft+1:
061500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600040708     C                   EXSR      CHKNUM
061700040708     C                   IF        PiInt=*on
061800040708     C                   Z-ADD     PiVal         VAOCRC
061900040708     C                   ENDIF
062000040708     C                   ENDIF
062100040708     C* FFD
062200040708     C                   IF        %subst(
062300040708     C                             %subst(vlrppt:posDaDft+1:
062400040708     C                             posADft-posDaDft-1):1:3)
062500040708     C                             = 'FFD'
062600040708     C                   EVAL      VAOFFD=%trim(%subst(
062700040708     C                             %subst(vlrppt:posDaDft+1:
062800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900040708     C                   ENDIF
063000040708     C* ORR
063100040708     C                   IF        %subst(
063200040708     C                             %subst(vlrppt:posDaDft+1:
063300040708     C                             posADft-posDaDft-1):1:3)
063400040708     C                             = 'ORR'
063500040708     C                   EVAL      PiStr=%trim(%subst(
063600040708     C                             %subst(vlrppt:posDaDft+1:
063700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800040708     C                   EXSR      CHKNUM
063900040708     C                   IF        PiInt=*on
064000040708     C                   Z-ADD     PiVal         VAOORR
064100040708     C                   ENDIF
064200040708     C                   ENDIF
064300040712     C* NCL
064400040712     C                   IF        %subst(
064500040712     C                             %subst(vlrppt:posDaDft+1:
064600040712     C                             posADft-posDaDft-1):1:3)
064700040712     C                             = 'NCL'
064800040712     C                   EVAL      PiStr=%trim(%subst(
064900040712     C                             %subst(vlrppt:posDaDft+1:
065000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100040712     C                   EXSR      CHKNUM
065200040712     C                   IF        PiInt=*on
065300040712     C                   Z-ADD     PiVal         VAONCL
065400040712     C                   ENDIF
065500040712     C                   ENDIF
065600040712     C* PKG
065700040712     C                   IF        %subst(
065800040712     C                             %subst(vlrppt:posDaDft+1:
065900040712     C                             posADft-posDaDft-1):1:3)
066000040712     C                             = 'PKG'
066100040712     C                   EVAL      PiStr=%trim(%subst(
066200040712     C                             %subst(vlrppt:posDaDft+1:
066300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400040712     C                   EXSR      CHKNUM
066500040712     C                   IF        PiNum=*on
066600040712     C                   Z-ADD     PiVal         VAOPKG
066700040712     C                   ENDIF
066800040712     C                   ENDIF
066900040712     C* VLM
067000040712     C                   IF        %subst(
067100040712     C                             %subst(vlrppt:posDaDft+1:
067200040712     C                             posADft-posDaDft-1):1:3)
067300040712     C                             = 'VLM'
067400040712     C                   EVAL      PiStr=%trim(%subst(
067500040712     C                             %subst(vlrppt:posDaDft+1:
067600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700040712     C                   EXSR      CHKNUM
067800040712     C                   IF        PiNum=*on
067900040712     C                   Z-ADD     PiVal         VAOVLM
068000040712     C                   ENDIF
068100040712     C                   ENDIF
068200040708     C* NAM
068300040708     C                   IF        %subst(
068400040708     C                             %subst(vlrppt:posDaDft+1:
068500040708     C                             posADft-posDaDft-1):1:3)
068600040708     C                             = 'NAM'
068700040708     C                   EVAL      VAONAM=%trim(%subst(
068800040708     C                             %subst(vlrppt:posDaDft+1:
068900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000040708     C                   ENDIF
069100040708     C* PAG
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500040708     C                             = 'PAG'
069600040708     C                   EVAL      VAOPAG=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000040708     C* KSC
070100040708     C                   IF        %subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):1:3)
070400040708     C                             = 'KSC'
070500040708     C                   EVAL      PiStr=%trim(%subst(
070600040708     C                             %subst(vlrppt:posDaDft+1:
070700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800040708     C                   EXSR      CHKNUM
070900040708     C                   IF        PiInt=*on
071000040708     C                   Z-ADD     PiVal         VAOKSC
071100040708     C                   ENDIF
071200040708     C                   ENDIF
071300040708     C* CTR
071400040708     C                   IF        %subst(
071500040708     C                             %subst(vlrppt:posDaDft+1:
071600040708     C                             posADft-posDaDft-1):1:3)
071700040708     C                             = 'CTR'
071800040708     C                   EVAL      VAOCTR=%trim(%subst(
071900040708     C                             %subst(vlrppt:posDaDft+1:
072000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100040708     C                   ENDIF
072200040708     C* POC
072300040708     C                   IF        %subst(
072400040708     C                             %subst(vlrppt:posDaDft+1:
072500040708     C                             posADft-posDaDft-1):1:3)
072600040708     C                             = 'POC'
072700040708     C                   EVAL      PiStr=%trim(%subst(
072800040708     C                             %subst(vlrppt:posDaDft+1:
072900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000040708     C                   EXSR      CHKNUM
073100040708     C                   IF        PiInt=*on
073200040708     C                   Z-ADD     PiVal         VAOPOC
073300040708     C                   ENDIF
073400040708     C                   ENDIF
073500040708     C* NO1
073600040708     C                   IF        %subst(
073700040708     C                             %subst(vlrppt:posDaDft+1:
073800121129     C                             posADft-posDaDft-1):1:3)
073900040708     C                             = 'NO1'
074000040708     C                   EVAL      VAONO1=%trim(%subst(
074100040708     C                             %subst(vlrppt:posDaDft+1:
074200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300040708     C                   ENDIF
074400040708     C* DDT
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800040708     C                             = 'DDT'
074900040708     C                   EVAL      VAODDT=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300040708     C* RFA
075400040708     C                   IF        %subst(
075500040708     C                             %subst(vlrppt:posDaDft+1:
075600040708     C                             posADft-posDaDft-1):1:3)
075700040708     C                             = 'RFA'
075800040708     C                   EVAL      VAORFA=%trim(%subst(
075900040708     C                             %subst(vlrppt:posDaDft+1:
076000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100040708     C                   ENDIF
076200040708     C* FLO
076300040708     C                   IF        %subst(
076400040708     C                             %subst(vlrppt:posDaDft+1:
076500040708     C                             posADft-posDaDft-1):1:3)
076600040708     C                             = 'FLO'
076700040708     C                   EVAL      VAOFLO=%trim(%subst(
076800040708     C                             %subst(vlrppt:posDaDft+1:
076900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000091103     C*
077100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
077200080702     C* e posizione 7 del VAOFLO
077300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
077400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
077500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
077600040708     C                   ENDIF
077700030822     C* ...
077800030822     C                   ENDIF
077900030822     C                   ENDDO
078000020204     C*
078100010330     C                   ENDSR
078200010607     C*----------------------------------------------------*
078300040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
078400010607     C*----------------------------------------------------*
078500040708     C     IMPVAO        BEGSR
078600010607     C*
078700010607     C                   SETOFF                                       3132
078800120124
078900030515     C*
079000040708     C                   MOVEL     datcor        VAODAO
079100040708     C                   MOVEL     wn14          VAOOAO
079200050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079300050314     C     minu:maiu     XLATE     vindta        vindta
079400010607     C*
079500030715     C**********
079600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079700030715     C**********
079800030715     C                   IF        wGiro = *zeros
079900040709     C*
080000040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080100040709     C                   EVAL      depvinDTA = vinDTA
080200040709     C*
080300040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
080400040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080500040709     C                                      %trim(intcol(3))
080600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080700050314     C**!!!minu:maiu     XLATE     vindta        vindta
080800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080900040324     C                   DOW       posDa <= %len(%trimr(vindta))
081000030715     C*
081100030715     C* Gestisco il 1° campo
081200030715     C                   IF        i = 1
081300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081400030715     C                   EVAL      posDa = 1
081500030715     C* Eseguo lo scan x trovare la fine del primo campo
081600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081700030715     C* A questo punto "estraggo" il campo corrente
081800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081900030715     C                                                    (posA-posDa))
082000030715     C* X i campi successivi al 1°
082100030715     C                   ELSE
082200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082300030715     C                   EVAL      posDa = posA + 1
082400030715     C* Eseguo lo scan x trovare la fine del campo corrente
082500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082600030715     C* Gestisco l'ultimo campo
082700030715     C                   IF        posA = *zeros
082800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
082900030715     C* A questo punto "estraggo" il campo corrente
083000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083100030715     C                                                    (posA-posDa))
083200030715     C                   LEAVE
083300030715     C                   ELSE
083400030715     C* A questo punto "estraggo" il campo corrente
083500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083600030715     C                                                    (posA-posDa))
083700030715     C                   ENDIF
083800030715     C                   ENDIF
083900030715     C* Incremento il contatore d campo
084000030715     C                   EVAL      i = i +1
084100030715     C                   ENDDO
084200030715     C*
084300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084400030715     C* delimitazione testo
084500030715     C                   EVAL      i = 1
084600030715     C                   DOW       i < %elem(SkSplitFLD)
084700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084900030715     C                   EVAL      i = i + 1
085000030715     C                   ENDDO
085100030715     C                   EVAL      wGiro = 1
085200040709     C* Ripristino la 1° riga originale
085300040709     C                   EVAL      vinDTA = depvinDTA
085400030715     C                   ELSE
085500030715     C                   EVAL      wGiro = 2
085600031204     C**********
085700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085800031204     C* d separatore campo
085900031204     C**********
086000040709     C                   EVAL      vindta = %trim(vindta)
086100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086200040709     C                   EVAL      vindta = %subst(vindta:2)
086300040709     C                   ENDDO
086400040709     C*
086500040709     C                   Z-ADD     *zeros        lunghInput        4 0
086600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
086700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087000040709     C                   ENDDO
087100030715     C**********
087200030715     C* Effettuo lo split del campo dati d input
087300030715     C**********
087400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087500040324     C                   DOW       posDa <= %len(%trimr(vindta))
087600030715     C*
087700030715     C* Gestisco il 1° campo
087800030715     C                   IF        i = 1
087900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088000030715     C                   EVAL      posDa = 1
088100030715     C* Eseguo lo scan x trovare la fine del primo campo
088200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C* X i campi successivi al 1°
088700030715     C                   ELSE
088800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088900030715     C                   EVAL      posDa = posA + 1
089000031204     C* Verifico che nn vi sia il campo nn valorizzato
089100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089200031204     C* Se campo nn valorizzato skippo al prossimo
089300031204     C                   EVAL      SkSplitCSV(i) = *blanks
089400031204     C                   EVAL      posA  = posA + 1
089500031204     C                   ELSE
089600030715     C* Eseguo lo scan x trovare la fine del campo corrente
089700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089800030715     C* Gestisco l'ultimo campo
089900030715     C                   IF        posA = *zeros
090000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C                   LEAVE
090500030715     C                   ELSE
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   ENDIF
091000031204     C                   ENDIF
091100030715     C                   ENDIF
091200030715     C* Incremento il contatore d campo
091300030715     C                   EVAL      i = i +1
091400030715     C                   ENDDO
091500030715     C*
091600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091700030715     C* delimitazione testo
091800030715     C                   EVAL      i = 1
091900030715     C                   DOW       i < %elem(SkSplitCSV)
092000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092200030715     C                   EVAL      i = i + 1
092300030715     C                   ENDDO
092400030715     C*
092500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitFLD)
092800030715     C*
092900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093100030715     C                             SkSplitCSV(i) <> *blanks
093200030721     C*** ATB
093300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
093400040708     C                   EVAL      VAOATB = SkSplitCSV(i)
093500030721     C                   ENDIF
093600040708     C*** TOR
093700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
093800040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
093900040708     C                   ENDIF
094000040708     C*** TCO
094100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
094200040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
094300040708     C                   ENDIF
094400040708     C*** RSO
094500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
094600040708     C                   EVAL      VAORSO = SkSplitCSV(i)
094700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094800040708     C     '@':'A'       XLATE     VAORSO        VAORSO
094900030715     C* ==
095000030715     C                   ENDIF
095100040708     C*** INO
095200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
095300040708     C                   EVAL      VAOINO = SkSplitCSV(i)
095400030715     C                   ENDIF
095500040708     C*** LOO
095600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
095700040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
095800030715     C                   ENDIF
095900040708     C*** PRO
096000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
096100040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
096200030715     C                   ENDIF
096300040708     C*** NAO
096400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
096500040708     C                   EVAL      VAONAO = SkSplitCSV(i)
096600030721     C                   ENDIF
096700040708     C*** RSR
096800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
096900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
097000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
097200040708     C* ==
097300040708     C                   IF        VAORSR = *blanks
097400040708     C                   SETON                                        31
097500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097600040708     C                             + ' ' + 'VAORSR'
097700040708     C                   ENDIF
097800040708     C                   ENDIF
097900040708     C*** INR
098000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
098100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
098200040708     C                   IF        VAOINR = *blanks
098300040708     C                   SETON                                        31
098400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098500040708     C                             + ' ' + 'VAOINR'
098600040708     C                   ENDIF
098700040708     C                   ENDIF
098800040708     C*** LOR
098900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
099000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
099100040708     C                   IF        VAOLOR = *blanks
099200040708     C                   SETON                                        31
099300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099400040708     C                             + ' ' + 'VAOLOR'
099500040708     C                   ENDIF
099600040708     C                   ENDIF
099700040708     C*** PRR
099800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
099900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
100000040708     C                   IF        VAOPRR = *blanks
100100040708     C                   SETON                                        31
100200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100300040708     C                             + ' ' + 'VAOPRR'
100400040708     C                   ENDIF
100500040708     C                   ENDIF
100600040708     C*** NAR
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
100800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
100900040708     C                   ENDIF
101000040708     C*** RER
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
101200040708     C                   EVAL      VAORER = SkSplitCSV(i)
101300040708     C                   ENDIF
101400040708     C*** TER
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
101600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
101700040708     C                   ENDIF
101800040708     C*** RSC
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
102000040708     C                   EVAL      VAORSC = SkSplitCSV(i)
102100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102200040708     C     '@':'A'       XLATE     VAORSC        VAORSC
102300040708     C* ==
102400040708     C                   ENDIF
102500040708     C*** INC
102600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
102700040708     C                   EVAL      VAOINC = SkSplitCSV(i)
102800040708     C                   ENDIF
102900040708     C*** LOC
103000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
103100040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
103200040708     C                   ENDIF
103300040708     C*** PRC
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
103500040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
103600040708     C                   ENDIF
103700040708     C*** NAC
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
103900040708     C                   EVAL      VAONAC = SkSplitCSV(i)
104000040708     C                   ENDIF
104100040708     C*** FFD
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
104300040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
104400040708     C                   ENDIF
104500040708     C*** RMP
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
104700040708     C                   EVAL      VAORMP = SkSplitCSV(i)
104800040708     C                   ENDIF
104900040708     C*** NAM
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
105100040708     C                   EVAL      VAONAM = SkSplitCSV(i)
105200040708     C                   ENDIF
105300040708     C*** PAG
105400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
105500040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
105600040708     C                   ENDIF
105700040708     C*** CTR
105800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
105900040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
106000040708     C                   ENDIF
106100040708     C*** ZOR
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
106300040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
106400040708     C                   ENDIF
106500040708     C*** NO1
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
106700040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
106800030715     C                   ENDIF
106900040708     C*** NO2
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
107100040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
107200030715     C                   ENDIF
107300040708     C*** DDT
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
107500040708     C                   EVAL      VAODDT = SkSplitCSV(i)
107600030715     C                   ENDIF
107700040708     C*** RFA
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
107900040708     C                   EVAL      VAORFA = SkSplitCSV(i)
108000030715     C                   ENDIF
108100040708     C*** SPI
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
108300040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
108400030715     C                   ENDIF
108500040708     C*** FLO
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
108700040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
108800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
108900080708     C* e la posizione 7 del VAOFLO
109000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
109100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
109200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
109300030715     C                   ENDIF
109400091103     C
109500091103     C*** DAR
109600050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
109700050412     C                   EVAL      PiStr=SkSplitCSV(i)
109800050412     C                   EXSR      CHKNUM
109900050412     C                   IF        PiInt=*on
110000050412     C                   Z-ADD     PiVal         VAODAR
110100050412     C                   ELSE
110200050412     C                   SETON                                        32
110300050412     C                   EVAL      VAODAR = *zeros
110400050412     C                   EVAL      vinmsg = %trimr(vinmsg)
110500050412     C                             + ' ' + 'VAODAR'
110600050412     C                   ENDIF
110700050314     C                   ENDIF
110800030715     C
110900030721     C*
111000030721     C* Reperisco quindi i campi numerici...
111100040708     C*** POE
111200040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
111300040723     C                   EVAL      PiStr=SkSplitCSV(i)
111400040723     C                   EXSR      CHKNUM
111500040723     C                   IF        PiInt=*on    AND
111600040723     C                             PiVal<=999   AND
111700040723     C                             PiVal>*zeros
111800040723     C                   Z-ADD     PiVal         VAOPOE
111900040723     C                   ELSE
112000040723     C                   SETON                                        32
112100040723     C                   EVAL      VAOPOE = *zeros
112200040723     C                   EVAL      vinmsg = %trimr(vinmsg)
112300040723     C                             + ' ' + 'VAOPOE'
112400040723     C                   ENDIF
112500040723     C                   ENDIF
112600040708     C*** COR
112700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
112800040708     C                   EVAL      PiStr=SkSplitCSV(i)
112900040708     C                   EXSR      CHKNUM
113000040708     C                   IF        PiInt=*on
113100040708     C                   Z-ADD     PiVal         VAOCOR
113200040708     C                   ELSE
113300040708     C                   SETON                                        31
113400040708     C                   EVAL      VAOCOR = *zeros
113500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040708     C                             + ' ' + 'VAOCOR'
113700040708     C                   ENDIF
113800040708     C                   ENDIF
113900040708     C*** CAO
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on
114400030721     C                   Z-ADD     PiVal         Num5_0
114500040708     C                   MOVEL(p)  Num5_0        VAOCAO
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800040708     C                   EVAL      VAOCAO = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000040708     C                             + ' ' + 'VAOCAO'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300040708     C*** CRA
115400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on
115800040708     C                   Z-ADD     PiVal         VAOCRA
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100040708     C                   EVAL      VAOCRA = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300040708     C                             + ' ' + 'VAOCRA'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600040708     C*** CAR
116700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
116800040708     C                   EVAL      PiStr=SkSplitCSV(i)
116900040708     C                   EXSR      CHKNUM
117000040708     C                   IF        PiInt=*on
117100040708     C                   Z-ADD     PiVal         Num5_0
117200040708     C                   MOVEL(p)  Num5_0        VAOCAR
117300040708     C                   ELSE
117400040708     C                   SETON                                        31
117500040708     C                   EVAL      VAOCAR = *zeros
117600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117700040708     C                             + ' ' + 'VAOCAR'
117800040708     C                   ENDIF
117900040708     C                   ENDIF
118000040708     C*** CRC
118100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
118200040708     C                   EVAL      PiStr=SkSplitCSV(i)
118300040708     C                   EXSR      CHKNUM
118400040708     C                   IF        PiInt=*on
118500040708     C                   Z-ADD     PiVal         VAOCRC
118600040708     C                   ELSE
118700040708     C                   SETON                                        32
118800040708     C                   EVAL      VAOCRC = *zeros
118900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119000040708     C                             + ' ' + 'VAOCRC'
119100040708     C                   ENDIF
119200040708     C                   ENDIF
119300040708     C*** CAC
119400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
119500040708     C                   EVAL      PiStr=SkSplitCSV(i)
119600040708     C                   EXSR      CHKNUM
119700040708     C                   IF        PiInt=*on
119800040708     C                   Z-ADD     PiVal         Num5_0
119900040708     C                   MOVEL(p)  Num5_0        VAOCAC
120000040708     C                   ELSE
120100040708     C                   SETON                                        32
120200040708     C                   EVAL      VAOCAC = *zeros
120300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040708     C                             + ' ' + 'VAOCAC'
120500040708     C                   ENDIF
120600040708     C                   ENDIF
120700040708     C*** DAR
120800090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
120900090428     C                   EVAL      PiStr=SkSplitCSV(i)
121000090428     C                   EXSR      CHKNUM
121100090428     C                   IF        PiInt=*on
121200090428     C                   Z-ADD     PiVal         VAODAR
121300090428     C                   ELSE
121400090428     C                   SETON                                        32
121500090428     C                   EVAL      VAODAR = *zeros
121600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
121700090428     C                             + ' ' + 'VAODAR'
121800090428     C                   ENDIF
121900090428     C                   ENDIF
122000040708     C*** ORR
122100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
122200040708     C                   EVAL      PiStr=SkSplitCSV(i)
122300040708     C                   EXSR      CHKNUM
122400040708     C                   IF        PiInt=*on
122500040708     C                   Z-ADD     PiVal         VAOORR
122600040708     C                   ELSE
122700040708     C                   SETON                                        32
122800040708     C                   EVAL      VAOORR = *zeros
122900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123000040708     C                             + ' ' + 'VAOORR'
123100040708     C                   ENDIF
123200040708     C                   ENDIF
123300030721     C*** NCL
123400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on
123800040723     C                   IF        PiVal > *zeros
123900040708     C                   Z-ADD     PiVal         VAONCL
124000040723     C                   ENDIF
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300040708     C                   EVAL      VAONCL = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500040708     C                             + ' ' + 'VAONCL'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800040708     C*** PKG
124900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiNum=*on
125300040723     C                   IF        PiVal > *zeros
125400040708     C                   Z-ADD     PiVal         VAOPKG
125500040723     C                   ENDIF
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800040708     C                   EVAL      VAOPKG = *zeros
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000040708     C                             + ' ' + 'VAOPKG'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300040708     C*** VLM
126400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
126500040708     C                   EVAL      PiStr=SkSplitCSV(i)
126600040708     C                   EXSR      CHKNUM
126700040708     C                   IF        PiNum=*on
126800040708     C                   Z-ADD     PiVal         VAOVLM
126900040708     C                   ELSE
127000040708     C                   SETON                                        32
127100040708     C                   EVAL      VAOVLM = *zeros
127200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127300040708     C                             + ' ' + 'VAOVLM'
127400040708     C                   ENDIF
127500040708     C                   ENDIF
127600040708     C*** BNC
127700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
127800040708     C                   EVAL      PiStr=SkSplitCSV(i)
127900040708     C                   EXSR      CHKNUM
128000040708     C                   IF        PiInt=*on
128100040708     C                   Z-ADD     PiVal         VAOBNC
128200040708     C                   ELSE
128300040708     C                   SETON                                        32
128400040708     C                   EVAL      VAOBNC = *zeros
128500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128600040708     C                             + ' ' + 'VAOBNC'
128700040708     C                   ENDIF
128800040708     C                   ENDIF
128900040708     C*** BLC
129000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
129100040708     C                   EVAL      PiStr=SkSplitCSV(i)
129200040708     C                   EXSR      CHKNUM
129300040708     C                   IF        PiInt=*on
129400040708     C                   Z-ADD     PiVal         VAOBLC
129500040708     C                   ELSE
129600040708     C                   SETON                                        32
129700040708     C                   EVAL      VAOBLC = *zeros
129800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129900040708     C                             + ' ' + 'VAOBLC'
130000040708     C                   ENDIF
130100040708     C                   ENDIF
130200040708     C*** ATT
130300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
130400040708     C                   EVAL      PiStr=SkSplitCSV(i)
130500040708     C                   EXSR      CHKNUM
130600040708     C                   IF        PiInt=*on
130700040708     C                   Z-ADD     PiVal         VAOATT
130800040708     C                   ELSE
130900040708     C                   SETON                                        32
131000040708     C                   EVAL      VAOATT = *zeros
131100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131200040708     C                             + ' ' + 'VAOATT'
131300040708     C                   ENDIF
131400040708     C                   ENDIF
131500040708     C*** MTC
131600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
131700040708     C                   EVAL      PiStr=SkSplitCSV(i)
131800040708     C                   EXSR      CHKNUM
131900040708     C                   IF        PiInt=*on
132000040708     C                   Z-ADD     PiVal         VAOMTC
132100040708     C                   ELSE
132200040708     C                   SETON                                        32
132300040708     C                   EVAL      VAOMTC = *zeros
132400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132500040708     C                             + ' ' + 'VAOMTC'
132600040708     C                   ENDIF
132700040708     C                   ENDIF
132800040708     C*** KSC
132900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on      AND
133300030721     C                             PiVal<=9999999 AND
133400030721     C                             PiVal>*zeros
133500040708     C                   Z-ADD     PiVal         VAOKSC
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800040708     C                   EVAL      VAOKSC = *zeros
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000040708     C                             + ' ' + 'VAOKSC'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300040708     C*** POR
134400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on    AND
134800030721     C                             PiVal<=999   AND
134900030721     C                             PiVal>*zeros
135000040708     C                   Z-ADD     PiVal         VAOPOR
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300040708     C                   EVAL      VAOPOR = *zeros
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500040708     C                             + ' ' + 'VAOPOR'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800040708     C*** POC
135900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
136000040708     C                   EVAL      PiStr=SkSplitCSV(i)
136100040708     C                   EXSR      CHKNUM
136200040708     C                   IF        PiInt=*on    AND
136300040708     C                             PiVal<=999   AND
136400040708     C                             PiVal>*zeros
136500040708     C                   Z-ADD     PiVal         VAOPOC
136600040708     C                   ELSE
136700040708     C                   SETON                                        32
136800040708     C                   EVAL      VAOPOC = *zeros
136900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137000040708     C                             + ' ' + 'VAOPOC'
137100040708     C                   ENDIF
137200040708     C                   ENDIF
137300040708     C*** STO
137400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
137500040708     C                   EVAL      PiStr=SkSplitCSV(i)
137600040708     C                   EXSR      CHKNUM
137700040708     C                   IF        PiInt=*on
137800040708     C                   Z-ADD     PiVal         VAOSTO
137900040708     C                   ELSE
138000040708     C                   SETON                                        32
138100040708     C                   EVAL      VAOSTO = *zeros
138200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138300040708     C                             + ' ' + 'VAOSTO'
138400040708     C                   ENDIF
138500040708     C                   ENDIF
138600090428     C*
138700091103     C********** GESTIONE CAMPI PERSONALIZZATI
138800120120
138900130716     C*** W_VAORSR
139000130716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSR'
139100130716     C                   EVAL      VAORSR = %trim(SkSplitCSV(i))
139200130716     C                   EVAL      VAORER = VAORSR
139300130513     C                   ENDIF
139400121207
139500090428     C*
139600090428     C****************************************
139700090428     C*
139800030715     C*
139900030715     C                   ENDIF
140000030715     C                   EVAL      i = i + 1
140100030715     C                   ENDDO
140200040723     C*
140300121207     C***  ===>  Gestione campi "particolari" fuori ciclo
140400121207     C*
140500130513
140600030715     C                   ENDIF
140700020204     C*
140800010607     C                   ENDSR
140900010607     C*----------------------------------------------------*
141000080702
141100080702
141200080702     C*----------------------------------------------------*
141300080702     C*  CONTROLLO NUMERICITA' CAMPI
141400080702     C*----------------------------------------------------*
141500080702     C     CHKNUM        BEGSR
141600080702     C*
141700080702     C                   IF        PiDecChr = *blanks
141800160719     C                   EVAL      PiDecChr = CharNUM
141900080702     C                   ENDIF
142000080702     C*
142100080702     C                   callp     UBISNUM_Check(PiStr
142200080702     C                                          :PiDecChr
142300080702     C                                          :PiVal
142400080702     C                                          :PiNum
142500080702     C                                          :PiInt)
142600080702     C*
142700080702     C                   ENDSR
142800080702     C***
142900080702
143000010601
143100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
143200010601     C     repfil        BEGSR
143300010601     C*
143400010601     C                   if        invfil = *zeros and
143500010601     C                             depfil > *zeros and
143600010629     C                             (vinflg = *blanks or
143700010629     C                              vinflg = *zeros)
143800010601     C
143900010601     C                   eval      invfil = depfil
144000010601     C                   endif
144100010601     C*
144200010601     C                   if        depfil <> invfil and
144300010601     C                             invfil > *zeros
144400010601     C                   eval      flgMulti = '1'
144500010601     C                   if        vinflg = *blanks
144600010601     C                   add       1             cntNonEl
144700010601     C                   endif
144800010601     C                   endif
144900010601     C*
145000010601     C                   if        vinflg = '2'
145100010601     C                   eval      flgStato = '2'
145200010601     C                   endif
145300010601     C*
145400010601     C                   ENDSR
145500010601     C***
145600010330
145700010330
145800990920      /TITLE Invio dei dati al punto operativo.
145900000613     C     invio         BEGSR
146000990920     C*
146100990920     C                   reset                   dscmz
146200010601     C                   move      invfil        cmzdst
146300990920     C                   eval      cmzfld = vlrfou
146400990920     C                   eval      cmzmbd = vlrhdl
146500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
146600000710     C                   if        prmfir = *blanks
146700040708     C                   eval      cmzfla = 'FNVAO00F'
146800040708     C                   eval      cmzmba = 'FNVAO00F'
146900000710     C                   else
147000000710     C                   eval      cmzfla = prmfir
147100000710     C                   eval      cmzmba = prmfir
147200000710     C                   endif
147300990920     C                   eval      cmznrr = *zeros
147400990920     C                   move      §ctrok        cmznrr
147500021018     C                   eval      cmzlba = vlrfl1
147600990920     C                   call(e)   'TIS711C'
147700990920     C                   parm                    dscmz
147800990921     C                   parm      *blanks       esito
147900990920     C                   if        %error
148000990920     C                             or cmzerr = '1'
148100990921     C                             or esito  = '1'
148200000710     C                   eval      wrkesito = '3'
148300990920     C                   endif
148400990920     C*
148500000613     C                   ENDSR
148600010601
148700010601
148800010601      /TITLE Invio dei dati al punto operativo.
148900010601     C     opeini        BEGSR
149000010601     C*
149100010601     C* Inizializzo flag e contatori operativi
149200010601     C                   movel     '0'           flgGiro           1
149300010601     C                   movel     '0'           flgMulti          1
149400010601     C                   movel     '1'           flgStato          1
149500010615     C                   movel     '0'           flgOk             1
149600010601     C                   z-add     *zeros        cntNonEl         10 0
149700010601     C                   z-add     *zeros        depfil            3 0
149800010601     C                   z-add     *zeros        invfil            3 0
149900010601     C*
150000010601     C                   ENDSR
150100010601     C***
150200050421
150300050421
150400091130     C     *pssr         BEGSR
150500050421     C*
150600091130     C                   eval      wrkesito = '2'
150700050421     C*
150800091130     C                   ENDSR     '*CANCL'
150900050421     C***
151000060306
151100060306
151200060306     C*-------------------------------------------------------------------------*
151300060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
151400060306     C*-------------------------------------------------------------------------*
151500060306     C     sr_Control    begsr
151600060306     C*
151700060306     C* inizializzo a ok il flag d controllo abilitazione
151800060306     C                   eval      Cli_Abil = *on
151900060306     C
152000060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
152100060306     C                   clear                   woriIDC
152200060306     C                   eval      kteCOD = 'VAP'
152300060306     C                   eval      kteKE1 = oriIDC
152400060306     C     keytbe        chain     TNTBE01L
152500060306     C                   if        %found(Tntbe01l)
152600060306     C                   eval      woriIDC = TbeKe2
152700060306     C                   else
152800060306     C                   eval      Cli_Abil = *off
152900060306     C                   endif
153000060306     C*
153100060306     C                   endsr
153200010330
153300010330
153400000613     C     *inzsr        BEGSR
153500990910     C*
153600990910     C     *entry        plist
153700990920     C                   parm                    tivlrds
153800990921     C                   parm      wrkesito      esito
153900000724     C                   parm                    prmlit
154000000710     C                   parm                    prmfir
154100010330     C*
154200010330     C* CALCOLA LA DATA CORRENTE
154300091127     C                   z-add     *zeros        datcor            8 0
154400091127     C                   time                    wn14             14 0
154500091127     C                   movel     wn14          wn6               6 0          *ORA (6)
154600130516     C                   eval      datcor = %dec(%date() : *ISO)
154700040723     C*
154800040723     C* Campi riferito al database
154900040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
155000040723     C     *like         define    tbeke1        kteKE1
155100040723     C*
155200040723     C* Definizione chiavi di lettura
155300040723     C* TNTBE01L - parziale
155400040723     C     keytbe        klist
155500040723     C                   kfld                    kteCOD                         *UTENTE
155600040723     C                   kfld                    kteKE1                         *CODICE
155700000613     C*
155800000613     C                   ENDSR
155900000613     C***
156000990908
156100040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
156200160719VAORFA;W_VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAOTER;VAONCL;VAOPKG;
156300160719VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;
156400160719
