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
042200140528     C                   movel     'N'           §orcomc
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
052100130513     C                   EVAL      VAONCL = 1
052200130513     C                   EVAL      VAOPKG = 1
052300130513     C                   EVAL      VAOCOR = 0505029000
052400130513     C                   EVAL      VAORSC = 'NESPRESSO C/O LOGISTICA BRT'
052500130513     C                   EVAL      VAOINC = 'LOC. CASCINA SCARAVELLA'
052600130513     C                   EVAL      VAOCAC = '20018'
052700130513     C                   EVAL      VAOLOC = 'SEDRIANO'
052800130513     C                   EVAL      VAOPRC = 'MI'
052900130513     C                   EVAL      VAOKSC = 0505029
053000130513     C                   EVAL      VAOPAG = 'O'
053100130513     C                   EVAL      VAOPOE = 050
053200130513     C                   EVAL      VAOTOR = 'S'
053300130513     C                   EVAL      VAOTCO = 'F'
053400150512     C                   EVAL      VAOORR = 1400
053500130513
053600130513     C***                EVAL      VAOCTR = '001'
053700120905     C* se il 1° char dei parametri è T=Test
053800120905     C                   SETOFF                                       55
053900120905     C                   IF        %subst(vlrppt:1:1) = 'T'
054000120905     C* inibisco la scrittura dei file TIORI e TIVGD
054100120905     C                   SETON                                        55
054200120905     C* imposto il PO di emissione come filiale fittizia
054300120905     C                   EVAL      VAOPOE = 999
054400120905     C                   ENDIF
054500091127     C*
054600091204     C* IMPOSTO I CAMPI PERSONALIZZATI
054700100416     C
054800030822     C*
054900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055000030822     C                   EVAL      posDaDft = 1
055100030822     C                   EVAL      posADft  = 0
055200030822     C                   EVAL      wGiroDft = 0
055300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055400030822     C                             posDaDft > 0
055500030822     C*
055600030822     C* Gestisco il 1° giro
055700030822     C                   IF        wGiroDft = 0
055800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056000030822     C* Incremento il contatore dei "giri"
056100030822     C                   EVAL      wGiroDft = 1
056200030822     C                   ELSE
056300030822     C                   EVAL      posDaDft = posADft
056400030822     C                   ENDIF
056500030822     C* Eseguo lo scan x trovare la fine del campo corrente
056600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056700030822     C*
056800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057000030822     C                   IF        posDaDft > 0 AND
057100030822     C                             posADft  > 0
057200040708     C* POE
057300040708     C                   IF        %subst(
057400040708     C                             %subst(vlrppt:posDaDft+1:
057500040708     C                             posADft-posDaDft-1):1:3)
057600040708     C                             = 'POE'
057700040708     C                   EVAL      PiStr=%trim(%subst(
057800040708     C                             %subst(vlrppt:posDaDft+1:
057900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000040708     C                   EXSR      CHKNUM
058100040708     C                   IF        PiInt=*on
058200040708     C                   Z-ADD     PiVal         VAOPOE
058300040708     C                   ENDIF
058400040708     C                   ENDIF
058500040708     C* TOR
058600040708     C                   IF        %subst(
058700040708     C                             %subst(vlrppt:posDaDft+1:
058800040708     C                             posADft-posDaDft-1):1:3)
058900040708     C                             = 'TOR'
059000040708     C                   EVAL      VAOTOR=%trim(%subst(
059100040708     C                             %subst(vlrppt:posDaDft+1:
059200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300040708     C                   ENDIF
059400040708     C* TCO
059500040708     C                   IF        %subst(
059600040708     C                             %subst(vlrppt:posDaDft+1:
059700040708     C                             posADft-posDaDft-1):1:3)
059800040708     C                             = 'TCO'
059900040708     C                   EVAL      VAOTCO=%trim(%subst(
060000040708     C                             %subst(vlrppt:posDaDft+1:
060100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200040708     C                   ENDIF
060300040707     C* COR
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700040707     C                             = 'COR'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300040708     C                   Z-ADD     PiVal         VAOCOR
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600040708     C* CRC
061700040708     C                   IF        %subst(
061800040708     C                             %subst(vlrppt:posDaDft+1:
061900040708     C                             posADft-posDaDft-1):1:3)
062000040708     C                             = 'CRC'
062100040708     C                   EVAL      PiStr=%trim(%subst(
062200040708     C                             %subst(vlrppt:posDaDft+1:
062300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400040708     C                   EXSR      CHKNUM
062500040708     C                   IF        PiInt=*on
062600040708     C                   Z-ADD     PiVal         VAOCRC
062700040708     C                   ENDIF
062800040708     C                   ENDIF
062900040708     C* FFD
063000040708     C                   IF        %subst(
063100040708     C                             %subst(vlrppt:posDaDft+1:
063200040708     C                             posADft-posDaDft-1):1:3)
063300040708     C                             = 'FFD'
063400040708     C                   EVAL      VAOFFD=%trim(%subst(
063500040708     C                             %subst(vlrppt:posDaDft+1:
063600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700040708     C                   ENDIF
063800040708     C* ORR
063900040708     C                   IF        %subst(
064000040708     C                             %subst(vlrppt:posDaDft+1:
064100040708     C                             posADft-posDaDft-1):1:3)
064200040708     C                             = 'ORR'
064300040708     C                   EVAL      PiStr=%trim(%subst(
064400040708     C                             %subst(vlrppt:posDaDft+1:
064500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600040708     C                   EXSR      CHKNUM
064700040708     C                   IF        PiInt=*on
064800040708     C                   Z-ADD     PiVal         VAOORR
064900040708     C                   ENDIF
065000040708     C                   ENDIF
065100040712     C* NCL
065200040712     C                   IF        %subst(
065300040712     C                             %subst(vlrppt:posDaDft+1:
065400040712     C                             posADft-posDaDft-1):1:3)
065500040712     C                             = 'NCL'
065600040712     C                   EVAL      PiStr=%trim(%subst(
065700040712     C                             %subst(vlrppt:posDaDft+1:
065800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900040712     C                   EXSR      CHKNUM
066000040712     C                   IF        PiInt=*on
066100040712     C                   Z-ADD     PiVal         VAONCL
066200040712     C                   ENDIF
066300040712     C                   ENDIF
066400040712     C* PKG
066500040712     C                   IF        %subst(
066600040712     C                             %subst(vlrppt:posDaDft+1:
066700040712     C                             posADft-posDaDft-1):1:3)
066800040712     C                             = 'PKG'
066900040712     C                   EVAL      PiStr=%trim(%subst(
067000040712     C                             %subst(vlrppt:posDaDft+1:
067100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200040712     C                   EXSR      CHKNUM
067300040712     C                   IF        PiNum=*on
067400040712     C                   Z-ADD     PiVal         VAOPKG
067500040712     C                   ENDIF
067600040712     C                   ENDIF
067700040712     C* VLM
067800040712     C                   IF        %subst(
067900040712     C                             %subst(vlrppt:posDaDft+1:
068000040712     C                             posADft-posDaDft-1):1:3)
068100040712     C                             = 'VLM'
068200040712     C                   EVAL      PiStr=%trim(%subst(
068300040712     C                             %subst(vlrppt:posDaDft+1:
068400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500040712     C                   EXSR      CHKNUM
068600040712     C                   IF        PiNum=*on
068700040712     C                   Z-ADD     PiVal         VAOVLM
068800040712     C                   ENDIF
068900040712     C                   ENDIF
069000040708     C* NAM
069100040708     C                   IF        %subst(
069200040708     C                             %subst(vlrppt:posDaDft+1:
069300040708     C                             posADft-posDaDft-1):1:3)
069400040708     C                             = 'NAM'
069500040708     C                   EVAL      VAONAM=%trim(%subst(
069600040708     C                             %subst(vlrppt:posDaDft+1:
069700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800040708     C                   ENDIF
069900040708     C* PAG
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300040708     C                             = 'PAG'
070400040708     C                   EVAL      VAOPAG=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800040708     C* KSC
070900040708     C                   IF        %subst(
071000040708     C                             %subst(vlrppt:posDaDft+1:
071100040708     C                             posADft-posDaDft-1):1:3)
071200040708     C                             = 'KSC'
071300040708     C                   EVAL      PiStr=%trim(%subst(
071400040708     C                             %subst(vlrppt:posDaDft+1:
071500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600040708     C                   EXSR      CHKNUM
071700040708     C                   IF        PiInt=*on
071800040708     C                   Z-ADD     PiVal         VAOKSC
071900040708     C                   ENDIF
072000040708     C                   ENDIF
072100040708     C* CTR
072200040708     C                   IF        %subst(
072300040708     C                             %subst(vlrppt:posDaDft+1:
072400040708     C                             posADft-posDaDft-1):1:3)
072500040708     C                             = 'CTR'
072600040708     C                   EVAL      VAOCTR=%trim(%subst(
072700040708     C                             %subst(vlrppt:posDaDft+1:
072800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900040708     C                   ENDIF
073000040708     C* POC
073100040708     C                   IF        %subst(
073200040708     C                             %subst(vlrppt:posDaDft+1:
073300040708     C                             posADft-posDaDft-1):1:3)
073400040708     C                             = 'POC'
073500040708     C                   EVAL      PiStr=%trim(%subst(
073600040708     C                             %subst(vlrppt:posDaDft+1:
073700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800040708     C                   EXSR      CHKNUM
073900040708     C                   IF        PiInt=*on
074000040708     C                   Z-ADD     PiVal         VAOPOC
074100040708     C                   ENDIF
074200040708     C                   ENDIF
074300040708     C* NO1
074400040708     C                   IF        %subst(
074500040708     C                             %subst(vlrppt:posDaDft+1:
074600121129     C                             posADft-posDaDft-1):1:3)
074700040708     C                             = 'NO1'
074800040708     C                   EVAL      VAONO1=%trim(%subst(
074900040708     C                             %subst(vlrppt:posDaDft+1:
075000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100040708     C                   ENDIF
075200040708     C* DDT
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600040708     C                             = 'DDT'
075700040708     C                   EVAL      VAODDT=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100040708     C* RFA
076200040708     C                   IF        %subst(
076300040708     C                             %subst(vlrppt:posDaDft+1:
076400040708     C                             posADft-posDaDft-1):1:3)
076500040708     C                             = 'RFA'
076600040708     C                   EVAL      VAORFA=%trim(%subst(
076700040708     C                             %subst(vlrppt:posDaDft+1:
076800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900040708     C                   ENDIF
077000040708     C* FLO
077100040708     C                   IF        %subst(
077200040708     C                             %subst(vlrppt:posDaDft+1:
077300040708     C                             posADft-posDaDft-1):1:3)
077400040708     C                             = 'FLO'
077500040708     C                   EVAL      VAOFLO=%trim(%subst(
077600040708     C                             %subst(vlrppt:posDaDft+1:
077700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800091103     C*
077900040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
078000080702     C* e posizione 7 del VAOFLO
078100040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
078200040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
078300050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
078400040708     C                   ENDIF
078500030822     C* ...
078600030822     C                   ENDIF
078700030822     C                   ENDDO
078800020204     C*
078900010330     C                   ENDSR
079000010607     C*----------------------------------------------------*
079100040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
079200010607     C*----------------------------------------------------*
079300040708     C     IMPVAO        BEGSR
079400010607     C*
079500010607     C                   SETOFF                                       3132
079600120124
079700030515     C*
079800040708     C                   MOVEL     datcor        VAODAO
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
081300040709     C                                      %trim(intcol(3))
081400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081500050314     C**!!!minu:maiu     XLATE     vindta        vindta
081600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081700040324     C                   DOW       posDa <= %len(%trimr(vindta))
081800030715     C*
081900030715     C* Gestisco il 1° campo
082000030715     C                   IF        i = 1
082100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082200030715     C                   EVAL      posDa = 1
082300030715     C* Eseguo lo scan x trovare la fine del primo campo
082400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C* X i campi successivi al 1°
082900030715     C                   ELSE
083000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083100030715     C                   EVAL      posDa = posA + 1
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600030715     C                   ENDIF
084700030715     C* Incremento il contatore d campo
084800030715     C                   EVAL      i = i +1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085200030715     C* delimitazione testo
085300030715     C                   EVAL      i = 1
085400030715     C                   DOW       i < %elem(SkSplitFLD)
085500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085700030715     C                   EVAL      i = i + 1
085800030715     C                   ENDDO
085900030715     C                   EVAL      wGiro = 1
086000040709     C* Ripristino la 1° riga originale
086100040709     C                   EVAL      vinDTA = depvinDTA
086200030715     C                   ELSE
086300030715     C                   EVAL      wGiro = 2
086400031204     C**********
086500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086600031204     C* d separatore campo
086700031204     C**********
086800040709     C                   EVAL      vindta = %trim(vindta)
086900040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087000040709     C                   EVAL      vindta = %subst(vindta:2)
087100040709     C                   ENDDO
087200040709     C*
087300040709     C                   Z-ADD     *zeros        lunghInput        4 0
087400040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087500040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087600040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087700040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087800040709     C                   ENDDO
087900030715     C**********
088000030715     C* Effettuo lo split del campo dati d input
088100030715     C**********
088200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088300040324     C                   DOW       posDa <= %len(%trimr(vindta))
088400030715     C*
088500030715     C* Gestisco il 1° campo
088600030715     C                   IF        i = 1
088700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088800030715     C                   EVAL      posDa = 1
088900030715     C* Eseguo lo scan x trovare la fine del primo campo
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C* X i campi successivi al 1°
089500030715     C                   ELSE
089600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089700030715     C                   EVAL      posDa = posA + 1
089800031204     C* Verifico che nn vi sia il campo nn valorizzato
089900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090000031204     C* Se campo nn valorizzato skippo al prossimo
090100031204     C                   EVAL      SkSplitCSV(i) = *blanks
090200031204     C                   EVAL      posA  = posA + 1
090300031204     C                   ELSE
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800031204     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitCSV)
092800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C*
093700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093900030715     C                             SkSplitCSV(i) <> *blanks
094000030721     C*** ATB
094100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
094200040708     C                   EVAL      VAOATB = SkSplitCSV(i)
094300030721     C                   ENDIF
094400040708     C*** TOR
094500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
094600040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
094700040708     C                   ENDIF
094800040708     C*** TCO
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
095000040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
095100040708     C                   ENDIF
095200040708     C*** RSO
095300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
095400040708     C                   EVAL      VAORSO = SkSplitCSV(i)
095500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095600040708     C     '@':'A'       XLATE     VAORSO        VAORSO
095700030715     C* ==
095800030715     C                   ENDIF
095900040708     C*** INO
096000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
096100040708     C                   EVAL      VAOINO = SkSplitCSV(i)
096200030715     C                   ENDIF
096300040708     C*** LOO
096400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
096500040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
096600030715     C                   ENDIF
096700040708     C*** PRO
096800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
096900040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
097000030715     C                   ENDIF
097100040708     C*** NAO
097200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
097300040708     C                   EVAL      VAONAO = SkSplitCSV(i)
097400030721     C                   ENDIF
097500040708     C*** RSR
097600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
097700040708     C                   EVAL      VAORSR = SkSplitCSV(i)
097800040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097900040708     C     '@':'A'       XLATE     VAORSR        VAORSR
098000040708     C* ==
098100040708     C                   IF        VAORSR = *blanks
098200040708     C                   SETON                                        31
098300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098400040708     C                             + ' ' + 'VAORSR'
098500040708     C                   ENDIF
098600040708     C                   ENDIF
098700040708     C*** INR
098800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
098900040708     C                   EVAL      VAOINR = SkSplitCSV(i)
099000040708     C                   IF        VAOINR = *blanks
099100040708     C                   SETON                                        31
099200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099300040708     C                             + ' ' + 'VAOINR'
099400040708     C                   ENDIF
099500040708     C                   ENDIF
099600040708     C*** LOR
099700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
099800040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
099900040708     C                   IF        VAOLOR = *blanks
100000040708     C                   SETON                                        31
100100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100200040708     C                             + ' ' + 'VAOLOR'
100300040708     C                   ENDIF
100400040708     C                   ENDIF
100500040708     C*** PRR
100600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
100700040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
100800040708     C                   IF        VAOPRR = *blanks
100900040708     C                   SETON                                        31
101000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
101100040708     C                             + ' ' + 'VAOPRR'
101200040708     C                   ENDIF
101300040708     C                   ENDIF
101400040708     C*** NAR
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
101600040708     C                   EVAL      VAONAR = SkSplitCSV(i)
101700040708     C                   ENDIF
101800040708     C*** RER
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
102000040708     C                   EVAL      VAORER = SkSplitCSV(i)
102100040708     C                   ENDIF
102200040708     C*** TER
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
102400040708     C                   EVAL      VAOTER = SkSplitCSV(i)
102500040708     C                   ENDIF
102600040708     C*** RSC
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
102800040708     C                   EVAL      VAORSC = SkSplitCSV(i)
102900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103000040708     C     '@':'A'       XLATE     VAORSC        VAORSC
103100040708     C* ==
103200040708     C                   ENDIF
103300040708     C*** INC
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
103500040708     C                   EVAL      VAOINC = SkSplitCSV(i)
103600040708     C                   ENDIF
103700040708     C*** LOC
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
103900040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
104000040708     C                   ENDIF
104100040708     C*** PRC
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
104300040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
104400040708     C                   ENDIF
104500040708     C*** NAC
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
104700040708     C                   EVAL      VAONAC = SkSplitCSV(i)
104800040708     C                   ENDIF
104900040708     C*** FFD
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
105100040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
105200040708     C                   ENDIF
105300040708     C*** RMP
105400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
105500040708     C                   EVAL      VAORMP = SkSplitCSV(i)
105600040708     C                   ENDIF
105700040708     C*** NAM
105800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
105900040708     C                   EVAL      VAONAM = SkSplitCSV(i)
106000040708     C                   ENDIF
106100040708     C*** PAG
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
106300040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
106400040708     C                   ENDIF
106500040708     C*** CTR
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
106700040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
106800040708     C                   ENDIF
106900040708     C*** ZOR
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
107100040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
107200040708     C                   ENDIF
107300040708     C*** NO1
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
107500040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
107600030715     C                   ENDIF
107700040708     C*** NO2
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
107900040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
108000030715     C                   ENDIF
108100040708     C*** DDT
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
108300040708     C                   EVAL      VAODDT = SkSplitCSV(i)
108400030715     C                   ENDIF
108500040708     C*** RFA
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
108700040708     C                   EVAL      VAORFA = SkSplitCSV(i)
108800030715     C                   ENDIF
108900040708     C*** SPI
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
109100040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
109200030715     C                   ENDIF
109300040708     C*** FLO
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
109500040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
109600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
109700080708     C* e la posizione 7 del VAOFLO
109800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
109900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
110000050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
110100030715     C                   ENDIF
110200091103     C
110300091103     C*** DAR
110400050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
110500050412     C                   EVAL      PiStr=SkSplitCSV(i)
110600050412     C                   EXSR      CHKNUM
110700050412     C                   IF        PiInt=*on
110800050412     C                   Z-ADD     PiVal         VAODAR
110900050412     C                   ELSE
111000050412     C                   SETON                                        32
111100050412     C                   EVAL      VAODAR = *zeros
111200050412     C                   EVAL      vinmsg = %trimr(vinmsg)
111300050412     C                             + ' ' + 'VAODAR'
111400050412     C                   ENDIF
111500050314     C                   ENDIF
111600030715     C
111700030721     C*
111800030721     C* Reperisco quindi i campi numerici...
111900040708     C*** POE
112000040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
112100040723     C                   EVAL      PiStr=SkSplitCSV(i)
112200040723     C                   EXSR      CHKNUM
112300040723     C                   IF        PiInt=*on    AND
112400040723     C                             PiVal<=999   AND
112500040723     C                             PiVal>*zeros
112600040723     C                   Z-ADD     PiVal         VAOPOE
112700040723     C                   ELSE
112800040723     C                   SETON                                        32
112900040723     C                   EVAL      VAOPOE = *zeros
113000040723     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040723     C                             + ' ' + 'VAOPOE'
113200040723     C                   ENDIF
113300040723     C                   ENDIF
113400040708     C*** COR
113500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
113600040708     C                   EVAL      PiStr=SkSplitCSV(i)
113700040708     C                   EXSR      CHKNUM
113800040708     C                   IF        PiInt=*on
113900040708     C                   Z-ADD     PiVal         VAOCOR
114000040708     C                   ELSE
114100040708     C                   SETON                                        31
114200040708     C                   EVAL      VAOCOR = *zeros
114300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
114400040708     C                             + ' ' + 'VAOCOR'
114500040708     C                   ENDIF
114600040708     C                   ENDIF
114700040708     C*** CAO
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on
115200030721     C                   Z-ADD     PiVal         Num5_0
115300040708     C                   MOVEL(p)  Num5_0        VAOCAO
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600040708     C                   EVAL      VAOCAO = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800040708     C                             + ' ' + 'VAOCAO'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100040708     C*** CRA
116200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on
116600040708     C                   Z-ADD     PiVal         VAOCRA
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900040708     C                   EVAL      VAOCRA = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040708     C                             + ' ' + 'VAOCRA'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400040708     C*** CAR
117500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
117600040708     C                   EVAL      PiStr=SkSplitCSV(i)
117700040708     C                   EXSR      CHKNUM
117800040708     C                   IF        PiInt=*on
117900040708     C                   Z-ADD     PiVal         Num5_0
118000040708     C                   MOVEL(p)  Num5_0        VAOCAR
118100040708     C                   ELSE
118200040708     C                   SETON                                        31
118300040708     C                   EVAL      VAOCAR = *zeros
118400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040708     C                             + ' ' + 'VAOCAR'
118600040708     C                   ENDIF
118700040708     C                   ENDIF
118800040708     C*** CRC
118900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
119000040708     C                   EVAL      PiStr=SkSplitCSV(i)
119100040708     C                   EXSR      CHKNUM
119200040708     C                   IF        PiInt=*on
119300040708     C                   Z-ADD     PiVal         VAOCRC
119400040708     C                   ELSE
119500040708     C                   SETON                                        32
119600040708     C                   EVAL      VAOCRC = *zeros
119700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119800040708     C                             + ' ' + 'VAOCRC'
119900040708     C                   ENDIF
120000040708     C                   ENDIF
120100040708     C*** CAC
120200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
120300040708     C                   EVAL      PiStr=SkSplitCSV(i)
120400040708     C                   EXSR      CHKNUM
120500040708     C                   IF        PiInt=*on
120600040708     C                   Z-ADD     PiVal         Num5_0
120700040708     C                   MOVEL(p)  Num5_0        VAOCAC
120800040708     C                   ELSE
120900040708     C                   SETON                                        32
121000040708     C                   EVAL      VAOCAC = *zeros
121100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121200040708     C                             + ' ' + 'VAOCAC'
121300040708     C                   ENDIF
121400040708     C                   ENDIF
121500040708     C*** DAR
121600090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
121700090428     C                   EVAL      PiStr=SkSplitCSV(i)
121800090428     C                   EXSR      CHKNUM
121900090428     C                   IF        PiInt=*on
122000090428     C                   Z-ADD     PiVal         VAODAR
122100090428     C                   ELSE
122200090428     C                   SETON                                        32
122300090428     C                   EVAL      VAODAR = *zeros
122400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
122500090428     C                             + ' ' + 'VAODAR'
122600090428     C                   ENDIF
122700090428     C                   ENDIF
122800040708     C*** ORR
122900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
123000040708     C                   EVAL      PiStr=SkSplitCSV(i)
123100040708     C                   EXSR      CHKNUM
123200040708     C                   IF        PiInt=*on
123300040708     C                   Z-ADD     PiVal         VAOORR
123400040708     C                   ELSE
123500040708     C                   SETON                                        32
123600040708     C                   EVAL      VAOORR = *zeros
123700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123800040708     C                             + ' ' + 'VAOORR'
123900040708     C                   ENDIF
124000040708     C                   ENDIF
124100030721     C*** NCL
124200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on
124600040723     C                   IF        PiVal > *zeros
124700040708     C                   Z-ADD     PiVal         VAONCL
124800040723     C                   ENDIF
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100040708     C                   EVAL      VAONCL = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040708     C                             + ' ' + 'VAONCL'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600040708     C*** PKG
125700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiNum=*on
126100040723     C                   IF        PiVal > *zeros
126200040708     C                   Z-ADD     PiVal         VAOPKG
126300040723     C                   ENDIF
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600040708     C                   EVAL      VAOPKG = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800040708     C                             + ' ' + 'VAOPKG'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100040708     C*** VLM
127200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
127300040708     C                   EVAL      PiStr=SkSplitCSV(i)
127400040708     C                   EXSR      CHKNUM
127500040708     C                   IF        PiNum=*on
127600040708     C                   Z-ADD     PiVal         VAOVLM
127700040708     C                   ELSE
127800040708     C                   SETON                                        32
127900040708     C                   EVAL      VAOVLM = *zeros
128000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128100040708     C                             + ' ' + 'VAOVLM'
128200040708     C                   ENDIF
128300040708     C                   ENDIF
128400040708     C*** BNC
128500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
128600040708     C                   EVAL      PiStr=SkSplitCSV(i)
128700040708     C                   EXSR      CHKNUM
128800040708     C                   IF        PiInt=*on
128900040708     C                   Z-ADD     PiVal         VAOBNC
129000040708     C                   ELSE
129100040708     C                   SETON                                        32
129200040708     C                   EVAL      VAOBNC = *zeros
129300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129400040708     C                             + ' ' + 'VAOBNC'
129500040708     C                   ENDIF
129600040708     C                   ENDIF
129700040708     C*** BLC
129800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
129900040708     C                   EVAL      PiStr=SkSplitCSV(i)
130000040708     C                   EXSR      CHKNUM
130100040708     C                   IF        PiInt=*on
130200040708     C                   Z-ADD     PiVal         VAOBLC
130300040708     C                   ELSE
130400040708     C                   SETON                                        32
130500040708     C                   EVAL      VAOBLC = *zeros
130600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130700040708     C                             + ' ' + 'VAOBLC'
130800040708     C                   ENDIF
130900040708     C                   ENDIF
131000040708     C*** ATT
131100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
131200040708     C                   EVAL      PiStr=SkSplitCSV(i)
131300040708     C                   EXSR      CHKNUM
131400040708     C                   IF        PiInt=*on
131500040708     C                   Z-ADD     PiVal         VAOATT
131600040708     C                   ELSE
131700040708     C                   SETON                                        32
131800040708     C                   EVAL      VAOATT = *zeros
131900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132000040708     C                             + ' ' + 'VAOATT'
132100040708     C                   ENDIF
132200040708     C                   ENDIF
132300040708     C*** MTC
132400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
132500040708     C                   EVAL      PiStr=SkSplitCSV(i)
132600040708     C                   EXSR      CHKNUM
132700040708     C                   IF        PiInt=*on
132800040708     C                   Z-ADD     PiVal         VAOMTC
132900040708     C                   ELSE
133000040708     C                   SETON                                        32
133100040708     C                   EVAL      VAOMTC = *zeros
133200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133300040708     C                             + ' ' + 'VAOMTC'
133400040708     C                   ENDIF
133500040708     C                   ENDIF
133600040708     C*** KSC
133700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on      AND
134100030721     C                             PiVal<=9999999 AND
134200030721     C                             PiVal>*zeros
134300040708     C                   Z-ADD     PiVal         VAOKSC
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600040708     C                   EVAL      VAOKSC = *zeros
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800040708     C                             + ' ' + 'VAOKSC'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100040708     C*** POR
135200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on    AND
135600030721     C                             PiVal<=999   AND
135700030721     C                             PiVal>*zeros
135800040708     C                   Z-ADD     PiVal         VAOPOR
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100040708     C                   EVAL      VAOPOR = *zeros
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300040708     C                             + ' ' + 'VAOPOR'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600040708     C*** POC
136700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
136800040708     C                   EVAL      PiStr=SkSplitCSV(i)
136900040708     C                   EXSR      CHKNUM
137000040708     C                   IF        PiInt=*on    AND
137100040708     C                             PiVal<=999   AND
137200040708     C                             PiVal>*zeros
137300040708     C                   Z-ADD     PiVal         VAOPOC
137400040708     C                   ELSE
137500040708     C                   SETON                                        32
137600040708     C                   EVAL      VAOPOC = *zeros
137700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137800040708     C                             + ' ' + 'VAOPOC'
137900040708     C                   ENDIF
138000040708     C                   ENDIF
138100040708     C*** STO
138200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
138300040708     C                   EVAL      PiStr=SkSplitCSV(i)
138400040708     C                   EXSR      CHKNUM
138500040708     C                   IF        PiInt=*on
138600040708     C                   Z-ADD     PiVal         VAOSTO
138700040708     C                   ELSE
138800040708     C                   SETON                                        32
138900040708     C                   EVAL      VAOSTO = *zeros
139000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139100040708     C                             + ' ' + 'VAOSTO'
139200040708     C                   ENDIF
139300040708     C                   ENDIF
139400090428     C*
139500091103     C********** GESTIONE CAMPI PERSONALIZZATI
139600120120
139700130513     C*** W_DAR
139800130513     C* la data arriva in formato ISO dd/mm/yyyy
139900130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_DAR'
140000130516     C* il cliente invia la data in formato dd/mm/yyyy , cioé in formato 'simil' *eur,
140100130513     C* la conversione diretta sarebbe fattibile, ma il formato *eur prevede come
140200130513     C* separatore SOLO il punto(.), per cui prima converto il separatore nel punto
140300130513     C                   EVAL      w10      = %xlate('/-' : '..' :
140400130513     C                                     %trim(SkSplitCSV(i)) )
140500130513     C                   EVAL      VAODAR   = %dec(%date(w10     :*eur):*iso)
140600130513     C                   ENDIF
140700130513
140800130513     C*** W_RSR_*
140900130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSR_1'
141000130513     C                   EVAL      wRSR1 = *blank
141100130513     C                   EVAL      wRSR1 = %trim(SkSplitCSV(i))
141200130513     C                   ENDIF
141300130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSR_2'
141400130513     C                   EVAL      wRSR2 = *blank
141500130513     C                   EVAL      wRSR2 = %trim(SkSplitCSV(i))
141600130513     C                   ENDIF
141700130614
141800130614     C*** W_RSC_*
141900130614     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSC'
142000171024     C***                IF        %trim(SkSplitCSV(i)) = '3' OR
142100170629     C***                          %trim(SkSplitCSV(i)) = '8' OR
142200171024     C***                          %trim(SkSplitCSV(i)) = '1'
142300171024     C***                EVAL      VAORSC = 'NESPRESSO C/O CHEMELLO'
142400171024     C***                EVAL      VAOINC = 'VIA BIRON DI SOTTO 29/C'
142500171024     C***                EVAL      VAOCAC = '36100'
142600171024     C***                EVAL      VAOLOC = 'VICENZA'
142700171024     C***                EVAL      VAOPRC = 'VI'
142800171024     C***                ENDIF
142900171024     C                   IF        %trim(SkSplitCSV(i)) = '2'
143000171024     C                   EVAL      VAORSC = 'NESPRESSO C/O LOGISTICA BRT'
143100171024     C                   EVAL      VAOINC = 'LOCALITA CASCINA SCARAVELLA'
143200171024     C                   EVAL      VAOCAC = '20018'
143300171024     C                   EVAL      VAOLOC = 'SEDRIANO'
143400171024     C                   EVAL      VAOPRC = 'MI'
143500171024     C                   ENDIF
143600170629     C                   IF        %trim(SkSplitCSV(i)) = '8'
143700171024     C                   EVAL      VAORSC =
143800170629     C                                      'ZABARDI SERVICE SRL'
143900170629     C                   EVAL      VAOINC = 'VIA SAGRA DI SAN MICHELE 21/A'
144000170629     C                   EVAL      VAOCAC = '10139'
144100170629     C                   EVAL      VAOLOC = 'TORINO'
144200170629     C                   EVAL      VAOPRC = 'TO'
144300171024     C                   EVAL      VAOCOR = 0507048000
144400171024     C                   EVAL      VAOKSC = 0507048
144500170629     C                   ENDIF
144600170629     C                   IF        %trim(SkSplitCSV(i)) = '11'
144700171024     C                   EVAL      VAORSC = 'ZABARDI SERVICE SRL'
144800171024     C                   EVAL      VAOINC = 'VIA SAGRA DI SAN MICHELE 21/A'
144900170117     C                   EVAL      VAOCAC = '10139'
145000170117     C                   EVAL      VAOLOC = 'TORINO'
145100170117     C                   EVAL      VAOPRC = 'TO'
145200170117     C                   EVAL      VAOCOR = 0507048000
145300170117     C                   EVAL      VAOKSC = 0507048
145400170117     C                   ENDIF
145500171024     C                   IF        %trim(SkSplitCSV(i)) = '17'
145600171024     C                   EVAL      VAORSC =
145700171024     C                                      'ZABARDI SERVICE SRL'
145800171024     C                   EVAL      VAOINC = 'VIA SAGRA DI SAN MICHELE 21/A'
145900171024     C                   EVAL      VAOCAC = '10139'
146000171024     C                   EVAL      VAOLOC = 'TORINO'
146100171024     C                   EVAL      VAOPRC = 'TO'
146200171024     C                   EVAL      VAOCOR = 0507048000
146300171024     C                   EVAL      VAOKSC = 0507048
146400171024     C                   ENDIF
146500171024     C                   IF        %trim(SkSplitCSV(i)) = '19'
146600171024     C                   EVAL      VAORSC = 'RELECO 2'
146700171024     C                   EVAL      VAOINC = 'VIA CREMASCA 2'
146800171024     C                   EVAL      VAOCAC = '24052'
146900171024     C                   EVAL      VAOLOC = 'AZZANO SAN PAOLO'
147000171024     C                   EVAL      VAOPRC = 'BG'
147100171024     C                   ENDIF
147200171130     C                   IF        %trim(SkSplitCSV(i)) = '21'
147300171130     C                   EVAL      VAORSC = 'ZABARDI SERVICE SRL'
147400171130     C                   EVAL      VAOINC = 'VIA SAGRA DI SAN MICHELE 21/A'
147500171130     C                   EVAL      VAOCAC = '10139'
147600171130     C                   EVAL      VAOLOC = 'TORINO'
147700171130     C                   EVAL      VAOPRC = 'TO'
147800171204     C                   EVAL      VAOCOR = 0507351000
147900171204     C                   EVAL      VAOKSC = 0507351
148000171130     C                   ENDIF
148100171130     C                   IF        %trim(SkSplitCSV(i)) = '20'
148200171130     C                   EVAL      VAORSC = 'RELECO 2'
148300171130     C                   EVAL      VAOINC = 'VIA CREMASCA 2'
148400171130     C                   EVAL      VAOCAC = '24052'
148500171130     C                   EVAL      VAOLOC = 'AZZANO SAN PAOLO'
148600171130     C                   EVAL      VAOPRC = 'BG'
148700171204     C                   EVAL      VAOCOR = 0507351000
148800171204     C                   EVAL      VAOKSC = 0507351
148900171130     C                   ENDIF
149000171024     C                   ENDIF
149100130513
149200130516     C*** W_INR*
149300130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_INR_1'
149400130513     C                   EVAL      wINR1 = *blank
149500130513     C                   EVAL      wINR1 = %trim(SkSplitCSV(i))
149600130513     C                   ENDIF
149700130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_INR_2'
149800130513     C                   EVAL      wINR2 = *blank
149900130513     C                   EVAL      wINR2 = %trim(SkSplitCSV(i))
150000130513     C                   ENDIF
150100130516
150200130516     C*** W_NO1
150300130516     C                   IF        %trim(SkSplitFLD(i)) = 'W_NO1'
150400130719     C***                MOVEL     *blanks       wNOT             70
150500130719     C***                EVAL      wNOT=SkSplitCSV(i)
150600130719     C***                EVAL      VAONO1=%subst(wNOT:1:35)
150700130719     C***                EVAL      VAONO2=%subst(wNOT:36:35)
150800130719     C                   EVAL      VAONO1=SkSplitCSV(i)
150900130516     C                   ENDIF
151000130719
151100130719     C*** W_NO2
151200130719     C                   IF        %trim(SkSplitFLD(i)) = 'W_NO2'
151300130719     C***                MOVEL     *blanks       wNOT             70
151400130719     C***                EVAL      wNOT=SkSplitCSV(i)
151500130719     C***                EVAL      VAONO1=%subst(wNOT:1:35)
151600130719     C***                EVAL      VAONO2=%subst(wNOT:36:35)
151700130719     C                   EVAL      VAONO2=SkSplitCSV(i)
151800130719     C                   ENDIF
151900121207
152000090428     C*
152100090428     C****************************************
152200090428     C*
152300030715     C*
152400030715     C                   ENDIF
152500030715     C                   EVAL      i = i + 1
152600030715     C                   ENDDO
152700040723     C*
152800121207     C***  ===>  Gestione campi "particolari" fuori ciclo
152900121207     C*
153000130513
153100130516     C                   EVAL      VAORSR = %trim(wRSR1)
153200130516     C                   IF        wRSR2 <> *blank
153300130516     C                   EVAL      VAORSR = %trim(VAORSR) + ' - ' + %trim(wRSR2)
153400130516     C                   ENDIF
153500130513     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
153600130513     C     '@':'A'       XLATE     VAORSR        VAORSR
153700130513     C                   EVAL      wRSR1 = *blank
153800130513     C                   EVAL      wRSR2 = *blank
153900130513
154000130513     C                   EVAL      VAOINR = %trim(wINR1) + ' ' + %trim(wINR2)
154100130513     C                   EVAL      wINR1 = *blank
154200130513     C                   EVAL      wINR2 = *blank
154300130513
154400030715     C                   ENDIF
154500020204     C*
154600010607     C                   ENDSR
154700010607     C*----------------------------------------------------*
154800080702
154900080702
155000080702     C*----------------------------------------------------*
155100080702     C*  CONTROLLO NUMERICITA' CAMPI
155200080702     C*----------------------------------------------------*
155300080702     C     CHKNUM        BEGSR
155400080702     C*
155500080702     C                   IF        PiDecChr = *blanks
155600080702     C                   EVAL      PiDecChr = ','
155700080702     C                   ENDIF
155800080702     C*
155900080702     C                   callp     UBISNUM_Check(PiStr
156000080702     C                                          :PiDecChr
156100080702     C                                          :PiVal
156200080702     C                                          :PiNum
156300080702     C                                          :PiInt)
156400080702     C*
156500080702     C                   ENDSR
156600080702     C***
156700080702
156800010601
156900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157000010601     C     repfil        BEGSR
157100010601     C*
157200010601     C                   if        invfil = *zeros and
157300010601     C                             depfil > *zeros and
157400010629     C                             (vinflg = *blanks or
157500010629     C                              vinflg = *zeros)
157600010601     C
157700010601     C                   eval      invfil = depfil
157800010601     C                   endif
157900010601     C*
158000010601     C                   if        depfil <> invfil and
158100010601     C                             invfil > *zeros
158200010601     C                   eval      flgMulti = '1'
158300010601     C                   if        vinflg = *blanks
158400010601     C                   add       1             cntNonEl
158500010601     C                   endif
158600010601     C                   endif
158700010601     C*
158800010601     C                   if        vinflg = '2'
158900010601     C                   eval      flgStato = '2'
159000010601     C                   endif
159100010601     C*
159200010601     C                   ENDSR
159300010601     C***
159400010330
159500010330
159600990920      /TITLE Invio dei dati al punto operativo.
159700000613     C     invio         BEGSR
159800990920     C*
159900990920     C                   reset                   dscmz
160000010601     C                   move      invfil        cmzdst
160100990920     C                   eval      cmzfld = vlrfou
160200990920     C                   eval      cmzmbd = vlrhdl
160300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
160400000710     C                   if        prmfir = *blanks
160500040708     C                   eval      cmzfla = 'FNVAO00F'
160600040708     C                   eval      cmzmba = 'FNVAO00F'
160700000710     C                   else
160800000710     C                   eval      cmzfla = prmfir
160900000710     C                   eval      cmzmba = prmfir
161000000710     C                   endif
161100990920     C                   eval      cmznrr = *zeros
161200990920     C                   move      §ctrok        cmznrr
161300021018     C                   eval      cmzlba = vlrfl1
161400990920     C                   call(e)   'TIS711C'
161500990920     C                   parm                    dscmz
161600990921     C                   parm      *blanks       esito
161700990920     C                   if        %error
161800990920     C                             or cmzerr = '1'
161900990921     C                             or esito  = '1'
162000000710     C                   eval      wrkesito = '3'
162100990920     C                   endif
162200990920     C*
162300000613     C                   ENDSR
162400010601
162500010601
162600010601      /TITLE Invio dei dati al punto operativo.
162700010601     C     opeini        BEGSR
162800010601     C*
162900010601     C* Inizializzo flag e contatori operativi
163000010601     C                   movel     '0'           flgGiro           1
163100010601     C                   movel     '0'           flgMulti          1
163200010601     C                   movel     '1'           flgStato          1
163300010615     C                   movel     '0'           flgOk             1
163400010601     C                   z-add     *zeros        cntNonEl         10 0
163500010601     C                   z-add     *zeros        depfil            3 0
163600010601     C                   z-add     *zeros        invfil            3 0
163700010601     C*
163800010601     C                   ENDSR
163900010601     C***
164000050421
164100050421
164200091130     C     *pssr         BEGSR
164300050421     C*
164400091130     C                   eval      wrkesito = '2'
164500050421     C*
164600091130     C                   ENDSR     '*CANCL'
164700050421     C***
164800060306
164900060306
165000060306     C*-------------------------------------------------------------------------*
165100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
165200060306     C*-------------------------------------------------------------------------*
165300060306     C     sr_Control    begsr
165400060306     C*
165500060306     C* inizializzo a ok il flag d controllo abilitazione
165600060306     C                   eval      Cli_Abil = *on
165700060306     C
165800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
165900060306     C                   clear                   woriIDC
166000060306     C                   eval      kteCOD = 'VAP'
166100060306     C                   eval      kteKE1 = oriIDC
166200060306     C     keytbe        chain     TNTBE01L
166300060306     C                   if        %found(Tntbe01l)
166400060306     C                   eval      woriIDC = TbeKe2
166500060306     C                   else
166600060306     C                   eval      Cli_Abil = *off
166700060306     C                   endif
166800060306     C*
166900060306     C                   endsr
167000010330
167100010330
167200000613     C     *inzsr        BEGSR
167300990910     C*
167400990910     C     *entry        plist
167500990920     C                   parm                    tivlrds
167600990921     C                   parm      wrkesito      esito
167700000724     C                   parm                    prmlit
167800000710     C                   parm                    prmfir
167900010330     C*
168000010330     C* CALCOLA LA DATA CORRENTE
168100091127     C                   z-add     *zeros        datcor            8 0
168200091127     C                   time                    wn14             14 0
168300091127     C                   movel     wn14          wn6               6 0          *ORA (6)
168400130516     C                   eval      datcor = %dec(%date() : *ISO)
168500040723     C*
168600040723     C* Campi riferito al database
168700040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
168800040723     C     *like         define    tbeke1        kteKE1
168900040723     C*
169000040723     C* Definizione chiavi di lettura
169100040723     C* TNTBE01L - parziale
169200040723     C     keytbe        klist
169300040723     C                   kfld                    kteCOD                         *UTENTE
169400040723     C                   kfld                    kteKE1                         *CODICE
169500000613     C*
169600000613     C                   ENDSR
169700000613     C***
169800990908
169900040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170000130516W_DAR;X;X;X;X;X;VAORER;W_RSR_1;W_RSR_2;X;W_INR_1;W_INR_2;VAOCAR;VAOLOR;VAOPRR;
170100130719VAOTER;X;X;W_RSC;X;X;VAONAM;X;X;X;W_NO2;X;VAORFA;X;X;X;W_NO1;X;X;X;X;X;X;X;X;X;
170200130222
