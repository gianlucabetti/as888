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
042200131031     C                   EVAL      §ORMCB = 'N'
042300131031     C                   EVAL      §ORMFR = 'S'
042400131031     C                   EVAL      §ORMKS = 'N'
042500131031     C                   EVAL      §ORMFD = 'S'
042600050412     C*
042700050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
042800140117     C                   movel     'S'           §ormfr
042900140117     C                   movel     'S'           §ormks
043000140117     C                   movel     'S'           §ormfd
043100050412     C*
043200130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
043300040723     C                   eval      vaoFLO = dorm01
043400130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
043500130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
043600130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
043700040723     C*
043800040723     C* ...quindi scrivo il file TIORI00F
043900120905     C  N55              WRITE     TIORI000
044000040723     C*
044100040723     C                   ENDSR
044200050826
044300050826
044400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
044500050826     C     wriVGD        BEGSR
044600050826     C*
044700050826     C* Reperisco la descrizione della fase da tabella
044800050826     C                   movel(p)  'FAR'         KteCOD
044900050826     C                   movel(p)  '000'         KteKE1
045000050826     C     KEYtbe        chain     tntbe01l
045100050826     C                   if        %found(tntbe01l)
045200050826     C                   movel(P)  tbeuni        DFAR
045300050826     C                   else
045400050826     C                   clear                   DFAR
045500050826     C                   endif
045600050826     C*
045700050826     C* Valorizzo buffer record
045800050826     C                   clear                   FNVAPDS
045900050826     C                   eval      vapIDC = oriIDC
046000050826     C                   eval      vapPOE = oriPOE
046100050826     C                   eval      vapRFA = vaoRFA
046200050826     C                   eval      vapPOG = oriPOE
046300050826     C                   eval      vapDAE = oriDER
046400050826     C                   eval      vapORE = oriOER
046500050826     C                   if        d§fardva <> *blanks
046600050826     C                   eval      vapDFA = d§fardva
046700050826     C                   else
046800050826     C                   eval      vapDFA = d§fardes
046900050826     C                   endif
047000050826     C*
047100050826     C                   clear                   tivgd000
047200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
047300050826     C                   eval      vgdTIP = 'VP'
047400050826     C                   eval      vgdKSU = vapIDC
047500050826     C                   eval      vgdTSC = 'WW'
047600050826     C                   eval      vgdDAT = datcor
047700151207     C                   eval      vgdPGM = procname
047800060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
047900060306     C                   exsr      sr_Control
048000060306     C                   if        Cli_Abil = *on
048100060306     C                   eval      vgdKSU = woriIDC
048200120905     C  N55              write     tivgd000
048300060306     C                   endif
048400050826     C*
048500050826     C                   ENDSR
048600050826     C*------------------------------------------------------------------------*
048700010601
048800010601
048900010330     C*----------------------------------------------------*
049000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049100010330     C*----------------------------------------------------*
049200010330     C     INZVAR        BEGSR
049300010330     C*
049400030822     C* Inizializzo variabili di wrk
049500020204     C                   Z-ADD     *zeros        Num5_0            5 0
049600120124
049700030822     C*
049800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049900120123     C                   CLEAR                   FNVAO000
050000120123     C                   CLEAR                   TIORI000
050100030822     C                   CLEAR                   SkSplitCSV
050200030822     C                   EVAL      i = 1
050300030822     C                   EVAL      posDa = *zeros
050400030822     C                   EVAL      posA  = *zeros
050500030822     C*
050600030822     C* Reimposto i valori di default
050700030822     C                   EXSR      DEFCAM
050800030822     C*
050900010330     C                   ENDSR
051000010330     C*----------------------------------------------------*
051100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051200010330     C*----------------------------------------------------*
051300010330     C     DEFCAM        BEGSR
051400030822     C*
051500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051600030715     C* e delimitatore testo.
051700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052000040723     C*
052100080707     C* Imposto i valori d default d alcuni campi
052200080708     C*
052300080708     C                   CLEAR                   DORM01
052400120822     C                   EVAL      VAOFLO = DORM01
052500080707     C*
052600140117     C                   EVAL      VAOPOE = 166
052700140117     C                   EVAL      VAOTOR = 'S'
052800140117     C                   EVAL      VAOPAG = 'O'
052900140117     C                   EVAL      VAOTCO = 'F'
053000140117     C                   EVAL      VAOKSC = 1662078
053100130513
053200120905     C* se il 1° char dei parametri è T=Test
053300120905     C                   SETOFF                                       55
053400120905     C                   IF        %subst(vlrppt:1:1) = 'T'
053500120905     C* inibisco la scrittura dei file TIORI e TIVGD
053600120905     C                   SETON                                        55
053700120905     C* imposto il PO di emissione come filiale fittizia
053800120905     C                   EVAL      VAOPOE = 999
053900120905     C                   ENDIF
054000091127     C*
054100091204     C* IMPOSTO I CAMPI PERSONALIZZATI
054200100416     C
054300030822     C*
054400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054500030822     C                   EVAL      posDaDft = 1
054600030822     C                   EVAL      posADft  = 0
054700030822     C                   EVAL      wGiroDft = 0
054800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054900030822     C                             posDaDft > 0
055000030822     C*
055100030822     C* Gestisco il 1° giro
055200030822     C                   IF        wGiroDft = 0
055300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055500030822     C* Incremento il contatore dei "giri"
055600030822     C                   EVAL      wGiroDft = 1
055700030822     C                   ELSE
055800030822     C                   EVAL      posDaDft = posADft
055900030822     C                   ENDIF
056000030822     C* Eseguo lo scan x trovare la fine del campo corrente
056100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056200030822     C*
056300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056500030822     C                   IF        posDaDft > 0 AND
056600030822     C                             posADft  > 0
056700040708     C* POE
056800040708     C                   IF        %subst(
056900040708     C                             %subst(vlrppt:posDaDft+1:
057000040708     C                             posADft-posDaDft-1):1:3)
057100040708     C                             = 'POE'
057200040708     C                   EVAL      PiStr=%trim(%subst(
057300040708     C                             %subst(vlrppt:posDaDft+1:
057400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500040708     C                   EXSR      CHKNUM
057600040708     C                   IF        PiInt=*on
057700040708     C                   Z-ADD     PiVal         VAOPOE
057800040708     C                   ENDIF
057900040708     C                   ENDIF
058000040708     C* TOR
058100040708     C                   IF        %subst(
058200040708     C                             %subst(vlrppt:posDaDft+1:
058300040708     C                             posADft-posDaDft-1):1:3)
058400040708     C                             = 'TOR'
058500040708     C                   EVAL      VAOTOR=%trim(%subst(
058600040708     C                             %subst(vlrppt:posDaDft+1:
058700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800040708     C                   ENDIF
058900040708     C* TCO
059000040708     C                   IF        %subst(
059100040708     C                             %subst(vlrppt:posDaDft+1:
059200040708     C                             posADft-posDaDft-1):1:3)
059300040708     C                             = 'TCO'
059400040708     C                   EVAL      VAOTCO=%trim(%subst(
059500040708     C                             %subst(vlrppt:posDaDft+1:
059600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700040708     C                   ENDIF
059800040707     C* COR
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200040707     C                             = 'COR'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800040708     C                   Z-ADD     PiVal         VAOCOR
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100040708     C* CRC
061200040708     C                   IF        %subst(
061300040708     C                             %subst(vlrppt:posDaDft+1:
061400040708     C                             posADft-posDaDft-1):1:3)
061500040708     C                             = 'CRC'
061600040708     C                   EVAL      PiStr=%trim(%subst(
061700040708     C                             %subst(vlrppt:posDaDft+1:
061800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900040708     C                   EXSR      CHKNUM
062000040708     C                   IF        PiInt=*on
062100040708     C                   Z-ADD     PiVal         VAOCRC
062200040708     C                   ENDIF
062300040708     C                   ENDIF
062400040708     C* FFD
062500040708     C                   IF        %subst(
062600040708     C                             %subst(vlrppt:posDaDft+1:
062700040708     C                             posADft-posDaDft-1):1:3)
062800040708     C                             = 'FFD'
062900040708     C                   EVAL      VAOFFD=%trim(%subst(
063000040708     C                             %subst(vlrppt:posDaDft+1:
063100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200040708     C                   ENDIF
063300040708     C* ORR
063400040708     C                   IF        %subst(
063500040708     C                             %subst(vlrppt:posDaDft+1:
063600040708     C                             posADft-posDaDft-1):1:3)
063700040708     C                             = 'ORR'
063800040708     C                   EVAL      PiStr=%trim(%subst(
063900040708     C                             %subst(vlrppt:posDaDft+1:
064000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100040708     C                   EXSR      CHKNUM
064200040708     C                   IF        PiInt=*on
064300040708     C                   Z-ADD     PiVal         VAOORR
064400040708     C                   ENDIF
064500040708     C                   ENDIF
064600040712     C* NCL
064700040712     C                   IF        %subst(
064800040712     C                             %subst(vlrppt:posDaDft+1:
064900040712     C                             posADft-posDaDft-1):1:3)
065000040712     C                             = 'NCL'
065100040712     C                   EVAL      PiStr=%trim(%subst(
065200040712     C                             %subst(vlrppt:posDaDft+1:
065300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400040712     C                   EXSR      CHKNUM
065500040712     C                   IF        PiInt=*on
065600040712     C                   Z-ADD     PiVal         VAONCL
065700040712     C                   ENDIF
065800040712     C                   ENDIF
065900040712     C* PKG
066000040712     C                   IF        %subst(
066100040712     C                             %subst(vlrppt:posDaDft+1:
066200040712     C                             posADft-posDaDft-1):1:3)
066300040712     C                             = 'PKG'
066400040712     C                   EVAL      PiStr=%trim(%subst(
066500040712     C                             %subst(vlrppt:posDaDft+1:
066600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700040712     C                   EXSR      CHKNUM
066800040712     C                   IF        PiNum=*on
066900040712     C                   Z-ADD     PiVal         VAOPKG
067000040712     C                   ENDIF
067100040712     C                   ENDIF
067200040712     C* VLM
067300040712     C                   IF        %subst(
067400040712     C                             %subst(vlrppt:posDaDft+1:
067500040712     C                             posADft-posDaDft-1):1:3)
067600040712     C                             = 'VLM'
067700040712     C                   EVAL      PiStr=%trim(%subst(
067800040712     C                             %subst(vlrppt:posDaDft+1:
067900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000040712     C                   EXSR      CHKNUM
068100040712     C                   IF        PiNum=*on
068200040712     C                   Z-ADD     PiVal         VAOVLM
068300040712     C                   ENDIF
068400040712     C                   ENDIF
068500040708     C* NAM
068600040708     C                   IF        %subst(
068700040708     C                             %subst(vlrppt:posDaDft+1:
068800040708     C                             posADft-posDaDft-1):1:3)
068900040708     C                             = 'NAM'
069000040708     C                   EVAL      VAONAM=%trim(%subst(
069100040708     C                             %subst(vlrppt:posDaDft+1:
069200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300040708     C                   ENDIF
069400040708     C* PAG
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800040708     C                             = 'PAG'
069900040708     C                   EVAL      VAOPAG=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300040708     C* KSC
070400040708     C                   IF        %subst(
070500040708     C                             %subst(vlrppt:posDaDft+1:
070600040708     C                             posADft-posDaDft-1):1:3)
070700040708     C                             = 'KSC'
070800040708     C                   EVAL      PiStr=%trim(%subst(
070900040708     C                             %subst(vlrppt:posDaDft+1:
071000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100040708     C                   EXSR      CHKNUM
071200040708     C                   IF        PiInt=*on
071300040708     C                   Z-ADD     PiVal         VAOKSC
071400040708     C                   ENDIF
071500040708     C                   ENDIF
071600040708     C* CTR
071700040708     C                   IF        %subst(
071800040708     C                             %subst(vlrppt:posDaDft+1:
071900040708     C                             posADft-posDaDft-1):1:3)
072000040708     C                             = 'CTR'
072100040708     C                   EVAL      VAOCTR=%trim(%subst(
072200040708     C                             %subst(vlrppt:posDaDft+1:
072300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400040708     C                   ENDIF
072500040708     C* POC
072600040708     C                   IF        %subst(
072700040708     C                             %subst(vlrppt:posDaDft+1:
072800040708     C                             posADft-posDaDft-1):1:3)
072900040708     C                             = 'POC'
073000040708     C                   EVAL      PiStr=%trim(%subst(
073100040708     C                             %subst(vlrppt:posDaDft+1:
073200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300040708     C                   EXSR      CHKNUM
073400040708     C                   IF        PiInt=*on
073500040708     C                   Z-ADD     PiVal         VAOPOC
073600040708     C                   ENDIF
073700040708     C                   ENDIF
073800040708     C* NO1
073900040708     C                   IF        %subst(
074000040708     C                             %subst(vlrppt:posDaDft+1:
074100121129     C                             posADft-posDaDft-1):1:3)
074200040708     C                             = 'NO1'
074300040708     C                   EVAL      VAONO1=%trim(%subst(
074400040708     C                             %subst(vlrppt:posDaDft+1:
074500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600040708     C                   ENDIF
074700040708     C* DDT
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100040708     C                             = 'DDT'
075200040708     C                   EVAL      VAODDT=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600040708     C* RFA
075700040708     C                   IF        %subst(
075800040708     C                             %subst(vlrppt:posDaDft+1:
075900040708     C                             posADft-posDaDft-1):1:3)
076000040708     C                             = 'RFA'
076100040708     C                   EVAL      VAORFA=%trim(%subst(
076200040708     C                             %subst(vlrppt:posDaDft+1:
076300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400040708     C                   ENDIF
076500040708     C* FLO
076600040708     C                   IF        %subst(
076700040708     C                             %subst(vlrppt:posDaDft+1:
076800040708     C                             posADft-posDaDft-1):1:3)
076900040708     C                             = 'FLO'
077000040708     C                   EVAL      VAOFLO=%trim(%subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300091103     C*
077400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
077500080702     C* e posizione 7 del VAOFLO
077600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
077700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
077800050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
077900040708     C                   ENDIF
078000030822     C* ...
078100030822     C                   ENDIF
078200030822     C                   ENDDO
078300020204     C*
078400010330     C                   ENDSR
078500010607     C*----------------------------------------------------*
078600040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
078700010607     C*----------------------------------------------------*
078800040708     C     IMPVAO        BEGSR
078900010607     C*
079000010607     C                   SETOFF                                       3132
079100120124
079200030515     C*
079300040708     C                   MOVEL     datcor        VAODAO
079400040708     C                   MOVEL     wn14          VAOOAO
079500050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079600050314     C     minu:maiu     XLATE     vindta        vindta
079700010607     C*
079800030715     C**********
079900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080000030715     C**********
080100030715     C                   IF        wGiro = *zeros
080200040709     C*
080300040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080400040709     C                   EVAL      depvinDTA = vinDTA
080500040709     C*
080600040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
080700040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080800040709     C                                      %trim(intcol(3))
080900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081000050314     C**!!!minu:maiu     XLATE     vindta        vindta
081100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081200040324     C                   DOW       posDa <= %len(%trimr(vindta))
081300030715     C*
081400030715     C* Gestisco il 1° campo
081500030715     C                   IF        i = 1
081600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081700030715     C                   EVAL      posDa = 1
081800030715     C* Eseguo lo scan x trovare la fine del primo campo
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* A questo punto "estraggo" il campo corrente
082100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082200030715     C                                                    (posA-posDa))
082300030715     C* X i campi successivi al 1°
082400030715     C                   ELSE
082500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082600030715     C                   EVAL      posDa = posA + 1
082700030715     C* Eseguo lo scan x trovare la fine del campo corrente
082800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082900030715     C* Gestisco l'ultimo campo
083000030715     C                   IF        posA = *zeros
083100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   LEAVE
083600030715     C                   ELSE
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   ENDIF
084100030715     C                   ENDIF
084200030715     C* Incremento il contatore d campo
084300030715     C                   EVAL      i = i +1
084400030715     C                   ENDDO
084500030715     C*
084600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084700030715     C* delimitazione testo
084800030715     C                   EVAL      i = 1
084900030715     C                   DOW       i < %elem(SkSplitFLD)
085000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085200030715     C                   EVAL      i = i + 1
085300030715     C                   ENDDO
085400030715     C                   EVAL      wGiro = 1
085500040709     C* Ripristino la 1° riga originale
085600040709     C                   EVAL      vinDTA = depvinDTA
085700030715     C                   ELSE
085800030715     C                   EVAL      wGiro = 2
085900031204     C**********
086000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086100031204     C* d separatore campo
086200031204     C**********
086300040709     C                   EVAL      vindta = %trim(vindta)
086400040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086500040709     C                   EVAL      vindta = %subst(vindta:2)
086600040709     C                   ENDDO
086700040709     C*
086800040709     C                   Z-ADD     *zeros        lunghInput        4 0
086900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087000040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087100040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087200040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087300040709     C                   ENDDO
087400030715     C**********
087500030715     C* Effettuo lo split del campo dati d input
087600030715     C**********
087700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087800040324     C                   DOW       posDa <= %len(%trimr(vindta))
087900030715     C*
088000030715     C* Gestisco il 1° campo
088100030715     C                   IF        i = 1
088200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088300030715     C                   EVAL      posDa = 1
088400030715     C* Eseguo lo scan x trovare la fine del primo campo
088500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C* X i campi successivi al 1°
089000030715     C                   ELSE
089100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089200030715     C                   EVAL      posDa = posA + 1
089300031204     C* Verifico che nn vi sia il campo nn valorizzato
089400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089500031204     C* Se campo nn valorizzato skippo al prossimo
089600031204     C                   EVAL      SkSplitCSV(i) = *blanks
089700031204     C                   EVAL      posA  = posA + 1
089800031204     C                   ELSE
089900030715     C* Eseguo lo scan x trovare la fine del campo corrente
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* Gestisco l'ultimo campo
090200030715     C                   IF        posA = *zeros
090300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C                   LEAVE
090800030715     C                   ELSE
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   ENDIF
091300031204     C                   ENDIF
091400030715     C                   ENDIF
091500030715     C* Incremento il contatore d campo
091600030715     C                   EVAL      i = i +1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092000030715     C* delimitazione testo
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitCSV)
092300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092500030715     C                   EVAL      i = i + 1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitFLD)
093100030715     C*
093200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093400030715     C                             SkSplitCSV(i) <> *blanks
093500030721     C*** ATB
093600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
093700040708     C                   EVAL      VAOATB = SkSplitCSV(i)
093800030721     C                   ENDIF
093900040708     C*** TOR
094000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
094100040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
094200040708     C                   ENDIF
094300040708     C*** TCO
094400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
094500040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
094600040708     C                   ENDIF
094700040708     C*** RSO
094800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
094900040708     C                   EVAL      VAORSO = SkSplitCSV(i)
095000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095100040708     C     '@':'A'       XLATE     VAORSO        VAORSO
095200030715     C* ==
095300030715     C                   ENDIF
095400040708     C*** INO
095500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
095600040708     C                   EVAL      VAOINO = SkSplitCSV(i)
095700030715     C                   ENDIF
095800040708     C*** LOO
095900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
096000040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
096100030715     C                   ENDIF
096200040708     C*** PRO
096300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
096400040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
096500030715     C                   ENDIF
096600040708     C*** NAO
096700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
096800040708     C                   EVAL      VAONAO = SkSplitCSV(i)
096900030721     C                   ENDIF
097000040708     C*** RSR
097100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
097200040708     C                   EVAL      VAORSR = SkSplitCSV(i)
097300040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097400040708     C     '@':'A'       XLATE     VAORSR        VAORSR
097500040708     C* ==
097600040708     C                   IF        VAORSR = *blanks
097700040708     C                   SETON                                        31
097800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097900040708     C                             + ' ' + 'VAORSR'
098000040708     C                   ENDIF
098100040708     C                   ENDIF
098200040708     C*** INR
098300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
098400040708     C                   EVAL      VAOINR = SkSplitCSV(i)
098500040708     C                   IF        VAOINR = *blanks
098600040708     C                   SETON                                        31
098700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098800040708     C                             + ' ' + 'VAOINR'
098900040708     C                   ENDIF
099000040708     C                   ENDIF
099100040708     C*** LOR
099200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
099300040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
099400040708     C                   IF        VAOLOR = *blanks
099500040708     C                   SETON                                        31
099600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099700040708     C                             + ' ' + 'VAOLOR'
099800040708     C                   ENDIF
099900040708     C                   ENDIF
100000040708     C*** PRR
100100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
100200040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
100300040708     C                   IF        VAOPRR = *blanks
100400040708     C                   SETON                                        31
100500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100600040708     C                             + ' ' + 'VAOPRR'
100700040708     C                   ENDIF
100800040708     C                   ENDIF
100900040708     C*** NAR
101000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
101100040708     C                   EVAL      VAONAR = SkSplitCSV(i)
101200040708     C                   ENDIF
101300040708     C*** RER
101400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
101500040708     C                   EVAL      VAORER = SkSplitCSV(i)
101600040708     C                   ENDIF
101700040708     C*** TER
101800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
101900040708     C                   EVAL      VAOTER = SkSplitCSV(i)
102000040708     C                   ENDIF
102100040708     C*** RSC
102200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
102300040708     C                   EVAL      VAORSC = SkSplitCSV(i)
102400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102500040708     C     '@':'A'       XLATE     VAORSC        VAORSC
102600040708     C* ==
102700040708     C                   ENDIF
102800040708     C*** INC
102900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
103000040708     C                   EVAL      VAOINC = SkSplitCSV(i)
103100040708     C                   ENDIF
103200040708     C*** LOC
103300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
103400040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
103500040708     C                   ENDIF
103600040708     C*** PRC
103700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
103800040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
103900040708     C                   ENDIF
104000040708     C*** NAC
104100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
104200040708     C                   EVAL      VAONAC = SkSplitCSV(i)
104300040708     C                   ENDIF
104400040708     C*** FFD
104500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
104600040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
104700040708     C                   ENDIF
104800040708     C*** RMP
104900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
105000040708     C                   EVAL      VAORMP = SkSplitCSV(i)
105100040708     C                   ENDIF
105200040708     C*** NAM
105300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
105400040708     C                   EVAL      VAONAM = SkSplitCSV(i)
105500040708     C                   ENDIF
105600040708     C*** PAG
105700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
105800040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
105900040708     C                   ENDIF
106000040708     C*** CTR
106100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
106200040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
106300040708     C                   ENDIF
106400040708     C*** ZOR
106500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
106600040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
106700040708     C                   ENDIF
106800040708     C*** NO1
106900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
107000040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
107100030715     C                   ENDIF
107200040708     C*** NO2
107300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
107400040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
107500030715     C                   ENDIF
107600040708     C*** DDT
107700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
107800040708     C                   EVAL      VAODDT = SkSplitCSV(i)
107900030715     C                   ENDIF
108000040708     C*** RFA
108100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
108200040708     C                   EVAL      VAORFA = SkSplitCSV(i)
108300030715     C                   ENDIF
108400040708     C*** SPI
108500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
108600040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
108700030715     C                   ENDIF
108800040708     C*** FLO
108900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
109000040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
109100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
109200080708     C* e la posizione 7 del VAOFLO
109300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
109400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
109500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
109600030715     C                   ENDIF
109700091103     C
109800091103     C*** DAR
109900050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
110000050412     C                   EVAL      PiStr=SkSplitCSV(i)
110100050412     C                   EXSR      CHKNUM
110200050412     C                   IF        PiInt=*on
110300050412     C                   Z-ADD     PiVal         VAODAR
110400050412     C                   ELSE
110500050412     C                   SETON                                        32
110600050412     C                   EVAL      VAODAR = *zeros
110700050412     C                   EVAL      vinmsg = %trimr(vinmsg)
110800050412     C                             + ' ' + 'VAODAR'
110900050412     C                   ENDIF
111000050314     C                   ENDIF
111100030715     C
111200030721     C*
111300030721     C* Reperisco quindi i campi numerici...
111400040708     C*** POE
111500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
111600040723     C                   EVAL      PiStr=SkSplitCSV(i)
111700040723     C                   EXSR      CHKNUM
111800040723     C                   IF        PiInt=*on    AND
111900040723     C                             PiVal<=999   AND
112000040723     C                             PiVal>*zeros
112100040723     C                   Z-ADD     PiVal         VAOPOE
112200040723     C                   ELSE
112300040723     C                   SETON                                        32
112400040723     C                   EVAL      VAOPOE = *zeros
112500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
112600040723     C                             + ' ' + 'VAOPOE'
112700040723     C                   ENDIF
112800040723     C                   ENDIF
112900040708     C*** COR
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
113100040708     C                   EVAL      PiStr=SkSplitCSV(i)
113200040708     C                   EXSR      CHKNUM
113300040708     C                   IF        PiInt=*on
113400040708     C                   Z-ADD     PiVal         VAOCOR
113500040708     C                   ELSE
113600040708     C                   SETON                                        31
113700040708     C                   EVAL      VAOCOR = *zeros
113800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113900040708     C                             + ' ' + 'VAOCOR'
114000040708     C                   ENDIF
114100040708     C                   ENDIF
114200040708     C*** CAO
114300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on
114700030721     C                   Z-ADD     PiVal         Num5_0
114800040708     C                   MOVEL(p)  Num5_0        VAOCAO
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100040708     C                   EVAL      VAOCAO = *zeros
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300040708     C                             + ' ' + 'VAOCAO'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600040708     C*** CRA
115700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on
116100040708     C                   Z-ADD     PiVal         VAOCRA
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400040708     C                   EVAL      VAOCRA = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600040708     C                             + ' ' + 'VAOCRA'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900040708     C*** CAR
117000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
117100040708     C                   EVAL      PiStr=SkSplitCSV(i)
117200040708     C                   EXSR      CHKNUM
117300040708     C                   IF        PiInt=*on
117400040708     C                   Z-ADD     PiVal         Num5_0
117500040708     C                   MOVEL(p)  Num5_0        VAOCAR
117600040708     C                   ELSE
117700040708     C                   SETON                                        31
117800040708     C                   EVAL      VAOCAR = *zeros
117900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118000040708     C                             + ' ' + 'VAOCAR'
118100040708     C                   ENDIF
118200040708     C                   ENDIF
118300040708     C*** CRC
118400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
118500040708     C                   EVAL      PiStr=SkSplitCSV(i)
118600040708     C                   EXSR      CHKNUM
118700040708     C                   IF        PiInt=*on
118800040708     C                   Z-ADD     PiVal         VAOCRC
118900040708     C                   ELSE
119000040708     C                   SETON                                        32
119100040708     C                   EVAL      VAOCRC = *zeros
119200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040708     C                             + ' ' + 'VAOCRC'
119400040708     C                   ENDIF
119500040708     C                   ENDIF
119600040708     C*** CAC
119700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
119800040708     C                   EVAL      PiStr=SkSplitCSV(i)
119900040708     C                   EXSR      CHKNUM
120000040708     C                   IF        PiInt=*on
120100040708     C                   Z-ADD     PiVal         Num5_0
120200040708     C                   MOVEL(p)  Num5_0        VAOCAC
120300040708     C                   ELSE
120400040708     C                   SETON                                        32
120500040708     C                   EVAL      VAOCAC = *zeros
120600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120700040708     C                             + ' ' + 'VAOCAC'
120800040708     C                   ENDIF
120900040708     C                   ENDIF
121000040708     C*** DAR
121100090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
121200090428     C                   EVAL      PiStr=SkSplitCSV(i)
121300090428     C                   EXSR      CHKNUM
121400090428     C                   IF        PiInt=*on
121500090428     C                   Z-ADD     PiVal         VAODAR
121600090428     C                   ELSE
121700090428     C                   SETON                                        32
121800090428     C                   EVAL      VAODAR = *zeros
121900090428     C                   EVAL      vinmsg = %trimr(vinmsg)
122000090428     C                             + ' ' + 'VAODAR'
122100090428     C                   ENDIF
122200090428     C                   ENDIF
122300040708     C*** ORR
122400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
122500040708     C                   EVAL      PiStr=SkSplitCSV(i)
122600040708     C                   EXSR      CHKNUM
122700040708     C                   IF        PiInt=*on
122800040708     C                   Z-ADD     PiVal         VAOORR
122900040708     C                   ELSE
123000040708     C                   SETON                                        32
123100040708     C                   EVAL      VAOORR = *zeros
123200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123300040708     C                             + ' ' + 'VAOORR'
123400040708     C                   ENDIF
123500040708     C                   ENDIF
123600030721     C*** NCL
123700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiInt=*on
124100040723     C                   IF        PiVal > *zeros
124200040708     C                   Z-ADD     PiVal         VAONCL
124300040723     C                   ENDIF
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600040708     C                   EVAL      VAONCL = *zeros
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800040708     C                             + ' ' + 'VAONCL'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100040708     C*** PKG
125200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiNum=*on
125600040723     C                   IF        PiVal > *zeros
125700040708     C                   Z-ADD     PiVal         VAOPKG
125800040723     C                   ENDIF
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100040708     C                   EVAL      VAOPKG = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300040708     C                             + ' ' + 'VAOPKG'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600040708     C*** VLM
126700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
126800040708     C                   EVAL      PiStr=SkSplitCSV(i)
126900040708     C                   EXSR      CHKNUM
127000040708     C                   IF        PiNum=*on
127100040708     C                   Z-ADD     PiVal         VAOVLM
127200040708     C                   ELSE
127300040708     C                   SETON                                        32
127400040708     C                   EVAL      VAOVLM = *zeros
127500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127600040708     C                             + ' ' + 'VAOVLM'
127700040708     C                   ENDIF
127800040708     C                   ENDIF
127900040708     C*** BNC
128000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
128100040708     C                   EVAL      PiStr=SkSplitCSV(i)
128200040708     C                   EXSR      CHKNUM
128300040708     C                   IF        PiInt=*on
128400040708     C                   Z-ADD     PiVal         VAOBNC
128500040708     C                   ELSE
128600040708     C                   SETON                                        32
128700040708     C                   EVAL      VAOBNC = *zeros
128800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128900040708     C                             + ' ' + 'VAOBNC'
129000040708     C                   ENDIF
129100040708     C                   ENDIF
129200040708     C*** BLC
129300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
129400040708     C                   EVAL      PiStr=SkSplitCSV(i)
129500040708     C                   EXSR      CHKNUM
129600040708     C                   IF        PiInt=*on
129700040708     C                   Z-ADD     PiVal         VAOBLC
129800040708     C                   ELSE
129900040708     C                   SETON                                        32
130000040708     C                   EVAL      VAOBLC = *zeros
130100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130200040708     C                             + ' ' + 'VAOBLC'
130300040708     C                   ENDIF
130400040708     C                   ENDIF
130500040708     C*** ATT
130600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
130700040708     C                   EVAL      PiStr=SkSplitCSV(i)
130800040708     C                   EXSR      CHKNUM
130900040708     C                   IF        PiInt=*on
131000040708     C                   Z-ADD     PiVal         VAOATT
131100040708     C                   ELSE
131200040708     C                   SETON                                        32
131300040708     C                   EVAL      VAOATT = *zeros
131400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131500040708     C                             + ' ' + 'VAOATT'
131600040708     C                   ENDIF
131700040708     C                   ENDIF
131800040708     C*** MTC
131900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
132000040708     C                   EVAL      PiStr=SkSplitCSV(i)
132100040708     C                   EXSR      CHKNUM
132200040708     C                   IF        PiInt=*on
132300040708     C                   Z-ADD     PiVal         VAOMTC
132400040708     C                   ELSE
132500040708     C                   SETON                                        32
132600040708     C                   EVAL      VAOMTC = *zeros
132700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132800040708     C                             + ' ' + 'VAOMTC'
132900040708     C                   ENDIF
133000040708     C                   ENDIF
133100040708     C*** KSC
133200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiInt=*on      AND
133600030721     C                             PiVal<=9999999 AND
133700030721     C                             PiVal>*zeros
133800040708     C                   Z-ADD     PiVal         VAOKSC
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100040708     C                   EVAL      VAOKSC = *zeros
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300040708     C                             + ' ' + 'VAOKSC'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600040708     C*** POR
134700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on    AND
135100030721     C                             PiVal<=999   AND
135200030721     C                             PiVal>*zeros
135300040708     C                   Z-ADD     PiVal         VAOPOR
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600040708     C                   EVAL      VAOPOR = *zeros
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800040708     C                             + ' ' + 'VAOPOR'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100040708     C*** POC
136200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
136300040708     C                   EVAL      PiStr=SkSplitCSV(i)
136400040708     C                   EXSR      CHKNUM
136500040708     C                   IF        PiInt=*on    AND
136600040708     C                             PiVal<=999   AND
136700040708     C                             PiVal>*zeros
136800040708     C                   Z-ADD     PiVal         VAOPOC
136900040708     C                   ELSE
137000040708     C                   SETON                                        32
137100040708     C                   EVAL      VAOPOC = *zeros
137200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137300040708     C                             + ' ' + 'VAOPOC'
137400040708     C                   ENDIF
137500040708     C                   ENDIF
137600040708     C*** STO
137700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
137800040708     C                   EVAL      PiStr=SkSplitCSV(i)
137900040708     C                   EXSR      CHKNUM
138000040708     C                   IF        PiInt=*on
138100040708     C                   Z-ADD     PiVal         VAOSTO
138200040708     C                   ELSE
138300040708     C                   SETON                                        32
138400040708     C                   EVAL      VAOSTO = *zeros
138500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138600040708     C                             + ' ' + 'VAOSTO'
138700040708     C                   ENDIF
138800040708     C                   ENDIF
138900090428     C*
139000091103     C********** GESTIONE CAMPI PERSONALIZZATI
139100121207
139200140117     C*** W_VAODAR
139300140117     C* è in formato gg/mm/aaaa invece che aaaammgg
139400140117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
139500140117     C                   IF        SkSplitCSV(i) <> *blank
139600140117     C                   EVAL      PiStr =
139700140117     C                              %subst(%trim(SkSplitCSV(i)) : 7 : 4) +
139800140117     C                              %subst(%trim(SkSplitCSV(i)) : 4 : 2) +
139900140117     C                              %subst(%trim(SkSplitCSV(i)) : 1 : 2)
140000140117     C                   EXSR      CHKNUM
140100140117     C                   IF        PiInt=*on
140200140117     C                   Z-ADD     PiVal         VAODAR
140300140117     C                   ELSE
140400140117     C                   SETON                                        32
140500140117     C                   EVAL      VAODAR = *zeros
140600140117     C                   EVAL      vinmsg = %trimr(vinmsg)
140700140117     C                             + ' ' + 'VAODAR'
140800140117     C                   ENDIF
140900140117     C                   ELSE
141000140117     C                   EVAL      VAODAR = *zeros
141100140117     C                   ENDIF
141200140117     C                   ENDIF
141300090428     C*
141400090428     C****************************************
141500090428     C*
141600030715     C*
141700030715     C                   ENDIF
141800030715     C                   EVAL      i = i + 1
141900030715     C                   ENDDO
142000040723     C*
142100121207     C***  ===>  Gestione campi "particolari" fuori ciclo
142200121207     C*
142300140108
142400140108     C*
142500140117     C* se VAOCOR non è valorizzato, crearlo  mediante VAOKSC
142600140117     C                   IF        VAOCOR = 0
142700140117     C                   EVAL      VAOCOR = VAOKSC * 1000
142800140108     C                   ENDIF
142900140108     C*
143000130513
143100030715     C                   ENDIF
143200020204     C*
143300010607     C                   ENDSR
143400010607     C*----------------------------------------------------*
143500080702
143600080702
143700080702     C*----------------------------------------------------*
143800080702     C*  CONTROLLO NUMERICITA' CAMPI
143900080702     C*----------------------------------------------------*
144000080702     C     CHKNUM        BEGSR
144100080702     C*
144200080702     C                   IF        PiDecChr = *blanks
144300080702     C                   EVAL      PiDecChr = ','
144400080702     C                   ENDIF
144500080702     C*
144600080702     C                   callp     UBISNUM_Check(PiStr
144700080702     C                                          :PiDecChr
144800080702     C                                          :PiVal
144900080702     C                                          :PiNum
145000080702     C                                          :PiInt)
145100080702     C*
145200080702     C                   ENDSR
145300080702     C***
145400080702
145500010601
145600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145700010601     C     repfil        BEGSR
145800010601     C*
145900010601     C                   if        invfil = *zeros and
146000010601     C                             depfil > *zeros and
146100010629     C                             (vinflg = *blanks or
146200010629     C                              vinflg = *zeros)
146300010601     C
146400010601     C                   eval      invfil = depfil
146500010601     C                   endif
146600010601     C*
146700010601     C                   if        depfil <> invfil and
146800010601     C                             invfil > *zeros
146900010601     C                   eval      flgMulti = '1'
147000010601     C                   if        vinflg = *blanks
147100010601     C                   add       1             cntNonEl
147200010601     C                   endif
147300010601     C                   endif
147400010601     C*
147500010601     C                   if        vinflg = '2'
147600010601     C                   eval      flgStato = '2'
147700010601     C                   endif
147800010601     C*
147900010601     C                   ENDSR
148000010601     C***
148100010330
148200010330
148300990920      /TITLE Invio dei dati al punto operativo.
148400000613     C     invio         BEGSR
148500990920     C*
148600990920     C                   reset                   dscmz
148700010601     C                   move      invfil        cmzdst
148800990920     C                   eval      cmzfld = vlrfou
148900990920     C                   eval      cmzmbd = vlrhdl
149000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
149100000710     C                   if        prmfir = *blanks
149200040708     C                   eval      cmzfla = 'FNVAO00F'
149300040708     C                   eval      cmzmba = 'FNVAO00F'
149400000710     C                   else
149500000710     C                   eval      cmzfla = prmfir
149600000710     C                   eval      cmzmba = prmfir
149700000710     C                   endif
149800990920     C                   eval      cmznrr = *zeros
149900990920     C                   move      §ctrok        cmznrr
150000021018     C                   eval      cmzlba = vlrfl1
150100990920     C                   call(e)   'TIS711C'
150200990920     C                   parm                    dscmz
150300990921     C                   parm      *blanks       esito
150400990920     C                   if        %error
150500990920     C                             or cmzerr = '1'
150600990921     C                             or esito  = '1'
150700000710     C                   eval      wrkesito = '3'
150800990920     C                   endif
150900990920     C*
151000000613     C                   ENDSR
151100010601
151200010601
151300010601      /TITLE Invio dei dati al punto operativo.
151400010601     C     opeini        BEGSR
151500010601     C*
151600010601     C* Inizializzo flag e contatori operativi
151700010601     C                   movel     '0'           flgGiro           1
151800010601     C                   movel     '0'           flgMulti          1
151900010601     C                   movel     '1'           flgStato          1
152000010615     C                   movel     '0'           flgOk             1
152100010601     C                   z-add     *zeros        cntNonEl         10 0
152200010601     C                   z-add     *zeros        depfil            3 0
152300010601     C                   z-add     *zeros        invfil            3 0
152400010601     C*
152500010601     C                   ENDSR
152600010601     C***
152700050421
152800050421
152900091130     C     *pssr         BEGSR
153000050421     C*
153100091130     C                   eval      wrkesito = '2'
153200050421     C*
153300091130     C                   ENDSR     '*CANCL'
153400050421     C***
153500060306
153600060306
153700060306     C*-------------------------------------------------------------------------*
153800060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
153900060306     C*-------------------------------------------------------------------------*
154000060306     C     sr_Control    begsr
154100060306     C*
154200060306     C* inizializzo a ok il flag d controllo abilitazione
154300060306     C                   eval      Cli_Abil = *on
154400060306     C
154500060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
154600060306     C                   clear                   woriIDC
154700060306     C                   eval      kteCOD = 'VAP'
154800060306     C                   eval      kteKE1 = oriIDC
154900060306     C     keytbe        chain     TNTBE01L
155000060306     C                   if        %found(Tntbe01l)
155100060306     C                   eval      woriIDC = TbeKe2
155200060306     C                   else
155300060306     C                   eval      Cli_Abil = *off
155400060306     C                   endif
155500060306     C*
155600060306     C                   endsr
155700010330
155800010330
155900000613     C     *inzsr        BEGSR
156000990910     C*
156100990910     C     *entry        plist
156200990920     C                   parm                    tivlrds
156300990921     C                   parm      wrkesito      esito
156400000724     C                   parm                    prmlit
156500000710     C                   parm                    prmfir
156600010330     C*
156700010330     C* CALCOLA LA DATA CORRENTE
156800091127     C                   z-add     *zeros        datcor            8 0
156900091127     C                   time                    wn14             14 0
157000091127     C                   movel     wn14          wn6               6 0          *ORA (6)
157100130516     C                   eval      datcor = %dec(%date() : *ISO)
157200040723     C*
157300040723     C* Campi riferito al database
157400040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
157500040723     C     *like         define    tbeke1        kteKE1
157600040723     C*
157700040723     C* Definizione chiavi di lettura
157800040723     C* TNTBE01L - parziale
157900040723     C     keytbe        klist
158000040723     C                   kfld                    kteCOD                         *UTENTE
158100040723     C                   kfld                    kteKE1                         *CODICE
158200000613     C*
158300000613     C                   ENDSR
158400000613     C***
158500990908
158600040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
158700140117X;VAONAM;VAORFA;W_VAODAR;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;
158800140117VAOPKG;VAOVLM;VAONCL;VAORER;VAOTER;VAONO1;VAONO2;
158900130222
