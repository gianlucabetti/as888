000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200131216      *
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
002600131216     D tisi95ds      e ds
002700040723     D dorm01        e ds
002800040723     D dfar          e ds
002900090618     d trul33ds      e ds
003000090618     d kpjba         e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003700060306     D Cli_Abil        s              1    inz(*off)
003800060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003900090618     d §numori         s              7  0 inz
004000120123     d wData_EUR       s               d   datfmt(*eur)
004100120123     d wData_ISO       s               d   datfmt(*iso)
004200120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004300120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004400130513     D Intestazioni    s               n   inz(*off)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700030822     D*------------------
005800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005900030822     D*------------------
006000030822     D posDaDft        S              3  0 INZ(*zeros)
006100030822     D posADft         S              3  0 INZ(*zeros)
006200030822     D j               s              3  0 INZ(1)
006300030822     D wGiroDft        s              1  0 INZ(*zeros)
006400040723     D*------------------
006500040723     D* DS REPERIMENTO DATI CLIENTE
006600040723     D*-------------------
006700040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006800040723     D ACODS         E DS                  EXTNAME(CNACO00F)
006900040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007000040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007100040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007200121129     D*------------------
007300130513     D wRSR1           s             35
007400130513     D wRSR2           s             35
007500130513     D wRSC1           s             35
007600130513     D wRSC2           s             35
007700130513     D w85             s             85
007800130514     D w5              s              5
007900080702
008000080702     D*------------------
008100080702     D* LINKING A DEFINIZIONI ESTERNE
008200080702     D*------------------
008300090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008400090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
010500131216     C*
010600131216     C* Esegue lancio TISI95R solo x chiusura
010700131216     C                   CLEAR                   TISI95DS
010800131216     C                   EVAL      I95TLA = 'C'
010900131216     C                   CALL      'TISI95R'
011000131216     C                   PARM                    TISI95DS
011100010601     C*
011200010601     C                   seton                                        lr
011300010601
011400010601
011500010601
011600010601
011700010601     C*--------------------------------------------------------
011800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011900010601     C*--------------------------------------------------------
012000010601     C     PREELA        BEGSR
012100010601     C*
012200010601     C* SE OCCORRE SPEDIRE IN FILIALE
012300041011     C                   if        flgGiro = '0'
012400010601     C*
012500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012600010601     C                   eval      flgGiro = '1'
012700041011     C*
012800041011     C                   endif
012900010601     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601     C*--------------------------------------------------------
013500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013600010601     C*--------------------------------------------------------
013700010601     C     ENDELA        BEGSR
013800000616     C*
013900120209     C                   ENDSR
014000010601
014100010601
014200010330     C*--------------------------------------------------------
014300040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
014400010330     C*--------------------------------------------------------
014500040708     C     RWVAO         BEGSR
014600010330     C*
014700010330     C                   if        not %open(tivin00r)
014800010330     C                   open      tivin00r
014900010330     C                   endif
015000040708     C                   if        not %open(fnvaowwr)
015100040708     C                   open      fnvaowwr
015200010330     C                   endif
015300010330     C*
015400010604     C                   clear                   §CTROK
015500010604     C                   clear                   §CTRMO
015600010604     C                   clear                   §CTRNO
015700120123     C*
015800120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
015900120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
016000120905     C                   IF        Intestazioni = *off
016100120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
016200120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016300120123     C                   exsr      inzvar
016400120123     C                   exsr      impvao
016500120905     C                   ENDIF
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
020300010601     C*
020400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020500040708     C                   eval      depfil = VAOPOE
020600010601     C                   exsr      repfil
020700010601     C                   if        depfil = invfil
020800021025     C                   if        vlrpoi = 999
020900040708     C                   move(P)   invfil        VAOPOE
021000021025     C                   else
021100040708     C                   move(P)   vlrpoi        VAOPOE
021200021025     C                   endif
021300010601     C*
021400010604     C* Ebbene...
021500010604     C*
021600030822     C  N31              add       1             §CTROK            7 0
021700030822     C   32              add       1             §CTRMO            7 0
021800030822     C   31              add       1             §CTRNO            7 0
021900030822     C                   if        wGiro = 2
022000120123     C  N31              EXSR      CHKCOR
022100120123     C  N31              EXSR      WRIORI
022200120123     C  N31              EXSR      WRIVGD
022300120123     C  N31              write     FNVAO000
022400030822     C                   endif
022500010604     C*
022600010604     C                   if        *in31 = *off and
022700010604     C                             *in32 = *off
022800010604     C                   eval      vinflg = '1'
022900010604     C                   else
023000010604     C                   eval      vinflg = '2'
023100010604     C                   endif
023200010604     C                   endif
023300010601     C*
023400010604     C                   endif
023500010604     C*
023600010330     C                   else
023700010330     C                   eval      vinflg = '1'
023800010330     C                   endif
023900010601     C*
024000010601     C  N70              update    tivin000
024100010330     C*
024200030822     C  N70              enddo
024300010601     C
024400010601     C*
024500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024600010601     C                   if        cntNonEl = *zeros or
024700010601     C                             flgMulti = '0'
024800010330     C* Se non ci sono record con errori ...
024900010601     C                   if        §ctrno = 0 and
025000010604     C                             §ctrmo = 0 and
025100010601     C                             flgStato <> '2'
025200010330     C* ... restituisco esito OK.
025300010330     C                   eval      wrkesito = '0'
025400010330     C                   else
025500010330     C                   if        §ctrok > 0
025600010330     C                   eval      wrkesito = '1'
025700010330     C                   else
025800010615     C                   if        flgOk = '0'
025900010615     C                   eval      wrkesito = '2'
026000010615     C                   else
026100010615     C                   eval      wrkesito = '6'
026200010615     C                   endif
026300010330     C                   endif
026400010330     C                   endif
026500010601     C                   else
026600010601     C                   eval      wrkesito = '9'
026700010601     C                   endif
026800010330     C*
026900010330     C                   if        %open(tivin00r)
027000010330     C                   close     tivin00r
027100010330     C                   endif
027200040708     C                   if        %open(fnvaowwr)
027300040708     C                   close     fnvaowwr
027400010330     C                   endif
027500010601     C*
027600010601     C                   if        vlrpoi <> 999
027700010601     C                   eval      invfil = vlrpoi
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        §ctrok > 0
028100010601     C                             and invfil > *zeros
028200010330     C                   exsr      invio
028300010330     C                   endif
028400010601     C*
028500010618     C                   if        flgGiro = '1'
028600010601     C                   exsr      endela
028700010618     C                   endif
028800010330     C*
028900010330     C                   ENDSR
029000010330     C***
029100040723
029200040723
029300040723     C*------------------------------------------------------------------------*
029400040723     C* CHKCOR - Considerazioni su VAOCOR
029500040723     C*------------------------------------------------------------------------*
029600040723     C     CHKCOR        BEGSR
029700040723     C*
029800040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
029900040723     C                   if        vaoTCO = 'F'
030000040723     C* Quindi verifico se il codice ordinante non è valorizzato
030100040723     C                   if        vaoCOR = *zeros  AND
030200040723     C                             vaoRSO = *blanks AND
030300040723     C                             vaoINO = *blanks AND
030400040723     C                             vaoCAO = *blanks AND
030500040723     C                             vaoLOO = *blanks AND
030600040723     C                             vaoPRO = *blanks AND
030700040723     C                             vaoNAO = *blanks
030800040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
030900040723     C                   movel     '0'           wFlgAcr           1
031000040723     C                   move      vlrKSC        wCli              7 0
031100040723     C                   move(p)   wCli          acrCRO
031200040723     C     acrCRO        setll     fnacr01l
031300040723     C                   if        %found(fnacr01l)
031400040723     C                   read      fnacr01l
031500040723     C                   dow       not %eof(fnacr01l)
031600040723     C                   movel     acrCRO        wCliAcr           7 0
031700040723     C                   if        wCliAcr = wCli
031800040723     C                   eval      vaoCOR = acrCRO
031900040723     C                   movel     '1'           wFlgAcr
032000040723     C                   leave
032100040723     C                   else
032200040723     C                   leave
032300040723     C                   endif
032400040723     C                   read      fnacr01l
032500040723     C                   enddo
032600040723     C                   endif
032700040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
032800040723     C* dati anagrafici dell'ordinante dal piano dei conti
032900040723     C                   if        wFlgAcr = '0'
033000040723     C                   clear                   BS69DS
033100040723     C                   clear                   ACODS
033200040723     C                   clear                   INDDS
033300040723     C                   clear                   CLPDS
033400040723     C                   clear                   CLSDS
033500040723     C                   move(P)   vlrKSC        I69KAC
033600040723     C                   move(P)   vlrKSC        I69KIN
033700040723     C                   call      'TIBS69R'
033800040723     C                   parm                    BS69DS
033900040723     C                   parm                    ACODS
034000040723     C                   parm                    INDDS
034100040723     C                   parm                    CLPDS
034200040723     C                   parm                    CLSDS
034300040723     C     O69ERR        ifne      '1'
034400040723     C                   eval      vaoRSO = ACORAG
034500040723     C                   eval      vaoINO = INDVIA
034600040723     C                   movel(P)  INDCAP        vaoCAO
034700040723     C                   eval      vaoLOO = INDCIT
034800040723     C                   eval      vaoPRO = INDPRV
034900040723     C                   eval      vaoNAO = INDSTA
035000040723     C                   endif
035100040723     C                   endif
035200040723     C                   endif
035300040723     C                   endif
035400040723     C*
035500040723     C                   ENDSR
035600040723
035700040723
035800040723     C*------------------------------------------------------------------------*
035900040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
036000040723     C*------------------------------------------------------------------------*
036100040723     C     WRIORI        BEGSR
036200040723     C*
036300090618     C* Come prima cosa stacco un numeratore da AZNUM
036400090618     C                   clear                   trul33ds
036500090618     C                   eval      I33OPE = *zeros
036600090618     C                   eval      I33CNU = 600
036700090618     C                   eval      I33NUM = 1
036800090618     C                   movel     TRUL33DS      KPJBU
036900090618     C                   call      'TRUL33R'
037000090618     C                   parm                    KPJBA
037100090618     C                   movel     KPJBU         TRUL33DS
037200090618     C                   if        O33ERR = *zeros
037300090618     C                   z-add     O33NRF        §numori
037400090618     c                   else
037500090618     c                   eval      §numori = *all'9'
037600090618     C                   endif
037700040723     C*
037800040723     C* Quindi imposto i campi "extra-VAO"
037900040723     C                   if        vlrKSC = '0ORM0000'
038000040723     C*
038100040723     C                   if        vaoCOR > *zeros
038200040723     C                   movel     vaoCOR        wrkCOR7           7 0
038300040723     C*
038400040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
038500040723     C* codice padre. Se è un padre assegno già il codice ordinante.
038600040723     C                   clear                   tibs10ds
038700040723     C                   eval      d10drf = datcor                              *data riferimento
038800040723     C                   eval      d10paf = 'P'                                 *cerca il padre
038900040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
039000091026     C*
039100091026     C* Preparazione chiamata al TIBS10R1
039200091026     C                   clear                   tibs10r1ds
039300091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
039400091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
039500091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
039600091026     C*
039700091026     C                   call      'TIBS10R1'
039800091026     C                   parm                    tibs10r1ds
039900091026     C*
040000091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
040100040723     C*
040200040723     C                   if        d10err <> *blanks or                         *No unif. o padre
040300040723     C                             d10cop =  *zeros  or
040400040723     C                             d10cop =  d10cod
040500040723     C                   move      d10cod        oriIDC
040600040723     C                   else                                                   *Figlio
040700040723     C                   move      d10cop        oriIDC
040800040723     C                   endif
040900040723     C*
041000040723     C                   else
041100040723     C                   eval      oriIDC = *zeros
041200040723     C                   endif
041300040723     C*
041400040723     C                   else
041500040723     C                   eval      oriIDC = vlrKSC
041600040723     C                   endif
041700040723     C*
041800040723     C                   eval      oriPRG = §numori
041900040723     C                   eval      oriDER = datcor
042000040723     C                   eval      oriOER = wn6
042100040723     C                   eval      oriPOE = vaoPOE
042200040723     C                   eval      oriNSR = vaoNSR
042300040723     C                   eval      oriNOR = vaoNOR
042400040723     C                   eval      oriNRV = vaoNRV
042500040723     C                   eval      oriFLO = *blanks
042600040723     C*
042700040723     C* Infine valorizzo la chiave esterna sul file FNVAO
042800040723     C                   eval      dorm01 = vaoFLO
042900040723     C                   movel     §numori       §ormpg
043000050412     C*
043100050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
043200130326     C*                   in questo caso non ci sono default
043300050412     C*
043400130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
043500040723     C                   eval      vaoFLO = dorm01
043600130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
043700130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
043800130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
043900040723     C*
044000040723     C* ...quindi scrivo il file TIORI00F
044100120905     C  N55              WRITE     TIORI000
044200040723     C*
044300040723     C                   ENDSR
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
047900151207     C                   eval      vgdPGM = procname
048000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
048100060306     C                   exsr      sr_Control
048200060306     C                   if        Cli_Abil = *on
048300060306     C                   eval      vgdKSU = woriIDC
048400120905     C  N55              write     tivgd000
048500060306     C                   endif
048600050826     C*
048700050826     C                   ENDSR
048800050826     C*------------------------------------------------------------------------*
048900010601
049000010601
049100010330     C*----------------------------------------------------*
049200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049300010330     C*----------------------------------------------------*
049400010330     C     INZVAR        BEGSR
049500010330     C*
049600030822     C* Inizializzo variabili di wrk
049700020204     C                   Z-ADD     *zeros        Num5_0            5 0
049800120124
049900030822     C*
050000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050100120123     C                   CLEAR                   FNVAO000
050200120123     C                   CLEAR                   TIORI000
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
052200040723     C*
052300080707     C* Imposto i valori d default d alcuni campi
052400080708     C*
052500080708     C                   CLEAR                   DORM01
052600120822     C                   EVAL      VAOFLO = DORM01
052700080707     C*
052800130513     C                   EVAL      VAOTOR = 'S'
052900130513     C                   EVAL      VAOTCO = 'F'
053000130513     C                   EVAL      VAOPOE = 311
053100130513     C                   EVAL      VAOCOR = 3110012000
053200130513     C                   EVAL      VAOKSC = 3110012
053300130513     C                   EVAL      VAOPAG = 'O'
053400130513
053500120905     C* se il 1° char dei parametri è T=Test
053600120905     C                   SETOFF                                       55
053700120905     C                   IF        %subst(vlrppt:1:1) = 'T'
053800120905     C* inibisco la scrittura dei file TIORI e TIVGD
053900120905     C                   SETON                                        55
054000120905     C* imposto il PO di emissione come filiale fittizia
054100120905     C                   EVAL      VAOPOE = 999
054200120905     C                   ENDIF
054300091127     C*
054400091204     C* IMPOSTO I CAMPI PERSONALIZZATI
054500100416     C
054600030822     C*
054700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054800030822     C                   EVAL      posDaDft = 1
054900030822     C                   EVAL      posADft  = 0
055000030822     C                   EVAL      wGiroDft = 0
055100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055200030822     C                             posDaDft > 0
055300030822     C*
055400030822     C* Gestisco il 1° giro
055500030822     C                   IF        wGiroDft = 0
055600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055800030822     C* Incremento il contatore dei "giri"
055900030822     C                   EVAL      wGiroDft = 1
056000030822     C                   ELSE
056100030822     C                   EVAL      posDaDft = posADft
056200030822     C                   ENDIF
056300030822     C* Eseguo lo scan x trovare la fine del campo corrente
056400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056500030822     C*
056600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056800030822     C                   IF        posDaDft > 0 AND
056900030822     C                             posADft  > 0
057000040708     C* POE
057100040708     C                   IF        %subst(
057200040708     C                             %subst(vlrppt:posDaDft+1:
057300040708     C                             posADft-posDaDft-1):1:3)
057400040708     C                             = 'POE'
057500040708     C                   EVAL      PiStr=%trim(%subst(
057600040708     C                             %subst(vlrppt:posDaDft+1:
057700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800040708     C                   EXSR      CHKNUM
057900040708     C                   IF        PiInt=*on
058000040708     C                   Z-ADD     PiVal         VAOPOE
058100040708     C                   ENDIF
058200040708     C                   ENDIF
058300040708     C* TOR
058400040708     C                   IF        %subst(
058500040708     C                             %subst(vlrppt:posDaDft+1:
058600040708     C                             posADft-posDaDft-1):1:3)
058700040708     C                             = 'TOR'
058800040708     C                   EVAL      VAOTOR=%trim(%subst(
058900040708     C                             %subst(vlrppt:posDaDft+1:
059000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100040708     C                   ENDIF
059200040708     C* TCO
059300040708     C                   IF        %subst(
059400040708     C                             %subst(vlrppt:posDaDft+1:
059500040708     C                             posADft-posDaDft-1):1:3)
059600040708     C                             = 'TCO'
059700040708     C                   EVAL      VAOTCO=%trim(%subst(
059800040708     C                             %subst(vlrppt:posDaDft+1:
059900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000040708     C                   ENDIF
060100040707     C* COR
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500040707     C                             = 'COR'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiInt=*on
061100040708     C                   Z-ADD     PiVal         VAOCOR
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400040708     C* CRC
061500040708     C                   IF        %subst(
061600040708     C                             %subst(vlrppt:posDaDft+1:
061700040708     C                             posADft-posDaDft-1):1:3)
061800040708     C                             = 'CRC'
061900040708     C                   EVAL      PiStr=%trim(%subst(
062000040708     C                             %subst(vlrppt:posDaDft+1:
062100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200040708     C                   EXSR      CHKNUM
062300040708     C                   IF        PiInt=*on
062400040708     C                   Z-ADD     PiVal         VAOCRC
062500040708     C                   ENDIF
062600040708     C                   ENDIF
062700040708     C* FFD
062800040708     C                   IF        %subst(
062900040708     C                             %subst(vlrppt:posDaDft+1:
063000040708     C                             posADft-posDaDft-1):1:3)
063100040708     C                             = 'FFD'
063200040708     C                   EVAL      VAOFFD=%trim(%subst(
063300040708     C                             %subst(vlrppt:posDaDft+1:
063400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500040708     C                   ENDIF
063600040708     C* ORR
063700040708     C                   IF        %subst(
063800040708     C                             %subst(vlrppt:posDaDft+1:
063900040708     C                             posADft-posDaDft-1):1:3)
064000040708     C                             = 'ORR'
064100040708     C                   EVAL      PiStr=%trim(%subst(
064200040708     C                             %subst(vlrppt:posDaDft+1:
064300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400040708     C                   EXSR      CHKNUM
064500040708     C                   IF        PiInt=*on
064600040708     C                   Z-ADD     PiVal         VAOORR
064700040708     C                   ENDIF
064800040708     C                   ENDIF
064900040712     C* NCL
065000040712     C                   IF        %subst(
065100040712     C                             %subst(vlrppt:posDaDft+1:
065200040712     C                             posADft-posDaDft-1):1:3)
065300040712     C                             = 'NCL'
065400040712     C                   EVAL      PiStr=%trim(%subst(
065500040712     C                             %subst(vlrppt:posDaDft+1:
065600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700040712     C                   EXSR      CHKNUM
065800040712     C                   IF        PiInt=*on
065900040712     C                   Z-ADD     PiVal         VAONCL
066000040712     C                   ENDIF
066100040712     C                   ENDIF
066200040712     C* PKG
066300040712     C                   IF        %subst(
066400040712     C                             %subst(vlrppt:posDaDft+1:
066500040712     C                             posADft-posDaDft-1):1:3)
066600040712     C                             = 'PKG'
066700040712     C                   EVAL      PiStr=%trim(%subst(
066800040712     C                             %subst(vlrppt:posDaDft+1:
066900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000040712     C                   EXSR      CHKNUM
067100040712     C                   IF        PiNum=*on
067200040712     C                   Z-ADD     PiVal         VAOPKG
067300040712     C                   ENDIF
067400040712     C                   ENDIF
067500040712     C* VLM
067600040712     C                   IF        %subst(
067700040712     C                             %subst(vlrppt:posDaDft+1:
067800040712     C                             posADft-posDaDft-1):1:3)
067900040712     C                             = 'VLM'
068000040712     C                   EVAL      PiStr=%trim(%subst(
068100040712     C                             %subst(vlrppt:posDaDft+1:
068200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300040712     C                   EXSR      CHKNUM
068400040712     C                   IF        PiNum=*on
068500040712     C                   Z-ADD     PiVal         VAOVLM
068600040712     C                   ENDIF
068700040712     C                   ENDIF
068800040708     C* NAM
068900040708     C                   IF        %subst(
069000040708     C                             %subst(vlrppt:posDaDft+1:
069100040708     C                             posADft-posDaDft-1):1:3)
069200040708     C                             = 'NAM'
069300040708     C                   EVAL      VAONAM=%trim(%subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600040708     C                   ENDIF
069700040708     C* PAG
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100040708     C                             = 'PAG'
070200040708     C                   EVAL      VAOPAG=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600040708     C* KSC
070700040708     C                   IF        %subst(
070800040708     C                             %subst(vlrppt:posDaDft+1:
070900040708     C                             posADft-posDaDft-1):1:3)
071000040708     C                             = 'KSC'
071100040708     C                   EVAL      PiStr=%trim(%subst(
071200040708     C                             %subst(vlrppt:posDaDft+1:
071300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400040708     C                   EXSR      CHKNUM
071500040708     C                   IF        PiInt=*on
071600040708     C                   Z-ADD     PiVal         VAOKSC
071700040708     C                   ENDIF
071800040708     C                   ENDIF
071900040708     C* CTR
072000040708     C                   IF        %subst(
072100040708     C                             %subst(vlrppt:posDaDft+1:
072200040708     C                             posADft-posDaDft-1):1:3)
072300040708     C                             = 'CTR'
072400040708     C                   EVAL      VAOCTR=%trim(%subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700040708     C                   ENDIF
072800040708     C* POC
072900040708     C                   IF        %subst(
073000040708     C                             %subst(vlrppt:posDaDft+1:
073100040708     C                             posADft-posDaDft-1):1:3)
073200040708     C                             = 'POC'
073300040708     C                   EVAL      PiStr=%trim(%subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600040708     C                   EXSR      CHKNUM
073700040708     C                   IF        PiInt=*on
073800040708     C                   Z-ADD     PiVal         VAOPOC
073900040708     C                   ENDIF
074000040708     C                   ENDIF
074100040708     C* NO1
074200040708     C                   IF        %subst(
074300040708     C                             %subst(vlrppt:posDaDft+1:
074400121129     C                             posADft-posDaDft-1):1:3)
074500040708     C                             = 'NO1'
074600040708     C                   EVAL      VAONO1=%trim(%subst(
074700040708     C                             %subst(vlrppt:posDaDft+1:
074800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900040708     C                   ENDIF
075000040708     C* DDT
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400040708     C                             = 'DDT'
075500040708     C                   EVAL      VAODDT=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900040708     C* RFA
076000040708     C                   IF        %subst(
076100040708     C                             %subst(vlrppt:posDaDft+1:
076200040708     C                             posADft-posDaDft-1):1:3)
076300040708     C                             = 'RFA'
076400040708     C                   EVAL      VAORFA=%trim(%subst(
076500040708     C                             %subst(vlrppt:posDaDft+1:
076600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700040708     C                   ENDIF
076800040708     C* FLO
076900040708     C                   IF        %subst(
077000040708     C                             %subst(vlrppt:posDaDft+1:
077100040708     C                             posADft-posDaDft-1):1:3)
077200040708     C                             = 'FLO'
077300040708     C                   EVAL      VAOFLO=%trim(%subst(
077400040708     C                             %subst(vlrppt:posDaDft+1:
077500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600091103     C*
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
079400120124
079500030515     C*
079600040708     C                   MOVEL     datcor        VAODAO
079700040708     C                   MOVEL     wn14          VAOOAO
079800050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079900050314     C     minu:maiu     XLATE     vindta        vindta
080000010607     C*
080100030715     C**********
080200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080300030715     C**********
080400030715     C                   IF        wGiro = *zeros
080500040709     C*
080600040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080700040709     C                   EVAL      depvinDTA = vinDTA
080800040709     C*
080900040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
081000040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081100040709     C                                      %trim(intcol(3))
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
109100140516     C* essendo un partner estero, non considero i CAP obbligatoriamente numerici
109200140516     C*** CAO
109300140516     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
109400140516     C                   EVAL      VAOCAO = SkSplitCSV(i)
109500140516     C                   ENDIF
109600140516     C*** CAR
109700140516     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
109800140516     C                   EVAL      VAOCAR = SkSplitCSV(i)
109900140516     C                   ENDIF
110000140516     C*** CAC
110100140516     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
110200140516     C                   EVAL      VAOCAC = SkSplitCSV(i)
110300140516     C                   ENDIF
110400040708     C*** FLO
110500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
110600040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
110700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
110800080708     C* e la posizione 7 del VAOFLO
110900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
111000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
111100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
111200030715     C                   ENDIF
111300091103     C
111400091103     C*** DAR
111500050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
111600050412     C                   EVAL      PiStr=SkSplitCSV(i)
111700050412     C                   EXSR      CHKNUM
111800050412     C                   IF        PiInt=*on
111900050412     C                   Z-ADD     PiVal         VAODAR
112000050412     C                   ELSE
112100050412     C                   SETON                                        32
112200050412     C                   EVAL      VAODAR = *zeros
112300050412     C                   EVAL      vinmsg = %trimr(vinmsg)
112400050412     C                             + ' ' + 'VAODAR'
112500050412     C                   ENDIF
112600050314     C                   ENDIF
112700030715     C
112800030721     C*
112900030721     C* Reperisco quindi i campi numerici...
113000040708     C*** POE
113100040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
113200040723     C                   EVAL      PiStr=SkSplitCSV(i)
113300040723     C                   EXSR      CHKNUM
113400040723     C                   IF        PiInt=*on    AND
113500040723     C                             PiVal<=999   AND
113600040723     C                             PiVal>*zeros
113700040723     C                   Z-ADD     PiVal         VAOPOE
113800040723     C                   ELSE
113900040723     C                   SETON                                        32
114000040723     C                   EVAL      VAOPOE = *zeros
114100040723     C                   EVAL      vinmsg = %trimr(vinmsg)
114200040723     C                             + ' ' + 'VAOPOE'
114300040723     C                   ENDIF
114400040723     C                   ENDIF
114500040708     C*** COR
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
114700040708     C                   EVAL      PiStr=SkSplitCSV(i)
114800040708     C                   EXSR      CHKNUM
114900040708     C                   IF        PiInt=*on
115000040708     C                   Z-ADD     PiVal         VAOCOR
115100040708     C                   ELSE
115200040708     C                   SETON                                        31
115300040708     C                   EVAL      VAOCOR = *zeros
115400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
115500040708     C                             + ' ' + 'VAOCOR'
115600040708     C                   ENDIF
115700040708     C                   ENDIF
115800040708     C*** CAO
115900140516     C***                IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
116000140516     C***                EVAL      PiStr=SkSplitCSV(i)
116100140516     C***                EXSR      CHKNUM
116200140516     C***                IF        PiInt=*on
116300140516     C***                Z-ADD     PiVal         Num5_0
116400140516     C***                MOVEL(p)  Num5_0        VAOCAO
116500140516     C***                ELSE
116600140516     C***                SETON                                        32
116700140516     C***                EVAL      VAOCAO = *zeros
116800140516     C***                EVAL      vinmsg = %trimr(vinmsg)
116900140516     C***                          + ' ' + 'VAOCAO'
117000140516     C***                ENDIF
117100140516     C***                ENDIF
117200040708     C*** CRA
117300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on
117700040708     C                   Z-ADD     PiVal         VAOCRA
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000040708     C                   EVAL      VAOCRA = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200040708     C                             + ' ' + 'VAOCRA'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500040708     C*** CAR
118600140516     C***                IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
118700140516     C***                EVAL      PiStr=SkSplitCSV(i)
118800140516     C***                EXSR      CHKNUM
118900140516     C***                IF        PiInt=*on
119000140516     C***                Z-ADD     PiVal         Num5_0
119100140516     C***                MOVEL(p)  Num5_0        VAOCAR
119200140516     C***                ELSE
119300140516     C***                SETON                                        31
119400140516     C***                EVAL      VAOCAR = *zeros
119500140516     C***                EVAL      vinmsg = %trimr(vinmsg)
119600140516     C***                          + ' ' + 'VAOCAR'
119700140516     C***                ENDIF
119800140516     C***                ENDIF
119900040708     C*** CRC
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
120100040708     C                   EVAL      PiStr=SkSplitCSV(i)
120200040708     C                   EXSR      CHKNUM
120300040708     C                   IF        PiInt=*on
120400040708     C                   Z-ADD     PiVal         VAOCRC
120500040708     C                   ELSE
120600040708     C                   SETON                                        32
120700040708     C                   EVAL      VAOCRC = *zeros
120800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040708     C                             + ' ' + 'VAOCRC'
121000040708     C                   ENDIF
121100040708     C                   ENDIF
121200040708     C*** CAC
121300140516     C***                IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
121400140516     C***                EVAL      PiStr=SkSplitCSV(i)
121500140516     C***                EXSR      CHKNUM
121600140516     C***                IF        PiInt=*on
121700140516     C***                Z-ADD     PiVal         Num5_0
121800140516     C***                MOVEL(p)  Num5_0        VAOCAC
121900140516     C***                ELSE
122000140516     C***                SETON                                        32
122100140516     C***                EVAL      VAOCAC = *zeros
122200140516     C***                EVAL      vinmsg = %trimr(vinmsg)
122300140516     C***                          + ' ' + 'VAOCAC'
122400140516     C***                ENDIF
122500140516     C***                ENDIF
122600040708     C*** DAR
122700090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
122800090428     C                   EVAL      PiStr=SkSplitCSV(i)
122900090428     C                   EXSR      CHKNUM
123000090428     C                   IF        PiInt=*on
123100090428     C                   Z-ADD     PiVal         VAODAR
123200090428     C                   ELSE
123300090428     C                   SETON                                        32
123400090428     C                   EVAL      VAODAR = *zeros
123500090428     C                   EVAL      vinmsg = %trimr(vinmsg)
123600090428     C                             + ' ' + 'VAODAR'
123700090428     C                   ENDIF
123800090428     C                   ENDIF
123900040708     C*** ORR
124000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
124100040708     C                   EVAL      PiStr=SkSplitCSV(i)
124200040708     C                   EXSR      CHKNUM
124300040708     C                   IF        PiInt=*on
124400040708     C                   Z-ADD     PiVal         VAOORR
124500040708     C                   ELSE
124600040708     C                   SETON                                        32
124700040708     C                   EVAL      VAOORR = *zeros
124800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040708     C                             + ' ' + 'VAOORR'
125000040708     C                   ENDIF
125100040708     C                   ENDIF
125200030721     C*** NCL
125300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on
125700040723     C                   IF        PiVal > *zeros
125800040708     C                   Z-ADD     PiVal         VAONCL
125900040723     C                   ENDIF
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200040708     C                   EVAL      VAONCL = *zeros
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400040708     C                             + ' ' + 'VAONCL'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700040708     C*** PKG
126800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200040723     C                   IF        PiVal > *zeros
127300040708     C                   Z-ADD     PiVal         VAOPKG
127400040723     C                   ENDIF
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700040708     C                   EVAL      VAOPKG = *zeros
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900040708     C                             + ' ' + 'VAOPKG'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200040708     C*** VLM
128300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
128400040708     C                   EVAL      PiStr=SkSplitCSV(i)
128500040708     C                   EXSR      CHKNUM
128600040708     C                   IF        PiNum=*on
128700040708     C                   Z-ADD     PiVal         VAOVLM
128800040708     C                   ELSE
128900040708     C                   SETON                                        32
129000040708     C                   EVAL      VAOVLM = *zeros
129100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129200040708     C                             + ' ' + 'VAOVLM'
129300040708     C                   ENDIF
129400040708     C                   ENDIF
129500040708     C*** BNC
129600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
129700040708     C                   EVAL      PiStr=SkSplitCSV(i)
129800040708     C                   EXSR      CHKNUM
129900040708     C                   IF        PiInt=*on
130000040708     C                   Z-ADD     PiVal         VAOBNC
130100040708     C                   ELSE
130200040708     C                   SETON                                        32
130300040708     C                   EVAL      VAOBNC = *zeros
130400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130500040708     C                             + ' ' + 'VAOBNC'
130600040708     C                   ENDIF
130700040708     C                   ENDIF
130800040708     C*** BLC
130900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
131000040708     C                   EVAL      PiStr=SkSplitCSV(i)
131100040708     C                   EXSR      CHKNUM
131200040708     C                   IF        PiInt=*on
131300040708     C                   Z-ADD     PiVal         VAOBLC
131400040708     C                   ELSE
131500040708     C                   SETON                                        32
131600040708     C                   EVAL      VAOBLC = *zeros
131700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131800040708     C                             + ' ' + 'VAOBLC'
131900040708     C                   ENDIF
132000040708     C                   ENDIF
132100040708     C*** ATT
132200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
132300040708     C                   EVAL      PiStr=SkSplitCSV(i)
132400040708     C                   EXSR      CHKNUM
132500040708     C                   IF        PiInt=*on
132600040708     C                   Z-ADD     PiVal         VAOATT
132700040708     C                   ELSE
132800040708     C                   SETON                                        32
132900040708     C                   EVAL      VAOATT = *zeros
133000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133100040708     C                             + ' ' + 'VAOATT'
133200040708     C                   ENDIF
133300040708     C                   ENDIF
133400040708     C*** MTC
133500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
133600040708     C                   EVAL      PiStr=SkSplitCSV(i)
133700040708     C                   EXSR      CHKNUM
133800040708     C                   IF        PiInt=*on
133900040708     C                   Z-ADD     PiVal         VAOMTC
134000040708     C                   ELSE
134100040708     C                   SETON                                        32
134200040708     C                   EVAL      VAOMTC = *zeros
134300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134400040708     C                             + ' ' + 'VAOMTC'
134500040708     C                   ENDIF
134600040708     C                   ENDIF
134700040708     C*** KSC
134800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiInt=*on      AND
135200030721     C                             PiVal<=9999999 AND
135300030721     C                             PiVal>*zeros
135400040708     C                   Z-ADD     PiVal         VAOKSC
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700040708     C                   EVAL      VAOKSC = *zeros
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900040708     C                             + ' ' + 'VAOKSC'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200040708     C*** POR
136300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on    AND
136700030721     C                             PiVal<=999   AND
136800030721     C                             PiVal>*zeros
136900040708     C                   Z-ADD     PiVal         VAOPOR
137000030721     C                   ELSE
137100030721     C                   SETON                                        32
137200040708     C                   EVAL      VAOPOR = *zeros
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400040708     C                             + ' ' + 'VAOPOR'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700040708     C*** POC
137800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
137900040708     C                   EVAL      PiStr=SkSplitCSV(i)
138000040708     C                   EXSR      CHKNUM
138100040708     C                   IF        PiInt=*on    AND
138200040708     C                             PiVal<=999   AND
138300040708     C                             PiVal>*zeros
138400040708     C                   Z-ADD     PiVal         VAOPOC
138500040708     C                   ELSE
138600040708     C                   SETON                                        32
138700040708     C                   EVAL      VAOPOC = *zeros
138800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138900040708     C                             + ' ' + 'VAOPOC'
139000040708     C                   ENDIF
139100040708     C                   ENDIF
139200040708     C*** STO
139300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
139400040708     C                   EVAL      PiStr=SkSplitCSV(i)
139500040708     C                   EXSR      CHKNUM
139600040708     C                   IF        PiInt=*on
139700040708     C                   Z-ADD     PiVal         VAOSTO
139800040708     C                   ELSE
139900040708     C                   SETON                                        32
140000040708     C                   EVAL      VAOSTO = *zeros
140100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140200040708     C                             + ' ' + 'VAOSTO'
140300040708     C                   ENDIF
140400040708     C                   ENDIF
140500090428     C*
140600091103     C********** GESTIONE CAMPI PERSONALIZZATI
140700120120
140800130513     C*** W_RSR_*
140900130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSR_1'
141000130513     C                   EVAL      wRSR1 = *blank
141100130513     C                   EVAL      wRSR1 = %trim(SkSplitCSV(i))
141200130513     C                   ENDIF
141300130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSR_2'
141400130513     C                   EVAL      wRSR2 = *blank
141500130513     C                   EVAL      wRSR2 = %trim(SkSplitCSV(i))
141600130513     C                   ENDIF
141700130513
141800130513     C*** W_RSC_*
141900130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSC_1'
142000130513     C                   EVAL      wRSC1 = *blank
142100130513     C                   EVAL      wRSC1 = %trim(SkSplitCSV(i))
142200130513     C                   ENDIF
142300130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSC_2'
142400130513     C                   EVAL      wRSC2 = *blank
142500130513     C                   EVAL      wRSC2 = %trim(SkSplitCSV(i))
142600130513     C                   ENDIF
142700130513
142800130513     C*** W_NO_*
142900130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_NO_1'
143000130513     C                   EVAL      %subst(w85:1:35) = %trim(SkSplitCSV(i))
143100130513     C                   ENDIF
143200130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_NO_2'
143300130513     C                   EVAL      %subst(w85:36:50) = %trim(SkSplitCSV(i))
143400130513     C                   ENDIF
143500130513
143600130513     C* converto una stringa oo:mm in variabile lunga 4,0 s
143700130513     C*** W_ORR
143800130513     C                   IF        %trim(SkSplitFLD(i)) = 'W_ORR'
143900130514     C* questo sarebbe il sistema di conversione con RPG se fossimo sempre certi che il separatore
144000130514     C* è ":" .
144100130514     C***                EVAL      VAOORR = %dec(%subst(%editc(
144200130514     C***                                 %dec(%time(%trim(SkSplitCSV(i))
144300130514     C***                                 + ':00' : *HMS):*HMS)
144400130514     C***                                 :'X') : 1 : 4):4:0)
144500130514     C* Siccome non lo si può essere, reperisco a pezzi la stringa
144600131029     C                   IF        SkSplitCSV(i) <> *blank
144700131029     C                             and %trim(SkSplitCSV(i)) <> ':'
144800130514     C                   EVAL      w5 = %trim(SkSplitCSV(i))
144900130514     C                   EVAL      VAOORR = %dec(%subst(w5:1:2)
145000130514     C                                           + %subst(w5:4:2) : 4 : 0)
145100130513     C                   ENDIF
145200131029     C                   ENDIF
145300121207
145400090428     C*
145500090428     C****************************************
145600090428     C*
145700030715     C*
145800030715     C                   ENDIF
145900030715     C                   EVAL      i = i + 1
146000030715     C                   ENDDO
146100040723     C*
146200121207     C***  ===>  Gestione campi "particolari" fuori ciclo
146300121207     C*
146400130513
146500130513     C                   EVAL      VAORSR = %trim(wRSR1) + ' ' + %trim(wRSR2)
146600130513     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
146700130513     C     '@':'A'       XLATE     VAORSR        VAORSR
146800130513     C                   EVAL      wRSR1 = *blank
146900130513     C                   EVAL      wRSR2 = *blank
147000130513
147100130513     C                   EVAL      VAORSC = %trim(wRSC1) + ' ' + %trim(wRSC2)
147200130513     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
147300130513     C     '@':'A'       XLATE     VAORSC        VAORSC
147400130513     C                   EVAL      wRSC1 = *blank
147500130513     C                   EVAL      wRSC2 = *blank
147600130513
147700130513     C                   EVAL      VAONO1 = %subst(w85:1:35)
147800130513     C                   EVAL      VAONO2 = %subst(w85:36:35)
147900131216
148000131216      * se la nazione è stata impostata come I o IT la pulisco
148100131216     C                   IF        VAONAR = 'I' or
148200131216     C                             VAONAR = 'IT'
148300131216     C                   EVAL      VAONAR = *blank
148400131216     C                   ENDIF
148500140519     C                   IF        VAONAC = 'I' or
148600140519     C                             VAONAC = 'IT'
148700140519     C                   EVAL      VAONAC = *blank
148800140519     C                   ENDIF
148900131216
149000131216     C* Reperisco la provincia dal CAP e dalla località
149100131216     C                   IF        VAOCAR <> *blanks AND
149200140519     C                             VAOLOR <> *blanks AND
149300131216     C                             VAOPRR  = *blanks AND
149400131216     C                             VAONAR  = *blanks
149500131216     C                   CLEAR                   TISI95DS
149600131216     C                   EVAL      I95TCN = '3'
149700131216     C                   Z-ADD     datcor        I95DAT
149800131216     C                   EVAL      I95CAP = VAOCAR
149900131216     C                   EVAL      I95LOC = VAOLOR
150000131216     C                   CALL      'TISI95R'
150100131216     C                   PARM                    TISI95DS
150200131216     C                   EVAL      VAOPRR = O95PRV
150300131216     C                   ENDIF
150400140519     C                   IF        VAOCAC <> *blanks AND
150500140519     C                             VAOLOC <> *blanks AND
150600140519     C                             VAOPRC  = *blanks AND
150700140519     C                             VAONAC  = *blanks
150800140519     C                   CLEAR                   TISI95DS
150900140519     C                   EVAL      I95TCN = '3'
151000140519     C                   Z-ADD     datcor        I95DAT
151100140519     C                   EVAL      I95CAP = VAOCAC
151200140519     C                   EVAL      I95LOC = VAOLOC
151300140519     C                   CALL      'TISI95R'
151400140519     C                   PARM                    TISI95DS
151500140519     C                   EVAL      VAOPRC = O95PRV
151600140519     C                   ENDIF
151700130513
151800030715     C                   ENDIF
151900020204     C*
152000010607     C                   ENDSR
152100010607     C*----------------------------------------------------*
152200080702
152300080702
152400080702     C*----------------------------------------------------*
152500080702     C*  CONTROLLO NUMERICITA' CAMPI
152600080702     C*----------------------------------------------------*
152700080702     C     CHKNUM        BEGSR
152800080702     C*
152900080702     C                   IF        PiDecChr = *blanks
153000080702     C                   EVAL      PiDecChr = ','
153100080702     C                   ENDIF
153200080702     C*
153300080702     C                   callp     UBISNUM_Check(PiStr
153400080702     C                                          :PiDecChr
153500080702     C                                          :PiVal
153600080702     C                                          :PiNum
153700080702     C                                          :PiInt)
153800080702     C*
153900080702     C                   ENDSR
154000080702     C***
154100080702
154200010601
154300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
154400010601     C     repfil        BEGSR
154500010601     C*
154600010601     C                   if        invfil = *zeros and
154700010601     C                             depfil > *zeros and
154800010629     C                             (vinflg = *blanks or
154900010629     C                              vinflg = *zeros)
155000010601     C
155100010601     C                   eval      invfil = depfil
155200010601     C                   endif
155300010601     C*
155400010601     C                   if        depfil <> invfil and
155500010601     C                             invfil > *zeros
155600010601     C                   eval      flgMulti = '1'
155700010601     C                   if        vinflg = *blanks
155800010601     C                   add       1             cntNonEl
155900010601     C                   endif
156000010601     C                   endif
156100010601     C*
156200010601     C                   if        vinflg = '2'
156300010601     C                   eval      flgStato = '2'
156400010601     C                   endif
156500010601     C*
156600010601     C                   ENDSR
156700010601     C***
156800010330
156900010330
157000990920      /TITLE Invio dei dati al punto operativo.
157100000613     C     invio         BEGSR
157200990920     C*
157300990920     C                   reset                   dscmz
157400010601     C                   move      invfil        cmzdst
157500990920     C                   eval      cmzfld = vlrfou
157600990920     C                   eval      cmzmbd = vlrhdl
157700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
157800000710     C                   if        prmfir = *blanks
157900040708     C                   eval      cmzfla = 'FNVAO00F'
158000040708     C                   eval      cmzmba = 'FNVAO00F'
158100000710     C                   else
158200000710     C                   eval      cmzfla = prmfir
158300000710     C                   eval      cmzmba = prmfir
158400000710     C                   endif
158500990920     C                   eval      cmznrr = *zeros
158600990920     C                   move      §ctrok        cmznrr
158700021018     C                   eval      cmzlba = vlrfl1
158800990920     C                   call(e)   'TIS711C'
158900990920     C                   parm                    dscmz
159000990921     C                   parm      *blanks       esito
159100990920     C                   if        %error
159200990920     C                             or cmzerr = '1'
159300990921     C                             or esito  = '1'
159400000710     C                   eval      wrkesito = '3'
159500990920     C                   endif
159600990920     C*
159700000613     C                   ENDSR
159800010601
159900010601
160000010601      /TITLE Invio dei dati al punto operativo.
160100010601     C     opeini        BEGSR
160200010601     C*
160300010601     C* Inizializzo flag e contatori operativi
160400010601     C                   movel     '0'           flgGiro           1
160500010601     C                   movel     '0'           flgMulti          1
160600010601     C                   movel     '1'           flgStato          1
160700010615     C                   movel     '0'           flgOk             1
160800010601     C                   z-add     *zeros        cntNonEl         10 0
160900010601     C                   z-add     *zeros        depfil            3 0
161000010601     C                   z-add     *zeros        invfil            3 0
161100010601     C*
161200010601     C                   ENDSR
161300010601     C***
161400050421
161500050421
161600091130     C     *pssr         BEGSR
161700050421     C*
161800091130     C                   eval      wrkesito = '2'
161900050421     C*
162000091130     C                   ENDSR     '*CANCL'
162100050421     C***
162200060306
162300060306
162400060306     C*-------------------------------------------------------------------------*
162500060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
162600060306     C*-------------------------------------------------------------------------*
162700060306     C     sr_Control    begsr
162800060306     C*
162900060306     C* inizializzo a ok il flag d controllo abilitazione
163000060306     C                   eval      Cli_Abil = *on
163100060306     C
163200060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
163300060306     C                   clear                   woriIDC
163400060306     C                   eval      kteCOD = 'VAP'
163500060306     C                   eval      kteKE1 = oriIDC
163600060306     C     keytbe        chain     TNTBE01L
163700060306     C                   if        %found(Tntbe01l)
163800060306     C                   eval      woriIDC = TbeKe2
163900060306     C                   else
164000060306     C                   eval      Cli_Abil = *off
164100060306     C                   endif
164200060306     C*
164300060306     C                   endsr
164400010330
164500010330
164600000613     C     *inzsr        BEGSR
164700990910     C*
164800990910     C     *entry        plist
164900990920     C                   parm                    tivlrds
165000990921     C                   parm      wrkesito      esito
165100000724     C                   parm                    prmlit
165200000710     C                   parm                    prmfir
165300010330     C*
165400010330     C* CALCOLA LA DATA CORRENTE
165500091127     C                   z-add     *zeros        datcor            8 0
165600091127     C                   time                    wn14             14 0
165700091127     C                   movel     wn14          wn6               6 0          *ORA (6)
165800091127     C                   eval      datcor = %dec(%date() : *ISO)
165900040723     C*
166000040723     C* Campi riferito al database
166100040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
166200040723     C     *like         define    tbeke1        kteKE1
166300040723     C*
166400040723     C* Definizione chiavi di lettura
166500040723     C* TNTBE01L - parziale
166600040723     C     keytbe        klist
166700040723     C                   kfld                    kteCOD                         *UTENTE
166800040723     C                   kfld                    kteKE1                         *CODICE
166900000613     C*
167000000613     C                   ENDSR
167100000613     C***
167200990908
167300040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
167400140516X;X;X;X;W_RSR_1;W_RSR_2;VAOINR;VAONAR;VAOCAR;VAOLOR;X;VAODAR;W_RSC_1;W_RSC_2;VAOINC;VAONAC;
167500130513VAOCAC;VAOLOC;VAONCL;X;X;VAOPKG;X;X;X;X;X;X;X;X;X;W_NO_1;W_NO_2;X;X;X;VAORFA;W_ORR;X;X;X;X;
167600130513VAORER;VAOTER;
