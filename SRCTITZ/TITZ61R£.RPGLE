000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121129      *
000300121129      * PARTICOLARITA':
000400121207      * per esigenze interne nostre deve essere creata una stringa di 15 caratteri comprensivi dei
000500121207      * seguenti elementi
000600121207      * - prime due caratteri devono riportare la dicitura RT
000700121207      * - il terzo carattere indica il marchio che può essere rilevato dal nostro campo Marchio
000800121207      * corrispondente al terzo carattere che può essere 0-1-2-3-4
000900121207      * - dal quarto carattere riportare i dati presenti nel campo N. lista
001000121207      * - dall'undicesimo carattere inserire i dati riportati nel campo Ord.(questo dato deve essere
001100121207      * fisso di 3 caratteri quindi in caso di un dato con un numero di cifre inferiori a 3 si deve
001200121207      * aggiungere di default degli zeri davanti)
001300121207      * - dal quattordicesimo carattere inserire i dati riportati nel campo Mult. (questo dato deve
001400121207      * essere fisso di 2 caratteri quindi in caso di un dato con un numero di cifre inferiori a 2
001500121207      * si deve aggiungere di default lo zero davanti)
001600121207      * - dal quattordicesimo carattere inserire i dati riportati nel campo Mult. (questo dato deve
001700121207      * essere fisso di 2 caratteri quindi in caso di un dato con un numero di cifre inferiori a 2
001800121207      * si deve aggiungere di default lo zero davanti)
001900140401      * - se non passati nei parametri, CRC e KSC vengono presi da COR
002000121129      *
002100121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
002200120209     F*
002300040723     Ffnacr01l  if   e           k disk
002400090618     Ftntbe01l  if   e           k disk
002500990910     Ftivin00r  uF   E             DISK    usropn
002600040708     FFnvaowwr  O    E             DISK    usropn
002700040723     Ftiori00f  O    E             DISK
002800050826     FTIVGD00F  O    E             DISK
002900000313     D*
003000040709     D*------------
003100040709     D* SCHIERE A PROGRAMMA
003200040709     D*------------
003300040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
003400010330     D*----------------------------------------------------
003500010330     D* DICHIARAZIOINE VARIABILI DI WRK
003600010330     D*----------------------------------------------------
003700010330     D dscmz         e ds                  inz
003800010330     D psds           sds
003900010330     D  procname         *PROC
004000010330     D tivlrds       e ds                  extname(tivlr00f)
004100050826     D fnvapds       e ds                  extname(fnvap00f)
004200040723     D dorm01        e ds
004300040723     D dfar          e ds
004400090618     d trul33ds      e ds
004500090618     d kpjba         e ds
004600010330     D esito           s              1
004700010330     D prmlit          s             10
004800010330     D prmfir          s             10
004900010330     D wrkesito        s                   like(esito)
005000010330     D rrnum           s              6  0 INZ(*zeros)
005100040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
005200060306     D Cli_Abil        s              1    inz(*off)
005300060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005400090618     d §numori         s              7  0 inz
005500120123     d wData_EUR       s               d   datfmt(*eur)
005600120123     d wData_ISO       s               d   datfmt(*iso)
005700120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
005800120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
005900120905     D Intestazioni    s               n   inz(*on)
006000030715     D*------------------
006100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006200030715     D*------------------
006300030715     D SkSplitFLD      S             10    DIM(100)
006400030715     D SkSplitCSV      S            256    DIM(100)
006500030715     D CharCSV         S              1
006600030715     D CharTXT         S              1
006700030715     D CharNUM         S              1
006800030715     D posDa           S              3  0 INZ(*zeros)
006900030715     D posA            S              3  0 INZ(*zeros)
007000030715     D i               s              3  0 INZ(1)
007100030715     D wGiro           s              1  0 INZ(*zeros)
007200030822     D*------------------
007300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007400030822     D*------------------
007500030822     D posDaDft        S              3  0 INZ(*zeros)
007600030822     D posADft         S              3  0 INZ(*zeros)
007700030822     D j               s              3  0 INZ(1)
007800030822     D wGiroDft        s              1  0 INZ(*zeros)
007900040723     D*------------------
008000040723     D* DS REPERIMENTO DATI CLIENTE
008100040723     D*-------------------
008200040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008300040723     D ACODS         E DS                  EXTNAME(CNACO00F)
008400040723     D INDDS         E DS                  EXTNAME(CNIND00F)
008500040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008600040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008700121129     D*------------------
008800121207     D wRFA1           s              1
008900121207     D wRFA2           s              7
009000121207     D wRFA3           s              3
009100121207     D wRFA4           s              2
009200080702
009300080702     D*------------------
009400080702     D* LINKING A DEFINIZIONI ESTERNE
009500080702     D*------------------
009600090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009800091026
009900091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
010000091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
010100091026
010200080702
010300031201     D*------------------
010400031201     D* Costanti
010500031201     D*------------------
010600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010800010330
010900010330
011000010330
011100000913     C                   reset                   rrnum
011200990921     C                   reset                   esito
011300990921     C                   reset                   wrkesito
011400010601     C*
011500010601     C                   exsr      opeini
011600041011     C                   exsr      PREELA
011700040708     C                   exsr      rwvao
011800010601     C*
011900010601     C                   seton                                        lr
012000010601
012100010601
012200010601
012300010601
012400010601     C*--------------------------------------------------------
012500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012600010601     C*--------------------------------------------------------
012700010601     C     PREELA        BEGSR
012800010601     C*
012900010601     C* SE OCCORRE SPEDIRE IN FILIALE
013000041011     C                   if        flgGiro = '0'
013100010601     C*
013200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013300010601     C                   eval      flgGiro = '1'
013400041011     C*
013500041011     C                   endif
013600010601     C*
013700010601     C                   ENDSR
013800010601     C***
013900010601
014000010601
014100010601     C*--------------------------------------------------------
014200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014300010601     C*--------------------------------------------------------
014400010601     C     ENDELA        BEGSR
014500000616     C*
014600120209     C                   ENDSR
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
015100010330     C*--------------------------------------------------------
015200040708     C     RWVAO         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700040708     C                   if        not %open(fnvaowwr)
015800040708     C                   open      fnvaowwr
015900010330     C                   endif
016000010330     C*
016100010604     C                   clear                   §CTROK
016200010604     C                   clear                   §CTRMO
016300010604     C                   clear                   §CTRNO
016400120123     C*
016500120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
016600120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
016700120905     C                   IF        Intestazioni = *off
016800120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
016900120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017000120123     C                   exsr      inzvar
017100120123     C                   exsr      impvao
017200120905     C                   ENDIF
017300010330     C*
017400030822     C                   do        *HIVAL
017500010330     C*
017600030822     C                   read      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018400040510     C                   z-add     *zeros        wLenVINDTA        4 0
018500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018900040510     C                   z-add     1             wFlgVINDTA
019000040510     C                   leave
019100040510     C                   endif
019200040510     C                   enddo
019300040510     C*
019400040218     C                   if        vindta > *blanks AND
019500040510     C                             wFlgVINDTA = 1
019600010330     C                   add       1             rrnum
019700010330     C*
019800010601     C                   if        *in70 = *off and
019900010330     C                             (vinflg = *blanks
020000010330     C                              or vinflg = '0'
020100010330     C                              or vinflg = '2')
020200010330     C*
020300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020400010711     C                   if        vinflg = *blanks or vinflg = '0'
020500010711     C                   clear                   vinmsg
020600010711     C                   endif
020700010601     C*
020800010330     C                   exsr      inzvar
020900040708     C                   exsr      impvao
021000010601     C*
021100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021200040708     C                   eval      depfil = VAOPOE
021300010601     C                   exsr      repfil
021400010601     C                   if        depfil = invfil
021500021025     C                   if        vlrpoi = 999
021600040708     C                   move(P)   invfil        VAOPOE
021700021025     C                   else
021800040708     C                   move(P)   vlrpoi        VAOPOE
021900021025     C                   endif
022000010601     C*
022100010604     C* Ebbene...
022200010604     C*
022300030822     C  N31              add       1             §CTROK            7 0
022400030822     C   32              add       1             §CTRMO            7 0
022500030822     C   31              add       1             §CTRNO            7 0
022600030822     C                   if        wGiro = 2
022700120123     C  N31              EXSR      CHKCOR
022800120123     C  N31              EXSR      WRIORI
022900120123     C  N31              EXSR      WRIVGD
023000120123     C  N31              write     FNVAO000
023100030822     C                   endif
023200010604     C*
023300010604     C                   if        *in31 = *off and
023400010604     C                             *in32 = *off
023500010604     C                   eval      vinflg = '1'
023600010604     C                   else
023700010604     C                   eval      vinflg = '2'
023800010604     C                   endif
023900010604     C                   endif
024000010601     C*
024100010604     C                   endif
024200010604     C*
024300010330     C                   else
024400010330     C                   eval      vinflg = '1'
024500010330     C                   endif
024600010601     C*
024700010601     C  N70              update    tivin000
024800010330     C*
024900030822     C  N70              enddo
025000010601     C
025100010601     C*
025200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025300010601     C                   if        cntNonEl = *zeros or
025400010601     C                             flgMulti = '0'
025500010330     C* Se non ci sono record con errori ...
025600010601     C                   if        §ctrno = 0 and
025700010604     C                             §ctrmo = 0 and
025800010601     C                             flgStato <> '2'
025900010330     C* ... restituisco esito OK.
026000010330     C                   eval      wrkesito = '0'
026100010330     C                   else
026200010330     C                   if        §ctrok > 0
026300010330     C                   eval      wrkesito = '1'
026400010330     C                   else
026500010615     C                   if        flgOk = '0'
026600010615     C                   eval      wrkesito = '2'
026700010615     C                   else
026800010615     C                   eval      wrkesito = '6'
026900010615     C                   endif
027000010330     C                   endif
027100010330     C                   endif
027200010601     C                   else
027300010601     C                   eval      wrkesito = '9'
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        %open(tivin00r)
027700010330     C                   close     tivin00r
027800010330     C                   endif
027900040708     C                   if        %open(fnvaowwr)
028000040708     C                   close     fnvaowwr
028100010330     C                   endif
028200010601     C*
028300010601     C                   if        vlrpoi <> 999
028400010601     C                   eval      invfil = vlrpoi
028500010601     C                   endif
028600010330     C*
028700010330     C                   if        §ctrok > 0
028800010601     C                             and invfil > *zeros
028900010330     C                   exsr      invio
029000010330     C                   endif
029100010601     C*
029200010618     C                   if        flgGiro = '1'
029300010601     C                   exsr      endela
029400010618     C                   endif
029500010330     C*
029600010330     C                   ENDSR
029700010330     C***
029800040723
029900040723
030000040723     C*------------------------------------------------------------------------*
030100040723     C* CHKCOR - Considerazioni su VAOCOR
030200040723     C*------------------------------------------------------------------------*
030300040723     C     CHKCOR        BEGSR
030400040723     C*
030500040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
030600040723     C                   if        vaoTCO = 'F'
030700040723     C* Quindi verifico se il codice ordinante non è valorizzato
030800040723     C                   if        vaoCOR = *zeros  AND
030900040723     C                             vaoRSO = *blanks AND
031000040723     C                             vaoINO = *blanks AND
031100040723     C                             vaoCAO = *blanks AND
031200040723     C                             vaoLOO = *blanks AND
031300040723     C                             vaoPRO = *blanks AND
031400040723     C                             vaoNAO = *blanks
031500040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
031600040723     C                   movel     '0'           wFlgAcr           1
031700040723     C                   move      vlrKSC        wCli              7 0
031800040723     C                   move(p)   wCli          acrCRO
031900040723     C     acrCRO        setll     fnacr01l
032000040723     C                   if        %found(fnacr01l)
032100040723     C                   read      fnacr01l
032200040723     C                   dow       not %eof(fnacr01l)
032300040723     C                   movel     acrCRO        wCliAcr           7 0
032400040723     C                   if        wCliAcr = wCli
032500040723     C                   eval      vaoCOR = acrCRO
032600040723     C                   movel     '1'           wFlgAcr
032700040723     C                   leave
032800040723     C                   else
032900040723     C                   leave
033000040723     C                   endif
033100040723     C                   read      fnacr01l
033200040723     C                   enddo
033300040723     C                   endif
033400040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
033500040723     C* dati anagrafici dell'ordinante dal piano dei conti
033600040723     C                   if        wFlgAcr = '0'
033700040723     C                   clear                   BS69DS
033800040723     C                   clear                   ACODS
033900040723     C                   clear                   INDDS
034000040723     C                   clear                   CLPDS
034100040723     C                   clear                   CLSDS
034200040723     C                   move(P)   vlrKSC        I69KAC
034300040723     C                   move(P)   vlrKSC        I69KIN
034400040723     C                   call      'TIBS69R'
034500040723     C                   parm                    BS69DS
034600040723     C                   parm                    ACODS
034700040723     C                   parm                    INDDS
034800040723     C                   parm                    CLPDS
034900040723     C                   parm                    CLSDS
035000040723     C     O69ERR        ifne      '1'
035100040723     C                   eval      vaoRSO = ACORAG
035200040723     C                   eval      vaoINO = INDVIA
035300040723     C                   movel(P)  INDCAP        vaoCAO
035400040723     C                   eval      vaoLOO = INDCIT
035500040723     C                   eval      vaoPRO = INDPRV
035600040723     C                   eval      vaoNAO = INDSTA
035700040723     C                   endif
035800040723     C                   endif
035900040723     C                   endif
036000040723     C                   endif
036100040723     C*
036200040723     C                   ENDSR
036300040723
036400040723
036500040723     C*------------------------------------------------------------------------*
036600040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
036700040723     C*------------------------------------------------------------------------*
036800040723     C     WRIORI        BEGSR
036900040723     C*
037000090618     C* Come prima cosa stacco un numeratore da AZNUM
037100090618     C                   clear                   trul33ds
037200090618     C                   eval      I33OPE = *zeros
037300090618     C                   eval      I33CNU = 600
037400090618     C                   eval      I33NUM = 1
037500090618     C                   movel     TRUL33DS      KPJBU
037600090618     C                   call      'TRUL33R'
037700090618     C                   parm                    KPJBA
037800090618     C                   movel     KPJBU         TRUL33DS
037900090618     C                   if        O33ERR = *zeros
038000090618     C                   z-add     O33NRF        §numori
038100090618     c                   else
038200090618     c                   eval      §numori = *all'9'
038300090618     C                   endif
038400040723     C*
038500040723     C* Quindi imposto i campi "extra-VAO"
038600040723     C                   if        vlrKSC = '0ORM0000'
038700040723     C*
038800040723     C                   if        vaoCOR > *zeros
038900040723     C                   movel     vaoCOR        wrkCOR7           7 0
039000040723     C*
039100040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
039200040723     C* codice padre. Se è un padre assegno già il codice ordinante.
039300040723     C                   clear                   tibs10ds
039400040723     C                   eval      d10drf = datcor                              *data riferimento
039500040723     C                   eval      d10paf = 'P'                                 *cerca il padre
039600040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
039700091026     C*
039800091026     C* Preparazione chiamata al TIBS10R1
039900091026     C                   clear                   tibs10r1ds
040000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
040100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
040200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
040300091026     C*
040400091026     C                   call      'TIBS10R1'
040500091026     C                   parm                    tibs10r1ds
040600091026     C*
040700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
040800040723     C*
040900040723     C                   if        d10err <> *blanks or                         *No unif. o padre
041000040723     C                             d10cop =  *zeros  or
041100040723     C                             d10cop =  d10cod
041200040723     C                   move      d10cod        oriIDC
041300040723     C                   else                                                   *Figlio
041400040723     C                   move      d10cop        oriIDC
041500040723     C                   endif
041600040723     C*
041700040723     C                   else
041800040723     C                   eval      oriIDC = *zeros
041900040723     C                   endif
042000040723     C*
042100040723     C                   else
042200040723     C                   eval      oriIDC = vlrKSC
042300040723     C                   endif
042400040723     C*
042500040723     C                   eval      oriPRG = §numori
042600040723     C                   eval      oriDER = datcor
042700040723     C                   eval      oriOER = wn6
042800040723     C                   eval      oriPOE = vaoPOE
042900040723     C                   eval      oriNSR = vaoNSR
043000040723     C                   eval      oriNOR = vaoNOR
043100040723     C                   eval      oriNRV = vaoNRV
043200040723     C                   eval      oriFLO = *blanks
043300040723     C*
043400040723     C* Infine valorizzo la chiave esterna sul file FNVAO
043500040723     C                   eval      dorm01 = vaoFLO
043600040723     C                   movel     §numori       §ormpg
043700050412     C*
043800130306     C* valorizzo DORM01 (in questo caso non ci sonodefault)
043900050412     C*
044000050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
044100040723     C                   eval      vaoFLO = dorm01
044200040723     C*
044300040723     C* ...quindi scrivo il file TIORI00F
044400120905     C  N55              WRITE     TIORI000
044500040723     C*
044600040723     C                   ENDSR
044700050826
044800050826
044900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
045000050826     C     wriVGD        BEGSR
045100050826     C*
045200050826     C* Reperisco la descrizione della fase da tabella
045300050826     C                   movel(p)  'FAR'         KteCOD
045400050826     C                   movel(p)  '000'         KteKE1
045500050826     C     KEYtbe        chain     tntbe01l
045600050826     C                   if        %found(tntbe01l)
045700050826     C                   movel(P)  tbeuni        DFAR
045800050826     C                   else
045900050826     C                   clear                   DFAR
046000050826     C                   endif
046100050826     C*
046200050826     C* Valorizzo buffer record
046300050826     C                   clear                   FNVAPDS
046400050826     C                   eval      vapIDC = oriIDC
046500050826     C                   eval      vapPOE = oriPOE
046600050826     C                   eval      vapRFA = vaoRFA
046700050826     C                   eval      vapPOG = oriPOE
046800050826     C                   eval      vapDAE = oriDER
046900050826     C                   eval      vapORE = oriOER
047000050826     C                   if        d§fardva <> *blanks
047100050826     C                   eval      vapDFA = d§fardva
047200050826     C                   else
047300050826     C                   eval      vapDFA = d§fardes
047400050826     C                   endif
047500050826     C*
047600050826     C                   clear                   tivgd000
047700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
047800050826     C                   eval      vgdTIP = 'VP'
047900050826     C                   eval      vgdKSU = vapIDC
048000050826     C                   eval      vgdTSC = 'WW'
048100050826     C                   eval      vgdDAT = datcor
048200120123     C                   eval      vgdPGM = 'TITZ10R'
048300060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
048400060306     C                   exsr      sr_Control
048500060306     C                   if        Cli_Abil = *on
048600060306     C                   eval      vgdKSU = woriIDC
048700120905     C  N55              write     tivgd000
048800060306     C                   endif
048900050826     C*
049000050826     C                   ENDSR
049100050826     C*------------------------------------------------------------------------*
049200010601
049300010601
049400010330     C*----------------------------------------------------*
049500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049600010330     C*----------------------------------------------------*
049700010330     C     INZVAR        BEGSR
049800010330     C*
049900030822     C* Inizializzo variabili di wrk
050000020204     C                   Z-ADD     *zeros        Num5_0            5 0
050100120124
050200030822     C*
050300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050400120123     C                   CLEAR                   FNVAO000
050500120123     C                   CLEAR                   TIORI000
050600030822     C                   CLEAR                   SkSplitCSV
050700030822     C                   EVAL      i = 1
050800030822     C                   EVAL      posDa = *zeros
050900030822     C                   EVAL      posA  = *zeros
051000030822     C*
051100030822     C* Reimposto i valori di default
051200030822     C                   EXSR      DEFCAM
051300030822     C*
051400010330     C                   ENDSR
051500010330     C*----------------------------------------------------*
051600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051700010330     C*----------------------------------------------------*
051800010330     C     DEFCAM        BEGSR
051900030822     C*
052000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052100030715     C* e delimitatore testo.
052200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052500040723     C*
052600080707     C* Imposto i valori d default d alcuni campi
052700080708     C*
052800080708     C                   CLEAR                   DORM01
052900120822     C                   EVAL      VAOFLO = DORM01
053000080707     C*
053100121207     C                   EVAL      VAOPOE = 045
053200121207     C                   EVAL      VAOTOR = 'S'
053300121207     C                   EVAL      VAOCOR = 0450652000
053400140401     C***                EVAL      VAOCRC = 0450652000
053500140401     C***                EVAL      VAOKSC = 0450652
053600121207     C                   EVAL      VAONAM = 'RESI'
053700121207     C                   EVAL      VAONCL = 1
053800121207     C                   EVAL      VAOPKG = 1
053900121207     C                   EVAL      VAOPAG = 'O'
054000121207     C                   EVAL      VAOCTR = '000'
054100120905     C* se il 1° char dei parametri è T=Test
054200120905     C                   SETOFF                                       55
054300120905     C                   IF        %subst(vlrppt:1:1) = 'T'
054400120905     C* inibisco la scrittura dei file TIORI e TIVGD
054500120905     C                   SETON                                        55
054600120905     C* imposto il PO di emissione come filiale fittizia
054700120905     C                   EVAL      VAOPOE = 999
054800120905     C                   ENDIF
054900091127     C*
055000091204     C* IMPOSTO I CAMPI PERSONALIZZATI
055100100416     C
055200030822     C*
055300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055400030822     C                   EVAL      posDaDft = 1
055500030822     C                   EVAL      posADft  = 0
055600030822     C                   EVAL      wGiroDft = 0
055700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055800030822     C                             posDaDft > 0
055900030822     C*
056000030822     C* Gestisco il 1° giro
056100030822     C                   IF        wGiroDft = 0
056200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056400030822     C* Incremento il contatore dei "giri"
056500030822     C                   EVAL      wGiroDft = 1
056600030822     C                   ELSE
056700030822     C                   EVAL      posDaDft = posADft
056800030822     C                   ENDIF
056900030822     C* Eseguo lo scan x trovare la fine del campo corrente
057000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057100030822     C*
057200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057400030822     C                   IF        posDaDft > 0 AND
057500030822     C                             posADft  > 0
057600040708     C* POE
057700040708     C                   IF        %subst(
057800040708     C                             %subst(vlrppt:posDaDft+1:
057900040708     C                             posADft-posDaDft-1):1:3)
058000040708     C                             = 'POE'
058100040708     C                   EVAL      PiStr=%trim(%subst(
058200040708     C                             %subst(vlrppt:posDaDft+1:
058300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400040708     C                   EXSR      CHKNUM
058500040708     C                   IF        PiInt=*on
058600040708     C                   Z-ADD     PiVal         VAOPOE
058700040708     C                   ENDIF
058800040708     C                   ENDIF
058900040708     C* TOR
059000040708     C                   IF        %subst(
059100040708     C                             %subst(vlrppt:posDaDft+1:
059200040708     C                             posADft-posDaDft-1):1:3)
059300040708     C                             = 'TOR'
059400040708     C                   EVAL      VAOTOR=%trim(%subst(
059500040708     C                             %subst(vlrppt:posDaDft+1:
059600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700040708     C                   ENDIF
059800040708     C* TCO
059900040708     C                   IF        %subst(
060000040708     C                             %subst(vlrppt:posDaDft+1:
060100040708     C                             posADft-posDaDft-1):1:3)
060200040708     C                             = 'TCO'
060300040708     C                   EVAL      VAOTCO=%trim(%subst(
060400040708     C                             %subst(vlrppt:posDaDft+1:
060500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600040708     C                   ENDIF
060700040707     C* COR
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100040707     C                             = 'COR'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiInt=*on
061700040708     C                   Z-ADD     PiVal         VAOCOR
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000040708     C* CRC
062100040708     C                   IF        %subst(
062200040708     C                             %subst(vlrppt:posDaDft+1:
062300040708     C                             posADft-posDaDft-1):1:3)
062400040708     C                             = 'CRC'
062500040708     C                   EVAL      PiStr=%trim(%subst(
062600040708     C                             %subst(vlrppt:posDaDft+1:
062700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800040708     C                   EXSR      CHKNUM
062900040708     C                   IF        PiInt=*on
063000040708     C                   Z-ADD     PiVal         VAOCRC
063100040708     C                   ENDIF
063200040708     C                   ENDIF
063300040708     C* FFD
063400040708     C                   IF        %subst(
063500040708     C                             %subst(vlrppt:posDaDft+1:
063600040708     C                             posADft-posDaDft-1):1:3)
063700040708     C                             = 'FFD'
063800040708     C                   EVAL      VAOFFD=%trim(%subst(
063900040708     C                             %subst(vlrppt:posDaDft+1:
064000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100040708     C                   ENDIF
064200040708     C* ORR
064300040708     C                   IF        %subst(
064400040708     C                             %subst(vlrppt:posDaDft+1:
064500040708     C                             posADft-posDaDft-1):1:3)
064600040708     C                             = 'ORR'
064700040708     C                   EVAL      PiStr=%trim(%subst(
064800040708     C                             %subst(vlrppt:posDaDft+1:
064900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000040708     C                   EXSR      CHKNUM
065100040708     C                   IF        PiInt=*on
065200040708     C                   Z-ADD     PiVal         VAOORR
065300040708     C                   ENDIF
065400040708     C                   ENDIF
065500040712     C* NCL
065600040712     C                   IF        %subst(
065700040712     C                             %subst(vlrppt:posDaDft+1:
065800040712     C                             posADft-posDaDft-1):1:3)
065900040712     C                             = 'NCL'
066000040712     C                   EVAL      PiStr=%trim(%subst(
066100040712     C                             %subst(vlrppt:posDaDft+1:
066200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300040712     C                   EXSR      CHKNUM
066400040712     C                   IF        PiInt=*on
066500040712     C                   Z-ADD     PiVal         VAONCL
066600040712     C                   ENDIF
066700040712     C                   ENDIF
066800040712     C* PKG
066900040712     C                   IF        %subst(
067000040712     C                             %subst(vlrppt:posDaDft+1:
067100040712     C                             posADft-posDaDft-1):1:3)
067200040712     C                             = 'PKG'
067300040712     C                   EVAL      PiStr=%trim(%subst(
067400040712     C                             %subst(vlrppt:posDaDft+1:
067500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600040712     C                   EXSR      CHKNUM
067700040712     C                   IF        PiNum=*on
067800040712     C                   Z-ADD     PiVal         VAOPKG
067900040712     C                   ENDIF
068000040712     C                   ENDIF
068100040712     C* VLM
068200040712     C                   IF        %subst(
068300040712     C                             %subst(vlrppt:posDaDft+1:
068400040712     C                             posADft-posDaDft-1):1:3)
068500040712     C                             = 'VLM'
068600040712     C                   EVAL      PiStr=%trim(%subst(
068700040712     C                             %subst(vlrppt:posDaDft+1:
068800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900040712     C                   EXSR      CHKNUM
069000040712     C                   IF        PiNum=*on
069100040712     C                   Z-ADD     PiVal         VAOVLM
069200040712     C                   ENDIF
069300040712     C                   ENDIF
069400040708     C* NAM
069500040708     C                   IF        %subst(
069600040708     C                             %subst(vlrppt:posDaDft+1:
069700040708     C                             posADft-posDaDft-1):1:3)
069800040708     C                             = 'NAM'
069900040708     C                   EVAL      VAONAM=%trim(%subst(
070000040708     C                             %subst(vlrppt:posDaDft+1:
070100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200040708     C                   ENDIF
070300040708     C* PAG
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700040708     C                             = 'PAG'
070800040708     C                   EVAL      VAOPAG=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200040708     C* KSC
071300040708     C                   IF        %subst(
071400040708     C                             %subst(vlrppt:posDaDft+1:
071500040708     C                             posADft-posDaDft-1):1:3)
071600040708     C                             = 'KSC'
071700040708     C                   EVAL      PiStr=%trim(%subst(
071800040708     C                             %subst(vlrppt:posDaDft+1:
071900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000040708     C                   EXSR      CHKNUM
072100040708     C                   IF        PiInt=*on
072200040708     C                   Z-ADD     PiVal         VAOKSC
072300040708     C                   ENDIF
072400040708     C                   ENDIF
072500040708     C* CTR
072600040708     C                   IF        %subst(
072700040708     C                             %subst(vlrppt:posDaDft+1:
072800040708     C                             posADft-posDaDft-1):1:3)
072900040708     C                             = 'CTR'
073000040708     C                   EVAL      VAOCTR=%trim(%subst(
073100040708     C                             %subst(vlrppt:posDaDft+1:
073200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300040708     C                   ENDIF
073400040708     C* POC
073500040708     C                   IF        %subst(
073600040708     C                             %subst(vlrppt:posDaDft+1:
073700040708     C                             posADft-posDaDft-1):1:3)
073800040708     C                             = 'POC'
073900040708     C                   EVAL      PiStr=%trim(%subst(
074000040708     C                             %subst(vlrppt:posDaDft+1:
074100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200040708     C                   EXSR      CHKNUM
074300040708     C                   IF        PiInt=*on
074400040708     C                   Z-ADD     PiVal         VAOPOC
074500040708     C                   ENDIF
074600040708     C                   ENDIF
074700040708     C* NO1
074800040708     C                   IF        %subst(
074900040708     C                             %subst(vlrppt:posDaDft+1:
075000121129     C                             posADft-posDaDft-1):1:3)
075100040708     C                             = 'NO1'
075200040708     C                   EVAL      VAONO1=%trim(%subst(
075300040708     C                             %subst(vlrppt:posDaDft+1:
075400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040708     C                   ENDIF
075600040708     C* DDT
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000040708     C                             = 'DDT'
076100040708     C                   EVAL      VAODDT=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500040708     C* RFA
076600040708     C                   IF        %subst(
076700040708     C                             %subst(vlrppt:posDaDft+1:
076800040708     C                             posADft-posDaDft-1):1:3)
076900040708     C                             = 'RFA'
077000040708     C                   EVAL      VAORFA=%trim(%subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300040708     C                   ENDIF
077400040708     C* FLO
077500040708     C                   IF        %subst(
077600040708     C                             %subst(vlrppt:posDaDft+1:
077700040708     C                             posADft-posDaDft-1):1:3)
077800040708     C                             = 'FLO'
077900040708     C                   EVAL      VAOFLO=%trim(%subst(
078000040708     C                             %subst(vlrppt:posDaDft+1:
078100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200091103     C*
078300040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
078400080702     C* e posizione 7 del VAOFLO
078500040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
078600040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
078700050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
078800040708     C                   ENDIF
078900030822     C* ...
079000030822     C                   ENDIF
079100030822     C                   ENDDO
079200020204     C*
079300010330     C                   ENDSR
079400010607     C*----------------------------------------------------*
079500040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
079600010607     C*----------------------------------------------------*
079700040708     C     IMPVAO        BEGSR
079800010607     C*
079900010607     C                   SETOFF                                       3132
080000120124
080100030515     C*
080200040708     C                   MOVEL     datcor        VAODAO
080300040708     C                   MOVEL     wn14          VAOOAO
080400050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080500050314     C     minu:maiu     XLATE     vindta        vindta
080600010607     C*
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080900030715     C**********
081000030715     C                   IF        wGiro = *zeros
081100040709     C*
081200040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
081300040709     C                   EVAL      depvinDTA = vinDTA
081400040709     C*
081500040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
081600040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081700040709     C                                      %trim(intcol(3))
081800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081900050314     C**!!!minu:maiu     XLATE     vindta        vindta
082000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082100040324     C                   DOW       posDa <= %len(%trimr(vindta))
082200030715     C*
082300030715     C* Gestisco il 1° campo
082400030715     C                   IF        i = 1
082500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082600030715     C                   EVAL      posDa = 1
082700030715     C* Eseguo lo scan x trovare la fine del primo campo
082800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082900030715     C* A questo punto "estraggo" il campo corrente
083000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083100030715     C                                                    (posA-posDa))
083200030715     C* X i campi successivi al 1°
083300030715     C                   ELSE
083400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083500030715     C                   EVAL      posDa = posA + 1
083600030715     C* Eseguo lo scan x trovare la fine del campo corrente
083700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083800030715     C* Gestisco l'ultimo campo
083900030715     C                   IF        posA = *zeros
084000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   LEAVE
084500030715     C                   ELSE
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C                   ENDIF
085000030715     C                   ENDIF
085100030715     C* Incremento il contatore d campo
085200030715     C                   EVAL      i = i +1
085300030715     C                   ENDDO
085400030715     C*
085500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085600030715     C* delimitazione testo
085700030715     C                   EVAL      i = 1
085800030715     C                   DOW       i < %elem(SkSplitFLD)
085900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086100030715     C                   EVAL      i = i + 1
086200030715     C                   ENDDO
086300030715     C                   EVAL      wGiro = 1
086400040709     C* Ripristino la 1° riga originale
086500040709     C                   EVAL      vinDTA = depvinDTA
086600030715     C                   ELSE
086700030715     C                   EVAL      wGiro = 2
086800031204     C**********
086900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087000031204     C* d separatore campo
087100031204     C**********
087200040709     C                   EVAL      vindta = %trim(vindta)
087300040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087400040709     C                   EVAL      vindta = %subst(vindta:2)
087500040709     C                   ENDDO
087600040709     C*
087700040709     C                   Z-ADD     *zeros        lunghInput        4 0
087800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
087900040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088000040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088100040709     C                   EVAL      lunghInput = %len(%trim(vindta))
088200040709     C                   ENDDO
088300030715     C**********
088400030715     C* Effettuo lo split del campo dati d input
088500030715     C**********
088600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088700040324     C                   DOW       posDa <= %len(%trimr(vindta))
088800030715     C*
088900030715     C* Gestisco il 1° campo
089000030715     C                   IF        i = 1
089100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089200030715     C                   EVAL      posDa = 1
089300030715     C* Eseguo lo scan x trovare la fine del primo campo
089400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C* X i campi successivi al 1°
089900030715     C                   ELSE
090000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090100030715     C                   EVAL      posDa = posA + 1
090200031204     C* Verifico che nn vi sia il campo nn valorizzato
090300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090400031204     C* Se campo nn valorizzato skippo al prossimo
090500031204     C                   EVAL      SkSplitCSV(i) = *blanks
090600031204     C                   EVAL      posA  = posA + 1
090700031204     C                   ELSE
090800030715     C* Eseguo lo scan x trovare la fine del campo corrente
090900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091000030715     C* Gestisco l'ultimo campo
091100030715     C                   IF        posA = *zeros
091200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   LEAVE
091700030715     C                   ELSE
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   ENDIF
092200031204     C                   ENDIF
092300030715     C                   ENDIF
092400030715     C* Incremento il contatore d campo
092500030715     C                   EVAL      i = i +1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092900030715     C* delimitazione testo
093000030715     C                   EVAL      i = 1
093100030715     C                   DOW       i < %elem(SkSplitCSV)
093200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093400030715     C                   EVAL      i = i + 1
093500030715     C                   ENDDO
093600030715     C*
093700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093800030715     C                   EVAL      i = 1
093900030715     C                   DOW       i < %elem(SkSplitFLD)
094000030715     C*
094100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094300030715     C                             SkSplitCSV(i) <> *blanks
094400030721     C*** ATB
094500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
094600040708     C                   EVAL      VAOATB = SkSplitCSV(i)
094700030721     C                   ENDIF
094800040708     C*** TOR
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
095000040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
095100040708     C                   ENDIF
095200040708     C*** TCO
095300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
095400040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
095500040708     C                   ENDIF
095600040708     C*** RSO
095700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
095800040708     C                   EVAL      VAORSO = SkSplitCSV(i)
095900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096000040708     C     '@':'A'       XLATE     VAORSO        VAORSO
096100030715     C* ==
096200030715     C                   ENDIF
096300040708     C*** INO
096400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
096500040708     C                   EVAL      VAOINO = SkSplitCSV(i)
096600030715     C                   ENDIF
096700040708     C*** LOO
096800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
096900040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
097000030715     C                   ENDIF
097100040708     C*** PRO
097200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
097300040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
097400030715     C                   ENDIF
097500040708     C*** NAO
097600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
097700040708     C                   EVAL      VAONAO = SkSplitCSV(i)
097800030721     C                   ENDIF
097900040708     C*** RSR
098000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
098100040708     C                   EVAL      VAORSR = SkSplitCSV(i)
098200040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098300040708     C     '@':'A'       XLATE     VAORSR        VAORSR
098400040708     C* ==
098500040708     C                   IF        VAORSR = *blanks
098600040708     C                   SETON                                        31
098700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098800040708     C                             + ' ' + 'VAORSR'
098900040708     C                   ENDIF
099000040708     C                   ENDIF
099100040708     C*** INR
099200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
099300040708     C                   EVAL      VAOINR = SkSplitCSV(i)
099400040708     C                   IF        VAOINR = *blanks
099500040708     C                   SETON                                        31
099600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099700040708     C                             + ' ' + 'VAOINR'
099800040708     C                   ENDIF
099900040708     C                   ENDIF
100000040708     C*** LOR
100100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
100200040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
100300040708     C                   IF        VAOLOR = *blanks
100400040708     C                   SETON                                        31
100500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100600040708     C                             + ' ' + 'VAOLOR'
100700040708     C                   ENDIF
100800040708     C                   ENDIF
100900040708     C*** PRR
101000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
101100040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
101200040708     C                   IF        VAOPRR = *blanks
101300040708     C                   SETON                                        31
101400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
101500040708     C                             + ' ' + 'VAOPRR'
101600040708     C                   ENDIF
101700040708     C                   ENDIF
101800040708     C*** NAR
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
102000040708     C                   EVAL      VAONAR = SkSplitCSV(i)
102100040708     C                   ENDIF
102200040708     C*** RER
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
102400040708     C                   EVAL      VAORER = SkSplitCSV(i)
102500040708     C                   ENDIF
102600040708     C*** TER
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
102800040708     C                   EVAL      VAOTER = SkSplitCSV(i)
102900040708     C                   ENDIF
103000040708     C*** RSC
103100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
103200040708     C                   EVAL      VAORSC = SkSplitCSV(i)
103300040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103400040708     C     '@':'A'       XLATE     VAORSC        VAORSC
103500040708     C* ==
103600040708     C                   ENDIF
103700040708     C*** INC
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
103900040708     C                   EVAL      VAOINC = SkSplitCSV(i)
104000040708     C                   ENDIF
104100040708     C*** LOC
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
104300040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
104400040708     C                   ENDIF
104500040708     C*** PRC
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
104700040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
104800040708     C                   ENDIF
104900040708     C*** NAC
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
105100040708     C                   EVAL      VAONAC = SkSplitCSV(i)
105200040708     C                   ENDIF
105300040708     C*** FFD
105400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
105500040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
105600040708     C                   ENDIF
105700040708     C*** RMP
105800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
105900040708     C                   EVAL      VAORMP = SkSplitCSV(i)
106000040708     C                   ENDIF
106100040708     C*** NAM
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
106300040708     C                   EVAL      VAONAM = SkSplitCSV(i)
106400040708     C                   ENDIF
106500040708     C*** PAG
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
106700040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
106800040708     C                   ENDIF
106900040708     C*** CTR
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
107100040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
107200040708     C                   ENDIF
107300040708     C*** ZOR
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
107500040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
107600040708     C                   ENDIF
107700040708     C*** NO1
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
107900040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
108000030715     C                   ENDIF
108100040708     C*** NO2
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
108300040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
108400030715     C                   ENDIF
108500040708     C*** DDT
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
108700040708     C                   EVAL      VAODDT = SkSplitCSV(i)
108800030715     C                   ENDIF
108900040708     C*** RFA
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
109100040708     C                   EVAL      VAORFA = SkSplitCSV(i)
109200030715     C                   ENDIF
109300040708     C*** SPI
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
109500040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
109600030715     C                   ENDIF
109700040708     C*** FLO
109800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
109900040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
110000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
110100080708     C* e la posizione 7 del VAOFLO
110200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
110300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
110400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
110500030715     C                   ENDIF
110600091103     C
110700091103     C*** DAR
110800050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
110900050412     C                   EVAL      PiStr=SkSplitCSV(i)
111000050412     C                   EXSR      CHKNUM
111100050412     C                   IF        PiInt=*on
111200050412     C                   Z-ADD     PiVal         VAODAR
111300050412     C                   ELSE
111400050412     C                   SETON                                        32
111500050412     C                   EVAL      VAODAR = *zeros
111600050412     C                   EVAL      vinmsg = %trimr(vinmsg)
111700050412     C                             + ' ' + 'VAODAR'
111800050412     C                   ENDIF
111900050314     C                   ENDIF
112000030715     C
112100030721     C*
112200030721     C* Reperisco quindi i campi numerici...
112300040708     C*** POE
112400040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
112500040723     C                   EVAL      PiStr=SkSplitCSV(i)
112600040723     C                   EXSR      CHKNUM
112700040723     C                   IF        PiInt=*on    AND
112800040723     C                             PiVal<=999   AND
112900040723     C                             PiVal>*zeros
113000040723     C                   Z-ADD     PiVal         VAOPOE
113100040723     C                   ELSE
113200040723     C                   SETON                                        32
113300040723     C                   EVAL      VAOPOE = *zeros
113400040723     C                   EVAL      vinmsg = %trimr(vinmsg)
113500040723     C                             + ' ' + 'VAOPOE'
113600040723     C                   ENDIF
113700040723     C                   ENDIF
113800040708     C*** COR
113900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
114000040708     C                   EVAL      PiStr=SkSplitCSV(i)
114100040708     C                   EXSR      CHKNUM
114200040708     C                   IF        PiInt=*on
114300040708     C                   Z-ADD     PiVal         VAOCOR
114400040708     C                   ELSE
114500040708     C                   SETON                                        31
114600040708     C                   EVAL      VAOCOR = *zeros
114700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
114800040708     C                             + ' ' + 'VAOCOR'
114900040708     C                   ENDIF
115000040708     C                   ENDIF
115100040708     C*** CAO
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on
115600030721     C                   Z-ADD     PiVal         Num5_0
115700040708     C                   MOVEL(p)  Num5_0        VAOCAO
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000040708     C                   EVAL      VAOCAO = *zeros
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200040708     C                             + ' ' + 'VAOCAO'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500040708     C*** CRA
116600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on
117000040708     C                   Z-ADD     PiVal         VAOCRA
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300040708     C                   EVAL      VAOCRA = *zeros
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500040708     C                             + ' ' + 'VAOCRA'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800040708     C*** CAR
117900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
118000040708     C                   EVAL      PiStr=SkSplitCSV(i)
118100040708     C                   EXSR      CHKNUM
118200040708     C                   IF        PiInt=*on
118300040708     C                   Z-ADD     PiVal         Num5_0
118400040708     C                   MOVEL(p)  Num5_0        VAOCAR
118500040708     C                   ELSE
118600040708     C                   SETON                                        31
118700040708     C                   EVAL      VAOCAR = *zeros
118800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040708     C                             + ' ' + 'VAOCAR'
119000040708     C                   ENDIF
119100040708     C                   ENDIF
119200040708     C*** CRC
119300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
119400040708     C                   EVAL      PiStr=SkSplitCSV(i)
119500040708     C                   EXSR      CHKNUM
119600040708     C                   IF        PiInt=*on
119700040708     C                   Z-ADD     PiVal         VAOCRC
119800040708     C                   ELSE
119900040708     C                   SETON                                        32
120000040708     C                   EVAL      VAOCRC = *zeros
120100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040708     C                             + ' ' + 'VAOCRC'
120300040708     C                   ENDIF
120400040708     C                   ENDIF
120500040708     C*** CAC
120600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
120700040708     C                   EVAL      PiStr=SkSplitCSV(i)
120800040708     C                   EXSR      CHKNUM
120900040708     C                   IF        PiInt=*on
121000040708     C                   Z-ADD     PiVal         Num5_0
121100040708     C                   MOVEL(p)  Num5_0        VAOCAC
121200040708     C                   ELSE
121300040708     C                   SETON                                        32
121400040708     C                   EVAL      VAOCAC = *zeros
121500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040708     C                             + ' ' + 'VAOCAC'
121700040708     C                   ENDIF
121800040708     C                   ENDIF
121900040708     C*** DAR
122000090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
122100090428     C                   EVAL      PiStr=SkSplitCSV(i)
122200090428     C                   EXSR      CHKNUM
122300090428     C                   IF        PiInt=*on
122400090428     C                   Z-ADD     PiVal         VAODAR
122500090428     C                   ELSE
122600090428     C                   SETON                                        32
122700090428     C                   EVAL      VAODAR = *zeros
122800090428     C                   EVAL      vinmsg = %trimr(vinmsg)
122900090428     C                             + ' ' + 'VAODAR'
123000090428     C                   ENDIF
123100090428     C                   ENDIF
123200040708     C*** ORR
123300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
123400040708     C                   EVAL      PiStr=SkSplitCSV(i)
123500040708     C                   EXSR      CHKNUM
123600040708     C                   IF        PiInt=*on
123700040708     C                   Z-ADD     PiVal         VAOORR
123800040708     C                   ELSE
123900040708     C                   SETON                                        32
124000040708     C                   EVAL      VAOORR = *zeros
124100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124200040708     C                             + ' ' + 'VAOORR'
124300040708     C                   ENDIF
124400040708     C                   ENDIF
124500030721     C*** NCL
124600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on
125000040723     C                   IF        PiVal > *zeros
125100040708     C                   Z-ADD     PiVal         VAONCL
125200040723     C                   ENDIF
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500040708     C                   EVAL      VAONCL = *zeros
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700040708     C                             + ' ' + 'VAONCL'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000040708     C*** PKG
126100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500040723     C                   IF        PiVal > *zeros
126600040708     C                   Z-ADD     PiVal         VAOPKG
126700040723     C                   ENDIF
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000040708     C                   EVAL      VAOPKG = *zeros
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200040708     C                             + ' ' + 'VAOPKG'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500040708     C*** VLM
127600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
127700040708     C                   EVAL      PiStr=SkSplitCSV(i)
127800040708     C                   EXSR      CHKNUM
127900040708     C                   IF        PiNum=*on
128000040708     C                   Z-ADD     PiVal         VAOVLM
128100040708     C                   ELSE
128200040708     C                   SETON                                        32
128300040708     C                   EVAL      VAOVLM = *zeros
128400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128500040708     C                             + ' ' + 'VAOVLM'
128600040708     C                   ENDIF
128700040708     C                   ENDIF
128800040708     C*** BNC
128900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
129000040708     C                   EVAL      PiStr=SkSplitCSV(i)
129100040708     C                   EXSR      CHKNUM
129200040708     C                   IF        PiInt=*on
129300040708     C                   Z-ADD     PiVal         VAOBNC
129400040708     C                   ELSE
129500040708     C                   SETON                                        32
129600040708     C                   EVAL      VAOBNC = *zeros
129700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129800040708     C                             + ' ' + 'VAOBNC'
129900040708     C                   ENDIF
130000040708     C                   ENDIF
130100040708     C*** BLC
130200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
130300040708     C                   EVAL      PiStr=SkSplitCSV(i)
130400040708     C                   EXSR      CHKNUM
130500040708     C                   IF        PiInt=*on
130600040708     C                   Z-ADD     PiVal         VAOBLC
130700040708     C                   ELSE
130800040708     C                   SETON                                        32
130900040708     C                   EVAL      VAOBLC = *zeros
131000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131100040708     C                             + ' ' + 'VAOBLC'
131200040708     C                   ENDIF
131300040708     C                   ENDIF
131400040708     C*** ATT
131500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
131600040708     C                   EVAL      PiStr=SkSplitCSV(i)
131700040708     C                   EXSR      CHKNUM
131800040708     C                   IF        PiInt=*on
131900040708     C                   Z-ADD     PiVal         VAOATT
132000040708     C                   ELSE
132100040708     C                   SETON                                        32
132200040708     C                   EVAL      VAOATT = *zeros
132300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132400040708     C                             + ' ' + 'VAOATT'
132500040708     C                   ENDIF
132600040708     C                   ENDIF
132700040708     C*** MTC
132800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
132900040708     C                   EVAL      PiStr=SkSplitCSV(i)
133000040708     C                   EXSR      CHKNUM
133100040708     C                   IF        PiInt=*on
133200040708     C                   Z-ADD     PiVal         VAOMTC
133300040708     C                   ELSE
133400040708     C                   SETON                                        32
133500040708     C                   EVAL      VAOMTC = *zeros
133600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133700040708     C                             + ' ' + 'VAOMTC'
133800040708     C                   ENDIF
133900040708     C                   ENDIF
134000040708     C*** KSC
134100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on      AND
134500030721     C                             PiVal<=9999999 AND
134600030721     C                             PiVal>*zeros
134700040708     C                   Z-ADD     PiVal         VAOKSC
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000040708     C                   EVAL      VAOKSC = *zeros
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200040708     C                             + ' ' + 'VAOKSC'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500040708     C*** POR
135600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on    AND
136000030721     C                             PiVal<=999   AND
136100030721     C                             PiVal>*zeros
136200040708     C                   Z-ADD     PiVal         VAOPOR
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500040708     C                   EVAL      VAOPOR = *zeros
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700040708     C                             + ' ' + 'VAOPOR'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000040708     C*** POC
137100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
137200040708     C                   EVAL      PiStr=SkSplitCSV(i)
137300040708     C                   EXSR      CHKNUM
137400040708     C                   IF        PiInt=*on    AND
137500040708     C                             PiVal<=999   AND
137600040708     C                             PiVal>*zeros
137700040708     C                   Z-ADD     PiVal         VAOPOC
137800040708     C                   ELSE
137900040708     C                   SETON                                        32
138000040708     C                   EVAL      VAOPOC = *zeros
138100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138200040708     C                             + ' ' + 'VAOPOC'
138300040708     C                   ENDIF
138400040708     C                   ENDIF
138500040708     C*** STO
138600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
138700040708     C                   EVAL      PiStr=SkSplitCSV(i)
138800040708     C                   EXSR      CHKNUM
138900040708     C                   IF        PiInt=*on
139000040708     C                   Z-ADD     PiVal         VAOSTO
139100040708     C                   ELSE
139200040708     C                   SETON                                        32
139300040708     C                   EVAL      VAOSTO = *zeros
139400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139500040708     C                             + ' ' + 'VAOSTO'
139600040708     C                   ENDIF
139700040708     C                   ENDIF
139800090428     C*
139900091103     C********** GESTIONE CAMPI PERSONALIZZATI
140000120120
140100121207     C*** RER
140200121207     C                   EVAL      VAORER = VAORSR
140300121207
140400121207     C*** RFA
140500121207     C* 1 char
140600121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_1'
140700121207     C                   EVAL      wRFA1=%char(%dec(%trim(SkSplitCSV(i)):1:0))
140800121207     C                   ENDIF
140900121207     C* 7 char
141000121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_2'
141100121207     C                   EVAL      wRFA2=%trim(SkSplitCSV(i))
141200121207     C                   ENDIF
141300121207     C* 3 char
141400121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_3'
141500121207     C                   EVAL      wRFA3=%editc(%dec(%trim(SkSplitCSV(i)):3:0)
141600121207     C                                   :'X')
141700121207     C                   ENDIF
141800121207     C* 2 char
141900121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_4'
142000121207     C                   EVAL      wRFA4=%editc(%dec(%trim(SkSplitCSV(i)):2:0)
142100121207     C                                   :'X')
142200121207     C                   ENDIF
142300090428     C*
142400090428     C****************************************
142500090428     C*
142600030715     C*
142700030715     C                   ENDIF
142800030715     C                   EVAL      i = i + 1
142900030715     C                   ENDDO
143000040723     C*
143100121207     C***  ===>  Gestione campi "particolari" fuori ciclo
143200121207     C*
143300121207     C* VAORFA va valorizzato concatenando più colonne
143400121207     C                   EVAL      VAORFA='RT' + wRFA1 + wRFA2 + wRFA3 + wRFA4
143500121207     C*
143600140401     C* se non passati nei parametri, CRC e KSC vengono presi da COR
143700140401     C                   IF        VAOCRC = *zeros
143800140401     C                   EVAL      VAOCRC = VAOCOR
143900140401     C                   ENDIF
144000140401     C                   IF        VAOKSC = *zeros
144100140401     C                   EVAL      VAOKSC = VAOCOR/1000
144200140401     C                   ENDIF
144300140401     C*
144400030715     C                   ENDIF
144500020204     C*
144600010607     C                   ENDSR
144700010607     C*----------------------------------------------------*
144800080702
144900080702
145000080702     C*----------------------------------------------------*
145100080702     C*  CONTROLLO NUMERICITA' CAMPI
145200080702     C*----------------------------------------------------*
145300080702     C     CHKNUM        BEGSR
145400080702     C*
145500080702     C                   IF        PiDecChr = *blanks
145600080702     C                   EVAL      PiDecChr = ','
145700080702     C                   ENDIF
145800080702     C*
145900080702     C                   callp     UBISNUM_Check(PiStr
146000080702     C                                          :PiDecChr
146100080702     C                                          :PiVal
146200080702     C                                          :PiNum
146300080702     C                                          :PiInt)
146400080702     C*
146500080702     C                   ENDSR
146600080702     C***
146700080702
146800010601
146900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
147000010601     C     repfil        BEGSR
147100010601     C*
147200010601     C                   if        invfil = *zeros and
147300010601     C                             depfil > *zeros and
147400010629     C                             (vinflg = *blanks or
147500010629     C                              vinflg = *zeros)
147600010601     C
147700010601     C                   eval      invfil = depfil
147800010601     C                   endif
147900010601     C*
148000010601     C                   if        depfil <> invfil and
148100010601     C                             invfil > *zeros
148200010601     C                   eval      flgMulti = '1'
148300010601     C                   if        vinflg = *blanks
148400010601     C                   add       1             cntNonEl
148500010601     C                   endif
148600010601     C                   endif
148700010601     C*
148800010601     C                   if        vinflg = '2'
148900010601     C                   eval      flgStato = '2'
149000010601     C                   endif
149100010601     C*
149200010601     C                   ENDSR
149300010601     C***
149400010330
149500010330
149600990920      /TITLE Invio dei dati al punto operativo.
149700000613     C     invio         BEGSR
149800990920     C*
149900990920     C                   reset                   dscmz
150000010601     C                   move      invfil        cmzdst
150100990920     C                   eval      cmzfld = vlrfou
150200990920     C                   eval      cmzmbd = vlrhdl
150300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
150400000710     C                   if        prmfir = *blanks
150500040708     C                   eval      cmzfla = 'FNVAO00F'
150600040708     C                   eval      cmzmba = 'FNVAO00F'
150700000710     C                   else
150800000710     C                   eval      cmzfla = prmfir
150900000710     C                   eval      cmzmba = prmfir
151000000710     C                   endif
151100990920     C                   eval      cmznrr = *zeros
151200990920     C                   move      §ctrok        cmznrr
151300021018     C                   eval      cmzlba = vlrfl1
151400990920     C                   call(e)   'TIS711C'
151500990920     C                   parm                    dscmz
151600990921     C                   parm      *blanks       esito
151700990920     C                   if        %error
151800990920     C                             or cmzerr = '1'
151900990921     C                             or esito  = '1'
152000000710     C                   eval      wrkesito = '3'
152100990920     C                   endif
152200990920     C*
152300000613     C                   ENDSR
152400010601
152500010601
152600010601      /TITLE Invio dei dati al punto operativo.
152700010601     C     opeini        BEGSR
152800010601     C*
152900010601     C* Inizializzo flag e contatori operativi
153000010601     C                   movel     '0'           flgGiro           1
153100010601     C                   movel     '0'           flgMulti          1
153200010601     C                   movel     '1'           flgStato          1
153300010615     C                   movel     '0'           flgOk             1
153400010601     C                   z-add     *zeros        cntNonEl         10 0
153500010601     C                   z-add     *zeros        depfil            3 0
153600010601     C                   z-add     *zeros        invfil            3 0
153700010601     C*
153800010601     C                   ENDSR
153900010601     C***
154000050421
154100050421
154200091130     C     *pssr         BEGSR
154300050421     C*
154400091130     C                   eval      wrkesito = '2'
154500050421     C*
154600091130     C                   ENDSR     '*CANCL'
154700050421     C***
154800060306
154900060306
155000060306     C*-------------------------------------------------------------------------*
155100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
155200060306     C*-------------------------------------------------------------------------*
155300060306     C     sr_Control    begsr
155400060306     C*
155500060306     C* inizializzo a ok il flag d controllo abilitazione
155600060306     C                   eval      Cli_Abil = *on
155700060306     C
155800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
155900060306     C                   clear                   woriIDC
156000060306     C                   eval      kteCOD = 'VAP'
156100060306     C                   eval      kteKE1 = oriIDC
156200060306     C     keytbe        chain     TNTBE01L
156300060306     C                   if        %found(Tntbe01l)
156400060306     C                   eval      woriIDC = TbeKe2
156500060306     C                   else
156600060306     C                   eval      Cli_Abil = *off
156700060306     C                   endif
156800060306     C*
156900060306     C                   endsr
157000010330
157100010330
157200000613     C     *inzsr        BEGSR
157300990910     C*
157400990910     C     *entry        plist
157500990920     C                   parm                    tivlrds
157600990921     C                   parm      wrkesito      esito
157700000724     C                   parm                    prmlit
157800000710     C                   parm                    prmfir
157900010330     C*
158000010330     C* CALCOLA LA DATA CORRENTE
158100091127     C                   z-add     *zeros        datcor            8 0
158200091127     C                   time                    wn14             14 0
158300091127     C                   movel     wn14          wn6               6 0          *ORA (6)
158400091127     C                   eval      datcor = %dec(%date() : *ISO)
158500040723     C*
158600040723     C* Campi riferito al database
158700040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
158800040723     C     *like         define    tbeke1        kteKE1
158900040723     C*
159000040723     C* Definizione chiavi di lettura
159100040723     C* TNTBE01L - parziale
159200040723     C     keytbe        klist
159300040723     C                   kfld                    kteCOD                         *UTENTE
159400040723     C                   kfld                    kteKE1                         *CODICE
159500000613     C*
159600000613     C                   ENDSR
159700000613     C***
159800990908
159900040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
160000121207X;W_VAORFA_1;X;VAORSR;VAOINR;VAOLOR;VAOPRR;VAOCAR;
160100121207W_VAORFA_2;W_VAORFA_3;W_VAORFA_4;X;X;X;X;X;X;X;X;VAONO1;
160200121207VAONO2;VAOTER
