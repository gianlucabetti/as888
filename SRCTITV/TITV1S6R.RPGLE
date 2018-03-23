000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200080702     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500040723     Ffnacr01l  if   e           k disk
000600090618     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800040708     FFnvaowwr  O    E             DISK    usropn
000900040723     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100091103     Ftitv1s6p  O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300091103     Ftitv1s6ps O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040709     D*------------
002100040709     D* SCHIERE A PROGRAMMA
002200040709     D*------------
002300040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002400010330     D*----------------------------------------------------
002500010330     D* DICHIARAZIOINE VARIABILI DI WRK
002600010330     D*----------------------------------------------------
002700010330     D dscmz         e ds                  inz
002800010330     D psds           sds
002900010330     D  procname         *PROC
003000010330     D tivlrds       e ds                  extname(tivlr00f)
003100050826     D fnvapds       e ds                  extname(fnvap00f)
003200040723     D dorm01        e ds
003300040723     D dfar          e ds
003400090618     d trul33ds      e ds
003500090618     d kpjba         e ds
003600010330     D esito           s              1
003700010330     D prmlit          s             10
003800010330     D prmfir          s             10
003900010330     D wrkesito        s                   like(esito)
004000010330     D wrkdata         s               d
004100010330     D wrkora          s               t
004200010330     D rrnum           s              6  0 INZ(*zeros)
004300010330     D recko           s            150    INZ(*blanks)
004400011113     D depcmd          s            150    INZ(*blanks)
004500040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004600060306     D Cli_Abil        s              1    inz(*off)
004700060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004800090618     d §numori         s              7  0 inz
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000030715     D wGiro           s              1  0 INZ(*zeros)
006100030822     D*------------------
006200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006300030822     D*------------------
006400030822     D posDaDft        S              3  0 INZ(*zeros)
006500030822     D posADft         S              3  0 INZ(*zeros)
006600030822     D j               s              3  0 INZ(1)
006700030822     D wGiroDft        s              1  0 INZ(*zeros)
006800010330     D*------------------
006900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007000010330     D*------------------
007100010330     D WLBDA8          DS                  INZ
007200010330     D  G08DAT                 1      8  0
007300010330     D  G08INV                 9     16  0
007400010330     D  G08ERR                17     17
007500010330     D  G08TGI                18     22  0
007600040723     D*------------------
007700040723     D* DS REPERIMENTO DATI CLIENTE
007800040723     D*-------------------
007900040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008000040723     D ACODS         E DS                  EXTNAME(CNACO00F)
008100040723     D INDDS         E DS                  EXTNAME(CNIND00F)
008200040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008300040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008400080702     D*------------------
008500080702
008600080702     D*------------------
008700080702     D* LINKING A DEFINIZIONI ESTERNE
008800080702     D*------------------
008900090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009000090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100091026
009200091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
009300091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
009400091026
009500080702
009600031201     D*------------------
009700031201     D* Costanti
009800031201     D*------------------
009900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010100010330
010200010330
010300010330
010400990915     C                   time                    wrkdata
010500990915     C                   time                    wrkora
010600000913     C                   reset                   rrnum
010700990921     C                   reset                   esito
010800990921     C                   reset                   wrkesito
010900010601     C*
011000010601     C                   exsr      opeini
011100041011     C                   exsr      PREELA
011200040708     C                   exsr      rwvao
011300010601     C*
011400010601     C                   seton                                        lr
011500010601
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012100010601     C*--------------------------------------------------------
012200010601     C     PREELA        BEGSR
012300010601     C*
012400010601     C* SE OCCORRE SPEDIRE IN FILIALE
012500041011     C                   if        flgGiro = '0'
012600010601     C*
012700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010601     C                   eval      flgGiro = '1'
012900000724     C*
013000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013200041011     C     vlrpoi        chain     azorg01l
013300000724     C                   if        %found
013400000724     C                   movel(p)  CMD(1)        depcmd
013500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013600000724     C*
013700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013800011113     C                   Z-ADD     150           LENGH            15 5
013900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014000000724     C                   PARM                    depcmd
014100000724     C                   PARM                    LENGH
014200000724     C*
014300041011     C                   else
014400041011     C                   setoff                                       50
014500000724     C                   endif
014600000616     C*
014700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014800000616     C                   movel(p)  CMD(2)        depcmd
014900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015000000616     C*
015100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015200011113     C                   Z-ADD     150           LENGH            15 5
015300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015400000616     C                   PARM                    depcmd
015500000616     C                   PARM                    LENGH
015600041011     C*
015700041011     C                   endif
015800000613     C*
015900091103     C                   if        not %open(titv1s6ps)
016000091103     C                   open      titv1s6ps
016100030715     C                   except    testdett
016200000613     C                   endif
016300010601     C*
016400010601     C                   ENDSR
016500010601     C***
016600010601
016700010601
016800010601
016900010601
017000010601
017100010601
017200010601
017300010601     C*--------------------------------------------------------
017400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017500010601     C*--------------------------------------------------------
017600010601     C     ENDELA        BEGSR
017700990915     C*
017800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017900000613     C*
018000091103     C                   if        %open(titv1s6ps)
018100000616     C                   except    findett
018200091103     C                   close     titv1s6ps
018300000613     C                   endif
018400000616     C*
018500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018600011113     C                   Z-ADD     150           LENGH            15 5
018700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018800000616     C                   PARM                    CMD(3)
018900000616     C                   PARM                    LENGH
019000000616     C*
019100010601     C                   ENDSR
019200010601     C***
019300010601
019400010601
019500010601
019600010601
019700000613
019800000613     C*--------------------------------------------------------
019900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020000000613     C*--------------------------------------------------------
020100000621     C     STPR          BEGSR
020200000613     C*
020300091103     C                   if        not %open(titv1s6p)
020400091103     C   50              open      titv1s6p
020500990915     C                   endif
020600990915     C*
020700030715     C   50              except    riepilogo
020800990915     C*
020900091103     C                   if        %open(titv1s6p)
021000091103     C   50              close     titv1s6p
021100990914     C                   endif
021200000613     C*
021300000613     C                   ENDSR
021400000613     C***
021500000613
021600010330
021700010601
021800010601
021900010601
022000010330     C*--------------------------------------------------------
022100040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
022200010330     C*--------------------------------------------------------
022300040708     C     RWVAO         BEGSR
022400010330     C*
022500010330     C                   if        not %open(tivin00r)
022600010330     C                   open      tivin00r
022700010330     C                   endif
022800040708     C                   if        not %open(fnvaowwr)
022900040708     C                   open      fnvaowwr
023000010330     C                   endif
023100010330     C*
023200010604     C                   clear                   §CTROK
023300010604     C                   clear                   §CTRMO
023400010604     C                   clear                   §CTRNO
023500010330     C*
023600030822     C                   do        *HIVAL
023700010330     C*
023800030822     C                   read      tivin00r                               70
023900010618     C*
024000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024100010618     C                   if        vinflg = '1'
024200010618     C                   eval      flgOk = '1'
024300010618     C                   endif
024400010618     C*
024500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024600040510     C                   z-add     *zeros        wLenVINDTA        4 0
024700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025100040510     C                   z-add     1             wFlgVINDTA
025200040510     C                   leave
025300040510     C                   endif
025400040510     C                   enddo
025500040510     C*
025600040218     C                   if        vindta > *blanks AND
025700040510     C                             wFlgVINDTA = 1
025800010330     C                   add       1             rrnum
025900010330     C*
026000010601     C                   if        *in70 = *off and
026100010330     C                             (vinflg = *blanks
026200010330     C                              or vinflg = '0'
026300010330     C                              or vinflg = '2')
026400010330     C*
026500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026600010711     C                   if        vinflg = *blanks or vinflg = '0'
026700010711     C                   clear                   vinmsg
026800010711     C                   endif
026900010601     C*
027000010330     C                   exsr      inzvar
027100040708     C                   exsr      impvao
027200010601     C*
027300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027400040708     C                   eval      depfil = VAOPOE
027500010601     C                   exsr      repfil
027600010601     C                   if        depfil = invfil
027700021025     C                   if        vlrpoi = 999
027800040708     C                   move(P)   invfil        VAOPOE
027900021025     C                   else
028000040708     C                   move(P)   vlrpoi        VAOPOE
028100021025     C                   endif
028200010601     C*
028300010604     C* Ebbene...
028400010604     C*
028500030822     C  N31              add       1             §CTROK            7 0
028600030822     C   32              add       1             §CTRMO            7 0
028700030822     C   31              add       1             §CTRNO            7 0
028800030822     C                   if        wGiro = 2
028900040723     C  N31              EXSR      CHKCOR
029000040723     C  N31              EXSR      WRIORI
029100050826     C  N31              EXSR      WRIVGD
029200040723     C  N31              write     FNVAO000
029300030822     C                   endif
029400010604     C*
029500010604     C                   if        *in31 = *off and
029600010604     C                             *in32 = *off
029700010604     C                   eval      vinflg = '1'
029800010604     C                   else
029900010604     C                   eval      recko = vindta
030000041011     C                   except    rigadett
030100010604     C                   eval      vinflg = '2'
030200010604     C                   endif
030300010604     C                   endif
030400010601     C*
030500010604     C                   endif
030600010604     C*
030700010330     C                   else
030800010330     C                   eval      vinflg = '1'
030900010330     C                   endif
031000010601     C*
031100010601     C  N70              update    tivin000
031200010330     C*
031300030822     C  N70              enddo
031400010601     C
031500010601     C*
031600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031700010601     C                   if        cntNonEl = *zeros or
031800010601     C                             flgMulti = '0'
031900010330     C* Se non ci sono record con errori ...
032000010601     C                   if        §ctrno = 0 and
032100010604     C                             §ctrmo = 0 and
032200010601     C                             flgStato <> '2'
032300010330     C* ... restituisco esito OK.
032400010330     C                   eval      wrkesito = '0'
032500010330     C                   else
032600010330     C                   if        §ctrok > 0
032700010330     C                   eval      wrkesito = '1'
032800010330     C                   else
032900010615     C                   if        flgOk = '0'
033000010615     C                   eval      wrkesito = '2'
033100010615     C                   else
033200010615     C                   eval      wrkesito = '6'
033300010615     C                   endif
033400010330     C                   endif
033500010330     C                   endif
033600010601     C                   else
033700010601     C                   eval      wrkesito = '9'
033800010601     C                   endif
033900010330     C*
034000010330     C                   if        %open(tivin00r)
034100010330     C                   close     tivin00r
034200010330     C                   endif
034300040708     C                   if        %open(fnvaowwr)
034400040708     C                   close     fnvaowwr
034500010330     C                   endif
034600010601     C*
034700010601     C                   if        vlrpoi <> 999
034800010601     C                   eval      invfil = vlrpoi
034900010601     C                   endif
035000010330     C*
035100010330     C                   if        §ctrok > 0
035200010601     C                             and invfil > *zeros
035300010330     C                   exsr      invio
035400010330     C                   endif
035500010601     C*
035600010618     C                   if        flgGiro = '1'
035700010601     C                   exsr      endela
035800010618     C                   endif
035900010330     C*
036000010330     C                   ENDSR
036100010330     C***
036200040723
036300040723
036400040723     C*------------------------------------------------------------------------*
036500040723     C* CHKCOR - Considerazioni su VAOCOR
036600040723     C*------------------------------------------------------------------------*
036700040723     C     CHKCOR        BEGSR
036800040723     C*
036900040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
037000040723     C                   if        vaoTCO = 'F'
037100040723     C* Quindi verifico se il codice ordinante non è valorizzato
037200040723     C                   if        vaoCOR = *zeros  AND
037300040723     C                             vaoRSO = *blanks AND
037400040723     C                             vaoINO = *blanks AND
037500040723     C                             vaoCAO = *blanks AND
037600040723     C                             vaoLOO = *blanks AND
037700040723     C                             vaoPRO = *blanks AND
037800040723     C                             vaoNAO = *blanks
037900040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
038000040723     C                   movel     '0'           wFlgAcr           1
038100040723     C                   move      vlrKSC        wCli              7 0
038200040723     C                   move(p)   wCli          acrCRO
038300040723     C     acrCRO        setll     fnacr01l
038400040723     C                   if        %found(fnacr01l)
038500040723     C                   read      fnacr01l
038600040723     C                   dow       not %eof(fnacr01l)
038700040723     C                   movel     acrCRO        wCliAcr           7 0
038800040723     C                   if        wCliAcr = wCli
038900040723     C                   eval      vaoCOR = acrCRO
039000040723     C                   movel     '1'           wFlgAcr
039100040723     C                   leave
039200040723     C                   else
039300040723     C                   leave
039400040723     C                   endif
039500040723     C                   read      fnacr01l
039600040723     C                   enddo
039700040723     C                   endif
039800040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
039900040723     C* dati anagrafici dell'ordinante dal piano dei conti
040000040723     C                   if        wFlgAcr = '0'
040100040723     C                   clear                   BS69DS
040200040723     C                   clear                   ACODS
040300040723     C                   clear                   INDDS
040400040723     C                   clear                   CLPDS
040500040723     C                   clear                   CLSDS
040600040723     C                   move(P)   vlrKSC        I69KAC
040700040723     C                   move(P)   vlrKSC        I69KIN
040800040723     C                   call      'TIBS69R'
040900040723     C                   parm                    BS69DS
041000040723     C                   parm                    ACODS
041100040723     C                   parm                    INDDS
041200040723     C                   parm                    CLPDS
041300040723     C                   parm                    CLSDS
041400040723     C     O69ERR        ifne      '1'
041500040723     C                   eval      vaoRSO = ACORAG
041600040723     C                   eval      vaoINO = INDVIA
041700040723     C                   movel(P)  INDCAP        vaoCAO
041800040723     C                   eval      vaoLOO = INDCIT
041900040723     C                   eval      vaoPRO = INDPRV
042000040723     C                   eval      vaoNAO = INDSTA
042100040723     C                   endif
042200040723     C                   endif
042300040723     C                   endif
042400040723     C                   endif
042500040723     C*
042600040723     C                   ENDSR
042700040723
042800040723
042900040723
043000040723     C*------------------------------------------------------------------------*
043100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
043200040723     C*------------------------------------------------------------------------*
043300040723     C     WRIORI        BEGSR
043400040723     C*
043500090618     C* Come prima cosa stacco un numeratore da AZNUM
043600090618     C                   clear                   trul33ds
043700090618     C                   eval      I33OPE = *zeros
043800090618     C                   eval      I33CNU = 600
043900090618     C                   eval      I33NUM = 1
044000090618     C                   movel     TRUL33DS      KPJBU
044100090618     C                   call      'TRUL33R'
044200090618     C                   parm                    KPJBA
044300090618     C                   movel     KPJBU         TRUL33DS
044400090618     C                   if        O33ERR = *zeros
044500090618     C                   z-add     O33NRF        §numori
044600090618     c                   else
044700090618     c                   eval      §numori = *all'9'
044800090618     C                   endif
044900040723     C*
045000040723     C* Quindi imposto i campi "extra-VAO"
045100040723     C                   if        vlrKSC = '0ORM0000'
045200040723     C*
045300040723     C                   if        vaoCOR > *zeros
045400040723     C                   movel     vaoCOR        wrkCOR7           7 0
045500040723     C*
045600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
045700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
045800040723     C                   clear                   tibs10ds
045900040723     C                   eval      d10drf = datcor                              *data riferimento
046000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
046100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
046200091026     C*
046300091026     C* Preparazione chiamata al TIBS10R1
046400091026     C                   clear                   tibs10r1ds
046500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
046600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
046700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
046800091026     C*
046900091026     C                   call      'TIBS10R1'
047000091026     C                   parm                    tibs10r1ds
047100091026     C*
047200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
047300040723     C*
047400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
047500040723     C                             d10cop =  *zeros  or
047600040723     C                             d10cop =  d10cod
047700040723     C                   move      d10cod        oriIDC
047800040723     C                   else                                                   *Figlio
047900040723     C                   move      d10cop        oriIDC
048000040723     C                   endif
048100040723     C*
048200040723     C                   else
048300040723     C                   eval      oriIDC = *zeros
048400040723     C                   endif
048500040723     C*
048600040723     C                   else
048700040723     C                   eval      oriIDC = vlrKSC
048800040723     C                   endif
048900040723     C*
049000040723     C                   eval      oriPRG = §numori
049100040723     C                   eval      oriDER = datcor
049200040723     C                   eval      oriOER = wn6
049300040723     C                   eval      oriPOE = vaoPOE
049400040723     C                   eval      oriNSR = vaoNSR
049500040723     C                   eval      oriNOR = vaoNOR
049600040723     C                   eval      oriNRV = vaoNRV
049700040723     C                   eval      oriFLO = *blanks
049800040723     C*
049900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
050000040723     C                   eval      dorm01 = vaoFLO
050100040723     C                   movel     §numori       §ormpg
050200050412     C*
050300050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
050400050412     C                   movel     'S'           §ormfr
050500050412     C*
050600050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
050700040723     C                   eval      vaoFLO = dorm01
050800040723     C*
050900040723     C* ...quindi scrivo il file TIORI00F
051000040723     C                   WRITE     TIORI000
051100040723     C*
051200040723     C                   ENDSR
051300050826
051400050826
051500050826
051600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
051700050826     C     wriVGD        BEGSR
051800050826     C*
051900050826     C* Reperisco la descrizione della fase da tabella
052000050826     C                   movel(p)  'FAR'         KteCOD
052100050826     C                   movel(p)  '000'         KteKE1
052200050826     C     KEYtbe        chain     tntbe01l
052300050826     C                   if        %found(tntbe01l)
052400050826     C                   movel(P)  tbeuni        DFAR
052500050826     C                   else
052600050826     C                   clear                   DFAR
052700050826     C                   endif
052800050826     C*
052900050826     C* Valorizzo buffer record
053000050826     C                   clear                   FNVAPDS
053100050826     C                   eval      vapIDC = oriIDC
053200050826     C                   eval      vapPOE = oriPOE
053300050826     C                   eval      vapRFA = vaoRFA
053400050826     C                   eval      vapPOG = oriPOE
053500050826     C                   eval      vapDAE = oriDER
053600050826     C                   eval      vapORE = oriOER
053700050826     C                   if        d§fardva <> *blanks
053800050826     C                   eval      vapDFA = d§fardva
053900050826     C                   else
054000050826     C                   eval      vapDFA = d§fardes
054100050826     C                   endif
054200050826     C*
054300050826     C                   clear                   tivgd000
054400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
054500050826     C                   eval      vgdTIP = 'VP'
054600050826     C                   eval      vgdKSU = vapIDC
054700050826     C                   eval      vgdTSC = 'WW'
054800050826     C                   eval      vgdDAT = datcor
054900091103     C                   eval      vgdPGM = 'TITV1S6R'
055000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
055100060306     C                   exsr      sr_Control
055200060306     C                   if        Cli_Abil = *on
055300060306     C                   eval      vgdKSU = woriIDC
055400050826     C                   write     tivgd000
055500060306     C                   endif
055600050826     C*
055700050826     C                   ENDSR
055800050826     C*------------------------------------------------------------------------*
055900010601
056000010601
056100010601
056200010330     C*----------------------------------------------------*
056300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
056400010330     C*----------------------------------------------------*
056500010330     C     INZVAR        BEGSR
056600010330     C*
056700030822     C* Inizializzo variabili di wrk
056800020204     C                   Z-ADD     *zeros        Num5_0            5 0
056900030822     C*
057000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
057100040708     C                   CLEAR                   FNVAO000
057200040723     C                   CLEAR                   TIORI000
057300030822     C                   CLEAR                   SkSplitCSV
057400030822     C                   EVAL      i = 1
057500030822     C                   EVAL      posDa = *zeros
057600030822     C                   EVAL      posA  = *zeros
057700030822     C*
057800030822     C* Reimposto i valori di default
057900030822     C                   EXSR      DEFCAM
058000030822     C*
058100010330     C                   ENDSR
058200010330     C*----------------------------------------------------*
058300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
058400010330     C*----------------------------------------------------*
058500010330     C     DEFCAM        BEGSR
058600030822     C*
058700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058800030715     C* e delimitatore testo.
058900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
059000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
059100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
059200030822     C*
059300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
059500030715     C                   SETON                                        50
059600030715     C                   ELSE
059700030715     C                   SETOFF                                       50
059800030715     C                   ENDIF
059900040723     C*
060000080707     C* Imposto i valori d default d alcuni campi
060100080708     C*
060200080708     C                   CLEAR                   DORM01
060300091103     C                   EVAL      VAOFLO = DORM01
060400080707     C*
060500091103     C                   EVAL      VAOTCO = 'F'
060600091103     C                   EVAL      VAOTOR = 'S'
060700030822     C*
060800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060900030822     C                   EVAL      posDaDft = 1
061000030822     C                   EVAL      posADft  = 0
061100030822     C                   EVAL      wGiroDft = 0
061200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061300030822     C                             posDaDft > 0
061400030822     C*
061500030822     C* Gestisco il 1° giro
061600030822     C                   IF        wGiroDft = 0
061700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061900030822     C* Incremento il contatore dei "giri"
062000030822     C                   EVAL      wGiroDft = 1
062100030822     C                   ELSE
062200030822     C                   EVAL      posDaDft = posADft
062300030822     C                   ENDIF
062400030822     C* Eseguo lo scan x trovare la fine del campo corrente
062500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062600030822     C*
062700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062900030822     C                   IF        posDaDft > 0 AND
063000030822     C                             posADft  > 0
063100040708     C* POE
063200040708     C                   IF        %subst(
063300040708     C                             %subst(vlrppt:posDaDft+1:
063400040708     C                             posADft-posDaDft-1):1:3)
063500040708     C                             = 'POE'
063600040708     C                   EVAL      PiStr=%trim(%subst(
063700040708     C                             %subst(vlrppt:posDaDft+1:
063800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900040708     C                   EXSR      CHKNUM
064000040708     C                   IF        PiInt=*on
064100040708     C                   Z-ADD     PiVal         VAOPOE
064200040708     C                   ENDIF
064300040708     C                   ENDIF
064400040708     C* TOR
064500040708     C                   IF        %subst(
064600040708     C                             %subst(vlrppt:posDaDft+1:
064700040708     C                             posADft-posDaDft-1):1:3)
064800040708     C                             = 'TOR'
064900040708     C                   EVAL      VAOTOR=%trim(%subst(
065000040708     C                             %subst(vlrppt:posDaDft+1:
065100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200040708     C                   ENDIF
065300040708     C* TCO
065400040708     C                   IF        %subst(
065500040708     C                             %subst(vlrppt:posDaDft+1:
065600040708     C                             posADft-posDaDft-1):1:3)
065700040708     C                             = 'TCO'
065800040708     C                   EVAL      VAOTCO=%trim(%subst(
065900040708     C                             %subst(vlrppt:posDaDft+1:
066000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100040708     C                   ENDIF
066200040707     C* COR
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600040707     C                             = 'COR'
066700030822     C                   EVAL      PiStr=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   EXSR      CHKNUM
067100030822     C                   IF        PiInt=*on
067200040708     C                   Z-ADD     PiVal         VAOCOR
067300030822     C                   ENDIF
067400030822     C                   ENDIF
067500040708     C* CRC
067600040708     C                   IF        %subst(
067700040708     C                             %subst(vlrppt:posDaDft+1:
067800040708     C                             posADft-posDaDft-1):1:3)
067900040708     C                             = 'CRC'
068000040708     C                   EVAL      PiStr=%trim(%subst(
068100040708     C                             %subst(vlrppt:posDaDft+1:
068200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300040708     C                   EXSR      CHKNUM
068400040708     C                   IF        PiInt=*on
068500040708     C                   Z-ADD     PiVal         VAOCRC
068600040708     C                   ENDIF
068700040708     C                   ENDIF
068800040708     C* FFD
068900040708     C                   IF        %subst(
069000040708     C                             %subst(vlrppt:posDaDft+1:
069100040708     C                             posADft-posDaDft-1):1:3)
069200040708     C                             = 'FFD'
069300040708     C                   EVAL      VAOFFD=%trim(%subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600040708     C                   ENDIF
069700040708     C* ORR
069800040708     C                   IF        %subst(
069900040708     C                             %subst(vlrppt:posDaDft+1:
070000040708     C                             posADft-posDaDft-1):1:3)
070100040708     C                             = 'ORR'
070200040708     C                   EVAL      PiStr=%trim(%subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500040708     C                   EXSR      CHKNUM
070600040708     C                   IF        PiInt=*on
070700040708     C                   Z-ADD     PiVal         VAOORR
070800040708     C                   ENDIF
070900040708     C                   ENDIF
071000040712     C* NCL
071100040712     C                   IF        %subst(
071200040712     C                             %subst(vlrppt:posDaDft+1:
071300040712     C                             posADft-posDaDft-1):1:3)
071400040712     C                             = 'NCL'
071500040712     C                   EVAL      PiStr=%trim(%subst(
071600040712     C                             %subst(vlrppt:posDaDft+1:
071700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800040712     C                   EXSR      CHKNUM
071900040712     C                   IF        PiInt=*on
072000040712     C                   Z-ADD     PiVal         VAONCL
072100040712     C                   ENDIF
072200040712     C                   ENDIF
072300040712     C* PKG
072400040712     C                   IF        %subst(
072500040712     C                             %subst(vlrppt:posDaDft+1:
072600040712     C                             posADft-posDaDft-1):1:3)
072700040712     C                             = 'PKG'
072800040712     C                   EVAL      PiStr=%trim(%subst(
072900040712     C                             %subst(vlrppt:posDaDft+1:
073000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040712     C                   EXSR      CHKNUM
073200040712     C                   IF        PiNum=*on
073300040712     C                   Z-ADD     PiVal         VAOPKG
073400040712     C                   ENDIF
073500040712     C                   ENDIF
073600040712     C* VLM
073700040712     C                   IF        %subst(
073800040712     C                             %subst(vlrppt:posDaDft+1:
073900040712     C                             posADft-posDaDft-1):1:3)
074000040712     C                             = 'VLM'
074100040712     C                   EVAL      PiStr=%trim(%subst(
074200040712     C                             %subst(vlrppt:posDaDft+1:
074300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400040712     C                   EXSR      CHKNUM
074500040712     C                   IF        PiNum=*on
074600040712     C                   Z-ADD     PiVal         VAOVLM
074700040712     C                   ENDIF
074800040712     C                   ENDIF
074900040708     C* NAM
075000040708     C                   IF        %subst(
075100040708     C                             %subst(vlrppt:posDaDft+1:
075200040708     C                             posADft-posDaDft-1):1:3)
075300040708     C                             = 'NAM'
075400040708     C                   EVAL      VAONAM=%trim(%subst(
075500040708     C                             %subst(vlrppt:posDaDft+1:
075600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700040708     C                   ENDIF
075800040708     C* PAG
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200040708     C                             = 'PAG'
076300040708     C                   EVAL      VAOPAG=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700040708     C* KSC
076800040708     C                   IF        %subst(
076900040708     C                             %subst(vlrppt:posDaDft+1:
077000040708     C                             posADft-posDaDft-1):1:3)
077100040708     C                             = 'KSC'
077200040708     C                   EVAL      PiStr=%trim(%subst(
077300040708     C                             %subst(vlrppt:posDaDft+1:
077400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500040708     C                   EXSR      CHKNUM
077600040708     C                   IF        PiInt=*on
077700040708     C                   Z-ADD     PiVal         VAOKSC
077800040708     C                   ENDIF
077900040708     C                   ENDIF
078000040708     C* CTR
078100040708     C                   IF        %subst(
078200040708     C                             %subst(vlrppt:posDaDft+1:
078300040708     C                             posADft-posDaDft-1):1:3)
078400040708     C                             = 'CTR'
078500040708     C                   EVAL      VAOCTR=%trim(%subst(
078600040708     C                             %subst(vlrppt:posDaDft+1:
078700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800040708     C                   ENDIF
078900040708     C* POC
079000040708     C                   IF        %subst(
079100040708     C                             %subst(vlrppt:posDaDft+1:
079200040708     C                             posADft-posDaDft-1):1:3)
079300040708     C                             = 'POC'
079400040708     C                   EVAL      PiStr=%trim(%subst(
079500040708     C                             %subst(vlrppt:posDaDft+1:
079600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700040708     C                   EXSR      CHKNUM
079800040708     C                   IF        PiInt=*on
079900040708     C                   Z-ADD     PiVal         VAOPOC
080000040708     C                   ENDIF
080100040708     C                   ENDIF
080200040708     C* NO1
080300040708     C                   IF        %subst(
080400040708     C                             %subst(vlrppt:posDaDft+1:
080500040708     C                             posADft-posDaDft-1):1:3)
080600040708     C                             = 'NO1'
080700040708     C                   EVAL      VAONO1=%trim(%subst(
080800040708     C                             %subst(vlrppt:posDaDft+1:
080900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000040708     C                   ENDIF
081100040708     C* DDT
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500040708     C                             = 'DDT'
081600040708     C                   EVAL      VAODDT=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000040708     C* RFA
082100040708     C                   IF        %subst(
082200040708     C                             %subst(vlrppt:posDaDft+1:
082300040708     C                             posADft-posDaDft-1):1:3)
082400040708     C                             = 'RFA'
082500040708     C                   EVAL      VAORFA=%trim(%subst(
082600040708     C                             %subst(vlrppt:posDaDft+1:
082700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800040708     C                   ENDIF
082900040708     C* FLO
083000040708     C                   IF        %subst(
083100040708     C                             %subst(vlrppt:posDaDft+1:
083200040708     C                             posADft-posDaDft-1):1:3)
083300040708     C                             = 'FLO'
083400040708     C                   EVAL      VAOFLO=%trim(%subst(
083500040708     C                             %subst(vlrppt:posDaDft+1:
083600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700091103     C*
083800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
083900080702     C* e posizione 7 del VAOFLO
084000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
084100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
084200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
084300040708     C                   ENDIF
084400030822     C* ...
084500030822     C                   ENDIF
084600030822     C                   ENDDO
084700020204     C*
084800010330     C                   ENDSR
084900010607     C*----------------------------------------------------*
085000040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
085100010607     C*----------------------------------------------------*
085200040708     C     IMPVAO        BEGSR
085300010607     C*
085400010607     C                   SETOFF                                       3132
085500030515     C*
085600040708     C                   MOVEL     datcor        VAODAO
085700040708     C                   MOVEL     wn14          VAOOAO
085800050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085900050314     C     minu:maiu     XLATE     vindta        vindta
086000010607     C*
086100030715     C**********
086200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
086300030715     C**********
086400030715     C                   IF        wGiro = *zeros
086500040709     C*
086600040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
086700040709     C                   EVAL      depvinDTA = vinDTA
086800040709     C*
086900040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
087000040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087100040709     C                                      %trim(intcol(3))
087200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087300050314     C**!!!minu:maiu     XLATE     vindta        vindta
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
088400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C* X i campi successivi al 1°
088700030715     C                   ELSE
088800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088900030715     C                   EVAL      posDa = posA + 1
089000030715     C* Eseguo lo scan x trovare la fine del campo corrente
089100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089200030715     C* Gestisco l'ultimo campo
089300030715     C                   IF        posA = *zeros
089400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C                   LEAVE
089900030715     C                   ELSE
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C                   ENDIF
090400030715     C                   ENDIF
090500030715     C* Incremento il contatore d campo
090600030715     C                   EVAL      i = i +1
090700030715     C                   ENDDO
090800030715     C*
090900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091000030715     C* delimitazione testo
091100030715     C                   EVAL      i = 1
091200030715     C                   DOW       i < %elem(SkSplitFLD)
091300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091500030715     C                   EVAL      i = i + 1
091600030715     C                   ENDDO
091700030715     C                   EVAL      wGiro = 1
091800040709     C* Ripristino la 1° riga originale
091900040709     C                   EVAL      vinDTA = depvinDTA
092000030715     C                   ELSE
092100030715     C                   EVAL      wGiro = 2
092200031204     C**********
092300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092400031204     C* d separatore campo
092500031204     C**********
092600040709     C                   EVAL      vindta = %trim(vindta)
092700040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092800040709     C                   EVAL      vindta = %subst(vindta:2)
092900040709     C                   ENDDO
093000040709     C*
093100040709     C                   Z-ADD     *zeros        lunghInput        4 0
093200040709     C                   EVAL      lunghInput = %len(%trim(vindta))
093300040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093400040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
093600040709     C                   ENDDO
093700030715     C**********
093800030715     C* Effettuo lo split del campo dati d input
093900030715     C**********
094000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094100040324     C                   DOW       posDa <= %len(%trimr(vindta))
094200030715     C*
094300030715     C* Gestisco il 1° campo
094400030715     C                   IF        i = 1
094500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094600030715     C                   EVAL      posDa = 1
094700030715     C* Eseguo lo scan x trovare la fine del primo campo
094800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C* X i campi successivi al 1°
095300030715     C                   ELSE
095400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095500030715     C                   EVAL      posDa = posA + 1
095600031204     C* Verifico che nn vi sia il campo nn valorizzato
095700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095800031204     C* Se campo nn valorizzato skippo al prossimo
095900031204     C                   EVAL      SkSplitCSV(i) = *blanks
096000031204     C                   EVAL      posA  = posA + 1
096100031204     C                   ELSE
096200030715     C* Eseguo lo scan x trovare la fine del campo corrente
096300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096400030715     C* Gestisco l'ultimo campo
096500030715     C                   IF        posA = *zeros
096600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
096700030715     C* A questo punto "estraggo" il campo corrente
096800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096900030715     C                                                    (posA-posDa))
097000030715     C                   LEAVE
097100030715     C                   ELSE
097200030715     C* A questo punto "estraggo" il campo corrente
097300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097400030715     C                                                    (posA-posDa))
097500030715     C                   ENDIF
097600031204     C                   ENDIF
097700030715     C                   ENDIF
097800030715     C* Incremento il contatore d campo
097900030715     C                   EVAL      i = i +1
098000030715     C                   ENDDO
098100030715     C*
098200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098300030715     C* delimitazione testo
098400030715     C                   EVAL      i = 1
098500030715     C                   DOW       i < %elem(SkSplitCSV)
098600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098800030715     C                   EVAL      i = i + 1
098900030715     C                   ENDDO
099000030715     C*
099100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitFLD)
099400030715     C*
099500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099700030715     C                             SkSplitCSV(i) <> *blanks
099800030721     C*** ATB
099900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
100000040708     C                   EVAL      VAOATB = SkSplitCSV(i)
100100030721     C                   ENDIF
100200040708     C*** TOR
100300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
100400040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
100500040708     C                   ENDIF
100600040708     C*** TCO
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
100800040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
100900040708     C                   ENDIF
101000040708     C*** RSO
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
101200040708     C                   EVAL      VAORSO = SkSplitCSV(i)
101300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101400040708     C     '@':'A'       XLATE     VAORSO        VAORSO
101500030715     C* ==
101600030715     C                   ENDIF
101700040708     C*** INO
101800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
101900040708     C                   EVAL      VAOINO = SkSplitCSV(i)
102000030715     C                   ENDIF
102100040708     C*** LOO
102200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
102300040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
102400030715     C                   ENDIF
102500040708     C*** PRO
102600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
102700040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
102800030715     C                   ENDIF
102900040708     C*** NAO
103000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
103100040708     C                   EVAL      VAONAO = SkSplitCSV(i)
103200030721     C                   ENDIF
103300040708     C*** RSR
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
103500040708     C                   EVAL      VAORSR = SkSplitCSV(i)
103600040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103700040708     C     '@':'A'       XLATE     VAORSR        VAORSR
103800040708     C* ==
103900040708     C                   IF        VAORSR = *blanks
104000040708     C                   SETON                                        31
104100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104200040708     C                             + ' ' + 'VAORSR'
104300040708     C                   ENDIF
104400040708     C                   ENDIF
104500040708     C*** INR
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
104700040708     C                   EVAL      VAOINR = SkSplitCSV(i)
104800040708     C                   IF        VAOINR = *blanks
104900040708     C                   SETON                                        31
105000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105100040708     C                             + ' ' + 'VAOINR'
105200040708     C                   ENDIF
105300040708     C                   ENDIF
105400040708     C*** LOR
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
105600040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
105700040708     C                   IF        VAOLOR = *blanks
105800040708     C                   SETON                                        31
105900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106000040708     C                             + ' ' + 'VAOLOR'
106100040708     C                   ENDIF
106200040708     C                   ENDIF
106300040708     C*** PRR
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
106500040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
106600040708     C                   IF        VAOPRR = *blanks
106700040708     C                   SETON                                        31
106800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106900040708     C                             + ' ' + 'VAOPRR'
107000040708     C                   ENDIF
107100040708     C                   ENDIF
107200040708     C*** NAR
107300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
107400040708     C                   EVAL      VAONAR = SkSplitCSV(i)
107500040708     C                   ENDIF
107600040708     C*** RER
107700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
107800040708     C                   EVAL      VAORER = SkSplitCSV(i)
107900040708     C                   ENDIF
108000040708     C*** TER
108100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
108200040708     C                   EVAL      VAOTER = SkSplitCSV(i)
108300040708     C                   ENDIF
108400040708     C*** RSC
108500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
108600040708     C                   EVAL      VAORSC = SkSplitCSV(i)
108700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
108800040708     C     '@':'A'       XLATE     VAORSC        VAORSC
108900040708     C* ==
109000040708     C                   ENDIF
109100040708     C*** INC
109200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
109300040708     C                   EVAL      VAOINC = SkSplitCSV(i)
109400040708     C                   ENDIF
109500040708     C*** LOC
109600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
109700040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
109800040708     C                   ENDIF
109900040708     C*** PRC
110000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
110100040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
110200040708     C                   ENDIF
110300040708     C*** NAC
110400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
110500040708     C                   EVAL      VAONAC = SkSplitCSV(i)
110600040708     C                   ENDIF
110700040708     C*** FFD
110800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
110900040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
111000040708     C                   ENDIF
111100040708     C*** RMP
111200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
111300040708     C                   EVAL      VAORMP = SkSplitCSV(i)
111400040708     C                   ENDIF
111500040708     C*** NAM
111600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
111700040708     C                   EVAL      VAONAM = SkSplitCSV(i)
111800040708     C                   ENDIF
111900040708     C*** PAG
112000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
112100040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
112200040708     C                   ENDIF
112300040708     C*** CTR
112400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
112500040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
112600040708     C                   ENDIF
112700040708     C*** ZOR
112800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
112900040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
113000040708     C                   ENDIF
113100040708     C*** NO1
113200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
113300040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
113400030715     C                   ENDIF
113500040708     C*** NO2
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
113700040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
113800030715     C                   ENDIF
113900040708     C*** DDT
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
114100040708     C                   EVAL      VAODDT = SkSplitCSV(i)
114200030715     C                   ENDIF
114300040708     C*** RFA
114400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
114500040708     C                   EVAL      VAORFA = SkSplitCSV(i)
114600030715     C                   ENDIF
114700040708     C*** SPI
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
114900040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
115000030715     C                   ENDIF
115100040708     C*** FLO
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
115300040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
115400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
115500080708     C* e la posizione 7 del VAOFLO
115600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
115700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
115800050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
115900030715     C                   ENDIF
116000091103     C
116100091103     C*** DAR
116200050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
116300050412     C                   EVAL      PiStr=SkSplitCSV(i)
116400050412     C                   EXSR      CHKNUM
116500050412     C                   IF        PiInt=*on
116600050412     C                   Z-ADD     PiVal         VAODAR
116700050412     C                   ELSE
116800050412     C                   SETON                                        32
116900050412     C                   EVAL      VAODAR = *zeros
117000050412     C                   EVAL      vinmsg = %trimr(vinmsg)
117100050412     C                             + ' ' + 'VAODAR'
117200050412     C                   ENDIF
117300050314     C                   ENDIF
117400030715     C
117500030721     C*
117600030721     C* Reperisco quindi i campi numerici...
117700040708     C*** POE
117800040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
117900040723     C                   EVAL      PiStr=SkSplitCSV(i)
118000040723     C                   EXSR      CHKNUM
118100040723     C                   IF        PiInt=*on    AND
118200040723     C                             PiVal<=999   AND
118300040723     C                             PiVal>*zeros
118400040723     C                   Z-ADD     PiVal         VAOPOE
118500040723     C                   ELSE
118600040723     C                   SETON                                        32
118700040723     C                   EVAL      VAOPOE = *zeros
118800040723     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040723     C                             + ' ' + 'VAOPOE'
119000040723     C                   ENDIF
119100040723     C                   ENDIF
119200040708     C*** COR
119300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
119400040708     C                   EVAL      PiStr=SkSplitCSV(i)
119500040708     C                   EXSR      CHKNUM
119600040708     C                   IF        PiInt=*on
119700040708     C                   Z-ADD     PiVal         VAOCOR
119800040708     C                   ELSE
119900040708     C                   SETON                                        31
120000040708     C                   EVAL      VAOCOR = *zeros
120100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040708     C                             + ' ' + 'VAOCOR'
120300040708     C                   ENDIF
120400040708     C                   ENDIF
120500040708     C*** CAO
120600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on
121000030721     C                   Z-ADD     PiVal         Num5_0
121100040708     C                   MOVEL(p)  Num5_0        VAOCAO
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400040708     C                   EVAL      VAOCAO = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040708     C                             + ' ' + 'VAOCAO'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900040708     C*** CRA
122000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on
122400040708     C                   Z-ADD     PiVal         VAOCRA
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700040708     C                   EVAL      VAOCRA = *zeros
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900040708     C                             + ' ' + 'VAOCRA'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200040708     C*** CAR
123300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
123400040708     C                   EVAL      PiStr=SkSplitCSV(i)
123500040708     C                   EXSR      CHKNUM
123600040708     C                   IF        PiInt=*on
123700040708     C                   Z-ADD     PiVal         Num5_0
123800040708     C                   MOVEL(p)  Num5_0        VAOCAR
123900040708     C                   ELSE
124000040708     C                   SETON                                        31
124100040708     C                   EVAL      VAOCAR = *zeros
124200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124300040708     C                             + ' ' + 'VAOCAR'
124400040708     C                   ENDIF
124500040708     C                   ENDIF
124600040708     C*** CRC
124700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
124800040708     C                   EVAL      PiStr=SkSplitCSV(i)
124900040708     C                   EXSR      CHKNUM
125000040708     C                   IF        PiInt=*on
125100040708     C                   Z-ADD     PiVal         VAOCRC
125200040708     C                   ELSE
125300040708     C                   SETON                                        32
125400040708     C                   EVAL      VAOCRC = *zeros
125500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125600040708     C                             + ' ' + 'VAOCRC'
125700040708     C                   ENDIF
125800040708     C                   ENDIF
125900040708     C*** CAC
126000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
126100040708     C                   EVAL      PiStr=SkSplitCSV(i)
126200040708     C                   EXSR      CHKNUM
126300040708     C                   IF        PiInt=*on
126400040708     C                   Z-ADD     PiVal         Num5_0
126500040708     C                   MOVEL(p)  Num5_0        VAOCAC
126600040708     C                   ELSE
126700040708     C                   SETON                                        32
126800040708     C                   EVAL      VAOCAC = *zeros
126900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127000040708     C                             + ' ' + 'VAOCAC'
127100040708     C                   ENDIF
127200040708     C                   ENDIF
127300040708     C*** DAR
127400090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
127500090428     C                   EVAL      PiStr=SkSplitCSV(i)
127600090428     C                   EXSR      CHKNUM
127700090428     C                   IF        PiInt=*on
127800090428     C                   Z-ADD     PiVal         VAODAR
127900090428     C                   ELSE
128000090428     C                   SETON                                        32
128100090428     C                   EVAL      VAODAR = *zeros
128200090428     C                   EVAL      vinmsg = %trimr(vinmsg)
128300090428     C                             + ' ' + 'VAODAR'
128400090428     C                   ENDIF
128500090428     C                   ENDIF
128600040708     C*** ORR
128700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
128800040708     C                   EVAL      PiStr=SkSplitCSV(i)
128900040708     C                   EXSR      CHKNUM
129000040708     C                   IF        PiInt=*on
129100040708     C                   Z-ADD     PiVal         VAOORR
129200040708     C                   ELSE
129300040708     C                   SETON                                        32
129400040708     C                   EVAL      VAOORR = *zeros
129500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129600040708     C                             + ' ' + 'VAOORR'
129700040708     C                   ENDIF
129800040708     C                   ENDIF
129900030721     C*** NCL
130000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on
130400040723     C                   IF        PiVal > *zeros
130500040708     C                   Z-ADD     PiVal         VAONCL
130600040723     C                   ENDIF
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900040708     C                   EVAL      VAONCL = *zeros
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100040708     C                             + ' ' + 'VAONCL'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400040708     C*** PKG
131500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiNum=*on
131900040723     C                   IF        PiVal > *zeros
132000040708     C                   Z-ADD     PiVal         VAOPKG
132100040723     C                   ENDIF
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400040708     C                   EVAL      VAOPKG = *zeros
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600040708     C                             + ' ' + 'VAOPKG'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900040708     C*** VLM
133000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
133100040708     C                   EVAL      PiStr=SkSplitCSV(i)
133200040708     C                   EXSR      CHKNUM
133300040708     C                   IF        PiNum=*on
133400040708     C                   Z-ADD     PiVal         VAOVLM
133500040708     C                   ELSE
133600040708     C                   SETON                                        32
133700040708     C                   EVAL      VAOVLM = *zeros
133800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133900040708     C                             + ' ' + 'VAOVLM'
134000040708     C                   ENDIF
134100040708     C                   ENDIF
134200040708     C*** BNC
134300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
134400040708     C                   EVAL      PiStr=SkSplitCSV(i)
134500040708     C                   EXSR      CHKNUM
134600040708     C                   IF        PiInt=*on
134700040708     C                   Z-ADD     PiVal         VAOBNC
134800040708     C                   ELSE
134900040708     C                   SETON                                        32
135000040708     C                   EVAL      VAOBNC = *zeros
135100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135200040708     C                             + ' ' + 'VAOBNC'
135300040708     C                   ENDIF
135400040708     C                   ENDIF
135500040708     C*** BLC
135600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
135700040708     C                   EVAL      PiStr=SkSplitCSV(i)
135800040708     C                   EXSR      CHKNUM
135900040708     C                   IF        PiInt=*on
136000040708     C                   Z-ADD     PiVal         VAOBLC
136100040708     C                   ELSE
136200040708     C                   SETON                                        32
136300040708     C                   EVAL      VAOBLC = *zeros
136400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136500040708     C                             + ' ' + 'VAOBLC'
136600040708     C                   ENDIF
136700040708     C                   ENDIF
136800040708     C*** ATT
136900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
137000040708     C                   EVAL      PiStr=SkSplitCSV(i)
137100040708     C                   EXSR      CHKNUM
137200040708     C                   IF        PiInt=*on
137300040708     C                   Z-ADD     PiVal         VAOATT
137400040708     C                   ELSE
137500040708     C                   SETON                                        32
137600040708     C                   EVAL      VAOATT = *zeros
137700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137800040708     C                             + ' ' + 'VAOATT'
137900040708     C                   ENDIF
138000040708     C                   ENDIF
138100040708     C*** MTC
138200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
138300040708     C                   EVAL      PiStr=SkSplitCSV(i)
138400040708     C                   EXSR      CHKNUM
138500040708     C                   IF        PiInt=*on
138600040708     C                   Z-ADD     PiVal         VAOMTC
138700040708     C                   ELSE
138800040708     C                   SETON                                        32
138900040708     C                   EVAL      VAOMTC = *zeros
139000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139100040708     C                             + ' ' + 'VAOMTC'
139200040708     C                   ENDIF
139300040708     C                   ENDIF
139400040708     C*** KSC
139500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on      AND
139900030721     C                             PiVal<=9999999 AND
140000030721     C                             PiVal>*zeros
140100040708     C                   Z-ADD     PiVal         VAOKSC
140200030721     C                   ELSE
140300030721     C                   SETON                                        32
140400040708     C                   EVAL      VAOKSC = *zeros
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600040708     C                             + ' ' + 'VAOKSC'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900040708     C*** POR
141000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiInt=*on    AND
141400030721     C                             PiVal<=999   AND
141500030721     C                             PiVal>*zeros
141600040708     C                   Z-ADD     PiVal         VAOPOR
141700030721     C                   ELSE
141800030721     C                   SETON                                        32
141900040708     C                   EVAL      VAOPOR = *zeros
142000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142100040708     C                             + ' ' + 'VAOPOR'
142200030721     C                   ENDIF
142300030721     C                   ENDIF
142400040708     C*** POC
142500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
142600040708     C                   EVAL      PiStr=SkSplitCSV(i)
142700040708     C                   EXSR      CHKNUM
142800040708     C                   IF        PiInt=*on    AND
142900040708     C                             PiVal<=999   AND
143000040708     C                             PiVal>*zeros
143100040708     C                   Z-ADD     PiVal         VAOPOC
143200040708     C                   ELSE
143300040708     C                   SETON                                        32
143400040708     C                   EVAL      VAOPOC = *zeros
143500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143600040708     C                             + ' ' + 'VAOPOC'
143700040708     C                   ENDIF
143800040708     C                   ENDIF
143900040708     C*** STO
144000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
144100040708     C                   EVAL      PiStr=SkSplitCSV(i)
144200040708     C                   EXSR      CHKNUM
144300040708     C                   IF        PiInt=*on
144400040708     C                   Z-ADD     PiVal         VAOSTO
144500040708     C                   ELSE
144600040708     C                   SETON                                        32
144700040708     C                   EVAL      VAOSTO = *zeros
144800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144900040708     C                             + ' ' + 'VAOSTO'
145000040708     C                   ENDIF
145100040708     C                   ENDIF
145200090428     C*
145300091103     C********** GESTIONE CAMPI PERSONALIZZATI
145400090428     C*
145500090428     C*
145600090428     C****************************************
145700090428     C*
145800030715     C*
145900030715     C                   ENDIF
146000030715     C                   EVAL      i = i + 1
146100030715     C                   ENDDO
146200040723     C*
146300030715     C                   ENDIF
146400020204     C*
146500010607     C                   ENDSR
146600010607     C*----------------------------------------------------*
146700010330
146800080702
146900080702
147000080702     C*----------------------------------------------------*
147100080702     C*  CONTROLLO NUMERICITA' CAMPI
147200080702     C*----------------------------------------------------*
147300080702     C     CHKNUM        BEGSR
147400080702     C*
147500080702     C                   IF        PiDecChr = *blanks
147600080702     C                   EVAL      PiDecChr = ','
147700080702     C                   ENDIF
147800080702     C*
147900080702     C                   callp     UBISNUM_Check(PiStr
148000080702     C                                          :PiDecChr
148100080702     C                                          :PiVal
148200080702     C                                          :PiNum
148300080702     C                                          :PiInt)
148400080702     C*
148500080702     C                   ENDSR
148600080702     C***
148700010601
148800080702
148900010601
149000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149100010601     C     repfil        BEGSR
149200010601     C*
149300010601     C                   if        invfil = *zeros and
149400010601     C                             depfil > *zeros and
149500010629     C                             (vinflg = *blanks or
149600010629     C                              vinflg = *zeros)
149700010601     C
149800010601     C                   eval      invfil = depfil
149900010601     C                   endif
150000010601     C*
150100010601     C                   if        depfil <> invfil and
150200010601     C                             invfil > *zeros
150300010601     C                   eval      flgMulti = '1'
150400010601     C                   if        vinflg = *blanks
150500010601     C                   add       1             cntNonEl
150600010601     C                   endif
150700010601     C                   endif
150800010601     C*
150900010601     C                   if        vinflg = '2'
151000010601     C                   eval      flgStato = '2'
151100010601     C                   endif
151200010601     C*
151300010601     C                   ENDSR
151400010601     C***
151500010601
151600010601
151700010601
151800010330
151900010330
152000010330
152100990920      /TITLE Invio dei dati al punto operativo.
152200000613     C     invio         BEGSR
152300990920     C*
152400990920     C                   reset                   dscmz
152500010601     C                   move      invfil        cmzdst
152600990920     C                   eval      cmzfld = vlrfou
152700990920     C                   eval      cmzmbd = vlrhdl
152800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
152900000710     C                   if        prmfir = *blanks
153000040708     C                   eval      cmzfla = 'FNVAO00F'
153100040708     C                   eval      cmzmba = 'FNVAO00F'
153200000710     C                   else
153300000710     C                   eval      cmzfla = prmfir
153400000710     C                   eval      cmzmba = prmfir
153500000710     C                   endif
153600990920     C                   eval      cmznrr = *zeros
153700990920     C                   move      §ctrok        cmznrr
153800021018     C                   eval      cmzlba = vlrfl1
153900990920     C                   call(e)   'TIS711C'
154000990920     C                   parm                    dscmz
154100990921     C                   parm      *blanks       esito
154200990920     C                   if        %error
154300990920     C                             or cmzerr = '1'
154400990921     C                             or esito  = '1'
154500000710     C                   eval      wrkesito = '3'
154600990920     C                   endif
154700990920     C*
154800000613     C                   ENDSR
154900990910
155000010601
155100010601
155200010601
155300010601
155400010601      /TITLE Invio dei dati al punto operativo.
155500010601     C     opeini        BEGSR
155600010601     C*
155700010601     C* Inizializzo flag e contatori operativi
155800010601     C                   movel     '0'           flgGiro           1
155900010601     C                   movel     '0'           flgMulti          1
156000010601     C                   movel     '1'           flgStato          1
156100010615     C                   movel     '0'           flgOk             1
156200010601     C                   z-add     *zeros        cntNonEl         10 0
156300010601     C                   z-add     *zeros        depfil            3 0
156400010601     C                   z-add     *zeros        invfil            3 0
156500010601     C*
156600010601     C                   ENDSR
156700010601     C***
156800010601
156900010601
157000050421
157100050421
157200050421     C     *pssr         BEGSR
157300050421     C*
157400050421     C                   eval      wrkesito = '2'
157500050421     C*
157600050421     C                   ENDSR     '*CANCL'
157700050421     C***
157800060306
157900060306
158000060306     C*-------------------------------------------------------------------------*
158100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
158200060306     C*-------------------------------------------------------------------------*
158300060306     C     sr_Control    begsr
158400060306     C*
158500060306     C* inizializzo a ok il flag d controllo abilitazione
158600060306     C                   eval      Cli_Abil = *on
158700060306     C
158800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
158900060306     C                   clear                   woriIDC
159000060306     C                   eval      kteCOD = 'VAP'
159100060306     C                   eval      kteKE1 = oriIDC
159200060306     C     keytbe        chain     TNTBE01L
159300060306     C                   if        %found(Tntbe01l)
159400060306     C                   eval      woriIDC = TbeKe2
159500060306     C                   else
159600060306     C                   eval      Cli_Abil = *off
159700060306     C                   endif
159800060306     C*
159900060306     C                   endsr
160000050421
160100010601
160200010330
160300010330
160400000613     C     *inzsr        BEGSR
160500990910     C*
160600990910     C     *entry        plist
160700990920     C                   parm                    tivlrds
160800990921     C                   parm      wrkesito      esito
160900000724     C                   parm                    prmlit
161000000710     C                   parm                    prmfir
161100010330     C*
161200010330     C* CALCOLA LA DATA CORRENTE
161300010330     C                   time                    wn14             14 0
161400040723     C                   movel     wn14          wn6               6 0          *ORA (6)
161500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
161600010330     C                   z-add     wn8           g08dat
161700010330     C                   z-add     *zeros        g08inv
161800010330     C                   movel     '0'           g08err
161900010330     C                   call      'XSRDA8'
162000010330     C                   parm                    wlbda8
162100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
162200040723     C*
162300040723     C* Campi riferito al database
162400040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
162500040723     C     *like         define    tbeke1        kteKE1
162600040723     C*
162700040723     C* Definizione chiavi di lettura
162800040723     C* TNTBE01L - parziale
162900040723     C     keytbe        klist
163000040723     C                   kfld                    kteCOD                         *UTENTE
163100040723     C                   kfld                    kteKE1                         *CODICE
163200000613     C*
163300000613     C                   ENDSR
163400000613     C***
163500990908
163600091103     Otitv1s6p  E            riepilogo         2
163700990915     O                                              'Upload via Internet'
163800990915     O                                           +1 'Traduzione TIVIN00R -'
163900040708     O                                           +1 'FNVAOWWR'
164000990915     O                                           +1 'Testate bolle'
164100990915     O          E            riepilogo   2
164200990915     O                       wrkdata
164300990915     O                       wrkora              +1
164400990915     O                       procname            +1
164500990915     O          E            riepilogo   2
164600990915     O                                              'Cliente..................:'
164700040708     O                       VAOCOR        z     +1
164800990915     O          E            riepilogo   2
164900990920     O                                              'Riferimento Strategi.....:'
165000990920     O                       vlrhdl              +1
165100990915     O          E            riepilogo   2
165200990915     O                                              'Giusti...................:'
165300971022     O                       §CTROK        2   +  1
165400990915     O          E            riepilogo   2
165500990915     O                                              'Sbagliati e corretti.....:'
165600971022     O                       §CTRMO        2   +  1
165700990915     O          E            riepilogo   2
165800990915     O                                              'Sbagliati e scartati.....:'
165900971022     O                       §CTRNO        2   +  1
166000000613
166100091103     Otitv1s6ps E            testdett          2
166200000613     O                                              'Upload via Internet'
166300000613     O                                           +1 'Traduzione TIVIN00R -'
166400040708     O                                           +1 'FNVAOWWR'
166500000613     O                                           +1 'Testate bolle'
166600000616     O          E            testdett    3
166700000613     O                                           +2 'N° rec'
166800000613     O                                           +3 'Anteprima contenuto'
166900000616     O          E            rigadett    2
167000000613     O                       rrnum               +2
167100000621     O                       recko               +3
167200000616     O          E            findett     2
167300000613     O                       wrkdata
167400000613     O                       wrkora              +1
167500000613     O                       procname            +1
167600000616     O          E            findett     2
167700000613     O                                              'Cliente..................:'
167800040708     O                       VAOCOR        z     +1
167900000616     O          E            findett     2
168000000613     O                                              'Riferimento Strategi.....:'
168100000613     O                       vlrhdl              +1
168200000616     O          E            findett     2
168300000613     O                                              'Giusti...................:'
168400000613     O                       §CTROK        2   +  1
168500000616     O          E            findett     2
168600000613     O                                              'Sbagliati e corretti.....:'
168700000613     O                       §CTRMO        2   +  1
168800000616     O          E            findett     2
168900000613     O                                              'Sbagliati e scartati.....:'
169000000613     O                       §CTRNO        2   +  1
169100000613** CMD - COMANDI CL
169200091103OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1S6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
169300091103OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1S6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
169400091103DLTOVR FILE(TITV1S6P TITV1S6PS) LVL(*)
169500010606
169600000613
169700040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
169800091103VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAONCL;VAOPKG;VAOTER;
169900091103VAORER;VAORFA;VAODAR;VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC
170000091103
