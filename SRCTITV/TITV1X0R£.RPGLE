000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200130125     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500040723     Ffnacr01l  if   e           k disk
000600090618     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800040708     FFnvaowwr  O    E             DISK    usropn
000900040723     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100100416     Ftitv1x0p  O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300100416     Ftitv1x0ps O    f  198        PRINTER usropn
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
006800040723     D*------------------
006900040723     D* DS REPERIMENTO DATI CLIENTE
007000040723     D*-------------------
007100040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007200040723     D ACODS         E DS                  EXTNAME(CNACO00F)
007300040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007400040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007500040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007600080702     D*------------------
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
009600990915     C                   time                    wrkdata
009700990915     C                   time                    wrkora
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300041011     C                   exsr      PREELA
010400040708     C                   exsr      rwvao
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700041011     C                   if        flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100000724     C*
012200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012400041011     C     vlrpoi        chain     azorg01l
012500000724     C                   if        %found
012600000724     C                   movel(p)  CMD(1)        depcmd
012700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012800000724     C*
012900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000724     C                   PARM                    depcmd
013300000724     C                   PARM                    LENGH
013400000724     C*
013500041011     C                   else
013600041011     C                   setoff                                       50
013700000724     C                   endif
013800000616     C*
013900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014000000616     C                   movel(p)  CMD(2)        depcmd
014100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014200000616     C*
014300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    depcmd
014700000616     C                   PARM                    LENGH
014800041011     C*
014900041011     C                   endif
015000000613     C*
015100100416     C                   if        not %open(titv1x0ps)
015200100416     C                   open      titv1x0ps
015300130125     C***                except    testdett
015400000613     C                   endif
015500010601     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601     C*--------------------------------------------------------
016100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016200010601     C*--------------------------------------------------------
016300010601     C     ENDELA        BEGSR
016400990915     C*
016500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016600000613     C*
016700100416     C                   if        %open(titv1x0ps)
016800130125     C***                except    findett
016900100416     C                   close     titv1x0ps
017000000613     C                   endif
017100000616     C*
017200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017300011113     C                   Z-ADD     150           LENGH            15 5
017400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017500000616     C                   PARM                    CMD(3)
017600000616     C                   PARM                    LENGH
017700000616     C*
017800010601     C                   ENDSR
017900010601     C***
018000010601
018100000613
018200000613     C*--------------------------------------------------------
018300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018400000613     C*--------------------------------------------------------
018500000621     C     STPR          BEGSR
018600000613     C*
018700100416     C                   if        not %open(titv1x0p)
018800100416     C   50              open      titv1x0p
018900990915     C                   endif
019000990915     C*
019100130125     C***50              except    riepilogo
019200990915     C*
019300100416     C                   if        %open(titv1x0p)
019400100416     C   50              close     titv1x0p
019500990914     C                   endif
019600000613     C*
019700000613     C                   ENDSR
019800000613     C***
019900010601
020000010601
020100010330     C*--------------------------------------------------------
020200040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
020300010330     C*--------------------------------------------------------
020400040708     C     RWVAO         BEGSR
020500010330     C*
020600010330     C                   if        not %open(tivin00r)
020700010330     C                   open      tivin00r
020800010330     C                   endif
020900040708     C                   if        not %open(fnvaowwr)
021000040708     C                   open      fnvaowwr
021100010330     C                   endif
021200010330     C*
021300010604     C                   clear                   §CTROK
021400010604     C                   clear                   §CTRMO
021500010604     C                   clear                   §CTRNO
021600010330     C*
021700030822     C                   do        *HIVAL
021800010330     C*
021900030822     C                   read      tivin00r                               70
022000010618     C*
022100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022200010618     C                   if        vinflg = '1'
022300010618     C                   eval      flgOk = '1'
022400010618     C                   endif
022500010618     C*
022600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
022700040510     C                   z-add     *zeros        wLenVINDTA        4 0
022800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
022900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
023000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
023100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
023200040510     C                   z-add     1             wFlgVINDTA
023300040510     C                   leave
023400040510     C                   endif
023500040510     C                   enddo
023600040510     C*
023700040218     C                   if        vindta > *blanks AND
023800040510     C                             wFlgVINDTA = 1
023900010330     C                   add       1             rrnum
024000010330     C*
024100010601     C                   if        *in70 = *off and
024200010330     C                             (vinflg = *blanks
024300010330     C                              or vinflg = '0'
024400010330     C                              or vinflg = '2')
024500010330     C*
024600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024700010711     C                   if        vinflg = *blanks or vinflg = '0'
024800010711     C                   clear                   vinmsg
024900010711     C                   endif
025000010601     C*
025100010330     C                   exsr      inzvar
025200040708     C                   exsr      impvao
025300010601     C*
025400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025500040708     C                   eval      depfil = VAOPOE
025600010601     C                   exsr      repfil
025700010601     C                   if        depfil = invfil
025800021025     C                   if        vlrpoi = 999
025900040708     C                   move(P)   invfil        VAOPOE
026000021025     C                   else
026100040708     C                   move(P)   vlrpoi        VAOPOE
026200021025     C                   endif
026300010601     C*
026400010604     C* Ebbene...
026500010604     C*
026600030822     C  N31              add       1             §CTROK            7 0
026700030822     C   32              add       1             §CTRMO            7 0
026800030822     C   31              add       1             §CTRNO            7 0
026900030822     C                   if        wGiro = 2
027000040723     C  N31              EXSR      CHKCOR
027100040723     C  N31              EXSR      WRIORI
027200050826     C  N31              EXSR      WRIVGD
027300040723     C  N31              write     FNVAO000
027400030822     C                   endif
027500010604     C*
027600010604     C                   if        *in31 = *off and
027700010604     C                             *in32 = *off
027800010604     C                   eval      vinflg = '1'
027900010604     C                   else
028000010604     C                   eval      recko = vindta
028100130125     C***                except    rigadett
028200010604     C                   eval      vinflg = '2'
028300010604     C                   endif
028400010604     C                   endif
028500010601     C*
028600010604     C                   endif
028700010604     C*
028800010330     C                   else
028900010330     C                   eval      vinflg = '1'
029000010330     C                   endif
029100010601     C*
029200010601     C  N70              update    tivin000
029300010330     C*
029400030822     C  N70              enddo
029500010601     C
029600010601     C*
029700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029800010601     C                   if        cntNonEl = *zeros or
029900010601     C                             flgMulti = '0'
030000010330     C* Se non ci sono record con errori ...
030100010601     C                   if        §ctrno = 0 and
030200010604     C                             §ctrmo = 0 and
030300010601     C                             flgStato <> '2'
030400010330     C* ... restituisco esito OK.
030500010330     C                   eval      wrkesito = '0'
030600010330     C                   else
030700010330     C                   if        §ctrok > 0
030800010330     C                   eval      wrkesito = '1'
030900010330     C                   else
031000010615     C                   if        flgOk = '0'
031100010615     C                   eval      wrkesito = '2'
031200010615     C                   else
031300010615     C                   eval      wrkesito = '6'
031400010615     C                   endif
031500010330     C                   endif
031600010330     C                   endif
031700010601     C                   else
031800010601     C                   eval      wrkesito = '9'
031900010601     C                   endif
032000010330     C*
032100010330     C                   if        %open(tivin00r)
032200010330     C                   close     tivin00r
032300010330     C                   endif
032400040708     C                   if        %open(fnvaowwr)
032500040708     C                   close     fnvaowwr
032600010330     C                   endif
032700010601     C*
032800010601     C                   if        vlrpoi <> 999
032900010601     C                   eval      invfil = vlrpoi
033000010601     C                   endif
033100010330     C*
033200010330     C                   if        §ctrok > 0
033300010601     C                             and invfil > *zeros
033400010330     C                   exsr      invio
033500010330     C                   endif
033600010601     C*
033700010618     C                   if        flgGiro = '1'
033800010601     C                   exsr      endela
033900010618     C                   endif
034000010330     C*
034100010330     C                   ENDSR
034200010330     C***
034300040723
034400040723
034500040723     C*------------------------------------------------------------------------*
034600040723     C* CHKCOR - Considerazioni su VAOCOR
034700040723     C*------------------------------------------------------------------------*
034800040723     C     CHKCOR        BEGSR
034900040723     C*
035000040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
035100040723     C                   if        vaoTCO = 'F'
035200040723     C* Quindi verifico se il codice ordinante non è valorizzato
035300040723     C                   if        vaoCOR = *zeros  AND
035400040723     C                             vaoRSO = *blanks AND
035500040723     C                             vaoINO = *blanks AND
035600040723     C                             vaoCAO = *blanks AND
035700040723     C                             vaoLOO = *blanks AND
035800040723     C                             vaoPRO = *blanks AND
035900040723     C                             vaoNAO = *blanks
036000040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
036100040723     C                   movel     '0'           wFlgAcr           1
036200040723     C                   move      vlrKSC        wCli              7 0
036300040723     C                   move(p)   wCli          acrCRO
036400040723     C     acrCRO        setll     fnacr01l
036500040723     C                   if        %found(fnacr01l)
036600040723     C                   read      fnacr01l
036700040723     C                   dow       not %eof(fnacr01l)
036800040723     C                   movel     acrCRO        wCliAcr           7 0
036900040723     C                   if        wCliAcr = wCli
037000040723     C                   eval      vaoCOR = acrCRO
037100040723     C                   movel     '1'           wFlgAcr
037200040723     C                   leave
037300040723     C                   else
037400040723     C                   leave
037500040723     C                   endif
037600040723     C                   read      fnacr01l
037700040723     C                   enddo
037800040723     C                   endif
037900040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
038000040723     C* dati anagrafici dell'ordinante dal piano dei conti
038100040723     C                   if        wFlgAcr = '0'
038200040723     C                   clear                   BS69DS
038300040723     C                   clear                   ACODS
038400040723     C                   clear                   INDDS
038500040723     C                   clear                   CLPDS
038600040723     C                   clear                   CLSDS
038700040723     C                   move(P)   vlrKSC        I69KAC
038800040723     C                   move(P)   vlrKSC        I69KIN
038900040723     C                   call      'TIBS69R'
039000040723     C                   parm                    BS69DS
039100040723     C                   parm                    ACODS
039200040723     C                   parm                    INDDS
039300040723     C                   parm                    CLPDS
039400040723     C                   parm                    CLSDS
039500040723     C     O69ERR        ifne      '1'
039600040723     C                   eval      vaoRSO = ACORAG
039700040723     C                   eval      vaoINO = INDVIA
039800040723     C                   movel(P)  INDCAP        vaoCAO
039900040723     C                   eval      vaoLOO = INDCIT
040000040723     C                   eval      vaoPRO = INDPRV
040100040723     C                   eval      vaoNAO = INDSTA
040200040723     C                   endif
040300040723     C                   endif
040400040723     C                   endif
040500040723     C                   endif
040600040723     C*
040700040723     C                   ENDSR
040800040723
040900040723
041000040723     C*------------------------------------------------------------------------*
041100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
041200040723     C*------------------------------------------------------------------------*
041300040723     C     WRIORI        BEGSR
041400040723     C*
041500090618     C* Come prima cosa stacco un numeratore da AZNUM
041600090618     C                   clear                   trul33ds
041700090618     C                   eval      I33OPE = *zeros
041800090618     C                   eval      I33CNU = 600
041900090618     C                   eval      I33NUM = 1
042000090618     C                   movel     TRUL33DS      KPJBU
042100090618     C                   call      'TRUL33R'
042200090618     C                   parm                    KPJBA
042300090618     C                   movel     KPJBU         TRUL33DS
042400090618     C                   if        O33ERR = *zeros
042500090618     C                   z-add     O33NRF        §numori
042600090618     c                   else
042700090618     c                   eval      §numori = *all'9'
042800090618     C                   endif
042900040723     C*
043000040723     C* Quindi imposto i campi "extra-VAO"
043100040723     C                   if        vlrKSC = '0ORM0000'
043200040723     C*
043300040723     C                   if        vaoCOR > *zeros
043400040723     C                   movel     vaoCOR        wrkCOR7           7 0
043500040723     C*
043600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
043700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
043800040723     C                   clear                   tibs10ds
043900040723     C                   eval      d10drf = datcor                              *data riferimento
044000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
044100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
044200091026     C*
044300091026     C* Preparazione chiamata al TIBS10R1
044400091026     C                   clear                   tibs10r1ds
044500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
044600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
044700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
044800091026     C*
044900091026     C                   call      'TIBS10R1'
045000091026     C                   parm                    tibs10r1ds
045100091026     C*
045200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
045300040723     C*
045400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
045500040723     C                             d10cop =  *zeros  or
045600040723     C                             d10cop =  d10cod
045700040723     C                   move      d10cod        oriIDC
045800040723     C                   else                                                   *Figlio
045900040723     C                   move      d10cop        oriIDC
046000040723     C                   endif
046100040723     C*
046200040723     C                   else
046300040723     C                   eval      oriIDC = *zeros
046400040723     C                   endif
046500040723     C*
046600040723     C                   else
046700040723     C                   eval      oriIDC = vlrKSC
046800040723     C                   endif
046900040723     C*
047000040723     C                   eval      oriPRG = §numori
047100040723     C                   eval      oriDER = datcor
047200040723     C                   eval      oriOER = wn6
047300040723     C                   eval      oriPOE = vaoPOE
047400040723     C                   eval      oriNSR = vaoNSR
047500040723     C                   eval      oriNOR = vaoNOR
047600040723     C                   eval      oriNRV = vaoNRV
047700040723     C                   eval      oriFLO = *blanks
047800040723     C*
047900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
048000040723     C                   eval      dorm01 = vaoFLO
048100040723     C                   movel     §numori       §ormpg
048200050412     C*
048300050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
048400050412     C                   movel     'S'           §ormfr
048500100416     C                   movel     'S'           §ormfd
048600100416     C                   movel     'N'           §orcomc
048700050412     C*
048800050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
048900040723     C                   eval      vaoFLO = dorm01
049000040723     C*
049100040723     C* ...quindi scrivo il file TIORI00F
049200040723     C                   WRITE     TIORI000
049300040723     C*
049400040723     C                   ENDSR
049500050826
049600050826
049700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
049800050826     C     wriVGD        BEGSR
049900050826     C*
050000050826     C* Reperisco la descrizione della fase da tabella
050100050826     C                   movel(p)  'FAR'         KteCOD
050200050826     C                   movel(p)  '000'         KteKE1
050300050826     C     KEYtbe        chain     tntbe01l
050400050826     C                   if        %found(tntbe01l)
050500050826     C                   movel(P)  tbeuni        DFAR
050600050826     C                   else
050700050826     C                   clear                   DFAR
050800050826     C                   endif
050900050826     C*
051000050826     C* Valorizzo buffer record
051100050826     C                   clear                   FNVAPDS
051200050826     C                   eval      vapIDC = oriIDC
051300050826     C                   eval      vapPOE = oriPOE
051400050826     C                   eval      vapRFA = vaoRFA
051500050826     C                   eval      vapPOG = oriPOE
051600050826     C                   eval      vapDAE = oriDER
051700050826     C                   eval      vapORE = oriOER
051800050826     C                   if        d§fardva <> *blanks
051900050826     C                   eval      vapDFA = d§fardva
052000050826     C                   else
052100050826     C                   eval      vapDFA = d§fardes
052200050826     C                   endif
052300050826     C*
052400050826     C                   clear                   tivgd000
052500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
052600050826     C                   eval      vgdTIP = 'VP'
052700050826     C                   eval      vgdKSU = vapIDC
052800050826     C                   eval      vgdTSC = 'WW'
052900050826     C                   eval      vgdDAT = datcor
053000100416     C                   eval      vgdPGM = 'TITV1X0R'
053100060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
053200060306     C                   exsr      sr_Control
053300060306     C                   if        Cli_Abil = *on
053400060306     C                   eval      vgdKSU = woriIDC
053500050826     C                   write     tivgd000
053600060306     C                   endif
053700050826     C*
053800050826     C                   ENDSR
053900050826     C*------------------------------------------------------------------------*
054000010601
054100010601
054200010330     C*----------------------------------------------------*
054300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054400010330     C*----------------------------------------------------*
054500010330     C     INZVAR        BEGSR
054600010330     C*
054700030822     C* Inizializzo variabili di wrk
054800020204     C                   Z-ADD     *zeros        Num5_0            5 0
054900030822     C*
055000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055100040708     C                   CLEAR                   FNVAO000
055200040723     C                   CLEAR                   TIORI000
055300030822     C                   CLEAR                   SkSplitCSV
055400030822     C                   EVAL      i = 1
055500030822     C                   EVAL      posDa = *zeros
055600030822     C                   EVAL      posA  = *zeros
055700030822     C*
055800030822     C* Reimposto i valori di default
055900030822     C                   EXSR      DEFCAM
056000030822     C*
056100010330     C                   ENDSR
056200010330     C*----------------------------------------------------*
056300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056400010330     C*----------------------------------------------------*
056500010330     C     DEFCAM        BEGSR
056600030822     C*
056700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056800030715     C* e delimitatore testo.
056900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057200030822     C*
057300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
057500030715     C                   SETON                                        50
057600030715     C                   ELSE
057700030715     C                   SETOFF                                       50
057800030715     C                   ENDIF
057900040723     C*
058000080707     C* Imposto i valori d default d alcuni campi
058100080708     C*
058200080708     C                   CLEAR                   DORM01
058300091103     C                   EVAL      VAOFLO = DORM01
058400080707     C*
058500091103     C                   EVAL      VAOTCO = 'F'
058600091103     C                   EVAL      VAOTOR = 'S'
058700091127     C*
058800091204     C* IMPOSTO I CAMPI PERSONALIZZATI
058900100416     C
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400040708     C* POE
061500040708     C                   IF        %subst(
061600040708     C                             %subst(vlrppt:posDaDft+1:
061700040708     C                             posADft-posDaDft-1):1:3)
061800040708     C                             = 'POE'
061900040708     C                   EVAL      PiStr=%trim(%subst(
062000040708     C                             %subst(vlrppt:posDaDft+1:
062100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200040708     C                   EXSR      CHKNUM
062300040708     C                   IF        PiInt=*on
062400040708     C                   Z-ADD     PiVal         VAOPOE
062500040708     C                   ENDIF
062600040708     C                   ENDIF
062700040708     C* TOR
062800040708     C                   IF        %subst(
062900040708     C                             %subst(vlrppt:posDaDft+1:
063000040708     C                             posADft-posDaDft-1):1:3)
063100040708     C                             = 'TOR'
063200040708     C                   EVAL      VAOTOR=%trim(%subst(
063300040708     C                             %subst(vlrppt:posDaDft+1:
063400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500040708     C                   ENDIF
063600040708     C* TCO
063700040708     C                   IF        %subst(
063800040708     C                             %subst(vlrppt:posDaDft+1:
063900040708     C                             posADft-posDaDft-1):1:3)
064000040708     C                             = 'TCO'
064100040708     C                   EVAL      VAOTCO=%trim(%subst(
064200040708     C                             %subst(vlrppt:posDaDft+1:
064300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400040708     C                   ENDIF
064500040707     C* COR
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900040707     C                             = 'COR'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiInt=*on
065500040708     C                   Z-ADD     PiVal         VAOCOR
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800040708     C* CRC
065900040708     C                   IF        %subst(
066000040708     C                             %subst(vlrppt:posDaDft+1:
066100040708     C                             posADft-posDaDft-1):1:3)
066200040708     C                             = 'CRC'
066300040708     C                   EVAL      PiStr=%trim(%subst(
066400040708     C                             %subst(vlrppt:posDaDft+1:
066500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600040708     C                   EXSR      CHKNUM
066700040708     C                   IF        PiInt=*on
066800040708     C                   Z-ADD     PiVal         VAOCRC
066900040708     C                   ENDIF
067000040708     C                   ENDIF
067100040708     C* FFD
067200040708     C                   IF        %subst(
067300040708     C                             %subst(vlrppt:posDaDft+1:
067400040708     C                             posADft-posDaDft-1):1:3)
067500040708     C                             = 'FFD'
067600040708     C                   EVAL      VAOFFD=%trim(%subst(
067700040708     C                             %subst(vlrppt:posDaDft+1:
067800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900040708     C                   ENDIF
068000040708     C* ORR
068100040708     C                   IF        %subst(
068200040708     C                             %subst(vlrppt:posDaDft+1:
068300040708     C                             posADft-posDaDft-1):1:3)
068400040708     C                             = 'ORR'
068500040708     C                   EVAL      PiStr=%trim(%subst(
068600040708     C                             %subst(vlrppt:posDaDft+1:
068700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800040708     C                   EXSR      CHKNUM
068900040708     C                   IF        PiInt=*on
069000040708     C                   Z-ADD     PiVal         VAOORR
069100040708     C                   ENDIF
069200040708     C                   ENDIF
069300040712     C* NCL
069400040712     C                   IF        %subst(
069500040712     C                             %subst(vlrppt:posDaDft+1:
069600040712     C                             posADft-posDaDft-1):1:3)
069700040712     C                             = 'NCL'
069800040712     C                   EVAL      PiStr=%trim(%subst(
069900040712     C                             %subst(vlrppt:posDaDft+1:
070000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100040712     C                   EXSR      CHKNUM
070200040712     C                   IF        PiInt=*on
070300040712     C                   Z-ADD     PiVal         VAONCL
070400040712     C                   ENDIF
070500040712     C                   ENDIF
070600040712     C* PKG
070700040712     C                   IF        %subst(
070800040712     C                             %subst(vlrppt:posDaDft+1:
070900040712     C                             posADft-posDaDft-1):1:3)
071000040712     C                             = 'PKG'
071100040712     C                   EVAL      PiStr=%trim(%subst(
071200040712     C                             %subst(vlrppt:posDaDft+1:
071300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400040712     C                   EXSR      CHKNUM
071500040712     C                   IF        PiNum=*on
071600040712     C                   Z-ADD     PiVal         VAOPKG
071700040712     C                   ENDIF
071800040712     C                   ENDIF
071900040712     C* VLM
072000040712     C                   IF        %subst(
072100040712     C                             %subst(vlrppt:posDaDft+1:
072200040712     C                             posADft-posDaDft-1):1:3)
072300040712     C                             = 'VLM'
072400040712     C                   EVAL      PiStr=%trim(%subst(
072500040712     C                             %subst(vlrppt:posDaDft+1:
072600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700040712     C                   EXSR      CHKNUM
072800040712     C                   IF        PiNum=*on
072900040712     C                   Z-ADD     PiVal         VAOVLM
073000040712     C                   ENDIF
073100040712     C                   ENDIF
073200040708     C* NAM
073300040708     C                   IF        %subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):1:3)
073600040708     C                             = 'NAM'
073700040708     C                   EVAL      VAONAM=%trim(%subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000040708     C                   ENDIF
074100040708     C* PAG
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500040708     C                             = 'PAG'
074600040708     C                   EVAL      VAOPAG=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000040708     C* KSC
075100040708     C                   IF        %subst(
075200040708     C                             %subst(vlrppt:posDaDft+1:
075300040708     C                             posADft-posDaDft-1):1:3)
075400040708     C                             = 'KSC'
075500040708     C                   EVAL      PiStr=%trim(%subst(
075600040708     C                             %subst(vlrppt:posDaDft+1:
075700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800040708     C                   EXSR      CHKNUM
075900040708     C                   IF        PiInt=*on
076000040708     C                   Z-ADD     PiVal         VAOKSC
076100040708     C                   ENDIF
076200040708     C                   ENDIF
076300040708     C* CTR
076400040708     C                   IF        %subst(
076500040708     C                             %subst(vlrppt:posDaDft+1:
076600040708     C                             posADft-posDaDft-1):1:3)
076700040708     C                             = 'CTR'
076800040708     C                   EVAL      VAOCTR=%trim(%subst(
076900040708     C                             %subst(vlrppt:posDaDft+1:
077000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100040708     C                   ENDIF
077200040708     C* POC
077300040708     C                   IF        %subst(
077400040708     C                             %subst(vlrppt:posDaDft+1:
077500040708     C                             posADft-posDaDft-1):1:3)
077600040708     C                             = 'POC'
077700040708     C                   EVAL      PiStr=%trim(%subst(
077800040708     C                             %subst(vlrppt:posDaDft+1:
077900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000040708     C                   EXSR      CHKNUM
078100040708     C                   IF        PiInt=*on
078200040708     C                   Z-ADD     PiVal         VAOPOC
078300040708     C                   ENDIF
078400040708     C                   ENDIF
078500040708     C* NO1
078600040708     C                   IF        %subst(
078700040708     C                             %subst(vlrppt:posDaDft+1:
078800040708     C                             posADft-posDaDft-1):1:3)
078900040708     C                             = 'NO1'
079000040708     C                   EVAL      VAONO1=%trim(%subst(
079100040708     C                             %subst(vlrppt:posDaDft+1:
079200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300040708     C                   ENDIF
079400040708     C* DDT
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800040708     C                             = 'DDT'
079900040708     C                   EVAL      VAODDT=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300040708     C* RFA
080400040708     C                   IF        %subst(
080500040708     C                             %subst(vlrppt:posDaDft+1:
080600040708     C                             posADft-posDaDft-1):1:3)
080700040708     C                             = 'RFA'
080800040708     C                   EVAL      VAORFA=%trim(%subst(
080900040708     C                             %subst(vlrppt:posDaDft+1:
081000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100040708     C                   ENDIF
081200040708     C* FLO
081300040708     C                   IF        %subst(
081400040708     C                             %subst(vlrppt:posDaDft+1:
081500040708     C                             posADft-posDaDft-1):1:3)
081600040708     C                             = 'FLO'
081700040708     C                   EVAL      VAOFLO=%trim(%subst(
081800040708     C                             %subst(vlrppt:posDaDft+1:
081900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000091103     C*
082100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
082200080702     C* e posizione 7 del VAOFLO
082300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
082400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
082500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
082600040708     C                   ENDIF
082700030822     C* ...
082800030822     C                   ENDIF
082900030822     C                   ENDDO
083000020204     C*
083100010330     C                   ENDSR
083200010607     C*----------------------------------------------------*
083300040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
083400010607     C*----------------------------------------------------*
083500040708     C     IMPVAO        BEGSR
083600010607     C*
083700010607     C                   SETOFF                                       3132
083800030515     C*
083900040708     C                   MOVEL     datcor        VAODAO
084000040708     C                   MOVEL     wn14          VAOOAO
084100050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084200050314     C     minu:maiu     XLATE     vindta        vindta
084300010607     C*
084400030715     C**********
084500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084600030715     C**********
084700030715     C                   IF        wGiro = *zeros
084800040709     C*
084900040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
085000040709     C                   EVAL      depvinDTA = vinDTA
085100040709     C*
085200040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
085300040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085400040709     C                                      %trim(intcol(3))
085500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085600050314     C**!!!minu:maiu     XLATE     vindta        vindta
085700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085800040324     C                   DOW       posDa <= %len(%trimr(vindta))
085900030715     C*
086000030715     C* Gestisco il 1° campo
086100030715     C                   IF        i = 1
086200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086300030715     C                   EVAL      posDa = 1
086400030715     C* Eseguo lo scan x trovare la fine del primo campo
086500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086600030715     C* A questo punto "estraggo" il campo corrente
086700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086800030715     C                                                    (posA-posDa))
086900030715     C* X i campi successivi al 1°
087000030715     C                   ELSE
087100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087200030715     C                   EVAL      posDa = posA + 1
087300030715     C* Eseguo lo scan x trovare la fine del campo corrente
087400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087500030715     C* Gestisco l'ultimo campo
087600030715     C                   IF        posA = *zeros
087700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C                   LEAVE
088200030715     C                   ELSE
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C                   ENDIF
088700030715     C                   ENDIF
088800030715     C* Incremento il contatore d campo
088900030715     C                   EVAL      i = i +1
089000030715     C                   ENDDO
089100030715     C*
089200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089300030715     C* delimitazione testo
089400030715     C                   EVAL      i = 1
089500030715     C                   DOW       i < %elem(SkSplitFLD)
089600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089800030715     C                   EVAL      i = i + 1
089900030715     C                   ENDDO
090000030715     C                   EVAL      wGiro = 1
090100040709     C* Ripristino la 1° riga originale
090200040709     C                   EVAL      vinDTA = depvinDTA
090300030715     C                   ELSE
090400030715     C                   EVAL      wGiro = 2
090500031204     C**********
090600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090700031204     C* d separatore campo
090800031204     C**********
090900040709     C                   EVAL      vindta = %trim(vindta)
091000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091100040709     C                   EVAL      vindta = %subst(vindta:2)
091200040709     C                   ENDDO
091300040709     C*
091400040709     C                   Z-ADD     *zeros        lunghInput        4 0
091500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
091600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
091900040709     C                   ENDDO
092000030715     C**********
092100030715     C* Effettuo lo split del campo dati d input
092200030715     C**********
092300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092400040324     C                   DOW       posDa <= %len(%trimr(vindta))
092500030715     C*
092600030715     C* Gestisco il 1° campo
092700030715     C                   IF        i = 1
092800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092900030715     C                   EVAL      posDa = 1
093000030715     C* Eseguo lo scan x trovare la fine del primo campo
093100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C* X i campi successivi al 1°
093600030715     C                   ELSE
093700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093800030715     C                   EVAL      posDa = posA + 1
093900031204     C* Verifico che nn vi sia il campo nn valorizzato
094000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094100031204     C* Se campo nn valorizzato skippo al prossimo
094200031204     C                   EVAL      SkSplitCSV(i) = *blanks
094300031204     C                   EVAL      posA  = posA + 1
094400031204     C                   ELSE
094500030715     C* Eseguo lo scan x trovare la fine del campo corrente
094600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094700030715     C* Gestisco l'ultimo campo
094800030715     C                   IF        posA = *zeros
094900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095000030715     C* A questo punto "estraggo" il campo corrente
095100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095200030715     C                                                    (posA-posDa))
095300030715     C                   LEAVE
095400030715     C                   ELSE
095500030715     C* A questo punto "estraggo" il campo corrente
095600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095700030715     C                                                    (posA-posDa))
095800030715     C                   ENDIF
095900031204     C                   ENDIF
096000030715     C                   ENDIF
096100030715     C* Incremento il contatore d campo
096200030715     C                   EVAL      i = i +1
096300030715     C                   ENDDO
096400030715     C*
096500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096600030715     C* delimitazione testo
096700030715     C                   EVAL      i = 1
096800030715     C                   DOW       i < %elem(SkSplitCSV)
096900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097100030715     C                   EVAL      i = i + 1
097200030715     C                   ENDDO
097300030715     C*
097400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitFLD)
097700030715     C*
097800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098000030715     C                             SkSplitCSV(i) <> *blanks
098100030721     C*** ATB
098200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
098300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
098400030721     C                   ENDIF
098500040708     C*** TOR
098600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
098700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
098800040708     C                   ENDIF
098900040708     C*** TCO
099000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
099100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
099200040708     C                   ENDIF
099300040708     C*** RSO
099400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
099500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
099600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
099800030715     C* ==
099900030715     C                   ENDIF
100000040708     C*** INO
100100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
100200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
100300030715     C                   ENDIF
100400040708     C*** LOO
100500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
100600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
100700030715     C                   ENDIF
100800040708     C*** PRO
100900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
101000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
101100030715     C                   ENDIF
101200040708     C*** NAO
101300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
101400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
101500030721     C                   ENDIF
101600040708     C*** RSR
101700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
101800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
101900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
102100040708     C* ==
102200040708     C                   IF        VAORSR = *blanks
102300040708     C                   SETON                                        31
102400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
102500040708     C                             + ' ' + 'VAORSR'
102600040708     C                   ENDIF
102700040708     C                   ENDIF
102800040708     C*** INR
102900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
103000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
103100040708     C                   IF        VAOINR = *blanks
103200040708     C                   SETON                                        31
103300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103400040708     C                             + ' ' + 'VAOINR'
103500040708     C                   ENDIF
103600040708     C                   ENDIF
103700040708     C*** LOR
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
103900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
104000040708     C                   IF        VAOLOR = *blanks
104100040708     C                   SETON                                        31
104200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104300040708     C                             + ' ' + 'VAOLOR'
104400040708     C                   ENDIF
104500040708     C                   ENDIF
104600040708     C*** PRR
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
104800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
104900040708     C                   IF        VAOPRR = *blanks
105000040708     C                   SETON                                        31
105100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105200040708     C                             + ' ' + 'VAOPRR'
105300040708     C                   ENDIF
105400040708     C                   ENDIF
105500040708     C*** NAR
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
105700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
105800040708     C                   ENDIF
105900040708     C*** RER
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
106100040708     C                   EVAL      VAORER = SkSplitCSV(i)
106200040708     C                   ENDIF
106300040708     C*** TER
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
106500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
106600040708     C                   ENDIF
106700040708     C*** RSC
106800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
106900040708     C                   EVAL      VAORSC = SkSplitCSV(i)
107000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107100040708     C     '@':'A'       XLATE     VAORSC        VAORSC
107200040708     C* ==
107300040708     C                   ENDIF
107400040708     C*** INC
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
107600040708     C                   EVAL      VAOINC = SkSplitCSV(i)
107700040708     C                   ENDIF
107800040708     C*** LOC
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
108000040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
108100040708     C                   ENDIF
108200040708     C*** PRC
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
108400040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
108500040708     C                   ENDIF
108600040708     C*** NAC
108700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
108800040708     C                   EVAL      VAONAC = SkSplitCSV(i)
108900040708     C                   ENDIF
109000040708     C*** FFD
109100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
109200040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
109300040708     C                   ENDIF
109400040708     C*** RMP
109500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
109600040708     C                   EVAL      VAORMP = SkSplitCSV(i)
109700040708     C                   ENDIF
109800040708     C*** NAM
109900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
110000040708     C                   EVAL      VAONAM = SkSplitCSV(i)
110100040708     C                   ENDIF
110200040708     C*** PAG
110300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
110400040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
110500040708     C                   ENDIF
110600040708     C*** CTR
110700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
110800040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
110900040708     C                   ENDIF
111000040708     C*** ZOR
111100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
111200040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
111300040708     C                   ENDIF
111400040708     C*** NO1
111500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
111600040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
111700030715     C                   ENDIF
111800040708     C*** NO2
111900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
112000040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
112100030715     C                   ENDIF
112200040708     C*** DDT
112300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
112400040708     C                   EVAL      VAODDT = SkSplitCSV(i)
112500030715     C                   ENDIF
112600040708     C*** RFA
112700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
112800040708     C                   EVAL      VAORFA = SkSplitCSV(i)
112900030715     C                   ENDIF
113000040708     C*** SPI
113100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
113200040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
113300030715     C                   ENDIF
113400040708     C*** FLO
113500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
113600040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
113700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
113800080708     C* e la posizione 7 del VAOFLO
113900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
114000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
114100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
114200030715     C                   ENDIF
114300091103     C
114400091103     C*** DAR
114500050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
114600050412     C                   EVAL      PiStr=SkSplitCSV(i)
114700050412     C                   EXSR      CHKNUM
114800050412     C                   IF        PiInt=*on
114900050412     C                   Z-ADD     PiVal         VAODAR
115000050412     C                   ELSE
115100050412     C                   SETON                                        32
115200050412     C                   EVAL      VAODAR = *zeros
115300050412     C                   EVAL      vinmsg = %trimr(vinmsg)
115400050412     C                             + ' ' + 'VAODAR'
115500050412     C                   ENDIF
115600050314     C                   ENDIF
115700030715     C
115800030721     C*
115900030721     C* Reperisco quindi i campi numerici...
116000040708     C*** POE
116100040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
116200040723     C                   EVAL      PiStr=SkSplitCSV(i)
116300040723     C                   EXSR      CHKNUM
116400040723     C                   IF        PiInt=*on    AND
116500040723     C                             PiVal<=999   AND
116600040723     C                             PiVal>*zeros
116700040723     C                   Z-ADD     PiVal         VAOPOE
116800040723     C                   ELSE
116900040723     C                   SETON                                        32
117000040723     C                   EVAL      VAOPOE = *zeros
117100040723     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040723     C                             + ' ' + 'VAOPOE'
117300040723     C                   ENDIF
117400040723     C                   ENDIF
117500040708     C*** COR
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
117700040708     C                   EVAL      PiStr=SkSplitCSV(i)
117800040708     C                   EXSR      CHKNUM
117900040708     C                   IF        PiInt=*on
118000040708     C                   Z-ADD     PiVal         VAOCOR
118100040708     C                   ELSE
118200040708     C                   SETON                                        31
118300040708     C                   EVAL      VAOCOR = *zeros
118400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040708     C                             + ' ' + 'VAOCOR'
118600040708     C                   ENDIF
118700040708     C                   ENDIF
118800040708     C*** CAO
118900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiInt=*on
119300030721     C                   Z-ADD     PiVal         Num5_0
119400040708     C                   MOVEL(p)  Num5_0        VAOCAO
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700040708     C                   EVAL      VAOCAO = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040708     C                             + ' ' + 'VAOCAO'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200040708     C*** CRA
120300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on
120700040708     C                   Z-ADD     PiVal         VAOCRA
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000040708     C                   EVAL      VAOCRA = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200040708     C                             + ' ' + 'VAOCRA'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500040708     C*** CAR
121600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
121700040708     C                   EVAL      PiStr=SkSplitCSV(i)
121800040708     C                   EXSR      CHKNUM
121900040708     C                   IF        PiInt=*on
122000040708     C                   Z-ADD     PiVal         Num5_0
122100040708     C                   MOVEL(p)  Num5_0        VAOCAR
122200040708     C                   ELSE
122300040708     C                   SETON                                        31
122400040708     C                   EVAL      VAOCAR = *zeros
122500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040708     C                             + ' ' + 'VAOCAR'
122700040708     C                   ENDIF
122800040708     C                   ENDIF
122900040708     C*** CRC
123000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
123100040708     C                   EVAL      PiStr=SkSplitCSV(i)
123200040708     C                   EXSR      CHKNUM
123300040708     C                   IF        PiInt=*on
123400040708     C                   Z-ADD     PiVal         VAOCRC
123500040708     C                   ELSE
123600040708     C                   SETON                                        32
123700040708     C                   EVAL      VAOCRC = *zeros
123800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123900040708     C                             + ' ' + 'VAOCRC'
124000040708     C                   ENDIF
124100040708     C                   ENDIF
124200040708     C*** CAC
124300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
124400040708     C                   EVAL      PiStr=SkSplitCSV(i)
124500040708     C                   EXSR      CHKNUM
124600040708     C                   IF        PiInt=*on
124700040708     C                   Z-ADD     PiVal         Num5_0
124800040708     C                   MOVEL(p)  Num5_0        VAOCAC
124900040708     C                   ELSE
125000040708     C                   SETON                                        32
125100040708     C                   EVAL      VAOCAC = *zeros
125200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040708     C                             + ' ' + 'VAOCAC'
125400040708     C                   ENDIF
125500040708     C                   ENDIF
125600040708     C*** DAR
125700090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
125800090428     C                   EVAL      PiStr=SkSplitCSV(i)
125900090428     C                   EXSR      CHKNUM
126000090428     C                   IF        PiInt=*on
126100090428     C                   Z-ADD     PiVal         VAODAR
126200090428     C                   ELSE
126300090428     C                   SETON                                        32
126400090428     C                   EVAL      VAODAR = *zeros
126500090428     C                   EVAL      vinmsg = %trimr(vinmsg)
126600090428     C                             + ' ' + 'VAODAR'
126700090428     C                   ENDIF
126800090428     C                   ENDIF
126900040708     C*** ORR
127000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
127100040708     C                   EVAL      PiStr=SkSplitCSV(i)
127200040708     C                   EXSR      CHKNUM
127300040708     C                   IF        PiInt=*on
127400040708     C                   Z-ADD     PiVal         VAOORR
127500040708     C                   ELSE
127600040708     C                   SETON                                        32
127700040708     C                   EVAL      VAOORR = *zeros
127800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127900040708     C                             + ' ' + 'VAOORR'
128000040708     C                   ENDIF
128100040708     C                   ENDIF
128200030721     C*** NCL
128300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on
128700040723     C                   IF        PiVal > *zeros
128800040708     C                   Z-ADD     PiVal         VAONCL
128900040723     C                   ENDIF
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200040708     C                   EVAL      VAONCL = *zeros
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400040708     C                             + ' ' + 'VAONCL'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700040708     C*** PKG
129800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiNum=*on
130200040723     C                   IF        PiVal > *zeros
130300040708     C                   Z-ADD     PiVal         VAOPKG
130400040723     C                   ENDIF
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700040708     C                   EVAL      VAOPKG = *zeros
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900040708     C                             + ' ' + 'VAOPKG'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200040708     C*** VLM
131300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
131400040708     C                   EVAL      PiStr=SkSplitCSV(i)
131500040708     C                   EXSR      CHKNUM
131600040708     C                   IF        PiNum=*on
131700040708     C                   Z-ADD     PiVal         VAOVLM
131800040708     C                   ELSE
131900040708     C                   SETON                                        32
132000040708     C                   EVAL      VAOVLM = *zeros
132100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132200040708     C                             + ' ' + 'VAOVLM'
132300040708     C                   ENDIF
132400040708     C                   ENDIF
132500040708     C*** BNC
132600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
132700040708     C                   EVAL      PiStr=SkSplitCSV(i)
132800040708     C                   EXSR      CHKNUM
132900040708     C                   IF        PiInt=*on
133000040708     C                   Z-ADD     PiVal         VAOBNC
133100040708     C                   ELSE
133200040708     C                   SETON                                        32
133300040708     C                   EVAL      VAOBNC = *zeros
133400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133500040708     C                             + ' ' + 'VAOBNC'
133600040708     C                   ENDIF
133700040708     C                   ENDIF
133800040708     C*** BLC
133900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
134000040708     C                   EVAL      PiStr=SkSplitCSV(i)
134100040708     C                   EXSR      CHKNUM
134200040708     C                   IF        PiInt=*on
134300040708     C                   Z-ADD     PiVal         VAOBLC
134400040708     C                   ELSE
134500040708     C                   SETON                                        32
134600040708     C                   EVAL      VAOBLC = *zeros
134700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134800040708     C                             + ' ' + 'VAOBLC'
134900040708     C                   ENDIF
135000040708     C                   ENDIF
135100040708     C*** ATT
135200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
135300040708     C                   EVAL      PiStr=SkSplitCSV(i)
135400040708     C                   EXSR      CHKNUM
135500040708     C                   IF        PiInt=*on
135600040708     C                   Z-ADD     PiVal         VAOATT
135700040708     C                   ELSE
135800040708     C                   SETON                                        32
135900040708     C                   EVAL      VAOATT = *zeros
136000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136100040708     C                             + ' ' + 'VAOATT'
136200040708     C                   ENDIF
136300040708     C                   ENDIF
136400040708     C*** MTC
136500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
136600040708     C                   EVAL      PiStr=SkSplitCSV(i)
136700040708     C                   EXSR      CHKNUM
136800040708     C                   IF        PiInt=*on
136900040708     C                   Z-ADD     PiVal         VAOMTC
137000040708     C                   ELSE
137100040708     C                   SETON                                        32
137200040708     C                   EVAL      VAOMTC = *zeros
137300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137400040708     C                             + ' ' + 'VAOMTC'
137500040708     C                   ENDIF
137600040708     C                   ENDIF
137700040708     C*** KSC
137800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on      AND
138200030721     C                             PiVal<=9999999 AND
138300030721     C                             PiVal>*zeros
138400040708     C                   Z-ADD     PiVal         VAOKSC
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700040708     C                   EVAL      VAOKSC = *zeros
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900040708     C                             + ' ' + 'VAOKSC'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200040708     C*** POR
139300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiInt=*on    AND
139700030721     C                             PiVal<=999   AND
139800030721     C                             PiVal>*zeros
139900040708     C                   Z-ADD     PiVal         VAOPOR
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200040708     C                   EVAL      VAOPOR = *zeros
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400040708     C                             + ' ' + 'VAOPOR'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700040708     C*** POC
140800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
140900040708     C                   EVAL      PiStr=SkSplitCSV(i)
141000040708     C                   EXSR      CHKNUM
141100040708     C                   IF        PiInt=*on    AND
141200040708     C                             PiVal<=999   AND
141300040708     C                             PiVal>*zeros
141400040708     C                   Z-ADD     PiVal         VAOPOC
141500040708     C                   ELSE
141600040708     C                   SETON                                        32
141700040708     C                   EVAL      VAOPOC = *zeros
141800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141900040708     C                             + ' ' + 'VAOPOC'
142000040708     C                   ENDIF
142100040708     C                   ENDIF
142200040708     C*** STO
142300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
142400040708     C                   EVAL      PiStr=SkSplitCSV(i)
142500040708     C                   EXSR      CHKNUM
142600040708     C                   IF        PiInt=*on
142700040708     C                   Z-ADD     PiVal         VAOSTO
142800040708     C                   ELSE
142900040708     C                   SETON                                        32
143000040708     C                   EVAL      VAOSTO = *zeros
143100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143200040708     C                             + ' ' + 'VAOSTO'
143300040708     C                   ENDIF
143400040708     C                   ENDIF
143500090428     C*
143600091103     C********** GESTIONE CAMPI PERSONALIZZATI
143700090428     C*
143800100419     C*** DAR
143900100419     C                   EVAL      VAODAR = %dec(%date() + %Days(1) : *ISO)
144000090428     C*
144100090428     C****************************************
144200090428     C*
144300030715     C*
144400030715     C                   ENDIF
144500030715     C                   EVAL      i = i + 1
144600030715     C                   ENDDO
144700040723     C*
144800030715     C                   ENDIF
144900020204     C*
145000010607     C                   ENDSR
145100010607     C*----------------------------------------------------*
145200080702
145300080702
145400080702     C*----------------------------------------------------*
145500080702     C*  CONTROLLO NUMERICITA' CAMPI
145600080702     C*----------------------------------------------------*
145700080702     C     CHKNUM        BEGSR
145800080702     C*
145900080702     C                   IF        PiDecChr = *blanks
146000080702     C                   EVAL      PiDecChr = ','
146100080702     C                   ENDIF
146200080702     C*
146300080702     C                   callp     UBISNUM_Check(PiStr
146400080702     C                                          :PiDecChr
146500080702     C                                          :PiVal
146600080702     C                                          :PiNum
146700080702     C                                          :PiInt)
146800080702     C*
146900080702     C                   ENDSR
147000080702     C***
147100080702
147200010601
147300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
147400010601     C     repfil        BEGSR
147500010601     C*
147600010601     C                   if        invfil = *zeros and
147700010601     C                             depfil > *zeros and
147800010629     C                             (vinflg = *blanks or
147900010629     C                              vinflg = *zeros)
148000010601     C
148100010601     C                   eval      invfil = depfil
148200010601     C                   endif
148300010601     C*
148400010601     C                   if        depfil <> invfil and
148500010601     C                             invfil > *zeros
148600010601     C                   eval      flgMulti = '1'
148700010601     C                   if        vinflg = *blanks
148800010601     C                   add       1             cntNonEl
148900010601     C                   endif
149000010601     C                   endif
149100010601     C*
149200010601     C                   if        vinflg = '2'
149300010601     C                   eval      flgStato = '2'
149400010601     C                   endif
149500010601     C*
149600010601     C                   ENDSR
149700010601     C***
149800010330
149900010330
150000990920      /TITLE Invio dei dati al punto operativo.
150100000613     C     invio         BEGSR
150200990920     C*
150300990920     C                   reset                   dscmz
150400010601     C                   move      invfil        cmzdst
150500990920     C                   eval      cmzfld = vlrfou
150600990920     C                   eval      cmzmbd = vlrhdl
150700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
150800000710     C                   if        prmfir = *blanks
150900040708     C                   eval      cmzfla = 'FNVAO00F'
151000040708     C                   eval      cmzmba = 'FNVAO00F'
151100000710     C                   else
151200000710     C                   eval      cmzfla = prmfir
151300000710     C                   eval      cmzmba = prmfir
151400000710     C                   endif
151500990920     C                   eval      cmznrr = *zeros
151600990920     C                   move      §ctrok        cmznrr
151700021018     C                   eval      cmzlba = vlrfl1
151800990920     C                   call(e)   'TIS711C'
151900990920     C                   parm                    dscmz
152000990921     C                   parm      *blanks       esito
152100990920     C                   if        %error
152200990920     C                             or cmzerr = '1'
152300990921     C                             or esito  = '1'
152400000710     C                   eval      wrkesito = '3'
152500990920     C                   endif
152600990920     C*
152700000613     C                   ENDSR
152800010601
152900010601
153000010601      /TITLE Invio dei dati al punto operativo.
153100010601     C     opeini        BEGSR
153200010601     C*
153300010601     C* Inizializzo flag e contatori operativi
153400010601     C                   movel     '0'           flgGiro           1
153500010601     C                   movel     '0'           flgMulti          1
153600010601     C                   movel     '1'           flgStato          1
153700010615     C                   movel     '0'           flgOk             1
153800010601     C                   z-add     *zeros        cntNonEl         10 0
153900010601     C                   z-add     *zeros        depfil            3 0
154000010601     C                   z-add     *zeros        invfil            3 0
154100010601     C*
154200010601     C                   ENDSR
154300010601     C***
154400050421
154500050421
154600091130     C     *pssr         BEGSR
154700050421     C*
154800091130     C                   eval      wrkesito = '2'
154900050421     C*
155000091130     C                   ENDSR     '*CANCL'
155100050421     C***
155200060306
155300060306
155400060306     C*-------------------------------------------------------------------------*
155500060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
155600060306     C*-------------------------------------------------------------------------*
155700060306     C     sr_Control    begsr
155800060306     C*
155900060306     C* inizializzo a ok il flag d controllo abilitazione
156000060306     C                   eval      Cli_Abil = *on
156100060306     C
156200060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
156300060306     C                   clear                   woriIDC
156400060306     C                   eval      kteCOD = 'VAP'
156500060306     C                   eval      kteKE1 = oriIDC
156600060306     C     keytbe        chain     TNTBE01L
156700060306     C                   if        %found(Tntbe01l)
156800060306     C                   eval      woriIDC = TbeKe2
156900060306     C                   else
157000060306     C                   eval      Cli_Abil = *off
157100060306     C                   endif
157200060306     C*
157300060306     C                   endsr
157400010330
157500010330
157600000613     C     *inzsr        BEGSR
157700990910     C*
157800990910     C     *entry        plist
157900990920     C                   parm                    tivlrds
158000990921     C                   parm      wrkesito      esito
158100000724     C                   parm                    prmlit
158200000710     C                   parm                    prmfir
158300010330     C*
158400010330     C* CALCOLA LA DATA CORRENTE
158500091127     C                   z-add     *zeros        datcor            8 0
158600091127     C                   time                    wn14             14 0
158700091127     C                   movel     wn14          wn6               6 0          *ORA (6)
158800091127     C                   eval      datcor = %dec(%date() : *ISO)
158900040723     C*
159000040723     C* Campi riferito al database
159100040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
159200040723     C     *like         define    tbeke1        kteKE1
159300040723     C*
159400040723     C* Definizione chiavi di lettura
159500040723     C* TNTBE01L - parziale
159600040723     C     keytbe        klist
159700040723     C                   kfld                    kteCOD                         *UTENTE
159800040723     C                   kfld                    kteKE1                         *CODICE
159900000613     C*
160000000613     C                   ENDSR
160100000613     C***
160200990908
160300100416     Otitv1x0p  E            riepilogo         2
160400990915     O                                              'Upload via Internet'
160500990915     O                                           +1 'Traduzione TIVIN00R -'
160600040708     O                                           +1 'FNVAOWWR'
160700990915     O                                           +1 'Testate bolle'
160800990915     O          E            riepilogo   2
160900990915     O                       wrkdata
161000990915     O                       wrkora              +1
161100990915     O                       procname            +1
161200990915     O          E            riepilogo   2
161300990915     O                                              'Cliente..................:'
161400040708     O                       VAOCOR        z     +1
161500990915     O          E            riepilogo   2
161600990920     O                                              'Riferimento Strategi.....:'
161700990920     O                       vlrhdl              +1
161800990915     O          E            riepilogo   2
161900990915     O                                              'Giusti...................:'
162000971022     O                       §CTROK        2   +  1
162100990915     O          E            riepilogo   2
162200990915     O                                              'Sbagliati e corretti.....:'
162300971022     O                       §CTRMO        2   +  1
162400990915     O          E            riepilogo   2
162500990915     O                                              'Sbagliati e scartati.....:'
162600971022     O                       §CTRNO        2   +  1
162700000613
162800100416     Otitv1x0ps E            testdett          2
162900000613     O                                              'Upload via Internet'
163000000613     O                                           +1 'Traduzione TIVIN00R -'
163100040708     O                                           +1 'FNVAOWWR'
163200000613     O                                           +1 'Testate bolle'
163300000616     O          E            testdett    3
163400000613     O                                           +2 'N° rec'
163500000613     O                                           +3 'Anteprima contenuto'
163600000616     O          E            rigadett    2
163700000613     O                       rrnum               +2
163800000621     O                       recko               +3
163900000616     O          E            findett     2
164000000613     O                       wrkdata
164100000613     O                       wrkora              +1
164200000613     O                       procname            +1
164300000616     O          E            findett     2
164400000613     O                                              'Cliente..................:'
164500040708     O                       VAOCOR        z     +1
164600000616     O          E            findett     2
164700000613     O                                              'Riferimento Strategi.....:'
164800000613     O                       vlrhdl              +1
164900000616     O          E            findett     2
165000000613     O                                              'Giusti...................:'
165100000613     O                       §CTROK        2   +  1
165200000616     O          E            findett     2
165300000613     O                                              'Sbagliati e corretti.....:'
165400000613     O                       §CTRMO        2   +  1
165500000616     O          E            findett     2
165600000613     O                                              'Sbagliati e scartati.....:'
165700000613     O                       §CTRNO        2   +  1
165800000613** CMD - COMANDI CL
165900100416OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1X0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
166000100416OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1X0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
166100100416DLTOVR FILE(TITV1X0P TITV1X0PS) LVL(*)
166200010606
166300000613
166400040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
166500100419FIL001;FIL002;VAOCOR;FIL003;FIL004;VAORSR;VAOINR;VAOLOR;VAOCAR;VAOPRR;
166600100420VAOTER;VAORER;VAORFA;FIL005;FIL006;FIL007;VAONCL;VAOPKG;VAOVLM;VAOBNC;
166700100416VAONO1
