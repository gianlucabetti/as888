000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200080702     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500040723     Ffnacr01l  if   e           k disk
000600090617     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800040708     FFnvaowwr  O    E             DISK    usropn
000900040723     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100080702     Ftitv1h6p  O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300080702     Ftitv1h6ps O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040709     D*------------
002100040709     D* SCHIERE A PROGRAMMA
002200040709     D*------------
002300110616     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
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
003400080707     D trul13ds      e ds                  inz
003500090617     d trul33ds      e ds
003600090617     d kpjba         e ds
003700010330     D esito           s              1
003800010330     D prmlit          s             10
003900010330     D prmfir          s             10
004000010330     D wrkesito        s                   like(esito)
004100010330     D wrkdata         s               d
004200010330     D wrkora          s               t
004300010330     D rrnum           s              6  0 INZ(*zeros)
004400010330     D recko           s            150    INZ(*blanks)
004500011113     D depcmd          s            150    INZ(*blanks)
004600040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004700060306     D Cli_Abil        s              1    inz(*off)
004800060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004900090617     d §numori         s              7  0 inz
005000030715     D*------------------
005100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005200030715     D*------------------
005300030715     D SkSplitFLD      S             10    DIM(100)
005400030715     D SkSplitCSV      S            256    DIM(100)
005500030715     D CharCSV         S              1
005600030715     D CharTXT         S              1
005700030715     D CharNUM         S              1
005800030715     D posDa           S              3  0 INZ(*zeros)
005900030715     D posA            S              3  0 INZ(*zeros)
006000030715     D i               s              3  0 INZ(1)
006100030715     D wGiro           s              1  0 INZ(*zeros)
006200030822     D*------------------
006300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006400030822     D*------------------
006500030822     D posDaDft        S              3  0 INZ(*zeros)
006600030822     D posADft         S              3  0 INZ(*zeros)
006700030822     D j               s              3  0 INZ(1)
006800030822     D wGiroDft        s              1  0 INZ(*zeros)
006900010330     D*------------------
007000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007100010330     D*------------------
007200010330     D WLBDA8          DS                  INZ
007300010330     D  G08DAT                 1      8  0
007400010330     D  G08INV                 9     16  0
007500010330     D  G08ERR                17     17
007600010330     D  G08TGI                18     22  0
007700040723     D*------------------
007800040723     D* DS REPERIMENTO DATI CLIENTE
007900040723     D*-------------------
008000040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008100040723     D ACODS         E DS                  EXTNAME(CNACO00F)
008200040723     D INDDS         E DS                  EXTNAME(CNIND00F)
008300040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008400040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008500080702     D*------------------
008600080702     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
008700080702     D*------------------
008800080702     D PiStr           S            100A   INZ
008900080702     D PiDecChr        S              1A   INZ
009000080702     D PiVal           S             63S30 INZ
009100080702     D PiInt           S               N   INZ
009200080702     D PiNum           S               N   INZ
009300080702
009400080702     D*------------------
009500080702     D* LINKING A DEFINIZIONI ESTERNE
009600080702     D*------------------
009700080702     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009800091026
009900091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
010000091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
010100091026
010200120605     D wRER_1          s                   like(VAORER)
010300120605     D wRER_2          s                   like(VAORER)
010400080702
010500031201     D*------------------
010600031201     D* Costanti
010700031201     D*------------------
010800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011000010330
011100010330
011200010330
011300990915     C                   time                    wrkdata
011400990915     C                   time                    wrkora
011500000913     C                   reset                   rrnum
011600990921     C                   reset                   esito
011700990921     C                   reset                   wrkesito
011800010601     C*
011900010601     C                   exsr      opeini
012000041011     C                   exsr      PREELA
012100040708     C                   exsr      rwvao
012200010601     C*
012300010601     C                   seton                                        lr
012400010601
012500010601
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013000010601     C*--------------------------------------------------------
013100010601     C     PREELA        BEGSR
013200010601     C*
013300010601     C* SE OCCORRE SPEDIRE IN FILIALE
013400041011     C                   if        flgGiro = '0'
013500010601     C*
013600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013700010601     C                   eval      flgGiro = '1'
013800000724     C*
013900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
014000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014100041011     C     vlrpoi        chain     azorg01l
014200000724     C                   if        %found
014300000724     C                   movel(p)  CMD(1)        depcmd
014400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014500000724     C*
014600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014700011113     C                   Z-ADD     150           LENGH            15 5
014800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014900000724     C                   PARM                    depcmd
015000000724     C                   PARM                    LENGH
015100000724     C*
015200041011     C                   else
015300041011     C                   setoff                                       50
015400000724     C                   endif
015500000616     C*
015600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015700000616     C                   movel(p)  CMD(2)        depcmd
015800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015900000616     C*
016000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
016100011113     C                   Z-ADD     150           LENGH            15 5
016200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016300000616     C                   PARM                    depcmd
016400000616     C                   PARM                    LENGH
016500041011     C*
016600041011     C                   endif
016700000613     C*
016800080702     C                   if        not %open(titv1h6ps)
016900080702     C                   open      titv1h6ps
017000030715     C                   except    testdett
017100000613     C                   endif
017200010601     C*
017300010601     C                   ENDSR
017400010601     C***
017500010601
017600010601
017700010601
017800010601
017900010601
018000010601
018100010601
018200010601     C*--------------------------------------------------------
018300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
018400010601     C*--------------------------------------------------------
018500010601     C     ENDELA        BEGSR
018600990915     C*
018700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018800000613     C*
018900080702     C                   if        %open(titv1h6ps)
019000000616     C                   except    findett
019100080702     C                   close     titv1h6ps
019200000613     C                   endif
019300000616     C*
019400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019500011113     C                   Z-ADD     150           LENGH            15 5
019600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019700000616     C                   PARM                    CMD(3)
019800000616     C                   PARM                    LENGH
019900080707     C*
020000080707     C* Chiusura TRUL13R
020100080707     C                   clear                   TRUL13DS
020200080707     C                   eval      I13TLA = 'C'
020300080707     C                   call      'TRUL13R'
020400080707     C                   parm                    TRUL13DS
020500000616     C*
020600010601     C                   ENDSR
020700010601     C***
020800010601
020900010601
021000010601
021100010601
021200000613
021300000613     C*--------------------------------------------------------
021400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021500000613     C*--------------------------------------------------------
021600000621     C     STPR          BEGSR
021700000613     C*
021800080702     C                   if        not %open(titv1h6p)
021900080702     C   50              open      titv1h6p
022000990915     C                   endif
022100990915     C*
022200030715     C   50              except    riepilogo
022300990915     C*
022400080702     C                   if        %open(titv1h6p)
022500080702     C   50              close     titv1h6p
022600990914     C                   endif
022700000613     C*
022800000613     C                   ENDSR
022900000613     C***
023000000613
023100010330
023200010601
023300010601
023400010601
023500010330     C*--------------------------------------------------------
023600040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
023700010330     C*--------------------------------------------------------
023800040708     C     RWVAO         BEGSR
023900010330     C*
024000010330     C                   if        not %open(tivin00r)
024100010330     C                   open      tivin00r
024200010330     C                   endif
024300040708     C                   if        not %open(fnvaowwr)
024400040708     C                   open      fnvaowwr
024500010330     C                   endif
024600010330     C*
024700010604     C                   clear                   §CTROK
024800010604     C                   clear                   §CTRMO
024900010604     C                   clear                   §CTRNO
025000010330     C*
025100030822     C                   do        *HIVAL
025200010330     C*
025300030822     C                   read      tivin00r                               70
025400010618     C*
025500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025600010618     C                   if        vinflg = '1'
025700010618     C                   eval      flgOk = '1'
025800010618     C                   endif
025900010618     C*
026000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026100040510     C                   z-add     *zeros        wLenVINDTA        4 0
026200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026600040510     C                   z-add     1             wFlgVINDTA
026700040510     C                   leave
026800040510     C                   endif
026900040510     C                   enddo
027000040510     C*
027100040218     C                   if        vindta > *blanks AND
027200040510     C                             wFlgVINDTA = 1
027300010330     C                   add       1             rrnum
027400010330     C*
027500010601     C                   if        *in70 = *off and
027600010330     C                             (vinflg = *blanks
027700010330     C                              or vinflg = '0'
027800010330     C                              or vinflg = '2')
027900010330     C*
028000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028100010711     C                   if        vinflg = *blanks or vinflg = '0'
028200010711     C                   clear                   vinmsg
028300010711     C                   endif
028400010601     C*
028500010330     C                   exsr      inzvar
028600040708     C                   exsr      impvao
028700100512     C                   exsr      exeforz
028800010601     C*
028900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029000040708     C                   eval      depfil = VAOPOE
029100010601     C                   exsr      repfil
029200010601     C                   if        depfil = invfil
029300021025     C                   if        vlrpoi = 999
029400040708     C                   move(P)   invfil        VAOPOE
029500021025     C                   else
029600040708     C                   move(P)   vlrpoi        VAOPOE
029700021025     C                   endif
029800010601     C*
029900010604     C* Ebbene...
030000010604     C*
030100030822     C  N31              add       1             §CTROK            7 0
030200030822     C   32              add       1             §CTRMO            7 0
030300030822     C   31              add       1             §CTRNO            7 0
030400030822     C                   if        wGiro = 2
030500040723     C  N31              EXSR      CHKCOR
030600040723     C  N31              EXSR      WRIORI
030700050826     C  N31              EXSR      WRIVGD
030800040723     C  N31              write     FNVAO000
030900030822     C                   endif
031000010604     C*
031100010604     C                   if        *in31 = *off and
031200010604     C                             *in32 = *off
031300010604     C                   eval      vinflg = '1'
031400010604     C                   else
031500010604     C                   eval      recko = vindta
031600041011     C                   except    rigadett
031700010604     C                   eval      vinflg = '2'
031800010604     C                   endif
031900010604     C                   endif
032000010601     C*
032100010604     C                   endif
032200010604     C*
032300010330     C                   else
032400010330     C                   eval      vinflg = '1'
032500010330     C                   endif
032600010601     C*
032700010601     C  N70              update    tivin000
032800010330     C*
032900030822     C  N70              enddo
033000010601     C
033100010601     C*
033200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033300010601     C                   if        cntNonEl = *zeros or
033400010601     C                             flgMulti = '0'
033500010330     C* Se non ci sono record con errori ...
033600010601     C                   if        §ctrno = 0 and
033700010604     C                             §ctrmo = 0 and
033800010601     C                             flgStato <> '2'
033900010330     C* ... restituisco esito OK.
034000010330     C                   eval      wrkesito = '0'
034100010330     C                   else
034200010330     C                   if        §ctrok > 0
034300010330     C                   eval      wrkesito = '1'
034400010330     C                   else
034500010615     C                   if        flgOk = '0'
034600010615     C                   eval      wrkesito = '2'
034700010615     C                   else
034800010615     C                   eval      wrkesito = '6'
034900010615     C                   endif
035000010330     C                   endif
035100010330     C                   endif
035200010601     C                   else
035300010601     C                   eval      wrkesito = '9'
035400010601     C                   endif
035500010330     C*
035600010330     C                   if        %open(tivin00r)
035700010330     C                   close     tivin00r
035800010330     C                   endif
035900040708     C                   if        %open(fnvaowwr)
036000040708     C                   close     fnvaowwr
036100010330     C                   endif
036200010601     C*
036300010601     C                   if        vlrpoi <> 999
036400010601     C                   eval      invfil = vlrpoi
036500010601     C                   endif
036600010330     C*
036700010330     C                   if        §ctrok > 0
036800010601     C                             and invfil > *zeros
036900010330     C                   exsr      invio
037000010330     C                   endif
037100010601     C*
037200010618     C                   if        flgGiro = '1'
037300010601     C                   exsr      endela
037400010618     C                   endif
037500010330     C*
037600010330     C                   ENDSR
037700010330     C***
037800040723
037900040723
038000040723     C*------------------------------------------------------------------------*
038100040723     C* CHKCOR - Considerazioni su VAOCOR
038200040723     C*------------------------------------------------------------------------*
038300040723     C     CHKCOR        BEGSR
038400040723     C*
038500040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
038600040723     C                   if        vaoTCO = 'F'
038700040723     C* Quindi verifico se il codice ordinante non è valorizzato
038800040723     C                   if        vaoCOR = *zeros  AND
038900040723     C                             vaoRSO = *blanks AND
039000040723     C                             vaoINO = *blanks AND
039100040723     C                             vaoCAO = *blanks AND
039200040723     C                             vaoLOO = *blanks AND
039300040723     C                             vaoPRO = *blanks AND
039400040723     C                             vaoNAO = *blanks
039500040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
039600040723     C                   movel     '0'           wFlgAcr           1
039700040723     C                   move      vlrKSC        wCli              7 0
039800040723     C                   move(p)   wCli          acrCRO
039900040723     C     acrCRO        setll     fnacr01l
040000040723     C                   if        %found(fnacr01l)
040100040723     C                   read      fnacr01l
040200040723     C                   dow       not %eof(fnacr01l)
040300040723     C                   movel     acrCRO        wCliAcr           7 0
040400040723     C                   if        wCliAcr = wCli
040500040723     C                   eval      vaoCOR = acrCRO
040600040723     C                   movel     '1'           wFlgAcr
040700040723     C                   leave
040800040723     C                   else
040900040723     C                   leave
041000040723     C                   endif
041100040723     C                   read      fnacr01l
041200040723     C                   enddo
041300040723     C                   endif
041400040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
041500040723     C* dati anagrafici dell'ordinante dal piano dei conti
041600040723     C                   if        wFlgAcr = '0'
041700040723     C                   clear                   BS69DS
041800040723     C                   clear                   ACODS
041900040723     C                   clear                   INDDS
042000040723     C                   clear                   CLPDS
042100040723     C                   clear                   CLSDS
042200040723     C                   move(P)   vlrKSC        I69KAC
042300040723     C                   move(P)   vlrKSC        I69KIN
042400040723     C                   call      'TIBS69R'
042500040723     C                   parm                    BS69DS
042600040723     C                   parm                    ACODS
042700040723     C                   parm                    INDDS
042800040723     C                   parm                    CLPDS
042900040723     C                   parm                    CLSDS
043000040723     C     O69ERR        ifne      '1'
043100040723     C                   eval      vaoRSO = ACORAG
043200040723     C                   eval      vaoINO = INDVIA
043300040723     C                   movel(P)  INDCAP        vaoCAO
043400040723     C                   eval      vaoLOO = INDCIT
043500040723     C                   eval      vaoPRO = INDPRV
043600040723     C                   eval      vaoNAO = INDSTA
043700040723     C                   endif
043800040723     C                   endif
043900040723     C                   endif
044000040723     C                   endif
044100040723     C*
044200040723     C                   ENDSR
044300040723
044400040723
044500040723
044600040723     C*------------------------------------------------------------------------*
044700040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
044800040723     C*------------------------------------------------------------------------*
044900040723     C     WRIORI        BEGSR
045000040723     C*
045100090617     C* Come prima cosa stacco un numeratore da AZNUM
045200090617     C                   clear                   trul33ds
045300090617     C                   eval      I33OPE = *zeros
045400090617     C                   eval      I33CNU = 600
045500090617     C                   eval      I33NUM = 1
045600090617     C                   movel     TRUL33DS      KPJBU
045700090617     C                   call      'TRUL33R'
045800090617     C                   parm                    KPJBA
045900090617     C                   movel     KPJBU         TRUL33DS
046000090617     C                   if        O33ERR = *zeros
046100090617     C                   z-add     O33NRF        §numori
046200090618     c                   else
046300090618     c                   eval      §numori = *all'9'
046400090617     C                   endif
046500040723     C*
046600040723     C* Quindi imposto i campi "extra-VAO"
046700040723     C                   if        vlrKSC = '0ORM0000'
046800040723     C*
046900040723     C                   if        vaoCOR > *zeros
047000040723     C                   movel     vaoCOR        wrkCOR7           7 0
047100040723     C*
047200040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
047300040723     C* codice padre. Se è un padre assegno già il codice ordinante.
047400040723     C                   clear                   tibs10ds
047500040723     C                   eval      d10drf = datcor                              *data riferimento
047600040723     C                   eval      d10paf = 'P'                                 *cerca il padre
047700040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
047800091026     C*
047900091026     C* Preparazione chiamata al TIBS10R1
048000091026     C                   clear                   tibs10r1ds
048100091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
048200091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
048300091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
048400091026     C*
048500091026     C                   call      'TIBS10R1'
048600091026     C                   parm                    tibs10r1ds
048700091026     C*
048800091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
048900040723     C*
049000040723     C                   if        d10err <> *blanks or                         *No unif. o padre
049100040723     C                             d10cop =  *zeros  or
049200040723     C                             d10cop =  d10cod
049300040723     C                   move      d10cod        oriIDC
049400040723     C                   else                                                   *Figlio
049500040723     C                   move      d10cop        oriIDC
049600040723     C                   endif
049700040723     C*
049800040723     C                   else
049900040723     C                   eval      oriIDC = *zeros
050000040723     C                   endif
050100040723     C*
050200040723     C                   else
050300040723     C                   eval      oriIDC = vlrKSC
050400040723     C                   endif
050500040723     C*
050600040723     C                   eval      oriPRG = §numori
050700040723     C                   eval      oriDER = datcor
050800040723     C                   eval      oriOER = wn6
050900040723     C                   eval      oriPOE = vaoPOE
051000040723     C                   eval      oriNSR = vaoNSR
051100040723     C                   eval      oriNOR = vaoNOR
051200040723     C                   eval      oriNRV = vaoNRV
051300040723     C                   eval      oriFLO = *blanks
051400040723     C*
051500040723     C* Infine valorizzo la chiave esterna sul file FNVAO
051600040723     C                   eval      dorm01 = vaoFLO
051700040723     C                   movel     §numori       §ormpg
051800050412     C*
051900050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
052000050412     C                   movel     'S'           §ormfr
052100050412     C*
052200050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
052300040723     C                   eval      vaoFLO = dorm01
052400040723     C*
052500040723     C* ...quindi scrivo il file TIORI00F
052600040723     C                   WRITE     TIORI000
052700040723     C*
052800040723     C                   ENDSR
052900050826
053000050826
053100050826
053200050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
053300050826     C     wriVGD        BEGSR
053400050826     C*
053500050826     C* Reperisco la descrizione della fase da tabella
053600050826     C                   movel(p)  'FAR'         KteCOD
053700050826     C                   movel(p)  '000'         KteKE1
053800050826     C     KEYtbe        chain     tntbe01l
053900050826     C                   if        %found(tntbe01l)
054000050826     C                   movel(P)  tbeuni        DFAR
054100050826     C                   else
054200050826     C                   clear                   DFAR
054300050826     C                   endif
054400050826     C*
054500050826     C* Valorizzo buffer record
054600050826     C                   clear                   FNVAPDS
054700050826     C                   eval      vapIDC = oriIDC
054800050826     C                   eval      vapPOE = oriPOE
054900050826     C                   eval      vapRFA = vaoRFA
055000050826     C                   eval      vapPOG = oriPOE
055100050826     C                   eval      vapDAE = oriDER
055200050826     C                   eval      vapORE = oriOER
055300050826     C                   if        d§fardva <> *blanks
055400050826     C                   eval      vapDFA = d§fardva
055500050826     C                   else
055600050826     C                   eval      vapDFA = d§fardes
055700050826     C                   endif
055800050826     C*
055900050826     C                   clear                   tivgd000
056000080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
056100050826     C                   eval      vgdTIP = 'VP'
056200050826     C                   eval      vgdKSU = vapIDC
056300050826     C                   eval      vgdTSC = 'WW'
056400050826     C                   eval      vgdDAT = datcor
056500080702     C                   eval      vgdPGM = 'TITV1H6R'
056600060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
056700060306     C                   exsr      sr_Control
056800060306     C                   if        Cli_Abil = *on
056900060306     C                   eval      vgdKSU = woriIDC
057000050826     C                   write     tivgd000
057100060306     C                   endif
057200050826     C*
057300050826     C                   ENDSR
057400050826     C*------------------------------------------------------------------------*
057500010601
057600010601
057700010601
057800010330     C*----------------------------------------------------*
057900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
058000010330     C*----------------------------------------------------*
058100010330     C     INZVAR        BEGSR
058200010330     C*
058300030822     C* Inizializzo variabili di wrk
058400020204     C                   Z-ADD     *zeros        Num5_0            5 0
058500030822     C*
058600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
058700040708     C                   CLEAR                   FNVAO000
058800040723     C                   CLEAR                   TIORI000
058900030822     C                   CLEAR                   SkSplitCSV
059000030822     C                   EVAL      i = 1
059100030822     C                   EVAL      posDa = *zeros
059200030822     C                   EVAL      posA  = *zeros
059300030822     C*
059400030822     C* Reimposto i valori di default
059500030822     C                   EXSR      DEFCAM
059600030822     C*
059700010330     C                   ENDSR
059800010330     C*----------------------------------------------------*
059900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
060000010330     C*----------------------------------------------------*
060100010330     C     DEFCAM        BEGSR
060200030822     C*
060300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
060400030715     C* e delimitatore testo.
060500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
060600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
060800030822     C*
060900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
061000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
061100030715     C                   SETON                                        50
061200030715     C                   ELSE
061300030715     C                   SETOFF                                       50
061400030715     C                   ENDIF
061500040723     C*
061600080707     C* Imposto i valori d default d alcuni campi
061700111109     C***                EVAL      VAOCOR = 0594068102
061800111109     C***                EVAL      VAOCRC = 0594068102
061900111109     C***                EVAL      VAOCTR = '100'
062000111109     C***                EVAL      VAOORR = 1600
062100111109     C***                EVAL      VAOPOE = 059
062200080708     C*
062300080708     C                   CLEAR                   DORM01
062400080708     C                   EVAL      §ORCOMC = 'S'
062500080708     C                   EVAL      VAOFLO = DORM01
062600100226     C***                EVAL      VAODAR = %dec(%date() + %Days(1) : *ISO)
062700110621     C***                EVAL      VAODAR = datcor
062800080707     C*
062900110628     C                   EVAL      VAOPAG = 'O'
063000111114     C***                EVAL      VAODDT = 'S'
063100080707     C                   EVAL      VAOTCO = 'F'
063200080707     C                   EVAL      VAOTOR = 'S'
063300030822     C*
063400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
063500030822     C                   EVAL      posDaDft = 1
063600030822     C                   EVAL      posADft  = 0
063700030822     C                   EVAL      wGiroDft = 0
063800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
063900030822     C                             posDaDft > 0
064000030822     C*
064100030822     C* Gestisco il 1° giro
064200030822     C                   IF        wGiroDft = 0
064300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
064400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
064500030822     C* Incremento il contatore dei "giri"
064600030822     C                   EVAL      wGiroDft = 1
064700030822     C                   ELSE
064800030822     C                   EVAL      posDaDft = posADft
064900030822     C                   ENDIF
065000030822     C* Eseguo lo scan x trovare la fine del campo corrente
065100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
065200030822     C*
065300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
065400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
065500030822     C                   IF        posDaDft > 0 AND
065600030822     C                             posADft  > 0
065700040708     C* POE
065800040708     C                   IF        %subst(
065900040708     C                             %subst(vlrppt:posDaDft+1:
066000040708     C                             posADft-posDaDft-1):1:3)
066100040708     C                             = 'POE'
066200040708     C                   EVAL      PiStr=%trim(%subst(
066300040708     C                             %subst(vlrppt:posDaDft+1:
066400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500040708     C                   EXSR      CHKNUM
066600040708     C                   IF        PiInt=*on
066700040708     C                   Z-ADD     PiVal         VAOPOE
066800040708     C                   ENDIF
066900040708     C                   ENDIF
067000040708     C* TOR
067100040708     C                   IF        %subst(
067200040708     C                             %subst(vlrppt:posDaDft+1:
067300040708     C                             posADft-posDaDft-1):1:3)
067400040708     C                             = 'TOR'
067500040708     C                   EVAL      VAOTOR=%trim(%subst(
067600040708     C                             %subst(vlrppt:posDaDft+1:
067700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800040708     C                   ENDIF
067900040708     C* TCO
068000040708     C                   IF        %subst(
068100040708     C                             %subst(vlrppt:posDaDft+1:
068200040708     C                             posADft-posDaDft-1):1:3)
068300040708     C                             = 'TCO'
068400040708     C                   EVAL      VAOTCO=%trim(%subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700040708     C                   ENDIF
068800040707     C* COR
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200040707     C                             = 'COR'
069300030822     C                   EVAL      PiStr=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   EXSR      CHKNUM
069700030822     C                   IF        PiInt=*on
069800040708     C                   Z-ADD     PiVal         VAOCOR
069900030822     C                   ENDIF
070000030822     C                   ENDIF
070100040708     C* CRC
070200040708     C                   IF        %subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):1:3)
070500040708     C                             = 'CRC'
070600040708     C                   EVAL      PiStr=%trim(%subst(
070700040708     C                             %subst(vlrppt:posDaDft+1:
070800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900040708     C                   EXSR      CHKNUM
071000040708     C                   IF        PiInt=*on
071100040708     C                   Z-ADD     PiVal         VAOCRC
071200040708     C                   ENDIF
071300040708     C                   ENDIF
071400040708     C* FFD
071500040708     C                   IF        %subst(
071600040708     C                             %subst(vlrppt:posDaDft+1:
071700040708     C                             posADft-posDaDft-1):1:3)
071800040708     C                             = 'FFD'
071900040708     C                   EVAL      VAOFFD=%trim(%subst(
072000040708     C                             %subst(vlrppt:posDaDft+1:
072100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200040708     C                   ENDIF
072300040708     C* ORR
072400040708     C                   IF        %subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):1:3)
072700040708     C                             = 'ORR'
072800040708     C                   EVAL      PiStr=%trim(%subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040708     C                   EXSR      CHKNUM
073200040708     C                   IF        PiInt=*on
073300040708     C                   Z-ADD     PiVal         VAOORR
073400040708     C                   ENDIF
073500040708     C                   ENDIF
073600040712     C* NCL
073700040712     C                   IF        %subst(
073800040712     C                             %subst(vlrppt:posDaDft+1:
073900040712     C                             posADft-posDaDft-1):1:3)
074000040712     C                             = 'NCL'
074100040712     C                   EVAL      PiStr=%trim(%subst(
074200040712     C                             %subst(vlrppt:posDaDft+1:
074300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400040712     C                   EXSR      CHKNUM
074500040712     C                   IF        PiInt=*on
074600040712     C                   Z-ADD     PiVal         VAONCL
074700040712     C                   ENDIF
074800040712     C                   ENDIF
074900040712     C* PKG
075000040712     C                   IF        %subst(
075100040712     C                             %subst(vlrppt:posDaDft+1:
075200040712     C                             posADft-posDaDft-1):1:3)
075300040712     C                             = 'PKG'
075400040712     C                   EVAL      PiStr=%trim(%subst(
075500040712     C                             %subst(vlrppt:posDaDft+1:
075600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700040712     C                   EXSR      CHKNUM
075800040712     C                   IF        PiNum=*on
075900040712     C                   Z-ADD     PiVal         VAOPKG
076000040712     C                   ENDIF
076100040712     C                   ENDIF
076200040712     C* VLM
076300040712     C                   IF        %subst(
076400040712     C                             %subst(vlrppt:posDaDft+1:
076500040712     C                             posADft-posDaDft-1):1:3)
076600040712     C                             = 'VLM'
076700040712     C                   EVAL      PiStr=%trim(%subst(
076800040712     C                             %subst(vlrppt:posDaDft+1:
076900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000040712     C                   EXSR      CHKNUM
077100040712     C                   IF        PiNum=*on
077200040712     C                   Z-ADD     PiVal         VAOVLM
077300040712     C                   ENDIF
077400040712     C                   ENDIF
077500040708     C* NAM
077600040708     C                   IF        %subst(
077700040708     C                             %subst(vlrppt:posDaDft+1:
077800040708     C                             posADft-posDaDft-1):1:3)
077900040708     C                             = 'NAM'
078000040708     C                   EVAL      VAONAM=%trim(%subst(
078100040708     C                             %subst(vlrppt:posDaDft+1:
078200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300040708     C                   ENDIF
078400040708     C* PAG
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800040708     C                             = 'PAG'
078900040708     C                   EVAL      VAOPAG=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300040708     C* KSC
079400040708     C                   IF        %subst(
079500040708     C                             %subst(vlrppt:posDaDft+1:
079600040708     C                             posADft-posDaDft-1):1:3)
079700040708     C                             = 'KSC'
079800040708     C                   EVAL      PiStr=%trim(%subst(
079900040708     C                             %subst(vlrppt:posDaDft+1:
080000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100040708     C                   EXSR      CHKNUM
080200040708     C                   IF        PiInt=*on
080300040708     C                   Z-ADD     PiVal         VAOKSC
080400040708     C                   ENDIF
080500040708     C                   ENDIF
080600040708     C* CTR
080700040708     C                   IF        %subst(
080800040708     C                             %subst(vlrppt:posDaDft+1:
080900040708     C                             posADft-posDaDft-1):1:3)
081000040708     C                             = 'CTR'
081100040708     C                   EVAL      VAOCTR=%trim(%subst(
081200040708     C                             %subst(vlrppt:posDaDft+1:
081300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400040708     C                   ENDIF
081500040708     C* POC
081600040708     C                   IF        %subst(
081700040708     C                             %subst(vlrppt:posDaDft+1:
081800040708     C                             posADft-posDaDft-1):1:3)
081900040708     C                             = 'POC'
082000040708     C                   EVAL      PiStr=%trim(%subst(
082100040708     C                             %subst(vlrppt:posDaDft+1:
082200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300040708     C                   EXSR      CHKNUM
082400040708     C                   IF        PiInt=*on
082500040708     C                   Z-ADD     PiVal         VAOPOC
082600040708     C                   ENDIF
082700040708     C                   ENDIF
082800040708     C* NO1
082900040708     C                   IF        %subst(
083000040708     C                             %subst(vlrppt:posDaDft+1:
083100040708     C                             posADft-posDaDft-1):1:3)
083200040708     C                             = 'NO1'
083300040708     C                   EVAL      VAONO1=%trim(%subst(
083400040708     C                             %subst(vlrppt:posDaDft+1:
083500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600040708     C                   ENDIF
083700040708     C* DDT
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100040708     C                             = 'DDT'
084200040708     C                   EVAL      VAODDT=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600040708     C* RFA
084700040708     C                   IF        %subst(
084800040708     C                             %subst(vlrppt:posDaDft+1:
084900040708     C                             posADft-posDaDft-1):1:3)
085000040708     C                             = 'RFA'
085100040708     C                   EVAL      VAORFA=%trim(%subst(
085200040708     C                             %subst(vlrppt:posDaDft+1:
085300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400040708     C                   ENDIF
085500040708     C* FLO
085600040708     C                   IF        %subst(
085700040708     C                             %subst(vlrppt:posDaDft+1:
085800040708     C                             posADft-posDaDft-1):1:3)
085900040708     C                             = 'FLO'
086000040708     C                   EVAL      VAOFLO=%trim(%subst(
086100040708     C                             %subst(vlrppt:posDaDft+1:
086200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086300040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
086400080702     C* e posizione 7 del VAOFLO
086500040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
086600040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
086700050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
086800040708     C                   ENDIF
086900030822     C* ...
087000030822     C                   ENDIF
087100030822     C                   ENDDO
087200020204     C*
087300010330     C                   ENDSR
087400010607     C*----------------------------------------------------*
087500040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
087600010607     C*----------------------------------------------------*
087700040708     C     IMPVAO        BEGSR
087800010607     C*
087900010607     C                   SETOFF                                       3132
088000030515     C*
088100100223     C                   MOVEL     datcor        VAODAO
088200040708     C                   MOVEL     wn14          VAOOAO
088300050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088400050314     C     minu:maiu     XLATE     vindta        vindta
088500010607     C*
088600030715     C**********
088700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088800030715     C**********
088900030715     C                   IF        wGiro = *zeros
089000040709     C*
089100040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
089200040709     C                   EVAL      depvinDTA = vinDTA
089300040709     C*
089400040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
089500040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
089600110616     C                                      %trim(intcol(3))+%trim(intcol(4))+
089700110616     C                                      %trim(intcol(5))
089800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089900050314     C**!!!minu:maiu     XLATE     vindta        vindta
090000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090100040324     C                   DOW       posDa <= %len(%trimr(vindta))
090200030715     C*
090300030715     C* Gestisco il 1° campo
090400030715     C                   IF        i = 1
090500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090600030715     C                   EVAL      posDa = 1
090700030715     C* Eseguo lo scan x trovare la fine del primo campo
090800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C* X i campi successivi al 1°
091300030715     C                   ELSE
091400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091500030715     C                   EVAL      posDa = posA + 1
091600030715     C* Eseguo lo scan x trovare la fine del campo corrente
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* Gestisco l'ultimo campo
091900030715     C                   IF        posA = *zeros
092000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C                   LEAVE
092500030715     C                   ELSE
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C                   ENDIF
093000030715     C                   ENDIF
093100030715     C* Incremento il contatore d campo
093200030715     C                   EVAL      i = i +1
093300030715     C                   ENDDO
093400030715     C*
093500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093600030715     C* delimitazione testo
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
094000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
094100030715     C                   EVAL      i = i + 1
094200030715     C                   ENDDO
094300030715     C                   EVAL      wGiro = 1
094400040709     C* Ripristino la 1° riga originale
094500040709     C                   EVAL      vinDTA = depvinDTA
094600030715     C                   ELSE
094700030715     C                   EVAL      wGiro = 2
094800031204     C**********
094900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
095000031204     C* d separatore campo
095100031204     C**********
095200040709     C                   EVAL      vindta = %trim(vindta)
095300040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
095400040709     C                   EVAL      vindta = %subst(vindta:2)
095500040709     C                   ENDDO
095600040709     C*
095700040709     C                   Z-ADD     *zeros        lunghInput        4 0
095800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
095900040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
096000040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
096100040709     C                   EVAL      lunghInput = %len(%trim(vindta))
096200040709     C                   ENDDO
096300030715     C**********
096400030715     C* Effettuo lo split del campo dati d input
096500030715     C**********
096600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096700040324     C                   DOW       posDa <= %len(%trimr(vindta))
096800030715     C*
096900030715     C* Gestisco il 1° campo
097000030715     C                   IF        i = 1
097100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
097200030715     C                   EVAL      posDa = 1
097300030715     C* Eseguo lo scan x trovare la fine del primo campo
097400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097500030715     C* A questo punto "estraggo" il campo corrente
097600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097700030715     C                                                    (posA-posDa))
097800030715     C* X i campi successivi al 1°
097900030715     C                   ELSE
098000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
098100030715     C                   EVAL      posDa = posA + 1
098200031204     C* Verifico che nn vi sia il campo nn valorizzato
098300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
098400031204     C* Se campo nn valorizzato skippo al prossimo
098500031204     C                   EVAL      SkSplitCSV(i) = *blanks
098600031204     C                   EVAL      posA  = posA + 1
098700031204     C                   ELSE
098800030715     C* Eseguo lo scan x trovare la fine del campo corrente
098900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099000030715     C* Gestisco l'ultimo campo
099100030715     C                   IF        posA = *zeros
099200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
099300030715     C* A questo punto "estraggo" il campo corrente
099400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099500030715     C                                                    (posA-posDa))
099600030715     C                   LEAVE
099700030715     C                   ELSE
099800030715     C* A questo punto "estraggo" il campo corrente
099900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100000030715     C                                                    (posA-posDa))
100100030715     C                   ENDIF
100200031204     C                   ENDIF
100300030715     C                   ENDIF
100400030715     C* Incremento il contatore d campo
100500030715     C                   EVAL      i = i +1
100600030715     C                   ENDDO
100700030715     C*
100800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100900030715     C* delimitazione testo
101000030715     C                   EVAL      i = 1
101100030715     C                   DOW       i < %elem(SkSplitCSV)
101200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
101300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
101400030715     C                   EVAL      i = i + 1
101500030715     C                   ENDDO
101600030715     C*
101700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101800030715     C                   EVAL      i = 1
101900030715     C                   DOW       i < %elem(SkSplitFLD)
102000030715     C*
102100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102300030715     C                             SkSplitCSV(i) <> *blanks
102400030721     C*** ATB
102500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
102600040708     C                   EVAL      VAOATB = SkSplitCSV(i)
102700030721     C                   ENDIF
102800040708     C*** TOR
102900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
103000040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
103100040708     C                   ENDIF
103200040708     C*** TCO
103300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
103400040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
103500040708     C                   ENDIF
103600040708     C*** RSO
103700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
103800040708     C                   EVAL      VAORSO = SkSplitCSV(i)
103900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
104000040708     C     '@':'A'       XLATE     VAORSO        VAORSO
104100030715     C* ==
104200030715     C                   ENDIF
104300040708     C*** INO
104400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
104500040708     C                   EVAL      VAOINO = SkSplitCSV(i)
104600030715     C                   ENDIF
104700040708     C*** LOO
104800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
104900040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
105000030715     C                   ENDIF
105100040708     C*** PRO
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
105300040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
105400030715     C                   ENDIF
105500040708     C*** NAO
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
105700040708     C                   EVAL      VAONAO = SkSplitCSV(i)
105800030721     C                   ENDIF
105900040708     C*** RSR
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
106100040708     C                   EVAL      VAORSR = SkSplitCSV(i)
106200040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
106300040708     C     '@':'A'       XLATE     VAORSR        VAORSR
106400040708     C* ==
106500040708     C                   IF        VAORSR = *blanks
106600040708     C                   SETON                                        31
106700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106800040708     C                             + ' ' + 'VAORSR'
106900040708     C                   ENDIF
107000040708     C                   ENDIF
107100040708     C*** INR
107200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
107300040708     C                   EVAL      VAOINR = SkSplitCSV(i)
107400040708     C                   IF        VAOINR = *blanks
107500040708     C                   SETON                                        31
107600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
107700040708     C                             + ' ' + 'VAOINR'
107800040708     C                   ENDIF
107900040708     C                   ENDIF
108000040708     C*** LOR
108100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
108200040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
108300040708     C                   IF        VAOLOR = *blanks
108400040708     C                   SETON                                        31
108500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040708     C                             + ' ' + 'VAOLOR'
108700040708     C                   ENDIF
108800040708     C                   ENDIF
108900040708     C*** PRR
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
109100040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
109200040708     C                   IF        VAOPRR = *blanks
109300040708     C                   SETON                                        31
109400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040708     C                             + ' ' + 'VAOPRR'
109600040708     C                   ENDIF
109700040708     C                   ENDIF
109800040708     C*** NAR
109900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
110000040708     C                   EVAL      VAONAR = SkSplitCSV(i)
110100040708     C                   ENDIF
110200040708     C*** RER
110300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
110400040708     C                   EVAL      VAORER = SkSplitCSV(i)
110500040708     C                   ENDIF
110600040708     C*** TER
110700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
110800040708     C                   EVAL      VAOTER = SkSplitCSV(i)
110900040708     C                   ENDIF
111000040708     C*** RSC
111100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
111200040708     C                   EVAL      VAORSC = SkSplitCSV(i)
111300040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
111400040708     C     '@':'A'       XLATE     VAORSC        VAORSC
111500040708     C* ==
111600040708     C                   ENDIF
111700040708     C*** INC
111800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
111900040708     C                   EVAL      VAOINC = SkSplitCSV(i)
112000040708     C                   ENDIF
112100040708     C*** LOC
112200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
112300040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
112400040708     C                   ENDIF
112500040708     C*** PRC
112600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
112700040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
112800040708     C                   ENDIF
112900040708     C*** NAC
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
113100040708     C                   EVAL      VAONAC = SkSplitCSV(i)
113200040708     C                   ENDIF
113300040708     C*** FFD
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
113500040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
113600040708     C                   ENDIF
113700040708     C*** RMP
113800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
113900040708     C                   EVAL      VAORMP = SkSplitCSV(i)
114000040708     C                   ENDIF
114100040708     C*** NAM
114200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
114300040708     C                   EVAL      VAONAM = SkSplitCSV(i)
114400040708     C                   ENDIF
114500040708     C*** PAG
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
114700040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
114800040708     C                   ENDIF
114900040708     C*** CTR
115000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
115100040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
115200040708     C                   ENDIF
115300040708     C*** ZOR
115400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
115500040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
115600040708     C                   ENDIF
115700040708     C*** NO1
115800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
115900040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
116000030715     C                   ENDIF
116100040708     C*** NO2
116200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
116300040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
116400030715     C                   ENDIF
116500040708     C*** DDT
116600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
116700040708     C                   EVAL      VAODDT = SkSplitCSV(i)
116800030715     C                   ENDIF
116900040708     C*** RFA
117000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
117100040708     C                   EVAL      VAORFA = SkSplitCSV(i)
117200030715     C                   ENDIF
117300040708     C*** SPI
117400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
117500040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
117600030715     C                   ENDIF
117700040708     C*** FLO
117800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
117900040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
118000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
118100080708     C* e la posizione 7 del VAOFLO
118200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
118300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
118400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
118500030715     C                   ENDIF
118600050314
118700050412      *** DAR
118800050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
118900050412     C                   EVAL      PiStr=SkSplitCSV(i)
119000050412     C                   EXSR      CHKNUM
119100050412     C                   IF        PiInt=*on
119200050412     C                   Z-ADD     PiVal         VAODAR
119300050412     C                   ELSE
119400050412     C                   SETON                                        32
119500050412     C                   EVAL      VAODAR = *zeros
119600050412     C                   EVAL      vinmsg = %trimr(vinmsg)
119700050412     C                             + ' ' + 'VAODAR'
119800050412     C                   ENDIF
119900050314     C                   ENDIF
120000030715     C
120100030721     C*
120200030721     C* Reperisco quindi i campi numerici...
120300040708     C*** POE
120400040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
120500040723     C                   EVAL      PiStr=SkSplitCSV(i)
120600040723     C                   EXSR      CHKNUM
120700040723     C                   IF        PiInt=*on    AND
120800040723     C                             PiVal<=999   AND
120900040723     C                             PiVal>*zeros
121000040723     C                   Z-ADD     PiVal         VAOPOE
121100040723     C                   ELSE
121200040723     C                   SETON                                        32
121300040723     C                   EVAL      VAOPOE = *zeros
121400040723     C                   EVAL      vinmsg = %trimr(vinmsg)
121500040723     C                             + ' ' + 'VAOPOE'
121600040723     C                   ENDIF
121700040723     C                   ENDIF
121800040708     C*** COR
121900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
122000040708     C                   EVAL      PiStr=SkSplitCSV(i)
122100040708     C                   EXSR      CHKNUM
122200040708     C                   IF        PiInt=*on
122300040708     C                   Z-ADD     PiVal         VAOCOR
122400040708     C                   ELSE
122500040708     C                   SETON                                        31
122600040708     C                   EVAL      VAOCOR = *zeros
122700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122800040708     C                             + ' ' + 'VAOCOR'
122900040708     C                   ENDIF
123000040708     C                   ENDIF
123100040708     C*** CAO
123200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on
123600030721     C                   Z-ADD     PiVal         Num5_0
123700040708     C                   MOVEL(p)  Num5_0        VAOCAO
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000040708     C                   EVAL      VAOCAO = *zeros
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200040708     C                             + ' ' + 'VAOCAO'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500040708     C*** CRA
124600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on
125000040708     C                   Z-ADD     PiVal         VAOCRA
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300040708     C                   EVAL      VAOCRA = *zeros
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500040708     C                             + ' ' + 'VAOCRA'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800040708     C*** CAR
125900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
126000040708     C                   EVAL      PiStr=SkSplitCSV(i)
126100040708     C                   EXSR      CHKNUM
126200040708     C                   IF        PiInt=*on
126300040708     C                   Z-ADD     PiVal         Num5_0
126400040708     C                   MOVEL(p)  Num5_0        VAOCAR
126500040708     C                   ELSE
126600040708     C                   SETON                                        31
126700040708     C                   EVAL      VAOCAR = *zeros
126800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126900040708     C                             + ' ' + 'VAOCAR'
127000040708     C                   ENDIF
127100040708     C                   ENDIF
127200040708     C*** CRC
127300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
127400040708     C                   EVAL      PiStr=SkSplitCSV(i)
127500040708     C                   EXSR      CHKNUM
127600040708     C                   IF        PiInt=*on
127700040708     C                   Z-ADD     PiVal         VAOCRC
127800040708     C                   ELSE
127900040708     C                   SETON                                        32
128000040708     C                   EVAL      VAOCRC = *zeros
128100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128200040708     C                             + ' ' + 'VAOCRC'
128300040708     C                   ENDIF
128400040708     C                   ENDIF
128500040708     C*** CAC
128600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
128700040708     C                   EVAL      PiStr=SkSplitCSV(i)
128800040708     C                   EXSR      CHKNUM
128900040708     C                   IF        PiInt=*on
129000040708     C                   Z-ADD     PiVal         Num5_0
129100040708     C                   MOVEL(p)  Num5_0        VAOCAC
129200040708     C                   ELSE
129300040708     C                   SETON                                        32
129400040708     C                   EVAL      VAOCAC = *zeros
129500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129600040708     C                             + ' ' + 'VAOCAC'
129700040708     C                   ENDIF
129800040708     C                   ENDIF
129900040708     C*** DAR
130000050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
130100050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
130200050314     C**!!!              EXSR      CHKNUM
130300050314     C**!!!              IF        PiInt=*on
130400050314     C**!!!              Z-ADD     PiVal         VAODAR
130500050314     C**!!!              ELSE
130600050314     C**!!!              SETON                                        32
130700050314     C**!!!              EVAL      VAODAR = *zeros
130800050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
130900050314     C**!!!                        + ' ' + 'VAODAR'
131000050314     C**!!!              ENDIF
131100050314     C**!!!              ENDIF
131200040708     C*** ORR
131300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
131400040708     C                   EVAL      PiStr=SkSplitCSV(i)
131500040708     C                   EXSR      CHKNUM
131600040708     C                   IF        PiInt=*on
131700040708     C                   Z-ADD     PiVal         VAOORR
131800040708     C                   ELSE
131900040708     C                   SETON                                        32
132000040708     C                   EVAL      VAOORR = *zeros
132100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132200040708     C                             + ' ' + 'VAOORR'
132300040708     C                   ENDIF
132400040708     C                   ENDIF
132500030721     C*** NCL
132600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on
133000040723     C                   IF        PiVal > *zeros
133100040708     C                   Z-ADD     PiVal         VAONCL
133200040723     C                   ENDIF
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500040708     C                   EVAL      VAONCL = *zeros
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700040708     C                             + ' ' + 'VAONCL'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000040708     C*** PKG
134100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiNum=*on
134500040723     C                   IF        PiVal > *zeros
134600040708     C                   Z-ADD     PiVal         VAOPKG
134700040723     C                   ENDIF
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000040708     C                   EVAL      VAOPKG = *zeros
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200040708     C                             + ' ' + 'VAOPKG'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500040708     C*** VLM
135600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
135700040708     C                   EVAL      PiStr=SkSplitCSV(i)
135800040708     C                   EXSR      CHKNUM
135900040708     C                   IF        PiNum=*on
136000040708     C                   Z-ADD     PiVal         VAOVLM
136100040708     C                   ELSE
136200040708     C                   SETON                                        32
136300040708     C                   EVAL      VAOVLM = *zeros
136400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136500040708     C                             + ' ' + 'VAOVLM'
136600040708     C                   ENDIF
136700040708     C                   ENDIF
136800040708     C*** BNC
136900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
137000040708     C                   EVAL      PiStr=SkSplitCSV(i)
137100040708     C                   EXSR      CHKNUM
137200040708     C                   IF        PiInt=*on
137300040708     C                   Z-ADD     PiVal         VAOBNC
137400040708     C                   ELSE
137500040708     C                   SETON                                        32
137600040708     C                   EVAL      VAOBNC = *zeros
137700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137800040708     C                             + ' ' + 'VAOBNC'
137900040708     C                   ENDIF
138000040708     C                   ENDIF
138100040708     C*** BLC
138200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
138300040708     C                   EVAL      PiStr=SkSplitCSV(i)
138400040708     C                   EXSR      CHKNUM
138500040708     C                   IF        PiInt=*on
138600040708     C                   Z-ADD     PiVal         VAOBLC
138700040708     C                   ELSE
138800040708     C                   SETON                                        32
138900040708     C                   EVAL      VAOBLC = *zeros
139000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139100040708     C                             + ' ' + 'VAOBLC'
139200040708     C                   ENDIF
139300040708     C                   ENDIF
139400040708     C*** ATT
139500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
139600040708     C                   EVAL      PiStr=SkSplitCSV(i)
139700040708     C                   EXSR      CHKNUM
139800040708     C                   IF        PiInt=*on
139900040708     C                   Z-ADD     PiVal         VAOATT
140000040708     C                   ELSE
140100040708     C                   SETON                                        32
140200040708     C                   EVAL      VAOATT = *zeros
140300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140400040708     C                             + ' ' + 'VAOATT'
140500040708     C                   ENDIF
140600040708     C                   ENDIF
140700040708     C*** MTC
140800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
140900040708     C                   EVAL      PiStr=SkSplitCSV(i)
141000040708     C                   EXSR      CHKNUM
141100040708     C                   IF        PiInt=*on
141200040708     C                   Z-ADD     PiVal         VAOMTC
141300040708     C                   ELSE
141400040708     C                   SETON                                        32
141500040708     C                   EVAL      VAOMTC = *zeros
141600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141700040708     C                             + ' ' + 'VAOMTC'
141800040708     C                   ENDIF
141900040708     C                   ENDIF
142000040708     C*** KSC
142100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
142200030721     C                   EVAL      PiStr=SkSplitCSV(i)
142300030721     C                   EXSR      CHKNUM
142400030721     C                   IF        PiInt=*on      AND
142500030721     C                             PiVal<=9999999 AND
142600030721     C                             PiVal>*zeros
142700040708     C                   Z-ADD     PiVal         VAOKSC
142800030721     C                   ELSE
142900030721     C                   SETON                                        32
143000040708     C                   EVAL      VAOKSC = *zeros
143100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143200040708     C                             + ' ' + 'VAOKSC'
143300030721     C                   ENDIF
143400030721     C                   ENDIF
143500040708     C*** POR
143600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
143700030721     C                   EVAL      PiStr=SkSplitCSV(i)
143800030721     C                   EXSR      CHKNUM
143900030721     C                   IF        PiInt=*on    AND
144000030721     C                             PiVal<=999   AND
144100030721     C                             PiVal>*zeros
144200040708     C                   Z-ADD     PiVal         VAOPOR
144300030721     C                   ELSE
144400030721     C                   SETON                                        32
144500040708     C                   EVAL      VAOPOR = *zeros
144600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144700040708     C                             + ' ' + 'VAOPOR'
144800030721     C                   ENDIF
144900030721     C                   ENDIF
145000040708     C*** POC
145100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
145200040708     C                   EVAL      PiStr=SkSplitCSV(i)
145300040708     C                   EXSR      CHKNUM
145400040708     C                   IF        PiInt=*on    AND
145500040708     C                             PiVal<=999   AND
145600040708     C                             PiVal>*zeros
145700040708     C                   Z-ADD     PiVal         VAOPOC
145800040708     C                   ELSE
145900040708     C                   SETON                                        32
146000040708     C                   EVAL      VAOPOC = *zeros
146100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146200040708     C                             + ' ' + 'VAOPOC'
146300040708     C                   ENDIF
146400040708     C                   ENDIF
146500040708     C*** STO
146600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
146700040708     C                   EVAL      PiStr=SkSplitCSV(i)
146800040708     C                   EXSR      CHKNUM
146900040708     C                   IF        PiInt=*on
147000040708     C                   Z-ADD     PiVal         VAOSTO
147100040708     C                   ELSE
147200040708     C                   SETON                                        32
147300040708     C                   EVAL      VAOSTO = *zeros
147400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147500040708     C                             + ' ' + 'VAOSTO'
147600040708     C                   ENDIF
147700040708     C                   ENDIF
147800110526     C*
147900110526     C*********  Gestione particolarità
148000110526     C*
148100110616     C*** DAR
148200110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
148300110630     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
148400110630     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
148500110630     C                   ENDIF
148600110616     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
148700110616     C                                   %subst(SkSplitCSV(i):3:2)+
148800110616     C                                   %subst(SkSplitCSV(i):1:2)
148900110616     C                   EXSR      CHKNUM
149000110616     C                   IF        PiInt=*on
149100110616     C                   Z-ADD     PiVal         VAODAR
149200110616     C                   ELSE
149300110616     C                   SETON                                        32
149400110616     C                   EVAL      VAODAR = *zeros
149500110616     C                   EVAL      vinmsg = %trimr(vinmsg)
149600110616     C                             + ' ' + 'VAODAR'
149700110616     C                   ENDIF
149800110616     C                   ENDIF
149900110616     C*** ORR
150000110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOORR'
150100110630     C***                EVAL      PiStr=%subst(SkSplitCSV(i):1:2)+
150200110630     C***                                %subst(SkSplitCSV(i):4:2)
150300110630     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
150400110616     C                   EXSR      CHKNUM
150500110616     C                   IF        PiInt=*on
150600110616     C                   Z-ADD     PiVal         VAOORR
150700110616     C                   ELSE
150800110616     C                   SETON                                        32
150900110616     C                   EVAL      VAOORR = *zeros
151000110616     C                   EVAL      vinmsg = %trimr(vinmsg)
151100110616     C                             + ' ' + 'VAOORR'
151200110616     C                   ENDIF
151300110616     C                   ENDIF
151400120605     C*
151500120605     C*** RER 1 e 2
151600120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORER_1'
151700120605     C                   EVAL      wRER_1=%trim(SkSplitCSV(i))
151800120605     C                   ENDIF
151900120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORER_2'
152000120605     C                   EVAL      wRER_2=%trim(SkSplitCSV(i))
152100120605     C                   ENDIF
152200110526     C*
152300110526     C*********
152400030715     C*
152500030715     C                   ENDIF
152600030715     C                   EVAL      i = i + 1
152700030715     C                   ENDDO
152800120605     C*
152900120605     C***  ===>  Gestione campi "particolari" fuori ciclo
153000120605     C*
153100120605     C                   EVAL      VAORER=%trim(wRER_1) + %trim(wRER_2)
153200040723     C*
153300030715     C                   ENDIF
153400020204     C*
153500010607     C                   ENDSR
153600010607     C*----------------------------------------------------*
153700010330
153800100512
153900100512
154000100512     C*------------------------------------------------------------------------*
154100100512     C* EXEFORZ - Considerazioni finali
154200100512     C*------------------------------------------------------------------------*
154300100512     C     EXEFORZ       BEGSR
154400100512     C*
154500100512     C                   if        VAOKSC = *zeros
154600100512     C                   select
154700100512     C                   when      VAOPAG = 'O'
154800100512     C                   movel     VAOCOR        VAOKSC
154900100512     C                   when      VAOPAG = 'M'
155000100512     C                   movel     VAOCRA        VAOKSC
155100100512     C                   when      VAOPAG = 'D'
155200100512     C                   movel     VAOCRC        VAOKSC
155300100512     C                   endsl
155400100512     C                   endif
155500100512     C*
155600100512     C                   ENDSR
155700080702
155800080702
155900080702     C*----------------------------------------------------*
156000080702     C*  CONTROLLO NUMERICITA' CAMPI
156100080702     C*----------------------------------------------------*
156200080702     C     CHKNUM        BEGSR
156300080702     C*
156400080702     C                   IF        PiDecChr = *blanks
156500080702     C                   EVAL      PiDecChr = ','
156600080702     C                   ENDIF
156700080702     C*
156800080702     C                   callp     UBISNUM_Check(PiStr
156900080702     C                                          :PiDecChr
157000080702     C                                          :PiVal
157100080702     C                                          :PiNum
157200080702     C                                          :PiInt)
157300080702     C*
157400080702     C                   ENDSR
157500080702     C***
157600010601
157700080702
157800010601
157900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158000010601     C     repfil        BEGSR
158100010601     C*
158200010601     C                   if        invfil = *zeros and
158300010601     C                             depfil > *zeros and
158400010629     C                             (vinflg = *blanks or
158500010629     C                              vinflg = *zeros)
158600010601     C
158700010601     C                   eval      invfil = depfil
158800010601     C                   endif
158900010601     C*
159000010601     C                   if        depfil <> invfil and
159100010601     C                             invfil > *zeros
159200010601     C                   eval      flgMulti = '1'
159300010601     C                   if        vinflg = *blanks
159400010601     C                   add       1             cntNonEl
159500010601     C                   endif
159600010601     C                   endif
159700010601     C*
159800010601     C                   if        vinflg = '2'
159900010601     C                   eval      flgStato = '2'
160000010601     C                   endif
160100010601     C*
160200010601     C                   ENDSR
160300010601     C***
160400010601
160500010601
160600010601
160700010330
160800010330
160900010330
161000990920      /TITLE Invio dei dati al punto operativo.
161100000613     C     invio         BEGSR
161200990920     C*
161300990920     C                   reset                   dscmz
161400010601     C                   move      invfil        cmzdst
161500990920     C                   eval      cmzfld = vlrfou
161600990920     C                   eval      cmzmbd = vlrhdl
161700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
161800000710     C                   if        prmfir = *blanks
161900040708     C                   eval      cmzfla = 'FNVAO00F'
162000040708     C                   eval      cmzmba = 'FNVAO00F'
162100000710     C                   else
162200000710     C                   eval      cmzfla = prmfir
162300000710     C                   eval      cmzmba = prmfir
162400000710     C                   endif
162500990920     C                   eval      cmznrr = *zeros
162600990920     C                   move      §ctrok        cmznrr
162700021018     C                   eval      cmzlba = vlrfl1
162800990920     C                   call(e)   'TIS711C'
162900990920     C                   parm                    dscmz
163000990921     C                   parm      *blanks       esito
163100990920     C                   if        %error
163200990920     C                             or cmzerr = '1'
163300990921     C                             or esito  = '1'
163400000710     C                   eval      wrkesito = '3'
163500990920     C                   endif
163600990920     C*
163700000613     C                   ENDSR
163800990910
163900010601
164000010601
164100010601
164200010601
164300010601      /TITLE Invio dei dati al punto operativo.
164400010601     C     opeini        BEGSR
164500010601     C*
164600010601     C* Inizializzo flag e contatori operativi
164700010601     C                   movel     '0'           flgGiro           1
164800010601     C                   movel     '0'           flgMulti          1
164900010601     C                   movel     '1'           flgStato          1
165000010615     C                   movel     '0'           flgOk             1
165100010601     C                   z-add     *zeros        cntNonEl         10 0
165200010601     C                   z-add     *zeros        depfil            3 0
165300010601     C                   z-add     *zeros        invfil            3 0
165400010601     C*
165500010601     C                   ENDSR
165600010601     C***
165700010601
165800010601
165900050421
166000050421
166100050421     C     *pssr         BEGSR
166200050421     C*
166300050421     C                   eval      wrkesito = '2'
166400050421     C*
166500050421     C                   ENDSR     '*CANCL'
166600050421     C***
166700060306
166800060306
166900060306     C*-------------------------------------------------------------------------*
167000060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
167100060306     C*-------------------------------------------------------------------------*
167200060306     C     sr_Control    begsr
167300060306     C*
167400060306     C* inizializzo a ok il flag d controllo abilitazione
167500060306     C                   eval      Cli_Abil = *on
167600060306     C
167700060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
167800060306     C                   clear                   woriIDC
167900060306     C                   eval      kteCOD = 'VAP'
168000060306     C                   eval      kteKE1 = oriIDC
168100060306     C     keytbe        chain     TNTBE01L
168200060306     C                   if        %found(Tntbe01l)
168300060306     C                   eval      woriIDC = TbeKe2
168400060306     C                   else
168500060306     C                   eval      Cli_Abil = *off
168600060306     C                   endif
168700060306     C*
168800060306     C                   endsr
168900050421
169000010601
169100010330
169200010330
169300000613     C     *inzsr        BEGSR
169400990910     C*
169500990910     C     *entry        plist
169600990920     C                   parm                    tivlrds
169700990921     C                   parm      wrkesito      esito
169800000724     C                   parm                    prmlit
169900000710     C                   parm                    prmfir
170000010330     C*
170100010330     C* CALCOLA LA DATA CORRENTE
170200100223     C                   time                    wn14             14 0
170300100223     C                   movel     wn14          wn6               6 0          *ORA (6)
170400100223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
170500100223     C                   eval      datcor = %dec(%date() : *ISO)
170600040723     C*
170700040723     C* Campi riferito al database
170800040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
170900040723     C     *like         define    tbeke1        kteKE1
171000040723     C*
171100040723     C* Definizione chiavi di lettura
171200040723     C* TNTBE01L - parziale
171300040723     C     keytbe        klist
171400040723     C                   kfld                    kteCOD                         *UTENTE
171500040723     C                   kfld                    kteKE1                         *CODICE
171600000613     C*
171700000613     C                   ENDSR
171800000613     C***
171900990908
172000080702     Otitv1h6p  E            riepilogo         2
172100990915     O                                              'Upload via Internet'
172200990915     O                                           +1 'Traduzione TIVIN00R -'
172300040708     O                                           +1 'FNVAOWWR'
172400990915     O                                           +1 'Testate bolle'
172500990915     O          E            riepilogo   2
172600990915     O                       wrkdata
172700990915     O                       wrkora              +1
172800990915     O                       procname            +1
172900990915     O          E            riepilogo   2
173000990915     O                                              'Cliente..................:'
173100040708     O                       VAOCOR        z     +1
173200990915     O          E            riepilogo   2
173300990920     O                                              'Riferimento Strategi.....:'
173400990920     O                       vlrhdl              +1
173500990915     O          E            riepilogo   2
173600990915     O                                              'Giusti...................:'
173700971022     O                       §CTROK        2   +  1
173800990915     O          E            riepilogo   2
173900990915     O                                              'Sbagliati e corretti.....:'
174000971022     O                       §CTRMO        2   +  1
174100990915     O          E            riepilogo   2
174200990915     O                                              'Sbagliati e scartati.....:'
174300971022     O                       §CTRNO        2   +  1
174400000613
174500080702     Otitv1h6ps E            testdett          2
174600000613     O                                              'Upload via Internet'
174700000613     O                                           +1 'Traduzione TIVIN00R -'
174800040708     O                                           +1 'FNVAOWWR'
174900000613     O                                           +1 'Testate bolle'
175000000616     O          E            testdett    3
175100000613     O                                           +2 'N° rec'
175200000613     O                                           +3 'Anteprima contenuto'
175300000616     O          E            rigadett    2
175400000613     O                       rrnum               +2
175500000621     O                       recko               +3
175600000616     O          E            findett     2
175700000613     O                       wrkdata
175800000613     O                       wrkora              +1
175900000613     O                       procname            +1
176000000616     O          E            findett     2
176100000613     O                                              'Cliente..................:'
176200040708     O                       VAOCOR        z     +1
176300000616     O          E            findett     2
176400000613     O                                              'Riferimento Strategi.....:'
176500000613     O                       vlrhdl              +1
176600000616     O          E            findett     2
176700000613     O                                              'Giusti...................:'
176800000613     O                       §CTROK        2   +  1
176900000616     O          E            findett     2
177000000613     O                                              'Sbagliati e corretti.....:'
177100000613     O                       §CTRMO        2   +  1
177200000616     O          E            findett     2
177300000613     O                                              'Sbagliati e scartati.....:'
177400000613     O                       §CTRNO        2   +  1
177500000613** CMD - COMANDI CL
177600080702OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
177700080702OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
177800080702DLTOVR FILE(TITV1H6P TITV1H6PS) LVL(*)
177900010606
178000000613
178100040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178200121026FILL01;FILL02;VAORSR;FILL03;VAOINR;VAOLOR;VAOCAR;VAOPRR;FILL04;VAOTER;FILLXX;
178300121026FILLXX;VAORFA;FILLXX;VAONCL;W_VAODAR;W_VAOORR;FILLXX;VAORSC;FILLXX;VAOINC;
178400121026VAOLOC;VAOCAC;VAOPRC;FILXXX;FILLXX;FILLXX;FILXX;VAONO1;
178500111109
178600111109
