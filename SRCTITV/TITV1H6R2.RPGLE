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
010200080702
010300031201     D*------------------
010400031201     D* Costanti
010500031201     D*------------------
010600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010800010330
010900010330
011000010330
011100990915     C                   time                    wrkdata
011200990915     C                   time                    wrkora
011300000913     C                   reset                   rrnum
011400990921     C                   reset                   esito
011500990921     C                   reset                   wrkesito
011600010601     C*
011700010601     C                   exsr      opeini
011800041011     C                   exsr      PREELA
011900040708     C                   exsr      rwvao
012000010601     C*
012100010601     C                   seton                                        lr
012200010601
012300010601
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012800010601     C*--------------------------------------------------------
012900010601     C     PREELA        BEGSR
013000010601     C*
013100010601     C* SE OCCORRE SPEDIRE IN FILIALE
013200041011     C                   if        flgGiro = '0'
013300010601     C*
013400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013500010601     C                   eval      flgGiro = '1'
013600000724     C*
013700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013900041011     C     vlrpoi        chain     azorg01l
014000000724     C                   if        %found
014100000724     C                   movel(p)  CMD(1)        depcmd
014200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014300000724     C*
014400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000724     C                   PARM                    depcmd
014800000724     C                   PARM                    LENGH
014900000724     C*
015000041011     C                   else
015100041011     C                   setoff                                       50
015200000724     C                   endif
015300000616     C*
015400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015500000616     C                   movel(p)  CMD(2)        depcmd
015600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015700000616     C*
015800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015900011113     C                   Z-ADD     150           LENGH            15 5
016000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016100000616     C                   PARM                    depcmd
016200000616     C                   PARM                    LENGH
016300041011     C*
016400041011     C                   endif
016500000613     C*
016600080702     C                   if        not %open(titv1h6ps)
016700080702     C                   open      titv1h6ps
016800030715     C                   except    testdett
016900000613     C                   endif
017000010601     C*
017100010601     C                   ENDSR
017200010601     C***
017300010601
017400010601
017500010601
017600010601
017700010601
017800010601
017900010601
018000010601     C*--------------------------------------------------------
018100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
018200010601     C*--------------------------------------------------------
018300010601     C     ENDELA        BEGSR
018400990915     C*
018500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018600000613     C*
018700080702     C                   if        %open(titv1h6ps)
018800000616     C                   except    findett
018900080702     C                   close     titv1h6ps
019000000613     C                   endif
019100000616     C*
019200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019300011113     C                   Z-ADD     150           LENGH            15 5
019400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019500000616     C                   PARM                    CMD(3)
019600000616     C                   PARM                    LENGH
019700080707     C*
019800080707     C* Chiusura TRUL13R
019900080707     C                   clear                   TRUL13DS
020000080707     C                   eval      I13TLA = 'C'
020100080707     C                   call      'TRUL13R'
020200080707     C                   parm                    TRUL13DS
020300000616     C*
020400010601     C                   ENDSR
020500010601     C***
020600010601
020700010601
020800010601
020900010601
021000000613
021100000613     C*--------------------------------------------------------
021200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021300000613     C*--------------------------------------------------------
021400000621     C     STPR          BEGSR
021500000613     C*
021600080702     C                   if        not %open(titv1h6p)
021700080702     C   50              open      titv1h6p
021800990915     C                   endif
021900990915     C*
022000030715     C   50              except    riepilogo
022100990915     C*
022200080702     C                   if        %open(titv1h6p)
022300080702     C   50              close     titv1h6p
022400990914     C                   endif
022500000613     C*
022600000613     C                   ENDSR
022700000613     C***
022800000613
022900010330
023000010601
023100010601
023200010601
023300010330     C*--------------------------------------------------------
023400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
023500010330     C*--------------------------------------------------------
023600040708     C     RWVAO         BEGSR
023700010330     C*
023800010330     C                   if        not %open(tivin00r)
023900010330     C                   open      tivin00r
024000010330     C                   endif
024100040708     C                   if        not %open(fnvaowwr)
024200040708     C                   open      fnvaowwr
024300010330     C                   endif
024400010330     C*
024500010604     C                   clear                   §CTROK
024600010604     C                   clear                   §CTRMO
024700010604     C                   clear                   §CTRNO
024800010330     C*
024900030822     C                   do        *HIVAL
025000010330     C*
025100030822     C                   read      tivin00r                               70
025200010618     C*
025300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025400010618     C                   if        vinflg = '1'
025500010618     C                   eval      flgOk = '1'
025600010618     C                   endif
025700010618     C*
025800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025900040510     C                   z-add     *zeros        wLenVINDTA        4 0
026000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026400040510     C                   z-add     1             wFlgVINDTA
026500040510     C                   leave
026600040510     C                   endif
026700040510     C                   enddo
026800040510     C*
026900040218     C                   if        vindta > *blanks AND
027000040510     C                             wFlgVINDTA = 1
027100010330     C                   add       1             rrnum
027200010330     C*
027300010601     C                   if        *in70 = *off and
027400010330     C                             (vinflg = *blanks
027500010330     C                              or vinflg = '0'
027600010330     C                              or vinflg = '2')
027700010330     C*
027800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027900010711     C                   if        vinflg = *blanks or vinflg = '0'
028000010711     C                   clear                   vinmsg
028100010711     C                   endif
028200010601     C*
028300010330     C                   exsr      inzvar
028400040708     C                   exsr      impvao
028500100512     C                   exsr      exeforz
028600010601     C*
028700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028800040708     C                   eval      depfil = VAOPOE
028900010601     C                   exsr      repfil
029000010601     C                   if        depfil = invfil
029100021025     C                   if        vlrpoi = 999
029200040708     C                   move(P)   invfil        VAOPOE
029300021025     C                   else
029400040708     C                   move(P)   vlrpoi        VAOPOE
029500021025     C                   endif
029600010601     C*
029700010604     C* Ebbene...
029800010604     C*
029900030822     C  N31              add       1             §CTROK            7 0
030000030822     C   32              add       1             §CTRMO            7 0
030100030822     C   31              add       1             §CTRNO            7 0
030200030822     C                   if        wGiro = 2
030300040723     C  N31              EXSR      CHKCOR
030400040723     C  N31              EXSR      WRIORI
030500050826     C  N31              EXSR      WRIVGD
030600040723     C  N31              write     FNVAO000
030700030822     C                   endif
030800010604     C*
030900010604     C                   if        *in31 = *off and
031000010604     C                             *in32 = *off
031100010604     C                   eval      vinflg = '1'
031200010604     C                   else
031300010604     C                   eval      recko = vindta
031400041011     C                   except    rigadett
031500010604     C                   eval      vinflg = '2'
031600010604     C                   endif
031700010604     C                   endif
031800010601     C*
031900010604     C                   endif
032000010604     C*
032100010330     C                   else
032200010330     C                   eval      vinflg = '1'
032300010330     C                   endif
032400010601     C*
032500010601     C  N70              update    tivin000
032600010330     C*
032700030822     C  N70              enddo
032800010601     C
032900010601     C*
033000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033100010601     C                   if        cntNonEl = *zeros or
033200010601     C                             flgMulti = '0'
033300010330     C* Se non ci sono record con errori ...
033400010601     C                   if        §ctrno = 0 and
033500010604     C                             §ctrmo = 0 and
033600010601     C                             flgStato <> '2'
033700010330     C* ... restituisco esito OK.
033800010330     C                   eval      wrkesito = '0'
033900010330     C                   else
034000010330     C                   if        §ctrok > 0
034100010330     C                   eval      wrkesito = '1'
034200010330     C                   else
034300010615     C                   if        flgOk = '0'
034400010615     C                   eval      wrkesito = '2'
034500010615     C                   else
034600010615     C                   eval      wrkesito = '6'
034700010615     C                   endif
034800010330     C                   endif
034900010330     C                   endif
035000010601     C                   else
035100010601     C                   eval      wrkesito = '9'
035200010601     C                   endif
035300010330     C*
035400010330     C                   if        %open(tivin00r)
035500010330     C                   close     tivin00r
035600010330     C                   endif
035700040708     C                   if        %open(fnvaowwr)
035800040708     C                   close     fnvaowwr
035900010330     C                   endif
036000010601     C*
036100010601     C                   if        vlrpoi <> 999
036200010601     C                   eval      invfil = vlrpoi
036300010601     C                   endif
036400010330     C*
036500010330     C                   if        §ctrok > 0
036600010601     C                             and invfil > *zeros
036700010330     C                   exsr      invio
036800010330     C                   endif
036900010601     C*
037000010618     C                   if        flgGiro = '1'
037100010601     C                   exsr      endela
037200010618     C                   endif
037300010330     C*
037400010330     C                   ENDSR
037500010330     C***
037600040723
037700040723
037800040723     C*------------------------------------------------------------------------*
037900040723     C* CHKCOR - Considerazioni su VAOCOR
038000040723     C*------------------------------------------------------------------------*
038100040723     C     CHKCOR        BEGSR
038200040723     C*
038300040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
038400040723     C                   if        vaoTCO = 'F'
038500040723     C* Quindi verifico se il codice ordinante non è valorizzato
038600040723     C                   if        vaoCOR = *zeros  AND
038700040723     C                             vaoRSO = *blanks AND
038800040723     C                             vaoINO = *blanks AND
038900040723     C                             vaoCAO = *blanks AND
039000040723     C                             vaoLOO = *blanks AND
039100040723     C                             vaoPRO = *blanks AND
039200040723     C                             vaoNAO = *blanks
039300040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
039400040723     C                   movel     '0'           wFlgAcr           1
039500040723     C                   move      vlrKSC        wCli              7 0
039600040723     C                   move(p)   wCli          acrCRO
039700040723     C     acrCRO        setll     fnacr01l
039800040723     C                   if        %found(fnacr01l)
039900040723     C                   read      fnacr01l
040000040723     C                   dow       not %eof(fnacr01l)
040100040723     C                   movel     acrCRO        wCliAcr           7 0
040200040723     C                   if        wCliAcr = wCli
040300040723     C                   eval      vaoCOR = acrCRO
040400040723     C                   movel     '1'           wFlgAcr
040500040723     C                   leave
040600040723     C                   else
040700040723     C                   leave
040800040723     C                   endif
040900040723     C                   read      fnacr01l
041000040723     C                   enddo
041100040723     C                   endif
041200040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
041300040723     C* dati anagrafici dell'ordinante dal piano dei conti
041400040723     C                   if        wFlgAcr = '0'
041500040723     C                   clear                   BS69DS
041600040723     C                   clear                   ACODS
041700040723     C                   clear                   INDDS
041800040723     C                   clear                   CLPDS
041900040723     C                   clear                   CLSDS
042000040723     C                   move(P)   vlrKSC        I69KAC
042100040723     C                   move(P)   vlrKSC        I69KIN
042200040723     C                   call      'TIBS69R'
042300040723     C                   parm                    BS69DS
042400040723     C                   parm                    ACODS
042500040723     C                   parm                    INDDS
042600040723     C                   parm                    CLPDS
042700040723     C                   parm                    CLSDS
042800040723     C     O69ERR        ifne      '1'
042900040723     C                   eval      vaoRSO = ACORAG
043000040723     C                   eval      vaoINO = INDVIA
043100040723     C                   movel(P)  INDCAP        vaoCAO
043200040723     C                   eval      vaoLOO = INDCIT
043300040723     C                   eval      vaoPRO = INDPRV
043400040723     C                   eval      vaoNAO = INDSTA
043500040723     C                   endif
043600040723     C                   endif
043700040723     C                   endif
043800040723     C                   endif
043900040723     C*
044000040723     C                   ENDSR
044100040723
044200040723
044300040723
044400040723     C*------------------------------------------------------------------------*
044500040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
044600040723     C*------------------------------------------------------------------------*
044700040723     C     WRIORI        BEGSR
044800040723     C*
044900090617     C* Come prima cosa stacco un numeratore da AZNUM
045000090617     C                   clear                   trul33ds
045100090617     C                   eval      I33OPE = *zeros
045200090617     C                   eval      I33CNU = 600
045300090617     C                   eval      I33NUM = 1
045400090617     C                   movel     TRUL33DS      KPJBU
045500090617     C                   call      'TRUL33R'
045600090617     C                   parm                    KPJBA
045700090617     C                   movel     KPJBU         TRUL33DS
045800090617     C                   if        O33ERR = *zeros
045900090617     C                   z-add     O33NRF        §numori
046000090618     c                   else
046100090618     c                   eval      §numori = *all'9'
046200090617     C                   endif
046300040723     C*
046400040723     C* Quindi imposto i campi "extra-VAO"
046500040723     C                   if        vlrKSC = '0ORM0000'
046600040723     C*
046700040723     C                   if        vaoCOR > *zeros
046800040723     C                   movel     vaoCOR        wrkCOR7           7 0
046900040723     C*
047000040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
047100040723     C* codice padre. Se è un padre assegno già il codice ordinante.
047200040723     C                   clear                   tibs10ds
047300040723     C                   eval      d10drf = datcor                              *data riferimento
047400040723     C                   eval      d10paf = 'P'                                 *cerca il padre
047500040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
047600091026     C*
047700091026     C* Preparazione chiamata al TIBS10R1
047800091026     C                   clear                   tibs10r1ds
047900091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
048000091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
048100091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
048200091026     C*
048300091026     C                   call      'TIBS10R1'
048400091026     C                   parm                    tibs10r1ds
048500091026     C*
048600091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
048700040723     C*
048800040723     C                   if        d10err <> *blanks or                         *No unif. o padre
048900040723     C                             d10cop =  *zeros  or
049000040723     C                             d10cop =  d10cod
049100040723     C                   move      d10cod        oriIDC
049200040723     C                   else                                                   *Figlio
049300040723     C                   move      d10cop        oriIDC
049400040723     C                   endif
049500040723     C*
049600040723     C                   else
049700040723     C                   eval      oriIDC = *zeros
049800040723     C                   endif
049900040723     C*
050000040723     C                   else
050100040723     C                   eval      oriIDC = vlrKSC
050200040723     C                   endif
050300040723     C*
050400040723     C                   eval      oriPRG = §numori
050500040723     C                   eval      oriDER = datcor
050600040723     C                   eval      oriOER = wn6
050700040723     C                   eval      oriPOE = vaoPOE
050800040723     C                   eval      oriNSR = vaoNSR
050900040723     C                   eval      oriNOR = vaoNOR
051000040723     C                   eval      oriNRV = vaoNRV
051100040723     C                   eval      oriFLO = *blanks
051200040723     C*
051300040723     C* Infine valorizzo la chiave esterna sul file FNVAO
051400040723     C                   eval      dorm01 = vaoFLO
051500040723     C                   movel     §numori       §ormpg
051600050412     C*
051700050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
051800050412     C                   movel     'S'           §ormfr
051900050412     C*
052000050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
052100040723     C                   eval      vaoFLO = dorm01
052200040723     C*
052300040723     C* ...quindi scrivo il file TIORI00F
052400040723     C                   WRITE     TIORI000
052500040723     C*
052600040723     C                   ENDSR
052700050826
052800050826
052900050826
053000050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
053100050826     C     wriVGD        BEGSR
053200050826     C*
053300050826     C* Reperisco la descrizione della fase da tabella
053400050826     C                   movel(p)  'FAR'         KteCOD
053500050826     C                   movel(p)  '000'         KteKE1
053600050826     C     KEYtbe        chain     tntbe01l
053700050826     C                   if        %found(tntbe01l)
053800050826     C                   movel(P)  tbeuni        DFAR
053900050826     C                   else
054000050826     C                   clear                   DFAR
054100050826     C                   endif
054200050826     C*
054300050826     C* Valorizzo buffer record
054400050826     C                   clear                   FNVAPDS
054500050826     C                   eval      vapIDC = oriIDC
054600050826     C                   eval      vapPOE = oriPOE
054700050826     C                   eval      vapRFA = vaoRFA
054800050826     C                   eval      vapPOG = oriPOE
054900050826     C                   eval      vapDAE = oriDER
055000050826     C                   eval      vapORE = oriOER
055100050826     C                   if        d§fardva <> *blanks
055200050826     C                   eval      vapDFA = d§fardva
055300050826     C                   else
055400050826     C                   eval      vapDFA = d§fardes
055500050826     C                   endif
055600050826     C*
055700050826     C                   clear                   tivgd000
055800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
055900050826     C                   eval      vgdTIP = 'VP'
056000050826     C                   eval      vgdKSU = vapIDC
056100050826     C                   eval      vgdTSC = 'WW'
056200050826     C                   eval      vgdDAT = datcor
056300080702     C                   eval      vgdPGM = 'TITV1H6R'
056400060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
056500060306     C                   exsr      sr_Control
056600060306     C                   if        Cli_Abil = *on
056700060306     C                   eval      vgdKSU = woriIDC
056800050826     C                   write     tivgd000
056900060306     C                   endif
057000050826     C*
057100050826     C                   ENDSR
057200050826     C*------------------------------------------------------------------------*
057300010601
057400010601
057500010601
057600010330     C*----------------------------------------------------*
057700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
057800010330     C*----------------------------------------------------*
057900010330     C     INZVAR        BEGSR
058000010330     C*
058100030822     C* Inizializzo variabili di wrk
058200020204     C                   Z-ADD     *zeros        Num5_0            5 0
058300030822     C*
058400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
058500040708     C                   CLEAR                   FNVAO000
058600040723     C                   CLEAR                   TIORI000
058700030822     C                   CLEAR                   SkSplitCSV
058800030822     C                   EVAL      i = 1
058900030822     C                   EVAL      posDa = *zeros
059000030822     C                   EVAL      posA  = *zeros
059100030822     C*
059200030822     C* Reimposto i valori di default
059300030822     C                   EXSR      DEFCAM
059400030822     C*
059500010330     C                   ENDSR
059600010330     C*----------------------------------------------------*
059700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
059800010330     C*----------------------------------------------------*
059900010330     C     DEFCAM        BEGSR
060000030822     C*
060100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
060200030715     C* e delimitatore testo.
060300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
060400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
060600030822     C*
060700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060900030715     C                   SETON                                        50
061000030715     C                   ELSE
061100030715     C                   SETOFF                                       50
061200030715     C                   ENDIF
061300040723     C*
061400080707     C* Imposto i valori d default d alcuni campi
061500111109     C***                EVAL      VAOCOR = 0594068102
061600111109     C***                EVAL      VAOCRC = 0594068102
061700111109     C***                EVAL      VAOCTR = '100'
061800111109     C***                EVAL      VAOORR = 1600
061900111109     C***                EVAL      VAOPOE = 059
062000080708     C*
062100080708     C                   CLEAR                   DORM01
062200160107     C                   EVAL      §ORCOMC = 'N'
062300080708     C                   EVAL      VAOFLO = DORM01
062400100226     C***                EVAL      VAODAR = %dec(%date() + %Days(1) : *ISO)
062500110621     C***                EVAL      VAODAR = datcor
062600080707     C*
062700110628     C                   EVAL      VAOPAG = 'O'
062800111114     C***                EVAL      VAODDT = 'S'
062900080707     C                   EVAL      VAOTCO = 'F'
063000080707     C                   EVAL      VAOTOR = 'S'
063100030822     C*
063200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
063300030822     C                   EVAL      posDaDft = 1
063400030822     C                   EVAL      posADft  = 0
063500030822     C                   EVAL      wGiroDft = 0
063600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
063700030822     C                             posDaDft > 0
063800030822     C*
063900030822     C* Gestisco il 1° giro
064000030822     C                   IF        wGiroDft = 0
064100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
064200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
064300030822     C* Incremento il contatore dei "giri"
064400030822     C                   EVAL      wGiroDft = 1
064500030822     C                   ELSE
064600030822     C                   EVAL      posDaDft = posADft
064700030822     C                   ENDIF
064800030822     C* Eseguo lo scan x trovare la fine del campo corrente
064900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
065000030822     C*
065100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
065200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
065300030822     C                   IF        posDaDft > 0 AND
065400030822     C                             posADft  > 0
065500040708     C* POE
065600040708     C                   IF        %subst(
065700040708     C                             %subst(vlrppt:posDaDft+1:
065800040708     C                             posADft-posDaDft-1):1:3)
065900040708     C                             = 'POE'
066000040708     C                   EVAL      PiStr=%trim(%subst(
066100040708     C                             %subst(vlrppt:posDaDft+1:
066200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300040708     C                   EXSR      CHKNUM
066400040708     C                   IF        PiInt=*on
066500040708     C                   Z-ADD     PiVal         VAOPOE
066600040708     C                   ENDIF
066700040708     C                   ENDIF
066800040708     C* TOR
066900040708     C                   IF        %subst(
067000040708     C                             %subst(vlrppt:posDaDft+1:
067100040708     C                             posADft-posDaDft-1):1:3)
067200040708     C                             = 'TOR'
067300040708     C                   EVAL      VAOTOR=%trim(%subst(
067400040708     C                             %subst(vlrppt:posDaDft+1:
067500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600040708     C                   ENDIF
067700040708     C* TCO
067800040708     C                   IF        %subst(
067900040708     C                             %subst(vlrppt:posDaDft+1:
068000040708     C                             posADft-posDaDft-1):1:3)
068100040708     C                             = 'TCO'
068200040708     C                   EVAL      VAOTCO=%trim(%subst(
068300040708     C                             %subst(vlrppt:posDaDft+1:
068400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500040708     C                   ENDIF
068600040707     C* COR
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000040707     C                             = 'COR'
069100030822     C                   EVAL      PiStr=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   EXSR      CHKNUM
069500030822     C                   IF        PiInt=*on
069600040708     C                   Z-ADD     PiVal         VAOCOR
069700030822     C                   ENDIF
069800030822     C                   ENDIF
069900040708     C* CRC
070000040708     C                   IF        %subst(
070100040708     C                             %subst(vlrppt:posDaDft+1:
070200040708     C                             posADft-posDaDft-1):1:3)
070300040708     C                             = 'CRC'
070400040708     C                   EVAL      PiStr=%trim(%subst(
070500040708     C                             %subst(vlrppt:posDaDft+1:
070600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700040708     C                   EXSR      CHKNUM
070800040708     C                   IF        PiInt=*on
070900040708     C                   Z-ADD     PiVal         VAOCRC
071000040708     C                   ENDIF
071100040708     C                   ENDIF
071200040708     C* FFD
071300040708     C                   IF        %subst(
071400040708     C                             %subst(vlrppt:posDaDft+1:
071500040708     C                             posADft-posDaDft-1):1:3)
071600040708     C                             = 'FFD'
071700040708     C                   EVAL      VAOFFD=%trim(%subst(
071800040708     C                             %subst(vlrppt:posDaDft+1:
071900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000040708     C                   ENDIF
072100040708     C* ORR
072200040708     C                   IF        %subst(
072300040708     C                             %subst(vlrppt:posDaDft+1:
072400040708     C                             posADft-posDaDft-1):1:3)
072500040708     C                             = 'ORR'
072600040708     C                   EVAL      PiStr=%trim(%subst(
072700040708     C                             %subst(vlrppt:posDaDft+1:
072800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900040708     C                   EXSR      CHKNUM
073000040708     C                   IF        PiInt=*on
073100040708     C                   Z-ADD     PiVal         VAOORR
073200040708     C                   ENDIF
073300040708     C                   ENDIF
073400040712     C* NCL
073500040712     C                   IF        %subst(
073600040712     C                             %subst(vlrppt:posDaDft+1:
073700040712     C                             posADft-posDaDft-1):1:3)
073800040712     C                             = 'NCL'
073900040712     C                   EVAL      PiStr=%trim(%subst(
074000040712     C                             %subst(vlrppt:posDaDft+1:
074100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200040712     C                   EXSR      CHKNUM
074300040712     C                   IF        PiInt=*on
074400040712     C                   Z-ADD     PiVal         VAONCL
074500040712     C                   ENDIF
074600040712     C                   ENDIF
074700040712     C* PKG
074800040712     C                   IF        %subst(
074900040712     C                             %subst(vlrppt:posDaDft+1:
075000040712     C                             posADft-posDaDft-1):1:3)
075100040712     C                             = 'PKG'
075200040712     C                   EVAL      PiStr=%trim(%subst(
075300040712     C                             %subst(vlrppt:posDaDft+1:
075400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040712     C                   EXSR      CHKNUM
075600040712     C                   IF        PiNum=*on
075700040712     C                   Z-ADD     PiVal         VAOPKG
075800040712     C                   ENDIF
075900040712     C                   ENDIF
076000040712     C* VLM
076100040712     C                   IF        %subst(
076200040712     C                             %subst(vlrppt:posDaDft+1:
076300040712     C                             posADft-posDaDft-1):1:3)
076400040712     C                             = 'VLM'
076500040712     C                   EVAL      PiStr=%trim(%subst(
076600040712     C                             %subst(vlrppt:posDaDft+1:
076700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800040712     C                   EXSR      CHKNUM
076900040712     C                   IF        PiNum=*on
077000040712     C                   Z-ADD     PiVal         VAOVLM
077100040712     C                   ENDIF
077200040712     C                   ENDIF
077300040708     C* NAM
077400040708     C                   IF        %subst(
077500040708     C                             %subst(vlrppt:posDaDft+1:
077600040708     C                             posADft-posDaDft-1):1:3)
077700040708     C                             = 'NAM'
077800040708     C                   EVAL      VAONAM=%trim(%subst(
077900040708     C                             %subst(vlrppt:posDaDft+1:
078000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100040708     C                   ENDIF
078200040708     C* PAG
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600040708     C                             = 'PAG'
078700040708     C                   EVAL      VAOPAG=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100040708     C* KSC
079200040708     C                   IF        %subst(
079300040708     C                             %subst(vlrppt:posDaDft+1:
079400040708     C                             posADft-posDaDft-1):1:3)
079500040708     C                             = 'KSC'
079600040708     C                   EVAL      PiStr=%trim(%subst(
079700040708     C                             %subst(vlrppt:posDaDft+1:
079800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900040708     C                   EXSR      CHKNUM
080000040708     C                   IF        PiInt=*on
080100040708     C                   Z-ADD     PiVal         VAOKSC
080200040708     C                   ENDIF
080300040708     C                   ENDIF
080400040708     C* CTR
080500040708     C                   IF        %subst(
080600040708     C                             %subst(vlrppt:posDaDft+1:
080700040708     C                             posADft-posDaDft-1):1:3)
080800040708     C                             = 'CTR'
080900040708     C                   EVAL      VAOCTR=%trim(%subst(
081000040708     C                             %subst(vlrppt:posDaDft+1:
081100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200040708     C                   ENDIF
081300040708     C* POC
081400040708     C                   IF        %subst(
081500040708     C                             %subst(vlrppt:posDaDft+1:
081600040708     C                             posADft-posDaDft-1):1:3)
081700040708     C                             = 'POC'
081800040708     C                   EVAL      PiStr=%trim(%subst(
081900040708     C                             %subst(vlrppt:posDaDft+1:
082000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082100040708     C                   EXSR      CHKNUM
082200040708     C                   IF        PiInt=*on
082300040708     C                   Z-ADD     PiVal         VAOPOC
082400040708     C                   ENDIF
082500040708     C                   ENDIF
082600040708     C* NO1
082700040708     C                   IF        %subst(
082800040708     C                             %subst(vlrppt:posDaDft+1:
082900040708     C                             posADft-posDaDft-1):1:3)
083000040708     C                             = 'NO1'
083100040708     C                   EVAL      VAONO1=%trim(%subst(
083200040708     C                             %subst(vlrppt:posDaDft+1:
083300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400040708     C                   ENDIF
083500040708     C* DDT
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900040708     C                             = 'DDT'
084000040708     C                   EVAL      VAODDT=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400040708     C* RFA
084500040708     C                   IF        %subst(
084600040708     C                             %subst(vlrppt:posDaDft+1:
084700040708     C                             posADft-posDaDft-1):1:3)
084800040708     C                             = 'RFA'
084900040708     C                   EVAL      VAORFA=%trim(%subst(
085000040708     C                             %subst(vlrppt:posDaDft+1:
085100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200040708     C                   ENDIF
085300040708     C* FLO
085400040708     C                   IF        %subst(
085500040708     C                             %subst(vlrppt:posDaDft+1:
085600040708     C                             posADft-posDaDft-1):1:3)
085700040708     C                             = 'FLO'
085800040708     C                   EVAL      VAOFLO=%trim(%subst(
085900040708     C                             %subst(vlrppt:posDaDft+1:
086000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
086200080702     C* e posizione 7 del VAOFLO
086300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
086400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
086500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
086600040708     C                   ENDIF
086700030822     C* ...
086800030822     C                   ENDIF
086900030822     C                   ENDDO
087000020204     C*
087100010330     C                   ENDSR
087200010607     C*----------------------------------------------------*
087300040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
087400010607     C*----------------------------------------------------*
087500040708     C     IMPVAO        BEGSR
087600010607     C*
087700010607     C                   SETOFF                                       3132
087800030515     C*
087900100223     C                   MOVEL     datcor        VAODAO
088000040708     C                   MOVEL     wn14          VAOOAO
088100050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088200050314     C     minu:maiu     XLATE     vindta        vindta
088300010607     C*
088400030715     C**********
088500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088600030715     C**********
088700030715     C                   IF        wGiro = *zeros
088800040709     C*
088900040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
089000040709     C                   EVAL      depvinDTA = vinDTA
089100040709     C*
089200040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
089300040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
089400110616     C                                      %trim(intcol(3))+%trim(intcol(4))+
089500110616     C                                      %trim(intcol(5))
089600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089700050314     C**!!!minu:maiu     XLATE     vindta        vindta
089800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089900040324     C                   DOW       posDa <= %len(%trimr(vindta))
090000030715     C*
090100030715     C* Gestisco il 1° campo
090200030715     C                   IF        i = 1
090300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090400030715     C                   EVAL      posDa = 1
090500030715     C* Eseguo lo scan x trovare la fine del primo campo
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C* X i campi successivi al 1°
091100030715     C                   ELSE
091200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091300030715     C                   EVAL      posDa = posA + 1
091400030715     C* Eseguo lo scan x trovare la fine del campo corrente
091500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091600030715     C* Gestisco l'ultimo campo
091700030715     C                   IF        posA = *zeros
091800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   LEAVE
092300030715     C                   ELSE
092400030715     C* A questo punto "estraggo" il campo corrente
092500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092600030715     C                                                    (posA-posDa))
092700030715     C                   ENDIF
092800030715     C                   ENDIF
092900030715     C* Incremento il contatore d campo
093000030715     C                   EVAL      i = i +1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093400030715     C* delimitazione testo
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitFLD)
093700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093900030715     C                   EVAL      i = i + 1
094000030715     C                   ENDDO
094100030715     C                   EVAL      wGiro = 1
094200040709     C* Ripristino la 1° riga originale
094300040709     C                   EVAL      vinDTA = depvinDTA
094400030715     C                   ELSE
094500030715     C                   EVAL      wGiro = 2
094600031204     C**********
094700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094800031204     C* d separatore campo
094900031204     C**********
095000040709     C                   EVAL      vindta = %trim(vindta)
095100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
095200040709     C                   EVAL      vindta = %subst(vindta:2)
095300040709     C                   ENDDO
095400040709     C*
095500040709     C                   Z-ADD     *zeros        lunghInput        4 0
095600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
095700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
096000040709     C                   ENDDO
096100030715     C**********
096200030715     C* Effettuo lo split del campo dati d input
096300030715     C**********
096400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096500040324     C                   DOW       posDa <= %len(%trimr(vindta))
096600030715     C*
096700030715     C* Gestisco il 1° campo
096800030715     C                   IF        i = 1
096900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
097000030715     C                   EVAL      posDa = 1
097100030715     C* Eseguo lo scan x trovare la fine del primo campo
097200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097300030715     C* A questo punto "estraggo" il campo corrente
097400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097500030715     C                                                    (posA-posDa))
097600030715     C* X i campi successivi al 1°
097700030715     C                   ELSE
097800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097900030715     C                   EVAL      posDa = posA + 1
098000031204     C* Verifico che nn vi sia il campo nn valorizzato
098100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
098200031204     C* Se campo nn valorizzato skippo al prossimo
098300031204     C                   EVAL      SkSplitCSV(i) = *blanks
098400031204     C                   EVAL      posA  = posA + 1
098500031204     C                   ELSE
098600030715     C* Eseguo lo scan x trovare la fine del campo corrente
098700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098800030715     C* Gestisco l'ultimo campo
098900030715     C                   IF        posA = *zeros
099000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
099100030715     C* A questo punto "estraggo" il campo corrente
099200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099300030715     C                                                    (posA-posDa))
099400030715     C                   LEAVE
099500030715     C                   ELSE
099600030715     C* A questo punto "estraggo" il campo corrente
099700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099800030715     C                                                    (posA-posDa))
099900030715     C                   ENDIF
100000031204     C                   ENDIF
100100030715     C                   ENDIF
100200030715     C* Incremento il contatore d campo
100300030715     C                   EVAL      i = i +1
100400030715     C                   ENDDO
100500030715     C*
100600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100700030715     C* delimitazione testo
100800030715     C                   EVAL      i = 1
100900030715     C                   DOW       i < %elem(SkSplitCSV)
101000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
101100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
101200030715     C                   EVAL      i = i + 1
101300030715     C                   ENDDO
101400030715     C*
101500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101600030715     C                   EVAL      i = 1
101700030715     C                   DOW       i < %elem(SkSplitFLD)
101800030715     C*
101900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102100030715     C                             SkSplitCSV(i) <> *blanks
102200030721     C*** ATB
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
102400040708     C                   EVAL      VAOATB = SkSplitCSV(i)
102500030721     C                   ENDIF
102600040708     C*** TOR
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
102800040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
102900040708     C                   ENDIF
103000040708     C*** TCO
103100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
103200040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
103300040708     C                   ENDIF
103400040708     C*** RSO
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
103600040708     C                   EVAL      VAORSO = SkSplitCSV(i)
103700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103800040708     C     '@':'A'       XLATE     VAORSO        VAORSO
103900030715     C* ==
104000030715     C                   ENDIF
104100040708     C*** INO
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
104300040708     C                   EVAL      VAOINO = SkSplitCSV(i)
104400030715     C                   ENDIF
104500040708     C*** LOO
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
104700040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
104800030715     C                   ENDIF
104900040708     C*** PRO
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
105100040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
105200030715     C                   ENDIF
105300040708     C*** NAO
105400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
105500040708     C                   EVAL      VAONAO = SkSplitCSV(i)
105600030721     C                   ENDIF
105700040708     C*** RSR
105800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
105900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
106000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
106100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
106200040708     C* ==
106300040708     C                   IF        VAORSR = *blanks
106400040708     C                   SETON                                        31
106500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106600040708     C                             + ' ' + 'VAORSR'
106700040708     C                   ENDIF
106800040708     C                   ENDIF
106900040708     C*** INR
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
107100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
107200040708     C                   IF        VAOINR = *blanks
107300040708     C                   SETON                                        31
107400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
107500040708     C                             + ' ' + 'VAOINR'
107600040708     C                   ENDIF
107700040708     C                   ENDIF
107800040708     C*** LOR
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
108000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
108100040708     C                   IF        VAOLOR = *blanks
108200040708     C                   SETON                                        31
108300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
108400040708     C                             + ' ' + 'VAOLOR'
108500040708     C                   ENDIF
108600040708     C                   ENDIF
108700040708     C*** PRR
108800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
108900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
109000040708     C                   IF        VAOPRR = *blanks
109100040708     C                   SETON                                        31
109200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109300040708     C                             + ' ' + 'VAOPRR'
109400040708     C                   ENDIF
109500040708     C                   ENDIF
109600040708     C*** NAR
109700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
109800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
109900040708     C                   ENDIF
110000040708     C*** RER
110100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
110200040708     C                   EVAL      VAORER = SkSplitCSV(i)
110300040708     C                   ENDIF
110400040708     C*** TER
110500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
110600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
110700040708     C                   ENDIF
110800040708     C*** RSC
110900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
111000040708     C                   EVAL      VAORSC = SkSplitCSV(i)
111100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
111200040708     C     '@':'A'       XLATE     VAORSC        VAORSC
111300040708     C* ==
111400040708     C                   ENDIF
111500040708     C*** INC
111600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
111700040708     C                   EVAL      VAOINC = SkSplitCSV(i)
111800040708     C                   ENDIF
111900040708     C*** LOC
112000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
112100040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
112200040708     C                   ENDIF
112300040708     C*** PRC
112400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
112500040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
112600040708     C                   ENDIF
112700040708     C*** NAC
112800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
112900040708     C                   EVAL      VAONAC = SkSplitCSV(i)
113000040708     C                   ENDIF
113100040708     C*** FFD
113200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
113300040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
113400040708     C                   ENDIF
113500040708     C*** RMP
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
113700040708     C                   EVAL      VAORMP = SkSplitCSV(i)
113800040708     C                   ENDIF
113900040708     C*** NAM
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
114100040708     C                   EVAL      VAONAM = SkSplitCSV(i)
114200040708     C                   ENDIF
114300040708     C*** PAG
114400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
114500040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
114600040708     C                   ENDIF
114700040708     C*** CTR
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
114900040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
115000040708     C                   ENDIF
115100040708     C*** ZOR
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
115300040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
115400040708     C                   ENDIF
115500040708     C*** NO1
115600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
115700040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
115800030715     C                   ENDIF
115900040708     C*** NO2
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
116100040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
116200030715     C                   ENDIF
116300040708     C*** DDT
116400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
116500040708     C                   EVAL      VAODDT = SkSplitCSV(i)
116600030715     C                   ENDIF
116700040708     C*** RFA
116800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
116900040708     C                   EVAL      VAORFA = SkSplitCSV(i)
117000030715     C                   ENDIF
117100040708     C*** SPI
117200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
117300040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
117400030715     C                   ENDIF
117500040708     C*** FLO
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
117700040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
117800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
117900080708     C* e la posizione 7 del VAOFLO
118000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
118100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
118200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
118300030715     C                   ENDIF
118400050314
118500050412      *** DAR
118600050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
118700050412     C                   EVAL      PiStr=SkSplitCSV(i)
118800050412     C                   EXSR      CHKNUM
118900050412     C                   IF        PiInt=*on
119000050412     C                   Z-ADD     PiVal         VAODAR
119100050412     C                   ELSE
119200050412     C                   SETON                                        32
119300050412     C                   EVAL      VAODAR = *zeros
119400050412     C                   EVAL      vinmsg = %trimr(vinmsg)
119500050412     C                             + ' ' + 'VAODAR'
119600050412     C                   ENDIF
119700050314     C                   ENDIF
119800030715     C
119900030721     C*
120000030721     C* Reperisco quindi i campi numerici...
120100040708     C*** POE
120200040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
120300040723     C                   EVAL      PiStr=SkSplitCSV(i)
120400040723     C                   EXSR      CHKNUM
120500040723     C                   IF        PiInt=*on    AND
120600040723     C                             PiVal<=999   AND
120700040723     C                             PiVal>*zeros
120800040723     C                   Z-ADD     PiVal         VAOPOE
120900040723     C                   ELSE
121000040723     C                   SETON                                        32
121100040723     C                   EVAL      VAOPOE = *zeros
121200040723     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040723     C                             + ' ' + 'VAOPOE'
121400040723     C                   ENDIF
121500040723     C                   ENDIF
121600040708     C*** COR
121700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
121800040708     C                   EVAL      PiStr=SkSplitCSV(i)
121900040708     C                   EXSR      CHKNUM
122000040708     C                   IF        PiInt=*on
122100040708     C                   Z-ADD     PiVal         VAOCOR
122200040708     C                   ELSE
122300040708     C                   SETON                                        31
122400040708     C                   EVAL      VAOCOR = *zeros
122500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040708     C                             + ' ' + 'VAOCOR'
122700040708     C                   ENDIF
122800040708     C                   ENDIF
122900040708     C*** CAO
123000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on
123400030721     C                   Z-ADD     PiVal         Num5_0
123500040708     C                   MOVEL(p)  Num5_0        VAOCAO
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800040708     C                   EVAL      VAOCAO = *zeros
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000040708     C                             + ' ' + 'VAOCAO'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300040708     C*** CRA
124400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on
124800040708     C                   Z-ADD     PiVal         VAOCRA
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100040708     C                   EVAL      VAOCRA = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040708     C                             + ' ' + 'VAOCRA'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600040708     C*** CAR
125700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
125800040708     C                   EVAL      PiStr=SkSplitCSV(i)
125900040708     C                   EXSR      CHKNUM
126000040708     C                   IF        PiInt=*on
126100040708     C                   Z-ADD     PiVal         Num5_0
126200040708     C                   MOVEL(p)  Num5_0        VAOCAR
126300040708     C                   ELSE
126400040708     C                   SETON                                        31
126500040708     C                   EVAL      VAOCAR = *zeros
126600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126700040708     C                             + ' ' + 'VAOCAR'
126800040708     C                   ENDIF
126900040708     C                   ENDIF
127000040708     C*** CRC
127100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
127200040708     C                   EVAL      PiStr=SkSplitCSV(i)
127300040708     C                   EXSR      CHKNUM
127400040708     C                   IF        PiInt=*on
127500040708     C                   Z-ADD     PiVal         VAOCRC
127600040708     C                   ELSE
127700040708     C                   SETON                                        32
127800040708     C                   EVAL      VAOCRC = *zeros
127900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128000040708     C                             + ' ' + 'VAOCRC'
128100040708     C                   ENDIF
128200040708     C                   ENDIF
128300040708     C*** CAC
128400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
128500040708     C                   EVAL      PiStr=SkSplitCSV(i)
128600040708     C                   EXSR      CHKNUM
128700040708     C                   IF        PiInt=*on
128800040708     C                   Z-ADD     PiVal         Num5_0
128900040708     C                   MOVEL(p)  Num5_0        VAOCAC
129000040708     C                   ELSE
129100040708     C                   SETON                                        32
129200040708     C                   EVAL      VAOCAC = *zeros
129300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129400040708     C                             + ' ' + 'VAOCAC'
129500040708     C                   ENDIF
129600040708     C                   ENDIF
129700040708     C*** DAR
129800050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
129900050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
130000050314     C**!!!              EXSR      CHKNUM
130100050314     C**!!!              IF        PiInt=*on
130200050314     C**!!!              Z-ADD     PiVal         VAODAR
130300050314     C**!!!              ELSE
130400050314     C**!!!              SETON                                        32
130500050314     C**!!!              EVAL      VAODAR = *zeros
130600050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
130700050314     C**!!!                        + ' ' + 'VAODAR'
130800050314     C**!!!              ENDIF
130900050314     C**!!!              ENDIF
131000040708     C*** ORR
131100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
131200040708     C                   EVAL      PiStr=SkSplitCSV(i)
131300040708     C                   EXSR      CHKNUM
131400040708     C                   IF        PiInt=*on
131500040708     C                   Z-ADD     PiVal         VAOORR
131600040708     C                   ELSE
131700040708     C                   SETON                                        32
131800040708     C                   EVAL      VAOORR = *zeros
131900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132000040708     C                             + ' ' + 'VAOORR'
132100040708     C                   ENDIF
132200040708     C                   ENDIF
132300030721     C*** NCL
132400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on
132800040723     C                   IF        PiVal > *zeros
132900040708     C                   Z-ADD     PiVal         VAONCL
133000040723     C                   ENDIF
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300040708     C                   EVAL      VAONCL = *zeros
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500040708     C                             + ' ' + 'VAONCL'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800040708     C*** PKG
133900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiNum=*on
134300040723     C                   IF        PiVal > *zeros
134400040708     C                   Z-ADD     PiVal         VAOPKG
134500040723     C                   ENDIF
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800040708     C                   EVAL      VAOPKG = *zeros
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000040708     C                             + ' ' + 'VAOPKG'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300040708     C*** VLM
135400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
135500040708     C                   EVAL      PiStr=SkSplitCSV(i)
135600040708     C                   EXSR      CHKNUM
135700040708     C                   IF        PiNum=*on
135800040708     C                   Z-ADD     PiVal         VAOVLM
135900040708     C                   ELSE
136000040708     C                   SETON                                        32
136100040708     C                   EVAL      VAOVLM = *zeros
136200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136300040708     C                             + ' ' + 'VAOVLM'
136400040708     C                   ENDIF
136500040708     C                   ENDIF
136600040708     C*** BNC
136700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
136800040708     C                   EVAL      PiStr=SkSplitCSV(i)
136900040708     C                   EXSR      CHKNUM
137000040708     C                   IF        PiInt=*on
137100040708     C                   Z-ADD     PiVal         VAOBNC
137200040708     C                   ELSE
137300040708     C                   SETON                                        32
137400040708     C                   EVAL      VAOBNC = *zeros
137500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137600040708     C                             + ' ' + 'VAOBNC'
137700040708     C                   ENDIF
137800040708     C                   ENDIF
137900040708     C*** BLC
138000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
138100040708     C                   EVAL      PiStr=SkSplitCSV(i)
138200040708     C                   EXSR      CHKNUM
138300040708     C                   IF        PiInt=*on
138400040708     C                   Z-ADD     PiVal         VAOBLC
138500040708     C                   ELSE
138600040708     C                   SETON                                        32
138700040708     C                   EVAL      VAOBLC = *zeros
138800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138900040708     C                             + ' ' + 'VAOBLC'
139000040708     C                   ENDIF
139100040708     C                   ENDIF
139200040708     C*** ATT
139300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
139400040708     C                   EVAL      PiStr=SkSplitCSV(i)
139500040708     C                   EXSR      CHKNUM
139600040708     C                   IF        PiInt=*on
139700040708     C                   Z-ADD     PiVal         VAOATT
139800040708     C                   ELSE
139900040708     C                   SETON                                        32
140000040708     C                   EVAL      VAOATT = *zeros
140100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140200040708     C                             + ' ' + 'VAOATT'
140300040708     C                   ENDIF
140400040708     C                   ENDIF
140500040708     C*** MTC
140600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
140700040708     C                   EVAL      PiStr=SkSplitCSV(i)
140800040708     C                   EXSR      CHKNUM
140900040708     C                   IF        PiInt=*on
141000040708     C                   Z-ADD     PiVal         VAOMTC
141100040708     C                   ELSE
141200040708     C                   SETON                                        32
141300040708     C                   EVAL      VAOMTC = *zeros
141400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141500040708     C                             + ' ' + 'VAOMTC'
141600040708     C                   ENDIF
141700040708     C                   ENDIF
141800040708     C*** KSC
141900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
142000030721     C                   EVAL      PiStr=SkSplitCSV(i)
142100030721     C                   EXSR      CHKNUM
142200030721     C                   IF        PiInt=*on      AND
142300030721     C                             PiVal<=9999999 AND
142400030721     C                             PiVal>*zeros
142500040708     C                   Z-ADD     PiVal         VAOKSC
142600030721     C                   ELSE
142700030721     C                   SETON                                        32
142800040708     C                   EVAL      VAOKSC = *zeros
142900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143000040708     C                             + ' ' + 'VAOKSC'
143100030721     C                   ENDIF
143200030721     C                   ENDIF
143300040708     C*** POR
143400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
143500030721     C                   EVAL      PiStr=SkSplitCSV(i)
143600030721     C                   EXSR      CHKNUM
143700030721     C                   IF        PiInt=*on    AND
143800030721     C                             PiVal<=999   AND
143900030721     C                             PiVal>*zeros
144000040708     C                   Z-ADD     PiVal         VAOPOR
144100030721     C                   ELSE
144200030721     C                   SETON                                        32
144300040708     C                   EVAL      VAOPOR = *zeros
144400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144500040708     C                             + ' ' + 'VAOPOR'
144600030721     C                   ENDIF
144700030721     C                   ENDIF
144800040708     C*** POC
144900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
145000040708     C                   EVAL      PiStr=SkSplitCSV(i)
145100040708     C                   EXSR      CHKNUM
145200040708     C                   IF        PiInt=*on    AND
145300040708     C                             PiVal<=999   AND
145400040708     C                             PiVal>*zeros
145500040708     C                   Z-ADD     PiVal         VAOPOC
145600040708     C                   ELSE
145700040708     C                   SETON                                        32
145800040708     C                   EVAL      VAOPOC = *zeros
145900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146000040708     C                             + ' ' + 'VAOPOC'
146100040708     C                   ENDIF
146200040708     C                   ENDIF
146300040708     C*** STO
146400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
146500040708     C                   EVAL      PiStr=SkSplitCSV(i)
146600040708     C                   EXSR      CHKNUM
146700040708     C                   IF        PiInt=*on
146800040708     C                   Z-ADD     PiVal         VAOSTO
146900040708     C                   ELSE
147000040708     C                   SETON                                        32
147100040708     C                   EVAL      VAOSTO = *zeros
147200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147300040708     C                             + ' ' + 'VAOSTO'
147400040708     C                   ENDIF
147500040708     C                   ENDIF
147600110526     C*
147700110526     C*********  Gestione particolarità
147800110526     C*
147900110616     C*** DAR
148000110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
148100110630     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
148200110630     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
148300110630     C                   ENDIF
148400110616     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
148500110616     C                                   %subst(SkSplitCSV(i):3:2)+
148600110616     C                                   %subst(SkSplitCSV(i):1:2)
148700110616     C                   EXSR      CHKNUM
148800110616     C                   IF        PiInt=*on
148900110616     C                   Z-ADD     PiVal         VAODAR
149000110616     C                   ELSE
149100110616     C                   SETON                                        32
149200110616     C                   EVAL      VAODAR = *zeros
149300110616     C                   EVAL      vinmsg = %trimr(vinmsg)
149400110616     C                             + ' ' + 'VAODAR'
149500110616     C                   ENDIF
149600110616     C                   ENDIF
149700110616     C*** ORR
149800110616     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOORR'
149900110630     C***                EVAL      PiStr=%subst(SkSplitCSV(i):1:2)+
150000110630     C***                                %subst(SkSplitCSV(i):4:2)
150100110630     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
150200110616     C                   EXSR      CHKNUM
150300110616     C                   IF        PiInt=*on
150400110616     C                   Z-ADD     PiVal         VAOORR
150500110616     C                   ELSE
150600110616     C                   SETON                                        32
150700110616     C                   EVAL      VAOORR = *zeros
150800110616     C                   EVAL      vinmsg = %trimr(vinmsg)
150900110616     C                             + ' ' + 'VAOORR'
151000110616     C                   ENDIF
151100110616     C                   ENDIF
151200110526     C*
151300110526     C*********
151400030715     C*
151500030715     C                   ENDIF
151600030715     C                   EVAL      i = i + 1
151700030715     C                   ENDDO
151800040723     C*
151900030715     C                   ENDIF
152000020204     C*
152100010607     C                   ENDSR
152200010607     C*----------------------------------------------------*
152300010330
152400100512
152500100512
152600100512     C*------------------------------------------------------------------------*
152700100512     C* EXEFORZ - Considerazioni finali
152800100512     C*------------------------------------------------------------------------*
152900100512     C     EXEFORZ       BEGSR
153000100512     C*
153100100512     C                   if        VAOKSC = *zeros
153200100512     C                   select
153300100512     C                   when      VAOPAG = 'O'
153400100512     C                   movel     VAOCOR        VAOKSC
153500100512     C                   when      VAOPAG = 'M'
153600100512     C                   movel     VAOCRA        VAOKSC
153700100512     C                   when      VAOPAG = 'D'
153800100512     C                   movel     VAOCRC        VAOKSC
153900100512     C                   endsl
154000100512     C                   endif
154100100512     C*
154200100512     C                   ENDSR
154300080702
154400080702
154500080702     C*----------------------------------------------------*
154600080702     C*  CONTROLLO NUMERICITA' CAMPI
154700080702     C*----------------------------------------------------*
154800080702     C     CHKNUM        BEGSR
154900080702     C*
155000080702     C                   IF        PiDecChr = *blanks
155100080702     C                   EVAL      PiDecChr = ','
155200080702     C                   ENDIF
155300080702     C*
155400080702     C                   callp     UBISNUM_Check(PiStr
155500080702     C                                          :PiDecChr
155600080702     C                                          :PiVal
155700080702     C                                          :PiNum
155800080702     C                                          :PiInt)
155900080702     C*
156000080702     C                   ENDSR
156100080702     C***
156200010601
156300080702
156400010601
156500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156600010601     C     repfil        BEGSR
156700010601     C*
156800010601     C                   if        invfil = *zeros and
156900010601     C                             depfil > *zeros and
157000010629     C                             (vinflg = *blanks or
157100010629     C                              vinflg = *zeros)
157200010601     C
157300010601     C                   eval      invfil = depfil
157400010601     C                   endif
157500010601     C*
157600010601     C                   if        depfil <> invfil and
157700010601     C                             invfil > *zeros
157800010601     C                   eval      flgMulti = '1'
157900010601     C                   if        vinflg = *blanks
158000010601     C                   add       1             cntNonEl
158100010601     C                   endif
158200010601     C                   endif
158300010601     C*
158400010601     C                   if        vinflg = '2'
158500010601     C                   eval      flgStato = '2'
158600010601     C                   endif
158700010601     C*
158800010601     C                   ENDSR
158900010601     C***
159000010601
159100010601
159200010601
159300010330
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
162400990910
162500010601
162600010601
162700010601
162800010601
162900010601      /TITLE Invio dei dati al punto operativo.
163000010601     C     opeini        BEGSR
163100010601     C*
163200010601     C* Inizializzo flag e contatori operativi
163300010601     C                   movel     '0'           flgGiro           1
163400010601     C                   movel     '0'           flgMulti          1
163500010601     C                   movel     '1'           flgStato          1
163600010615     C                   movel     '0'           flgOk             1
163700010601     C                   z-add     *zeros        cntNonEl         10 0
163800010601     C                   z-add     *zeros        depfil            3 0
163900010601     C                   z-add     *zeros        invfil            3 0
164000010601     C*
164100010601     C                   ENDSR
164200010601     C***
164300010601
164400010601
164500050421
164600050421
164700050421     C     *pssr         BEGSR
164800050421     C*
164900050421     C                   eval      wrkesito = '2'
165000050421     C*
165100050421     C                   ENDSR     '*CANCL'
165200050421     C***
165300060306
165400060306
165500060306     C*-------------------------------------------------------------------------*
165600060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
165700060306     C*-------------------------------------------------------------------------*
165800060306     C     sr_Control    begsr
165900060306     C*
166000060306     C* inizializzo a ok il flag d controllo abilitazione
166100060306     C                   eval      Cli_Abil = *on
166200060306     C
166300060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
166400060306     C                   clear                   woriIDC
166500060306     C                   eval      kteCOD = 'VAP'
166600060306     C                   eval      kteKE1 = oriIDC
166700060306     C     keytbe        chain     TNTBE01L
166800060306     C                   if        %found(Tntbe01l)
166900060306     C                   eval      woriIDC = TbeKe2
167000060306     C                   else
167100060306     C                   eval      Cli_Abil = *off
167200060306     C                   endif
167300060306     C*
167400060306     C                   endsr
167500050421
167600010601
167700010330
167800010330
167900000613     C     *inzsr        BEGSR
168000990910     C*
168100990910     C     *entry        plist
168200990920     C                   parm                    tivlrds
168300990921     C                   parm      wrkesito      esito
168400000724     C                   parm                    prmlit
168500000710     C                   parm                    prmfir
168600010330     C*
168700010330     C* CALCOLA LA DATA CORRENTE
168800100223     C                   time                    wn14             14 0
168900100223     C                   movel     wn14          wn6               6 0          *ORA (6)
169000100223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
169100100223     C                   eval      datcor = %dec(%date() : *ISO)
169200040723     C*
169300040723     C* Campi riferito al database
169400040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
169500040723     C     *like         define    tbeke1        kteKE1
169600040723     C*
169700040723     C* Definizione chiavi di lettura
169800040723     C* TNTBE01L - parziale
169900040723     C     keytbe        klist
170000040723     C                   kfld                    kteCOD                         *UTENTE
170100040723     C                   kfld                    kteKE1                         *CODICE
170200000613     C*
170300000613     C                   ENDSR
170400000613     C***
170500990908
170600080702     Otitv1h6p  E            riepilogo         2
170700990915     O                                              'Upload via Internet'
170800990915     O                                           +1 'Traduzione TIVIN00R -'
170900040708     O                                           +1 'FNVAOWWR'
171000990915     O                                           +1 'Testate bolle'
171100990915     O          E            riepilogo   2
171200990915     O                       wrkdata
171300990915     O                       wrkora              +1
171400990915     O                       procname            +1
171500990915     O          E            riepilogo   2
171600990915     O                                              'Cliente..................:'
171700040708     O                       VAOCOR        z     +1
171800990915     O          E            riepilogo   2
171900990920     O                                              'Riferimento Strategi.....:'
172000990920     O                       vlrhdl              +1
172100990915     O          E            riepilogo   2
172200990915     O                                              'Giusti...................:'
172300971022     O                       §CTROK        2   +  1
172400990915     O          E            riepilogo   2
172500990915     O                                              'Sbagliati e corretti.....:'
172600971022     O                       §CTRMO        2   +  1
172700990915     O          E            riepilogo   2
172800990915     O                                              'Sbagliati e scartati.....:'
172900971022     O                       §CTRNO        2   +  1
173000000613
173100080702     Otitv1h6ps E            testdett          2
173200000613     O                                              'Upload via Internet'
173300000613     O                                           +1 'Traduzione TIVIN00R -'
173400040708     O                                           +1 'FNVAOWWR'
173500000613     O                                           +1 'Testate bolle'
173600000616     O          E            testdett    3
173700000613     O                                           +2 'N° rec'
173800000613     O                                           +3 'Anteprima contenuto'
173900000616     O          E            rigadett    2
174000000613     O                       rrnum               +2
174100000621     O                       recko               +3
174200000616     O          E            findett     2
174300000613     O                       wrkdata
174400000613     O                       wrkora              +1
174500000613     O                       procname            +1
174600000616     O          E            findett     2
174700000613     O                                              'Cliente..................:'
174800040708     O                       VAOCOR        z     +1
174900000616     O          E            findett     2
175000000613     O                                              'Riferimento Strategi.....:'
175100000613     O                       vlrhdl              +1
175200000616     O          E            findett     2
175300000613     O                                              'Giusti...................:'
175400000613     O                       §CTROK        2   +  1
175500000616     O          E            findett     2
175600000613     O                                              'Sbagliati e corretti.....:'
175700000613     O                       §CTRMO        2   +  1
175800000616     O          E            findett     2
175900000613     O                                              'Sbagliati e scartati.....:'
176000000613     O                       §CTRNO        2   +  1
176100000613** CMD - COMANDI CL
176200080702OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
176300080702OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
176400080702DLTOVR FILE(TITV1H6P TITV1H6PS) LVL(*)
176500010606
176600000613
176700040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176800080702FILL01;FILL02;VAORSR;FILL03;VAOINR;VAOLOR;VAOCAR;VAOPRR;FILL04;VAOTER;
176900111109VAORER;FILL05;VAONAM;VAONCL;W_VAODAR;W_VAOORR;VAORSC;VAOINC;VAOLOC;
177000111114VAOCAC;VAOPRC;FILXXX;VAONO1;VAONO2;VAORFA;
177100111109
177200111109
