000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500040723     Ffnacr01l  if   e           k disk
000600090617     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800040708     FFnvaowwr  O    E             DISK    usropn
000900040723     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100050412     Ftitvl9p   O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300050412     Ftitvl9ps  O    f  198        PRINTER usropn
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
003400090617     d trul33ds      e ds
003500090617     d kpjba         e ds
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
004800090617     d §numori         s              7  0 inz
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
008400031201     D*------------------
008500031201     D* Costanti
008600031201     D*------------------
008700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008900010330
009000091026
009100091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
009200091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
009300091026
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
015100050412     C                   if        not %open(titvl9ps)
015200050412     C                   open      titvl9ps
015300030715     C                   except    testdett
015400000613     C                   endif
015500010601     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200010601
016300010601
016400010601
016500010601     C*--------------------------------------------------------
016600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016700010601     C*--------------------------------------------------------
016800010601     C     ENDELA        BEGSR
016900990915     C*
017000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017100000613     C*
017200050412     C                   if        %open(titvl9ps)
017300000616     C                   except    findett
017400050412     C                   close     titvl9ps
017500000613     C                   endif
017600000616     C*
017700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017800011113     C                   Z-ADD     150           LENGH            15 5
017900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018000000616     C                   PARM                    CMD(3)
018100000616     C                   PARM                    LENGH
018200000616     C*
018300010601     C                   ENDSR
018400010601     C***
018500010601
018600010601
018700010601
018800010601
018900000613
019000000613     C*--------------------------------------------------------
019100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019200000613     C*--------------------------------------------------------
019300000621     C     STPR          BEGSR
019400000613     C*
019500050412     C                   if        not %open(titvl9p)
019600050412     C   50              open      titvl9p
019700990915     C                   endif
019800990915     C*
019900030715     C   50              except    riepilogo
020000990915     C*
020100050412     C                   if        %open(titvl9p)
020200050412     C   50              close     titvl9p
020300990914     C                   endif
020400000613     C*
020500000613     C                   ENDSR
020600000613     C***
020700000613
020800010330
020900010601
021000010601
021100010601
021200010330     C*--------------------------------------------------------
021300040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
021400010330     C*--------------------------------------------------------
021500040708     C     RWVAO         BEGSR
021600010330     C*
021700010330     C                   if        not %open(tivin00r)
021800010330     C                   open      tivin00r
021900010330     C                   endif
022000040708     C                   if        not %open(fnvaowwr)
022100040708     C                   open      fnvaowwr
022200010330     C                   endif
022300010330     C*
022400010604     C                   clear                   §CTROK
022500010604     C                   clear                   §CTRMO
022600010604     C                   clear                   §CTRNO
022700010330     C*
022800030822     C                   do        *HIVAL
022900010330     C*
023000030822     C                   read      tivin00r                               70
023100010618     C*
023200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023300010618     C                   if        vinflg = '1'
023400010618     C                   eval      flgOk = '1'
023500010618     C                   endif
023600010618     C*
023700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023800040510     C                   z-add     *zeros        wLenVINDTA        4 0
023900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024300040510     C                   z-add     1             wFlgVINDTA
024400040510     C                   leave
024500040510     C                   endif
024600040510     C                   enddo
024700040510     C*
024800040218     C                   if        vindta > *blanks AND
024900040510     C                             wFlgVINDTA = 1
025000010330     C                   add       1             rrnum
025100010330     C*
025200010601     C                   if        *in70 = *off and
025300010330     C                             (vinflg = *blanks
025400010330     C                              or vinflg = '0'
025500010330     C                              or vinflg = '2')
025600010330     C*
025700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025800010711     C                   if        vinflg = *blanks or vinflg = '0'
025900010711     C                   clear                   vinmsg
026000010711     C                   endif
026100010601     C*
026200010330     C                   exsr      inzvar
026300040708     C                   exsr      impvao
026400010601     C*
026500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600040708     C                   eval      depfil = VAOPOE
026700010601     C                   exsr      repfil
026800010601     C                   if        depfil = invfil
026900021025     C                   if        vlrpoi = 999
027000040708     C                   move(P)   invfil        VAOPOE
027100021025     C                   else
027200040708     C                   move(P)   vlrpoi        VAOPOE
027300021025     C                   endif
027400010601     C*
027500010604     C* Ebbene...
027600010604     C*
027700030822     C  N31              add       1             §CTROK            7 0
027800030822     C   32              add       1             §CTRMO            7 0
027900030822     C   31              add       1             §CTRNO            7 0
028000030822     C                   if        wGiro = 2
028100040723     C  N31              EXSR      CHKCOR
028200040723     C  N31              EXSR      WRIORI
028300050826     C  N31              EXSR      WRIVGD
028400040723     C  N31              write     FNVAO000
028500030822     C                   endif
028600010604     C*
028700010604     C                   if        *in31 = *off and
028800010604     C                             *in32 = *off
028900010604     C                   eval      vinflg = '1'
029000010604     C                   else
029100010604     C                   eval      recko = vindta
029200041011     C                   except    rigadett
029300010604     C                   eval      vinflg = '2'
029400010604     C                   endif
029500010604     C                   endif
029600010601     C*
029700010604     C                   endif
029800010604     C*
029900010330     C                   else
030000010330     C                   eval      vinflg = '1'
030100010330     C                   endif
030200010601     C*
030300010601     C  N70              update    tivin000
030400010330     C*
030500030822     C  N70              enddo
030600010601     C
030700010601     C*
030800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030900010601     C                   if        cntNonEl = *zeros or
031000010601     C                             flgMulti = '0'
031100010330     C* Se non ci sono record con errori ...
031200010601     C                   if        §ctrno = 0 and
031300010604     C                             §ctrmo = 0 and
031400010601     C                             flgStato <> '2'
031500010330     C* ... restituisco esito OK.
031600010330     C                   eval      wrkesito = '0'
031700010330     C                   else
031800010330     C                   if        §ctrok > 0
031900010330     C                   eval      wrkesito = '1'
032000010330     C                   else
032100010615     C                   if        flgOk = '0'
032200010615     C                   eval      wrkesito = '2'
032300010615     C                   else
032400010615     C                   eval      wrkesito = '6'
032500010615     C                   endif
032600010330     C                   endif
032700010330     C                   endif
032800010601     C                   else
032900010601     C                   eval      wrkesito = '9'
033000010601     C                   endif
033100010330     C*
033200010330     C                   if        %open(tivin00r)
033300010330     C                   close     tivin00r
033400010330     C                   endif
033500040708     C                   if        %open(fnvaowwr)
033600040708     C                   close     fnvaowwr
033700010330     C                   endif
033800010601     C*
033900010601     C                   if        vlrpoi <> 999
034000010601     C                   eval      invfil = vlrpoi
034100010601     C                   endif
034200010330     C*
034300010330     C                   if        §ctrok > 0
034400010601     C                             and invfil > *zeros
034500010330     C                   exsr      invio
034600010330     C                   endif
034700010601     C*
034800010618     C                   if        flgGiro = '1'
034900010601     C                   exsr      endela
035000010618     C                   endif
035100010330     C*
035200010330     C                   ENDSR
035300010330     C***
035400040723
035500040723
035600040723     C*------------------------------------------------------------------------*
035700040723     C* CHKCOR - Considerazioni su VAOCOR
035800040723     C*------------------------------------------------------------------------*
035900040723     C     CHKCOR        BEGSR
036000040723     C*
036100040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
036200040723     C                   if        vaoTCO = 'F'
036300040723     C* Quindi verifico se il codice ordinante non è valorizzato
036400040723     C                   if        vaoCOR = *zeros  AND
036500040723     C                             vaoRSO = *blanks AND
036600040723     C                             vaoINO = *blanks AND
036700040723     C                             vaoCAO = *blanks AND
036800040723     C                             vaoLOO = *blanks AND
036900040723     C                             vaoPRO = *blanks AND
037000040723     C                             vaoNAO = *blanks
037100040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
037200040723     C                   movel     '0'           wFlgAcr           1
037300040723     C                   move      vlrKSC        wCli              7 0
037400040723     C                   move(p)   wCli          acrCRO
037500040723     C     acrCRO        setll     fnacr01l
037600040723     C                   if        %found(fnacr01l)
037700040723     C                   read      fnacr01l
037800040723     C                   dow       not %eof(fnacr01l)
037900040723     C                   movel     acrCRO        wCliAcr           7 0
038000040723     C                   if        wCliAcr = wCli
038100040723     C                   eval      vaoCOR = acrCRO
038200040723     C                   movel     '1'           wFlgAcr
038300040723     C                   leave
038400040723     C                   else
038500040723     C                   leave
038600040723     C                   endif
038700040723     C                   read      fnacr01l
038800040723     C                   enddo
038900040723     C                   endif
039000040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
039100040723     C* dati anagrafici dell'ordinante dal piano dei conti
039200040723     C                   if        wFlgAcr = '0'
039300040723     C                   clear                   BS69DS
039400040723     C                   clear                   ACODS
039500040723     C                   clear                   INDDS
039600040723     C                   clear                   CLPDS
039700040723     C                   clear                   CLSDS
039800040723     C                   move(P)   vlrKSC        I69KAC
039900040723     C                   move(P)   vlrKSC        I69KIN
040000040723     C                   call      'TIBS69R'
040100040723     C                   parm                    BS69DS
040200040723     C                   parm                    ACODS
040300040723     C                   parm                    INDDS
040400040723     C                   parm                    CLPDS
040500040723     C                   parm                    CLSDS
040600040723     C     O69ERR        ifne      '1'
040700040723     C                   eval      vaoRSO = ACORAG
040800040723     C                   eval      vaoINO = INDVIA
040900040723     C                   movel(P)  INDCAP        vaoCAO
041000040723     C                   eval      vaoLOO = INDCIT
041100040723     C                   eval      vaoPRO = INDPRV
041200040723     C                   eval      vaoNAO = INDSTA
041300040723     C                   endif
041400040723     C                   endif
041500040723     C                   endif
041600040723     C                   endif
041700040723     C*
041800040723     C                   ENDSR
041900040723
042000040723
042100040723
042200040723     C*------------------------------------------------------------------------*
042300040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
042400040723     C*------------------------------------------------------------------------*
042500040723     C     WRIORI        BEGSR
042600040723     C*
042700090617     C* Come prima cosa stacco un numeratore da AZNUM
042800090617     C                   clear                   trul33ds
042900090617     C                   eval      I33OPE = *zeros
043000090617     C                   eval      I33CNU = 600
043100090617     C                   eval      I33NUM = 1
043200090617     C                   movel     TRUL33DS      KPJBU
043300090617     C                   call      'TRUL33R'
043400090617     C                   parm                    KPJBA
043500090617     C                   movel     KPJBU         TRUL33DS
043600090617     C                   if        O33ERR = *zeros
043700090617     C                   z-add     O33NRF        §numori
043800090618     c                   else
043900090618     c                   eval      §numori = *all'9'
044000090617     C                   endif
044100040723     C*
044200040723     C* Quindi imposto i campi "extra-VAO"
044300040723     C                   if        vlrKSC = '0ORM0000'
044400040723     C*
044500040723     C                   if        vaoCOR > *zeros
044600040723     C                   movel     vaoCOR        wrkCOR7           7 0
044700040723     C*
044800040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
044900040723     C* codice padre. Se è un padre assegno già il codice ordinante.
045000040723     C                   clear                   tibs10ds
045100040723     C                   eval      d10drf = datcor                              *data riferimento
045200040723     C                   eval      d10paf = 'P'                                 *cerca il padre
045300040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
045400091026     C*
045500091026     C* Preparazione chiamata al TIBS10R1
045600091026     C                   clear                   tibs10r1ds
045700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
045800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
045900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
046000091026     C*
046100091026     C                   call      'TIBS10R1'
046200091026     C                   parm                    tibs10r1ds
046300091026     C*
046400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
046500040723     C*
046600040723     C                   if        d10err <> *blanks or                         *No unif. o padre
046700040723     C                             d10cop =  *zeros  or
046800040723     C                             d10cop =  d10cod
046900040723     C                   move      d10cod        oriIDC
047000040723     C                   else                                                   *Figlio
047100040723     C                   move      d10cop        oriIDC
047200040723     C                   endif
047300040723     C*
047400040723     C                   else
047500040723     C                   eval      oriIDC = *zeros
047600040723     C                   endif
047700040723     C*
047800040723     C                   else
047900040723     C                   eval      oriIDC = vlrKSC
048000040723     C                   endif
048100040723     C*
048200040723     C                   eval      oriPRG = §numori
048300040723     C                   eval      oriDER = datcor
048400040723     C                   eval      oriOER = wn6
048500040723     C                   eval      oriPOE = vaoPOE
048600040723     C                   eval      oriNSR = vaoNSR
048700040723     C                   eval      oriNOR = vaoNOR
048800040723     C                   eval      oriNRV = vaoNRV
048900040723     C                   eval      oriFLO = *blanks
049000040723     C*
049100040723     C* Infine valorizzo la chiave esterna sul file FNVAO
049200040723     C                   eval      dorm01 = vaoFLO
049300040723     C                   movel     §numori       §ormpg
049400050412     C*
049500050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
049600050412     C                   movel     'S'           §ormfr
049700050531     C                   movel     'N'           §orcomc
049800050412     C*
049900050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
050000040723     C                   eval      vaoFLO = dorm01
050100040723     C*
050200040723     C* ...quindi scrivo il file TIORI00F
050300040723     C                   WRITE     TIORI000
050400040723     C*
050500040723     C                   ENDSR
050600050826
050700050826
050800050826
050900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
051000050826     C     wriVGD        BEGSR
051100050826     C*
051200050826     C* Reperisco la descrizione della fase da tabella
051300050826     C                   movel(p)  'FAR'         KteCOD
051400050826     C                   movel(p)  '000'         KteKE1
051500050826     C     KEYtbe        chain     tntbe01l
051600050826     C                   if        %found(tntbe01l)
051700050826     C                   movel(P)  tbeuni        DFAR
051800050826     C                   else
051900050826     C                   clear                   DFAR
052000050826     C                   endif
052100050826     C*
052200050826     C* Valorizzo buffer record
052300050826     C                   clear                   FNVAPDS
052400050826     C                   eval      vapIDC = oriIDC
052500050826     C                   eval      vapPOE = oriPOE
052600050826     C                   eval      vapRFA = vaoRFA
052700050826     C                   eval      vapPOG = oriPOE
052800050826     C                   eval      vapDAE = oriDER
052900050826     C                   eval      vapORE = oriOER
053000050826     C                   if        d§fardva <> *blanks
053100050826     C                   eval      vapDFA = d§fardva
053200050826     C                   else
053300050826     C                   eval      vapDFA = d§fardes
053400050826     C                   endif
053500050826     C*
053600050826     C                   clear                   tivgd000
053700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
053800050826     C                   eval      vgdTIP = 'VP'
053900050826     C                   eval      vgdKSU = vapIDC
054000050826     C                   eval      vgdTSC = 'WW'
054100050826     C                   eval      vgdDAT = datcor
054200050826     C                   eval      vgdPGM = 'TITVL9R'
054300060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
054400060306     C                   exsr      sr_Control
054500060306     C                   if        Cli_Abil = *on
054600060306     C                   eval      vgdKSU = woriIDC
054700050826     C                   write     tivgd000
054800060306     C                   endif
054900050826     C*
055000050826     C                   ENDSR
055100050826     C*------------------------------------------------------------------------*
055200010601
055300010601
055400010601
055500010330     C*----------------------------------------------------*
055600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055700010330     C*----------------------------------------------------*
055800010330     C     INZVAR        BEGSR
055900010330     C*
056000030822     C* Inizializzo variabili di wrk
056100020204     C                   Z-ADD     *zeros        Num5_0            5 0
056200030822     C*
056300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056400040708     C                   CLEAR                   FNVAO000
056500040723     C                   CLEAR                   TIORI000
056600030822     C                   CLEAR                   SkSplitCSV
056700030822     C                   EVAL      i = 1
056800030822     C                   EVAL      posDa = *zeros
056900030822     C                   EVAL      posA  = *zeros
057000030822     C*
057100030822     C* Reimposto i valori di default
057200030822     C                   EXSR      DEFCAM
057300030822     C*
057400010330     C                   ENDSR
057500010330     C*----------------------------------------------------*
057600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057700010330     C*----------------------------------------------------*
057800010330     C     DEFCAM        BEGSR
057900030822     C*
058000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058100030715     C* e delimitatore testo.
058200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058500030822     C*
058600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058800030715     C                   SETON                                        50
058900030715     C                   ELSE
059000030715     C                   SETOFF                                       50
059100030715     C                   ENDIF
059200040723     C*
059300040723     C* Imposto i valori d default d alcuni campi
059400050412     C                   EVAL      VAOCOR = 0633397000
059500050412     C                   EVAL      VAOCRC = 0633397000
059600050412     C                   EVAL      VAOKSC = 0633397
059700040723     C                   EVAL      VAOTCO = 'F'
059800040723     C                   EVAL      VAOTOR = 'S'
059900040723     C                   EVAL      VAOPAG = 'O'
060000050419     C                   EVAL      VAOPKG = 10
060100050419     C                   EVAL      VAOORR = 1100
060200030822     C*
060300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060400030822     C                   EVAL      posDaDft = 1
060500030822     C                   EVAL      posADft  = 0
060600030822     C                   EVAL      wGiroDft = 0
060700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060800030822     C                             posDaDft > 0
060900030822     C*
061000030822     C* Gestisco il 1° giro
061100030822     C                   IF        wGiroDft = 0
061200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061400030822     C* Incremento il contatore dei "giri"
061500030822     C                   EVAL      wGiroDft = 1
061600030822     C                   ELSE
061700030822     C                   EVAL      posDaDft = posADft
061800030822     C                   ENDIF
061900030822     C* Eseguo lo scan x trovare la fine del campo corrente
062000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062100030822     C*
062200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062400030822     C                   IF        posDaDft > 0 AND
062500030822     C                             posADft  > 0
062600040708     C* POE
062700040708     C                   IF        %subst(
062800040708     C                             %subst(vlrppt:posDaDft+1:
062900040708     C                             posADft-posDaDft-1):1:3)
063000040708     C                             = 'POE'
063100040708     C                   EVAL      PiStr=%trim(%subst(
063200040708     C                             %subst(vlrppt:posDaDft+1:
063300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400040708     C                   EXSR      CHKNUM
063500040708     C                   IF        PiInt=*on
063600040708     C                   Z-ADD     PiVal         VAOPOE
063700040708     C                   ENDIF
063800040708     C                   ENDIF
063900040708     C* TOR
064000040708     C                   IF        %subst(
064100040708     C                             %subst(vlrppt:posDaDft+1:
064200040708     C                             posADft-posDaDft-1):1:3)
064300040708     C                             = 'TOR'
064400040708     C                   EVAL      VAOTOR=%trim(%subst(
064500040708     C                             %subst(vlrppt:posDaDft+1:
064600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700040708     C                   ENDIF
064800040708     C* TCO
064900040708     C                   IF        %subst(
065000040708     C                             %subst(vlrppt:posDaDft+1:
065100040708     C                             posADft-posDaDft-1):1:3)
065200040708     C                             = 'TCO'
065300040708     C                   EVAL      VAOTCO=%trim(%subst(
065400040708     C                             %subst(vlrppt:posDaDft+1:
065500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600040708     C                   ENDIF
065700040707     C* COR
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100040707     C                             = 'COR'
066200030822     C                   EVAL      PiStr=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   EXSR      CHKNUM
066600030822     C                   IF        PiInt=*on
066700040708     C                   Z-ADD     PiVal         VAOCOR
066800030822     C                   ENDIF
066900030822     C                   ENDIF
067000040708     C* CRC
067100040708     C                   IF        %subst(
067200040708     C                             %subst(vlrppt:posDaDft+1:
067300040708     C                             posADft-posDaDft-1):1:3)
067400040708     C                             = 'CRC'
067500040708     C                   EVAL      PiStr=%trim(%subst(
067600040708     C                             %subst(vlrppt:posDaDft+1:
067700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800040708     C                   EXSR      CHKNUM
067900040708     C                   IF        PiInt=*on
068000040708     C                   Z-ADD     PiVal         VAOCRC
068100040708     C                   ENDIF
068200040708     C                   ENDIF
068300040708     C* FFD
068400040708     C                   IF        %subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):1:3)
068700040708     C                             = 'FFD'
068800040708     C                   EVAL      VAOFFD=%trim(%subst(
068900040708     C                             %subst(vlrppt:posDaDft+1:
069000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100040708     C                   ENDIF
069200040708     C* ORR
069300040708     C                   IF        %subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):1:3)
069600040708     C                             = 'ORR'
069700040708     C                   EVAL      PiStr=%trim(%subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000040708     C                   EXSR      CHKNUM
070100040708     C                   IF        PiInt=*on
070200040708     C                   Z-ADD     PiVal         VAOORR
070300040708     C                   ENDIF
070400040708     C                   ENDIF
070500040712     C* NCL
070600040712     C                   IF        %subst(
070700040712     C                             %subst(vlrppt:posDaDft+1:
070800040712     C                             posADft-posDaDft-1):1:3)
070900040712     C                             = 'NCL'
071000040712     C                   EVAL      PiStr=%trim(%subst(
071100040712     C                             %subst(vlrppt:posDaDft+1:
071200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300040712     C                   EXSR      CHKNUM
071400040712     C                   IF        PiInt=*on
071500040712     C                   Z-ADD     PiVal         VAONCL
071600040712     C                   ENDIF
071700040712     C                   ENDIF
071800040712     C* PKG
071900040712     C                   IF        %subst(
072000040712     C                             %subst(vlrppt:posDaDft+1:
072100040712     C                             posADft-posDaDft-1):1:3)
072200040712     C                             = 'PKG'
072300040712     C                   EVAL      PiStr=%trim(%subst(
072400040712     C                             %subst(vlrppt:posDaDft+1:
072500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600040712     C                   EXSR      CHKNUM
072700040712     C                   IF        PiNum=*on
072800040712     C                   Z-ADD     PiVal         VAOPKG
072900040712     C                   ENDIF
073000040712     C                   ENDIF
073100040712     C* VLM
073200040712     C                   IF        %subst(
073300040712     C                             %subst(vlrppt:posDaDft+1:
073400040712     C                             posADft-posDaDft-1):1:3)
073500040712     C                             = 'VLM'
073600040712     C                   EVAL      PiStr=%trim(%subst(
073700040712     C                             %subst(vlrppt:posDaDft+1:
073800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900040712     C                   EXSR      CHKNUM
074000040712     C                   IF        PiNum=*on
074100040712     C                   Z-ADD     PiVal         VAOVLM
074200040712     C                   ENDIF
074300040712     C                   ENDIF
074400040708     C* NAM
074500040708     C                   IF        %subst(
074600040708     C                             %subst(vlrppt:posDaDft+1:
074700040708     C                             posADft-posDaDft-1):1:3)
074800040708     C                             = 'NAM'
074900040708     C                   EVAL      VAONAM=%trim(%subst(
075000040708     C                             %subst(vlrppt:posDaDft+1:
075100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200040708     C                   ENDIF
075300040708     C* PAG
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700040708     C                             = 'PAG'
075800040708     C                   EVAL      VAOPAG=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200040708     C* KSC
076300040708     C                   IF        %subst(
076400040708     C                             %subst(vlrppt:posDaDft+1:
076500040708     C                             posADft-posDaDft-1):1:3)
076600040708     C                             = 'KSC'
076700040708     C                   EVAL      PiStr=%trim(%subst(
076800040708     C                             %subst(vlrppt:posDaDft+1:
076900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000040708     C                   EXSR      CHKNUM
077100040708     C                   IF        PiInt=*on
077200040708     C                   Z-ADD     PiVal         VAOKSC
077300040708     C                   ENDIF
077400040708     C                   ENDIF
077500040708     C* CTR
077600040708     C                   IF        %subst(
077700040708     C                             %subst(vlrppt:posDaDft+1:
077800040708     C                             posADft-posDaDft-1):1:3)
077900040708     C                             = 'CTR'
078000040708     C                   EVAL      VAOCTR=%trim(%subst(
078100040708     C                             %subst(vlrppt:posDaDft+1:
078200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300040708     C                   ENDIF
078400040708     C* POC
078500040708     C                   IF        %subst(
078600040708     C                             %subst(vlrppt:posDaDft+1:
078700040708     C                             posADft-posDaDft-1):1:3)
078800040708     C                             = 'POC'
078900040708     C                   EVAL      PiStr=%trim(%subst(
079000040708     C                             %subst(vlrppt:posDaDft+1:
079100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200040708     C                   EXSR      CHKNUM
079300040708     C                   IF        PiInt=*on
079400040708     C                   Z-ADD     PiVal         VAOPOC
079500040708     C                   ENDIF
079600040708     C                   ENDIF
079700040708     C* NO1
079800040708     C                   IF        %subst(
079900040708     C                             %subst(vlrppt:posDaDft+1:
080000040708     C                             posADft-posDaDft-1):1:3)
080100040708     C                             = 'NO1'
080200040708     C                   EVAL      VAONO1=%trim(%subst(
080300040708     C                             %subst(vlrppt:posDaDft+1:
080400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500040708     C                   ENDIF
080600040708     C* DDT
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000040708     C                             = 'DDT'
081100040708     C                   EVAL      VAODDT=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500040708     C* RFA
081600040708     C                   IF        %subst(
081700040708     C                             %subst(vlrppt:posDaDft+1:
081800040708     C                             posADft-posDaDft-1):1:3)
081900040708     C                             = 'RFA'
082000040708     C                   EVAL      VAORFA=%trim(%subst(
082100040708     C                             %subst(vlrppt:posDaDft+1:
082200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300040708     C                   ENDIF
082400040708     C* FLO
082500040708     C                   IF        %subst(
082600040708     C                             %subst(vlrppt:posDaDft+1:
082700040708     C                             posADft-posDaDft-1):1:3)
082800040708     C                             = 'FLO'
082900040708     C                   EVAL      VAOFLO=%trim(%subst(
083000040708     C                             %subst(vlrppt:posDaDft+1:
083100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083200040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
083300050314      * e posizione 7 del VAOFLO
083400040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
083500040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
083600050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
083700040708     C                   ENDIF
083800030822     C* ...
083900030822     C                   ENDIF
084000030822     C                   ENDDO
084100020204     C*
084200010330     C                   ENDSR
084300010607     C*----------------------------------------------------*
084400040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
084500010607     C*----------------------------------------------------*
084600040708     C     IMPVAO        BEGSR
084700010607     C*
084800010607     C                   SETOFF                                       3132
084900030515     C*
085000040708     C                   MOVEL     datcor        VAODAO
085100040708     C                   MOVEL     wn14          VAOOAO
085200050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085300050314     C     minu:maiu     XLATE     vindta        vindta
085400010607     C*
085500030715     C**********
085600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085700030715     C**********
085800030715     C                   IF        wGiro = *zeros
085900040709     C*
086000040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
086100040709     C                   EVAL      depvinDTA = vinDTA
086200040709     C*
086300040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
086400040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086500040709     C                                      %trim(intcol(3))
086600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086700050314     C**!!!minu:maiu     XLATE     vindta        vindta
086800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086900040324     C                   DOW       posDa <= %len(%trimr(vindta))
087000030715     C*
087100030715     C* Gestisco il 1° campo
087200030715     C                   IF        i = 1
087300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087400030715     C                   EVAL      posDa = 1
087500030715     C* Eseguo lo scan x trovare la fine del primo campo
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C* X i campi successivi al 1°
088100030715     C                   ELSE
088200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088300030715     C                   EVAL      posDa = posA + 1
088400030715     C* Eseguo lo scan x trovare la fine del campo corrente
088500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088600030715     C* Gestisco l'ultimo campo
088700030715     C                   IF        posA = *zeros
088800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C                   LEAVE
089300030715     C                   ELSE
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C                   ENDIF
089800030715     C                   ENDIF
089900030715     C* Incremento il contatore d campo
090000030715     C                   EVAL      i = i +1
090100030715     C                   ENDDO
090200030715     C*
090300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090400030715     C* delimitazione testo
090500030715     C                   EVAL      i = 1
090600030715     C                   DOW       i < %elem(SkSplitFLD)
090700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090900030715     C                   EVAL      i = i + 1
091000030715     C                   ENDDO
091100030715     C                   EVAL      wGiro = 1
091200040709     C* Ripristino la 1° riga originale
091300040709     C                   EVAL      vinDTA = depvinDTA
091400030715     C                   ELSE
091500030715     C                   EVAL      wGiro = 2
091600031204     C**********
091700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091800031204     C* d separatore campo
091900031204     C**********
092000040709     C                   EVAL      vindta = %trim(vindta)
092100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092200040709     C                   EVAL      vindta = %subst(vindta:2)
092300040709     C                   ENDDO
092400040709     C*
092500040709     C                   Z-ADD     *zeros        lunghInput        4 0
092600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
092700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
093000040709     C                   ENDDO
093100030715     C**********
093200030715     C* Effettuo lo split del campo dati d input
093300030715     C**********
093400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093500040324     C                   DOW       posDa <= %len(%trimr(vindta))
093600030715     C*
093700030715     C* Gestisco il 1° campo
093800030715     C                   IF        i = 1
093900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094000030715     C                   EVAL      posDa = 1
094100030715     C* Eseguo lo scan x trovare la fine del primo campo
094200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C* X i campi successivi al 1°
094700030715     C                   ELSE
094800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094900030715     C                   EVAL      posDa = posA + 1
095000031204     C* Verifico che nn vi sia il campo nn valorizzato
095100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095200031204     C* Se campo nn valorizzato skippo al prossimo
095300031204     C                   EVAL      SkSplitCSV(i) = *blanks
095400031204     C                   EVAL      posA  = posA + 1
095500031204     C                   ELSE
095600030715     C* Eseguo lo scan x trovare la fine del campo corrente
095700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095800030715     C* Gestisco l'ultimo campo
095900030715     C                   IF        posA = *zeros
096000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C                   LEAVE
096500030715     C                   ELSE
096600030715     C* A questo punto "estraggo" il campo corrente
096700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096800030715     C                                                    (posA-posDa))
096900030715     C                   ENDIF
097000031204     C                   ENDIF
097100030715     C                   ENDIF
097200030715     C* Incremento il contatore d campo
097300030715     C                   EVAL      i = i +1
097400030715     C                   ENDDO
097500030715     C*
097600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097700030715     C* delimitazione testo
097800030715     C                   EVAL      i = 1
097900030715     C                   DOW       i < %elem(SkSplitCSV)
098000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098200030715     C                   EVAL      i = i + 1
098300030715     C                   ENDDO
098400030715     C*
098500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098600030715     C                   EVAL      i = 1
098700030715     C                   DOW       i < %elem(SkSplitFLD)
098800030715     C*
098900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099100030715     C                             SkSplitCSV(i) <> *blanks
099200030721     C*** ATB
099300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
099400040708     C                   EVAL      VAOATB = SkSplitCSV(i)
099500030721     C                   ENDIF
099600040708     C*** TOR
099700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
099800040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
099900040708     C                   ENDIF
100000040708     C*** TCO
100100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
100200040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
100300040708     C                   ENDIF
100400040708     C*** RSO
100500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
100600040708     C                   EVAL      VAORSO = SkSplitCSV(i)
100700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100800040708     C     '@':'A'       XLATE     VAORSO        VAORSO
100900030715     C* ==
101000030715     C                   ENDIF
101100040708     C*** INO
101200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
101300040708     C                   EVAL      VAOINO = SkSplitCSV(i)
101400030715     C                   ENDIF
101500040708     C*** LOO
101600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
101700040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
101800030715     C                   ENDIF
101900040708     C*** PRO
102000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
102100040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
102200030715     C                   ENDIF
102300040708     C*** NAO
102400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
102500040708     C                   EVAL      VAONAO = SkSplitCSV(i)
102600030721     C                   ENDIF
102700040708     C*** RSR
102800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
102900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
103000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
103200040708     C* ==
103300040708     C                   IF        VAORSR = *blanks
103400040708     C                   SETON                                        31
103500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103600040708     C                             + ' ' + 'VAORSR'
103700040708     C                   ENDIF
103800040708     C                   ENDIF
103900040708     C*** INR
104000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
104100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
104200040708     C                   IF        VAOINR = *blanks
104300040708     C                   SETON                                        31
104400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104500040708     C                             + ' ' + 'VAOINR'
104600040708     C                   ENDIF
104700040708     C                   ENDIF
104800040708     C*** LOR
104900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
105000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
105100040708     C                   IF        VAOLOR = *blanks
105200040708     C                   SETON                                        31
105300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105400040708     C                             + ' ' + 'VAOLOR'
105500040708     C                   ENDIF
105600040708     C                   ENDIF
105700040708     C*** PRR
105800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
105900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
106000040708     C                   IF        VAOPRR = *blanks
106100040708     C                   SETON                                        31
106200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106300040708     C                             + ' ' + 'VAOPRR'
106400040708     C                   ENDIF
106500040708     C                   ENDIF
106600040708     C*** NAR
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
106800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
106900040708     C                   ENDIF
107000040708     C*** RER
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
107200040708     C                   EVAL      VAORER = SkSplitCSV(i)
107300040708     C                   ENDIF
107400040708     C*** TER
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
107600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
107700040708     C                   ENDIF
107800040708     C*** RSC
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
108000040708     C                   EVAL      VAORSC = SkSplitCSV(i)
108100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
108200040708     C     '@':'A'       XLATE     VAORSC        VAORSC
108300040708     C* ==
108400040708     C                   ENDIF
108500040708     C*** INC
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
108700040708     C                   EVAL      VAOINC = SkSplitCSV(i)
108800040708     C                   ENDIF
108900040708     C*** LOC
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
109100040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
109200040708     C                   ENDIF
109300040708     C*** PRC
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
109500040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
109600040708     C                   ENDIF
109700040708     C*** NAC
109800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
109900040708     C                   EVAL      VAONAC = SkSplitCSV(i)
110000040708     C                   ENDIF
110100040708     C*** FFD
110200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
110300040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
110400040708     C                   ENDIF
110500040708     C*** RMP
110600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
110700040708     C                   EVAL      VAORMP = SkSplitCSV(i)
110800040708     C                   ENDIF
110900040708     C*** NAM
111000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
111100040708     C                   EVAL      VAONAM = SkSplitCSV(i)
111200040708     C                   ENDIF
111300040708     C*** PAG
111400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
111500040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
111600040708     C                   ENDIF
111700040708     C*** CTR
111800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
111900040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
112000040708     C                   ENDIF
112100040708     C*** ZOR
112200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
112300040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
112400040708     C                   ENDIF
112500040708     C*** NO1
112600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
112700040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
112800030715     C                   ENDIF
112900040708     C*** NO2
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
113100040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
113200030715     C                   ENDIF
113300040708     C*** DDT
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
113500040708     C                   EVAL      VAODDT = SkSplitCSV(i)
113600030715     C                   ENDIF
113700040708     C*** RFA
113800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
113900040708     C                   EVAL      VAORFA = SkSplitCSV(i)
114000030715     C                   ENDIF
114100040708     C*** SPI
114200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
114300040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
114400030715     C                   ENDIF
114500040708     C*** FLO
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
114700040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
114800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
114900050314      * e la posizione 7 del VAOFLO
115000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
115100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
115200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
115300030715     C                   ENDIF
115400050314
115500050412      *** DAR
115600050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
115700050412     C                   EVAL      PiStr=SkSplitCSV(i)
115800050412     C                   EXSR      CHKNUM
115900050412     C                   IF        PiInt=*on
116000050412     C                   Z-ADD     PiVal         VAODAR
116100050412     C                   ELSE
116200050412     C                   SETON                                        32
116300050412     C                   EVAL      VAODAR = *zeros
116400050412     C                   EVAL      vinmsg = %trimr(vinmsg)
116500050412     C                             + ' ' + 'VAODAR'
116600050412     C                   ENDIF
116700050314     C                   ENDIF
116800030715     C
116900030721     C*
117000030721     C* Reperisco quindi i campi numerici...
117100040708     C*** POE
117200040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
117300040723     C                   EVAL      PiStr=SkSplitCSV(i)
117400040723     C                   EXSR      CHKNUM
117500040723     C                   IF        PiInt=*on    AND
117600040723     C                             PiVal<=999   AND
117700040723     C                             PiVal>*zeros
117800040723     C                   Z-ADD     PiVal         VAOPOE
117900040723     C                   ELSE
118000040723     C                   SETON                                        32
118100040723     C                   EVAL      VAOPOE = *zeros
118200040723     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040723     C                             + ' ' + 'VAOPOE'
118400040723     C                   ENDIF
118500040723     C                   ENDIF
118600040708     C*** COR
118700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
118800040708     C                   EVAL      PiStr=SkSplitCSV(i)
118900040708     C                   EXSR      CHKNUM
119000040708     C                   IF        PiInt=*on
119100040708     C                   Z-ADD     PiVal         VAOCOR
119200040708     C                   ELSE
119300040708     C                   SETON                                        31
119400040708     C                   EVAL      VAOCOR = *zeros
119500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040708     C                             + ' ' + 'VAOCOR'
119700040708     C                   ENDIF
119800040708     C                   ENDIF
119900040708     C*** CAO
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on
120400030721     C                   Z-ADD     PiVal         Num5_0
120500040708     C                   MOVEL(p)  Num5_0        VAOCAO
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800040708     C                   EVAL      VAOCAO = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000040708     C                             + ' ' + 'VAOCAO'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300040708     C*** CRA
121400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on
121800040708     C                   Z-ADD     PiVal         VAOCRA
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100040708     C                   EVAL      VAOCRA = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300040708     C                             + ' ' + 'VAOCRA'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600040708     C*** CAR
122700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
122800040708     C                   EVAL      PiStr=SkSplitCSV(i)
122900040708     C                   EXSR      CHKNUM
123000040708     C                   IF        PiInt=*on
123100040708     C                   Z-ADD     PiVal         Num5_0
123200040708     C                   MOVEL(p)  Num5_0        VAOCAR
123300040708     C                   ELSE
123400040708     C                   SETON                                        31
123500040708     C                   EVAL      VAOCAR = *zeros
123600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123700040708     C                             + ' ' + 'VAOCAR'
123800040708     C                   ENDIF
123900040708     C                   ENDIF
124000040708     C*** CRC
124100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
124200040708     C                   EVAL      PiStr=SkSplitCSV(i)
124300040708     C                   EXSR      CHKNUM
124400040708     C                   IF        PiInt=*on
124500040708     C                   Z-ADD     PiVal         VAOCRC
124600040708     C                   ELSE
124700040708     C                   SETON                                        32
124800040708     C                   EVAL      VAOCRC = *zeros
124900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125000040708     C                             + ' ' + 'VAOCRC'
125100040708     C                   ENDIF
125200040708     C                   ENDIF
125300040708     C*** CAC
125400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
125500040708     C                   EVAL      PiStr=SkSplitCSV(i)
125600040708     C                   EXSR      CHKNUM
125700040708     C                   IF        PiInt=*on
125800040708     C                   Z-ADD     PiVal         Num5_0
125900040708     C                   MOVEL(p)  Num5_0        VAOCAC
126000040708     C                   ELSE
126100040708     C                   SETON                                        32
126200040708     C                   EVAL      VAOCAC = *zeros
126300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126400040708     C                             + ' ' + 'VAOCAC'
126500040708     C                   ENDIF
126600040708     C                   ENDIF
126700040708     C*** DAR
126800050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
126900050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
127000050314     C**!!!              EXSR      CHKNUM
127100050314     C**!!!              IF        PiInt=*on
127200050314     C**!!!              Z-ADD     PiVal         VAODAR
127300050314     C**!!!              ELSE
127400050314     C**!!!              SETON                                        32
127500050314     C**!!!              EVAL      VAODAR = *zeros
127600050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
127700050314     C**!!!                        + ' ' + 'VAODAR'
127800050314     C**!!!              ENDIF
127900050314     C**!!!              ENDIF
128000040708     C*** ORR
128100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
128200040708     C                   EVAL      PiStr=SkSplitCSV(i)
128300040708     C                   EXSR      CHKNUM
128400040708     C                   IF        PiInt=*on
128500040708     C                   Z-ADD     PiVal         VAOORR
128600040708     C                   ELSE
128700040708     C                   SETON                                        32
128800040708     C                   EVAL      VAOORR = *zeros
128900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129000040708     C                             + ' ' + 'VAOORR'
129100040708     C                   ENDIF
129200040708     C                   ENDIF
129300030721     C*** NCL
129400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on
129800040723     C                   IF        PiVal > *zeros
129900040708     C                   Z-ADD     PiVal         VAONCL
130000040723     C                   ENDIF
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300040708     C                   EVAL      VAONCL = *zeros
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500040708     C                             + ' ' + 'VAONCL'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800040708     C*** PKG
130900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiNum=*on
131300040723     C                   IF        PiVal > *zeros
131400040708     C                   Z-ADD     PiVal         VAOPKG
131500040723     C                   ENDIF
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800040708     C                   EVAL      VAOPKG = *zeros
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000040708     C                             + ' ' + 'VAOPKG'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300040708     C*** VLM
132400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
132500040708     C                   EVAL      PiStr=SkSplitCSV(i)
132600040708     C                   EXSR      CHKNUM
132700040708     C                   IF        PiNum=*on
132800040708     C                   Z-ADD     PiVal         VAOVLM
132900040708     C                   ELSE
133000040708     C                   SETON                                        32
133100040708     C                   EVAL      VAOVLM = *zeros
133200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133300040708     C                             + ' ' + 'VAOVLM'
133400040708     C                   ENDIF
133500040708     C                   ENDIF
133600040708     C*** BNC
133700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
133800040708     C                   EVAL      PiStr=SkSplitCSV(i)
133900040708     C                   EXSR      CHKNUM
134000040708     C                   IF        PiInt=*on
134100040708     C                   Z-ADD     PiVal         VAOBNC
134200040708     C                   ELSE
134300040708     C                   SETON                                        32
134400040708     C                   EVAL      VAOBNC = *zeros
134500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134600040708     C                             + ' ' + 'VAOBNC'
134700040708     C                   ENDIF
134800040708     C                   ENDIF
134900040708     C*** BLC
135000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
135100040708     C                   EVAL      PiStr=SkSplitCSV(i)
135200040708     C                   EXSR      CHKNUM
135300040708     C                   IF        PiInt=*on
135400040708     C                   Z-ADD     PiVal         VAOBLC
135500040708     C                   ELSE
135600040708     C                   SETON                                        32
135700040708     C                   EVAL      VAOBLC = *zeros
135800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135900040708     C                             + ' ' + 'VAOBLC'
136000040708     C                   ENDIF
136100040708     C                   ENDIF
136200040708     C*** ATT
136300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
136400040708     C                   EVAL      PiStr=SkSplitCSV(i)
136500040708     C                   EXSR      CHKNUM
136600040708     C                   IF        PiInt=*on
136700040708     C                   Z-ADD     PiVal         VAOATT
136800040708     C                   ELSE
136900040708     C                   SETON                                        32
137000040708     C                   EVAL      VAOATT = *zeros
137100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137200040708     C                             + ' ' + 'VAOATT'
137300040708     C                   ENDIF
137400040708     C                   ENDIF
137500040708     C*** MTC
137600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
137700040708     C                   EVAL      PiStr=SkSplitCSV(i)
137800040708     C                   EXSR      CHKNUM
137900040708     C                   IF        PiInt=*on
138000040708     C                   Z-ADD     PiVal         VAOMTC
138100040708     C                   ELSE
138200040708     C                   SETON                                        32
138300040708     C                   EVAL      VAOMTC = *zeros
138400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138500040708     C                             + ' ' + 'VAOMTC'
138600040708     C                   ENDIF
138700040708     C                   ENDIF
138800040708     C*** KSC
138900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiInt=*on      AND
139300030721     C                             PiVal<=9999999 AND
139400030721     C                             PiVal>*zeros
139500040708     C                   Z-ADD     PiVal         VAOKSC
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800040708     C                   EVAL      VAOKSC = *zeros
139900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140000040708     C                             + ' ' + 'VAOKSC'
140100030721     C                   ENDIF
140200030721     C                   ENDIF
140300040708     C*** POR
140400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
140500030721     C                   EVAL      PiStr=SkSplitCSV(i)
140600030721     C                   EXSR      CHKNUM
140700030721     C                   IF        PiInt=*on    AND
140800030721     C                             PiVal<=999   AND
140900030721     C                             PiVal>*zeros
141000040708     C                   Z-ADD     PiVal         VAOPOR
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300040708     C                   EVAL      VAOPOR = *zeros
141400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141500040708     C                             + ' ' + 'VAOPOR'
141600030721     C                   ENDIF
141700030721     C                   ENDIF
141800040708     C*** POC
141900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
142000040708     C                   EVAL      PiStr=SkSplitCSV(i)
142100040708     C                   EXSR      CHKNUM
142200040708     C                   IF        PiInt=*on    AND
142300040708     C                             PiVal<=999   AND
142400040708     C                             PiVal>*zeros
142500040708     C                   Z-ADD     PiVal         VAOPOC
142600040708     C                   ELSE
142700040708     C                   SETON                                        32
142800040708     C                   EVAL      VAOPOC = *zeros
142900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143000040708     C                             + ' ' + 'VAOPOC'
143100040708     C                   ENDIF
143200040708     C                   ENDIF
143300040708     C*** STO
143400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
143500040708     C                   EVAL      PiStr=SkSplitCSV(i)
143600040708     C                   EXSR      CHKNUM
143700040708     C                   IF        PiInt=*on
143800040708     C                   Z-ADD     PiVal         VAOSTO
143900040708     C                   ELSE
144000040708     C                   SETON                                        32
144100040708     C                   EVAL      VAOSTO = *zeros
144200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144300040708     C                             + ' ' + 'VAOSTO'
144400040708     C                   ENDIF
144500040708     C                   ENDIF
144600030715     C*
144700030715     C                   ENDIF
144800030715     C                   EVAL      i = i + 1
144900030715     C                   ENDDO
145000040723     C*
145100030715     C                   ENDIF
145200020204     C*
145300010607     C                   ENDSR
145400010607     C*----------------------------------------------------*
145500020204
145600010330
145700010330
145800010330     C*----------------------------------------------------*
145900010330     C*  CONTROLLO NUMERICITA' CAMPI
146000010330     C*----------------------------------------------------*
146100010330     C     CHKNUM        BEGSR
146200010330     C*
146300010606     C                   IF        PiDecChr = *blanks
146400030715     C                   EVAL      PiDecChr = CharNUM
146500010606     C                   ENDIF
146600010606     C*
146700010606     C                   CALL(e)   'ISNUMERIC'
146800010330     C                   PARM                    PiStr            30
146900010606     C                   PARM                    PiDecChr          1
147000010330     C                   PARM      *ZEROS        PiVal            30 9
147100010330     C                   PARM      '0'           PiInt             1
147200010330     C                   PARM      '0'           PiNum             1
147300010330     C                   IF        %error
147400010606     C                   EVAL      PiNum=*off
147500010330     C                   ENDIF
147600010330     C*
147700010330     C                   ENDSR
147800010330     C***
147900010330
148000010601
148100010601
148200010601
148300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
148400010601     C     repfil        BEGSR
148500010601     C*
148600010601     C                   if        invfil = *zeros and
148700010601     C                             depfil > *zeros and
148800010629     C                             (vinflg = *blanks or
148900010629     C                              vinflg = *zeros)
149000010601     C
149100010601     C                   eval      invfil = depfil
149200010601     C                   endif
149300010601     C*
149400010601     C                   if        depfil <> invfil and
149500010601     C                             invfil > *zeros
149600010601     C                   eval      flgMulti = '1'
149700010601     C                   if        vinflg = *blanks
149800010601     C                   add       1             cntNonEl
149900010601     C                   endif
150000010601     C                   endif
150100010601     C*
150200010601     C                   if        vinflg = '2'
150300010601     C                   eval      flgStato = '2'
150400010601     C                   endif
150500010601     C*
150600010601     C                   ENDSR
150700010601     C***
150800010601
150900010601
151000010601
151100010330
151200010330
151300010330
151400990920      /TITLE Invio dei dati al punto operativo.
151500000613     C     invio         BEGSR
151600990920     C*
151700990920     C                   reset                   dscmz
151800010601     C                   move      invfil        cmzdst
151900990920     C                   eval      cmzfld = vlrfou
152000990920     C                   eval      cmzmbd = vlrhdl
152100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
152200000710     C                   if        prmfir = *blanks
152300040708     C                   eval      cmzfla = 'FNVAO00F'
152400040708     C                   eval      cmzmba = 'FNVAO00F'
152500000710     C                   else
152600000710     C                   eval      cmzfla = prmfir
152700000710     C                   eval      cmzmba = prmfir
152800000710     C                   endif
152900990920     C                   eval      cmznrr = *zeros
153000990920     C                   move      §ctrok        cmznrr
153100021018     C                   eval      cmzlba = vlrfl1
153200990920     C                   call(e)   'TIS711C'
153300990920     C                   parm                    dscmz
153400990921     C                   parm      *blanks       esito
153500990920     C                   if        %error
153600990920     C                             or cmzerr = '1'
153700990921     C                             or esito  = '1'
153800000710     C                   eval      wrkesito = '3'
153900990920     C                   endif
154000990920     C*
154100000613     C                   ENDSR
154200990910
154300010601
154400010601
154500010601
154600010601
154700010601      /TITLE Invio dei dati al punto operativo.
154800010601     C     opeini        BEGSR
154900010601     C*
155000010601     C* Inizializzo flag e contatori operativi
155100010601     C                   movel     '0'           flgGiro           1
155200010601     C                   movel     '0'           flgMulti          1
155300010601     C                   movel     '1'           flgStato          1
155400010615     C                   movel     '0'           flgOk             1
155500010601     C                   z-add     *zeros        cntNonEl         10 0
155600010601     C                   z-add     *zeros        depfil            3 0
155700010601     C                   z-add     *zeros        invfil            3 0
155800010601     C*
155900010601     C                   ENDSR
156000010601     C***
156100010601
156200010601
156300050421
156400050421
156500050421     C     *pssr         BEGSR
156600050421     C*
156700050421     C                   eval      wrkesito = '2'
156800050421     C*
156900050421     C                   ENDSR     '*CANCL'
157000050421     C***
157100060306
157200060306
157300060306     C*-------------------------------------------------------------------------*
157400060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
157500060306     C*-------------------------------------------------------------------------*
157600060306     C     sr_Control    begsr
157700060306     C*
157800060306     C* inizializzo a ok il flag d controllo abilitazione
157900060306     C                   eval      Cli_Abil = *on
158000060306     C
158100060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
158200060306     C                   clear                   woriIDC
158300060306     C                   eval      kteCOD = 'VAP'
158400060306     C                   eval      kteKE1 = oriIDC
158500060306     C     keytbe        chain     TNTBE01L
158600060306     C                   if        %found(Tntbe01l)
158700060306     C                   eval      woriIDC = TbeKe2
158800060306     C                   else
158900060306     C                   eval      Cli_Abil = *off
159000060306     C                   endif
159100060306     C*
159200060306     C                   endsr
159300050421
159400010601
159500010330
159600010330
159700000613     C     *inzsr        BEGSR
159800990910     C*
159900990910     C     *entry        plist
160000990920     C                   parm                    tivlrds
160100990921     C                   parm      wrkesito      esito
160200000724     C                   parm                    prmlit
160300000710     C                   parm                    prmfir
160400010330     C*
160500010330     C* CALCOLA LA DATA CORRENTE
160600010330     C                   time                    wn14             14 0
160700040723     C                   movel     wn14          wn6               6 0          *ORA (6)
160800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
160900010330     C                   z-add     wn8           g08dat
161000010330     C                   z-add     *zeros        g08inv
161100010330     C                   movel     '0'           g08err
161200010330     C                   call      'XSRDA8'
161300010330     C                   parm                    wlbda8
161400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161500040723     C*
161600040723     C* Campi riferito al database
161700040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
161800040723     C     *like         define    tbeke1        kteKE1
161900040723     C*
162000040723     C* Definizione chiavi di lettura
162100040723     C* TNTBE01L - parziale
162200040723     C     keytbe        klist
162300040723     C                   kfld                    kteCOD                         *UTENTE
162400040723     C                   kfld                    kteKE1                         *CODICE
162500000613     C*
162600000613     C                   ENDSR
162700000613     C***
162800990908
162900050412     Otitvl9p   E            riepilogo         2
163000990915     O                                              'Upload via Internet'
163100990915     O                                           +1 'Traduzione TIVIN00R -'
163200040708     O                                           +1 'FNVAOWWR'
163300990915     O                                           +1 'Testate bolle'
163400990915     O          E            riepilogo   2
163500990915     O                       wrkdata
163600990915     O                       wrkora              +1
163700990915     O                       procname            +1
163800990915     O          E            riepilogo   2
163900990915     O                                              'Cliente..................:'
164000040708     O                       VAOCOR        z     +1
164100990915     O          E            riepilogo   2
164200990920     O                                              'Riferimento Strategi.....:'
164300990920     O                       vlrhdl              +1
164400990915     O          E            riepilogo   2
164500990915     O                                              'Giusti...................:'
164600971022     O                       §CTROK        2   +  1
164700990915     O          E            riepilogo   2
164800990915     O                                              'Sbagliati e corretti.....:'
164900971022     O                       §CTRMO        2   +  1
165000990915     O          E            riepilogo   2
165100990915     O                                              'Sbagliati e scartati.....:'
165200971022     O                       §CTRNO        2   +  1
165300000613
165400050412     Otitvl9ps  E            testdett          2
165500000613     O                                              'Upload via Internet'
165600000613     O                                           +1 'Traduzione TIVIN00R -'
165700040708     O                                           +1 'FNVAOWWR'
165800000613     O                                           +1 'Testate bolle'
165900000616     O          E            testdett    3
166000000613     O                                           +2 'N° rec'
166100000613     O                                           +3 'Anteprima contenuto'
166200000616     O          E            rigadett    2
166300000613     O                       rrnum               +2
166400000621     O                       recko               +3
166500000616     O          E            findett     2
166600000613     O                       wrkdata
166700000613     O                       wrkora              +1
166800000613     O                       procname            +1
166900000616     O          E            findett     2
167000000613     O                                              'Cliente..................:'
167100040708     O                       VAOCOR        z     +1
167200000616     O          E            findett     2
167300000613     O                                              'Riferimento Strategi.....:'
167400000613     O                       vlrhdl              +1
167500000616     O          E            findett     2
167600000613     O                                              'Giusti...................:'
167700000613     O                       §CTROK        2   +  1
167800000616     O          E            findett     2
167900000613     O                                              'Sbagliati e corretti.....:'
168000000613     O                       §CTRMO        2   +  1
168100000616     O          E            findett     2
168200000613     O                                              'Sbagliati e scartati.....:'
168300000613     O                       §CTRNO        2   +  1
168400000613** CMD - COMANDI CL
168500050412OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
168600050412OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
168700050412DLTOVR FILE(TITVL9P TITVL9PS) LVL(*)
168800010606
168900000613
169000040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
169100050412VAONAM;FILLER1;FILLER2;VAORFA;FILLER3;VAORSR;VAOINR;VAOCAR;VAOLOR;
169200050412VAOPRR;VAOTER;FILLER4;VAORER;VAONCL;VAODAR
169300040709
