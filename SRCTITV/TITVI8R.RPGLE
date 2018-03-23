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
001100050314     Ftitvi8p   O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300050314     Ftitvi8ps  O    f  198        PRINTER usropn
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
015100050314     C                   if        not %open(titvi8ps)
015200050314     C                   open      titvi8ps
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
017200050314     C                   if        %open(titvi8ps)
017300000616     C                   except    findett
017400050314     C                   close     titvi8ps
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
019500050314     C                   if        not %open(titvi8p)
019600050314     C   50              open      titvi8p
019700990915     C                   endif
019800990915     C*
019900030715     C   50              except    riepilogo
020000990915     C*
020100050314     C                   if        %open(titvi8p)
020200050314     C   50              close     titvi8p
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
030600010601     C*
030700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030800010601     C                   if        cntNonEl = *zeros or
030900010601     C                             flgMulti = '0'
031000010330     C* Se non ci sono record con errori ...
031100010601     C                   if        §ctrno = 0 and
031200010604     C                             §ctrmo = 0 and
031300010601     C                             flgStato <> '2'
031400010330     C* ... restituisco esito OK.
031500010330     C                   eval      wrkesito = '0'
031600010330     C                   else
031700010330     C                   if        §ctrok > 0
031800010330     C                   eval      wrkesito = '1'
031900010330     C                   else
032000010615     C                   if        flgOk = '0'
032100010615     C                   eval      wrkesito = '2'
032200010615     C                   else
032300010615     C                   eval      wrkesito = '6'
032400010615     C                   endif
032500010330     C                   endif
032600010330     C                   endif
032700010601     C                   else
032800010601     C                   eval      wrkesito = '9'
032900010601     C                   endif
033000010330     C*
033100010330     C                   if        %open(tivin00r)
033200010330     C                   close     tivin00r
033300010330     C                   endif
033400040708     C                   if        %open(fnvaowwr)
033500040708     C                   close     fnvaowwr
033600010330     C                   endif
033700010601     C*
033800010601     C                   if        vlrpoi <> 999
033900010601     C                   eval      invfil = vlrpoi
034000010601     C                   endif
034100010330     C*
034200010330     C                   if        §ctrok > 0
034300010601     C                             and invfil > *zeros
034400010330     C                   exsr      invio
034500010330     C                   endif
034600010601     C*
034700010618     C                   if        flgGiro = '1'
034800010601     C                   exsr      endela
034900010618     C                   endif
035000010330     C*
035100010330     C                   ENDSR
035200010330     C***
035300040723
035400040723
035500040723     C*------------------------------------------------------------------------*
035600040723     C* CHKCOR - Considerazioni su VAOCOR
035700040723     C*------------------------------------------------------------------------*
035800040723     C     CHKCOR        BEGSR
035900040723     C*
036000040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
036100040723     C                   if        vaoTCO = 'F'
036200040723     C* Quindi verifico se il codice ordinante non è valorizzato
036300040723     C                   if        vaoCOR = *zeros  AND
036400040723     C                             vaoRSO = *blanks AND
036500040723     C                             vaoINO = *blanks AND
036600040723     C                             vaoCAO = *blanks AND
036700040723     C                             vaoLOO = *blanks AND
036800040723     C                             vaoPRO = *blanks AND
036900040723     C                             vaoNAO = *blanks
037000040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
037100040723     C                   movel     '0'           wFlgAcr           1
037200040723     C                   move      vlrKSC        wCli              7 0
037300040723     C                   move(p)   wCli          acrCRO
037400040723     C     acrCRO        setll     fnacr01l
037500040723     C                   if        %found(fnacr01l)
037600040723     C                   read      fnacr01l
037700040723     C                   dow       not %eof(fnacr01l)
037800040723     C                   movel     acrCRO        wCliAcr           7 0
037900040723     C                   if        wCliAcr = wCli
038000040723     C                   eval      vaoCOR = acrCRO
038100040723     C                   movel     '1'           wFlgAcr
038200040723     C                   leave
038300040723     C                   else
038400040723     C                   leave
038500040723     C                   endif
038600040723     C                   read      fnacr01l
038700040723     C                   enddo
038800040723     C                   endif
038900040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
039000040723     C* dati anagrafici dell'ordinante dal piano dei conti
039100040723     C                   if        wFlgAcr = '0'
039200040723     C                   clear                   BS69DS
039300040723     C                   clear                   ACODS
039400040723     C                   clear                   INDDS
039500040723     C                   clear                   CLPDS
039600040723     C                   clear                   CLSDS
039700040723     C                   move(P)   vlrKSC        I69KAC
039800040723     C                   move(P)   vlrKSC        I69KIN
039900040723     C                   call      'TIBS69R'
040000040723     C                   parm                    BS69DS
040100040723     C                   parm                    ACODS
040200040723     C                   parm                    INDDS
040300040723     C                   parm                    CLPDS
040400040723     C                   parm                    CLSDS
040500040723     C     O69ERR        ifne      '1'
040600040723     C                   eval      vaoRSO = ACORAG
040700040723     C                   eval      vaoINO = INDVIA
040800040723     C                   movel(P)  INDCAP        vaoCAO
040900040723     C                   eval      vaoLOO = INDCIT
041000040723     C                   eval      vaoPRO = INDPRV
041100040723     C                   eval      vaoNAO = INDSTA
041200040723     C                   endif
041300040723     C                   endif
041400040723     C                   endif
041500040723     C                   endif
041600040723     C*
041700040723     C                   ENDSR
041800040723
041900040723
042000040723
042100040723     C*------------------------------------------------------------------------*
042200040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
042300040723     C*------------------------------------------------------------------------*
042400040723     C     WRIORI        BEGSR
042500040723     C*
042600090617     C* Come prima cosa stacco un numeratore da AZNUM
042700090617     C                   clear                   trul33ds
042800090617     C                   eval      I33OPE = *zeros
042900090617     C                   eval      I33CNU = 600
043000090617     C                   eval      I33NUM = 1
043100090617     C                   movel     TRUL33DS      KPJBU
043200090617     C                   call      'TRUL33R'
043300090617     C                   parm                    KPJBA
043400090617     C                   movel     KPJBU         TRUL33DS
043500090617     C                   if        O33ERR = *zeros
043600090617     C                   z-add     O33NRF        §numori
043700090618     c                   else
043800090618     c                   eval      §numori = *all'9'
043900090617     C                   endif
044000040723     C*
044100040723     C* Quindi imposto i campi "extra-VAO"
044200040723     C                   if        vlrKSC = '0ORM0000'
044300040723     C*
044400040723     C                   if        vaoCOR > *zeros
044500040723     C                   movel     vaoCOR        wrkCOR7           7 0
044600040723     C*
044700040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
044800040723     C* codice padre. Se è un padre assegno già il codice ordinante.
044900040723     C                   clear                   tibs10ds
045000040723     C                   eval      d10drf = datcor                              *data riferimento
045100040723     C                   eval      d10paf = 'P'                                 *cerca il padre
045200040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
045300091026     C*
045400091026     C* Preparazione chiamata al TIBS10R1
045500091026     C                   clear                   tibs10r1ds
045600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
045700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
045800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
045900091026     C*
046000091026     C                   call      'TIBS10R1'
046100091026     C                   parm                    tibs10r1ds
046200091026     C*
046300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
046400040723     C*
046500040723     C                   if        d10err <> *blanks or                         *No unif. o padre
046600040723     C                             d10cop =  *zeros  or
046700040723     C                             d10cop =  d10cod
046800040723     C                   move      d10cod        oriIDC
046900040723     C                   else                                                   *Figlio
047000040723     C                   move      d10cop        oriIDC
047100040723     C                   endif
047200040723     C*
047300040723     C                   else
047400040723     C                   eval      oriIDC = *zeros
047500040723     C                   endif
047600040723     C*
047700040723     C                   else
047800040723     C                   eval      oriIDC = vlrKSC
047900040723     C                   endif
048000040723     C*
048100040723     C                   eval      oriPRG = §numori
048200040723     C                   eval      oriDER = datcor
048300040723     C                   eval      oriOER = wn6
048400040723     C                   eval      oriPOE = vaoPOE
048500040723     C                   eval      oriNSR = vaoNSR
048600040723     C                   eval      oriNOR = vaoNOR
048700040723     C                   eval      oriNRV = vaoNRV
048800040723     C                   eval      oriFLO = *blanks
048900040723     C*
049000040723     C* Infine valorizzo la chiave esterna sul file FNVAO
049100040723     C                   eval      dorm01 = vaoFLO
049200040723     C                   movel     §numori       §ormpg
049300040723     C                   eval      vaoFLO = dorm01
049400040723     C*
049500040723     C* ...quindi scrivo il file TIORI00F
049600040723     C                   WRITE     TIORI000
049700040723     C*
049800040723     C                   ENDSR
049900050826
050000050826
050100050826
050200050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
050300050826     C     wriVGD        BEGSR
050400050826     C*
050500050826     C* Reperisco la descrizione della fase da tabella
050600050826     C                   movel(p)  'FAR'         KteCOD
050700050826     C                   movel(p)  '000'         KteKE1
050800050826     C     KEYtbe        chain     tntbe01l
050900050826     C                   if        %found(tntbe01l)
051000050826     C                   movel(P)  tbeuni        DFAR
051100050826     C                   else
051200050826     C                   clear                   DFAR
051300050826     C                   endif
051400050826     C*
051500050826     C* Valorizzo buffer record
051600050826     C                   clear                   FNVAPDS
051700050826     C                   eval      vapIDC = oriIDC
051800050826     C                   eval      vapPOE = oriPOE
051900050826     C                   eval      vapRFA = vaoRFA
052000050826     C                   eval      vapPOG = oriPOE
052100050826     C                   eval      vapDAE = oriDER
052200050826     C                   eval      vapORE = oriOER
052300050826     C                   if        d§fardva <> *blanks
052400050826     C                   eval      vapDFA = d§fardva
052500050826     C                   else
052600050826     C                   eval      vapDFA = d§fardes
052700050826     C                   endif
052800050826     C*
052900050826     C                   clear                   tivgd000
053000080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
053100050826     C                   eval      vgdTIP = 'VP'
053200050826     C                   eval      vgdKSU = vapIDC
053300050826     C                   eval      vgdTSC = 'WW'
053400050826     C                   eval      vgdDAT = datcor
053500050826     C                   eval      vgdPGM = 'TITVI8R'
053600060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
053700060306     C                   exsr      sr_Control
053800060306     C                   if        Cli_Abil = *on
053900060306     C                   eval      vgdKSU = woriIDC
054000050826     C                   write     tivgd000
054100060306     C                   endif
054200050826     C*
054300050826     C                   ENDSR
054400050826     C*------------------------------------------------------------------------*
054500010601
054600010601
054700010601
054800010330     C*----------------------------------------------------*
054900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055000010330     C*----------------------------------------------------*
055100010330     C     INZVAR        BEGSR
055200010330     C*
055300030822     C* Inizializzo variabili di wrk
055400020204     C                   Z-ADD     *zeros        Num5_0            5 0
055500030822     C*
055600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055700040708     C                   CLEAR                   FNVAO000
055800040723     C                   CLEAR                   TIORI000
055900030822     C                   CLEAR                   SkSplitCSV
056000030822     C                   EVAL      i = 1
056100030822     C                   EVAL      posDa = *zeros
056200030822     C                   EVAL      posA  = *zeros
056300030822     C*
056400030822     C* Reimposto i valori di default
056500030822     C                   EXSR      DEFCAM
056600030822     C*
056700010330     C                   ENDSR
056800010330     C*----------------------------------------------------*
056900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057000010330     C*----------------------------------------------------*
057100010330     C     DEFCAM        BEGSR
057200030822     C*
057300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057400030715     C* e delimitatore testo.
057500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057800030822     C*
057900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058100030715     C                   SETON                                        50
058200030715     C                   ELSE
058300030715     C                   SETOFF                                       50
058400030715     C                   ENDIF
058500040723     C*
058600040723     C* Imposto i valori d default d alcuni campi
058700050408     C                   EVAL      VAOCOR = 1200707000
058800050314     C                   EVAL      VAOPKG = 1
058900040723     C                   EVAL      VAOTCO = 'F'
059000040723     C                   EVAL      VAOTOR = 'S'
059100040723     C                   EVAL      VAOPAG = 'O'
059200040723     C                   EVAL      VAODDT = 'S'
059300050408     C                   EVAL      VAOCTR = '200'
059400050314     C                   EVAL      VAOORR = 1000
059500050314     C                   EVAL      VAONAM = 'MERCE DA RIPARARE'
059600030822     C*
059700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059800030822     C                   EVAL      posDaDft = 1
059900030822     C                   EVAL      posADft  = 0
060000030822     C                   EVAL      wGiroDft = 0
060100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060200030822     C                             posDaDft > 0
060300030822     C*
060400030822     C* Gestisco il 1° giro
060500030822     C                   IF        wGiroDft = 0
060600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060800030822     C* Incremento il contatore dei "giri"
060900030822     C                   EVAL      wGiroDft = 1
061000030822     C                   ELSE
061100030822     C                   EVAL      posDaDft = posADft
061200030822     C                   ENDIF
061300030822     C* Eseguo lo scan x trovare la fine del campo corrente
061400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061500030822     C*
061600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061800030822     C                   IF        posDaDft > 0 AND
061900030822     C                             posADft  > 0
062000040708     C* POE
062100040708     C                   IF        %subst(
062200040708     C                             %subst(vlrppt:posDaDft+1:
062300040708     C                             posADft-posDaDft-1):1:3)
062400040708     C                             = 'POE'
062500040708     C                   EVAL      PiStr=%trim(%subst(
062600040708     C                             %subst(vlrppt:posDaDft+1:
062700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800040708     C                   EXSR      CHKNUM
062900040708     C                   IF        PiInt=*on
063000040708     C                   Z-ADD     PiVal         VAOPOE
063100040708     C                   ENDIF
063200040708     C                   ENDIF
063300040708     C* TOR
063400040708     C                   IF        %subst(
063500040708     C                             %subst(vlrppt:posDaDft+1:
063600040708     C                             posADft-posDaDft-1):1:3)
063700040708     C                             = 'TOR'
063800040708     C                   EVAL      VAOTOR=%trim(%subst(
063900040708     C                             %subst(vlrppt:posDaDft+1:
064000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100040708     C                   ENDIF
064200040708     C* TCO
064300040708     C                   IF        %subst(
064400040708     C                             %subst(vlrppt:posDaDft+1:
064500040708     C                             posADft-posDaDft-1):1:3)
064600040708     C                             = 'TCO'
064700040708     C                   EVAL      VAOTCO=%trim(%subst(
064800040708     C                             %subst(vlrppt:posDaDft+1:
064900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000040708     C                   ENDIF
065100040707     C* COR
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500040707     C                             = 'COR'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiInt=*on
066100040708     C                   Z-ADD     PiVal         VAOCOR
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400040708     C* CRC
066500040708     C                   IF        %subst(
066600040708     C                             %subst(vlrppt:posDaDft+1:
066700040708     C                             posADft-posDaDft-1):1:3)
066800040708     C                             = 'CRC'
066900040708     C                   EVAL      PiStr=%trim(%subst(
067000040708     C                             %subst(vlrppt:posDaDft+1:
067100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200040708     C                   EXSR      CHKNUM
067300040708     C                   IF        PiInt=*on
067400040708     C                   Z-ADD     PiVal         VAOCRC
067500040708     C                   ENDIF
067600040708     C                   ENDIF
067700040708     C* FFD
067800040708     C                   IF        %subst(
067900040708     C                             %subst(vlrppt:posDaDft+1:
068000040708     C                             posADft-posDaDft-1):1:3)
068100040708     C                             = 'FFD'
068200040708     C                   EVAL      VAOFFD=%trim(%subst(
068300040708     C                             %subst(vlrppt:posDaDft+1:
068400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500040708     C                   ENDIF
068600040708     C* ORR
068700040708     C                   IF        %subst(
068800040708     C                             %subst(vlrppt:posDaDft+1:
068900040708     C                             posADft-posDaDft-1):1:3)
069000040708     C                             = 'ORR'
069100040708     C                   EVAL      PiStr=%trim(%subst(
069200040708     C                             %subst(vlrppt:posDaDft+1:
069300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400040708     C                   EXSR      CHKNUM
069500040708     C                   IF        PiInt=*on
069600040708     C                   Z-ADD     PiVal         VAOORR
069700040708     C                   ENDIF
069800040708     C                   ENDIF
069900040712     C* NCL
070000040712     C                   IF        %subst(
070100040712     C                             %subst(vlrppt:posDaDft+1:
070200040712     C                             posADft-posDaDft-1):1:3)
070300040712     C                             = 'NCL'
070400040712     C                   EVAL      PiStr=%trim(%subst(
070500040712     C                             %subst(vlrppt:posDaDft+1:
070600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700040712     C                   EXSR      CHKNUM
070800040712     C                   IF        PiInt=*on
070900040712     C                   Z-ADD     PiVal         VAONCL
071000040712     C                   ENDIF
071100040712     C                   ENDIF
071200040712     C* PKG
071300040712     C                   IF        %subst(
071400040712     C                             %subst(vlrppt:posDaDft+1:
071500040712     C                             posADft-posDaDft-1):1:3)
071600040712     C                             = 'PKG'
071700040712     C                   EVAL      PiStr=%trim(%subst(
071800040712     C                             %subst(vlrppt:posDaDft+1:
071900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000040712     C                   EXSR      CHKNUM
072100040712     C                   IF        PiNum=*on
072200040712     C                   Z-ADD     PiVal         VAOPKG
072300040712     C                   ENDIF
072400040712     C                   ENDIF
072500040712     C* VLM
072600040712     C                   IF        %subst(
072700040712     C                             %subst(vlrppt:posDaDft+1:
072800040712     C                             posADft-posDaDft-1):1:3)
072900040712     C                             = 'VLM'
073000040712     C                   EVAL      PiStr=%trim(%subst(
073100040712     C                             %subst(vlrppt:posDaDft+1:
073200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300040712     C                   EXSR      CHKNUM
073400040712     C                   IF        PiNum=*on
073500040712     C                   Z-ADD     PiVal         VAOVLM
073600040712     C                   ENDIF
073700040712     C                   ENDIF
073800040708     C* NAM
073900040708     C                   IF        %subst(
074000040708     C                             %subst(vlrppt:posDaDft+1:
074100040708     C                             posADft-posDaDft-1):1:3)
074200040708     C                             = 'NAM'
074300040708     C                   EVAL      VAONAM=%trim(%subst(
074400040708     C                             %subst(vlrppt:posDaDft+1:
074500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600040708     C                   ENDIF
074700040708     C* PAG
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100040708     C                             = 'PAG'
075200040708     C                   EVAL      VAOPAG=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600040708     C* KSC
075700040708     C                   IF        %subst(
075800040708     C                             %subst(vlrppt:posDaDft+1:
075900040708     C                             posADft-posDaDft-1):1:3)
076000040708     C                             = 'KSC'
076100040708     C                   EVAL      PiStr=%trim(%subst(
076200040708     C                             %subst(vlrppt:posDaDft+1:
076300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400040708     C                   EXSR      CHKNUM
076500040708     C                   IF        PiInt=*on
076600040708     C                   Z-ADD     PiVal         VAOKSC
076700040708     C                   ENDIF
076800040708     C                   ENDIF
076900040708     C* CTR
077000040708     C                   IF        %subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):1:3)
077300040708     C                             = 'CTR'
077400040708     C                   EVAL      VAOCTR=%trim(%subst(
077500040708     C                             %subst(vlrppt:posDaDft+1:
077600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700040708     C                   ENDIF
077800040708     C* POC
077900040708     C                   IF        %subst(
078000040708     C                             %subst(vlrppt:posDaDft+1:
078100040708     C                             posADft-posDaDft-1):1:3)
078200040708     C                             = 'POC'
078300040708     C                   EVAL      PiStr=%trim(%subst(
078400040708     C                             %subst(vlrppt:posDaDft+1:
078500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600040708     C                   EXSR      CHKNUM
078700040708     C                   IF        PiInt=*on
078800040708     C                   Z-ADD     PiVal         VAOPOC
078900040708     C                   ENDIF
079000040708     C                   ENDIF
079100040708     C* NO1
079200040708     C                   IF        %subst(
079300040708     C                             %subst(vlrppt:posDaDft+1:
079400040708     C                             posADft-posDaDft-1):1:3)
079500040708     C                             = 'NO1'
079600040708     C                   EVAL      VAONO1=%trim(%subst(
079700040708     C                             %subst(vlrppt:posDaDft+1:
079800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900040708     C                   ENDIF
080000040708     C* DDT
080100030822     C                   IF        %subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):1:3)
080400040708     C                             = 'DDT'
080500040708     C                   EVAL      VAODDT=%trim(%subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800030822     C                   ENDIF
080900040708     C* RFA
081000040708     C                   IF        %subst(
081100040708     C                             %subst(vlrppt:posDaDft+1:
081200040708     C                             posADft-posDaDft-1):1:3)
081300040708     C                             = 'RFA'
081400040708     C                   EVAL      VAORFA=%trim(%subst(
081500040708     C                             %subst(vlrppt:posDaDft+1:
081600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081700040708     C                   ENDIF
081800040708     C* FLO
081900040708     C                   IF        %subst(
082000040708     C                             %subst(vlrppt:posDaDft+1:
082100040708     C                             posADft-posDaDft-1):1:3)
082200040708     C                             = 'FLO'
082300040708     C                   EVAL      VAOFLO=%trim(%subst(
082400040708     C                             %subst(vlrppt:posDaDft+1:
082500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
082700050314      * e posizione 7 del VAOFLO
082800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
082900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
083000050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
083100040708     C                   ENDIF
083200030822     C* ...
083300030822     C                   ENDIF
083400030822     C                   ENDDO
083500020204     C*
083600010330     C                   ENDSR
083700010607     C*----------------------------------------------------*
083800040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
083900010607     C*----------------------------------------------------*
084000040708     C     IMPVAO        BEGSR
084100010607     C*
084200010607     C                   SETOFF                                       3132
084300030515     C*
084400040708     C                   MOVEL     datcor        VAODAO
084500040708     C                   MOVEL     wn14          VAOOAO
084600050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084700050314     C     minu:maiu     XLATE     vindta        vindta
084800010607     C*
084900030715     C**********
085000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085100030715     C**********
085200030715     C                   IF        wGiro = *zeros
085300040709     C*
085400040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
085500040709     C                   EVAL      depvinDTA = vinDTA
085600040709     C*
085700040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
085800040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085900040709     C                                      %trim(intcol(3))
086000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086100050314     C**!!!minu:maiu     XLATE     vindta        vindta
086200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086300040324     C                   DOW       posDa <= %len(%trimr(vindta))
086400030715     C*
086500030715     C* Gestisco il 1° campo
086600030715     C                   IF        i = 1
086700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086800030715     C                   EVAL      posDa = 1
086900030715     C* Eseguo lo scan x trovare la fine del primo campo
087000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C* X i campi successivi al 1°
087500030715     C                   ELSE
087600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087700030715     C                   EVAL      posDa = posA + 1
087800030715     C* Eseguo lo scan x trovare la fine del campo corrente
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* Gestisco l'ultimo campo
088100030715     C                   IF        posA = *zeros
088200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C                   LEAVE
088700030715     C                   ELSE
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   ENDIF
089200030715     C                   ENDIF
089300030715     C* Incremento il contatore d campo
089400030715     C                   EVAL      i = i +1
089500030715     C                   ENDDO
089600030715     C*
089700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089800030715     C* delimitazione testo
089900030715     C                   EVAL      i = 1
090000030715     C                   DOW       i < %elem(SkSplitFLD)
090100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090300030715     C                   EVAL      i = i + 1
090400030715     C                   ENDDO
090500030715     C                   EVAL      wGiro = 1
090600040709     C* Ripristino la 1° riga originale
090700040709     C                   EVAL      vinDTA = depvinDTA
090800030715     C                   ELSE
090900030715     C                   EVAL      wGiro = 2
091000031204     C**********
091100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091200031204     C* d separatore campo
091300031204     C**********
091400040709     C                   EVAL      vindta = %trim(vindta)
091500040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091600040709     C                   EVAL      vindta = %subst(vindta:2)
091700040709     C                   ENDDO
091800040709     C*
091900040709     C                   Z-ADD     *zeros        lunghInput        4 0
092000040709     C                   EVAL      lunghInput = %len(%trim(vindta))
092100040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092200040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092300040709     C                   EVAL      lunghInput = %len(%trim(vindta))
092400040709     C                   ENDDO
092500030715     C**********
092600030715     C* Effettuo lo split del campo dati d input
092700030715     C**********
092800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092900040324     C                   DOW       posDa <= %len(%trimr(vindta))
093000030715     C*
093100030715     C* Gestisco il 1° campo
093200030715     C                   IF        i = 1
093300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093400030715     C                   EVAL      posDa = 1
093500030715     C* Eseguo lo scan x trovare la fine del primo campo
093600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093700030715     C* A questo punto "estraggo" il campo corrente
093800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093900030715     C                                                    (posA-posDa))
094000030715     C* X i campi successivi al 1°
094100030715     C                   ELSE
094200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094300030715     C                   EVAL      posDa = posA + 1
094400031204     C* Verifico che nn vi sia il campo nn valorizzato
094500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094600031204     C* Se campo nn valorizzato skippo al prossimo
094700031204     C                   EVAL      SkSplitCSV(i) = *blanks
094800031204     C                   EVAL      posA  = posA + 1
094900031204     C                   ELSE
095000030715     C* Eseguo lo scan x trovare la fine del campo corrente
095100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095200030715     C* Gestisco l'ultimo campo
095300030715     C                   IF        posA = *zeros
095400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095500030715     C* A questo punto "estraggo" il campo corrente
095600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095700030715     C                                                    (posA-posDa))
095800030715     C                   LEAVE
095900030715     C                   ELSE
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C                   ENDIF
096400031204     C                   ENDIF
096500030715     C                   ENDIF
096600030715     C* Incremento il contatore d campo
096700030715     C                   EVAL      i = i +1
096800030715     C                   ENDDO
096900030715     C*
097000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097100030715     C* delimitazione testo
097200030715     C                   EVAL      i = 1
097300030715     C                   DOW       i < %elem(SkSplitCSV)
097400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097600030715     C                   EVAL      i = i + 1
097700030715     C                   ENDDO
097800030715     C*
097900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098000030715     C                   EVAL      i = 1
098100030715     C                   DOW       i < %elem(SkSplitFLD)
098200030715     C*
098300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098500030715     C                             SkSplitCSV(i) <> *blanks
098600030721     C*** ATB
098700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
098800040708     C                   EVAL      VAOATB = SkSplitCSV(i)
098900030721     C                   ENDIF
099000040708     C*** TOR
099100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
099200040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
099300040708     C                   ENDIF
099400040708     C*** TCO
099500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
099600040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
099700040708     C                   ENDIF
099800040708     C*** RSO
099900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
100000040708     C                   EVAL      VAORSO = SkSplitCSV(i)
100100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100200040708     C     '@':'A'       XLATE     VAORSO        VAORSO
100300030715     C* ==
100400030715     C                   ENDIF
100500040708     C*** INO
100600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
100700040708     C                   EVAL      VAOINO = SkSplitCSV(i)
100800030715     C                   ENDIF
100900040708     C*** LOO
101000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
101100040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
101200030715     C                   ENDIF
101300040708     C*** PRO
101400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
101500040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
101600030715     C                   ENDIF
101700040708     C*** NAO
101800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
101900040708     C                   EVAL      VAONAO = SkSplitCSV(i)
102000030721     C                   ENDIF
102100040708     C*** RSR
102200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
102300040708     C                   EVAL      VAORSR = SkSplitCSV(i)
102400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102500040708     C     '@':'A'       XLATE     VAORSR        VAORSR
102600040708     C* ==
102700040708     C                   IF        VAORSR = *blanks
102800040708     C                   SETON                                        31
102900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103000040708     C                             + ' ' + 'VAORSR'
103100040708     C                   ENDIF
103200040708     C                   ENDIF
103300040708     C*** INR
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
103500040708     C                   EVAL      VAOINR = SkSplitCSV(i)
103600040708     C                   IF        VAOINR = *blanks
103700040708     C                   SETON                                        31
103800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103900040708     C                             + ' ' + 'VAOINR'
104000040708     C                   ENDIF
104100040708     C                   ENDIF
104200040708     C*** LOR
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
104400040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
104500040708     C                   IF        VAOLOR = *blanks
104600040708     C                   SETON                                        31
104700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104800040708     C                             + ' ' + 'VAOLOR'
104900040708     C                   ENDIF
105000040708     C                   ENDIF
105100040708     C*** PRR
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
105300040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
105400040708     C                   IF        VAOPRR = *blanks
105500040708     C                   SETON                                        31
105600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105700040708     C                             + ' ' + 'VAOPRR'
105800040708     C                   ENDIF
105900040708     C                   ENDIF
106000040708     C*** NAR
106100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
106200040708     C                   EVAL      VAONAR = SkSplitCSV(i)
106300040708     C                   ENDIF
106400040708     C*** RER
106500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
106600040708     C                   EVAL      VAORER = SkSplitCSV(i)
106700040708     C                   ENDIF
106800040708     C*** TER
106900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
107000040708     C                   EVAL      VAOTER = SkSplitCSV(i)
107100040708     C                   ENDIF
107200040708     C*** RSC
107300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
107400040708     C                   EVAL      VAORSC = SkSplitCSV(i)
107500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107600040708     C     '@':'A'       XLATE     VAORSC        VAORSC
107700040708     C* ==
107800040708     C                   ENDIF
107900040708     C*** INC
108000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
108100040708     C                   EVAL      VAOINC = SkSplitCSV(i)
108200040708     C                   ENDIF
108300040708     C*** LOC
108400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
108500040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
108600040708     C                   ENDIF
108700040708     C*** PRC
108800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
108900040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
109000040708     C                   ENDIF
109100040708     C*** NAC
109200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
109300040708     C                   EVAL      VAONAC = SkSplitCSV(i)
109400040708     C                   ENDIF
109500040708     C*** FFD
109600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
109700040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
109800040708     C                   ENDIF
109900040708     C*** RMP
110000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
110100040708     C                   EVAL      VAORMP = SkSplitCSV(i)
110200040708     C                   ENDIF
110300040708     C*** NAM
110400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
110500040708     C                   EVAL      VAONAM = SkSplitCSV(i)
110600040708     C                   ENDIF
110700040708     C*** PAG
110800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
110900040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
111000040708     C                   ENDIF
111100040708     C*** CTR
111200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
111300040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
111400040708     C                   ENDIF
111500040708     C*** ZOR
111600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
111700040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
111800040708     C                   ENDIF
111900040708     C*** NO1
112000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
112100040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
112200030715     C                   ENDIF
112300040708     C*** NO2
112400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
112500040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
112600030715     C                   ENDIF
112700040708     C*** DDT
112800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
112900040708     C                   EVAL      VAODDT = SkSplitCSV(i)
113000030715     C                   ENDIF
113100040708     C*** RFA
113200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
113300040708     C                   EVAL      VAORFA = SkSplitCSV(i)
113400030715     C                   ENDIF
113500040708     C*** SPI
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
113700040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
113800030715     C                   ENDIF
113900040708     C*** FLO
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
114100040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
114200040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
114300050314      * e la posizione 7 del VAOFLO
114400040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
114500040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
114600050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
114700030715     C                   ENDIF
114800050314
114900050314      * DATA DI RITIRO PASSATA IN FORMATO GG/MM/AAAA
115000050314     C                   CLEAR                   POS1              3 0
115100050314     C                   CLEAR                   POS2              3 0
115200050314     C                   CLEAR                   WDAR              8
115300050314      *** DAR
115400050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
115500050513     C                   IF        %scan('/':SkSplitCSV(i):1) > 0
115600050314     C                   EVAL      pos1 = %scan('/':SkSplitCSV(i))
115700050314     C                   EVAL      pos2 = %scan('/':SkSplitCSV(i):pos1+1)
115800050314      * giorno
115900050314     C                   IF        pos1 = 2
116000050314     C                   EVAL      %subst(WDAR:7:1) = '0'
116100050314     C                   EVAL      %subst(WDAR:8:1) = (%subst(SkSplitCSV(i):
116200050314     C                                                 1:pos1-1))
116300050314     C                   ELSE
116400050314     C                   EVAL      %subst(WDAR:7:2) = (%subst(SkSplitCSV(i):
116500050314     C                                                 1:pos1-1))
116600050314     C                   ENDIF
116700050314      * mese
116800050314     C                   IF        pos2 - pos1 = 2
116900050314     C                   EVAL      %subst(WDAR:5:1) = '0'
117000050314     C                   EVAL      %subst(WDAR:6:1) = (%subst(SkSplitCSV(i):
117100050314     C                                                 pos1+1:pos2-1))
117200050314     C                   ELSE
117300050314     C                   EVAL      %subst(WDAR:5:2) = (%subst(SkSplitCSV(i):
117400050314     C                                                 pos1+1:pos2-1))
117500050314     C                   ENDIF
117600050314      * anno
117700050314     C                   EVAL      %subst(WDAR:1:4) = (%subst(SkSplitCSV(i):
117800050314     C                                        pos2+1:4))
117900050314     C                   MOVE      WDAR          VAODAR
118000050314     C                   ENDIF
118100050513     C                   ENDIF
118200030721     C*
118300030721     C* Reperisco quindi i campi numerici...
118400040708     C*** POE
118500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
118600040723     C                   EVAL      PiStr=SkSplitCSV(i)
118700040723     C                   EXSR      CHKNUM
118800040723     C                   IF        PiInt=*on    AND
118900040723     C                             PiVal<=999   AND
119000040723     C                             PiVal>*zeros
119100040723     C                   Z-ADD     PiVal         VAOPOE
119200040723     C                   ELSE
119300040723     C                   SETON                                        32
119400040723     C                   EVAL      VAOPOE = *zeros
119500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040723     C                             + ' ' + 'VAOPOE'
119700040723     C                   ENDIF
119800040723     C                   ENDIF
119900040708     C*** COR
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
120100040708     C                   EVAL      PiStr=SkSplitCSV(i)
120200040708     C                   EXSR      CHKNUM
120300040708     C                   IF        PiInt=*on
120400040708     C                   Z-ADD     PiVal         VAOCOR
120500040708     C                   ELSE
120600040708     C                   SETON                                        31
120700040708     C                   EVAL      VAOCOR = *zeros
120800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040708     C                             + ' ' + 'VAOCOR'
121000040708     C                   ENDIF
121100040708     C                   ENDIF
121200040708     C*** CAO
121300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on
121700030721     C                   Z-ADD     PiVal         Num5_0
121800040708     C                   MOVEL(p)  Num5_0        VAOCAO
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100040708     C                   EVAL      VAOCAO = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300040708     C                             + ' ' + 'VAOCAO'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600040708     C*** CRA
122700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on
123100040708     C                   Z-ADD     PiVal         VAOCRA
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400040708     C                   EVAL      VAOCRA = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040708     C                             + ' ' + 'VAOCRA'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900040708     C*** CAR
124000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
124100040708     C                   EVAL      PiStr=SkSplitCSV(i)
124200040708     C                   EXSR      CHKNUM
124300040708     C                   IF        PiInt=*on
124400040708     C                   Z-ADD     PiVal         Num5_0
124500040708     C                   MOVEL(p)  Num5_0        VAOCAR
124600040708     C                   ELSE
124700040708     C                   SETON                                        31
124800040708     C                   EVAL      VAOCAR = *zeros
124900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125000040708     C                             + ' ' + 'VAOCAR'
125100040708     C                   ENDIF
125200040708     C                   ENDIF
125300040708     C*** CRC
125400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
125500040708     C                   EVAL      PiStr=SkSplitCSV(i)
125600040708     C                   EXSR      CHKNUM
125700040708     C                   IF        PiInt=*on
125800040708     C                   Z-ADD     PiVal         VAOCRC
125900040708     C                   ELSE
126000040708     C                   SETON                                        32
126100040708     C                   EVAL      VAOCRC = *zeros
126200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126300040708     C                             + ' ' + 'VAOCRC'
126400040708     C                   ENDIF
126500040708     C                   ENDIF
126600040708     C*** CAC
126700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
126800040708     C                   EVAL      PiStr=SkSplitCSV(i)
126900040708     C                   EXSR      CHKNUM
127000040708     C                   IF        PiInt=*on
127100040708     C                   Z-ADD     PiVal         Num5_0
127200040708     C                   MOVEL(p)  Num5_0        VAOCAC
127300040708     C                   ELSE
127400040708     C                   SETON                                        32
127500040708     C                   EVAL      VAOCAC = *zeros
127600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127700040708     C                             + ' ' + 'VAOCAC'
127800040708     C                   ENDIF
127900040708     C                   ENDIF
128000040708     C*** DAR
128100050314     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VAODAR'
128200050314     C**!!!              EVAL      PiStr=SkSplitCSV(i)
128300050314     C**!!!              EXSR      CHKNUM
128400050314     C**!!!              IF        PiInt=*on
128500050314     C**!!!              Z-ADD     PiVal         VAODAR
128600050314     C**!!!              ELSE
128700050314     C**!!!              SETON                                        32
128800050314     C**!!!              EVAL      VAODAR = *zeros
128900050314     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
129000050314     C**!!!                        + ' ' + 'VAODAR'
129100050314     C**!!!              ENDIF
129200050314     C**!!!              ENDIF
129300040708     C*** ORR
129400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
129500040708     C                   EVAL      PiStr=SkSplitCSV(i)
129600040708     C                   EXSR      CHKNUM
129700040708     C                   IF        PiInt=*on
129800040708     C                   Z-ADD     PiVal         VAOORR
129900040708     C                   ELSE
130000040708     C                   SETON                                        32
130100040708     C                   EVAL      VAOORR = *zeros
130200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130300040708     C                             + ' ' + 'VAOORR'
130400040708     C                   ENDIF
130500040708     C                   ENDIF
130600030721     C*** NCL
130700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on
131100040723     C                   IF        PiVal > *zeros
131200040708     C                   Z-ADD     PiVal         VAONCL
131300040723     C                   ENDIF
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600040708     C                   EVAL      VAONCL = *zeros
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800040708     C                             + ' ' + 'VAONCL'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100040708     C*** PKG
132200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiNum=*on
132600040723     C                   IF        PiVal > *zeros
132700040708     C                   Z-ADD     PiVal         VAOPKG
132800040723     C                   ENDIF
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100040708     C                   EVAL      VAOPKG = *zeros
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300040708     C                             + ' ' + 'VAOPKG'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600040708     C*** VLM
133700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
133800040708     C                   EVAL      PiStr=SkSplitCSV(i)
133900040708     C                   EXSR      CHKNUM
134000040708     C                   IF        PiNum=*on
134100040708     C                   Z-ADD     PiVal         VAOVLM
134200040708     C                   ELSE
134300040708     C                   SETON                                        32
134400040708     C                   EVAL      VAOVLM = *zeros
134500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134600040708     C                             + ' ' + 'VAOVLM'
134700040708     C                   ENDIF
134800040708     C                   ENDIF
134900040708     C*** BNC
135000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
135100040708     C                   EVAL      PiStr=SkSplitCSV(i)
135200040708     C                   EXSR      CHKNUM
135300040708     C                   IF        PiInt=*on
135400040708     C                   Z-ADD     PiVal         VAOBNC
135500040708     C                   ELSE
135600040708     C                   SETON                                        32
135700040708     C                   EVAL      VAOBNC = *zeros
135800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135900040708     C                             + ' ' + 'VAOBNC'
136000040708     C                   ENDIF
136100040708     C                   ENDIF
136200040708     C*** BLC
136300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
136400040708     C                   EVAL      PiStr=SkSplitCSV(i)
136500040708     C                   EXSR      CHKNUM
136600040708     C                   IF        PiInt=*on
136700040708     C                   Z-ADD     PiVal         VAOBLC
136800040708     C                   ELSE
136900040708     C                   SETON                                        32
137000040708     C                   EVAL      VAOBLC = *zeros
137100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137200040708     C                             + ' ' + 'VAOBLC'
137300040708     C                   ENDIF
137400040708     C                   ENDIF
137500040708     C*** ATT
137600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
137700040708     C                   EVAL      PiStr=SkSplitCSV(i)
137800040708     C                   EXSR      CHKNUM
137900040708     C                   IF        PiInt=*on
138000040708     C                   Z-ADD     PiVal         VAOATT
138100040708     C                   ELSE
138200040708     C                   SETON                                        32
138300040708     C                   EVAL      VAOATT = *zeros
138400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138500040708     C                             + ' ' + 'VAOATT'
138600040708     C                   ENDIF
138700040708     C                   ENDIF
138800040708     C*** MTC
138900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
139000040708     C                   EVAL      PiStr=SkSplitCSV(i)
139100040708     C                   EXSR      CHKNUM
139200040708     C                   IF        PiInt=*on
139300040708     C                   Z-ADD     PiVal         VAOMTC
139400040708     C                   ELSE
139500040708     C                   SETON                                        32
139600040708     C                   EVAL      VAOMTC = *zeros
139700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139800040708     C                             + ' ' + 'VAOMTC'
139900040708     C                   ENDIF
140000040708     C                   ENDIF
140100040708     C*** KSC
140200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiInt=*on      AND
140600030721     C                             PiVal<=9999999 AND
140700030721     C                             PiVal>*zeros
140800040708     C                   Z-ADD     PiVal         VAOKSC
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100040708     C                   EVAL      VAOKSC = *zeros
141200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141300040708     C                             + ' ' + 'VAOKSC'
141400030721     C                   ENDIF
141500030721     C                   ENDIF
141600040708     C*** POR
141700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
141800030721     C                   EVAL      PiStr=SkSplitCSV(i)
141900030721     C                   EXSR      CHKNUM
142000030721     C                   IF        PiInt=*on    AND
142100030721     C                             PiVal<=999   AND
142200030721     C                             PiVal>*zeros
142300040708     C                   Z-ADD     PiVal         VAOPOR
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600040708     C                   EVAL      VAOPOR = *zeros
142700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142800040708     C                             + ' ' + 'VAOPOR'
142900030721     C                   ENDIF
143000030721     C                   ENDIF
143100040708     C*** POC
143200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
143300040708     C                   EVAL      PiStr=SkSplitCSV(i)
143400040708     C                   EXSR      CHKNUM
143500040708     C                   IF        PiInt=*on    AND
143600040708     C                             PiVal<=999   AND
143700040708     C                             PiVal>*zeros
143800040708     C                   Z-ADD     PiVal         VAOPOC
143900040708     C                   ELSE
144000040708     C                   SETON                                        32
144100040708     C                   EVAL      VAOPOC = *zeros
144200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144300040708     C                             + ' ' + 'VAOPOC'
144400040708     C                   ENDIF
144500040708     C                   ENDIF
144600040708     C*** STO
144700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
144800040708     C                   EVAL      PiStr=SkSplitCSV(i)
144900040708     C                   EXSR      CHKNUM
145000040708     C                   IF        PiInt=*on
145100040708     C                   Z-ADD     PiVal         VAOSTO
145200040708     C                   ELSE
145300040708     C                   SETON                                        32
145400040708     C                   EVAL      VAOSTO = *zeros
145500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145600040708     C                             + ' ' + 'VAOSTO'
145700040708     C                   ENDIF
145800040708     C                   ENDIF
145900030715     C*
146000030715     C                   ENDIF
146100030715     C                   EVAL      i = i + 1
146200030715     C                   ENDDO
146300040723     C*
146400030715     C                   ENDIF
146500020204     C*
146600010607     C                   ENDSR
146700010607     C*----------------------------------------------------*
146800020204
146900010330
147000010330
147100010330     C*----------------------------------------------------*
147200010330     C*  CONTROLLO NUMERICITA' CAMPI
147300010330     C*----------------------------------------------------*
147400010330     C     CHKNUM        BEGSR
147500010330     C*
147600010606     C                   IF        PiDecChr = *blanks
147700030715     C                   EVAL      PiDecChr = CharNUM
147800010606     C                   ENDIF
147900010606     C*
148000010606     C                   CALL(e)   'ISNUMERIC'
148100010330     C                   PARM                    PiStr            30
148200010606     C                   PARM                    PiDecChr          1
148300010330     C                   PARM      *ZEROS        PiVal            30 9
148400010330     C                   PARM      '0'           PiInt             1
148500010330     C                   PARM      '0'           PiNum             1
148600010330     C                   IF        %error
148700010606     C                   EVAL      PiNum=*off
148800010330     C                   ENDIF
148900010330     C*
149000010330     C                   ENDSR
149100010330     C***
149200010330
149300010601
149400010601
149500010601
149600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149700010601     C     repfil        BEGSR
149800010601     C*
149900010601     C                   if        invfil = *zeros and
150000010601     C                             depfil > *zeros and
150100010629     C                             (vinflg = *blanks or
150200010629     C                              vinflg = *zeros)
150300010601     C
150400010601     C                   eval      invfil = depfil
150500010601     C                   endif
150600010601     C*
150700010601     C                   if        depfil <> invfil and
150800010601     C                             invfil > *zeros
150900010601     C                   eval      flgMulti = '1'
151000010601     C                   if        vinflg = *blanks
151100010601     C                   add       1             cntNonEl
151200010601     C                   endif
151300010601     C                   endif
151400010601     C*
151500010601     C                   if        vinflg = '2'
151600010601     C                   eval      flgStato = '2'
151700010601     C                   endif
151800010601     C*
151900010601     C                   ENDSR
152000010601     C***
152100010601
152200010601
152300010601
152400010330
152500010330
152600010330
152700990920      /TITLE Invio dei dati al punto operativo.
152800000613     C     invio         BEGSR
152900990920     C*
153000990920     C                   reset                   dscmz
153100010601     C                   move      invfil        cmzdst
153200990920     C                   eval      cmzfld = vlrfou
153300990920     C                   eval      cmzmbd = vlrhdl
153400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
153500000710     C                   if        prmfir = *blanks
153600040708     C                   eval      cmzfla = 'FNVAO00F'
153700040708     C                   eval      cmzmba = 'FNVAO00F'
153800000710     C                   else
153900000710     C                   eval      cmzfla = prmfir
154000000710     C                   eval      cmzmba = prmfir
154100000710     C                   endif
154200990920     C                   eval      cmznrr = *zeros
154300990920     C                   move      §ctrok        cmznrr
154400021018     C                   eval      cmzlba = vlrfl1
154500990920     C                   call(e)   'TIS711C'
154600990920     C                   parm                    dscmz
154700990921     C                   parm      *blanks       esito
154800990920     C                   if        %error
154900990920     C                             or cmzerr = '1'
155000990921     C                             or esito  = '1'
155100000710     C                   eval      wrkesito = '3'
155200990920     C                   endif
155300990920     C*
155400000613     C                   ENDSR
155500990910
155600010601
155700010601
155800010601
155900010601
156000010601      /TITLE Invio dei dati al punto operativo.
156100010601     C     opeini        BEGSR
156200010601     C*
156300010601     C* Inizializzo flag e contatori operativi
156400010601     C                   movel     '0'           flgGiro           1
156500010601     C                   movel     '0'           flgMulti          1
156600010601     C                   movel     '1'           flgStato          1
156700010615     C                   movel     '0'           flgOk             1
156800010601     C                   z-add     *zeros        cntNonEl         10 0
156900010601     C                   z-add     *zeros        depfil            3 0
157000010601     C                   z-add     *zeros        invfil            3 0
157100010601     C*
157200010601     C                   ENDSR
157300010601     C***
157400050421
157500050421
157600050421
157700050421     C     *pssr         BEGSR
157800050421     C*
157900050421     C                   eval      wrkesito = '2'
158000050421     C*
158100050421     C                   ENDSR     '*CANCL'
158200050421     C***
158300060306
158400060306
158500060306     C*-------------------------------------------------------------------------*
158600060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
158700060306     C*-------------------------------------------------------------------------*
158800060306     C     sr_Control    begsr
158900060306     C*
159000060306     C* inizializzo a ok il flag d controllo abilitazione
159100060306     C                   eval      Cli_Abil = *on
159200060306     C
159300060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
159400060306     C                   clear                   woriIDC
159500060306     C                   eval      kteCOD = 'VAP'
159600060306     C                   eval      kteKE1 = oriIDC
159700060306     C     keytbe        chain     TNTBE01L
159800060306     C                   if        %found(Tntbe01l)
159900060306     C                   eval      woriIDC = TbeKe2
160000060306     C                   else
160100060306     C                   eval      Cli_Abil = *off
160200060306     C                   endif
160300060306     C*
160400060306     C                   endsr
160500050421
160600010601
160700010330
160800010330
160900000613     C     *inzsr        BEGSR
161000990910     C*
161100990910     C     *entry        plist
161200990920     C                   parm                    tivlrds
161300990921     C                   parm      wrkesito      esito
161400000724     C                   parm                    prmlit
161500000710     C                   parm                    prmfir
161600010330     C*
161700010330     C* CALCOLA LA DATA CORRENTE
161800010330     C                   time                    wn14             14 0
161900040723     C                   movel     wn14          wn6               6 0          *ORA (6)
162000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
162100010330     C                   z-add     wn8           g08dat
162200010330     C                   z-add     *zeros        g08inv
162300010330     C                   movel     '0'           g08err
162400010330     C                   call      'XSRDA8'
162500010330     C                   parm                    wlbda8
162600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
162700040723     C*
162800040723     C* Campi riferito al database
162900040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
163000040723     C     *like         define    tbeke1        kteKE1
163100040723     C*
163200040723     C* Definizione chiavi di lettura
163300040723     C* TNTBE01L - parziale
163400040723     C     keytbe        klist
163500040723     C                   kfld                    kteCOD                         *UTENTE
163600040723     C                   kfld                    kteKE1                         *CODICE
163700000613     C*
163800000613     C                   ENDSR
163900000613     C***
164000990908
164100050314     Otitvi8p   E            riepilogo         2
164200990915     O                                              'Upload via Internet'
164300990915     O                                           +1 'Traduzione TIVIN00R -'
164400040708     O                                           +1 'FNVAOWWR'
164500990915     O                                           +1 'Testate bolle'
164600990915     O          E            riepilogo   2
164700990915     O                       wrkdata
164800990915     O                       wrkora              +1
164900990915     O                       procname            +1
165000990915     O          E            riepilogo   2
165100990915     O                                              'Cliente..................:'
165200040708     O                       VAOCOR        z     +1
165300990915     O          E            riepilogo   2
165400990920     O                                              'Riferimento Strategi.....:'
165500990920     O                       vlrhdl              +1
165600990915     O          E            riepilogo   2
165700990915     O                                              'Giusti...................:'
165800971022     O                       §CTROK        2   +  1
165900990915     O          E            riepilogo   2
166000990915     O                                              'Sbagliati e corretti.....:'
166100971022     O                       §CTRMO        2   +  1
166200990915     O          E            riepilogo   2
166300990915     O                                              'Sbagliati e scartati.....:'
166400971022     O                       §CTRNO        2   +  1
166500000613
166600050314     Otitvi8ps  E            testdett          2
166700000613     O                                              'Upload via Internet'
166800000613     O                                           +1 'Traduzione TIVIN00R -'
166900040708     O                                           +1 'FNVAOWWR'
167000000613     O                                           +1 'Testate bolle'
167100000616     O          E            testdett    3
167200000613     O                                           +2 'N° rec'
167300000613     O                                           +3 'Anteprima contenuto'
167400000616     O          E            rigadett    2
167500000613     O                       rrnum               +2
167600000621     O                       recko               +3
167700000616     O          E            findett     2
167800000613     O                       wrkdata
167900000613     O                       wrkora              +1
168000000613     O                       procname            +1
168100000616     O          E            findett     2
168200000613     O                                              'Cliente..................:'
168300040708     O                       VAOCOR        z     +1
168400000616     O          E            findett     2
168500000613     O                                              'Riferimento Strategi.....:'
168600000613     O                       vlrhdl              +1
168700000616     O          E            findett     2
168800000613     O                                              'Giusti...................:'
168900000613     O                       §CTROK        2   +  1
169000000616     O          E            findett     2
169100000613     O                                              'Sbagliati e corretti.....:'
169200000613     O                       §CTRMO        2   +  1
169300000616     O          E            findett     2
169400000613     O                                              'Sbagliati e scartati.....:'
169500000613     O                       §CTRNO        2   +  1
169600000613** CMD - COMANDI CL
169700050314OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
169800050314OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
169900050314DLTOVR FILE(TITVI8P TITVI8PS) LVL(*)
170000010606
170100000613
170200040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170300050314NUM;DATA INSERIMENTO;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;
170400050314VAORER;VAOTER;VAODAR;VAONO1
170500040709
