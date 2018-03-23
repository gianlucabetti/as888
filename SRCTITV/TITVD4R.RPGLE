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
001100040709     Ftitvd4p   O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300040709     Ftitvd4ps  O    f  198        PRINTER usropn
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
015100040709     C                   if        not %open(titvd4ps)
015200040709     C                   open      titvd4ps
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
017200040709     C                   if        %open(titvd4ps)
017300000616     C                   except    findett
017400040709     C                   close     titvd4ps
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
019500040709     C                   if        not %open(titvd4p)
019600040709     C   50              open      titvd4p
019700990915     C                   endif
019800990915     C*
019900030715     C   50              except    riepilogo
020000990915     C*
020100040709     C                   if        %open(titvd4p)
020200040709     C   50              close     titvd4p
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
049400040723     C                   eval      vaoFLO = dorm01
049500040723     C*
049600040723     C* ...quindi scrivo il file TIORI00F
049700040723     C                   WRITE     TIORI000
049800040723     C*
049900040723     C                   ENDSR
050000050826
050100050826
050200050826
050300050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
050400050826     C     wriVGD        BEGSR
050500050826     C*
050600050826     C* Reperisco la descrizione della fase da tabella
050700050826     C                   movel(p)  'FAR'         KteCOD
050800050826     C                   movel(p)  '000'         KteKE1
050900050826     C     KEYtbe        chain     tntbe01l
051000050826     C                   if        %found(tntbe01l)
051100050826     C                   movel(P)  tbeuni        DFAR
051200050826     C                   else
051300050826     C                   clear                   DFAR
051400050826     C                   endif
051500050826     C*
051600050826     C* Valorizzo buffer record
051700050826     C                   clear                   FNVAPDS
051800050826     C                   eval      vapIDC = oriIDC
051900050826     C                   eval      vapPOE = oriPOE
052000050826     C                   eval      vapRFA = vaoRFA
052100050826     C                   eval      vapPOG = oriPOE
052200050826     C                   eval      vapDAE = oriDER
052300050826     C                   eval      vapORE = oriOER
052400050826     C                   if        d§fardva <> *blanks
052500050826     C                   eval      vapDFA = d§fardva
052600050826     C                   else
052700050826     C                   eval      vapDFA = d§fardes
052800050826     C                   endif
052900050826     C*
053000050826     C                   clear                   tivgd000
053100080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
053200050826     C                   eval      vgdTIP = 'VP'
053300050826     C                   eval      vgdKSU = vapIDC
053400050826     C                   eval      vgdTSC = 'WW'
053500050826     C                   eval      vgdDAT = datcor
053600050826     C                   eval      vgdPGM = 'TITVD4R'
053700060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
053800060306     C                   exsr      sr_Control
053900060306     C                   if        Cli_Abil = *on
054000060306     C                   eval      vgdKSU = woriIDC
054100050826     C                   write     tivgd000
054200060306     C                   endif
054300050826     C*
054400050826     C                   ENDSR
054500050826     C*------------------------------------------------------------------------*
054600010601
054700010601
054800010601
054900010330     C*----------------------------------------------------*
055000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055100010330     C*----------------------------------------------------*
055200010330     C     INZVAR        BEGSR
055300010330     C*
055400030822     C* Inizializzo variabili di wrk
055500020204     C                   Z-ADD     *zeros        Num5_0            5 0
055600030822     C*
055700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055800040708     C                   CLEAR                   FNVAO000
055900040723     C                   CLEAR                   TIORI000
056000030822     C                   CLEAR                   SkSplitCSV
056100030822     C                   EVAL      i = 1
056200030822     C                   EVAL      posDa = *zeros
056300030822     C                   EVAL      posA  = *zeros
056400030822     C*
056500030822     C* Reimposto i valori di default
056600030822     C                   EXSR      DEFCAM
056700030822     C*
056800010330     C                   ENDSR
056900010330     C*----------------------------------------------------*
057000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057100010330     C*----------------------------------------------------*
057200010330     C     DEFCAM        BEGSR
057300030822     C*
057400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057500030715     C* e delimitatore testo.
057600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057900030822     C*
058000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058200030715     C                   SETON                                        50
058300030715     C                   ELSE
058400030715     C                   SETOFF                                       50
058500030715     C                   ENDIF
058600040723     C*
058700040723     C* Imposto i valori d default d alcuni campi
058800040723     C                   EVAL      VAOCOR = 0632928000
058900040723     C                   EVAL      VAONCL = 1
059000040723     C                   EVAL      VAOTCO = 'F'
059100040723     C                   EVAL      VAOTOR = 'S'
059200040723     C                   EVAL      VAOPAG = 'O'
059300040723     C                   EVAL      VAODDT = 'S'
059400030822     C*
059500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059600030822     C                   EVAL      posDaDft = 1
059700030822     C                   EVAL      posADft  = 0
059800030822     C                   EVAL      wGiroDft = 0
059900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060000030822     C                             posDaDft > 0
060100030822     C*
060200030822     C* Gestisco il 1° giro
060300030822     C                   IF        wGiroDft = 0
060400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060600030822     C* Incremento il contatore dei "giri"
060700030822     C                   EVAL      wGiroDft = 1
060800030822     C                   ELSE
060900030822     C                   EVAL      posDaDft = posADft
061000030822     C                   ENDIF
061100030822     C* Eseguo lo scan x trovare la fine del campo corrente
061200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061300030822     C*
061400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061600030822     C                   IF        posDaDft > 0 AND
061700030822     C                             posADft  > 0
061800040708     C* POE
061900040708     C                   IF        %subst(
062000040708     C                             %subst(vlrppt:posDaDft+1:
062100040708     C                             posADft-posDaDft-1):1:3)
062200040708     C                             = 'POE'
062300040708     C                   EVAL      PiStr=%trim(%subst(
062400040708     C                             %subst(vlrppt:posDaDft+1:
062500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600040708     C                   EXSR      CHKNUM
062700040708     C                   IF        PiInt=*on
062800040708     C                   Z-ADD     PiVal         VAOPOE
062900040708     C                   ENDIF
063000040708     C                   ENDIF
063100040708     C* TOR
063200040708     C                   IF        %subst(
063300040708     C                             %subst(vlrppt:posDaDft+1:
063400040708     C                             posADft-posDaDft-1):1:3)
063500040708     C                             = 'TOR'
063600040708     C                   EVAL      VAOTOR=%trim(%subst(
063700040708     C                             %subst(vlrppt:posDaDft+1:
063800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900040708     C                   ENDIF
064000040708     C* TCO
064100040708     C                   IF        %subst(
064200040708     C                             %subst(vlrppt:posDaDft+1:
064300040708     C                             posADft-posDaDft-1):1:3)
064400040708     C                             = 'TCO'
064500040708     C                   EVAL      VAOTCO=%trim(%subst(
064600040708     C                             %subst(vlrppt:posDaDft+1:
064700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800040708     C                   ENDIF
064900040707     C* COR
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300040707     C                             = 'COR'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiInt=*on
065900040708     C                   Z-ADD     PiVal         VAOCOR
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200040708     C* CRC
066300040708     C                   IF        %subst(
066400040708     C                             %subst(vlrppt:posDaDft+1:
066500040708     C                             posADft-posDaDft-1):1:3)
066600040708     C                             = 'CRC'
066700040708     C                   EVAL      PiStr=%trim(%subst(
066800040708     C                             %subst(vlrppt:posDaDft+1:
066900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000040708     C                   EXSR      CHKNUM
067100040708     C                   IF        PiInt=*on
067200040708     C                   Z-ADD     PiVal         VAOCRC
067300040708     C                   ENDIF
067400040708     C                   ENDIF
067500040708     C* FFD
067600040708     C                   IF        %subst(
067700040708     C                             %subst(vlrppt:posDaDft+1:
067800040708     C                             posADft-posDaDft-1):1:3)
067900040708     C                             = 'FFD'
068000040708     C                   EVAL      VAOFFD=%trim(%subst(
068100040708     C                             %subst(vlrppt:posDaDft+1:
068200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300040708     C                   ENDIF
068400040708     C* ORR
068500040708     C                   IF        %subst(
068600040708     C                             %subst(vlrppt:posDaDft+1:
068700040708     C                             posADft-posDaDft-1):1:3)
068800040708     C                             = 'ORR'
068900040708     C                   EVAL      PiStr=%trim(%subst(
069000040708     C                             %subst(vlrppt:posDaDft+1:
069100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200040708     C                   EXSR      CHKNUM
069300040708     C                   IF        PiInt=*on
069400040708     C                   Z-ADD     PiVal         VAOORR
069500040708     C                   ENDIF
069600040708     C                   ENDIF
069700040712     C* NCL
069800040712     C                   IF        %subst(
069900040712     C                             %subst(vlrppt:posDaDft+1:
070000040712     C                             posADft-posDaDft-1):1:3)
070100040712     C                             = 'NCL'
070200040712     C                   EVAL      PiStr=%trim(%subst(
070300040712     C                             %subst(vlrppt:posDaDft+1:
070400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500040712     C                   EXSR      CHKNUM
070600040712     C                   IF        PiInt=*on
070700040712     C                   Z-ADD     PiVal         VAONCL
070800040712     C                   ENDIF
070900040712     C                   ENDIF
071000040712     C* PKG
071100040712     C                   IF        %subst(
071200040712     C                             %subst(vlrppt:posDaDft+1:
071300040712     C                             posADft-posDaDft-1):1:3)
071400040712     C                             = 'PKG'
071500040712     C                   EVAL      PiStr=%trim(%subst(
071600040712     C                             %subst(vlrppt:posDaDft+1:
071700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800040712     C                   EXSR      CHKNUM
071900040712     C                   IF        PiNum=*on
072000040712     C                   Z-ADD     PiVal         VAOPKG
072100040712     C                   ENDIF
072200040712     C                   ENDIF
072300040712     C* VLM
072400040712     C                   IF        %subst(
072500040712     C                             %subst(vlrppt:posDaDft+1:
072600040712     C                             posADft-posDaDft-1):1:3)
072700040712     C                             = 'VLM'
072800040712     C                   EVAL      PiStr=%trim(%subst(
072900040712     C                             %subst(vlrppt:posDaDft+1:
073000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040712     C                   EXSR      CHKNUM
073200040712     C                   IF        PiNum=*on
073300040712     C                   Z-ADD     PiVal         VAOVLM
073400040712     C                   ENDIF
073500040712     C                   ENDIF
073600040708     C* NAM
073700040708     C                   IF        %subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):1:3)
074000040708     C                             = 'NAM'
074100040708     C                   EVAL      VAONAM=%trim(%subst(
074200040708     C                             %subst(vlrppt:posDaDft+1:
074300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400040708     C                   ENDIF
074500040708     C* PAG
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900040708     C                             = 'PAG'
075000040708     C                   EVAL      VAOPAG=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400040708     C* KSC
075500040708     C                   IF        %subst(
075600040708     C                             %subst(vlrppt:posDaDft+1:
075700040708     C                             posADft-posDaDft-1):1:3)
075800040708     C                             = 'KSC'
075900040708     C                   EVAL      PiStr=%trim(%subst(
076000040708     C                             %subst(vlrppt:posDaDft+1:
076100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200040708     C                   EXSR      CHKNUM
076300040708     C                   IF        PiInt=*on
076400040708     C                   Z-ADD     PiVal         VAOKSC
076500040708     C                   ENDIF
076600040708     C                   ENDIF
076700040708     C* CTR
076800040708     C                   IF        %subst(
076900040708     C                             %subst(vlrppt:posDaDft+1:
077000040708     C                             posADft-posDaDft-1):1:3)
077100040708     C                             = 'CTR'
077200040708     C                   EVAL      VAOCTR=%trim(%subst(
077300040708     C                             %subst(vlrppt:posDaDft+1:
077400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500040708     C                   ENDIF
077600040708     C* POC
077700040708     C                   IF        %subst(
077800040708     C                             %subst(vlrppt:posDaDft+1:
077900040708     C                             posADft-posDaDft-1):1:3)
078000040708     C                             = 'POC'
078100040708     C                   EVAL      PiStr=%trim(%subst(
078200040708     C                             %subst(vlrppt:posDaDft+1:
078300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400040708     C                   EXSR      CHKNUM
078500040708     C                   IF        PiInt=*on
078600040708     C                   Z-ADD     PiVal         VAOPOC
078700040708     C                   ENDIF
078800040708     C                   ENDIF
078900040708     C* NO1
079000040708     C                   IF        %subst(
079100040708     C                             %subst(vlrppt:posDaDft+1:
079200040708     C                             posADft-posDaDft-1):1:3)
079300040708     C                             = 'NO1'
079400040708     C                   EVAL      VAONO1=%trim(%subst(
079500040708     C                             %subst(vlrppt:posDaDft+1:
079600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700040708     C                   ENDIF
079800040708     C* DDT
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200040708     C                             = 'DDT'
080300040708     C                   EVAL      VAODDT=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700040708     C* RFA
080800040708     C                   IF        %subst(
080900040708     C                             %subst(vlrppt:posDaDft+1:
081000040708     C                             posADft-posDaDft-1):1:3)
081100040708     C                             = 'RFA'
081200040708     C                   EVAL      VAORFA=%trim(%subst(
081300040708     C                             %subst(vlrppt:posDaDft+1:
081400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500040708     C                   ENDIF
081600040708     C* FLO
081700040708     C                   IF        %subst(
081800040708     C                             %subst(vlrppt:posDaDft+1:
081900040708     C                             posADft-posDaDft-1):1:3)
082000040708     C                             = 'FLO'
082100040708     C                   EVAL      VAOFLO=%trim(%subst(
082200040708     C                             %subst(vlrppt:posDaDft+1:
082300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
082500040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
082600040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
082700040708     C                   ENDIF
082800030822     C* ...
082900030822     C                   ENDIF
083000030822     C                   ENDDO
083100020204     C*
083200010330     C                   ENDSR
083300010607     C*----------------------------------------------------*
083400040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
083500010607     C*----------------------------------------------------*
083600040708     C     IMPVAO        BEGSR
083700010607     C*
083800010607     C                   SETOFF                                       3132
083900030515     C*
084000040708     C                   MOVEL     datcor        VAODAO
084100040708     C                   MOVEL     wn14          VAOOAO
084200010607     C*
084300030715     C**********
084400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084500030715     C**********
084600030715     C                   IF        wGiro = *zeros
084700040709     C*
084800040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
084900040709     C                   EVAL      depvinDTA = vinDTA
085000040709     C*
085100040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
085200040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085300040709     C                                      %trim(intcol(3))
085400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085500031201     C     minu:maiu     XLATE     vindta        vindta
085600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085700040324     C                   DOW       posDa <= %len(%trimr(vindta))
085800030715     C*
085900030715     C* Gestisco il 1° campo
086000030715     C                   IF        i = 1
086100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086200030715     C                   EVAL      posDa = 1
086300030715     C* Eseguo lo scan x trovare la fine del primo campo
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C* X i campi successivi al 1°
086900030715     C                   ELSE
087000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087100030715     C                   EVAL      posDa = posA + 1
087200030715     C* Eseguo lo scan x trovare la fine del campo corrente
087300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087400030715     C* Gestisco l'ultimo campo
087500030715     C                   IF        posA = *zeros
087600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   LEAVE
088100030715     C                   ELSE
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C                   ENDIF
088600030715     C                   ENDIF
088700030715     C* Incremento il contatore d campo
088800030715     C                   EVAL      i = i +1
088900030715     C                   ENDDO
089000030715     C*
089100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089200030715     C* delimitazione testo
089300030715     C                   EVAL      i = 1
089400030715     C                   DOW       i < %elem(SkSplitFLD)
089500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089700030715     C                   EVAL      i = i + 1
089800030715     C                   ENDDO
089900030715     C                   EVAL      wGiro = 1
090000040709     C* Ripristino la 1° riga originale
090100040709     C                   EVAL      vinDTA = depvinDTA
090200030715     C                   ELSE
090300030715     C                   EVAL      wGiro = 2
090400031204     C**********
090500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090600031204     C* d separatore campo
090700031204     C**********
090800040709     C                   EVAL      vindta = %trim(vindta)
090900040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091000040709     C                   EVAL      vindta = %subst(vindta:2)
091100040709     C                   ENDDO
091200040709     C*
091300040709     C                   Z-ADD     *zeros        lunghInput        4 0
091400040709     C                   EVAL      lunghInput = %len(%trim(vindta))
091500040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091600040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091700040709     C                   EVAL      lunghInput = %len(%trim(vindta))
091800040709     C                   ENDDO
091900030715     C**********
092000030715     C* Effettuo lo split del campo dati d input
092100030715     C**********
092200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092300040324     C                   DOW       posDa <= %len(%trimr(vindta))
092400030715     C*
092500030715     C* Gestisco il 1° campo
092600030715     C                   IF        i = 1
092700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092800030715     C                   EVAL      posDa = 1
092900030715     C* Eseguo lo scan x trovare la fine del primo campo
093000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093100030715     C* A questo punto "estraggo" il campo corrente
093200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093300030715     C                                                    (posA-posDa))
093400030715     C* X i campi successivi al 1°
093500030715     C                   ELSE
093600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093700030715     C                   EVAL      posDa = posA + 1
093800031204     C* Verifico che nn vi sia il campo nn valorizzato
093900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094000031204     C* Se campo nn valorizzato skippo al prossimo
094100031204     C                   EVAL      SkSplitCSV(i) = *blanks
094200031204     C                   EVAL      posA  = posA + 1
094300031204     C                   ELSE
094400030715     C* Eseguo lo scan x trovare la fine del campo corrente
094500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094600030715     C* Gestisco l'ultimo campo
094700030715     C                   IF        posA = *zeros
094800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C                   LEAVE
095300030715     C                   ELSE
095400030715     C* A questo punto "estraggo" il campo corrente
095500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095600030715     C                                                    (posA-posDa))
095700030715     C                   ENDIF
095800031204     C                   ENDIF
095900030715     C                   ENDIF
096000030715     C* Incremento il contatore d campo
096100030715     C                   EVAL      i = i +1
096200030715     C                   ENDDO
096300030715     C*
096400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096500030715     C* delimitazione testo
096600030715     C                   EVAL      i = 1
096700030715     C                   DOW       i < %elem(SkSplitCSV)
096800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097000030715     C                   EVAL      i = i + 1
097100030715     C                   ENDDO
097200030715     C*
097300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097400030715     C                   EVAL      i = 1
097500030715     C                   DOW       i < %elem(SkSplitFLD)
097600030715     C*
097700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097900030715     C                             SkSplitCSV(i) <> *blanks
098000030721     C*** ATB
098100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
098200040708     C                   EVAL      VAOATB = SkSplitCSV(i)
098300030721     C                   ENDIF
098400040708     C*** TOR
098500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
098600040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
098700040708     C                   ENDIF
098800040708     C*** TCO
098900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
099000040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
099100040708     C                   ENDIF
099200040708     C*** RSO
099300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
099400040708     C                   EVAL      VAORSO = SkSplitCSV(i)
099500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099600040708     C     '@':'A'       XLATE     VAORSO        VAORSO
099700030715     C* ==
099800030715     C                   ENDIF
099900040708     C*** INO
100000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
100100040708     C                   EVAL      VAOINO = SkSplitCSV(i)
100200030715     C                   ENDIF
100300040708     C*** LOO
100400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
100500040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
100600030715     C                   ENDIF
100700040708     C*** PRO
100800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
100900040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
101000030715     C                   ENDIF
101100040708     C*** NAO
101200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
101300040708     C                   EVAL      VAONAO = SkSplitCSV(i)
101400030721     C                   ENDIF
101500040708     C*** RSR
101600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
101700040708     C                   EVAL      VAORSR = SkSplitCSV(i)
101800040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101900040708     C     '@':'A'       XLATE     VAORSR        VAORSR
102000040708     C* ==
102100040708     C                   IF        VAORSR = *blanks
102200040708     C                   SETON                                        31
102300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
102400040708     C                             + ' ' + 'VAORSR'
102500040708     C                   ENDIF
102600040708     C                   ENDIF
102700040708     C*** INR
102800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
102900040708     C                   EVAL      VAOINR = SkSplitCSV(i)
103000040708     C                   IF        VAOINR = *blanks
103100040708     C                   SETON                                        31
103200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103300040708     C                             + ' ' + 'VAOINR'
103400040708     C                   ENDIF
103500040708     C                   ENDIF
103600040708     C*** LOR
103700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
103800040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
103900040708     C                   IF        VAOLOR = *blanks
104000040708     C                   SETON                                        31
104100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104200040708     C                             + ' ' + 'VAOLOR'
104300040708     C                   ENDIF
104400040708     C                   ENDIF
104500040708     C*** PRR
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
104700040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
104800040708     C                   IF        VAOPRR = *blanks
104900040708     C                   SETON                                        31
105000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105100040708     C                             + ' ' + 'VAOPRR'
105200040708     C                   ENDIF
105300040708     C                   ENDIF
105400040708     C*** NAR
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
105600040708     C                   EVAL      VAONAR = SkSplitCSV(i)
105700040708     C                   ENDIF
105800040708     C*** RER
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
106000040708     C                   EVAL      VAORER = SkSplitCSV(i)
106100040708     C                   ENDIF
106200040708     C*** TER
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
106400040708     C                   EVAL      VAOTER = SkSplitCSV(i)
106500040708     C                   ENDIF
106600040708     C*** RSC
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
106800040708     C                   EVAL      VAORSC = SkSplitCSV(i)
106900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107000040708     C     '@':'A'       XLATE     VAORSC        VAORSC
107100040708     C* ==
107200040708     C                   ENDIF
107300040708     C*** INC
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
107500040708     C                   EVAL      VAOINC = SkSplitCSV(i)
107600040708     C                   ENDIF
107700040708     C*** LOC
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
107900040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
108000040708     C                   ENDIF
108100040708     C*** PRC
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
108300040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
108400040708     C                   ENDIF
108500040708     C*** NAC
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
108700040708     C                   EVAL      VAONAC = SkSplitCSV(i)
108800040708     C                   ENDIF
108900040708     C*** FFD
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
109100040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
109200040708     C                   ENDIF
109300040708     C*** RMP
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
109500040708     C                   EVAL      VAORMP = SkSplitCSV(i)
109600040708     C                   ENDIF
109700040708     C*** NAM
109800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
109900040708     C                   EVAL      VAONAM = SkSplitCSV(i)
110000040708     C                   ENDIF
110100040708     C*** PAG
110200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
110300040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
110400040708     C                   ENDIF
110500040708     C*** CTR
110600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
110700040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
110800040708     C                   ENDIF
110900040708     C*** ZOR
111000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
111100040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
111200040708     C                   ENDIF
111300040708     C*** NO1
111400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
111500040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
111600030715     C                   ENDIF
111700040708     C*** NO2
111800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
111900040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
112000030715     C                   ENDIF
112100040708     C*** DDT
112200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
112300040708     C                   EVAL      VAODDT = SkSplitCSV(i)
112400030715     C                   ENDIF
112500040708     C*** RFA
112600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
112700040708     C                   EVAL      VAORFA = SkSplitCSV(i)
112800030715     C                   ENDIF
112900040708     C*** SPI
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
113100040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
113200030715     C                   ENDIF
113300040708     C*** FLO
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
113500040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
113600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
113700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
113800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
113900030715     C                   ENDIF
114000030715     C
114100030721     C*
114200030721     C* Reperisco quindi i campi numerici...
114300040708     C*** POE
114400040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
114500040723     C                   EVAL      PiStr=SkSplitCSV(i)
114600040723     C                   EXSR      CHKNUM
114700040723     C                   IF        PiInt=*on    AND
114800040723     C                             PiVal<=999   AND
114900040723     C                             PiVal>*zeros
115000040723     C                   Z-ADD     PiVal         VAOPOE
115100040723     C                   ELSE
115200040723     C                   SETON                                        32
115300040723     C                   EVAL      VAOPOE = *zeros
115400040723     C                   EVAL      vinmsg = %trimr(vinmsg)
115500040723     C                             + ' ' + 'VAOPOE'
115600040723     C                   ENDIF
115700040723     C                   ENDIF
115800040708     C*** COR
115900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
116000040708     C                   EVAL      PiStr=SkSplitCSV(i)
116100040708     C                   EXSR      CHKNUM
116200040708     C                   IF        PiInt=*on
116300040708     C                   Z-ADD     PiVal         VAOCOR
116400040708     C                   ELSE
116500040708     C                   SETON                                        31
116600040708     C                   EVAL      VAOCOR = *zeros
116700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
116800040708     C                             + ' ' + 'VAOCOR'
116900040708     C                   ENDIF
117000040708     C                   ENDIF
117100040708     C*** CAO
117200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on
117600030721     C                   Z-ADD     PiVal         Num5_0
117700040708     C                   MOVEL(p)  Num5_0        VAOCAO
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000040708     C                   EVAL      VAOCAO = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200040708     C                             + ' ' + 'VAOCAO'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500040708     C*** CRA
118600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on
119000040708     C                   Z-ADD     PiVal         VAOCRA
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300040708     C                   EVAL      VAOCRA = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500040708     C                             + ' ' + 'VAOCRA'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800040708     C*** CAR
119900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
120000040708     C                   EVAL      PiStr=SkSplitCSV(i)
120100040708     C                   EXSR      CHKNUM
120200040708     C                   IF        PiInt=*on
120300040708     C                   Z-ADD     PiVal         Num5_0
120400040708     C                   MOVEL(p)  Num5_0        VAOCAR
120500040708     C                   ELSE
120600040708     C                   SETON                                        31
120700040708     C                   EVAL      VAOCAR = *zeros
120800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040708     C                             + ' ' + 'VAOCAR'
121000040708     C                   ENDIF
121100040708     C                   ENDIF
121200040708     C*** CRC
121300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
121400040708     C                   EVAL      PiStr=SkSplitCSV(i)
121500040708     C                   EXSR      CHKNUM
121600040708     C                   IF        PiInt=*on
121700040708     C                   Z-ADD     PiVal         VAOCRC
121800040708     C                   ELSE
121900040708     C                   SETON                                        32
122000040708     C                   EVAL      VAOCRC = *zeros
122100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122200040708     C                             + ' ' + 'VAOCRC'
122300040708     C                   ENDIF
122400040708     C                   ENDIF
122500040708     C*** CAC
122600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
122700040708     C                   EVAL      PiStr=SkSplitCSV(i)
122800040708     C                   EXSR      CHKNUM
122900040708     C                   IF        PiInt=*on
123000040708     C                   Z-ADD     PiVal         Num5_0
123100040708     C                   MOVEL(p)  Num5_0        VAOCAC
123200040708     C                   ELSE
123300040708     C                   SETON                                        32
123400040708     C                   EVAL      VAOCAC = *zeros
123500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040708     C                             + ' ' + 'VAOCAC'
123700040708     C                   ENDIF
123800040708     C                   ENDIF
123900040708     C*** DAR
124000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
124100040708     C                   EVAL      PiStr=SkSplitCSV(i)
124200040708     C                   EXSR      CHKNUM
124300040708     C                   IF        PiInt=*on
124400040708     C                   Z-ADD     PiVal         VAODAR
124500040708     C                   ELSE
124600040708     C                   SETON                                        32
124700040708     C                   EVAL      VAODAR = *zeros
124800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040708     C                             + ' ' + 'VAODAR'
125000040708     C                   ENDIF
125100040708     C                   ENDIF
125200040708     C*** ORR
125300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
125400040708     C                   EVAL      PiStr=SkSplitCSV(i)
125500040708     C                   EXSR      CHKNUM
125600040708     C                   IF        PiInt=*on
125700040708     C                   Z-ADD     PiVal         VAOORR
125800040708     C                   ELSE
125900040708     C                   SETON                                        32
126000040708     C                   EVAL      VAOORR = *zeros
126100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126200040708     C                             + ' ' + 'VAOORR'
126300040708     C                   ENDIF
126400040708     C                   ENDIF
126500030721     C*** NCL
126600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on
127000040723     C                   IF        PiVal > *zeros
127100040708     C                   Z-ADD     PiVal         VAONCL
127200040723     C                   ENDIF
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500040708     C                   EVAL      VAONCL = *zeros
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700040708     C                             + ' ' + 'VAONCL'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000040708     C*** PKG
128100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500040723     C                   IF        PiVal > *zeros
128600040708     C                   Z-ADD     PiVal         VAOPKG
128700040723     C                   ENDIF
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000040708     C                   EVAL      VAOPKG = *zeros
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200040708     C                             + ' ' + 'VAOPKG'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500040708     C*** VLM
129600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
129700040708     C                   EVAL      PiStr=SkSplitCSV(i)
129800040708     C                   EXSR      CHKNUM
129900040708     C                   IF        PiNum=*on
130000040708     C                   Z-ADD     PiVal         VAOVLM
130100040708     C                   ELSE
130200040708     C                   SETON                                        32
130300040708     C                   EVAL      VAOVLM = *zeros
130400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130500040708     C                             + ' ' + 'VAOVLM'
130600040708     C                   ENDIF
130700040708     C                   ENDIF
130800040708     C*** BNC
130900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
131000040708     C                   EVAL      PiStr=SkSplitCSV(i)
131100040708     C                   EXSR      CHKNUM
131200040708     C                   IF        PiInt=*on
131300040708     C                   Z-ADD     PiVal         VAOBNC
131400040708     C                   ELSE
131500040708     C                   SETON                                        32
131600040708     C                   EVAL      VAOBNC = *zeros
131700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131800040708     C                             + ' ' + 'VAOBNC'
131900040708     C                   ENDIF
132000040708     C                   ENDIF
132100040708     C*** BLC
132200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
132300040708     C                   EVAL      PiStr=SkSplitCSV(i)
132400040708     C                   EXSR      CHKNUM
132500040708     C                   IF        PiInt=*on
132600040708     C                   Z-ADD     PiVal         VAOBLC
132700040708     C                   ELSE
132800040708     C                   SETON                                        32
132900040708     C                   EVAL      VAOBLC = *zeros
133000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133100040708     C                             + ' ' + 'VAOBLC'
133200040708     C                   ENDIF
133300040708     C                   ENDIF
133400040708     C*** ATT
133500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
133600040708     C                   EVAL      PiStr=SkSplitCSV(i)
133700040708     C                   EXSR      CHKNUM
133800040708     C                   IF        PiInt=*on
133900040708     C                   Z-ADD     PiVal         VAOATT
134000040708     C                   ELSE
134100040708     C                   SETON                                        32
134200040708     C                   EVAL      VAOATT = *zeros
134300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134400040708     C                             + ' ' + 'VAOATT'
134500040708     C                   ENDIF
134600040708     C                   ENDIF
134700040708     C*** MTC
134800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
134900040708     C                   EVAL      PiStr=SkSplitCSV(i)
135000040708     C                   EXSR      CHKNUM
135100040708     C                   IF        PiInt=*on
135200040708     C                   Z-ADD     PiVal         VAOMTC
135300040708     C                   ELSE
135400040708     C                   SETON                                        32
135500040708     C                   EVAL      VAOMTC = *zeros
135600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135700040708     C                             + ' ' + 'VAOMTC'
135800040708     C                   ENDIF
135900040708     C                   ENDIF
136000040708     C*** KSC
136100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on      AND
136500030721     C                             PiVal<=9999999 AND
136600030721     C                             PiVal>*zeros
136700040708     C                   Z-ADD     PiVal         VAOKSC
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000040708     C                   EVAL      VAOKSC = *zeros
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200040708     C                             + ' ' + 'VAOKSC'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500040708     C*** POR
137600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on    AND
138000030721     C                             PiVal<=999   AND
138100030721     C                             PiVal>*zeros
138200040708     C                   Z-ADD     PiVal         VAOPOR
138300030721     C                   ELSE
138400030721     C                   SETON                                        32
138500040708     C                   EVAL      VAOPOR = *zeros
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700040708     C                             + ' ' + 'VAOPOR'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000040708     C*** POC
139100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
139200040708     C                   EVAL      PiStr=SkSplitCSV(i)
139300040708     C                   EXSR      CHKNUM
139400040708     C                   IF        PiInt=*on    AND
139500040708     C                             PiVal<=999   AND
139600040708     C                             PiVal>*zeros
139700040708     C                   Z-ADD     PiVal         VAOPOC
139800040708     C                   ELSE
139900040708     C                   SETON                                        32
140000040708     C                   EVAL      VAOPOC = *zeros
140100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140200040708     C                             + ' ' + 'VAOPOC'
140300040708     C                   ENDIF
140400040708     C                   ENDIF
140500040708     C*** STO
140600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
140700040708     C                   EVAL      PiStr=SkSplitCSV(i)
140800040708     C                   EXSR      CHKNUM
140900040708     C                   IF        PiInt=*on
141000040708     C                   Z-ADD     PiVal         VAOSTO
141100040708     C                   ELSE
141200040708     C                   SETON                                        32
141300040708     C                   EVAL      VAOSTO = *zeros
141400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141500040708     C                             + ' ' + 'VAOSTO'
141600040708     C                   ENDIF
141700040708     C                   ENDIF
141800030715     C*
141900030715     C                   ENDIF
142000030715     C                   EVAL      i = i + 1
142100030715     C                   ENDDO
142200040723     C*
142300030715     C                   ENDIF
142400020204     C*
142500010607     C                   ENDSR
142600010607     C*----------------------------------------------------*
142700020204
142800010330
142900010330
143000010330     C*----------------------------------------------------*
143100010330     C*  CONTROLLO NUMERICITA' CAMPI
143200010330     C*----------------------------------------------------*
143300010330     C     CHKNUM        BEGSR
143400010330     C*
143500010606     C                   IF        PiDecChr = *blanks
143600030715     C                   EVAL      PiDecChr = CharNUM
143700010606     C                   ENDIF
143800010606     C*
143900010606     C                   CALL(e)   'ISNUMERIC'
144000010330     C                   PARM                    PiStr            30
144100010606     C                   PARM                    PiDecChr          1
144200010330     C                   PARM      *ZEROS        PiVal            30 9
144300010330     C                   PARM      '0'           PiInt             1
144400010330     C                   PARM      '0'           PiNum             1
144500010330     C                   IF        %error
144600010606     C                   EVAL      PiNum=*off
144700010330     C                   ENDIF
144800010330     C*
144900010330     C                   ENDSR
145000010330     C***
145100010330
145200010601
145300010601
145400010601
145500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145600010601     C     repfil        BEGSR
145700010601     C*
145800010601     C                   if        invfil = *zeros and
145900010601     C                             depfil > *zeros and
146000010629     C                             (vinflg = *blanks or
146100010629     C                              vinflg = *zeros)
146200010601     C
146300010601     C                   eval      invfil = depfil
146400010601     C                   endif
146500010601     C*
146600010601     C                   if        depfil <> invfil and
146700010601     C                             invfil > *zeros
146800010601     C                   eval      flgMulti = '1'
146900010601     C                   if        vinflg = *blanks
147000010601     C                   add       1             cntNonEl
147100010601     C                   endif
147200010601     C                   endif
147300010601     C*
147400010601     C                   if        vinflg = '2'
147500010601     C                   eval      flgStato = '2'
147600010601     C                   endif
147700010601     C*
147800010601     C                   ENDSR
147900010601     C***
148000010601
148100010601
148200010601
148300010330
148400010330
148500010330
148600990920      /TITLE Invio dei dati al punto operativo.
148700000613     C     invio         BEGSR
148800990920     C*
148900990920     C                   reset                   dscmz
149000010601     C                   move      invfil        cmzdst
149100990920     C                   eval      cmzfld = vlrfou
149200990920     C                   eval      cmzmbd = vlrhdl
149300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
149400000710     C                   if        prmfir = *blanks
149500040708     C                   eval      cmzfla = 'FNVAO00F'
149600040708     C                   eval      cmzmba = 'FNVAO00F'
149700000710     C                   else
149800000710     C                   eval      cmzfla = prmfir
149900000710     C                   eval      cmzmba = prmfir
150000000710     C                   endif
150100990920     C                   eval      cmznrr = *zeros
150200990920     C                   move      §ctrok        cmznrr
150300021018     C                   eval      cmzlba = vlrfl1
150400990920     C                   call(e)   'TIS711C'
150500990920     C                   parm                    dscmz
150600990921     C                   parm      *blanks       esito
150700990920     C                   if        %error
150800990920     C                             or cmzerr = '1'
150900990921     C                             or esito  = '1'
151000000710     C                   eval      wrkesito = '3'
151100990920     C                   endif
151200990920     C*
151300000613     C                   ENDSR
151400990910
151500010601
151600010601
151700010601
151800010601
151900010601      /TITLE Invio dei dati al punto operativo.
152000010601     C     opeini        BEGSR
152100010601     C*
152200010601     C* Inizializzo flag e contatori operativi
152300010601     C                   movel     '0'           flgGiro           1
152400010601     C                   movel     '0'           flgMulti          1
152500010601     C                   movel     '1'           flgStato          1
152600010615     C                   movel     '0'           flgOk             1
152700010601     C                   z-add     *zeros        cntNonEl         10 0
152800010601     C                   z-add     *zeros        depfil            3 0
152900010601     C                   z-add     *zeros        invfil            3 0
153000010601     C*
153100010601     C                   ENDSR
153200010601     C***
153300010601
153400010601
153500050421
153600050421
153700050421     C     *pssr         BEGSR
153800050421     C*
153900050421     C                   eval      wrkesito = '2'
154000050421     C*
154100050421     C                   ENDSR     '*CANCL'
154200050421     C***
154300060306
154400060306
154500060306     C*-------------------------------------------------------------------------*
154600060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
154700060306     C*-------------------------------------------------------------------------*
154800060306     C     sr_Control    begsr
154900060306     C*
155000060306     C* inizializzo a ok il flag d controllo abilitazione
155100060306     C                   eval      Cli_Abil = *on
155200060306     C
155300060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
155400060306     C                   clear                   woriIDC
155500060306     C                   eval      kteCOD = 'VAP'
155600060306     C                   eval      kteKE1 = oriIDC
155700060306     C     keytbe        chain     TNTBE01L
155800060306     C                   if        %found(Tntbe01l)
155900060306     C                   eval      woriIDC = TbeKe2
156000060306     C                   else
156100060306     C                   eval      Cli_Abil = *off
156200060306     C                   endif
156300060306     C*
156400060306     C                   endsr
156500050421
156600010601
156700010330
156800010330
156900000613     C     *inzsr        BEGSR
157000990910     C*
157100990910     C     *entry        plist
157200990920     C                   parm                    tivlrds
157300990921     C                   parm      wrkesito      esito
157400000724     C                   parm                    prmlit
157500000710     C                   parm                    prmfir
157600010330     C*
157700010330     C* CALCOLA LA DATA CORRENTE
157800010330     C                   time                    wn14             14 0
157900040723     C                   movel     wn14          wn6               6 0          *ORA (6)
158000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
158100010330     C                   z-add     wn8           g08dat
158200010330     C                   z-add     *zeros        g08inv
158300010330     C                   movel     '0'           g08err
158400010330     C                   call      'XSRDA8'
158500010330     C                   parm                    wlbda8
158600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
158700040723     C*
158800040723     C* Campi riferito al database
158900040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
159000040723     C     *like         define    tbeke1        kteKE1
159100040723     C*
159200040723     C* Definizione chiavi di lettura
159300040723     C* TNTBE01L - parziale
159400040723     C     keytbe        klist
159500040723     C                   kfld                    kteCOD                         *UTENTE
159600040723     C                   kfld                    kteKE1                         *CODICE
159700000613     C*
159800000613     C                   ENDSR
159900000613     C***
160000990908
160100040709     Otitvd4p   E            riepilogo         2
160200990915     O                                              'Upload via Internet'
160300990915     O                                           +1 'Traduzione TIVIN00R -'
160400040708     O                                           +1 'FNVAOWWR'
160500990915     O                                           +1 'Testate bolle'
160600990915     O          E            riepilogo   2
160700990915     O                       wrkdata
160800990915     O                       wrkora              +1
160900990915     O                       procname            +1
161000990915     O          E            riepilogo   2
161100990915     O                                              'Cliente..................:'
161200040708     O                       VAOCOR        z     +1
161300990915     O          E            riepilogo   2
161400990920     O                                              'Riferimento Strategi.....:'
161500990920     O                       vlrhdl              +1
161600990915     O          E            riepilogo   2
161700990915     O                                              'Giusti...................:'
161800971022     O                       §CTROK        2   +  1
161900990915     O          E            riepilogo   2
162000990915     O                                              'Sbagliati e corretti.....:'
162100971022     O                       §CTRMO        2   +  1
162200990915     O          E            riepilogo   2
162300990915     O                                              'Sbagliati e scartati.....:'
162400971022     O                       §CTRNO        2   +  1
162500000613
162600040709     Otitvd4ps  E            testdett          2
162700000613     O                                              'Upload via Internet'
162800000613     O                                           +1 'Traduzione TIVIN00R -'
162900040708     O                                           +1 'FNVAOWWR'
163000000613     O                                           +1 'Testate bolle'
163100000616     O          E            testdett    3
163200000613     O                                           +2 'N° rec'
163300000613     O                                           +3 'Anteprima contenuto'
163400000616     O          E            rigadett    2
163500000613     O                       rrnum               +2
163600000621     O                       recko               +3
163700000616     O          E            findett     2
163800000613     O                       wrkdata
163900000613     O                       wrkora              +1
164000000613     O                       procname            +1
164100000616     O          E            findett     2
164200000613     O                                              'Cliente..................:'
164300040708     O                       VAOCOR        z     +1
164400000616     O          E            findett     2
164500000613     O                                              'Riferimento Strategi.....:'
164600000613     O                       vlrhdl              +1
164700000616     O          E            findett     2
164800000613     O                                              'Giusti...................:'
164900000613     O                       §CTROK        2   +  1
165000000616     O          E            findett     2
165100000613     O                                              'Sbagliati e corretti.....:'
165200000613     O                       §CTRMO        2   +  1
165300000616     O          E            findett     2
165400000613     O                                              'Sbagliati e scartati.....:'
165500000613     O                       §CTRNO        2   +  1
165600000613** CMD - COMANDI CL
165700040709OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
165800040709OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
165900040709DLTOVR FILE(TITVD4P TITVD4PS) LVL(*)
166000010606
166100000613
166200040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
166300040709VAORER;VAORSR;VAOINR;VAOLOR;VAOCAR;VAOPRR;VAOTER;VAORFA;
166400040709VAONCL;VAOPKG;ESITO;VAONO1;DATA INVIO RICHIESTA;CODICE RESO
166500040709
