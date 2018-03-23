000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200100308     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000313     F*
000400040723     Ffnacr01l  if   e           k disk
000500090618     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700040708     FFnvaowwr  O    E             DISK    usropn
000800100308     Ffnvaoe1l  uf a e           k disk    usropn
000900100308     F                                     extfile(LibFileVAOE)
001000040723     Ftiori00f  O    E             DISK
001100100308     Ftiore00f  O    E             DISK
001200050826     FTIVGD00F  O    E             DISK
001300000313     D*
001400040709     D*------------
001500040709     D* SCHIERE A PROGRAMMA
001600040709     D*------------
001700100317     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
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
002800100317     D doregen       e ds
002900100317     D trul33ds      e ds
003000100317     D kpjba         e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003700060306     D Cli_Abil        s              1    inz(*off)
003800060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003900100308     D §numori         s              7  0 inz
004000100308     D currSysNeta     s              8A   inz('*NULL')
004100100308     D LibFileVAOE     s             21A   inz
004200100308     D w_VAOEDATI_M    s                   like(vaoeDATI)
004300100308     D w_VAOEDATI_N    s                   like(vaoeDATI)
004400100317     D w_VAOEDATI_G    s                   like(vaoeDATI)
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
006400010330     D*------------------
006500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600010330     D*------------------
006700010330     D WLBDA8          DS                  INZ
006800010330     D  G08DAT                 1      8  0
006900010330     D  G08INV                 9     16  0
007000010330     D  G08ERR                17     17
007100010330     D  G08TGI                18     22  0
007200040723     D*------------------
007300040723     D* DS REPERIMENTO DATI CLIENTE
007400040723     D*-------------------
007500040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007600040723     D ACODS         E DS                  EXTNAME(CNACO00F)
007700040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007800040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007900040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008000080702     D*------------------
008100130626
008200130626     D*
008300130626     D TxtInOut        S           2048
008400130626     D ElencoChar      S            256
008500130626     D TipoElenco      S              1
008600130626     D CharSost        S              1
008700130626     D UpperCase       S              1
008800130626     D ChkNull         S              1
008900130626     D CharNull        S              1
009000130626     D EsitoBon        S              1
009100130626
009200080702
009300080702     D*------------------
009400080702     D* LINKING A DEFINIZIONI ESTERNE
009500080702     D*------------------
009600090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009800091026
009900091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
010000091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
010100100308
010200100308     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
010300100308     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
010400080702
010500100308
010600100308
010700031201     D*------------------
010800031201     D* Costanti
010900031201     D*------------------
011000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011200010330
011300100308     C*
011400100308     C* Reperisco il sistema AS/400 corrente
011500100308     C                   callp     UBRTVNETA_Rtv(currSysNeta)
011600100308     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
011700100308     C                   eval      LibFileVAOE = 'FILTRAGRU/FNVAOE1L'
011800100308     C                   else
011900100308     C                   eval      LibFileVAOE = 'FILTRAGRPF/FNVAOE1L'
012000100308     C                   endif
012100100308     C*
012200100308     C                   if        not %open(FNVAOE1L)
012300100308     C                   open      FNVAOE1L
012400100308     C                   endif
012500100308     C*
012600000913     C                   reset                   rrnum
012700990921     C                   reset                   esito
012800990921     C                   reset                   wrkesito
012900010601     C*
013000010601     C                   exsr      opeini
013100041011     C                   exsr      PREELA
013200040708     C                   exsr      rwvao
013300010601     C*
013400100308     C                   if        %open(FNVAOE1L)
013500100308     C                   close     FNVAOE1L
013600100308     C                   endif
013700100308     C*
013800010601     C                   seton                                        lr
013900010601
014000010601
014100010601
014200010601
014300010601     C*--------------------------------------------------------
014400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014500010601     C*--------------------------------------------------------
014600010601     C     PREELA        BEGSR
014700010601     C*
014800010601     C* SE OCCORRE SPEDIRE IN FILIALE
014900041011     C                   if        flgGiro = '0'
015000010601     C*
015100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015200010601     C                   eval      flgGiro = '1'
015300041011     C*
015400041011     C                   endif
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
016900000616     C*
017000010601     C                   ENDSR
017100010601     C***
017200000613
017300010330
017400010601
017500010601
017600010601
017700010330     C*--------------------------------------------------------
017800040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
017900010330     C*--------------------------------------------------------
018000040708     C     RWVAO         BEGSR
018100010330     C*
018200010330     C                   if        not %open(tivin00r)
018300010330     C                   open      tivin00r
018400010330     C                   endif
018500040708     C                   if        not %open(fnvaowwr)
018600040708     C                   open      fnvaowwr
018700010330     C                   endif
018800010330     C*
018900010604     C                   clear                   §CTROK
019000010604     C                   clear                   §CTRMO
019100010604     C                   clear                   §CTRNO
019200100316     C*
019300100316     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019400100316     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019500100316     C                   exsr      inzvar
019600100316     C                   exsr      impvao
019700010330     C*
019800030822     C                   do        *HIVAL
019900010330     C*
020000030822     C                   read      tivin00r                               70
020100010618     C*
020200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020300010618     C                   if        vinflg = '1'
020400010618     C                   eval      flgOk = '1'
020500010618     C                   endif
020600010618     C*
020700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020800040510     C                   z-add     *zeros        wLenVINDTA        4 0
020900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021300040510     C                   z-add     1             wFlgVINDTA
021400040510     C                   leave
021500040510     C                   endif
021600040510     C                   enddo
021700040510     C*
021800040218     C                   if        vindta > *blanks AND
021900040510     C                             wFlgVINDTA = 1
022000010330     C                   add       1             rrnum
022100010330     C*
022200010601     C                   if        *in70 = *off and
022300010330     C                             (vinflg = *blanks
022400010330     C                              or vinflg = '0'
022500010330     C                              or vinflg = '2')
022600010330     C*
022700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022800010711     C                   if        vinflg = *blanks or vinflg = '0'
022900010711     C                   clear                   vinmsg
023000010711     C                   endif
023100010601     C*
023200010330     C                   exsr      inzvar
023300040708     C                   exsr      impvao
023400010601     C*
023500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023600040708     C                   eval      depfil = VAOPOE
023700010601     C                   exsr      repfil
023800010601     C                   if        depfil = invfil
023900021025     C                   if        vlrpoi = 999
024000040708     C                   move(P)   invfil        VAOPOE
024100021025     C                   else
024200040708     C                   move(P)   vlrpoi        VAOPOE
024300021025     C                   endif
024400010601     C*
024500010604     C* Ebbene...
024600010604     C*
024700030822     C  N31              add       1             §CTROK            7 0
024800030822     C   32              add       1             §CTRMO            7 0
024900030822     C   31              add       1             §CTRNO            7 0
025000030822     C                   if        wGiro = 2
025100040723     C  N31              EXSR      CHKCOR
025200100308     C  N31              EXSR      EXEFORZ
025300100308     C  N31              EXSR      WRIORI
025400100308     C  N31              EXSR      WRIORE
025500050826     C  N31              EXSR      WRIVGD
025600100308     C  N31              EXSR      WRIVAOE
025700040723     C  N31              write     FNVAO000
025800030822     C                   endif
025900010604     C*
026000010604     C                   if        *in31 = *off and
026100010604     C                             *in32 = *off
026200010604     C                   eval      vinflg = '1'
026300010604     C                   else
026400010604     C                   eval      vinflg = '2'
026500010604     C                   endif
026600010604     C                   endif
026700010601     C*
026800010604     C                   endif
026900010604     C*
027000010330     C                   else
027100010330     C                   eval      vinflg = '1'
027200010330     C                   endif
027300010601     C*
027400010601     C  N70              update    tivin000
027500010330     C*
027600030822     C  N70              enddo
027700010601     C
027800010601     C*
027900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028000010601     C                   if        cntNonEl = *zeros or
028100010601     C                             flgMulti = '0'
028200010330     C* Se non ci sono record con errori ...
028300010601     C                   if        §ctrno = 0 and
028400010604     C                             §ctrmo = 0 and
028500010601     C                             flgStato <> '2'
028600010330     C* ... restituisco esito OK.
028700010330     C                   eval      wrkesito = '0'
028800010330     C                   else
028900010330     C                   if        §ctrok > 0
029000010330     C                   eval      wrkesito = '1'
029100010330     C                   else
029200010615     C                   if        flgOk = '0'
029300010615     C                   eval      wrkesito = '2'
029400010615     C                   else
029500010615     C                   eval      wrkesito = '6'
029600010615     C                   endif
029700010330     C                   endif
029800010330     C                   endif
029900010601     C                   else
030000010601     C                   eval      wrkesito = '9'
030100010601     C                   endif
030200010330     C*
030300010330     C                   if        %open(tivin00r)
030400010330     C                   close     tivin00r
030500010330     C                   endif
030600040708     C                   if        %open(fnvaowwr)
030700040708     C                   close     fnvaowwr
030800010330     C                   endif
030900010601     C*
031000010601     C                   if        vlrpoi <> 999
031100010601     C                   eval      invfil = vlrpoi
031200010601     C                   endif
031300010330     C*
031400010330     C                   if        §ctrok > 0
031500010601     C                             and invfil > *zeros
031600010330     C                   exsr      invio
031700010330     C                   endif
031800010601     C*
031900010618     C                   if        flgGiro = '1'
032000010601     C                   exsr      endela
032100010618     C                   endif
032200010330     C*
032300010330     C                   ENDSR
032400010330     C***
032500040723
032600040723
032700040723     C*------------------------------------------------------------------------*
032800040723     C* CHKCOR - Considerazioni su VAOCOR
032900040723     C*------------------------------------------------------------------------*
033000040723     C     CHKCOR        BEGSR
033100040723     C*
033200040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
033300040723     C                   if        vaoTCO = 'F'
033400040723     C* Quindi verifico se il codice ordinante non è valorizzato
033500040723     C                   if        vaoCOR = *zeros  AND
033600040723     C                             vaoRSO = *blanks AND
033700040723     C                             vaoINO = *blanks AND
033800040723     C                             vaoCAO = *blanks AND
033900040723     C                             vaoLOO = *blanks AND
034000040723     C                             vaoPRO = *blanks AND
034100040723     C                             vaoNAO = *blanks
034200040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
034300040723     C                   movel     '0'           wFlgAcr           1
034400040723     C                   move      vlrKSC        wCli              7 0
034500040723     C                   move(p)   wCli          acrCRO
034600040723     C     acrCRO        setll     fnacr01l
034700040723     C                   if        %found(fnacr01l)
034800040723     C                   read      fnacr01l
034900040723     C                   dow       not %eof(fnacr01l)
035000040723     C                   movel     acrCRO        wCliAcr           7 0
035100040723     C                   if        wCliAcr = wCli
035200040723     C                   eval      vaoCOR = acrCRO
035300040723     C                   movel     '1'           wFlgAcr
035400040723     C                   leave
035500040723     C                   else
035600040723     C                   leave
035700040723     C                   endif
035800040723     C                   read      fnacr01l
035900040723     C                   enddo
036000040723     C                   endif
036100040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
036200040723     C* dati anagrafici dell'ordinante dal piano dei conti
036300040723     C                   if        wFlgAcr = '0'
036400040723     C                   clear                   BS69DS
036500040723     C                   clear                   ACODS
036600040723     C                   clear                   INDDS
036700040723     C                   clear                   CLPDS
036800040723     C                   clear                   CLSDS
036900040723     C                   move(P)   vlrKSC        I69KAC
037000040723     C                   move(P)   vlrKSC        I69KIN
037100040723     C                   call      'TIBS69R'
037200040723     C                   parm                    BS69DS
037300040723     C                   parm                    ACODS
037400040723     C                   parm                    INDDS
037500040723     C                   parm                    CLPDS
037600040723     C                   parm                    CLSDS
037700040723     C     O69ERR        ifne      '1'
037800040723     C                   eval      vaoRSO = ACORAG
037900040723     C                   eval      vaoINO = INDVIA
038000040723     C                   movel(P)  INDCAP        vaoCAO
038100040723     C                   eval      vaoLOO = INDCIT
038200040723     C                   eval      vaoPRO = INDPRV
038300040723     C                   eval      vaoNAO = INDSTA
038400040723     C                   endif
038500040723     C                   endif
038600040723     C                   endif
038700040723     C                   endif
038800040723     C*
038900040723     C                   ENDSR
039000040723
039100040723
039200040723
039300040723     C*------------------------------------------------------------------------*
039400040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
039500040723     C*------------------------------------------------------------------------*
039600040723     C     WRIORI        BEGSR
039700040723     C*
039800040723     C* Quindi imposto i campi "extra-VAO"
039900040723     C                   if        vlrKSC = '0ORM0000'
040000040723     C*
040100040723     C                   if        vaoCOR > *zeros
040200040723     C                   movel     vaoCOR        wrkCOR7           7 0
040300040723     C*
040400040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
040500040723     C* codice padre. Se è un padre assegno già il codice ordinante.
040600040723     C                   clear                   tibs10ds
040700040723     C                   eval      d10drf = datcor                              *data riferimento
040800040723     C                   eval      d10paf = 'P'                                 *cerca il padre
040900040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
041000091026     C*
041100091026     C* Preparazione chiamata al TIBS10R1
041200091026     C                   clear                   tibs10r1ds
041300091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
041400091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
041500091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
041600091026     C*
041700091026     C                   call      'TIBS10R1'
041800091026     C                   parm                    tibs10r1ds
041900091026     C*
042000091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
042100040723     C*
042200040723     C                   if        d10err <> *blanks or                         *No unif. o padre
042300040723     C                             d10cop =  *zeros  or
042400040723     C                             d10cop =  d10cod
042500040723     C                   move      d10cod        oriIDC
042600040723     C                   else                                                   *Figlio
042700040723     C                   move      d10cop        oriIDC
042800040723     C                   endif
042900040723     C*
043000040723     C                   else
043100040723     C                   eval      oriIDC = *zeros
043200040723     C                   endif
043300040723     C*
043400040723     C                   else
043500040723     C                   eval      oriIDC = vlrKSC
043600040723     C                   endif
043700040723     C*
043800040723     C                   eval      oriPRG = §numori
043900040723     C                   eval      oriDER = datcor
044000040723     C                   eval      oriOER = wn6
044100040723     C                   eval      oriPOE = vaoPOE
044200040723     C                   eval      oriNSR = vaoNSR
044300100308     C                   eval      oriNOR = vaoNOR
044400040723     C                   eval      oriNRV = vaoNRV
044500040723     C                   eval      oriFLO = *blanks
044600040723     C*
044700100308     C* Valorizzo la chiave esterna sul file FNVAO
044800040723     C                   eval      dorm01 = vaoFLO
044900040723     C                   movel     §numori       §ormpg
045000100308     C*
045100100308     C* Valorizzo i flag operativi necessari x preavvisi orm
045200100308     C                   eval      §ORMPRE = 'M'
045300100308     C                   eval      §ORCOMC = 'N'
045400100308     C                   eval      §ORMFD  = 'S'
045500100309     C                   eval      §ORMKS  = 'S'
045600100309     C                   eval      §ORMFR  = 'S'
045700050412     C*
045800050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
045900040723     C                   eval      vaoFLO = dorm01
046000040723     C*
046100040723     C* ...quindi scrivo il file TIORI00F
046200040723     C                   WRITE     TIORI000
046300040723     C*
046400040723     C                   ENDSR
046500050826
046600100308
046700100308
046800100308     C*------------------------------------------------------------------------*
046900100308     C* WRIORE - Reperimento informazioni necessarie e scrittura del file spia TIORE00F
047000100308     C*------------------------------------------------------------------------*
047100100308     C     WRIORE        BEGSR
047200100308     C*
047300100308     C                   eval      OREPRG   = oriPRG
047400100308     C*
047500100308     C* Inserimento tipo record 'M' => Mail x preavviso
047600100308     C                   eval      VAOETRC  = 'M'
047700100308     C                   eval      VAOEDATI = w_VAOEDATI_M
047800100308     C*
047900100308     C                   WRITE     TIORE000
048000100308     C*
048100100308     C* Inserimento tipo record 'N' => Note aggiuntive
048200100308     C                   eval      VAOETRC  = 'N'
048300100308     C                   eval      VAOEDATI = w_VAOEDATI_N
048400100308     C*
048500100308     C                   WRITE     TIORE000
048600100317     C*
048700100317     C* Inserimento tipo record 'G' => Estensiioni ORM (tipo servizio, etc..)
048800100317     C                   eval      VAOETRC  = 'G'
048900100317     C                   eval      §ORETSP  = w_VAOEDATI_G
049000100317     C                   eval      VAOEDATI = DOREGEN
049100100317     C*
049200100317     C                   WRITE     TIORE000
049300100308     C*
049400100308     C                   ENDSR
049500100308
049600100308
049700100308
049800100308     C*------------------------------------------------------------------------*
049900100308     C* WRIVAOE - Reperimento informazioni necessarie e scrittura del file spia FNVAOE0F
050000100308     C*------------------------------------------------------------------------*
050100100308     C     WRIVAOE       BEGSR
050200100308     C*
050300100308     C                   eval      VAOECOR  = vaoCOR
050400100308     C                   eval      VAOEPOE  = vaoPOE
050500100308     C                   eval      VAOENSR  = vaoNSR
050600100308     C                   eval      VAOENOR  = vaoNOR
050700100308     C                   eval      VAOENRV  = vaoNRV
050800100308     C*
050900100308     C* Inserimento tipo record 'M' => Mail x preavviso
051000100308     C                   eval      VAOETRC  = 'M'
051100100308     C                   eval      VAOEDATI = w_VAOEDATI_M
051200100308     C*
051300100308     C                   WRITE     FNVAOE00
051400100308     C*
051500100308     C* Inserimento tipo record 'N' => Note aggiuntive
051600100308     C                   eval      VAOETRC  = 'N'
051700100308     C                   eval      VAOEDATI = w_VAOEDATI_N
051800100308     C*
051900100308     C                   WRITE     FNVAOE00
052000100317     C*
052100100317     C* Inserimento tipo record 'G' => Estensiioni ORM (tipo servizio, etc..)
052200100317     C                   eval      VAOETRC  = 'G'
052300100317     C                   eval      §ORETSP  = w_VAOEDATI_G
052400100317     C                   eval      VAOEDATI = DOREGEN
052500100317     C*
052600100317     C                   WRITE     FNVAOE00
052700100308     C*
052800100308     C                   ENDSR
052900050826
053000100308
053100100308
053200100308
053300100308     C*------------------------------------------------------------------------*
053400100308     C* EXEFORZ - Effettuo forzature
053500100308     C*------------------------------------------------------------------------*
053600100308     C     EXEFORZ       BEGSR
053700100308     C*
053800100308     C* Come prima cosa stacco un numeratore da AZNUM
053900100308     C                   clear                   trul33ds
054000100308     C                   eval      I33OPE = *zeros
054100100308     C                   eval      I33CNU = 600
054200100308     C                   eval      I33NUM = 1
054300100308     C                   movel     TRUL33DS      KPJBU
054400100308     C                   call      'TRUL33R'
054500100308     C                   parm                    KPJBA
054600100308     C                   movel     KPJBU         TRUL33DS
054700100308     C                   if        O33ERR = *zeros
054800100308     C                   z-add     O33NRF        §numori
054900100308     c                   else
055000100308     c                   eval      §numori = *all'9'
055100100308     C                   endif
055200100308     C*
055300100308     C                   eval      vaoNOR = §numori
055400100308     C*
055500100308     C                   ENDSR
055600100308
055700100308
055800100308
055900050826
056000050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
056100050826     C     wriVGD        BEGSR
056200050826     C*
056300050826     C* Reperisco la descrizione della fase da tabella
056400050826     C                   movel(p)  'FAR'         KteCOD
056500050826     C                   movel(p)  '000'         KteKE1
056600050826     C     KEYtbe        chain     tntbe01l
056700050826     C                   if        %found(tntbe01l)
056800050826     C                   movel(P)  tbeuni        DFAR
056900050826     C                   else
057000050826     C                   clear                   DFAR
057100050826     C                   endif
057200050826     C*
057300050826     C* Valorizzo buffer record
057400050826     C                   clear                   FNVAPDS
057500050826     C                   eval      vapIDC = oriIDC
057600050826     C                   eval      vapPOE = oriPOE
057700050826     C                   eval      vapRFA = vaoRFA
057800050826     C                   eval      vapPOG = oriPOE
057900050826     C                   eval      vapDAE = oriDER
058000050826     C                   eval      vapORE = oriOER
058100050826     C                   if        d§fardva <> *blanks
058200050826     C                   eval      vapDFA = d§fardva
058300050826     C                   else
058400050826     C                   eval      vapDFA = d§fardes
058500050826     C                   endif
058600050826     C*
058700050826     C                   clear                   tivgd000
058800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
058900050826     C                   eval      vgdTIP = 'VP'
059000050826     C                   eval      vgdKSU = vapIDC
059100050826     C                   eval      vgdTSC = 'WW'
059200050826     C                   eval      vgdDAT = datcor
059300090428     C                   eval      vgdPGM = 'TITV1P3R'
059400060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
059500060306     C                   exsr      sr_Control
059600060306     C                   if        Cli_Abil = *on
059700060306     C                   eval      vgdKSU = woriIDC
059800050826     C                   write     tivgd000
059900060306     C                   endif
060000050826     C*
060100050826     C                   ENDSR
060200050826     C*------------------------------------------------------------------------*
060300010601
060400010601
060500010601
060600010330     C*----------------------------------------------------*
060700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
060800010330     C*----------------------------------------------------*
060900010330     C     INZVAR        BEGSR
061000010330     C*
061100030822     C* Inizializzo variabili di wrk
061200020204     C                   Z-ADD     *zeros        Num5_0            5 0
061300030822     C*
061400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
061500040708     C                   CLEAR                   FNVAO000
061600100308     C                   CLEAR                   FNVAOE00
061700040723     C                   CLEAR                   TIORI000
061800100308     C                   CLEAR                   TIORE000
061900030822     C                   CLEAR                   SkSplitCSV
062000100308     C*
062100100308     C                   CLEAR                   w_VAOEDATI_M
062200100308     C                   CLEAR                   w_VAOEDATI_N
062300100317     C                   CLEAR                   w_VAOEDATI_G
062400100308     C*
062500030822     C                   EVAL      i = 1
062600030822     C                   EVAL      posDa = *zeros
062700030822     C                   EVAL      posA  = *zeros
062800030822     C*
062900030822     C* Reimposto i valori di default
063000030822     C                   EXSR      DEFCAM
063100030822     C*
063200010330     C                   ENDSR
063300010330     C*----------------------------------------------------*
063400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
063500010330     C*----------------------------------------------------*
063600010330     C     DEFCAM        BEGSR
063700030822     C*
063800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
063900030715     C* e delimitatore testo.
064000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
064100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
064200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
064300030822     C*
064400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
064500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
064600030715     C                   SETON                                        50
064700030715     C                   ELSE
064800030715     C                   SETOFF                                       50
064900030715     C                   ENDIF
065000040723     C*
065100080707     C* Imposto i valori d default d alcuni campi
065200100315     C***                EVAL      VAOCOR = 0634984000
065300100315     C***                EVAL      VAOKSC = 0634984
065400100315     C***                EVAL      VAOORR = 1030
065500100315     C***                EVAL      VAOPOE = 063
065600100309     C*
065700080708     C                   CLEAR                   DORM01
065800080708     C                   EVAL      VAOFLO = DORM01
065900080707     C*
066000090428     C                   EVAL      VAOPAG = 'O'
066100080707     C                   EVAL      VAOTCO = 'F'
066200080707     C                   EVAL      VAOTOR = 'S'
066300030822     C*
066400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
066500030822     C                   EVAL      posDaDft = 1
066600030822     C                   EVAL      posADft  = 0
066700030822     C                   EVAL      wGiroDft = 0
066800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
066900030822     C                             posDaDft > 0
067000030822     C*
067100030822     C* Gestisco il 1° giro
067200030822     C                   IF        wGiroDft = 0
067300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
067400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
067500030822     C* Incremento il contatore dei "giri"
067600030822     C                   EVAL      wGiroDft = 1
067700030822     C                   ELSE
067800030822     C                   EVAL      posDaDft = posADft
067900030822     C                   ENDIF
068000030822     C* Eseguo lo scan x trovare la fine del campo corrente
068100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
068200030822     C*
068300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
068400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
068500030822     C                   IF        posDaDft > 0 AND
068600030822     C                             posADft  > 0
068700040708     C* POE
068800040708     C                   IF        %subst(
068900040708     C                             %subst(vlrppt:posDaDft+1:
069000040708     C                             posADft-posDaDft-1):1:3)
069100040708     C                             = 'POE'
069200040708     C                   EVAL      PiStr=%trim(%subst(
069300040708     C                             %subst(vlrppt:posDaDft+1:
069400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500040708     C                   EXSR      CHKNUM
069600040708     C                   IF        PiInt=*on
069700040708     C                   Z-ADD     PiVal         VAOPOE
069800040708     C                   ENDIF
069900040708     C                   ENDIF
070000040708     C* TOR
070100040708     C                   IF        %subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):1:3)
070400040708     C                             = 'TOR'
070500040708     C                   EVAL      VAOTOR=%trim(%subst(
070600040708     C                             %subst(vlrppt:posDaDft+1:
070700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800040708     C                   ENDIF
070900040708     C* TCO
071000040708     C                   IF        %subst(
071100040708     C                             %subst(vlrppt:posDaDft+1:
071200040708     C                             posADft-posDaDft-1):1:3)
071300040708     C                             = 'TCO'
071400040708     C                   EVAL      VAOTCO=%trim(%subst(
071500040708     C                             %subst(vlrppt:posDaDft+1:
071600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700040708     C                   ENDIF
071800040707     C* COR
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200040707     C                             = 'COR'
072300030822     C                   EVAL      PiStr=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   EXSR      CHKNUM
072700030822     C                   IF        PiInt=*on
072800040708     C                   Z-ADD     PiVal         VAOCOR
072900030822     C                   ENDIF
073000030822     C                   ENDIF
073100040708     C* CRC
073200040708     C                   IF        %subst(
073300040708     C                             %subst(vlrppt:posDaDft+1:
073400040708     C                             posADft-posDaDft-1):1:3)
073500040708     C                             = 'CRC'
073600040708     C                   EVAL      PiStr=%trim(%subst(
073700040708     C                             %subst(vlrppt:posDaDft+1:
073800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900040708     C                   EXSR      CHKNUM
074000040708     C                   IF        PiInt=*on
074100040708     C                   Z-ADD     PiVal         VAOCRC
074200040708     C                   ENDIF
074300040708     C                   ENDIF
074400040708     C* FFD
074500040708     C                   IF        %subst(
074600040708     C                             %subst(vlrppt:posDaDft+1:
074700040708     C                             posADft-posDaDft-1):1:3)
074800040708     C                             = 'FFD'
074900040708     C                   EVAL      VAOFFD=%trim(%subst(
075000040708     C                             %subst(vlrppt:posDaDft+1:
075100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200040708     C                   ENDIF
075300040708     C* ORR
075400040708     C                   IF        %subst(
075500040708     C                             %subst(vlrppt:posDaDft+1:
075600040708     C                             posADft-posDaDft-1):1:3)
075700040708     C                             = 'ORR'
075800040708     C                   EVAL      PiStr=%trim(%subst(
075900040708     C                             %subst(vlrppt:posDaDft+1:
076000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100040708     C                   EXSR      CHKNUM
076200040708     C                   IF        PiInt=*on
076300040708     C                   Z-ADD     PiVal         VAOORR
076400040708     C                   ENDIF
076500040708     C                   ENDIF
076600040712     C* NCL
076700040712     C                   IF        %subst(
076800040712     C                             %subst(vlrppt:posDaDft+1:
076900040712     C                             posADft-posDaDft-1):1:3)
077000040712     C                             = 'NCL'
077100040712     C                   EVAL      PiStr=%trim(%subst(
077200040712     C                             %subst(vlrppt:posDaDft+1:
077300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400040712     C                   EXSR      CHKNUM
077500040712     C                   IF        PiInt=*on
077600040712     C                   Z-ADD     PiVal         VAONCL
077700040712     C                   ENDIF
077800040712     C                   ENDIF
077900040712     C* PKG
078000040712     C                   IF        %subst(
078100040712     C                             %subst(vlrppt:posDaDft+1:
078200040712     C                             posADft-posDaDft-1):1:3)
078300040712     C                             = 'PKG'
078400040712     C                   EVAL      PiStr=%trim(%subst(
078500040712     C                             %subst(vlrppt:posDaDft+1:
078600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700040712     C                   EXSR      CHKNUM
078800040712     C                   IF        PiNum=*on
078900040712     C                   Z-ADD     PiVal         VAOPKG
079000040712     C                   ENDIF
079100040712     C                   ENDIF
079200040712     C* VLM
079300040712     C                   IF        %subst(
079400040712     C                             %subst(vlrppt:posDaDft+1:
079500040712     C                             posADft-posDaDft-1):1:3)
079600040712     C                             = 'VLM'
079700040712     C                   EVAL      PiStr=%trim(%subst(
079800040712     C                             %subst(vlrppt:posDaDft+1:
079900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000040712     C                   EXSR      CHKNUM
080100040712     C                   IF        PiNum=*on
080200040712     C                   Z-ADD     PiVal         VAOVLM
080300040712     C                   ENDIF
080400040712     C                   ENDIF
080500040708     C* NAM
080600040708     C                   IF        %subst(
080700040708     C                             %subst(vlrppt:posDaDft+1:
080800040708     C                             posADft-posDaDft-1):1:3)
080900040708     C                             = 'NAM'
081000040708     C                   EVAL      VAONAM=%trim(%subst(
081100040708     C                             %subst(vlrppt:posDaDft+1:
081200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300040708     C                   ENDIF
081400040708     C* PAG
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800040708     C                             = 'PAG'
081900040708     C                   EVAL      VAOPAG=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300040708     C* KSC
082400040708     C                   IF        %subst(
082500040708     C                             %subst(vlrppt:posDaDft+1:
082600040708     C                             posADft-posDaDft-1):1:3)
082700040708     C                             = 'KSC'
082800040708     C                   EVAL      PiStr=%trim(%subst(
082900040708     C                             %subst(vlrppt:posDaDft+1:
083000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100040708     C                   EXSR      CHKNUM
083200040708     C                   IF        PiInt=*on
083300040708     C                   Z-ADD     PiVal         VAOKSC
083400040708     C                   ENDIF
083500040708     C                   ENDIF
083600040708     C* CTR
083700040708     C                   IF        %subst(
083800040708     C                             %subst(vlrppt:posDaDft+1:
083900040708     C                             posADft-posDaDft-1):1:3)
084000040708     C                             = 'CTR'
084100040708     C                   EVAL      VAOCTR=%trim(%subst(
084200040708     C                             %subst(vlrppt:posDaDft+1:
084300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084400040708     C                   ENDIF
084500040708     C* POC
084600040708     C                   IF        %subst(
084700040708     C                             %subst(vlrppt:posDaDft+1:
084800040708     C                             posADft-posDaDft-1):1:3)
084900040708     C                             = 'POC'
085000040708     C                   EVAL      PiStr=%trim(%subst(
085100040708     C                             %subst(vlrppt:posDaDft+1:
085200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085300040708     C                   EXSR      CHKNUM
085400040708     C                   IF        PiInt=*on
085500040708     C                   Z-ADD     PiVal         VAOPOC
085600040708     C                   ENDIF
085700040708     C                   ENDIF
085800040708     C* NO1
085900040708     C                   IF        %subst(
086000040708     C                             %subst(vlrppt:posDaDft+1:
086100040708     C                             posADft-posDaDft-1):1:3)
086200040708     C                             = 'NO1'
086300040708     C                   EVAL      VAONO1=%trim(%subst(
086400040708     C                             %subst(vlrppt:posDaDft+1:
086500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086600040708     C                   ENDIF
086700040708     C* DDT
086800030822     C                   IF        %subst(
086900030822     C                             %subst(vlrppt:posDaDft+1:
087000030822     C                             posADft-posDaDft-1):1:3)
087100040708     C                             = 'DDT'
087200040708     C                   EVAL      VAODDT=%trim(%subst(
087300030822     C                             %subst(vlrppt:posDaDft+1:
087400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087500030822     C                   ENDIF
087600040708     C* RFA
087700040708     C                   IF        %subst(
087800040708     C                             %subst(vlrppt:posDaDft+1:
087900040708     C                             posADft-posDaDft-1):1:3)
088000040708     C                             = 'RFA'
088100040708     C                   EVAL      VAORFA=%trim(%subst(
088200040708     C                             %subst(vlrppt:posDaDft+1:
088300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088400040708     C                   ENDIF
088500100401     C* SPI
088600100401     C                   IF        %subst(
088700100401     C                             %subst(vlrppt:posDaDft+1:
088800100401     C                             posADft-posDaDft-1):1:3)
088900100401     C                             = 'SPI'
089000100401     C                   EVAL      VAOSPI=%trim(%subst(
089100100401     C                             %subst(vlrppt:posDaDft+1:
089200100401     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089300100401     C                   ENDIF
089400040708     C* FLO
089500040708     C                   IF        %subst(
089600040708     C                             %subst(vlrppt:posDaDft+1:
089700040708     C                             posADft-posDaDft-1):1:3)
089800040708     C                             = 'FLO'
089900040708     C                   EVAL      VAOFLO=%trim(%subst(
090000040708     C                             %subst(vlrppt:posDaDft+1:
090100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090200040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
090300080702     C* e posizione 7 del VAOFLO
090400040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
090500040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
090600050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
090700040708     C                   ENDIF
090800030822     C* ...
090900030822     C                   ENDIF
091000030822     C                   ENDDO
091100020204     C*
091200010330     C                   ENDSR
091300010607     C*----------------------------------------------------*
091400040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
091500010607     C*----------------------------------------------------*
091600040708     C     IMPVAO        BEGSR
091700010607     C*
091800010607     C                   SETOFF                                       3132
091900030515     C*
092000040708     C                   MOVEL     datcor        VAODAO
092100040708     C                   MOVEL     wn14          VAOOAO
092200050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
092300050314     C     minu:maiu     XLATE     vindta        vindta
092400010607     C*
092500030715     C**********
092600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
092700030715     C**********
092800030715     C                   IF        wGiro = *zeros
092900040709     C*
093000040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
093100040709     C                   EVAL      depvinDTA = vinDTA
093200040709     C*
093300040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
093400040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
093500100317     C                                      %trim(intcol(3))+%trim(intcol(4))
093600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
093700050314     C**!!!minu:maiu     XLATE     vindta        vindta
093800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093900040324     C                   DOW       posDa <= %len(%trimr(vindta))
094000030715     C*
094100030715     C* Gestisco il 1° campo
094200030715     C                   IF        i = 1
094300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094400030715     C                   EVAL      posDa = 1
094500030715     C* Eseguo lo scan x trovare la fine del primo campo
094600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094700030715     C* A questo punto "estraggo" il campo corrente
094800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094900030715     C                                                    (posA-posDa))
095000030715     C* X i campi successivi al 1°
095100030715     C                   ELSE
095200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095300030715     C                   EVAL      posDa = posA + 1
095400030715     C* Eseguo lo scan x trovare la fine del campo corrente
095500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095600030715     C* Gestisco l'ultimo campo
095700030715     C                   IF        posA = *zeros
095800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095900030715     C* A questo punto "estraggo" il campo corrente
096000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096100030715     C                                                    (posA-posDa))
096200030715     C                   LEAVE
096300030715     C                   ELSE
096400030715     C* A questo punto "estraggo" il campo corrente
096500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096600030715     C                                                    (posA-posDa))
096700030715     C                   ENDIF
096800030715     C                   ENDIF
096900030715     C* Incremento il contatore d campo
097000030715     C                   EVAL      i = i +1
097100030715     C                   ENDDO
097200030715     C*
097300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097400030715     C* delimitazione testo
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitFLD)
097700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
097800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
097900030715     C                   EVAL      i = i + 1
098000030715     C                   ENDDO
098100030715     C                   EVAL      wGiro = 1
098200040709     C* Ripristino la 1° riga originale
098300040709     C                   EVAL      vinDTA = depvinDTA
098400030715     C                   ELSE
098500030715     C                   EVAL      wGiro = 2
098600031204     C**********
098700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
098800031204     C* d separatore campo
098900031204     C**********
099000040709     C                   EVAL      vindta = %trim(vindta)
099100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
099200040709     C                   EVAL      vindta = %subst(vindta:2)
099300040709     C                   ENDDO
099400040709     C*
099500040709     C                   Z-ADD     *zeros        lunghInput        4 0
099600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
099800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
099900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
100000040709     C                   ENDDO
100100030715     C**********
100200030715     C* Effettuo lo split del campo dati d input
100300030715     C**********
100400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
100500040324     C                   DOW       posDa <= %len(%trimr(vindta))
100600030715     C*
100700030715     C* Gestisco il 1° campo
100800030715     C                   IF        i = 1
100900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
101000030715     C                   EVAL      posDa = 1
101100030715     C* Eseguo lo scan x trovare la fine del primo campo
101200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
101300030715     C* A questo punto "estraggo" il campo corrente
101400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101500030715     C                                                    (posA-posDa))
101600030715     C* X i campi successivi al 1°
101700030715     C                   ELSE
101800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
101900030715     C                   EVAL      posDa = posA + 1
102000031204     C* Verifico che nn vi sia il campo nn valorizzato
102100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
102200031204     C* Se campo nn valorizzato skippo al prossimo
102300031204     C                   EVAL      SkSplitCSV(i) = *blanks
102400031204     C                   EVAL      posA  = posA + 1
102500031204     C                   ELSE
102600030715     C* Eseguo lo scan x trovare la fine del campo corrente
102700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102800030715     C* Gestisco l'ultimo campo
102900030715     C                   IF        posA = *zeros
103000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
103100030715     C* A questo punto "estraggo" il campo corrente
103200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103300030715     C                                                    (posA-posDa))
103400030715     C                   LEAVE
103500030715     C                   ELSE
103600030715     C* A questo punto "estraggo" il campo corrente
103700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103800030715     C                                                    (posA-posDa))
103900030715     C                   ENDIF
104000031204     C                   ENDIF
104100030715     C                   ENDIF
104200030715     C* Incremento il contatore d campo
104300030715     C                   EVAL      i = i +1
104400030715     C                   ENDDO
104500030715     C*
104600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
104700030715     C* delimitazione testo
104800030715     C                   EVAL      i = 1
104900030715     C                   DOW       i < %elem(SkSplitCSV)
105000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
105100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
105200030715     C                   EVAL      i = i + 1
105300030715     C                   ENDDO
105400030715     C*
105500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
105600030715     C                   EVAL      i = 1
105700030715     C                   DOW       i < %elem(SkSplitFLD)
105800030715     C*
105900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
106000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
106100030715     C                             SkSplitCSV(i) <> *blanks
106200030721     C*** ATB
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
106400040708     C                   EVAL      VAOATB = SkSplitCSV(i)
106500030721     C                   ENDIF
106600040708     C*** TOR
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
106800040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
106900040708     C                   ENDIF
107000040708     C*** TCO
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
107200040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
107300040708     C                   ENDIF
107400040708     C*** RSO
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
107600040708     C                   EVAL      VAORSO = SkSplitCSV(i)
107700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107800040708     C     '@':'A'       XLATE     VAORSO        VAORSO
107900030715     C* ==
108000030715     C                   ENDIF
108100040708     C*** INO
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
108300040708     C                   EVAL      VAOINO = SkSplitCSV(i)
108400030715     C                   ENDIF
108500040708     C*** LOO
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
108700040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
108800030715     C                   ENDIF
108900040708     C*** PRO
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
109100040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
109200030715     C                   ENDIF
109300040708     C*** NAO
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
109500040708     C                   EVAL      VAONAO = SkSplitCSV(i)
109600030721     C                   ENDIF
109700040708     C*** RSR
109800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
109900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
110000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
110100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
110200040708     C* ==
110300040708     C                   IF        VAORSR = *blanks
110400040708     C                   SETON                                        31
110500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110600040708     C                             + ' ' + 'VAORSR'
110700040708     C                   ENDIF
110800040708     C                   ENDIF
110900040708     C*** INR
111000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
111100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
111200040708     C                   IF        VAOINR = *blanks
111300040708     C                   SETON                                        31
111400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040708     C                             + ' ' + 'VAOINR'
111600040708     C                   ENDIF
111700040708     C                   ENDIF
111800040708     C*** LOR
111900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
112000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
112100040708     C                   IF        VAOLOR = *blanks
112200040708     C                   SETON                                        31
112300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112400040708     C                             + ' ' + 'VAOLOR'
112500040708     C                   ENDIF
112600040708     C                   ENDIF
112700040708     C*** PRR
112800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
112900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
113000040708     C                   IF        VAOPRR = *blanks
113100040708     C                   SETON                                        31
113200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113300040708     C                             + ' ' + 'VAOPRR'
113400040708     C                   ENDIF
113500040708     C                   ENDIF
113600040708     C*** NAR
113700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
113800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
113900040708     C                   ENDIF
114000040708     C*** RER
114100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
114200040708     C                   EVAL      VAORER = SkSplitCSV(i)
114300040708     C                   ENDIF
114400040708     C*** TER
114500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
114600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
114700040708     C                   ENDIF
114800040708     C*** RSC
114900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
115000040708     C                   EVAL      VAORSC = SkSplitCSV(i)
115100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
115200040708     C     '@':'A'       XLATE     VAORSC        VAORSC
115300040708     C* ==
115400040708     C                   ENDIF
115500040708     C*** INC
115600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
115700040708     C                   EVAL      VAOINC = SkSplitCSV(i)
115800040708     C                   ENDIF
115900040708     C*** LOC
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
116100040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
116200040708     C                   ENDIF
116300040708     C*** PRC
116400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
116500040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
116600040708     C                   ENDIF
116700040708     C*** NAC
116800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
116900040708     C                   EVAL      VAONAC = SkSplitCSV(i)
117000040708     C                   ENDIF
117100040708     C*** FFD
117200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
117300040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
117400040708     C                   ENDIF
117500040708     C*** RMP
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
117700040708     C                   EVAL      VAORMP = SkSplitCSV(i)
117800040708     C                   ENDIF
117900040708     C*** NAM
118000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
118100040708     C                   EVAL      VAONAM = SkSplitCSV(i)
118200040708     C                   ENDIF
118300040708     C*** PAG
118400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
118500040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
118600040708     C                   ENDIF
118700040708     C*** CTR
118800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
118900040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
119000040708     C                   ENDIF
119100040708     C*** ZOR
119200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
119300040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
119400040708     C                   ENDIF
119500040708     C*** NO1
119600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
119700040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
119800030715     C                   ENDIF
119900040708     C*** NO2
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
120100040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
120200030715     C                   ENDIF
120300040708     C*** DDT
120400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
120500040708     C                   EVAL      VAODDT = SkSplitCSV(i)
120600030715     C                   ENDIF
120700040708     C*** RFA
120800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
120900040708     C                   EVAL      VAORFA = SkSplitCSV(i)
121000030715     C                   ENDIF
121100040708     C*** SPI
121200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
121300040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
121400030715     C                   ENDIF
121500040708     C*** FLO
121600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
121700040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
121800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
121900080708     C* e la posizione 7 del VAOFLO
122000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
122100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
122200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
122300030715     C                   ENDIF
122400050314
122500050412      *** DAR
122600050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
122700050412     C                   EVAL      PiStr=SkSplitCSV(i)
122800050412     C                   EXSR      CHKNUM
122900050412     C                   IF        PiInt=*on
123000050412     C                   Z-ADD     PiVal         VAODAR
123100050412     C                   ELSE
123200050412     C                   SETON                                        32
123300050412     C                   EVAL      VAODAR = *zeros
123400050412     C                   EVAL      vinmsg = %trimr(vinmsg)
123500050412     C                             + ' ' + 'VAODAR'
123600050412     C                   ENDIF
123700050314     C                   ENDIF
123800030715     C
123900030721     C*
124000030721     C* Reperisco quindi i campi numerici...
124100040708     C*** POE
124200040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
124300040723     C                   EVAL      PiStr=SkSplitCSV(i)
124400040723     C                   EXSR      CHKNUM
124500040723     C                   IF        PiInt=*on    AND
124600040723     C                             PiVal<=999   AND
124700040723     C                             PiVal>*zeros
124800040723     C                   Z-ADD     PiVal         VAOPOE
124900040723     C                   ELSE
125000040723     C                   SETON                                        32
125100040723     C                   EVAL      VAOPOE = *zeros
125200040723     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040723     C                             + ' ' + 'VAOPOE'
125400040723     C                   ENDIF
125500040723     C                   ENDIF
125600040708     C*** COR
125700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
125800040708     C                   EVAL      PiStr=SkSplitCSV(i)
125900040708     C                   EXSR      CHKNUM
126000040708     C                   IF        PiInt=*on
126100040708     C                   Z-ADD     PiVal         VAOCOR
126200040708     C                   ELSE
126300040708     C                   SETON                                        31
126400040708     C                   EVAL      VAOCOR = *zeros
126500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126600040708     C                             + ' ' + 'VAOCOR'
126700040708     C                   ENDIF
126800040708     C                   ENDIF
126900040708     C*** CAO
127000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on
127400030721     C                   Z-ADD     PiVal         Num5_0
127500040708     C                   MOVEL(p)  Num5_0        VAOCAO
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800040708     C                   EVAL      VAOCAO = *zeros
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000040708     C                             + ' ' + 'VAOCAO'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300040708     C*** CRA
128400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on
128800040708     C                   Z-ADD     PiVal         VAOCRA
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100040708     C                   EVAL      VAOCRA = *zeros
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300040708     C                             + ' ' + 'VAOCRA'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600040708     C*** CAR
129700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
129800040708     C                   EVAL      PiStr=SkSplitCSV(i)
129900040708     C                   EXSR      CHKNUM
130000040708     C                   IF        PiInt=*on
130100040708     C                   Z-ADD     PiVal         Num5_0
130200040708     C                   MOVEL(p)  Num5_0        VAOCAR
130300040708     C                   ELSE
130400040708     C                   SETON                                        31
130500040708     C                   EVAL      VAOCAR = *zeros
130600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130700040708     C                             + ' ' + 'VAOCAR'
130800040708     C                   ENDIF
130900040708     C                   ENDIF
131000040708     C*** CRC
131100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
131200040708     C                   EVAL      PiStr=SkSplitCSV(i)
131300040708     C                   EXSR      CHKNUM
131400040708     C                   IF        PiInt=*on
131500040708     C                   Z-ADD     PiVal         VAOCRC
131600040708     C                   ELSE
131700040708     C                   SETON                                        32
131800040708     C                   EVAL      VAOCRC = *zeros
131900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132000040708     C                             + ' ' + 'VAOCRC'
132100040708     C                   ENDIF
132200040708     C                   ENDIF
132300040708     C*** CAC
132400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
132500040708     C                   EVAL      PiStr=SkSplitCSV(i)
132600040708     C                   EXSR      CHKNUM
132700040708     C                   IF        PiInt=*on
132800040708     C                   Z-ADD     PiVal         Num5_0
132900040708     C                   MOVEL(p)  Num5_0        VAOCAC
133000040708     C                   ELSE
133100040708     C                   SETON                                        32
133200040708     C                   EVAL      VAOCAC = *zeros
133300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133400040708     C                             + ' ' + 'VAOCAC'
133500040708     C                   ENDIF
133600040708     C                   ENDIF
133700040708     C*** DAR
133800090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
133900090428     C                   EVAL      PiStr=SkSplitCSV(i)
134000090428     C                   EXSR      CHKNUM
134100090428     C                   IF        PiInt=*on
134200090428     C                   Z-ADD     PiVal         VAODAR
134300090428     C                   ELSE
134400090428     C                   SETON                                        32
134500090428     C                   EVAL      VAODAR = *zeros
134600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
134700090428     C                             + ' ' + 'VAODAR'
134800090428     C                   ENDIF
134900090428     C                   ENDIF
135000040708     C*** ORR
135100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
135200040708     C                   EVAL      PiStr=SkSplitCSV(i)
135300040708     C                   EXSR      CHKNUM
135400040708     C                   IF        PiInt=*on
135500040708     C                   Z-ADD     PiVal         VAOORR
135600040708     C                   ELSE
135700040708     C                   SETON                                        32
135800040708     C                   EVAL      VAOORR = *zeros
135900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136000040708     C                             + ' ' + 'VAOORR'
136100040708     C                   ENDIF
136200040708     C                   ENDIF
136300030721     C*** NCL
136400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
136500030721     C                   EVAL      PiStr=SkSplitCSV(i)
136600030721     C                   EXSR      CHKNUM
136700030721     C                   IF        PiInt=*on
136800040723     C                   IF        PiVal > *zeros
136900040708     C                   Z-ADD     PiVal         VAONCL
137000040723     C                   ENDIF
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300040708     C                   EVAL      VAONCL = *zeros
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500040708     C                             + ' ' + 'VAONCL'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800040708     C*** PKG
137900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiNum=*on
138300040723     C                   IF        PiVal > *zeros
138400040708     C                   Z-ADD     PiVal         VAOPKG
138500040723     C                   ENDIF
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800040708     C                   EVAL      VAOPKG = *zeros
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000040708     C                             + ' ' + 'VAOPKG'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300040708     C*** VLM
139400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
139500040708     C                   EVAL      PiStr=SkSplitCSV(i)
139600040708     C                   EXSR      CHKNUM
139700040708     C                   IF        PiNum=*on
139800040708     C                   Z-ADD     PiVal         VAOVLM
139900040708     C                   ELSE
140000040708     C                   SETON                                        32
140100040708     C                   EVAL      VAOVLM = *zeros
140200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140300040708     C                             + ' ' + 'VAOVLM'
140400040708     C                   ENDIF
140500040708     C                   ENDIF
140600040708     C*** BNC
140700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
140800040708     C                   EVAL      PiStr=SkSplitCSV(i)
140900040708     C                   EXSR      CHKNUM
141000040708     C                   IF        PiInt=*on
141100040708     C                   Z-ADD     PiVal         VAOBNC
141200040708     C                   ELSE
141300040708     C                   SETON                                        32
141400040708     C                   EVAL      VAOBNC = *zeros
141500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141600040708     C                             + ' ' + 'VAOBNC'
141700040708     C                   ENDIF
141800040708     C                   ENDIF
141900040708     C*** BLC
142000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
142100040708     C                   EVAL      PiStr=SkSplitCSV(i)
142200040708     C                   EXSR      CHKNUM
142300040708     C                   IF        PiInt=*on
142400040708     C                   Z-ADD     PiVal         VAOBLC
142500040708     C                   ELSE
142600040708     C                   SETON                                        32
142700040708     C                   EVAL      VAOBLC = *zeros
142800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142900040708     C                             + ' ' + 'VAOBLC'
143000040708     C                   ENDIF
143100040708     C                   ENDIF
143200040708     C*** ATT
143300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
143400040708     C                   EVAL      PiStr=SkSplitCSV(i)
143500040708     C                   EXSR      CHKNUM
143600040708     C                   IF        PiInt=*on
143700040708     C                   Z-ADD     PiVal         VAOATT
143800040708     C                   ELSE
143900040708     C                   SETON                                        32
144000040708     C                   EVAL      VAOATT = *zeros
144100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144200040708     C                             + ' ' + 'VAOATT'
144300040708     C                   ENDIF
144400040708     C                   ENDIF
144500040708     C*** MTC
144600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
144700040708     C                   EVAL      PiStr=SkSplitCSV(i)
144800040708     C                   EXSR      CHKNUM
144900040708     C                   IF        PiInt=*on
145000040708     C                   Z-ADD     PiVal         VAOMTC
145100040708     C                   ELSE
145200040708     C                   SETON                                        32
145300040708     C                   EVAL      VAOMTC = *zeros
145400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145500040708     C                             + ' ' + 'VAOMTC'
145600040708     C                   ENDIF
145700040708     C                   ENDIF
145800040708     C*** KSC
145900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
146000030721     C                   EVAL      PiStr=SkSplitCSV(i)
146100030721     C                   EXSR      CHKNUM
146200030721     C                   IF        PiInt=*on      AND
146300030721     C                             PiVal<=9999999 AND
146400030721     C                             PiVal>*zeros
146500040708     C                   Z-ADD     PiVal         VAOKSC
146600030721     C                   ELSE
146700030721     C                   SETON                                        32
146800040708     C                   EVAL      VAOKSC = *zeros
146900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147000040708     C                             + ' ' + 'VAOKSC'
147100030721     C                   ENDIF
147200030721     C                   ENDIF
147300040708     C*** POR
147400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
147500030721     C                   EVAL      PiStr=SkSplitCSV(i)
147600030721     C                   EXSR      CHKNUM
147700030721     C                   IF        PiInt=*on    AND
147800030721     C                             PiVal<=999   AND
147900030721     C                             PiVal>*zeros
148000040708     C                   Z-ADD     PiVal         VAOPOR
148100030721     C                   ELSE
148200030721     C                   SETON                                        32
148300040708     C                   EVAL      VAOPOR = *zeros
148400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
148500040708     C                             + ' ' + 'VAOPOR'
148600030721     C                   ENDIF
148700030721     C                   ENDIF
148800040708     C*** POC
148900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
149000040708     C                   EVAL      PiStr=SkSplitCSV(i)
149100040708     C                   EXSR      CHKNUM
149200040708     C                   IF        PiInt=*on    AND
149300040708     C                             PiVal<=999   AND
149400040708     C                             PiVal>*zeros
149500040708     C                   Z-ADD     PiVal         VAOPOC
149600040708     C                   ELSE
149700040708     C                   SETON                                        32
149800040708     C                   EVAL      VAOPOC = *zeros
149900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
150000040708     C                             + ' ' + 'VAOPOC'
150100040708     C                   ENDIF
150200040708     C                   ENDIF
150300040708     C*** STO
150400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
150500040708     C                   EVAL      PiStr=SkSplitCSV(i)
150600040708     C                   EXSR      CHKNUM
150700040708     C                   IF        PiInt=*on
150800040708     C                   Z-ADD     PiVal         VAOSTO
150900040708     C                   ELSE
151000040708     C                   SETON                                        32
151100040708     C                   EVAL      VAOSTO = *zeros
151200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
151300040708     C                             + ' ' + 'VAOSTO'
151400040708     C                   ENDIF
151500040708     C                   ENDIF
151600100308     C*** vaoeDATI_M
151700100308     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEDATI_M'
151800130626     C                   EVAL      w_VAOEDATI_M = SkSplitCSV(i)
151900130626     C*
152000130626     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
152100130626     C                   EVAL      TxtInOut   = SkSplitCSV(i)
152200130626     C                   EVAL      EsitoBon   = *blanks
152300130626     C*
152400130626     C                   CALL      'XCHKCHAR'
152500130626     C                   PARM                    TxtInOut
152600130626     C                   PARM                    ElencoChar
152700130626     C                   PARM                    TipoElenco
152800130626     C                   PARM                    CharSost
152900130626     C                   PARM                    UpperCase
153000130626     C                   PARM                    ChkNull
153100130626     C                   PARM                    CharNull
153200130626     C                   PARM                    EsitoBon
153300130626     C*
153400130626     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
153500130626     C                   IF        EsitoBon = '1'
153600130626     C                   EVAL      w_VAOEDATI_M = TxtInOut
153700130626     C                   ENDIF
153800100308     C                   ENDIF
153900100308     C*** vaoeDATI_N
154000100308     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEDATI_N'
154100100308     C                   EVAL      w_VAOEDATI_N = SkSplitCSV(i)
154200100308     C                   ENDIF
154300100317     C*** vaoeDATI_G
154400100317     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEDATI_G'
154500100317     C                   EVAL      w_VAOEDATI_G = SkSplitCSV(i)
154600100317     C                   ENDIF
154700090428     C*
154800090428     C********** GESTIONE CAMPI PERSONALIZZATI
154900090428     C*
155000090428     C*** W_VAODAR
155100090428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
155200090604     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
155300090604     C                   EVAL      SkSplitCSV(i) = '0'+%trim(SkSplitCSV(i))
155400090604     C                   ENDIF
155500090428     C                   MOVEL     *blanks       wDAR              8
155600090428     C                   EVAL      wDAR=%subst(SkSplitCSV(i):5:4)+
155700090428     C                                  %subst(SkSplitCSV(i):3:2)+
155800090428     C                                  %subst(SkSplitCSV(i):1:2)
155900090428     C                   EVAL      PiStr=wDAR
156000090428     C                   EXSR      CHKNUM
156100090428     C                   IF        PiInt=*on
156200090428     C                   Z-ADD     PiVal         VAODAR
156300090428     C                   ELSE
156400090428     C                   SETON                                        32
156500090428     C                   EVAL      VAODAR = *zeros
156600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
156700090428     C                             + ' ' + 'VAODAR'
156800090428     C                   ENDIF
156900090428     C                   ENDIF
157000090428     C*** W_VAONO1_1
157100090428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO1_1'
157200090428     C                   EVAL      VAONO1='Dalle: '+%trim(SkSplitCSV(i))
157300090428     C                   ENDIF
157400090428     C*** W_VAONO1_2
157500090428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO1_2'
157600090902     C                   EVAL      VAONO1=%trim(VAONO1)+
157700090902     C                                    ' Alle: '+%trim(SkSplitCSV(i))
157800090428     C                   ENDIF
157900100323     C*** W_VAONO2_1
158000100323     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO2_1'
158100100323     C                   EVAL      VAONO2=%trim(VAONO2)+
158200100323     C                                    'Dalle: '+%trim(SkSplitCSV(i))
158300090428     C                   ENDIF
158400100323     C*** W_VAONO2_2
158500100323     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO2_2'
158600100323     C                   EVAL      VAONO2=%trim(VAONO2)+
158700090902     C                                    ' Alle: '+%trim(SkSplitCSV(i))
158800090428     C                   ENDIF
158900100316     C*** W_VAOKSC
159000100316     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOKSC'
159100100316     C                   EVAL      PiStr=SkSplitCSV(i)
159200100316     C                   EXSR      CHKNUM
159300100316     C                   IF        PiInt=*on
159400100316     C                   Z-ADD     PiVal         VAOKSC
159500100316     C                   EVAL      VAOCOR = VAOKSC * 1000
159600100316     C                   ELSE
159700100316     C                   SETON                                        32
159800100316     C                   EVAL      VAOKSC = *zeros
159900100316     C                   EVAL      vinmsg = %trimr(vinmsg)
160000100316     C                             + ' ' + 'VAOKSC VAOCOR'
160100100316     C                   ENDIF
160200100316     C                   ENDIF
160300100324     C*
160400100324     C* Verifica correttezza codice ordinante/pagante => altrimenti forzo "default"
160500100324     C                   IF        VAOKSC = 2551766 OR
160600100324     C                             VAOKSC = 2551767 OR
160700100324     C                             VAOKSC = 0635154 OR
160800100324     C                             VAOKSC = 0635155 OR
160900100324     C                             VAOKSC = 0635160 OR
161000100324     C                             VAOKSC = 0635161 OR
161100100324     C                             VAOKSC = 0635127 OR
161200130625     C                             VAOKSC = 0635189 OR
161300130625     C                             VAOKSC = 0636348
161400100324     C                   ELSE
161500100324     C                   EVAL      VAOKSC = 0635155
161600100324     C                   EVAL      VAOCOR = VAOKSC * 1000
161700100324     C                   ENDIF
161800090902     C*** VAONO2
161900100315     C***                EVAL      VAONO2 =
162000100315     C***                          'RITIRARE SOLO SE IMBALLO IDONEO !!'
162100090428     C*
162200090428     C****************************************
162300090428     C*
162400030715     C*
162500030715     C                   ENDIF
162600030715     C                   EVAL      i = i + 1
162700030715     C                   ENDDO
162800040723     C*
162900030715     C                   ENDIF
163000020204     C*
163100010607     C                   ENDSR
163200010607     C*----------------------------------------------------*
163300010330
163400080702
163500080702
163600080702     C*----------------------------------------------------*
163700080702     C*  CONTROLLO NUMERICITA' CAMPI
163800080702     C*----------------------------------------------------*
163900080702     C     CHKNUM        BEGSR
164000080702     C*
164100080702     C                   IF        PiDecChr = *blanks
164200080702     C                   EVAL      PiDecChr = ','
164300080702     C                   ENDIF
164400080702     C*
164500080702     C                   callp     UBISNUM_Check(PiStr
164600080702     C                                          :PiDecChr
164700080702     C                                          :PiVal
164800080702     C                                          :PiNum
164900080702     C                                          :PiInt)
165000080702     C*
165100080702     C                   ENDSR
165200080702     C***
165300010601
165400080702
165500010601
165600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165700010601     C     repfil        BEGSR
165800010601     C*
165900010601     C                   if        invfil = *zeros and
166000010601     C                             depfil > *zeros and
166100010629     C                             (vinflg = *blanks or
166200010629     C                              vinflg = *zeros)
166300010601     C
166400010601     C                   eval      invfil = depfil
166500010601     C                   endif
166600010601     C*
166700010601     C                   if        depfil <> invfil and
166800010601     C                             invfil > *zeros
166900010601     C                   eval      flgMulti = '1'
167000010601     C                   if        vinflg = *blanks
167100010601     C                   add       1             cntNonEl
167200010601     C                   endif
167300010601     C                   endif
167400010601     C*
167500010601     C                   if        vinflg = '2'
167600010601     C                   eval      flgStato = '2'
167700010601     C                   endif
167800010601     C*
167900010601     C                   ENDSR
168000010601     C***
168100010601
168200010601
168300010601
168400010330
168500010330
168600010330
168700990920      /TITLE Invio dei dati al punto operativo.
168800000613     C     invio         BEGSR
168900990920     C*
169000990920     C                   reset                   dscmz
169100010601     C                   move      invfil        cmzdst
169200990920     C                   eval      cmzfld = vlrfou
169300990920     C                   eval      cmzmbd = vlrhdl
169400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
169500000710     C                   if        prmfir = *blanks
169600040708     C                   eval      cmzfla = 'FNVAO00F'
169700040708     C                   eval      cmzmba = 'FNVAO00F'
169800000710     C                   else
169900000710     C                   eval      cmzfla = prmfir
170000000710     C                   eval      cmzmba = prmfir
170100000710     C                   endif
170200990920     C                   eval      cmznrr = *zeros
170300990920     C                   move      §ctrok        cmznrr
170400021018     C                   eval      cmzlba = vlrfl1
170500990920     C                   call(e)   'TIS711C'
170600990920     C                   parm                    dscmz
170700990921     C                   parm      *blanks       esito
170800990920     C                   if        %error
170900990920     C                             or cmzerr = '1'
171000990921     C                             or esito  = '1'
171100000710     C                   eval      wrkesito = '3'
171200990920     C                   endif
171300990920     C*
171400000613     C                   ENDSR
171500990910
171600010601
171700010601
171800010601
171900010601
172000010601      /TITLE Invio dei dati al punto operativo.
172100010601     C     opeini        BEGSR
172200010601     C*
172300010601     C* Inizializzo flag e contatori operativi
172400010601     C                   movel     '0'           flgGiro           1
172500010601     C                   movel     '0'           flgMulti          1
172600010601     C                   movel     '1'           flgStato          1
172700010615     C                   movel     '0'           flgOk             1
172800010601     C                   z-add     *zeros        cntNonEl         10 0
172900010601     C                   z-add     *zeros        depfil            3 0
173000010601     C                   z-add     *zeros        invfil            3 0
173100010601     C*
173200010601     C                   ENDSR
173300010601     C***
173400010601
173500010601
173600050421
173700050421
173800050421     C     *pssr         BEGSR
173900050421     C*
174000050421     C                   eval      wrkesito = '2'
174100050421     C*
174200050421     C                   ENDSR     '*CANCL'
174300050421     C***
174400060306
174500060306
174600060306     C*-------------------------------------------------------------------------*
174700060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
174800060306     C*-------------------------------------------------------------------------*
174900060306     C     sr_Control    begsr
175000060306     C*
175100060306     C* inizializzo a ok il flag d controllo abilitazione
175200060306     C                   eval      Cli_Abil = *on
175300060306     C
175400060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
175500060306     C                   clear                   woriIDC
175600060306     C                   eval      kteCOD = 'VAP'
175700060306     C                   eval      kteKE1 = oriIDC
175800060306     C     keytbe        chain     TNTBE01L
175900060306     C                   if        %found(Tntbe01l)
176000060306     C                   eval      woriIDC = TbeKe2
176100060306     C                   else
176200060306     C                   eval      Cli_Abil = *off
176300060306     C                   endif
176400060306     C*
176500060306     C                   endsr
176600050421
176700010601
176800010330
176900010330
177000000613     C     *inzsr        BEGSR
177100990910     C*
177200990910     C     *entry        plist
177300990920     C                   parm                    tivlrds
177400990921     C                   parm      wrkesito      esito
177500000724     C                   parm                    prmlit
177600000710     C                   parm                    prmfir
177700010330     C*
177800010330     C* CALCOLA LA DATA CORRENTE
177900010330     C                   time                    wn14             14 0
178000040723     C                   movel     wn14          wn6               6 0          *ORA (6)
178100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
178200010330     C                   z-add     wn8           g08dat
178300010330     C                   z-add     *zeros        g08inv
178400010330     C                   movel     '0'           g08err
178500010330     C                   call      'XSRDA8'
178600010330     C                   parm                    wlbda8
178700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
178800040723     C*
178900040723     C* Campi riferito al database
179000040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
179100040723     C     *like         define    tbeke1        kteKE1
179200040723     C*
179300040723     C* Definizione chiavi di lettura
179400040723     C* TNTBE01L - parziale
179500040723     C     keytbe        klist
179600040723     C                   kfld                    kteCOD                         *UTENTE
179700040723     C                   kfld                    kteKE1                         *CODICE
179800000613     C*
179900000613     C                   ENDSR
180000000613     C***
180100040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180200100315W_VAOKSC;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAOTER;VAORER;VAOEDATI_N;
180300100308VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;W_VAODAR;W_VAONO1_1;W_VAONO1_2;
180400100323W_VAONO2_1;W_VAONO2_2;FIL01;VAONCL;VAOPKG;FIL02;VAORFA;VAOEDATI_M;
180500100317VAOEDATI_G
