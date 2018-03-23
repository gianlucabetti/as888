000100170120     /*END
000200980506      *------------------------------------------------------------------------*
000300981124      * Stampa  primo/secondo  Sollecito Invio Documenti
000400980506      *------------------------------------------------------------------------*
000500990520      *
000600990520      * se tipo bolla uguale a F il sollecito è diretto alla filiale
000700990521      *
000800981124      ****************************************************************
000900981124      *  FASI C.A. FISSATE A PROGRAMMA
001000981124      ****************************************************************
001100990125      *    605 = 1° Sollecito invio documenti                (WFASO1)
001200990125      *    615 = 2° Sollecito invio documenti                (WFASO2)
001300990125      *    575 = Sollecito x chi non ancora inviato nulla    (WFASO0)
001400990125
001500010406      ****************************************************************
001600010406      *  TESTO LETTERA FISSO A PROGRAMMA PER RECUPERARE LE NOTE
001700010406      ****************************************************************
001800010406      *    A12 = 1° Sollecito invio documenti                (WTLSO1)
001900010406
002000980729      ****************************************************************
002100980729      *  RIEPILOGO INDICATORI
002200980729      ***************************************************************
002300981123      * 01   - Salta pagina
002400990125      * 02   - Salta ulteriore descrizione anomalia
002500990520      * 03   - stampo numero evento
002600010406      * 05   - STAMPO per postel
002700990125      * 06   - Stampo l'interlocutore
002800990310      * 08   - Cliente Estero documenti in lingua
002900990524      * 10   - Se esiste già il documento di richiesta risarcimento
003000990520      * 13   - stampo quantità danneggiate
003100041118      * 20   - mando via e-mail
003200041118      * 21   - Salta pagina via e-mail
003300980911      * 30   - Generico
003400980915      * 31   - Generico
003500980915      * 32   - Generico
003600990308      * 33   - Generico
003700990310      * 50   - Alta intensità per l'intestazione
003800990520      * 90   - Stampo Sollecito alla filiale
003900980729      ****************************************************************
004000980729
004100980730     H DECEDIT('0,') DATEDIT(*DMY/)
004200100709     h option(*nodebugio)
004300980729
004400981124     ffndct01l  uf   e           K DISK
004500981120     ffndcc01l  if   e           K DISK
004600990524     ffndcl01l  if   e           K DISK
004700010420     ffndcs01l  uf a e           K DISK
004800990520     ffndet01l  if   e           K DISK
004900081229     fTITAS30c  if   e           K DISK
005000081229     ftntmd01l  if   e           K DISK
005100981120     ffndcf01l  uf a e           K DISK
005200981123     Fazorg01l  if   e           k DISK
005300981230     Ftabel00f  if   E           k DISK
005400981123     ffidn28p   O    e             PRINTER  oflind(*IN01)  usropn
005500041118     ffidn28pm  O    e             PRINTER  oflind(*IN21)  usropn
005600010418     fPRTF198   O    F  198        PRINTER OFLIND(*INOF)
005700980729
005800980914      *----------------------------------
005900990520     D SO0             S            100    DIM(10) CTDATA PERRCD(1)             Mai ricevuto documen
006000990520     D SO1             S            100    DIM(8)  CTDATA PERRCD(1)             testo lettera 1° sol
006100990520     D SO2             S            100    DIM(10) CTDATA PERRCD(1)             testo lettera 2° sol
006200990520     D SO3             S            100    DIM(4)  CTDATA PERRCD(1)             Sollecito Filiale
006300041119     D CMD             S             85    DIM(6)  CTDATA PERRCD(1)             OVRPRTF
006400120418     D DOA             S            100    DIM(5)  CTDATA PERRCD(1)             Testo documenti A
006500120418     D DOB             S            100    DIM(6)  CTDATA PERRCD(1)             Testo Documenti B
006600120418     D DOC             S            100    DIM(4)  CTDATA PERRCD(1)             Testo Documenti C
006700990308     D CDA             S              2    DIM(3)  CTDATA PERRCD(3)             Cod.doc.x testo A
006800990308     D CDB             S              2    DIM(4)  CTDATA PERRCD(4)             Cod.doc.x testo B
006900990308     D CDC             S              2    DIM(1)  CTDATA PERRCD(1)             Cod.doc.x testo C
007000010411     D TES             S            100    DIM(6)  CTDATA PERRCD(1)             TESTATA STAMPA
007100010420     D NOTE            S             35    DIM(2)  CTDATA PERRCD(1)             note
007200981123     D w35             S             35    DIM(4)                               descr. tipo anomalia
007300990308     D cd              S              2    DIM(250)                             codice documenti
007400010411     D cdf             S              2    DIM(250)                             codice documenti
007500980915      *----------------------------------
007600110526     D KBAR            C                   CONST('BRT  S.p.A.')
007700981123     D wsocie          S             20
007800990603     D command         S             85
007900981124     D lung            S             15  5
008000980730      *----------------------------------
008100980730     D WLBDAT          DS                  INZ
008200980730     D  g02dat                 1      8  0
008300980730     D  g02inv                 9     16  0
008400980730     D  g02err                17     17
008500980730     D  g02tgi                18     22  0
008600980914      *
008700980915      *----------------------------------
008800981120     D FIDN00DS      E DS
008900981123     d   Wnumca                6     19
009000081229     D FIDNBEDS      E DS
009100980921     D FIDN05DS      E DS
009200981124     D DS_FNDFA      E DS                  extname(fndfa00f)
009300980507     D KPJBA         E DS
009400981120     D DTAD          E DS
009500981123     D DCDD          E DS
009600981124     D DTLD          E DS
009700981230     D DSTB          E DS
009800010409     D DS15          E DS
009900981230     D DDCT01        E DS
010000020129     d ddcf01        e ds
010100990310
010200981123      *
010300980915     D UT§DSE0F      E DS
010400981123      *
010500981123     D CNCR80        E DS
010600980915     D TIBS02DS      E DS
010700041118      *
010800041118     D TRUL44DS      E DS
010900050902     D TRTCM1DS      E DS
011000980729      *----------------------------------
011100981123     D Ktpd            S                   LIKE(DCStpd)
011200981123     D Knkt            S                   LIKE(DCSnkt)
011300981123     D Kstd            S                   LIKE(DCSstd)
011400981123     D Kdim            S                   LIKE(DCSdim)
011500981123     D Khim            S                   LIKE(DCShim)
011600981123     D Knks            S                   LIKE(DCSnks)
011700981123     D Ktrc            S                   LIKE(DCStrc)
011800981230     D §cod            S                   LIKE(TBLcod)
011900981230     D §key            S                   LIKE(TBLkey)
012000081229     D Kcli            S                   LIKE(DCTksc)
012100981123     D Kkcc            S                   LIKE(DCTkcc)
012200981124     D w§tldstp        S                   LIKE(§tldstp1)
012300990309     d w§dctport       S                   like(§dctport)
012400981124     d Wfaso1          s                   like(i00fca)
012500981124     d                                     inz(605)
012600981124     d Wfaso2          s                   like(i00fca)
012700981124     d                                     inz(615)
012800990125     d Wfaso0          s                   like(i00fca)
012900990125     d                                     inz(575)
013000010406     d Wtlso1          s                   like(dfale1)
013100010406     d                                     inz('A12')
013200081229     D wnaz            s                   LIKE(OBEnaz)
013300010420     d prgnot          S                   LIKE(dcspno)                         *progressivo note
013400020328     D wclsflo7        S              1
013500981123     D wdataoggi       S             10
013600981123     D wanno           s              4
013700990505     D wannop          s              4
013800981123     D wfil            s              3
013900981123     D wnum            s              7
014000990505     D wnump           s              7
014100990125     D wpzman          S             11
014200990125     D wcoman          S             11
014300990125     D wkgman          S             11
014400981123     D w007a           s              7
014500990125     D w007b           s              7
014600990125     D w005a           s              5
014700010420     D waac            s              2
014800990308     D gruppoa         s              1
014900990308     D gruppob         s              1
015000990308     D gruppoc         s              1
015100990310     D estero          s              1
015200041122     D INVIO           s              1
015300010409     D prima           s              1
015400010411     D stampa          s              1
015500010411     D si99            s              1
015600050520     D mailno          s              1
015700010411     D rsut            S             20
015800990308     D y               s              3  0
015900990308     D z               s              3  0
016000990308     D i               s              3  0
016100990308     D e               s              3  0
016200010411     D f               s              3  0
016300990310     D x               s              3  0
016400010420     D NN              s              1  0
016500981123     D yy              s              3  0
016600981123     D xx              s              3  0
016700981123     D dateu           s              8  0
016800981123     D wdtgio          S              8  0
016900981123     D wora            S              6  0
017000981123     D w0140           S             14  0
017100981123     D w0040           S              4  0
017200981123     D w0030           S              3  0
017300981230     D Wdctlnp         s              3  0
017400010411     D docfil          s              3  0                                      contatore doc P.O.
017500010411     D doccli          s              3  0                                      contatore doc client
017600041125     D svcom           s             80
017700010411     D TES1            S            100
017800010411     D TES2            S             98
017900010411     D TESTA1          S            198
018000010411     D TESTA2          S            198
018100010411     D TRAT            S            198    INZ(*all'-')
018200981123      *
018300981123     D DATA_eur        S               D   DATFMT(*eur)
018400981123     D DATA_oggi       S               D   DATFMT(*eur)
018500080114     d                 ds
018600981126     d  aa04                   1      4
018700981126     d  aa02                   3      4
018800981124     d  mm02                   5      6
018900981124     d  gg02                   7      8
019000981124     d  data08                 1      8
019100990225      *
019200990225     d DS_DESCR        DS
019300990610     d WNOTET                       525
019400990610     d DES                           35    DIM(15)  overlay(Wnotet:1)           Note testo
019500990225     d W100A                        100             overlay(WNOTET:1)           1° 100 caratteri
019600990225     d W100B                        100             overlay(WNOTET:106)         2° 100 caratteri
019700990610     d W100C                        100             overlay(WNOTET:211)         2° 100 caratteri
019800990610     d W100D                        100             overlay(WNOTET:316)         2° 100 caratteri
019900990610     d W100E                        100             overlay(WNOTET:421)         2° 100 caratteri
020000100709
020100100709       // -?Strutture Dati per la definizione dei campi in stampa?
020200100709       //  ?per la "bonifica" degli stessi da caratteri indesiderati?
020300100709       //  ?da POSTEL?
020400100709     d ds_DN28PI1      ds
020500100709     d   PICrag
020600100709     d   PICind
020700100709     d   PICcap
020800100709     d   PICloc
020900100709     d   PICpro
021000100709     d   PICnaz
021100100709     d   PICpec
021200100709     d   PICdal
021300100709     d ds_DN28PO1      ds
021400100709     d   POCogg
021500100709     d   POCog1
021600100709     d ds_DN28PO2      ds
021700100709     d   POCrsm
021800100709     d   POCrsd
021900100709     d   POClod
022000100709     d   POCprn
022100100709     d   POCkev
022200100709     d   POCtae
022300100709     d   POCtad
022400100709     d   POCdan
022500100709     d   POCqtd
022600100709     d   POCdpz
022700100709     d ds_DN28PT1      ds
022800100709     d   PTCde1
022900100709     d ds_DN28PF1      ds
023000100709     d   PFDsoc
023100981123      *
023200010420      *
023300010420     D PE_TESTA1       C                   'Richiesta documenti per avanzamento-
023400010420     D                                      C.A. '
023500010420     D PE_RIGA1_1      C                   'Con riferimento alla pratica assicu-
023600010420     D                                     rativa in oggetto Vi preghiamo'
023700010420     D PE_RIGA1_2      C                   'di volerci inviare al più presto il-
023800010420     D                                      documento sotto elencato'
023900010420      *
024000010420     D PE_SALUTI       C                   'Distinti Saluti.     Ufficio Danni'
024100010420     D
024200010420     D KK_utente       S             10    INZ('RAC*      ')
024300100709
024400100709       //--------------------------------------------------------------
024500100709       //?Definizione procedure usate.                                 ?
024600100709       //--------------------------------------------------------------
024700100709
024800100709       // -?Parametri per XCHKCHAR?
024900100709      /copy gaitrasrc/srcProtoPI,XCHKCHAR
025000100709       // -?"Bonifica" stringa da caratteri indesiderati?
025100100709      /copy gaitrasrc/srcProtoPR,XCHKCHAR
025200100709
025300980731      *---------------------------------------------------------------------
025400980910
025500010409
025600010409      *
025700010409      * routine iniziale
025800010409     c                   EXSR      INIZIO
025900981124      *
026000981123      * aggancio la CA
026100981123     c     kdct          chain     fndct01l                           30
026200981230     c*
026300981230     c                   movel     dctflo        ddct01
026400981230     c                   movel     §dctlnpc      Wdctlnp
026500981230     c*
026600081229      * aggancio dati bolle
026700081229     c     kbol01        chain     tntmd000                           30
026800081229     c     kbol01        chain     TITAS30c                           30
026900081229      * se recupero rileggo
027000081229     C                   movel     'TB'          §cod
027100081229     C                   movel(P)  tastbl        §key
027200081229     C     ktab          chain     tabel00f                           31
027300081229     C                   movel     tbluni        dstb
027400081229     c                   if        §TBrbl = 'R'
027500081229     c     kbol01        reade     TITAS30c                               30
027600081229     c                   endif
027700990520     c
027800990309     c                   EXSR      SUB_DOCUM                                     recupero documenti
027900010411      *
028000010411     c                   clear                   stampa
028100010410      * se 2° sollecito verifico:
028200010410      *                         -se devo sollecitare solo documenti della filiale non stampo
028300010410      *                          il sollecito al cliente ma stampo una riga di tabulato
028400010410      *                          solleciti alla filiale
028500981123      *
028600010410     c                   if        i00fca = wfaso2
028700010410     c                   EXSR      SUB_VERIFICA
028800010410     c                   endif
028900010411      *
029000010411      * se il flag stampa è diverso da N stampo
029100010411      *
029200010411     c                   if        stampa <> 'N'
029300010411      *
029400010410      *
029500990520     c  n90              EXSR      SUB_INTES                                     intestazione
029600990520     c   90              EXSR      SUB_PO                                        intest./PO
029700981123     c                   EXSR      SUB_OGGE                                      oggetto
029800981123     c                   EXSR      SUB_TESTO                                     testo
029900981123     c                   EXSR      SUB_FIRMA                                     firma
030000981123      *
030100981123     c                   EXSR      SUB_AGG                                       aggiorno files
030200990520      *
030300990520      *
030400981124      * Chiudo file stampa testo sollecito
030500010409      * solo se non è stampa postel
030600010409     c  N05              CLOSE     fidn28p
030700041122     c   20              CLOSE     fidn28pm
030800010411      *
030900010411     c                   endif
031000980910
031100981029     c                   clear                   tibs02ds
031200981029     c                   movel     'C'           t02tla
031300981029     c                   call      'TIBS02R'
031400981029     c                   parm                    kpjba
031500981029     c                   parm                    tibs02ds
031600010409     C  N05              EVAL      *INLR = *ON
031700010409     C   05              EVAL      *INRT = *ON
031800980915      *****************************************************************
031900981120      *   PREAPARO I DATI DELLA TESTATA DELLA LETTERA
032000980915      *****************************************************************
032100981123     C     SUB_INTES     BEGSR
032200050520
032300050520     c                   eval      mailno = ' '
032400081229      * cerco il beneficiario
032500081229     c                   clear                   fidnbeds
032600081229      *
032700081229     c                   movel     'B'           IBEmod
032800081229     c                   z-add     dctaac        IBEaac
032900081229     c                   z-add     dctfil        IBEfil
033000081229     c                   z-add     dctnca        IBEnca
033100081229     c                   z-add     dctaas        IBEaas
033200081229     c                   z-add     dctlnp        IBElnp
033300081229     c                   z-add     dctnrs        IBEnrs
033400081229     c                   z-add     dctnsp        IBEnsp
033500081229     c                   z-add     dctksc        IBEksc
033600081229     c                   movel     dctptr        IBEptr
033700081229     c                   movel     dctflo        IBEflo
033800081229     c                   movel     'S'           IBEtpb
033900081229     c                   If        %subst(knsif:7:1) = 'P'
034000081229     c                   eval      IBEsif = 'P'
034100081229     c                   EndIf
034200081229      *
034300081229     c                   call      'FIDNBER'
034400081229     c                   parm                    fidnbeds
034500081229      *
034600081229     c                   If        oberag <> *blanks
034700081229      *
034800081229     c                   movel     oberag        PICrag
034900081229     c                   movel     obecap        PICcap
035000081229     c                   movel     obevia        PICind
035100081229     c                   movel     obeloc        PICloc
035200081229     c                   if        obenaz  = *blanks
035300081229     c                   movel     obeprv        PICpro
035400081229     c                   else
035500081229     c                   movel     obenaz        wnaz
035600081229     c                   exsr      ctrnaz
035700081229     c                   movel     §15des        PICnaz
035800081229     c                   endif
035900081229     c                   movel     obeksc        Kcli
036000081229     c                   endif
036100081229     c*
036200081229     c* controllo se cliente estero e devo stampare in inglese
036300081229     c*
036400081229      * Imposto se devo stampare in Inglese
036500081229     c                   clear                   ESTERO
036600081229     c                   clear                   wclsflo7
036700081229     c                   IF        obeing <> *blanks
036800081229     c                   eval      wclsflo7 = obeing
036900081229     c                   ENDIF
037000081229     c                   eval      estero = wclsflo7
037100081229     c*
037200980917      *
037300981123      * Intestatario
037400990310      *-------------
037500050520      * verifico se inviarlo via e-mail oppure no anche in base al flag mailno
037600041119     c                   move      kcli          w0040
037700041119     c                   If        kcli <> *zeros and
037800041119     c                             w0040 <> 8888 and w0040 <> 9999
037900081229     c                             and OBEinv = 'M'
038000041119     c                   clear                   trul44ds
038100041119     c                   eval      D44pgm  = 'FIDN28R'
038200041119     c                   eval      D44Ksc  = Kcli
038300041119     c                   eval      D44cl   = '006'
038400041119     c                   eval      d44apl = 'C'
038500041119     c                   eval      d44nk1 = '0151' + %editc(Kcli:'X')
038600041119     c                   eval      d44tnt = '87'
038700041119     c                   eval      D44ovr  = ' '
038800041119     c                   eval      D44scope= '*CALLLVL'
038900041119     c                   eval      D44prtf = 'FIDN28P'
039000041119
039100041119     c                   IF        Wclsflo7 <> *blanks
039200170120     ***C                   eval      D44obj =  '*OBJM*Documents needed for Anoma-
039300170120     ***C                             ly information N. ' +  %char(dctfil)+  '/' +
039400170120     ***C                             %trim(%editc(dctnca:'Z')) +
039500170120     ***c                             ' Year '   +   %char(dctaac)
039600170120     c                   eval      D44obj =  '*OBJM*Documents needed for Anoma-
039700170120     c                             ly N. ' +  %char(dctfil)+  '/' +
039800170120     c                             %trim(%editc(dctnca:'Z')) +
039900170120     c                             ' Year ' + %char(dctaac)  +
040000170123     c                             ' (R: ' + %trimR(KNMUS) + ')'
040100041119     c                   ELSE
040200170120     ***C                   eval      D44obj = '*OBJM*Sollecito invio Documenti -
040300170120     ***C                             COMUNICAZIONE ANOMALIA N. ' +
040400170120     ***C                             %char(dctfil)+  '/'      +
040500170120     ***C                             %trim(%editc(dctnca:'Z')) +
040600170120     ***c                             ' Anno '  +  %char(dctaac)
040700170120     c                   eval      D44obj = '*OBJM*Sollecito invio Documenti -
040800170120     c                             C.A. N. ' +
040900170120     c                             %char(dctfil)+  '/'      +
041000170120     c                             %trim(%editc(dctnca:'Z')) +
041100170120     c                             ' Anno ' + %char(dctaac)  +
041200170123     c                             ' (R: ' + %trimR(KNMUS) + ')'
041300041119     c                   ENDIF
041400041119     c                   call      'TRUL44R'
041500041119     c                   parm                    trul44ds
041600050902     c                   parm                    trtcm1ds
041700041119      *
041800041119      * se c'è stato qualche problema faccio invio in altro modo
041900041119      *
042000041119     c                   if        d44err  <> ' '
042100041122     c   05              eval      invio  = 'P'
042200041122     c  n05              eval      invio  = 'L'
042300041119     c                   setoff                                       20
042400041119     c                   ELSE
042500041119     c                   seton                                        20
042600041119      * ovrprtf
042700041125     c                   clear                   svcom
042800041125     c                   movea     cmd(6)        svcom
042900041125     c                   eval      %subst(svcom:40:10) = d44outq
043000041125     c                   eval      commanl = %trim(SVCOM)+ d44dta  + ''')'
043100041119     c                   eval      lungo = %len(%trim(commanl))
043200041119     C                   CALL      'QCMDEXC'                            60
043300041119     C                   PARM                    COMMANl         500
043400041119     C                   PARM                    LUNGO            15 5
043500041119      * se c'è errore
043600041119     c                   if        *in60
043700041122     c   05              eval      invio = 'P'
043800041122     c  n05              eval      invio  = 'L'
043900041119     c                   setoff                                       20
044000041119     c                   else
044100041119     c                   eval      invio = 'M'
044200041119     c                   endif
044300041119      *
044400041119
044500041119     c                   endif
044600041119
044700041119     c                   endif
044800041119      *
044900041119     c
045000981123     c* recupero all'attenzione del sig........
045100981123      *
045200981123      *
045300981123     C                   movel(P)  'C'           Ktpd
045400981123     C                   movel(P)  Wnumca        Knkt
045500981123     C                   movel(P)  'N'           Kstd
045600981123     C                   clear                   Khim
045700981123     C                   clear                   Kdim
045800981123     C                   clear                   Knks
045900981123     C                   movel     'D'           Ktrc
046000981123      *
046100010409     C                   CLEAR                   PICPEC
046200010515     c                   eval      *IN06 = *Off
046300010515     c
046400010409      *
046500981123     c     KDCS          CHAIN     FNDCS000                           06
046600981123      *
046700981123     c                   IF        *IN06 = *OFF and  DCSnot <> *BLANKS
046800981123     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
046900981123     c                   eval      *IN06 = *ON
047000981123     c                   ENDIF
047100981123      *
047200981123      * Località + data
047300981123     C     *eur          movel     DATA_oggi     wdataoggi
047400981123     c                   eval      PICdal = 'Bologna, ' + wdataoggi
047500081229
047600081229      * filiale del cliente
047700081229     c                   movel     kcli          w0030
047800081229      *
047900981124      * identifico la società di appartenenza del cliente
048000981124     c     w0030         chain     azorg01l                           31
048100981124     c                   movel     kbar          wsocie
048200981124     c                   movel     §TLDstp1      W§TLDstp
048300100709      *
048400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
048500100709     c                   if        *in05  and  Not *in20
048600100709     c                   eval      TxtInOut   = ds_DN28PI1
048700100709     c                   exsr      sr_xChkChar
048800100709     c                   if        esito = '1'
048900100709     c                   eval      ds_DN28PI1 = TxtInOut
049000100709     c                   endif
049100100709     c                   endif
049200100709      *
049300981124      * Apro file stampa e imposto Tipo Modulo e Stampante
049400981124      *
049500981124     c                   EXSR      IMPPRT
049600981124      *
049700981123      * Scrivo l'intestazione
049800041119     c  n20              WRITE     DN28PP
049900041119     c  n20              WRITE     DN28PI1
050000041119     c   20              WRITE     DN28MP
050100041119     c   20              WRITE     DN28MI1
050200981123      *
050300981123     C                   ENDSR
050400990520      *****************************************************************
050500990520      *   PREAPARO I DATI DELLA TESTATA DELLA LETTERA  AL P.O. APERTURA C.A.
050600990520      *****************************************************************
050700990520     C     SUB_PO        BEGSR
050800990520      *
050900990520      *
051000990520      * Località + data
051100990520     C     *eur          movel     DATA_oggi     wdataoggi
051200990520     c                   eval      PICdal = 'Bologna, ' + wdataoggi
051300990520      * Intestatario
051400990520      *-------------
051500990520      *
051600990520     c     DCTFIL        chain     azorg01l                           31
051700990520     c*
051800990520     c                   movel     orgdes        PICrag
051900990520     c                   movel     orgind        PICind
052000990520     c                   movel     orgcpf        PICcap
052100990520     c                   movel     orgloc        PICloc
052200990520     c                   movel     orgpro        PICpro
052300990521     c                   movel     kbar          wsocie
052400990521     c                   movel     §TLDstp1      W§TLDstp
052500100709      *
052600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
052700100709     c                   if        *in05  and  Not *in20
052800100709     c                   eval      TxtInOut   = ds_DN28PI1
052900100709     c                   exsr      sr_xChkChar
053000100709     c                   if        esito = '1'
053100100709     c                   eval      ds_DN28PI1 = TxtInOut
053200100709     c                   endif
053300100709     c                   endif
053400100709      *
053500990520      * Apro file stampa e imposto Tipo Modulo e Stampante
053600990520      *
053700990520     c                   EXSR      IMPPRT
053800990520      *
053900990520      * Scrivo l'intestazione
054000041119     c  n20              WRITE     DN28PI1
054100041119     c   20              WRITE     DN28MI1
054200990520      *
054300990520     C                   ENDSR
054400981123      *****************************************************************
054500981123      *   OGGETTO
054600981123      *****************************************************************
054700981123     C     SUB_ogge      BEGSR
054800981123
054900010515      *
055000010515      *  pulizia Oggetto
055100010515     c                   CLEAR                   DN28PO1
055200010515      *
055300010515      *  pulizia Dati Anomalia
055400010515     c                   CLEAR                   DN28PO2
055500010515      * Pulizia riferimenti
055600010515     c                   CLEAR                   DN28PO3
055700010608      *
055800010608      * se i00tpb (tipo bolla ) c'è il valore "F" significa che è un sollecito alla filiale
055900010608     c                   if        i00tpb = 'F'
056000010608     c                   eval      *in90 = *on
056100010608     c                   endif
056200020402
056300020402      * se non è un sollecito alla filiale controllo se il sollecito è da scrivere in italiano
056400020402      * o in inglese
056500020402     c                   eval      *in08 = *off
056600020402     c                   if        not *in90 and wclsflo7 <> *blanks
056700020402     c                   eval      *in08 = *on
056800020402     c                   endif
056900010608      *
057000010515      ***
057100981123     c                   movel     I00aac        wanno
057200981123     c                   movel     I00fil        wfil
057300981123     c                   movel     I00nca        w007a
057400981123     c     '0'           check     w007a         yy
057500981123     c                   eval      wnum = %subst(W007a:yy)
057600990505      *
057700990505     c                   movel     DCTpra        wannop
057800990505     c                   movel     DCTprn        w007a
057900990505     c     '0'           check     w007a         yy
058000990505     c                   eval      wnump = %subst(W007a:yy)
058100990505      *
058200990520     c                   if        *in08 = *off
058300990520      *
058400120417     C                   eval      POCogg  =   ' N. '
058500990505     C                                       +  DCTdit  +  ' '  +  %trim(wnump)
058600990505     C                                       +  ' Anno '  +  wannop
058700990505     C                                       +  '   COMUNICAZIONE ANOMALIA N. '
058800990505     c                                       +  wfil  +  '/'   +  %trim(wnum)
058900990505     C                                       +  ' Anno '  +  wanno
059000990520      *
059100990520     c                   else
059200990520      * INGLESE
059300120418     C                   eval      POCog1  =   ' N.  '
059400990520     C                                       +  DCTdit  +  ' '  +  %trim(wnump)
059500990520     C                                       +  ' Year '  +  wannop
059600990520     C                                       +  '   DAMAGE COMUNICATION N. '
059700990520     c                                       +  wfil  +  '/'   +  %trim(wnum)
059800990520     C                                       +  ' Year '  +  wanno
059900990520     c                   endif
060000990505      *
060100981123     c                   z-add     i00lnp        POClnp
060200981123     c                   z-add     i00nrs        POCnrs
060300981123     c                   z-add     i00nsp        POCnsp
060400981123     c                   z-add     i00aas        POCdat
060500981123     c                   movel     tasmgs        POCdat
060600981123     c     *mdy          move      POCdat        DATA_eur
060700981123     c     *dmy          move      DATA_eur      POCdat
060800981123     c                   movel     tmdrsm        POCrsm
060900981123     c                   z-add     tasrmn        POCrmn
061000981123     c                   movel     tmdrsd        POCrsd
061100981123     c                   movel     tmdlod        POClod
061200990118     c                   if        TMDnzd <> *blanks
061300990118     c                   movel     TMDnzd        POCprn
061400990118     c                   else
061500990118     c                   movel     TMDprd        POCprn
061600990118     c                   endif
061700981123     c                   z-add     tasncl        POCncl
061800990809     c                   z-add     taspkf        POCpkf
061900990520      *
062000990520      * Evento
062100010515     c                   eval      *IN03 = *OFF
062200990520     c                   IF        DCTnev > *zeros
062300990520     c                   eval      *IN03 = *ON
062400990520     c                   movel     DCTaae        wanno
062500990520     c                   movel     DCTnev        w007a
062600990520     c     '0'           check     w007a         yy
062700990520     c                   eval      wnum = %subst(W007a:yy)
062800990520     C                   eval      POCkev  =  wannop  +  '/'  +  %trim(wnum)
062900990520     c     Kdet          chain     FNDET000                           31
063000990520     c                   clear                   tibs02ds
063100990520     c                   clear                   dtad
063200990520      *
063300990520     C                   movel     'C'           t02mod
063400990520     C                   movel     knsif         t02sif
063500990520     C                   movel     'TAD'         t02cod
063600990520     C                   movel(P)  dettad        t02ke1
063700990520      *
063800990520     C                   CALL      'TIBS02R'
063900990520     C                   PARM                    KPJBA
064000990520     C                   PARM                    TIBS02DS
064100990520      *
064200990520     C                   movel     T02uni        DTAD
064300990520     c*
064400990520     c                   SELECT
064500990521     c                   WHEN      *in08
064600990520     c                                                  and  §TADdesi <> *blanks
064700990520     C                   movel     §TADdesi      POCtae
064800990520     c                   WHEN        §TADdest <> *blanks
064900990520     C                   movel     §TADdest      POCtae
065000990520     c                   OTHER
065100990520     C                   movel     §TADdesc      POCtae
065200990520     c                   ENDSL
065300990520     c                   ENDIF
065400981123      * Decodifico tipo anomalia
065500981123     c                   clear                   tibs02ds
065600981123     c                   clear                   dtad
065700981123      *
065800981123     C                   movel     'C'           t02mod
065900981123     C                   movel     knsif         t02sif
066000981123     C                   movel     'TAD'         t02cod
066100981123     C                   movel(P)  I00tad        t02ke1
066200981123      *
066300981123     C                   CALL      'TIBS02R'
066400981123     C                   PARM                    KPJBA
066500981123     C                   PARM                    TIBS02DS
066600981123      *
066700981123     C                   movel     T02uni        DTAD
066800990520     c                   SELECT
066900990521     c                   WHEN      *in08
067000990520     c                                                  and  §TADdesi <> *blanks
067100990520     C                   movel     §TADdesi      POCtad
067200990520     c                   WHEN        §TADdest <> *blanks
067300990520     C                   movel     §TADdest      POCtad
067400990520     c                   OTHER
067500990520     C                   movel     §TADdesc      POCtad
067600990520     c                   ENDSL
067700981123      * Descrizione anomalia
067800981123     c                   exsr      DESTAD
067900990125      * Quantità danneggiate
068000990125      *      Peso
068100990521     c                   IF        DCTpkd > *zeros
068200990125     c                   move(P)   DCTpkd        w007a
068300990125     c     '0'           check     w007a         xx
068400990125     c                   eval      W007b = %subst(w007a:xx)
068500990125     c     ' '           checkr    w007b         xx
068600990125     c                   if        xx = 1
068700990125     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
068800990125     c                   else
068900990125     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(xx-1))
069000990125     c                                      +  ','  +  %subst(w007b:XX:1)
069100990125     c                   endif
069200990125     c                   ENDIF
069300990125      *      Colli
069400990521     c                   IF        DCTncn > *zeros
069500990125     c                   move      DCTncn        w005a
069600990125     c     '0'           check     w005a         xx
069700990521     c                   IF        *in08
069800990521     c                   eval      Wcoman = ' Parcels '  +  %subst(W005a:XX)
069900990521     c                   ELSE
070000990125     c                   eval      Wcoman = ' Colli '  +  %subst(W005a:XX)
070100990521     c                   ENDIF
070200990125     c                   ENDIF
070300990125      *      Pezzi
070400990125     c                   IF        DCTnpz > *zeros
070500990125     c                   move      DCTnpz        w005a
070600990125     c     '0'           check     w005a         xx
070700990521     c                   IF        *in08
070800990521     c                   eval      Wpzman = ' Pieces '  +  %subst(W005a:XX)
070900990521     c                   ELSE
071000990125     c                   eval      Wpzman = ' Pezzi '  +  %subst(W005a:XX)
071100990521     c                   ENDIF
071200990125     c                   ENDIF
071300990125      *
071400990125     c                   eval      POCqtd  =  %trimr(Wkgman)  +
071500990125     c                                        %trimr(Wcoman)  +  %trimr(Wpzman)
071600990125      *
071700990125      * Descrizione pezzi danneggiati
071800990125     c                   IF        DCTnpz > *zeros
071900990125     c                   EXSR      DESPZD
072000990524     c                   ENDIF
072100100709      *
072200100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
072300100709     c                   if        *in05  and  Not *in20
072400100709     c                   eval      TxtInOut   = ds_DN28PO1
072500100709     c                   exsr      sr_xChkChar
072600100709     c                   if        esito = '1'
072700100709     c                   eval      ds_DN28PO1 = TxtInOut
072800100709     c                   endif
072900100709     c                   eval      TxtInOut   = ds_DN28PO2
073000100709     c                   exsr      sr_xChkChar
073100100709     c                   if        esito = '1'
073200100709     c                   eval      ds_DN28PO2 = TxtInOut
073300100709     c                   endif
073400100709     c                   endif
073500010409      *
073600010409      * controllo il salto pagina
073700010409     c                   EXSR      PRT_TESTAT
073800981123      *
073900981123      *  Oggetto
074000041119     c  n20              WRITE     DN28PO1
074100041119     c   20              WRITE     DN28MO1
074200010409      *
074300010409      * controllo il salto pagina
074400010409     c                   EXSR      PRT_TESTAT
074500981123      *  Dati Anomalia
074600041119     c  n20              WRITE     DN28PO2
074700041119     c   20              WRITE     DN28MO2
074800990524     c** Se esiste già una richiesta di risarcimento o nota di addebito visualizzo i dati
074900990524     c     kdct          chain     fndcl01l                           32
075000990524     c**
075100990524     c                   if        not *in32 and dclndc <> 0
075200990524     c                   seton                                        10
075300990524     c                   z-add     dclndc        pocndc
075400990803     c                   IF        dclddc > *zeros
075500990803     c     *iso          move      DCLddc        DATA_eur
075600990524     c     *eur          move      DATA_eur      POCddc
075700100709      *
075800100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
075900100709      * => NO: il rec. DN28PO3 contiene solo campi NUMERICI!
076000100709     c*//                if        *in05  and  Not *in20
076100100709     c*//                eval      TxtInOut   = ds_DN28PO3
076200100709     c*//                exsr      sr_xChkChar
076300100709     c*//                if        esito = '1'
076400100709     c*//                eval      ds_DN28PO3 = TxtInOut
076500100709     c*//                endif
076600100709     c*//                endif
076700100709      *
076800010409      * controllo il salto pagina
076900010409     c                   EXSR      PRT_TESTAT
077000010409      *
077100041119     c  n20              WRITE     DN28PO3
077200041119     c   20              WRITE     DN28MO3
077300990803     c                   ENDIF
077400990803     c                   endif
077500981123
077600981123     C                   ENDSR
077700981123      *****************************************************************
077800981123      *  REPERIMENTO DESCRIZIONE ANOMALIA
077900981123      *****************************************************************
078000981123     C     DESTAD        BEGSR
078100981123      *
078200981123     C                   movel(P)  'C'           Ktpd
078300981123     C                   movel(P)  Wnumca        Knkt
078400981123     C                   movel(P)  'A'           Kstd
078500981123     C                   clear                   Knks
078600981123     C                   movel     'D'           Ktrc
078700981123      *
078800981123     c                   clear                   yy
078900981123     c                   clear                   W35
079000981123      *
079100981123     c     kdcs          chain     fndcs000                           30
079200981123      *
079300981123     c     *in30         DOWeq     *off
079400981123     c     yy            ANDle     3
079500981123     c                   add       1             yy
079600981123     c                   movel     dcsnot        W35(yy)
079700981123     c     kdcs          reade     fndcs000                               30
079800981123     c                   ENDDO
079900981123      *
080000990521     c                   eval      POCdan = W35(1)
080100981123      *
080200981123     C                   ENDSR
080300990125      *****************************************************************
080400990125      *  REPERIMENTO DESCRIZIONE PEZZI DANNEGGIATI
080500990125      *****************************************************************
080600990125     C     DESPZD        BEGSR
080700990125      *
080800990125      * Descrizione pezzi mancanti
080900990125     C                   movel(P)  'C'           Ktpd
081000990125     C                   movel(P)  Wnumca        Knkt
081100990125     C                   movel(P)  'P'           Kstd
081200990125     C                   clear                   Knks
081300990125     C                   movel     'D'           Ktrc
081400990125      *
081500990125     c                   clear                   yy
081600990125     c                   clear                   W35
081700990125      *
081800990125     c     kdcs          chain     fndcs000                           30
081900990125      *
082000990125     c     *in30         DOWeq     *off
082100990125     c     yy            ANDle     3
082200990125     c                   add       1             yy
082300990125     c                   movel     dcsnot        W35(yy)
082400990125     c     kdcs          reade     fndcs000                               30
082500990125     c                   ENDDO
082600990125      *
082700990125     c                   eval      POCdpz =  W35(1)
082800990125      *
082900990125     c                   ENDSR
083000981123      *****************************************************************
083100981123      *   TESTO
083200981123      *****************************************************************
083300981123     C     SUB_testo     BEGSR
083400981123
083500010409      * controllo il salto pagina
083600010409     c                   EXSR      PRT_TESTAT
083700981123      *  Lascio 2 righe dall'oggetto
083800041119     C  n20              WRITE     DN28P00
083900041119     C  n20              WRITE     DN28P00
084000041119     C   20              WRITE     DN28m00
084100041119     C   20              WRITE     DN28m00
084200990521
084300990521      *
084400990125      *  sollecito non ho mai ricevuto nessun documento
084500990521    1c                   if        i00fca = wfaso0
084600990521     c  n08              movel(P)  so0(1)        PTCde1
084700990521     c   08              movel(P)  so0(6)        PTCde1
084800100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
084900100709     c                   if        *in05  and  Not *in20
085000100709     c                   eval      TxtInOut   = ds_DN28PT1
085100100709     c                   exsr      sr_xChkChar
085200100709     c                   if        esito = '1'
085300100709     c                   eval      ds_DN28PT1 = TxtInOut
085400100709     c                   endif
085500100709     c                   endif
085600010409      * controllo il salto pagina
085700010409     c                   EXSR      PRT_TESTAT
085800010409      *
085900041119     C  n20              WRITE     DN28P00                                      vuota
086000041119     C  n20              WRITE     DN28PT1
086100041119     C   20              WRITE     DN28m00                                      vuota
086200041119     C   20              WRITE     DN28mT1
086300990125      *
086400990521     c  n08              movel(P)  so0(2)        PTCde1
086500990521     c   08              movel(P)  so0(7)        PTCde1
086600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
086700100709     c                   if        *in05  and  Not *in20
086800100709     c                   eval      TxtInOut   = ds_DN28PT1
086900100709     c                   exsr      sr_xChkChar
087000100709     c                   if        esito = '1'
087100100709     c                   eval      ds_DN28PT1 = TxtInOut
087200100709     c                   endif
087300100709     c                   endif
087400010409      * controllo il salto pagina
087500010409     c                   EXSR      PRT_TESTAT
087600010409      *
087700041119     C  n20              WRITE     DN28PT1
087800041119     C   20              WRITE     DN28mT1
087900990125      *
088000990521     c  n08              movel(P)  so0(3)        PTCde1
088100990521     c   08              movel(P)  so0(8)        PTCde1
088200100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
088300100709     c                   if        *in05  and  Not *in20
088400100709     c                   eval      TxtInOut   = ds_DN28PT1
088500100709     c                   exsr      sr_xChkChar
088600100709     c                   if        esito = '1'
088700100709     c                   eval      ds_DN28PT1 = TxtInOut
088800100709     c                   endif
088900100709     c                   endif
089000010409      * controllo il salto pagina
089100010409     c                   EXSR      PRT_TESTAT
089200010409      *
089300041119     C  n20              WRITE     DN28PT1
089400041119     C   20              WRITE     DN28mT1
089500120423      *
089600120423     c                   clear                   ptcde1
089700120423     c  n08              movel(P)  so0(4)        PTCde1
089800120423      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
089900120423     c                   if        *in05  and  Not *in20
090000120423     c                   eval      TxtInOut   = ds_DN28PT1
090100120423     c                   exsr      sr_xChkChar
090200120423     c                   if        esito = '1'
090300120423     c                   eval      ds_DN28PT1 = TxtInOut
090400120423     c                   endif
090500120423     c                   endif
090600120423     C  n20              WRITE     DN28PT1
090700120423     C   20              WRITE     DN28mT1
090800990125      *
090900990521   x1c                   else
091000981123      *
091100981123     c                   clear                   PTCde1
091200990521      *  sollecito alla filiale  I00TPB = 'F'
091300990521    2c                   if        i00tpb = 'F'
091400990521
091500990521     c                   movel(P)  so3(1)        PTCde1
091600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
091700100709     c                   if        *in05  and  Not *in20
091800100709     c                   eval      TxtInOut   = ds_DN28PT1
091900100709     c                   exsr      sr_xChkChar
092000100709     c                   if        esito = '1'
092100100709     c                   eval      ds_DN28PT1 = TxtInOut
092200100709     c                   endif
092300100709     c                   endif
092400010409      * controllo il salto pagina
092500010409     c                   EXSR      PRT_TESTAT
092600010409      *
092700041119     C  n20              WRITE     DN28P00                                      vuota
092800041119     C  n20              WRITE     DN28PT1
092900041119     C   20              WRITE     DN28M00                                      vuota
093000041119     C   20              WRITE     DN28MT1
093100990521      *
093200990521     c                   movel(P)  so3(2)        PTCde1
093300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
093400100709     c                   if        *in05  and  Not *in20
093500100709     c                   eval      TxtInOut   = ds_DN28PT1
093600100709     c                   exsr      sr_xChkChar
093700100709     c                   if        esito = '1'
093800100709     c                   eval      ds_DN28PT1 = TxtInOut
093900100709     c                   endif
094000100709     c                   endif
094100010409      * controllo il salto pagina
094200010409     c                   EXSR      PRT_TESTAT
094300010409      *
094400041119     C  n20              WRITE     DN28PT1
094500041119     C   20              WRITE     DN28MT1
094600990521      *
094700990521     c                   movel(P)  so3(3)        PTCde1
094800100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
094900100709     c                   if        *in05  and  Not *in20
095000100709     c                   eval      TxtInOut   = ds_DN28PT1
095100100709     c                   exsr      sr_xChkChar
095200100709     c                   if        esito = '1'
095300100709     c                   eval      ds_DN28PT1 = TxtInOut
095400100709     c                   endif
095500100709     c                   endif
095600010409      * controllo il salto pagina
095700010409     c                   EXSR      PRT_TESTAT
095800010409      *
095900041119     C  n20              WRITE     DN28PT1
096000041119     C   20              WRITE     DN28MT1
096100990521      *
096200990521   x2c                   else
096300990521      *
096400981123      *  1° sollecito
096500990521    3c                   if        i00fca = wfaso1
096600990521     c  n08              movel(P)  so1(1)        PTCde1
096700990521     c   08              movel(P)  so1(5)        PTCde1
096800100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
096900100709     c                   if        *in05  and  Not *in20
097000100709     c                   eval      TxtInOut   = ds_DN28PT1
097100100709     c                   exsr      sr_xChkChar
097200100709     c                   if        esito = '1'
097300100709     c                   eval      ds_DN28PT1 = TxtInOut
097400100709     c                   endif
097500100709     c                   endif
097600010409      * controllo il salto pagina
097700010409     c                   EXSR      PRT_TESTAT
097800010409      *
097900041119     C  n20              WRITE     DN28P00                                      vuota
098000041119     C  n20              WRITE     DN28PT1
098100041119     C   20              WRITE     DN28M00                                      vuota
098200041119     C   20              WRITE     DN28MT1
098300981123      *
098400990521     c  n08              movel(P)  so1(2)        PTCde1
098500990521     c   08              movel(P)  so1(6)        PTCde1
098600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
098700100709     c                   if        *in05  and  Not *in20
098800100709     c                   eval      TxtInOut   = ds_DN28PT1
098900100709     c                   exsr      sr_xChkChar
099000100709     c                   if        esito = '1'
099100100709     c                   eval      ds_DN28PT1 = TxtInOut
099200100709     c                   endif
099300100709     c                   endif
099400010409      * controllo il salto pagina
099500010409     c                   EXSR      PRT_TESTAT
099600010409      *
099700041122     C  n20              WRITE     DN28PT1
099800041119     C   20              WRITE     DN28MT1
099900981124      *
100000990521    3c                   endif
100100981124      *  2° sollecito
100200990521    3c                   if        i00fca = wfaso2
100300981124     c* data  guarda fidn09r
100400990521     c  n08              movel(P)  so2(1)        PTCde1
100500990521     c   08              movel(P)  so2(6)        PTCde1
100600981124     c* data ultimo sollecito
100700120418     c  n08              eval      %subst(PTCde1:84:8) = gg02 + '/' + mm02 + '/'
100800981126     c                             + aa02
100900120418     c   08              eval      %subst(PTCde1:79:8) = gg02 + '/' + mm02 + '/'
101000990521     c                             + aa02
101100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
101200100709     c                   if        *in05  and  Not *in20
101300100709     c                   eval      TxtInOut   = ds_DN28PT1
101400100709     c                   exsr      sr_xChkChar
101500100709     c                   if        esito = '1'
101600100709     c                   eval      ds_DN28PT1 = TxtInOut
101700100709     c                   endif
101800100709     c                   endif
101900010409      * controllo il salto pagina
102000010409     c                   EXSR      PRT_TESTAT
102100010409      *
102200041119     C  n20              WRITE     DN28P00                                      vuota
102300041119     C  n20              WRITE     DN28PT1
102400041119     C   20              WRITE     DN28M00                                      vuota
102500041119     C   20              WRITE     DN28MT1
102600981124      *
102700990521     c  n08              movel(P)  so2(2)        PTCde1
102800990521     c   08              movel(P)  so2(7)        PTCde1
102900100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
103000100709     c                   if        *in05  and  Not *in20
103100100709     c                   eval      TxtInOut   = ds_DN28PT1
103200100709     c                   exsr      sr_xChkChar
103300100709     c                   if        esito = '1'
103400100709     c                   eval      ds_DN28PT1 = TxtInOut
103500100709     c                   endif
103600100709     c                   endif
103700010409      * controllo il salto pagina
103800010409     c                   EXSR      PRT_TESTAT
103900010409      *
104000041119     C  N20              WRITE     DN28PT1
104100041119     C   20              WRITE     DN28MT1
104200981124      *
104300990521     c  n08              movel(P)  so2(3)        PTCde1
104400990521     c   08              movel(P)  so2(8)        PTCde1
104500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
104600100709     c                   if        *in05  and  Not *in20
104700100709     c                   eval      TxtInOut   = ds_DN28PT1
104800100709     c                   exsr      sr_xChkChar
104900100709     c                   if        esito = '1'
105000100709     c                   eval      ds_DN28PT1 = TxtInOut
105100100709     c                   endif
105200100709     c                   endif
105300010409      * controllo il salto pagina
105400010409     c                   EXSR      PRT_TESTAT
105500010409      *
105600041119     C  N20              WRITE     DN28PT1
105700041119     C   20              WRITE     DN28MT1
105800981124      *
105900990521    3c                   endif
106000990521    2c                   endif
106100981123      *
106200990308     c* stampo i documenti da sollecitare
106300990521     c* controllo se devo stampare il gruppo A     non stampa alle filiali
106400990521     c                   if        gruppoa = 'Y'   and not *in90
106500010409      * controllo il salto pagina
106600010409     c                   EXSR      PRT_TESTAT
106700010409      *
106800041119     C  N20              WRITE     DN28P00                                      vuota
106900041119     C   20              WRITE     DN28M00                                      vuota
107000990521     c  n08              z-add     1             x
107100990521     c   08              z-add     4             x
107200990521     c                   dow       (x <= 3 and not *in08) or
107300120418     c                             (x <= 5 and *in08)
107400990308     c                   eval      ptcde1 = doa(x)
107500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
107600100709     c                   if        *in05  and  Not *in20
107700100709     c                   eval      TxtInOut   = ds_DN28PT1
107800100709     c                   exsr      sr_xChkChar
107900100709     c                   if        esito = '1'
108000100709     c                   eval      ds_DN28PT1 = TxtInOut
108100100709     c                   endif
108200100709     c                   endif
108300010409      * controllo il salto pagina
108400010409     c                   EXSR      PRT_TESTAT
108500010409      *
108600041119     C  N20              WRITE     DN28PT1
108700041119     C   20              WRITE     DN28MT1
108800990308     c                   add       1             x
108900990308     c                   enddo
109000990308     c                   endif
109100990521     c* controllo se devo stampare il gruppo B     non stampa alle filiali
109200990521     c                   if        gruppob = 'Y'   and not *in90
109300010409      * controllo il salto pagina
109400010409     c                   EXSR      PRT_TESTAT
109500010409      *
109600041119     C  N20              WRITE     DN28P00                                      vuota
109700041119     C   20              WRITE     DN28M00                                      vuota
109800990521     c  n08              z-add     1             x
109900990521     c   08              z-add     4             x
110000990521     c                   dow       (x <= 3 and not *in08) or
110100120418     c                             (x <= 6 and *in08)
110200990308     c                   eval      ptcde1 = dob(x)
110300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
110400100709     c                   if        *in05  and  Not *in20
110500100709     c                   eval      TxtInOut   = ds_DN28PT1
110600100709     c                   exsr      sr_xChkChar
110700100709     c                   if        esito = '1'
110800100709     c                   eval      ds_DN28PT1 = TxtInOut
110900100709     c                   endif
111000100709     c                   endif
111100010409      * controllo il salto pagina
111200010409     c                   EXSR      PRT_TESTAT
111300010409      *
111400041119     C  N20              WRITE     DN28PT1
111500041119     C   20              WRITE     DN28MT1
111600990308     c                   add       1             x
111700990308     c                   enddo
111800990308     c                   endif
111900990521     c* controllo se devo stampare il gruppo C     non stampa alle filiali
112000990521     c                   if        gruppoc = 'Y'   and not *in90
112100010409      * controllo il salto pagina
112200010409     c                   EXSR      PRT_TESTAT
112300010409      *
112400041119     C  N20              WRITE     DN28P00                                      vuota
112500041119     C   20              WRITE     DN28M00                                      vuota
112600990521     c  n08              z-add     1             x
112700080114     c   08              z-add     3             x
112800080114     c                   dow       (x <= 2 and not *in08) or
112900120418     c                             (x <= 4 and *in08)
113000990308     c                   eval      ptcde1 = doc(x)
113100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
113200100709     c                   if        *in05  and  Not *in20
113300100709     c                   eval      TxtInOut   = ds_DN28PT1
113400100709     c                   exsr      sr_xChkChar
113500100709     c                   if        esito = '1'
113600100709     c                   eval      ds_DN28PT1 = TxtInOut
113700100709     c                   endif
113800100709     c                   endif
113900010409      * controllo il salto pagina
114000010409     c                   EXSR      PRT_TESTAT
114100010409      *
114200041119     C  N20              WRITE     DN28PT1
114300041119     C   20              WRITE     DN28MT1
114400990308     c                   add       1             x
114500990308     c                   enddo
114600990308     c                   endif
114700990308     c
114800990308     c                   z-add     1             z
114900990308     c*
115000990308     c                   dow       z <= e
115100990308     c                   if        cd(z) <> *blank
115200981123      * decodifico il documento
115300981123     c                   clear                   tibs02ds
115400981123     c                   clear                   dcdd
115500981123      *
115600981123     C                   movel     'C'           t02mod
115700981123     C                   movel     knsif         t02sif
115800981123     C                   movel     'CDD'         t02cod
115900990308     C                   movel(P)  cd(z)         t02ke1
116000981123      *
116100990521     C                   CALL      'TIBS02R'
116200981123     C                   PARM                    KPJBA
116300981123     C                   PARM                    TIBS02DS
116400981123      *
116500981123     C                   movel     T02uni        DCDD
116600990521     c* se sto stampando il sollecito alla filiale richiedo solo i documenti che devono
116700990521      * essere richiesti dalla filiale
116800990521     c                   if        (i00tpb = 'F' and §cddsopo = 'S') or
116900990521     c                             (i00tpb <> 'F' and §cddsopo = ' ')
117000990521     c*
117100981123     c                   clear                   ptcde1
117200990521     c*
117300990521     c                   select
117400990521     c* in lingua
117500990521     c                   when      *in08 and §cddengl <> *blanks
117600990521     c                   eval      ptcde1 = '- ' + §CDDengl
117700990521     c                   other
117800981123     c                   eval      ptcde1 = '- ' + §CDDdesc
117900990521     c                   endsl
118000100709      *
118100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
118200100709     c                   if        *in05  and  Not *in20
118300100709     c                   eval      TxtInOut   = ds_DN28PT1
118400100709     c                   exsr      sr_xChkChar
118500100709     c                   if        esito = '1'
118600100709     c                   eval      ds_DN28PT1 = TxtInOut
118700100709     c                   endif
118800100709     c                   endif
118900990521     c*
119000010409      * controllo il salto pagina
119100010409     c                   EXSR      PRT_TESTAT
119200010409      *
119300041119     C  N20              WRITE     DN28P00                                      vuota
119400041119     C  N20              WRITE     DN28PT1
119500041119     C   20              WRITE     DN28M00                                      vuota
119600041119     C   20              WRITE     DN28MT1
119700981123     c* controllo se questo tipo di documento,richiede un'intestazione
119800981123    3c                   if        §cddpint = 'S'
119900981123     c                   clear                   ptcde1
120000120423     c  n08              eval      ptcde1 = '  intestato a ' +  %trim(wsocie)
120100981123     c                             + ' Via Mattei 42 40138 Bologna'
120200120423     c   08              eval      ptcde1 = '  addressed to ' +  %trim(wsocie)
120300120423     c                             + ' Via Mattei 42 40138 Bologna'
120400990310     c                   seton                                        50
120500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
120600100709     c                   if        *in05  and  Not *in20
120700100709     c                   eval      TxtInOut   = ds_DN28PT1
120800100709     c                   exsr      sr_xChkChar
120900100709     c                   if        esito = '1'
121000100709     c                   eval      ds_DN28PT1 = TxtInOut
121100100709     c                   endif
121200100709     c                   endif
121300010409      * controllo il salto pagina
121400010409     c                   EXSR      PRT_TESTAT
121500010409      *
121600041119     c  N20              WRITE     DN28PT1
121700041119     c   20              WRITE     DN28MT1
121800990310     c                   setoff                                       50
121900981123    3c                   endif
122000990521    3c                   endif
122100981123    2c                   endif
122200981123      *
122300990308     c                   add       1             z
122400981123    1c                   enddo
122500990125      *
122600990125     c                   endif
122700981123      *
122800981123     C                   ENDSR
122900981123      *****************************************************************
123000981123      *   FIRMA
123100981123      *****************************************************************
123200981123     C     SUB_firma     BEGSR
123300990225      *
123400990521      * verifico se esistono delle note legate al testo che sto stampando se non sto scrivendo
123500010411      * alla filiale ed esiste un documento con codice 99
123600010411     c                   if        WNOTET <> *blanks  and i00tPB <> 'F' and
123700010411     c                             si99 = 'S'
123800010409      * controllo il salto pagina
123900010409     c                   EXSR      PRT_TESTAT
124000010409      *
124100041119     C  N20              WRITE     DN28P00
124200041119     C   20              WRITE     DN28M00
124300990610     c* stampo le due o cinque righe di note
124400990225     c* 1° riga
124500990225     c                   clear                   PTCde1
124600120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
124700120516     c                   move      w100a         ptcde1
124800100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
124900100709     c                   if        *in05  and  Not *in20
125000100709     c                   eval      TxtInOut   = ds_DN28PT1
125100100709     c                   exsr      sr_xChkChar
125200100709     c                   if        esito = '1'
125300100709     c                   eval      ds_DN28PT1 = TxtInOut
125400100709     c                   endif
125500100709     c                   endif
125600010409      * controllo il salto pagina
125700010409     c                   EXSR      PRT_TESTAT
125800010409      *
125900041119     c  N20              write     DN28PT1
126000041119     c   20              write     DN28MT1
126100990225     c* 2° riga
126200990225     c                   if        w100b <> *blanks
126300990225     c                   clear                   PTCde1
126400120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
126500120516     c                   move      w100b         ptcde1
126600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
126700100709     c                   if        *in05  and  Not *in20
126800100709     c                   eval      TxtInOut   = ds_DN28PT1
126900100709     c                   exsr      sr_xChkChar
127000100709     c                   if        esito = '1'
127100100709     c                   eval      ds_DN28PT1 = TxtInOut
127200100709     c                   endif
127300100709     c                   endif
127400010409      * controllo il salto pagina
127500010409     c                   EXSR      PRT_TESTAT
127600010409      *
127700041119     c  N20              write     DN28PT1
127800041119     c   20              write     DN28MT1
127900990225     c                   endif
128000990610     c* 3° riga
128100990610     c                   if        w100c <> *blanks
128200990610     c                   clear                   PTCde1
128300120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
128400120516     c                   move      w100c         ptcde1
128500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
128600100709     c                   if        *in05  and  Not *in20
128700100709     c                   eval      TxtInOut   = ds_DN28PT1
128800100709     c                   exsr      sr_xChkChar
128900100709     c                   if        esito = '1'
129000100709     c                   eval      ds_DN28PT1 = TxtInOut
129100100709     c                   endif
129200100709     c                   endif
129300010409      * controllo il salto pagina
129400010409     c                   EXSR      PRT_TESTAT
129500010409      *
129600041119     c  N20              write     DN28PT1
129700041119     c   20              write     DN28MT1
129800990610     c                   endif
129900990610     c* 4° riga
130000990610     c                   if        w100d <> *blanks
130100990610     c                   clear                   PTCde1
130200120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
130300120516     c                   move      w100d         ptcde1
130400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
130500100709     c                   if        *in05  and  Not *in20
130600100709     c                   eval      TxtInOut   = ds_DN28PT1
130700100709     c                   exsr      sr_xChkChar
130800100709     c                   if        esito = '1'
130900100709     c                   eval      ds_DN28PT1 = TxtInOut
131000100709     c                   endif
131100100709     c                   endif
131200010409      * controllo il salto pagina
131300010409     c                   EXSR      PRT_TESTAT
131400010409      *
131500041119     c  N20              write     DN28PT1
131600041119     c   20              write     DN28MT1
131700990610     c                   endif
131800990610     c* 5° riga
131900990610     c                   if        w100e <> *blanks
132000990610     c                   clear                   PTCde1
132100120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
132200120516     c                   move      w100e         ptcde1
132300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
132400100709     c                   if        *in05  and  Not *in20
132500100709     c                   eval      TxtInOut   = ds_DN28PT1
132600100709     c                   exsr      sr_xChkChar
132700100709     c                   if        esito = '1'
132800100709     c                   eval      ds_DN28PT1 = TxtInOut
132900100709     c                   endif
133000100709     c                   endif
133100010409      * controllo il salto pagina
133200010409     c                   EXSR      PRT_TESTAT
133300010409      *
133400041119     c  N20              write     DN28PT1
133500041119     c   20              write     DN28MT1
133600990610     c                   endif
133700990225     c*
133800990225     c                   endif
133900981123      *
134000981123      *  Lascio 2 righe dal testo
134100010409      * controllo il salto pagina
134200010409     c                   EXSR      PRT_TESTAT
134300010409      *
134400041119     C  N20              WRITE     DN28P00
134500041119     C  N20              WRITE     DN28P00
134600041119     C   20              WRITE     DN28M00
134700041119     C   20              WRITE     DN28M00
134800981123
134900981123      *
135000981123     c                   clear                   PTCde1
135100120417
135200120417     c                   select
135300120418      * filiale
135400120418     c                   when      i00tpb = 'F'
135500120418     c                   clear                   ptcde1
135600120417      *  sollecitone
135700120417     c                   when      i00fca = wfaso0
135800120423     c                   clear                   ptcde1
135900120417      *  1° sollecito
136000120417     c                   when      i00fca = wfaso1
136100120417     c  n08              movel(P)  so1(3)        PTCde1
136200120418     c   08              movel(P)  so1(7)        PTCde1
136300120417     c*
136400120417      *  2° sollecito
136500120417     c                   when      i00fca = wfaso2
136600120417     c  n08              movel(P)  so2(4)        PTCde1
136700120418     c   08              movel(P)  so2(9)        PTCde1
136800120417      *
136900120417     c                   endsl
137000120417      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
137100120417     c                   if        *in05  and  Not *in20
137200120417     c                   eval      TxtInOut   = ds_DN28PT1
137300120417     c                   exsr      sr_xChkChar
137400120417     c                   if        esito = '1'
137500120417     c                   eval      ds_DN28PT1 = TxtInOut
137600120417     c                   endif
137700120417     c                   endif
137800120417     c*
137900120417      * controllo il salto pagina
138000120417     c                   EXSR      PRT_TESTAT
138100120417      *
138200120417     C  N20              WRITE     DN28PT1
138300120417     C  N20              WRITE     DN28P00                                      vuota
138400120417     C   20              WRITE     DN28MT1
138500120417     C   20              WRITE     DN28M00                                      vuota
138600990521      *
138700990521     c                   select
138800990521      * filiale
138900990521     c                   when      i00tpb = 'F'
139000120418     c                   movel(P)  so3(4)        PTCde1
139100990225      *  sollecitone
139200990521     c                   when      i00fca = wfaso0
139300990521     c  n08              movel(P)  so0(5)        PTCde1
139400990521     c   08              movel(P)  so0(10)       PTCde1
139500981123      *  1° sollecito
139600990521     c                   when      i00fca = wfaso1
139700990521     c  n08              movel(P)  so1(4)        PTCde1
139800990521     c   08              movel(P)  so1(8)        PTCde1
139900990208     c*
140000990225      *  2° sollecito
140100990521     c                   when      i00fca = wfaso2
140200990611     c  n08              movel(P)  so2(5)        PTCde1
140300990521     c   08              movel(P)  so2(10)       PTCde1
140400990521      *
140500990521     c                   endsl
140600100709      *
140700100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
140800100709     c                   if        *in05  and  Not *in20
140900100709     c                   eval      TxtInOut   = ds_DN28PT1
141000100709     c                   exsr      sr_xChkChar
141100100709     c                   if        esito = '1'
141200100709     c                   eval      ds_DN28PT1 = TxtInOut
141300100709     c                   endif
141400100709     c                   endif
141500990208     c*
141600010409      * controllo il salto pagina
141700010409     c                   EXSR      PRT_TESTAT
141800010409      *
141900041119     C  N20              WRITE     DN28PT1
142000041119     C  N20              WRITE     DN28P00                                      vuota
142100041119     C   20              WRITE     DN28MT1
142200041119     C   20              WRITE     DN28M00                                      vuota
142300981123      *
142400981123     c                   movel     Wsocie        PFDsoc
142500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
142600100709     c                   if        *in05  and  Not *in20
142700100709     c                   eval      TxtInOut   = ds_DN28PT1
142800100709     c                   exsr      sr_xChkChar
142900100709     c                   if        esito = '1'
143000100709     c                   eval      ds_DN28PT1 = TxtInOut
143100100709     c                   endif
143200100709     c                   endif
143300010409      * controllo il salto pagina
143400010409     c                   EXSR      PRT_TESTAT
143500010409      *
143600010409      *  1 Spazio
143700041119     C  N20              WRITE     DN28P00
143800041119     C  N20              WRITE     DN28PF1
143900041119     C   20              WRITE     DN28M00
144000041119     C   20              WRITE     DN28MF1
144100981123      *
144200981123     C                   ENDSR
144300981123      *****************************************************************
144400981123      *   SCRIVO STORICO FASI
144500981123      *****************************************************************
144600981123     C     SUB_AGG       BEGSR
144700981123      *
144800981123     c                   clear                   fndcf000
144900981123      *
145000981123     c                   z-add     i00aac        DCFaac
145100981123     c                   z-add     i00fil        DCFfil
145200981123     c                   z-add     i00nca        DCFnca
145300990521     c*  se stampa alla filiale imposto 'F' nel ptr
145400990521     c                   if        i00tpb = 'F'
145500990521     c                   movel     'F'           dcfptr
145600990521     c                   else
145700981123     c                   movel     DCTptr        DCFptr
145800990521     c                   endif
145900990521     c*
146000981123     c                   z-add     i00fca        DCFfca
146100981123     c     *ISO          movel     DATA_oggi     DCFdfc
146200981123     c                   movel     w0140         DCFhfc
146300981123     c                   z-add     i00fgs        DCFfev
146400981123     c                   movel     knmus         DCFpru
146500981123     c                   clear                   DCFftr
146600981123     c                   clear                   DCFdtr
146700020129
146800020129      * Imposto il flag di pratica assicurativa in Franchigia
146900020129     c                   eval      §dcfpfra = §dctpfra
147000041119      * IMPOSTO  INVIO LETTERA
147100041119     c                   eval      §dcfinvi = invio
147200041119      *
147300020129     c                   movel     ddcf01        dcflet
147400981123      *
147500981123     c                   write     fndcf000
147600981123      *
147700981124      * Ricerca fase successiva
147800981124     c                   CLEAR                   FIDN05DS
147900981124     c                   movel     'F'           I05MOD
148000981124     c                   z-add     I00fca        I05fca
148100981124     c                   movel     'P'           I05fpr
148200981124     c                   movel     dctptr        I05tpc
148300981124     c                   movel     'O'           I05ffs
148400981124     c                   movel     ' '           I05fpe
148500981124     c                   z-add     DATEU         i05dta
148600981124     c                   move      dcttad        i05tad
148700981125      * Valorizzo numero CA
148800981125     c                   z-add     DCTaac        i05aac
148900981125     c                   z-add     DCTfil        i05fil
149000981125     c                   z-add     DCTnca        i05nca
149100981124
149200981124     C                   CALL      'FIDN05R'
149300981124     c                   PARM                    KPJBA
149400981124     c                   PARM                    FIDN05DS
149500981124      *
149600981124     c                   movel     O05REC        DS_FNDFA
149700981124      *
149800981124     c                   SELECT
149900981124     c                   WHEN      DFAgfs = 'P'
150000981230     c                   z-add     WDCTlnp       dctgfc
150100981124     c                   WHEN      DFAgfs = 'S'
150200981124     c                   z-add     46            dctgfc
150300981124     c                   WHEN      DFAgfs = 'A'
150400981124     c                   z-add     DCTlna        dctgfc
150500981124     c                   OTHER
150600981124     c                   z-add     DCTfil        dctgfc
150700981124     c                   ENDSL
150800981124      *
150900981124     c* aggiorno  testata CA
151000981124     c* ricerca del punto operativo abilitato alla gestione della fase successiva
151100981124     c                   movel     i00fca        dctfca
151200981222     c                   clear                   dctft1
151300981222     c                   clear                   dctft2
151400981124     c                   update    fndct000
151500981124      *
151600981124      *
151700981123     C                   ENDSR
151800990225      *****************************************************************
151900990225      *   CARICO LE NOTE TESTO RELATIVE A QUESTO SOLLECITO
152000990225      *****************************************************************
152100990225     C     SUB_NOTET     BEGSR
152200990225      *
152300990225     c                   Clear                   wnotet
152400990225      *
152500990225     C                   movel(P)  'C'           Ktpd
152600990225     C                   movel(P)  Wnumca        Knkt
152700990225     C                   movel(P)  'T'           Kstd
152800990225     C                   clear                   Knks
152900990225     c                   movel     dfale1        knks
153000990225     C                   movel     'D'           Ktrc
153100990225      *
153200990225     c                   clear                   yy
153300010409     c                   clear                   des
153400990225      *
153500990225     c     kdcs          chain     fndcs000                           30
153600010406      *
153700010406      * se 30 acceso e sto facendo il 2° sollecito recupero le note relative
153800010406      * al 1° sollecito
153900010406      *
154000010406     c                   if        *in30 and i00fca = wfaso2
154100010406     c                   movel     Wtlso1        knks
154200010406     c     kdcs          chain     fndcs000                           30
154300010406     c                   endif
154400010406      *
154500990225      *
154600990225     c     *in30         DOWeq     *off
154700990610     c     yy            ANDle     15
154800990225     c                   add       1             yy
154900990225     c                   movel     dcsnot        DES(yy)
155000990225     c     kdcs          reade     fndcs000                               30
155100990225     c                   ENDDO
155200990225      *
155300990225     C                   ENDSR
155400981124      *****************************************************************
155500981124      *   IMPOSTO TIPO MODULO E STAMPANTE
155600981124      *****************************************************************
155700981124     C     IMPPRT        BEGSR
155800981124      *
155900981124     c                   Clear                   COMMAND
156000981124     c                   Clear                   Lung
156100981124      *
156200981124     c                   SELECT
156300010409      * se postel
156400010409     c                   WHEN      *in05
156500010418      * solo se è la prima volta che richiamo il PGM ovrprtf file di stampa
156600010418      * dopo visto che chiudo con RT dovrebbe essere ancora aperto
156700010418     c                   if        prima = ' ' and *in05
156800010418     c                   movea     CMD(5)        COMMAND
156900010418     c                   z-add     76            LUNG
157000010418      *
157100010418     c                   eval      prima = 'Y'
157200010418     c                   endif
157300981124      *
157400981124     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp <> *blanks
157500981124     c                   movea     CMD(1)        COMMAND
157600981124     c                   eval      %subst(COMMAND:39:10) = W§TLDstp
157700981124     c                   eval      %subst(COMMAND:60:10) = §TLDtmod
157800001227     c                   eval      %subst(COMMAND:67:3) = 'BAR'
157900990603     c                   z-add     81            LUNG
158000981124      *
158100981124     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp = *blanks
158200981124     c                   movea     CMD(2)        COMMAND
158300981124     c                   eval      %subst(COMMAND:43:10) = §TLDtmod
158400001227     c                   eval      %subst(COMMAND:50:3) = 'BAR'
158500990603     c                   z-add     75            LUNG
158600981124      *
158700981124     c                   WHEN      §TLDtmod = *blanks  and  W§TLDstp <> *blanks
158800981124     c                   movea     CMD(3)        COMMAND
158900981124     c                   eval      %subst(COMMAND:39:10) = W§TLDstp
159000990603     c                   z-add     60            LUNG
159100981124      *
159200981124     c                   OTHER
159300981124     c                   movea     CMD(4)        COMMAND
159400990603     c                   z-add     43            LUNG
159500981124      *
159600981124     c                   ENDSL
159700981124      *
159800981124      * Se ho impostato qualcosa eseguo l'OVRPRTF
159900981124     c                   IF        LUNG > *ZEROS
160000981124     c                   call      'QCMDEXC'
160100981124     c                   parm                    COMMAND
160200981124     c                   parm                    LUNG
160300981124      *
160400981124      *  Apro file di stampa
160500981124     c                   open      FIDN28P
160600010409     c                   ENDIF
160700041122      * SE INVIO MAIL APRO IL FILE FIDN28PM
160800041122     c                   if        *in20
160900041122     c                   open      fidn28pm
161000041122     c                   endif
161100981124      *
161200981124     C                   ENDSR
161300010409      *****************************************************************
161400010409      *   STAMPO TESTATA
161500010409      *****************************************************************
161600010409     C     PRT_TESTAT    BEGSR
161700010409      *
161800010409     c                   if         *IN01 = *ON
161900010409     c                   WRITE     DN28PP
162000010409     c                   EVAL      *in01 = *off
162100010409     c                   endif
162200010409      *
162300041119     c                   if         *IN21 = *ON  AND *IN20
162400041119     c                   WRITE     DN28MP
162500041119     c                   EVAL      *in21 = *off
162600041119     c                   endif
162700010409      *
162800010409     c                   endsr
162900010406      *****************************************************************
163000990308      *   PREPARO LA SCHIERA DEI DATI DA STAMPARE
163100990308      *****************************************************************
163200990308     C     SUB_DOCUM     BEGSR
163300990308      *
163400010409      *
163500010409     c                   clear                   e
163600010409     c                   clear                   cd
163700010410     c                   clear                   gruppoa
163800010410     c                   clear                   gruppob
163900010410     c                   clear                   gruppoc
164000010411     c                   clear                   si99
164100010409      *
164200990308     c* recupero i documenti che mancano
164300990308     c     kdct          setll     fndcc01l
164400990308     c     kdct          reade     fndcc01l                               31
164500990308    1c                   dow       *in31 = *off
164600990308     c* controllo data ricezione
164700990715    2c                   if        dccdrz = *zeros and dccatb = ' '
164800990308     c                   add       1             e
164900990308     c                   movea     dcccdd        cd(e)
165000010411     c* se c'è il codice 99 lo memorizzo
165100010411     c*
165200010411     c                   if        dcccdd = '99'
165300010411     c                   eval      si99 = 'S'
165400010411     c                   endif
165500010411      *
165600990308    2c                   endif
165700990308      *
165800990308     c     kdct          reade     fndcc01l                               31
165900990308    1c                   enddo
166000990308     c*
166100990308     c* per ogni elemento controllo se fa parte di un gruppo
166200990308     c*
166300990308     c                   z-add     1             z
166400990308     c                   dow       z <= e
166500990308     c*
166600990308     c     cd(z)         lookup    cda                                    33
166700990308     c* se trovato nella schiera dei documenti di serie A controllo se devo richiedere
166800990308     c* tutti i documenti del gruppo A
166900990308     c                   if        *in33
167000990309     c                   movel     'Y'           gruppoa
167100990308     c*
167200990308     c                   z-add     1             i
167300990308     c                   dow       i <= 3
167400990308     c     cda(i)        lookup    cd                                     34
167500990308     c* basta che non venga trovato uno dei 3 documenti del gruppo A che vengono stampati
167600990308     c* i singoli documenti del gruppo
167700990308     c                   if        *in34 = *off
167800990309     c                   movel     ' '           gruppoa
167900990308     c                   endif
168000990308     c*
168100990308     c                   add       1             i
168200990308     c                   enddo
168300990308     c* se il flag gruppoa è uguale a 'Y' metto al posto dei codici cd blank
168400990308     c                   if        gruppoa = 'Y'
168500990308     c                   z-add     1             i
168600990308     c                   dow       i <= 3
168700990308     c                   z-add     1             y
168800990308     c     cda(i)        lookup    cd(Y)                                  34
168900990308     c* basta che non venga trovato uno dei 3 documenti del gruppo A che vengono stampati
169000990308     c* i singoli documenti del gruppo
169100990308     c*
169200990308     c   34              movel     '  '          cd(Y)
169300990308     c*
169400990308     c                   add       1             i
169500990308     c                   enddo
169600990308     c                   endif
169700990308     c                   endif                                                  * gruppo A
169800990308     c*
169900990308     c     cd(z)         lookup    cdb                                    33
170000990308     c* se trovato nella schiera dei documenti di serie B controllo se devo richiedere
170100990308     c* tutti i documenti del gruppo B
170200990308     c                   if        *in33
170300990309     c                   movel     'Y'           gruppob
170400990308     c*
170500990308     c                   z-add     1             i
170600990308     c                   dow       i <= 4
170700990308     c     cdb(i)        lookup    cd                                     34
170800990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
170900990308     c* i singoli documenti del gruppo
171000990308     c                   if        *in34 = *off
171100990309     c                   movel     ' '           gruppob
171200990308     c                   endif
171300990308     c*
171400990308     c                   add       1             i
171500990308     c                   enddo
171600990308     c* se il flag gruppoa è uguale a 'Y' metto al posto dei codici cd blank
171700990308     c                   if        gruppob = 'Y'
171800990308     c                   z-add     1             i
171900990308     c                   dow       i <= 4
172000990308     c                   z-add     1             y
172100990308     c     cdb(i)        lookup    cd(Y)                                  34
172200990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
172300990308     c* i singoli documenti del gruppo
172400990308     c*
172500990308     c   34              movel     '  '          cd(Y)
172600990308     c*
172700990308     c                   add       1             i
172800990308     c                   enddo
172900990308     c                   endif
173000990308     c                   endif                                                  * gruppo B
173100990308     c*
173200990308     c     cd(z)         lookup    cdc                                    33
173300990308     c* se trovato nella schiera dei documenti di serie C controllo se devo richiedere
173400990308     c* tutti i documenti del gruppo C
173500990308     c                   if        *in33
173600990309     c                   movel     'Y'           gruppoc
173700990308     c*
173800990308     c                   z-add     1             i
173900990308     c                   dow       i <= 1
174000990308     c     cdc(i)        lookup    cd                                     34
174100990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
174200990308     c* i singoli documenti del gruppo
174300990308     c                   if        *in34 = *off
174400990309     c                   movel     ' '           gruppoc
174500990308     c                   endif
174600990308     c*
174700990308     c                   add       1             i
174800990308     c                   enddo
174900990308     c* se il flag gruppoa è uguale a 'Y' metto al posto dei codici cd blank
175000990308     c                   if        gruppoc = 'Y'
175100990308     c                   z-add     1             i
175200990308     c                   dow       i <= 1
175300990308     c                   z-add     1             y
175400990308     c     cdc(i)        lookup    cd(Y)                                  34
175500990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
175600990308     c* i singoli documenti del gruppo
175700990308     c*
175800990308     c   34              movel     '  '          cd(Y)
175900990308     c*
176000990308     c                   add       1             i
176100990308     c                   enddo
176200990308     c                   endif
176300990308     c                   endif                                                  * gruppo B
176400990308     c*
176500990308     c                   add       1             z
176600990308     c*
176700990308     c                   enddo
176800990308     c*
176900990308     c                   endsr
177000010411      *****************************************************************
177100010411      *   VERIFICO IL CONTENUTO DELLA SCHIERA DEI DOCUMENTI
177200010411      *****************************************************************
177300010411     C     SUB_VERIFICA  BEGSR
177400010411      *
177500010411     c                   z-add     1             z
177600010411     c                   z-add     0             f
177700010411     c                   clear                   docfil
177800010411     c                   clear                   doccli
177900010411     c                   clear                   cdf
178000010411      *
178100010411      * verifico ogni documento che leggo se è per la filiale
178200010411      *
178300010411     c                   dow       z <= e
178400010411     c                   if        cd(z) <> *blank
178500010411      * decodifico il documento
178600010411     c                   clear                   tibs02ds
178700010411     c                   clear                   dcdd
178800010411      *
178900010411     C                   movel     'C'           t02mod
179000010411     C                   movel     knsif         t02sif
179100010411     C                   movel     'CDD'         t02cod
179200010411     C                   movel(P)  cd(z)         t02ke1
179300010411      *
179400010411     C                   CALL      'TIBS02R'
179500010411     C                   PARM                    KPJBA
179600010411     C                   PARM                    TIBS02DS
179700010411      *
179800010411     C                   movel     T02uni        DCDD
179900010411      * se documento da sollecitare alla filiale  lo conteggio nel gruppoo filiale
180000010411      * altrimenti nel gruppo cliente
180100010411     c                   if        §cddsopo = 'S'
180200010411     c                   add       1             docfil
180300010411     c                   movea     cd(z)         cdf(docfil)
180400010411     c                   else
180500010411     c                   add       1             doccli
180600010411     c                   endif
180700010411      *
180800010411     c                   endif
180900010411      *
181000010411     c                   add       1             z
181100010411    1c                   enddo
181200010411      *
181300010411      * se il numero dei documenti da sollecitare ai clienti è uguale a zero
181400010411      * e non devo stampare nè il gruppoa nè il gruppob nè il gruppoc
181500010411      * e quelli alla filiale è diverso da 0 stampo solo una riga di sollecito
181600010411      * alle filiali
181700010411      *
181800010411     c                   if        doccli = 0  and gruppoa= ' ' and gruppob=' '
181900010411     c                             and gruppoc= ' '
182000010411     c                   eval      stampa = 'N'
182100010411     c                   endif
182200010411      *
182300010411     c                   if        docfil > 0
182400010411      * stampo  alla filiale
182500010411     c     dctfil        chain     azorg01l
182600010411     c                   if        not %found(azorg01l) or orgfva <> ' '
182700010411     c                   clear                   orgdes
182800010411     c                   endif
182900010411      * per ogni documento stampo una riga
183000010411     c                   z-add     1             f
183100010411     c                   dow       f <= docfil
183200010411     c                   if        cdf(f) <> *blanks
183300010411      * decodifico il documento
183400010411     c                   clear                   tibs02ds
183500010411     c                   clear                   dcdd
183600010411      *
183700010411     C                   movel     'C'           t02mod
183800010411     C                   movel     knsif         t02sif
183900010411     C                   movel     'CDD'         t02cod
184000010411     C                   movel(P)  cdf(f)        t02ke1
184100010411      *
184200010411     C                   CALL      'TIBS02R'
184300010411     C                   PARM                    KPJBA
184400010411     C                   PARM                    TIBS02DS
184500010411      *
184600010411     C                   movel     T02uni        DCDD
184700010411      *
184800010411      * verifico se devo stampare la testata
184900010411     c                   if        not *in99 or *inof
185000010411     c                   except    testa
185100010411     c                   seton                                        99
185200010411     c                   setoff                                       of
185300010411     c                   endif
185400010411      *
185500010411     c                   except    filiale
185600010420      * Posta elettronica alle filiali
185700130411      * dal 11 Aprile 2013 non facciamo più la posta elettronica ma solo
185800130411      * la scritta delle note relative al sollecito alla filiale
185900010420     c                   exsr      crtposta
186000010420      *
186100010411     c                   endif
186200010411      *
186300010411     c                   add       1             f
186400010411      *
186500010411     c                   enddo
186600010411      *
186700010411     c                   endif
186800010411      *
186900010411     c                   ENDSR
187000010420      *----------------------------------------------------------------
187100010420      * CREAZIONE POSTA ELETTRONICA ALLE FILIALI
187200010420      *----------------------------------------------------------------
187300010420     C     CRTPOSTA      BEGSR
187400010420      *
187500010420     c*
187600010420      * scrittura nelle note dei solleciti alle filiali
187700010420     c*
187800010420     c                   eval      dcstrc='N'
187900010420     c                   eval      dcspno=0
188000010420     c*
188100010420     c* scrittura note preparazione dei campi strandard
188200010420     c                   eval      dcsatb=' '
188300010420     c                   eval      dcstpd='C'
188400010420     c                   eval      dcsnkt=Wnumca
188500010420     c                   eval      dcsstd=' '
188600010420     c                   movel     i00fca        dcsnks
188700010420     c     *ISO          movel     DATA_oggi     DCSdim
188800010420     c                   movel     w0140         DCShim
188900010420     c*
189000010420     c* cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
189100010420     c     keydcs        setgt     fndcs01l
189200010420     c     keydcsp       readpe    fndcs01l                               93
189300010420     c*
189400010420     c                   move      knmus         dcspru
189500010420     c                   movel     i00fgs        dcspos
189600010420     c                   clear                   dcsstn
189700010420     c                   clear                   dcsft1
189800010420     c                   clear                   dcsdt1
189900010420     c* scrivo la prima riga
190000010420     c                   add       1             dcspno
190100010420     c                   movel(p)  NOTE(1)       dcsnot
190200010420     c                   move      dctfil        dcsnot
190300010420     c                   write     fndcs000
190400010420     c*
190500010420     c* scrivo la seconda riga
190600010420     c                   add       1             dcspno
190700010420     c                   movel(p)  NOTE(2)       dcsnot
190800010420     c                   write     fndcs000
190900010420     c*
191000010420     c* scrivo la riga documento
191100010420     c                   add       1             dcspno
191200010420     c                   movel(p)  §cdddesc      dcsnot
191300010420     c                   write     fndcs000
191400010420     c*
191500010420     c* scrivo la riga documento
191600010420     c                   eval      dcsnot = %subst(§cdddesc:36:25)
191700010420     c*
191800010420     c                   if        dcsnot <> *blanks
191900010420     c                   add       1             dcspno
192000010420     c                   write     fndcs000
192100010420     c                   endif
192200010420     c*
192300010420     c                   endsr
192400010409      *****************************************************************
192500010409      * DECODIFICO NAZIONE
192600010409      *****************************************************************
192700010409     C     CTRNAZ        BEGSR
192800010409      *
192900010409     C                   clear                   DS15
193000010409     C                   movel     '15'          §COD
193100010409     C                   movel(P)  WNAZ          §KEY
193200010409     C     KTAB          CHAIN     TABEL00f                           30
193300010409      *
193400010409     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
193500010409     C                   movel     TBLUNI        DS15
193600010409     C                   endif
193700010409      *
193800010409     C                   ENDSR
193900980506      *****************************************************************
194000980506      *   ROUTINE INIZIALE
194100980506      *****************************************************************
194200010409     C     INIZIO        BEGSR
194300980506      *
194400980506     C     *ENTRY        PLIST
194500980506     C                   PARM                    KPJBA
194600980730      *
194700981120     C                   MOVEL     KPJBU         FIDN00DS
194800010409     c                   z-add     *zeros        page
194900011002     c                   EVAL      *in01 = *off
195000990310      *
195100981124      * recupero il codice del testo da stampare
195200981124     c                   CLEAR                   FIDN05DS
195300981124     c                   movel     'C'           I05MOD
195400981124     c                   z-add     I00fca        I05fca
195500981124
195600981124     C                   CALL      'FIDN05R'
195700981124     c                   PARM                    KPJBA
195800981124     c                   PARM                    FIDN05DS
195900981124      *
196000981124     c                   movel     O05REC        DS_FNDFA
196100981124      *
196200981124     c* aggancio tabella documenti
196300981124      *
196400981124     c                   clear                   tibs02ds
196500981124     c                   clear                   dtld
196600981124      *
196700981124     C                   movel     'C'           t02mod
196800981124     C                   movel     knsif         t02sif
196900981124     C                   movel     'TLD'         t02cod
197000981124     C                   movel(P)  Dfale1        t02ke1
197100981124      *
197200981124     C                   CALL      'TIBS02R'
197300981124     C                   PARM                    KPJBA
197400981124     C                   PARM                    TIBS02DS
197500981124      *
197600981124     C                   movel     T02uni        DTLD
197700990225      *
197800990225      * se previte note testo le recupero
197900990225     c                   if        §tldnote <> 'N'
198000990225      *
198100990225     c                   exsr      SUB_NOTET
198200990225      *
198300990225     c                   endif
198400981124      * recupero la data ultimo sollecito
198500981124     c* se 2° sollecito
198600981124     c                   if        i00fca = wfaso2
198700981124     c     kdct          setgt     fndcf01l
198800981124     c     kdct          readpe(n) fndcf01l                               31
198900981124     c                   if        not *in31
199000981124     c                   movel     dcfdfc        data08
199100981124     c                   endif
199200981124     c                   endif
199300990520      *
199400990520      * se i00tpb (tipo bolla ) c'è il valore "F" significa che è un sollecito alla filiale
199500990521     c                   if        i00tpb = 'F'
199600990520     c                   eval      *in90 = *on
199700990520     c                   endif
199800010406      *
199900010406      * se 2° sollecito o sollecitone stampo per postel
200000010406      *
200100010406     c                   if        i00fca = wfaso2 or i00fca = wfaso0
200200010406     c                   seton                                        05
200300010406     c                   endif
200400981124      *
200500980729     C                   ENDSR
200600100709      /free
200700100709       //--------------------------------------------------------------
200800100709       //?"Bonifica" campi in stampa da caratteri indesiderati         ?
200900100709       //--------------------------------------------------------------
201000100709       BEGSR  sr_xChkChar;
201100100709
201200100709         ChkNull = '1';
201300100709
201400100709         xChkChar ( TxtInOut :
201500100709                    ElencoChar :
201600100709                    TipoElenco :
201700100709                    CharSost :
201800100709                    Uppercase :
201900100709                    ChkNull :
202000100709                    CharNull :
202100100709                    Esito);
202200100709
202300100709       ENDSR;
202400100709      /end-free
202500010411      *****************************************************************
202600010411      *   ROUTINE INIZIALE  SOLO LA PRIMA VOLTA
202700010411      *****************************************************************
202800010411     C     *INZSR        BEGSR
202900010411      *
203000010411      *
203100010411     C                   Z-ADD     1             CODUT
203200010411     C                   CALL      'X§PARUT'
203300010411     C                   PARM                    UT§DSE0F
203400010411     C                   MOVEL     RAGUT         RSUT
203500010411     C                   MOVEL     REC80         CNCR80
203600010411
203700010411      * DATA
203800010411     C                   TIME                    W0140
203900010411     C                   MOVE      W0140         WDTGIO
204000010411     C                   MOVEL     W0140         WORA
204100010411      *
204200010411     C                   Z-ADD     WDTGIO        G02DAT
204300010411     C                   MOVEL     *BLANK        G02ERR
204400010411     C                   CALL      'XSRDA8'
204500010411     C                   PARM                    WLBDAT
204600010411     C                   MOVEL     G02INV        DATEU
204700010411     C     *iso          MOVEL     DATEU         DATA_oggi
204800010411      * accesso testata CA
204900010411     C     kdct          KLIST
205000010411     C                   KFLD                    i00aac
205100010411     C                   KFLD                    i00fil
205200010411     C                   KFLD                    i00nca
205300010411      * accesso archivio bolle
205400010411     c     kbol01        klist
205500010411     c                   kfld                    i00aas
205600010411     c                   kfld                    i00lnp
205700010411     c                   kfld                    i00nrs
205800010411     c                   kfld                    i00nsp
205900010411      * accesso note CA
206000010411     C     KDCS          klist
206100010411     C                   KFLD                    Ktpd
206200010411     C                   KFLD                    Knkt
206300010411     C                   KFLD                    Kstd
206400010411     C                   KFLD                    Kdim
206500010411     C                   KFLD                    Khim
206600010411     C                   KFLD                    Knks
206700010411     C                   KFLD                    Ktrc
206800010420     c*
206900010420     c     keydcs        KLIST
207000010420     c                   KFLD                    dcstpd
207100010420     c                   KFLD                    dcsnkt
207200010420     c                   KFLD                    dcsstd
207300010420     c                   KFLD                    dcsdim
207400010420     c                   KFLD                    dcshim
207500010420     c                   KFLD                    dcsnks
207600010420     c                   KFLD                    dcstrc
207700010420     c                   KFLD                    prgnot
207800010420     c                   eval      prgnot= 999
207900010420     c* di fndcs01l - parziale
208000010420     c*
208100010420     c     keydcsp       KLIST
208200010420     c                   KFLD                    dcstpd
208300010420     c                   KFLD                    dcsnkt
208400010420     c                   KFLD                    dcsstd
208500010420     c                   KFLD                    dcsdim
208600010420     c                   KFLD                    dcshim
208700010420     c                   KFLD                    dcsnks
208800010420     c                   KFLD                    dcstrc
208900010411     C* ACCESSO   TABEL
209000010411     C     KTAB          KLIST
209100010411     C                   KFLD                    CODUT
209200010411     C                   KFLD                    §COD
209300010411     C                   KFLD                    §KEY
209400010411     C* ACCESSO   TABEL
209500010411     C     KTABe         KLIST
209600010411     C                   KFLD                    CODUT
209700010411     C                   KFLD                    §COD
209800010411      *
209900010411     C     Kdet          klist
210000010411     C                   KFLD                    DCTaae
210100010411     C                   KFLD                    DCTnev
210200010411      *
210300010411     c* valorizzo le costanti di stampa TESTA1 e TESTA2
210400010411     c                   movel     tes(1)        tes1
210500010411     c                   movel     tes(3)        tes2
210600010411     c                   movel     tes1          testa1
210700010411     c                   move      tes2          testa1
210800010411     c*
210900010411     c                   movel     tes(2)        tes1
211000010411     c                   movel     tes(4)        tes2
211100010411     c                   movel     tes1          testa2
211200010411     c                   move      tes2          testa2
211300010411      *
211400010411     C                   ENDSR
211500010411     c*
211600010411      *----------------------------------------------------------------
211700010411     O*
211800010411     OPRTF198   E            TESTA             2
211900010411     O                       RSUT              +  0
212000010411     O                                         +  0 '/'
212100010420     O                       i00fgs            +  0
212200010411     O                                           90 'DOCUMENTI ANCORA DA RICEVE'
212300010411     O                                              'RE DAI P.O. PER AVANZAMENT'
212400010411     o                                              'O C.A.'
212500010411     O                                          165 'FIDN28R'
212600010411     O                       WDTGIO        Y    180
212700010411     O                                          194 'PAG.'
212800010411     O                       PAGE1         Z    198
212900010411     O          E            TESTA             3
213000010411     O                       KNSIF             +  0
213100010411     O                       KNMUS             +  1
213200010411     O                       WORA               180 '  :  :  '
213300010411     O          E            TESTA       1
213400010411     O                       testa1
213500010411     O          E            TESTA       1
213600010411     O                       testa2
213700010411     O          E            TESTA       1
213800010411     O                       TRAT               198
213900010418     O          E            FILIALE     2
214000010411     O                       dctaac
214100010411     O                       dctfil           +   1
214200010411     O                       dctnca        Z  +   1
214300010418     O                       dctaas           +   2
214400010411     O                       dctlnp           +   1
214500010411     O                       dctnrs        Z  +   1
214600010411     O                       dctnsp        Z  +   1
214700010411     O                       tasmgs           +   1 '0  /  '
214800010411     O                       dctfil           +   1
214900010411     O                       orgdes           +   1
215000010411     O                       §cdddesc         +   1
215100010418     O                                        +   1 '__________________________'
215200010418     O                                              '__________________________'
215300980914      *----------------------------------------------------------------
215400080114** SO0 -- Allegato 13 Bis Sollecito chi non ha ancora inviato niente ------------------------------*
215500120423         Con riferimento alla  pratica assicurativa in oggetto Vi comunichiamo che non ci sono mai
215600120423pervenuti i  documenti richiesti  Vi  preghiamo di volerceli inviare al piu' presto per consentire
215700120423l'evasione della pratica, ricordandoVi  che  IN  MANCANZA  DI  ESSI IL DANNO  NON  POTRA'  ESSERE
215800120423RISARCITO.
215900120417In attesa di quanto sopra porgiamo cordiali saluti.
216000120423         We refer to the above mentioned insurance file to inform you we never received the
216100120423documents werequested. Please send them us soon to enable us to procede with your file. We remind
216200120423you that NON RECEIVING THEM WE WILL NOT BE IN A POSITION TO PAY THE DAMAGE.
216300120423
216400120423Waiting to as above we hand Yours sincerely.
216500080114** SO1 -- Allegato 12 1° Sollecito ----------------------------------------------------------------*
216600120417         Analizzata la documentazione precedentemente fornitaci, di seguito elenchiamo
216700120417quanto mancante:
216800120417In mancanza di essi, il danno non potrà essere risarcito.
216900120417In attesa di quanto sopra porgiamo cordiali saluti.
217000120418         After checking the documents you sent us previously, here as follows you can find which
217100120418documents are missing:
217200120418If we will not receive them, we will not be able to refund the claim.
217300120418We wait for these documents and in the meantime we remain with our best regards.
217400080114** SO2 -- Allegato 13 2° Sollecito ----------------------------------------------------------------*
217500120418         Analizzata la documentazione fornitaci a seguito del nostro sollecito del xx/xx/xx, di
217600120418seguito elenchiamo quanto mancante:
217700120417
217800120417In mancanza di essi, il danno non potrà essere risarcito.
217900120417In attesa di quanto sopra porgiamo cordiali saluti.
218000120418         Checking the documents you sent us previously, after our reminder of xx/xx/xx here as
218100120418follows you can find which documents are missing:
218200120418
218300120418If we will not receive them, we will not be able to refund the claim.
218400120418We wait for these documents and in the meantime we remain with our best regards.
218500080114** SO3 -- Sollecito alla Filiale ------------------------------------------------------------------*
218600990520         Con riferimento alla pratica assicurativa in oggetto Vi preghiamo di volerci inviare i
218700990520documenti sotto segnalati per consentire l'evasione della pratica.
218800990520
218900990520         In attesa di quanto sopra porgiamo cordiali saluti.
219000080114** CMD - (Lungh. 85) ---------------------------------------------------------------*
219100990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx) HOLD(*YES)    L81
219200990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) FORMTYPE(xxxxxxxxxx) SAVE(*YES) HOLD(*YES)          L75
219300990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) OUTQ(xxxxxxxxxx) HOLD(*YES)                         L60
219400990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) HOLD(*YES)                                          L43
219500010409OVRPRTF FIDN28P SPLFNAME(LET198) FORMTYPE(SOLDOC) OUTQ(PICCIONEDB/BARPICSTM)         L76
219600041125OVRPRTF FIDN28PM FORMTYPE(SOLDOC) OUTQ(xxxxxxxxxx) USRDFNDTA('
219700080114** DOC A ------------------------------------------------------------------------------------------*
219800120418- se causale vendita: fotocopia della fattura della merce spedita completa di tutte le pagine
219900120418  se diversa causale: fotocopia del listino di vendita o della fattura d'acquisto di tutta la
220000120418  merce spedita
220100120418- in case of sale: whole photocopy of the original invoice of the sent goods
220200120418  in the other cases: photocopy of the price list or of the purchase invoice of all the goods sent
220300080114** DOC B ------------------------------------------------------------------------------------------*
220400120418- se la merce è stata riparata: fotocopia della fattura di riparazione oppure scheda interna di
220500120418  lavorazione con listino prezzi della parti sostituite
220600120418  se la merce non è riparabile: dichiarazione a riguardo
220700120418- in case the damaged goods are fixed: copy of the invoice only relevant to the fixing or the
220800120418  internal document of the manufacturing with the price list of the replaced parts
220900120418  in case the goods are not repairable: please send us declaration
221000080114** DOC C ------------------------------------------------------------------------------------------*
221100120418- copia della contestazione di danno inviata alla BRT S.p.A. entro i termini di legge (8 gg.
221200120418  dalla consegna o dal reso merce - art. 1698 c.c.) e relativa prova d'invio
221300120418- copy of the damage claim sent to BRT S.p.A. within the prescribed terms (within 8 days from the
221400120418  delivery or the return of goods article 1698 c.c.) and the relevant evidence of its sending
221500990308**  CDA
221600990308A1A2A3
221700990308**  CDB
221800990308B1B2B3B4
221900990308**  CDC
222000990308C1
222100080114** TES  (Lungh. 100) - -Disegno stampa- -----------------------------------------------------------*
222200070515                  S P E D I Z I O N E         RICHIEDERE ALLA           Documento
222300070515Anno P.O.  N°C.A. Anno LNP Ser Numero  Data       Filiale
222400010411
222500010411
222600010411xxxx xxx xxxxxxx  xxxx xxx xx xxxxxxx xx/xx xxx xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxx
222700010418xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
222800080114** NOTE --------------------------*
2229000705152° sollecito alla Filiale
223000010420del documento sotto segnalato
