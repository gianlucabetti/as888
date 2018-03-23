000100980506      *------------------------------------------------------------------------*
000200981124      * Stampa  primo/secondo  Sollecito Invio Documenti
000300980506      *------------------------------------------------------------------------*
000400990520      *
000500990520      * se tipo bolla uguale a F il sollecito è diretto alla filiale
000600990521      *
000700981124      ****************************************************************
000800981124      *  FASI C.A. FISSATE A PROGRAMMA
000900981124      ****************************************************************
001000990125      *    605 = 1° Sollecito invio documenti                (WFASO1)
001100990125      *    615 = 2° Sollecito invio documenti                (WFASO2)
001200990125      *    575 = Sollecito x chi non ancora inviato nulla    (WFASO0)
001300990125
001400010406      ****************************************************************
001500010406      *  TESTO LETTERA FISSO A PROGRAMMA PER RECUPERARE LE NOTE
001600010406      ****************************************************************
001700010406      *    A12 = 1° Sollecito invio documenti                (WTLSO1)
001800010406
001900980729      ****************************************************************
002000980729      *  RIEPILOGO INDICATORI
002100980729      ***************************************************************
002200981123      * 01   - Salta pagina
002300990125      * 02   - Salta ulteriore descrizione anomalia
002400990520      * 03   - stampo numero evento
002500010406      * 05   - STAMPO per postel
002600990125      * 06   - Stampo l'interlocutore
002700990310      * 08   - Cliente Estero documenti in lingua
002800990524      * 10   - Se esiste già il documento di richiesta risarcimento
002900990520      * 13   - stampo quantità danneggiate
003000041118      * 20   - mando via e-mail
003100041118      * 21   - Salta pagina via e-mail
003200980911      * 30   - Generico
003300980915      * 31   - Generico
003400980915      * 32   - Generico
003500990308      * 33   - Generico
003600990310      * 50   - Alta intensità per l'intestazione
003700990520      * 90   - Stampo Sollecito alla filiale
003800980729      ****************************************************************
003900980729
004000980730     H DECEDIT('0,') DATEDIT(*DMY/)
004100100709     h option(*nodebugio)
004200980729
004300981124     ffndct01l  uf   e           K DISK
004400981120     ffndcc01l  if   e           K DISK
004500990524     ffndcl01l  if   e           K DISK
004600010420     ffndcs01l  uf a e           K DISK
004700990520     ffndet01l  if   e           K DISK
004800081229     fTITAS30c  if   e           K DISK
004900081229     ftntmd01l  if   e           K DISK
005000981120     ffndcf01l  uf a e           K DISK
005100981123     Fazorg01l  if   e           k DISK
005200981230     Ftabel00f  if   E           k DISK
005300981123     ffidn28p   O    e             PRINTER  oflind(*IN01)  usropn
005400041118     ffidn28pm  O    e             PRINTER  oflind(*IN21)  usropn
005500010418     fPRTF198   O    F  198        PRINTER OFLIND(*INOF)
005600980729
005700980914      *----------------------------------
005800990520     D SO0             S            100    DIM(10) CTDATA PERRCD(1)             Mai ricevuto documen
005900990520     D SO1             S            100    DIM(8)  CTDATA PERRCD(1)             testo lettera 1° sol
006000990520     D SO2             S            100    DIM(10) CTDATA PERRCD(1)             testo lettera 2° sol
006100990520     D SO3             S            100    DIM(4)  CTDATA PERRCD(1)             Sollecito Filiale
006200041119     D CMD             S             85    DIM(6)  CTDATA PERRCD(1)             OVRPRTF
006300120418     D DOA             S            100    DIM(5)  CTDATA PERRCD(1)             Testo documenti A
006400120418     D DOB             S            100    DIM(6)  CTDATA PERRCD(1)             Testo Documenti B
006500120418     D DOC             S            100    DIM(4)  CTDATA PERRCD(1)             Testo Documenti C
006600990308     D CDA             S              2    DIM(3)  CTDATA PERRCD(3)             Cod.doc.x testo A
006700990308     D CDB             S              2    DIM(4)  CTDATA PERRCD(4)             Cod.doc.x testo B
006800990308     D CDC             S              2    DIM(1)  CTDATA PERRCD(1)             Cod.doc.x testo C
006900010411     D TES             S            100    DIM(6)  CTDATA PERRCD(1)             TESTATA STAMPA
007000010420     D NOTE            S             35    DIM(2)  CTDATA PERRCD(1)             note
007100981123     D w35             S             35    DIM(4)                               descr. tipo anomalia
007200990308     D cd              S              2    DIM(250)                             codice documenti
007300010411     D cdf             S              2    DIM(250)                             codice documenti
007400980915      *----------------------------------
007500110526     D KBAR            C                   CONST('BRT  S.p.A.')
007600981123     D wsocie          S             20
007700990603     D command         S             85
007800981124     D lung            S             15  5
007900980730      *----------------------------------
008000980730     D WLBDAT          DS                  INZ
008100980730     D  g02dat                 1      8  0
008200980730     D  g02inv                 9     16  0
008300980730     D  g02err                17     17
008400980730     D  g02tgi                18     22  0
008500980914      *
008600980915      *----------------------------------
008700981120     D FIDN00DS      E DS
008800981123     d   Wnumca                6     19
008900081229     D FIDNBEDS      E DS
009000980921     D FIDN05DS      E DS
009100981124     D DS_FNDFA      E DS                  extname(fndfa00f)
009200980507     D KPJBA         E DS
009300981120     D DTAD          E DS
009400981123     D DCDD          E DS
009500981124     D DTLD          E DS
009600981230     D DSTB          E DS
009700010409     D DS15          E DS
009800981230     D DDCT01        E DS
009900020129     d ddcf01        e ds
010000990310
010100981123      *
010200980915     D UT§DSE0F      E DS
010300981123      *
010400981123     D CNCR80        E DS
010500980915     D TIBS02DS      E DS
010600041118      *
010700041118     D TRUL44DS      E DS
010800050902     D TRTCM1DS      E DS
010900980729      *----------------------------------
011000981123     D Ktpd            S                   LIKE(DCStpd)
011100981123     D Knkt            S                   LIKE(DCSnkt)
011200981123     D Kstd            S                   LIKE(DCSstd)
011300981123     D Kdim            S                   LIKE(DCSdim)
011400981123     D Khim            S                   LIKE(DCShim)
011500981123     D Knks            S                   LIKE(DCSnks)
011600981123     D Ktrc            S                   LIKE(DCStrc)
011700981230     D §cod            S                   LIKE(TBLcod)
011800981230     D §key            S                   LIKE(TBLkey)
011900081229     D Kcli            S                   LIKE(DCTksc)
012000981123     D Kkcc            S                   LIKE(DCTkcc)
012100981124     D w§tldstp        S                   LIKE(§tldstp1)
012200990309     d w§dctport       S                   like(§dctport)
012300981124     d Wfaso1          s                   like(i00fca)
012400981124     d                                     inz(605)
012500981124     d Wfaso2          s                   like(i00fca)
012600981124     d                                     inz(615)
012700990125     d Wfaso0          s                   like(i00fca)
012800990125     d                                     inz(575)
012900010406     d Wtlso1          s                   like(dfale1)
013000010406     d                                     inz('A12')
013100081229     D wnaz            s                   LIKE(OBEnaz)
013200010420     d prgnot          S                   LIKE(dcspno)                         *progressivo note
013300020328     D wclsflo7        S              1
013400981123     D wdataoggi       S             10
013500981123     D wanno           s              4
013600990505     D wannop          s              4
013700981123     D wfil            s              3
013800981123     D wnum            s              7
013900990505     D wnump           s              7
014000990125     D wpzman          S             11
014100990125     D wcoman          S             11
014200990125     D wkgman          S             11
014300981123     D w007a           s              7
014400990125     D w007b           s              7
014500990125     D w005a           s              5
014600010420     D waac            s              2
014700990308     D gruppoa         s              1
014800990308     D gruppob         s              1
014900990308     D gruppoc         s              1
015000990310     D estero          s              1
015100041122     D INVIO           s              1
015200010409     D prima           s              1
015300010411     D stampa          s              1
015400010411     D si99            s              1
015500050520     D mailno          s              1
015600010411     D rsut            S             20
015700990308     D y               s              3  0
015800990308     D z               s              3  0
015900990308     D i               s              3  0
016000990308     D e               s              3  0
016100010411     D f               s              3  0
016200990310     D x               s              3  0
016300010420     D NN              s              1  0
016400981123     D yy              s              3  0
016500981123     D xx              s              3  0
016600981123     D dateu           s              8  0
016700981123     D wdtgio          S              8  0
016800981123     D wora            S              6  0
016900981123     D w0140           S             14  0
017000981123     D w0040           S              4  0
017100981123     D w0030           S              3  0
017200981230     D Wdctlnp         s              3  0
017300010411     D docfil          s              3  0                                      contatore doc P.O.
017400010411     D doccli          s              3  0                                      contatore doc client
017500041125     D svcom           s             80
017600010411     D TES1            S            100
017700010411     D TES2            S             98
017800010411     D TESTA1          S            198
017900010411     D TESTA2          S            198
018000010411     D TRAT            S            198    INZ(*all'-')
018100981123      *
018200981123     D DATA_eur        S               D   DATFMT(*eur)
018300981123     D DATA_oggi       S               D   DATFMT(*eur)
018400080114     d                 ds
018500981126     d  aa04                   1      4
018600981126     d  aa02                   3      4
018700981124     d  mm02                   5      6
018800981124     d  gg02                   7      8
018900981124     d  data08                 1      8
019000990225      *
019100990225     d DS_DESCR        DS
019200990610     d WNOTET                       525
019300990610     d DES                           35    DIM(15)  overlay(Wnotet:1)           Note testo
019400990225     d W100A                        100             overlay(WNOTET:1)           1° 100 caratteri
019500990225     d W100B                        100             overlay(WNOTET:106)         2° 100 caratteri
019600990610     d W100C                        100             overlay(WNOTET:211)         2° 100 caratteri
019700990610     d W100D                        100             overlay(WNOTET:316)         2° 100 caratteri
019800990610     d W100E                        100             overlay(WNOTET:421)         2° 100 caratteri
019900100709
020000100709       // -?Strutture Dati per la definizione dei campi in stampa?
020100100709       //  ?per la "bonifica" degli stessi da caratteri indesiderati?
020200100709       //  ?da POSTEL?
020300100709     d ds_DN28PI1      ds
020400100709     d   PICrag
020500100709     d   PICind
020600100709     d   PICcap
020700100709     d   PICloc
020800100709     d   PICpro
020900100709     d   PICnaz
021000100709     d   PICpec
021100100709     d   PICdal
021200100709     d ds_DN28PO1      ds
021300100709     d   POCogg
021400100709     d   POCog1
021500100709     d ds_DN28PO2      ds
021600100709     d   POCrsm
021700100709     d   POCrsd
021800100709     d   POClod
021900100709     d   POCprn
022000100709     d   POCkev
022100100709     d   POCtae
022200100709     d   POCtad
022300100709     d   POCdan
022400100709     d   POCqtd
022500100709     d   POCdpz
022600100709     d ds_DN28PT1      ds
022700100709     d   PTCde1
022800100709     d ds_DN28PF1      ds
022900100709     d   PFDsoc
023000981123      *
023100010420      *
023200010420     D PE_TESTA1       C                   'Richiesta documenti per avanzamento-
023300010420     D                                      C.A. '
023400010420     D PE_RIGA1_1      C                   'Con riferimento alla pratica assicu-
023500010420     D                                     rativa in oggetto Vi preghiamo'
023600010420     D PE_RIGA1_2      C                   'di volerci inviare al più presto il-
023700010420     D                                      documento sotto elencato'
023800010420      *
023900010420     D PE_SALUTI       C                   'Distinti Saluti.     Ufficio Danni'
024000010420     D
024100010420     D KK_utente       S             10    INZ('RAC*      ')
024200100709
024300100709       //--------------------------------------------------------------
024400100709       //?Definizione procedure usate.                                 ?
024500100709       //--------------------------------------------------------------
024600100709
024700100709       // -?Parametri per XCHKCHAR?
024800100709      /copy gaitrasrc/srcProtoPI,XCHKCHAR
024900100709       // -?"Bonifica" stringa da caratteri indesiderati?
025000100709      /copy gaitrasrc/srcProtoPR,XCHKCHAR
025100100709
025200980731      *---------------------------------------------------------------------
025300980910
025400010409
025500010409      *
025600010409      * routine iniziale
025700010409     c                   EXSR      INIZIO
025800981124      *
025900981123      * aggancio la CA
026000981123     c     kdct          chain     fndct01l                           30
026100981230     c*
026200981230     c                   movel     dctflo        ddct01
026300981230     c                   movel     §dctlnpc      Wdctlnp
026400981230     c*
026500081229      * aggancio dati bolle
026600081229     c     kbol01        chain     tntmd000                           30
026700081229     c     kbol01        chain     TITAS30c                           30
026800081229      * se recupero rileggo
026900081229     C                   movel     'TB'          §cod
027000081229     C                   movel(P)  tastbl        §key
027100081229     C     ktab          chain     tabel00f                           31
027200081229     C                   movel     tbluni        dstb
027300081229     c                   if        §TBrbl = 'R'
027400081229     c     kbol01        reade     TITAS30c                               30
027500081229     c                   endif
027600990520     c
027700990309     c                   EXSR      SUB_DOCUM                                     recupero documenti
027800010411      *
027900010411     c                   clear                   stampa
028000010410      * se 2° sollecito verifico:
028100010410      *                         -se devo sollecitare solo documenti della filiale non stampo
028200010410      *                          il sollecito al cliente ma stampo una riga di tabulato
028300010410      *                          solleciti alla filiale
028400981123      *
028500010410     c                   if        i00fca = wfaso2
028600010410     c                   EXSR      SUB_VERIFICA
028700010410     c                   endif
028800010411      *
028900010411      * se il flag stampa è diverso da N stampo
029000010411      *
029100010411     c                   if        stampa <> 'N'
029200010411      *
029300010410      *
029400990520     c  n90              EXSR      SUB_INTES                                     intestazione
029500990520     c   90              EXSR      SUB_PO                                        intest./PO
029600981123     c                   EXSR      SUB_OGGE                                      oggetto
029700981123     c                   EXSR      SUB_TESTO                                     testo
029800981123     c                   EXSR      SUB_FIRMA                                     firma
029900981123      *
030000981123     c                   EXSR      SUB_AGG                                       aggiorno files
030100990520      *
030200990520      *
030300981124      * Chiudo file stampa testo sollecito
030400010409      * solo se non è stampa postel
030500010409     c  N05              CLOSE     fidn28p
030600041122     c   20              CLOSE     fidn28pm
030700010411      *
030800010411     c                   endif
030900980910
031000981029     c                   clear                   tibs02ds
031100981029     c                   movel     'C'           t02tla
031200981029     c                   call      'TIBS02R'
031300981029     c                   parm                    kpjba
031400981029     c                   parm                    tibs02ds
031500010409     C  N05              EVAL      *INLR = *ON
031600010409     C   05              EVAL      *INRT = *ON
031700980915      *****************************************************************
031800981120      *   PREAPARO I DATI DELLA TESTATA DELLA LETTERA
031900980915      *****************************************************************
032000981123     C     SUB_INTES     BEGSR
032100050520
032200050520     c                   eval      mailno = ' '
032300081229      * cerco il beneficiario
032400081229     c                   clear                   fidnbeds
032500081229      *
032600081229     c                   movel     'B'           IBEmod
032700081229     c                   z-add     dctaac        IBEaac
032800081229     c                   z-add     dctfil        IBEfil
032900081229     c                   z-add     dctnca        IBEnca
033000081229     c                   z-add     dctaas        IBEaas
033100081229     c                   z-add     dctlnp        IBElnp
033200081229     c                   z-add     dctnrs        IBEnrs
033300081229     c                   z-add     dctnsp        IBEnsp
033400081229     c                   z-add     dctksc        IBEksc
033500081229     c                   movel     dctptr        IBEptr
033600081229     c                   movel     dctflo        IBEflo
033700081229     c                   movel     'S'           IBEtpb
033800081229     c                   If        %subst(knsif:7:1) = 'P'
033900081229     c                   eval      IBEsif = 'P'
034000081229     c                   EndIf
034100081229      *
034200081229     c                   call      'FIDNBER'
034300081229     c                   parm                    fidnbeds
034400081229      *
034500081229     c                   If        oberag <> *blanks
034600081229      *
034700081229     c                   movel     oberag        PICrag
034800081229     c                   movel     obecap        PICcap
034900081229     c                   movel     obevia        PICind
035000081229     c                   movel     obeloc        PICloc
035100081229     c                   if        obenaz  = *blanks
035200081229     c                   movel     obeprv        PICpro
035300081229     c                   else
035400081229     c                   movel     obenaz        wnaz
035500081229     c                   exsr      ctrnaz
035600081229     c                   movel     §15des        PICnaz
035700081229     c                   endif
035800081229     c                   movel     obeksc        Kcli
035900081229     c                   endif
036000081229     c*
036100081229     c* controllo se cliente estero e devo stampare in inglese
036200081229     c*
036300081229      * Imposto se devo stampare in Inglese
036400081229     c                   clear                   ESTERO
036500081229     c                   clear                   wclsflo7
036600081229     c                   IF        obeing <> *blanks
036700081229     c                   eval      wclsflo7 = obeing
036800081229     c                   ENDIF
036900081229     c                   eval      estero = wclsflo7
037000081229     c*
037100980917      *
037200981123      * Intestatario
037300990310      *-------------
037400050520      * verifico se inviarlo via e-mail oppure no anche in base al flag mailno
037500041119     c                   move      kcli          w0040
037600041119     c                   If        kcli <> *zeros and
037700041119     c                             w0040 <> 8888 and w0040 <> 9999
037800081229     c                             and OBEinv = 'M'
037900041119     c                   clear                   trul44ds
038000041119     c                   eval      D44pgm  = 'FIDN28R'
038100041119     c                   eval      D44Ksc  = Kcli
038200041119     c                   eval      D44cl   = '006'
038300041119     c                   eval      d44apl = 'C'
038400041119     c                   eval      d44nk1 = '0151' + %editc(Kcli:'X')
038500041119     c                   eval      d44tnt = '87'
038600041119     c                   eval      D44ovr  = ' '
038700041119     c                   eval      D44scope= '*CALLLVL'
038800041119     c                   eval      D44prtf = 'FIDN28P'
038900041119
039000041119     c                   IF        Wclsflo7 <> *blanks
039100041123     C                   eval      D44obj =  '*OBJM*Documents needed for Anoma-
039200041123     C                             ly information N. ' +  %char(dctfil)+  '/' +
039300041119     C                             %trim(%editc(dctnca:'Z')) +
039400041119     c                             ' Year '   +   %char(dctaac)
039500041119     c                   ELSE
039600041119     C                   eval      D44obj = '*OBJM*Sollecito invio Documenti -
039700041119     C                             COMUNICAZIONE ANOMALIA N. ' +
039800041119     C                             %char(dctfil)+  '/'      +
039900041119     C                             %trim(%editc(dctnca:'Z')) +
040000041119     c                             ' Anno '  +  %char(dctaac)
040100041119     c                   ENDIF
040200041119     c                   call      'TRUL44R'
040300041119     c                   parm                    trul44ds
040400050902     c                   parm                    trtcm1ds
040500041119      *
040600041119      * se c'è stato qualche problema faccio invio in altro modo
040700041119      *
040800041119     c                   if        d44err  <> ' '
040900041122     c   05              eval      invio  = 'P'
041000041122     c  n05              eval      invio  = 'L'
041100041119     c                   setoff                                       20
041200041119     c                   ELSE
041300041119     c                   seton                                        20
041400041119      * ovrprtf
041500041125     c                   clear                   svcom
041600041125     c                   movea     cmd(6)        svcom
041700041125     c                   eval      %subst(svcom:40:10) = d44outq
041800041125     c                   eval      commanl = %trim(SVCOM)+ d44dta  + ''')'
041900041119     c                   eval      lungo = %len(%trim(commanl))
042000041119     C                   CALL      'QCMDEXC'                            60
042100041119     C                   PARM                    COMMANl         500
042200041119     C                   PARM                    LUNGO            15 5
042300041119      * se c'è errore
042400041119     c                   if        *in60
042500041122     c   05              eval      invio = 'P'
042600041122     c  n05              eval      invio  = 'L'
042700041119     c                   setoff                                       20
042800041119     c                   else
042900041119     c                   eval      invio = 'M'
043000041119     c                   endif
043100041119      *
043200041119
043300041119     c                   endif
043400041119
043500041119     c                   endif
043600041119      *
043700041119     c
043800981123     c* recupero all'attenzione del sig........
043900981123      *
044000981123      *
044100981123     C                   movel(P)  'C'           Ktpd
044200981123     C                   movel(P)  Wnumca        Knkt
044300981123     C                   movel(P)  'N'           Kstd
044400981123     C                   clear                   Khim
044500981123     C                   clear                   Kdim
044600981123     C                   clear                   Knks
044700981123     C                   movel     'D'           Ktrc
044800981123      *
044900010409     C                   CLEAR                   PICPEC
045000010515     c                   eval      *IN06 = *Off
045100010515     c
045200010409      *
045300981123     c     KDCS          CHAIN     FNDCS000                           06
045400981123      *
045500981123     c                   IF        *IN06 = *OFF and  DCSnot <> *BLANKS
045600981123     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
045700981123     c                   eval      *IN06 = *ON
045800981123     c                   ENDIF
045900981123      *
046000981123      * Località + data
046100981123     C     *eur          movel     DATA_oggi     wdataoggi
046200981123     c                   eval      PICdal = 'Bologna, ' + wdataoggi
046300081229
046400081229      * filiale del cliente
046500081229     c                   movel     kcli          w0030
046600081229      *
046700981124      * identifico la società di appartenenza del cliente
046800981124     c     w0030         chain     azorg01l                           31
046900981124     c                   movel     kbar          wsocie
047000981124     c                   movel     §TLDstp1      W§TLDstp
047100100709      *
047200100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
047300100709     c                   if        *in05  and  Not *in20
047400100709     c                   eval      TxtInOut   = ds_DN28PI1
047500100709     c                   exsr      sr_xChkChar
047600100709     c                   if        esito = '1'
047700100709     c                   eval      ds_DN28PI1 = TxtInOut
047800100709     c                   endif
047900100709     c                   endif
048000100709      *
048100981124      * Apro file stampa e imposto Tipo Modulo e Stampante
048200981124      *
048300981124     c                   EXSR      IMPPRT
048400981124      *
048500981123      * Scrivo l'intestazione
048600041119     c  n20              WRITE     DN28PP
048700041119     c  n20              WRITE     DN28PI1
048800041119     c   20              WRITE     DN28MP
048900041119     c   20              WRITE     DN28MI1
049000981123      *
049100981123     C                   ENDSR
049200990520      *****************************************************************
049300990520      *   PREAPARO I DATI DELLA TESTATA DELLA LETTERA  AL P.O. APERTURA C.A.
049400990520      *****************************************************************
049500990520     C     SUB_PO        BEGSR
049600990520      *
049700990520      *
049800990520      * Località + data
049900990520     C     *eur          movel     DATA_oggi     wdataoggi
050000990520     c                   eval      PICdal = 'Bologna, ' + wdataoggi
050100990520      * Intestatario
050200990520      *-------------
050300990520      *
050400990520     c     DCTFIL        chain     azorg01l                           31
050500990520     c*
050600990520     c                   movel     orgdes        PICrag
050700990520     c                   movel     orgind        PICind
050800990520     c                   movel     orgcpf        PICcap
050900990520     c                   movel     orgloc        PICloc
051000990520     c                   movel     orgpro        PICpro
051100990521     c                   movel     kbar          wsocie
051200990521     c                   movel     §TLDstp1      W§TLDstp
051300100709      *
051400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
051500100709     c                   if        *in05  and  Not *in20
051600100709     c                   eval      TxtInOut   = ds_DN28PI1
051700100709     c                   exsr      sr_xChkChar
051800100709     c                   if        esito = '1'
051900100709     c                   eval      ds_DN28PI1 = TxtInOut
052000100709     c                   endif
052100100709     c                   endif
052200100709      *
052300990520      * Apro file stampa e imposto Tipo Modulo e Stampante
052400990520      *
052500990520     c                   EXSR      IMPPRT
052600990520      *
052700990520      * Scrivo l'intestazione
052800041119     c  n20              WRITE     DN28PI1
052900041119     c   20              WRITE     DN28MI1
053000990520      *
053100990520     C                   ENDSR
053200981123      *****************************************************************
053300981123      *   OGGETTO
053400981123      *****************************************************************
053500981123     C     SUB_ogge      BEGSR
053600981123
053700010515      *
053800010515      *  pulizia Oggetto
053900010515     c                   CLEAR                   DN28PO1
054000010515      *
054100010515      *  pulizia Dati Anomalia
054200010515     c                   CLEAR                   DN28PO2
054300010515      * Pulizia riferimenti
054400010515     c                   CLEAR                   DN28PO3
054500010608      *
054600010608      * se i00tpb (tipo bolla ) c'è il valore "F" significa che è un sollecito alla filiale
054700010608     c                   if        i00tpb = 'F'
054800010608     c                   eval      *in90 = *on
054900010608     c                   endif
055000020402
055100020402      * se non è un sollecito alla filiale controllo se il sollecito è da scrivere in italiano
055200020402      * o in inglese
055300020402     c                   eval      *in08 = *off
055400020402     c                   if        not *in90 and wclsflo7 <> *blanks
055500020402     c                   eval      *in08 = *on
055600020402     c                   endif
055700010608      *
055800010515      ***
055900981123     c                   movel     I00aac        wanno
056000981123     c                   movel     I00fil        wfil
056100981123     c                   movel     I00nca        w007a
056200981123     c     '0'           check     w007a         yy
056300981123     c                   eval      wnum = %subst(W007a:yy)
056400990505      *
056500990505     c                   movel     DCTpra        wannop
056600990505     c                   movel     DCTprn        w007a
056700990505     c     '0'           check     w007a         yy
056800990505     c                   eval      wnump = %subst(W007a:yy)
056900990505      *
057000990520     c                   if        *in08 = *off
057100990520      *
057200120417     C                   eval      POCogg  =   ' N. '
057300990505     C                                       +  DCTdit  +  ' '  +  %trim(wnump)
057400990505     C                                       +  ' Anno '  +  wannop
057500990505     C                                       +  '   COMUNICAZIONE ANOMALIA N. '
057600990505     c                                       +  wfil  +  '/'   +  %trim(wnum)
057700990505     C                                       +  ' Anno '  +  wanno
057800990520      *
057900990520     c                   else
058000990520      * INGLESE
058100120418     C                   eval      POCog1  =   ' N.  '
058200990520     C                                       +  DCTdit  +  ' '  +  %trim(wnump)
058300990520     C                                       +  ' Year '  +  wannop
058400990520     C                                       +  '   DAMAGE COMUNICATION N. '
058500990520     c                                       +  wfil  +  '/'   +  %trim(wnum)
058600990520     C                                       +  ' Year '  +  wanno
058700990520     c                   endif
058800990505      *
058900981123     c                   z-add     i00lnp        POClnp
059000981123     c                   z-add     i00nrs        POCnrs
059100981123     c                   z-add     i00nsp        POCnsp
059200981123     c                   z-add     i00aas        POCdat
059300981123     c                   movel     tasmgs        POCdat
059400981123     c     *mdy          move      POCdat        DATA_eur
059500981123     c     *dmy          move      DATA_eur      POCdat
059600981123     c                   movel     tmdrsm        POCrsm
059700981123     c                   z-add     tasrmn        POCrmn
059800981123     c                   movel     tmdrsd        POCrsd
059900981123     c                   movel     tmdlod        POClod
060000990118     c                   if        TMDnzd <> *blanks
060100990118     c                   movel     TMDnzd        POCprn
060200990118     c                   else
060300990118     c                   movel     TMDprd        POCprn
060400990118     c                   endif
060500981123     c                   z-add     tasncl        POCncl
060600990809     c                   z-add     taspkf        POCpkf
060700990520      *
060800990520      * Evento
060900010515     c                   eval      *IN03 = *OFF
061000990520     c                   IF        DCTnev > *zeros
061100990520     c                   eval      *IN03 = *ON
061200990520     c                   movel     DCTaae        wanno
061300990520     c                   movel     DCTnev        w007a
061400990520     c     '0'           check     w007a         yy
061500990520     c                   eval      wnum = %subst(W007a:yy)
061600990520     C                   eval      POCkev  =  wannop  +  '/'  +  %trim(wnum)
061700990520     c     Kdet          chain     FNDET000                           31
061800990520     c                   clear                   tibs02ds
061900990520     c                   clear                   dtad
062000990520      *
062100990520     C                   movel     'C'           t02mod
062200990520     C                   movel     knsif         t02sif
062300990520     C                   movel     'TAD'         t02cod
062400990520     C                   movel(P)  dettad        t02ke1
062500990520      *
062600990520     C                   CALL      'TIBS02R'
062700990520     C                   PARM                    KPJBA
062800990520     C                   PARM                    TIBS02DS
062900990520      *
063000990520     C                   movel     T02uni        DTAD
063100990520     c*
063200990520     c                   SELECT
063300990521     c                   WHEN      *in08
063400990520     c                                                  and  §TADdesi <> *blanks
063500990520     C                   movel     §TADdesi      POCtae
063600990520     c                   WHEN        §TADdest <> *blanks
063700990520     C                   movel     §TADdest      POCtae
063800990520     c                   OTHER
063900990520     C                   movel     §TADdesc      POCtae
064000990520     c                   ENDSL
064100990520     c                   ENDIF
064200981123      * Decodifico tipo anomalia
064300981123     c                   clear                   tibs02ds
064400981123     c                   clear                   dtad
064500981123      *
064600981123     C                   movel     'C'           t02mod
064700981123     C                   movel     knsif         t02sif
064800981123     C                   movel     'TAD'         t02cod
064900981123     C                   movel(P)  I00tad        t02ke1
065000981123      *
065100981123     C                   CALL      'TIBS02R'
065200981123     C                   PARM                    KPJBA
065300981123     C                   PARM                    TIBS02DS
065400981123      *
065500981123     C                   movel     T02uni        DTAD
065600990520     c                   SELECT
065700990521     c                   WHEN      *in08
065800990520     c                                                  and  §TADdesi <> *blanks
065900990520     C                   movel     §TADdesi      POCtad
066000990520     c                   WHEN        §TADdest <> *blanks
066100990520     C                   movel     §TADdest      POCtad
066200990520     c                   OTHER
066300990520     C                   movel     §TADdesc      POCtad
066400990520     c                   ENDSL
066500981123      * Descrizione anomalia
066600981123     c                   exsr      DESTAD
066700990125      * Quantità danneggiate
066800990125      *      Peso
066900990521     c                   IF        DCTpkd > *zeros
067000990125     c                   move(P)   DCTpkd        w007a
067100990125     c     '0'           check     w007a         xx
067200990125     c                   eval      W007b = %subst(w007a:xx)
067300990125     c     ' '           checkr    w007b         xx
067400990125     c                   if        xx = 1
067500990125     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
067600990125     c                   else
067700990125     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(xx-1))
067800990125     c                                      +  ','  +  %subst(w007b:XX:1)
067900990125     c                   endif
068000990125     c                   ENDIF
068100990125      *      Colli
068200990521     c                   IF        DCTncn > *zeros
068300990125     c                   move      DCTncn        w005a
068400990125     c     '0'           check     w005a         xx
068500990521     c                   IF        *in08
068600990521     c                   eval      Wcoman = ' Parcels '  +  %subst(W005a:XX)
068700990521     c                   ELSE
068800990125     c                   eval      Wcoman = ' Colli '  +  %subst(W005a:XX)
068900990521     c                   ENDIF
069000990125     c                   ENDIF
069100990125      *      Pezzi
069200990125     c                   IF        DCTnpz > *zeros
069300990125     c                   move      DCTnpz        w005a
069400990125     c     '0'           check     w005a         xx
069500990521     c                   IF        *in08
069600990521     c                   eval      Wpzman = ' Pieces '  +  %subst(W005a:XX)
069700990521     c                   ELSE
069800990125     c                   eval      Wpzman = ' Pezzi '  +  %subst(W005a:XX)
069900990521     c                   ENDIF
070000990125     c                   ENDIF
070100990125      *
070200990125     c                   eval      POCqtd  =  %trimr(Wkgman)  +
070300990125     c                                        %trimr(Wcoman)  +  %trimr(Wpzman)
070400990125      *
070500990125      * Descrizione pezzi danneggiati
070600990125     c                   IF        DCTnpz > *zeros
070700990125     c                   EXSR      DESPZD
070800990524     c                   ENDIF
070900100709      *
071000100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
071100100709     c                   if        *in05  and  Not *in20
071200100709     c                   eval      TxtInOut   = ds_DN28PO1
071300100709     c                   exsr      sr_xChkChar
071400100709     c                   if        esito = '1'
071500100709     c                   eval      ds_DN28PO1 = TxtInOut
071600100709     c                   endif
071700100709     c                   eval      TxtInOut   = ds_DN28PO2
071800100709     c                   exsr      sr_xChkChar
071900100709     c                   if        esito = '1'
072000100709     c                   eval      ds_DN28PO2 = TxtInOut
072100100709     c                   endif
072200100709     c                   endif
072300010409      *
072400010409      * controllo il salto pagina
072500010409     c                   EXSR      PRT_TESTAT
072600981123      *
072700981123      *  Oggetto
072800041119     c  n20              WRITE     DN28PO1
072900041119     c   20              WRITE     DN28MO1
073000010409      *
073100010409      * controllo il salto pagina
073200010409     c                   EXSR      PRT_TESTAT
073300981123      *  Dati Anomalia
073400041119     c  n20              WRITE     DN28PO2
073500041119     c   20              WRITE     DN28MO2
073600990524     c** Se esiste già una richiesta di risarcimento o nota di addebito visualizzo i dati
073700990524     c     kdct          chain     fndcl01l                           32
073800990524     c**
073900990524     c                   if        not *in32 and dclndc <> 0
074000990524     c                   seton                                        10
074100990524     c                   z-add     dclndc        pocndc
074200990803     c                   IF        dclddc > *zeros
074300990803     c     *iso          move      DCLddc        DATA_eur
074400990524     c     *eur          move      DATA_eur      POCddc
074500100709      *
074600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
074700100709      * => NO: il rec. DN28PO3 contiene solo campi NUMERICI!
074800100709     c*//                if        *in05  and  Not *in20
074900100709     c*//                eval      TxtInOut   = ds_DN28PO3
075000100709     c*//                exsr      sr_xChkChar
075100100709     c*//                if        esito = '1'
075200100709     c*//                eval      ds_DN28PO3 = TxtInOut
075300100709     c*//                endif
075400100709     c*//                endif
075500100709      *
075600010409      * controllo il salto pagina
075700010409     c                   EXSR      PRT_TESTAT
075800010409      *
075900041119     c  n20              WRITE     DN28PO3
076000041119     c   20              WRITE     DN28MO3
076100990803     c                   ENDIF
076200990803     c                   endif
076300981123
076400981123     C                   ENDSR
076500981123      *****************************************************************
076600981123      *  REPERIMENTO DESCRIZIONE ANOMALIA
076700981123      *****************************************************************
076800981123     C     DESTAD        BEGSR
076900981123      *
077000981123     C                   movel(P)  'C'           Ktpd
077100981123     C                   movel(P)  Wnumca        Knkt
077200981123     C                   movel(P)  'A'           Kstd
077300981123     C                   clear                   Knks
077400981123     C                   movel     'D'           Ktrc
077500981123      *
077600981123     c                   clear                   yy
077700981123     c                   clear                   W35
077800981123      *
077900981123     c     kdcs          chain     fndcs000                           30
078000981123      *
078100981123     c     *in30         DOWeq     *off
078200981123     c     yy            ANDle     3
078300981123     c                   add       1             yy
078400981123     c                   movel     dcsnot        W35(yy)
078500981123     c     kdcs          reade     fndcs000                               30
078600981123     c                   ENDDO
078700981123      *
078800990521     c                   eval      POCdan = W35(1)
078900981123      *
079000981123     C                   ENDSR
079100990125      *****************************************************************
079200990125      *  REPERIMENTO DESCRIZIONE PEZZI DANNEGGIATI
079300990125      *****************************************************************
079400990125     C     DESPZD        BEGSR
079500990125      *
079600990125      * Descrizione pezzi mancanti
079700990125     C                   movel(P)  'C'           Ktpd
079800990125     C                   movel(P)  Wnumca        Knkt
079900990125     C                   movel(P)  'P'           Kstd
080000990125     C                   clear                   Knks
080100990125     C                   movel     'D'           Ktrc
080200990125      *
080300990125     c                   clear                   yy
080400990125     c                   clear                   W35
080500990125      *
080600990125     c     kdcs          chain     fndcs000                           30
080700990125      *
080800990125     c     *in30         DOWeq     *off
080900990125     c     yy            ANDle     3
081000990125     c                   add       1             yy
081100990125     c                   movel     dcsnot        W35(yy)
081200990125     c     kdcs          reade     fndcs000                               30
081300990125     c                   ENDDO
081400990125      *
081500990125     c                   eval      POCdpz =  W35(1)
081600990125      *
081700990125     c                   ENDSR
081800981123      *****************************************************************
081900981123      *   TESTO
082000981123      *****************************************************************
082100981123     C     SUB_testo     BEGSR
082200981123
082300010409      * controllo il salto pagina
082400010409     c                   EXSR      PRT_TESTAT
082500981123      *  Lascio 2 righe dall'oggetto
082600041119     C  n20              WRITE     DN28P00
082700041119     C  n20              WRITE     DN28P00
082800041119     C   20              WRITE     DN28m00
082900041119     C   20              WRITE     DN28m00
083000990521
083100990521      *
083200990125      *  sollecito non ho mai ricevuto nessun documento
083300990521    1c                   if        i00fca = wfaso0
083400990521     c  n08              movel(P)  so0(1)        PTCde1
083500990521     c   08              movel(P)  so0(6)        PTCde1
083600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
083700100709     c                   if        *in05  and  Not *in20
083800100709     c                   eval      TxtInOut   = ds_DN28PT1
083900100709     c                   exsr      sr_xChkChar
084000100709     c                   if        esito = '1'
084100100709     c                   eval      ds_DN28PT1 = TxtInOut
084200100709     c                   endif
084300100709     c                   endif
084400010409      * controllo il salto pagina
084500010409     c                   EXSR      PRT_TESTAT
084600010409      *
084700041119     C  n20              WRITE     DN28P00                                      vuota
084800041119     C  n20              WRITE     DN28PT1
084900041119     C   20              WRITE     DN28m00                                      vuota
085000041119     C   20              WRITE     DN28mT1
085100990125      *
085200990521     c  n08              movel(P)  so0(2)        PTCde1
085300990521     c   08              movel(P)  so0(7)        PTCde1
085400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
085500100709     c                   if        *in05  and  Not *in20
085600100709     c                   eval      TxtInOut   = ds_DN28PT1
085700100709     c                   exsr      sr_xChkChar
085800100709     c                   if        esito = '1'
085900100709     c                   eval      ds_DN28PT1 = TxtInOut
086000100709     c                   endif
086100100709     c                   endif
086200010409      * controllo il salto pagina
086300010409     c                   EXSR      PRT_TESTAT
086400010409      *
086500041119     C  n20              WRITE     DN28PT1
086600041119     C   20              WRITE     DN28mT1
086700990125      *
086800990521     c  n08              movel(P)  so0(3)        PTCde1
086900990521     c   08              movel(P)  so0(8)        PTCde1
087000100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
087100100709     c                   if        *in05  and  Not *in20
087200100709     c                   eval      TxtInOut   = ds_DN28PT1
087300100709     c                   exsr      sr_xChkChar
087400100709     c                   if        esito = '1'
087500100709     c                   eval      ds_DN28PT1 = TxtInOut
087600100709     c                   endif
087700100709     c                   endif
087800010409      * controllo il salto pagina
087900010409     c                   EXSR      PRT_TESTAT
088000010409      *
088100041119     C  n20              WRITE     DN28PT1
088200041119     C   20              WRITE     DN28mT1
088300120423      *
088400120423     c                   clear                   ptcde1
088500120423     c  n08              movel(P)  so0(4)        PTCde1
088600120423      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
088700120423     c                   if        *in05  and  Not *in20
088800120423     c                   eval      TxtInOut   = ds_DN28PT1
088900120423     c                   exsr      sr_xChkChar
089000120423     c                   if        esito = '1'
089100120423     c                   eval      ds_DN28PT1 = TxtInOut
089200120423     c                   endif
089300120423     c                   endif
089400120423     C  n20              WRITE     DN28PT1
089500120423     C   20              WRITE     DN28mT1
089600990125      *
089700990521   x1c                   else
089800981123      *
089900981123     c                   clear                   PTCde1
090000990521      *  sollecito alla filiale  I00TPB = 'F'
090100990521    2c                   if        i00tpb = 'F'
090200990521
090300990521     c                   movel(P)  so3(1)        PTCde1
090400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
090500100709     c                   if        *in05  and  Not *in20
090600100709     c                   eval      TxtInOut   = ds_DN28PT1
090700100709     c                   exsr      sr_xChkChar
090800100709     c                   if        esito = '1'
090900100709     c                   eval      ds_DN28PT1 = TxtInOut
091000100709     c                   endif
091100100709     c                   endif
091200010409      * controllo il salto pagina
091300010409     c                   EXSR      PRT_TESTAT
091400010409      *
091500041119     C  n20              WRITE     DN28P00                                      vuota
091600041119     C  n20              WRITE     DN28PT1
091700041119     C   20              WRITE     DN28M00                                      vuota
091800041119     C   20              WRITE     DN28MT1
091900990521      *
092000990521     c                   movel(P)  so3(2)        PTCde1
092100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
092200100709     c                   if        *in05  and  Not *in20
092300100709     c                   eval      TxtInOut   = ds_DN28PT1
092400100709     c                   exsr      sr_xChkChar
092500100709     c                   if        esito = '1'
092600100709     c                   eval      ds_DN28PT1 = TxtInOut
092700100709     c                   endif
092800100709     c                   endif
092900010409      * controllo il salto pagina
093000010409     c                   EXSR      PRT_TESTAT
093100010409      *
093200041119     C  n20              WRITE     DN28PT1
093300041119     C   20              WRITE     DN28MT1
093400990521      *
093500990521     c                   movel(P)  so3(3)        PTCde1
093600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
093700100709     c                   if        *in05  and  Not *in20
093800100709     c                   eval      TxtInOut   = ds_DN28PT1
093900100709     c                   exsr      sr_xChkChar
094000100709     c                   if        esito = '1'
094100100709     c                   eval      ds_DN28PT1 = TxtInOut
094200100709     c                   endif
094300100709     c                   endif
094400010409      * controllo il salto pagina
094500010409     c                   EXSR      PRT_TESTAT
094600010409      *
094700041119     C  n20              WRITE     DN28PT1
094800041119     C   20              WRITE     DN28MT1
094900990521      *
095000990521   x2c                   else
095100990521      *
095200981123      *  1° sollecito
095300990521    3c                   if        i00fca = wfaso1
095400990521     c  n08              movel(P)  so1(1)        PTCde1
095500990521     c   08              movel(P)  so1(5)        PTCde1
095600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
095700100709     c                   if        *in05  and  Not *in20
095800100709     c                   eval      TxtInOut   = ds_DN28PT1
095900100709     c                   exsr      sr_xChkChar
096000100709     c                   if        esito = '1'
096100100709     c                   eval      ds_DN28PT1 = TxtInOut
096200100709     c                   endif
096300100709     c                   endif
096400010409      * controllo il salto pagina
096500010409     c                   EXSR      PRT_TESTAT
096600010409      *
096700041119     C  n20              WRITE     DN28P00                                      vuota
096800041119     C  n20              WRITE     DN28PT1
096900041119     C   20              WRITE     DN28M00                                      vuota
097000041119     C   20              WRITE     DN28MT1
097100981123      *
097200990521     c  n08              movel(P)  so1(2)        PTCde1
097300990521     c   08              movel(P)  so1(6)        PTCde1
097400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
097500100709     c                   if        *in05  and  Not *in20
097600100709     c                   eval      TxtInOut   = ds_DN28PT1
097700100709     c                   exsr      sr_xChkChar
097800100709     c                   if        esito = '1'
097900100709     c                   eval      ds_DN28PT1 = TxtInOut
098000100709     c                   endif
098100100709     c                   endif
098200010409      * controllo il salto pagina
098300010409     c                   EXSR      PRT_TESTAT
098400010409      *
098500041122     C  n20              WRITE     DN28PT1
098600041119     C   20              WRITE     DN28MT1
098700981124      *
098800990521    3c                   endif
098900981124      *  2° sollecito
099000990521    3c                   if        i00fca = wfaso2
099100981124     c* data  guarda fidn09r
099200990521     c  n08              movel(P)  so2(1)        PTCde1
099300990521     c   08              movel(P)  so2(6)        PTCde1
099400981124     c* data ultimo sollecito
099500120418     c  n08              eval      %subst(PTCde1:84:8) = gg02 + '/' + mm02 + '/'
099600981126     c                             + aa02
099700120418     c   08              eval      %subst(PTCde1:79:8) = gg02 + '/' + mm02 + '/'
099800990521     c                             + aa02
099900100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
100000100709     c                   if        *in05  and  Not *in20
100100100709     c                   eval      TxtInOut   = ds_DN28PT1
100200100709     c                   exsr      sr_xChkChar
100300100709     c                   if        esito = '1'
100400100709     c                   eval      ds_DN28PT1 = TxtInOut
100500100709     c                   endif
100600100709     c                   endif
100700010409      * controllo il salto pagina
100800010409     c                   EXSR      PRT_TESTAT
100900010409      *
101000041119     C  n20              WRITE     DN28P00                                      vuota
101100041119     C  n20              WRITE     DN28PT1
101200041119     C   20              WRITE     DN28M00                                      vuota
101300041119     C   20              WRITE     DN28MT1
101400981124      *
101500990521     c  n08              movel(P)  so2(2)        PTCde1
101600990521     c   08              movel(P)  so2(7)        PTCde1
101700100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
101800100709     c                   if        *in05  and  Not *in20
101900100709     c                   eval      TxtInOut   = ds_DN28PT1
102000100709     c                   exsr      sr_xChkChar
102100100709     c                   if        esito = '1'
102200100709     c                   eval      ds_DN28PT1 = TxtInOut
102300100709     c                   endif
102400100709     c                   endif
102500010409      * controllo il salto pagina
102600010409     c                   EXSR      PRT_TESTAT
102700010409      *
102800041119     C  N20              WRITE     DN28PT1
102900041119     C   20              WRITE     DN28MT1
103000981124      *
103100990521     c  n08              movel(P)  so2(3)        PTCde1
103200990521     c   08              movel(P)  so2(8)        PTCde1
103300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
103400100709     c                   if        *in05  and  Not *in20
103500100709     c                   eval      TxtInOut   = ds_DN28PT1
103600100709     c                   exsr      sr_xChkChar
103700100709     c                   if        esito = '1'
103800100709     c                   eval      ds_DN28PT1 = TxtInOut
103900100709     c                   endif
104000100709     c                   endif
104100010409      * controllo il salto pagina
104200010409     c                   EXSR      PRT_TESTAT
104300010409      *
104400041119     C  N20              WRITE     DN28PT1
104500041119     C   20              WRITE     DN28MT1
104600981124      *
104700990521    3c                   endif
104800990521    2c                   endif
104900981123      *
105000990308     c* stampo i documenti da sollecitare
105100990521     c* controllo se devo stampare il gruppo A     non stampa alle filiali
105200990521     c                   if        gruppoa = 'Y'   and not *in90
105300010409      * controllo il salto pagina
105400010409     c                   EXSR      PRT_TESTAT
105500010409      *
105600041119     C  N20              WRITE     DN28P00                                      vuota
105700041119     C   20              WRITE     DN28M00                                      vuota
105800990521     c  n08              z-add     1             x
105900990521     c   08              z-add     4             x
106000990521     c                   dow       (x <= 3 and not *in08) or
106100120418     c                             (x <= 5 and *in08)
106200990308     c                   eval      ptcde1 = doa(x)
106300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
106400100709     c                   if        *in05  and  Not *in20
106500100709     c                   eval      TxtInOut   = ds_DN28PT1
106600100709     c                   exsr      sr_xChkChar
106700100709     c                   if        esito = '1'
106800100709     c                   eval      ds_DN28PT1 = TxtInOut
106900100709     c                   endif
107000100709     c                   endif
107100010409      * controllo il salto pagina
107200010409     c                   EXSR      PRT_TESTAT
107300010409      *
107400041119     C  N20              WRITE     DN28PT1
107500041119     C   20              WRITE     DN28MT1
107600990308     c                   add       1             x
107700990308     c                   enddo
107800990308     c                   endif
107900990521     c* controllo se devo stampare il gruppo B     non stampa alle filiali
108000990521     c                   if        gruppob = 'Y'   and not *in90
108100010409      * controllo il salto pagina
108200010409     c                   EXSR      PRT_TESTAT
108300010409      *
108400041119     C  N20              WRITE     DN28P00                                      vuota
108500041119     C   20              WRITE     DN28M00                                      vuota
108600990521     c  n08              z-add     1             x
108700990521     c   08              z-add     4             x
108800990521     c                   dow       (x <= 3 and not *in08) or
108900120418     c                             (x <= 6 and *in08)
109000990308     c                   eval      ptcde1 = dob(x)
109100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
109200100709     c                   if        *in05  and  Not *in20
109300100709     c                   eval      TxtInOut   = ds_DN28PT1
109400100709     c                   exsr      sr_xChkChar
109500100709     c                   if        esito = '1'
109600100709     c                   eval      ds_DN28PT1 = TxtInOut
109700100709     c                   endif
109800100709     c                   endif
109900010409      * controllo il salto pagina
110000010409     c                   EXSR      PRT_TESTAT
110100010409      *
110200041119     C  N20              WRITE     DN28PT1
110300041119     C   20              WRITE     DN28MT1
110400990308     c                   add       1             x
110500990308     c                   enddo
110600990308     c                   endif
110700990521     c* controllo se devo stampare il gruppo C     non stampa alle filiali
110800990521     c                   if        gruppoc = 'Y'   and not *in90
110900010409      * controllo il salto pagina
111000010409     c                   EXSR      PRT_TESTAT
111100010409      *
111200041119     C  N20              WRITE     DN28P00                                      vuota
111300041119     C   20              WRITE     DN28M00                                      vuota
111400990521     c  n08              z-add     1             x
111500080114     c   08              z-add     3             x
111600080114     c                   dow       (x <= 2 and not *in08) or
111700120418     c                             (x <= 4 and *in08)
111800990308     c                   eval      ptcde1 = doc(x)
111900100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
112000100709     c                   if        *in05  and  Not *in20
112100100709     c                   eval      TxtInOut   = ds_DN28PT1
112200100709     c                   exsr      sr_xChkChar
112300100709     c                   if        esito = '1'
112400100709     c                   eval      ds_DN28PT1 = TxtInOut
112500100709     c                   endif
112600100709     c                   endif
112700010409      * controllo il salto pagina
112800010409     c                   EXSR      PRT_TESTAT
112900010409      *
113000041119     C  N20              WRITE     DN28PT1
113100041119     C   20              WRITE     DN28MT1
113200990308     c                   add       1             x
113300990308     c                   enddo
113400990308     c                   endif
113500990308     c
113600990308     c                   z-add     1             z
113700990308     c*
113800990308     c                   dow       z <= e
113900990308     c                   if        cd(z) <> *blank
114000981123      * decodifico il documento
114100981123     c                   clear                   tibs02ds
114200981123     c                   clear                   dcdd
114300981123      *
114400981123     C                   movel     'C'           t02mod
114500981123     C                   movel     knsif         t02sif
114600981123     C                   movel     'CDD'         t02cod
114700990308     C                   movel(P)  cd(z)         t02ke1
114800981123      *
114900990521     C                   CALL      'TIBS02R'
115000981123     C                   PARM                    KPJBA
115100981123     C                   PARM                    TIBS02DS
115200981123      *
115300981123     C                   movel     T02uni        DCDD
115400990521     c* se sto stampando il sollecito alla filiale richiedo solo i documenti che devono
115500990521      * essere richiesti dalla filiale
115600990521     c                   if        (i00tpb = 'F' and §cddsopo = 'S') or
115700990521     c                             (i00tpb <> 'F' and §cddsopo = ' ')
115800990521     c*
115900981123     c                   clear                   ptcde1
116000990521     c*
116100990521     c                   select
116200990521     c* in lingua
116300990521     c                   when      *in08 and §cddengl <> *blanks
116400990521     c                   eval      ptcde1 = '- ' + §CDDengl
116500990521     c                   other
116600981123     c                   eval      ptcde1 = '- ' + §CDDdesc
116700990521     c                   endsl
116800100709      *
116900100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
117000100709     c                   if        *in05  and  Not *in20
117100100709     c                   eval      TxtInOut   = ds_DN28PT1
117200100709     c                   exsr      sr_xChkChar
117300100709     c                   if        esito = '1'
117400100709     c                   eval      ds_DN28PT1 = TxtInOut
117500100709     c                   endif
117600100709     c                   endif
117700990521     c*
117800010409      * controllo il salto pagina
117900010409     c                   EXSR      PRT_TESTAT
118000010409      *
118100041119     C  N20              WRITE     DN28P00                                      vuota
118200041119     C  N20              WRITE     DN28PT1
118300041119     C   20              WRITE     DN28M00                                      vuota
118400041119     C   20              WRITE     DN28MT1
118500981123     c* controllo se questo tipo di documento,richiede un'intestazione
118600981123    3c                   if        §cddpint = 'S'
118700981123     c                   clear                   ptcde1
118800120423     c  n08              eval      ptcde1 = '  intestato a ' +  %trim(wsocie)
118900981123     c                             + ' Via Mattei 42 40138 Bologna'
119000120423     c   08              eval      ptcde1 = '  addressed to ' +  %trim(wsocie)
119100120423     c                             + ' Via Mattei 42 40138 Bologna'
119200990310     c                   seton                                        50
119300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
119400100709     c                   if        *in05  and  Not *in20
119500100709     c                   eval      TxtInOut   = ds_DN28PT1
119600100709     c                   exsr      sr_xChkChar
119700100709     c                   if        esito = '1'
119800100709     c                   eval      ds_DN28PT1 = TxtInOut
119900100709     c                   endif
120000100709     c                   endif
120100010409      * controllo il salto pagina
120200010409     c                   EXSR      PRT_TESTAT
120300010409      *
120400041119     c  N20              WRITE     DN28PT1
120500041119     c   20              WRITE     DN28MT1
120600990310     c                   setoff                                       50
120700981123    3c                   endif
120800990521    3c                   endif
120900981123    2c                   endif
121000981123      *
121100990308     c                   add       1             z
121200981123    1c                   enddo
121300990125      *
121400990125     c                   endif
121500981123      *
121600981123     C                   ENDSR
121700981123      *****************************************************************
121800981123      *   FIRMA
121900981123      *****************************************************************
122000981123     C     SUB_firma     BEGSR
122100990225      *
122200990521      * verifico se esistono delle note legate al testo che sto stampando se non sto scrivendo
122300010411      * alla filiale ed esiste un documento con codice 99
122400010411     c                   if        WNOTET <> *blanks  and i00tPB <> 'F' and
122500010411     c                             si99 = 'S'
122600010409      * controllo il salto pagina
122700010409     c                   EXSR      PRT_TESTAT
122800010409      *
122900041119     C  N20              WRITE     DN28P00
123000041119     C   20              WRITE     DN28M00
123100990610     c* stampo le due o cinque righe di note
123200990225     c* 1° riga
123300990225     c                   clear                   PTCde1
123400120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
123500120516     c                   move      w100a         ptcde1
123600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
123700100709     c                   if        *in05  and  Not *in20
123800100709     c                   eval      TxtInOut   = ds_DN28PT1
123900100709     c                   exsr      sr_xChkChar
124000100709     c                   if        esito = '1'
124100100709     c                   eval      ds_DN28PT1 = TxtInOut
124200100709     c                   endif
124300100709     c                   endif
124400010409      * controllo il salto pagina
124500010409     c                   EXSR      PRT_TESTAT
124600010409      *
124700041119     c  N20              write     DN28PT1
124800041119     c   20              write     DN28MT1
124900990225     c* 2° riga
125000990225     c                   if        w100b <> *blanks
125100990225     c                   clear                   PTCde1
125200120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
125300120516     c                   move      w100b         ptcde1
125400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
125500100709     c                   if        *in05  and  Not *in20
125600100709     c                   eval      TxtInOut   = ds_DN28PT1
125700100709     c                   exsr      sr_xChkChar
125800100709     c                   if        esito = '1'
125900100709     c                   eval      ds_DN28PT1 = TxtInOut
126000100709     c                   endif
126100100709     c                   endif
126200010409      * controllo il salto pagina
126300010409     c                   EXSR      PRT_TESTAT
126400010409      *
126500041119     c  N20              write     DN28PT1
126600041119     c   20              write     DN28MT1
126700990225     c                   endif
126800990610     c* 3° riga
126900990610     c                   if        w100c <> *blanks
127000990610     c                   clear                   PTCde1
127100120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
127200120516     c                   move      w100c         ptcde1
127300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
127400100709     c                   if        *in05  and  Not *in20
127500100709     c                   eval      TxtInOut   = ds_DN28PT1
127600100709     c                   exsr      sr_xChkChar
127700100709     c                   if        esito = '1'
127800100709     c                   eval      ds_DN28PT1 = TxtInOut
127900100709     c                   endif
128000100709     c                   endif
128100010409      * controllo il salto pagina
128200010409     c                   EXSR      PRT_TESTAT
128300010409      *
128400041119     c  N20              write     DN28PT1
128500041119     c   20              write     DN28MT1
128600990610     c                   endif
128700990610     c* 4° riga
128800990610     c                   if        w100d <> *blanks
128900990610     c                   clear                   PTCde1
129000120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
129100120516     c                   move      w100d         ptcde1
129200100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
129300100709     c                   if        *in05  and  Not *in20
129400100709     c                   eval      TxtInOut   = ds_DN28PT1
129500100709     c                   exsr      sr_xChkChar
129600100709     c                   if        esito = '1'
129700100709     c                   eval      ds_DN28PT1 = TxtInOut
129800100709     c                   endif
129900100709     c                   endif
130000010409      * controllo il salto pagina
130100010409     c                   EXSR      PRT_TESTAT
130200010409      *
130300041119     c  N20              write     DN28PT1
130400041119     c   20              write     DN28MT1
130500990610     c                   endif
130600990610     c* 5° riga
130700990610     c                   if        w100e <> *blanks
130800990610     c                   clear                   PTCde1
130900120516       // -?la "MOVE" è voluta, in questo modo la nota viene allineata bene in stampa
131000120516     c                   move      w100e         ptcde1
131100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
131200100709     c                   if        *in05  and  Not *in20
131300100709     c                   eval      TxtInOut   = ds_DN28PT1
131400100709     c                   exsr      sr_xChkChar
131500100709     c                   if        esito = '1'
131600100709     c                   eval      ds_DN28PT1 = TxtInOut
131700100709     c                   endif
131800100709     c                   endif
131900010409      * controllo il salto pagina
132000010409     c                   EXSR      PRT_TESTAT
132100010409      *
132200041119     c  N20              write     DN28PT1
132300041119     c   20              write     DN28MT1
132400990610     c                   endif
132500990225     c*
132600990225     c                   endif
132700981123      *
132800981123      *  Lascio 2 righe dal testo
132900010409      * controllo il salto pagina
133000010409     c                   EXSR      PRT_TESTAT
133100010409      *
133200041119     C  N20              WRITE     DN28P00
133300041119     C  N20              WRITE     DN28P00
133400041119     C   20              WRITE     DN28M00
133500041119     C   20              WRITE     DN28M00
133600981123
133700981123      *
133800981123     c                   clear                   PTCde1
133900120417
134000120417     c                   select
134100120418      * filiale
134200120418     c                   when      i00tpb = 'F'
134300120418     c                   clear                   ptcde1
134400120417      *  sollecitone
134500120417     c                   when      i00fca = wfaso0
134600120423     c                   clear                   ptcde1
134700120417      *  1° sollecito
134800120417     c                   when      i00fca = wfaso1
134900120417     c  n08              movel(P)  so1(3)        PTCde1
135000120418     c   08              movel(P)  so1(7)        PTCde1
135100120417     c*
135200120417      *  2° sollecito
135300120417     c                   when      i00fca = wfaso2
135400120417     c  n08              movel(P)  so2(4)        PTCde1
135500120418     c   08              movel(P)  so2(9)        PTCde1
135600120417      *
135700120417     c                   endsl
135800120417      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
135900120417     c                   if        *in05  and  Not *in20
136000120417     c                   eval      TxtInOut   = ds_DN28PT1
136100120417     c                   exsr      sr_xChkChar
136200120417     c                   if        esito = '1'
136300120417     c                   eval      ds_DN28PT1 = TxtInOut
136400120417     c                   endif
136500120417     c                   endif
136600120417     c*
136700120417      * controllo il salto pagina
136800120417     c                   EXSR      PRT_TESTAT
136900120417      *
137000120417     C  N20              WRITE     DN28PT1
137100120417     C  N20              WRITE     DN28P00                                      vuota
137200120417     C   20              WRITE     DN28MT1
137300120417     C   20              WRITE     DN28M00                                      vuota
137400990521      *
137500990521     c                   select
137600990521      * filiale
137700990521     c                   when      i00tpb = 'F'
137800120418     c                   movel(P)  so3(4)        PTCde1
137900990225      *  sollecitone
138000990521     c                   when      i00fca = wfaso0
138100990521     c  n08              movel(P)  so0(5)        PTCde1
138200990521     c   08              movel(P)  so0(10)       PTCde1
138300981123      *  1° sollecito
138400990521     c                   when      i00fca = wfaso1
138500990521     c  n08              movel(P)  so1(4)        PTCde1
138600990521     c   08              movel(P)  so1(8)        PTCde1
138700990208     c*
138800990225      *  2° sollecito
138900990521     c                   when      i00fca = wfaso2
139000990611     c  n08              movel(P)  so2(5)        PTCde1
139100990521     c   08              movel(P)  so2(10)       PTCde1
139200990521      *
139300990521     c                   endsl
139400100709      *
139500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
139600100709     c                   if        *in05  and  Not *in20
139700100709     c                   eval      TxtInOut   = ds_DN28PT1
139800100709     c                   exsr      sr_xChkChar
139900100709     c                   if        esito = '1'
140000100709     c                   eval      ds_DN28PT1 = TxtInOut
140100100709     c                   endif
140200100709     c                   endif
140300990208     c*
140400010409      * controllo il salto pagina
140500010409     c                   EXSR      PRT_TESTAT
140600010409      *
140700041119     C  N20              WRITE     DN28PT1
140800041119     C  N20              WRITE     DN28P00                                      vuota
140900041119     C   20              WRITE     DN28MT1
141000041119     C   20              WRITE     DN28M00                                      vuota
141100981123      *
141200981123     c                   movel     Wsocie        PFDsoc
141300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
141400100709     c                   if        *in05  and  Not *in20
141500100709     c                   eval      TxtInOut   = ds_DN28PT1
141600100709     c                   exsr      sr_xChkChar
141700100709     c                   if        esito = '1'
141800100709     c                   eval      ds_DN28PT1 = TxtInOut
141900100709     c                   endif
142000100709     c                   endif
142100010409      * controllo il salto pagina
142200010409     c                   EXSR      PRT_TESTAT
142300010409      *
142400010409      *  1 Spazio
142500041119     C  N20              WRITE     DN28P00
142600041119     C  N20              WRITE     DN28PF1
142700041119     C   20              WRITE     DN28M00
142800041119     C   20              WRITE     DN28MF1
142900981123      *
143000981123     C                   ENDSR
143100981123      *****************************************************************
143200981123      *   SCRIVO STORICO FASI
143300981123      *****************************************************************
143400981123     C     SUB_AGG       BEGSR
143500981123      *
143600981123     c                   clear                   fndcf000
143700981123      *
143800981123     c                   z-add     i00aac        DCFaac
143900981123     c                   z-add     i00fil        DCFfil
144000981123     c                   z-add     i00nca        DCFnca
144100990521     c*  se stampa alla filiale imposto 'F' nel ptr
144200990521     c                   if        i00tpb = 'F'
144300990521     c                   movel     'F'           dcfptr
144400990521     c                   else
144500981123     c                   movel     DCTptr        DCFptr
144600990521     c                   endif
144700990521     c*
144800981123     c                   z-add     i00fca        DCFfca
144900981123     c     *ISO          movel     DATA_oggi     DCFdfc
145000981123     c                   movel     w0140         DCFhfc
145100981123     c                   z-add     i00fgs        DCFfev
145200981123     c                   movel     knmus         DCFpru
145300981123     c                   clear                   DCFftr
145400981123     c                   clear                   DCFdtr
145500020129
145600020129      * Imposto il flag di pratica assicurativa in Franchigia
145700020129     c                   eval      §dcfpfra = §dctpfra
145800041119      * IMPOSTO  INVIO LETTERA
145900041119     c                   eval      §dcfinvi = invio
146000041119      *
146100020129     c                   movel     ddcf01        dcflet
146200981123      *
146300981123     c                   write     fndcf000
146400981123      *
146500981124      * Ricerca fase successiva
146600981124     c                   CLEAR                   FIDN05DS
146700981124     c                   movel     'F'           I05MOD
146800981124     c                   z-add     I00fca        I05fca
146900981124     c                   movel     'P'           I05fpr
147000981124     c                   movel     dctptr        I05tpc
147100981124     c                   movel     'O'           I05ffs
147200981124     c                   movel     ' '           I05fpe
147300981124     c                   z-add     DATEU         i05dta
147400981124     c                   move      dcttad        i05tad
147500981125      * Valorizzo numero CA
147600981125     c                   z-add     DCTaac        i05aac
147700981125     c                   z-add     DCTfil        i05fil
147800981125     c                   z-add     DCTnca        i05nca
147900981124
148000981124     C                   CALL      'FIDN05R'
148100981124     c                   PARM                    KPJBA
148200981124     c                   PARM                    FIDN05DS
148300981124      *
148400981124     c                   movel     O05REC        DS_FNDFA
148500981124      *
148600981124     c                   SELECT
148700981124     c                   WHEN      DFAgfs = 'P'
148800981230     c                   z-add     WDCTlnp       dctgfc
148900981124     c                   WHEN      DFAgfs = 'S'
149000981124     c                   z-add     46            dctgfc
149100981124     c                   WHEN      DFAgfs = 'A'
149200981124     c                   z-add     DCTlna        dctgfc
149300981124     c                   OTHER
149400981124     c                   z-add     DCTfil        dctgfc
149500981124     c                   ENDSL
149600981124      *
149700981124     c* aggiorno  testata CA
149800981124     c* ricerca del punto operativo abilitato alla gestione della fase successiva
149900981124     c                   movel     i00fca        dctfca
150000981222     c                   clear                   dctft1
150100981222     c                   clear                   dctft2
150200981124     c                   update    fndct000
150300981124      *
150400981124      *
150500981123     C                   ENDSR
150600990225      *****************************************************************
150700990225      *   CARICO LE NOTE TESTO RELATIVE A QUESTO SOLLECITO
150800990225      *****************************************************************
150900990225     C     SUB_NOTET     BEGSR
151000990225      *
151100990225     c                   Clear                   wnotet
151200990225      *
151300990225     C                   movel(P)  'C'           Ktpd
151400990225     C                   movel(P)  Wnumca        Knkt
151500990225     C                   movel(P)  'T'           Kstd
151600990225     C                   clear                   Knks
151700990225     c                   movel     dfale1        knks
151800990225     C                   movel     'D'           Ktrc
151900990225      *
152000990225     c                   clear                   yy
152100010409     c                   clear                   des
152200990225      *
152300990225     c     kdcs          chain     fndcs000                           30
152400010406      *
152500010406      * se 30 acceso e sto facendo il 2° sollecito recupero le note relative
152600010406      * al 1° sollecito
152700010406      *
152800010406     c                   if        *in30 and i00fca = wfaso2
152900010406     c                   movel     Wtlso1        knks
153000010406     c     kdcs          chain     fndcs000                           30
153100010406     c                   endif
153200010406      *
153300990225      *
153400990225     c     *in30         DOWeq     *off
153500990610     c     yy            ANDle     15
153600990225     c                   add       1             yy
153700990225     c                   movel     dcsnot        DES(yy)
153800990225     c     kdcs          reade     fndcs000                               30
153900990225     c                   ENDDO
154000990225      *
154100990225     C                   ENDSR
154200981124      *****************************************************************
154300981124      *   IMPOSTO TIPO MODULO E STAMPANTE
154400981124      *****************************************************************
154500981124     C     IMPPRT        BEGSR
154600981124      *
154700981124     c                   Clear                   COMMAND
154800981124     c                   Clear                   Lung
154900981124      *
155000981124     c                   SELECT
155100010409      * se postel
155200010409     c                   WHEN      *in05
155300010418      * solo se è la prima volta che richiamo il PGM ovrprtf file di stampa
155400010418      * dopo visto che chiudo con RT dovrebbe essere ancora aperto
155500010418     c                   if        prima = ' ' and *in05
155600010418     c                   movea     CMD(5)        COMMAND
155700010418     c                   z-add     76            LUNG
155800010418      *
155900010418     c                   eval      prima = 'Y'
156000010418     c                   endif
156100981124      *
156200981124     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp <> *blanks
156300981124     c                   movea     CMD(1)        COMMAND
156400981124     c                   eval      %subst(COMMAND:39:10) = W§TLDstp
156500981124     c                   eval      %subst(COMMAND:60:10) = §TLDtmod
156600001227     c                   eval      %subst(COMMAND:67:3) = 'BAR'
156700990603     c                   z-add     81            LUNG
156800981124      *
156900981124     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp = *blanks
157000981124     c                   movea     CMD(2)        COMMAND
157100981124     c                   eval      %subst(COMMAND:43:10) = §TLDtmod
157200001227     c                   eval      %subst(COMMAND:50:3) = 'BAR'
157300990603     c                   z-add     75            LUNG
157400981124      *
157500981124     c                   WHEN      §TLDtmod = *blanks  and  W§TLDstp <> *blanks
157600981124     c                   movea     CMD(3)        COMMAND
157700981124     c                   eval      %subst(COMMAND:39:10) = W§TLDstp
157800990603     c                   z-add     60            LUNG
157900981124      *
158000981124     c                   OTHER
158100981124     c                   movea     CMD(4)        COMMAND
158200990603     c                   z-add     43            LUNG
158300981124      *
158400981124     c                   ENDSL
158500981124      *
158600981124      * Se ho impostato qualcosa eseguo l'OVRPRTF
158700981124     c                   IF        LUNG > *ZEROS
158800981124     c                   call      'QCMDEXC'
158900981124     c                   parm                    COMMAND
159000981124     c                   parm                    LUNG
159100981124      *
159200981124      *  Apro file di stampa
159300981124     c                   open      FIDN28P
159400010409     c                   ENDIF
159500041122      * SE INVIO MAIL APRO IL FILE FIDN28PM
159600041122     c                   if        *in20
159700041122     c                   open      fidn28pm
159800041122     c                   endif
159900981124      *
160000981124     C                   ENDSR
160100010409      *****************************************************************
160200010409      *   STAMPO TESTATA
160300010409      *****************************************************************
160400010409     C     PRT_TESTAT    BEGSR
160500010409      *
160600010409     c                   if         *IN01 = *ON
160700010409     c                   WRITE     DN28PP
160800010409     c                   EVAL      *in01 = *off
160900010409     c                   endif
161000010409      *
161100041119     c                   if         *IN21 = *ON  AND *IN20
161200041119     c                   WRITE     DN28MP
161300041119     c                   EVAL      *in21 = *off
161400041119     c                   endif
161500010409      *
161600010409     c                   endsr
161700010406      *****************************************************************
161800990308      *   PREPARO LA SCHIERA DEI DATI DA STAMPARE
161900990308      *****************************************************************
162000990308     C     SUB_DOCUM     BEGSR
162100990308      *
162200010409      *
162300010409     c                   clear                   e
162400010409     c                   clear                   cd
162500010410     c                   clear                   gruppoa
162600010410     c                   clear                   gruppob
162700010410     c                   clear                   gruppoc
162800010411     c                   clear                   si99
162900010409      *
163000990308     c* recupero i documenti che mancano
163100990308     c     kdct          setll     fndcc01l
163200990308     c     kdct          reade     fndcc01l                               31
163300990308    1c                   dow       *in31 = *off
163400990308     c* controllo data ricezione
163500990715    2c                   if        dccdrz = *zeros and dccatb = ' '
163600990308     c                   add       1             e
163700990308     c                   movea     dcccdd        cd(e)
163800010411     c* se c'è il codice 99 lo memorizzo
163900010411     c*
164000010411     c                   if        dcccdd = '99'
164100010411     c                   eval      si99 = 'S'
164200010411     c                   endif
164300010411      *
164400990308    2c                   endif
164500990308      *
164600990308     c     kdct          reade     fndcc01l                               31
164700990308    1c                   enddo
164800990308     c*
164900990308     c* per ogni elemento controllo se fa parte di un gruppo
165000990308     c*
165100990308     c                   z-add     1             z
165200990308     c                   dow       z <= e
165300990308     c*
165400990308     c     cd(z)         lookup    cda                                    33
165500990308     c* se trovato nella schiera dei documenti di serie A controllo se devo richiedere
165600990308     c* tutti i documenti del gruppo A
165700990308     c                   if        *in33
165800990309     c                   movel     'Y'           gruppoa
165900990308     c*
166000990308     c                   z-add     1             i
166100990308     c                   dow       i <= 3
166200990308     c     cda(i)        lookup    cd                                     34
166300990308     c* basta che non venga trovato uno dei 3 documenti del gruppo A che vengono stampati
166400990308     c* i singoli documenti del gruppo
166500990308     c                   if        *in34 = *off
166600990309     c                   movel     ' '           gruppoa
166700990308     c                   endif
166800990308     c*
166900990308     c                   add       1             i
167000990308     c                   enddo
167100990308     c* se il flag gruppoa è uguale a 'Y' metto al posto dei codici cd blank
167200990308     c                   if        gruppoa = 'Y'
167300990308     c                   z-add     1             i
167400990308     c                   dow       i <= 3
167500990308     c                   z-add     1             y
167600990308     c     cda(i)        lookup    cd(Y)                                  34
167700990308     c* basta che non venga trovato uno dei 3 documenti del gruppo A che vengono stampati
167800990308     c* i singoli documenti del gruppo
167900990308     c*
168000990308     c   34              movel     '  '          cd(Y)
168100990308     c*
168200990308     c                   add       1             i
168300990308     c                   enddo
168400990308     c                   endif
168500990308     c                   endif                                                  * gruppo A
168600990308     c*
168700990308     c     cd(z)         lookup    cdb                                    33
168800990308     c* se trovato nella schiera dei documenti di serie B controllo se devo richiedere
168900990308     c* tutti i documenti del gruppo B
169000990308     c                   if        *in33
169100990309     c                   movel     'Y'           gruppob
169200990308     c*
169300990308     c                   z-add     1             i
169400990308     c                   dow       i <= 4
169500990308     c     cdb(i)        lookup    cd                                     34
169600990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
169700990308     c* i singoli documenti del gruppo
169800990308     c                   if        *in34 = *off
169900990309     c                   movel     ' '           gruppob
170000990308     c                   endif
170100990308     c*
170200990308     c                   add       1             i
170300990308     c                   enddo
170400990308     c* se il flag gruppoa è uguale a 'Y' metto al posto dei codici cd blank
170500990308     c                   if        gruppob = 'Y'
170600990308     c                   z-add     1             i
170700990308     c                   dow       i <= 4
170800990308     c                   z-add     1             y
170900990308     c     cdb(i)        lookup    cd(Y)                                  34
171000990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
171100990308     c* i singoli documenti del gruppo
171200990308     c*
171300990308     c   34              movel     '  '          cd(Y)
171400990308     c*
171500990308     c                   add       1             i
171600990308     c                   enddo
171700990308     c                   endif
171800990308     c                   endif                                                  * gruppo B
171900990308     c*
172000990308     c     cd(z)         lookup    cdc                                    33
172100990308     c* se trovato nella schiera dei documenti di serie C controllo se devo richiedere
172200990308     c* tutti i documenti del gruppo C
172300990308     c                   if        *in33
172400990309     c                   movel     'Y'           gruppoc
172500990308     c*
172600990308     c                   z-add     1             i
172700990308     c                   dow       i <= 1
172800990308     c     cdc(i)        lookup    cd                                     34
172900990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
173000990308     c* i singoli documenti del gruppo
173100990308     c                   if        *in34 = *off
173200990309     c                   movel     ' '           gruppoc
173300990308     c                   endif
173400990308     c*
173500990308     c                   add       1             i
173600990308     c                   enddo
173700990308     c* se il flag gruppoa è uguale a 'Y' metto al posto dei codici cd blank
173800990308     c                   if        gruppoc = 'Y'
173900990308     c                   z-add     1             i
174000990308     c                   dow       i <= 1
174100990308     c                   z-add     1             y
174200990308     c     cdc(i)        lookup    cd(Y)                                  34
174300990308     c* basta che non venga trovato uno dei 4 documenti del gruppo B che vengono stampati
174400990308     c* i singoli documenti del gruppo
174500990308     c*
174600990308     c   34              movel     '  '          cd(Y)
174700990308     c*
174800990308     c                   add       1             i
174900990308     c                   enddo
175000990308     c                   endif
175100990308     c                   endif                                                  * gruppo B
175200990308     c*
175300990308     c                   add       1             z
175400990308     c*
175500990308     c                   enddo
175600990308     c*
175700990308     c                   endsr
175800010411      *****************************************************************
175900010411      *   VERIFICO IL CONTENUTO DELLA SCHIERA DEI DOCUMENTI
176000010411      *****************************************************************
176100010411     C     SUB_VERIFICA  BEGSR
176200010411      *
176300010411     c                   z-add     1             z
176400010411     c                   z-add     0             f
176500010411     c                   clear                   docfil
176600010411     c                   clear                   doccli
176700010411     c                   clear                   cdf
176800010411      *
176900010411      * verifico ogni documento che leggo se è per la filiale
177000010411      *
177100010411     c                   dow       z <= e
177200010411     c                   if        cd(z) <> *blank
177300010411      * decodifico il documento
177400010411     c                   clear                   tibs02ds
177500010411     c                   clear                   dcdd
177600010411      *
177700010411     C                   movel     'C'           t02mod
177800010411     C                   movel     knsif         t02sif
177900010411     C                   movel     'CDD'         t02cod
178000010411     C                   movel(P)  cd(z)         t02ke1
178100010411      *
178200010411     C                   CALL      'TIBS02R'
178300010411     C                   PARM                    KPJBA
178400010411     C                   PARM                    TIBS02DS
178500010411      *
178600010411     C                   movel     T02uni        DCDD
178700010411      * se documento da sollecitare alla filiale  lo conteggio nel gruppoo filiale
178800010411      * altrimenti nel gruppo cliente
178900010411     c                   if        §cddsopo = 'S'
179000010411     c                   add       1             docfil
179100010411     c                   movea     cd(z)         cdf(docfil)
179200010411     c                   else
179300010411     c                   add       1             doccli
179400010411     c                   endif
179500010411      *
179600010411     c                   endif
179700010411      *
179800010411     c                   add       1             z
179900010411    1c                   enddo
180000010411      *
180100010411      * se il numero dei documenti da sollecitare ai clienti è uguale a zero
180200010411      * e non devo stampare nè il gruppoa nè il gruppob nè il gruppoc
180300010411      * e quelli alla filiale è diverso da 0 stampo solo una riga di sollecito
180400010411      * alle filiali
180500010411      *
180600010411     c                   if        doccli = 0  and gruppoa= ' ' and gruppob=' '
180700010411     c                             and gruppoc= ' '
180800010411     c                   eval      stampa = 'N'
180900010411     c                   endif
181000010411      *
181100010411     c                   if        docfil > 0
181200010411      * stampo  alla filiale
181300010411     c     dctfil        chain     azorg01l
181400010411     c                   if        not %found(azorg01l) or orgfva <> ' '
181500010411     c                   clear                   orgdes
181600010411     c                   endif
181700010411      * per ogni documento stampo una riga
181800010411     c                   z-add     1             f
181900010411     c                   dow       f <= docfil
182000010411     c                   if        cdf(f) <> *blanks
182100010411      * decodifico il documento
182200010411     c                   clear                   tibs02ds
182300010411     c                   clear                   dcdd
182400010411      *
182500010411     C                   movel     'C'           t02mod
182600010411     C                   movel     knsif         t02sif
182700010411     C                   movel     'CDD'         t02cod
182800010411     C                   movel(P)  cdf(f)        t02ke1
182900010411      *
183000010411     C                   CALL      'TIBS02R'
183100010411     C                   PARM                    KPJBA
183200010411     C                   PARM                    TIBS02DS
183300010411      *
183400010411     C                   movel     T02uni        DCDD
183500010411      *
183600010411      * verifico se devo stampare la testata
183700010411     c                   if        not *in99 or *inof
183800010411     c                   except    testa
183900010411     c                   seton                                        99
184000010411     c                   setoff                                       of
184100010411     c                   endif
184200010411      *
184300010411     c                   except    filiale
184400010420      * Posta elettronica alle filiali
184500130411      * dal 11 Aprile 2013 non facciamo più la posta elettronica ma solo
184600130411      * la scritta delle note relative al sollecito alla filiale
184700010420     c                   exsr      crtposta
184800010420      *
184900010411     c                   endif
185000010411      *
185100010411     c                   add       1             f
185200010411      *
185300010411     c                   enddo
185400010411      *
185500010411     c                   endif
185600010411      *
185700010411     c                   ENDSR
185800010420      *----------------------------------------------------------------
185900010420      * CREAZIONE POSTA ELETTRONICA ALLE FILIALI
186000010420      *----------------------------------------------------------------
186100010420     C     CRTPOSTA      BEGSR
186200010420      *
186300010420     c*
186400010420      * scrittura nelle note dei solleciti alle filiali
186500010420     c*
186600010420     c                   eval      dcstrc='N'
186700010420     c                   eval      dcspno=0
186800010420     c*
186900010420     c* scrittura note preparazione dei campi strandard
187000010420     c                   eval      dcsatb=' '
187100010420     c                   eval      dcstpd='C'
187200010420     c                   eval      dcsnkt=Wnumca
187300010420     c                   eval      dcsstd=' '
187400010420     c                   movel     i00fca        dcsnks
187500010420     c     *ISO          movel     DATA_oggi     DCSdim
187600010420     c                   movel     w0140         DCShim
187700010420     c*
187800010420     c* cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
187900010420     c     keydcs        setgt     fndcs01l
188000010420     c     keydcsp       readpe    fndcs01l                               93
188100010420     c*
188200010420     c                   move      knmus         dcspru
188300010420     c                   movel     i00fgs        dcspos
188400010420     c                   clear                   dcsstn
188500010420     c                   clear                   dcsft1
188600010420     c                   clear                   dcsdt1
188700010420     c* scrivo la prima riga
188800010420     c                   add       1             dcspno
188900010420     c                   movel(p)  NOTE(1)       dcsnot
189000010420     c                   move      dctfil        dcsnot
189100010420     c                   write     fndcs000
189200010420     c*
189300010420     c* scrivo la seconda riga
189400010420     c                   add       1             dcspno
189500010420     c                   movel(p)  NOTE(2)       dcsnot
189600010420     c                   write     fndcs000
189700010420     c*
189800010420     c* scrivo la riga documento
189900010420     c                   add       1             dcspno
190000010420     c                   movel(p)  §cdddesc      dcsnot
190100010420     c                   write     fndcs000
190200010420     c*
190300010420     c* scrivo la riga documento
190400010420     c                   eval      dcsnot = %subst(§cdddesc:36:25)
190500010420     c*
190600010420     c                   if        dcsnot <> *blanks
190700010420     c                   add       1             dcspno
190800010420     c                   write     fndcs000
190900010420     c                   endif
191000010420     c*
191100010420     c                   endsr
191200010409      *****************************************************************
191300010409      * DECODIFICO NAZIONE
191400010409      *****************************************************************
191500010409     C     CTRNAZ        BEGSR
191600010409      *
191700010409     C                   clear                   DS15
191800010409     C                   movel     '15'          §COD
191900010409     C                   movel(P)  WNAZ          §KEY
192000010409     C     KTAB          CHAIN     TABEL00f                           30
192100010409      *
192200010409     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
192300010409     C                   movel     TBLUNI        DS15
192400010409     C                   endif
192500010409      *
192600010409     C                   ENDSR
192700980506      *****************************************************************
192800980506      *   ROUTINE INIZIALE
192900980506      *****************************************************************
193000010409     C     INIZIO        BEGSR
193100980506      *
193200980506     C     *ENTRY        PLIST
193300980506     C                   PARM                    KPJBA
193400980730      *
193500981120     C                   MOVEL     KPJBU         FIDN00DS
193600010409     c                   z-add     *zeros        page
193700011002     c                   EVAL      *in01 = *off
193800990310      *
193900981124      * recupero il codice del testo da stampare
194000981124     c                   CLEAR                   FIDN05DS
194100981124     c                   movel     'C'           I05MOD
194200981124     c                   z-add     I00fca        I05fca
194300981124
194400981124     C                   CALL      'FIDN05R'
194500981124     c                   PARM                    KPJBA
194600981124     c                   PARM                    FIDN05DS
194700981124      *
194800981124     c                   movel     O05REC        DS_FNDFA
194900981124      *
195000981124     c* aggancio tabella documenti
195100981124      *
195200981124     c                   clear                   tibs02ds
195300981124     c                   clear                   dtld
195400981124      *
195500981124     C                   movel     'C'           t02mod
195600981124     C                   movel     knsif         t02sif
195700981124     C                   movel     'TLD'         t02cod
195800981124     C                   movel(P)  Dfale1        t02ke1
195900981124      *
196000981124     C                   CALL      'TIBS02R'
196100981124     C                   PARM                    KPJBA
196200981124     C                   PARM                    TIBS02DS
196300981124      *
196400981124     C                   movel     T02uni        DTLD
196500990225      *
196600990225      * se previte note testo le recupero
196700990225     c                   if        §tldnote <> 'N'
196800990225      *
196900990225     c                   exsr      SUB_NOTET
197000990225      *
197100990225     c                   endif
197200981124      * recupero la data ultimo sollecito
197300981124     c* se 2° sollecito
197400981124     c                   if        i00fca = wfaso2
197500981124     c     kdct          setgt     fndcf01l
197600981124     c     kdct          readpe(n) fndcf01l                               31
197700981124     c                   if        not *in31
197800981124     c                   movel     dcfdfc        data08
197900981124     c                   endif
198000981124     c                   endif
198100990520      *
198200990520      * se i00tpb (tipo bolla ) c'è il valore "F" significa che è un sollecito alla filiale
198300990521     c                   if        i00tpb = 'F'
198400990520     c                   eval      *in90 = *on
198500990520     c                   endif
198600010406      *
198700010406      * se 2° sollecito o sollecitone stampo per postel
198800010406      *
198900010406     c                   if        i00fca = wfaso2 or i00fca = wfaso0
199000010406     c                   seton                                        05
199100010406     c                   endif
199200981124      *
199300980729     C                   ENDSR
199400100709      /free
199500100709       //--------------------------------------------------------------
199600100709       //?"Bonifica" campi in stampa da caratteri indesiderati         ?
199700100709       //--------------------------------------------------------------
199800100709       BEGSR  sr_xChkChar;
199900100709
200000100709         ChkNull = '1';
200100100709
200200100709         xChkChar ( TxtInOut :
200300100709                    ElencoChar :
200400100709                    TipoElenco :
200500100709                    CharSost :
200600100709                    Uppercase :
200700100709                    ChkNull :
200800100709                    CharNull :
200900100709                    Esito);
201000100709
201100100709       ENDSR;
201200100709      /end-free
201300010411      *****************************************************************
201400010411      *   ROUTINE INIZIALE  SOLO LA PRIMA VOLTA
201500010411      *****************************************************************
201600010411     C     *INZSR        BEGSR
201700010411      *
201800010411      *
201900010411     C                   Z-ADD     1             CODUT
202000010411     C                   CALL      'X§PARUT'
202100010411     C                   PARM                    UT§DSE0F
202200010411     C                   MOVEL     RAGUT         RSUT
202300010411     C                   MOVEL     REC80         CNCR80
202400010411
202500010411      * DATA
202600010411     C                   TIME                    W0140
202700010411     C                   MOVE      W0140         WDTGIO
202800010411     C                   MOVEL     W0140         WORA
202900010411      *
203000010411     C                   Z-ADD     WDTGIO        G02DAT
203100010411     C                   MOVEL     *BLANK        G02ERR
203200010411     C                   CALL      'XSRDA8'
203300010411     C                   PARM                    WLBDAT
203400010411     C                   MOVEL     G02INV        DATEU
203500010411     C     *iso          MOVEL     DATEU         DATA_oggi
203600010411      * accesso testata CA
203700010411     C     kdct          KLIST
203800010411     C                   KFLD                    i00aac
203900010411     C                   KFLD                    i00fil
204000010411     C                   KFLD                    i00nca
204100010411      * accesso archivio bolle
204200010411     c     kbol01        klist
204300010411     c                   kfld                    i00aas
204400010411     c                   kfld                    i00lnp
204500010411     c                   kfld                    i00nrs
204600010411     c                   kfld                    i00nsp
204700010411      * accesso note CA
204800010411     C     KDCS          klist
204900010411     C                   KFLD                    Ktpd
205000010411     C                   KFLD                    Knkt
205100010411     C                   KFLD                    Kstd
205200010411     C                   KFLD                    Kdim
205300010411     C                   KFLD                    Khim
205400010411     C                   KFLD                    Knks
205500010411     C                   KFLD                    Ktrc
205600010420     c*
205700010420     c     keydcs        KLIST
205800010420     c                   KFLD                    dcstpd
205900010420     c                   KFLD                    dcsnkt
206000010420     c                   KFLD                    dcsstd
206100010420     c                   KFLD                    dcsdim
206200010420     c                   KFLD                    dcshim
206300010420     c                   KFLD                    dcsnks
206400010420     c                   KFLD                    dcstrc
206500010420     c                   KFLD                    prgnot
206600010420     c                   eval      prgnot= 999
206700010420     c* di fndcs01l - parziale
206800010420     c*
206900010420     c     keydcsp       KLIST
207000010420     c                   KFLD                    dcstpd
207100010420     c                   KFLD                    dcsnkt
207200010420     c                   KFLD                    dcsstd
207300010420     c                   KFLD                    dcsdim
207400010420     c                   KFLD                    dcshim
207500010420     c                   KFLD                    dcsnks
207600010420     c                   KFLD                    dcstrc
207700010411     C* ACCESSO   TABEL
207800010411     C     KTAB          KLIST
207900010411     C                   KFLD                    CODUT
208000010411     C                   KFLD                    §COD
208100010411     C                   KFLD                    §KEY
208200010411     C* ACCESSO   TABEL
208300010411     C     KTABe         KLIST
208400010411     C                   KFLD                    CODUT
208500010411     C                   KFLD                    §COD
208600010411      *
208700010411     C     Kdet          klist
208800010411     C                   KFLD                    DCTaae
208900010411     C                   KFLD                    DCTnev
209000010411      *
209100010411     c* valorizzo le costanti di stampa TESTA1 e TESTA2
209200010411     c                   movel     tes(1)        tes1
209300010411     c                   movel     tes(3)        tes2
209400010411     c                   movel     tes1          testa1
209500010411     c                   move      tes2          testa1
209600010411     c*
209700010411     c                   movel     tes(2)        tes1
209800010411     c                   movel     tes(4)        tes2
209900010411     c                   movel     tes1          testa2
210000010411     c                   move      tes2          testa2
210100010411      *
210200010411     C                   ENDSR
210300010411     c*
210400010411      *----------------------------------------------------------------
210500010411     O*
210600010411     OPRTF198   E            TESTA             2
210700010411     O                       RSUT              +  0
210800010411     O                                         +  0 '/'
210900010420     O                       i00fgs            +  0
211000010411     O                                           90 'DOCUMENTI ANCORA DA RICEVE'
211100010411     O                                              'RE DAI P.O. PER AVANZAMENT'
211200010411     o                                              'O C.A.'
211300010411     O                                          165 'FIDN28R'
211400010411     O                       WDTGIO        Y    180
211500010411     O                                          194 'PAG.'
211600010411     O                       PAGE1         Z    198
211700010411     O          E            TESTA             3
211800010411     O                       KNSIF             +  0
211900010411     O                       KNMUS             +  1
212000010411     O                       WORA               180 '  :  :  '
212100010411     O          E            TESTA       1
212200010411     O                       testa1
212300010411     O          E            TESTA       1
212400010411     O                       testa2
212500010411     O          E            TESTA       1
212600010411     O                       TRAT               198
212700010418     O          E            FILIALE     2
212800010411     O                       dctaac
212900010411     O                       dctfil           +   1
213000010411     O                       dctnca        Z  +   1
213100010418     O                       dctaas           +   2
213200010411     O                       dctlnp           +   1
213300010411     O                       dctnrs        Z  +   1
213400010411     O                       dctnsp        Z  +   1
213500010411     O                       tasmgs           +   1 '0  /  '
213600010411     O                       dctfil           +   1
213700010411     O                       orgdes           +   1
213800010411     O                       §cdddesc         +   1
213900010418     O                                        +   1 '__________________________'
214000010418     O                                              '__________________________'
214100980914      *----------------------------------------------------------------
214200080114** SO0 -- Allegato 13 Bis Sollecito chi non ha ancora inviato niente ------------------------------*
214300120423         Con riferimento alla  pratica assicurativa in oggetto Vi comunichiamo che non ci sono mai
214400120423pervenuti i  documenti richiesti  Vi  preghiamo di volerceli inviare al piu' presto per consentire
214500120423l'evasione della pratica, ricordandoVi  che  IN  MANCANZA  DI  ESSI IL DANNO  NON  POTRA'  ESSERE
214600120423RISARCITO.
214700120417In attesa di quanto sopra porgiamo cordiali saluti.
214800120423         We refer to the above mentioned insurance file to inform you we never received the
214900120423documents werequested. Please send them us soon to enable us to procede with your file. We remind
215000120423you that NON RECEIVING THEM WE WILL NOT BE IN A POSITION TO PAY THE DAMAGE.
215100120423
215200120423Waiting to as above we hand Yours sincerely.
215300080114** SO1 -- Allegato 12 1° Sollecito ----------------------------------------------------------------*
215400120417         Analizzata la documentazione precedentemente fornitaci, di seguito elenchiamo
215500120417quanto mancante:
215600120417In mancanza di essi, il danno non potrà essere risarcito.
215700120417In attesa di quanto sopra porgiamo cordiali saluti.
215800120418         After checking the documents you sent us previously, here as follows you can find which
215900120418documents are missing:
216000120418If we will not receive them, we will not be able to refund the claim.
216100120418We wait for these documents and in the meantime we remain with our best regards.
216200080114** SO2 -- Allegato 13 2° Sollecito ----------------------------------------------------------------*
216300120418         Analizzata la documentazione fornitaci a seguito del nostro sollecito del xx/xx/xx, di
216400120418seguito elenchiamo quanto mancante:
216500120417
216600120417In mancanza di essi, il danno non potrà essere risarcito.
216700120417In attesa di quanto sopra porgiamo cordiali saluti.
216800120418         Checking the documents you sent us previously, after our reminder of xx/xx/xx here as
216900120418follows you can find which documents are missing:
217000120418
217100120418If we will not receive them, we will not be able to refund the claim.
217200120418We wait for these documents and in the meantime we remain with our best regards.
217300080114** SO3 -- Sollecito alla Filiale ------------------------------------------------------------------*
217400990520         Con riferimento alla pratica assicurativa in oggetto Vi preghiamo di volerci inviare i
217500990520documenti sotto segnalati per consentire l'evasione della pratica.
217600990520
217700990520         In attesa di quanto sopra porgiamo cordiali saluti.
217800080114** CMD - (Lungh. 85) ---------------------------------------------------------------*
217900990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) OUTQ(xxxxxxxxxx) FORMTYPE(xxxxxxxxxx) HOLD(*YES)    L81
218000990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) FORMTYPE(xxxxxxxxxx) SAVE(*YES) HOLD(*YES)          L75
218100990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) OUTQ(xxxxxxxxxx) HOLD(*YES)                         L60
218200990603OVRPRTF FIDN28P SPLFNAME(SOLDOC) HOLD(*YES)                                          L43
218300010409OVRPRTF FIDN28P SPLFNAME(LET198) FORMTYPE(SOLDOC) OUTQ(PICCIONEDB/BARPICSTM)         L76
218400041125OVRPRTF FIDN28PM FORMTYPE(SOLDOC) OUTQ(xxxxxxxxxx) USRDFNDTA('
218500080114** DOC A ------------------------------------------------------------------------------------------*
218600120418- se causale vendita: fotocopia della fattura della merce spedita completa di tutte le pagine
218700120418  se diversa causale: fotocopia del listino di vendita o della fattura d'acquisto di tutta la
218800120418  merce spedita
218900120418- in case of sale: whole photocopy of the original invoice of the sent goods
219000120418  in the other cases: photocopy of the price list or of the purchase invoice of all the goods sent
219100080114** DOC B ------------------------------------------------------------------------------------------*
219200120418- se la merce è stata riparata: fotocopia della fattura di riparazione oppure scheda interna di
219300120418  lavorazione con listino prezzi della parti sostituite
219400120418  se la merce non è riparabile: dichiarazione a riguardo
219500120418- in case the damaged goods are fixed: copy of the invoice only relevant to the fixing or the
219600120418  internal document of the manufacturing with the price list of the replaced parts
219700120418  in case the goods are not repairable: please send us declaration
219800080114** DOC C ------------------------------------------------------------------------------------------*
219900120418- copia della contestazione di danno inviata alla BRT S.p.A. entro i termini di legge (8 gg.
220000120418  dalla consegna o dal reso merce - art. 1698 c.c.) e relativa prova d'invio
220100120418- copy of the damage claim sent to BRT S.p.A. within the prescribed terms (within 8 days from the
220200120418  delivery or the return of goods article 1698 c.c.) and the relevant evidence of its sending
220300990308**  CDA
220400990308A1A2A3
220500990308**  CDB
220600990308B1B2B3B4
220700990308**  CDC
220800990308C1
220900080114** TES  (Lungh. 100) - -Disegno stampa- -----------------------------------------------------------*
221000070515                  S P E D I Z I O N E         RICHIEDERE ALLA           Documento
221100070515Anno P.O.  N°C.A. Anno LNP Ser Numero  Data       Filiale
221200010411
221300010411
221400010411xxxx xxx xxxxxxx  xxxx xxx xx xxxxxxx xx/xx xxx xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxx
221500010418xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
221600080114** NOTE --------------------------*
2217000705152° sollecito alla Filiale
221800010420del documento sotto segnalato
