000100010704
000200010704
000300981125      *--------------------------------------------------------------------------*
000400080212      * Stampa Comunicazione LT a cliente                                        *
000500080212      *                                                                          *
000600981125      *--------------------------------------------------------------------------*
000700981214
000800980731
000900981124      ***************************************************************
001000981023      *  FASI C.A. FISSATE A PROGRAMMA
001100080213      ***************************************************************
001200080213      * 100 = Conferma CA                                   WFCCA
001300081030      * 110 = Cambio Gestione CA                            WFCGCA
001400080212      * 190 = Stampa Comunicazione LT                       WFSCL
001500980731      ***************************************************************
001600980729
001700981124      ***************************************************************
001800980729      *  RIEPILOGO INDICATORI
001900980729      ***************************************************************
002000990224      * 01   - SALTO PAGINA 1o prtf
002100981125      * 02   - NON STAMPO PER POSTEL
002200981119      * 03   - STAMPO Evento
002300990224      * 04   - ALTA INTENSITA riga di testo
002400981203      * 07   - SALTO PAGINA 2o prtf
002500990222      * 08   - TESTO IN INGLESE
002600050303      * 09   - "All'attenzione di..."
002700981214      * 10   - ESEGUO apertura file solo 1° giro
002800021204      * 12   - STAMPO riferimento mittente ALFAnumerico
002900041110      * 20   - Invio Via E-Mail
003000041110      * 21   - Salto pagina prtf mail
003100050303      * 22~27- comodo in ricerca capoconti
003200020717      * 30   - COMODO
003300981124      * 31   - COMODO
003400050303      * 60   - Chiamata a QCMDEXC (ovrprtf) in errore
003500981214      ***************************************************************
003600980729
003700080213     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
003800980729
003900980804     fFNDCT01L  UF   E           K DISK
004000020124     fFNDCS01L  IF a E           K DISK
004100080312     fFNDCL01L  UF a E           K DISK
004200981124     fFNDFA01L  IF   E           K DISK
004300080219     fFNDPT01L  IF   E           K DISK
004400080218     fFNDET01L  IF   E           K DISK
004500981124     fFNDCF01L  UF A E           K DISK
004600990809     fTITAS30C  IF   E           K DISK
004700981120     fTNTMD01L  IF   E           K DISK
004800021204     fTITA430C  IF   E           K DISK
004900980729     fAZORG01L  IF   E           K DISK
005000981124     fTABEL00F  IF   E           K DISK
005100080311     fFIDN66P   O    E             PRINTER oflind(*IN01) infds(PRTBAR) usropn
005200080311     fFIDN66PM  O    E             PRINTER oflind(*IN21) infds(PRTMAI) usropn
005300981204
005400990429     D W35             S             35    DIM(4)                               descr. tipo anomalia
005500981214     D NCA             S              1    DIM(105)                             Comodo per stampa
005600981222     D CAE             S             19    DIM(999)                             CA estero
005700980803      *----------------------------------
005800081030     D CMD             S             90    DIM(3)  CTDATA PERRCD(1)             OVRPRTF
005900050216      *--------------------------------------
006000080526     D TES1            S            100    DIM(27) CTDATA PERRCD(1)             testo lettera 1 riga
006100050216      *--------------------------------------
006200050216
006300050216      *--------------------------------------
006400050216      * posizioni testi schiera TESP
006500060310     D COTP            S              3    DIM(8) CTDATA PERRCD(1)              codici testi partic.
006600060310     D COSP            s             12S 0 DIM(8) ALT(cotp)                     coord. schiera part.
006700050216      * In caso di modifica agli elementi di TESP bisogna
006800050216      * rivedere le sostituzioni effettuate nella sbr  INSTSTPAR
006900060310     D TESP            S            100    DIM(36) CTDATA PERRCD(1)             testi particolari
007000050216      *--------------------------------------
007100050216
007200050216      *--------------------------------------
007300050216      * Schiera testo per mail
007400041118     D TESM            S            100    DIM(4)  CTDATA PERRCD(1)             testo lettera x mail
007500980730      *----------------------------------
007600050216
007700110526     D KBAR            C                   CONST('BRT S.p.A.')
007800980729      *----------------------------------
007900980730     D WLBDAT          DS                  INZ
008000980730     D  g02dat                 1      8  0
008100980730     D  g02inv                 9     16  0
008200980730     D  g02err                17     17
008300980730     D  g02tgi                18     22  0
008400980928      *
008500990302     D                 DS
008600990302     D  WtstITAda              1      3  0
008700990302     D  WtstITAa               4      6  0
008800990302     D  WtstGBda               7      9  0
008900990302     D  WtstGBa               10     12  0
009000990302     D  Wtstdaa                1     12  0
009100990302      *
009200990302     D                 DS
009300990302     D  WrisITAda              1      3  0
009400990302     D  WrisITAa               4      6  0
009500990302     D  WrisGBda               7      9  0
009600990302     D  WrisGBa               10     12  0
009700990302     D  Wrisdaa                1     12  0
009800981026      *
009900080311     D DSNUMCA         DS
010000080311     D  dctaac
010100080311     D  dctfil
010200080311     D  dctnca
010300981123      *
010400981123     d TCUDS           DS
010500981123     d  f1                     1      1
010600981123     d  f3                     3      3
010700981123     d  f2                     2      2
010800981123     d  f4                     4      4
010900981123     d  f56                    5      6
011000990224      *
011100990224     d DS_DESCR        DS
011200990224     d W900A                        900
011300990224     d DES                           35    DIM(20)  overlay(W900A:1)            Descrizione
011400990224     d w035a                         35             overlay(W900A:1)
011500990224     d DES70                         70    dim(11)  overlay(W900A:36)
011600990224      *
011700990224     D PRTBAR          DS
011800990224     D  cur_lineB            367    368I 0
011900990224      *
012000041110     D PRTMAI          DS
012100041110     D  cur_lineM            367    368I 0
012200100709
012300100709       // -?Strutture Dati per la definizione dei campi in stampa?
012400100709       //  ?per la "bonifica" degli stessi da caratteri indesiderati?
012500100709       //  ?da POSTEL?
012600100709     d ds_DN66PI       ds
012700100709     d   PICrag
012800100709     d   PICind
012900100709     d   PICcap
013000100709     d   PICloc
013100100709     d   PICprv
013200100709     d   PICnaz
013300100709     d   PICdal
013400100709     d   PICpec
013500100709     d ds_DN66PO       ds
013600100709     d   POCogg
013700100709     d   POCrsm
013800100709     d   POCrma
013900100709     d   POCrsd
014000100709     d   POClod
014100100709     d   POCprd
014200100709     d   POCkev
014300100709     d   POCtae
014400100709     d   POCtad
014500100709     d   POCdan
014600100709     d ds_DN66PO1      ds
014700100709     d   POCqtd
014800100709     d   POCdpz
014900100709     d ds_DN66PO2      ds
015000100709     d   POCkos
015100100709     d ds_DN66PT       ds
015200100709     d   PTCtes
015300100709
015400981116      *
015500990224     D UT§DSE0F      E DS
015600990224     d  tcu                  398    697     DIM(50)
015700990224     d  kcu                  698    847P 0  DIM(50)  PACKEVEN
015800981026     D CNCR80        E DS
015900090108     D FIDNBEDS      E DS
016000980730     D FIDN05DS      E DS
016100980730     D TIBS02DS      E DS
016200980804     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
016300981204     D DS_TNTMD      E DS                  EXTNAME(TNTMD00F)
016400080215     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F) PREFIX(X_)
016500980507     D KPJBA         E DS
016600081029     D FIDN00DS      E ds
016700980729     D DTAD          E DS
016800981027     D DTLD          E DS
016900010910     D DGEDDN        E DS
017000010413     D DGEI          E DS
017100980918     D DSTB          E DS
017200981203     D DSCV          E DS
017300981230     D DS15          E DS
017400981229     D DDCT01        E DS
017500020124     d ddcf01        e ds
017600990226     D DSTD          E DS
017700041110     D TRUL44DS      E DS                                                       X Invio e-mail
017800050902     D TRTCM1DS      E DS                                                       X Invio e-mail
017900010906     D YEURCODS      E DS                  inz
018000010906     D   YECTAR      E                     inz('H')
018100010907      * Routine editazione di un numero
018200010907     D XEDITDS       E DS                  inz
018300010907     D   OedFlgErr   e                     inz(*off)
018400021118      *
018500021118     d OG143         e ds                  inz
018600021204     d DTA4A         e ds                  inz
018700081029      *
018800081029     d Status         sds
018900081029     d  PARMS            *parms
019000081029      *
019100980729      *----------------------------------
019200080219     d wSpedAss        s              1    inz(*off)
019300080219     D wfndpt          S              1
019400020327     D wclsflo7        S              1
019500990222     D wpzman          S             13
019600990222     D wcoman          S             14
019700981123     D wkgman          S             11
019800980928     D wanno           s              4
019900980730     D wfil            s              3
020000980928     D wnum            s              7
020100980803     D wdataoggi       S             10
020200010328     D wdatastampa     S             10
020300071206     D wdatapreavv     S             10
020400990302     d Wdfvm           s             10
020500080219     D wtiporis        S              3
020600990302     D wtipodoc        S              3
020700990415     D w003a           s              3
020800990415     D w005a           s              5
020900981124     D w007a           s              7
021000981124     D w007b           s              7
021100981124     D w011a           S             11
021200980803     D wsocie          S             20
021300010906     D wcmr            S             16
021400981214     D Wkca            S             19
021500981210     D command         S             90
021600990205     D lineaa          s             12
021700990205     D lineap          s             12
021800981215     D lung            S             15  5 inz(90)
021900980730     D XX              s              3  0
022000980731     D YY              s              3  0
022100980731     D JJ              s              3  0
022200981214     D QQ              s              3  0
022300020606     D WW              s              3  0
022400980730     D dateu           s              8  0
022500980730     D wdtgio          S              8  0
022600080311     D dtaris          S              8  0
022700980730     D wora            S              6  0
022800981203     D nrr1            S              4  0
022900981203     D nrr2            S              4  0
023000980730     D w0140           S             14  0
023100990415     D w0133           S             13  3
023200041118     D w0030           S              3  0
023300980918     D w0040           S              4  0
023400981210     D w0040a          S              4  0
023500981210     D wpointes        S              3  0
023600990302     D wdaxx           S              3  0
023700990302     D waxx            S              3  0
023800990302     D wdayy           S              3  0
023900990302     D wayy            S              3  0
024000990302     D wdaH            S              3  0
024100990302     D waH             S              3  0
024200041123     D wlast           S              2  0
024300041123     D wlblank         S              2  0
024400041123     D wd44mit         S             30
024500041125     D svcom           S             90
024600080311     D prttad          S             12
024700041110
024800041110     D Invio           s              1                                         wrk per invio e-mail
024900041110
025000081029     D Ristampa        s              1                                         wrk per ristampa
025100081030     D Fase_190        s              1                                         Fase 190 ok
025200081029
025300010906      *-------------
025400010906     D WImporto        S             30S 5 inz
025500010906     D WNrDec          S              2S 0 inz
025600010906     D WLungh          S              2S 0 inz
025700010906      *-------------
025800980730      *
025900980730     D DATA_eur        S               D   DATFMT(*eur)
026000980730     D DATA_oggi       S               D   DATFMT(*eur)
026100021127      *
026200990309     D W§dctport       S                   LIKE(§DCTport)
026300031211     d w§STDlpf        s                   like(§STDlpf1) inz(*loval)
026400001228     D Wdtaad          S                   LIKE(DCTaac)                         data stampa A.D.
026500090108     D Kcli            S                   LIKE(DCTksc)
026600980918     D Kcod            S                   LIKE(TBLcod)
026700980929     D Ktpd            S                   LIKE(DCStpd)
026800980929     D Knkt            S                   LIKE(DCSnkt)
026900980929     D Kstd            S                   LIKE(DCSstd)
027000980929     D Kdim            S                   LIKE(DCSdim)
027100980929     D Khim            S                   LIKE(DCShim)
027200980929     D Knks            S                   LIKE(DCSnks)
027300980929     D Ktrc            S                   LIKE(DCStrc)
027400981028     D Kkey            S                   LIKE(TBLkey)
027500981123     D Wtad            S                   LIKE(DCTtad)
027600981230     D wnaz            s                   LIKE(TMDnzm)
027700080526     d WFcca           s                   LIKE(DCTfca)   inz(100)
027800080212     d WFscl           s                   LIKE(DCTfca)   inz(190)
027900081030     d WFcgca          s                   LIKE(DCTfca)   inz(110)
028000020124
028100020125     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
028200100709
028300100709       //--------------------------------------------------------------
028400100709       //?Definizione procedure usate.                                 ?
028500100709       //--------------------------------------------------------------
028600100709
028700100709       // -?Parametri per XCHKCHAR?
028800100709      /copy gaitrasrc/srcProtoPI,XCHKCHAR
028900100709       // -?"Bonifica" stringa da caratteri indesiderati?
029000100709      /copy gaitrasrc/srcProtoPR,XCHKCHAR
029100100709
029200980731      *---------------------------------------------------------------------
029300040427      *
029400080213     c     *ENTRY        PLIST
029500080213     c                   PARM                    KPJBA
029600081029     c                   PARM                    FIDN00DS
029700081029
029800090309      * Aggancio anagrafico fasi per recuperare il tipo testo
029900090309     c     WFscl         CHAIN     FNDFA000                           30
030000090309      * Se non ci sono testi non elaboro
030100090309     c                   IF          *IN30 = *off
030200090309
030300090309      * Aggancio tabella TIPO TESTO
030400090309     c                   EXSR      DECtld
030500090309
030600081029     c                   If        parms = 1
030700080311     c                   movel     kpjbu         dtaris
030800081029      * Stampo C.A. allegate e aggiorno testata C.A. e file fasi
030900081029     c                   exsr      CA_multi
031000081029
031100081029     c                   else
031200081029      * In caso di più parametri si parla di ristampa
031300081029     c                   eval      Ristampa = 'S'
031400081030     c                   exsr      SR_Valorizza
031500081029     c                   exsr      CA_singola
031600081029     c                   endif
031700040427      *
031800090309     c                   endif
031900090309      *
032000981214
032100981203      * Chiudo TIBS02R
032200981203     c                   clear                   tibs02ds
032300981203     c                   movel     'C'           t02tla
032400981203     c                   call      'TIBS02R'
032500981203     c                   parm                    kpjba
032600981203     c                   parm                    tibs02ds
032700981204      *
032800081106     c                   if        Ristampa = 'S' and I00fca  = 998
032900081103     c                   eval      *inRT   = *on
033000081103     c                   else
033100081103     C                   EVAL      *INLR = *ON
033200081103     c                   endif
033300081030      *---------------------------------------------------------------------
033400081030      *  Valorizzo campi per la ristampa
033500081030      *---------------------------------------------------------------------
033600081030     c     Sr_valorizza  BEGSR
033700081030
033800081030     c                   eval      x_dctaas = i00aas
033900081030     c                   eval      x_dctlnp = i00lnp
034000081030     c                   eval      x_dctnrs = i00nrs
034100081030     c                   eval      x_dctnsp = i00nsp
034200081030     c                   eval      x_dctaac = i00aac
034300081030     c                   eval      x_dctfil = i00fil
034400081030     c                   eval      x_dctnca = i00nca
034500090309
034600081030     c                   endsr
034700081030      *
034800981203      *---------------------------------------------------------------------
034900981203      *  ELABORO SUBFILE
035000981203      *---------------------------------------------------------------------
035100981203     c     CA_multi      BEGSR
035200981203      *
035300080213      * Leggo  file
035400080213     C/EXEC SQL
035500080215     C+ DECLARE B1 CURSOR FOR SELECT * FROM fndct01l WHERE
035600080213     C+ dctfpr = 'T' and (dctfca = 100 or dctfca = 110)
035700080215     C+       for read only
035800080213     C/END-EXEC
035900080213
036000080213     C/EXEC SQL
036100080213     C+ OPEN b1
036200080213     C/END-EXEC
036300080213
036400080213     C                   do        *hival
036500080213
036600080213     C/EXEC SQL
036700080215     C+ FETCH NEXT FROM b1 INTO :ds_fndct
036800080213     C/END-EXEC
036900080213
037000080215     c                   select
037100080215     c                   when      sqlcod = 100
037200080213     c                   leave
037300080215     c                   WHEN      SqlCod < 0
037400080213     c                   other
037500080213      * verifico data della fase se entro data limite impostata a video
037600080215     c     knca          setgt     fndcf01l
037700080526     c                   do        *hival
037800080215     c     knca          readpe(n) fndcf01l
037900080527     c                   if        %eof(fndcf01l)
038000080527     c                   leave
038100080527     c                   endif
038200080529     c                   if        x_dctfca = dcffca
038300080529     c                   if        dcfdfc <= dtaris
038400080529      * imposto la data preavviso
038500080529     c     *iso          movel     dcfdfc        data_EUR
038600080529     C     *eur          movel     DATA_EUR      wdatapreavv
038700080529
038800080215     c                   exsr      CA_singola
038900080529     c                   endif
039000080526     c                   leave
039100080215     c                   endif
039200080526     c                   enddo
039300080213     C                   endsl
039400080213
039500080213     C                   enddo
039600080213
039700080213     C/EXEC SQL
039800080213     C+ CLOSE b1
039900080213     C/END-EXEC
040000981203      *
040100981203     c                   ENDSR
040200981203      *---------------------------------------------------------------------
040300981203      *  ELABORO SINGOLA C.A.
040400981203      *---------------------------------------------------------------------
040500981203     c     CA_singola    BEGSR
040600981203      *
040700981203     c                   eval      *in28 = *off
040800080312     c                   eval      *in02 = *off
040900981203     c                   clear                   page
041000080219     c                   clear                   Wtiporis
041100081103     c                   If        Ristampa = 'S'  and i00fca = 999
041200081030     c                   eval      *in02 = *on
041300081030     c                   endif
041400981203      *
041500981203      * Aggancio file bolle e C.A.
041600981203     c                   EXSR      SUB_files
041700980803      *
041800080215     c                   EXSR      SUB_LETTERA
041900981215      * Azzero PRTF
042000981215     c                   EXSR      INZPRTF
042100080215      *  imposto indicatore per invio o no tramite POSTEL
042200080215     c                   eval      *in02  = *on
042300080215      * Aggiorno file
042400081030      * in caso di ristampa non si aggiorna
042500081030     c                   If        Ristampa <> 'S'
042600080215     c                   EXSR      SUB_AGGIOR
042700081030     c                   endif
042800981203      *
042900981203     c                   ENDSR
043000981203      *****************************************************************
043100981203      *   AGGANCIO FILES
043200981203      *****************************************************************
043300981203     C     SUB_files     BEGSR
043400021118      *
043500080219     c                   Reset                   wSpedAss
043600981203      *
043700981203      * Aggancio file bolle
043800990809     c     KBOL          chain     TITAS30C                           30
043900981229      * se tipo porto recupero rileggo
044000981229     C                   movel     'TB'          kCOD
044100981229     C                   movel(P)  TAStbl        kKEY
044200981229     C     ktab          CHAIN     TABEL                              31
044300981229     C                   movel     TBLuni        DSTB
044400981229     c                   if        §TBrbl = 'R'
044500990809     c     KBOL          reade     TITAS30C                               30
044600981229     c                   endif
044700080219if  1c                   If        not *in30
044800080219     c                             and TASias >  0
044900080219     c                             and TASfcm <> 'F'
045000080219     c                   Eval      wSpedAss = *on
045100080219e   1c                   Endif
045200981203      *
045300981203      * Aggancio file mittenti/destinatari
045400981204     c                   clear                   DS_TNTMD
045500981204     c     KBOL          chain     TNTMD000                           30
045600981203      *
045700981203      * Aggancio testata C.A.
045800080215     c     Knca          chain     FNDCT000                           30
045900981229     c                   movel     DCTflo        DDCT01
046000981203      *
046100080219      * Aggancio file mandato assicurativo
046200080311     c     KNCA          chain     FNDPT000                           30
046300080219     c                   if        *in30 = *on  or  DPTatb = 'A'
046400080219     c                   clear                   Wfndpt
046500080219     c                   else
046600080219     c                   eval      Wfndpt = 'S'
046700080219     c                   If        DPTftc = 'C' or  DPTftc = '8' or DPTFTC = 'd'
046800080219     c                   Eval      wSpedAss = *on
046900080219     c                   Endif
047000080219     c                   endif
047100080219      *
047200981203     c                   ENDSR
047300980729      *****************************************************************
047400981203      *   STAMPO LETTERA
047500980729      *****************************************************************
047600080215     C     SUB_LETTERA   BEGSR
047700981203      *
047800001228      *
047900010328     c                   movel     wdataoggi     wdatastampa
048000081029      * in caso di ristampa (singola o totale da EDP)
048100081030     c                   If        ristampa = 'S'
048200081029     c                   exsr      Sub_dtaad                                     dta stampa A.D.LT
048300081029     c                   endif
048400010328      *
048500981203     c                   EXSR      SUB_INTES                                     intestazione
048600981203      *
048700981203     c                   EXSR      SUB_OGGE                                      oggetto
048800981203     c                   EXSR      SUB_TESTO                                     testo
048900981203      *
049000981203     c                   ENDSR
049100081029      *****************************************************************
049200081029      *   RICERCO DATA ULTIMA STAMPA AVVISO DANNO DALLO STORICO FASI
049300081029      *****************************************************************
049400081029     C     SUB_dtaad     BEGSR
049500081029      *
049600081029     c                   clear                   wdtaad
049700081029      *
049800081029     c     kdct          setgt     FNDCF01L
049900081029     c     kdct          readpe    FNDCF01L
050000081029     c*
050100081029     c                   dow       NOT %eof(FNDCF01l)
050200081030     c* se la fase che leggo è uguale alla 190 recupero la data stampa
050300081030     c                   if        dcffca =   Wfscl
050400081029     c                   movel     dcfdfc        wdtaad
050500081029     c     *iso          movel     dcfdfc        data_EUR
050600081029     C     *eur          movel     DATA_EUR      wdatastampa
050700081029     c                   endif
050800081030     c* se la fase che leggo è uguale alla 110 o 100 recupero la data stampa preavviso
050900081030     c                   if        dcffca = wfcca or dcffca = wfcgca
051000081030      * imposto la data preavviso
051100081030     c     *iso          movel     dcfdfc        data_EUR
051200081030     C     *eur          movel     DATA_EUR      wdatapreavv
051300081030     c                   endif
051400081030     c*
051500081029     c     kdct          readpe    fndcf01l
051600081029     c*
051700081029     c                   enddo
051800081029     c*
051900081029     c                   endsr
052000981203      *****************************************************************
052100981214      *   AGGIORNO ARCHIVI E SE HO EVENTO FORTUITO O PF IMPORT CHIUDO LA CA
052200981203      *****************************************************************
052300981203     C     SUB_aggior    BEGSR
052400990429      *
052500981203     c                   EXSR      WRTdcf                                       Dettaglio fase
052600080312     c                   EXSR      WRTdcl                                       Imp.concordato
052700981203     c                   EXSR      WRTdct                                       Testata fase
052800981203      *
052900981203     c                   ENDSR
053000981203      *****************************************************************
053100981203      *   INTESTAZIONE
053200981203      *****************************************************************
053300981203     C     SUB_intes     BEGSR
053400041119      *
053500041119     c                   setoff                                       210107
053600980730      *
053700080218      * DATI dell'intestatario Comunicazione LT
053800980928     C                   exsr      RIEcli
053900981103      *
054000080218      * Interlocutore
054100050303     c                   eval      *in09   =  *off
054200981103     c                   exsr      RECpercon
054300981007      *
054400981007      * Località + data
054500010328     c                   eval      PICdal = 'BOLOGNA, '  +  wdatastampa
054600981120      *
054700981120      *
054800981211     c                   movel(P)  KBAR          Wsocie
054900100709      *
055000100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
055100100709     c                   if        Not *in02   and   Not *in20
055200100709     c                   eval      TxtInOut  = ds_DN66PI
055300100709     c                   exsr      sr_xChkChar
055400100709     c                   if        esito = '1'
055500100709     c                   eval      ds_DN66PI = TxtInOut
055600100709     c                   endif
055700100709     c                   endif
055800981214      *
055900981214      * SOLO PRIMA VOLTA apro file stampa e imposto Tipo Modulo e Stampante
056000981214     c                   if        *in10 = *off
056100981214     c                   EXSR      IMPPRT
056200981214     c                   eval      *in10 = *on
056300981214     c                   endif
056400041117      * se invio via mail apro anche il file di spool per invio mail
056500041117     c                   if        *in20
056600080218     c                   open      fidn66pm
056700041117     c                   endif
056800981113      *
056900980805      * Scrivo l'intestazione
057000080218     c  n20              WRITE     DN66PP
057100080218     c  n20              WRITE     DN66PI
057200080218     c   20              WRITE     DN66MI
057300981120      *
057400980730     C                   ENDSR
057500980731      *****************************************************************
057600980918      *   IMPOSTO DATI CLIENTE PER STAMPA
057700980731      *****************************************************************
057800980918     C     RIEcli        BEGSR
057900981210      *
058000981210     c                   clear                   kcli
058100041117     c                   eval      invio = 'P'
058200041117     c                   setoff                                       20
058300090108      * recupero il beneficiario
058400090108     c                   clear                   fidnbeds
058500090108      *
058600090108     c                   movel     'B'           IBEmod
058700090108     c                   z-add     dctaac        IBEaac
058800090108     c                   z-add     dctfil        IBEfil
058900090108     c                   z-add     dctnca        IBEnca
059000090108     c                   z-add     dctaas        IBEaas
059100090108     c                   z-add     dctlnp        IBElnp
059200090108     c                   z-add     dctnrs        IBEnrs
059300090108     c                   z-add     dctnsp        IBEnsp
059400090108     c                   z-add     dctksc        IBEksc
059500090108     c                   movel     dctptr        IBEptr
059600090108     c                   movel     dctflo        IBEflo
059700090108     c                   movel     'S'           IBEtpb
059800090108     c                   If        %subst(knsif:7:1) = 'P'
059900090108     c                   eval      IBEsif = 'P'
060000090108     c                   EndIf
060100090108      *
060200090108     c                   call      'FIDNBER'
060300090108     c                   parm                    fidnbeds
060400090108      *
060500090108     c                   If        oberag <> *blanks
060600090108      *
060700090108     c                   movel     oberag        PICrag
060800090108     c                   movel     obecap        PICcap
060900090108     c                   movel     obevia        PICind
061000090108     c                   movel     obeloc        PICloc
061100090108     c                   if        obenaz  =  *blanks
061200090108     c                   movel     obeprv        PICprv
061300090108     c                   else
061400090108     c                   movel     obenaz        wnaz
061500090108     c                   exsr      ctrnaz
061600090108     c                   movel     §15des        PICnaz
061700090108     c                   endif
061800090108     c                   movel     obeksc        Kcli
061900090108     c                   endif
062000041117      * Imposto se devo stampare in Inglese
062100041117     c                   clear                   WCLSflo7
062200041117     c                   eval      *in08 = *OFF
062300090108     c                   IF        OBEing <> *blanks
062400090108     c                   eval      wclsflo7 = OBEing
062500041117     c                   eval      *in08 = *on
062600041117     c                   ENDIF
062700080218      * verfico se il cliente al quale devo inviare la lettera ha un indirizzo e-mail
062800041116     c                   If        kcli <> *zeros and not *in02
062900041116     c                   clear                   trul44ds
063000080218     c                   eval      D44pgm  = 'FIDN66R'
063100041116     c                   eval      D44Ksc  = Kcli
063200041117     c                   eval      D44cl   = '006'
063300041117     c                   eval      d44apl = 'C'
063400041118     c                   eval      d44nk1 = '0151' + %editc(Kcli:'X')
063500041117     c                   eval      d44tnt = '87'
063600041116     c                   eval      D44ovr  = ' '
063700041116     c                   eval      D44scope= '*CALLLVL'
063800080218     c                   eval      D44prtf = 'FIDN66P'
063900041116
064000041116     c                   IF        Wclsflo7 <> *blanks
064100071217     C                   eval      D44obj =  '*OBJM*Alleged damage Notice - Ano-
064200041122     C                             maly information n. ' +  %char(dctfil)+  '/'+
064300041116     C                             %trim(%editc(dctnca:'Z')) +
064400041118     c                             ' Year '   +   %char(dctaac)
064500041116     c                   ELSE
064600071207     C                   eval      D44obj = '*OBJM*Avviso Danno COMU-
064700041117     C                             NICAZIONE ANOMALIA N. ' +
064800041116     C                             %char(dctfil)+  '/'      +
064900041116     C                             %trim(%editc(dctnca:'Z')) +
065000041118     c                             ' Anno '  +  %char(dctaac)
065100041116     c                   ENDIF
065200041116     c                   call      'TRUL44R'
065300041116     c                   parm                    trul44ds
065400050902     c                   parm                    trtcm1ds
065500041116      *
065600041116      * se c'è stato qualche problema faccio invio in altro modo
065700041116      *
065800041116     c                   if        d44err  <> ' '
065900041117     c                   eval      invio  = 'P'
066000041117     c                   setoff                                       20
066100041116     c                   ELSE
066200041117     c                   seton                                        20
066300041116      * ovrprtf
066400081030     c                   movea     CMD(3)        svcom
066500081106if  3c                   if        Ristampa = 'S' and I00tpb  = '*'
066600081106     c                             and i00fca = 999
066700081103     c                   eval      %subst(svcom:38:16) = *blanks
066800081103x   3c                   else
066900081103     c                   eval      %subst(svcom:43:10) = d44outq
067000081103e   3c                   endif
067100041125     c                   eval      commanl = %trim(svcom)+ d44dta  + ''')'
067200041116     c                   eval      lung = %len(%trim(commanl))
067300041116     C                   CALL      'QCMDEXC'                            60
067400041116     C                   PARM                    COMMANl         500
067500041116     C                   PARM                    LUNG             15 5
067600041116      * se c'è errore
067700041116     c                   if        *in60
067800041117     c                   eval      invio = 'P'
067900041117     c                   setoff                                       20
068000041117     c                   else
068100041117     c                   eval      invio = 'M'
068200041116     c                   endif
068300041116      *
068400041116
068500041116     c                   endif
068600041117
068700041117     c                   endif
068800980918      *
068900980918     C                   ENDSR
069000981210      *****************************************************************
069100981210      *   RECUPERO IL NOMINATIVO DELL'INTERLOCUTORE
069200981210      *****************************************************************
069300981210     C     RECpercon     BEGSR
069400981210      *
069500980731     c                   CLEAR                   DCSnot
069600981002     c                   CLEAR                   PICpec
069700980731      *
069800980929     C                   movel(P)  'C'           Ktpd
069900981215     C                   movel(P)  DSnumca       Knkt
070000980929     C                   movel(P)  'N'           Kstd
070100980929     C                   clear                   Khim
070200980929     C                   clear                   Kdim
070300980929     C                   clear                   Knks
070400980929     C                   movel     'D'           Ktrc
070500980731      *
070600981119     c     KDCS          CHAIN     FNDCS000                           30
070700981026      *
070800981119     c                   IF        *IN30 = *OFF and  DCSnot <> *BLANKS
070900050303     c                   eval      *in09   =  *on
071000981103     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
071100981026     c                   ENDIF
071200980731      *
071300980731     C                   ENDSR
071400980730      *****************************************************************
071500980730      *   OGGETTO
071600980730      *****************************************************************
071700980730     C     SUB_ogge      BEGSR
071800980928
071900981215     c                   clear                   POCqtd
072000981215     c                   clear                   Wkgman
072100981215     c                   clear                   Wcoman
072200981215     c                   clear                   Wpzman
072300981123      *
072400981123      * Intestazione
072500981120     c                   movel     DCTaac        wanno
072600981120     c                   movel     DCTfil        wfil
072700981120     c                   movel     DCTnca        w007a
072800980803     c     '0'           check     w007a         yy
072900980928     c                   eval      wnum = %subst(W007a:yy)
073000981120      *
073100990120      *
073200020327     c                   IF        Wclsflo7 <> *blanks
073300080312     C                   eval      POCogg  =  'Subject:' +
073400080312     C                                       ' DAMAGE COMUNICATION N. '
073500020717     c                                       + wfil + '/' + %trim(wnum)
073600020717     C                                       + ' Year ' + wanno
073700990222     c                   ELSE
073800080312     C                   eval      POCogg  =  'Oggetto:' +
073900080312     C                                         ' COMUNICAZIONE ANOMALIA N.'
074000020717     c                                       + wfil + '/' + %trim(wnum)
074100020717     C                                       + ' Anno ' + wanno
074200990222     c                   ENDIF
074300980803      *
074400981120     c                   z-add     DCTlnp        POClnp
074500981120     c                   z-add     DCTnrs        POCnrs
074600981120     c                   z-add     DCTnsp        POCnsp
074700981120     c                   z-add     DCTaas        POCdat
074800981120     c                   movel     TAsmgs        POCdat
074900981002     c     *mdy          move      POCdat        DATA_eur
075000981002     c     *dmy          move      DATA_eur      POCdat
075100981120     c                   movel     TMDrsm        POCrsm
075200981120     c                   z-add     TASrmn        POCrmn
075300021204      * il riferimento mittente alfanum. per la sede si trova in TITA4
075400021204     c     ktas4         chain     TITA430C
075500021204     c                   if        %found(TITA430C)
075600021204     c                   movel     TA4not        DTA4A
075700021204     c                   else
075800021204     c                   clear                   DTA4A
075900021204     c                   endif
076000021204     c                   movel     §TA4Arma      POCrma
076100021204     c     §TA4Arma      comp      *blanks                            1212
076200981120     c                   movel     TMDrsd        POCrsd
076300981120     c                   movel     TMDlod        POClod
076400990118     c                   if        TMDnzd <> *blanks
076500990118     c                   movel     TMDnzd        POCprd
076600990118     c                   else
076700990118     c                   movel     TMDprd        POCprd
076800990118     c                   endif
076900981120     c                   z-add     TASncl        POCncl
077000990809     c                   z-add     TASpkf        POCpkf
077100981123      *
077200981123      * Evento
077300981123     c                   IF        DCTnev > *zeros
077400981123     c                   eval      *IN03 = *ON
077500981123     c                   movel     DCTaae        wanno
077600981123     c                   movel     DCTnev        w007a
077700981123     c     '0'           check     w007a         yy
077800981123     c                   eval      wnum = %subst(W007a:yy)
077900080311     C                   eval      POCkev = wanno + '/' + %trim(wnum)
078000080311     c     Kdet          chain     FNDET000                           31
078100981123     C                   movel     DETtad        Wtad
078200981123     c                   exsr      CHTAD
078300990302     c                   SELECT
078400020327     c                   WHEN        Wclsflo7 <> *blanks
078500020717     c                                      and  §TADdesi <> *blanks
078600990302     C                   movel     §TADdesi      POCtae
078700990302     c                   WHEN        §TADdest <> *blanks
078800990302     C                   movel     §TADdest      POCtae
078900990302     c                   OTHER
079000990302     C                   movel     §TADdesc      POCtae
079100990302     c                   ENDSL
079200981123     c                   ENDIF
079300981123      *
079400980729      * Decodifico tipo anomalia
079500981123     C                   movel     DCTtad        Wtad
079600980729     c                   exsr      CHTAD
079700990302     c                   SELECT
079800020327     c                   WHEN        Wclsflo7 <> *blanks
079900020717     c                                       and  §TADdesi <> *blanks
080000990302     C                   movel     §TADdesi      POCtad
080100990302     c                   WHEN        §TADdest <> *blanks
080200990302     C                   movel     §TADdest      POCtad
080300990302     c                   OTHER
080400990302     C                   movel     §TADdesc      POCtad
080500990302     c                   ENDSL
080600080311      * verifico il tipo anomalia
080700080311     c                   clear                   prttad
080800080311     c                   if        §tadragr = 'M'
080900080312     c                   if        Wclsflo7 =  *blank
081000080311     c                   eval      prttad = 'smarrimento.'
081100080311     c                   else
081200080311     c                   eval      prttad = 'is missing.'
081300080311     c                   endif
081400080311     c                   else
081500080312     c                   if        Wclsflo7 =  *blank
081600080311     c                   eval      prttad = 'avaria.'
081700080311     c                   else
081800080311     c                   eval      prttad = 'was damaged.'
081900080311     c                   endif
082000080312     c                   endif
082100080312      *
082200981123      *
082300980729      * Descrizione anomalia
082400990224     c                   clear                   W900A
082500980729     c                   exsr      DESTAD
082600990224     c                   movel     W035a         POCdan
082700100709      *
082800100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
082900100709     c                   if        Not *in02   and   Not *in20
083000100709     c                   eval      TxtInOut  = ds_DN66PO
083100100709     c                   exsr      sr_xChkChar
083200100709     c                   if        esito = '1'
083300100709     c                   eval      ds_DN66PO = TxtInOut
083400100709     c                   endif
083500100709     c                   endif
083600990224      *
083700990224      *  Stampa prima parte dell'oggetto
083800990310     c                   exsr      PRT_testat
083900080218     c  N20              WRITE     DN66PO
084000080218     c   20              WRITE     DN66MO
084100990224      *
084200990224      *  Stampa ulteriore descrizione anomalia
084300990224     c                   EXSR      PRTDESAGG
084400981123      *
084500981123      * Quantità danneggiate
084600981123      *      Peso
084700990223     c                   IF          DCTpkd > *zeros
084800981123     c                   move(P)   DCTpkd        w007a
084900020606     c     '0'           check     w007a         ww
085000020606     c                   eval      W007b = %subst(w007a:ww)
085100020606     c     ' '           checkr    w007b         ww
085200020606     c                   if        ww = 1
085300981123     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
085400981123     c                   else
085500020606     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(ww-1))
085600020606     c                                      +  ','  +  %subst(w007b:ww:1)
085700981123     c                   endif
085800981123     c                   ENDIF
085900981123      *      Colli
086000990223     c                   IF          DCTncn > *zeros
086100981123     c                   move      DCTncn        w005a
086200020606     c     '0'           check     w005a         ww
086300020327     c                   IF        Wclsflo7 <> *blanks
086400020606     c                   eval      Wcoman = ' Parcels '  +  %subst(W005a:ww)
086500990222     c                   ELSE
086600020606     c                   eval      Wcoman = ' Colli '  +  %subst(W005a:ww)
086700981123     c                   ENDIF
086800990222     c                   ENDIF
086900981123      *      Pezzi
087000981123     c                   IF        DCTnpz > *zeros
087100981123     c                   move      DCTnpz        w005a
087200020606     c     '0'           check     w005a         ww
087300020327     c                   IF        Wclsflo7 <> *blanks
087400020606     c                   eval      Wpzman = ' Pieces '  +  %subst(W005a:ww)
087500990222     c                   ELSE
087600020606     c                   eval      Wpzman = ' Pezzi '  +  %subst(W005a:ww)
087700980729     c                   ENDIF
087800990222     c                   ENDIF
087900981123      *
088000981123     c                   eval      POCqtd  =  %trimr(Wkgman)  +
088100981123     c                                        %trimr(Wcoman)  +  %trimr(Wpzman)
088200981123      *
088300981123      * Descrizione pezzi danneggiati
088400990224     c                   clear                   W900A
088500981123     c                   IF        DCTnpz > *zeros
088600981123     c                   EXSR      DESPZD
088700981123     c                   ENDIF
088800990224     c                   movel     W035a         POCdpz
088900100709      *
089000100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
089100100709     c                   if        Not *in02   and   Not *in20
089200100709     c                   eval      TxtInOut   = ds_DN66PO1
089300100709     c                   exsr      sr_xChkChar
089400100709     c                   if        esito = '1'
089500100709     c                   eval      ds_DN66PO1 = TxtInOut
089600100709     c                   endif
089700100709     c                   endif
089800980730      *
089900990224      *  Stampa seconda parte dell'oggetto
090000990310     c                   exsr      PRT_testat
090100080218     c  N20              WRITE     DN66PO1
090200080218     c   20              WRITE     DN66MO1
090300990224      *
090400990224      *  Stampa ulteriore descrizione pezzi danneggiati
090500990224     c                   EXSR      PRTDESAGG
090600980928
090700980729     C                   ENDSR
090800980928      *****************************************************************
090900980928      *  REPERIMENTO DESCRIZIONE EVENTO
091000980928      *****************************************************************
091100980928     C     DESEVE        BEGSR
091200980929      *
091300980929     C                   movel     'E'           Ktpd
091400981204     C                   movel     DCTAAE        W011A
091500981204     C                   move      DCTNEV        W011A
091600980929     C                   movel(P)  W011A         Knkt
091700980928     C                   clear                   Khim
091800980928     C                   clear                   Kdim
091900980929     C                   clear                   Kstd
092000980929     C                   clear                   Knks
092100980929     C                   movel     'D'           Ktrc
092200980928      *
092300980928     c                   clear                   yy
092400980929     c                   clear                   W35
092500980928      *
092600980929     c     kdcse         chain     fndcs000                           30
092700980928      *
092800980928     c     *in30         DOWeq     *off
092900980929     c     yy            ANDle     3
093000980928     c                   add       1             yy
093100980928     c                   movel     dcsnot        W35(yy)
093200980929     c     kdcse         reade     fndcs000                               30
093300980928     c                   ENDDO
093400980928      *
093500980928     C                   ENDSR
093600980729      *****************************************************************
093700980729      *  REPERIMENTO DESCRIZIONE ANOMALIA
093800980729      *****************************************************************
093900980729     C     DESTAD        BEGSR
094000980729      *
094100980929     C                   movel(P)  'C'           Ktpd
094200981215     C                   movel(P)  DSnumca       Knkt
094300980929     C                   movel(P)  'A'           Kstd
094400980929     C                   clear                   Khim
094500980929     C                   clear                   Kdim
094600980929     C                   clear                   Knks
094700980929     C                   movel     'D'           Ktrc
094800980929      *
094900980803     c                   clear                   yy
095000980730      *
095100980730     c     kdcs          chain     fndcs000                           30
095200980730      *
095300980730     c     *in30         DOWeq     *off
095400990224     c     yy            ANDlt     20
095500980918     c                   add       1             yy
095600990224     c                   movel     dcsnot        DES(yy)
095700980730     c     kdcs          reade     fndcs000                               30
095800980730     c                   ENDDO
095900980730      *
096000980729     C                   ENDSR
096100981123      *****************************************************************
096200981123      *  REPERIMENTO DESCRIZIONE PEZZI DANNEGGIATI
096300981123      *****************************************************************
096400981123     C     DESPZD        BEGSR
096500981123      *
096600981123      * Descrizione pezzi mancanti
096700981123     C                   movel(P)  'C'           Ktpd
096800981215     C                   movel(P)  DSnumca       Knkt
096900981123     C                   movel(P)  'P'           Kstd
097000990224     C                   clear                   Khim
097100990224     C                   clear                   Kdim
097200990224     C                   clear                   Knks
097300981123     c                   clear                   yy
097400981123      *
097500981123     c     kdcs          chain     fndcs000                           30
097600981123      *
097700981123     c     *in30         DOWeq     *off
097800990224     c     yy            ANDlt     20
097900981124     c                   add       1             yy
098000990224     c                   movel     dcsnot        DES(yy)
098100981123     c     kdcs          reade     fndcs000                               30
098200981123     c                   ENDDO
098300981123      *
098400981124     c                   ENDSR
098500990224      *****************************************************************
098600990224      *  STAMPA DESCRIZIONI AGGIUNTIVE NELL'OGGETTO
098700990224      *****************************************************************
098800990224     C     PRTDESAGG     BEGSR
098900990224      *
099000990224     c                   clear                   POCkos
099100020606     c                   DO        11            ww
099200990224      *
099300020606     c                   IF         DES70(ww)  <> *blanks
099400020606     c                   movel     DES70(ww)     POCkos
099500100709      *
099600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
099700100709     c                   if        Not *in02   and   Not *in20
099800100709     c                   eval      TxtInOut   = ds_DN66PO2
099900100709     c                   exsr      sr_xChkChar
100000100709     c                   if        esito = '1'
100100100709     c                   eval      ds_DN66PO2 = TxtInOut
100200100709     c                   endif
100300100709     c                   endif
100400990224      *
100500990310     c                   exsr      PRT_testat
100600080218     c  N20              WRITE     DN66PO2
100700080218     c   20              WRITE     DN66MO2
100800990224      *
100900990224     c                   ENDIF
101000990224     c                   ENDDO
101100990224      *
101200990224     c                   ENDSR
101300980729      *****************************************************************
101400980729      *  REPERIMENTO DATI TIPO ANOMALIA
101500980729      *****************************************************************
101600980729     C     CHTAD         BEGSR
101700980729      *
101800980730     c                   clear                   tibs02ds
101900980729     c                   clear                   dtad
102000980729      *
102100980729     C                   movel     'C'           t02mod
102200980729     C                   movel     knsif         t02sif
102300980729     C                   movel     'TAD'         t02cod
102400981123     C                   movel(P)  Wtad          t02ke1
102500980729      *
102600980729     C                   CALL      'TIBS02R'
102700980729     C                   PARM                    KPJBA
102800980730     C                   PARM                    TIBS02DS
102900980729      *
103000980729     C                   movel     T02uni        DTAD
103100980729      *
103200980729     C                   ENDSR
103300090306      *****************************************************************
103400090306      *   NOTE
103500090306      *****************************************************************
103600090306     C     SUB_note      BEGSR
103700090306      *
103800090306     c                   eval      *in31 = *on
103900090306     c                   clear                   yy
104000090306     c                   clear                   W35
104100090306     c                   clear                   PTCtes
104200090306      *
104300090306     C                   movel(P)  'C'           Ktpd
104400090306     C                   movel(P)  DSnumca       Knkt
104500090306     C                   movel     'T'           Kstd
104600090306     C                   movel(P)  DFAle1        Knks
104700090306     C                   movel     'D'           Ktrc
104800090306     C                   clear                   Khim
104900090306     C                   clear                   Kdim
105000090306      *
105100090306     c     kdcse         chain     fndcs000                           30
105200090306     c     *in30         DOWeq     *off
105300090306      *
105400090306      * Ogni 3 rcd letti scrivo
105500090306     c                   IF        yy = 3
105600090306     c                   exsr      noteprt
105700090306     c                   ENDIF
105800090306      *
105900090306     c                   add       1             yy
106000090306     c                   movel     dcsnot        W35(yy)
106100090306     c     kdcse         reade     fndcs000                               30
106200090306     c                   ENDDO
106300090306      *
106400090306      * Scrivo rcd residui
106500090306     c                   IF        yy > *zeros
106600090306     c                   exsr      noteprt
106700090306     c                   ENDIF
106800090306      *
106900090306     C                   ENDSR
107000090306      *****************************************************************
107100090306      *   STAMPA NOTE
107200090306      *****************************************************************
107300090306     C     NOTEPRT       BEGSR
107400090306      *
107500090306     c                   eval      PTCtes = W35(1)  +  W35(2)  +  W35(3)
107600090306     c                   IF        PTCtes <> *blanks
107700090306      *
107800090306      * La prima volta lascio uno spazio
107900090306     c                   If        *IN31 = *ON
108000090306     c                   exsr      PRT_spazio
108100090306     c                   eval      *in31 = *off
108200090306     c                   Endif
108300090306      *
108400090306     c                   exsr      WRTtesto
108500090306      *
108600090306     c                   clear                   yy
108700090306     c                   clear                   W35
108800090306     c                   clear                   PTCtes
108900090306     c                   ENDIF
109000090306      *
109100090306     C                   ENDSR
109200980730      *****************************************************************
109300980730      *   TESTO
109400980730      *****************************************************************
109500980730     C     SUB_testo     BEGSR
109600080526
109700080219      * Determino il tipo di risarcimento da applicare al cliente
109800080219     c                   EXSR      SUB_TIPRIS
109900981123
110000981124      *  Lascio 2 righe dall'oggetto prima di stampare il testo
110100050303      *  =====> 1 riga !
110200990310     c                   exsr      PRT_spazio
110300981123
110400090306
110500090306      *  Stampo eventuali note aggiuntive
110600090306     c                   EXSR      SUB_NOTE
110700090306
110800990302      *  Imposto posizioni iniziale e finali
110900020327     c                   IF          Wclsflo7 <> *blanks
111000080526     c                   eval      Wdaxx = 15
111100080526     c                   eval      Waxx  = 27
111200990302     c                   ELSE
111300080219     c                   eval      Wdaxx = 1
111400080526     c                   eval      Waxx  = 14
111500990302     c                   ENDIF
111600981123
111700990302     c     Wdaxx         DO        Waxx          XX
111800981123     c                   clear                   PTCtes
111900980803      *
112000080219     c                   SELECT
112100080219      *
112200080219     c                   WHEN      %subst(TES1(xx):1:5) = 'XJUMP'
112300080219     c                   exsr      WRTtesto
112400080219      *
112500080219     c                   WHEN      %subst(TES1(xx):1:5) = 'XTESP'
112600080219     c                   exsr      SUB_tstpar
112700080219      *
112800080219      *
112900080219     c                   OTHER
113000981124     c                   movel(P)  TES1(xx)      PTCtes
113100990223      *       inserisco variabili nel testo standard
113200990302     c                   exsr      SUB_vartst
113300990223     c                   exsr      WRTtesto
113400990223      *
113500080219      *
113600080219     c                   ENDSL
113700980730     c                   ENDDO
113800041123      *  se non ho inviato via e-mail stampo e va via postel
113900041123     c                   if        not *in20 and not *in02
114000041118      *
114100041118     c                   movel     kcli          W0030
114200041118     c                   move      kcli          W0040
114300041118      * chiamo il trul per il recupero dell'indirizzo del mittente
114400041118     c                   clear                   trul44ds
114500080219     c                   eval      d44pgm = 'FIDN66R'
114600041118     c                   eval      d44po  = W0030
114700041118     c                   call      'TRUL44R'
114800041118     c                   parm                    trul44ds
114900050902     c                   parm                    trtcm1ds
115000041123      *
115100041123      * imposto d44mit con doppia @
115200041123     c                   clear                   wlast
115300041123     c                   clear                   wlblank
115400041123     c                   clear                   wd44mit
115500041123     c                   eval      Wlast = %scan('@':d44mit)
115600041123     c                   eval      Wlblank = %scan(' ':d44mit)
115700041123     c                   eval      wd44mit = %subst(d44mit:1:Wlast) + '@' +
115800041123     c                             %subst(d44mit:wlast+1:Wlblank)
115900041123     c
116000041118      *
116100041118      *
116200041118      *
116300041118     c                   if        w0040 <> 8888 and w0040 <> 9999
116400051013     c                                           and w0040 <> *zeros
116500041118      *
116600041118     c                   If        not *in08
116700041118     c                   movel(p)  TESM(1)       PTCtes
116800041118     c                   else
116900041118     c                   movel(p)  TESM(3)       PTCtes
117000041118     c                   endif
117100041118      *
117200041118      *  Lascio 2 righe
117300041118     c                   exsr      PRT_spazio
117400041118     c                   exsr      WRTtesto
117500041118     c                   If        not *in08
117600041118     c                   movel(P)  TESM(2)       PTCtes
117700041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
117800041118     c                   eval      %subst(ptctes:66:7) = %char(Kcli)
117900041118     c                   else
118000041118     c                   movel(P)  TESM(4)       PTCtes
118100041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
118200041118     c                   eval      %subst(ptctes:73:7) = %char(Kcli)
118300041118     c                   endif
118400041118      *
118500041118     c                   exsr      WRTtesto
118600041118     c                   endif
118700041118
118800041118     c                   endif
118900980730      *
119000980730     C                   ENDSR
119100990302      *****************************************************************
119200990302      *   SCRIVO TESTO
119300990302      *****************************************************************
119400990302     C     WRTtesto      BEGSR
119500080526      *
119600080526      * se è la terza riga del testo di avviso danno imposto la data della stampa
119700080526      * di presunto avviso danno
119800080526     c                   If        xx = 3
119900080526     c                   eval      %subst(PTCtes:47:10)  = wdatapreavv
120000080526     c                   endif
120100080526      * inglese
120200080526     c                   If        xx = 17
120300080526     c                   eval      %subst(PTCtes:35:10)  = wdatapreavv
120400080526     c                   endif
120500100709      *
120600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
120700100709     c                   if        Not *in02   and   Not *in20
120800100709     c                   eval      TxtInOut  = ds_DN66PT
120900100709     c                   exsr      sr_xChkChar
121000100709     c                   if        esito = '1'
121100100709     c                   eval      ds_DN66PT = TxtInOut
121200100709     c                   endif
121300100709     c                   endif
121400080526
121500071206
121600990310     c                   exsr      PRT_testat
121700080219     C  N20              WRITE     DN66PT
121800080219     C   20              WRITE     DN66MT
121900990302      *
122000990302     C                   ENDSR
122100990302      *****************************************************************
122200990302      *   TESTO PARTICOLARE PER TIPO RISARCIMENTO
122300990302      *****************************************************************
122400990302     C     SUB_tstpar    BEGSR
122500990302
122600990302      *  Ricavo le coordinate del testo legato al tipo risarcimento
122700990302     c                   z-add     1             yy
122800990302     c     Wtiporis      lookup    COTP(yy)                               30
122900990302     c   30              movel     COSP(yy)      Wrisdaa
123000990302      *
123100990302      *  Imposto posizioni iniziale e finali
123200020327     c                   IF          Wclsflo7 <> *blanks
123300990302     c                   eval      Wdayy = WrisGBda
123400990302     c                   eval      Wayy  = WrisGBa
123500990302     c                   ELSE
123600990302     c                   eval      Wdayy = WrisITAda
123700990302     c                   eval      Wayy  = WrisITAa
123800990302     c                   ENDIF
123900990302
124000990302     c     Wdayy         DO        Wayy          YY
124100990302      *
124200990302     c                   clear                   PTCtes
124300990302     c                   IF        %SUBST(TESP(yy):1:5)  <>  'XJUMP'
124400990302     c                   movel     TESP(yy)      PTCtes
124500990302      *    Inserisco variabili
124600990302     c                   exsr      INStstpar
124700990302     c                   ENDIF
124800990302      *
124900990302     c                   exsr      WRTtesto
125000990302      *
125100990302     c                   ENDDO
125200990302      *
125300990302     C                   ENDSR
125400990224      *****************************************************************
125500990224      *   INSERIMENTO VARIABILI NEL TESTO PARTICOLARE PER TIPO RIMBORSO
125600990224      *****************************************************************
125700990224     C     INStstpar     BEGSR
125800990224
125900010906     c                   exsr      Edit
126000990302
126100990224     c                   SELECT
126200020125
126300060310      *    TIPO RIMBORSO "A"  RCV 450/85
126400020125     c                   WHEN      YY = 2
126500071207     c                   eval      %subst(PTCtes:33:3)  = §GEDDBA
126600071207     c                   eval      %subst(PTCtes:37:9) =
126700020125     c                                %TRIM(%subst(OedNUMAN:1:9))
126800990224
126900990302      *    TIPO RIMBORSO "B"  Elevazione limite risarcibile
127000050216     c                   WHEN      YY = 4
127100071207     c                   eval      %subst(PTCtes:35:3)  = §GEDDBA
127200071207     c                   eval      %subst(PTCtes:39:10) =
127300010907     c                                %TRIM(%subst(OedNUMAN:1:10))
127400990224
127500990302      *    TIPO RIMBORSO "C"  Mandato per conto
127600050216     c                   WHEN      YY = 5
127700080312     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
127800080312     c                   eval      %subst(PTCtes:47:10) =
127900010907     c                                %TRIM(%subst(OedNUMAN:1:10))
128000080312     c                   eval      %subst(PTCtes:60:10) = Wdfvm
128100990224
128200990224      *    TIPO RIMBORSO "D"  CMR
128300050216     c                   WHEN      YY = 8 and  §GEcmr > *zeros
128400010907     c                   eval      WCMR  =    'D.S.P.'
128500010907     c                                      +  %subst(OedNumAN:1:6)
128600080312     c                   eval      %subst(PTCtes:64:16) = WCMR
128700990224
128800990302      *    TIPO RIMBORSO "F"  Valore complessivo in bolla
128900050216     c                   WHEN      YY = 12
129000080312     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
129100080312     c                   eval      %subst(PTCtes:47:10) =
129200010907     c                                %TRIM(%subst(OedNUMAN:1:10))
129300990302
129400060310      *    TIPO RIMBORSO "G"  RCV  286/05
129500060310     c                   WHEN      YY = 16
129600071207     c                   eval      %subst(PTCtes:59:3)  = §GEDDBA
129700071207     c                   eval      %subst(PTCtes:63:9) =
129800060310     c                                %TRIM(%subst(OedNUMAN:1:9))
129900060310
130000020130      *    inglese TIPO RIMBORSO "A"  RCV
130100060315     c                   WHEN      YY = 21
130200050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
130300050216     c                   eval      %subst(PTCtes:22:9) =
130400020130     c                                %TRIM(%subst(OedNUMAN:1:9))
130500020130
130600990302      *    inglese TIPO RIMBORSO "B"  Elevazione limite risarcibile
130700060315     c                   WHEN      YY = 23
130800050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
130900050216     c                   eval      %subst(PTCtes:22:10) =
131000060310     c                                %TRIM(%subst(OedNUMAN:1:10))
131100990302
131200990302      *    inglese TIPO RIMBORSO "C"  Mandato per conto
131300060315     c                   WHEN      YY = 24
131400080312     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
131500080312     c                   eval      %subst(PTCtes:51:10) =
131600010907     c                                %TRIM(%subst(OedNUMAN:1:10))
131700080312     c                   eval      %subst(PTCtes:67:10) = Wdfvm
131800990302
131900990302      *    inglese TIPO RIMBORSO "D"  CMR
132000060315     c                   WHEN      YY = 26  and  §GEcmr > *zeros
132100010907     c                   eval      WCMR  =    'D.S.P.'
132200010907     c                                      +  %subst(OedNumAN:1:6)
132300080312     c                   eval      %subst(PTCtes:60:16) = WCMR
132400990302
132500990302      *    inglese TIPO RIMBORSO "F"  Valore complessivo in bolla
132600060315     c                   WHEN      YY = 29
132700080312     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
132800080312     c                   eval      %subst(PTCtes:51:10) =
132900010907     c                                %TRIM(%subst(OedNUMAN:1:10))
133000990224
133100060310      *    inglese TIPO RIMBORSO "G"  RCV  286/05
133200060315     c                   WHEN      YY = 33
133300071214     c                   eval      %subst(PTCtes:80:3)  = §GEDDBA
133400071214     c                   eval      %subst(PTCtes:84:9) =
133500060310     c                                %TRIM(%subst(OedNUMAN:1:9))
133600060310
133700990224     c                   ENDSL
133800990224
133900990224     C                   ENDSR
134000990302      *****************************************************************
134100990302      *   INSERISCO VARIABILI NEL TESTO
134200990302      *****************************************************************
134300990302     C     SUB_vartst    BEGSR
134400990302      *
134500050208      *    Inserisco nome società
134600080526     c                   IF        XX = 27          or                          inglese
134700080526     c                             XX = 14                                      italiano
134800990302     c                   eval      %subst(PTCtes:52:20) = %TRIM(Wsocie)
134900990302     c                   endif
135000990302      *
135100990302     C                   ENDSR
135200080219      *****************************************************************
135300080219      *   DETERMINO IL TIPO RISARCIMENTO
135400080219      *****************************************************************
135500080219     C     SUB_tipris    BEGSR
135600080219
135700080219     c                   clear                   wImporto
135800080219     c                   clear                   wNrDec
135900080219     c                   clear                   wLungh
136000080219
136100080219      * Se PA EXPORT o PF IMPORT, non DPD,
136200080219      *    abbiamo il tipo risarcimento "D"
136300080219      *
136400080219     C                   If        ( (§DCTtisp = 'E' and  §DCTport = 'A') OR
136500080219     C                               (§DCTtisp = 'I' and  §DCTport = 'F'
136600080219     c                                               and  §DCTeuro <> 'D') )
136700080219      *
136800080219     c                   movel(P)  'D'           Wtiporis
136900080219     c                   z-add     §GEcmr        WImporto
137000080219     c                   z-add     3             wNrDec
137100080219     c                   z-add     6             wLungh
137200080219   x1c                   ELSE
137300080219
137400080219      * Se è esposto il valore in bolla (non calcolato dalla fatturazione)
137500080219      *   tipo risarcimento "F"
137600080219    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
137700080219     c                   movel(P)  'F'           Wtiporis
137800080219     c                   if        TASias <> *zeros and TASvas = *blanks
137900080219     c                   movel     'ITL'         TASvas
138000080219     c                   endif
138100080219     c                   z-add     TASias        W0133
138200080219     c                   movel     TASvas        W003a
138300080219     C                   if        TASvas <> §gedDBA
138400080219     C                   exsr      srCMB
138500080219     c                   endif
138600080219     c                   z-add     W0133         wImporto
138700080219     c                   z-add     §cvDEC        wNrDec
138800080219     c                   z-add     10            wLungh
138900080219   x2c                   ELSE
139000080219      *
139100080219      * Cliente assicurato
139200080219    3c                   IF        Wfndpt = 'S'
139300080219      *   Importo Assicurato
139400080219    4c                   IF        DPTvlm > *zeros
139500080219     c                   z-add     DPTvlm        W0133
139600080219     c                   movel     DPTvvm        W003a
139700080219     c                   IF        DPTvvm <> §gedDBA
139800080219     C                   exsr      srCMB
139900080219     c                   ENDIF
140000080219     c                   z-add     W0133         WImporto
140100080219     c                   z-add     §cvDEC        wNrDec
140200080219     c                   z-add     10            wLungh
140300080219   -4c                   ENDIF
140400080219      *   Decodifica tipo valore
140500080219     c                   SELECT
140600080219     c                   WHEN      DPTfvm = '1'
140700080219     c                   movel(P)  'collo'       Wdfvm
140800080219     c                   WHEN      DPTfvm = '2'
140900080219     c                   movel(P)  'spedizione'  Wdfvm
141000080219     c                   WHEN      DPTfvm = '3'
141100080219     c                   movel(P)  'Kg merce'    Wdfvm
141200080219     c                   OTHER
141300080219     c                   clear                   Wdfvm
141400080219     c                   ENDSL
141500080219      *   Tipo Risarcimento: "C" mandato per conto,
141600080219      *                      "B" elevazione del limite risarcibile
141700080219      *
141800080219    4c                   IF        DPTftc = '8'  or  DPTftc  =  'C'
141900080219     c                             or DPTftc = 'd'
142000080219     c                   movel     'C'           Wtiporis
142100080219     c                   ELSE
142200080219      *   l'elevazione limite risarcibile
142300080219     c                   movel     'B'           Wtiporis
142400080219      *
142500080219   -4c                   ENDIF
142600080219      *
142700080219   -3c                   ENDIF
142800080219
142900080219      * Se non assicurato e non è un evento fortuito (tipo risarcimento
143000080219      *  "E") si applica la legge 450 (tipo risarcimento "A") o il CMR
143100080219      *  (tipo risarcimento "D")  se PA IMPORT o PF EXPORT o DPD EXPORT
143200080219      *  o FEDEX EXPORT
143300080219     c                   SELECT
143400080219     c                   WHEN      Wtiporis <> *blanks
143500080219     c                   WHEN          (§DCTeuro =  'X'
143600080219     c                              or  §DCTeuro =  'P')
143700080219     c                             and  §DCTtisp <> *blanks
143800080219     c                             or   §DCTeuro =  'D'
143900080219     c                             and  §DCTtisp =  'E'
144000080219     c                             or   §DCTeuro =  'F'
144100080219     c                             and  §DCTtisp =  'E'
144200080219     c                   movel(P)  'D'           Wtiporis
144300080219     c                   z-add     §GEcmr        WImporto
144400080219     c                   z-add     3             wNrDec
144500080219     c                   z-add     6             wLungh
144600080219     c                   OTHER
144700080219      * Verifico il tipo di limite risarcibile in base al flag in dctflo
144800080219     c                   If        §dcttlrp = ' '
144900080219     c                   movel(P)  'A'           Wtiporis
145000080219     c                   z-add     6,2           WImporto
145100080219     c                   endif
145200080219     c                   If        §dcttlrp = '6'
145300080219     c                   movel(P)  'H'           Wtiporis
145400080219     c                   z-add     6,2           WImporto
145500080219     c                   endif
145600080219     c                   If        §dcttlrp = '1'
145700080219     c                   movel(P)  'G'           Wtiporis
145800080219     c                   z-add     1             WImporto
145900080219     c                   endif
146000080219     c                   z-add     2             wNrDec
146100080219     c                   z-add     6             wLungh
146200080219     c                   ENDSL
146300080219      *
146400080219   -2c                   ENDIF
146500080219   -1c                   ENDIF
146600080219      *
146700080219     C                   ENDSR
146800020124      *****************************************************************
146900020124      *   SCRIVO NOTE
147000020124      *****************************************************************
147100020124     c     wrtdcs        begsr
147200020124
147300020124     c                   clear                   dcspno
147400020124
147500020124     c                   eval      ktpd='C'
147600020124     c                   eval      knkt=dsnumca
147700020124     c                   eval      kstd=' '
147800020124     c                   eval      kdim=dateu
147900020124     c                   movel     wora          khim
148000080311     c                   movel     wfscl         knks
148100020124     c                   eval      ktrc='N'
148200020124
148300020124      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA se esiste
148400020124     c     kdcsp         setgt     fndcs01l
148500020124     c     kdcse         readpe    fndcs01l
148600020124
148700020124     c                   eval      dcstpd = ktpd
148800020124     c                   eval      dcsnkt = knkt
148900020124     c                   eval      dcsstd = kstd
149000020124     c                   eval      dcsdim = kdim
149100020124     c                   eval      dcshim = khim
149200020124     c                   eval      dcsnks = knks
149300020124     c                   eval      dcstrc = ktrc
149400020124     c                   eval      dcsatb = ' '
149500020124
149600020124     c                   move      knmus         dcspru
149700020125     c                   if        simfel <> *zeros
149800020124     c                   movel     simfel        dcspos
149900020125     c                   else
150000020125     c                   eval      dcspos = 046
150100020125     c                   endif
150200020124     c                   clear                   dcsstn
150300020124     c                   clear                   dcsft1
150400020124     c                   clear                   dcsdt1
150500020124
150600020124      * Scrivo 2 note xchè su una non ci sto con tutta la frase
150700020124     c                   add       1             dcspno
150800020124     c                   clear                   dcsnot
150900020205     c                   eval      dcsnot = 'Ins. Automatico Pratica Franchigia'
151000020124     c                   write     fndcs000
151100020124
151200020124     c                   endsr
151300980804      *****************************************************************
151400980804      *   SCRIVO STORICO FASI
151500980804      *****************************************************************
151600980804     C     WRTdcf        BEGSR
151700980804      *
151800980804     c                   clear                   fndcf000
151900980804      *
152000080311     c                   z-add     DCTaac        DCFaac
152100080311     c                   z-add     DCTfil        DCFfil
152200080311     c                   z-add     DCTnca        DCFnca
152300980804     c                   movel     DCTptr        DCFptr
152400980804     c     *ISO          movel     DATA_oggi     DCFdfc
152500980804     c                   movel     w0140         DCFhfc
152600080311     c                   z-add     46            DCFfev
152700980804     c                   movel     knmus         DCFpru
152800980907     c                   clear                   DCFftr
152900980907     c                   clear                   DCFdtr
153000080311     c                   z-add     wfscl         DCFfca
153100041117      * Se Fase 545 Valorizzo l'invio
153200080311     c                   clear                   ddcf01
153300041117     c                   eval      §dcfinvi = invio
153400041117      *
153500020124     c                   movel     ddcf01        dcflet
153600020124
153700980804      *
153800980804     c                   write     fndcf000
153900980804      *
154000980804     C                   ENDSR
154100080312      *****************************************************************
154200080312      *   AGGIORNA File importi CA
154300080312      *****************************************************************
154400080312     C     WRTdcl        BEGSR
154500080312
154600080312     c     knca          chain     fndcl01l
154700080312
154800080312     c                   if        not %Found(fndcl01l)
154900080312     c                   clear                   fndcl000
155000080312     c                   movel     x_dctaac      Dclaac
155100080312     c                   movel     x_dctfil      Dclfil
155200080312     c                   z-add     x_dctnca      Dclnca
155300080312     c                   endif
155400080312     c* importo concordato
155500080312     c                   z-add     x_dctipv      dclipr
155600080312     c                   movel     x_dctvpv      dclvpr
155700080626     c                   z-add     x_dctipv      dclipl
155800080626     c                   movel     x_dctvpv      dclvpl
155900080312     c                   clear                   dclftr
156000080312     c*
156100080312     c                   If        %found(fndcl01l)
156200080312     c                   update    fndcl000
156300080312     c                   else
156400080312     c                   write     fndcl000
156500080312     c                   endif
156600080312     c*
156700080312     c                   endsr
156800080312      *
156900980804      *****************************************************************
157000980804      *   AGGIORNA TESTATA C.A.
157100980804      *****************************************************************
157200981124     C     WRTdct        BEGSR
157300981124      *
157400080311     c                   z-add     wfscl         DCTfca
157500980804      *
157600980804      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
157700980804     c                   CLEAR                   FIDN05DS
157800980804     c                   movel     'F'           I05MOD
157900080311     c                   z-add     190           I05fca
158000981204     c                   movel     DCTfpr        I05fpr
158100980804     c                   movel     DCTptr        I05tpc
158200981026     c                   if        DCTnev > *zeros
158300980804     c                   movel     'E'           I05fpe
158400980804     c                   else
158500980804     c                   movel     'N'           I05fpe
158600980804     c                   endif
158700981030     c                   if        DCTnde > *zeros
158800981030     c                   movel     'D'           I05fde
158900981102     c                   else
159000981102     c                   movel     'N'           I05fde
159100981030     c                   endif
159200980804     c                   movel     'O'           I05ffs
159300980804     c     *iso          movel     DATA_oggi     i05dta
159400981204     c                   move      DCTtad        i05tad
159500981125      * Valorizzo numero CA
159600981125     c                   z-add     DCTaac        i05aac
159700981125     c                   z-add     DCTfil        i05fil
159800981125     c                   z-add     DCTnca        i05nca
159900980804
160000980804     C                   CALL      'FIDN05R'
160100980804     c                   PARM                    KPJBA
160200980804     c                   PARM                    FIDN05DS
160300980804
160400980804     c                   IF        O05err = *blanks
160500980804     c                   movel     O05REC        DS_FNDFA
160600980804     c                   SELECT
160700980804     c                   WHEN      DFAgfs = 'P'
160800981229     c                   movel     §DCTlnpc      dctgfc
160900981023     c                   WHEN      DFAgfs = 'A'
161000981023     c                   z-add     DCTlna        dctgfc
161100981026     c                   WHEN      DFAgfs = 'S'
161200981026     c                   z-add     46            dctgfc
161300980804     c                   OTHER
161400981023     c                   z-add     DCTfil        dctgfc
161500981027     C                   ENDSL
161600980804     c                   ENDIF
161700980804      *
161800981222     c                   clear                   dctft1
161900981222     c                   clear                   dctft2
162000020124
162100020124     c                   movel     ddct01        dctflo
162200980804      *
162300980804     c                   EXCEPT    AGGDCT
162400980804      *
162500980804     C                   ENDSR
162600990310      *****************************************************************
162700990310      *   STAMPO spazio
162800990310      *****************************************************************
162900990310     C     PRT_SPAZIO    BEGSR
163000990310      *
163100990310     c                   exsr      PRT_testat
163200080218     C  N20              WRITE     DN66P0
163300080218     C   20              WRITE     DN66M0
163400990310      *
163500990310     c                   endsr
163600990310      *****************************************************************
163700990310      *   STAMPO TESTATA
163800990310      *****************************************************************
163900990310     C     PRT_TESTAT    BEGSR
164000990310      *
164100990310     c                   if         *IN01 = *ON
164200080218     c                   WRITE     DN66PP
164300990310     c                   EVAL      *in01 = *off
164400990310     c                   endif
164500041117      *
164600041117     c                   if         *IN21 = *ON  AND *IN20
164700080218     c                   WRITE     DN66MP
164800041117     c                   EVAL      *in21 = *off
164900041117     c                   endif
165000990310      *
165100990310     c                   endsr
165200981007      *****************************************************************
165300981007      *   AZZERO CAMPI PRTF
165400981007      *****************************************************************
165500981007     C     INZPRTF       BEGSR
165600981007      *
165700080215     c                   clear                   DN66pI
165800080215     c                   clear                   DN66pO
165900080215     c                   clear                   DN66pO1
166000080215     c                   clear                   DN66pO2
166100080215     c                   clear                   DN66pT
166200041118      *
166300041118     c                   if        *in20
166400080215     c                   close     fidn66pm
166500041118     c                   endif
166600981007      *
166700981007     C                   ENDSR
166800981027      *****************************************************************
166900981027      *   IMPOSTO TIPO MODULO E STAMPANTE
167000981027      *****************************************************************
167100981027     C     IMPPRT        BEGSR
167200981027      *
167300080218     c                   Clear                   COMMAND
167400081030      *  Ristampa
167500081030     c                   IF        *in02 = *on
167600081030      *          Bartolini
167700081030     c                   movea     CMD(1)        COMMAND
167800081030     c                   eval      %subst(COMMAND:60:10) = §TLDstp1
167900081030     c                   z-add     90            lung
168000081030     c                   call      'QCMDEXC'
168100081030     c                   parm                    COMMAND
168200081030     c                   parm                    LUNG
168300081030      *
168400081030     c                   else
168500981203      *
168600981214      *  PT-Postel Bartolini
168700081030     c                   movea     CMD(2)        COMMAND
168800041118     c                   z-add     90            lung
168900981203     c                   call      'QCMDEXC'
169000981203     c                   parm                    COMMAND
169100981203     c                   parm                    LUNG
169200081030     c                   endif
169300981027      *
169400981113      *  Apro file di stampa
169500080218     c                   open      FIDN66P
169600981113      *
169700981027     C                   ENDSR
169800981027      *****************************************************************
169900981027      *   AGGANCIO TABELLA TIPO TESTO
170000981027      *****************************************************************
170100981027     C     DECTLD        BEGSR
170200981027      *
170300981027     c                   clear                   tibs02ds
170400981027     c                   clear                   dtld
170500981027      *
170600981027     C                   movel     'C'           t02mod
170700981027     C                   movel     knsif         t02sif
170800981124     C                   movel     'TLD'         t02cod
170900981204     C                   movel(P)  DFAle1        t02ke1
171000981027      *
171100981027     C                   CALL      'TIBS02R'
171200981027     C                   PARM                    KPJBA
171300981027     C                   PARM                    TIBS02DS
171400981027      *
171500981027     C                   movel     T02uni        DTLD
171600981027      *
171700981027     C                   ENDSR
171800981203      *****************************************************************
171900981230      * DECODIFICO NAZIONE
172000981203      *****************************************************************
172100981230     C     CTRNAZ        BEGSR
172200981203      *
172300981230     C                   clear                   DS15
172400981230     C                   movel     '15'          KCOD
172500981230     C                   movel(P)  WNAZ          KKEY
172600981203     C     KTAB          CHAIN     TABEL00f                           30
172700981203      *
172800981230     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
172900981230     C                   movel     TBLUNI        DS15
173000981230     C                   endif
173100981203      *
173200981203     C                   ENDSR
173300981230      *****************************************************************
173400981230      * CONTROLLO DIVISA
173500981230      *****************************************************************
173600981230     C     CTRVLT        BEGSR
173700981230      *
173800981230     C                   MOVEL     'CV'          KCOD
173900990415     C                   MOVEL(P)  W003a         KKEY
174000981230     C     KTAB          CHAIN     TABEL00f                           30
174100981230      *
174200981230     C                   IF        *IN30 = *ON  or
174300981230     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
174400011026     C                   clear                   DSCV
174500981230     C                   ELSE
174600981230      *
174700981230     C                   movel     TBLUNI        DSCV
174800981230     C                   ENDIF
174900981230      *
175000011026     C                   ENDSR
175100010906
175200010906      *****************************************************************
175300010906      * CONVERSIONE IMPORTI IN MONETA CORRENTE
175400010906      *****************************************************************
175500010906     C     srCMB         BEGSR
175600010906      *
175700010906     C                   reset                   YEURCODS
175800010906      *
175900010906     C                   movel     W003a         yecDVI
176000010906     C                   z-add     W0133         yecIMI
176100010910     C                   movel     §gedDBA       yecDVO
176200010906      *
176300010906     C                   call      'YEURCO'
176400010906     C                   parm                    YEURCODS
176500010906      *
176600010906     C                   IF        yecESI  = ' '
176700010906     C                   z-add     yecIMO        W0133
176800011002     C                   movel     yecDVO        W003a
176900010906     C                   ENDIF
177000010906      *
177100010906     C                   ENDSR
177200010906
177300010906      ************************************************************
177400010907      * Editazione di un numero
177500010906      ************************************************************
177600010906     C     Edit          Begsr
177700010906     C*
177800010907     C                   reset                   XEDITDS
177900010906     C* numero da elaborare
178000010906     C* (WImporto ha 5 decimali, $SrcVar ha 0 decimali)
178100010906     C                   Select
178200010907     C                   When      wNrDec    = 0
178300010907     C                   eval      IedNUMERO = wImporto
178400010907     C                   When      wNrDec    = 1
178500010907     C                   eval      IedNUMERO = wImporto * 10
178600010907     C                   When      wNrDec    = 2
178700010907     C                   eval      IedNUMERO = wImporto * 100
178800010907     C                   When      wNrDec    = 3
178900010907     C                   eval      IedNUMERO = wImporto * 1000
179000010907     C                   When      wNrDec    = 4
179100010907     C                   eval      IedNUMERO = wImporto * 10000
179200010907     C                   When      wNrDec    = 5
179300010907     C                   eval      IedNUMERO = wImporto * 100000
179400010906     C                   Endsl
179500010906     C* numero decimali
179600010907     C                   eval      IedNRDEC  = wNrDec
179700010906     C* codice di editazione
179800010907     C                   eval      IedEDTCOD = '3'
179900010906     C* max lunghezza campo in output
180000010907     C                   eval      IedLENFLD = wLungh
180100010906     C*
180200010907     C                   call      'XEDIT'
180300010907     C                   parm                    XEDITDS
180400010906      *
180500010906     C                   ENDSR
180600010906
180700981123      *****************************************************************
180800981123      *   RICERCA CAPOCONTI
180900981123      *****************************************************************
181000981123     C     CAPOCON       BEGSR
181100981123      *
181200981123      *--- RICERCA CAPOCONTI
181300020606     C                   DO        50            ww
181400020606     C                   MOVE      TCU(ww)       TCUDS
181500981123     C                   IF        F56 = 'CG'
181600981123      *
181700981123     C     f4            COMP      '1'                                    21
181800981123     C     f4            COMP      '2'                                    22
181900981123     C     f4            COMP      '3'                                    23
182000981123     C     f4            COMP      '6'                                    27
182100981123      ** 1 CLIENTI   21    ** 2 FORNITORI 22    ** 3 AGENTI    23
182200981123     C     f3            comp      '0'                                242425
182300981123     C     f3            comp      'I'                                    26
182400981123      ** 0 ITALIA   25      ** 1 ESTERO   24    ** I CAPO CONTO IVA
182500981123     c                   if        *in21 = *on
182600020606     C   24              Z-ADD     KCU(ww)       KCE               4 0
182700020606     C   25              Z-ADD     KCU(ww)       KCI               4 0
182800981123     c                   end
182900981123     c                   if        *in22 = *on
183000020606     C   24              Z-ADD     KCU(ww)       KFE               4 0
183100020606     C   25              Z-ADD     KCU(ww)       KFI               4 0
183200981123     c                   end
183300981123     c                   if        *in23 = *on
183400020606     C   24              Z-ADD     KCU(ww)       KAE               4 0
183500020606     C   25              Z-ADD     KCU(ww)       KAI               4 0
183600981123     c                   end
183700020606     C   26              Z-ADD     KCU(ww)       KIVA              4 0
183800020606     C   27              Z-ADD     KCU(ww)       KBNA              4 0
183900981123      *
184000981123     C                   ENDIF
184100981123     C                   ENDDO
184200981123      *
184300981123     C                   ENDSR
184400100709      /free
184500100709       //--------------------------------------------------------------
184600100709       //?"Bonifica" campi in stampa da caratteri indesiderati         ?
184700100709       //--------------------------------------------------------------
184800100709       BEGSR  sr_xChkChar;
184900100709
185000100709         ChkNull = '1';
185100100709
185200100709         xChkChar ( TxtInOut :
185300100709                    ElencoChar :
185400100709                    TipoElenco :
185500100709                    CharSost :
185600100709                    Uppercase :
185700100709                    ChkNull :
185800100709                    CharNull :
185900100709                    Esito);
186000100709
186100100709       ENDSR;
186200100709      /end-free
186300981116      *****************************************************************
186400981116      *   ROUTINE INIZIALE
186500981116      *****************************************************************
186600981116     C     *INZSR        BEGSR
186700981116      *
186800981116      *
186900981116     C                   Z-ADD     1             CODUT
187000981116     C                   CALL      'X§PARUT'
187100981123     C                   PARM                    UT§DSE0F
187200981116      *
187300981116     C                   MOVEL     REC80         CNCR80
187400981124      *
187500981116      *  Ricerca capoconti
187600981116     C                   EXSR      CAPOCON
187700990222
187800010413      ***
187900010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
188000010413      ***
188100010413      *
188200010413      * recupero la moneta corrente
188300010413     C                   CLEAR                   tibs02ds
188400010413      *
188500010413     C                   MOVEL     'L'           T02TLA
188600010413     C                   MOVEL     'C'           T02MOD
188700010413     C                   MOVEL     KNSIF         T02SIF
188800010413     C                   MOVEL     'GED'         T02COD
188900010906     C                   MOVEL(P)  'DANNI'       T02KE1
189000010413      *
189100010413     C                   CALL      'TIBS02R'
189200010413     C                   PARM                    KPJBA
189300010413     C                   PARM                    tibs02ds
189400010413      *
189500010910     c                   movel     T02UNI        DGEDDN
189600011026      *
189700011026      * ...ed i relativi dati
189800011026     C                   movel     §gedDBA       W003a
189900011026     C                   exsr      ctrvlt
190000011026      *
190100010413      * recupero i valori nella moneta corrente
190200010413     C                   CLEAR                   tibs02ds
190300010413      *
190400010413     C                   MOVEL     'L'           T02TLA
190500010413     C                   MOVEL     'C'           T02MOD
190600010413     C                   MOVEL     KNSIF         T02SIF
190700010413     C                   MOVEL     'GEI'         T02COD
190800010910     C                   MOVEL     §GEDDBA       T02KE1
190900010413      *
191000010413     C                   CALL      'TIBS02R'
191100010413     C                   PARM                    KPJBA
191200010413     C                   PARM                    tibs02ds
191300010413      *
191400010413     c                   movel     t02uni        DGEI
191500990226      *
191600990226     c                   clear                   tibs02ds
191700990226     c                   clear                   dstd
191800990226      *
191900990226     C                   movel     'C'           t02mod
192000990226     C                   movel     knsif         t02sif
192100990226     C                   movel     'STD'         t02cod
192200990226     C                   movel(P)  1             t02ke1
192300010910     C                   movel(P)  §gedDBA       t02ke2
192400990226      *
192500990226     C                   CALL      'TIBS02R'
192600990226     C                   PARM                    KPJBA
192700990226     C                   PARM                    TIBS02DS
192800990226      *
192900990301     C                   movel     T02uni        DSTD
193000981116
193100981116     C                   TIME                    W0140
193200981116      *
193300981116     C                   MOVE      W0140         WDTGIO
193400981116     C                   MOVEL     W0140         WORA
193500981116      *
193600981116      * UDATE IN AAAAMMGG
193700981116     C                   Z-ADD     WDTGIO        G02DAT
193800981116     C                   MOVEL     *BLANK        G02ERR
193900981116     C                   CALL      'XSRDA8'
194000981116     C                   PARM                    WLBDAT
194100981116     C                   MOVEL     G02INV        DATEU
194200981116     C     *iso          MOVEL     DATEU         DATA_oggi
194300990316     C     *eur          movel     DATA_oggi     wdataoggi
194400080213
194500080215     C     Knca          klist
194600080215     C                   KFLD                    X_DCTaac
194700080215     C                   KFLD                    X_DCTfil
194800080215     C                   KFLD                    X_DCTnca
194900081029
195000081029     C     Kdct          klist
195100081029     C                   KFLD                    i00aac
195200081029     C                   KFLD                    i00fil
195300081029     C                   KFLD                    i00nca
195400001228      *
195500981123     C     Kdet          klist
195600981123     C                   KFLD                    DCTaae
195700981123     C                   KFLD                    DCTnev
195800021204      *
195900021204     C     Ktas4         klist
196000021204     C                   kfld                    DCTaas
196100021204     C                   kfld                    DCTlnp
196200021204     C                   kfld                    DCTnrs
196300021204     C                   kfld                    DCTnsp
196400021204     C                   kfld                    TA4trc
196500021204     c                   eval      TA4trc = 'A'
196600981116      *
196700981116     C     KDCS          klist
196800981116     C                   KFLD                    Ktpd
196900981116     C                   KFLD                    Knkt
197000981116     C                   KFLD                    Kstd
197100981116      *
197200981116     C     KDCSE         klist
197300981116     C                   KFLD                    KTPD
197400981116     C                   KFLD                    KNKT
197500981116     C                   KFLD                    KSTD
197600981116     C                   KFLD                    KDIM
197700981116     C                   KFLD                    KHIM
197800981116     C                   KFLD                    KNKS
197900981116     C                   KFLD                    KTRC
198000020124
198100020124     c     kdcsp         KLIST
198200020124     c                   KFLD                    ktpd
198300020124     c                   KFLD                    knkt
198400020124     c                   KFLD                    kstd
198500020124     c                   KFLD                    kdim
198600020124     c                   KFLD                    khim
198700020124     c                   KFLD                    knks
198800020124     c                   KFLD                    ktrc
198900020124     c                   KFLD                    prgnot
199000981116      *
199100981116     c     kbol          klist
199200080215     c                   kfld                    x_dctaas
199300080215     c                   kfld                    x_dctlnp
199400080215     c                   kfld                    x_dctnrs
199500080215     c                   kfld                    x_dctnsp
199600981116      *
199700981116     C     KTAB          KLIST
199800981116     C                   KFLD                    CODUT
199900981116     C                   KFLD                    KCOD
200000981116     C                   KFLD                    KKEY
200100990222      *
200200990222     C     KTABE         KLIST
200300990222     C                   KFLD                    CODUT
200400990222     C                   KFLD                    KCOD
200500981116
200600981116     C                   ENDSR
200700980804      *****************************************************************
200800980804     OFNDCT000  E            AGGDCT
200900980804     O                       DCTgfc
201000980804     O                       DCTfca
201100020124     o                       dctflo
201200980804      *****************************************************************
201300981210** CMD   (Lungh. 90)                                                                     *
201400081106OVRPRTF FIDN66P SPLFNAME(LET198) FORMTYPE(AVVDANNLT)  OUTQ(xxxxxxxxxx) HOLD(*YES)
201500080312OVRPRTF FIDN66P SPLFNAME(LET198) FORMTYPE(AVVDANNLT) OUTQ(PICCIONEDB/BARPICSTM)
201600080312OVRPRTF FIDN66PM FORMTYPE(AVVDANNLT) OUTQ(xxxxxxxxxx) USRDFNDTA('
201700981123** TES1     *            (Lungh. 100)                                                              *
201800080225Gentili signori,
201900080225XJUMP
202000080526Facendo seguito alla nostra comunicazione del xx/xx/xxxx siamo spiacenti di confermare
202100080526l'anomalia sopra descritta.
202200981123XJUMP
202300981123XTESP
202400981123XJUMP
202500080331Nello scusarci per lo spiacevole inconveniente, Vi informiamo che la somma liquidata sara'
202600110928corrisposta tramite assegno di traenza inviato direttamente al Vostro indirizzo.
202700981123XJUMP
202800080225Rimaniamo a completa disposizione per ogni eventuale ulteriore chiarimento.
202900981123XJUMP
203000080225Distinti saluti.
203100981124                                                   XXXXXXXXXXXXXXXXXXXX
203200080226Dear Sir or Madam,
203300080226XJUMP
203400080526Following our damage notice dated xx/xx/xxxx we regret to confirm that the a.m. parcel
203500080526is damaged.
203600990302XJUMP
203700990302XTESP
203800990302XJUMP
203900080404We apologise for the inconvenience caused and we would like to advice you that the paid
204000080404off amount will be paid with a drawing cheque sent directly to your address.
204100990302XJUMP
204200080404We are at your disposal for any further clarification and reamin with our best regards.
204300071217XJUMP
204400990302                                                   XXXXXXXXXXXXXXXXXXXX
204500990302** COTP: 1-3 cod.testo; COSP: 4-6  7-9 1o e ultimo elemento ITA, 10-12 13-15 1o e ultimo elemento GB
204600060310A  001002020021
204700060310B  003004022023
204800060310C  005007024025
204900060310D  008008026026
205000060310E  009011027028
205100060310F  012014029031
205200060310G  015017032034
205300060310H  018019035036
205400981123** TESP     *            (Lungh. 100)                                                              *
205500080312Il danno sara' risarcito nel rispetto del limite previsto dalla vigente L. 450/85
205600071207modificata dalla 162/93, ovvero xxx xxxxxxxxx per ogni Kg di merce perduta o danneggiata.
205700080312Il danno sara' risarcito nel rispetto del limite previsto dalla vigente L. 450/85
205800071207modificata dalla 162/93 elevato a xxx xxxxxxxxxx per ogni Kg di merce perduta o danneggiata.
205900080312Il danno sara' risarcito con il limite di xxx xxxxxxxxxx a xxxxxxxxxx, giusto mandato
206000080526conferitoci di assicurare per Vostro conto, contro tutti i rischi, le merci oggetto
206100080526della spedizione stessa.
206200080312Il danno sara' risarcito con il limite previsto dalla C.M.R. : xxxxxxxxxxxxxxxx
206300071217Il risarcimento non Vi potra' essere riconosciuto poiche', per le modalita' di accadimento,
206400071207rientra fra quelli considerati "fortuiti" dal Codice Civile /art. 1693 e 1694), non ravvisandosi
206500071207alcuna nostra responsabilita'.
206600080312Il danno sara' risarcito con il limite di xxx xxxxxxxxxx, valore complessivo riportato
206700080526nel DDT, giusto mandato conferitoci di assicurare per Vostro conto, contro tutti i rischi,
206800080526le merci oggetto della spedizione stessa
206900080312Il danno sara' risarcito nel rispetto del limite previsto dall'articolo 1696 c.c. cosi'
207000080526come modificato dal d.lgs 286/2005 ovvero non superiore a xxx xxxxxxxxx per ogni chilogrammo
207100080526di peso lordo della merce perduta o avariata.
207200080312Il danno sara' risarcito nel rispetto del limite di EUR 6,20 per ogni Kg di merce
207300071207perduta o danneggiata.
207400050216the compensation will be recognized you according to the present law L. 450/85 amended by
207500050216the 162/93, i.e. xxx xxxxxxxxx per each Kg merchandise lost or damaged.
207600050216the compensation will be recognized you according to the present law L. 450/85 amended by
207700050216the 162/93 up to xxx xxxxxxxxxx each Kg merchandise lost or damaged.
207800080312The damage will be refund within the limit of xxx xxxxxxxxxx each xxxxxxxxxx, based upon
207900071214your mandate to insure "all risks" your merchandise being the object of the shipment himself.
208000080312The damage will be refund within the limit of the C.M.R. : xxxxxxxxxxxxxxxx
208100080312The refund will not be paid since the events are classified under the "fortuitous
208200071214events" by the Civil Code (art. 1693 and 1694), and we have no liability for them.
208300080312The damage will be refund within the limit of xxx xxxxxxxxxx, that is the total amount
208400071217of the delivery note, as per the amount we were asked to insure on your behalf, against all risks,
208500071217of this shipment.
208600080312The damage will be refund within the limit fixed by the article 1696 c.c. as it was
208700071214modified by the legislative decree 286/2005 that is the refund will not exceed xxx xxxxxxxxx
208800071214per each kilo (gross weight) of the missing or damaged goods.
208900060310you will be refund within the limit of EUR 6,20 per each kilo (gross weight) of the missing or
209000060310damaged goods.
209100041118** TESM testo da stampare per richiesta via e-mail
209200041118Per ricevere le prossime comunicazioni via e-mail,Vi invitiamo a darne segnalazione a
209300041118                              indicando i seguenti dati : Codice xxxxxxx e-mail ___________________
209400041118Should you need the next information via e-mail, We ask you to be so kind and ask it at
209500041118                              specifying the following details : Codice xxxxxxx e-mail ____________
