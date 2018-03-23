000100981125      *--------------------------------------------------------------------------*
000200080212      * Stampa Comunicazione LT a cliente                                        *
000300981125      *--------------------------------------------------------------------------*
000400980731
000500981124      ***************************************************************
000600981023      *  FASI C.A. FISSATE A PROGRAMMA
000700080213      ***************************************************************
000800080213      * 100 = Conferma CA                                   WFCCA
000900081030      * 110 = Cambio Gestione CA                            WFCGCA
001000080212      * 190 = Stampa Comunicazione LT                       WFSCL
001100980731      ***************************************************************
001200980729
001300981124      ***************************************************************
001400980729      *  RIEPILOGO INDICATORI
001500980729      ***************************************************************
001600990224      * 01   - SALTO PAGINA 1o prtf
001700981125      * 02   - NON STAMPO PER POSTEL
001800981119      * 03   - STAMPO Evento
001900990224      * 04   - ALTA INTENSITA riga di testo
002000981203      * 07   - SALTO PAGINA 2o prtf
002100990222      * 08   - TESTO IN INGLESE
002200050303      * 09   - "All'attenzione di..."
002300981214      * 10   - ESEGUO apertura file solo 1° giro
002400021204      * 12   - STAMPO riferimento mittente ALFAnumerico
002500041110      * 20   - Invio Via E-Mail
002600041110      * 21   - Salto pagina prtf mail
002700050303      * 22~27- comodo in ricerca capoconti
002800020717      * 30   - COMODO
002900981124      * 31   - COMODO
003000050303      * 60   - Chiamata a QCMDEXC (ovrprtf) in errore
003100981214      ***************************************************************
003200980729
003300080213     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
003400980729
003500980804     fFNDCT01L  UF   E           K DISK
003600020124     fFNDCS01L  IF a E           K DISK
003700080312     fFNDCL01L  UF a E           K DISK
003800981124     fFNDFA01L  IF   E           K DISK
003900080219     fFNDPT01L  IF   E           K DISK
004000080218     fFNDET01L  IF   E           K DISK
004100981124     fFNDCF01L  UF A E           K DISK
004200990809     fTITAS30C  IF   E           K DISK
004300981120     fTNTMD01L  IF   E           K DISK
004400021204     fTITA430C  IF   E           K DISK
004500980729     fAZORG01L  IF   E           K DISK
004600981124     fTABEL00F  IF   E           K DISK
004700080311     fFIDN66P   O    E             PRINTER oflind(*IN01) infds(PRTBAR) usropn
004800080311     fFIDN66PM  O    E             PRINTER oflind(*IN21) infds(PRTMAI) usropn
004900981204
005000990429     D W35             S             35    DIM(4)                               descr. tipo anomalia
005100981214     D NCA             S              1    DIM(105)                             Comodo per stampa
005200981222     D CAE             S             19    DIM(999)                             CA estero
005300980803      *----------------------------------
005400081030     D CMD             S             90    DIM(3)  CTDATA PERRCD(1)             OVRPRTF
005500050216      *--------------------------------------
005600160701     D TES1            S            100    DIM(28) CTDATA PERRCD(1)             testo lettera 1 riga
005700050216      *--------------------------------------
005800050216
005900050216      *--------------------------------------
006000050216      * posizioni testi schiera TESP
006100060310     D COTP            S              3    DIM(8) CTDATA PERRCD(1)              codici testi partic.
006200060310     D COSP            s             12S 0 DIM(8) ALT(cotp)                     coord. schiera part.
006300050216      * In caso di modifica agli elementi di TESP bisogna
006400050216      * rivedere le sostituzioni effettuate nella sbr  INSTSTPAR
006500060310     D TESP            S            100    DIM(36) CTDATA PERRCD(1)             testi particolari
006600050216      *--------------------------------------
006700050216
006800050216      *--------------------------------------
006900050216      * Schiera testo per mail
007000041118     D TESM            S            100    DIM(4)  CTDATA PERRCD(1)             testo lettera x mail
007100980730      *----------------------------------
007200050216
007300110526     D KBAR            C                   CONST('BRT S.p.A.')
007400980729      *----------------------------------
007500980730     D WLBDAT          DS                  INZ
007600980730     D  g02dat                 1      8  0
007700980730     D  g02inv                 9     16  0
007800980730     D  g02err                17     17
007900980730     D  g02tgi                18     22  0
008000980928      *
008100990302     D                 DS
008200990302     D  WtstITAda              1      3  0
008300990302     D  WtstITAa               4      6  0
008400990302     D  WtstGBda               7      9  0
008500990302     D  WtstGBa               10     12  0
008600990302     D  Wtstdaa                1     12  0
008700990302      *
008800990302     D                 DS
008900990302     D  WrisITAda              1      3  0
009000990302     D  WrisITAa               4      6  0
009100990302     D  WrisGBda               7      9  0
009200990302     D  WrisGBa               10     12  0
009300990302     D  Wrisdaa                1     12  0
009400981026      *
009500080311     D DSNUMCA         DS
009600080311     D  dctaac
009700080311     D  dctfil
009800080311     D  dctnca
009900981123      *
010000981123     d TCUDS           DS
010100981123     d  f1                     1      1
010200981123     d  f3                     3      3
010300981123     d  f2                     2      2
010400981123     d  f4                     4      4
010500981123     d  f56                    5      6
010600990224      *
010700990224     d DS_DESCR        DS
010800990224     d W900A                        900
010900990224     d DES                           35    DIM(20)  overlay(W900A:1)            Descrizione
011000990224     d w035a                         35             overlay(W900A:1)
011100990224     d DES70                         70    dim(11)  overlay(W900A:36)
011200990224      *
011300990224     D PRTBAR          DS
011400990224     D  cur_lineB            367    368I 0
011500990224      *
011600041110     D PRTMAI          DS
011700041110     D  cur_lineM            367    368I 0
011800100709
011900100709       // -?Strutture Dati per la definizione dei campi in stampa?
012000100709       //  ?per la "bonifica" degli stessi da caratteri indesiderati?
012100100709       //  ?da POSTEL?
012200100709     d ds_DN66PI       ds
012300100709     d   PICrag
012400100709     d   PICind
012500100709     d   PICcap
012600100709     d   PICloc
012700100709     d   PICprv
012800100709     d   PICnaz
012900100709     d   PICdal
013000100709     d   PICpec
013100100709     d ds_DN66PO       ds
013200100709     d   POCogg
013300100709     d   POCrsm
013400100709     d   POCrma
013500100709     d   POCrsd
013600100709     d   POClod
013700100709     d   POCprd
013800100709     d   POCkev
013900100709     d   POCtae
014000100709     d   POCtad
014100100709     d   POCdan
014200100709     d ds_DN66PO1      ds
014300100709     d   POCqtd
014400100709     d   POCdpz
014500100709     d ds_DN66PO2      ds
014600100709     d   POCkos
014700100709     d ds_DN66PT       ds
014800100709     d   PTCtes
014900100709
015000981116      *
015100990224     D UT§DSE0F      E DS
015200990224     d  tcu                  398    697     DIM(50)
015300990224     d  kcu                  698    847P 0  DIM(50)  PACKEVEN
015400981026     D CNCR80        E DS
015500090108     D FIDNBEDS      E DS
015600980730     D FIDN05DS      E DS
015700980730     D TIBS02DS      E DS
015800980804     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
015900981204     D DS_TNTMD      E DS                  EXTNAME(TNTMD00F)
016000080215     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F) PREFIX(X_)
016100980507     D KPJBA         E DS
016200081029     D FIDN00DS      E ds
016300980729     D DTAD          E DS
016400981027     D DTLD          E DS
016500010910     D DGEDDN        E DS
016600010413     D DGEI          E DS
016700980918     D DSTB          E DS
016800981203     D DSCV          E DS
016900981230     D DS15          E DS
017000981229     D DDCT01        E DS
017100020124     d ddcf01        e ds
017200990226     D DSTD          E DS
017300041110     D TRUL44DS      E DS                                                       X Invio e-mail
017400050902     D TRTCM1DS      E DS                                                       X Invio e-mail
017500010906     D YEURCODS      E DS                  inz
017600010906     D   YECTAR      E                     inz('H')
017700010907      * Routine editazione di un numero
017800010907     D XEDITDS       E DS                  inz
017900010907     D   OedFlgErr   e                     inz(*off)
018000021118      *
018100021118     d OG143         e ds                  inz
018200021204     d DTA4A         e ds                  inz
018300081029      *
018400081029     d Status         sds
018500081029     d  PARMS            *parms
018600081029      *
018700980729      *----------------------------------
018800080219     d wSpedAss        s              1    inz(*off)
018900080219     D wfndpt          S              1
019000020327     D wclsflo7        S              1
019100990222     D wpzman          S             13
019200990222     D wcoman          S             14
019300981123     D wkgman          S             11
019400980928     D wanno           s              4
019500980730     D wfil            s              3
019600980928     D wnum            s              7
019700980803     D wdataoggi       S             10
019800010328     D wdatastampa     S             10
019900071206     D wdatapreavv     S             10
020000990302     d Wdfvm           s             10
020100080219     D wtiporis        S              3
020200990302     D wtipodoc        S              3
020300990415     D w003a           s              3
020400990415     D w005a           s              5
020500981124     D w007a           s              7
020600981124     D w007b           s              7
020700981124     D w011a           S             11
020800980803     D wsocie          S             20
020900010906     D wcmr            S             16
021000981214     D Wkca            S             19
021100981210     D command         S             90
021200990205     D lineaa          s             12
021300990205     D lineap          s             12
021400981215     D lung            S             15  5 inz(90)
021500980730     D XX              s              3  0
021600980731     D YY              s              3  0
021700980731     D JJ              s              3  0
021800981214     D QQ              s              3  0
021900020606     D WW              s              3  0
022000980730     D dateu           s              8  0
022100980730     D wdtgio          S              8  0
022200080311     D dtaris          S              8  0
022300980730     D wora            S              6  0
022400981203     D nrr1            S              4  0
022500981203     D nrr2            S              4  0
022600980730     D w0140           S             14  0
022700990415     D w0133           S             13  3
022800041118     D w0030           S              3  0
022900980918     D w0040           S              4  0
023000981210     D w0040a          S              4  0
023100981210     D wpointes        S              3  0
023200990302     D wdaxx           S              3  0
023300990302     D waxx            S              3  0
023400990302     D wdayy           S              3  0
023500990302     D wayy            S              3  0
023600990302     D wdaH            S              3  0
023700990302     D waH             S              3  0
023800041123     D wlast           S              2  0
023900041123     D wlblank         S              2  0
024000041123     D wd44mit         S             30
024100041125     D svcom           S             90
024200080311     D prttad          S             12
024300041110
024400041110     D Invio           s              1                                         wrk per invio e-mail
024500041110
024600081029     D Ristampa        s              1                                         wrk per ristampa
024700081030     D Fase_190        s              1                                         Fase 190 ok
024800081029
024900010906      *-------------
025000010906     D WImporto        S             30S 5 inz
025100010906     D WNrDec          S              2S 0 inz
025200010906     D WLungh          S              2S 0 inz
025300010906      *-------------
025400980730      *
025500980730     D DATA_eur        S               D   DATFMT(*eur)
025600980730     D DATA_oggi       S               D   DATFMT(*eur)
025700021127      *
025800990309     D W§dctport       S                   LIKE(§DCTport)
025900031211     d w§STDlpf        s                   like(§STDlpf1) inz(*loval)
026000001228     D Wdtaad          S                   LIKE(DCTaac)                         data stampa A.D.
026100090108     D Kcli            S                   LIKE(DCTksc)
026200980918     D Kcod            S                   LIKE(TBLcod)
026300980929     D Ktpd            S                   LIKE(DCStpd)
026400980929     D Knkt            S                   LIKE(DCSnkt)
026500980929     D Kstd            S                   LIKE(DCSstd)
026600980929     D Kdim            S                   LIKE(DCSdim)
026700980929     D Khim            S                   LIKE(DCShim)
026800980929     D Knks            S                   LIKE(DCSnks)
026900980929     D Ktrc            S                   LIKE(DCStrc)
027000981028     D Kkey            S                   LIKE(TBLkey)
027100981123     D Wtad            S                   LIKE(DCTtad)
027200981230     D wnaz            s                   LIKE(TMDnzm)
027300080526     d WFcca           s                   LIKE(DCTfca)   inz(100)
027400080212     d WFscl           s                   LIKE(DCTfca)   inz(190)
027500081030     d WFcgca          s                   LIKE(DCTfca)   inz(110)
027600020124
027700020125     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
027800100709
027900100709       //--------------------------------------------------------------
028000100709       //?Definizione procedure usate.                                 ?
028100100709       //--------------------------------------------------------------
028200100709
028300100709       // -?Parametri per XCHKCHAR?
028400100709      /copy gaitrasrc/srcProtoPI,XCHKCHAR
028500100709       // -?"Bonifica" stringa da caratteri indesiderati?
028600100709      /copy gaitrasrc/srcProtoPR,XCHKCHAR
028700100709
028800980731      *---------------------------------------------------------------------
028900040427      *
029000080213     c     *ENTRY        PLIST
029100080213     c                   PARM                    KPJBA
029200081029     c                   PARM                    FIDN00DS
029300081029
029400090309      * Aggancio anagrafico fasi per recuperare il tipo testo
029500090309     c     WFscl         CHAIN     FNDFA000                           30
029600090309      * Se non ci sono testi non elaboro
029700090309     c                   IF          *IN30 = *off
029800090309
029900090309      * Aggancio tabella TIPO TESTO
030000090309     c                   EXSR      DECtld
030100090309
030200081029     c                   If        parms = 1
030300080311     c                   movel     kpjbu         dtaris
030400081029      * Stampo C.A. allegate e aggiorno testata C.A. e file fasi
030500081029     c                   exsr      CA_multi
030600081029
030700081029     c                   else
030800081029      * In caso di più parametri si parla di ristampa
030900081029     c                   eval      Ristampa = 'S'
031000081030     c                   exsr      SR_Valorizza
031100081029     c                   exsr      CA_singola
031200081029     c                   endif
031300040427      *
031400090309     c                   endif
031500090309      *
031600981214
031700981203      * Chiudo TIBS02R
031800981203     c                   clear                   tibs02ds
031900981203     c                   movel     'C'           t02tla
032000981203     c                   call      'TIBS02R'
032100981203     c                   parm                    kpjba
032200981203     c                   parm                    tibs02ds
032300981204      *
032400081106     c                   if        Ristampa = 'S' and I00fca  = 998
032500081103     c                   eval      *inRT   = *on
032600081103     c                   else
032700081103     C                   EVAL      *INLR = *ON
032800081103     c                   endif
032900081030      *---------------------------------------------------------------------
033000081030      *  Valorizzo campi per la ristampa
033100081030      *---------------------------------------------------------------------
033200081030     c     Sr_valorizza  BEGSR
033300081030
033400081030     c                   eval      x_dctaas = i00aas
033500081030     c                   eval      x_dctlnp = i00lnp
033600081030     c                   eval      x_dctnrs = i00nrs
033700081030     c                   eval      x_dctnsp = i00nsp
033800081030     c                   eval      x_dctaac = i00aac
033900081030     c                   eval      x_dctfil = i00fil
034000081030     c                   eval      x_dctnca = i00nca
034100090309
034200081030     c                   endsr
034300081030      *
034400981203      *---------------------------------------------------------------------
034500981203      *  ELABORO SUBFILE
034600981203      *---------------------------------------------------------------------
034700981203     c     CA_multi      BEGSR
034800981203      *
034900080213      * Leggo  file
035000080213     C/EXEC SQL
035100080215     C+ DECLARE B1 CURSOR FOR SELECT * FROM fndct01l WHERE
035200080213     C+ dctfpr = 'T' and (dctfca = 100 or dctfca = 110)
035300080215     C+       for read only
035400080213     C/END-EXEC
035500080213
035600080213     C/EXEC SQL
035700080213     C+ OPEN b1
035800080213     C/END-EXEC
035900080213
036000080213     C                   do        *hival
036100080213
036200080213     C/EXEC SQL
036300080215     C+ FETCH NEXT FROM b1 INTO :ds_fndct
036400080213     C/END-EXEC
036500080213
036600080215     c                   select
036700080215     c                   when      sqlcod = 100
036800080213     c                   leave
036900080215     c                   WHEN      SqlCod < 0
037000080213     c                   other
037100080213      * verifico data della fase se entro data limite impostata a video
037200080215     c     knca          setgt     fndcf01l
037300080526     c                   do        *hival
037400080215     c     knca          readpe(n) fndcf01l
037500080527     c                   if        %eof(fndcf01l)
037600080527     c                   leave
037700080527     c                   endif
037800080529     c                   if        x_dctfca = dcffca
037900080529     c                   if        dcfdfc <= dtaris
038000080529      * imposto la data preavviso
038100080529     c     *iso          movel     dcfdfc        data_EUR
038200080529     C     *eur          movel     DATA_EUR      wdatapreavv
038300080529
038400080215     c                   exsr      CA_singola
038500080529     c                   endif
038600080526     c                   leave
038700080215     c                   endif
038800080526     c                   enddo
038900080213     C                   endsl
039000080213
039100080213     C                   enddo
039200080213
039300080213     C/EXEC SQL
039400080213     C+ CLOSE b1
039500080213     C/END-EXEC
039600981203      *
039700981203     c                   ENDSR
039800981203      *---------------------------------------------------------------------
039900981203      *  ELABORO SINGOLA C.A.
040000981203      *---------------------------------------------------------------------
040100981203     c     CA_singola    BEGSR
040200981203      *
040300981203     c                   eval      *in28 = *off
040400080312     c                   eval      *in02 = *off
040500981203     c                   clear                   page
040600080219     c                   clear                   Wtiporis
040700081103     c                   If        Ristampa = 'S'  and i00fca = 999
040800081030     c                   eval      *in02 = *on
040900081030     c                   endif
041000981203      *
041100981203      * Aggancio file bolle e C.A.
041200981203     c                   EXSR      SUB_files
041300980803      *
041400080215     c                   EXSR      SUB_LETTERA
041500981215      * Azzero PRTF
041600981215     c                   EXSR      INZPRTF
041700080215      *  imposto indicatore per invio o no tramite POSTEL
041800080215     c                   eval      *in02  = *on
041900080215      * Aggiorno file
042000081030      * in caso di ristampa non si aggiorna
042100081030     c                   If        Ristampa <> 'S'
042200080215     c                   EXSR      SUB_AGGIOR
042300081030     c                   endif
042400981203      *
042500981203     c                   ENDSR
042600981203      *****************************************************************
042700981203      *   AGGANCIO FILES
042800981203      *****************************************************************
042900981203     C     SUB_files     BEGSR
043000021118      *
043100080219     c                   Reset                   wSpedAss
043200981203      *
043300981203      * Aggancio file bolle
043400990809     c     KBOL          chain     TITAS30C                           30
043500981229      * se tipo porto recupero rileggo
043600981229     C                   movel     'TB'          kCOD
043700981229     C                   movel(P)  TAStbl        kKEY
043800981229     C     ktab          CHAIN     TABEL                              31
043900981229     C                   movel     TBLuni        DSTB
044000981229     c                   if        §TBrbl = 'R'
044100990809     c     KBOL          reade     TITAS30C                               30
044200981229     c                   endif
044300080219if  1c                   If        not *in30
044400080219     c                             and TASias >  0
044500080219     c                             and TASfcm <> 'F'
044600080219     c                   Eval      wSpedAss = *on
044700080219e   1c                   Endif
044800981203      *
044900981203      * Aggancio file mittenti/destinatari
045000981204     c                   clear                   DS_TNTMD
045100981204     c     KBOL          chain     TNTMD000                           30
045200981203      *
045300981203      * Aggancio testata C.A.
045400080215     c     Knca          chain     FNDCT000                           30
045500981229     c                   movel     DCTflo        DDCT01
045600981203      *
045700080219      * Aggancio file mandato assicurativo
045800080311     c     KNCA          chain     FNDPT000                           30
045900080219     c                   if        *in30 = *on  or  DPTatb = 'A'
046000080219     c                   clear                   Wfndpt
046100080219     c                   else
046200080219     c                   eval      Wfndpt = 'S'
046300080219     c                   If        DPTftc = 'C' or  DPTftc = '8' or DPTFTC = 'd'
046400080219     c                   Eval      wSpedAss = *on
046500080219     c                   Endif
046600080219     c                   endif
046700080219      *
046800981203     c                   ENDSR
046900980729      *****************************************************************
047000981203      *   STAMPO LETTERA
047100980729      *****************************************************************
047200080215     C     SUB_LETTERA   BEGSR
047300981203      *
047400001228      *
047500010328     c                   movel     wdataoggi     wdatastampa
047600081029      * in caso di ristampa (singola o totale da EDP)
047700081030     c                   If        ristampa = 'S'
047800081029     c                   exsr      Sub_dtaad                                     dta stampa A.D.LT
047900081029     c                   endif
048000010328      *
048100981203     c                   EXSR      SUB_INTES                                     intestazione
048200981203      *
048300981203     c                   EXSR      SUB_OGGE                                      oggetto
048400981203     c                   EXSR      SUB_TESTO                                     testo
048500981203      *
048600981203     c                   ENDSR
048700081029      *****************************************************************
048800081029      *   RICERCO DATA ULTIMA STAMPA AVVISO DANNO DALLO STORICO FASI
048900081029      *****************************************************************
049000081029     C     SUB_dtaad     BEGSR
049100081029      *
049200081029     c                   clear                   wdtaad
049300081029      *
049400081029     c     kdct          setgt     FNDCF01L
049500081029     c     kdct          readpe    FNDCF01L
049600081029     c*
049700081029     c                   dow       NOT %eof(FNDCF01l)
049800081030     c* se la fase che leggo è uguale alla 190 recupero la data stampa
049900081030     c                   if        dcffca =   Wfscl
050000081029     c                   movel     dcfdfc        wdtaad
050100081029     c     *iso          movel     dcfdfc        data_EUR
050200081029     C     *eur          movel     DATA_EUR      wdatastampa
050300081029     c                   endif
050400081030     c* se la fase che leggo è uguale alla 110 o 100 recupero la data stampa preavviso
050500081030     c                   if        dcffca = wfcca or dcffca = wfcgca
050600081030      * imposto la data preavviso
050700081030     c     *iso          movel     dcfdfc        data_EUR
050800081030     C     *eur          movel     DATA_EUR      wdatapreavv
050900081030     c                   endif
051000081030     c*
051100081029     c     kdct          readpe    fndcf01l
051200081029     c*
051300081029     c                   enddo
051400081029     c*
051500081029     c                   endsr
051600981203      *****************************************************************
051700981214      *   AGGIORNO ARCHIVI E SE HO EVENTO FORTUITO O PF IMPORT CHIUDO LA CA
051800981203      *****************************************************************
051900981203     C     SUB_aggior    BEGSR
052000990429      *
052100981203     c                   EXSR      WRTdcf                                       Dettaglio fase
052200080312     c                   EXSR      WRTdcl                                       Imp.concordato
052300981203     c                   EXSR      WRTdct                                       Testata fase
052400981203      *
052500981203     c                   ENDSR
052600981203      *****************************************************************
052700981203      *   INTESTAZIONE
052800981203      *****************************************************************
052900981203     C     SUB_intes     BEGSR
053000041119      *
053100041119     c                   setoff                                       210107
053200980730      *
053300080218      * DATI dell'intestatario Comunicazione LT
053400980928     C                   exsr      RIEcli
053500981103      *
053600080218      * Interlocutore
053700050303     c                   eval      *in09   =  *off
053800981103     c                   exsr      RECpercon
053900981007      *
054000981007      * Località + data
054100010328     c                   eval      PICdal = 'BOLOGNA, '  +  wdatastampa
054200981120      *
054300981120      *
054400981211     c                   movel(P)  KBAR          Wsocie
054500100709      *
054600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
054700100709     c                   if        Not *in02   and   Not *in20
054800100709     c                   eval      TxtInOut  = ds_DN66PI
054900100709     c                   exsr      sr_xChkChar
055000100709     c                   if        esito = '1'
055100100709     c                   eval      ds_DN66PI = TxtInOut
055200100709     c                   endif
055300100709     c                   endif
055400981214      *
055500981214      * SOLO PRIMA VOLTA apro file stampa e imposto Tipo Modulo e Stampante
055600981214     c                   if        *in10 = *off
055700981214     c                   EXSR      IMPPRT
055800981214     c                   eval      *in10 = *on
055900981214     c                   endif
056000041117      * se invio via mail apro anche il file di spool per invio mail
056100041117     c                   if        *in20
056200080218     c                   open      fidn66pm
056300041117     c                   endif
056400981113      *
056500980805      * Scrivo l'intestazione
056600080218     c  n20              WRITE     DN66PP
056700080218     c  n20              WRITE     DN66PI
056800080218     c   20              WRITE     DN66MI
056900981120      *
057000980730     C                   ENDSR
057100980731      *****************************************************************
057200980918      *   IMPOSTO DATI CLIENTE PER STAMPA
057300980731      *****************************************************************
057400980918     C     RIEcli        BEGSR
057500981210      *
057600981210     c                   clear                   kcli
057700041117     c                   eval      invio = 'P'
057800041117     c                   setoff                                       20
057900090108      * recupero il beneficiario
058000090108     c                   clear                   fidnbeds
058100090108      *
058200090108     c                   movel     'B'           IBEmod
058300090108     c                   z-add     dctaac        IBEaac
058400090108     c                   z-add     dctfil        IBEfil
058500090108     c                   z-add     dctnca        IBEnca
058600090108     c                   z-add     dctaas        IBEaas
058700090108     c                   z-add     dctlnp        IBElnp
058800090108     c                   z-add     dctnrs        IBEnrs
058900090108     c                   z-add     dctnsp        IBEnsp
059000090108     c                   z-add     dctksc        IBEksc
059100090108     c                   movel     dctptr        IBEptr
059200090108     c                   movel     dctflo        IBEflo
059300090108     c                   movel     'S'           IBEtpb
059400090108     c                   If        %subst(knsif:7:1) = 'P'
059500090108     c                   eval      IBEsif = 'P'
059600090108     c                   EndIf
059700090108      *
059800090108     c                   call      'FIDNBER'
059900090108     c                   parm                    fidnbeds
060000090108      *
060100090108     c                   If        oberag <> *blanks
060200090108      *
060300090108     c                   movel     oberag        PICrag
060400090108     c                   movel     obecap        PICcap
060500090108     c                   movel     obevia        PICind
060600090108     c                   movel     obeloc        PICloc
060700090108     c                   if        obenaz  =  *blanks
060800090108     c                   movel     obeprv        PICprv
060900090108     c                   else
061000090108     c                   movel     obenaz        wnaz
061100090108     c                   exsr      ctrnaz
061200090108     c                   movel     §15des        PICnaz
061300090108     c                   endif
061400090108     c                   movel     obeksc        Kcli
061500090108     c                   endif
061600041117      * Imposto se devo stampare in Inglese
061700041117     c                   clear                   WCLSflo7
061800041117     c                   eval      *in08 = *OFF
061900090108     c                   IF        OBEing <> *blanks
062000090108     c                   eval      wclsflo7 = OBEing
062100041117     c                   eval      *in08 = *on
062200041117     c                   ENDIF
062300080218      * verfico se il cliente al quale devo inviare la lettera ha un indirizzo e-mail
062400041116     c                   If        kcli <> *zeros and not *in02
062500041116     c                   clear                   trul44ds
062600080218     c                   eval      D44pgm  = 'FIDN66R'
062700041116     c                   eval      D44Ksc  = Kcli
062800041117     c                   eval      D44cl   = '006'
062900041117     c                   eval      d44apl = 'C'
063000041118     c                   eval      d44nk1 = '0151' + %editc(Kcli:'X')
063100041117     c                   eval      d44tnt = '87'
063200041116     c                   eval      D44ovr  = ' '
063300041116     c                   eval      D44scope= '*CALLLVL'
063400080218     c                   eval      D44prtf = 'FIDN66P'
063500041116
063600041116     c                   IF        Wclsflo7 <> *blanks
063700071217     C                   eval      D44obj =  '*OBJM*Alleged damage Notice - Ano-
063800041122     C                             maly information n. ' +  %char(dctfil)+  '/'+
063900041116     C                             %trim(%editc(dctnca:'Z')) +
064000041118     c                             ' Year '   +   %char(dctaac)
064100041116     c                   ELSE
064200071207     C                   eval      D44obj = '*OBJM*Avviso Danno COMU-
064300041117     C                             NICAZIONE ANOMALIA N. ' +
064400041116     C                             %char(dctfil)+  '/'      +
064500041116     C                             %trim(%editc(dctnca:'Z')) +
064600041118     c                             ' Anno '  +  %char(dctaac)
064700041116     c                   ENDIF
064800041116     c                   call      'TRUL44R'
064900041116     c                   parm                    trul44ds
065000050902     c                   parm                    trtcm1ds
065100041116      *
065200041116      * se c'è stato qualche problema faccio invio in altro modo
065300041116      *
065400041116     c                   if        d44err  <> ' '
065500041117     c                   eval      invio  = 'P'
065600041117     c                   setoff                                       20
065700041116     c                   ELSE
065800041117     c                   seton                                        20
065900041116      * ovrprtf
066000081030     c                   movea     CMD(3)        svcom
066100081106if  3c                   if        Ristampa = 'S' and I00tpb  = '*'
066200081106     c                             and i00fca = 999
066300081103     c                   eval      %subst(svcom:38:16) = *blanks
066400081103x   3c                   else
066500081103     c                   eval      %subst(svcom:43:10) = d44outq
066600081103e   3c                   endif
066700041125     c                   eval      commanl = %trim(svcom)+ d44dta  + ''')'
066800041116     c                   eval      lung = %len(%trim(commanl))
066900041116     C                   CALL      'QCMDEXC'                            60
067000041116     C                   PARM                    COMMANl         500
067100041116     C                   PARM                    LUNG             15 5
067200041116      * se c'è errore
067300041116     c                   if        *in60
067400041117     c                   eval      invio = 'P'
067500041117     c                   setoff                                       20
067600041117     c                   else
067700041117     c                   eval      invio = 'M'
067800041116     c                   endif
067900041116      *
068000041116
068100041116     c                   endif
068200041117
068300041117     c                   endif
068400980918      *
068500980918     C                   ENDSR
068600981210      *****************************************************************
068700981210      *   RECUPERO IL NOMINATIVO DELL'INTERLOCUTORE
068800981210      *****************************************************************
068900981210     C     RECpercon     BEGSR
069000981210      *
069100980731     c                   CLEAR                   DCSnot
069200981002     c                   CLEAR                   PICpec
069300980731      *
069400980929     C                   movel(P)  'C'           Ktpd
069500981215     C                   movel(P)  DSnumca       Knkt
069600980929     C                   movel(P)  'N'           Kstd
069700980929     C                   clear                   Khim
069800980929     C                   clear                   Kdim
069900980929     C                   clear                   Knks
070000980929     C                   movel     'D'           Ktrc
070100980731      *
070200981119     c     KDCS          CHAIN     FNDCS000                           30
070300981026      *
070400981119     c                   IF        *IN30 = *OFF and  DCSnot <> *BLANKS
070500050303     c                   eval      *in09   =  *on
070600981103     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
070700981026     c                   ENDIF
070800980731      *
070900980731     C                   ENDSR
071000980730      *****************************************************************
071100980730      *   OGGETTO
071200980730      *****************************************************************
071300980730     C     SUB_ogge      BEGSR
071400980928
071500981215     c                   clear                   POCqtd
071600981215     c                   clear                   Wkgman
071700981215     c                   clear                   Wcoman
071800981215     c                   clear                   Wpzman
071900981123      *
072000981123      * Intestazione
072100981120     c                   movel     DCTaac        wanno
072200981120     c                   movel     DCTfil        wfil
072300981120     c                   movel     DCTnca        w007a
072400980803     c     '0'           check     w007a         yy
072500980928     c                   eval      wnum = %subst(W007a:yy)
072600981120      *
072700990120      *
072800020327     c                   IF        Wclsflo7 <> *blanks
072900080312     C                   eval      POCogg  =  'Subject:' +
073000080312     C                                       ' DAMAGE COMUNICATION N. '
073100020717     c                                       + wfil + '/' + %trim(wnum)
073200020717     C                                       + ' Year ' + wanno
073300990222     c                   ELSE
073400080312     C                   eval      POCogg  =  'Oggetto:' +
073500080312     C                                         ' COMUNICAZIONE ANOMALIA N.'
073600020717     c                                       + wfil + '/' + %trim(wnum)
073700020717     C                                       + ' Anno ' + wanno
073800990222     c                   ENDIF
073900980803      *
074000981120     c                   z-add     DCTlnp        POClnp
074100981120     c                   z-add     DCTnrs        POCnrs
074200981120     c                   z-add     DCTnsp        POCnsp
074300981120     c                   z-add     DCTaas        POCdat
074400981120     c                   movel     TAsmgs        POCdat
074500981002     c     *mdy          move      POCdat        DATA_eur
074600981002     c     *dmy          move      DATA_eur      POCdat
074700981120     c                   movel     TMDrsm        POCrsm
074800981120     c                   z-add     TASrmn        POCrmn
074900021204      * il riferimento mittente alfanum. per la sede si trova in TITA4
075000021204     c     ktas4         chain     TITA430C
075100021204     c                   if        %found(TITA430C)
075200021204     c                   movel     TA4not        DTA4A
075300021204     c                   else
075400021204     c                   clear                   DTA4A
075500021204     c                   endif
075600021204     c                   movel     §TA4Arma      POCrma
075700021204     c     §TA4Arma      comp      *blanks                            1212
075800981120     c                   movel     TMDrsd        POCrsd
075900981120     c                   movel     TMDlod        POClod
076000990118     c                   if        TMDnzd <> *blanks
076100990118     c                   movel     TMDnzd        POCprd
076200990118     c                   else
076300990118     c                   movel     TMDprd        POCprd
076400990118     c                   endif
076500981120     c                   z-add     TASncl        POCncl
076600990809     c                   z-add     TASpkf        POCpkf
076700981123      *
076800981123      * Evento
076900981123     c                   IF        DCTnev > *zeros
077000981123     c                   eval      *IN03 = *ON
077100981123     c                   movel     DCTaae        wanno
077200981123     c                   movel     DCTnev        w007a
077300981123     c     '0'           check     w007a         yy
077400981123     c                   eval      wnum = %subst(W007a:yy)
077500080311     C                   eval      POCkev = wanno + '/' + %trim(wnum)
077600080311     c     Kdet          chain     FNDET000                           31
077700981123     C                   movel     DETtad        Wtad
077800981123     c                   exsr      CHTAD
077900990302     c                   SELECT
078000020327     c                   WHEN        Wclsflo7 <> *blanks
078100020717     c                                      and  §TADdesi <> *blanks
078200990302     C                   movel     §TADdesi      POCtae
078300990302     c                   WHEN        §TADdest <> *blanks
078400990302     C                   movel     §TADdest      POCtae
078500990302     c                   OTHER
078600990302     C                   movel     §TADdesc      POCtae
078700990302     c                   ENDSL
078800981123     c                   ENDIF
078900981123      *
079000980729      * Decodifico tipo anomalia
079100981123     C                   movel     DCTtad        Wtad
079200980729     c                   exsr      CHTAD
079300990302     c                   SELECT
079400020327     c                   WHEN        Wclsflo7 <> *blanks
079500020717     c                                       and  §TADdesi <> *blanks
079600990302     C                   movel     §TADdesi      POCtad
079700990302     c                   WHEN        §TADdest <> *blanks
079800990302     C                   movel     §TADdest      POCtad
079900990302     c                   OTHER
080000990302     C                   movel     §TADdesc      POCtad
080100990302     c                   ENDSL
080200080311      * verifico il tipo anomalia
080300080311     c                   clear                   prttad
080400080311     c                   if        §tadragr = 'M'
080500080312     c                   if        Wclsflo7 =  *blank
080600080311     c                   eval      prttad = 'smarrimento.'
080700080311     c                   else
080800080311     c                   eval      prttad = 'is missing.'
080900080311     c                   endif
081000080311     c                   else
081100080312     c                   if        Wclsflo7 =  *blank
081200080311     c                   eval      prttad = 'avaria.'
081300080311     c                   else
081400080311     c                   eval      prttad = 'was damaged.'
081500080311     c                   endif
081600080312     c                   endif
081700080312      *
081800981123      *
081900980729      * Descrizione anomalia
082000990224     c                   clear                   W900A
082100980729     c                   exsr      DESTAD
082200990224     c                   movel     W035a         POCdan
082300100709      *
082400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
082500100709     c                   if        Not *in02   and   Not *in20
082600100709     c                   eval      TxtInOut  = ds_DN66PO
082700100709     c                   exsr      sr_xChkChar
082800100709     c                   if        esito = '1'
082900100709     c                   eval      ds_DN66PO = TxtInOut
083000100709     c                   endif
083100100709     c                   endif
083200990224      *
083300990224      *  Stampa prima parte dell'oggetto
083400990310     c                   exsr      PRT_testat
083500080218     c  N20              WRITE     DN66PO
083600080218     c   20              WRITE     DN66MO
083700990224      *
083800990224      *  Stampa ulteriore descrizione anomalia
083900990224     c                   EXSR      PRTDESAGG
084000981123      *
084100981123      * Quantità danneggiate
084200981123      *      Peso
084300990223     c                   IF          DCTpkd > *zeros
084400981123     c                   move(P)   DCTpkd        w007a
084500020606     c     '0'           check     w007a         ww
084600020606     c                   eval      W007b = %subst(w007a:ww)
084700020606     c     ' '           checkr    w007b         ww
084800020606     c                   if        ww = 1
084900981123     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
085000981123     c                   else
085100020606     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(ww-1))
085200020606     c                                      +  ','  +  %subst(w007b:ww:1)
085300981123     c                   endif
085400981123     c                   ENDIF
085500981123      *      Colli
085600990223     c                   IF          DCTncn > *zeros
085700981123     c                   move      DCTncn        w005a
085800020606     c     '0'           check     w005a         ww
085900020327     c                   IF        Wclsflo7 <> *blanks
086000020606     c                   eval      Wcoman = ' Parcels '  +  %subst(W005a:ww)
086100990222     c                   ELSE
086200020606     c                   eval      Wcoman = ' Colli '  +  %subst(W005a:ww)
086300981123     c                   ENDIF
086400990222     c                   ENDIF
086500981123      *      Pezzi
086600981123     c                   IF        DCTnpz > *zeros
086700981123     c                   move      DCTnpz        w005a
086800020606     c     '0'           check     w005a         ww
086900020327     c                   IF        Wclsflo7 <> *blanks
087000020606     c                   eval      Wpzman = ' Pieces '  +  %subst(W005a:ww)
087100990222     c                   ELSE
087200020606     c                   eval      Wpzman = ' Pezzi '  +  %subst(W005a:ww)
087300980729     c                   ENDIF
087400990222     c                   ENDIF
087500981123      *
087600981123     c                   eval      POCqtd  =  %trimr(Wkgman)  +
087700981123     c                                        %trimr(Wcoman)  +  %trimr(Wpzman)
087800981123      *
087900981123      * Descrizione pezzi danneggiati
088000990224     c                   clear                   W900A
088100981123     c                   IF        DCTnpz > *zeros
088200981123     c                   EXSR      DESPZD
088300981123     c                   ENDIF
088400990224     c                   movel     W035a         POCdpz
088500100709      *
088600100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
088700100709     c                   if        Not *in02   and   Not *in20
088800100709     c                   eval      TxtInOut   = ds_DN66PO1
088900100709     c                   exsr      sr_xChkChar
089000100709     c                   if        esito = '1'
089100100709     c                   eval      ds_DN66PO1 = TxtInOut
089200100709     c                   endif
089300100709     c                   endif
089400980730      *
089500990224      *  Stampa seconda parte dell'oggetto
089600990310     c                   exsr      PRT_testat
089700080218     c  N20              WRITE     DN66PO1
089800080218     c   20              WRITE     DN66MO1
089900990224      *
090000990224      *  Stampa ulteriore descrizione pezzi danneggiati
090100990224     c                   EXSR      PRTDESAGG
090200980928
090300980729     C                   ENDSR
090400980928      *****************************************************************
090500980928      *  REPERIMENTO DESCRIZIONE EVENTO
090600980928      *****************************************************************
090700980928     C     DESEVE        BEGSR
090800980929      *
090900980929     C                   movel     'E'           Ktpd
091000981204     C                   movel     DCTAAE        W011A
091100981204     C                   move      DCTNEV        W011A
091200980929     C                   movel(P)  W011A         Knkt
091300980928     C                   clear                   Khim
091400980928     C                   clear                   Kdim
091500980929     C                   clear                   Kstd
091600980929     C                   clear                   Knks
091700980929     C                   movel     'D'           Ktrc
091800980928      *
091900980928     c                   clear                   yy
092000980929     c                   clear                   W35
092100980928      *
092200980929     c     kdcse         chain     fndcs000                           30
092300980928      *
092400980928     c     *in30         DOWeq     *off
092500980929     c     yy            ANDle     3
092600980928     c                   add       1             yy
092700980928     c                   movel     dcsnot        W35(yy)
092800980929     c     kdcse         reade     fndcs000                               30
092900980928     c                   ENDDO
093000980928      *
093100980928     C                   ENDSR
093200980729      *****************************************************************
093300980729      *  REPERIMENTO DESCRIZIONE ANOMALIA
093400980729      *****************************************************************
093500980729     C     DESTAD        BEGSR
093600980729      *
093700980929     C                   movel(P)  'C'           Ktpd
093800981215     C                   movel(P)  DSnumca       Knkt
093900980929     C                   movel(P)  'A'           Kstd
094000980929     C                   clear                   Khim
094100980929     C                   clear                   Kdim
094200980929     C                   clear                   Knks
094300980929     C                   movel     'D'           Ktrc
094400980929      *
094500980803     c                   clear                   yy
094600980730      *
094700980730     c     kdcs          chain     fndcs000                           30
094800980730      *
094900980730     c     *in30         DOWeq     *off
095000990224     c     yy            ANDlt     20
095100980918     c                   add       1             yy
095200990224     c                   movel     dcsnot        DES(yy)
095300980730     c     kdcs          reade     fndcs000                               30
095400980730     c                   ENDDO
095500980730      *
095600980729     C                   ENDSR
095700981123      *****************************************************************
095800981123      *  REPERIMENTO DESCRIZIONE PEZZI DANNEGGIATI
095900981123      *****************************************************************
096000981123     C     DESPZD        BEGSR
096100981123      *
096200981123      * Descrizione pezzi mancanti
096300981123     C                   movel(P)  'C'           Ktpd
096400981215     C                   movel(P)  DSnumca       Knkt
096500981123     C                   movel(P)  'P'           Kstd
096600990224     C                   clear                   Khim
096700990224     C                   clear                   Kdim
096800990224     C                   clear                   Knks
096900981123     c                   clear                   yy
097000981123      *
097100981123     c     kdcs          chain     fndcs000                           30
097200981123      *
097300981123     c     *in30         DOWeq     *off
097400990224     c     yy            ANDlt     20
097500981124     c                   add       1             yy
097600990224     c                   movel     dcsnot        DES(yy)
097700981123     c     kdcs          reade     fndcs000                               30
097800981123     c                   ENDDO
097900981123      *
098000981124     c                   ENDSR
098100990224      *****************************************************************
098200990224      *  STAMPA DESCRIZIONI AGGIUNTIVE NELL'OGGETTO
098300990224      *****************************************************************
098400990224     C     PRTDESAGG     BEGSR
098500990224      *
098600990224     c                   clear                   POCkos
098700020606     c                   DO        11            ww
098800990224      *
098900020606     c                   IF         DES70(ww)  <> *blanks
099000020606     c                   movel     DES70(ww)     POCkos
099100100709      *
099200100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
099300100709     c                   if        Not *in02   and   Not *in20
099400100709     c                   eval      TxtInOut   = ds_DN66PO2
099500100709     c                   exsr      sr_xChkChar
099600100709     c                   if        esito = '1'
099700100709     c                   eval      ds_DN66PO2 = TxtInOut
099800100709     c                   endif
099900100709     c                   endif
100000990224      *
100100990310     c                   exsr      PRT_testat
100200080218     c  N20              WRITE     DN66PO2
100300080218     c   20              WRITE     DN66MO2
100400990224      *
100500990224     c                   ENDIF
100600990224     c                   ENDDO
100700990224      *
100800990224     c                   ENDSR
100900980729      *****************************************************************
101000980729      *  REPERIMENTO DATI TIPO ANOMALIA
101100980729      *****************************************************************
101200980729     C     CHTAD         BEGSR
101300980729      *
101400980730     c                   clear                   tibs02ds
101500980729     c                   clear                   dtad
101600980729      *
101700980729     C                   movel     'C'           t02mod
101800980729     C                   movel     knsif         t02sif
101900980729     C                   movel     'TAD'         t02cod
102000981123     C                   movel(P)  Wtad          t02ke1
102100980729      *
102200980729     C                   CALL      'TIBS02R'
102300980729     C                   PARM                    KPJBA
102400980730     C                   PARM                    TIBS02DS
102500980729      *
102600980729     C                   movel     T02uni        DTAD
102700980729      *
102800980729     C                   ENDSR
102900090306      *****************************************************************
103000090306      *   NOTE
103100090306      *****************************************************************
103200090306     C     SUB_note      BEGSR
103300090306      *
103400090306     c                   eval      *in31 = *on
103500090306     c                   clear                   yy
103600090306     c                   clear                   W35
103700090306     c                   clear                   PTCtes
103800090306      *
103900090306     C                   movel(P)  'C'           Ktpd
104000090306     C                   movel(P)  DSnumca       Knkt
104100090306     C                   movel     'T'           Kstd
104200090306     C                   movel(P)  DFAle1        Knks
104300090306     C                   movel     'D'           Ktrc
104400090306     C                   clear                   Khim
104500090306     C                   clear                   Kdim
104600090306      *
104700090306     c     kdcse         chain     fndcs000                           30
104800090306     c     *in30         DOWeq     *off
104900090306      *
105000090306      * Ogni 3 rcd letti scrivo
105100090306     c                   IF        yy = 3
105200090306     c                   exsr      noteprt
105300090306     c                   ENDIF
105400090306      *
105500090306     c                   add       1             yy
105600090306     c                   movel     dcsnot        W35(yy)
105700090306     c     kdcse         reade     fndcs000                               30
105800090306     c                   ENDDO
105900090306      *
106000090306      * Scrivo rcd residui
106100090306     c                   IF        yy > *zeros
106200090306     c                   exsr      noteprt
106300090306     c                   ENDIF
106400090306      *
106500090306     C                   ENDSR
106600090306      *****************************************************************
106700090306      *   STAMPA NOTE
106800090306      *****************************************************************
106900090306     C     NOTEPRT       BEGSR
107000090306      *
107100090306     c                   eval      PTCtes = W35(1)  +  W35(2)  +  W35(3)
107200090306     c                   IF        PTCtes <> *blanks
107300090306      *
107400090306      * La prima volta lascio uno spazio
107500090306     c                   If        *IN31 = *ON
107600090306     c                   exsr      PRT_spazio
107700090306     c                   eval      *in31 = *off
107800090306     c                   Endif
107900090306      *
108000090306     c                   exsr      WRTtesto
108100090306      *
108200090306     c                   clear                   yy
108300090306     c                   clear                   W35
108400090306     c                   clear                   PTCtes
108500090306     c                   ENDIF
108600090306      *
108700090306     C                   ENDSR
108800980730      *****************************************************************
108900980730      *   TESTO
109000980730      *****************************************************************
109100980730     C     SUB_testo     BEGSR
109200080526
109300080219      * Determino il tipo di risarcimento da applicare al cliente
109400080219     c                   EXSR      SUB_TIPRIS
109500981123
109600981124      *  Lascio 2 righe dall'oggetto prima di stampare il testo
109700050303      *  =====> 1 riga !
109800990310     c                   exsr      PRT_spazio
109900981123
110000090306
110100090306      *  Stampo eventuali note aggiuntive
110200090306     c                   EXSR      SUB_NOTE
110300090306
110400990302      *  Imposto posizioni iniziale e finali
110500020327     c                   IF          Wclsflo7 <> *blanks
110600160701     c                   eval      Wdaxx = 16
110700160701     c                   eval      Waxx  = 28
110800990302     c                   ELSE
110900080219     c                   eval      Wdaxx = 1
111000160701     c                   eval      Waxx  = 15
111100990302     c                   ENDIF
111200981123
111300990302     c     Wdaxx         DO        Waxx          XX
111400981123     c                   clear                   PTCtes
111500980803      *
111600080219     c                   SELECT
111700080219      *
111800080219     c                   WHEN      %subst(TES1(xx):1:5) = 'XJUMP'
111900080219     c                   exsr      WRTtesto
112000080219      *
112100080219     c                   WHEN      %subst(TES1(xx):1:5) = 'XTESP'
112200080219     c                   exsr      SUB_tstpar
112300080219      *
112400080219      *
112500080219     c                   OTHER
112600981124     c                   movel(P)  TES1(xx)      PTCtes
112700990223      *       inserisco variabili nel testo standard
112800990302     c                   exsr      SUB_vartst
112900990223     c                   exsr      WRTtesto
113000990223      *
113100080219      *
113200080219     c                   ENDSL
113300980730     c                   ENDDO
113400041123      *  se non ho inviato via e-mail stampo e va via postel
113500041123     c                   if        not *in20 and not *in02
113600041118      *
113700041118     c                   movel     kcli          W0030
113800041118     c                   move      kcli          W0040
113900041118      * chiamo il trul per il recupero dell'indirizzo del mittente
114000041118     c                   clear                   trul44ds
114100080219     c                   eval      d44pgm = 'FIDN66R'
114200041118     c                   eval      d44po  = W0030
114300041118     c                   call      'TRUL44R'
114400041118     c                   parm                    trul44ds
114500050902     c                   parm                    trtcm1ds
114600041123      *
114700041123      * imposto d44mit con doppia @
114800041123     c                   clear                   wlast
114900041123     c                   clear                   wlblank
115000041123     c                   clear                   wd44mit
115100041123     c                   eval      Wlast = %scan('@':d44mit)
115200041123     c                   eval      Wlblank = %scan(' ':d44mit)
115300041123     c                   eval      wd44mit = %subst(d44mit:1:Wlast) + '@' +
115400041123     c                             %subst(d44mit:wlast+1:Wlblank)
115500041123     c
115600041118      *
115700041118      *
115800041118      *
115900041118     c                   if        w0040 <> 8888 and w0040 <> 9999
116000051013     c                                           and w0040 <> *zeros
116100041118      *
116200041118     c                   If        not *in08
116300041118     c                   movel(p)  TESM(1)       PTCtes
116400041118     c                   else
116500041118     c                   movel(p)  TESM(3)       PTCtes
116600041118     c                   endif
116700041118      *
116800041118      *  Lascio 2 righe
116900041118     c                   exsr      PRT_spazio
117000041118     c                   exsr      WRTtesto
117100041118     c                   If        not *in08
117200041118     c                   movel(P)  TESM(2)       PTCtes
117300041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
117400041118     c                   eval      %subst(ptctes:66:7) = %char(Kcli)
117500041118     c                   else
117600041118     c                   movel(P)  TESM(4)       PTCtes
117700041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
117800041118     c                   eval      %subst(ptctes:73:7) = %char(Kcli)
117900041118     c                   endif
118000041118      *
118100041118     c                   exsr      WRTtesto
118200041118     c                   endif
118300041118
118400041118     c                   endif
118500980730      *
118600980730     C                   ENDSR
118700990302      *****************************************************************
118800990302      *   SCRIVO TESTO
118900990302      *****************************************************************
119000990302     C     WRTtesto      BEGSR
119100080526      *
119200080526      * se è la terza riga del testo di avviso danno imposto la data della stampa
119300080526      * di presunto avviso danno
119400080526     c                   If        xx = 3
119500080526     c                   eval      %subst(PTCtes:47:10)  = wdatapreavv
119600080526     c                   endif
119700080526      * inglese
119800160701     c                   If        xx = 18
119900080526     c                   eval      %subst(PTCtes:35:10)  = wdatapreavv
120000080526     c                   endif
120100100709      *
120200100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
120300100709     c                   if        Not *in02   and   Not *in20
120400100709     c                   eval      TxtInOut  = ds_DN66PT
120500100709     c                   exsr      sr_xChkChar
120600100709     c                   if        esito = '1'
120700100709     c                   eval      ds_DN66PT = TxtInOut
120800100709     c                   endif
120900100709     c                   endif
121000080526
121100071206
121200990310     c                   exsr      PRT_testat
121300080219     C  N20              WRITE     DN66PT
121400080219     C   20              WRITE     DN66MT
121500990302      *
121600990302     C                   ENDSR
121700990302      *****************************************************************
121800990302      *   TESTO PARTICOLARE PER TIPO RISARCIMENTO
121900990302      *****************************************************************
122000990302     C     SUB_tstpar    BEGSR
122100990302
122200990302      *  Ricavo le coordinate del testo legato al tipo risarcimento
122300990302     c                   z-add     1             yy
122400990302     c     Wtiporis      lookup    COTP(yy)                               30
122500990302     c   30              movel     COSP(yy)      Wrisdaa
122600990302      *
122700990302      *  Imposto posizioni iniziale e finali
122800020327     c                   IF          Wclsflo7 <> *blanks
122900990302     c                   eval      Wdayy = WrisGBda
123000990302     c                   eval      Wayy  = WrisGBa
123100990302     c                   ELSE
123200990302     c                   eval      Wdayy = WrisITAda
123300990302     c                   eval      Wayy  = WrisITAa
123400990302     c                   ENDIF
123500990302
123600990302     c     Wdayy         DO        Wayy          YY
123700990302      *
123800990302     c                   clear                   PTCtes
123900990302     c                   IF        %SUBST(TESP(yy):1:5)  <>  'XJUMP'
124000990302     c                   movel     TESP(yy)      PTCtes
124100990302      *    Inserisco variabili
124200990302     c                   exsr      INStstpar
124300990302     c                   ENDIF
124400990302      *
124500990302     c                   exsr      WRTtesto
124600990302      *
124700990302     c                   ENDDO
124800990302      *
124900990302     C                   ENDSR
125000990224      *****************************************************************
125100990224      *   INSERIMENTO VARIABILI NEL TESTO PARTICOLARE PER TIPO RIMBORSO
125200990224      *****************************************************************
125300990224     C     INStstpar     BEGSR
125400990224
125500010906     c                   exsr      Edit
125600990302
125700990224     c                   SELECT
125800020125
125900060310      *    TIPO RIMBORSO "A"  RCV 450/85
126000020125     c                   WHEN      YY = 2
126100071207     c                   eval      %subst(PTCtes:33:3)  = §GEDDBA
126200071207     c                   eval      %subst(PTCtes:37:9) =
126300020125     c                                %TRIM(%subst(OedNUMAN:1:9))
126400990224
126500990302      *    TIPO RIMBORSO "B"  Elevazione limite risarcibile
126600050216     c                   WHEN      YY = 4
126700071207     c                   eval      %subst(PTCtes:35:3)  = §GEDDBA
126800071207     c                   eval      %subst(PTCtes:39:10) =
126900010907     c                                %TRIM(%subst(OedNUMAN:1:10))
127000990224
127100990302      *    TIPO RIMBORSO "C"  Mandato per conto
127200050216     c                   WHEN      YY = 5
127300080312     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
127400080312     c                   eval      %subst(PTCtes:47:10) =
127500010907     c                                %TRIM(%subst(OedNUMAN:1:10))
127600080312     c                   eval      %subst(PTCtes:60:10) = Wdfvm
127700990224
127800990224      *    TIPO RIMBORSO "D"  CMR
127900050216     c                   WHEN      YY = 8 and  §GEcmr > *zeros
128000010907     c                   eval      WCMR  =    'D.S.P.'
128100010907     c                                      +  %subst(OedNumAN:1:6)
128200080312     c                   eval      %subst(PTCtes:64:16) = WCMR
128300990224
128400990302      *    TIPO RIMBORSO "F"  Valore complessivo in bolla
128500050216     c                   WHEN      YY = 12
128600080312     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
128700080312     c                   eval      %subst(PTCtes:47:10) =
128800010907     c                                %TRIM(%subst(OedNUMAN:1:10))
128900990302
129000060310      *    TIPO RIMBORSO "G"  RCV  286/05
129100060310     c                   WHEN      YY = 16
129200071207     c                   eval      %subst(PTCtes:59:3)  = §GEDDBA
129300071207     c                   eval      %subst(PTCtes:63:9) =
129400060310     c                                %TRIM(%subst(OedNUMAN:1:9))
129500060310
129600020130      *    inglese TIPO RIMBORSO "A"  RCV
129700060315     c                   WHEN      YY = 21
129800050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
129900050216     c                   eval      %subst(PTCtes:22:9) =
130000020130     c                                %TRIM(%subst(OedNUMAN:1:9))
130100020130
130200990302      *    inglese TIPO RIMBORSO "B"  Elevazione limite risarcibile
130300060315     c                   WHEN      YY = 23
130400050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
130500050216     c                   eval      %subst(PTCtes:22:10) =
130600060310     c                                %TRIM(%subst(OedNUMAN:1:10))
130700990302
130800990302      *    inglese TIPO RIMBORSO "C"  Mandato per conto
130900060315     c                   WHEN      YY = 24
131000080312     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
131100080312     c                   eval      %subst(PTCtes:51:10) =
131200010907     c                                %TRIM(%subst(OedNUMAN:1:10))
131300080312     c                   eval      %subst(PTCtes:67:10) = Wdfvm
131400990302
131500990302      *    inglese TIPO RIMBORSO "D"  CMR
131600060315     c                   WHEN      YY = 26  and  §GEcmr > *zeros
131700010907     c                   eval      WCMR  =    'D.S.P.'
131800010907     c                                      +  %subst(OedNumAN:1:6)
131900080312     c                   eval      %subst(PTCtes:60:16) = WCMR
132000990302
132100990302      *    inglese TIPO RIMBORSO "F"  Valore complessivo in bolla
132200060315     c                   WHEN      YY = 29
132300080312     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
132400080312     c                   eval      %subst(PTCtes:51:10) =
132500010907     c                                %TRIM(%subst(OedNUMAN:1:10))
132600990224
132700060310      *    inglese TIPO RIMBORSO "G"  RCV  286/05
132800060315     c                   WHEN      YY = 33
132900071214     c                   eval      %subst(PTCtes:80:3)  = §GEDDBA
133000071214     c                   eval      %subst(PTCtes:84:9) =
133100060310     c                                %TRIM(%subst(OedNUMAN:1:9))
133200060310
133300990224     c                   ENDSL
133400990224
133500990224     C                   ENDSR
133600990302      *****************************************************************
133700990302      *   INSERISCO VARIABILI NEL TESTO
133800990302      *****************************************************************
133900990302     C     SUB_vartst    BEGSR
134000990302      *
134100050208      *    Inserisco nome società
134200160701     c                   IF        XX = 28          or                          inglese
134300160701     c                             XX = 15                                      italiano
134400990302     c                   eval      %subst(PTCtes:52:20) = %TRIM(Wsocie)
134500990302     c                   endif
134600990302      *
134700990302     C                   ENDSR
134800080219      *****************************************************************
134900080219      *   DETERMINO IL TIPO RISARCIMENTO
135000080219      *****************************************************************
135100080219     C     SUB_tipris    BEGSR
135200080219
135300080219     c                   clear                   wImporto
135400080219     c                   clear                   wNrDec
135500080219     c                   clear                   wLungh
135600080219
135700080219      * Se PA EXPORT o PF IMPORT, non DPD,
135800080219      *    abbiamo il tipo risarcimento "D"
135900080219      *
136000080219     C                   If        ( (§DCTtisp = 'E' and  §DCTport = 'A') OR
136100080219     C                               (§DCTtisp = 'I' and  §DCTport = 'F'
136200080219     c                                               and  §DCTeuro <> 'D') )
136300080219      *
136400080219     c                   movel(P)  'D'           Wtiporis
136500080219     c                   z-add     §GEcmr        WImporto
136600080219     c                   z-add     3             wNrDec
136700080219     c                   z-add     6             wLungh
136800080219   x1c                   ELSE
136900080219
137000080219      * Se è esposto il valore in bolla (non calcolato dalla fatturazione)
137100080219      *   tipo risarcimento "F"
137200080219    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
137300080219     c                   movel(P)  'F'           Wtiporis
137400080219     c                   if        TASias <> *zeros and TASvas = *blanks
137500080219     c                   movel     'ITL'         TASvas
137600080219     c                   endif
137700080219     c                   z-add     TASias        W0133
137800080219     c                   movel     TASvas        W003a
137900080219     C                   if        TASvas <> §gedDBA
138000080219     C                   exsr      srCMB
138100080219     c                   endif
138200080219     c                   z-add     W0133         wImporto
138300080219     c                   z-add     §cvDEC        wNrDec
138400080219     c                   z-add     10            wLungh
138500080219   x2c                   ELSE
138600080219      *
138700080219      * Cliente assicurato
138800080219    3c                   IF        Wfndpt = 'S'
138900080219      *   Importo Assicurato
139000080219    4c                   IF        DPTvlm > *zeros
139100080219     c                   z-add     DPTvlm        W0133
139200080219     c                   movel     DPTvvm        W003a
139300080219     c                   IF        DPTvvm <> §gedDBA
139400080219     C                   exsr      srCMB
139500080219     c                   ENDIF
139600080219     c                   z-add     W0133         WImporto
139700080219     c                   z-add     §cvDEC        wNrDec
139800080219     c                   z-add     10            wLungh
139900080219   -4c                   ENDIF
140000080219      *   Decodifica tipo valore
140100080219     c                   SELECT
140200080219     c                   WHEN      DPTfvm = '1'
140300080219     c                   movel(P)  'collo'       Wdfvm
140400080219     c                   WHEN      DPTfvm = '2'
140500080219     c                   movel(P)  'spedizione'  Wdfvm
140600080219     c                   WHEN      DPTfvm = '3'
140700080219     c                   movel(P)  'Kg merce'    Wdfvm
140800080219     c                   OTHER
140900080219     c                   clear                   Wdfvm
141000080219     c                   ENDSL
141100080219      *   Tipo Risarcimento: "C" mandato per conto,
141200080219      *                      "B" elevazione del limite risarcibile
141300080219      *
141400080219    4c                   IF        DPTftc = '8'  or  DPTftc  =  'C'
141500080219     c                             or DPTftc = 'd'
141600080219     c                   movel     'C'           Wtiporis
141700080219     c                   ELSE
141800080219      *   l'elevazione limite risarcibile
141900080219     c                   movel     'B'           Wtiporis
142000080219      *
142100080219   -4c                   ENDIF
142200080219      *
142300080219   -3c                   ENDIF
142400080219
142500080219      * Se non assicurato e non è un evento fortuito (tipo risarcimento
142600080219      *  "E") si applica la legge 450 (tipo risarcimento "A") o il CMR
142700080219      *  (tipo risarcimento "D")  se PA IMPORT o PF EXPORT o DPD EXPORT
142800080219      *  o FEDEX EXPORT
142900080219     c                   SELECT
143000080219     c                   WHEN      Wtiporis <> *blanks
143100080219     c                   WHEN          (§DCTeuro =  'X'
143200080219     c                              or  §DCTeuro =  'P')
143300080219     c                             and  §DCTtisp <> *blanks
143400080219     c                             or   §DCTeuro =  'D'
143500080219     c                             and  §DCTtisp =  'E'
143600080219     c                             or   §DCTeuro =  'F'
143700080219     c                             and  §DCTtisp =  'E'
143800080219     c                   movel(P)  'D'           Wtiporis
143900080219     c                   z-add     §GEcmr        WImporto
144000080219     c                   z-add     3             wNrDec
144100080219     c                   z-add     6             wLungh
144200080219     c                   OTHER
144300080219      * Verifico il tipo di limite risarcibile in base al flag in dctflo
144400080219     c                   If        §dcttlrp = ' '
144500080219     c                   movel(P)  'A'           Wtiporis
144600080219     c                   z-add     6,2           WImporto
144700080219     c                   endif
144800080219     c                   If        §dcttlrp = '6'
144900080219     c                   movel(P)  'H'           Wtiporis
145000080219     c                   z-add     6,2           WImporto
145100080219     c                   endif
145200080219     c                   If        §dcttlrp = '1'
145300080219     c                   movel(P)  'G'           Wtiporis
145400080219     c                   z-add     1             WImporto
145500080219     c                   endif
145600080219     c                   z-add     2             wNrDec
145700080219     c                   z-add     6             wLungh
145800080219     c                   ENDSL
145900080219      *
146000080219   -2c                   ENDIF
146100080219   -1c                   ENDIF
146200080219      *
146300080219     C                   ENDSR
146400020124      *****************************************************************
146500020124      *   SCRIVO NOTE
146600020124      *****************************************************************
146700020124     c     wrtdcs        begsr
146800020124
146900020124     c                   clear                   dcspno
147000020124
147100020124     c                   eval      ktpd='C'
147200020124     c                   eval      knkt=dsnumca
147300020124     c                   eval      kstd=' '
147400020124     c                   eval      kdim=dateu
147500020124     c                   movel     wora          khim
147600080311     c                   movel     wfscl         knks
147700020124     c                   eval      ktrc='N'
147800020124
147900020124      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA se esiste
148000020124     c     kdcsp         setgt     fndcs01l
148100020124     c     kdcse         readpe    fndcs01l
148200020124
148300020124     c                   eval      dcstpd = ktpd
148400020124     c                   eval      dcsnkt = knkt
148500020124     c                   eval      dcsstd = kstd
148600020124     c                   eval      dcsdim = kdim
148700020124     c                   eval      dcshim = khim
148800020124     c                   eval      dcsnks = knks
148900020124     c                   eval      dcstrc = ktrc
149000020124     c                   eval      dcsatb = ' '
149100020124
149200020124     c                   move      knmus         dcspru
149300020125     c                   if        simfel <> *zeros
149400020124     c                   movel     simfel        dcspos
149500020125     c                   else
149600020125     c                   eval      dcspos = 046
149700020125     c                   endif
149800020124     c                   clear                   dcsstn
149900020124     c                   clear                   dcsft1
150000020124     c                   clear                   dcsdt1
150100020124
150200020124      * Scrivo 2 note xchè su una non ci sto con tutta la frase
150300020124     c                   add       1             dcspno
150400020124     c                   clear                   dcsnot
150500020205     c                   eval      dcsnot = 'Ins. Automatico Pratica Franchigia'
150600020124     c                   write     fndcs000
150700020124
150800020124     c                   endsr
150900980804      *****************************************************************
151000980804      *   SCRIVO STORICO FASI
151100980804      *****************************************************************
151200980804     C     WRTdcf        BEGSR
151300980804      *
151400980804     c                   clear                   fndcf000
151500980804      *
151600080311     c                   z-add     DCTaac        DCFaac
151700080311     c                   z-add     DCTfil        DCFfil
151800080311     c                   z-add     DCTnca        DCFnca
151900980804     c                   movel     DCTptr        DCFptr
152000980804     c     *ISO          movel     DATA_oggi     DCFdfc
152100980804     c                   movel     w0140         DCFhfc
152200080311     c                   z-add     46            DCFfev
152300980804     c                   movel     knmus         DCFpru
152400980907     c                   clear                   DCFftr
152500980907     c                   clear                   DCFdtr
152600080311     c                   z-add     wfscl         DCFfca
152700041117      * Se Fase 545 Valorizzo l'invio
152800080311     c                   clear                   ddcf01
152900041117     c                   eval      §dcfinvi = invio
153000041117      *
153100020124     c                   movel     ddcf01        dcflet
153200020124
153300980804      *
153400980804     c                   write     fndcf000
153500980804      *
153600980804     C                   ENDSR
153700080312      *****************************************************************
153800080312      *   AGGIORNA File importi CA
153900080312      *****************************************************************
154000080312     C     WRTdcl        BEGSR
154100080312
154200080312     c     knca          chain     fndcl01l
154300080312
154400080312     c                   if        not %Found(fndcl01l)
154500080312     c                   clear                   fndcl000
154600080312     c                   movel     x_dctaac      Dclaac
154700080312     c                   movel     x_dctfil      Dclfil
154800080312     c                   z-add     x_dctnca      Dclnca
154900080312     c                   endif
155000080312     c* importo concordato
155100080312     c                   z-add     x_dctipv      dclipr
155200080312     c                   movel     x_dctvpv      dclvpr
155300080626     c                   z-add     x_dctipv      dclipl
155400080626     c                   movel     x_dctvpv      dclvpl
155500080312     c                   clear                   dclftr
155600080312     c*
155700080312     c                   If        %found(fndcl01l)
155800080312     c                   update    fndcl000
155900080312     c                   else
156000080312     c                   write     fndcl000
156100080312     c                   endif
156200080312     c*
156300080312     c                   endsr
156400080312      *
156500980804      *****************************************************************
156600980804      *   AGGIORNA TESTATA C.A.
156700980804      *****************************************************************
156800981124     C     WRTdct        BEGSR
156900981124      *
157000080311     c                   z-add     wfscl         DCTfca
157100980804      *
157200980804      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
157300980804     c                   CLEAR                   FIDN05DS
157400980804     c                   movel     'F'           I05MOD
157500080311     c                   z-add     190           I05fca
157600981204     c                   movel     DCTfpr        I05fpr
157700980804     c                   movel     DCTptr        I05tpc
157800981026     c                   if        DCTnev > *zeros
157900980804     c                   movel     'E'           I05fpe
158000980804     c                   else
158100980804     c                   movel     'N'           I05fpe
158200980804     c                   endif
158300981030     c                   if        DCTnde > *zeros
158400981030     c                   movel     'D'           I05fde
158500981102     c                   else
158600981102     c                   movel     'N'           I05fde
158700981030     c                   endif
158800980804     c                   movel     'O'           I05ffs
158900980804     c     *iso          movel     DATA_oggi     i05dta
159000981204     c                   move      DCTtad        i05tad
159100981125      * Valorizzo numero CA
159200981125     c                   z-add     DCTaac        i05aac
159300981125     c                   z-add     DCTfil        i05fil
159400981125     c                   z-add     DCTnca        i05nca
159500980804
159600980804     C                   CALL      'FIDN05R'
159700980804     c                   PARM                    KPJBA
159800980804     c                   PARM                    FIDN05DS
159900980804
160000980804     c                   IF        O05err = *blanks
160100980804     c                   movel     O05REC        DS_FNDFA
160200980804     c                   SELECT
160300980804     c                   WHEN      DFAgfs = 'P'
160400981229     c                   movel     §DCTlnpc      dctgfc
160500981023     c                   WHEN      DFAgfs = 'A'
160600981023     c                   z-add     DCTlna        dctgfc
160700981026     c                   WHEN      DFAgfs = 'S'
160800981026     c                   z-add     46            dctgfc
160900980804     c                   OTHER
161000981023     c                   z-add     DCTfil        dctgfc
161100981027     C                   ENDSL
161200980804     c                   ENDIF
161300980804      *
161400981222     c                   clear                   dctft1
161500981222     c                   clear                   dctft2
161600020124
161700020124     c                   movel     ddct01        dctflo
161800980804      *
161900980804     c                   EXCEPT    AGGDCT
162000980804      *
162100980804     C                   ENDSR
162200990310      *****************************************************************
162300990310      *   STAMPO spazio
162400990310      *****************************************************************
162500990310     C     PRT_SPAZIO    BEGSR
162600990310      *
162700990310     c                   exsr      PRT_testat
162800080218     C  N20              WRITE     DN66P0
162900080218     C   20              WRITE     DN66M0
163000990310      *
163100990310     c                   endsr
163200990310      *****************************************************************
163300990310      *   STAMPO TESTATA
163400990310      *****************************************************************
163500990310     C     PRT_TESTAT    BEGSR
163600990310      *
163700990310     c                   if         *IN01 = *ON
163800080218     c                   WRITE     DN66PP
163900990310     c                   EVAL      *in01 = *off
164000990310     c                   endif
164100041117      *
164200041117     c                   if         *IN21 = *ON  AND *IN20
164300080218     c                   WRITE     DN66MP
164400041117     c                   EVAL      *in21 = *off
164500041117     c                   endif
164600990310      *
164700990310     c                   endsr
164800981007      *****************************************************************
164900981007      *   AZZERO CAMPI PRTF
165000981007      *****************************************************************
165100981007     C     INZPRTF       BEGSR
165200981007      *
165300080215     c                   clear                   DN66pI
165400080215     c                   clear                   DN66pO
165500080215     c                   clear                   DN66pO1
165600080215     c                   clear                   DN66pO2
165700080215     c                   clear                   DN66pT
165800041118      *
165900041118     c                   if        *in20
166000080215     c                   close     fidn66pm
166100041118     c                   endif
166200981007      *
166300981007     C                   ENDSR
166400981027      *****************************************************************
166500981027      *   IMPOSTO TIPO MODULO E STAMPANTE
166600981027      *****************************************************************
166700981027     C     IMPPRT        BEGSR
166800981027      *
166900080218     c                   Clear                   COMMAND
167000081030      *  Ristampa
167100081030     c                   IF        *in02 = *on
167200081030      *          Bartolini
167300081030     c                   movea     CMD(1)        COMMAND
167400081030     c                   eval      %subst(COMMAND:60:10) = §TLDstp1
167500081030     c                   z-add     90            lung
167600081030     c                   call      'QCMDEXC'
167700081030     c                   parm                    COMMAND
167800081030     c                   parm                    LUNG
167900081030      *
168000081030     c                   else
168100981203      *
168200981214      *  PT-Postel Bartolini
168300081030     c                   movea     CMD(2)        COMMAND
168400041118     c                   z-add     90            lung
168500981203     c                   call      'QCMDEXC'
168600981203     c                   parm                    COMMAND
168700981203     c                   parm                    LUNG
168800081030     c                   endif
168900981027      *
169000981113      *  Apro file di stampa
169100080218     c                   open      FIDN66P
169200981113      *
169300981027     C                   ENDSR
169400981027      *****************************************************************
169500981027      *   AGGANCIO TABELLA TIPO TESTO
169600981027      *****************************************************************
169700981027     C     DECTLD        BEGSR
169800981027      *
169900981027     c                   clear                   tibs02ds
170000981027     c                   clear                   dtld
170100981027      *
170200981027     C                   movel     'C'           t02mod
170300981027     C                   movel     knsif         t02sif
170400981124     C                   movel     'TLD'         t02cod
170500981204     C                   movel(P)  DFAle1        t02ke1
170600981027      *
170700981027     C                   CALL      'TIBS02R'
170800981027     C                   PARM                    KPJBA
170900981027     C                   PARM                    TIBS02DS
171000981027      *
171100981027     C                   movel     T02uni        DTLD
171200981027      *
171300981027     C                   ENDSR
171400981203      *****************************************************************
171500981230      * DECODIFICO NAZIONE
171600981203      *****************************************************************
171700981230     C     CTRNAZ        BEGSR
171800981203      *
171900981230     C                   clear                   DS15
172000981230     C                   movel     '15'          KCOD
172100981230     C                   movel(P)  WNAZ          KKEY
172200981203     C     KTAB          CHAIN     TABEL00f                           30
172300981203      *
172400981230     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
172500981230     C                   movel     TBLUNI        DS15
172600981230     C                   endif
172700981203      *
172800981203     C                   ENDSR
172900981230      *****************************************************************
173000981230      * CONTROLLO DIVISA
173100981230      *****************************************************************
173200981230     C     CTRVLT        BEGSR
173300981230      *
173400981230     C                   MOVEL     'CV'          KCOD
173500990415     C                   MOVEL(P)  W003a         KKEY
173600981230     C     KTAB          CHAIN     TABEL00f                           30
173700981230      *
173800981230     C                   IF        *IN30 = *ON  or
173900981230     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
174000011026     C                   clear                   DSCV
174100981230     C                   ELSE
174200981230      *
174300981230     C                   movel     TBLUNI        DSCV
174400981230     C                   ENDIF
174500981230      *
174600011026     C                   ENDSR
174700010906
174800010906      *****************************************************************
174900010906      * CONVERSIONE IMPORTI IN MONETA CORRENTE
175000010906      *****************************************************************
175100010906     C     srCMB         BEGSR
175200010906      *
175300010906     C                   reset                   YEURCODS
175400010906      *
175500010906     C                   movel     W003a         yecDVI
175600010906     C                   z-add     W0133         yecIMI
175700010910     C                   movel     §gedDBA       yecDVO
175800010906      *
175900010906     C                   call      'YEURCO'
176000010906     C                   parm                    YEURCODS
176100010906      *
176200010906     C                   IF        yecESI  = ' '
176300010906     C                   z-add     yecIMO        W0133
176400011002     C                   movel     yecDVO        W003a
176500010906     C                   ENDIF
176600010906      *
176700010906     C                   ENDSR
176800010906
176900010906      ************************************************************
177000010907      * Editazione di un numero
177100010906      ************************************************************
177200010906     C     Edit          Begsr
177300010906     C*
177400010907     C                   reset                   XEDITDS
177500010906     C* numero da elaborare
177600010906     C* (WImporto ha 5 decimali, $SrcVar ha 0 decimali)
177700010906     C                   Select
177800010907     C                   When      wNrDec    = 0
177900010907     C                   eval      IedNUMERO = wImporto
178000010907     C                   When      wNrDec    = 1
178100010907     C                   eval      IedNUMERO = wImporto * 10
178200010907     C                   When      wNrDec    = 2
178300010907     C                   eval      IedNUMERO = wImporto * 100
178400010907     C                   When      wNrDec    = 3
178500010907     C                   eval      IedNUMERO = wImporto * 1000
178600010907     C                   When      wNrDec    = 4
178700010907     C                   eval      IedNUMERO = wImporto * 10000
178800010907     C                   When      wNrDec    = 5
178900010907     C                   eval      IedNUMERO = wImporto * 100000
179000010906     C                   Endsl
179100010906     C* numero decimali
179200010907     C                   eval      IedNRDEC  = wNrDec
179300010906     C* codice di editazione
179400010907     C                   eval      IedEDTCOD = '3'
179500010906     C* max lunghezza campo in output
179600010907     C                   eval      IedLENFLD = wLungh
179700010906     C*
179800010907     C                   call      'XEDIT'
179900010907     C                   parm                    XEDITDS
180000010906      *
180100010906     C                   ENDSR
180200010906
180300981123      *****************************************************************
180400981123      *   RICERCA CAPOCONTI
180500981123      *****************************************************************
180600981123     C     CAPOCON       BEGSR
180700981123      *
180800981123      *--- RICERCA CAPOCONTI
180900020606     C                   DO        50            ww
181000020606     C                   MOVE      TCU(ww)       TCUDS
181100981123     C                   IF        F56 = 'CG'
181200981123      *
181300981123     C     f4            COMP      '1'                                    21
181400981123     C     f4            COMP      '2'                                    22
181500981123     C     f4            COMP      '3'                                    23
181600981123     C     f4            COMP      '6'                                    27
181700981123      ** 1 CLIENTI   21    ** 2 FORNITORI 22    ** 3 AGENTI    23
181800981123     C     f3            comp      '0'                                242425
181900981123     C     f3            comp      'I'                                    26
182000981123      ** 0 ITALIA   25      ** 1 ESTERO   24    ** I CAPO CONTO IVA
182100981123     c                   if        *in21 = *on
182200020606     C   24              Z-ADD     KCU(ww)       KCE               4 0
182300020606     C   25              Z-ADD     KCU(ww)       KCI               4 0
182400981123     c                   end
182500981123     c                   if        *in22 = *on
182600020606     C   24              Z-ADD     KCU(ww)       KFE               4 0
182700020606     C   25              Z-ADD     KCU(ww)       KFI               4 0
182800981123     c                   end
182900981123     c                   if        *in23 = *on
183000020606     C   24              Z-ADD     KCU(ww)       KAE               4 0
183100020606     C   25              Z-ADD     KCU(ww)       KAI               4 0
183200981123     c                   end
183300020606     C   26              Z-ADD     KCU(ww)       KIVA              4 0
183400020606     C   27              Z-ADD     KCU(ww)       KBNA              4 0
183500981123      *
183600981123     C                   ENDIF
183700981123     C                   ENDDO
183800981123      *
183900981123     C                   ENDSR
184000100709      /free
184100100709       //--------------------------------------------------------------
184200100709       //?"Bonifica" campi in stampa da caratteri indesiderati         ?
184300100709       //--------------------------------------------------------------
184400100709       BEGSR  sr_xChkChar;
184500100709
184600100709         ChkNull = '1';
184700100709
184800100709         xChkChar ( TxtInOut :
184900100709                    ElencoChar :
185000100709                    TipoElenco :
185100100709                    CharSost :
185200100709                    Uppercase :
185300100709                    ChkNull :
185400100709                    CharNull :
185500100709                    Esito);
185600100709
185700100709       ENDSR;
185800100709      /end-free
185900981116      *****************************************************************
186000981116      *   ROUTINE INIZIALE
186100981116      *****************************************************************
186200981116     C     *INZSR        BEGSR
186300981116      *
186400981116      *
186500981116     C                   Z-ADD     1             CODUT
186600981116     C                   CALL      'X§PARUT'
186700981123     C                   PARM                    UT§DSE0F
186800981116      *
186900981116     C                   MOVEL     REC80         CNCR80
187000981124      *
187100981116      *  Ricerca capoconti
187200981116     C                   EXSR      CAPOCON
187300990222
187400010413      ***
187500010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
187600010413      ***
187700010413      *
187800010413      * recupero la moneta corrente
187900010413     C                   CLEAR                   tibs02ds
188000010413      *
188100010413     C                   MOVEL     'L'           T02TLA
188200010413     C                   MOVEL     'C'           T02MOD
188300010413     C                   MOVEL     KNSIF         T02SIF
188400010413     C                   MOVEL     'GED'         T02COD
188500010906     C                   MOVEL(P)  'DANNI'       T02KE1
188600010413      *
188700010413     C                   CALL      'TIBS02R'
188800010413     C                   PARM                    KPJBA
188900010413     C                   PARM                    tibs02ds
189000010413      *
189100010910     c                   movel     T02UNI        DGEDDN
189200011026      *
189300011026      * ...ed i relativi dati
189400011026     C                   movel     §gedDBA       W003a
189500011026     C                   exsr      ctrvlt
189600011026      *
189700010413      * recupero i valori nella moneta corrente
189800010413     C                   CLEAR                   tibs02ds
189900010413      *
190000010413     C                   MOVEL     'L'           T02TLA
190100010413     C                   MOVEL     'C'           T02MOD
190200010413     C                   MOVEL     KNSIF         T02SIF
190300010413     C                   MOVEL     'GEI'         T02COD
190400010910     C                   MOVEL     §GEDDBA       T02KE1
190500010413      *
190600010413     C                   CALL      'TIBS02R'
190700010413     C                   PARM                    KPJBA
190800010413     C                   PARM                    tibs02ds
190900010413      *
191000010413     c                   movel     t02uni        DGEI
191100990226      *
191200990226     c                   clear                   tibs02ds
191300990226     c                   clear                   dstd
191400990226      *
191500990226     C                   movel     'C'           t02mod
191600990226     C                   movel     knsif         t02sif
191700990226     C                   movel     'STD'         t02cod
191800990226     C                   movel(P)  1             t02ke1
191900010910     C                   movel(P)  §gedDBA       t02ke2
192000990226      *
192100990226     C                   CALL      'TIBS02R'
192200990226     C                   PARM                    KPJBA
192300990226     C                   PARM                    TIBS02DS
192400990226      *
192500990301     C                   movel     T02uni        DSTD
192600981116
192700981116     C                   TIME                    W0140
192800981116      *
192900981116     C                   MOVE      W0140         WDTGIO
193000981116     C                   MOVEL     W0140         WORA
193100981116      *
193200981116      * UDATE IN AAAAMMGG
193300981116     C                   Z-ADD     WDTGIO        G02DAT
193400981116     C                   MOVEL     *BLANK        G02ERR
193500981116     C                   CALL      'XSRDA8'
193600981116     C                   PARM                    WLBDAT
193700981116     C                   MOVEL     G02INV        DATEU
193800981116     C     *iso          MOVEL     DATEU         DATA_oggi
193900990316     C     *eur          movel     DATA_oggi     wdataoggi
194000080213
194100080215     C     Knca          klist
194200080215     C                   KFLD                    X_DCTaac
194300080215     C                   KFLD                    X_DCTfil
194400080215     C                   KFLD                    X_DCTnca
194500081029
194600081029     C     Kdct          klist
194700081029     C                   KFLD                    i00aac
194800081029     C                   KFLD                    i00fil
194900081029     C                   KFLD                    i00nca
195000001228      *
195100981123     C     Kdet          klist
195200981123     C                   KFLD                    DCTaae
195300981123     C                   KFLD                    DCTnev
195400021204      *
195500021204     C     Ktas4         klist
195600021204     C                   kfld                    DCTaas
195700021204     C                   kfld                    DCTlnp
195800021204     C                   kfld                    DCTnrs
195900021204     C                   kfld                    DCTnsp
196000021204     C                   kfld                    TA4trc
196100021204     c                   eval      TA4trc = 'A'
196200981116      *
196300981116     C     KDCS          klist
196400981116     C                   KFLD                    Ktpd
196500981116     C                   KFLD                    Knkt
196600981116     C                   KFLD                    Kstd
196700981116      *
196800981116     C     KDCSE         klist
196900981116     C                   KFLD                    KTPD
197000981116     C                   KFLD                    KNKT
197100981116     C                   KFLD                    KSTD
197200981116     C                   KFLD                    KDIM
197300981116     C                   KFLD                    KHIM
197400981116     C                   KFLD                    KNKS
197500981116     C                   KFLD                    KTRC
197600020124
197700020124     c     kdcsp         KLIST
197800020124     c                   KFLD                    ktpd
197900020124     c                   KFLD                    knkt
198000020124     c                   KFLD                    kstd
198100020124     c                   KFLD                    kdim
198200020124     c                   KFLD                    khim
198300020124     c                   KFLD                    knks
198400020124     c                   KFLD                    ktrc
198500020124     c                   KFLD                    prgnot
198600981116      *
198700981116     c     kbol          klist
198800080215     c                   kfld                    x_dctaas
198900080215     c                   kfld                    x_dctlnp
199000080215     c                   kfld                    x_dctnrs
199100080215     c                   kfld                    x_dctnsp
199200981116      *
199300981116     C     KTAB          KLIST
199400981116     C                   KFLD                    CODUT
199500981116     C                   KFLD                    KCOD
199600981116     C                   KFLD                    KKEY
199700990222      *
199800990222     C     KTABE         KLIST
199900990222     C                   KFLD                    CODUT
200000990222     C                   KFLD                    KCOD
200100981116
200200981116     C                   ENDSR
200300980804      *****************************************************************
200400980804     OFNDCT000  E            AGGDCT
200500980804     O                       DCTgfc
200600980804     O                       DCTfca
200700020124     o                       dctflo
200800980804      *****************************************************************
200900981210** CMD   (Lungh. 90)                                                                     *
201000081106OVRPRTF FIDN66P SPLFNAME(LET198) FORMTYPE(AVVDANNLT)  OUTQ(xxxxxxxxxx) HOLD(*YES)
201100080312OVRPRTF FIDN66P SPLFNAME(LET198) FORMTYPE(AVVDANNLT) OUTQ(PICCIONEDB/BARPICSTM)
201200080312OVRPRTF FIDN66PM FORMTYPE(AVVDANNLT) OUTQ(xxxxxxxxxx) USRDFNDTA('
201300981123** TES1     *            (Lungh. 100)                                                              *
201400080225Gentili signori,
201500080225XJUMP
201600080526Facendo seguito alla nostra comunicazione del xx/xx/xxxx siamo spiacenti di confermare
201700080526l'anomalia sopra descritta.
201800981123XJUMP
201900981123XTESP
202000981123XJUMP
202100080331Nello scusarci per lo spiacevole inconveniente, Vi informiamo che la somma liquidata sara'
202200160701corrisposta tramite bonifico bancario o tramite assegno di traenza inviato direttamente al
202300160701Vostro indirizzo.
202400981123XJUMP
202500080225Rimaniamo a completa disposizione per ogni eventuale ulteriore chiarimento.
202600981123XJUMP
202700080225Distinti saluti.
202800981124                                                   XXXXXXXXXXXXXXXXXXXX
202900080226Dear Sir or Madam,
203000080226XJUMP
203100080526Following our damage notice dated xx/xx/xxxx we regret to confirm that the a.m. parcel
203200080526is damaged.
203300990302XJUMP
203400990302XTESP
203500990302XJUMP
203600160701We apologise for the inconvenience caused and we would like to advice you that the amount
203700160701due will be paid either by bank transfer or by drawing cheque sent directly to Your address.
203800990302XJUMP
203900080404We are at your disposal for any further clarification and reamin with our best regards.
204000071217XJUMP
204100990302                                                   XXXXXXXXXXXXXXXXXXXX
204200990302** COTP: 1-3 cod.testo; COSP: 4-6  7-9 1o e ultimo elemento ITA, 10-12 13-15 1o e ultimo elemento GB
204300060310A  001002020021
204400060310B  003004022023
204500060310C  005007024025
204600060310D  008008026026
204700060310E  009011027028
204800060310F  012014029031
204900060310G  015017032034
205000060310H  018019035036
205100981123** TESP     *            (Lungh. 100)                                                              *
205200080312Il danno sara' risarcito nel rispetto del limite previsto dalla vigente L. 450/85
205300071207modificata dalla 162/93, ovvero xxx xxxxxxxxx per ogni Kg di merce perduta o danneggiata.
205400080312Il danno sara' risarcito nel rispetto del limite previsto dalla vigente L. 450/85
205500071207modificata dalla 162/93 elevato a xxx xxxxxxxxxx per ogni Kg di merce perduta o danneggiata.
205600080312Il danno sara' risarcito con il limite di xxx xxxxxxxxxx a xxxxxxxxxx, giusto mandato
205700080526conferitoci di assicurare per Vostro conto, contro tutti i rischi, le merci oggetto
205800080526della spedizione stessa.
205900080312Il danno sara' risarcito con il limite previsto dalla C.M.R. : xxxxxxxxxxxxxxxx
206000071217Il risarcimento non Vi potra' essere riconosciuto poiche', per le modalita' di accadimento,
206100071207rientra fra quelli considerati "fortuiti" dal Codice Civile /art. 1693 e 1694), non ravvisandosi
206200071207alcuna nostra responsabilita'.
206300080312Il danno sara' risarcito con il limite di xxx xxxxxxxxxx, valore complessivo riportato
206400080526nel DDT, giusto mandato conferitoci di assicurare per Vostro conto, contro tutti i rischi,
206500080526le merci oggetto della spedizione stessa
206600080312Il danno sara' risarcito nel rispetto del limite previsto dall'articolo 1696 c.c. cosi'
206700080526come modificato dal d.lgs 286/2005 ovvero non superiore a xxx xxxxxxxxx per ogni chilogrammo
206800080526di peso lordo della merce perduta o avariata.
206900080312Il danno sara' risarcito nel rispetto del limite di EUR 6,20 per ogni Kg di merce
207000071207perduta o danneggiata.
207100050216the compensation will be recognized you according to the present law L. 450/85 amended by
207200050216the 162/93, i.e. xxx xxxxxxxxx per each Kg merchandise lost or damaged.
207300050216the compensation will be recognized you according to the present law L. 450/85 amended by
207400050216the 162/93 up to xxx xxxxxxxxxx each Kg merchandise lost or damaged.
207500080312The damage will be refund within the limit of xxx xxxxxxxxxx each xxxxxxxxxx, based upon
207600071214your mandate to insure "all risks" your merchandise being the object of the shipment himself.
207700080312The damage will be refund within the limit of the C.M.R. : xxxxxxxxxxxxxxxx
207800080312The refund will not be paid since the events are classified under the "fortuitous
207900071214events" by the Civil Code (art. 1693 and 1694), and we have no liability for them.
208000080312The damage will be refund within the limit of xxx xxxxxxxxxx, that is the total amount
208100071217of the delivery note, as per the amount we were asked to insure on your behalf, against all risks,
208200071217of this shipment.
208300080312The damage will be refund within the limit fixed by the article 1696 c.c. as it was
208400071214modified by the legislative decree 286/2005 that is the refund will not exceed xxx xxxxxxxxx
208500071214per each kilo (gross weight) of the missing or damaged goods.
208600060310you will be refund within the limit of EUR 6,20 per each kilo (gross weight) of the missing or
208700060310damaged goods.
208800041118** TESM testo da stampare per richiesta via e-mail
208900041118Per ricevere le prossime comunicazioni via e-mail,Vi invitiamo a darne segnalazione a
209000041118                              indicando i seguenti dati : Codice xxxxxxx e-mail ___________________
209100041118Should you need the next information via e-mail, We ask you to be so kind and ask it at
209200041118                              specifying the following details : Codice xxxxxxx e-mail ____________
