000100100709     /*CMD ovrdbf file(FIDN35PS) tofile(*libl/FIDN35P) ovrscope(*calllvl)
000200100709     /*END dltovr file(FIDN35PS) lvl(*)
000300100709     /*END
000400010704
000500981125      *--------------------------------------------------------------------------*
000600981125      * Stampa e ristampa lettere                                                *
000700981125      *    A10 - Avviso Presunto Danno                                           *
000800981125      *--------------------------------------------------------------------------*
000900981214      * Chiudo la C.A. nel caso di: evento fortuito; PF Import Euroexpress       *
001000981214      *    PF Import Europolitan solo se responsabilità del partner              *
001100981214      *--------------------------------------------------------------------------*
001200981214
001300981214      ***************************************************************
001400981214      *   Legenda tipi rimborso
001500981214      ***************************************************************
001600981214      *   A = legge 450
001700981214      *   B = Elevazione limite risarcibile
001800981214      *   C = Mandato per conto
001900981214      *   D = Estero CMR
002000981214      *   E = Evento fortuito
002100981214      *   F = Importo da assicurare esposto in bolla
002200060310      *   G = Dlgs 286 dal 1/05/2006
002300060310      *   H = RCV dal 1/3/2006 al 30/04/2006
002400981214      ***************************************************************
002500050324      *
002600050324      * ? N.B. ?
002700050324      *   I segg. altri programmi gestiscono la suddetta legenda:
002800050324      *   - FIDN03R
002900050324      *   - FIDN47R1
003000050324      *
003100050324      ***************************************************************
003200980731
003300981124      ***************************************************************
003400981023      *  FASI C.A. FISSATE A PROGRAMMA
003500981124      ***************************************************************
003600071206      * 400 = Stampa Avviso Presunto Danno                  WFPD
003700981125      * 545 = Stampa Avviso Presunto Danno                  WFAPD
003800981203      * 660 = Invio Documenti a Consuldanni                 WFIDC
003900981125      * 900 = Chiusura C.A.                                 WFCHI
004000980731      ***************************************************************
004100980729
004200981215      ***************************************************************
004300981215      *  CAUSALI CHIUSURA FISSATE A PROGRAMMA
004400981215      ***************************************************************
004500981215      * 04 = Evento fortuito                                WCEVF
004600981215      * 06 = Gestita dall'uff. internazionale con Partner   WCINT
004700981215      ***************************************************************
004800981215
004900981124      ***************************************************************
005000980729      *  RIEPILOGO INDICATORI
005100980729      ***************************************************************
005200990224      * 01   - SALTO PAGINA 1o prtf
005300981125      * 02   - NON STAMPO PER POSTEL
005400981119      * 03   - STAMPO Evento
005500990224      * 04   - ALTA INTENSITA riga di testo
005600981203      * 07   - SALTO PAGINA 2o prtf
005700990222      * 08   - TESTO IN INGLESE
005800050303      * 09   - "All'attenzione di..."
005900981214      * 10   - ESEGUO apertura file solo 1° giro
006000020717      * 11   - ESEGUO stampa testo per uff. internazionale solo 1° giro e
006100020717      *                                                    stampa chiusura
006200021204      * 12   - STAMPO riferimento mittente ALFAnumerico
006300041110      * 20   - Invio Via E-Mail
006400041110      * 21   - Salto pagina prtf mail
006500050303      * 22~27- comodo in ricerca capoconti
006600050303      * 28   - Errore n° pratica (da FIDN20R)
006700020717      * 30   - COMODO
006800981124      * 31   - COMODO
006900050303      * 60   - Chiamata a QCMDEXC (ovrprtf) in errore
007000981214      ***************************************************************
007100980729
007200980730     H DECEDIT('0,') DATEDIT(*DMY/)
007300100709     h option(*nodebugio)
007400980729
007500981203     FFIDN00D   CF   E             WORKSTN  sfile(FI00S02:nrr1)
007600980804     fFNDCT01L  UF   E           K DISK
007700981124     fFNDCD01L  UF   E           K DISK
007800020124     fFNDCS01L  IF a E           K DISK
007900981124     fFNDFA01L  IF   E           K DISK
008000990415     fFNDPT01L  IF   E           K DISK
008100060619     fFNDET01L  UF   E           K DISK
008200990205     fFNDET02L  IF   E           K DISK    rename(fndet000:fndet2)
008300981124     fFNDCF01L  UF A E           K DISK
008400990809     fTITAS30C  IF   E           K DISK
008500981120     fTNTMD01L  IF   E           K DISK
008600021204     fTITA430C  IF   E           K DISK
008700980729     fAZORG01L  IF   E           K DISK
008800981124     fTABEL00F  IF   E           K DISK
008900020717     fFIDN35P   O    E             PRINTER oflind(*IN01) infds(PRTBAR) usropn
009000041110     fFIDN35PM  O    E             PRINTER oflind(*IN21) infds(PRTMAI) usropn
009100981214     fPRTF198   O    F  198        PRINTER OFLIND(*INOF)     USROPN
009200981204
009300990429     D W35             S             35    DIM(4)                               descr. tipo anomalia
009400981214     D NCA             S              1    DIM(105)                             Comodo per stampa
009500981222     D CAE             S             19    DIM(999)                             CA estero
009600980803      *----------------------------------
009700120404     D CMD             S             90    DIM(4)  CTDATA PERRCD(1)             OVRPRTF
009800050216      *--------------------------------------
009900050216      * posizioni testi schiera TES1
010000990302     D COTE            S              3    DIM(1) CTDATA PERRCD(1)              codice testo
010100990302     D COSC            s             12S 0 DIM(1) ALT(cote)                     coordinate schiera
010200050208      * In caso di modifica al numero elementi di TES1 attenzione a
010300050208      * reimpostare le costanti WTES1_* con le nuove posizioni
010400120412     D TES1            S            100    DIM(26) CTDATA PERRCD(1)             testo lettera 1 riga
010500120412     D wtes1_SA_G      S              2s 0 INZ(25)                              Saluti INGLESE
010600120412     D wtes1_SA_I      S              2s 0 INZ(12)                              Saluti Italiano
010700120412     D wtes1_SO_G      S              2s 0 INZ(26)                              Società INGLESE
010800120412     D wtes1_SO_I      S              2s 0 INZ(13)                              Società ITALIANO
010900050216      *--------------------------------------
011000050216
011100050216      *--------------------------------------
011200050216      * posizioni testi schiera TESP
011300060310     D COTP            S              3    DIM(8) CTDATA PERRCD(1)              codici testi partic.
011400060310     D COSP            s             12S 0 DIM(8) ALT(cotp)                     coord. schiera part.
011500050216      * In caso di modifica agli elementi di TESP bisogna
011600050216      * rivedere le sostituzioni effettuate nella sbr  INSTSTPAR
011700060310     D TESP            S            100    DIM(36) CTDATA PERRCD(1)             testi particolari
011800050216      *--------------------------------------
011900050216
012000050216      *--------------------------------------
012100050216      * posizioni testi schiera TESD
012200120404     D COTD            S              3    DIM(7) CTDATA PERRCD(1)              codici documenti
012300120404     D COSD            s             24S 0 DIM(7) ALT(cotd)                     coord. schiera doc.
012400050217      * In caso di modifica agli elementi di TESD bisogna
012500050217      * rivedere le sostituzioni effettuate nella sbr  SUB_VARDOC
012600120406     D TESD            S            100    DIM(43) CTDATA PERRCD(1)             documenti
012700050216      *--------------------------------------
012800050216
012900050216      *--------------------------------------
013000050216      * Schiera testo per mail
013100041118     D TESM            S            100    DIM(4)  CTDATA PERRCD(1)             testo lettera x mail
013200120404
013300120404      *--------------------------------------
013400120404      * Schiera testo per assicurazione BRT
013500120418     D TESACB          S            100    DIM(4)  CTDATA PERRCD(1)             testo lettera x ass.
013600980730      *----------------------------------
013700050216
013800120404     D KBAR            C                   CONST('BRT  S.p.A.')
013900980729      *----------------------------------
014000980730     D WLBDAT          DS                  INZ
014100980730     D  g02dat                 1      8  0
014200980730     D  g02inv                 9     16  0
014300980730     D  g02err                17     17
014400980730     D  g02tgi                18     22  0
014500980928      *
014600990302     D                 DS
014700990302     D  WtstITAda              1      3  0
014800990302     D  WtstITAa               4      6  0
014900990302     D  WtstGBda               7      9  0
015000990302     D  WtstGBa               10     12  0
015100990302     D  Wtstdaa                1     12  0
015200990302      *
015300990302     D                 DS
015400990302     D  WrisITAda              1      3  0
015500990302     D  WrisITAa               4      6  0
015600990302     D  WrisGBda               7      9  0
015700990302     D  WrisGBa               10     12  0
015800990302     D  Wrisdaa                1     12  0
015900990302      *
016000980731     D                 DS
016100990302     D  WdocITAda              1      3  0
016200990302     D  WdocITAa               4      6  0
016300990302     D  WdocITAdaH             7      9  0
016400990302     D  WdocITAaH             10     12  0
016500990302     D  WdocGBda              13     15  0
016600990302     D  WdocGBa               16     18  0
016700990302     D  WdocGBdaH             19     21  0
016800990302     D  WdocGBaH              22     24  0
016900990302     D  Wdocdaa                1     24  0
017000981026      *
017100981215     D DSNUMCA         DS                  INZ
017200981215     D  v2haac
017300981215     D  v2cfil
017400981215     D  v2cnca
017500981123      *
017600981123     d TCUDS           DS
017700981123     d  f1                     1      1
017800981123     d  f3                     3      3
017900981123     d  f2                     2      2
018000981123     d  f4                     4      4
018100981123     d  f56                    5      6
018200990224      *
018300990224     d DS_DESCR        DS
018400990224     d W900A                        900
018500990224     d DES                           35    DIM(20)  overlay(W900A:1)            Descrizione
018600990224     d w035a                         35             overlay(W900A:1)
018700990224     d DES70                         70    dim(11)  overlay(W900A:36)
018800100709
018900100709       // -?Strutture Dati per la definizione dei campi in stampa?
019000100709       //  ?per la "bonifica" degli stessi da caratteri indesiderati?
019100100709       //  ?da POSTEL?
019200100709     d ds_DN35PI       ds
019300100709     d   PICrag
019400100709     d   PICind
019500100709     d   PICcap
019600100709     d   PICloc
019700100709     d   PICprv
019800100709     d   PICnaz
019900100709     d   PICfra
020000100709     d   PICdal
020100100709     d   PICpec
020200100709     d ds_DN35PO       ds
020300100709     d   POCogg
020400100709     d   POCrsm
020500100709     d   POCrma
020600100709     d   POCrsd
020700100709     d   POClod
020800100709     d   POCprd
020900100709     d   POCkev
021000100709     d   POCtae
021100100709     d   POCtad
021200100709     d   POCdan
021300100709     d ds_DN35PO1      ds
021400100709     d   POCqtd
021500100709     d   POCdpz
021600100709     d ds_DN35PO2      ds
021700100709     d   POCkos
021800100709     d ds_DN35PT       ds
021900100709     d   PTCtes
022000990224      *
022100990224     D PRTBAR          DS
022200990224     D  cur_lineB            367    368I 0
022300041110      *
022400041110     D PRTMAI          DS
022500041110     D  cur_lineM            367    368I 0
022600981116      *
022700990224     D UT§DSE0F      E DS
022800990224     d  tcu                  398    697     DIM(50)
022900990224     d  kcu                  698    847P 0  DIM(50)  PACKEVEN
023000981026     D CNCR80        E DS
023100981215     D FIDN00DS      E DS
023200081230     D FIDNBEDS      E DS
023300980730     D FIDN05DS      E DS
023400981124     D FIDN20DS      E DS
023500980730     D TIBS02DS      E DS
023600980804     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
023700981204     D DS_TNTMD      E DS                  EXTNAME(TNTMD00F)
023800981204     d TIBS69DS      E DS
023900981204     d DS_cnaco      E DS                  extname(CNACO00F)
024000981204     d DS_cnind      E DS                  extname(CNIND00F)
024100981204     d DS_cnclp      E DS                  extname(CNCLP00F)
024200981204     d DS_fncls      E DS                  extname(FNCLS00F)
024300980507     D KPJBA         E DS
024400980729     D DTAD          E DS
024500981027     D DTLD          E DS
024600010910     D DGEDDN        E DS
024700010413     D DGEI          E DS
024800980918     D DSTB          E DS
024900981203     D DSCV          E DS
025000981230     D DS15          E DS
025100981229     D DDCT01        E DS
025200020124     d ddcf01        e ds
025300990226     D DSTD          E DS
025400041110     D TRUL44DS      E DS                                                       X Invio e-mail
025500050902     D TRTCM1DS      E DS                                                       X Invio e-mail
025600010906     D YEURCODS      E DS                  inz
025700010906     D   YECTAR      E                     inz('H')
025800010907      * Routine editazione di un numero
025900010907     D XEDITDS       E DS                  inz
026000010907     D   OedFlgErr   e                     inz(*off)
026100021118      *
026200021118     d OG143         e ds                  inz
026300021204     d DTA4A         e ds                  inz
026400980729      *----------------------------------
026500021118     d wSpedAss        s              1    inz(*off)
026600990415     D wfndpt          S              1
026700020327     D wclsflo7        S              1
026800981204     D wtibs69r        s              1
026900990222     D wpzman          S             13
027000990222     D wcoman          S             14
027100981123     D wkgman          S             11
027200980928     D wanno           s              4
027300980730     D wfil            s              3
027400980928     D wnum            s              7
027500981120     D wannop          s              4
027600981120     D wnump           s              7
027700980803     D wdataoggi       S             10
027800010328     D wdatastampa     S             10
027900071206     D wdatapreavv     S             10
028000990302     d Wdfvm           s             10
028100990302     D wtiporis        S              3
028200990302     D wtipodoc        S              3
028300990302     D wrivalsa        S              1
028400990302     D wimpclose       S              1
028500990415     D w003a           s              3
028600990415     D w005a           s              5
028700981124     D w007a           s              7
028800981124     D w007b           s              7
028900981124     D w011a           S             11
029000980803     D wsocie          S             20
029100010906     D wcmr            S             16
029200981214     D Wkca            S             19
029300981210     D command         S             90
029400990205     D lineaa          s             12
029500990205     D lineap          s             12
029600990205     D svrag           s             20
029700981215     D lung            S             15  5 inz(90)
029800980730     D XX              s              3  0
029900980731     D YY              s              3  0
030000980731     D JJ              s              3  0
030100981214     D QQ              s              3  0
030200020606     D WW              s              3  0
030300980730     D dateu           s              8  0
030400980730     D wdtgio          S              8  0
030500980730     D wora            S              6  0
030600981203     D nrr1            S              4  0
030700981203     D nrr2            S              4  0
030800980730     D w0140           S             14  0
030900990415     D w0133           S             13  3
031000041118     D w0030           S              3  0
031100980918     D w0040           S              4  0
031200981210     D w0040a          S              4  0
031300981210     D wpointes        S              3  0
031400990302     D wdaxx           S              3  0
031500990302     D waxx            S              3  0
031600990302     D wdayy           S              3  0
031700990302     D wayy            S              3  0
031800990302     D wdaH            S              3  0
031900990302     D waH             S              3  0
032000041123     D wlast           S              2  0
032100041123     D wlblank         S              2  0
032200041123     D wd44mit         S             30
032300041125     D svcom           S             90
032400041110
032500041110     D Invio           s              1                                         wrk per invio e-mail
032600041110
032700010906      *-------------
032800010906     D WImporto        S             30S 5 inz
032900010906     D WNrDec          S              2S 0 inz
033000010906     D WLungh          S              2S 0 inz
033100010906      *-------------
033200980730      *
033300980730     D DATA_eur        S               D   DATFMT(*eur)
033400980730     D DATA_oggi       S               D   DATFMT(*eur)
033500021127      *
033600990309     D W§dctport       S                   LIKE(§DCTport)
033700981214     d W§TADevfo       s                   LIKE(§TADevfo)
033800990312     d W§TADtisi       s                   LIKE(§TADtisi)
033900031211     d w§STDlpf        s                   like(§STDlpf1) inz(*loval)
034000990429     D WI00fca         S                   LIKE(I00fca)
034100001228     D Wdtaad          S                   LIKE(DCTaac)                         data stampa A.D.
034200081230     D Kcli            S                   LIKE(OBEksc)
034300980918     D Kcod            S                   LIKE(TBLcod)
034400980929     D Ktpd            S                   LIKE(DCStpd)
034500980929     D Knkt            S                   LIKE(DCSnkt)
034600980929     D Kstd            S                   LIKE(DCSstd)
034700980929     D Kdim            S                   LIKE(DCSdim)
034800980929     D Khim            S                   LIKE(DCShim)
034900980929     D Knks            S                   LIKE(DCSnks)
035000980929     D Ktrc            S                   LIKE(DCStrc)
035100981028     D Kkey            S                   LIKE(TBLkey)
035200981123     D Wtad            S                   LIKE(DCTtad)
035300981230     D wnaz            s                   LIKE(TMDnzm)
035400071206     d WFpd            s                   LIKE(DCTfca)   inz(400)
035500981125     d WFapd           s                   LIKE(DCTfca)   inz(545)
035600981125     d WFidc           s                   LIKE(DCTfca)   inz(660)
035700981125     d WFchi           s                   LIKE(DCTfca)   inz(900)
035800981215     d WCevf           s                   LIKE(DCTcch)   inz('04')
035900981215     d WCint           s                   LIKE(DCTcch)   inz('06')
036000020124
036100020125     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
036200100709
036300100709       //--------------------------------------------------------------
036400100709       //?Definizione procedure usate.                                 ?
036500100709       //--------------------------------------------------------------
036600100709
036700100709       // -?Parametri per XCHKCHAR?
036800100709      /copy gaitrasrc/srcProtoPI,XCHKCHAR
036900100709       // -?"Bonifica" stringa da caratteri indesiderati?
037000100709      /copy gaitrasrc/srcProtoPR,XCHKCHAR
037100100709
037200980731      *---------------------------------------------------------------------
037300040427      *
037400040427     C     *ENTRY        PLIST
037500040427     C                   PARM                    KPJBA
037600040427      *
037700040427     C                   MOVEL     KPJBU         fidn00ds
037800981203
037900990429      * Memorizzo fase C.A. ricevuta
038000990429     c                   z-add     I00fca        WI00fca
038100010704      *  Se ricevuta fase 999, quindi il pgm è richiamato da Ristampa C.A.
038200010704      *  forzo fase invio documenti a Consuldanni per gestione pgm
038300040428      *  Se ricevuta fase 998, quindi il pgm è richiamato da Ristampa
038400040428      *    C.A. di un giorno, forzo fase invio documenti a Consuldanni
038500040428      *    per gestione pgm
038600040428     c                   IF           WI00fca = 999
038700040428     c                             or WI00fca = 998
038800990429     c                   z-add     WFidc         I00fca
038900990429     c                   ENDIF
039000990429
039100020717      * Aggancio anagrafico fasi per tipi testo,
039200020717      * se non ci sono testi non elaboro
039300981203     c     I00fca        CHAIN     FNDFA000                           30
039400981203     c                   z-add     1             YY
039500981204
039600981204      * Memorizzo le coordinate del testo in schiera
039700981204     c     DFAle1        lookup    COTE(yy)                               30
039800990302     c   30              movel     COSC(yy)      Wtstdaa
039900981204
040000981204      * Se non ci sono testi non elaboro
040100981204     c                   IF          *IN30 = *on
040200981204
040300020717      * In base alla fase in esecuzione determino se inviare con POSTEL
040400040428     c                   eval      *in02  = ((I00fca  =  WFidc)
040500040428     c                                   and (WI00fca <> 998))
040600981204
040700981204      * Aggancio tabella TIPO TESTO
040800981204     c                   EXSR      DECtld
040900980731
041000020717      *  Lettera per Uff. Internazionale su stampante Bartolini
041100020717      *   stampa avviso danno
041200040428     c                   IF            *in02   =  *off
041300040428     c                             and WI00fca <> 998
041400120404     c                   movea     CMD(3)        COMMAND
041500981214     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
041600981214     c                   eval      %subst(COMMAND:43:10) = §TLDstp1
041700041118     c                   z-add     90            lung
041800981214     c                   call      'QCMDEXC'
041900981214     c                   parm                    COMMAND
042000981214     c                   parm                    LUNG
042100981214      *
042200981214     c                   open      PRTF198
042300981214     c                   ENDIF
042400981214
042500981203      * Stampo C.A. allegate e aggiorno testata C.A. e file fasi
042600981203     c                   IF        i00nce > 1
042700981203     c                   exsr      CA_multi
042800981203     c                   ELSE
042900981204      *
043000981204     c                   z-add     I00aas        V2Haas
043100981204     c                   z-add     I00lnp        V2Clnp
043200981204     c                   z-add     I00nrs        V2Cnrs
043300981204     c                   z-add     I00nsp        V2Cnsp
043400981204     c                   z-add     I00aac        V2Haac
043500981204     c                   z-add     I00fil        V2Cfil
043600981204     c                   z-add     I00nca        V2Cnca
043700981203     c                   exsr      CA_singola
043800981203     c                   ENDIF
043900981214
044000981214      * Stampo C.A. PF import se non ancora stampate e stampo la firma
044100981214     c                   IF        *IN11 = *on
044200981214     c                   if        QQ > *zeros
044300981214     c                   except    Pnumca
044400981214     c                   endif
044500981214     c                   except    Pfirma
044600981214     c                   ENDIF
044700981203
044800981203      * Chiudo TIBS02R
044900981203     c                   clear                   tibs02ds
045000981203     c                   movel     'C'           t02tla
045100981203     c                   call      'TIBS02R'
045200981203     c                   parm                    kpjba
045300981203     c                   parm                    tibs02ds
045400981204      * Chiudo TIBS69R
045500981204     C                   IF        Wtibs69r  <>  *BLANKS
045600981204     C                   eval      I69TLA  = 'C'
045700981204     C                   call      'TIBS69R'
045800981204     C                   parm                    TIBS69DS
045900981204     C                   ENDIF
046000981204      *
046100981204     c                   ENDIF
046200981204      *
046300040428     c                   if        WI00fca = 998
046400040428     c                   eval      *inRT   = *on
046500040427     c                   else
046600981203     C                   EVAL      *INLR = *ON
046700040427     c                   endif
046800981203      *---------------------------------------------------------------------
046900981203      *  ELABORO SUBFILE
047000981203      *---------------------------------------------------------------------
047100981203     c     CA_multi      BEGSR
047200981203      *
047300981203      * Leggo  subfile
047400981203     c                   z-add     1             nrr2
047500981204     c     nrr2          CHAIN     FI00S02                            31
047600981203     c     *IN31         DOWEQ     *OFF
047700981204      *
047800981204      * Elaboro se C.A. non esclusa e scelta non bloccata
047900981203     c                   IF        V2Csce <> 'E'  and  V2Hi82 <> '1'
048000981203     c                   exsr      CA_singola
048100981203     c                   ENDIF
048200981203      *
048300981203     c                   add       1             nrr2
048400981203     c     NRR2          CHAIN     FI00S02                            31
048500981203     c                   ENDDO
048600981203      *
048700981203     c                   ENDSR
048800981203      *---------------------------------------------------------------------
048900981203      *  ELABORO SINGOLA C.A.
049000981203      *---------------------------------------------------------------------
049100981203     c     CA_singola    BEGSR
049200981203      *
049300981203     c                   eval      *in28 = *off
049400981203     c                   clear                   page
049500981215     c                   clear                   Wtiporis
049600981203      *
049700981203      * Aggancio file bolle e C.A.
049800981203     c                   EXSR      SUB_files
049900980803      *
050000020717      * Se sped. NON è PF Import (per Europolitan solo se responsabilità
050100020717      *  Partner) stampo l'avviso danno, in caso contrario una lettera
050200020717      *  per l'ufficio internazionale
050300981214     c                   if        Wimpclose <> 'S'
050400981214     c                   EXSR      SUB_AVVDAN
050500981214     c                   else
050600981214     c                   EXSR      SUB_LETINT
050700981214     c                   endif
050800981215      * Azzero PRTF
050900981215     c                   EXSR      INZPRTF
051000981215      *   Reimposto indicatore per invio o no tramite POSTEL
051100040428     c                   eval      *in02  = ((I00fca  =  WFidc)
051200040428     c                                   and (WI00fca <> 998))
051300981203      *
051400981203      * Aggiorno file e se ho un'evento fortuito chiudo la C.A.
051500981203     c  N28              EXSR      SUB_AGGIOR
051600981203      *
051700981203     c                   ENDSR
051800981203      *****************************************************************
051900981203      *   AGGANCIO FILES
052000981203      *****************************************************************
052100981203     C     SUB_files     BEGSR
052200021118      *
052300021118     c                   Reset                   wSpedAss
052400981203      *
052500981203      * Aggancio file bolle
052600990809     c     KBOL          chain     TITAS30C                           30
052700981229      * se tipo porto recupero rileggo
052800981229     C                   movel     'TB'          kCOD
052900981229     C                   movel(P)  TAStbl        kKEY
053000981229     C     ktab          CHAIN     TABEL                              31
053100981229     C                   movel     TBLuni        DSTB
053200981229     c                   if        §TBrbl = 'R'
053300990809     c     KBOL          reade     TITAS30C                               30
053400981229     c                   endif
053500021118if  1c                   If        not *in30
053600021118     c                             and TASias >  0
053700021118     c                             and TASfcm <> 'F'
053800021118     c                   Eval      wSpedAss = *on
053900021118e   1c                   Endif
054000981203      *
054100981203      * Aggancio file mittenti/destinatari
054200981204     c                   clear                   DS_TNTMD
054300981204     c     KBOL          chain     TNTMD000                           30
054400981203      *
054500981203      * Aggancio testata C.A.
054600981203     c     KDCT          chain     FNDCT000                           30
054700981229     c                   movel     DCTflo        DDCT01
054800981214      *
054900020422      * Sped. Import in PF (per Europolitan considero solo se ho
055000020422      *  responsabilità del Partner)
055100981214     c                   clear                   Wimpclose
055200981229     c                   if        §DCTtisp = 'I'  and  §DCTport = 'F'
055300981230     c                             and  (§DCTeuro = 'X'  or
055400020422     c                                  (§DCTeuro = 'P'  and  DCTRES = 'P'))
055500981214     c                   eval      Wimpclose = 'S'
055600981214     c                   Endif
055700990415      *
055800990415      * Aggancio file mandato assicurativo
055900990415     c     KDCT          chain     FNDPT000                           30
056000990415     c                   if        *in30 = *on  or  DPTatb = 'A'
056100990415     c                   clear                   Wfndpt
056200990415     c                   else
056300990415     c                   eval      Wfndpt = 'S'
056400060315     c                   If        DPTftc = 'C' or  DPTftc = '8' or DPTFTC = 'd'
056500021118     c                   Eval      wSpedAss = *on
056600021118     c                   Endif
056700990415     c                   endif
056800981203      *
056900981203     c                   ENDSR
057000980729      *****************************************************************
057100981203      *   STAMPO LETTERA
057200980729      *****************************************************************
057300981214     C     SUB_avvdan    BEGSR
057400981203      *
057500010704      *  In caso di ristampa o invio dati a Consuldanni
057600010704      *  recupero la data dell'ultima stampa effettuata
057700001228      *
057800010328     c                   movel     wdataoggi     wdatastampa
057900010328      *
058000010704     c                   IF        WI00fca = 999  or  WI00fca = 660
058100040428     c                                            or  wI00fca = 998
058200001228     c                   EXSR      SUB_DTAAD                                     dta stampa A.D.
058300001228     c                   ENDIF
058400001228      *
058500981203     c                   EXSR      SUB_INTES                                     intestazione
058600981203      *
058700981203     c                   IF        *in28 = *on                                   errore N. pratica
058800981203     c                   z-add     99            JJ
058900981203     c                   else
059000981203     c                   EXSR      SUB_OGGE                                      oggetto
059100981203     c                   EXSR      SUB_TESTO                                     testo
059200120404     c                   EXSR      SUB_ACB                                       testo x polizza BRT
059300981203     c                   ENDIF
059400981203      *
059500981203     c                   ENDSR
059600001228      *****************************************************************
059700001228      *   RICERCO DATA ULTIMA STAMPA AVVISO DANNO DALLO STORICO FASI
059800001228      *****************************************************************
059900001228     C     SUB_dtaad     BEGSR
060000001228      *
060100001228     c                   clear                   wdtaad
060200001228      *
060300001228     c     kdct          setgt     FNDCF01L
060400001228     c     kdct          readpe    FNDCF01L
060500001228     c*
060600031216     c                   dow       NOT %eof(FNDCF01l)
060700001228     c* se la fase che leggo è uguale alla 545 recupero l'anno
060800001228     c                   if        dcffca =   Wfapd
060900001228     c                   movel     dcfdfc        wdtaad
061000010328     c     *iso          movel     dcfdfc        data_EUR
061100010328     C     *eur          movel     DATA_EUR      wdatastampa
061200001228     c                   leave
061300001228     c                   endif
061400001228     c*
061500001228     c     kdct          readpe    fndcf01l
061600001228     c*
061700001228     c                   enddo
061800001228     c*
061900001228     c                   endsr
062000071206      *****************************************************************
062100071206      *   RICERCO DATA STAMPA PREAVVISO DANNO DALLO STORICO FASI
062200071206      *****************************************************************
062300071206     C     SUB_dtapd     BEGSR
062400071206      *
062500071206      *
062600071206     c     kdct          setgt     FNDCF01L
062700071206     c     kdct          readpe    FNDCF01L
062800071206      *
062900071206     c                   dow       NOT %eof(FNDCF01l)
063000071206      * se la fase che leggo è uguale alla 400 recupero la data
063100071206     c                   if        dcffca =   Wfpd
063200071206     c     *iso          movel     dcfdfc        data_EUR
063300071206     C     *eur          movel     DATA_EUR      wdatapreavv
063400071206     c                   leave
063500071206     c                   endif
063600071206     c*
063700071206     c     kdct          readpe    fndcf01l
063800071206     c*
063900071206     c                   enddo
064000071206     c*
064100071206     c                   endsr
064200981203      *****************************************************************
064300981214      *   AGGIORNO ARCHIVI E SE HO EVENTO FORTUITO O PF IMPORT CHIUDO LA CA
064400981203      *****************************************************************
064500981203     C     SUB_aggior    BEGSR
064600981203      *
064700990429      * Se richiamato da pgm ristampa non aggiorno
064800040428     c                   IF            WI00fca <> 999
064900040428     c                             and wI00fca <> 998
065000020124
065100020125      * Aggiorno flag franchigia
065200020717      * - Se il risarcimento è calcolato con legge 450 o con CMR
065300020125      * - Se non è una c/a da chiudere
065400020125      * - Se sto facendo la fase 545
065500020125      * - Se il flag non è già impostato
065600031211      * - L'anno della spedizione è oltre il 2001 (w0040=2002)
065700020125      * - L'importo massimo risarcibile <= §stdlpf
065800031211      *   (Prima imposto tale importo "limite" da testare in base alla
065900031211      *    data spedizione)
066000031211     c                   reset                   w§STDlpf
066100031211     c                   eval      w0040 = §STDdlpf1 / 10000
066200020124
066300051123
066400051123      * Aggiorno flag franchigia
066500051123      * - Se non è una c/a da chiudere
066600051123      * - Se sto facendo la fase 545
066700051123      * - Se il flag non è già impostato
066800051123      * - L'anno della spedizione è oltre il 2001 (w0040=2002)
066900051123      * - L'importo massimo risarcibile <= §stdlpf
067000051123      *   (Prima imposto tale importo "limite" da testare in base alla
067100051123      *    data spedizione)
067200051124     c                   if        Wimpclose <> 'S'     and
067300051123     c                             wi00fca   =  wfapd   and
067400051123     c                             §dctpfra  =  *blanks and
067500051123     c                             DCTaas    >= w0040
067600051123sel 3c                   select
067700051123w   3c                   when      §STDdlpf2 <= ((TASaas*10000)+TASmgs)
067800051123     c                   eval      w§STDlpf   = §STDlpf2
067900051123w   3c                   when      §STDdlpf1 <= ((TASaas*10000)+TASmgs)
068000051123     c                   eval      w§STDlpf   = §STDlpf1
068100051123e   3c                   endsl
068200051123     c                   if        DCTipv    <= w§STDlpf
068300051123     c                   eval      §dctpfra   = 'F'
068400051123     c                   exsr      wrtdcs                                       Note
068500051123     c                   endif
068600051123e   2c                   endif
068700051123
068800051123
068900990429      *
069000981203     c                   EXSR      WRTdcf                                       Dettaglio fase
069100981203     c                   EXSR      WRTdct                                       Testata fase
069200981214     c                   if        Wtiporis = 'E  '  or  Wimpclose = 'S'
069300981214     c                   EXSR      WRTdcd                                       Segnacolli
069400990429     c                   endif
069500981203      *
069600990429     c                   ENDIF
069700990429      *
069800981203     c                   ENDSR
069900981203      *****************************************************************
070000981203      *   INTESTAZIONE
070100981203      *****************************************************************
070200981203     C     SUB_intes     BEGSR
070300041119      *
070400041119     c                   setoff                                       210107
070500020222      *
070600020222      * RISTAMPA A.D. in fase 660
070700020222      * (vedi prime 4 specifiche "C" del pgm)
070800020222     C                   clear                   PICfra
070900040428if  1C                   IF           (WI00fca  = 999
071000040428     c                              or wI00fca  = 998)
071100020226     C                             and DCTfca   = 660
071200020226     C                             and §DCTpfra = 'F'
071300020222      * - Flag "Pratica in Franchigia"
071400020222     C                   movel     §DCTpfra      PICfra
071500020222e   1C                   endif
071600980730      *
071700981120      * DATI dell'intestatario Avviso Danno
071800980928     C                   exsr      RIEcli
071900990222      *
072000990222      * salvo l'intestatario per dati alla consuldanni
072100990205     c                   clear                   svrag
072200990205     c                   eval      svrag = picrag
072300981103      *
072400981211      * Interlocutore se non ho rivalsa
072500050303     c                   eval      *in09   =  *off
072600981211     c                   IF        Wrivalsa <> 'S'
072700981103     c                   exsr      RECpercon
072800981211     c                   ENDIF
072900981007      *
073000981007      * Località + data
073100010328     c                   eval      PICdal = 'BOLOGNA, '  +  wdatastampa
073200981124      *
073300981210      * Per determinare l'azienda utilizzo il P.O. del cliente intestatario
073400981210     C     Wpointes      chain     AZORG                              31
073500981120      *
073600981125      * Se non ho il numero pratica (Avarie senza evento) lo assegno
073700981125     c                   IF        DCTprn = *zeros
073800981120     c                   exsr      NUMPRN
073900981120     c                   ENDIF
074000981120      *
074100981120     c                   IF        *IN28 = *OFF                                 N. pratica  OK
074200981211     c                   movel(P)  KBAR          Wsocie
074300981120     c                   ENDIF
074400981214      *
074500981214      * SOLO PRIMA VOLTA apro file stampa e imposto Tipo Modulo e Stampante
074600981214     c                   if        *in10 = *off
074700981214     c                   EXSR      IMPPRT
074800981214     c                   eval      *in10 = *on
074900981214     c                   endif
075000041117      * se invio via mail apro anche il file di spool per invio mail
075100041117     c                   if        *in20
075200041117     c                   open      fidn35pm
075300041117     c                   endif
075400100709      *
075500100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
075600100709     c                   if        Not *in20
075700100709     c                   eval      TxtInOut  = ds_DN35PI
075800100709     c                   exsr      sr_xChkChar
075900100709     c                   if        esito = '1'
076000100709     c                   eval      ds_DN35PI = TxtInOut
076100100709     c                   endif
076200100709     c                   endif
076300981113      *
076400980805      * Scrivo l'intestazione
076500100709      *
076600041117     c  n20              WRITE     DN35PP
076700041117     c  n20              WRITE     DN35PI
076800041117     c   20              WRITE     DN35MI
076900980730      *
077000980730     C                   ENDSR
077100980731      *****************************************************************
077200980918      *   IMPOSTO DATI CLIENTE PER STAMPA
077300980731      *****************************************************************
077400980918     C     RIEcli        BEGSR
077500981210      *
077600981210     c                   clear                   kcli
077700981210     c                   clear                   Wrivalsa
077800041117     c                   eval      invio = 'P'
077900041117     c                   setoff                                       20
078000081230      * recupero il beneficiario
078100081230     c                   clear                   fidnbeds
078200081230      *
078300081230     c                   movel     'B'           IBEmod
078400081230     c                   z-add     dctaac        IBEaac
078500081230     c                   z-add     dctfil        IBEfil
078600081230     c                   z-add     dctnca        IBEnca
078700081230     c                   z-add     dctaas        IBEaas
078800081230     c                   z-add     dctlnp        IBElnp
078900081230     c                   z-add     dctnrs        IBEnrs
079000081230     c                   z-add     dctnsp        IBEnsp
079100081230     c                   z-add     dctksc        IBEksc
079200081230     c                   movel     dctptr        IBEptr
079300081230     c                   movel     dctflo        IBEflo
079400081230     c                   movel     'S'           IBEtpb
079500081230     c                   If        %subst(knsif:7:1) = 'P'
079600081230     c                   eval      IBEsif = 'P'
079700081230     c                   EndIf
079800081230      *
079900081230     c                   call      'FIDNBER'
080000081230     c                   parm                    fidnbeds
080100081230      *
080200081230     c                   If        oberag <> *blanks
080300081230      *
080400081230     c                   movel     oberag        PICrag
080500081230     c                   movel     obecap        PICcap
080600081230     c                   movel     obevia        PICind
080700081230     c                   movel     obeloc        PICloc
080800081230     c                   if        obenaz  =  *blanks
080900081230     c                   movel     obeprv        PICprv
081000081230     c                   else
081100081230     c                   movel     obenaz        wnaz
081200081230     c                   exsr      ctrnaz
081300081230     c                   movel     §15des        PICnaz
081400081230     c                   endif
081500081230     c                   movel     obeksc        Kcli
081600120410     c                   movel     oberiv        Wrivalsa
081700081230     c                   endif
081800981120      *
081900981210      * Dal P.O. dell'intestatario determino la società
082000020717      *   Se ho rivalsa o beneficiario alternativo non codificato utilizzo
082100020717      *     il P.O. cliente M/D
082200981210      *   Se non ho codice utilizzo lnp (M) o lna (D)
082300981210     c                   movel     kcli          wpointes
082400981210      *
082500981210     c                   IF        Wpointes = *zeros
082600981210     c                   IF        DCTptr = 'D'
082700981229     C                   z-add     DCTlna        Wpointes
082800981120     c                   ELSE
082900981229     C                   movel     §DCTlnpc      Wpointes
083000981120     c                   ENDIF
083100981120     c                   ENDIF
083200041117      *
083300041117      * Imposto se devo stampare in Inglese
083400041117     c                   clear                   WCLSflo7
083500041117     c                   eval      *in08 = *OFF
083600081230     c                   IF        obeing <> *blanks
083700081230     c                   eval      wclsflo7 = obeing
083800041117     c                   eval      *in08 = *on
083900041117     c                   ENDIF
084000041116      * verfico se il cliente al quale devo inviare l'avviso danno ha un indirizzo e-mail
084100041116     c                   If        kcli <> *zeros and not *in02
084200041116     c                   clear                   trul44ds
084300110519     c                   eval      D44pgm  = 'FIDN35R'
084400041116     c                   eval      D44Ksc  = Kcli
084500041117     c                   eval      D44cl   = '006'
084600041117     c                   eval      d44apl = 'C'
084700041118     c                   eval      d44nk1 = '0151' + %editc(Kcli:'X')
084800041117     c                   eval      d44tnt = '87'
084900041116     c                   eval      D44ovr  = ' '
085000041116     c                   eval      D44scope= '*CALLLVL'
085100041117     c                   eval      D44prtf = 'FIDN35P'
085200041116
085300041116     c                   IF        Wclsflo7 <> *blanks
085400071217     C                   eval      D44obj =  '*OBJM*Alleged damage Notice - Ano-
085500041122     C                             maly information n. ' +  %char(dctfil)+  '/'+
085600041116     C                             %trim(%editc(dctnca:'Z')) +
085700041118     c                             ' Year '   +   %char(dctaac)
085800041116     c                   ELSE
085900071207     C                   eval      D44obj = '*OBJM*Avviso Danno COMU-
086000041117     C                             NICAZIONE ANOMALIA N. ' +
086100041116     C                             %char(dctfil)+  '/'      +
086200041116     C                             %trim(%editc(dctnca:'Z')) +
086300041118     c                             ' Anno '  +  %char(dctaac)
086400041116     c                   ENDIF
086500041116     c                   call      'TRUL44R'
086600041116     c                   parm                    trul44ds
086700050902     c                   parm                    trtcm1ds
086800041116      *
086900041116      * se c'è stato qualche problema faccio invio in altro modo
087000041116      *
087100041116     c                   if        d44err  <> ' '
087200041117     c                   eval      invio  = 'P'
087300041117     c                   setoff                                       20
087400041116     c                   ELSE
087500041117     c                   seton                                        20
087600041116      * ovrprtf
087700120404     c                   movea     CMD(4)        svcom
087800050324if  3c                   if        I00tpb  = '*'
087900050324     c                   eval      %subst(svcom:38:16) = *blanks
088000050324x   3c                   else
088100041125     c                   eval      %subst(svcom:43:10) = d44outq
088200050324e   3c                   endif
088300041125     c                   eval      commanl = %trim(svcom)+ d44dta  + ''')'
088400041116     c                   eval      lung = %len(%trim(commanl))
088500041116     C                   CALL      'QCMDEXC'                            60
088600041116     C                   PARM                    COMMANl         500
088700041116     C                   PARM                    LUNG             15 5
088800041116      * se c'è errore
088900041116     c                   if        *in60
089000041117     c                   eval      invio = 'P'
089100041117     c                   setoff                                       20
089200041117     c                   else
089300041117     c                   eval      invio = 'M'
089400041116     c                   endif
089500041116      *
089600041116
089700041116     c                   endif
089800041117
089900041117     c                   endif
090000980918      *
090100980918     C                   ENDSR
090200981210      *****************************************************************
090300981210      *   RECUPERO IL NOMINATIVO DELL'INTERLOCUTORE
090400981210      *****************************************************************
090500981210     C     RECpercon     BEGSR
090600981210      *
090700980731     c                   CLEAR                   DCSnot
090800981002     c                   CLEAR                   PICpec
090900980731      *
091000980929     C                   movel(P)  'C'           Ktpd
091100981215     C                   movel(P)  DSnumca       Knkt
091200980929     C                   movel(P)  'N'           Kstd
091300980929     C                   clear                   Khim
091400980929     C                   clear                   Kdim
091500980929     C                   clear                   Knks
091600980929     C                   movel     'D'           Ktrc
091700980731      *
091800981119     c     KDCS          CHAIN     FNDCS000                           30
091900981026      *
092000981119     c                   IF        *IN30 = *OFF and  DCSnot <> *BLANKS
092100050303     c                   eval      *in09   =  *on
092200981103     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
092300981026     c                   ENDIF
092400980731      *
092500980731     C                   ENDSR
092600981204      *****************************************************************
092700981204      *  AGGANCIO ANAGRAFICO
092800981204      *****************************************************************
092900981204     C     CTR_ANAGRA    BEGSR
093000981204      *
093100990302     C                   clear                   DS_cnaco
093200990302     C                   clear                   DS_cnind
093300990302     C                   clear                   DS_cnclp
093400990302     C                   clear                   DS_fncls
093500990302      *
093600981204     C                   CALL      'TIBS69R'
093700981204     C                   PARM                    tibs69DS
093800981204     C                   PARM                    DS_cnaco
093900981204     C                   PARM                    DS_cnind
094000981204     C                   PARM                    DS_cnclp
094100981204     C                   PARM                    DS_fncls
094200981204      *
094300981210     C                   eval      *IN30 = (O69ERR =  '1')
094400981204     C                   eval      wtibs69r = '1'
094500981204      *
094600981204     C                   ENDSR
094700980730      *****************************************************************
094800980730      *   OGGETTO
094900980730      *****************************************************************
095000980730     C     SUB_ogge      BEGSR
095100980928
095200981215     c                   clear                   POCqtd
095300981215     c                   clear                   Wkgman
095400981215     c                   clear                   Wcoman
095500981215     c                   clear                   Wpzman
095600981123      *
095700981123      * Intestazione
095800981120     c                   movel     DCTaac        wanno
095900981120     c                   movel     DCTfil        wfil
096000981120     c                   movel     DCTnca        w007a
096100980803     c     '0'           check     w007a         yy
096200980928     c                   eval      wnum = %subst(W007a:yy)
096300981120      *
096400981120     c                   movel     DCTpra        wannop
096500981120     c                   movel     DCTprn        w007a
096600981120     c     '0'           check     w007a         yy
096700981120     c                   eval      wnump = %subst(W007a:yy)
096800990120      *
096900020327     c                   IF        Wclsflo7 <> *blanks
097000071214     C                   eval      POCogg  = 'Damage Notice N.  '
097100020717     C                                       + DCTdit + ' ' + %trim(wnump)
097200020717     C                                       + ' Year ' + wannop
097300020717     C                                       + '   DAMAGE COMUNICATION N. '
097400020717     c                                       + wfil + '/' + %trim(wnum)
097500020717     C                                       + ' Year ' + wanno
097600990222     c                   ELSE
097700071207     C                   eval      POCogg  =   'Avviso Danno N. '
097800020717     C                                       + DCTdit + ' ' + %trim(wnump)
097900020717     C                                       + ' Anno ' + wannop
098000020717     C                                       + '   COMUNICAZIONE ANOMALIA N. '
098100020717     c                                       + wfil + '/' + %trim(wnum)
098200020717     C                                       + ' Anno ' + wanno
098300990222     c                   ENDIF
098400980803      *
098500981120     c                   z-add     DCTlnp        POClnp
098600981120     c                   z-add     DCTnrs        POCnrs
098700981120     c                   z-add     DCTnsp        POCnsp
098800981120     c                   z-add     DCTaas        POCdat
098900981120     c                   movel     TAsmgs        POCdat
099000981002     c     *mdy          move      POCdat        DATA_eur
099100981002     c     *dmy          move      DATA_eur      POCdat
099200981120     c                   movel     TMDrsm        POCrsm
099300981120     c                   z-add     TASrmn        POCrmn
099400021204      * il riferimento mittente alfanum. per la sede si trova in TITA4
099500021204     c     ktas4         chain     TITA430C
099600021204     c                   if        %found(TITA430C)
099700021204     c                   movel     TA4not        DTA4A
099800021204     c                   else
099900021204     c                   clear                   DTA4A
100000021204     c                   endif
100100021204     c                   movel     §TA4Arma      POCrma
100200021204     c     §TA4Arma      comp      *blanks                            1212
100300981120     c                   movel     TMDrsd        POCrsd
100400981120     c                   movel     TMDlod        POClod
100500990118     c                   if        TMDnzd <> *blanks
100600990118     c                   movel     TMDnzd        POCprd
100700990118     c                   else
100800990118     c                   movel     TMDprd        POCprd
100900990118     c                   endif
101000981120     c                   z-add     TASncl        POCncl
101100990809     c                   z-add     TASpkf        POCpkf
101200981123      *
101300981123      * Evento
101400981203     C                   clear                   W§TADevfo
101500990312     C                   clear                   W§TADtisi
101600981123     c                   IF        DCTnev > *zeros
101700981123     c                   eval      *IN03 = *ON
101800981123     c                   movel     DCTaae        wanno
101900981123     c                   movel     DCTnev        w007a
102000981123     c     '0'           check     w007a         yy
102100981123     c                   eval      wnum = %subst(W007a:yy)
102200020717     C                   eval      POCkev = wannop + '/' + %trim(wnum)
102300060619     c     Kdet          chain(n)  FNDET000                           31
102400981123     C                   movel     DETtad        Wtad
102500981123     c                   exsr      CHTAD
102600990302     C                   movel     §TADevfo      W§TADevfo
102700990312     C                   movel     §TADtisi      W§TADtisi
102800990302     c                   SELECT
102900020327     c                   WHEN        Wclsflo7 <> *blanks
103000020717     c                                      and  §TADdesi <> *blanks
103100990302     C                   movel     §TADdesi      POCtae
103200990302     c                   WHEN        §TADdest <> *blanks
103300990302     C                   movel     §TADdest      POCtae
103400990302     c                   OTHER
103500990302     C                   movel     §TADdesc      POCtae
103600990302     c                   ENDSL
103700981123     c                   ENDIF
103800981123      *
103900980729      * Decodifico tipo anomalia
104000981123     C                   movel     DCTtad        Wtad
104100980729     c                   exsr      CHTAD
104200990302     c                   SELECT
104300020327     c                   WHEN        Wclsflo7 <> *blanks
104400020717     c                                       and  §TADdesi <> *blanks
104500990302     C                   movel     §TADdesi      POCtad
104600990302     c                   WHEN        §TADdest <> *blanks
104700990302     C                   movel     §TADdest      POCtad
104800990302     c                   OTHER
104900990302     C                   movel     §TADdesc      POCtad
105000990302     c                   ENDSL
105100981123      *
105200980729      * Descrizione anomalia
105300990224     c                   clear                   W900A
105400980729     c                   exsr      DESTAD
105500990224     c                   movel     W035a         POCdan
105600100709      *
105700100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
105800100709     c                   if        Not *in20
105900100709     c                   eval      TxtInOut  = ds_DN35PO
106000100709     c                   exsr      sr_xChkChar
106100100709     c                   if        esito = '1'
106200100709     c                   eval      ds_DN35PO = TxtInOut
106300100709     c                   endif
106400100709     c                   endif
106500990224      *
106600990224      *  Stampa prima parte dell'oggetto
106700990310     c                   exsr      PRT_testat
106800041117     c  N20              WRITE     DN35PO
106900041117     c   20              WRITE     DN35MO
107000990224      *
107100990224      *  Stampa ulteriore descrizione anomalia
107200990224     c                   EXSR      PRTDESAGG
107300981123      *
107400981123      * Quantità danneggiate
107500981123      *      Peso
107600990223     c                   IF          DCTpkd > *zeros
107700981123     c                   move(P)   DCTpkd        w007a
107800020606     c     '0'           check     w007a         ww
107900020606     c                   eval      W007b = %subst(w007a:ww)
108000020606     c     ' '           checkr    w007b         ww
108100020606     c                   if        ww = 1
108200981123     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
108300981123     c                   else
108400020606     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(ww-1))
108500020606     c                                      +  ','  +  %subst(w007b:ww:1)
108600981123     c                   endif
108700981123     c                   ENDIF
108800981123      *      Colli
108900990223     c                   IF          DCTncn > *zeros
109000981123     c                   move      DCTncn        w005a
109100020606     c     '0'           check     w005a         ww
109200020327     c                   IF        Wclsflo7 <> *blanks
109300020606     c                   eval      Wcoman = ' Parcels '  +  %subst(W005a:ww)
109400990222     c                   ELSE
109500020606     c                   eval      Wcoman = ' Colli '  +  %subst(W005a:ww)
109600981123     c                   ENDIF
109700990222     c                   ENDIF
109800981123      *      Pezzi
109900981123     c                   IF        DCTnpz > *zeros
110000981123     c                   move      DCTnpz        w005a
110100020606     c     '0'           check     w005a         ww
110200020327     c                   IF        Wclsflo7 <> *blanks
110300020606     c                   eval      Wpzman = ' Pieces '  +  %subst(W005a:ww)
110400990222     c                   ELSE
110500020606     c                   eval      Wpzman = ' Pezzi '  +  %subst(W005a:ww)
110600980729     c                   ENDIF
110700990222     c                   ENDIF
110800981123      *
110900981123     c                   eval      POCqtd  =  %trimr(Wkgman)  +
111000981123     c                                        %trimr(Wcoman)  +  %trimr(Wpzman)
111100981123      *
111200981123      * Descrizione pezzi danneggiati
111300990224     c                   clear                   W900A
111400981123     c                   IF        DCTnpz > *zeros
111500981123     c                   EXSR      DESPZD
111600981123     c                   ENDIF
111700990224     c                   movel     W035a         POCdpz
111800100709      *
111900100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
112000100709     c                   if        Not *in20
112100100709     c                   eval      TxtInOut   = ds_DN35PO1
112200100709     c                   exsr      sr_xChkChar
112300100709     c                   if        esito = '1'
112400100709     c                   eval      ds_DN35PO1 = TxtInOut
112500100709     c                   endif
112600100709     c                   endif
112700980730      *
112800990224      *  Stampa seconda parte dell'oggetto
112900990310     c                   exsr      PRT_testat
113000041117     c  N20              WRITE     DN35PO1
113100041117     c   20              WRITE     DN35MO1
113200990224      *
113300990224      *  Stampa ulteriore descrizione pezzi danneggiati
113400990224     c                   EXSR      PRTDESAGG
113500120403
113600120403      //?Stampo 'Spettabile Cliente'
113700120403
113800120403     c  N20              WRITE     DN35PO3
113900120403     c   20              WRITE     DN35MO3
114000980928
114100980729     C                   ENDSR
114200980928      *****************************************************************
114300980928      *  REPERIMENTO DESCRIZIONE EVENTO
114400980928      *****************************************************************
114500980928     C     DESEVE        BEGSR
114600980929      *
114700980929     C                   movel     'E'           Ktpd
114800981204     C                   movel     DCTAAE        W011A
114900981204     C                   move      DCTNEV        W011A
115000980929     C                   movel(P)  W011A         Knkt
115100980928     C                   clear                   Khim
115200980928     C                   clear                   Kdim
115300980929     C                   clear                   Kstd
115400980929     C                   clear                   Knks
115500980929     C                   movel     'D'           Ktrc
115600980928      *
115700980928     c                   clear                   yy
115800980929     c                   clear                   W35
115900980928      *
116000980929     c     kdcse         chain     fndcs000                           30
116100980928      *
116200980928     c     *in30         DOWeq     *off
116300980929     c     yy            ANDle     3
116400980928     c                   add       1             yy
116500980928     c                   movel     dcsnot        W35(yy)
116600980929     c     kdcse         reade     fndcs000                               30
116700980928     c                   ENDDO
116800980928      *
116900980928     C                   ENDSR
117000980729      *****************************************************************
117100980729      *  REPERIMENTO DESCRIZIONE ANOMALIA
117200980729      *****************************************************************
117300980729     C     DESTAD        BEGSR
117400980729      *
117500980929     C                   movel(P)  'C'           Ktpd
117600981215     C                   movel(P)  DSnumca       Knkt
117700980929     C                   movel(P)  'A'           Kstd
117800980929     C                   clear                   Khim
117900980929     C                   clear                   Kdim
118000980929     C                   clear                   Knks
118100980929     C                   movel     'D'           Ktrc
118200980929      *
118300980803     c                   clear                   yy
118400980730      *
118500980730     c     kdcs          chain     fndcs000                           30
118600980730      *
118700980730     c     *in30         DOWeq     *off
118800990224     c     yy            ANDlt     20
118900980918     c                   add       1             yy
119000990224     c                   movel     dcsnot        DES(yy)
119100980730     c     kdcs          reade     fndcs000                               30
119200980730     c                   ENDDO
119300980730      *
119400980729     C                   ENDSR
119500981123      *****************************************************************
119600981123      *  REPERIMENTO DESCRIZIONE PEZZI DANNEGGIATI
119700981123      *****************************************************************
119800981123     C     DESPZD        BEGSR
119900981123      *
120000981123      * Descrizione pezzi mancanti
120100981123     C                   movel(P)  'C'           Ktpd
120200981215     C                   movel(P)  DSnumca       Knkt
120300981123     C                   movel(P)  'P'           Kstd
120400990224     C                   clear                   Khim
120500990224     C                   clear                   Kdim
120600990224     C                   clear                   Knks
120700981123     c                   clear                   yy
120800981123      *
120900981123     c     kdcs          chain     fndcs000                           30
121000981123      *
121100981123     c     *in30         DOWeq     *off
121200990224     c     yy            ANDlt     20
121300981124     c                   add       1             yy
121400990224     c                   movel     dcsnot        DES(yy)
121500981123     c     kdcs          reade     fndcs000                               30
121600981123     c                   ENDDO
121700981123      *
121800981124     c                   ENDSR
121900990224      *****************************************************************
122000990224      *  STAMPA DESCRIZIONI AGGIUNTIVE NELL'OGGETTO
122100990224      *****************************************************************
122200990224     C     PRTDESAGG     BEGSR
122300990224      *
122400990224     c                   clear                   POCkos
122500020606     c                   DO        11            ww
122600990224      *
122700020606     c                   IF         DES70(ww)  <> *blanks
122800020606     c                   movel     DES70(ww)     POCkos
122900100709      *
123000100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
123100100709     c                   if        Not *in20
123200100709     c                   eval      TxtInOut   = ds_DN35PO2
123300100709     c                   exsr      sr_xChkChar
123400100709     c                   if        esito = '1'
123500100709     c                   eval      ds_DN35PO2 = TxtInOut
123600100709     c                   endif
123700100709     c                   endif
123800990224      *
123900990310     c                   exsr      PRT_testat
124000041117     c  N20              WRITE     DN35PO2
124100041117     c   20              WRITE     DN35MO2
124200990224      *
124300990224     c                   ENDIF
124400990224     c                   ENDDO
124500990224      *
124600990224     c                   ENDSR
124700980729      *****************************************************************
124800980729      *  REPERIMENTO DATI TIPO ANOMALIA
124900980729      *****************************************************************
125000980729     C     CHTAD         BEGSR
125100980729      *
125200980730     c                   clear                   tibs02ds
125300980729     c                   clear                   dtad
125400980729      *
125500980729     C                   movel     'C'           t02mod
125600980729     C                   movel     knsif         t02sif
125700980729     C                   movel     'TAD'         t02cod
125800981123     C                   movel(P)  Wtad          t02ke1
125900980729      *
126000980729     C                   CALL      'TIBS02R'
126100980729     C                   PARM                    KPJBA
126200980730     C                   PARM                    TIBS02DS
126300980729      *
126400980729     C                   movel     T02uni        DTAD
126500980729      *
126600980729     C                   ENDSR
126700980730      *****************************************************************
126800980730      *   TESTO
126900980730      *****************************************************************
127000980730     C     SUB_testo     BEGSR
127100071206      * recupero data stampa preavviso
127200071206     c                   exsr      SUB_DTAPD
127300980804
127400990302      * Determino il tipo di risarcimento da applicare al cliente
127500981123     c                   EXSR      SUB_TIPRIS
127600981123
127700981124      *  Lascio 2 righe dall'oggetto prima di stampare il testo
127800050303      *  =====> 1 riga !
127900990310     c                   exsr      PRT_spazio
128000980731
128100981123      *  Stampo eventuali note aggiuntive
128200981124     c                   EXSR      SUB_NOTE
128300981123
128400990224      *  Se il valore delle prime 5 posizioni è:
128500020717      *      "XJUMP" scrivo una riga blank
128600020717      *      "XTESP" scrivo righe di testo in funzione del tipo risarcimento
128700020717      *      "XDOCP" scrivo righe di testo con i documenti da richiedere
128800990302
128900990302      *  Imposto posizioni iniziale e finali
129000020327     c                   IF          Wclsflo7 <> *blanks
129100990302     c                   eval      Wdaxx = WtstGBda
129200990302     c                   eval      Waxx  = WtstGBa
129300990302     c                   ELSE
129400990302     c                   eval      Wdaxx = WtstITAda
129500990302     c                   eval      Waxx  = WtstITAa
129600990302     c                   ENDIF
129700981123
129800990302     c     Wdaxx         DO        Waxx          XX
129900981123     c                   clear                   PTCtes
130000120412     c                   clear                   wConta            2 0
130100980803      *
130200990223     c                   SELECT
130300990223      *
130400990224     c                   WHEN      %subst(TES1(xx):1:5) = 'XJUMP'
130500990223     c                   exsr      WRTtesto
130600990223      *
130700990223     c                   WHEN      %subst(TES1(xx):1:5) = 'XTESP'
130800981123     c                   exsr      SUB_tstpar
130900990302      *       se tipo risarcimento è "E" = evento fortuito passo ai saluti
131000990420     c                   If        Wtiporis = 'E  '
131100020327     c                   If          Wclsflo7 <> *blanks
131200050208     c                   z-add     WTES1_SA_G    XX                             inglese
131300990420     c                   Else
131400050208     c                   z-add     WTES1_SA_I    XX                             italiano
131500990420     c                   Endif
131600990420     c                   Endif
131700990223      *
131800990223     c                   WHEN      %subst(TES1(xx):1:5) = 'XDOCP'
131900990224      *       gestione documenti da ricevere
132000990223     c                   exsr      SUB_tstdoc
132100981123      *
132200990223     c                   OTHER
132300981124     c                   movel(P)  TES1(xx)      PTCtes
132400990223      *       inserisco variabili nel testo standard
132500990302     c                   exsr      SUB_vartst
132600990223     c                   exsr      WRTtesto
132700990223      *
132800990223     c                   ENDSL
132900990223      *
133000980730     c                   ENDDO
133100041123      *  se non ho inviato via e-mail stampo e va via postel
133200041123     c                   if        not *in20 and not *in02
133300041118      *
133400041118     c                   movel     kcli          W0030
133500041118     c                   move      kcli          W0040
133600041118      * chiamo il trul per il recupero dell'indirizzo del mittente
133700041118     c                   clear                   trul44ds
133800041118     c                   eval      d44pgm = 'FIDN35R'
133900041118     c                   eval      d44po  = W0030
134000041118     c                   call      'TRUL44R'
134100041118     c                   parm                    trul44ds
134200050902     c                   parm                    trtcm1ds
134300041123      *
134400041123      * imposto d44mit con doppia @
134500041123     c                   clear                   wlast
134600041123     c                   clear                   wlblank
134700041123     c                   clear                   wd44mit
134800041123     c                   eval      Wlast = %scan('@':d44mit)
134900041123     c                   eval      Wlblank = %scan(' ':d44mit)
135000100727     c                   eval      wd44mit = %subst(d44mit:1:Wlast) + '@' +
135100041123     c                             %subst(d44mit:wlast+1:Wlblank)
135200041123     c
135300041118      *
135400041118      *
135500041118      *
135600041118     c                   if        w0040 <> 8888 and w0040 <> 9999
135700051013     c                                           and w0040 <> *zeros
135800041118      *
135900041118     c                   If        not *in08
136000041118     c                   movel(p)  TESM(1)       PTCtes
136100041118     c                   else
136200041118     c                   movel(p)  TESM(3)       PTCtes
136300041118     c                   endif
136400041118      *
136500041118      *  Lascio 2 righe
136600041118     c                   exsr      PRT_spazio
136700041118     c                   exsr      WRTtesto
136800041118     c                   If        not *in08
136900041118     c                   movel(P)  TESM(2)       PTCtes
137000041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
137100041118     c                   eval      %subst(ptctes:66:7) = %char(Kcli)
137200041118     c                   else
137300041118     c                   movel(P)  TESM(4)       PTCtes
137400041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
137500041118     c                   eval      %subst(ptctes:73:7) = %char(Kcli)
137600041118     c                   endif
137700041118      *
137800041118     c                   exsr      WRTtesto
137900041118     c                   endif
138000041118
138100041118     c                   endif
138200980730      *
138300980730     C                   ENDSR
138400990302      *****************************************************************
138500990302      *   SCRIVO TESTO
138600990302      *****************************************************************
138700990302     C     WRTtesto      BEGSR
138800990302      *
138900071206      * se è la prima riga del testo di avviso danno imposto la data della stampa
139000071206      * di presunto avviso danno
139100080116     c                   If        xx = 1 and xx = wdaxx
139200120403     c                   eval      %subst(PTCtes:50:10)  = wdatapreavv
139300071206     c                   endif
139400071214      * inglese
139500120412     c                   If        xx = 14 and xx = wdaxx
139600120403     c                   eval      %subst(PTCtes:35:10)  = wdatapreavv
139700071214     c                   endif
139800100709      *
139900100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
140000100709     c                   if        Not *in20
140100100709     c                   eval      TxtInOut  = ds_DN35PT
140200100709     c                   exsr      sr_xChkChar
140300100709     c                   if        esito = '1'
140400100709     c                   eval      ds_DN35PT = TxtInOut
140500100709     c                   endif
140600100709     c                   endif
140700071206
140800990310     c                   exsr      PRT_testat
140900041117     C  N20              WRITE     DN35PT
141000041117     C   20              WRITE     DN35MT
141100990302      *
141200990302     C                   ENDSR
141300990302      *****************************************************************
141400990302      *   TESTO PARTICOLARE PER TIPO RISARCIMENTO
141500990302      *****************************************************************
141600990302     C     SUB_tstpar    BEGSR
141700990302
141800990302      *  Ricavo le coordinate del testo legato al tipo risarcimento
141900990302     c                   z-add     1             yy
142000990302     c     Wtiporis      lookup    COTP(yy)                               30
142100990302     c   30              movel     COSP(yy)      Wrisdaa
142200990302      *
142300990302      *  Imposto posizioni iniziale e finali
142400020327     c                   IF          Wclsflo7 <> *blanks
142500990302     c                   eval      Wdayy = WrisGBda
142600990302     c                   eval      Wayy  = WrisGBa
142700990302     c                   ELSE
142800990302     c                   eval      Wdayy = WrisITAda
142900990302     c                   eval      Wayy  = WrisITAa
143000990302     c                   ENDIF
143100990302
143200990302     c     Wdayy         DO        Wayy          YY
143300990302      *
143400990302     c                   clear                   PTCtes
143500990302     c                   IF        %SUBST(TESP(yy):1:5)  <>  'XJUMP'
143600990302     c                   movel     TESP(yy)      PTCtes
143700990302      *    Inserisco variabili
143800990302     c                   exsr      INStstpar
143900990302     c                   ENDIF
144000990302      *
144100990302     c                   exsr      WRTtesto
144200990302      *
144300990302     c                   ENDDO
144400990302      *
144500990302     C                   ENDSR
144600990224      *****************************************************************
144700990224      *   INSERIMENTO VARIABILI NEL TESTO PARTICOLARE PER TIPO RIMBORSO
144800990224      *****************************************************************
144900990224     C     INStstpar     BEGSR
145000990224
145100010906     c                   exsr      Edit
145200990302
145300990224     c                   SELECT
145400020125
145500060310      *    TIPO RIMBORSO "A"  RCV 450/85
145600020125     c                   WHEN      YY = 2
145700071207     c                   eval      %subst(PTCtes:33:3)  = §GEDDBA
145800071207     c                   eval      %subst(PTCtes:37:9) =
145900020125     c                                %TRIM(%subst(OedNUMAN:1:9))
146000990224
146100990302      *    TIPO RIMBORSO "B"  Elevazione limite risarcibile
146200050216     c                   WHEN      YY = 4
146300071207     c                   eval      %subst(PTCtes:35:3)  = §GEDDBA
146400071207     c                   eval      %subst(PTCtes:39:10) =
146500010907     c                                %TRIM(%subst(OedNUMAN:1:10))
146600990224
146700990302      *    TIPO RIMBORSO "C"  Mandato per conto
146800050216     c                   WHEN      YY = 5
146900120412     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
147000120412     c                   eval      %subst(PTCtes:47:10) =
147100010907     c                                %TRIM(%subst(OedNUMAN:1:10))
147200120412     c                   eval      %subst(PTCtes:60:10) = Wdfvm
147300990224
147400990224      *    TIPO RIMBORSO "D"  CMR
147500050216     c                   WHEN      YY = 8 and  §GEcmr > *zeros
147600010907     c                   eval      WCMR  =    'D.S.P.'
147700010907     c                                      +  %subst(OedNumAN:1:6)
147800120412     c                   eval      %subst(PTCtes:64:16) = WCMR
147900990224
148000990302      *    TIPO RIMBORSO "F"  Valore complessivo in bolla
148100050216     c                   WHEN      YY = 12
148200120412     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
148300120412     c                   eval      %subst(PTCtes:47:10) =
148400010907     c                                %TRIM(%subst(OedNUMAN:1:10))
148500990302
148600060310      *    TIPO RIMBORSO "G"  RCV  286/05
148700060310     c                   WHEN      YY = 16
148800120404     c                   eval      %subst(PTCtes:54:3)  = §GEDDBA
148900120404     c                   eval      %subst(PTCtes:58:9) =
149000060310     c                                %TRIM(%subst(OedNUMAN:1:9))
149100060310
149200020130      *    inglese TIPO RIMBORSO "A"  RCV
149300060315     c                   WHEN      YY = 21
149400050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
149500050216     c                   eval      %subst(PTCtes:22:9) =
149600020130     c                                %TRIM(%subst(OedNUMAN:1:9))
149700020130
149800990302      *    inglese TIPO RIMBORSO "B"  Elevazione limite risarcibile
149900060315     c                   WHEN      YY = 23
150000050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
150100050216     c                   eval      %subst(PTCtes:22:10) =
150200060310     c                                %TRIM(%subst(OedNUMAN:1:10))
150300990302
150400990302      *    inglese TIPO RIMBORSO "C"  Mandato per conto
150500060315     c                   WHEN      YY = 24
150600120412     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
150700120412     c                   eval      %subst(PTCtes:51:10) =
150800010907     c                                %TRIM(%subst(OedNUMAN:1:10))
150900120404     c                   eval      %subst(PTCtes:67:10) = Wdfvm
151000990302
151100990302      *    inglese TIPO RIMBORSO "D"  CMR
151200060315     c                   WHEN      YY = 26  and  §GEcmr > *zeros
151300010907     c                   eval      WCMR  =    'D.S.P.'
151400010907     c                                      +  %subst(OedNumAN:1:6)
151500120412     c                   eval      %subst(PTCtes:60:16) = WCMR
151600990302
151700990302      *    inglese TIPO RIMBORSO "F"  Valore complessivo in bolla
151800060315     c                   WHEN      YY = 29
151900120412     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
152000120412     c                   eval      %subst(PTCtes:51:10) =
152100010907     c                                %TRIM(%subst(OedNUMAN:1:10))
152200990224
152300060310      *    inglese TIPO RIMBORSO "G"  RCV  286/05
152400060315     c                   WHEN      YY = 33
152500120412     c                   eval      %subst(PTCtes:78:3)  = §GEDDBA
152600120412     c                   eval      %subst(PTCtes:82:9) =
152700060310     c                                %TRIM(%subst(OedNUMAN:1:9))
152800060310
152900990224     c                   ENDSL
153000990224
153100990224     C                   ENDSR
153200990223      *****************************************************************
153300990224      *   GESTIONE DOCUMENTI DA RICEVERE
153400990223      *****************************************************************
153500990224     C     SUB_tstdoc    BEGSR
153600120404
153700120404      * Richiedere documento "D01" solo per Avaria e Ammanchi
153800120404     c                   IF        §TADragr = 'V'  or  §TADragr = 'A'
153900120404     c                   eval      Wtipodoc = 'D01'
154000120404     c                   exsr      SUB_prtdoc
154100120404     c                   ENDIF
154200120404
154300120404      * Richiedere documento "D02" sempre
154400120404     c                   eval      Wtipodoc = 'D02'
154500120404     c                   exsr      SUB_prtdoc
154600120404
154700120404      * Richiedere documento "D03" sempre
154800120404     c                   eval      Wtipodoc = 'D03'
154900120404     c                   exsr      SUB_prtdoc
155000120404
155100120404      * Richiedere documento "D04" solo per Avaria con importo massimo risarcibile maggiore 250 euro
155200120404     c                   IF        §TADragr = 'V' and dctipv > 250
155300120404     c                   eval      Wtipodoc = 'D04'
155400120404     c                   exsr      SUB_prtdoc
155500120404     c                   ENDIF
155600120404
155700120404      * Richiedere documento "D05" sempre
155800120404     c                   eval      Wtipodoc = 'D05'
155900120404     c                   exsr      SUB_prtdoc
156000120404
156100120404      * Richiedere documento "D06" se non ho rivalsa
156200120404      *  viceversa richiedere documento "D07"
156300990224     c                   IF        Wrivalsa <> 'S'
156400120404     c                   eval      Wtipodoc = 'D06'
156500990224     c                   ELSE
156600120404     c                   eval      Wtipodoc = 'D07'
156700990224     c                   ENDIF
156800990224     c                   exsr      SUB_prtdoc
156900981210      *
157000990224     c                   eval      *IN04 = *off
157100990224      *
157200981210     C                   ENDSR
157300990224      *****************************************************************
157400990224      *   STAMPO DOCUMENTO
157500990224      *****************************************************************
157600990224     C     SUB_prtdoc    BEGSR
157700990302
157800990302      *  Ricavo le coordinate del testo dei documenti da richiedere
157900990302     c                   z-add     1             yy
158000990302     c     Wtipodoc      lookup    COTD(yy)                               30
158100990302     c   30              movel     COSD(yy)      Wdocdaa
158200990302      *
158300990302      *  Imposto posizioni iniziale e finali
158400020327     c                   IF          Wclsflo7 <> *blanks
158500990302     c                   eval      Wdayy  =  WdocGBda
158600990302     c                   eval      Wayy   =  WdocGBa
158700990302     c                   eval      WdaH   =  WdocGBdaH
158800990302     c                   eval      WaH    =  WdocGBaH
158900990302     c                   ELSE
159000990302     c                   eval      Wdayy  =  WdocITAda
159100990302     c                   eval      Wayy   =  WdocITAa
159200990302     c                   eval      WdaH   =  WdocITAdaH
159300990302     c                   eval      WaH    =  WdocITAaH
159400990302     c                   ENDIF
159500990302
159600990302     c     Wdayy         DO        Wayy          YY
159700990302      *
159800990302     c                   eval      *IN04 =  (yy <= WdaH  AND  yy >= WaH)
159900990224     c                   clear                   PTCtes
160000990224      *
160100990302     c                   IF          %SUBST(TESD(yy):1:5)  <>  'XJUMP'
160200120412      * imposto il numero a fianco della intestazione del documento richiesto
160300120412     c                   IF        wdayy = yy
160400120412     c                   eval      wConta += 1
160500120412     c                   eval      PTCtes = '  ' + %editc(wConta:'Z') +
160600120412     c                             '. ' + %trim(TESD(yy))
160700120412     c                   ELSE
160800120412     c                   movel(P)  TESD(yy)      PTCtes
160900120412     c                   ENDIF
161000990302     c                   exsr      SUB_vardoc
161100990224     c                   ENDIF
161200990224      *
161300990224     c                   exsr      WRTtesto
161400990224     c                   ENDDO
161500990224      *
161600990224     C                   ENDSR
161700990302      *****************************************************************
161800990302      *   INSERISCO VARIABILI NEI DOCUMENTI DA RICHIEDERE
161900990302      *****************************************************************
162000990302     C     SUB_vardoc    BEGSR
162100990302      *
162200990302      *
162300990302     C                   ENDSR
162400990302      *****************************************************************
162500990302      *   INSERISCO VARIABILI NEL TESTO
162600990302      *****************************************************************
162700990302     C     SUB_vartst    BEGSR
162800990302      *
162900120404      *    Inserisco nome società
163000120404     c                   IF        XX = WTES1_SO_G  or                          inglese
163100120404     c                             XX = WTES1_SO_I                              italiano
163200120517     c                   eval      %subst(PTCtes:54:20) = %TRIM(Wsocie)
163300120404     c                   endif
163400990302      *
163500990302     C                   ENDSR
163600981123      *****************************************************************
163700981123      *   DETERMINO IL TIPO RISARCIMENTO
163800981123      *****************************************************************
163900981123     C     SUB_tipris    BEGSR
164000981123
164100010906     c                   clear                   wImporto
164200010906     c                   clear                   wNrDec
164300010906     c                   clear                   wLungh
164400981203
164500020724      * Se PA EXPORT o PF IMPORT, non DPD, e non è evento fortuito
164600020717      *    abbiamo il tipo risarcimento "D"
164700020717      *
164800021111    1c                   IF        W§TADevfo <> 'S'  AND
164900020724     C                             ( (§DCTtisp = 'E' and  §DCTport = 'A') OR
165000020724     C                               (§DCTtisp = 'I' and  §DCTport = 'F'
165100020724     c                                               and  §DCTeuro <> 'D') )
165200020717      *
165300981203     c                   movel(P)  'D'           Wtiporis
165400010906     c                   z-add     §GEcmr        WImporto
165500010906     c                   z-add     3             wNrDec
165600010906     c                   z-add     6             wLungh
165700021111   x1c                   ELSE
165800981124
165900020717      * Se è esposto il valore in bolla (non calcolato dalla fatturazione)
166000020717      *   tipo risarcimento "F"
166100021111    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
166200981123     c                   movel(P)  'F'           Wtiporis
166300011114     c                   if        TASias <> *zeros and TASvas = *blanks
166400011114     c                   movel     'ITL'         TASvas
166500011114     c                   endif
166600990415     c                   z-add     TASias        W0133
166700990415     c                   movel     TASvas        W003a
166800010910     C                   if        TASvas <> §gedDBA
166900010910     C                   exsr      srCMB
167000981203     c                   endif
167100010906     c                   z-add     W0133         wImporto
167200010906     c                   z-add     §cvDEC        wNrDec
167300010906     c                   z-add     10            wLungh
167400021111   x2c                   ELSE
167500981123      *
167600990415      * Cliente assicurato
167700021111    3c                   IF        Wfndpt = 'S'
167800990415      *   Importo Assicurato
167900021111    4c                   IF        DPTvlm > *zeros
168000990415     c                   z-add     DPTvlm        W0133
168100990415     c                   movel     DPTvvm        W003a
168200010910     c                   IF        DPTvvm <> §gedDBA
168300010906     C                   exsr      srCMB
168400990415     c                   ENDIF
168500010907     c                   z-add     W0133         WImporto
168600010906     c                   z-add     §cvDEC        wNrDec
168700010906     c                   z-add     10            wLungh
168800021111   -4c                   ENDIF
168900990415      *   Decodifica tipo valore
169000990415     c                   SELECT
169100990415     c                   WHEN      DPTfvm = '1'
169200120412     c  n08              movel(P)  'collo'       Wdfvm
169300120412     c   08              movel(P)  'parcel'      Wdfvm
169400990415     c                   WHEN      DPTfvm = '2'
169500120412     c  n08              movel(P)  'spedizione'  Wdfvm
169600120412     c   08              movel(P)  'shipment'    Wdfvm
169700990415     c                   WHEN      DPTfvm = '3'
169800120412     c  n08              movel(P)  'Kg merce'    Wdfvm
169900120412     c   08              movel(P)  'weight Kg.'  Wdfvm
170000990415     c                   OTHER
170100990415     c                   clear                   Wdfvm
170200990415     c                   ENDSL
170300020717      *   Tipo Risarcimento: "C" mandato per conto,
170400020717      *                      "B" elevazione del limite risarcibile
170500060315      *
170600021111    4c                   IF        DPTftc = '8'  or  DPTftc  =  'C'
170700060315     c                             or DPTftc = 'd'
170800990415     c                   movel     'C'           Wtiporis
170900990415     c                   ELSE
171000020717      * Se c'è un evento fortuito non considero
171100020717      *   l'elevazione limite risarcibile
171200991025     c                   if        W§TADevfo <> 'S'
171300990415     c                   movel     'B'           Wtiporis
171400991025     c                   ENDIF
171500991025      *
171600021111   -4c                   ENDIF
171700990415      *
171800021111   -3c                   ENDIF
171900981204
172000021111      * Se non assicurato e non è un evento fortuito (tipo risarcimento
172100021111      *  "E") si applica la legge 450 (tipo risarcimento "A") o il CMR
172200021111      *  (tipo risarcimento "D")  se PA IMPORT o PF EXPORT o DPD EXPORT
172300021111      *  o FEDEX EXPORT
172400981203     c                   SELECT
172500981203     c                   WHEN      Wtiporis <> *blanks
172600981203     c                   WHEN      W§TADevfo = 'S'
172700981123     c                   movel(P)  'E'           Wtiporis
172800020422     c                   WHEN          (§DCTeuro =  'X'
172900020422     c                              or  §DCTeuro =  'P')
173000020422     c                             and  §DCTtisp <> *blanks
173100020422     c                             or   §DCTeuro =  'D'
173200020422     c                             and  §DCTtisp =  'E'
173300021111     c                             or   §DCTeuro =  'F'
173400021111     c                             and  §DCTtisp =  'E'
173500981203     c                   movel(P)  'D'           Wtiporis
173600010906     c                   z-add     §GEcmr        WImporto
173700010906     c                   z-add     3             wNrDec
173800010906     c                   z-add     6             wLungh
173900981203     c                   OTHER
174000060314      * Verifico il tipo di limite risarcibile in base al flag in dctflo
174100060314     c                   If        §dcttlrp = ' '
174200981123     c                   movel(P)  'A'           Wtiporis
174300060314     c                   z-add     6,2           WImporto
174400060314     c                   endif
174500060314     c                   If        §dcttlrp = '6'
174600060314     c                   movel(P)  'H'           Wtiporis
174700060314     c                   z-add     6,2           WImporto
174800060314     c                   endif
174900060314     c                   If        §dcttlrp = '1'
175000060314     c                   movel(P)  'G'           Wtiporis
175100060314     c                   z-add     1             WImporto
175200060314     c                   endif
175300020125     c                   z-add     2             wNrDec
175400020125     c                   z-add     6             wLungh
175500981203     c                   ENDSL
175600981123      *
175700021111   -2c                   ENDIF
175800021111   -1c                   ENDIF
175900981123      *
176000981123     C                   ENDSR
176100981026      *****************************************************************
176200981026      *   NOTE
176300981026      *****************************************************************
176400981026     C     SUB_note      BEGSR
176500981026      *
176600981027     c                   eval      *in31 = *on
176700981027     c                   clear                   yy
176800981027     c                   clear                   W35
176900981124     c                   clear                   PTCtes
177000981026      *
177100981026     C                   movel(P)  'C'           Ktpd
177200981215     C                   movel(P)  DSnumca       Knkt
177300981027     C                   movel     'T'           Kstd
177400981204     C                   movel(P)  DFAle1        Knks
177500981026     C                   movel     'D'           Ktrc
177600981027     C                   clear                   Khim
177700981027     C                   clear                   Kdim
177800981026      *
177900990531     c     kdcse         chain     fndcs000                           30
178000981026     c     *in30         DOWeq     *off
178100981026      *
178200981026      * Ogni 3 rcd letti scrivo
178300981215     c                   IF        yy = 3
178400981215     c                   exsr      noteprt
178500981215     c                   ENDIF
178600981026      *
178700981026     c                   add       1             yy
178800981026     c                   movel     dcsnot        W35(yy)
178900990531     c     kdcse         reade     fndcs000                               30
179000981026     c                   ENDDO
179100981026      *
179200990223      * Scrivo rcd residui
179300981215     c                   IF        yy > *zeros
179400981215     c                   exsr      noteprt
179500981215     c                   ENDIF
179600981026      *
179700981026     C                   ENDSR
179800981215      *****************************************************************
179900981215      *   STAMPA NOTE
180000981215      *****************************************************************
180100981215     C     NOTEPRT       BEGSR
180200981215      *
180300981215     c                   eval      PTCtes = W35(1)  +  W35(2)  +  W35(3)
180400981215     c                   IF        PTCtes <> *blanks
180500981215      *
180600981215      * La prima volta lascio uno spazio
180700981215     c                   If        *IN31 = *ON
180800990310     c                   exsr      PRT_spazio
180900981215     c                   eval      *in31 = *off
181000981215     c                   Endif
181100981215      *
181200990223     c                   exsr      WRTtesto
181300981215      *
181400981215     c                   clear                   yy
181500981215     c                   clear                   W35
181600981215     c                   clear                   PTCtes
181700981215     c                   ENDIF
181800981215      *
181900981215     C                   ENDSR
182000120404      *****************************************************************
182100120404      *   FINE AVVISO DANNO con info x polizza BRT
182200120404      *****************************************************************
182300120404     C     SUB_ACB       BEGSR
182400120404
182500120404      * questa parte è da stampare solo per i clienti senza assicurazione
182600120404      * AC Base o AC Plus
182700120404     c                   IF        wSpedAss = *on
182800120404     c                   leavesr
182900120404     c                   ENDIF
183000120404
183100120404      * lacio 2 righe prima di stampare la parte finale
183200120404     c                   exsr      PRT_spazio
183300120404
183400120418     c  n08              eval      PTCacb = TESacb(01)
183500120418     c   08              eval      PTCacb = TESacb(03)
183600120404     c  n20              write     DN35PACB
183700120404     c   20              write     DN35MACB
183800120418     c  n08              eval      PTCacb = TESacb(02)
183900120418     c   08              eval      PTCacb = TESacb(04)
184000120404     c  n20              write     DN35PACB
184100120404     c   20              write     DN35MACB
184200120404
184300120404     C                   ENDSR
184400981214      *****************************************************************
184500981214      *   SCRIVO LETTERA PER UFFICIO INTERNAZIONALE
184600981214      *****************************************************************
184700981214     C     SUB_letint    BEGSR
184800981214      *
184900981214      * Intestazione + testo
185000981214     c                   IF        *in11 = *off
185100981214     c                   except    Pintes
185200981214     c                   eval      QQ = -20
185300981214     c                   eval      *in11 = *on
185400981214     c                   ENDIF
185500981214      *
185600981214      * Memorizzo in schiera il numero C.A.
185700981214     c                   movel     V2Haac        Wanno
185800981214     c                   movel     V2Cfil        Wfil
185900981214     c                   movel     V2Cnca        w007a
186000981214      *
186100981214     c     '0'           check     w007a         yy
186200981214     c                   eval      Wnum = %subst(W007a:yy)
186300981214      *
186400981214     C                   eval      Wkca = 'N. '  +  Wanno  +  ' '  +  Wfil
186500981214     c                                     +  ' '  +  %trim(Wnum)
186600981214      *
186700981214     c                   add       21            QQ
186800981214     c                   movea     Wkca          NCA(QQ)
186900981214      *
187000981214     c                   IF        QQ = 85                                        5  C.A. per riga
187100981214     c                   except    PNUMCA
187200981214     c                   eval       QQ = -20
187300981214     c                   clear                   NCA
187400981214     c                   ENDIF
187500981214      *
187600981214      *
187700981214     C                   ENDSR
187800020124      *****************************************************************
187900020124      *   SCRIVO NOTE
188000020124      *****************************************************************
188100020124     c     wrtdcs        begsr
188200020124
188300020124     c                   clear                   dcspno
188400020124
188500020124     c                   eval      ktpd='C'
188600020124     c                   eval      knkt=dsnumca
188700020124     c                   eval      kstd=' '
188800020124     c                   eval      kdim=dateu
188900020124     c                   movel     wora          khim
189000020124     c                   movel     i00fca        knks
189100020124     c                   eval      ktrc='N'
189200020124
189300020124      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA se esiste
189400020124     c     kdcsp         setgt     fndcs01l
189500020124     c     kdcse         readpe    fndcs01l
189600020124
189700020124     c                   eval      dcstpd = ktpd
189800020124     c                   eval      dcsnkt = knkt
189900020124     c                   eval      dcsstd = kstd
190000020124     c                   eval      dcsdim = kdim
190100020124     c                   eval      dcshim = khim
190200020124     c                   eval      dcsnks = knks
190300020124     c                   eval      dcstrc = ktrc
190400020124     c                   eval      dcsatb = ' '
190500020124
190600020124     c                   move      knmus         dcspru
190700020125     c                   if        simfel <> *zeros
190800020124     c                   movel     simfel        dcspos
190900020125     c                   else
191000020125     c                   eval      dcspos = 046
191100020125     c                   endif
191200020124     c                   clear                   dcsstn
191300020124     c                   clear                   dcsft1
191400020124     c                   clear                   dcsdt1
191500020124
191600020124      * Scrivo 2 note xchè su una non ci sto con tutta la frase
191700020124     c                   add       1             dcspno
191800020124     c                   clear                   dcsnot
191900020205     c                   eval      dcsnot = 'Ins. Automatico Pratica Franchigia'
192000020124     c                   write     fndcs000
192100020124
192200020124     c                   endsr
192300980804      *****************************************************************
192400980804      *   SCRIVO STORICO FASI
192500980804      *****************************************************************
192600980804     C     WRTdcf        BEGSR
192700980804      *
192800980804     c                   clear                   fndcf000
192900980804      *
193000981204     c                   z-add     V2Haac        DCFaac
193100981204     c                   z-add     V2Cfil        DCFfil
193200981204     c                   z-add     V2Cnca        DCFnca
193300980804     c                   movel     DCTptr        DCFptr
193400980804     c     *ISO          movel     DATA_oggi     DCFdfc
193500980804     c                   movel     w0140         DCFhfc
193600981204     c                   z-add     I00fgs        DCFfev
193700980804     c                   movel     knmus         DCFpru
193800980907     c                   clear                   DCFftr
193900980907     c                   clear                   DCFdtr
194000981214      * Se PF Import scrivo rcd chiusura
194100981214     c                   if        Wimpclose = 'S'
194200981214     c                   z-add     WFchi         DCFfca
194300981214     c                   else
194400981214     c                   z-add     i00fca        DCFfca
194500981214     c                   endif
194600020124
194700020124      * Imposto il flag di pratica assicurativa in Franchigia
194800020124     c                   eval      §dcfpfra = §dctpfra
194900041117      * Se Fase 545 Valorizzo l'invio
195000041117     c                   If        DCFfca = WFAPD
195100041117     c                   eval      §dcfinvi = invio
195200041117     c                   endif
195300041117      *
195400020124     c                   movel     ddcf01        dcflet
195500020124
195600980804      *
195700980804     c                   write     fndcf000
195800981124      *
195900981124      * Se evento fortuito scrivo rcd chiusura
196000981124     c                   if        Wtiporis = 'E  '
196100981124     c                   z-add     WFchi         DCFfca
196200981124     c                   write     fndcf000
196300981124     c                   endif
196400980804      *
196500980804     C                   ENDSR
196600980804      *****************************************************************
196700980804      *   AGGIORNA TESTATA C.A.
196800980804      *****************************************************************
196900981124     C     WRTdct        BEGSR
197000981124      *
197100981214      * Se evento fortuito o PF import scrivo chiusura,
197200020717      *    quindi lascio invariato P.O. gestione,
197300020717      *    imposto fase, causale e data chiusura
197400981214     c                   IF        Wtiporis = 'E  '  or  Wimpclose = 'S'
197500981124     c                   z-add     WFchi         DCTfca
197600981124     c     *ISO          movel     DATA_oggi     DCTdch
197700981215     c                   if        Wtiporis = 'E  '
197800981215     c                   movel     WCevf         DCTcch
197900990212      * imposto = 'S' richiesta copia denuncia
198000990212     C                   eval      dctrcd = 'S'
198100990212      *
198200981215     c                   else
198300981215     c                   movel     WCint         DCTcch
198400981215     c                   endif
198500981215      *
198600981124     c                   ELSE
198700981124      *
198800981124     c                   z-add     I00fca        DCTfca
198900980804      *
199000980804      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
199100980804     c                   CLEAR                   FIDN05DS
199200980804     c                   movel     'F'           I05MOD
199300980804     c                   z-add     I00fca        I05fca
199400981204     c                   movel     DCTfpr        I05fpr
199500980804     c                   movel     DCTptr        I05tpc
199600981026     c                   if        DCTnev > *zeros
199700980804     c                   movel     'E'           I05fpe
199800980804     c                   else
199900980804     c                   movel     'N'           I05fpe
200000980804     c                   endif
200100981030     c                   if        DCTnde > *zeros
200200981030     c                   movel     'D'           I05fde
200300981102     c                   else
200400981102     c                   movel     'N'           I05fde
200500981030     c                   endif
200600980804     c                   movel     'O'           I05ffs
200700980804     c     *iso          movel     DATA_oggi     i05dta
200800981204     c                   move      DCTtad        i05tad
200900981125      * Valorizzo numero CA
201000981125     c                   z-add     DCTaac        i05aac
201100981125     c                   z-add     DCTfil        i05fil
201200981125     c                   z-add     DCTnca        i05nca
201300980804
201400980804     C                   CALL      'FIDN05R'
201500980804     c                   PARM                    KPJBA
201600980804     c                   PARM                    FIDN05DS
201700980804
201800980804     c                   IF        O05err = *blanks
201900980804     c                   movel     O05REC        DS_FNDFA
202000980804     c                   SELECT
202100980804     c                   WHEN      DFAgfs = 'P'
202200981229     c                   movel     §DCTlnpc      dctgfc
202300981023     c                   WHEN      DFAgfs = 'A'
202400981023     c                   z-add     DCTlna        dctgfc
202500981026     c                   WHEN      DFAgfs = 'S'
202600981026     c                   z-add     46            dctgfc
202700980804     c                   OTHER
202800981023     c                   z-add     DCTfil        dctgfc
202900981027     C                   ENDSL
203000980804     c                   ENDIF
203100980804      *
203200981124     c                   ENDIF
203300981124      *
203400981222     c                   clear                   dctft1
203500981222     c                   clear                   dctft2
203600020124
203700020124     c                   movel     ddct01        dctflo
203800980804      *
203900980804     c                   EXCEPT    AGGDCT
204000980804      *
204100980804     C                   ENDSR
204200981124      *****************************************************************
204300981124      * AGGIORNO FNDCD Dettaglio C.A.
204400981124      *****************************************************************
204500981124     C     WRTDCD        BEGSR
204600981124      *
204700981124     c     kDCD          setll     fndcd000
204800981124     c     kDCD          reade     fndcd000                               31
204900981124      *
205000981124      * Imposto i dati della chiusura C.A. e il flag trasmissione
205100981124     c                   DOW       *in31 = *off
205200981124     c                   z-add     DCTdch        DCDdch
205300981124     c                   movel     DCTcch        DCDcch
205400981124     c                   clear                   DCdftr
205500981124     c                   update    fndcd000
205600981124      *
205700981124     c     kDCD          reade     fndcd000                               31
205800981124     c                   ENDDO
205900981124      *
206000981124     c                   endsr
206100990310      *
206200990310      *****************************************************************
206300990310      *   STAMPO spazio
206400990310      *****************************************************************
206500990310     C     PRT_SPAZIO    BEGSR
206600990310      *
206700990310     c                   exsr      PRT_testat
206800990310      *
206900041117     C  N20              WRITE     DN35P0
207000041117     C   20              WRITE     DN35M0
207100990310      *
207200990310     c                   endsr
207300990310      *****************************************************************
207400990310      *   STAMPO TESTATA
207500990310      *****************************************************************
207600990310     C     PRT_TESTAT    BEGSR
207700990310      *
207800990310      *
207900990310     c                   if         *IN01 = *ON
208000990310     c                   WRITE     DN35PP
208100990310     c                   EVAL      *in01 = *off
208200990310     c                   endif
208300041117      *
208400041117     c                   if         *IN21 = *ON  AND *IN20
208500041117     c                   WRITE     DN35MP
208600041117     c                   EVAL      *in21 = *off
208700041117     c                   endif
208800990310      *
208900990310     c                   endsr
209000981007      *****************************************************************
209100981007      *   AZZERO CAMPI PRTF
209200981007      *****************************************************************
209300981007     C     INZPRTF       BEGSR
209400981007      *
209500990216     c                   clear                   DN35pI
209600990216     c                   clear                   DN35pO
209700990224     c                   clear                   DN35pO1
209800990224     c                   clear                   DN35pO2
209900120404     c                   clear                   DN35pO3
210000990216     c                   clear                   DN35pT
210100120404     c                   clear                   DN35pACB
210200041118      *
210300041118     c                   if        *in20
210400041118     c                   close     fidn35pm
210500041118     c                   endif
210600981007      *
210700981007     C                   ENDSR
210800981027      *****************************************************************
210900981027      *   IMPOSTO TIPO MODULO E STAMPANTE
211000981027      *****************************************************************
211100981027     C     IMPPRT        BEGSR
211200981027      *
211300981027     c                   Clear                   COMMAND
211400981027      *
211500981214      *  Ristampa
211600981203     c                   IF        *in02 = *on
211700981113     c                   movea     CMD(1)        COMMAND
211800990429     c                   eval      %subst(COMMAND:60:10) = §TLDstp1
211900041118     c                   z-add     90            lung
212000981203     c                   call      'QCMDEXC'
212100981203     c                   parm                    COMMAND
212200981203     c                   parm                    LUNG
212300981203      *
212400981203     c                   ELSE
212500120404      *  PT-Postel
212600981203     c                   movea     CMD(2)        COMMAND
212700041118     c                   z-add     90            lung
212800981203     c                   call      'QCMDEXC'
212900981203     c                   parm                    COMMAND
213000981203     c                   parm                    LUNG
213100981203      *
213200981203     c                   ENDIF
213300981027      *
213400981113      *  Apro file di stampa
213500981119     c                   open      FIDN35P
213600981113      *
213700981027     C                   ENDSR
213800981027      *****************************************************************
213900981027      *   AGGANCIO TABELLA TIPO TESTO
214000981027      *****************************************************************
214100981027     C     DECTLD        BEGSR
214200981027      *
214300981027     c                   clear                   tibs02ds
214400981027     c                   clear                   dtld
214500981027      *
214600981027     C                   movel     'C'           t02mod
214700981027     C                   movel     knsif         t02sif
214800981124     C                   movel     'TLD'         t02cod
214900981204     C                   movel(P)  DFAle1        t02ke1
215000981027      *
215100981027     C                   CALL      'TIBS02R'
215200981027     C                   PARM                    KPJBA
215300981027     C                   PARM                    TIBS02DS
215400981027      *
215500981027     C                   movel     T02uni        DTLD
215600981027      *
215700981027     C                   ENDSR
215800981120      *****************************************************************
215900981120      *   REPERISCO NUMERO PRATICA ASSICURATIVA
216000981120      *****************************************************************
216100981120     C     NUMPRN        BEGSR
216200060619
216300060619      * verifico se CA legata ad evento
216400060619      * se legata verifico se nel file delle denucnce c'è il suo numero pratica
216500060619     c                   If        dctnev > 0
216600060619
216700060619     c     kdet          chain     fndet01l
216800060619     c                   If        %found(fndet01l)
216900060619
217000060619     c                   If        detprn = 0
217100060619     c                   Clear                   FIDN20DS
217200060619     c                   Movel     'N'           I20mod
217300060619     c                   Z-add     DETaae        I20pra
217400060712     c                   Movel     DETdit        I20dit
217500060619      *
217600060619     c                   movel(P)  FIDN20DS      KPJBU
217700060619      *
217800060619     c                   CALL      'FIDN20R'
217900060619     c                   PARM                    KPJBA
218000060619      *
218100060619     c                   movel     KPJBU         FIDN20DS
218200060619      *
218300060619     c                   IF        O20err = 'E'
218400060619     c                   eval      *in28 = *ON
218500060619     c                   ELSE
218600060619     c                   movel     I20dit        DETdit
218700060619     c                   z-add     I20pra        DETpra
218800060619     c                   z-add     O20prn        DETprn
218900060619     c                   update    fndet000
219000060619     c                   endif
219100060619     c                   else
219200060620     c                   unlock    fndet01l
219300060619     c                   endif
219400060619
219500060619     c                   eval      dctdit  =   DETdit
219600060619     c                   eval      dctpra  =   DETpra
219700060619     c                   eval      dctprn  =   DETprn
219800060619     c                   endif
219900060619
220000060619     c                   else
220100981120      *
220200981120     C                   clear                   FIDN20DS
220300981120     C                   movel     'N'           I20mod
220400981120     c                   z-add     DCTaas        I20pra
220500981124     c                   movel     ORGdit        I20dit
220600001227      *
220700120404      * se il P.O. del beneficiario è SDI
220800001227      * forzo la ditta Bartolini
220900120404     c                   if        ORGdit = 'SDI'
221000001227     c                   eval      I20dit = 'BAR'
221100001227     c                   endif
221200021118      *
221300021118      * se la spedizione ha valore da assicurare esposto in bolla o
221400021118      *   assicurazione per conto: verifico se il P.O. di arrivo ha
221500021118      *   FEDEX come network
221600021118     c                   if        wSpedAss = *on
221700021118     c                   exsr      Ctr_Ntw
221800021118     c                   endif
221900981120      *
222000981120     C                   movel(P)  FIDN20DS      KPJBU
222100981120      *
222200981120     c                   CALL      'FIDN20R'
222300981120     c                   PARM                    KPJBA
222400981120      *
222500981120     C                   movel     KPJBU         FIDN20DS
222600981120      *
222700981120     c                   IF        O20err = 'E'
222800981120     c                   eval      *in28 = *ON
222900981120     c                   ELSE
223000981120     c                   movel     I20dit        DCTdit
223100981120     c                   z-add     I20pra        DCTpra
223200981120     c                   z-add     O20prn        DCTprn
223300981120     c                   ENDIF
223400060619
223500060619     c                   endif
223600981120      *
223700981120     c                   ENDSR
223800021118      *****************************************************************
223900021118      * Verifico se il P.O. di arrivo ha "FED" come network
224000021118      * (se la spedizione è assicurata)
224100021118      *****************************************************************
224200021118     C     Ctr_Ntw       BEGSR
224300021118      *
224400021118     c                   Clear                   OG143
224500021118     c     DCTlna        Chain     AZORG
224600021118if  1c                   If            %found(AZORG01L)
224700021118     c                             and ORGfva = *blanks
224800021118     c                   Movel     ORGde3        OG143
224900021118if  2c                   If        §OGntw = 'FED'
225000021118     c                   Movel     §OGntw        I20dit
225100021118e   2c                   Endif
225200021118e   1c                   Endif
225300021118      *
225400021118      * ripristino i dati dell'ultimo record letto...
225500021118     c     Wpointes      chain     AZORG                              31
225600021118      *
225700021118     C                   ENDSR
225800981203      *****************************************************************
225900981230      * DECODIFICO NAZIONE
226000981203      *****************************************************************
226100981230     C     CTRNAZ        BEGSR
226200981203      *
226300981230     C                   clear                   DS15
226400981230     C                   movel     '15'          KCOD
226500981230     C                   movel(P)  WNAZ          KKEY
226600981203     C     KTAB          CHAIN     TABEL00f                           30
226700981203      *
226800981230     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
226900981230     C                   movel     TBLUNI        DS15
227000981230     C                   endif
227100981203      *
227200981203     C                   ENDSR
227300981230      *****************************************************************
227400981230      * CONTROLLO DIVISA
227500981230      *****************************************************************
227600981230     C     CTRVLT        BEGSR
227700981230      *
227800981230     C                   MOVEL     'CV'          KCOD
227900990415     C                   MOVEL(P)  W003a         KKEY
228000981230     C     KTAB          CHAIN     TABEL00f                           30
228100981230      *
228200981230     C                   IF        *IN30 = *ON  or
228300981230     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
228400011026     C                   clear                   DSCV
228500981230     C                   ELSE
228600981230      *
228700981230     C                   movel     TBLUNI        DSCV
228800981230     C                   ENDIF
228900981230      *
229000011026     C                   ENDSR
229100010906
229200010906      *****************************************************************
229300010906      * CONVERSIONE IMPORTI IN MONETA CORRENTE
229400010906      *****************************************************************
229500010906     C     srCMB         BEGSR
229600010906      *
229700010906     C                   reset                   YEURCODS
229800010906      *
229900010906     C                   movel     W003a         yecDVI
230000010906     C                   z-add     W0133         yecIMI
230100010910     C                   movel     §gedDBA       yecDVO
230200010906      *
230300010906     C                   call      'YEURCO'
230400010906     C                   parm                    YEURCODS
230500010906      *
230600010906     C                   IF        yecESI  = ' '
230700010906     C                   z-add     yecIMO        W0133
230800011002     C                   movel     yecDVO        W003a
230900010906     C                   ENDIF
231000010906      *
231100010906     C                   ENDSR
231200010906
231300010906      ************************************************************
231400010907      * Editazione di un numero
231500010906      ************************************************************
231600010906     C     Edit          Begsr
231700010906     C*
231800010907     C                   reset                   XEDITDS
231900010906     C* numero da elaborare
232000010906     C* (WImporto ha 5 decimali, $SrcVar ha 0 decimali)
232100010906     C                   Select
232200010907     C                   When      wNrDec    = 0
232300010907     C                   eval      IedNUMERO = wImporto
232400010907     C                   When      wNrDec    = 1
232500010907     C                   eval      IedNUMERO = wImporto * 10
232600010907     C                   When      wNrDec    = 2
232700010907     C                   eval      IedNUMERO = wImporto * 100
232800010907     C                   When      wNrDec    = 3
232900010907     C                   eval      IedNUMERO = wImporto * 1000
233000010907     C                   When      wNrDec    = 4
233100010907     C                   eval      IedNUMERO = wImporto * 10000
233200010907     C                   When      wNrDec    = 5
233300010907     C                   eval      IedNUMERO = wImporto * 100000
233400010906     C                   Endsl
233500010906     C* numero decimali
233600010907     C                   eval      IedNRDEC  = wNrDec
233700010906     C* codice di editazione
233800010907     C                   eval      IedEDTCOD = '3'
233900010906     C* max lunghezza campo in output
234000010907     C                   eval      IedLENFLD = wLungh
234100010906     C*
234200010907     C                   call      'XEDIT'
234300010907     C                   parm                    XEDITDS
234400010906      *
234500010906     C                   ENDSR
234600010906
234700981123      *****************************************************************
234800981123      *   RICERCA CAPOCONTI
234900981123      *****************************************************************
235000981123     C     CAPOCON       BEGSR
235100981123      *
235200981123      *--- RICERCA CAPOCONTI
235300020606     C                   DO        50            ww
235400020606     C                   MOVE      TCU(ww)       TCUDS
235500981123     C                   IF        F56 = 'CG'
235600981123      *
235700981123     C     f4            COMP      '1'                                    21
235800981123     C     f4            COMP      '2'                                    22
235900981123     C     f4            COMP      '3'                                    23
236000981123     C     f4            COMP      '6'                                    27
236100981123      ** 1 CLIENTI   21    ** 2 FORNITORI 22    ** 3 AGENTI    23
236200981123     C     f3            comp      '0'                                242425
236300981123     C     f3            comp      'I'                                    26
236400981123      ** 0 ITALIA   25      ** 1 ESTERO   24    ** I CAPO CONTO IVA
236500981123     c                   if        *in21 = *on
236600020606     C   24              Z-ADD     KCU(ww)       KCE               4 0
236700020606     C   25              Z-ADD     KCU(ww)       KCI               4 0
236800981123     c                   end
236900981123     c                   if        *in22 = *on
237000020606     C   24              Z-ADD     KCU(ww)       KFE               4 0
237100020606     C   25              Z-ADD     KCU(ww)       KFI               4 0
237200981123     c                   end
237300981123     c                   if        *in23 = *on
237400020606     C   24              Z-ADD     KCU(ww)       KAE               4 0
237500020606     C   25              Z-ADD     KCU(ww)       KAI               4 0
237600981123     c                   end
237700020606     C   26              Z-ADD     KCU(ww)       KIVA              4 0
237800020606     C   27              Z-ADD     KCU(ww)       KBNA              4 0
237900981123      *
238000981123     C                   ENDIF
238100981123     C                   ENDDO
238200981123      *
238300981123     C                   ENDSR
238400100709      /free
238500100709       //--------------------------------------------------------------
238600100709       //?"Bonifica" campi in stampa da caratteri indesiderati         ?
238700100709       //--------------------------------------------------------------
238800100709       BEGSR  sr_xChkChar;
238900100709
239000100709         ChkNull = '1';
239100100709
239200100709         xChkChar ( TxtInOut :
239300100709                    ElencoChar :
239400100709                    TipoElenco :
239500100709                    CharSost :
239600100709                    Uppercase :
239700100709                    ChkNull :
239800100709                    CharNull :
239900100709                    Esito);
240000100709
240100100709       ENDSR;
240200100709      /end-free
240300981116      *****************************************************************
240400981116      *   ROUTINE INIZIALE
240500981116      *****************************************************************
240600981116     C     *INZSR        BEGSR
240700981116      *
240800981116     C                   Z-ADD     1             CODUT
240900981116     C                   CALL      'X§PARUT'
241000981123     C                   PARM                    UT§DSE0F
241100981116      *
241200981116     C                   MOVEL     REC80         CNCR80
241300981124      *
241400981116      *  Ricerca capoconti
241500981116     C                   EXSR      CAPOCON
241600990222
241700010413      ***
241800010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
241900010413      ***
242000010413      *
242100010413      * recupero la moneta corrente
242200010413     C                   CLEAR                   tibs02ds
242300010413      *
242400010413     C                   MOVEL     'L'           T02TLA
242500010413     C                   MOVEL     'C'           T02MOD
242600010413     C                   MOVEL     KNSIF         T02SIF
242700010413     C                   MOVEL     'GED'         T02COD
242800010906     C                   MOVEL(P)  'DANNI'       T02KE1
242900010413      *
243000010413     C                   CALL      'TIBS02R'
243100010413     C                   PARM                    KPJBA
243200010413     C                   PARM                    tibs02ds
243300010413      *
243400010910     c                   movel     T02UNI        DGEDDN
243500011026      *
243600011026      * ...ed i relativi dati
243700011026     C                   movel     §gedDBA       W003a
243800011026     C                   exsr      ctrvlt
243900011026      *
244000010413      * recupero i valori nella moneta corrente
244100010413     C                   CLEAR                   tibs02ds
244200010413      *
244300010413     C                   MOVEL     'L'           T02TLA
244400010413     C                   MOVEL     'C'           T02MOD
244500010413     C                   MOVEL     KNSIF         T02SIF
244600010413     C                   MOVEL     'GEI'         T02COD
244700010910     C                   MOVEL     §GEDDBA       T02KE1
244800010413      *
244900010413     C                   CALL      'TIBS02R'
245000010413     C                   PARM                    KPJBA
245100010413     C                   PARM                    tibs02ds
245200010413      *
245300010413     c                   movel     t02uni        DGEI
245400990226      *
245500990226     c                   clear                   tibs02ds
245600990226     c                   clear                   dstd
245700990226      *
245800990226     C                   movel     'C'           t02mod
245900990226     C                   movel     knsif         t02sif
246000990226     C                   movel     'STD'         t02cod
246100990226     C                   movel(P)  1             t02ke1
246200010910     C                   movel(P)  §gedDBA       t02ke2
246300990226      *
246400990226     C                   CALL      'TIBS02R'
246500990226     C                   PARM                    KPJBA
246600990226     C                   PARM                    TIBS02DS
246700990226      *
246800990301     C                   movel     T02uni        DSTD
246900981116
247000981116     C                   TIME                    W0140
247100981116      *
247200981116     C                   MOVE      W0140         WDTGIO
247300981116     C                   MOVEL     W0140         WORA
247400981116      *
247500981116      * UDATE IN AAAAMMGG
247600981116     C                   Z-ADD     WDTGIO        G02DAT
247700981116     C                   MOVEL     *BLANK        G02ERR
247800981116     C                   CALL      'XSRDA8'
247900981116     C                   PARM                    WLBDAT
248000981116     C                   MOVEL     G02INV        DATEU
248100981116     C     *iso          MOVEL     DATEU         DATA_oggi
248200990316     C     *eur          movel     DATA_oggi     wdataoggi
248300981116
248400981116     C     Kdct          klist
248500981204     C                   KFLD                    V2Haac
248600981204     C                   KFLD                    V2Cfil
248700981204     C                   KFLD                    V2Cnca
248800001228      *
248900981123     C     Kdet          klist
249000981123     C                   KFLD                    DCTaae
249100981123     C                   KFLD                    DCTnev
249200990205      *
249300990205     C     Kdetd         klist
249400990205     C                   KFLD                    DCTaad
249500990205     C                   KFLD                    DCTpod
249600990205     C                   KFLD                    DCTnde
249700021204      *
249800021204     C     Ktas4         klist
249900021204     C                   kfld                    DCTaas
250000021204     C                   kfld                    DCTlnp
250100021204     C                   kfld                    DCTnrs
250200021204     C                   kfld                    DCTnsp
250300021204     C                   kfld                    TA4trc
250400021204     c                   eval      TA4trc = 'A'
250500981124      *
250600981124     C     KDCD          klist
250700981204     C                   KFLD                    V2Haac
250800981204     C                   KFLD                    V2Cfil
250900981204     C                   KFLD                    V2Cnca
251000981116      *
251100981116     C     KDCS          klist
251200981116     C                   KFLD                    Ktpd
251300981116     C                   KFLD                    Knkt
251400981116     C                   KFLD                    Kstd
251500981116      *
251600981116     C     KDCSE         klist
251700981116     C                   KFLD                    KTPD
251800981116     C                   KFLD                    KNKT
251900981116     C                   KFLD                    KSTD
252000981116     C                   KFLD                    KDIM
252100981116     C                   KFLD                    KHIM
252200981116     C                   KFLD                    KNKS
252300981116     C                   KFLD                    KTRC
252400020124
252500020124     c     kdcsp         KLIST
252600020124     c                   KFLD                    ktpd
252700020124     c                   KFLD                    knkt
252800020124     c                   KFLD                    kstd
252900020124     c                   KFLD                    kdim
253000020124     c                   KFLD                    khim
253100020124     c                   KFLD                    knks
253200020124     c                   KFLD                    ktrc
253300020124     c                   KFLD                    prgnot
253400981116      *
253500981116     c     kbol          klist
253600981204     c                   kfld                    V2Haas
253700981204     c                   kfld                    V2Clnp
253800981204     c                   kfld                    V2Cnrs
253900981204     c                   kfld                    V2Cnsp
254000981116      *
254100981116     C     KTAB          KLIST
254200981116     C                   KFLD                    CODUT
254300981116     C                   KFLD                    KCOD
254400981116     C                   KFLD                    KKEY
254500990222      *
254600990222     C     KTABE         KLIST
254700990222     C                   KFLD                    CODUT
254800990222     C                   KFLD                    KCOD
254900981116
255000981116     C                   ENDSR
255100980804      *****************************************************************
255200980804     OFNDCT000  E            AGGDCT
255300980804     O                       DCTgfc
255400980804     O                       DCTfca
255500981222     O                       DCTft1
255600981222     O                       DCTft2
255700981120     O                       DCTdit
255800981120     O                       DCTpra
255900981120     O                       DCTprn
256000981125     O                       DCTcch
256100981125     O                       DCTdch
256200000302     O                       DCTrcd
256300020124     o                       dctflo
256400980804      *****************************************************************
256500981214     OPRTF198   E            PINTES           15
256600981214     o                                        +   9 'Ufficio ASSICURAZIONI MERC-
256700981214     o                                              I'
256800981214     o          E            PINTES      1
256900981214     o                                        +   9 'BOLOGNA SEDE'
257000981214     o          E            PINTES      1
257100981214     o                                        +  74 'Spett.le'
257200981214     o          E            PINTES      1
257300981214     o                                        +  74 'Ufficio INTERNAZIONALE'
257400981214     o          E            PINTES      1
257500981214     o                                        +  74 'MILANO'
257600990303     o          E            PINTES      1
257700990303     o                                        +   9 'BOLOGNA,'
257800990303     o                       wdataoggi        +   2
257900981214     o          E            PINTES      8
258000990225     o                                        +   9 'Oggetto: Invio C.A. relati-
258100981214     o                                              ve a spedizioni Import in P-
258200990225     o                                              orto Franco chiuse con caus-
258300990225     o                                              ale 06'
258400981214     o          E            PINTES      0
258500990225     o                                        +   9 'Oggetto: Invio C.A. relati-
258600990225     o                                              ve a spedizioni Import in P-
258700990225     o                                              orto Franco chiuse con caus-
258800990225     o                                              ale 06'
258900981214     o          E            PINTES      3
259000981214     o                                        +  28 'Con la presente Vi inviamo-
259100981214     o                                               documentazione relativa al-
259200981214     o                                              le seguenti C.A.'
259300981214      *
259400981214     o          E            PNUMCA      2
259500981214     O                       NCA              +  13
259600981214      *
259700981214     o          E            PFIRMA      3
259800120516     O                                        +  74 'BRT S.p.A.'
259900981214     o          E            PFIRMA      1
260000120516     o                                        +  74 'Ufficio ASSICURAZIONI MERC-
260100981214     o                                              I'
260200981214      *-------------------------------------------------------------------------
260300981214N. aaa* fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnn
260400981214nnn   *
260500981214      *****************************************************************
260600981210** CMD   (Lungh. 90)                                                                     *
260700990519OVRPRTF FIDN35P SPLFNAME(LET198) FORMTYPE(AVVDANBAR)  OUTQ(xxxxxxxxxx) HOLD(*YES)
260800990224OVRPRTF FIDN35P SPLFNAME(LET198) FORMTYPE(AVVDANBAR) OUTQ(PICCIONEDB/BARPICSTM)
260900030507OVRPRTF PRTF198 FORMTYPE(xxxxxxxxxx) OUTQ(xxxxxxxxxx) SAVE(*YES)
261000041125OVRPRTF FIDN35PM FORMTYPE(AVVDANBAR) OUTQ(xxxxxxxxxx) USRDFNDTA('
261100990302** COTE: 1-3 cod.testo; COSC: 4-6  7-9 1o e ultimo elemento ITA, 10-12 13-15 1o e ultimo elemento GB
261200120412A10001013014026
261300981123** TES1     *            (Lungh. 100)                                                              *
261400120412facendo seguito al nostro preavviso di danno del xx/xx/xxxx siamo spiacenti di confermare l'anomalia
261500120405sopra descritta.
261600981123XTESP
261700981123XJUMP
261800120412Al fine di procedere alla liquidazione, Vi preghiamo di inviare a BRT S.p.a. - Ufficio Servizi
261900120404Assicurativi - Via Enrico Mattei 42  40138 Bologna, la seguente documentazione:
262000990222XDOCP
262100981123XJUMP
262200120412Nello scusarci per lo spiacevole inconveniente, Vi ricordiamo che la Vs. filiale BRT di zona
262300120404rimane a completa disposizione per ogni eventuale ulteriore chiarimento.
262400981123XJUMP
262500120404Distinti saluti.
262600120516                                                     XXXXXXXXXXXXXXXXXXXX
262700120412following our damage notice dated xx/xx/xxxx we regret to confirm that the a.m. parcel is damaged.
262800990302XTESP
262900990302XJUMP
263000120412In order to speed up the verification and investigation process, we kindly ask you to send to
263100120404BRT S.p.A. - Ufficio Servizi Assicurativi -  Via Enrico Mattei 42 40138 Bologna, the following
263200120404documents:
263300990302XDOCP
263400990302XJUMP
263500120412We apologise for the inconvenient occured, we inform that our BRT's local branch is at your
263600120404disposal for any further information.
263700071217XJUMP
263800120404Best Regards.
263900120516                                                     XXXXXXXXXXXXXXXXXXXX
264000990302** COTP: 1-3 cod.testo; COSP: 4-6  7-9 1o e ultimo elemento ITA, 10-12 13-15 1o e ultimo elemento GB
264100060310A  001002020021
264200060310B  003004022023
264300060310C  005007024025
264400060310D  008008026026
264500060310E  009011027028
264600060310F  012014029031
264700060310G  015017032034
264800060310H  018019035036
264900981123** TESP     *            (Lungh. 100)                                                              *
265000080213            Il danno sara' risarcito nel rispetto del limite previsto dalla L. 450/85
265100071207modificata dalla 162/93, ovvero xxx xxxxxxxxx per ogni Kg di merce perduta o danneggiata.
265200080213            Il danno sara' risarcito nel rispetto del limite previsto dalla L. 450/85
265300071207modificata dalla 162/93 elevato a xxx xxxxxxxxxx per ogni Kg di merce perduta o danneggiata.
265400120412Il danno sara' risarcito con il limite di xxx xxxxxxxxxx a xxxxxxxxxx, sulla base del mandato
265500120403conferitoci di assicurare per Vostro conto e contro tutti i rischi, le merci oggetto della
265600120403spedizione di cui sopra.
265700120412Il danno sara' risarcito con il limite previsto dalla C.M.R. : xxxxxxxxxxxxxxxx
265800120412Il risarcimento non Vi potra' essere riconosciuto poiche', per le modalita' di accadimento,
265900071207rientra fra quelli considerati "fortuiti" dal Codice Civile /art. 1693 e 1694), non ravvisandosi
266000071207alcuna nostra responsabilita'.
266100120412Il danno sara' risarcito con il limite di xxx xxxxxxxxxx, valore complessivo riportato nel DDT,
266200120403giusto mandato conferitoci di assicurare per Vostro conto, contro tutti i rischi, le merci oggetto
266300120406della spedizione stessa.
266400120412Il danno sara' risarcito nel rispetto del limite previsto dall'articolo 1696 c.c. cosi' come
266500120403modificato dal d.lgs 286/2005 ovvero non superiore a xxx xxxxxxxxx per ogni chilogrammo di peso
266600120403lordo per la merce perduta o avariata.
266700071207            Il danno sara' risarcito nel rispetto del limite di EUR 6,20 per ogni Kg di merce
266800071207perduta o danneggiata.
266900080213The compensation will be recognized you according to the present law L. 450/85 amended by
267000050216the 162/93, i.e. xxx xxxxxxxxx per each Kg merchandise lost or damaged.
267100080213The compensation will be recognized you according to the present law L. 450/85 amended by
267200050216the 162/93 up to xxx xxxxxxxxxx each Kg merchandise lost or damaged.
267300120412The damage will be refund within the limit of xxx xxxxxxxxxx each xxxxxxxxxx, according to the
267400120404insurance writ which we were asked to insure on your behalf, against all risks, of this shipment.
267500120412The damage will be refund within the limit of the C.M.R. : xxxxxxxxxxxxxxxx
267600120412The refund will not be paid since the events are classified under the "fortuitous events" by the
267700120412Civil Code (art. 1693 and 1694), and we have no liability for them.
267800120412The damage will be refund within the limit of xxx xxxxxxxxxx, that is the total amount of the
267900120404delivery note, as per the amount we were asked to insure on your behalf, against all risks, of
268000120404this shipment.
268100120412The damage will be refund within the limit fixed by the article 1696 c.c. as it was modified
268200120404by the legislative decree dated 286/2005 that is the refund will not exceed xxx xxxxxxxxx per
268300120404each kilo (gross weight) of the missing or damaged goods.
268400080213You will be refund within the limit of EUR 6,20 per each kilo (gross weight) of the missing or
268500060310damaged goods.
268600050217** COTD: 1-3 doc.; COSD: 4-6  7-9 1o e ultimo e 9-12 13-15 Highlight ITA, 16-18 19-21 22-24 25-27 GB
268700120404D01001003001001023025023023
268800120404D02004005004004026027026026
268900120406D03006009006006028030028028
269000120406D04010013010010031034031031
269100120406D05014015014014035036035035
269200120406D06016018016016037039037037
269300120406D07019022019019040043040040
269400990224** TESD     *            (Lungh. 100)                                                              *
269500120404   Contestazione:
269600120404   Copia della contestazione di danno inviata alla BRT S.p.A. entro i termini di legge (8 gg.
269700120406   dalla consegna o dal reso merce - art. 1698 c.c.) e relativa prova d'invio
269800120404   D.d.t.:
269900120406   Copia del documento di trasporto accompagnatorio (d.d.t.) redatto dal mittente
270000120404   Valore merce:
270100120406   Se causale vendita: fotocopia della fattura della merce spedita completa di tutte le pagine
270200120404   Se diversa causale: fotocopia del listino di vendita o della fattura d'acquisto di tutta la
270300120406   merce spedita
270400120404   Riparazione:
270500120404   Se la merce è stata riparata: fotocopia della fattura di riparazione oppure scheda interna di
270600120406   lavorazione con listino prezzi della parti sostituite
270700120406   Se la merce non è riparabile: dichiarazione a riguardo
270800120404   Reintegro:
270900120406   Bolla di reintegro o nota credito qualora emesse
271000120404   Risarcimento:
271100120516   Vs. lettera di richiesta risarcimento danno con importo quantificato intestata alla BRT S.p.A.
271200120406   riportante gli estremi del presente avviso danno
271300120404   Surroga:
271400120404   Copia Vostro atto di quietanza firmato e timbrato dall'avente diritto, che legittima la Vostra
271500120404   surrogazione
271600120404   RIPORTANTE GLI ESTREMI DEL PRESENTE AVVISO DI PRESUNTO DANNO
271700120404   Damage claim
271800120404   Copy of the damage claim sent to BRT S.p.A. within the prescribed terms (within 8 days from the
271900120404   delivery or the return of goods article 1698 c.c.) and the relevant evidence of its sending
272000120404   Delivery note
272100120404   Copy of the original delivery note
272200120404   Value of the goods
272300120404   In case of sale: whole photocopy of the original invoice of the sent goods
272400120406   In the other cases: photocopy of the price list or of the purchase invoice of all the goods sent.
272500120404   Repair
272600120404   In case the damaged goods are fixed: copy of the invoice only relevant to the fixing or the
272700120406   internal document of the manufacturing with the price list of the replaced parts
272800120404   In case the goods are not repairable: please send us declaration
272900120404   Replacement
273000120404   Delivery note for replacement or credit note if issued
273100120404   Damage refund
273200120404   Your request of damage refund with the requested amount addressed to BRT S.p.A. with the
273300120404   details of this alleged damage notice
273400120404   Subrogation
273500120404   Copy of your release document signed and stamped by the person in charge who legitimises
273600120404   your legal subrogation
273700120404   WITH THE DETAILS OF THIS ALLEGED DAMAGE NOTICE
273800041118** TESM testo da stampare per richiesta via e-mail
273900041118Per ricevere le prossime comunicazioni via e-mail,Vi invitiamo a darne segnalazione a
274000041118                              indicando i seguenti dati : Codice xxxxxxx e-mail ___________________
274100041118Should you need the next information via e-mail, We ask you to be so kind and ask it at
274200041118                              specifying the following details : Codice xxxxxxx e-mail ____________
274300120404** TESACB testo da stampare per info su assicurazione BRT
274400120404Cogliamo l'occasione per ricordarVi che la BRT S.p.A. offre alla propria clientela, un servizio di
274500120405copertura assicurativa per conto per le merci trasportate conveniente e personalizzato.
274600120418Herewith we remember you that BRT, offer to all the customers, the possibility to insure all the
274700120418goods in a convenient and custom way.
