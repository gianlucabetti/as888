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
023300171106     d fidnBE2ds     E ds                  inz
023500980730     D FIDN05DS      E DS
023600981124     D FIDN20DS      E DS
023700980730     D TIBS02DS      E DS
023800980804     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
023900981204     D DS_TNTMD      E DS                  EXTNAME(TNTMD00F)
024000981204     d TIBS69DS      E DS
024100981204     d DS_cnaco      E DS                  extname(CNACO00F)
024200981204     d DS_cnind      E DS                  extname(CNIND00F)
024300981204     d DS_cnclp      E DS                  extname(CNCLP00F)
024400981204     d DS_fncls      E DS                  extname(FNCLS00F)
024500980507     D KPJBA         E DS
024600980729     D DTAD          E DS
024700981027     D DTLD          E DS
024800010910     D DGEDDN        E DS
024900010413     D DGEI          E DS
025000980918     D DSTB          E DS
025100981203     D DSCV          E DS
025200981230     D DS15          E DS
025300981229     D DDCT01        E DS
025400020124     d ddcf01        e ds
025500990226     D DSTD          E DS
025600041110     D TRUL44DS      E DS                                                       X Invio e-mail
025700050902     D TRTCM1DS      E DS                                                       X Invio e-mail
025800010906     D YEURCODS      E DS                  inz
025900010906     D   YECTAR      E                     inz('H')
026000010907      * Routine editazione di un numero
026100010907     D XEDITDS       E DS                  inz
026200010907     D   OedFlgErr   e                     inz(*off)
026300021118      *
026400021118     d OG143         e ds                  inz
026500021204     d DTA4A         e ds                  inz
026600980729      *----------------------------------
026700021118     d wSpedAss        s              1    inz(*off)
026800990415     D wfndpt          S              1
026900020327     D wclsflo7        S              1
027000981204     D wtibs69r        s              1
027100990222     D wpzman          S             13
027200990222     D wcoman          S             14
027300981123     D wkgman          S             11
027400980928     D wanno           s              4
027500980730     D wfil            s              3
027600980928     D wnum            s              7
027700981120     D wannop          s              4
027800981120     D wnump           s              7
027900980803     D wdataoggi       S             10
028000010328     D wdatastampa     S             10
028100071206     D wdatapreavv     S             10
028200990302     d Wdfvm           s             10
028300990302     D wtiporis        S              3
028400990302     D wtipodoc        S              3
028500990302     D wrivalsa        S              1
028600990302     D wimpclose       S              1
028700990415     D w003a           s              3
028800990415     D w005a           s              5
028900981124     D w007a           s              7
029000981124     D w007b           s              7
029100981124     D w011a           S             11
029200980803     D wsocie          S             20
029300010906     D wcmr            S             16
029400981214     D Wkca            S             19
029500981210     D command         S             90
029600990205     D lineaa          s             12
029700990205     D lineap          s             12
029800990205     D svrag           s             20
029900981215     D lung            S             15  5 inz(90)
030000980730     D XX              s              3  0
030100980731     D YY              s              3  0
030200980731     D JJ              s              3  0
030300981214     D QQ              s              3  0
030400020606     D WW              s              3  0
030500980730     D dateu           s              8  0
030600980730     D wdtgio          S              8  0
030700980730     D wora            S              6  0
030800981203     D nrr1            S              4  0
030900981203     D nrr2            S              4  0
031000980730     D w0140           S             14  0
031100990415     D w0133           S             13  3
031200041118     D w0030           S              3  0
031300980918     D w0040           S              4  0
031400981210     D w0040a          S              4  0
031500981210     D wpointes        S              3  0
031600990302     D wdaxx           S              3  0
031700990302     D waxx            S              3  0
031800990302     D wdayy           S              3  0
031900990302     D wayy            S              3  0
032000990302     D wdaH            S              3  0
032100990302     D waH             S              3  0
032200041123     D wlast           S              2  0
032300041123     D wlblank         S              2  0
032400041123     D wd44mit         S             30
032500041125     D svcom           S             90
032600041110
032700041110     D Invio           s              1                                         wrk per invio e-mail
032800041110
032900010906      *-------------
033000010906     D WImporto        S             30S 5 inz
033100010906     D WNrDec          S              2S 0 inz
033200010906     D WLungh          S              2S 0 inz
033300010906      *-------------
033400980730      *
033500980730     D DATA_eur        S               D   DATFMT(*eur)
033600980730     D DATA_oggi       S               D   DATFMT(*eur)
033700021127      *
033800990309     D W§dctport       S                   LIKE(§DCTport)
033900981214     d W§TADevfo       s                   LIKE(§TADevfo)
034000990312     d W§TADtisi       s                   LIKE(§TADtisi)
034100031211     d w§STDlpf        s                   like(§STDlpf1) inz(*loval)
034200990429     D WI00fca         S                   LIKE(I00fca)
034300001228     D Wdtaad          S                   LIKE(DCTaac)                         data stampa A.D.
034400081230     D Kcli            S                   LIKE(OBEksc)
034500980918     D Kcod            S                   LIKE(TBLcod)
034600980929     D Ktpd            S                   LIKE(DCStpd)
034700980929     D Knkt            S                   LIKE(DCSnkt)
034800980929     D Kstd            S                   LIKE(DCSstd)
034900980929     D Kdim            S                   LIKE(DCSdim)
035000980929     D Khim            S                   LIKE(DCShim)
035100980929     D Knks            S                   LIKE(DCSnks)
035200980929     D Ktrc            S                   LIKE(DCStrc)
035300981028     D Kkey            S                   LIKE(TBLkey)
035400981123     D Wtad            S                   LIKE(DCTtad)
035500981230     D wnaz            s                   LIKE(TMDnzm)
035600071206     d WFpd            s                   LIKE(DCTfca)   inz(400)
035700981125     d WFapd           s                   LIKE(DCTfca)   inz(545)
035800981125     d WFidc           s                   LIKE(DCTfca)   inz(660)
035900981125     d WFchi           s                   LIKE(DCTfca)   inz(900)
036000981215     d WCevf           s                   LIKE(DCTcch)   inz('04')
036100981215     d WCint           s                   LIKE(DCTcch)   inz('06')
036200020124
036300020125     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
036400100709
036500100709       //--------------------------------------------------------------
036600100709       //?Definizione procedure usate.                                 ?
036700100709       //--------------------------------------------------------------
036800100709
036900100709       // -?Parametri per XCHKCHAR?
037000100709      /copy gaitrasrc/srcProtoPI,XCHKCHAR
037100100709       // -?"Bonifica" stringa da caratteri indesiderati?
037200100709      /copy gaitrasrc/srcProtoPR,XCHKCHAR
037300100709
037400980731      *---------------------------------------------------------------------
037500040427      *
037600040427     C     *ENTRY        PLIST
037700040427     C                   PARM                    KPJBA
037800040427      *
037900040427     C                   MOVEL     KPJBU         fidn00ds
038000981203
038100990429      * Memorizzo fase C.A. ricevuta
038200990429     c                   z-add     I00fca        WI00fca
038300010704      *  Se ricevuta fase 999, quindi il pgm è richiamato da Ristampa C.A.
038400010704      *  forzo fase invio documenti a Consuldanni per gestione pgm
038500040428      *  Se ricevuta fase 998, quindi il pgm è richiamato da Ristampa
038600040428      *    C.A. di un giorno, forzo fase invio documenti a Consuldanni
038700040428      *    per gestione pgm
038800040428     c                   IF           WI00fca = 999
038900040428     c                             or WI00fca = 998
039000990429     c                   z-add     WFidc         I00fca
039100990429     c                   ENDIF
039200990429
039300020717      * Aggancio anagrafico fasi per tipi testo,
039400020717      * se non ci sono testi non elaboro
039500981203     c     I00fca        CHAIN     FNDFA000                           30
039600981203     c                   z-add     1             YY
039700981204
039800981204      * Memorizzo le coordinate del testo in schiera
039900981204     c     DFAle1        lookup    COTE(yy)                               30
040000990302     c   30              movel     COSC(yy)      Wtstdaa
040100981204
040200981204      * Se non ci sono testi non elaboro
040300981204     c                   IF          *IN30 = *on
040400981204
040500020717      * In base alla fase in esecuzione determino se inviare con POSTEL
040600040428     c                   eval      *in02  = ((I00fca  =  WFidc)
040700040428     c                                   and (WI00fca <> 998))
040800981204
040900981204      * Aggancio tabella TIPO TESTO
041000981204     c                   EXSR      DECtld
041100980731
041200020717      *  Lettera per Uff. Internazionale su stampante Bartolini
041300020717      *   stampa avviso danno
041400040428     c                   IF            *in02   =  *off
041500040428     c                             and WI00fca <> 998
041600120404     c                   movea     CMD(3)        COMMAND
041700981214     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
041800981214     c                   eval      %subst(COMMAND:43:10) = §TLDstp1
041900041118     c                   z-add     90            lung
042000981214     c                   call      'QCMDEXC'
042100981214     c                   parm                    COMMAND
042200981214     c                   parm                    LUNG
042300981214      *
042400981214     c                   open      PRTF198
042500981214     c                   ENDIF
042600981214
042700981203      * Stampo C.A. allegate e aggiorno testata C.A. e file fasi
042800981203     c                   IF        i00nce > 1
042900981203     c                   exsr      CA_multi
043000981203     c                   ELSE
043100981204      *
043200981204     c                   z-add     I00aas        V2Haas
043300981204     c                   z-add     I00lnp        V2Clnp
043400981204     c                   z-add     I00nrs        V2Cnrs
043500981204     c                   z-add     I00nsp        V2Cnsp
043600981204     c                   z-add     I00aac        V2Haac
043700981204     c                   z-add     I00fil        V2Cfil
043800981204     c                   z-add     I00nca        V2Cnca
043900981203     c                   exsr      CA_singola
044000981203     c                   ENDIF
044100981214
044200981214      * Stampo C.A. PF import se non ancora stampate e stampo la firma
044300981214     c                   IF        *IN11 = *on
044400981214     c                   if        QQ > *zeros
044500981214     c                   except    Pnumca
044600981214     c                   endif
044700981214     c                   except    Pfirma
044800981214     c                   ENDIF
044900981203
045000981203      * Chiudo TIBS02R
045100981203     c                   clear                   tibs02ds
045200981203     c                   movel     'C'           t02tla
045300981203     c                   call      'TIBS02R'
045400981203     c                   parm                    kpjba
045500981203     c                   parm                    tibs02ds
045600981204      * Chiudo TIBS69R
045700981204     C                   IF        Wtibs69r  <>  *BLANKS
045800981204     C                   eval      I69TLA  = 'C'
045900981204     C                   call      'TIBS69R'
046000981204     C                   parm                    TIBS69DS
046100981204     C                   ENDIF
046200981204      *
046300981204     c                   ENDIF
046400981204      *
046500040428     c                   if        WI00fca = 998
046600040428     c                   eval      *inRT   = *on
046700040427     c                   else
046800981203     C                   EVAL      *INLR = *ON
046900040427     c                   endif
047000981203      *---------------------------------------------------------------------
047100981203      *  ELABORO SUBFILE
047200981203      *---------------------------------------------------------------------
047300981203     c     CA_multi      BEGSR
047400981203      *
047500981203      * Leggo  subfile
047600981203     c                   z-add     1             nrr2
047700981204     c     nrr2          CHAIN     FI00S02                            31
047800981203     c     *IN31         DOWEQ     *OFF
047900981204      *
048000981204      * Elaboro se C.A. non esclusa e scelta non bloccata
048100981203     c                   IF        V2Csce <> 'E'  and  V2Hi82 <> '1'
048200981203     c                   exsr      CA_singola
048300981203     c                   ENDIF
048400981203      *
048500981203     c                   add       1             nrr2
048600981203     c     NRR2          CHAIN     FI00S02                            31
048700981203     c                   ENDDO
048800981203      *
048900981203     c                   ENDSR
049000981203      *---------------------------------------------------------------------
049100981203      *  ELABORO SINGOLA C.A.
049200981203      *---------------------------------------------------------------------
049300981203     c     CA_singola    BEGSR
049400981203      *
049500981203     c                   eval      *in28 = *off
049600981203     c                   clear                   page
049700981215     c                   clear                   Wtiporis
049800981203      *
049900981203      * Aggancio file bolle e C.A.
050000981203     c                   EXSR      SUB_files
050100980803      *
050200020717      * Se sped. NON è PF Import (per Europolitan solo se responsabilità
050300020717      *  Partner) stampo l'avviso danno, in caso contrario una lettera
050400020717      *  per l'ufficio internazionale
050500981214     c                   if        Wimpclose <> 'S'
050600981214     c                   EXSR      SUB_AVVDAN
050700981214     c                   else
050800981214     c                   EXSR      SUB_LETINT
050900981214     c                   endif
051000981215      * Azzero PRTF
051100981215     c                   EXSR      INZPRTF
051200981215      *   Reimposto indicatore per invio o no tramite POSTEL
051300040428     c                   eval      *in02  = ((I00fca  =  WFidc)
051400040428     c                                   and (WI00fca <> 998))
051500981203      *
051600981203      * Aggiorno file e se ho un'evento fortuito chiudo la C.A.
051700981203     c  N28              EXSR      SUB_AGGIOR
051800981203      *
051900981203     c                   ENDSR
052000981203      *****************************************************************
052100981203      *   AGGANCIO FILES
052200981203      *****************************************************************
052300981203     C     SUB_files     BEGSR
052400021118      *
052500021118     c                   Reset                   wSpedAss
052600981203      *
052700981203      * Aggancio file bolle
052800990809     c     KBOL          chain     TITAS30C                           30
052900981229      * se tipo porto recupero rileggo
053000981229     C                   movel     'TB'          kCOD
053100981229     C                   movel(P)  TAStbl        kKEY
053200981229     C     ktab          CHAIN     TABEL                              31
053300981229     C                   movel     TBLuni        DSTB
053400981229     c                   if        §TBrbl = 'R'
053500990809     c     KBOL          reade     TITAS30C                               30
053600981229     c                   endif
053700021118if  1c                   If        not *in30
053800021118     c                             and TASias >  0
053900021118     c                             and TASfcm <> 'F'
054000021118     c                   Eval      wSpedAss = *on
054100021118e   1c                   Endif
054200981203      *
054300981203      * Aggancio file mittenti/destinatari
054400981204     c                   clear                   DS_TNTMD
054500981204     c     KBOL          chain     TNTMD000                           30
054600981203      *
054700981203      * Aggancio testata C.A.
054800981203     c     KDCT          chain     FNDCT000                           30
054900981229     c                   movel     DCTflo        DDCT01
055000981214      *
055100020422      * Sped. Import in PF (per Europolitan considero solo se ho
055200020422      *  responsabilità del Partner)
055300981214     c                   clear                   Wimpclose
055400981229     c                   if        §DCTtisp = 'I'  and  §DCTport = 'F'
055500981230     c                             and  (§DCTeuro = 'X'  or
055600020422     c                                  (§DCTeuro = 'P'  and  DCTRES = 'P'))
055700981214     c                   eval      Wimpclose = 'S'
055800981214     c                   Endif
055900990415      *
056000990415      * Aggancio file mandato assicurativo
056100990415     c     KDCT          chain     FNDPT000                           30
056200990415     c                   if        *in30 = *on  or  DPTatb = 'A'
056300990415     c                   clear                   Wfndpt
056400990415     c                   else
056500990415     c                   eval      Wfndpt = 'S'
056600060315     c                   If        DPTftc = 'C' or  DPTftc = '8' or DPTFTC = 'd'
056700021118     c                   Eval      wSpedAss = *on
056800021118     c                   Endif
056900990415     c                   endif
057000981203      *
057100981203     c                   ENDSR
057200980729      *****************************************************************
057300981203      *   STAMPO LETTERA
057400980729      *****************************************************************
057500981214     C     SUB_avvdan    BEGSR
057600981203      *
057700010704      *  In caso di ristampa o invio dati a Consuldanni
057800010704      *  recupero la data dell'ultima stampa effettuata
057900001228      *
058000010328     c                   movel     wdataoggi     wdatastampa
058100010328      *
058200010704     c                   IF        WI00fca = 999  or  WI00fca = 660
058300040428     c                                            or  wI00fca = 998
058400001228     c                   EXSR      SUB_DTAAD                                     dta stampa A.D.
058500001228     c                   ENDIF
058600001228      *
058700981203     c                   EXSR      SUB_INTES                                     intestazione
058800981203      *
058900981203     c                   IF        *in28 = *on                                   errore N. pratica
059000981203     c                   z-add     99            JJ
059100981203     c                   else
059200981203     c                   EXSR      SUB_OGGE                                      oggetto
059300981203     c                   EXSR      SUB_TESTO                                     testo
059400120404     c                   EXSR      SUB_ACB                                       testo x polizza BRT
059500981203     c                   ENDIF
059600981203      *
059700981203     c                   ENDSR
059800001228      *****************************************************************
059900001228      *   RICERCO DATA ULTIMA STAMPA AVVISO DANNO DALLO STORICO FASI
060000001228      *****************************************************************
060100001228     C     SUB_dtaad     BEGSR
060200001228      *
060300001228     c                   clear                   wdtaad
060400001228      *
060500001228     c     kdct          setgt     FNDCF01L
060600001228     c     kdct          readpe    FNDCF01L
060700001228     c*
060800031216     c                   dow       NOT %eof(FNDCF01l)
060900001228     c* se la fase che leggo è uguale alla 545 recupero l'anno
061000001228     c                   if        dcffca =   Wfapd
061100001228     c                   movel     dcfdfc        wdtaad
061200010328     c     *iso          movel     dcfdfc        data_EUR
061300010328     C     *eur          movel     DATA_EUR      wdatastampa
061400001228     c                   leave
061500001228     c                   endif
061600001228     c*
061700001228     c     kdct          readpe    fndcf01l
061800001228     c*
061900001228     c                   enddo
062000001228     c*
062100001228     c                   endsr
062200071206      *****************************************************************
062300071206      *   RICERCO DATA STAMPA PREAVVISO DANNO DALLO STORICO FASI
062400071206      *****************************************************************
062500071206     C     SUB_dtapd     BEGSR
062600071206      *
062700071206      *
062800071206     c     kdct          setgt     FNDCF01L
062900071206     c     kdct          readpe    FNDCF01L
063000071206      *
063100071206     c                   dow       NOT %eof(FNDCF01l)
063200071206      * se la fase che leggo è uguale alla 400 recupero la data
063300071206     c                   if        dcffca =   Wfpd
063400071206     c     *iso          movel     dcfdfc        data_EUR
063500071206     C     *eur          movel     DATA_EUR      wdatapreavv
063600071206     c                   leave
063700071206     c                   endif
063800071206     c*
063900071206     c     kdct          readpe    fndcf01l
064000071206     c*
064100071206     c                   enddo
064200071206     c*
064300071206     c                   endsr
064400981203      *****************************************************************
064500981214      *   AGGIORNO ARCHIVI E SE HO EVENTO FORTUITO O PF IMPORT CHIUDO LA CA
064600981203      *****************************************************************
064700981203     C     SUB_aggior    BEGSR
064800981203      *
064900990429      * Se richiamato da pgm ristampa non aggiorno
065000040428     c                   IF            WI00fca <> 999
065100040428     c                             and wI00fca <> 998
065200020124
065300020125      * Aggiorno flag franchigia
065400020717      * - Se il risarcimento è calcolato con legge 450 o con CMR
065500020125      * - Se non è una c/a da chiudere
065600020125      * - Se sto facendo la fase 545
065700020125      * - Se il flag non è già impostato
065800031211      * - L'anno della spedizione è oltre il 2001 (w0040=2002)
065900020125      * - L'importo massimo risarcibile <= §stdlpf
066000031211      *   (Prima imposto tale importo "limite" da testare in base alla
066100031211      *    data spedizione)
066200031211     c                   reset                   w§STDlpf
066300031211     c                   eval      w0040 = §STDdlpf1 / 10000
066400020124
066500051123
066600051123      * Aggiorno flag franchigia
066700051123      * - Se non è una c/a da chiudere
066800051123      * - Se sto facendo la fase 545
066900051123      * - Se il flag non è già impostato
067000051123      * - L'anno della spedizione è oltre il 2001 (w0040=2002)
067100051123      * - L'importo massimo risarcibile <= §stdlpf
067200051123      *   (Prima imposto tale importo "limite" da testare in base alla
067300051123      *    data spedizione)
067400051124     c                   if        Wimpclose <> 'S'     and
067500051123     c                             wi00fca   =  wfapd   and
067600051123     c                             §dctpfra  =  *blanks and
067700051123     c                             DCTaas    >= w0040
067800051123sel 3c                   select
067900051123w   3c                   when      §STDdlpf2 <= ((TASaas*10000)+TASmgs)
068000051123     c                   eval      w§STDlpf   = §STDlpf2
068100051123w   3c                   when      §STDdlpf1 <= ((TASaas*10000)+TASmgs)
068200051123     c                   eval      w§STDlpf   = §STDlpf1
068300051123e   3c                   endsl
068400051123     c                   if        DCTipv    <= w§STDlpf
068500051123     c                   eval      §dctpfra   = 'F'
068600051123     c                   exsr      wrtdcs                                       Note
068700051123     c                   endif
068800051123e   2c                   endif
068900051123
069000051123
069100990429      *
069200981203     c                   EXSR      WRTdcf                                       Dettaglio fase
069300981203     c                   EXSR      WRTdct                                       Testata fase
069400981214     c                   if        Wtiporis = 'E  '  or  Wimpclose = 'S'
069500981214     c                   EXSR      WRTdcd                                       Segnacolli
069600990429     c                   endif
069700981203      *
069800990429     c                   ENDIF
069900990429      *
070000981203     c                   ENDSR
070100981203      *****************************************************************
070200981203      *   INTESTAZIONE
070300981203      *****************************************************************
070400981203     C     SUB_intes     BEGSR
070500041119      *
070600041119     c                   setoff                                       210107
070700020222      *
070800020222      * RISTAMPA A.D. in fase 660
070900020222      * (vedi prime 4 specifiche "C" del pgm)
071000020222     C                   clear                   PICfra
071100040428if  1C                   IF           (WI00fca  = 999
071200040428     c                              or wI00fca  = 998)
071300020226     C                             and DCTfca   = 660
071400020226     C                             and §DCTpfra = 'F'
071500020222      * - Flag "Pratica in Franchigia"
071600020222     C                   movel     §DCTpfra      PICfra
071700020222e   1C                   endif
071800980730      *
071900981120      * DATI dell'intestatario Avviso Danno
072000980928     C                   exsr      RIEcli
072100990222      *
072200990222      * salvo l'intestatario per dati alla consuldanni
072300990205     c                   clear                   svrag
072400990205     c                   eval      svrag = picrag
072500981103      *
072600981211      * Interlocutore se non ho rivalsa
072700050303     c                   eval      *in09   =  *off
072800981211     c                   IF        Wrivalsa <> 'S'
072900981103     c                   exsr      RECpercon
073000981211     c                   ENDIF
073100981007      *
073200981007      * Località + data
073300010328     c                   eval      PICdal = 'BOLOGNA, '  +  wdatastampa
073400981124      *
073500981210      * Per determinare l'azienda utilizzo il P.O. del cliente intestatario
073600981210     C     Wpointes      chain     AZORG                              31
073700981120      *
073800981125      * Se non ho il numero pratica (Avarie senza evento) lo assegno
073900981125     c                   IF        DCTprn = *zeros
074000981120     c                   exsr      NUMPRN
074100981120     c                   ENDIF
074200981120      *
074300981120     c                   IF        *IN28 = *OFF                                 N. pratica  OK
074400981211     c                   movel(P)  KBAR          Wsocie
074500981120     c                   ENDIF
074600981214      *
074700981214      * SOLO PRIMA VOLTA apro file stampa e imposto Tipo Modulo e Stampante
074800981214     c                   if        *in10 = *off
074900981214     c                   EXSR      IMPPRT
075000981214     c                   eval      *in10 = *on
075100981214     c                   endif
075200041117      * se invio via mail apro anche il file di spool per invio mail
075300041117     c                   if        *in20
075400041117     c                   open      fidn35pm
075500041117     c                   endif
075600100709      *
075700100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
075800100709     c                   if        Not *in20
075900100709     c                   eval      TxtInOut  = ds_DN35PI
076000100709     c                   exsr      sr_xChkChar
076100100709     c                   if        esito = '1'
076200100709     c                   eval      ds_DN35PI = TxtInOut
076300100709     c                   endif
076400100709     c                   endif
076500981113      *
076600980805      * Scrivo l'intestazione
076700100709      *
076800041117     c  n20              WRITE     DN35PP
076900041117     c  n20              WRITE     DN35PI
077000041117     c   20              WRITE     DN35MI
077100980730      *
077200980730     C                   ENDSR
077300980731      *****************************************************************
077400980918      *   IMPOSTO DATI CLIENTE PER STAMPA
077500980731      *****************************************************************
077600980918     C     RIEcli        BEGSR
077700981210      *
077800981210     c                   clear                   kcli
077900981210     c                   clear                   Wrivalsa
078000041117     c                   eval      invio = 'P'
078100041117     c                   setoff                                       20
078200081230      * recupero il beneficiario
078300130329     c                   clear                   fidnbeds
078400171106     c                   reset                   fidnBE2ds
078401171106     c                   eval                    ibedpd = 'S'
078403171106     c                   eval                    ibetisp = §DCTtisp
078500081230      *
078600081230     c                   movel     'B'           IBEmod
078700081230     c                   z-add     dctaac        IBEaac
078800081230     c                   z-add     dctfil        IBEfil
078900081230     c                   z-add     dctnca        IBEnca
079000081230     c                   z-add     dctaas        IBEaas
079100081230     c                   z-add     dctlnp        IBElnp
079200081230     c                   z-add     dctnrs        IBEnrs
079300081230     c                   z-add     dctnsp        IBEnsp
079400081230     c                   z-add     dctksc        IBEksc
079500081230     c                   movel     dctptr        IBEptr
079600081230     c                   movel     dctflo        IBEflo
079700081230     c                   movel     'S'           IBEtpb
079800081230     c                   If        %subst(knsif:7:1) = 'P'
079900081230     c                   eval      IBEsif = 'P'
080000081230     c                   EndIf
080100081230      *
080200081230     c                   call      'FIDNBER'
080300081230     c                   parm                    fidnbeds
080400171106     c                   parm                    fidnBE2ds
080500081230      *
080600081230     c                   If        oberag <> *blanks
080700081230      *
080800081230     c                   movel     oberag        PICrag
080900081230     c                   movel     obecap        PICcap
081000081230     c                   movel     obevia        PICind
081100081230     c                   movel     obeloc        PICloc
081200081230     c                   if        obenaz  =  *blanks
081300081230     c                   movel     obeprv        PICprv
081400081230     c                   else
081500081230     c                   movel     obenaz        wnaz
081600081230     c                   exsr      ctrnaz
081700081230     c                   movel     §15des        PICnaz
081800081230     c                   endif
081900081230     c                   movel     obeksc        Kcli
082000120410     c                   movel     oberiv        Wrivalsa
082100081230     c                   endif
082200981120      *
082300981210      * Dal P.O. dell'intestatario determino la società
082400020717      *   Se ho rivalsa o beneficiario alternativo non codificato utilizzo
082500020717      *     il P.O. cliente M/D
082600981210      *   Se non ho codice utilizzo lnp (M) o lna (D)
082700981210     c                   movel     kcli          wpointes
082800981210      *
082900981210     c                   IF        Wpointes = *zeros
083000981210     c                   IF        DCTptr = 'D'
083100981229     C                   z-add     DCTlna        Wpointes
083200981120     c                   ELSE
083300981229     C                   movel     §DCTlnpc      Wpointes
083400981120     c                   ENDIF
083500981120     c                   ENDIF
083600041117      *
083700041117      * Imposto se devo stampare in Inglese
083800041117     c                   clear                   WCLSflo7
083900041117     c                   eval      *in08 = *OFF
084000081230     c                   IF        obeing <> *blanks
084100081230     c                   eval      wclsflo7 = obeing
084200041117     c                   eval      *in08 = *on
084300041117     c                   ENDIF
084400041116      * verfico se il cliente al quale devo inviare l'avviso danno ha un indirizzo e-mail
084500041116     c                   If        kcli <> *zeros and not *in02
084600041116     c                   clear                   trul44ds
084700110519     c                   eval      D44pgm  = 'FIDN35R'
084800041116     c                   eval      D44Ksc  = Kcli
084900041117     c                   eval      D44cl   = '006'
085000041117     c                   eval      d44apl = 'C'
085100041118     c                   eval      d44nk1 = '0151' + %editc(Kcli:'X')
085200041117     c                   eval      d44tnt = '87'
085300041116     c                   eval      D44ovr  = ' '
085400041116     c                   eval      D44scope= '*CALLLVL'
085500041117     c                   eval      D44prtf = 'FIDN35P'
085600041116
085700041116     c                   IF        Wclsflo7 <> *blanks
085800071217     C                   eval      D44obj =  '*OBJM*Alleged damage Notice - Ano-
085900041122     C                             maly information n. ' +  %char(dctfil)+  '/'+
086000041116     C                             %trim(%editc(dctnca:'Z')) +
086100041118     c                             ' Year '   +   %char(dctaac)
086200041116     c                   ELSE
086300071207     C                   eval      D44obj = '*OBJM*Avviso Danno COMU-
086400041117     C                             NICAZIONE ANOMALIA N. ' +
086500041116     C                             %char(dctfil)+  '/'      +
086600041116     C                             %trim(%editc(dctnca:'Z')) +
086700041118     c                             ' Anno '  +  %char(dctaac)
086800041116     c                   ENDIF
086900041116     c                   call      'TRUL44R'
087000041116     c                   parm                    trul44ds
087100050902     c                   parm                    trtcm1ds
087200041116      *
087300041116      * se c'è stato qualche problema faccio invio in altro modo
087400041116      *
087500041116     c                   if        d44err  <> ' '
087600041117     c                   eval      invio  = 'P'
087700041117     c                   setoff                                       20
087800041116     c                   ELSE
087900041117     c                   seton                                        20
088000041116      * ovrprtf
088100120404     c                   movea     CMD(4)        svcom
088200050324if  3c                   if        I00tpb  = '*'
088300050324     c                   eval      %subst(svcom:38:16) = *blanks
088400050324x   3c                   else
088500041125     c                   eval      %subst(svcom:43:10) = d44outq
088600050324e   3c                   endif
088700041125     c                   eval      commanl = %trim(svcom)+ d44dta  + ''')'
088800041116     c                   eval      lung = %len(%trim(commanl))
088900041116     C                   CALL      'QCMDEXC'                            60
089000041116     C                   PARM                    COMMANl         500
089100041116     C                   PARM                    LUNG             15 5
089200041116      * se c'è errore
089300041116     c                   if        *in60
089400041117     c                   eval      invio = 'P'
089500041117     c                   setoff                                       20
089600041117     c                   else
089700041117     c                   eval      invio = 'M'
089800041116     c                   endif
089900041116      *
090000041116
090100041116     c                   endif
090200041117
090300041117     c                   endif
090400980918      *
090500980918     C                   ENDSR
090600981210      *****************************************************************
090700981210      *   RECUPERO IL NOMINATIVO DELL'INTERLOCUTORE
090800981210      *****************************************************************
090900981210     C     RECpercon     BEGSR
091000981210      *
091100980731     c                   CLEAR                   DCSnot
091200981002     c                   CLEAR                   PICpec
091300980731      *
091400980929     C                   movel(P)  'C'           Ktpd
091500981215     C                   movel(P)  DSnumca       Knkt
091600980929     C                   movel(P)  'N'           Kstd
091700980929     C                   clear                   Khim
091800980929     C                   clear                   Kdim
091900980929     C                   clear                   Knks
092000980929     C                   movel     'D'           Ktrc
092100980731      *
092200981119     c     KDCS          CHAIN     FNDCS000                           30
092300981026      *
092400981119     c                   IF        *IN30 = *OFF and  DCSnot <> *BLANKS
092500050303     c                   eval      *in09   =  *on
092600981103     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
092700981026     c                   ENDIF
092800980731      *
092900980731     C                   ENDSR
093000981204      *****************************************************************
093100981204      *  AGGANCIO ANAGRAFICO
093200981204      *****************************************************************
093300981204     C     CTR_ANAGRA    BEGSR
093400981204      *
093500990302     C                   clear                   DS_cnaco
093600990302     C                   clear                   DS_cnind
093700990302     C                   clear                   DS_cnclp
093800990302     C                   clear                   DS_fncls
093900990302      *
094000981204     C                   CALL      'TIBS69R'
094100981204     C                   PARM                    tibs69DS
094200981204     C                   PARM                    DS_cnaco
094300981204     C                   PARM                    DS_cnind
094400981204     C                   PARM                    DS_cnclp
094500981204     C                   PARM                    DS_fncls
094600981204      *
094700981210     C                   eval      *IN30 = (O69ERR =  '1')
094800981204     C                   eval      wtibs69r = '1'
094900981204      *
095000981204     C                   ENDSR
095100980730      *****************************************************************
095200980730      *   OGGETTO
095300980730      *****************************************************************
095400980730     C     SUB_ogge      BEGSR
095500980928
095600981215     c                   clear                   POCqtd
095700981215     c                   clear                   Wkgman
095800981215     c                   clear                   Wcoman
095900981215     c                   clear                   Wpzman
096000981123      *
096100981123      * Intestazione
096200981120     c                   movel     DCTaac        wanno
096300981120     c                   movel     DCTfil        wfil
096400981120     c                   movel     DCTnca        w007a
096500980803     c     '0'           check     w007a         yy
096600980928     c                   eval      wnum = %subst(W007a:yy)
096700981120      *
096800981120     c                   movel     DCTpra        wannop
096900981120     c                   movel     DCTprn        w007a
097000981120     c     '0'           check     w007a         yy
097100981120     c                   eval      wnump = %subst(W007a:yy)
097200990120      *
097300020327     c                   IF        Wclsflo7 <> *blanks
097400071214     C                   eval      POCogg  = 'Damage Notice N.  '
097500020717     C                                       + DCTdit + ' ' + %trim(wnump)
097600020717     C                                       + ' Year ' + wannop
097700020717     C                                       + '   DAMAGE COMUNICATION N. '
097800020717     c                                       + wfil + '/' + %trim(wnum)
097900020717     C                                       + ' Year ' + wanno
098000990222     c                   ELSE
098100071207     C                   eval      POCogg  =   'Avviso Danno N. '
098200020717     C                                       + DCTdit + ' ' + %trim(wnump)
098300020717     C                                       + ' Anno ' + wannop
098400020717     C                                       + '   COMUNICAZIONE ANOMALIA N. '
098500020717     c                                       + wfil + '/' + %trim(wnum)
098600020717     C                                       + ' Anno ' + wanno
098700990222     c                   ENDIF
098800980803      *
098900981120     c                   z-add     DCTlnp        POClnp
099000981120     c                   z-add     DCTnrs        POCnrs
099100981120     c                   z-add     DCTnsp        POCnsp
099200981120     c                   z-add     DCTaas        POCdat
099300981120     c                   movel     TAsmgs        POCdat
099400981002     c     *mdy          move      POCdat        DATA_eur
099500981002     c     *dmy          move      DATA_eur      POCdat
099600981120     c                   movel     TMDrsm        POCrsm
099700981120     c                   z-add     TASrmn        POCrmn
099800021204      * il riferimento mittente alfanum. per la sede si trova in TITA4
099900021204     c     ktas4         chain     TITA430C
100000021204     c                   if        %found(TITA430C)
100100021204     c                   movel     TA4not        DTA4A
100200021204     c                   else
100300021204     c                   clear                   DTA4A
100400021204     c                   endif
100500021204     c                   movel     §TA4Arma      POCrma
100600021204     c     §TA4Arma      comp      *blanks                            1212
100700981120     c                   movel     TMDrsd        POCrsd
100800981120     c                   movel     TMDlod        POClod
100900990118     c                   if        TMDnzd <> *blanks
101000990118     c                   movel     TMDnzd        POCprd
101100990118     c                   else
101200990118     c                   movel     TMDprd        POCprd
101300990118     c                   endif
101400981120     c                   z-add     TASncl        POCncl
101500990809     c                   z-add     TASpkf        POCpkf
101600981123      *
101700981123      * Evento
101800981203     C                   clear                   W§TADevfo
101900990312     C                   clear                   W§TADtisi
102000981123     c                   IF        DCTnev > *zeros
102100981123     c                   eval      *IN03 = *ON
102200981123     c                   movel     DCTaae        wanno
102300981123     c                   movel     DCTnev        w007a
102400981123     c     '0'           check     w007a         yy
102500981123     c                   eval      wnum = %subst(W007a:yy)
102600020717     C                   eval      POCkev = wannop + '/' + %trim(wnum)
102700060619     c     Kdet          chain(n)  FNDET000                           31
102800981123     C                   movel     DETtad        Wtad
102900981123     c                   exsr      CHTAD
103000990302     C                   movel     §TADevfo      W§TADevfo
103100990312     C                   movel     §TADtisi      W§TADtisi
103200990302     c                   SELECT
103300020327     c                   WHEN        Wclsflo7 <> *blanks
103400020717     c                                      and  §TADdesi <> *blanks
103500990302     C                   movel     §TADdesi      POCtae
103600990302     c                   WHEN        §TADdest <> *blanks
103700990302     C                   movel     §TADdest      POCtae
103800990302     c                   OTHER
103900990302     C                   movel     §TADdesc      POCtae
104000990302     c                   ENDSL
104100981123     c                   ENDIF
104200981123      *
104300980729      * Decodifico tipo anomalia
104400981123     C                   movel     DCTtad        Wtad
104500980729     c                   exsr      CHTAD
104600990302     c                   SELECT
104700020327     c                   WHEN        Wclsflo7 <> *blanks
104800020717     c                                       and  §TADdesi <> *blanks
104900990302     C                   movel     §TADdesi      POCtad
105000990302     c                   WHEN        §TADdest <> *blanks
105100990302     C                   movel     §TADdest      POCtad
105200990302     c                   OTHER
105300990302     C                   movel     §TADdesc      POCtad
105400990302     c                   ENDSL
105500981123      *
105600980729      * Descrizione anomalia
105700990224     c                   clear                   W900A
105800980729     c                   exsr      DESTAD
105900990224     c                   movel     W035a         POCdan
106000100709      *
106100100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
106200100709     c                   if        Not *in20
106300100709     c                   eval      TxtInOut  = ds_DN35PO
106400100709     c                   exsr      sr_xChkChar
106500100709     c                   if        esito = '1'
106600100709     c                   eval      ds_DN35PO = TxtInOut
106700100709     c                   endif
106800100709     c                   endif
106900990224      *
107000990224      *  Stampa prima parte dell'oggetto
107100990310     c                   exsr      PRT_testat
107200041117     c  N20              WRITE     DN35PO
107300041117     c   20              WRITE     DN35MO
107400990224      *
107500990224      *  Stampa ulteriore descrizione anomalia
107600990224     c                   EXSR      PRTDESAGG
107700981123      *
107800981123      * Quantità danneggiate
107900981123      *      Peso
108000990223     c                   IF          DCTpkd > *zeros
108100981123     c                   move(P)   DCTpkd        w007a
108200020606     c     '0'           check     w007a         ww
108300020606     c                   eval      W007b = %subst(w007a:ww)
108400020606     c     ' '           checkr    w007b         ww
108500020606     c                   if        ww = 1
108600981123     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
108700981123     c                   else
108800020606     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(ww-1))
108900020606     c                                      +  ','  +  %subst(w007b:ww:1)
109000981123     c                   endif
109100981123     c                   ENDIF
109200981123      *      Colli
109300990223     c                   IF          DCTncn > *zeros
109400981123     c                   move      DCTncn        w005a
109500020606     c     '0'           check     w005a         ww
109600020327     c                   IF        Wclsflo7 <> *blanks
109700020606     c                   eval      Wcoman = ' Parcels '  +  %subst(W005a:ww)
109800990222     c                   ELSE
109900020606     c                   eval      Wcoman = ' Colli '  +  %subst(W005a:ww)
110000981123     c                   ENDIF
110100990222     c                   ENDIF
110200981123      *      Pezzi
110300981123     c                   IF        DCTnpz > *zeros
110400981123     c                   move      DCTnpz        w005a
110500020606     c     '0'           check     w005a         ww
110600020327     c                   IF        Wclsflo7 <> *blanks
110700020606     c                   eval      Wpzman = ' Pieces '  +  %subst(W005a:ww)
110800990222     c                   ELSE
110900020606     c                   eval      Wpzman = ' Pezzi '  +  %subst(W005a:ww)
111000980729     c                   ENDIF
111100990222     c                   ENDIF
111200981123      *
111300981123     c                   eval      POCqtd  =  %trimr(Wkgman)  +
111400981123     c                                        %trimr(Wcoman)  +  %trimr(Wpzman)
111500981123      *
111600981123      * Descrizione pezzi danneggiati
111700990224     c                   clear                   W900A
111800981123     c                   IF        DCTnpz > *zeros
111900981123     c                   EXSR      DESPZD
112000981123     c                   ENDIF
112100990224     c                   movel     W035a         POCdpz
112200100709      *
112300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
112400100709     c                   if        Not *in20
112500100709     c                   eval      TxtInOut   = ds_DN35PO1
112600100709     c                   exsr      sr_xChkChar
112700100709     c                   if        esito = '1'
112800100709     c                   eval      ds_DN35PO1 = TxtInOut
112900100709     c                   endif
113000100709     c                   endif
113100980730      *
113200990224      *  Stampa seconda parte dell'oggetto
113300990310     c                   exsr      PRT_testat
113400041117     c  N20              WRITE     DN35PO1
113500041117     c   20              WRITE     DN35MO1
113600990224      *
113700990224      *  Stampa ulteriore descrizione pezzi danneggiati
113800990224     c                   EXSR      PRTDESAGG
113900120403
114000120403      //?Stampo 'Spettabile Cliente'
114100120403
114200120403     c  N20              WRITE     DN35PO3
114300120403     c   20              WRITE     DN35MO3
114400980928
114500980729     C                   ENDSR
114600980928      *****************************************************************
114700980928      *  REPERIMENTO DESCRIZIONE EVENTO
114800980928      *****************************************************************
114900980928     C     DESEVE        BEGSR
115000980929      *
115100980929     C                   movel     'E'           Ktpd
115200981204     C                   movel     DCTAAE        W011A
115300981204     C                   move      DCTNEV        W011A
115400980929     C                   movel(P)  W011A         Knkt
115500980928     C                   clear                   Khim
115600980928     C                   clear                   Kdim
115700980929     C                   clear                   Kstd
115800980929     C                   clear                   Knks
115900980929     C                   movel     'D'           Ktrc
116000980928      *
116100980928     c                   clear                   yy
116200980929     c                   clear                   W35
116300980928      *
116400980929     c     kdcse         chain     fndcs000                           30
116500980928      *
116600980928     c     *in30         DOWeq     *off
116700980929     c     yy            ANDle     3
116800980928     c                   add       1             yy
116900980928     c                   movel     dcsnot        W35(yy)
117000980929     c     kdcse         reade     fndcs000                               30
117100980928     c                   ENDDO
117200980928      *
117300980928     C                   ENDSR
117400980729      *****************************************************************
117500980729      *  REPERIMENTO DESCRIZIONE ANOMALIA
117600980729      *****************************************************************
117700980729     C     DESTAD        BEGSR
117800980729      *
117900980929     C                   movel(P)  'C'           Ktpd
118000981215     C                   movel(P)  DSnumca       Knkt
118100980929     C                   movel(P)  'A'           Kstd
118200980929     C                   clear                   Khim
118300980929     C                   clear                   Kdim
118400980929     C                   clear                   Knks
118500980929     C                   movel     'D'           Ktrc
118600980929      *
118700980803     c                   clear                   yy
118800980730      *
118900980730     c     kdcs          chain     fndcs000                           30
119000980730      *
119100980730     c     *in30         DOWeq     *off
119200990224     c     yy            ANDlt     20
119300980918     c                   add       1             yy
119400990224     c                   movel     dcsnot        DES(yy)
119500980730     c     kdcs          reade     fndcs000                               30
119600980730     c                   ENDDO
119700980730      *
119800980729     C                   ENDSR
119900981123      *****************************************************************
120000981123      *  REPERIMENTO DESCRIZIONE PEZZI DANNEGGIATI
120100981123      *****************************************************************
120200981123     C     DESPZD        BEGSR
120300981123      *
120400981123      * Descrizione pezzi mancanti
120500981123     C                   movel(P)  'C'           Ktpd
120600981215     C                   movel(P)  DSnumca       Knkt
120700981123     C                   movel(P)  'P'           Kstd
120800990224     C                   clear                   Khim
120900990224     C                   clear                   Kdim
121000990224     C                   clear                   Knks
121100981123     c                   clear                   yy
121200981123      *
121300981123     c     kdcs          chain     fndcs000                           30
121400981123      *
121500981123     c     *in30         DOWeq     *off
121600990224     c     yy            ANDlt     20
121700981124     c                   add       1             yy
121800990224     c                   movel     dcsnot        DES(yy)
121900981123     c     kdcs          reade     fndcs000                               30
122000981123     c                   ENDDO
122100981123      *
122200981124     c                   ENDSR
122300990224      *****************************************************************
122400990224      *  STAMPA DESCRIZIONI AGGIUNTIVE NELL'OGGETTO
122500990224      *****************************************************************
122600990224     C     PRTDESAGG     BEGSR
122700990224      *
122800990224     c                   clear                   POCkos
122900020606     c                   DO        11            ww
123000990224      *
123100020606     c                   IF         DES70(ww)  <> *blanks
123200020606     c                   movel     DES70(ww)     POCkos
123300100709      *
123400100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
123500100709     c                   if        Not *in20
123600100709     c                   eval      TxtInOut   = ds_DN35PO2
123700100709     c                   exsr      sr_xChkChar
123800100709     c                   if        esito = '1'
123900100709     c                   eval      ds_DN35PO2 = TxtInOut
124000100709     c                   endif
124100100709     c                   endif
124200990224      *
124300990310     c                   exsr      PRT_testat
124400041117     c  N20              WRITE     DN35PO2
124500041117     c   20              WRITE     DN35MO2
124600990224      *
124700990224     c                   ENDIF
124800990224     c                   ENDDO
124900990224      *
125000990224     c                   ENDSR
125100980729      *****************************************************************
125200980729      *  REPERIMENTO DATI TIPO ANOMALIA
125300980729      *****************************************************************
125400980729     C     CHTAD         BEGSR
125500980729      *
125600980730     c                   clear                   tibs02ds
125700980729     c                   clear                   dtad
125800980729      *
125900980729     C                   movel     'C'           t02mod
126000980729     C                   movel     knsif         t02sif
126100980729     C                   movel     'TAD'         t02cod
126200981123     C                   movel(P)  Wtad          t02ke1
126300980729      *
126400980729     C                   CALL      'TIBS02R'
126500980729     C                   PARM                    KPJBA
126600980730     C                   PARM                    TIBS02DS
126700980729      *
126800980729     C                   movel     T02uni        DTAD
126900980729      *
127000980729     C                   ENDSR
127100980730      *****************************************************************
127200980730      *   TESTO
127300980730      *****************************************************************
127400980730     C     SUB_testo     BEGSR
127500071206      * recupero data stampa preavviso
127600071206     c                   exsr      SUB_DTAPD
127700980804
127800990302      * Determino il tipo di risarcimento da applicare al cliente
127900981123     c                   EXSR      SUB_TIPRIS
128000981123
128100981124      *  Lascio 2 righe dall'oggetto prima di stampare il testo
128200050303      *  =====> 1 riga !
128300990310     c                   exsr      PRT_spazio
128400980731
128500981123      *  Stampo eventuali note aggiuntive
128600981124     c                   EXSR      SUB_NOTE
128700981123
128800990224      *  Se il valore delle prime 5 posizioni è:
128900020717      *      "XJUMP" scrivo una riga blank
129000020717      *      "XTESP" scrivo righe di testo in funzione del tipo risarcimento
129100020717      *      "XDOCP" scrivo righe di testo con i documenti da richiedere
129200990302
129300990302      *  Imposto posizioni iniziale e finali
129400020327     c                   IF          Wclsflo7 <> *blanks
129500990302     c                   eval      Wdaxx = WtstGBda
129600990302     c                   eval      Waxx  = WtstGBa
129700990302     c                   ELSE
129800990302     c                   eval      Wdaxx = WtstITAda
129900990302     c                   eval      Waxx  = WtstITAa
130000990302     c                   ENDIF
130100981123
130200990302     c     Wdaxx         DO        Waxx          XX
130300981123     c                   clear                   PTCtes
130400120412     c                   clear                   wConta            2 0
130500980803      *
130600990223     c                   SELECT
130700990223      *
130800990224     c                   WHEN      %subst(TES1(xx):1:5) = 'XJUMP'
130900990223     c                   exsr      WRTtesto
131000990223      *
131100990223     c                   WHEN      %subst(TES1(xx):1:5) = 'XTESP'
131200981123     c                   exsr      SUB_tstpar
131300990302      *       se tipo risarcimento è "E" = evento fortuito passo ai saluti
131400990420     c                   If        Wtiporis = 'E  '
131500020327     c                   If          Wclsflo7 <> *blanks
131600050208     c                   z-add     WTES1_SA_G    XX                             inglese
131700990420     c                   Else
131800050208     c                   z-add     WTES1_SA_I    XX                             italiano
131900990420     c                   Endif
132000990420     c                   Endif
132100990223      *
132200990223     c                   WHEN      %subst(TES1(xx):1:5) = 'XDOCP'
132300990224      *       gestione documenti da ricevere
132400990223     c                   exsr      SUB_tstdoc
132500981123      *
132600990223     c                   OTHER
132700981124     c                   movel(P)  TES1(xx)      PTCtes
132800990223      *       inserisco variabili nel testo standard
132900990302     c                   exsr      SUB_vartst
133000990223     c                   exsr      WRTtesto
133100990223      *
133200990223     c                   ENDSL
133300990223      *
133400980730     c                   ENDDO
133500041123      *  se non ho inviato via e-mail stampo e va via postel
133600041123     c                   if        not *in20 and not *in02
133700041118      *
133800041118     c                   movel     kcli          W0030
133900041118     c                   move      kcli          W0040
134000041118      * chiamo il trul per il recupero dell'indirizzo del mittente
134100041118     c                   clear                   trul44ds
134200041118     c                   eval      d44pgm = 'FIDN35R'
134300041118     c                   eval      d44po  = W0030
134400041118     c                   call      'TRUL44R'
134500041118     c                   parm                    trul44ds
134600050902     c                   parm                    trtcm1ds
134700041123      *
134800041123      * imposto d44mit con doppia @
134900041123     c                   clear                   wlast
135000041123     c                   clear                   wlblank
135100041123     c                   clear                   wd44mit
135200041123     c                   eval      Wlast = %scan('@':d44mit)
135300041123     c                   eval      Wlblank = %scan(' ':d44mit)
135400100727     c                   eval      wd44mit = %subst(d44mit:1:Wlast) + '@' +
135500041123     c                             %subst(d44mit:wlast+1:Wlblank)
135600041123     c
135700041118      *
135800041118      *
135900041118      *
136000041118     c                   if        w0040 <> 8888 and w0040 <> 9999
136100051013     c                                           and w0040 <> *zeros
136200041118      *
136300041118     c                   If        not *in08
136400041118     c                   movel(p)  TESM(1)       PTCtes
136500041118     c                   else
136600041118     c                   movel(p)  TESM(3)       PTCtes
136700041118     c                   endif
136800041118      *
136900041118      *  Lascio 2 righe
137000041118     c                   exsr      PRT_spazio
137100041118     c                   exsr      WRTtesto
137200041118     c                   If        not *in08
137300041118     c                   movel(P)  TESM(2)       PTCtes
137400041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
137500041118     c                   eval      %subst(ptctes:66:7) = %char(Kcli)
137600041118     c                   else
137700041118     c                   movel(P)  TESM(4)       PTCtes
137800041123     c                   eval      %subst(ptctes:1:30) = Wd44mit
137900041118     c                   eval      %subst(ptctes:73:7) = %char(Kcli)
138000041118     c                   endif
138100041118      *
138200041118     c                   exsr      WRTtesto
138300041118     c                   endif
138400041118
138500041118     c                   endif
138600980730      *
138700980730     C                   ENDSR
138800990302      *****************************************************************
138900990302      *   SCRIVO TESTO
139000990302      *****************************************************************
139100990302     C     WRTtesto      BEGSR
139200990302      *
139300071206      * se è la prima riga del testo di avviso danno imposto la data della stampa
139400071206      * di presunto avviso danno
139500080116     c                   If        xx = 1 and xx = wdaxx
139600120403     c                   eval      %subst(PTCtes:50:10)  = wdatapreavv
139700071206     c                   endif
139800071214      * inglese
139900120412     c                   If        xx = 14 and xx = wdaxx
140000120403     c                   eval      %subst(PTCtes:35:10)  = wdatapreavv
140100071214     c                   endif
140200100709      *
140300100709      * "Bonifica" campi in stampa da caratteri indesiderati da POSTEL
140400100709     c                   if        Not *in20
140500100709     c                   eval      TxtInOut  = ds_DN35PT
140600100709     c                   exsr      sr_xChkChar
140700100709     c                   if        esito = '1'
140800100709     c                   eval      ds_DN35PT = TxtInOut
140900100709     c                   endif
141000100709     c                   endif
141100071206
141200990310     c                   exsr      PRT_testat
141300041117     C  N20              WRITE     DN35PT
141400041117     C   20              WRITE     DN35MT
141500990302      *
141600990302     C                   ENDSR
141700990302      *****************************************************************
141800990302      *   TESTO PARTICOLARE PER TIPO RISARCIMENTO
141900990302      *****************************************************************
142000990302     C     SUB_tstpar    BEGSR
142100990302
142200990302      *  Ricavo le coordinate del testo legato al tipo risarcimento
142300990302     c                   z-add     1             yy
142400990302     c     Wtiporis      lookup    COTP(yy)                               30
142500990302     c   30              movel     COSP(yy)      Wrisdaa
142600990302      *
142700990302      *  Imposto posizioni iniziale e finali
142800020327     c                   IF          Wclsflo7 <> *blanks
142900990302     c                   eval      Wdayy = WrisGBda
143000990302     c                   eval      Wayy  = WrisGBa
143100990302     c                   ELSE
143200990302     c                   eval      Wdayy = WrisITAda
143300990302     c                   eval      Wayy  = WrisITAa
143400990302     c                   ENDIF
143500990302
143600990302     c     Wdayy         DO        Wayy          YY
143700990302      *
143800990302     c                   clear                   PTCtes
143900990302     c                   IF        %SUBST(TESP(yy):1:5)  <>  'XJUMP'
144000990302     c                   movel     TESP(yy)      PTCtes
144100990302      *    Inserisco variabili
144200990302     c                   exsr      INStstpar
144300990302     c                   ENDIF
144400990302      *
144500990302     c                   exsr      WRTtesto
144600990302      *
144700990302     c                   ENDDO
144800990302      *
144900990302     C                   ENDSR
145000990224      *****************************************************************
145100990224      *   INSERIMENTO VARIABILI NEL TESTO PARTICOLARE PER TIPO RIMBORSO
145200990224      *****************************************************************
145300990224     C     INStstpar     BEGSR
145400990224
145500010906     c                   exsr      Edit
145600990302
145700990224     c                   SELECT
145800020125
145900060310      *    TIPO RIMBORSO "A"  RCV 450/85
146000020125     c                   WHEN      YY = 2
146100071207     c                   eval      %subst(PTCtes:33:3)  = §GEDDBA
146200071207     c                   eval      %subst(PTCtes:37:9) =
146300020125     c                                %TRIM(%subst(OedNUMAN:1:9))
146400990224
146500990302      *    TIPO RIMBORSO "B"  Elevazione limite risarcibile
146600050216     c                   WHEN      YY = 4
146700071207     c                   eval      %subst(PTCtes:35:3)  = §GEDDBA
146800071207     c                   eval      %subst(PTCtes:39:10) =
146900010907     c                                %TRIM(%subst(OedNUMAN:1:10))
147000990224
147100990302      *    TIPO RIMBORSO "C"  Mandato per conto
147200050216     c                   WHEN      YY = 5
147300120412     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
147400120412     c                   eval      %subst(PTCtes:47:10) =
147500010907     c                                %TRIM(%subst(OedNUMAN:1:10))
147600120412     c                   eval      %subst(PTCtes:60:10) = Wdfvm
147700990224
147800990224      *    TIPO RIMBORSO "D"  CMR
147900050216     c                   WHEN      YY = 8 and  §GEcmr > *zeros
148000010907     c                   eval      WCMR  =    'D.S.P.'
148100010907     c                                      +  %subst(OedNumAN:1:6)
148200120412     c                   eval      %subst(PTCtes:64:16) = WCMR
148300990224
148400990302      *    TIPO RIMBORSO "F"  Valore complessivo in bolla
148500050216     c                   WHEN      YY = 12
148600120412     c                   eval      %subst(PTCtes:43:3)  = §GEDDBA
148700120412     c                   eval      %subst(PTCtes:47:10) =
148800010907     c                                %TRIM(%subst(OedNUMAN:1:10))
148900990302
149000060310      *    TIPO RIMBORSO "G"  RCV  286/05
149100060310     c                   WHEN      YY = 16
149200120404     c                   eval      %subst(PTCtes:54:3)  = §GEDDBA
149300120404     c                   eval      %subst(PTCtes:58:9) =
149400060310     c                                %TRIM(%subst(OedNUMAN:1:9))
149500060310
149600020130      *    inglese TIPO RIMBORSO "A"  RCV
149700060315     c                   WHEN      YY = 21
149800050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
149900050216     c                   eval      %subst(PTCtes:22:9) =
150000020130     c                                %TRIM(%subst(OedNUMAN:1:9))
150100020130
150200990302      *    inglese TIPO RIMBORSO "B"  Elevazione limite risarcibile
150300060315     c                   WHEN      YY = 23
150400050216     c                   eval      %subst(PTCtes:18:3)  = §GEDDBA
150500050216     c                   eval      %subst(PTCtes:22:10) =
150600060310     c                                %TRIM(%subst(OedNUMAN:1:10))
150700990302
150800990302      *    inglese TIPO RIMBORSO "C"  Mandato per conto
150900060315     c                   WHEN      YY = 24
151000120412     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
151100120412     c                   eval      %subst(PTCtes:51:10) =
151200010907     c                                %TRIM(%subst(OedNUMAN:1:10))
151300120404     c                   eval      %subst(PTCtes:67:10) = Wdfvm
151400990302
151500990302      *    inglese TIPO RIMBORSO "D"  CMR
151600060315     c                   WHEN      YY = 26  and  §GEcmr > *zeros
151700010907     c                   eval      WCMR  =    'D.S.P.'
151800010907     c                                      +  %subst(OedNumAN:1:6)
151900120412     c                   eval      %subst(PTCtes:60:16) = WCMR
152000990302
152100990302      *    inglese TIPO RIMBORSO "F"  Valore complessivo in bolla
152200060315     c                   WHEN      YY = 29
152300120412     c                   eval      %subst(PTCtes:47:3)  = §GEDDBA
152400120412     c                   eval      %subst(PTCtes:51:10) =
152500010907     c                                %TRIM(%subst(OedNUMAN:1:10))
152600990224
152700060310      *    inglese TIPO RIMBORSO "G"  RCV  286/05
152800060315     c                   WHEN      YY = 33
152900120412     c                   eval      %subst(PTCtes:78:3)  = §GEDDBA
153000120412     c                   eval      %subst(PTCtes:82:9) =
153100060310     c                                %TRIM(%subst(OedNUMAN:1:9))
153200060310
153300990224     c                   ENDSL
153400990224
153500990224     C                   ENDSR
153600990223      *****************************************************************
153700990224      *   GESTIONE DOCUMENTI DA RICEVERE
153800990223      *****************************************************************
153900990224     C     SUB_tstdoc    BEGSR
154000120404
154100120404      * Richiedere documento "D01" solo per Avaria e Ammanchi
154200120404     c                   IF        §TADragr = 'V'  or  §TADragr = 'A'
154300120404     c                   eval      Wtipodoc = 'D01'
154400120404     c                   exsr      SUB_prtdoc
154500120404     c                   ENDIF
154600120404
154700120404      * Richiedere documento "D02" sempre
154800120404     c                   eval      Wtipodoc = 'D02'
154900120404     c                   exsr      SUB_prtdoc
155000120404
155100120404      * Richiedere documento "D03" sempre
155200120404     c                   eval      Wtipodoc = 'D03'
155300120404     c                   exsr      SUB_prtdoc
155400120404
155500120404      * Richiedere documento "D04" solo per Avaria con importo massimo risarcibile maggiore 250 euro
155600120404     c                   IF        §TADragr = 'V' and dctipv > 250
155700120404     c                   eval      Wtipodoc = 'D04'
155800120404     c                   exsr      SUB_prtdoc
155900120404     c                   ENDIF
156000120404
156100120404      * Richiedere documento "D05" sempre
156200120404     c                   eval      Wtipodoc = 'D05'
156300120404     c                   exsr      SUB_prtdoc
156400120404
156500120404      * Richiedere documento "D06" se non ho rivalsa
156600120404      *  viceversa richiedere documento "D07"
156700990224     c                   IF        Wrivalsa <> 'S'
156800120404     c                   eval      Wtipodoc = 'D06'
156900990224     c                   ELSE
157000120404     c                   eval      Wtipodoc = 'D07'
157100990224     c                   ENDIF
157200990224     c                   exsr      SUB_prtdoc
157300981210      *
157400990224     c                   eval      *IN04 = *off
157500990224      *
157600981210     C                   ENDSR
157700990224      *****************************************************************
157800990224      *   STAMPO DOCUMENTO
157900990224      *****************************************************************
158000990224     C     SUB_prtdoc    BEGSR
158100990302
158200990302      *  Ricavo le coordinate del testo dei documenti da richiedere
158300990302     c                   z-add     1             yy
158400990302     c     Wtipodoc      lookup    COTD(yy)                               30
158500990302     c   30              movel     COSD(yy)      Wdocdaa
158600990302      *
158700990302      *  Imposto posizioni iniziale e finali
158800020327     c                   IF          Wclsflo7 <> *blanks
158900990302     c                   eval      Wdayy  =  WdocGBda
159000990302     c                   eval      Wayy   =  WdocGBa
159100990302     c                   eval      WdaH   =  WdocGBdaH
159200990302     c                   eval      WaH    =  WdocGBaH
159300990302     c                   ELSE
159400990302     c                   eval      Wdayy  =  WdocITAda
159500990302     c                   eval      Wayy   =  WdocITAa
159600990302     c                   eval      WdaH   =  WdocITAdaH
159700990302     c                   eval      WaH    =  WdocITAaH
159800990302     c                   ENDIF
159900990302
160000990302     c     Wdayy         DO        Wayy          YY
160100990302      *
160200990302     c                   eval      *IN04 =  (yy <= WdaH  AND  yy >= WaH)
160300990224     c                   clear                   PTCtes
160400990224      *
160500990302     c                   IF          %SUBST(TESD(yy):1:5)  <>  'XJUMP'
160600120412      * imposto il numero a fianco della intestazione del documento richiesto
160700120412     c                   IF        wdayy = yy
160800120412     c                   eval      wConta += 1
160900120412     c                   eval      PTCtes = '  ' + %editc(wConta:'Z') +
161000120412     c                             '. ' + %trim(TESD(yy))
161100120412     c                   ELSE
161200120412     c                   movel(P)  TESD(yy)      PTCtes
161300120412     c                   ENDIF
161400990302     c                   exsr      SUB_vardoc
161500990224     c                   ENDIF
161600990224      *
161700990224     c                   exsr      WRTtesto
161800990224     c                   ENDDO
161900990224      *
162000990224     C                   ENDSR
162100990302      *****************************************************************
162200990302      *   INSERISCO VARIABILI NEI DOCUMENTI DA RICHIEDERE
162300990302      *****************************************************************
162400990302     C     SUB_vardoc    BEGSR
162500990302      *
162600990302      *
162700990302     C                   ENDSR
162800990302      *****************************************************************
162900990302      *   INSERISCO VARIABILI NEL TESTO
163000990302      *****************************************************************
163100990302     C     SUB_vartst    BEGSR
163200990302      *
163300120404      *    Inserisco nome società
163400120404     c                   IF        XX = WTES1_SO_G  or                          inglese
163500120404     c                             XX = WTES1_SO_I                              italiano
163600120517     c                   eval      %subst(PTCtes:54:20) = %TRIM(Wsocie)
163700120404     c                   endif
163800990302      *
163900990302     C                   ENDSR
164000981123      *****************************************************************
164100981123      *   DETERMINO IL TIPO RISARCIMENTO
164200981123      *****************************************************************
164300981123     C     SUB_tipris    BEGSR
164400981123
164500010906     c                   clear                   wImporto
164600010906     c                   clear                   wNrDec
164700010906     c                   clear                   wLungh
164800981203
164900020724      * Se PA EXPORT o PF IMPORT, non DPD, e non è evento fortuito
165000020717      *    abbiamo il tipo risarcimento "D"
165100020717      *
165200021111    1c                   IF        W§TADevfo <> 'S'  AND
165300020724     C                             ( (§DCTtisp = 'E' and  §DCTport = 'A') OR
165400020724     C                               (§DCTtisp = 'I' and  §DCTport = 'F'
165500020724     c                                               and  §DCTeuro <> 'D') )
165600020717      *
165700981203     c                   movel(P)  'D'           Wtiporis
165800010906     c                   z-add     §GEcmr        WImporto
165900010906     c                   z-add     3             wNrDec
166000010906     c                   z-add     6             wLungh
166100021111   x1c                   ELSE
166200981124
166300020717      * Se è esposto il valore in bolla (non calcolato dalla fatturazione)
166400020717      *   tipo risarcimento "F"
166500021111    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
166600981123     c                   movel(P)  'F'           Wtiporis
166700011114     c                   if        TASias <> *zeros and TASvas = *blanks
166800011114     c                   movel     'ITL'         TASvas
166900011114     c                   endif
167000990415     c                   z-add     TASias        W0133
167100990415     c                   movel     TASvas        W003a
167200010910     C                   if        TASvas <> §gedDBA
167300010910     C                   exsr      srCMB
167400981203     c                   endif
167500010906     c                   z-add     W0133         wImporto
167600010906     c                   z-add     §cvDEC        wNrDec
167700010906     c                   z-add     10            wLungh
167800021111   x2c                   ELSE
167900981123      *
168000990415      * Cliente assicurato
168100021111    3c                   IF        Wfndpt = 'S'
168200990415      *   Importo Assicurato
168300021111    4c                   IF        DPTvlm > *zeros
168400990415     c                   z-add     DPTvlm        W0133
168500990415     c                   movel     DPTvvm        W003a
168600010910     c                   IF        DPTvvm <> §gedDBA
168700010906     C                   exsr      srCMB
168800990415     c                   ENDIF
168900010907     c                   z-add     W0133         WImporto
169000010906     c                   z-add     §cvDEC        wNrDec
169100010906     c                   z-add     10            wLungh
169200021111   -4c                   ENDIF
169300990415      *   Decodifica tipo valore
169400990415     c                   SELECT
169500990415     c                   WHEN      DPTfvm = '1'
169600120412     c  n08              movel(P)  'collo'       Wdfvm
169700120412     c   08              movel(P)  'parcel'      Wdfvm
169800990415     c                   WHEN      DPTfvm = '2'
169900120412     c  n08              movel(P)  'spedizione'  Wdfvm
170000120412     c   08              movel(P)  'shipment'    Wdfvm
170100990415     c                   WHEN      DPTfvm = '3'
170200120412     c  n08              movel(P)  'Kg merce'    Wdfvm
170300120412     c   08              movel(P)  'weight Kg.'  Wdfvm
170400990415     c                   OTHER
170500990415     c                   clear                   Wdfvm
170600990415     c                   ENDSL
170700020717      *   Tipo Risarcimento: "C" mandato per conto,
170800020717      *                      "B" elevazione del limite risarcibile
170900060315      *
171000021111    4c                   IF        DPTftc = '8'  or  DPTftc  =  'C'
171100060315     c                             or DPTftc = 'd'
171200990415     c                   movel     'C'           Wtiporis
171300990415     c                   ELSE
171400020717      * Se c'è un evento fortuito non considero
171500020717      *   l'elevazione limite risarcibile
171600991025     c                   if        W§TADevfo <> 'S'
171700990415     c                   movel     'B'           Wtiporis
171800991025     c                   ENDIF
171900991025      *
172000021111   -4c                   ENDIF
172100990415      *
172200021111   -3c                   ENDIF
172300981204
172400021111      * Se non assicurato e non è un evento fortuito (tipo risarcimento
172500021111      *  "E") si applica la legge 450 (tipo risarcimento "A") o il CMR
172600021111      *  (tipo risarcimento "D")  se PA IMPORT o PF EXPORT o DPD EXPORT
172700021111      *  o FEDEX EXPORT
172800981203     c                   SELECT
172900981203     c                   WHEN      Wtiporis <> *blanks
173000981203     c                   WHEN      W§TADevfo = 'S'
173100981123     c                   movel(P)  'E'           Wtiporis
173200020422     c                   WHEN          (§DCTeuro =  'X'
173300020422     c                              or  §DCTeuro =  'P')
173400020422     c                             and  §DCTtisp <> *blanks
173500020422     c                             or   §DCTeuro =  'D'
173600020422     c                             and  §DCTtisp =  'E'
173700021111     c                             or   §DCTeuro =  'F'
173800021111     c                             and  §DCTtisp =  'E'
173900981203     c                   movel(P)  'D'           Wtiporis
174000010906     c                   z-add     §GEcmr        WImporto
174100010906     c                   z-add     3             wNrDec
174200010906     c                   z-add     6             wLungh
174300981203     c                   OTHER
174400060314      * Verifico il tipo di limite risarcibile in base al flag in dctflo
174500060314     c                   If        §dcttlrp = ' '
174600981123     c                   movel(P)  'A'           Wtiporis
174700060314     c                   z-add     6,2           WImporto
174800060314     c                   endif
174900060314     c                   If        §dcttlrp = '6'
175000060314     c                   movel(P)  'H'           Wtiporis
175100060314     c                   z-add     6,2           WImporto
175200060314     c                   endif
175300060314     c                   If        §dcttlrp = '1'
175400060314     c                   movel(P)  'G'           Wtiporis
175500060314     c                   z-add     1             WImporto
175600060314     c                   endif
175700020125     c                   z-add     2             wNrDec
175800020125     c                   z-add     6             wLungh
175900981203     c                   ENDSL
176000981123      *
176100021111   -2c                   ENDIF
176200021111   -1c                   ENDIF
176300981123      *
176400981123     C                   ENDSR
176500981026      *****************************************************************
176600981026      *   NOTE
176700981026      *****************************************************************
176800981026     C     SUB_note      BEGSR
176900981026      *
177000981027     c                   eval      *in31 = *on
177100981027     c                   clear                   yy
177200981027     c                   clear                   W35
177300981124     c                   clear                   PTCtes
177400981026      *
177500981026     C                   movel(P)  'C'           Ktpd
177600981215     C                   movel(P)  DSnumca       Knkt
177700981027     C                   movel     'T'           Kstd
177800981204     C                   movel(P)  DFAle1        Knks
177900981026     C                   movel     'D'           Ktrc
178000981027     C                   clear                   Khim
178100981027     C                   clear                   Kdim
178200981026      *
178300990531     c     kdcse         chain     fndcs000                           30
178400981026     c     *in30         DOWeq     *off
178500981026      *
178600981026      * Ogni 3 rcd letti scrivo
178700981215     c                   IF        yy = 3
178800981215     c                   exsr      noteprt
178900981215     c                   ENDIF
179000981026      *
179100981026     c                   add       1             yy
179200981026     c                   movel     dcsnot        W35(yy)
179300990531     c     kdcse         reade     fndcs000                               30
179400981026     c                   ENDDO
179500981026      *
179600990223      * Scrivo rcd residui
179700981215     c                   IF        yy > *zeros
179800981215     c                   exsr      noteprt
179900981215     c                   ENDIF
180000981026      *
180100981026     C                   ENDSR
180200981215      *****************************************************************
180300981215      *   STAMPA NOTE
180400981215      *****************************************************************
180500981215     C     NOTEPRT       BEGSR
180600981215      *
180700981215     c                   eval      PTCtes = W35(1)  +  W35(2)  +  W35(3)
180800981215     c                   IF        PTCtes <> *blanks
180900981215      *
181000981215      * La prima volta lascio uno spazio
181100981215     c                   If        *IN31 = *ON
181200990310     c                   exsr      PRT_spazio
181300981215     c                   eval      *in31 = *off
181400981215     c                   Endif
181500981215      *
181600990223     c                   exsr      WRTtesto
181700981215      *
181800981215     c                   clear                   yy
181900981215     c                   clear                   W35
182000981215     c                   clear                   PTCtes
182100981215     c                   ENDIF
182200981215      *
182300981215     C                   ENDSR
182400120404      *****************************************************************
182500120404      *   FINE AVVISO DANNO con info x polizza BRT
182600120404      *****************************************************************
182700120404     C     SUB_ACB       BEGSR
182800120404
182900120404      * questa parte è da stampare solo per i clienti senza assicurazione
183000120404      * AC Base o AC Plus
183100120404     c                   IF        wSpedAss = *on
183200120404     c                   leavesr
183300120404     c                   ENDIF
183400120404
183500120404      * lacio 2 righe prima di stampare la parte finale
183600120404     c                   exsr      PRT_spazio
183700120404
183800120418     c  n08              eval      PTCacb = TESacb(01)
183900120418     c   08              eval      PTCacb = TESacb(03)
184000120404     c  n20              write     DN35PACB
184100120404     c   20              write     DN35MACB
184200120418     c  n08              eval      PTCacb = TESacb(02)
184300120418     c   08              eval      PTCacb = TESacb(04)
184400120404     c  n20              write     DN35PACB
184500120404     c   20              write     DN35MACB
184600120404
184700120404     C                   ENDSR
184800981214      *****************************************************************
184900981214      *   SCRIVO LETTERA PER UFFICIO INTERNAZIONALE
185000981214      *****************************************************************
185100981214     C     SUB_letint    BEGSR
185200981214      *
185300981214      * Intestazione + testo
185400981214     c                   IF        *in11 = *off
185500981214     c                   except    Pintes
185600981214     c                   eval      QQ = -20
185700981214     c                   eval      *in11 = *on
185800981214     c                   ENDIF
185900981214      *
186000981214      * Memorizzo in schiera il numero C.A.
186100981214     c                   movel     V2Haac        Wanno
186200981214     c                   movel     V2Cfil        Wfil
186300981214     c                   movel     V2Cnca        w007a
186400981214      *
186500981214     c     '0'           check     w007a         yy
186600981214     c                   eval      Wnum = %subst(W007a:yy)
186700981214      *
186800981214     C                   eval      Wkca = 'N. '  +  Wanno  +  ' '  +  Wfil
186900981214     c                                     +  ' '  +  %trim(Wnum)
187000981214      *
187100981214     c                   add       21            QQ
187200981214     c                   movea     Wkca          NCA(QQ)
187300981214      *
187400981214     c                   IF        QQ = 85                                        5  C.A. per riga
187500981214     c                   except    PNUMCA
187600981214     c                   eval       QQ = -20
187700981214     c                   clear                   NCA
187800981214     c                   ENDIF
187900981214      *
188000981214      *
188100981214     C                   ENDSR
188200020124      *****************************************************************
188300020124      *   SCRIVO NOTE
188400020124      *****************************************************************
188500020124     c     wrtdcs        begsr
188600020124
188700020124     c                   clear                   dcspno
188800020124
188900020124     c                   eval      ktpd='C'
189000020124     c                   eval      knkt=dsnumca
189100020124     c                   eval      kstd=' '
189200020124     c                   eval      kdim=dateu
189300020124     c                   movel     wora          khim
189400020124     c                   movel     i00fca        knks
189500020124     c                   eval      ktrc='N'
189600020124
189700020124      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA se esiste
189800020124     c     kdcsp         setgt     fndcs01l
189900020124     c     kdcse         readpe    fndcs01l
190000020124
190100020124     c                   eval      dcstpd = ktpd
190200020124     c                   eval      dcsnkt = knkt
190300020124     c                   eval      dcsstd = kstd
190400020124     c                   eval      dcsdim = kdim
190500020124     c                   eval      dcshim = khim
190600020124     c                   eval      dcsnks = knks
190700020124     c                   eval      dcstrc = ktrc
190800020124     c                   eval      dcsatb = ' '
190900020124
191000020124     c                   move      knmus         dcspru
191100020125     c                   if        simfel <> *zeros
191200020124     c                   movel     simfel        dcspos
191300020125     c                   else
191400020125     c                   eval      dcspos = 046
191500020125     c                   endif
191600020124     c                   clear                   dcsstn
191700020124     c                   clear                   dcsft1
191800020124     c                   clear                   dcsdt1
191900020124
192000020124      * Scrivo 2 note xchè su una non ci sto con tutta la frase
192100020124     c                   add       1             dcspno
192200020124     c                   clear                   dcsnot
192300020205     c                   eval      dcsnot = 'Ins. Automatico Pratica Franchigia'
192400020124     c                   write     fndcs000
192500020124
192600020124     c                   endsr
192700980804      *****************************************************************
192800980804      *   SCRIVO STORICO FASI
192900980804      *****************************************************************
193000980804     C     WRTdcf        BEGSR
193100980804      *
193200980804     c                   clear                   fndcf000
193300980804      *
193400981204     c                   z-add     V2Haac        DCFaac
193500981204     c                   z-add     V2Cfil        DCFfil
193600981204     c                   z-add     V2Cnca        DCFnca
193700980804     c                   movel     DCTptr        DCFptr
193800980804     c     *ISO          movel     DATA_oggi     DCFdfc
193900980804     c                   movel     w0140         DCFhfc
194000981204     c                   z-add     I00fgs        DCFfev
194100980804     c                   movel     knmus         DCFpru
194200980907     c                   clear                   DCFftr
194300980907     c                   clear                   DCFdtr
194400981214      * Se PF Import scrivo rcd chiusura
194500981214     c                   if        Wimpclose = 'S'
194600981214     c                   z-add     WFchi         DCFfca
194700981214     c                   else
194800981214     c                   z-add     i00fca        DCFfca
194900981214     c                   endif
195000020124
195100020124      * Imposto il flag di pratica assicurativa in Franchigia
195200020124     c                   eval      §dcfpfra = §dctpfra
195300041117      * Se Fase 545 Valorizzo l'invio
195400041117     c                   If        DCFfca = WFAPD
195500041117     c                   eval      §dcfinvi = invio
195600041117     c                   endif
195700041117      *
195800020124     c                   movel     ddcf01        dcflet
195900020124
196000980804      *
196100980804     c                   write     fndcf000
196200981124      *
196300981124      * Se evento fortuito scrivo rcd chiusura
196400981124     c                   if        Wtiporis = 'E  '
196500981124     c                   z-add     WFchi         DCFfca
196600981124     c                   write     fndcf000
196700981124     c                   endif
196800980804      *
196900980804     C                   ENDSR
197000980804      *****************************************************************
197100980804      *   AGGIORNA TESTATA C.A.
197200980804      *****************************************************************
197300981124     C     WRTdct        BEGSR
197400981124      *
197500981214      * Se evento fortuito o PF import scrivo chiusura,
197600020717      *    quindi lascio invariato P.O. gestione,
197700020717      *    imposto fase, causale e data chiusura
197800981214     c                   IF        Wtiporis = 'E  '  or  Wimpclose = 'S'
197900981124     c                   z-add     WFchi         DCTfca
198000981124     c     *ISO          movel     DATA_oggi     DCTdch
198100981215     c                   if        Wtiporis = 'E  '
198200981215     c                   movel     WCevf         DCTcch
198300990212      * imposto = 'S' richiesta copia denuncia
198400990212     C                   eval      dctrcd = 'S'
198500990212      *
198600981215     c                   else
198700981215     c                   movel     WCint         DCTcch
198800981215     c                   endif
198900981215      *
199000981124     c                   ELSE
199100981124      *
199200981124     c                   z-add     I00fca        DCTfca
199300980804      *
199400980804      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
199500980804     c                   CLEAR                   FIDN05DS
199600980804     c                   movel     'F'           I05MOD
199700980804     c                   z-add     I00fca        I05fca
199800981204     c                   movel     DCTfpr        I05fpr
199900980804     c                   movel     DCTptr        I05tpc
200000981026     c                   if        DCTnev > *zeros
200100980804     c                   movel     'E'           I05fpe
200200980804     c                   else
200300980804     c                   movel     'N'           I05fpe
200400980804     c                   endif
200500981030     c                   if        DCTnde > *zeros
200600981030     c                   movel     'D'           I05fde
200700981102     c                   else
200800981102     c                   movel     'N'           I05fde
200900981030     c                   endif
201000980804     c                   movel     'O'           I05ffs
201100980804     c     *iso          movel     DATA_oggi     i05dta
201200981204     c                   move      DCTtad        i05tad
201300981125      * Valorizzo numero CA
201400981125     c                   z-add     DCTaac        i05aac
201500981125     c                   z-add     DCTfil        i05fil
201600981125     c                   z-add     DCTnca        i05nca
201700980804
201800980804     C                   CALL      'FIDN05R'
201900980804     c                   PARM                    KPJBA
202000980804     c                   PARM                    FIDN05DS
202100980804
202200980804     c                   IF        O05err = *blanks
202300980804     c                   movel     O05REC        DS_FNDFA
202400980804     c                   SELECT
202500980804     c                   WHEN      DFAgfs = 'P'
202600981229     c                   movel     §DCTlnpc      dctgfc
202700981023     c                   WHEN      DFAgfs = 'A'
202800981023     c                   z-add     DCTlna        dctgfc
202900981026     c                   WHEN      DFAgfs = 'S'
203000981026     c                   z-add     46            dctgfc
203100980804     c                   OTHER
203200981023     c                   z-add     DCTfil        dctgfc
203300981027     C                   ENDSL
203400980804     c                   ENDIF
203500980804      *
203600981124     c                   ENDIF
203700981124      *
203800981222     c                   clear                   dctft1
203900981222     c                   clear                   dctft2
204000020124
204100020124     c                   movel     ddct01        dctflo
204200980804      *
204300980804     c                   EXCEPT    AGGDCT
204400980804      *
204500980804     C                   ENDSR
204600981124      *****************************************************************
204700981124      * AGGIORNO FNDCD Dettaglio C.A.
204800981124      *****************************************************************
204900981124     C     WRTDCD        BEGSR
205000981124      *
205100981124     c     kDCD          setll     fndcd000
205200981124     c     kDCD          reade     fndcd000                               31
205300981124      *
205400981124      * Imposto i dati della chiusura C.A. e il flag trasmissione
205500981124     c                   DOW       *in31 = *off
205600981124     c                   z-add     DCTdch        DCDdch
205700981124     c                   movel     DCTcch        DCDcch
205800981124     c                   clear                   DCdftr
205900981124     c                   update    fndcd000
206000981124      *
206100981124     c     kDCD          reade     fndcd000                               31
206200981124     c                   ENDDO
206300981124      *
206400981124     c                   endsr
206500990310      *
206600990310      *****************************************************************
206700990310      *   STAMPO spazio
206800990310      *****************************************************************
206900990310     C     PRT_SPAZIO    BEGSR
207000990310      *
207100990310     c                   exsr      PRT_testat
207200990310      *
207300041117     C  N20              WRITE     DN35P0
207400041117     C   20              WRITE     DN35M0
207500990310      *
207600990310     c                   endsr
207700990310      *****************************************************************
207800990310      *   STAMPO TESTATA
207900990310      *****************************************************************
208000990310     C     PRT_TESTAT    BEGSR
208100990310      *
208200990310      *
208300990310     c                   if         *IN01 = *ON
208400990310     c                   WRITE     DN35PP
208500990310     c                   EVAL      *in01 = *off
208600990310     c                   endif
208700041117      *
208800041117     c                   if         *IN21 = *ON  AND *IN20
208900041117     c                   WRITE     DN35MP
209000041117     c                   EVAL      *in21 = *off
209100041117     c                   endif
209200990310      *
209300990310     c                   endsr
209400981007      *****************************************************************
209500981007      *   AZZERO CAMPI PRTF
209600981007      *****************************************************************
209700981007     C     INZPRTF       BEGSR
209800981007      *
209900990216     c                   clear                   DN35pI
210000990216     c                   clear                   DN35pO
210100990224     c                   clear                   DN35pO1
210200990224     c                   clear                   DN35pO2
210300120404     c                   clear                   DN35pO3
210400990216     c                   clear                   DN35pT
210500120404     c                   clear                   DN35pACB
210600041118      *
210700041118     c                   if        *in20
210800041118     c                   close     fidn35pm
210900041118     c                   endif
211000981007      *
211100981007     C                   ENDSR
211200981027      *****************************************************************
211300981027      *   IMPOSTO TIPO MODULO E STAMPANTE
211400981027      *****************************************************************
211500981027     C     IMPPRT        BEGSR
211600981027      *
211700981027     c                   Clear                   COMMAND
211800981027      *
211900981214      *  Ristampa
212000981203     c                   IF        *in02 = *on
212100981113     c                   movea     CMD(1)        COMMAND
212200990429     c                   eval      %subst(COMMAND:60:10) = §TLDstp1
212300041118     c                   z-add     90            lung
212400981203     c                   call      'QCMDEXC'
212500981203     c                   parm                    COMMAND
212600981203     c                   parm                    LUNG
212700981203      *
212800981203     c                   ELSE
212900120404      *  PT-Postel
213000981203     c                   movea     CMD(2)        COMMAND
213100041118     c                   z-add     90            lung
213200981203     c                   call      'QCMDEXC'
213300981203     c                   parm                    COMMAND
213400981203     c                   parm                    LUNG
213500981203      *
213600981203     c                   ENDIF
213700981027      *
213800981113      *  Apro file di stampa
213900981119     c                   open      FIDN35P
214000981113      *
214100981027     C                   ENDSR
214200981027      *****************************************************************
214300981027      *   AGGANCIO TABELLA TIPO TESTO
214400981027      *****************************************************************
214500981027     C     DECTLD        BEGSR
214600981027      *
214700981027     c                   clear                   tibs02ds
214800981027     c                   clear                   dtld
214900981027      *
215000981027     C                   movel     'C'           t02mod
215100981027     C                   movel     knsif         t02sif
215200981124     C                   movel     'TLD'         t02cod
215300981204     C                   movel(P)  DFAle1        t02ke1
215400981027      *
215500981027     C                   CALL      'TIBS02R'
215600981027     C                   PARM                    KPJBA
215700981027     C                   PARM                    TIBS02DS
215800981027      *
215900981027     C                   movel     T02uni        DTLD
216000981027      *
216100981027     C                   ENDSR
216200981120      *****************************************************************
216300981120      *   REPERISCO NUMERO PRATICA ASSICURATIVA
216400981120      *****************************************************************
216500981120     C     NUMPRN        BEGSR
216600060619
216700060619      * verifico se CA legata ad evento
216800060619      * se legata verifico se nel file delle denucnce c'è il suo numero pratica
216900060619     c                   If        dctnev > 0
217000060619
217100060619     c     kdet          chain     fndet01l
217200060619     c                   If        %found(fndet01l)
217300060619
217400060619     c                   If        detprn = 0
217500060619     c                   Clear                   FIDN20DS
217600060619     c                   Movel     'N'           I20mod
217700060619     c                   Z-add     DETaae        I20pra
217800060712     c                   Movel     DETdit        I20dit
217900060619      *
218000060619     c                   movel(P)  FIDN20DS      KPJBU
218100060619      *
218200060619     c                   CALL      'FIDN20R'
218300060619     c                   PARM                    KPJBA
218400060619      *
218500060619     c                   movel     KPJBU         FIDN20DS
218600060619      *
218700060619     c                   IF        O20err = 'E'
218800060619     c                   eval      *in28 = *ON
218900060619     c                   ELSE
219000060619     c                   movel     I20dit        DETdit
219100060619     c                   z-add     I20pra        DETpra
219200060619     c                   z-add     O20prn        DETprn
219300060619     c                   update    fndet000
219400060619     c                   endif
219500060619     c                   else
219600060620     c                   unlock    fndet01l
219700060619     c                   endif
219800060619
219900060619     c                   eval      dctdit  =   DETdit
220000060619     c                   eval      dctpra  =   DETpra
220100060619     c                   eval      dctprn  =   DETprn
220200060619     c                   endif
220300060619
220400060619     c                   else
220500981120      *
220600981120     C                   clear                   FIDN20DS
220700981120     C                   movel     'N'           I20mod
220800981120     c                   z-add     DCTaas        I20pra
220900981124     c                   movel     ORGdit        I20dit
221000001227      *
221100120404      * se il P.O. del beneficiario è SDI
221200001227      * forzo la ditta Bartolini
221300120404     c                   if        ORGdit = 'SDI'
221400001227     c                   eval      I20dit = 'BAR'
221500001227     c                   endif
221600021118      *
221700021118      * se la spedizione ha valore da assicurare esposto in bolla o
221800021118      *   assicurazione per conto: verifico se il P.O. di arrivo ha
221900021118      *   FEDEX come network
222000021118     c                   if        wSpedAss = *on
222100021118     c                   exsr      Ctr_Ntw
222200021118     c                   endif
222300981120      *
222400981120     C                   movel(P)  FIDN20DS      KPJBU
222500981120      *
222600981120     c                   CALL      'FIDN20R'
222700981120     c                   PARM                    KPJBA
222800981120      *
222900981120     C                   movel     KPJBU         FIDN20DS
223000981120      *
223100981120     c                   IF        O20err = 'E'
223200981120     c                   eval      *in28 = *ON
223300981120     c                   ELSE
223400981120     c                   movel     I20dit        DCTdit
223500981120     c                   z-add     I20pra        DCTpra
223600981120     c                   z-add     O20prn        DCTprn
223700981120     c                   ENDIF
223800060619
223900060619     c                   endif
224000981120      *
224100981120     c                   ENDSR
224200021118      *****************************************************************
224300021118      * Verifico se il P.O. di arrivo ha "FED" come network
224400021118      * (se la spedizione è assicurata)
224500021118      *****************************************************************
224600021118     C     Ctr_Ntw       BEGSR
224700021118      *
224800021118     c                   Clear                   OG143
224900021118     c     DCTlna        Chain     AZORG
225000021118if  1c                   If            %found(AZORG01L)
225100021118     c                             and ORGfva = *blanks
225200021118     c                   Movel     ORGde3        OG143
225300021118if  2c                   If        §OGntw = 'FED'
225400021118     c                   Movel     §OGntw        I20dit
225500021118e   2c                   Endif
225600021118e   1c                   Endif
225700021118      *
225800021118      * ripristino i dati dell'ultimo record letto...
225900021118     c     Wpointes      chain     AZORG                              31
226000021118      *
226100021118     C                   ENDSR
226200981203      *****************************************************************
226300981230      * DECODIFICO NAZIONE
226400981203      *****************************************************************
226500981230     C     CTRNAZ        BEGSR
226600981203      *
226700981230     C                   clear                   DS15
226800981230     C                   movel     '15'          KCOD
226900981230     C                   movel(P)  WNAZ          KKEY
227000981203     C     KTAB          CHAIN     TABEL00f                           30
227100981203      *
227200981230     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
227300981230     C                   movel     TBLUNI        DS15
227400981230     C                   endif
227500981203      *
227600981203     C                   ENDSR
227700981230      *****************************************************************
227800981230      * CONTROLLO DIVISA
227900981230      *****************************************************************
228000981230     C     CTRVLT        BEGSR
228100981230      *
228200981230     C                   MOVEL     'CV'          KCOD
228300990415     C                   MOVEL(P)  W003a         KKEY
228400981230     C     KTAB          CHAIN     TABEL00f                           30
228500981230      *
228600981230     C                   IF        *IN30 = *ON  or
228700981230     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
228800011026     C                   clear                   DSCV
228900981230     C                   ELSE
229000981230      *
229100981230     C                   movel     TBLUNI        DSCV
229200981230     C                   ENDIF
229300981230      *
229400011026     C                   ENDSR
229500010906
229600010906      *****************************************************************
229700010906      * CONVERSIONE IMPORTI IN MONETA CORRENTE
229800010906      *****************************************************************
229900010906     C     srCMB         BEGSR
230000010906      *
230100010906     C                   reset                   YEURCODS
230200010906      *
230300010906     C                   movel     W003a         yecDVI
230400010906     C                   z-add     W0133         yecIMI
230500010910     C                   movel     §gedDBA       yecDVO
230600010906      *
230700010906     C                   call      'YEURCO'
230800010906     C                   parm                    YEURCODS
230900010906      *
231000010906     C                   IF        yecESI  = ' '
231100010906     C                   z-add     yecIMO        W0133
231200011002     C                   movel     yecDVO        W003a
231300010906     C                   ENDIF
231400010906      *
231500010906     C                   ENDSR
231600010906
231700010906      ************************************************************
231800010907      * Editazione di un numero
231900010906      ************************************************************
232000010906     C     Edit          Begsr
232100010906     C*
232200010907     C                   reset                   XEDITDS
232300010906     C* numero da elaborare
232400010906     C* (WImporto ha 5 decimali, $SrcVar ha 0 decimali)
232500010906     C                   Select
232600010907     C                   When      wNrDec    = 0
232700010907     C                   eval      IedNUMERO = wImporto
232800010907     C                   When      wNrDec    = 1
232900010907     C                   eval      IedNUMERO = wImporto * 10
233000010907     C                   When      wNrDec    = 2
233100010907     C                   eval      IedNUMERO = wImporto * 100
233200010907     C                   When      wNrDec    = 3
233300010907     C                   eval      IedNUMERO = wImporto * 1000
233400010907     C                   When      wNrDec    = 4
233500010907     C                   eval      IedNUMERO = wImporto * 10000
233600010907     C                   When      wNrDec    = 5
233700010907     C                   eval      IedNUMERO = wImporto * 100000
233800010906     C                   Endsl
233900010906     C* numero decimali
234000010907     C                   eval      IedNRDEC  = wNrDec
234100010906     C* codice di editazione
234200010907     C                   eval      IedEDTCOD = '3'
234300010906     C* max lunghezza campo in output
234400010907     C                   eval      IedLENFLD = wLungh
234500010906     C*
234600010907     C                   call      'XEDIT'
234700010907     C                   parm                    XEDITDS
234800010906      *
234900010906     C                   ENDSR
235000010906
235100981123      *****************************************************************
235200981123      *   RICERCA CAPOCONTI
235300981123      *****************************************************************
235400981123     C     CAPOCON       BEGSR
235500981123      *
235600981123      *--- RICERCA CAPOCONTI
235700020606     C                   DO        50            ww
235800020606     C                   MOVE      TCU(ww)       TCUDS
235900981123     C                   IF        F56 = 'CG'
236000981123      *
236100981123     C     f4            COMP      '1'                                    21
236200981123     C     f4            COMP      '2'                                    22
236300981123     C     f4            COMP      '3'                                    23
236400981123     C     f4            COMP      '6'                                    27
236500981123      ** 1 CLIENTI   21    ** 2 FORNITORI 22    ** 3 AGENTI    23
236600981123     C     f3            comp      '0'                                242425
236700981123     C     f3            comp      'I'                                    26
236800981123      ** 0 ITALIA   25      ** 1 ESTERO   24    ** I CAPO CONTO IVA
236900981123     c                   if        *in21 = *on
237000020606     C   24              Z-ADD     KCU(ww)       KCE               4 0
237100020606     C   25              Z-ADD     KCU(ww)       KCI               4 0
237200981123     c                   end
237300981123     c                   if        *in22 = *on
237400020606     C   24              Z-ADD     KCU(ww)       KFE               4 0
237500020606     C   25              Z-ADD     KCU(ww)       KFI               4 0
237600981123     c                   end
237700981123     c                   if        *in23 = *on
237800020606     C   24              Z-ADD     KCU(ww)       KAE               4 0
237900020606     C   25              Z-ADD     KCU(ww)       KAI               4 0
238000981123     c                   end
238100020606     C   26              Z-ADD     KCU(ww)       KIVA              4 0
238200020606     C   27              Z-ADD     KCU(ww)       KBNA              4 0
238300981123      *
238400981123     C                   ENDIF
238500981123     C                   ENDDO
238600981123      *
238700981123     C                   ENDSR
238800100709      /free
238900100709       //--------------------------------------------------------------
239000100709       //?"Bonifica" campi in stampa da caratteri indesiderati         ?
239100100709       //--------------------------------------------------------------
239200100709       BEGSR  sr_xChkChar;
239300100709
239400100709         ChkNull = '1';
239500100709
239600100709         xChkChar ( TxtInOut :
239700100709                    ElencoChar :
239800100709                    TipoElenco :
239900100709                    CharSost :
240000100709                    Uppercase :
240100100709                    ChkNull :
240200100709                    CharNull :
240300100709                    Esito);
240400100709
240500100709       ENDSR;
240600100709      /end-free
240700981116      *****************************************************************
240800981116      *   ROUTINE INIZIALE
240900981116      *****************************************************************
241000981116     C     *INZSR        BEGSR
241100981116      *
241200981116     C                   Z-ADD     1             CODUT
241300981116     C                   CALL      'X§PARUT'
241400981123     C                   PARM                    UT§DSE0F
241500981116      *
241600981116     C                   MOVEL     REC80         CNCR80
241700981124      *
241800981116      *  Ricerca capoconti
241900981116     C                   EXSR      CAPOCON
242000990222
242100010413      ***
242200010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
242300010413      ***
242400010413      *
242500010413      * recupero la moneta corrente
242600010413     C                   CLEAR                   tibs02ds
242700010413      *
242800010413     C                   MOVEL     'L'           T02TLA
242900010413     C                   MOVEL     'C'           T02MOD
243000010413     C                   MOVEL     KNSIF         T02SIF
243100010413     C                   MOVEL     'GED'         T02COD
243200010906     C                   MOVEL(P)  'DANNI'       T02KE1
243300010413      *
243400010413     C                   CALL      'TIBS02R'
243500010413     C                   PARM                    KPJBA
243600010413     C                   PARM                    tibs02ds
243700010413      *
243800010910     c                   movel     T02UNI        DGEDDN
243900011026      *
244000011026      * ...ed i relativi dati
244100011026     C                   movel     §gedDBA       W003a
244200011026     C                   exsr      ctrvlt
244300011026      *
244400010413      * recupero i valori nella moneta corrente
244500010413     C                   CLEAR                   tibs02ds
244600010413      *
244700010413     C                   MOVEL     'L'           T02TLA
244800010413     C                   MOVEL     'C'           T02MOD
244900010413     C                   MOVEL     KNSIF         T02SIF
245000010413     C                   MOVEL     'GEI'         T02COD
245100010910     C                   MOVEL     §GEDDBA       T02KE1
245200010413      *
245300010413     C                   CALL      'TIBS02R'
245400010413     C                   PARM                    KPJBA
245500010413     C                   PARM                    tibs02ds
245600010413      *
245700010413     c                   movel     t02uni        DGEI
245800990226      *
245900990226     c                   clear                   tibs02ds
246000990226     c                   clear                   dstd
246100990226      *
246200990226     C                   movel     'C'           t02mod
246300990226     C                   movel     knsif         t02sif
246400990226     C                   movel     'STD'         t02cod
246500990226     C                   movel(P)  1             t02ke1
246600010910     C                   movel(P)  §gedDBA       t02ke2
246700990226      *
246800990226     C                   CALL      'TIBS02R'
246900990226     C                   PARM                    KPJBA
247000990226     C                   PARM                    TIBS02DS
247100990226      *
247200990301     C                   movel     T02uni        DSTD
247300981116
247400981116     C                   TIME                    W0140
247500981116      *
247600981116     C                   MOVE      W0140         WDTGIO
247700981116     C                   MOVEL     W0140         WORA
247800981116      *
247900981116      * UDATE IN AAAAMMGG
248000981116     C                   Z-ADD     WDTGIO        G02DAT
248100981116     C                   MOVEL     *BLANK        G02ERR
248200981116     C                   CALL      'XSRDA8'
248300981116     C                   PARM                    WLBDAT
248400981116     C                   MOVEL     G02INV        DATEU
248500981116     C     *iso          MOVEL     DATEU         DATA_oggi
248600990316     C     *eur          movel     DATA_oggi     wdataoggi
248700981116
248800981116     C     Kdct          klist
248900981204     C                   KFLD                    V2Haac
249000981204     C                   KFLD                    V2Cfil
249100981204     C                   KFLD                    V2Cnca
249200001228      *
249300981123     C     Kdet          klist
249400981123     C                   KFLD                    DCTaae
249500981123     C                   KFLD                    DCTnev
249600990205      *
249700990205     C     Kdetd         klist
249800990205     C                   KFLD                    DCTaad
249900990205     C                   KFLD                    DCTpod
250000990205     C                   KFLD                    DCTnde
250100021204      *
250200021204     C     Ktas4         klist
250300021204     C                   kfld                    DCTaas
250400021204     C                   kfld                    DCTlnp
250500021204     C                   kfld                    DCTnrs
250600021204     C                   kfld                    DCTnsp
250700021204     C                   kfld                    TA4trc
250800021204     c                   eval      TA4trc = 'A'
250900981124      *
251000981124     C     KDCD          klist
251100981204     C                   KFLD                    V2Haac
251200981204     C                   KFLD                    V2Cfil
251300981204     C                   KFLD                    V2Cnca
251400981116      *
251500981116     C     KDCS          klist
251600981116     C                   KFLD                    Ktpd
251700981116     C                   KFLD                    Knkt
251800981116     C                   KFLD                    Kstd
251900981116      *
252000981116     C     KDCSE         klist
252100981116     C                   KFLD                    KTPD
252200981116     C                   KFLD                    KNKT
252300981116     C                   KFLD                    KSTD
252400981116     C                   KFLD                    KDIM
252500981116     C                   KFLD                    KHIM
252600981116     C                   KFLD                    KNKS
252700981116     C                   KFLD                    KTRC
252800020124
252900020124     c     kdcsp         KLIST
253000020124     c                   KFLD                    ktpd
253100020124     c                   KFLD                    knkt
253200020124     c                   KFLD                    kstd
253300020124     c                   KFLD                    kdim
253400020124     c                   KFLD                    khim
253500020124     c                   KFLD                    knks
253600020124     c                   KFLD                    ktrc
253700020124     c                   KFLD                    prgnot
253800981116      *
253900981116     c     kbol          klist
254000981204     c                   kfld                    V2Haas
254100981204     c                   kfld                    V2Clnp
254200981204     c                   kfld                    V2Cnrs
254300981204     c                   kfld                    V2Cnsp
254400981116      *
254500981116     C     KTAB          KLIST
254600981116     C                   KFLD                    CODUT
254700981116     C                   KFLD                    KCOD
254800981116     C                   KFLD                    KKEY
254900990222      *
255000990222     C     KTABE         KLIST
255100990222     C                   KFLD                    CODUT
255200990222     C                   KFLD                    KCOD
255300981116
255400981116     C                   ENDSR
255500980804      *****************************************************************
255600980804     OFNDCT000  E            AGGDCT
255700980804     O                       DCTgfc
255800980804     O                       DCTfca
255900981222     O                       DCTft1
256000981222     O                       DCTft2
256100981120     O                       DCTdit
256200981120     O                       DCTpra
256300981120     O                       DCTprn
256400981125     O                       DCTcch
256500981125     O                       DCTdch
256600000302     O                       DCTrcd
256700020124     o                       dctflo
256800980804      *****************************************************************
256900981214     OPRTF198   E            PINTES           15
257000981214     o                                        +   9 'Ufficio ASSICURAZIONI MERC-
257100981214     o                                              I'
257200981214     o          E            PINTES      1
257300981214     o                                        +   9 'BOLOGNA SEDE'
257400981214     o          E            PINTES      1
257500981214     o                                        +  74 'Spett.le'
257600981214     o          E            PINTES      1
257700981214     o                                        +  74 'Ufficio INTERNAZIONALE'
257800981214     o          E            PINTES      1
257900981214     o                                        +  74 'MILANO'
258000990303     o          E            PINTES      1
258100990303     o                                        +   9 'BOLOGNA,'
258200990303     o                       wdataoggi        +   2
258300981214     o          E            PINTES      8
258400990225     o                                        +   9 'Oggetto: Invio C.A. relati-
258500981214     o                                              ve a spedizioni Import in P-
258600990225     o                                              orto Franco chiuse con caus-
258700990225     o                                              ale 06'
258800981214     o          E            PINTES      0
258900990225     o                                        +   9 'Oggetto: Invio C.A. relati-
259000990225     o                                              ve a spedizioni Import in P-
259100990225     o                                              orto Franco chiuse con caus-
259200990225     o                                              ale 06'
259300981214     o          E            PINTES      3
259400981214     o                                        +  28 'Con la presente Vi inviamo-
259500981214     o                                               documentazione relativa al-
259600981214     o                                              le seguenti C.A.'
259700981214      *
259800981214     o          E            PNUMCA      2
259900981214     O                       NCA              +  13
260000981214      *
260100981214     o          E            PFIRMA      3
260200120516     O                                        +  74 'BRT S.p.A.'
260300981214     o          E            PFIRMA      1
260400120516     o                                        +  74 'Ufficio ASSICURAZIONI MERC-
260500981214     o                                              I'
260600981214      *-------------------------------------------------------------------------
260700981214N. aaa* fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnn
260800981214nnn   *
260900981214      *****************************************************************
261000981210** CMD   (Lungh. 90)                                                                     *
261100990519OVRPRTF FIDN35P SPLFNAME(LET198) FORMTYPE(AVVDANBAR)  OUTQ(xxxxxxxxxx) HOLD(*YES)
261200990224OVRPRTF FIDN35P SPLFNAME(LET198) FORMTYPE(AVVDANBAR) OUTQ(PICCIONEDB/BARPICSTM)
261300030507OVRPRTF PRTF198 FORMTYPE(xxxxxxxxxx) OUTQ(xxxxxxxxxx) SAVE(*YES)
261400041125OVRPRTF FIDN35PM FORMTYPE(AVVDANBAR) OUTQ(xxxxxxxxxx) USRDFNDTA('
261500990302** COTE: 1-3 cod.testo; COSC: 4-6  7-9 1o e ultimo elemento ITA, 10-12 13-15 1o e ultimo elemento GB
261600120412A10001013014026
261700981123** TES1     *            (Lungh. 100)                                                              *
261800120412facendo seguito al nostro preavviso di danno del xx/xx/xxxx siamo spiacenti di confermare l'anomalia
261900120405sopra descritta.
262000981123XTESP
262100981123XJUMP
262200120412Al fine di procedere alla liquidazione, Vi preghiamo di inviare a BRT S.p.a. - Ufficio Servizi
262300120404Assicurativi - Via Enrico Mattei 42  40138 Bologna, la seguente documentazione:
262400990222XDOCP
262500981123XJUMP
262600120412Nello scusarci per lo spiacevole inconveniente, Vi ricordiamo che la Vs. filiale BRT di zona
262700120404rimane a completa disposizione per ogni eventuale ulteriore chiarimento.
262800981123XJUMP
262900120404Distinti saluti.
263000120516                                                     XXXXXXXXXXXXXXXXXXXX
263100120412following our damage notice dated xx/xx/xxxx we regret to confirm that the a.m. parcel is damaged.
263200990302XTESP
263300990302XJUMP
263400120412In order to speed up the verification and investigation process, we kindly ask you to send to
263500120404BRT S.p.A. - Ufficio Servizi Assicurativi -  Via Enrico Mattei 42 40138 Bologna, the following
263600120404documents:
263700990302XDOCP
263800990302XJUMP
263900120412We apologise for the inconvenient occured, we inform that our BRT's local branch is at your
264000120404disposal for any further information.
264100071217XJUMP
264200120404Best Regards.
264300120516                                                     XXXXXXXXXXXXXXXXXXXX
264400990302** COTP: 1-3 cod.testo; COSP: 4-6  7-9 1o e ultimo elemento ITA, 10-12 13-15 1o e ultimo elemento GB
264500060310A  001002020021
264600060310B  003004022023
264700060310C  005007024025
264800060310D  008008026026
264900060310E  009011027028
265000060310F  012014029031
265100060310G  015017032034
265200060310H  018019035036
265300981123** TESP     *            (Lungh. 100)                                                              *
265400080213            Il danno sara' risarcito nel rispetto del limite previsto dalla L. 450/85
265500071207modificata dalla 162/93, ovvero xxx xxxxxxxxx per ogni Kg di merce perduta o danneggiata.
265600080213            Il danno sara' risarcito nel rispetto del limite previsto dalla L. 450/85
265700071207modificata dalla 162/93 elevato a xxx xxxxxxxxxx per ogni Kg di merce perduta o danneggiata.
265800120412Il danno sara' risarcito con il limite di xxx xxxxxxxxxx a xxxxxxxxxx, sulla base del mandato
265900120403conferitoci di assicurare per Vostro conto e contro tutti i rischi, le merci oggetto della
266000120403spedizione di cui sopra.
266100120412Il danno sara' risarcito con il limite previsto dalla C.M.R. : xxxxxxxxxxxxxxxx
266200120412Il risarcimento non Vi potra' essere riconosciuto poiche', per le modalita' di accadimento,
266300071207rientra fra quelli considerati "fortuiti" dal Codice Civile /art. 1693 e 1694), non ravvisandosi
266400071207alcuna nostra responsabilita'.
266500120412Il danno sara' risarcito con il limite di xxx xxxxxxxxxx, valore complessivo riportato nel DDT,
266600120403giusto mandato conferitoci di assicurare per Vostro conto, contro tutti i rischi, le merci oggetto
266700120406della spedizione stessa.
266800120412Il danno sara' risarcito nel rispetto del limite previsto dall'articolo 1696 c.c. cosi' come
266900120403modificato dal d.lgs 286/2005 ovvero non superiore a xxx xxxxxxxxx per ogni chilogrammo di peso
267000120403lordo per la merce perduta o avariata.
267100071207            Il danno sara' risarcito nel rispetto del limite di EUR 6,20 per ogni Kg di merce
267200071207perduta o danneggiata.
267300080213The compensation will be recognized you according to the present law L. 450/85 amended by
267400050216the 162/93, i.e. xxx xxxxxxxxx per each Kg merchandise lost or damaged.
267500080213The compensation will be recognized you according to the present law L. 450/85 amended by
267600050216the 162/93 up to xxx xxxxxxxxxx each Kg merchandise lost or damaged.
267700120412The damage will be refund within the limit of xxx xxxxxxxxxx each xxxxxxxxxx, according to the
267800120404insurance writ which we were asked to insure on your behalf, against all risks, of this shipment.
267900120412The damage will be refund within the limit of the C.M.R. : xxxxxxxxxxxxxxxx
268000120412The refund will not be paid since the events are classified under the "fortuitous events" by the
268100120412Civil Code (art. 1693 and 1694), and we have no liability for them.
268200120412The damage will be refund within the limit of xxx xxxxxxxxxx, that is the total amount of the
268300120404delivery note, as per the amount we were asked to insure on your behalf, against all risks, of
268400120404this shipment.
268500120412The damage will be refund within the limit fixed by the article 1696 c.c. as it was modified
268600120404by the legislative decree dated 286/2005 that is the refund will not exceed xxx xxxxxxxxx per
268700120404each kilo (gross weight) of the missing or damaged goods.
268800080213You will be refund within the limit of EUR 6,20 per each kilo (gross weight) of the missing or
268900060310damaged goods.
269000050217** COTD: 1-3 doc.; COSD: 4-6  7-9 1o e ultimo e 9-12 13-15 Highlight ITA, 16-18 19-21 22-24 25-27 GB
269100120404D01001003001001023025023023
269200120404D02004005004004026027026026
269300120406D03006009006006028030028028
269400120406D04010013010010031034031031
269500120406D05014015014014035036035035
269600120406D06016018016016037039037037
269700120406D07019022019019040043040040
269800990224** TESD     *            (Lungh. 100)                                                              *
269900120404   Contestazione:
270000120404   Copia della contestazione di danno inviata alla BRT S.p.A. entro i termini di legge (8 gg.
270100120406   dalla consegna o dal reso merce - art. 1698 c.c.) e relativa prova d'invio
270200120404   D.d.t.:
270300120406   Copia del documento di trasporto accompagnatorio (d.d.t.) redatto dal mittente
270400120404   Valore merce:
270500120406   Se causale vendita: fotocopia della fattura della merce spedita completa di tutte le pagine
270600120404   Se diversa causale: fotocopia del listino di vendita o della fattura d'acquisto di tutta la
270700120406   merce spedita
270800120404   Riparazione:
270900120404   Se la merce è stata riparata: fotocopia della fattura di riparazione oppure scheda interna di
271000120406   lavorazione con listino prezzi della parti sostituite
271100120406   Se la merce non è riparabile: dichiarazione a riguardo
271200120404   Reintegro:
271300120406   Bolla di reintegro o nota credito qualora emesse
271400120404   Risarcimento:
271500120516   Vs. lettera di richiesta risarcimento danno con importo quantificato intestata alla BRT S.p.A.
271600120406   riportante gli estremi del presente avviso danno
271700120404   Surroga:
271800120404   Copia Vostro atto di quietanza firmato e timbrato dall'avente diritto, che legittima la Vostra
271900120404   surrogazione
272000120404   RIPORTANTE GLI ESTREMI DEL PRESENTE AVVISO DI PRESUNTO DANNO
272100120404   Damage claim
272200120404   Copy of the damage claim sent to BRT S.p.A. within the prescribed terms (within 8 days from the
272300120404   delivery or the return of goods article 1698 c.c.) and the relevant evidence of its sending
272400120404   Delivery note
272500120404   Copy of the original delivery note
272600120404   Value of the goods
272700120404   In case of sale: whole photocopy of the original invoice of the sent goods
272800120406   In the other cases: photocopy of the price list or of the purchase invoice of all the goods sent.
272900120404   Repair
273000120404   In case the damaged goods are fixed: copy of the invoice only relevant to the fixing or the
273100120406   internal document of the manufacturing with the price list of the replaced parts
273200120404   In case the goods are not repairable: please send us declaration
273300120404   Replacement
273400120404   Delivery note for replacement or credit note if issued
273500120404   Damage refund
273600120404   Your request of damage refund with the requested amount addressed to BRT S.p.A. with the
273700120404   details of this alleged damage notice
273800120404   Subrogation
273900120404   Copy of your release document signed and stamped by the person in charge who legitimises
274000120404   your legal subrogation
274100120404   WITH THE DETAILS OF THIS ALLEGED DAMAGE NOTICE
274200041118** TESM testo da stampare per richiesta via e-mail
274300041118Per ricevere le prossime comunicazioni via e-mail,Vi invitiamo a darne segnalazione a
274400041118                              indicando i seguenti dati : Codice xxxxxxx e-mail ___________________
274500041118Should you need the next information via e-mail, We ask you to be so kind and ask it at
274600041118                              specifying the following details : Codice xxxxxxx e-mail ____________
274700120404** TESACB testo da stampare per info su assicurazione BRT
274800120404Cogliamo l'occasione per ricordarVi che la BRT S.p.A. offre alla propria clientela, un servizio di
274900120405copertura assicurativa per conto per le merci trasportate conveniente e personalizzato.
275000120418Herewith we remember you that BRT, offer to all the customers, the possibility to insure all the
275100120418goods in a convenient and custom way.
