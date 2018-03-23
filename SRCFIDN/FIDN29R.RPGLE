000100150604     /*END
000200980618      *-------------------------------------------------------------------------*
000300150806      *    EMISSIONE PROGETTO DI LIQUIDAZIONE                                   *
000400980618      *-------------------------------------------------------------------------*
000500050520
000600980831      ****************************************************************
000700150608      *  FASI C.A. FISSATE A PROGRAMMA
000800980831      ****************************************************************
000900150806      *    700 = Emissione progetto di liqidazione            (c_FLiq)
001000150608      ****************************************************************
001100980715
001200980625      ****************************************************************
001300980625      *  RIEPILOGO INDICATORI
001400130911      ****************************************************************
001500990108      * 01 - VISUALIZZO evento
001600990126      * 04 - VISUALIZZO rivalsa
001700131014      * 05 - VISUALIZZA "C.A. AFFIDATA AD A.I.G."
001800990415      * 06 - NON VISUALIZZO mandato assicurativo
001900020125      * 07 - NON VISUALIZZO calcolo del risarcimento
002000150909      * 08 - STAMPA (non e-mail): abilita F8, non F6
002100150923      * 08 - sr PRTPDL: evita UNDERLINE/HIGHLIGHT in e-mail
002200990126      *-- Errori video 1
002300990126      * 10 - ERRORE data stampa
002400050513      * 11 - libero
002500990126      * 12 - ERRORE importo da liquidare
002600990203      * 13 - ERRORE modalità calcolo risarcimento
002700050516      *-- Errori window
002800050516      * 20 - ERRORE Chi paga
002900050516      *--
003000050516      * 21 - Evento fortuito
003100990126      *--
003200050519      * 25 - Stampa Numero polizza
003300050519      * 26 - Stampa Riferimento mittente alfanumerico
003400050519      *--
003500050516      * 28   - ERRORE MSG
003600990126      * 30   - COMODO
003700060615      *--
003800060615      * 35 - Stampo frase relative alle sole spedizioni assicurate
003900980625      ***************************************************************
004000980618
004100951009     H DECEDIT('0,') DATEDIT(*DMY.)
004200980625      * ---------------------------------------------------------
004300990107     fFIDN29D   CF   E             WORKSTN
004400981005     fTABEL00F  IF   E           K DISK
004500990108     fFNDCT01L  UF A E           K DISK
004600981005     fFNDET01L  IF   E           K DISK
004700990202     fFNDCF01L  IF A E           K DISK
004800990202     fFNDCL01L  UF A E           K DISK
004900990202     fFNDKA01L  UF A E           K DISK
005000990202     fFNDCS01L  UF A E           K DISK
005100990415     fFNDPT01L  IF   E           K DISK
005200990108     fTNTMD01L  IF   E           K DISK
005300990809     fTITAS30C  IF   E           K DISK
005400050519     fTITA430C  IF   E           K DISK
005500990108     fAZORG01L  IF   E           K DISK
005600150529      *
005700120426     fFIDN29P   o    e             printer usropn
005800120426     fFIDN29P2  o    e             printer usropn
005900150529     fFIDN29PG  o    e             printer usropn
006000150529     fFIDN29PG2 o    e             printer usropn
006100980611      * ---------------------------------------------------------
006200050516     D MSG             S             78    DIM(12) CTDATA PERRCD(1)             MSG VIDEO
006300060316     D VLR             S             78    DIM(9) CTDATA PERRCD(1)              Limite Risar. Video
006400060316     D SLR             S             78    DIM(9) CTDATA PERRCD(1)              Limite Risar. Stampa
006500120502     D SLR2            S             78    DIM(9) CTDATA PERRCD(1)              Limite Risar. Stampa
006600990126     D W35             S             35    DIM(10)
006700980717
006800050208     D KPJBA         E DS
006900050208      *
007000981020     D FIDN00DS      E DS
007100981029     d  ds_numca               6     19
007200050208      *
007300050208      * - Parametri x Controllo profilo utenti
007400050208     d TIBS34ds      e ds
007500050208      * - Ds di riferimento al file esterno AZUTE00F
007600050208     d AZUTEds       e ds                  extname(AZUTE00F)
007700050208      * - Ds per dati organigramma
007800050208     d dDATIUTE      e ds
007900050208      *
008000081229     D FIDNBEDS      E DS
008100980717     D FIDN05DS      E DS
008200020125     D FIDN22ds      E DS                  inz
008300981020     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
008400990415     D FIDN30DS      E DS
008500050208     D TIBS02DS      E DS
008600981020     D DTAD          E DS
008700050519     d DTA4A         e ds                  inz
008800981020     D DSTB          E DS
008900981020     D DSCV          E DS
009000010910     D DGEDDN        E DS
009100010413     D DGEI          E DS
009200990125     D DS15          E DS
009300981228     D DDCT01        E DS
009400010905     D DSTD          E DS                  INZ
009500990204     D DSTD2V        E DS                  Extname (DSTD2)  Prefix(V)
009600050519     d ddpz          e ds
009700150529       // -?Tab. MRA/DAN = Bart-Maling - Danni?
009800150529     d dMRAdan       e ds
009900150529
010000150529       // -?Parametri x Ridefinizione dati utente estesi per mailing?
010100150529     d TRTCM1ds      e ds                  inz
010200150529       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010300150915     d   §CM1mitt    e                     inz('servizi.assicurativi')
010400150529       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010500150915     d   §CM1dst     e                     inz('servizi.assicurativi@brt.it')
010600150529       //  ?·§CM1tips = Tipo lettera via e-mail:?
010700150529       //   ?"LET" = testo allegato in corpo con logo?
010800150529       //           ?(richiede righe libere iniziali per il logo)?
010900150529       //   ?"COR" = testo integrato senza logo?
011000150529       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011100150608     d   §CM1tips    e                     inz('E08')
011200150529       //  ?·§CM1po   = Filiale?
011300150529     d   §CM1po      e                     inz('046')
011400150529       //  ?·§CM1var  = Oggetto e-mail?
011500150529     d   §CM1var     e                     inz('*OBJM*+
011600150529     d                                     PROGETTO DI LIQUIDAZIONE +
011700150529     d                                     E QUIETANZA C.A.')
011800150529       //  ?·§CM1sts  = Stato?
011900150529     d   §CM1sts     e                     inz(*off)
012000150529       //  ?·§CM1sts  = Id processo?
012100150529     d   §CM1idp     e                     inz('1')
012200150529
012300010905     D YEURCODS      E DS                  inz
012400010905     D   YECTAR      E                     inz('H')
012500011017     D XDECDS        E DS                  inz
012600011017     D   OCDESI      E                     inz(*OFF)
012700010907      * Routine editazione di un numero
012800010907     D XEDITDS       E DS                  inz
012900010907     D   OedFlgErr   e                     inz(*off)
013000980615
013100980610     d                 ds                  INZ
013200990108     d  DS_dataca              1      8  0
013300990108     d  DCTaac                 1      4  0
013400990108     d  DCTmgc                 5      8  0
013500980615
013600990108     d                 ds                  INZ
013700990108     d  DS_datasp              1      8  0
013800990108     d  TASaas                 1      4  0
013900990108     d  TASmgs                 5      8  0
014000990108
014100990108     d                 ds                  INZ
014200990108     d  DS_dataev              1      8  0
014300990108     d  DETaae                 1      4  0
014400990108     d  DETmge                 5      8  0
014500990108
014600971016     D WLBDAT          DS                  INZ
014700971016     D  G02DAT                 1      8  0
014800971016     D  G02INV                 9     16  0
014900971016     D  G02ERR                17     17
015000971016     D  G02TGI                18     22  0
015100150609
015200150609     d ds_Time         ds
015300150609     d   wDate                        8s 0 inz
015400150609     d   wTime                        6s 0 inz
015500980615
015600951009     D                SDS
015700150529     d   SDSpgm          *proc
015800971016     D  VTCPGM                 1     10
015900150529
016000150529       //--------------------------------------------------------------
016100150529       //?Definizione costanti.                                        ?
016200150529       //--------------------------------------------------------------
016300990107
016400990107      *  titolo videata (lunghezza massima 34)
016500150806     D TIT1            C                   CONST('EMISSIONE PROGETTO DI LIQUIDA-
016600150806     D                                     ZIONE ')
016700050203
016800050203     d C_Text_A        c                   const('quale risarcimento per -
016900050203     d                                     il danno più sopra indicato.')
017000120427     d C_Text_A2       c                   const('as compensation for the -
017100150603     d                                     a.m. claim.')
017200990122
017300110504     D KBAR            C                   CONST('BRT S.p.A.')
017400990203     D KSDI            C                   CONST('S.D.I. S.p.A.')
017500150608
017600150608     d c_FLiq          c                   const(700)
017700150529
017800150529       // -?Comandi di override ai PrtF (e-mail)?
017900150529     d c_CmdOvrPrtF    c                   const('OVRPRTF +
018000150529     d                                           file(FIDN29P) +
018100150529     d                                           ovrscope(*actgrpdfn) +
018200150529     d                                           hold(*no) +
018300150529     d                                           ')
018400150529     d c_CmdOvrPrtF2   c                   const('OVRPRTF +
018500150529     d                                           file(FIDN29P2) +
018600150529     d                                           ovrscope(*actgrpdfn) +
018700150529     d                                           hold(*no) +
018800150529     d                                           ')
018900150529     d c_CmdDltOvr     c                   const('DLTOVR +
019000150529     d                                            file(FIDN29P) +
019100150529     d                                            lvl(*actgrpdfn)')
019200150529     d c_CmdDltOvr2    c                   const('DLTOVR +
019300150529     d                                            file(FIDN29P2) +
019400150529     d                                            lvl(*actgrpdfn)')
019500150529
019600150529       //--------------------------------------------------------------
019700150529       //?Definizione variabili globali.                               ?
019800150529       //--------------------------------------------------------------
019900980615
020000990126     D Wdataeur        S               D   DATFMT(*eur)
020100990126     D Wdataoggi       S               D   DATFMT(*eur)
020200990309     D W§dctport       S                   LIKE(§DCTport)
020300031211     D w§STDlpf        s                   like(§STDlpf1) inz(*hival)
020400050201     D Wind            S                   LIKE(DKAvia)
020500990203     D Wprv            S                   LIKE(DKAprv)
020600050204     D Wnaz            S                   like(DKAnaz)
020700990203     D Wv1cdds         S                   LIKE(V1Cdds)
020800990125     D wksc            S                   LIKE(TASksc)
020900981020     D Wtrc            S                   LIKE(DKAtrc)
021000980625     D Kcod            S                   LIKE(TBLcod)
021100980611     D Kkey            S                   LIKE(TBLkey)
021200980720     d Wdataca         s                   LIKE(V1Cdca)
021300990202     D Ktpd            S                   LIKE(DCStpd)
021400990202     D Knkt            S                   LIKE(DCSnkt)
021500990202     D Kstd            S                   LIKE(DCSstd)
021600990202     D Kdim            S                   LIKE(DCSdim)
021700990202     D Khim            S                   LIKE(DCShim)
021800990202     D Knks            S                   LIKE(DCSnks)
021900990202     D Ktrc            S                   LIKE(DCStrc)
022000050714     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
022100981124
022200990204     D Wlri            S            100
022300990126     D wrivalsa        s              1
022400990126     D wfndcl          s              1
022500980625     D Wkey1           s             15
022600990205     D xcodice         s             31
022700990205     D xtipo           s              1
022800010910     D w002a           s              2
022900981104     D w003a           s              3
023000990125     D w004a           s              4
023100990205     D w015a           s             15
023200990204     D w030b           s             30
023300000105     D w035a           s             35
023400000105     D w035b           s             35
023500980720     D w0033           s              3  3
023600990126     D w0133           s             13  3
023700980720     D w0140           s             14  0
023800050714     D WORA            S              6  0
023900980720     D w0040           s              4  0
024000990126     D JJ              s              3  0
024100980626     D XX              s              2  0
024200990205     D xlen            s              5  0
024300990125     D wpointes        S              3  0
024400980611     D wdtgio          s              8  0
024500980611     D dateu           s              8  0
024600130920     D wDataGLD        s              8  0
024700010905     D W§GEcmr         S             13  3
024800010905     D W§GElrp         S             13  3
024900020222     D $ErrF6          s              1    inz(*off)
025000050516     D wforzAIG        s              1
025100050516     D wforzGLD        s              1
025200050516     D wtipopag        s              1
025300010905      *-------------
025400010906     D WImporto        S             30S 5 inz
025500010905     D WNrDec          S              3S 0 inz
025600120426
025700120426     d wInglese        s               n   inz(*off)
025800150729     d $MancaMail      s               n   inz
025900150529
026000150529       //--------------------------------------------------------------
026100150529       //?Definizione procedure usate e relativi parametri.            ?
026200150529       //--------------------------------------------------------------
026300150529
026400150529       // -?Ricerca/Controllo tabelle?
026500150529     d*// TIBS02ds      e ds                  inz ?(già definita)?
026600150529      /copy gaitrasrc/srcProtoPR,TIBS02R
026700150529
026800150529       // -?API QCAPCMD (Process Commands)?
026900150529     d Qcmd            s           2048    inz  varying
027000150529      /copy qSysInc/qRpgleSrc,QCAPCMD
027100150529      /copy gaitrasrc/srcProtoPR,QCAPCMD
027200150529
027300150529       // -?Parametri gestione errori API.?
027400150529      /copy qsysinc/qrpglesrc,QUSEC
027500120426
027600980625      * ---------------------------------------------------------
027700990107      * Riempio dati video
027800990107     C                   EXSR      RIEvid
027900020222
028000020222     c                   eval      $ErrF6 = *off
028100980626
028200980716      *            G E S T I O N E   V I D E O  1
028300971020     C     EMD01         TAG
028400981022      *
028500990127     C                   WRITE     FI29Z01
028600990127     C                   EXFMT     FI29D01
028700980626      *
028800971020     C                   CLEAR                   V1CMSG
028900990107     C                   eval      *in28 = *off
029000980625
029100980625      * F3 = FINE
029200980623     C                   IF        *INKC = *ON
029300980626     C                   movel     'S'           O00F03
029400990127     C                   movel(P)  FIDN00DS      KPJBU
029500980626     C                   goto      FINE
029600980623     C                   ENDIF
029700980625
029800980625      * F12 = RITORNO
029900980619     C                   IF        *INKL = *ON
030000980626     C                   movel     'S'           O00F12
030100990127     C                   movel(P)  FIDN00DS      KPJBU
030200980626     C                   goto      FINE
030300980619     C                   ENDIF
030400990107
030500990202      * F7 = VISUALIZZAZIONE C.A.
030600990108     C                   IF        *INKG = *ON
030700990127     C                   movel(P)  FIDN00DS      KPJBU
030800990108     c                   call      'FIDN01R'
030900990108     c                   parm                    kpjba
031000990107     C                   goto      EMD01
031100990108     c                   ENDIF
031200990415
031300990415      * F14 = MANDATO ASSICURATIVO
031400990415     C                   IF        *INKN = *ON
031500990415     C                   call      'FIDN30R'
031600990415     C                   parm                    KPJBA
031700990415     C                   parm                    FIDN30DS
031800990415     C                   goto      EMD01
031900990415     c                   ENDIF
032000980625
032100980619      * Controlli video 1
032200980615     C                   EXSR      CTRD01
032300981209      *
032400150909     C*//                if        *IN28 = *ON  or  *INKF = *OFF
032500150909     C                   if        *IN28 = *ON  or  ( *INKF = *OFF
032600150909     C                                          and   *inKH = *off )
032700980623     C                   goto      EMD01
032800980623     C                   endif
032900060213
033000060213if  2c                   if        §DCTaffi =  'A'
033100131014      * - altrimenti (se la pratica risulta già affidata ad A.I.G.)
033200060213      *   emetto la finestra relativa alla data di ricezione delle
033300131014      *   disposizioni A.I.G. (da registrare in FNDRA) ed alle note
033400060213      *   (max 10 righe di 35 char, da registrare in FNDCS)
033500060213     c                   movel(p)  FIDN00ds      KPJBU
033600060213     c                   call      'FIDN8CR'
033700060213     c                   parm                    KPJBA
033800060213      *   se premuto F12 riemetto la videata
033900060213     c                   movel     KPJBU         FIDN00ds
034000060213     c                   if        O00f12 = 'S'
034100060213     c                   clear                   O00f12
034200060213     c                   goto      EMD01
034300060213     c                   endif
034400060213e   2c                   endif
034500050513      * se dato KF vedo se devo emettere la finestra solo se non è franchigia e data
034600050513      * spedizione maggiore del 17/04/2005
034700050713     c                   If        not *in07 and  ds_datasp > 20050417
034800050929      * - emetto la finestra relativa alla gestione della pratica  SE
034900131014      *   la pratica NON risulta già affidata ad A.I.G. (§DCTAFFI<>'A')
035000050929if  2c                   if        §DCTaffi <> 'A'
035100050513     c                   exsr      GES_WIND
035200050516      * se dato cmd12 riemetto la videata
035300050516     c                   if        *in28
035400050516     c                   goto      EMD01
035500050517     c                   setoff                                       28
035600050516     c                   endif
035700051125x   2c                   endif
035800051125x   2c                   endif
035900051125
036000980717
036100990107      * A Z I O N I   P E R   C O N F E R M A
036200990122     C                   EXSR      ELABORA
036300981020
036400990122     C     FINE          TAG
036500981020
036600980623      * Chiudo pgm di servizo
036700981029     c                   clear                   tibs02ds
036800981029     c                   movel     'C'           t02tla
036900981029     c                   call      'TIBS02R'
037000981029     c                   parm                    kpjba
037100981029     c                   parm                    tibs02ds
037200981029      *
037300981029     c                   movel     FIDN00DS      kpjbu
037400981029      *
037500981005     C                   EVAL      *INLR = *ON
037600980615      ****************************************************************
037700980615      *  CARICAMENTO DATI DI TESTATA VIDEO 1
037800980615      ****************************************************************
037900990107     C     RIEVID        BEGSR
038000980625
038100990125      * Spedizione
038200990108     c     kbol          chain     tntmd000                           30
038300990108
038400990809     c     kbol          chain     TITAS30c                           30
038500990108      *  se tipo bolla non è valido cerco altro rcd
038600990108     C                   movel     'TB'          kCOD
038700990108     C                   movel(P)  TAStbl        kKEY
038800990108     C     ktab          CHAIN     TABEL                              30
038900990108     C                   movel     TBLUNI        DSTB
039000990108     c                   if        §TBrbl = 'R'
039100990809     c     kbol          reade     TITAS30c                               30
039200990108     c                   endif
039300990108
039400990125      * Testata C.A.
039500990108     c     knumca        CHAIN     fndct000                           30
039600990108     c                   movel     dctflo        DDCT01
039700131014      * segnalo pratica già affidata ad A.I.G. (§DCTAFFI = 'A')
039800051006     c                   eval      *in05 = (§DCTaffi = 'A')
039900050518      * segnalo pratica in franchigia
040000050518      * spedizione è minore   18/04/2005  controllo il flag in fndct
040100050518      * spedizione è maggiore 17/04/2005  controllo il flag in fndct ed escludo eventi fortuiti
040200050518     c                   If        ds_datasp < 20050418
040300050518     c                   eval      *in07 = (§DCTPFRA = 'F')
040400050518     c                   else
040500050518     c                   eval      *in07 = (§DCTPFRA = 'F'and not *in21)
040600050518     c                   endif
040700020125     c                   clear                   V1Dcr1
040800020125     c                   clear                   V1Dcr2
040900990108
041000990125      * Importi C.A.
041100990108     c     knumca        CHAIN     fndcl000                           30
041200990126     c                   if        *IN30 = *OFF
041300990126     C                   eval      Wfndcl = 'S'
041400990126     c                   endif
041500990415
041600990415      * Mandato Assicurativo
041700990415     c     knumca        CHAIN     fndpt000                           06
041800990415     c                   IF        *IN06 = *OFF
041900990415     c                   If        DPTATB = 'A'
042000990415     c                   eval      *in06 = *on
042100990415     c                   Else
042200990415     c                   z-add     dptksc        I30ksc
042300990415     c                   z-add     dptctr        I30ctr
042400990415     c                   z-add     dptprg        I30prg
042500990415     c                   movel     dptftc        I30ftc
042600990415     c                   movel     dptfvm        I30fvm
042700990415     c                   z-add     dptvlm        I30vlm
042800990415     c                   movel     dptvvm        I30vvm
042900990415     c                   Endif
043000990415     c                   ENDIF
043100120427      *
043200120427      *  INTESTATARIO
043300120427     C                   exsr      RIEcli
043400990203
043500990127     c                   movel     DCTdit        V1Cdit
043600990127     c                   z-add     DCTpra        V1Cpra
043700990108     c                   z-add     DCTprn        V1Cprn
043800990108     c                   z-add     DCTlnp        V1Clnp
043900990108     c                   z-add     DCTnrs        V1Cnrs
044000990108     c                   z-add     DCTnsp        V1Cnsp
044100990126     c     *iso          move      DS_datasp     Wdataeur
044200990126     c                   move      Wdataeur      V1Cdsp
044300990108     c                   z-add     DCTfil        V1Cfil
044400990108     c                   z-add     DCTnca        V1Cnca
044500990126     c     *iso          move      DS_dataca     Wdataeur
044600990126     c                   move      Wdataeur      V1Cdca
044700990108     C                   movel     DCTtad        V1Ctad
044800990108     C                   movel(P)  DCTtad        WKEY1
044900990108     C                   exsr      CHTAD
045000990125     c                   If        §TADdest <> *blanks
045100990125     C                   movel     §TADdest      V1Dtad
045200050201     C                   movel     §TADdest      P1tad
045300990125     c                   Else
045400990125     C                   movel     §TADdesc      V1Dtad
045500050201     C                   movel     §TADdesc      P1tad
045600990125     c                   Endif
045700120502     c                   IF        wInglese and §TADdesi <> *blanks
045800120502     c                   eval      P1tad = §TADdesi
045900120502     c                   ENDIF
046000990203
046100990203     c                   IF        DCTnev > *zeros
046200990203      *   reperisco decodifica tipo anomalia evento
046300990203     C     KDET          chain     FNDET000                           30
046400990203     C                   movel(P)  DETtad        WKEY1
046500990203     C                   movel     DETtad        V1Ctae
046600990203     C                   exsr      CHTAD
046700990203     C                   eval      *IN01 = *ON
046800050516      * verifico se evento fortuito accendo il 21
046900050516     c                   eval      *in21 = (§tadevfo = 'S')
047000050516
047100990203     c                   If        §TADdest <> *blanks
047200990203     C                   movel     §TADdest      V1Dtae
047300050201     C                   movel     §TADdest      P1tad
047400990203     c                   Else
047500990203     C                   movel     §TADdesc      V1Dtae
047600050201     C                   movel     §TADdesc      P1tad
047700990203     c                   Endif
047800120502     c                   IF        wInglese and §TADdesi <> *blanks
047900120502     c                   eval      P1tad = §TADdesi
048000120502     c                   ENDIF
048100990203     c                   z-add     DCTnev        V1Cnev
048200990203     c                   z-add     DCTaae        V1Caae
048300990203     c     *iso          move      DS_dataev     Wdataeur
048400990203     c                   move      Wdataeur      V1Cdev
048500990203     c                   ENDIF
048600990125      *
048700990122      *  NUMERO POLIZZA
048800990125      *     Per determinala utilizzo l'azienda e l'anno del Numero Pratica Assicurativa
048900050519     c                   clear                   TIBS02DS
049000050519     C                   MOVEL     'C'           t02mod
049100050519     C                   MOVEL     knsif         t02sif
049200050519     C                   MOVEL     'DPZ'         t02cod
049300050519     C                   MOVEL     DCTpra        W004a
049400050519     C                   EVAL      t02ke1 = DCTdit + W004a
049500990122      *
049600050519     C                   CALL      'TIBS02R'
049700050519     C                   PARM                    KPJBA
049800050519     C                   PARM                    TIBS02DS
049900990122      *
050000050519     C                   If        t02err = *BLANKS
050100050519     c                   movel     t02uni        ddpz
050200050519     c                   eval      v1cnpl = §dpznpla
050300050519     c                   Else
050400050519     c                   eval      *in28 = *ON
050500050519     c                   movel     msg(4)        V1Cmsg
050600050519     C                   Endif
050700990122      *
050800990122      *  CONTRAENTE
050900990125      *     Per determinalo utilizzo l'azienda del Numero Pratica Assicurativa
051000990125     c                   IF        DCTdit = 'SDI'
051100990127     C                   movel     KSDI          V1Ccnt
051200990122     c                   Else
051300990127     C                   movel     KBAR          V1Ccnt
051400990122     c                   Endif
051500990125      *
051600990108      *  RISARCIBILE CON IL LIMITE
051700990122      * Determino il tipo di risarcimento da applicare al cliente e determino il testo associato
051800990122     c                   EXSR      SUB_TIPRIS
051900990125      *
052000990122      *  IMPORTO MASSIMO RISARCIBILE
052100990122     c                   z-add     DCTipv        V1Cipv
052200010905     c                   movel     DCTvpv        V1Cvpv
052300990125      *
052400990108      *  IMPORTO RICHIESTO
052500990122     c                   z-add     DCLipr        V1Cipr
052600010905     c                   movel     DCLvpr        V1Cvpr
052700990126      *
052800990126      *  IMPORTO DA LIQUIDARE
052900990428      * Imposto il minore tra Imp. max risarcibile e Imp. richiesto
053000010905      * (dando per scontato che siano espressi nella stessa valuta)
053100990428     c                   IF        V1Cipv < V1Cipr
053200990126     c                   z-add     V1Cipv        V1Cipl
053300010905     c                   movel     V1Cvpv        V1Cvpl
053400990126     c                   ELSE
053500990126     c                   z-add     V1Cipr        V1Cipl
053600010905     c                   movel     V1Cvpr        V1Cvpl
053700990126     c                   ENDIF
053800010911      * se comunque non compare nessun importo (quindi il codice valuta
053900010911      *  è vuoto) predispongo il codice valuta con la moneta corrente
054000081229      *  BARTOLINI.
054100010911     c                   IF        V1Cvpl = *blanks
054200010911     c                   movel     §gedDBA       V1Cvpl
054300010911     c                   ENDIF
054400150909      *
054500150909      * -?Disabilitazione "F6 = E-Mail" per D01?
054600151102     c                   eval      *in08  = ( oBEinv  <> 'M'  or  *in07 )
054700150909      *
054800150731      * -?Rilevata mancanza della e-mail del cliente (non 8888)?
054900151102      *  ?per Pratica  NON  in Franchigia?
055000151102     c                   IF        $MancaMail  and  Not *in07
055100150731      *
055200150731     c                   eval      ORGfil = oBEksc / 10000
055300150731     c     ORGfil        chain     AZORG
055400150731     c                   if        NOT %found( AZORG01L )
055500150731     c                   clear                   ORGdes
055600150731     c                   endif
055700150731     c                   eval      W02fil = %editc( ORGfil : 'X' ) +
055800150731     c                                      '-' + ORGdes
055900150806      * ·?Emissione window W02?
056000150910      *  ?(F12=Ritorno)?
056100150731     c                   write     FI29Z01
056200150731     c                   write     FI29D01
056300150910     c                   DoU       *inKL
056400150805     c                   exfmt     FI29W02
056500150805     c                   EndDo
056600150731      *
056700150731     c                   ENDIF
056800990122      *
056900990122     c                   ENDSR
057000990122      *****************************************************************
057100990126      *   IMPOSTO DATI INTESTATARIO
057200990122      *****************************************************************
057300990122     C     RIEcli        BEGSR
057400081229      *
057500081229      * cerco il beneficiario
057600081229     c                   clear                   fidnbeds
057700081229      *
057800081229     c                   movel     'B'           IBEmod
057900081229     c                   z-add     dctaac        IBEaac
058000081229     c                   z-add     dctfil        IBEfil
058100081229     c                   z-add     dctnca        IBEnca
058200081229     c                   z-add     dctaas        IBEaas
058300081229     c                   z-add     dctlnp        IBElnp
058400081229     c                   z-add     dctnrs        IBEnrs
058500081229     c                   z-add     dctnsp        IBEnsp
058600081229     c                   z-add     dctksc        IBEksc
058700081229     c                   movel     dctptr        IBEptr
058800081229     c                   movel     dctflo        IBEflo
058900120723      * SE  nella C.A. c'è l'importo trattenuto  e  non è franchigia
059000120723      * => il P.L. viene intestato a BRT
059100120723     c                   if        Wfndcl = 'S'  and  DCLipt > *zero
059200120723     c                                           and  not *in07
059300081229     c                   movel     'V'           IBEpgd
059400081229     c                   endif
059500081229      *
059600081229      *     controllo se esiste il luogo 8 o in subordine il luogo 6
059700081229     c                   movel     '008'         IBEpre
059800081229     c                   movel     'S'           IBEtpb
059900081229     c                   If        %subst(knsif:7:1) = 'P'
060000081229     c                   eval      IBEsif = 'P'
060100081229     c                   EndIf
060200081229      *
060300081229     c                   call      'FIDNBER'
060400081229     c                   parm                    fidnbeds
060500130924      *
060600130924     c                   If        oberag <> *blanks
060700130924      *
060800130924      * Imposto se devo stampare in Inglese
060900130924     c                   eval      wInglese = *off
061000130924     c                   IF        obeing <> *blanks
061100130924     c                   eval      wInglese = *on
061200130924     c                   ENDIF
061300130924      *
061400130924     c                   movel     oberag        V1Crsc
061500130924     c                   movel     obecap        V1Ccac
061600130924     c                   movel     obevia        P1ind
061700130924     c                   movel     obevia        Wind
061800130924     c                   movel     obeloc        V1Cloc
061900130924     c                   if        obeprv  <> *blanks
062000130924     c                   movel     obeprv        V1Cpnz
062100130924     c                   movel     obeprv        Wprv
062200130924     c                   else
062300130924     c                   movel     obenaz        V1Cpnz
062400130924     c                   movel     obenaz        wnaz
062500130924     c                   exsr      ctrnaz
062600130924     c                   movel     §15des        P1naz
062700130924     c                   endif
062800130924     c                   movel     obeksc        V1cksc
062900130924     c                   endif
063000150729      *
063100150729      * -?Se NON ricevuto alcun indirizzo e-mail (per cliente NON 8888):?
063200150729      *  ?si emetterà una window & NON si stamperà il P.L.?
063300151102      *  ?(SE Pratica NON in Franchigia)?
063400151102     c                   eval      $MancaMail = *off
063500151102     c                   if        Not *in07
063600150729     c                   eval      $MancaMail = ( oBEinv <> 'M'     and
063700150729     c                                            oBEksc <> *zeros  and
063800150729     c                             %subst( %editc( oBEksc : 'X' ) : 4 : 4 )
063900150729     c                                                   <> '8888' )
064000151102     c                   endif
064100150729      *
064200140725      * Determino il P.O. del cliente beneficiario,  se non è codificato utilizzo lnp (M) o lna (D
064300140725     C                   clear                   Wpointes
064400140725     c                   If        v1cksc = 0  and
064500140725     c                               DCTptr = 'D'
064600140725     C                   z-add     DCTlna        Wpointes
064700140725     c                   endif
064800140725     c                   If        v1cksc = 0  and
064900140725     c                               DCTptr = 'M'
065000140725     C                   movel     §DCTlnpc      Wpointes
065100140725     c                   endif
065200140725
065300990122      *
065400990122     C                   ENDSR
065500990122      *****************************************************************
065600990122      *   DETERMINO IL TIPO RISARCIMENTO
065700990122      *****************************************************************
065800990122     C     SUB_tipris    BEGSR
065900990122
066000990126     c                   clear                   xx
066100990126
066200021104      * Se PA EXPORT o PF IMPORT, non DPD, ho tipo risarcimento Estero CMR
066300021111    1c                   IF        (§DCTtisp = 'E'  and  §DCTport = 'A')  or
066400021104     c                             (§DCTtisp = 'I'  and  §DCTport = 'F'
066500021104     c                                              and  §DCTeuro <> 'D')
066600990126     c                   eval      XX = 4
066700010905     c                   z-add     W§GEcmr       WImporto
066800011026     c                   z-add     3             wNrDec
066900021111   x1c                   ELSE
067000990122
067100990125      * Esposto il valore in bolla
067200021111    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
067300990126     c                   eval      XX = 3
067400011114     c                   if        TASias <> *zeros and TASvas = *blanks
067500011114     c                   movel     'ITL'         TASvas
067600011114     c                   endif
067700990126      *
067800990126     c                   z-add     TASias        W0133
067900990126     c                   movel     TAsvas        W003a
068000010910     c                   if        TAsvas <> §gedDBA
068100010905     C                   exsr      srCMB
068200990122     c                   endif
068300010905     c                   z-add     W0133         WImporto
068400011026     c                   z-add     §cvdec        wNrDec
068500021111   x2c                   ELSE
068600990122      *
068700990415      * Se il cliente è assicurato imposto dati assicurazione
068800021111    3c                   IF        *IN06 = *OFF
068900990415      *  Importo Assicurato
069000990415     c                   z-add     DPTvlm        W0133
069100990415     c                   movel     DPTvvm        W003a
069200021111    4c                   IF        DPTvlm > *zeros
069300010910     c                   IF        DPTvvm <> §gedDBA
069400010905     C                   exsr      srCMB
069500010906     c                   ENDIF
069600021111   -4c                   ENDIF
069700010905     c                   z-add     W0133         WImporto
069800011026     c                   z-add     §cvdec        wNrDec
069900990415      *  Imposto costante in base al tipo valore
070000990415     c                   SELECT
070100990415     c                   WHEN      DPTftc = 'R'  or  DPTftc = '9'                 Limite Risarcibile
070200990415     c                   eval      XX = 2
070300990415     c                   WHEN        DPTfvm = '3'                                 Kg merce
070400990415     c                   eval      XX = 5
070500990415     c                   WHEN        DPTfvm = '1'                                 colli
070600990415     c                   eval      XX = 6
070700990415     c                   WHEN        DPTfvm = '2'                                 spedizione
070800990415     c                   eval      XX = 7
070900990415     c                   ENDSL
071000990415      *
071100021111   -3c                   ENDIF
071200000202      *
071300990122
071400000202      * Se non è assicurato oppure c'è RIVALSA si applica
071500020422      *   la legge 450 o la CMR se PA Import o PF Export
071600990122     c                   SELECT
071700990126     c                   WHEN        XX <> *zeros
071800021111      * se spedizione EEX o EUP, oppure export DPD, o export FedEx
071900021111     c                   WHEN           §DCTeuro =  'X'
072000021111     c                             or   §DCTeuro =  'P'
072100020422     c                             or   §DCTeuro =  'D'
072200020422     c                             and  §DCTtisp =  'E'
072300021111     c                             or   §DCTeuro =  'F'
072400021111     c                             and  §DCTtisp =  'E'
072500990126     c                   eval      XX = 4
072600010905     c                   z-add     W§GEcmr       WImporto
072700011026     c                   z-add     3             wNrDec
072800990122     c                   OTHER
072900060316      * Verifico il tipo di limite risarcibile in base al flag in dctflo
073000060316     c                   If        §dcttlrp = ' '
073100060316     c                   eval      XX = 1                                       * legge 450
073200060316     c                   z-add     6,2           WImporto
073300060316     c                   endif
073400060316     c                   If        §dcttlrp = '6'
073500060316     c                   eval      XX = 9                                       * rcv transitoria
073600060316     c                   z-add     6,2           WImporto
073700060316     c                   endif
073800060316     c                   If        §dcttlrp = '1'
073900060316     c                   eval      XX = 8                                       * legge 286
074000060316     c                   z-add     1             WImporto
074100060316     c                   endif
074200060316
074300011026     c                   z-add     §cvdec        wNrDec
074400990122     c                   ENDSL
074500990122      *
074600021111   -2c                   ENDIF
074700021111   -1c                   ENDIF
074800000204      * Se esiste la Rivalsa il tipo risarcimanto è secondo la legge 450
074900000204     c                   IF        Wrivalsa = 'S'
075000010905     c                   clear                   WImporto
075100011026     c                   clear                   wNrDec
075200021111      * se spedizione EEX o EUP, oppure export DPD, o export FedEx
075300021111     c                   if             §DCTeuro =  'X'
075400021111     c                             or   §DCTeuro =  'P'
075500020422     c                             or   §DCTeuro =  'D'
075600020422     c                             and  §DCTtisp =  'E'
075700021111     c                             or   §DCTeuro =  'F'
075800021111     c                             and  §DCTtisp =  'E'
075900000204     c                   eval      XX = 4
076000010905     c                   z-add     W§GEcmr       WImporto
076100011026     c                   z-add     3             wNrDec
076200000204     c                   else
076300000204     c                   eval      XX = 1
076400060411      * Verifico il tipo di limite risarcibile in base al flag in dctflo
076500060411     c                   If        §dcttlrp = ' '
076600060411     c                   eval      XX = 1                                       * legge 450
076700060411     c                   z-add     6,2           WImporto
076800060411     c                   endif
076900060411     c                   If        §dcttlrp = '6'
077000060411     c                   eval      XX = 9                                       * rcv transitoria
077100060411     c                   z-add     6,2           WImporto
077200060411     c                   endif
077300060411     c                   If        §dcttlrp = '1'
077400060411     c                   eval      XX = 8                                       * legge 286
077500060411     c                   z-add     1             WImporto
077600060411     c                   endif
077700011026     c                   z-add     §cvdec        wNrDec
077800000204     c                   ENDIF
077900000204     c                   ENDIF
078000990122      *
078100990126      * Imposto costante per video
078200010905     c                   exsr      Edit
078300010910     c                   eval      Wlri = §gedDBA  +   ' '
078400010907     c                                  +  %TRIM(OedNUMAN)
078500010905     c                                  +  VLR(xx)
078600990203     c                   eval      V1Dlr1 =  %subst(Wlri:1:49)
078700990205     c                   eval      V1Dlr2 =  %subst(Wlri:50)
078800990126      *
078900060615      * Verifico se spedizione assicurata per pilotare la stampa di una frase
079000990126      *
079100060615     c                   eval      *in35 = (xx = 2 or xx = 3 or xx>= 5)
079200060615
079300990122     C                   ENDSR
079400010905      ************************************************************
079500010905      * Editazione di un numero
079600010905      ************************************************************
079700010905     C     Edit          Begsr
079800010905     C*
079900010907     C                   reset                   XEDITDS
080000010906     C* numero da elaborare
080100010907     C* (WImporto ha 5 decimali, IedNUMERO ha 0 decimali)
080200010906     C                   Select
080300010907     C                   When      wNrDec    = 0
080400010907     C                   eval      IedNUMERO = wImporto
080500010907     C                   When      wNrDec    = 1
080600010907     C                   eval      IedNUMERO = wImporto * 10
080700010907     C                   When      wNrDec    = 2
080800010907     C                   eval      IedNUMERO = wImporto * 100
080900010907     C                   When      wNrDec    = 3
081000010907     C                   eval      IedNUMERO = wImporto * 1000
081100010907     C                   When      wNrDec    = 4
081200010907     C                   eval      IedNUMERO = wImporto * 10000
081300010907     C                   When      wNrDec    = 5
081400010907     C                   eval      IedNUMERO = wImporto * 100000
081500010906     C                   Endsl
081600010905     C* numero decimali
081700010907     C                   eval      IedNRDEC  = wNrDec
081800010906     C* codice di editazione
081900050203     C                   eval      IedEDTCOD = '3'
082000010906     C* max lunghezza campo in output
082100050203     C                   eval      IedLENFLD = 16
082200010905     C*
082300010907     C                   call      'XEDIT'
082400010907     C                   parm                    XEDITDS
082500010905      *
082600010905     C                   ENDSR
082700990126      ****************************************************************
082800990126      * CONTROLLI VIDEO 1
082900990126      ****************************************************************
083000990126     C     CTRD01        BEGSR
083100990126
083200050513     c                   setoff                                       1012
083300990126
083400990126      *  NUMERO POLIZZA se non impostata manca la tabella
083500050524     C                   IF        V1Cnpl = *blanks
083600050524     c                   eval      *in28 = *ON
083700050524     c                   movel     msg(4)        V1Cmsg
083800050524     C                   ENDIF
083900050524     C   28              GOTO      ECTD01
084000990126      *
084100990126      * Data stampa
084200990126     C                   IF        V1Cdds  = *zeros
084300990126     C                   seton                                        1028
084400990126     C                   movel     MSG(1)        V1CMSG
084500990126     C                   ELSE
084600990126     C                   clear                   WLBDAT
084700990126     C                   z-add     V1Cdds        G02DAT
084800990126     C                   CALL      'XSRDA8'
084900990126     C                   PARM                    WLBDAT
085000990126     C                   IF        G02ERR = '1'
085100990126     C                   seton                                        1028
085200990126     C                   movel     MSG(1)        V1CMSG
085300990126     C                   ELSE
085400990126     C                   z-add     G02DAT        V1Cdds
085500990126     C                   z-add     G02INV        WV1Cdds
085600990126     C                   ENDIF
085700990126     C                   ENDIF
085800990126     C   28              GOTO      ECTD01
085900150924      *?La data di stampa NON può essere precedente alla data di?
086000150924      *?fusione GLD con SETRAS (nella e-mail è prevista solo la?
086100150924      *?firma SETRAS)?
086200150924     c                   if        wV1Cdds < wDataGLD
086300150924     c                   seton                                        10  28
086400150924     c                   eval      V1Cmsg = %trimR(Msg(2)) + ' '
086500150924     c                             + %subst(%editc(wDataGLD:'X'):7:2) + '/'
086600150924     c                             + %subst(%editc(wDataGLD:'X'):5:2) + '/'
086700150924     c                             + %subst(%editc(wDataGLD:'X'):1:4)
086800150924     c
086900150924     c                   leavesr
087000150924     c                   endif
087100990126      *
087200990203      * Importo da Liquidare
087300990203     C                   IF        V1Cipl = *zeros
087400990203     C                   seton                                        1228
087500990203     C                   movel     MSG(5)        V1CMSG
087600010911     C                   else
087700010911     c                   z-add     V1Cipl        W0133
087800010911     c                   movel     V1Cvpl        W003a
087900010911     C                   exsr      CTRVLT
088000010911     C   28              seton                                        12
088100990126     C                   ENDIF
088200990126     C   28              GOTO      ECTD01
088300020222      *
088400020222      * Importo da liquidare > 103,30 Euro
088500031211      * - Prima imposto l'importo "limite" da testare in base alla data
088600031211      *   spedizione
088700031211     c                   reset                   w§STDlpf
088800031211if  1c                   if            *in07     =  *on
088900031211     c                             and $ErrF6    =  *off
089000031211     c                             and DS_DataSp >= §STDdlpf1
089100031211sel 2c                   select
089200031211w   2c                   when      DS_DataSp >= §STDdlpf2
089300031211     c                   eval      w§STDlpf   = §STDlpf2
089400031211w   2c                   when      DS_DataSp >= §STDdlpf1
089500031211     c                   eval      w§STDlpf   = §STDlpf1
089600031211e   2c                   endsl
089700031217     C                   IF        V1Cipl    >  w§STDlpf
089800020222     C                   eval      $ErrF6 = *on
089900020222     C                   seton                                        1228
090000020222     C                   movel     msg(8)        V1Cmsg
090100020222     C                   goto      ECTD01
090200020222     C                   ENDIF
090300031217e   1c                   endif
090400990203      *
090500990203      * Modalità di calcolo
090600990203     C                   IF        V1Dcr1 = *blanks  and  V1Dcr2 = *blanks
090700020125     C                             and  not *in07
090800990203     C                   seton                                        1328
090900990203     C                   movel     MSG(3)        V1CMSG
091000990203     C                   ENDIF
091100990203     C   28              GOTO      ECTD01
091200990126      *
091300990126     C     ECTD01        ENDSR
091400980623      *****************************************************************
091500150806      *   EMISSIONE PROGETTO DI LIQUIDAZIONE E AGGIORNA FILES
091600980623      *****************************************************************
091700990122     C     ELABORA       BEGSR
091800990122      *
091900150806      * Emissione Progetto di Liquidazione
092000020125      * (non se flag di Franchigia = 'F')
092100131014      * (e se chi paga NON è A.I.G.)
092200050520     C                   if        not *in07 and w01pag <> 'A'
092300990203     C                   EXSR      PRTPDL
092400020125     C                   endif
092500990203      * Descrizioni
092600150609      * (le note di calcolo si scrivono solo se c'è progetto di liquidazione)
092700150609      * (le note di Invio P.L. anche, oltre all'indirizzo e-mail)
092800050714     C                   if        not *in07 and w01pag <> 'A'
092900990203     C                   EXSR      WRTDCS
093000150609      *?Inserisco anche l'indirizzo e-mail al quale abbiamo inviato il P.L.?
093100150609      *?SE inviata e-mail, cioè:?
093200150609      *?· C.A. in fase 700  &?
093300150910      *?· prevista e-mail al cliente (quindi premuto F6, non F8)  &?
093400150910      *?· C.A. non a carico della GLD?
093500150910if  2c                   if        i00fca  =  c_FLiq    and
093600150910     c                             oBEinv  =  'M'       and
093700150910     c                             wV1Cdds >= wDataGLD
093800150609     c                   exsr      WrtDCS_2
093900150609e   2c                   endif
094000050714     c                   endif
094100990122      * Fasi
094200020201      * (non se flag di Franchigia = 'F': andrà in chiusura)
094300131014      * (non se Affidata ad A.I.G. non si fa progetto di liquidazione)
094400050714     C                   if        not *in07 and w01pag <> 'A'
094500981020     C                   EXSR      WRTDCF
094600020125     C                   endif
094700990126      * Liquidazione
094800981022     C                   EXSR      WRTDCL
094900990126      * Anagrafica
095000981020     C                   EXSR      WRTDKA
095100981020      * Testata C.A.
095200020201      * (non se flag di Franchigia = 'F': andrà in chiusura)
095300020128      * Se fase di chiusura occorre disallocare il record del file
095400020128      *   FNDCT, che sarà poi aggiornato dal pgm. FIDN22R !
095500020125     C                   if        not *in07
095600130920      * per Setras
095700050714     c                   if        w01pag <> 'A'
095800981020     C                   EXSR      WRTDCT
095900050714     c                   else
096000050714      * valorizzo i flag del dctflo
096100050714     c                   eval      dctflo = ddct01
096200050714     c                   update    fndct000
096300050714     c                   endif
096400050714
096500020128     C                   else
096600020128     C                   unlock    FNDCT01L
096700020125     C                   endif
096800020125      *
096900020125      * Registrazione fase di chiusura
097000020125     C                   if        *in07
097100020125     C                   exsr      CallFIDN22
097200020125     C                   endif
097300980623      *
097400980623     C                   ENDSR
097500990203      ****************************************************************
097600990203      * PREPARO CAMPI E STAMPO PROGETTO DI LIQUIDAZIONE
097700990203      ****************************************************************
097800990203     C     PRTPDL        BEGSR
097900150529
098000150529      /free
098100150605
098200150605       *in08 = *off;
098300150529
098400150529       // -?Invio Progetto di Liquidazione via e-mail?
098500150910       IF  oBEinv = 'M'  and  wV1Cdds >= wDataGLD;
098600150529
098700150529         // -?Reperimento tab. MRA = Bart-Maling - Danni?
098800150529         clear  dMRAdan;
098900150529         clear  TIBS02ds;
099000150529         T02mod = 'C';
099100150529         //T02sif = knsif;
099200150529         T02cod = 'MRA';
099300150529         T02ke1 = %trimr(SDSpgm);
099400150529
099500150529         TNTBE_RicercaControllo ( kpjba : tibs02ds );
099600150529
099700150529         if T02err = *blanks;
099800150529           dMRAdan = T02uni;
099900150529         endif;
100000150529
100100150529         // -?In stampa (NON RI-stampa) lo spool viene inviato via e-mail?
100200150529         //  ?(quindi niente "HIGHLIGHT" né "UNDERLINE")?
100300150529         *in08 = *on;
100400150529
100500150529         // -?Esecuzione override?
100600150529         reset  TRTCM1ds;
100700150529         If  §MRADreg <> *blank;
100800150529
100900150529           §CM1mitt = %trim(§MRAdMitt);
101000150529           §CM1dst  = oBEima;
101100150529           §CM1tips = §MRAdReg;
101200150529           §CM1po   = %editc( DUTpou : 'X' );
101300150529           §CM1var  = %trim( §CM1var ) + ' '
101400150529                    + %char( DCTaac )
101500150529                    + %editw( DCTfil : '0   ' )
101600150529                    + '/'
101700150529                    + %trim( %editc( DCTnca : '3' ) );
101800150529           §CM1idp  = §MRAdIdPro;
101900150529
102000150529           if  wInglese;
102100150529             Qcmd = c_CmdOvrPrtF2;
102200150529           else;
102300150529             Qcmd = c_CmdOvrPrtF;
102400150529           endif;
102500150529           Qcmd = Qcmd
102600150529                + ' outq(' + %trim(§MRAdOutqI) + ')'
102700150529                + ' usrdfndta(''' + TRTCM1ds + ''')';
102800150529           exsr  sr_ExecCmd;
102900150529
103000150529         EndIf;
103100150529
103200150529       ENDIF;
103300150529
103400150529      /end-free
103500130924      *
103600130924      * Apro il file di stampa
103700130924     c                   If        wV1Cdds >= wDataGLD
103800130924     c                   IF        wInglese
103900130924     c                   open      FIDN29P2
104000130924     c                   ELSE
104100130924     c                   open      FIDN29P
104200130924     c                   ENDIF
104300130924     c                   Else
104400130924     c                   IF        wInglese
104500130924     c                   open      FIDN29PG2
104600130924     c                   ELSE
104700130924     c                   open      FIDN29PG
104800130924     c                   ENDIF
104900130924     c                   EndIf
105000130924      *
105100050519      * se ca su spedizione maggiore del 17042005 stampo anche il numero polizza
105200050519     c                   If        ds_datasp > 20050417
105300050519     c                   seton                                        25
105400050519     c                   eval      P1npz = v1cnpl
105500050519     c                   endif
105600990204      *
105700990204      * Località di stampa
105800050201      * => Bologna (fissa in output)
105900050201      * Data di stampa
106000050201     c                   eval      P1dds  = V1Cdds
106100050201      * Numero danno
106200050201     c                   eval      P1prd  = %trim(%editc(DCTpra:'Z'))
106300050201     c                                    + '/'
106400050201     c                                    + %trim(%editc(DCTprn:'Z'))
106500050201      * Data Spedizione
106600050201     c                   z-add     V1Cdsp        P1dsp
106700990325      * Serie spedizione
106800050201if  1c                   if        DCTnrs > *zeros
106900050201     c                   eval      P1sesp = %trim(%editc(DCTnrs:'Z'))
107000050201     c                                    + ' '
107100050201     c                                    + %trim(%editc(DCTnsp:'Z'))
107200050201x   1c                   else
107300050201     c                   eval      P1sesp = %trim(%editc(DCTnsp:'Z'))
107400050201e   1c                   endif
107500050519      * Riferimenti
107600050519     c                   z-add     TASrmn        P1rmn
107700050519      * il riferimento mittente alfanum. per la sede si trova in TITA4
107800050519     c     ktas4         chain     TITA430C
107900050519     c                   if        %found(TITA430C)
108000050519     c                   movel     TA4not        DTA4A
108100050519     c                   else
108200050519     c                   clear                   DTA4A
108300050519     c                   endif
108400050519     c                   movel     §TA4Arma      P1rma
108500050519     c     §TA4Arma      comp      *blanks                            2626
108600050201      * Dati beneficiario
108700050201     c                   eval      P1rsc  = V1Crsc
108800050201     c                   eval      P1rsc2 = V1Crsc
108900990204      * Compongo "Risarcibile con il limite"
109000010910     c                   Eval      wNrDec    = §cvDEC
109100010905     c                   exsr      Edit
109200120502     c                   IF        not wInglese
109300050201     c                   eval      P1lri1 = §gedDBA  +   ' '
109400050201     c                                    +  %TRIM(OedNUMAN)
109500050201     c                                    +  SLR(xx)
109600120502     c                   ELSE
109700120502     c                   eval      P1lri1 = §gedDBA  +   ' '
109800120502     c                                    +  %TRIM(OedNUMAN)
109900120502     c                                    +  SLR2(xx)
110000120502     c                   ENDIF
110100000105      * Compongo la 1a riga del "Calcolo del risarcimento"
110200010905     c                   movel     V1dcr1        W035A
110300010905     c                   move      V1dcr1        W035B
110400050201     c                   eval      P1cr1  = V1Dcr1
110500050201     c                   eval      P1cr2  = V1Dcr2
110600050203      * Valuta & Importo da Liquidare (con 2 decimali)
110700050202     c                   eval      wImporto  = V1Cipl
110800050202     c                   eval      wNrDec    = §cvDEC
110900050202     c                   exsr      Edit
111000050202     c                   eval      P1ipl =  %trim(V1Cvpl) + ' '
111100050202     c                                   +  %trim(OedNumAN)
111200050203     c                   clear                   P1txt
111300120427     c                   IF        not wInglese
111400150603     c                   eval      %subst(P1txt :
111500150603     c                                    %len(%trim(P1ipl)) + 2) =
111600150603     c                                    C_Text_A
111700120427     c                   ELSE
111800150603     c                   eval      %subst(P1txt :
111900150603     c                                    %len(%trim(P1ipl)) + 2) =
112000150603     c                                    C_Text_A2
112100120427     c                   ENDIF
112200050201      * Dati Bartolini
112300050201      * => "chiodati" nel prtf
112400990204      * Dati Intestatario
112500050201     c                   eval      P1clp =  %trim(V1Ccac)  +  ' '
112600050201     c                                   +  %trim(V1Cloc)  +  ' '
112700050201     c                                   +  Wprv
112800990203      *
112900130920if  1c                   If        wV1Cdds >= wDataGLD
113000120427     c                   IF        not wInglese
113100050201     c                   WRITE     DN29T1
113200130911     c                   WRITE     DN29P1
113300120427     c                   ELSE
113400120427     c                   WRITE     DN29T12
113500130911     c                   WRITE     DN29P12
113600120427     c                   ENDIF
113700130920x   1c                   Else
113800130920     c                   IF        not wInglese
113900130920     c                   WRITE     DN29T1G
114000130920     c                   ELSE
114100130920     c                   WRITE     DN29T1G2
114200130920     c                   ENDIF
114300130920e   1c                   EndIf
114400150529
114500150529       // -?Chiusura file stampa & Cancellazione override?
114600150910     c                   If        oBEinv = 'M'  and  wV1Cdds >= wDataGLD
114700150529     c                   exsr      sr_DeleteOvr
114800150529     c                   EndIf
114900990203      *
115000990203     C                   ENDSR
115100050516      ****************************************************************
115200050516      *  GESTIONE FINESTRA SUL CHI PAGA
115300050516      ****************************************************************
115400050516     C     GES_WIND      BEGSR
115500050516
115600050516     c                   clear                   Wtipopag
115700050516     c                   clear                   WforzAIG
115800050516     c                   clear                   WforzGLD
115900050516
116000050516      * verifico in base ad alcune caratteristiche della CA chi pagherà
116100050516
116200131014      * 1 = se importo da liquidare > limite minimo in tabella   propongo A.I.G.
116300130920      * 2 = se importo da liquidare < limite minimo in tabella   propongo SETRAS (ex GLD)
116400131014      * 3 = evento fortuito                                      propongo A.I.G.
116500130920      * 4 = evento non fortuito                                  propongo SETRAS (ex GLD)
116600050516
116700050516      * non c'è evento
116800050516     c                   If        not *in01
116900050516
117000050516     c                   If        v1cipl > §stdlac
117100050516     c                   eval      Wtipopag = '1'
117200050516     c                   else
117300050516     c                   eval      Wtipopag = '2'
117400050516     c                   endif
117500050516      * se c'è evento (*in01 = ON ) verifico se fortuito (*in21 = on)
117600050516     c                   else
117700050516      * verifico se fortuito (*in21 = on)
117800050516     c                   If        *in21
117900050516     c                   eval      Wtipopag = '3'
118000050516     c                   else
118100050516     c                   eval      Wtipopag = '4'
118200050516     c                   endif
118300050516
118400050516     c                   endif
118500050516      *
118600131014      * se tipo pagamento 1/3 propongo A.I.G.
118700050613     c                   if        Wtipopag = '1' or Wtipopag = '3'
118800050613     c                   eval      w01pag = 'A'
118900050516     c                   else
119000130924     c                   eval      w01pag = 'S'
119100050516     c                   endif
119200050516
119300050516     c                   do        *hival
119400050517      *
119500050516     c                   exfmt     fi29w01
119600050517     c                   setoff                                       2028
119700050516      * se dato CMD 12 esco
119800050516     c                   If        *inkl
119900050516     c                   seton                                        28
120000050516     c                   leave
120100050516     c                   endif
120200050516      * eseguo i controlli
120300131014      * se importo da liquidare < limite A.I.G. (10000)
120400050516     c                   if        w01pag = 'A' and wtipopag = '2'
120500050516     c                             and WforzAIG  = ' '
120600050516     c                   seton                                        2028
120700050516     c                   eval      w01msg = msg(12)
120800050516     c                   eval      WforzAIG  = 'Y'
120900050516     c                   iter
121000050516     c                   endif
121100131014      * se importo da liquidare > limite A.I.G. (10000)
121200130924     c                   if        w01pag = 'S' and wtipopag = '1'
121300050516     c                             and WforzGLD  = ' '
121400050516     c                   seton                                        2028
121500050516     c                   eval      w01msg = msg(10)
121600050516     c                   eval      WforzGLD  = 'Y'
121700050516     c                   iter
121800050516     c                   endif
121900050516      * se evento fortuito
122000130924     c                   if        w01pag = 'S' and wtipopag = '3'
122100050516     c                             and WforzGLD  = ' '
122200050516     c                   seton                                        2028
122300050516     c                   eval      w01msg = msg(09)
122400050516     c                   eval      WforzGLD  = 'Y'
122500050516     c                   iter
122600050516     c                   endif
122700050516      * se evento non fortuito
122800050516     c                   if        w01pag = 'A' and wtipopag = '4'
122900050516     c                             and WforzAIG  = ' '
123000050516     c                   seton                                        2028
123100050516     c                   eval      w01msg = msg(11)
123200050516     c                   eval      WforzAIG  = 'Y'
123300050516     c                   iter
123400050516     c                   endif
123500150910      * per F6 vado a fine wundow
123600150910     c                   if        *inkf
123700131014      * se affidata ad A.I.G. valorizzo il dctflo
123800050516     c                   if        w01pag = 'A'
123900050713     c                   eval      §dctaffi = 'A'
124000060214      * pulisco la data stampa progetto di liquidazione Wv1cdds
124100060214     c                   clear                   wv1cdds
124200050714      * scrivo le note di affidamento
124300050714     c                   exsr      sub_noteaff
124400050713     c                   else
124500050713     c                   eval      §dctaffi = ' '
124600050714      *
124700050516     c                   endif
124800050516     c                   leave
124900050516     c                   endif
125000050516
125100050516     c                   enddo
125200050516
125300050516     C                   ENDSR
125400050714      *-------------------------------------------------------------------------*
125500131014      * Sub_noteaff   Routine scrittura note x affidamento pratica ad A.I.G.
125600050714      *-------------------------------------------------------------------------*
125700050714     c     sub_noteaff   begsr
125800050714
125900050714     c                   clear                   dcspno
126000050714
126100050714     c                   eval      ktpd='C'
126200050714     c                   eval      knkt=ds_numca
126300050714     c                   eval      kstd=' '
126400050714     c                   eval      kdim=dateu
126500050714     c                   movel     wora          khim
126600050714     c                   movel     dctfca        knks
126700050714     c                   eval      ktrc='N'
126800050714
126900050714      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA
127000050714      *  se esiste
127100050714     c     kdcsc         setgt     fndcs01l
127200050714     c     kdcs          readpe    fndcs01l
127300050714
127400050714     c                   eval      dcstpd = ktpd
127500050714     c                   eval      dcsnkt = knkt
127600050714     c                   eval      dcsstd = kstd
127700050714     c                   eval      dcsdim = kdim
127800050714     c                   eval      dcshim = khim
127900050714     c                   eval      dcsnks = knks
128000050714     c                   eval      dcstrc = ktrc
128100050714     c                   eval      dcsatb = ' '
128200050714
128300050714     c                   move      knmus         dcspru
128400050714     c                   eval      dcspos = 046
128500050714     c                   clear                   dcsstn
128600050714     c                   clear                   dcsft1
128700050714     c                   clear                   dcsdt1
128800050714
128900050714      * Scrivo la nota
129000050714     c                   add       1             dcspno
129100050714     c                   clear                   dcsnot
129200131014     c                   eval      dcsnot = 'Pratica affidata ad A.I.G.'
129300050714     c                   write     fndcs000
129400050714
129500050714     c                   endsr
129600990202      ****************************************************************
129700990202      * SCRIVO LA MODALITA' DI CALCOLO NEL FILE DESCRIZIONI
129800990202      ****************************************************************
129900990202     C     WRTDCS        BEGSR
130000990202      *
130100990202     c                   movel     'C'           ktpd
130200990202     c                   movel(P)  DS_numca      knkt
130300990202     c                   movel     'C'           kstd
130400990202     c                   clear                   kdim
130500990202     c                   clear                   khim
130600990202     c                   clear                   knks
130700990202     c                   movel     'D'           ktrc
130800990202      *
130900020212      * Cancello note esistenti se non si tratta di franchigia altrimenti le annullo e le trasmetto
131000020212     c                   if        *in07 = *off                                 No franchigia
131100990202     c                   do        *HIVAL
131200990202     c     kdcs          delete    fndcs000                           30
131300990202     c  N30              enddo
131400020212      *
131500020212     c                   else                                                   Si  franchigia
131600020212      * Annullo le note
131700020212     c     kdcs          setll     fndcs000
131800020212      *
131900020212     c                   do        *hival
132000020212     c     kdcs          reade     fndcs000
132100020212     c                   if        %eof(fndcs01l)
132200020212     c                   leave
132300020212     c                   endif
132400020212      *
132500020212     c                   eval      dcsatb = 'A'
132600020212     c                   clear                   dcsft1
132700020212     c                   clear                   dcsdt1
132800020212     c                   update    fndcs000
132900020212      *
133000020212     c                   enddo
133100020212      *
133200020212     c                   goto      EndWRTdcs
133300020212     c                   endif
133400990202      *
133500990202      * Imposto campi fissi per scrittura file
133600990202     c                   CLEAR                   FNDCS000
133700990202     c                   movel     'C'           DCStpd
133800990202     c                   movel(P)  DS_numca      DCSnkt
133900990202     c                   movel     'C'           DCSstd
134000990202     c                   movel     'D'           DCStrc
134100990202      *
134200990202      * Scrivo note inserite
134300990205    2c                   DO        4             JJ
134400990202      *
134500990202     c                   SELECT
134600990202     c                   WHEN      JJ = 1
134700000105     c                   eval      DCSnot = W035A
134800990202     c                   WHEN      JJ = 2
134900000105     c                   eval      DCSnot = W035b
135000990204     c                   WHEN      JJ = 3
135100990202     c                   eval      DCSnot = %subst(V1Dcr2: 1: 35)
135200990204     c                   WHEN      JJ = 4
135300990202     c                   eval      DCSnot = %subst(V1Dcr2: 36: 35)
135400990202     c                   ENDSL
135500990202      *
135600990202     c                   z-add     JJ            DCSpno
135700990203     c                   write     fndcs000
135800990202    1c                   ENDDO
135900990202      *
136000020125     C     EndWrtDCS     ENDSR
136100150609      *
136200150609      ****************************************************************
136300150609      *?Inserisco l'indirizzo e-mail al quale abbiamo inviato il P.L.?
136400150609      ****************************************************************
136500150609     c     WrtDCS_2      BEGSR
136600150609      *
136700150609      *//*?Il *pgm FIDN10R consente l'inserimento automatico di 1 sola?
136800150609      *//*?nota (con progressivo 1)?
136900150609     c*//                clear                   FIDN10ds
137000150609     c*//                eval      i10flm = 'W'
137100150609     c*//                eval      i10tpd = 'C'
137200150609     c*//                eval      i10nkt = ds_NumCA
137300150609     c*//                eval      i10nks = %editc( i00fca : 'X' )
137400150609     c*//                eval      i10trc = 'N'
137500150609     c*//                eval      i10not = 'Invio P.L. a:' +
137600150609     c*//                                   %subst(oBEima : 1 : 22)
137700150609     c*//                movel(P)  FIDN10ds      KPJBU
137800150609     c*//                call      'FIDN10R'
137900150609     c*//                parm                    KPJBA
138000150609      *
138100150609     c                   eval      ds_Time = %char( %dec( %timestamp() ) )
138200150811      *
138300150811      *?Cancellazione dell'eventuale indirizzo e-mail pre-esistente?
138400150811     c                   eval      kTPD = 'C'
138500150811     c                   eval      kNKT = DS_numca
138600150811     c                   eval      kSTD = *blanks
138700150811     c                   eval      kDIM = wDate
138800150811     c                   eval      kHIM = wTime / 100
138900150811     c                   eval      kNKS = %editc( i00fca : 'X' )
139000150811     c                   eval      kTRC = 'N'
139100150811      *
139200150811     c                   do        *HIVAL
139300150811     c     kdcs          delete    fndcs000                           30
139400150811     c  N30              enddo
139500150609      *
139600150811      *?Registrazione dell'indirizzo e-mail del beneficiario?
139700150609     c                   clear                   FNDCS000
139800150609      *
139900150609     c                   eval      DCStpd = 'C'
140000150609     c                   movel(P)  DS_numca      DCSnkt
140100150609     c                   eval      DCSdim = wDate
140200150609     c                   eval      DCShim = wTime / 100
140300150609     c                   eval      DCSnks = %editc( i00fca : 'X' )
140400150609     c                   eval      DCStrc = 'N'
140500150609     c                   eval      DCSpru = KNMUS
140600150609     c                   eval      DCSpos = DUTpou
140700150609      *
140800150609     c                   eval      DCSpno = 1
140900150609     c                   eval      DCSnot = 'Invio P.L. a:' +
141000150609     c                                      %subst(oBEima : 1 : 22)
141100150609     c                   write     FNDCS000
141200150609      *
141300150609     c                   if        %subst(oBEima : 23) <> *blank
141400150609     c                   eval      DCSpno = 2
141500150609     c                   eval      DCSnot = %subst(oBEima : 23)
141600150609     c                   write     FNDCS000
141700150609     c                   endif
141800150609      *
141900150609     c                   if        %subst(oBEima : 58) <> *blank
142000150609     c                   eval      DCSpno = 3
142100150609     c                   eval      DCSnot = %subst(oBEima : 58)
142200150609     c                   write     FNDCS000
142300150609     c                   endif
142400150609      *
142500150609     c                   ENDSR
142600980623      *****************************************************************
142700980623      *  SCRITTURA FASI C.A.
142800980623      *****************************************************************
142900980623     c     WRTDCF        BEGSR
143000980623      *
143100980429     c                   clear                   fndcf000
143200980722      *
143300990126     c                   z-add     I00aac        DCFaac
143400990126     c                   z-add     I00fil        DCFfil
143500990126     c                   z-add     I00nca        DCFnca
143600990126     c                   movel     DCTptr        DCFptr
143700990126     c                   z-add     I00fca        DCFfca
143800980722     c                   z-add     dateu         DCFdfc
143900990126     c                   movel     W0140         DCFhfc
144000990126     c                   z-add     I00fgs        DCFfev
144100980722     c                   movel     knmus         DCFpru
144200980722     c                   clear                   DCFlet
144300980907     c                   clear                   DCFftr
144400980907     c                   clear                   DCFdtr
144500980722      *
144600980519     c                   write     fndcf000
144700980623      *
144800971022     C                   ENDSR
144900981022      *****************************************************************
145000981022      *  SCRITTURA/AGGIORNAMENTO liquidazione C.A.
145100981022      *****************************************************************
145200981022     C     WRTDCL        BEGSR
145300981022      *
145400990126     c                   z-add     V1Cipl        DCLipl
145500010905     c                   IF        V1Cipl <> 0
145600010905     c                   movel     V1Cvpl        DCLvpl
145700010905     c                   else
145800010905     c                   clear                   DCLvpl
145900010907     c                   ENDIF
146000990126     c                   clear                   DCLcpl
146100990126     c                   clear                   DCLfrp
146200990126     c                   z-add     WV1Cdds       DCLdds
146300990126     c                   clear                   DCLftr
146400020201      * SE franchigia avalorizzo ANCHE la data accettazione
146500020201     c                   if        *in07
146600020201     c                   z-add     WV1Cdds       DCLdpl
146700020201     c                   endif
146800981022      *
146900990126      * Se non esiste RCD imposto chiave
147000990126     c                   If        Wfndcl <> 'S'
147100990126     c                   z-add     i00aac        DCLaac
147200990126     c                   z-add     i00fil        DCLfil
147300990126     c                   z-add     i00nca        DCLnca
147400990126     c                   WRITE     FNDCL000
147500990126     c                   Else
147600990126     c                   UPDATE    FNDCL000
147700990126     c                   Endif
147800990126      *
147900981022     C                   ENDSR
148000980722      *****************************************************************
148100990126      *  SCRITTURA anagrafica clienti per intestatario
148200980722      *****************************************************************
148300980520     C     WRTDKA        BEGSR
148400020212      *
148500020212     c                   movel     'I'           Wtrc
148600990126      *
148700020212      * se non c'è franchigia deleto e riscrivo se c'è franchigia annullo e sflaggo e non
148800020212      * riscrivo +
148900020212      *
149000020212     c                   if        *in07                                        07 on = Franchigia
149100020212     c     KDKA          chain     fndka01l
149200020212     c                   if        %found(fndka01l)
149300020212     c                   eval      dkaatb = 'A'
149400020212     c                   clear                   dkaftr
149500020212     c                   clear                   dkadtr
149600020212     c                   update    fndka000
149700020212     c                   endif
149800020212      *
149900020212     c                   goto      endwrtdka
150000020212      *
150100020212     c                   else
150200020212     c
150300990126     c     KDKA          delete    fndka01l                           30
150400020212      *
150500020212     c                   endif
150600981022      *
150700990126     c                   clear                   FNDKA000
150800981022     c                   z-add     i00aac        dkaaac
150900981022     c                   z-add     i00fil        dkafil
151000981022     c                   z-add     i00nca        dkanca
151100990212     c                   Select
151200990212     c                   When        V1Cksc > *zeros
151300990126     c                   z-add     V1Cksc        DKAksc
151400050208     c                   movel     DUTkci        DKAkcc
151500120723     c                   When      Wfndcl <> 'S'  or  DCLipt = *zero
151600120723     c                                            or  *in07
151700990212     c                   z-add     8888          DKAksc
151800990212     c                   movel     Wpointes      DKAksc
151900050208     c                   movel     DUTkci        DKAkcc
152000990212     c                   Endsl
152100981022     c                   movel     Wtrc          DKAtrc
152200990126     c                   movel     V1Crsc        DKArag
152300990203     c                   movel     WIND          DKAvia
152400990126     c                   movel     V1Ccac        DKAcap
152500990126     c                   movel     V1Cloc        DKAloc
152600990203     c                   movel     WPRV          DKAprv
152700050204     c                   movel     WNAZ          DKAnaz
152800981022      *
152900981022     c                   write     fndka000
153000990126      *
153100020125     C     EndWrtDKA     ENDSR
153200980717      **********************************************************************
153300980717      *  SCRITTURA/AGGIORNAMENTO TESTATA C.A.
153400980717      **********************************************************************
153500971022     C     WRTDCT        BEGSR
153600980723      *
153700980717      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
153800990126      *
153900990126     c                   CLEAR                   DS_FNDFA
154000990126     c                   CLEAR                   FIDN05DS
154100990126      *
154200990126     c                   movel     'F'           I05MOD
154300990126     c                   z-add     i00fca        I05fca
154400990126     c                   movel     DCTfpr        I05fpr
154500990126     c                   movel     DCTptr        I05tpc
154600990126     c                   If        DCTnev > *zeros
154700990126     c                   movel     'E'           I05fpe
154800990126     c                   Else
154900990126     c                   movel     'N'           I05fpe
155000990126     c                   Endif
155100990126     c                   If        DCTnde > *zeros
155200990126     c                   movel     'D'           I05fde
155300990126     c                   Else
155400990126     c                   movel     'N'           I05fde
155500990126     c                   Endif
155600990126     c                   movel     'O'           I05ffs
155700990126     c                   z-add     DCFdfc        i05dta
155800990126     c                   move      DCTtad        i05tad
155900990126      * valorizzo il numero c.a.
156000990126     c                   z-add     dctaac        i05aac
156100990126     c                   z-add     dctfil        i05fil
156200990126     c                   z-add     dctnca        i05nca
156300990126      *
156400990126     C                   CALL      'FIDN05R'
156500990126     c                   PARM                    KPJBA
156600990126     c                   PARM                    FIDN05DS
156700990126      *
156800990126     c                   movel     O05REC        DS_FNDFA
156900990126      *
157000980722     c                   SELECT
157100990518     c                   WHEN      O05ERR <> ' '
157200980722     c                   WHEN      DFAgfs = 'P'
157300981229     c                   movel     §DCTlnpc      dctgfc
157400980722     c                   WHEN      DFAgfs = 'S'
157500980717     c                   z-add     46            dctgfc
157600981023     c                   WHEN      DFAgfs = 'A'
157700981023     c                   z-add     DCTlna        dctgfc
157800980722     c                   OTHER
157900981023     c                   z-add     DCTfil        dctgfc
158000980722     c                   ENDSL
158100981223      *
158200990126     c                   z-add     i00fca        DCTfca
158300981223     c                   clear                   dctft1
158400981223     c                   clear                   dctft2
158500050713      * valorizzo i flag del dctflo
158600050713     c                   eval      dctflo = ddct01
158700050713
158800980722
158900981223     c                   update    fndct000
159000990122
159100971022     C                   ENDSR
159200020125      ****************************************************************
159300020125      * RICHIAMO PGM FIDN22R PER CHIUSURA C.A.
159400020125      ****************************************************************
159500020125     C     CallFIDN22    BEGSR
159600020125      *
159700020125     c                   clear                   FIDN22DS
159800020125     c                   eval      i22tri = 'R'                                 * Richiamato
159900020125     c                   eval      i22mod = 'C'                                 * Chiusura
160000020125     c                   eval      i22aac = i00aac                              * Anno CA
160100020125     c                   eval      i22fil = i00fil                              * P.O. apertura CA
160200020125     c                   eval      i22nca = i00nca                              * Numero CA
160300020125     c                   eval      i22cch = '15'                                * Causale chiusura
160400020125     c                   eval      i22fgs = i00fgs                              * P.O. gestione
160500020125     c                   movel(p)  FIDN22DS      kpjbu
160600020125     c                   call      'FIDN22R'
160700020125     c                   parm                    kpjba
160800020125      *
160900020125     C                   ENDSR
161000990126      *****************************************************************
161100990126      *  REPERIMENTO DATI TIPO ANOMALIA
161200990126      *****************************************************************
161300990126     C     CHTAD         BEGSR
161400990126      *
161500990126     c                   clear                   DTAD
161600990126      *
161700990126     c                   clear                   TIBS02DS
161800990126     C                   MOVEL     'C'           t02mod
161900990126     C                   MOVEL     knsif         t02sif
162000990126     C                   MOVEL     'TAD'         t02cod
162100990126     C                   MOVEL(P)  Wkey1         t02ke1
162200990126      *
162300990126     C                   CALL      'TIBS02R'
162400990126     C                   PARM                    KPJBA
162500990126     C                   PARM                    TIBS02DS
162600990126      *
162700990126     C                   if        t02err = *BLANKS
162800990126     C                   MOVEL     T02UNI        DTAD
162900990126     C                   endif
163000990126      *
163100990126     C                   ENDSR
163200990125      *****************************************************************
163300990125      * CONTROLLO DIVISA
163400990125      *****************************************************************
163500990125     C     CTRVLT        BEGSR
163600990125      *
163700990125     C                   MOVEL     'CV'          KCOD
163800990126     C                   MOVEL(P)  W003a         KKEY
163900990125     C     KTAB          CHAIN     TABEL00F                           30
164000990125      *
164100990125     C                   IF        *IN30 = *ON  or
164200990125     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
164300990125     C                   eval      *IN28 = *ON
164400990125     C                   ELSE
164500990125      *
164600990125     C                   movel     TBLuni        DSCV
164700010905      *
164800010905      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
164900010905     C                   IF        §CVFDC <> 'S'
165000990126     C                   move      W0133         W0033
165100990125      * Controllo se ammessi importi decimali
165200990125     C                   eval      *IN28  =  (§CVFDC <> 'S'  and  W0033 <> 0)
165300010912     C                   IF        *in28
165400010912     C                   MOVEL     MSG(6)        V1CMSG
165500010912     C                   GOTO      ENDVLT
165600010912     C                   ENDIF
165700010905     C                   else
165800010910      * controllo numero massimo di decimali impostabili per la valuta
165900010910     C     W0133         casNE     0             srCTRDEC
166000010905     C                   ENDCS
166100010910     C                   IF        *in28
166200010914     C                   MOVEL     MSG(7)        V1CMSG
166300010910     c                   move      §cvDEC        w002a
166400010914     c                   eval      %subst(V1CMSG:29:2) = w002a
166500010910     C                   GOTO      ENDVLT
166600010910     C                   ENDIF
166700010905     C                   ENDIF
166800010905      *
166900990125     C                   ENDIF
167000990125      *
167100010907     C     endvlt        ENDSR
167200010905
167300010905      *****************************************************************
167400010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
167500010905      *****************************************************************
167600010910     C     srCTRDEC      BEGSR
167700010905      *
167800011017     C                   reset                   XDECDS
167900010905      *
168000011017     C                   z-add     W0133         ICDnum
168100011017     C                   z-add     §cvDEC        ICDnrd
168200010905      *
168300011017     C                   call      'XDEC'
168400011017     C                   parm                    XDECDS
168500010905      *
168600011017     C                   IF        OCDesi  <> *off
168700010910     C                   eval      *IN28 = *ON
168800010905     C                   ENDIF
168900010905      *
169000010905     C                   ENDSR
169100010905
169200010905      *****************************************************************
169300010905      * CONVERSIONE IMPORTI IN MONETA CORRENTE
169400010905      *****************************************************************
169500010905     C     srCMB         BEGSR
169600010905      *
169700010905     C                   reset                   YEURCODS
169800010905      *
169900010905     C                   movel     W003a         yecDVI
170000010905     C                   z-add     W0133         yecIMI
170100010910     C                   movel     §gedDBA       yecDVO
170200010905      *
170300010905     C                   call      'YEURCO'
170400010905     C                   parm                    YEURCODS
170500010905      *
170600010905     C                   IF        yecESI  = ' '
170700010905     C                   z-add     yecIMO        W0133
170800011002     C                   movel     yecDVO        W003a
170900010905     C                   ENDIF
171000010905      *
171100010905     C                   ENDSR
171200010905
171300990125      *****************************************************************
171400990125      * DECODIFICO NAZIONE
171500990125      *****************************************************************
171600990125     C     CTRNAZ        BEGSR
171700990125      *
171800990125     C                   clear                   DS15
171900990125     C                   movel     '15'          KCOD
172000990125     C                   movel(P)  WNAZ          KKEY
172100990125     C     KTAB          CHAIN     TABEL00f                           30
172200990125      *
172300990125     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
172400990125     C                   movel     TBLUNI        DS15
172500990125     C                   endif
172600990125      *
172700990125     C                   ENDSR
172800150529
172900150529      /free
173000150529
173100150529       //--------------------------------------------------------------
173200150529       //?Chiusura file stampa & Cancellazione override.               ?
173300150529       //--------------------------------------------------------------
173400150529       BEGSR  sr_DeleteOvr;
173500150529
173600150529         if  %open(FIDN29P);
173700150529
173800150529           close  FIDN29P;
173900150529
174000150529           Qcmd = c_CmdDltOvr;
174100150529           exsr  sr_ExecCmd;
174200150529
174300150529         endif;
174400150529
174500150529         if  %open(FIDN29P2);
174600150529
174700150529           close  FIDN29P2;
174800150529
174900150529           Qcmd = c_CmdDltOvr2;
175000150529           exsr  sr_ExecCmd;
175100150529
175200150529         endif;
175300150529
175400150529       ENDSR;
175500150529
175600150529       //--------------------------------------------------------------
175700150529       //?Esecuzione del comando (già impostato).                      ?
175800150529       //--------------------------------------------------------------
175900150529       BEGSR  sr_ExecCmd;
176000150529
176100150529         clear Qcap0100;
176200150529         Qcabcsdh = *off;
176300150529         Qcapa    = *off;
176400150529         Qcacmdss = *off;
176500150529         Qcaerved = *allX'00';
176600150529
176700150529         clear Qusec;
176800150529         Qusbprv  = %size(Qusec);
176900150529
177000150529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
177100150529                           %size(Qcap0100) : 'CPOP0100' : *omit :
177200150529                           0 : 0 : Qusec);
177300150529
177400150529         //if  Qusei <> *blank;
177500150529         //  ...;
177600150529         //endif;
177700150529
177800150529       ENDSR;
177900150529
178000150529      /end-free
178100150529
178200980722      *****************************************************************
178300980623      *  ROUTINE INIZIALE
178400980722      *****************************************************************
178500951009     C     *INZSR        BEGSR
178600980722
178700951009     C     *ENTRY        PLIST
178800951009     C                   PARM                    KPJBA
178900980722
179000981020     C                   MOVEL     KPJBU         FIDN00DS
179100980625
179200990126     C                   MOVEL     TIT1          VCTtit
179300990126
179400981126      * Dati società
179500050208     c     *dtaara       define    §azute        AZUTEds
179600050208     c     *dtaara       define    §datiute      dDATIUTE
179700050208     c                   in(E)     *dtaara
179800050208     c                   if        %ERROR or RSUT = *blanks
179900050208     c                   clear                   Tibs34Ds
180000050208     c                   call      'TIBS34R'
180100050208     c                   parm                    Tibs34Ds
180200050208     c                   in        *dtaara
180300050208     c                   endif
180400050208      *
180500050208     c                   eval      TBLkut = 1
180600010413      ***
180700010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
180800010413      ***
180900010413      *
181000010413      * recupero la moneta corrente
181100010413     C                   CLEAR                   tibs02ds
181200010413      *
181300010413     C                   MOVEL     'L'           T02TLA
181400010413     C                   MOVEL     'C'           T02MOD
181500010413     C                   MOVEL     KNSIF         T02SIF
181600010413     C                   MOVEL     'GED'         T02COD
181700010905     C                   MOVEL(P)  'DANNI'       t02ke1
181800010413      *
181900010413     C                   CALL      'TIBS02R'
182000010413     C                   PARM                    KPJBA
182100010413     C                   PARM                    tibs02ds
182200010413      *
182300010910     c                   movel     T02UNI        DGEDDN
182400011026      *
182500011026      * ...ed i relativi dati
182600011026     c                   movel     §gedDBA       W003a
182700011026     C                   exsr      CTRVLT
182800011026      *
182900010413      * recupero i valori nella moneta corrente
183000010413     C                   CLEAR                   tibs02ds
183100010413      *
183200010413     C                   MOVEL     'L'           T02TLA
183300010413     C                   MOVEL     'C'           T02MOD
183400010413     C                   MOVEL     KNSIF         T02SIF
183500010413     C                   MOVEL     'GEI'         T02COD
183600010910     C                   MOVEL     §GEDDBA       T02KE1
183700010413      *
183800010413     C                   CALL      'TIBS02R'
183900010413     C                   PARM                    KPJBA
184000010413     C                   PARM                    tibs02ds
184100010413      *
184200010413     c                   movel     t02uni        DGEI
184300010413      * VALORIZZO il comapo del controvalore CMR
184400010413     c                   z-add     §geccm        W§GEcmr
184500010905      * Valorizzo il valore del limite risarcibile
184600010417     c                   z-add     §gelrp        W§GElrp
184700010413      *
184800990204      ***
184900990204      * Aggancio tabella con dati consuldanni
185000990204      ***
185100010905     c                   clear                   TIBS02DS
185200010905     C                   MOVEL     'C'           t02mod
185300010905     C                   MOVEL     knsif         t02sif
185400010905     C                   MOVEL     'STD'         t02cod
185500010905     C                   MOVEL(P)  '1'           t02ke1
185600010910     C                   MOVEL(P)  §gedDBA       t02ke2
185700010905      *
185800010905     C                   CALL      'TIBS02R'
185900010905     C                   PARM                    KPJBA
186000010905     C                   PARM                    TIBS02DS
186100010905      *
186200010905     C                   clear                   DSTD
186300010905     C                   If        t02err = *BLANKS
186400010905     C                   MOVEL     T02UNI        DSTD
186500010905     C                   Endif
186600130920     C                   clear                   WLBDAT
186700130920     C                   eval      G02inv = §STDgld
186800130920     C                   eval      G02err = '3'
186900130920     C                   call      'XSRDA8'
187000130920     C                   parm                    WLBDAT
187100130920     C                   z-add     G02inv        wDataGLD
187200981009
187300980611      * ORA
187400980611     C                   TIME                    W0140
187500980619      * UDATE IN GGMMAAAA
187600980611     C                   MOVE      W0140         WDTGIO
187700050714      * ORA
187800050714     C                   MOVEL     W0140         WORA
187900980619      * UDATE IN AAAAMMGG
188000971016     C                   Z-ADD     WDTGIO        G02DAT
188100971016     C                   MOVEL     *BLANK        G02ERR
188200971016     C                   CALL      'XSRDA8'
188300971016     C                   PARM                    WLBDAT
188400980611     C                   MOVEL     G02INV        DATEU
188500980623     C     *iso          MOVEL     DATEU         Wdataoggi
188600980625
188700990203      * Imposto la data del giorno nella data stampa
188800990203     C                   z-add     WDTGIO        V1Cdds
188900990203
189000971020     C     KTAB          KLIST
189100050208     C                   KFLD                    TBLkut
189200971020     C                   KFLD                    KCOD
189300971020     C                   KFLD                    KKEY
189400980625      *
189500990108     c     kbol          klist
189600980515     c                   kfld                    i00aas
189700980515     c                   kfld                    i00lnp
189800980515     c                   kfld                    i00nrs
189900980515     c                   kfld                    i00nsp
190000050519      *
190100050519     C     Ktas4         klist
190200050519     C                   kfld                    DCTaas
190300050519     C                   kfld                    DCTlnp
190400050519     C                   kfld                    DCTnrs
190500050519     C                   kfld                    DCTnsp
190600050519     C                   kfld                    TA4trc
190700050519     c                   eval      TA4trc = 'A'
190800980625      *
190900971021     C     KDET          KLIST
191000990108     C                   KFLD                    DCTaae
191100990108     C                   KFLD                    DCTnev
191200980625      *
191300971021     C     KDCS          KLIST
191400990202     C                   KFLD                    kTPD
191500990202     C                   KFLD                    kNKT
191600990202     C                   KFLD                    kSTD
191700990202     C                   KFLD                    kDIM
191800990202     C                   KFLD                    kHIM
191900990202     C                   KFLD                    kNKS
192000990202     C                   KFLD                    kTRC
192100050714      * completa
192200050714     c     kdcsc         klist
192300050714     c                   kfld                    ktpd
192400050714     c                   kfld                    knkt
192500050714     c                   kfld                    kstd
192600050714     c                   kfld                    kdim
192700050714     c                   kfld                    khim
192800050714     c                   kfld                    knks
192900050714     c                   kfld                    ktrc
193000050714     c                   kfld                    prgnot
193100980625      *
193200980520     c     knumca        klist
193300980520     c                   kfld                    i00aac
193400980520     c                   kfld                    i00fil
193500980520     c                   kfld                    i00nca
193600981020      *
193700981020     c     KDKA          klist
193800981020     c                   kfld                    i00aac
193900981020     c                   kfld                    i00fil
194000981020     c                   kfld                    i00nca
194100981020     c                   kfld                    Wtrc
194200981224      *
194300951009     C                   ENDSR
194400050201      *---------------------------------------------------------------*
194500981014** MSG  L78                                                                  *
194600990203Immetere Data Stampa corretta.                                                 1
194700150924La data emissione non può essere precedente al                                 2
194800990203Inserire la modalità di calcolo.                                               3
194900040223ATTENZIONE: manca la tabella Polizza avvertire l'ufficio ASS di SEDE    !!     4
195000990203Inserire l'importo da liquidare.                                               5
195100010914Non ammessi importi con decimali per la divisa indicata                        6
195200010914Ammessi importi con massimo __ decimali per la divisa indicata                 7
195300020222Il Mass.Risarcib. è > del limite per Pratiche in Franchigia. ENTER per forzare 8
195400131014La C.A. è legata ad evento fortuito paga A.I.G. - ENTER forzare                9
195500131014Importo da liquidare > limite gestirebbe A.I.G. - ENTER forzare                10
195600130920C.A. legata evento non fortuito gestirebbe SETRAS - ENTER forzare              11
195700130920Importo da liquidare < limite gestirebbe SETRAS - ENTER forzare                12
195800990204** VLR  L78                                                          *
195900990203 a Kg merce perduta o danneggiata (legge 450/85).                              1  Legge 450
196000990126 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo          2  Elevazione
196100990126 in base all'assicurazione stipulata sul valore merce.                         3  Valore in bolla
196200990126 a Kg merce perduta o danneggiata (C.M.R.).                                    4  CMR
196300990204 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo          5  Mandato a Kg
196400990204 a collo in base al Vostro mandato assicurativo                                6  Mandato a collo
196500990204 a spedizione in base al Vostro mandato assicurativo                           7  Mandato a sped.
196600060316 a Kg merce perduta o danneggiata (Art 1696 c.c. mod. Dlgs 286/2005)           8  Decreto legge 286
196700060316 a Kg merce perduta o danneggiata                                              9  rcv transitoria
196800050222** SLR  L56                                            *------------>*
196900990204 a Kg merce perduta o danneggiata (legge 450/85).
197000050222 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo
197100990204 In base all'assicurazione stipulata sul valore merce.
197200990204 a Kg merce perduta o danneggiata (C.M.R.).
197300050222 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo
197400990204 a collo in base al Vostro mandato assicurativo
197500990204 a spedizione in base al Vostro mandato assicurativo
197600060316 a Kg merce perduta o danneggiata (Art 1696 c.c. mod. Dlgs 286/2005)
197700060316 a Kg merce perduta o danneggiata
197800120502** SLR2 L78                                            *------------>*
197900120502 per each kilo of missing or damaged goods (lgs decree 450/85)
198000120507 per each kilo of missing or damaged goods, according to the insurance writ
198100120507 according to the insurance writ, based to value of goods
198200120502 per each kilo of missing or damaged goods (C.M.R.)
198300120507 per each kilo of missing or damaged goods, according to the insurance writ
198400120507 per each parcel, according to the insurance writ
198500120507 according to the insurance writ, each shipment
198600120507 per each kilo of missing or damaged goods (art 1696 m.lgs d.286/2005)
198700120502 per each kilo of missing or damaged goods
