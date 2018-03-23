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
054600150923     c*//                eval      *in08  = $MancaMail
054700150923     c                   eval      *in08  = ( oBEinv <> 'M' )
054800150909      *
054900150731      * -?Rilevata mancanza della e-mail del cliente (non 8888)?
055000150910     c                   IF        $MancaMail
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
063300150729     c                   eval      $MancaMail = ( oBEinv <> 'M'     and
063400150729     c                                            oBEksc <> *zeros  and
063500150729     c                             %subst( %editc( oBEksc : 'X' ) : 4 : 4 )
063600150729     c                                                   <> '8888' )
063700150729      *
063800140725      * Determino il P.O. del cliente beneficiario,  se non è codificato utilizzo lnp (M) o lna (D
063900140725     C                   clear                   Wpointes
064000140725     c                   If        v1cksc = 0  and
064100140725     c                               DCTptr = 'D'
064200140725     C                   z-add     DCTlna        Wpointes
064300140725     c                   endif
064400140725     c                   If        v1cksc = 0  and
064500140725     c                               DCTptr = 'M'
064600140725     C                   movel     §DCTlnpc      Wpointes
064700140725     c                   endif
064800140725
064900990122      *
065000990122     C                   ENDSR
065100990122      *****************************************************************
065200990122      *   DETERMINO IL TIPO RISARCIMENTO
065300990122      *****************************************************************
065400990122     C     SUB_tipris    BEGSR
065500990122
065600990126     c                   clear                   xx
065700990126
065800021104      * Se PA EXPORT o PF IMPORT, non DPD, ho tipo risarcimento Estero CMR
065900021111    1c                   IF        (§DCTtisp = 'E'  and  §DCTport = 'A')  or
066000021104     c                             (§DCTtisp = 'I'  and  §DCTport = 'F'
066100021104     c                                              and  §DCTeuro <> 'D')
066200990126     c                   eval      XX = 4
066300010905     c                   z-add     W§GEcmr       WImporto
066400011026     c                   z-add     3             wNrDec
066500021111   x1c                   ELSE
066600990122
066700990125      * Esposto il valore in bolla
066800021111    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
066900990126     c                   eval      XX = 3
067000011114     c                   if        TASias <> *zeros and TASvas = *blanks
067100011114     c                   movel     'ITL'         TASvas
067200011114     c                   endif
067300990126      *
067400990126     c                   z-add     TASias        W0133
067500990126     c                   movel     TAsvas        W003a
067600010910     c                   if        TAsvas <> §gedDBA
067700010905     C                   exsr      srCMB
067800990122     c                   endif
067900010905     c                   z-add     W0133         WImporto
068000011026     c                   z-add     §cvdec        wNrDec
068100021111   x2c                   ELSE
068200990122      *
068300990415      * Se il cliente è assicurato imposto dati assicurazione
068400021111    3c                   IF        *IN06 = *OFF
068500990415      *  Importo Assicurato
068600990415     c                   z-add     DPTvlm        W0133
068700990415     c                   movel     DPTvvm        W003a
068800021111    4c                   IF        DPTvlm > *zeros
068900010910     c                   IF        DPTvvm <> §gedDBA
069000010905     C                   exsr      srCMB
069100010906     c                   ENDIF
069200021111   -4c                   ENDIF
069300010905     c                   z-add     W0133         WImporto
069400011026     c                   z-add     §cvdec        wNrDec
069500990415      *  Imposto costante in base al tipo valore
069600990415     c                   SELECT
069700990415     c                   WHEN      DPTftc = 'R'  or  DPTftc = '9'                 Limite Risarcibile
069800990415     c                   eval      XX = 2
069900990415     c                   WHEN        DPTfvm = '3'                                 Kg merce
070000990415     c                   eval      XX = 5
070100990415     c                   WHEN        DPTfvm = '1'                                 colli
070200990415     c                   eval      XX = 6
070300990415     c                   WHEN        DPTfvm = '2'                                 spedizione
070400990415     c                   eval      XX = 7
070500990415     c                   ENDSL
070600990415      *
070700021111   -3c                   ENDIF
070800000202      *
070900990122
071000000202      * Se non è assicurato oppure c'è RIVALSA si applica
071100020422      *   la legge 450 o la CMR se PA Import o PF Export
071200990122     c                   SELECT
071300990126     c                   WHEN        XX <> *zeros
071400021111      * se spedizione EEX o EUP, oppure export DPD, o export FedEx
071500021111     c                   WHEN           §DCTeuro =  'X'
071600021111     c                             or   §DCTeuro =  'P'
071700020422     c                             or   §DCTeuro =  'D'
071800020422     c                             and  §DCTtisp =  'E'
071900021111     c                             or   §DCTeuro =  'F'
072000021111     c                             and  §DCTtisp =  'E'
072100990126     c                   eval      XX = 4
072200010905     c                   z-add     W§GEcmr       WImporto
072300011026     c                   z-add     3             wNrDec
072400990122     c                   OTHER
072500060316      * Verifico il tipo di limite risarcibile in base al flag in dctflo
072600060316     c                   If        §dcttlrp = ' '
072700060316     c                   eval      XX = 1                                       * legge 450
072800060316     c                   z-add     6,2           WImporto
072900060316     c                   endif
073000060316     c                   If        §dcttlrp = '6'
073100060316     c                   eval      XX = 9                                       * rcv transitoria
073200060316     c                   z-add     6,2           WImporto
073300060316     c                   endif
073400060316     c                   If        §dcttlrp = '1'
073500060316     c                   eval      XX = 8                                       * legge 286
073600060316     c                   z-add     1             WImporto
073700060316     c                   endif
073800060316
073900011026     c                   z-add     §cvdec        wNrDec
074000990122     c                   ENDSL
074100990122      *
074200021111   -2c                   ENDIF
074300021111   -1c                   ENDIF
074400000204      * Se esiste la Rivalsa il tipo risarcimanto è secondo la legge 450
074500000204     c                   IF        Wrivalsa = 'S'
074600010905     c                   clear                   WImporto
074700011026     c                   clear                   wNrDec
074800021111      * se spedizione EEX o EUP, oppure export DPD, o export FedEx
074900021111     c                   if             §DCTeuro =  'X'
075000021111     c                             or   §DCTeuro =  'P'
075100020422     c                             or   §DCTeuro =  'D'
075200020422     c                             and  §DCTtisp =  'E'
075300021111     c                             or   §DCTeuro =  'F'
075400021111     c                             and  §DCTtisp =  'E'
075500000204     c                   eval      XX = 4
075600010905     c                   z-add     W§GEcmr       WImporto
075700011026     c                   z-add     3             wNrDec
075800000204     c                   else
075900000204     c                   eval      XX = 1
076000060411      * Verifico il tipo di limite risarcibile in base al flag in dctflo
076100060411     c                   If        §dcttlrp = ' '
076200060411     c                   eval      XX = 1                                       * legge 450
076300060411     c                   z-add     6,2           WImporto
076400060411     c                   endif
076500060411     c                   If        §dcttlrp = '6'
076600060411     c                   eval      XX = 9                                       * rcv transitoria
076700060411     c                   z-add     6,2           WImporto
076800060411     c                   endif
076900060411     c                   If        §dcttlrp = '1'
077000060411     c                   eval      XX = 8                                       * legge 286
077100060411     c                   z-add     1             WImporto
077200060411     c                   endif
077300011026     c                   z-add     §cvdec        wNrDec
077400000204     c                   ENDIF
077500000204     c                   ENDIF
077600990122      *
077700990126      * Imposto costante per video
077800010905     c                   exsr      Edit
077900010910     c                   eval      Wlri = §gedDBA  +   ' '
078000010907     c                                  +  %TRIM(OedNUMAN)
078100010905     c                                  +  VLR(xx)
078200990203     c                   eval      V1Dlr1 =  %subst(Wlri:1:49)
078300990205     c                   eval      V1Dlr2 =  %subst(Wlri:50)
078400990126      *
078500060615      * Verifico se spedizione assicurata per pilotare la stampa di una frase
078600990126      *
078700060615     c                   eval      *in35 = (xx = 2 or xx = 3 or xx>= 5)
078800060615
078900990122     C                   ENDSR
079000010905      ************************************************************
079100010905      * Editazione di un numero
079200010905      ************************************************************
079300010905     C     Edit          Begsr
079400010905     C*
079500010907     C                   reset                   XEDITDS
079600010906     C* numero da elaborare
079700010907     C* (WImporto ha 5 decimali, IedNUMERO ha 0 decimali)
079800010906     C                   Select
079900010907     C                   When      wNrDec    = 0
080000010907     C                   eval      IedNUMERO = wImporto
080100010907     C                   When      wNrDec    = 1
080200010907     C                   eval      IedNUMERO = wImporto * 10
080300010907     C                   When      wNrDec    = 2
080400010907     C                   eval      IedNUMERO = wImporto * 100
080500010907     C                   When      wNrDec    = 3
080600010907     C                   eval      IedNUMERO = wImporto * 1000
080700010907     C                   When      wNrDec    = 4
080800010907     C                   eval      IedNUMERO = wImporto * 10000
080900010907     C                   When      wNrDec    = 5
081000010907     C                   eval      IedNUMERO = wImporto * 100000
081100010906     C                   Endsl
081200010905     C* numero decimali
081300010907     C                   eval      IedNRDEC  = wNrDec
081400010906     C* codice di editazione
081500050203     C                   eval      IedEDTCOD = '3'
081600010906     C* max lunghezza campo in output
081700050203     C                   eval      IedLENFLD = 16
081800010905     C*
081900010907     C                   call      'XEDIT'
082000010907     C                   parm                    XEDITDS
082100010905      *
082200010905     C                   ENDSR
082300990126      ****************************************************************
082400990126      * CONTROLLI VIDEO 1
082500990126      ****************************************************************
082600990126     C     CTRD01        BEGSR
082700990126
082800050513     c                   setoff                                       1012
082900990126
083000990126      *  NUMERO POLIZZA se non impostata manca la tabella
083100050524     C                   IF        V1Cnpl = *blanks
083200050524     c                   eval      *in28 = *ON
083300050524     c                   movel     msg(4)        V1Cmsg
083400050524     C                   ENDIF
083500050524     C   28              GOTO      ECTD01
083600990126      *
083700990126      * Data stampa
083800990126     C                   IF        V1Cdds  = *zeros
083900990126     C                   seton                                        1028
084000990126     C                   movel     MSG(1)        V1CMSG
084100990126     C                   ELSE
084200990126     C                   clear                   WLBDAT
084300990126     C                   z-add     V1Cdds        G02DAT
084400990126     C                   CALL      'XSRDA8'
084500990126     C                   PARM                    WLBDAT
084600990126     C                   IF        G02ERR = '1'
084700990126     C                   seton                                        1028
084800990126     C                   movel     MSG(1)        V1CMSG
084900990126     C                   ELSE
085000990126     C                   z-add     G02DAT        V1Cdds
085100990126     C                   z-add     G02INV        WV1Cdds
085200990126     C                   ENDIF
085300990126     C                   ENDIF
085400990126     C   28              GOTO      ECTD01
085500150924      *?La data di stampa NON può essere precedente alla data di?
085600150924      *?fusione GLD con SETRAS (nella e-mail è prevista solo la?
085700150924      *?firma SETRAS)?
085800150924     c                   if        wV1Cdds < wDataGLD
085900150924     c                   seton                                        10  28
086000150924     c                   eval      V1Cmsg = %trimR(Msg(2)) + ' '
086100150924     c                             + %subst(%editc(wDataGLD:'X'):7:2) + '/'
086200150924     c                             + %subst(%editc(wDataGLD:'X'):5:2) + '/'
086300150924     c                             + %subst(%editc(wDataGLD:'X'):1:4)
086400150924     c
086500150924     c                   leavesr
086600150924     c                   endif
086700990126      *
086800990203      * Importo da Liquidare
086900990203     C                   IF        V1Cipl = *zeros
087000990203     C                   seton                                        1228
087100990203     C                   movel     MSG(5)        V1CMSG
087200010911     C                   else
087300010911     c                   z-add     V1Cipl        W0133
087400010911     c                   movel     V1Cvpl        W003a
087500010911     C                   exsr      CTRVLT
087600010911     C   28              seton                                        12
087700990126     C                   ENDIF
087800990126     C   28              GOTO      ECTD01
087900020222      *
088000020222      * Importo da liquidare > 103,30 Euro
088100031211      * - Prima imposto l'importo "limite" da testare in base alla data
088200031211      *   spedizione
088300031211     c                   reset                   w§STDlpf
088400031211if  1c                   if            *in07     =  *on
088500031211     c                             and $ErrF6    =  *off
088600031211     c                             and DS_DataSp >= §STDdlpf1
088700031211sel 2c                   select
088800031211w   2c                   when      DS_DataSp >= §STDdlpf2
088900031211     c                   eval      w§STDlpf   = §STDlpf2
089000031211w   2c                   when      DS_DataSp >= §STDdlpf1
089100031211     c                   eval      w§STDlpf   = §STDlpf1
089200031211e   2c                   endsl
089300031217     C                   IF        V1Cipl    >  w§STDlpf
089400020222     C                   eval      $ErrF6 = *on
089500020222     C                   seton                                        1228
089600020222     C                   movel     msg(8)        V1Cmsg
089700020222     C                   goto      ECTD01
089800020222     C                   ENDIF
089900031217e   1c                   endif
090000990203      *
090100990203      * Modalità di calcolo
090200990203     C                   IF        V1Dcr1 = *blanks  and  V1Dcr2 = *blanks
090300020125     C                             and  not *in07
090400990203     C                   seton                                        1328
090500990203     C                   movel     MSG(3)        V1CMSG
090600990203     C                   ENDIF
090700990203     C   28              GOTO      ECTD01
090800990126      *
090900990126     C     ECTD01        ENDSR
091000980623      *****************************************************************
091100150806      *   EMISSIONE PROGETTO DI LIQUIDAZIONE E AGGIORNA FILES
091200980623      *****************************************************************
091300990122     C     ELABORA       BEGSR
091400990122      *
091500150806      * Emissione Progetto di Liquidazione
091600020125      * (non se flag di Franchigia = 'F')
091700131014      * (e se chi paga NON è A.I.G.)
091800050520     C                   if        not *in07 and w01pag <> 'A'
091900990203     C                   EXSR      PRTPDL
092000020125     C                   endif
092100990203      * Descrizioni
092200150609      * (le note di calcolo si scrivono solo se c'è progetto di liquidazione)
092300150609      * (le note di Invio P.L. anche, oltre all'indirizzo e-mail)
092400050714     C                   if        not *in07 and w01pag <> 'A'
092500990203     C                   EXSR      WRTDCS
092600150609      *?Inserisco anche l'indirizzo e-mail al quale abbiamo inviato il P.L.?
092700150609      *?SE inviata e-mail, cioè:?
092800150609      *?· C.A. in fase 700  &?
092900150910      *?· prevista e-mail al cliente (quindi premuto F6, non F8)  &?
093000150910      *?· C.A. non a carico della GLD?
093100150910if  2c                   if        i00fca  =  c_FLiq    and
093200150910     c                             oBEinv  =  'M'       and
093300150910     c                             wV1Cdds >= wDataGLD
093400150609     c                   exsr      WrtDCS_2
093500150609e   2c                   endif
093600050714     c                   endif
093700990122      * Fasi
093800020201      * (non se flag di Franchigia = 'F': andrà in chiusura)
093900131014      * (non se Affidata ad A.I.G. non si fa progetto di liquidazione)
094000050714     C                   if        not *in07 and w01pag <> 'A'
094100981020     C                   EXSR      WRTDCF
094200020125     C                   endif
094300990126      * Liquidazione
094400981022     C                   EXSR      WRTDCL
094500990126      * Anagrafica
094600981020     C                   EXSR      WRTDKA
094700981020      * Testata C.A.
094800020201      * (non se flag di Franchigia = 'F': andrà in chiusura)
094900020128      * Se fase di chiusura occorre disallocare il record del file
095000020128      *   FNDCT, che sarà poi aggiornato dal pgm. FIDN22R !
095100020125     C                   if        not *in07
095200130920      * per Setras
095300050714     c                   if        w01pag <> 'A'
095400981020     C                   EXSR      WRTDCT
095500050714     c                   else
095600050714      * valorizzo i flag del dctflo
095700050714     c                   eval      dctflo = ddct01
095800050714     c                   update    fndct000
095900050714     c                   endif
096000050714
096100020128     C                   else
096200020128     C                   unlock    FNDCT01L
096300020125     C                   endif
096400020125      *
096500020125      * Registrazione fase di chiusura
096600020125     C                   if        *in07
096700020125     C                   exsr      CallFIDN22
096800020125     C                   endif
096900980623      *
097000980623     C                   ENDSR
097100990203      ****************************************************************
097200990203      * PREPARO CAMPI E STAMPO PROGETTO DI LIQUIDAZIONE
097300990203      ****************************************************************
097400990203     C     PRTPDL        BEGSR
097500150529
097600150529      /free
097700150605
097800150605       *in08 = *off;
097900150529
098000150529       // -?Invio Progetto di Liquidazione via e-mail?
098100150910       IF  oBEinv = 'M'  and  wV1Cdds >= wDataGLD;
098200150529
098300150529         // -?Reperimento tab. MRA = Bart-Maling - Danni?
098400150529         clear  dMRAdan;
098500150529         clear  TIBS02ds;
098600150529         T02mod = 'C';
098700150529         //T02sif = knsif;
098800150529         T02cod = 'MRA';
098900150529         T02ke1 = %trimr(SDSpgm);
099000150529
099100150529         TNTBE_RicercaControllo ( kpjba : tibs02ds );
099200150529
099300150529         if T02err = *blanks;
099400150529           dMRAdan = T02uni;
099500150529         endif;
099600150529
099700150529         // -?In stampa (NON RI-stampa) lo spool viene inviato via e-mail?
099800150529         //  ?(quindi niente "HIGHLIGHT" né "UNDERLINE")?
099900150529         *in08 = *on;
100000150529
100100150529         // -?Esecuzione override?
100200150529         reset  TRTCM1ds;
100300150529         If  §MRADreg <> *blank;
100400150529
100500150529           §CM1mitt = %trim(§MRAdMitt);
100600150529           §CM1dst  = oBEima;
100700150529           §CM1tips = §MRAdReg;
100800150529           §CM1po   = %editc( DUTpou : 'X' );
100900150529           §CM1var  = %trim( §CM1var ) + ' '
101000150529                    + %char( DCTaac )
101100150529                    + %editw( DCTfil : '0   ' )
101200150529                    + '/'
101300150529                    + %trim( %editc( DCTnca : '3' ) );
101400150529           §CM1idp  = §MRAdIdPro;
101500150529
101600150529           if  wInglese;
101700150529             Qcmd = c_CmdOvrPrtF2;
101800150529           else;
101900150529             Qcmd = c_CmdOvrPrtF;
102000150529           endif;
102100150529           Qcmd = Qcmd
102200150529                + ' outq(' + %trim(§MRAdOutqI) + ')'
102300150529                + ' usrdfndta(''' + TRTCM1ds + ''')';
102400150529           exsr  sr_ExecCmd;
102500150529
102600150529         EndIf;
102700150529
102800150529       ENDIF;
102900150529
103000150529      /end-free
103100130924      *
103200130924      * Apro il file di stampa
103300130924     c                   If        wV1Cdds >= wDataGLD
103400130924     c                   IF        wInglese
103500130924     c                   open      FIDN29P2
103600130924     c                   ELSE
103700130924     c                   open      FIDN29P
103800130924     c                   ENDIF
103900130924     c                   Else
104000130924     c                   IF        wInglese
104100130924     c                   open      FIDN29PG2
104200130924     c                   ELSE
104300130924     c                   open      FIDN29PG
104400130924     c                   ENDIF
104500130924     c                   EndIf
104600130924      *
104700050519      * se ca su spedizione maggiore del 17042005 stampo anche il numero polizza
104800050519     c                   If        ds_datasp > 20050417
104900050519     c                   seton                                        25
105000050519     c                   eval      P1npz = v1cnpl
105100050519     c                   endif
105200990204      *
105300990204      * Località di stampa
105400050201      * => Bologna (fissa in output)
105500050201      * Data di stampa
105600050201     c                   eval      P1dds  = V1Cdds
105700050201      * Numero danno
105800050201     c                   eval      P1prd  = %trim(%editc(DCTpra:'Z'))
105900050201     c                                    + '/'
106000050201     c                                    + %trim(%editc(DCTprn:'Z'))
106100050201      * Data Spedizione
106200050201     c                   z-add     V1Cdsp        P1dsp
106300990325      * Serie spedizione
106400050201if  1c                   if        DCTnrs > *zeros
106500050201     c                   eval      P1sesp = %trim(%editc(DCTnrs:'Z'))
106600050201     c                                    + ' '
106700050201     c                                    + %trim(%editc(DCTnsp:'Z'))
106800050201x   1c                   else
106900050201     c                   eval      P1sesp = %trim(%editc(DCTnsp:'Z'))
107000050201e   1c                   endif
107100050519      * Riferimenti
107200050519     c                   z-add     TASrmn        P1rmn
107300050519      * il riferimento mittente alfanum. per la sede si trova in TITA4
107400050519     c     ktas4         chain     TITA430C
107500050519     c                   if        %found(TITA430C)
107600050519     c                   movel     TA4not        DTA4A
107700050519     c                   else
107800050519     c                   clear                   DTA4A
107900050519     c                   endif
108000050519     c                   movel     §TA4Arma      P1rma
108100050519     c     §TA4Arma      comp      *blanks                            2626
108200050201      * Dati beneficiario
108300050201     c                   eval      P1rsc  = V1Crsc
108400050201     c                   eval      P1rsc2 = V1Crsc
108500990204      * Compongo "Risarcibile con il limite"
108600010910     c                   Eval      wNrDec    = §cvDEC
108700010905     c                   exsr      Edit
108800120502     c                   IF        not wInglese
108900050201     c                   eval      P1lri1 = §gedDBA  +   ' '
109000050201     c                                    +  %TRIM(OedNUMAN)
109100050201     c                                    +  SLR(xx)
109200120502     c                   ELSE
109300120502     c                   eval      P1lri1 = §gedDBA  +   ' '
109400120502     c                                    +  %TRIM(OedNUMAN)
109500120502     c                                    +  SLR2(xx)
109600120502     c                   ENDIF
109700000105      * Compongo la 1a riga del "Calcolo del risarcimento"
109800010905     c                   movel     V1dcr1        W035A
109900010905     c                   move      V1dcr1        W035B
110000050201     c                   eval      P1cr1  = V1Dcr1
110100050201     c                   eval      P1cr2  = V1Dcr2
110200050203      * Valuta & Importo da Liquidare (con 2 decimali)
110300050202     c                   eval      wImporto  = V1Cipl
110400050202     c                   eval      wNrDec    = §cvDEC
110500050202     c                   exsr      Edit
110600050202     c                   eval      P1ipl =  %trim(V1Cvpl) + ' '
110700050202     c                                   +  %trim(OedNumAN)
110800050203     c                   clear                   P1txt
110900120427     c                   IF        not wInglese
111000150603     c                   eval      %subst(P1txt :
111100150603     c                                    %len(%trim(P1ipl)) + 2) =
111200150603     c                                    C_Text_A
111300120427     c                   ELSE
111400150603     c                   eval      %subst(P1txt :
111500150603     c                                    %len(%trim(P1ipl)) + 2) =
111600150603     c                                    C_Text_A2
111700120427     c                   ENDIF
111800050201      * Dati Bartolini
111900050201      * => "chiodati" nel prtf
112000990204      * Dati Intestatario
112100050201     c                   eval      P1clp =  %trim(V1Ccac)  +  ' '
112200050201     c                                   +  %trim(V1Cloc)  +  ' '
112300050201     c                                   +  Wprv
112400990203      *
112500130920if  1c                   If        wV1Cdds >= wDataGLD
112600120427     c                   IF        not wInglese
112700050201     c                   WRITE     DN29T1
112800130911     c                   WRITE     DN29P1
112900120427     c                   ELSE
113000120427     c                   WRITE     DN29T12
113100130911     c                   WRITE     DN29P12
113200120427     c                   ENDIF
113300130920x   1c                   Else
113400130920     c                   IF        not wInglese
113500130920     c                   WRITE     DN29T1G
113600130920     c                   ELSE
113700130920     c                   WRITE     DN29T1G2
113800130920     c                   ENDIF
113900130920e   1c                   EndIf
114000150529
114100150529       // -?Chiusura file stampa & Cancellazione override?
114200150910     c                   If        oBEinv = 'M'  and  wV1Cdds >= wDataGLD
114300150529     c                   exsr      sr_DeleteOvr
114400150529     c                   EndIf
114500990203      *
114600990203     C                   ENDSR
114700050516      ****************************************************************
114800050516      *  GESTIONE FINESTRA SUL CHI PAGA
114900050516      ****************************************************************
115000050516     C     GES_WIND      BEGSR
115100050516
115200050516     c                   clear                   Wtipopag
115300050516     c                   clear                   WforzAIG
115400050516     c                   clear                   WforzGLD
115500050516
115600050516      * verifico in base ad alcune caratteristiche della CA chi pagherà
115700050516
115800131014      * 1 = se importo da liquidare > limite minimo in tabella   propongo A.I.G.
115900130920      * 2 = se importo da liquidare < limite minimo in tabella   propongo SETRAS (ex GLD)
116000131014      * 3 = evento fortuito                                      propongo A.I.G.
116100130920      * 4 = evento non fortuito                                  propongo SETRAS (ex GLD)
116200050516
116300050516      * non c'è evento
116400050516     c                   If        not *in01
116500050516
116600050516     c                   If        v1cipl > §stdlac
116700050516     c                   eval      Wtipopag = '1'
116800050516     c                   else
116900050516     c                   eval      Wtipopag = '2'
117000050516     c                   endif
117100050516      * se c'è evento (*in01 = ON ) verifico se fortuito (*in21 = on)
117200050516     c                   else
117300050516      * verifico se fortuito (*in21 = on)
117400050516     c                   If        *in21
117500050516     c                   eval      Wtipopag = '3'
117600050516     c                   else
117700050516     c                   eval      Wtipopag = '4'
117800050516     c                   endif
117900050516
118000050516     c                   endif
118100050516      *
118200131014      * se tipo pagamento 1/3 propongo A.I.G.
118300050613     c                   if        Wtipopag = '1' or Wtipopag = '3'
118400050613     c                   eval      w01pag = 'A'
118500050516     c                   else
118600130924     c                   eval      w01pag = 'S'
118700050516     c                   endif
118800050516
118900050516     c                   do        *hival
119000050517      *
119100050516     c                   exfmt     fi29w01
119200050517     c                   setoff                                       2028
119300050516      * se dato CMD 12 esco
119400050516     c                   If        *inkl
119500050516     c                   seton                                        28
119600050516     c                   leave
119700050516     c                   endif
119800050516      * eseguo i controlli
119900131014      * se importo da liquidare < limite A.I.G. (10000)
120000050516     c                   if        w01pag = 'A' and wtipopag = '2'
120100050516     c                             and WforzAIG  = ' '
120200050516     c                   seton                                        2028
120300050516     c                   eval      w01msg = msg(12)
120400050516     c                   eval      WforzAIG  = 'Y'
120500050516     c                   iter
120600050516     c                   endif
120700131014      * se importo da liquidare > limite A.I.G. (10000)
120800130924     c                   if        w01pag = 'S' and wtipopag = '1'
120900050516     c                             and WforzGLD  = ' '
121000050516     c                   seton                                        2028
121100050516     c                   eval      w01msg = msg(10)
121200050516     c                   eval      WforzGLD  = 'Y'
121300050516     c                   iter
121400050516     c                   endif
121500050516      * se evento fortuito
121600130924     c                   if        w01pag = 'S' and wtipopag = '3'
121700050516     c                             and WforzGLD  = ' '
121800050516     c                   seton                                        2028
121900050516     c                   eval      w01msg = msg(09)
122000050516     c                   eval      WforzGLD  = 'Y'
122100050516     c                   iter
122200050516     c                   endif
122300050516      * se evento non fortuito
122400050516     c                   if        w01pag = 'A' and wtipopag = '4'
122500050516     c                             and WforzAIG  = ' '
122600050516     c                   seton                                        2028
122700050516     c                   eval      w01msg = msg(11)
122800050516     c                   eval      WforzAIG  = 'Y'
122900050516     c                   iter
123000050516     c                   endif
123100150910      * per F6 vado a fine wundow
123200150910     c                   if        *inkf
123300131014      * se affidata ad A.I.G. valorizzo il dctflo
123400050516     c                   if        w01pag = 'A'
123500050713     c                   eval      §dctaffi = 'A'
123600060214      * pulisco la data stampa progetto di liquidazione Wv1cdds
123700060214     c                   clear                   wv1cdds
123800050714      * scrivo le note di affidamento
123900050714     c                   exsr      sub_noteaff
124000050713     c                   else
124100050713     c                   eval      §dctaffi = ' '
124200050714      *
124300050516     c                   endif
124400050516     c                   leave
124500050516     c                   endif
124600050516
124700050516     c                   enddo
124800050516
124900050516     C                   ENDSR
125000050714      *-------------------------------------------------------------------------*
125100131014      * Sub_noteaff   Routine scrittura note x affidamento pratica ad A.I.G.
125200050714      *-------------------------------------------------------------------------*
125300050714     c     sub_noteaff   begsr
125400050714
125500050714     c                   clear                   dcspno
125600050714
125700050714     c                   eval      ktpd='C'
125800050714     c                   eval      knkt=ds_numca
125900050714     c                   eval      kstd=' '
126000050714     c                   eval      kdim=dateu
126100050714     c                   movel     wora          khim
126200050714     c                   movel     dctfca        knks
126300050714     c                   eval      ktrc='N'
126400050714
126500050714      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA
126600050714      *  se esiste
126700050714     c     kdcsc         setgt     fndcs01l
126800050714     c     kdcs          readpe    fndcs01l
126900050714
127000050714     c                   eval      dcstpd = ktpd
127100050714     c                   eval      dcsnkt = knkt
127200050714     c                   eval      dcsstd = kstd
127300050714     c                   eval      dcsdim = kdim
127400050714     c                   eval      dcshim = khim
127500050714     c                   eval      dcsnks = knks
127600050714     c                   eval      dcstrc = ktrc
127700050714     c                   eval      dcsatb = ' '
127800050714
127900050714     c                   move      knmus         dcspru
128000050714     c                   eval      dcspos = 046
128100050714     c                   clear                   dcsstn
128200050714     c                   clear                   dcsft1
128300050714     c                   clear                   dcsdt1
128400050714
128500050714      * Scrivo la nota
128600050714     c                   add       1             dcspno
128700050714     c                   clear                   dcsnot
128800131014     c                   eval      dcsnot = 'Pratica affidata ad A.I.G.'
128900050714     c                   write     fndcs000
129000050714
129100050714     c                   endsr
129200990202      ****************************************************************
129300990202      * SCRIVO LA MODALITA' DI CALCOLO NEL FILE DESCRIZIONI
129400990202      ****************************************************************
129500990202     C     WRTDCS        BEGSR
129600990202      *
129700990202     c                   movel     'C'           ktpd
129800990202     c                   movel(P)  DS_numca      knkt
129900990202     c                   movel     'C'           kstd
130000990202     c                   clear                   kdim
130100990202     c                   clear                   khim
130200990202     c                   clear                   knks
130300990202     c                   movel     'D'           ktrc
130400990202      *
130500020212      * Cancello note esistenti se non si tratta di franchigia altrimenti le annullo e le trasmetto
130600020212     c                   if        *in07 = *off                                 No franchigia
130700990202     c                   do        *HIVAL
130800990202     c     kdcs          delete    fndcs000                           30
130900990202     c  N30              enddo
131000020212      *
131100020212     c                   else                                                   Si  franchigia
131200020212      * Annullo le note
131300020212     c     kdcs          setll     fndcs000
131400020212      *
131500020212     c                   do        *hival
131600020212     c     kdcs          reade     fndcs000
131700020212     c                   if        %eof(fndcs01l)
131800020212     c                   leave
131900020212     c                   endif
132000020212      *
132100020212     c                   eval      dcsatb = 'A'
132200020212     c                   clear                   dcsft1
132300020212     c                   clear                   dcsdt1
132400020212     c                   update    fndcs000
132500020212      *
132600020212     c                   enddo
132700020212      *
132800020212     c                   goto      EndWRTdcs
132900020212     c                   endif
133000990202      *
133100990202      * Imposto campi fissi per scrittura file
133200990202     c                   CLEAR                   FNDCS000
133300990202     c                   movel     'C'           DCStpd
133400990202     c                   movel(P)  DS_numca      DCSnkt
133500990202     c                   movel     'C'           DCSstd
133600990202     c                   movel     'D'           DCStrc
133700990202      *
133800990202      * Scrivo note inserite
133900990205    2c                   DO        4             JJ
134000990202      *
134100990202     c                   SELECT
134200990202     c                   WHEN      JJ = 1
134300000105     c                   eval      DCSnot = W035A
134400990202     c                   WHEN      JJ = 2
134500000105     c                   eval      DCSnot = W035b
134600990204     c                   WHEN      JJ = 3
134700990202     c                   eval      DCSnot = %subst(V1Dcr2: 1: 35)
134800990204     c                   WHEN      JJ = 4
134900990202     c                   eval      DCSnot = %subst(V1Dcr2: 36: 35)
135000990202     c                   ENDSL
135100990202      *
135200990202     c                   z-add     JJ            DCSpno
135300990203     c                   write     fndcs000
135400990202    1c                   ENDDO
135500990202      *
135600020125     C     EndWrtDCS     ENDSR
135700150609      *
135800150609      ****************************************************************
135900150609      *?Inserisco l'indirizzo e-mail al quale abbiamo inviato il P.L.?
136000150609      ****************************************************************
136100150609     c     WrtDCS_2      BEGSR
136200150609      *
136300150609      *//*?Il *pgm FIDN10R consente l'inserimento automatico di 1 sola?
136400150609      *//*?nota (con progressivo 1)?
136500150609     c*//                clear                   FIDN10ds
136600150609     c*//                eval      i10flm = 'W'
136700150609     c*//                eval      i10tpd = 'C'
136800150609     c*//                eval      i10nkt = ds_NumCA
136900150609     c*//                eval      i10nks = %editc( i00fca : 'X' )
137000150609     c*//                eval      i10trc = 'N'
137100150609     c*//                eval      i10not = 'Invio P.L. a:' +
137200150609     c*//                                   %subst(oBEima : 1 : 22)
137300150609     c*//                movel(P)  FIDN10ds      KPJBU
137400150609     c*//                call      'FIDN10R'
137500150609     c*//                parm                    KPJBA
137600150609      *
137700150609     c                   eval      ds_Time = %char( %dec( %timestamp() ) )
137800150811      *
137900150811      *?Cancellazione dell'eventuale indirizzo e-mail pre-esistente?
138000150811     c                   eval      kTPD = 'C'
138100150811     c                   eval      kNKT = DS_numca
138200150811     c                   eval      kSTD = *blanks
138300150811     c                   eval      kDIM = wDate
138400150811     c                   eval      kHIM = wTime / 100
138500150811     c                   eval      kNKS = %editc( i00fca : 'X' )
138600150811     c                   eval      kTRC = 'N'
138700150811      *
138800150811     c                   do        *HIVAL
138900150811     c     kdcs          delete    fndcs000                           30
139000150811     c  N30              enddo
139100150609      *
139200150811      *?Registrazione dell'indirizzo e-mail del beneficiario?
139300150609     c                   clear                   FNDCS000
139400150609      *
139500150609     c                   eval      DCStpd = 'C'
139600150609     c                   movel(P)  DS_numca      DCSnkt
139700150609     c                   eval      DCSdim = wDate
139800150609     c                   eval      DCShim = wTime / 100
139900150609     c                   eval      DCSnks = %editc( i00fca : 'X' )
140000150609     c                   eval      DCStrc = 'N'
140100150609     c                   eval      DCSpru = KNMUS
140200150609     c                   eval      DCSpos = DUTpou
140300150609      *
140400150609     c                   eval      DCSpno = 1
140500150609     c                   eval      DCSnot = 'Invio P.L. a:' +
140600150609     c                                      %subst(oBEima : 1 : 22)
140700150609     c                   write     FNDCS000
140800150609      *
140900150609     c                   if        %subst(oBEima : 23) <> *blank
141000150609     c                   eval      DCSpno = 2
141100150609     c                   eval      DCSnot = %subst(oBEima : 23)
141200150609     c                   write     FNDCS000
141300150609     c                   endif
141400150609      *
141500150609     c                   if        %subst(oBEima : 58) <> *blank
141600150609     c                   eval      DCSpno = 3
141700150609     c                   eval      DCSnot = %subst(oBEima : 58)
141800150609     c                   write     FNDCS000
141900150609     c                   endif
142000150609      *
142100150609     c                   ENDSR
142200980623      *****************************************************************
142300980623      *  SCRITTURA FASI C.A.
142400980623      *****************************************************************
142500980623     c     WRTDCF        BEGSR
142600980623      *
142700980429     c                   clear                   fndcf000
142800980722      *
142900990126     c                   z-add     I00aac        DCFaac
143000990126     c                   z-add     I00fil        DCFfil
143100990126     c                   z-add     I00nca        DCFnca
143200990126     c                   movel     DCTptr        DCFptr
143300990126     c                   z-add     I00fca        DCFfca
143400980722     c                   z-add     dateu         DCFdfc
143500990126     c                   movel     W0140         DCFhfc
143600990126     c                   z-add     I00fgs        DCFfev
143700980722     c                   movel     knmus         DCFpru
143800980722     c                   clear                   DCFlet
143900980907     c                   clear                   DCFftr
144000980907     c                   clear                   DCFdtr
144100980722      *
144200980519     c                   write     fndcf000
144300980623      *
144400971022     C                   ENDSR
144500981022      *****************************************************************
144600981022      *  SCRITTURA/AGGIORNAMENTO liquidazione C.A.
144700981022      *****************************************************************
144800981022     C     WRTDCL        BEGSR
144900981022      *
145000990126     c                   z-add     V1Cipl        DCLipl
145100010905     c                   IF        V1Cipl <> 0
145200010905     c                   movel     V1Cvpl        DCLvpl
145300010905     c                   else
145400010905     c                   clear                   DCLvpl
145500010907     c                   ENDIF
145600990126     c                   clear                   DCLcpl
145700990126     c                   clear                   DCLfrp
145800990126     c                   z-add     WV1Cdds       DCLdds
145900990126     c                   clear                   DCLftr
146000020201      * SE franchigia avalorizzo ANCHE la data accettazione
146100020201     c                   if        *in07
146200020201     c                   z-add     WV1Cdds       DCLdpl
146300020201     c                   endif
146400981022      *
146500990126      * Se non esiste RCD imposto chiave
146600990126     c                   If        Wfndcl <> 'S'
146700990126     c                   z-add     i00aac        DCLaac
146800990126     c                   z-add     i00fil        DCLfil
146900990126     c                   z-add     i00nca        DCLnca
147000990126     c                   WRITE     FNDCL000
147100990126     c                   Else
147200990126     c                   UPDATE    FNDCL000
147300990126     c                   Endif
147400990126      *
147500981022     C                   ENDSR
147600980722      *****************************************************************
147700990126      *  SCRITTURA anagrafica clienti per intestatario
147800980722      *****************************************************************
147900980520     C     WRTDKA        BEGSR
148000020212      *
148100020212     c                   movel     'I'           Wtrc
148200990126      *
148300020212      * se non c'è franchigia deleto e riscrivo se c'è franchigia annullo e sflaggo e non
148400020212      * riscrivo +
148500020212      *
148600020212     c                   if        *in07                                        07 on = Franchigia
148700020212     c     KDKA          chain     fndka01l
148800020212     c                   if        %found(fndka01l)
148900020212     c                   eval      dkaatb = 'A'
149000020212     c                   clear                   dkaftr
149100020212     c                   clear                   dkadtr
149200020212     c                   update    fndka000
149300020212     c                   endif
149400020212      *
149500020212     c                   goto      endwrtdka
149600020212      *
149700020212     c                   else
149800020212     c
149900990126     c     KDKA          delete    fndka01l                           30
150000020212      *
150100020212     c                   endif
150200981022      *
150300990126     c                   clear                   FNDKA000
150400981022     c                   z-add     i00aac        dkaaac
150500981022     c                   z-add     i00fil        dkafil
150600981022     c                   z-add     i00nca        dkanca
150700990212     c                   Select
150800990212     c                   When        V1Cksc > *zeros
150900990126     c                   z-add     V1Cksc        DKAksc
151000050208     c                   movel     DUTkci        DKAkcc
151100120723     c                   When      Wfndcl <> 'S'  or  DCLipt = *zero
151200120723     c                                            or  *in07
151300990212     c                   z-add     8888          DKAksc
151400990212     c                   movel     Wpointes      DKAksc
151500050208     c                   movel     DUTkci        DKAkcc
151600990212     c                   Endsl
151700981022     c                   movel     Wtrc          DKAtrc
151800990126     c                   movel     V1Crsc        DKArag
151900990203     c                   movel     WIND          DKAvia
152000990126     c                   movel     V1Ccac        DKAcap
152100990126     c                   movel     V1Cloc        DKAloc
152200990203     c                   movel     WPRV          DKAprv
152300050204     c                   movel     WNAZ          DKAnaz
152400981022      *
152500981022     c                   write     fndka000
152600990126      *
152700020125     C     EndWrtDKA     ENDSR
152800980717      **********************************************************************
152900980717      *  SCRITTURA/AGGIORNAMENTO TESTATA C.A.
153000980717      **********************************************************************
153100971022     C     WRTDCT        BEGSR
153200980723      *
153300980717      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
153400990126      *
153500990126     c                   CLEAR                   DS_FNDFA
153600990126     c                   CLEAR                   FIDN05DS
153700990126      *
153800990126     c                   movel     'F'           I05MOD
153900990126     c                   z-add     i00fca        I05fca
154000990126     c                   movel     DCTfpr        I05fpr
154100990126     c                   movel     DCTptr        I05tpc
154200990126     c                   If        DCTnev > *zeros
154300990126     c                   movel     'E'           I05fpe
154400990126     c                   Else
154500990126     c                   movel     'N'           I05fpe
154600990126     c                   Endif
154700990126     c                   If        DCTnde > *zeros
154800990126     c                   movel     'D'           I05fde
154900990126     c                   Else
155000990126     c                   movel     'N'           I05fde
155100990126     c                   Endif
155200990126     c                   movel     'O'           I05ffs
155300990126     c                   z-add     DCFdfc        i05dta
155400990126     c                   move      DCTtad        i05tad
155500990126      * valorizzo il numero c.a.
155600990126     c                   z-add     dctaac        i05aac
155700990126     c                   z-add     dctfil        i05fil
155800990126     c                   z-add     dctnca        i05nca
155900990126      *
156000990126     C                   CALL      'FIDN05R'
156100990126     c                   PARM                    KPJBA
156200990126     c                   PARM                    FIDN05DS
156300990126      *
156400990126     c                   movel     O05REC        DS_FNDFA
156500990126      *
156600980722     c                   SELECT
156700990518     c                   WHEN      O05ERR <> ' '
156800980722     c                   WHEN      DFAgfs = 'P'
156900981229     c                   movel     §DCTlnpc      dctgfc
157000980722     c                   WHEN      DFAgfs = 'S'
157100980717     c                   z-add     46            dctgfc
157200981023     c                   WHEN      DFAgfs = 'A'
157300981023     c                   z-add     DCTlna        dctgfc
157400980722     c                   OTHER
157500981023     c                   z-add     DCTfil        dctgfc
157600980722     c                   ENDSL
157700981223      *
157800990126     c                   z-add     i00fca        DCTfca
157900981223     c                   clear                   dctft1
158000981223     c                   clear                   dctft2
158100050713      * valorizzo i flag del dctflo
158200050713     c                   eval      dctflo = ddct01
158300050713
158400980722
158500981223     c                   update    fndct000
158600990122
158700971022     C                   ENDSR
158800020125      ****************************************************************
158900020125      * RICHIAMO PGM FIDN22R PER CHIUSURA C.A.
159000020125      ****************************************************************
159100020125     C     CallFIDN22    BEGSR
159200020125      *
159300020125     c                   clear                   FIDN22DS
159400020125     c                   eval      i22tri = 'R'                                 * Richiamato
159500020125     c                   eval      i22mod = 'C'                                 * Chiusura
159600020125     c                   eval      i22aac = i00aac                              * Anno CA
159700020125     c                   eval      i22fil = i00fil                              * P.O. apertura CA
159800020125     c                   eval      i22nca = i00nca                              * Numero CA
159900020125     c                   eval      i22cch = '15'                                * Causale chiusura
160000020125     c                   eval      i22fgs = i00fgs                              * P.O. gestione
160100020125     c                   movel(p)  FIDN22DS      kpjbu
160200020125     c                   call      'FIDN22R'
160300020125     c                   parm                    kpjba
160400020125      *
160500020125     C                   ENDSR
160600990126      *****************************************************************
160700990126      *  REPERIMENTO DATI TIPO ANOMALIA
160800990126      *****************************************************************
160900990126     C     CHTAD         BEGSR
161000990126      *
161100990126     c                   clear                   DTAD
161200990126      *
161300990126     c                   clear                   TIBS02DS
161400990126     C                   MOVEL     'C'           t02mod
161500990126     C                   MOVEL     knsif         t02sif
161600990126     C                   MOVEL     'TAD'         t02cod
161700990126     C                   MOVEL(P)  Wkey1         t02ke1
161800990126      *
161900990126     C                   CALL      'TIBS02R'
162000990126     C                   PARM                    KPJBA
162100990126     C                   PARM                    TIBS02DS
162200990126      *
162300990126     C                   if        t02err = *BLANKS
162400990126     C                   MOVEL     T02UNI        DTAD
162500990126     C                   endif
162600990126      *
162700990126     C                   ENDSR
162800990125      *****************************************************************
162900990125      * CONTROLLO DIVISA
163000990125      *****************************************************************
163100990125     C     CTRVLT        BEGSR
163200990125      *
163300990125     C                   MOVEL     'CV'          KCOD
163400990126     C                   MOVEL(P)  W003a         KKEY
163500990125     C     KTAB          CHAIN     TABEL00F                           30
163600990125      *
163700990125     C                   IF        *IN30 = *ON  or
163800990125     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
163900990125     C                   eval      *IN28 = *ON
164000990125     C                   ELSE
164100990125      *
164200990125     C                   movel     TBLuni        DSCV
164300010905      *
164400010905      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
164500010905     C                   IF        §CVFDC <> 'S'
164600990126     C                   move      W0133         W0033
164700990125      * Controllo se ammessi importi decimali
164800990125     C                   eval      *IN28  =  (§CVFDC <> 'S'  and  W0033 <> 0)
164900010912     C                   IF        *in28
165000010912     C                   MOVEL     MSG(6)        V1CMSG
165100010912     C                   GOTO      ENDVLT
165200010912     C                   ENDIF
165300010905     C                   else
165400010910      * controllo numero massimo di decimali impostabili per la valuta
165500010910     C     W0133         casNE     0             srCTRDEC
165600010905     C                   ENDCS
165700010910     C                   IF        *in28
165800010914     C                   MOVEL     MSG(7)        V1CMSG
165900010910     c                   move      §cvDEC        w002a
166000010914     c                   eval      %subst(V1CMSG:29:2) = w002a
166100010910     C                   GOTO      ENDVLT
166200010910     C                   ENDIF
166300010905     C                   ENDIF
166400010905      *
166500990125     C                   ENDIF
166600990125      *
166700010907     C     endvlt        ENDSR
166800010905
166900010905      *****************************************************************
167000010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
167100010905      *****************************************************************
167200010910     C     srCTRDEC      BEGSR
167300010905      *
167400011017     C                   reset                   XDECDS
167500010905      *
167600011017     C                   z-add     W0133         ICDnum
167700011017     C                   z-add     §cvDEC        ICDnrd
167800010905      *
167900011017     C                   call      'XDEC'
168000011017     C                   parm                    XDECDS
168100010905      *
168200011017     C                   IF        OCDesi  <> *off
168300010910     C                   eval      *IN28 = *ON
168400010905     C                   ENDIF
168500010905      *
168600010905     C                   ENDSR
168700010905
168800010905      *****************************************************************
168900010905      * CONVERSIONE IMPORTI IN MONETA CORRENTE
169000010905      *****************************************************************
169100010905     C     srCMB         BEGSR
169200010905      *
169300010905     C                   reset                   YEURCODS
169400010905      *
169500010905     C                   movel     W003a         yecDVI
169600010905     C                   z-add     W0133         yecIMI
169700010910     C                   movel     §gedDBA       yecDVO
169800010905      *
169900010905     C                   call      'YEURCO'
170000010905     C                   parm                    YEURCODS
170100010905      *
170200010905     C                   IF        yecESI  = ' '
170300010905     C                   z-add     yecIMO        W0133
170400011002     C                   movel     yecDVO        W003a
170500010905     C                   ENDIF
170600010905      *
170700010905     C                   ENDSR
170800010905
170900990125      *****************************************************************
171000990125      * DECODIFICO NAZIONE
171100990125      *****************************************************************
171200990125     C     CTRNAZ        BEGSR
171300990125      *
171400990125     C                   clear                   DS15
171500990125     C                   movel     '15'          KCOD
171600990125     C                   movel(P)  WNAZ          KKEY
171700990125     C     KTAB          CHAIN     TABEL00f                           30
171800990125      *
171900990125     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
172000990125     C                   movel     TBLUNI        DS15
172100990125     C                   endif
172200990125      *
172300990125     C                   ENDSR
172400150529
172500150529      /free
172600150529
172700150529       //--------------------------------------------------------------
172800150529       //?Chiusura file stampa & Cancellazione override.               ?
172900150529       //--------------------------------------------------------------
173000150529       BEGSR  sr_DeleteOvr;
173100150529
173200150529         if  %open(FIDN29P);
173300150529
173400150529           close  FIDN29P;
173500150529
173600150529           Qcmd = c_CmdDltOvr;
173700150529           exsr  sr_ExecCmd;
173800150529
173900150529         endif;
174000150529
174100150529         if  %open(FIDN29P2);
174200150529
174300150529           close  FIDN29P2;
174400150529
174500150529           Qcmd = c_CmdDltOvr2;
174600150529           exsr  sr_ExecCmd;
174700150529
174800150529         endif;
174900150529
175000150529       ENDSR;
175100150529
175200150529       //--------------------------------------------------------------
175300150529       //?Esecuzione del comando (già impostato).                      ?
175400150529       //--------------------------------------------------------------
175500150529       BEGSR  sr_ExecCmd;
175600150529
175700150529         clear Qcap0100;
175800150529         Qcabcsdh = *off;
175900150529         Qcapa    = *off;
176000150529         Qcacmdss = *off;
176100150529         Qcaerved = *allX'00';
176200150529
176300150529         clear Qusec;
176400150529         Qusbprv  = %size(Qusec);
176500150529
176600150529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
176700150529                           %size(Qcap0100) : 'CPOP0100' : *omit :
176800150529                           0 : 0 : Qusec);
176900150529
177000150529         //if  Qusei <> *blank;
177100150529         //  ...;
177200150529         //endif;
177300150529
177400150529       ENDSR;
177500150529
177600150529      /end-free
177700150529
177800980722      *****************************************************************
177900980623      *  ROUTINE INIZIALE
178000980722      *****************************************************************
178100951009     C     *INZSR        BEGSR
178200980722
178300951009     C     *ENTRY        PLIST
178400951009     C                   PARM                    KPJBA
178500980722
178600981020     C                   MOVEL     KPJBU         FIDN00DS
178700980625
178800990126     C                   MOVEL     TIT1          VCTtit
178900990126
179000981126      * Dati società
179100050208     c     *dtaara       define    §azute        AZUTEds
179200050208     c     *dtaara       define    §datiute      dDATIUTE
179300050208     c                   in(E)     *dtaara
179400050208     c                   if        %ERROR or RSUT = *blanks
179500050208     c                   clear                   Tibs34Ds
179600050208     c                   call      'TIBS34R'
179700050208     c                   parm                    Tibs34Ds
179800050208     c                   in        *dtaara
179900050208     c                   endif
180000050208      *
180100050208     c                   eval      TBLkut = 1
180200010413      ***
180300010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
180400010413      ***
180500010413      *
180600010413      * recupero la moneta corrente
180700010413     C                   CLEAR                   tibs02ds
180800010413      *
180900010413     C                   MOVEL     'L'           T02TLA
181000010413     C                   MOVEL     'C'           T02MOD
181100010413     C                   MOVEL     KNSIF         T02SIF
181200010413     C                   MOVEL     'GED'         T02COD
181300010905     C                   MOVEL(P)  'DANNI'       t02ke1
181400010413      *
181500010413     C                   CALL      'TIBS02R'
181600010413     C                   PARM                    KPJBA
181700010413     C                   PARM                    tibs02ds
181800010413      *
181900010910     c                   movel     T02UNI        DGEDDN
182000011026      *
182100011026      * ...ed i relativi dati
182200011026     c                   movel     §gedDBA       W003a
182300011026     C                   exsr      CTRVLT
182400011026      *
182500010413      * recupero i valori nella moneta corrente
182600010413     C                   CLEAR                   tibs02ds
182700010413      *
182800010413     C                   MOVEL     'L'           T02TLA
182900010413     C                   MOVEL     'C'           T02MOD
183000010413     C                   MOVEL     KNSIF         T02SIF
183100010413     C                   MOVEL     'GEI'         T02COD
183200010910     C                   MOVEL     §GEDDBA       T02KE1
183300010413      *
183400010413     C                   CALL      'TIBS02R'
183500010413     C                   PARM                    KPJBA
183600010413     C                   PARM                    tibs02ds
183700010413      *
183800010413     c                   movel     t02uni        DGEI
183900010413      * VALORIZZO il comapo del controvalore CMR
184000010413     c                   z-add     §geccm        W§GEcmr
184100010905      * Valorizzo il valore del limite risarcibile
184200010417     c                   z-add     §gelrp        W§GElrp
184300010413      *
184400990204      ***
184500990204      * Aggancio tabella con dati consuldanni
184600990204      ***
184700010905     c                   clear                   TIBS02DS
184800010905     C                   MOVEL     'C'           t02mod
184900010905     C                   MOVEL     knsif         t02sif
185000010905     C                   MOVEL     'STD'         t02cod
185100010905     C                   MOVEL(P)  '1'           t02ke1
185200010910     C                   MOVEL(P)  §gedDBA       t02ke2
185300010905      *
185400010905     C                   CALL      'TIBS02R'
185500010905     C                   PARM                    KPJBA
185600010905     C                   PARM                    TIBS02DS
185700010905      *
185800010905     C                   clear                   DSTD
185900010905     C                   If        t02err = *BLANKS
186000010905     C                   MOVEL     T02UNI        DSTD
186100010905     C                   Endif
186200130920     C                   clear                   WLBDAT
186300130920     C                   eval      G02inv = §STDgld
186400130920     C                   eval      G02err = '3'
186500130920     C                   call      'XSRDA8'
186600130920     C                   parm                    WLBDAT
186700130920     C                   z-add     G02inv        wDataGLD
186800981009
186900980611      * ORA
187000980611     C                   TIME                    W0140
187100980619      * UDATE IN GGMMAAAA
187200980611     C                   MOVE      W0140         WDTGIO
187300050714      * ORA
187400050714     C                   MOVEL     W0140         WORA
187500980619      * UDATE IN AAAAMMGG
187600971016     C                   Z-ADD     WDTGIO        G02DAT
187700971016     C                   MOVEL     *BLANK        G02ERR
187800971016     C                   CALL      'XSRDA8'
187900971016     C                   PARM                    WLBDAT
188000980611     C                   MOVEL     G02INV        DATEU
188100980623     C     *iso          MOVEL     DATEU         Wdataoggi
188200980625
188300990203      * Imposto la data del giorno nella data stampa
188400990203     C                   z-add     WDTGIO        V1Cdds
188500990203
188600971020     C     KTAB          KLIST
188700050208     C                   KFLD                    TBLkut
188800971020     C                   KFLD                    KCOD
188900971020     C                   KFLD                    KKEY
189000980625      *
189100990108     c     kbol          klist
189200980515     c                   kfld                    i00aas
189300980515     c                   kfld                    i00lnp
189400980515     c                   kfld                    i00nrs
189500980515     c                   kfld                    i00nsp
189600050519      *
189700050519     C     Ktas4         klist
189800050519     C                   kfld                    DCTaas
189900050519     C                   kfld                    DCTlnp
190000050519     C                   kfld                    DCTnrs
190100050519     C                   kfld                    DCTnsp
190200050519     C                   kfld                    TA4trc
190300050519     c                   eval      TA4trc = 'A'
190400980625      *
190500971021     C     KDET          KLIST
190600990108     C                   KFLD                    DCTaae
190700990108     C                   KFLD                    DCTnev
190800980625      *
190900971021     C     KDCS          KLIST
191000990202     C                   KFLD                    kTPD
191100990202     C                   KFLD                    kNKT
191200990202     C                   KFLD                    kSTD
191300990202     C                   KFLD                    kDIM
191400990202     C                   KFLD                    kHIM
191500990202     C                   KFLD                    kNKS
191600990202     C                   KFLD                    kTRC
191700050714      * completa
191800050714     c     kdcsc         klist
191900050714     c                   kfld                    ktpd
192000050714     c                   kfld                    knkt
192100050714     c                   kfld                    kstd
192200050714     c                   kfld                    kdim
192300050714     c                   kfld                    khim
192400050714     c                   kfld                    knks
192500050714     c                   kfld                    ktrc
192600050714     c                   kfld                    prgnot
192700980625      *
192800980520     c     knumca        klist
192900980520     c                   kfld                    i00aac
193000980520     c                   kfld                    i00fil
193100980520     c                   kfld                    i00nca
193200981020      *
193300981020     c     KDKA          klist
193400981020     c                   kfld                    i00aac
193500981020     c                   kfld                    i00fil
193600981020     c                   kfld                    i00nca
193700981020     c                   kfld                    Wtrc
193800981224      *
193900951009     C                   ENDSR
194000050201      *---------------------------------------------------------------*
194100981014** MSG  L78                                                                  *
194200990203Immetere Data Stampa corretta.                                                 1
194300150924La data emissione non può essere precedente al                                 2
194400990203Inserire la modalità di calcolo.                                               3
194500040223ATTENZIONE: manca la tabella Polizza avvertire l'ufficio ASS di SEDE    !!     4
194600990203Inserire l'importo da liquidare.                                               5
194700010914Non ammessi importi con decimali per la divisa indicata                        6
194800010914Ammessi importi con massimo __ decimali per la divisa indicata                 7
194900020222Il Mass.Risarcib. è > del limite per Pratiche in Franchigia. ENTER per forzare 8
195000131014La C.A. è legata ad evento fortuito paga A.I.G. - ENTER forzare                9
195100131014Importo da liquidare > limite gestirebbe A.I.G. - ENTER forzare                10
195200130920C.A. legata evento non fortuito gestirebbe SETRAS - ENTER forzare              11
195300130920Importo da liquidare < limite gestirebbe SETRAS - ENTER forzare                12
195400990204** VLR  L78                                                          *
195500990203 a Kg merce perduta o danneggiata (legge 450/85).                              1  Legge 450
195600990126 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo          2  Elevazione
195700990126 in base all'assicurazione stipulata sul valore merce.                         3  Valore in bolla
195800990126 a Kg merce perduta o danneggiata (C.M.R.).                                    4  CMR
195900990204 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo          5  Mandato a Kg
196000990204 a collo in base al Vostro mandato assicurativo                                6  Mandato a collo
196100990204 a spedizione in base al Vostro mandato assicurativo                           7  Mandato a sped.
196200060316 a Kg merce perduta o danneggiata (Art 1696 c.c. mod. Dlgs 286/2005)           8  Decreto legge 286
196300060316 a Kg merce perduta o danneggiata                                              9  rcv transitoria
196400050222** SLR  L56                                            *------------>*
196500990204 a Kg merce perduta o danneggiata (legge 450/85).
196600050222 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo
196700990204 In base all'assicurazione stipulata sul valore merce.
196800990204 a Kg merce perduta o danneggiata (C.M.R.).
196900050222 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo
197000990204 a collo in base al Vostro mandato assicurativo
197100990204 a spedizione in base al Vostro mandato assicurativo
197200060316 a Kg merce perduta o danneggiata (Art 1696 c.c. mod. Dlgs 286/2005)
197300060316 a Kg merce perduta o danneggiata
197400120502** SLR2 L78                                            *------------>*
197500120502 per each kilo of missing or damaged goods (lgs decree 450/85)
197600120507 per each kilo of missing or damaged goods, according to the insurance writ
197700120507 according to the insurance writ, based to value of goods
197800120502 per each kilo of missing or damaged goods (C.M.R.)
197900120507 per each kilo of missing or damaged goods, according to the insurance writ
198000120507 per each parcel, according to the insurance writ
198100120507 according to the insurance writ, each shipment
198200120507 per each kilo of missing or damaged goods (art 1696 m.lgs d.286/2005)
198300120502 per each kilo of missing or damaged goods
