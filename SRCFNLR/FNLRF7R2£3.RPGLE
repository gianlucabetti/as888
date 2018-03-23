000100950525     H DECEDIT('0,') DATEDIT(*DMY.)
000200950525      *---------*----------------------------------------------------*
000300030707      * FNLRF7R2*                                                    *
000400950525      *---------*                                                    *
000500950525      *         - PREPARAZIONE DISTINTA DI CONSEGNA -                *
000600030703      *         --    DA SPUNTE (solo per poste)   --                *
000700950525      *--------------------------------------------------------------*
000800030805     FFIar501L  IF   E           K DISK
000900950526     FFNARBY1L  UF   E           K DISK
001000950526     FFNARBZ0F  UF   E           K DISK
001100030721     FFNARB01L  UF   E           K DISK    commit
001200990204     FFNDCD02L  IF   E           K DISK
001300040827     Fazorg01l  IF   E           K DISK
001400990204     FFNDCT01L  IF   E           K DISK
001500051115     FFIAR901L  IF   E           K DISK
001600950606     FFNLBL01L  IF   E           K DISK
001700950615     FFNLBL02L  IF   E           K DISK
001800950615     F                                     RENAME(FNLBL000:FNLBL002)
001900950525     FFNART01L  IF   E           K DISK
002000021204     Ffiapd01L  IF   E           K DISK
002100950605     FFNFVV01L  IF   E           K DISK
002200070129     FFNBRV01L  IF   E           K DISK
002300070129     FFNBRV09L  IF   E           K DISK
002400070129     F                                     RENAME(FNBRV000:FNBRV009)
002500030721     FFNANM02L  UF A E           K DISK    commit
002600100517     FTABEL00F  IF   E           K DISK
002700101124     FTNTBE01L  IF   E           K DISK
002800131218      *---
002900131218      * x non allocare legge col logico e aggiorna/scrive col Fisico
003000131218     FFNEVB04L  IF   E           K DISK    RENAME(FNEVB000:FNEVB004)
003100131218     F                                     infds(evbds)
003200131218     FFNEVB00F  UF A E             DISK    commit
003300131218      *---
003400130326     FwFNEVB01L UF   E           K DISK
003500990803     F                                     RENAME(FNEVB000:FNEVBPRV)
003600070129     FFNBRVE1L  IF   E           K DISK
003700070129     F                                     RENAME(FNBRV000:FNBRVE01)
003800070129     F                                     prefix(BRE:3)
003900070129     FFNBRVE2L  IF   E           K DISK
004000070129     F                                     RENAME(FNBRV000:FNBRVE02)
004100070129     F                                     prefix(BRE:3)
004200110518     FFiDST01L  IF   E           K DISK
004300030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
004400950612     F                                     USROPN
004500980325     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
004600101124     Fwfnlrq11l uf a E           k DISK
004700101124     Fwfnlrq20f o    E             DISK
004800950525      *--------------------------------------------------------------*
004900950525      * Schiere
005000950525      *--------------------------------------------------------------*
005100950526     D RIC             S              1    DIM(18)
005200950526     D DCR             S              1    DIM(8)
005300950525     D CDA             S              2    DIM(10)
005400950525     D C4H             S              2    DIM(100)
005500950525     D D4H             S             70    DIM(100)
005600950525     D T4H             S              1    DIM(100)
005700950908     D S4H             S              1    DIM(100)
005800990204     D TAD             S              2    DIM(100)
005900990204     D DTA             S            256    DIM(100)
006000040419     D ERR             S             90    DIM(13) CTDATA PERRCD(1)
006100990204     D TPC             S             29    DIM(2) CTDATA PERRCD(1)
006200950605     D DER             S              1    DIM(90)
006300010702     D COS             S              1    DIM(33) CTDATA PERRCD(33)
006400010702     D W33             S              1    DIM(33)
006500010712     D W06             S              1    DIM(6)
006600030703     c* poste
006700040827     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
006800950525      *--------------------------------------------------------------*
006900950525      *   DS
007000950525      *--------------------------------------------------------------*
007100131218     d evbds           ds
007200131218     d  evb_nrr              397    400b 0
007300131218      *
007400990204     D DTAD          E DS
007500990204     D  DDESC        E                     EXTFLD(§TADDESC)
007600990204     D  DRAGR        E                     EXTFLD(§TADRAGR)
007700990204     D  DCDNC        E                     EXTFLD(§TADCDNC)
007800990204     D  DCTLT        E                     EXTFLD(§TADCTLT)
007900990204     D  DCTLS        E                     EXTFLD(§TADCTLS)
008000990204     D  DDAGG        E                     EXTFLD(§TADDAGG)
008100990204     D  DMTVD        E                     EXTFLD(§TADMTVD)
008200990204     D  DDCDN        E                     EXTFLD(§TADDCDN)
008300990204     D  DTICO        E                     EXTFLD(§TADTICO)
008400990204     D  DFDDT        E                     EXTFLD(§TADFDDT)
008500990204     D  DFMED        E                     EXTFLD(§TADFMED)
008600990204     D  DPZDO        E                     EXTFLD(§TADPZDO)
008700990204     D  DKGDO        E                     EXTFLD(§TADKGDO)
008800990204     D  DEVFO        E                     EXTFLD(§TADEVFO)
008900990204     D  DIMPR        E                     EXTFLD(§TADIMPR)
009000990204     D  DTISI        E                     EXTFLD(§TADTISI)
009100990204     D  DDEST        E                     EXTFLD(§TADDEST)
009200990204     D  DRITR        E                     EXTFLD(§TADRITR)
009300990519     D  DDESI        E                     EXTFLD(§TADDESI)
009400990519     D  DCARE        E                     EXTFLD(§TADCARE)
009500990519     D  DCIDD        E                     EXTFLD(§TADCIDD)
009600101124     D wfnlrq1ds     e ds                  extname(wfnlrq10F)
009700101124     D wfnlrq2ds     e ds                  extname(wfnlrq20F)
009800101124     D dsqa          e ds
009900950525     D WLBDA8          DS                  INZ
010000950525     D  G02DAT                 1      8  0
010100950525     D  G02INV                 9     16  0
010200950525     D  G02ERR                17     17
010300950525     D  G02TGI                18     22  0
010400950525     D KPJBA         E DS
010500040721     D Danmflo       e ds
010600040721     D Dar5gen       e ds
010700040721     D DSARB         e ds                  extname(FNARB00F)
010800031009     D DSARBS        e ds                  extname(FNARB00F) PREFIX(S_)
010900031013     D DSEVB         e ds                  extname(FNEVB00F)
011000031013     D DSEVBS        e ds                  extname(FNEVB00F) PREFIX(S_)
011100070129     D DSbrv         e ds                  extname(Fnbrv00F)
011200070129     D DSbrvs        e ds                  extname(fnbrv00F) PREFIX(S_)
011300070129     D DSbrvc        e ds                  extname(fnbrv00F) PREFIX(c_)
011400040409     D DSfvv         e ds                  extname(fnfvv00F)
011500040409     D DSfvvc        e ds                  extname(fnfvv00F) prefix(c_)
011600030722     D ddatiute      e ds
011700031009     D azuteds       e ds                  extname(AZUTE00F)
011800030722     D tibs34ds      E DS                  inz
011900031006     D fnlr99ds      E DS                  inz
012000081014     D fnlrv7ds      e ds                  inz
012100110118     D fnls46ds      e ds                  inz
012200950525     D FNLR59        E DS                  EXTNAME(FNLR59DS)
012300950526     D DS4H          E DS
012400040827     D OG143         E DS
012500070129     D DSBRVE        E DS                  EXTNAME(FNBRVE0F)
012600070129     d                                     prefix(BRE:3)
012700070129     D* BREATB       E                     EXTFLD(BRVATB)
012800070129     D* BREFGS       E                     EXTFLD(BRVFGS)
012900070129     D* BRENPG       E                     EXTFLD(BRVNPG)
013000070129     D* BRENFV       E                     EXTFLD(BRVNFV)
013100070129     D* BREPES       E                     EXTFLD(BRVPES)
013200070129     D* BRELNP       E                     EXTFLD(BRVLNP)
013300070129     D* BRELNA       E                     EXTFLD(BRVLNA)
013400070129     D* BRENRS       E                     EXTFLD(BRVNRS)
013500070129     D* BRENSC       E                     EXTFLD(BRVNSC)
013600070129     D* BREZNC       E                     EXTFLD(BRVZNC)
013700070129     D* BRENPS       E                     EXTFLD(BRVNPS)
013800070129     D* BRECAN       E                     EXTFLD(BRVCAN)
013900070129     D* BREVUC       E                     EXTFLD(BRVVUC)
014000070129     D* BREPUC       E                     EXTFLD(BRVPUC)
014100070129     D* BREDCS       E                     EXTFLD(BRVDCS)
014200070129     D* BREHCS       E                     EXTFLD(BRVHCS)
014300070129     D* BREDFS       E                     EXTFLD(BRVDFS)
014400070129     D* BREHMS       E                     EXTFLD(BRVHMS)
014500070129     D* BREATR       E                     EXTFLD(BRVATR)
014600070129     D* BREFTR       E                     EXTFLD(BRVFTR)
014700070129     D* BREDTR       E                     EXTFLD(BRVDTR)
014800070129     D* BREHTR       E                     EXTFLD(BRVHTR)
014900070129     D* BREFLE       E                     EXTFLD(BRVFLE)
015000010620     D DSLV53        E DS                  EXTNAME(FNLV53DS)
015100010620     D DS7A2         E DS
015200100514     D DS7r          E DS
015300030805     D dar5ban       E DS                  inz
015400030805     D dar5bnb       E DS                  inz
015500010620     D* DS PER XDTORA  - DIFFERENZA FRA DUE DATA/ORA
015600010620     D WXDTOR          DS
015700010620     D  WXDT1                  1      8  0
015800010620     D  WXHM1                  9     12  0
015900010620     D  WXDT2                 13     20  0
016000010620     D  WXHM2                 21     24  0
016100010620     D  WXFLG                 25     25
016200010620     D  WXMIN                 26     30  0
016300960507     D DSFASE          DS
016400960507     D  WTLA                   1      1
016500960507     D  WNPG                   2      2
016600960507     D  WSPG                   3      3
016700960507     D  WFASE                  4      4
016800131104     c*
016900131104     d dEVB01        e ds
017000131104     d dEVB01sav       s                   like(dEVB01)
017100140430     d s_kpjbu         s                   like(kpjbu)
017200131104     c*
017300031029     D DSxmic          DS
017400031029     D  xdtmic                        8  0
017500031029     D  xoramic                       4  0
017600031029     D  xdtwrtmic                     8  0
017700031029     D  xorawrtmic                    6  0
017800031029     D DSxnic          DS
017900031029     D  xdtnic                        8  0
018000031029     D  xoranic                       4  0
018100031029     D  xdtwrtnic                     8  0
018200031029     D  xorawrtnic                    6  0
018300950526     D*--------------------------------------------------------------*
018400950526     D*  Costanti
018500950526     D COST3           C                   CONST('DOPO IL')
018600950526     D COST4           C                   CONST('IL')
018700030723     D CMAT            C                   CONST('MATTINO   ')
018800030723     D CPOM            C                   CONST('POMERIGGIO')
018900030805     d nban            S              5  0
019000040419     d sfgs            s                   like(fvvfgs)
019100040419     d snpg            s                   like(fvvnpg)
019200040419     d snfv            s                   like(fvvnfv)
019300110117     d sdfv            s                   like(fvvdfv)
019400040419     d shms            s                   like(brvhms)
019500950525      *--------------------------------------------------------------*
019600950525      * Ridenomino i campi dei codici anomalie in schiere
019700950525      *--------------------------------------------------------------*
019800950525     IFNARBY00
019900950525     I              ARBA01                      CDA(1)
020000950525     I              ARBA02                      CDA(2)
020100950525     I              ARBA03                      CDA(3)
020200950525     I              ARBA04                      CDA(4)
020300950525     I              ARBA05                      CDA(5)
020400950525     I              ARBA06                      CDA(6)
020500950525     I              ARBA07                      CDA(7)
020600950525     I              ARBA08                      CDA(8)
020700950525     I              ARBA09                      CDA(9)
020800950525     I              ARBA10                      CDA(10)
020900010702      *--------------------------------------------------------------*
021000010702      * Ridenomino tutti i campi DEL FILE FIBRVE
021100010702      *--------------------------------------------------------------*
021200070129     I*FIBRVE01
021300070129     I*             BRVATB                      BREATB
021400070129     I*             BRVFGS                      BREFGS
021500070129     I*             BRVNPG                      BRENPG
021600070129     I*             BRVNFV                      BRENFV
021700070129     I*             BRVPES                      BREPES
021800070129     I*             BRVLNP                      BRELNP
021900070129     I*             BRVLNA                      BRELNA
022000070129     I*             BRVNRS                      BRENRS
022100070129     I*             BRVNSC                      BRENSC
022200070129     I*             BRVZNC                      BREZNC
022300070129     I*             BRVNPS                      BRENPS
022400070129     I*             BRVCAN                      BRECAN
022500070129     I*             BRVVUC                      BREVUC
022600070129     I*             BRVPUC                      BREPUC
022700070129     I*             BRVDCS                      BREDCS
022800070129     I*             BRVHCS                      BREHCS
022900070129     I*             BRVDFS                      BREDFS
023000070129     I*             BRVHMS                      BREHMS
023100070129     I*             BRVATR                      BREATR
023200070129     I*             BRVFTR                      BREFTR
023300070129     I*             BRVDTR                      BREDTR
023400070129     I*             BRVHTR                      BREHTR
023500070129     I*             BRVFLE                      BREFLE
023600070129     I*FIBRVE02
023700070129     I*             BRVATB                      BREATB
023800070129     I*             BRVFGS                      BREFGS
023900070129     I*             BRVNPG                      BRENPG
024000070129     I*             BRVNFV                      BRENFV
024100070129     I*             BRVPES                      BREPES
024200070129     I*             BRVLNP                      BRELNP
024300070129     I*             BRVLNA                      BRELNA
024400070129     I*             BRVNRS                      BRENRS
024500070129     I*             BRVNSC                      BRENSC
024600070129     I*             BRVZNC                      BREZNC
024700070129     I*             BRVNPS                      BRENPS
024800070129     I*             BRVCAN                      BRECAN
024900070129     I*             BRVVUC                      BREVUC
025000070129     I*             BRVPUC                      BREPUC
025100070129     I*             BRVDCS                      BREDCS
025200070129     I*             BRVHCS                      BREHCS
025300070129     I*             BRVDFS                      BREDFS
025400070129     I*             BRVHMS                      BREHMS
025500070129     I*             BRVATR                      BREATR
025600070129     I*             BRVFTR                      BREFTR
025700070129     I*             BRVDTR                      BREDTR
025800070129     I*             BRVHTR                      BREHTR
025900070129     I*             BRVFLE                      BREFLE
026000950525     C*--------------------------------------------------------------*
026100950525     C*  Ciclo principale
026200950525     C*--------------------------------------------------------------*
026300950526     C     *ENTRY        PLIST
026400950526     C                   PARM                    KPJBA
026500030721     C                   MOVEL(p)  KPJBU         FNLR59
026600030805     c                   clear                   nban
026700050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
026800050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
026900050204     C* AUTOTRASPORTATORE
027000050204     C                   IF        D59STP = 'A'
027100050204     C                   SETON                                        61
027200050204     C                   SETOFF                                       62
027300050204     C                   ELSE
027400050204     C                   SETOFF                                       61
027500050204     C                   SETON                                        62
027600050204     C                   END
027700040930     c*
027800040930     c                   movel(p)  'FNLRF7R'     nompgm           10
027900030721     C*  A fine elaborazione leggo archivio eventi e scrivo
028000030721     C*  NIC in definitivo
028100030721     C     D59TPR        IFeq      'U'
028200030721     C                   EXSR      WRTNIC
028300030721     C                   MOVEL     'C'           D53TLA
028400030721     C                   CALL      'FNLV53R'
028500030721     C                   PARM                    DSLV53
028600140430     c
028700140430     c                   eval      s_kpjbu=kpjbu
028800140430     c                   clear                   fnlrv7ds
028900140430     C                   MOVEL     'C'           ILRV7TLA
029000140430     c                   movel(p)  fnlrv7ds      kpjbu
029100140430     c                   call      'FNLRV7R'
029200140430     c                   parm                    kpjba
029300140430     c                   eval      kpjbu=s_kpjbu
029400140430     c
029500030721     C                   SETON                                        LR
029600030721     c                   return
029700030721     c                   end
029800030729     C* spengo indicatore utilizzati nella routine di stampa
029900030729     C* spunte doppie.
030000030729     C                   SETOFF                                       2021
030100950526     C*  Valorizzo campi testata
030200950526     C                   EXSR      STPTES
030300040825     C     D59NDT        IFNE      EXNDT
030400040825     C                   Z-ADD     D59NDT        EXNDT
030500040825     C                   END
030600031009     C*  Leggo FNARBY dove ho memorizzato le spedizioni della distinta
030700031009     c*  con o senza errori
030800950529     C     *LOVAL        SETLL     FNARBY1L
030900031009     c                   do        *hival
031000031009     C                   READ      FNARBY1L
031100031009     c                   if        %eof(fnarby1l)
031200031009     c                   leave
031300031009     c                   end
031400950525     C                   EXSR      GESSPE
031500950525     C                   DELETE    FNARBY1L
031600030805     C                   ENDdo
031700030825     C*  Controllo se numero bancali caricati è maggiore a quello
031800110518     C*  in FIQDT
031900030825     C     nban          IFgt      D59NBL
032000030805     C                   Z-ADD     1             Y                 3 0
032100030805     C     '16'          LOOKUP    C4H(Y)                                 32
032200031016     C*  Controllo se si deve stampare l'aomalia
032300030805     C     S4H(Y)        IFNE      'N'
032400031016     C                   MOVEL     D4H(Y)        DErrq
032500110414     C                   CLEAR                   Terrq
032600031016     C                   Z-ADD     nban          WNRB1
032700031016     C                   Z-ADD     D59NBL        WNRB2
032800030805     C                   MOVEL     'S'           D59ANM
032900101201     c                   if        d59sqa =  ' ' or
033000101201     c                             d59sqa =  'T'
033100101125     c                   exsr      states
033200030805     C                   write     ERRQUA
033300101125     c                   end
033400030805     C                   ADD       2             D59LIN
033500030805     C                   END
033600030805     C                   END
033700010620     C*  Routine di stampa spunte consegna doppie
033800101201     c                   if        d59sqa =  ' ' or
033900101201     c                             d59sqa =  'T'
034000010620     C                   EXSR      DOPPIE
034100101125     c                   end
034200950526     C*  Loop x eventuale cancellazione FNARBZ
034300950526     C                   EXSR      DLTARZ
034400030721     C                   MOVEL(p)  FNLR59        KPJBU
034500950525     C                   SETON                                        RT
034600950525     C*--------------------------------------------------------------*
034700950526     C*  GESSPE: Gestione spedizione
034800950525     C*--------------------------------------------------------------*
034900950526     C     GESSPE        BEGSR
035000950525     C*  Cerco la spedizione su FNARB
035100950525     C                   Z-ADD     ARBAAS        KAAS
035200950525     C                   Z-ADD     ARBLNP        KLNP
035300950525     C                   Z-ADD     ARBNRS        KNRS
035400950525     C                   Z-ADD     ARBNSP        KNSP
035500950525     C     KARB          CHAIN     FNARB01L                           33
035600030825     c                   if        not *in33
035700031009     c* mi salvo la spedizione
035800031009     C                   MOVEL     DSARB         DSARBS
035900030805     C* CALCOLO IL NUMERO DEI BANCALI
036000030805     c                   exsr      srnban
036100031009     c* IMPOSTA I CAMPI IN STAMPA
036200110414     C                   MOVEL     ARBTSP        TIPERR
036300031009     C                   Z-ADD     ARBAAS        WAAS
036400031009     C                   Z-ADD     ARBLNP        WLNP
036500031009     C                   Z-ADD     ARBNRS        WNRS
036600031009     C                   Z-ADD     ARBNSP        WNSP
036700030731     C                   Z-ADD     ARBlna        Wlna
036800031009     C                   MOVEL     ARBRSD        WRSD
036900031009     C                   Z-ADD     ARBNCD        WNCD
037000031009     C                   Z-ADD     ARBNCA        WNCA
037100031009     C                   Z-ADD     ARBZNC        WZNC
037200030805     C                   movel     ARBmgs        Wcommes           2 0
037300030805     C                   move      ARBmgs        Wcomgg            2 0
037400031009     c                   movel     wcomgg        wdat
037500030805     c                   move      wcommes       wdat
037600031009     C* segnacolli non consecutivi
037700031009     C     ARBFNS        COMP      'S'                                    44
037800031009     C                   IF        NOT *IN44
037900031009     C                   MOVEL     '/'           WBAR
038000031009     C                   ELSE
038100031009     C                   CLEAR                   WBAR
038200031009     C                   END
038300030825     C                   END
038400031009     C* LOOP SU ERRORI
038500950525     C                   MOVEL     'N'           WERRBL            1
038600950525     C                   MOVEL     'N'           WNODIS            1
038700950525     C                   Z-ADD     1             X                 3 0
038800010702     C     X             DOWLE     10
038900950525     C     CDA(X)        ANDNE     *BLANKS
039000950530     C                   MOVEL     CDA(X)        WCDANO            2
039100950525     C                   EXSR      GESANO
039200950530     C                   ADD       1             X
039300031009     C                   ENDdo
039400950525     C*  Controllo se adesso è già stata messa in consegna
039500950529     C     ARBNDC        IFNE      *ZEROS
039600950530     C     ARBDCM        ANDEQ     0
039700950529     C     ARBNDC        ANDNE     D59NDT
039800950525     C     WNODIS        ANDEQ     'N'
039900031009     C                   MOVEL     '03'          WCDANO
040000950525     C                   EXSR      GESANO
040100040713     c* rendo bloccante l'errore anche se in tabella è stato deciso di
040200040713     c* non stamparlo
040300040713     C                   MOVEL     'S'           WERRBL
040400950525     C                   END
040500070530     c* METTE IN DISTINTA LA SPEDIZIONE se pgm "spunte"
040600040827     c                   movel     arbaas        arbdsp            8 0
040700040827     c                   move      arbmgs        arbdsp
040800031016     C* Se non ho trovato errore bloccante aggiorno la distinta
040900030721     C     WERRBL        IFEQ      'N'
041000070601     c                   if        arbngd=0 and arbndc = 0 and
041100070530     c                             (kcdaz='LR59' or kcdaz='LR60')
041200020917     C                   Z-ADD     D59FGS        ARBIFP
041300020917     C                   Z-ADD     D59NDT        ARBNDC
041400950525     C                   Z-ADD     D59DDT        ARBDDC
041500950525     C                   Z-ADD     D59PDR        ARBPDC
041600140410     C                   movel     *blanks       ARBfdc
041700140410     c                   z-add     0             arbnmi
041800140410     c                   z-add     0             arbnss
041900951121     C*  Attribuisco lo stop
042000951211     C     ARBKCS        IFNE      0
042100951211     C                   MOVEL     *BLANKS       KRAG
042200951211     C                   MOVEL     ARBKCS        KRAG
042300951211     C                   ELSE
042400951121     C                   MOVEL     ARBRSD        KRAG
042500951122     C                   MOVE      ARBIND        KRAG
042600951211     C                   END
042700951121     C     KRAG          CHAIN     FNARBZ0F                           32
042800950526     C*  Aggiorno la  bolla
042900950526     C                   EXCEPT    AGGARB
043000031013     C                   MOVEL     DSARB         DSARBS
043100031020     C                   END
043200031009     C*  DETTAGLIO SEGNACOLLI
043300980330     C                   Z-ADD     ARBAAS        KAAS
043400980330     C                   Z-ADD     ARBLNP        KLNP
043500980330     C                   Z-ADD     ARBNRS        KNRS
043600980330     C                   Z-ADD     ARBNSP        KNSP
043700950614     C                   MOVEL     'N'           WERRPZ            1
043800031009     C                   MOVEL     'N'           OKMIC             1
043900950614     C     KART          CHAIN     FNART01L                           32
044000030721     C*  Se non esiste e spedizione figlia mi posiziono su mamma
044100950614     C     *IN32         IFEQ      '1'
044200950606     C     KLBL          CHAIN     FNLBL01L                           32
044300950606     C     *IN32         IFEQ      '0'
044400950606     C     LBLLAP        ANDEQ     LBLLAN
044500950606     C                   Z-ADD     LBLAAP        KAAS
044600950606     C                   Z-ADD     LBLLPP        KLNP
044700950606     C                   Z-ADD     LBLNRP        KNRS
044800950606     C                   Z-ADD     LBLNSP        KNSP
044900950606     C     KARB          CHAIN     FNARB01L                           32
045000980430     C     ARBCCA        IFNE      '2'
045100980430     C     ARBCCA        ANDNE     '6'
045200950614     C     KART          CHAIN     FNART01L                           32
045300980430     C                   ELSE
045400980430     C                   Z-ADD     LBLAAN        KAAS
045500980430     C                   Z-ADD     LBLLPN        KLNP
045600980430     C                   Z-ADD     LBLNRN        KNRS
045700980430     C                   Z-ADD     LBLNSN        KNSP
045800980430     C     KARB          CHAIN     FNARB01L                           32
045900950614     C                   END
046000980430     C                   END
046100950614     C                   END
046200950614     C*  Loop su dettaglio segnacolli per verifcare se la bolla
046300950614     C*  è consegnata parzialmente su altra distinta
046400950526     C     *IN32         DOWEQ     '0'
046500000705     C                   EXSR      GESDET
046600031009     C                   ENDdo
046700070604     c* SCRIVO EVENTO MIC E ORM CONTESTUALE ALLA CONSEGNA
046800031020     c                   movel     dsarbs        dsarb
046900070530     c                   if        okmic = 'S'
047000100514     c                   clear                   ds7r
047100100514     c                   if        arbgma <> ' '
047200100514     c                   exsr      sr7r
047300100514     c                   end
047400100514     c                   if        §7RRC ='S'
047500031020     c                   clear                   fnlr99ds
047600071002     c                   eval      i99nftl= 99999
047700071002     c                   eval      i99fao = 400
047800031009     c                   exsr      aggorm
047900031020     c                   end
048000031020     c                   exsr      aggeve
048100070601     c*                  end
048200070530     c                   end
048300950526     C*
048400951109     C                   ELSE
048500030721     C  N33              unlock    fnarb01l
048600950526     C                   END
048700950526     C*
048800950526     C                   ENDSR
048900031003     C*--------------------------------------------------------------*
049000031003     C*  GESDET: Gestione dettaglio segnacolli
049100031003     C*--------------------------------------------------------------*
049200031003     C     GESDET        BEGSR
049300950526     C*
049400040409     C                   SETOFF                                       02
049500031009     C                   MOVEL     *BLANKS       WTIPCA
049600031009     C                   MOVEL     *BLANKS       WDETER
049700950530     C                   MOVEL     *BLANKS       WNRP              2
049800031009     C                   Z-ADD     0             WNRP1             2 0
049900950614     C                   MOVEL     'N'           WTRDAT            1
050000950614     C                   MOVEL     'N'           WTRDA1            1
050100950918     C                   MOVEL     'N'           WTRSPU            1
050200950526     C*  Controllo se non presente su distinta
050300950526     C                   Z-ADD     4             KNPG
050400950526     C                   Z-ADD     D59NDT        KNFV
050500950526     C                   Z-ADD     D59FGS        KFGS
050600950526     C                   Z-ADD     ARBLNA        KLNA
050700950526     C                   Z-ADD     ARBFLS        KFLS
050800950526     C                   Z-ADD     ARBNRS        KNRS
050900950526     C                   Z-ADD     ARTNSC        KNSC
051000000706      *
051100070129     C     KBRV1         CHAIN     FNBRV01L                           33
051200040408     c* NON HO LA SPUNTA 4
051300030729     C     *IN33         IFEQ      '1'
051400040421     c* solo se la spedizione è in distinta
051500040507     c                   if        s_arbndc  <> 0
051600990204     C* Controllo Dati Danni
051700040420     C*                  EXSR      TESDAN
051800950526     C*  Se non è presente su questa distinta verifico se è
051900950526     C*  in consegna su di un'altra
052000040408     c                   exsr      srfibrv
052100040408     c                   movel     okspu         wtrdat
052200030801     C*    se su altra distinta
052300030729     C     WTRDAT        IFEQ      'S'
052400950526     C     BRVNFV        ANDNE     0
052500040409     C                   MOVEL(p)  ERR(1)        WDETER
052600101124     c                   movel     '001'         tbeke2
052700040419     c                   movel     fvvnfv        com05
052800040419     c                   eval      %subst(wdeter: 51: 5)= com05
052900040409     c                   eval      %subst(wdeter: 71: 20)= fvvdsf
053000101126     c                   eval      sfgs=fvvfgs
053100101126     c                   eval      snpg=fvvnpg
053200101126     c                   eval      snfv=fvvnfv
053300110117     c                   eval      sdfv=fvvdfv
053400101126     c                   eval      shms=brvhms
053500950614     C                   MOVEL     BRVNPS        WNRP
053600950526     C                   EXSR      STAMPA
053700960507     C*  Richiamo TRUL11 per reperimento della fase
053800960507     C                   CLEAR                   DSFASE
053900960507     C                   MOVEL     '4'           WNPG
054000960507     C                   CALL      'TRUL11R'
054100960507     C                   PARM                    DSFASE
054200950919     C                   MOVEL     'S'           WTRSPU
054300950918     C                   Z-ADD     150           WCAA
054400950918     C                   Z-ADD     BRVNFV        WNFV
054500950918     C                   Z-ADD     BRVNPS        WNPS
054600960507     C                   Z-ADD     FVVFGS        WFGS
054700950918     C                   EXSR      WRTANM
054800030729     C                   END
054900950614     C*  Controllo se collo spuntato in I.M.A.
055000950614     C                   Z-ADD     3             KNPG
055100040408     c                   exsr      srfibrv
055200040408     c                   movel     okspu         wtrda1
055300040408     C*
055400950918     C                   MOVEL     *BLANKS       WDETER
055500950919     C                   SETOFF                                       02
055600950614     C                   Z-ADD     4             KNPG
055700030801     c*    se in inventario IMA
055800030729     C     WTRDA1        IFEQ      'S'
055900950614     C     BRVNFV        ANDNE     0
056000950614     C                   MOVEL     BRVNPS        WNRP
056100030801     c*    ...e su altra distinta
056200950919     C     WTRSPU        IFEQ      'S'
056300960507     C     FVVFCF        IFNE      'S'
056400040421     C                   MOVEL(p)  ERR(8)        WDETER
056500101124     c                   movel     '008'         tbeke2
056600040419     c                   eval      sfgs=fvvfgs
056700040419     c                   eval      snpg=fvvnpg
056800040419     c                   eval      snfv=fvvnfv
056900110117     c                   eval      sdfv=fvvdfv
057000040419     c                   eval      shms=brvhms
057100960507     C                   END
057200950919     C                   ELSE
057300030801     c*    ...no su altra distinta
057400040415     C                   MOVEL(p)  ERR(9)        WDETER
057500101124     c                   movel     '009'         tbeke2
057600040419     c                   eval      sfgs=fvvfgs
057700040419     c                   eval      snpg=fvvnpg
057800040419     c                   eval      snfv=fvvnfv
057900110117     c                   eval      sdfv=fvvdfv
058000040419     c                   eval      shms=brvhms
058100950919     C                   END
058200030801     c*
058300960507     C     WTRSPU        IFNE      'S'
058400960507     C     FVVFCF        ORNE      'S'
058500950614     C                   EXSR      STAMPA
058600960507     C*  Richiamo TRUL11 per reperimento della fase
058700960507     C                   CLEAR                   DSFASE
058800960507     C                   MOVEL     '3'           WNPG
058900960507     C                   MOVEL     'A'           WSPG
059000960507     C                   CALL      'TRUL11R'
059100960507     C                   PARM                    DSFASE
059200950919     C                   MOVEL     'S'           WTRSPU
059300950918     C                   Z-ADD     135           WCAA
059400950920     C                   Z-ADD     BRVNFV        WNFV
059500950918     C                   Z-ADD     WNRP1         WNPS
059600960507     C                   Z-ADD     FVVFGS        WFGS
059700950915     C                   EXSR      WRTANM
059800960507     C                   END
059900030729     C                   END
060000950918     C     WTRSPU        IFEQ      'S'
060100950918     C                   GOTO      NEWSGN
060200950918     C                   END
060300950526     C*  Controllo se il collo è arrivato
060400040419     C*    ARTDAM        IFNE      0
060500040419     C*    ARTDCM        ANDEQ     0
060600040419     C*    WDAVM         ANDEQ     'N'
060700040419     C*                  MOVEL(p)  ERR(2)        WDETER
060800040419     C*                  MOVEL     ARTNAP        WNRP
060900040419     C*                  EXSR      STAMPA
061000040419     C*                  GOTO      NEWSGN
061100040419     C*                  END
061200950526     C*  Collo partito e non arrivato
061300040419     C*    ARTDFV        IFNE      0
061400040419     C*    ARTDCM        ANDEQ     0
061500040419     C*    WDAVM         ANDEQ     'N'
061600040419     C*                  MOVEL(p)  ERR(3)        WDETER
061700040419     C*                  MOVEL     ARTNPP        WNRP
061800040419     C*                  EXSR      STAMPA
061900040419     C*                  GOTO      NEWSGN
062000040419     C*                  END
062100950526     C*  Collo non arrivato e non partito
062200040420     C*    ARTDCM        IFEQ      0
062300040420     C*    WDAVM         ANDEQ     'N'
062400040420     C*                  MOVEL(p)  ERR(5)        WDETER
062500040420     C*                  EXSR      STAMPA
062600040420     C*                  GOTO      NEWSGN
062700040420     C*                  END
062800950526     C*
062900040421     C                   end
063000030729     C                   ELSE
063100081103     c*
063200081103     C                   MOVEL     BRVNPS        WNRP
063300081103     C                   Z-ADD     BRVNPS        WNRP1
063400081103     C                   eval      dsbrvs = dsbrv
063500040408     c* HO LA SPUNTA 4
063600081014     c* per prima memorizzo segnazione di probabile etichetta doppia
063700081014     c                   clear                   fnlrv7ds
063800081014     c                   eval      ILRV7FLS=kfls
063900081014     c                   eval      ILRV7LNA=klna
064000081014     c                   eval      ILRV7NRS=knrs
064100081014     c                   eval      ILRV7NSC=knsc
064200081014     c                   movel(p)  fnlrv7ds      kpjbu
064300081014     c                   call      'FNLRV7R'
064400081014     c                   parm                    kpjba
064500081014     c                   movel     kpjbu         fnlrv7ds
064600081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
064700090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
064800081014     C                   MOVEL(p)  ERR(10)       WDETER
064900101124     c                   movel     '010'         tbeke2
065000101126     c                   eval      shms=s_brvhms
065100101126     c                   eval      sfgs=s_brvfgs
065200101126     c                   eval      snpg=s_brvnpg
065300110117     c                   eval      snfv=s_brvnfv
065400110117     c                   eval      sdfv=d59ddt
065500081014     C                   EXSR      STAMPA
065600081014     C                   END
065700040408     C* Controllo se collo spuntato anche in I.M.A.
065800040408     C                   Z-ADD     3             KNPG
065900040408     c                   exsr      srfibrv
066000040408     c                   movel     okspu         wtrda1
066100040408     C                   Z-ADD     4             KNPG
066200110118     c**********************************************************************
066300110118     c* se sono chiamato dalla sistemazione controllo aut, non segnalo
066400110118     c* l'errore e tento di cancellare la spunta + vecchia tra la 3 e la 4
066500110120     c                   if        kcdaz = 'LRQ1' and wtrda1 = 'S'
066600110118     c                   exsr      srls46
066700110118     c* se no errore ed effettuato l'aggiornamento proseguo
066800110118     c                   if        OLS46ERR = ' ' and
066900110118     c                             OLS46ANN = 'S'
067000110118     c* se cancellato la 3 e resta la 4 verifico se bolla in distinta ok
067100110118     c* segnalo errore
067200110118     c                   if        s_brvhms > brvhms
067300110118     c                   if        s_arbndc = 0
067400110118     c                   eval      wtrda1 = 'N'
067500110118     c                   clear                   dsbrv
067600110118     c                   clear                   dsfvv
067700110118     c                   else
067800110118     C                   GOTO      NEWSGN
067900110118     c                   end
068000110118     c                   else
068100110118     c* se cancellato la 4 e resta la 3 verifico se bolla in distinta errore
068200110118     c* altrimenti ok
068300110118     c                   if        s_arbndc = 0
068400110118     C                   GOTO      NEWSGN
068500110118     c                   else
068600110118     C                   MOVEL(p)  ERR(9)        WDETER
068700110118     c                   movel     '009'         tbeke2
068800110118     c                   eval      sfgs=fvvfgs
068900110118     c                   eval      snpg=fvvnpg
069000110118     c                   eval      snfv=fvvnfv
069100110118     c                   eval      sdfv=fvvdfv
069200110118     c                   eval      shms=brvhms
069300110118     C                   EXSR      STAMPA
069400110118     C                   GOTO      NEWSGN
069500110118     c                   end
069600110118     c                   end
069700110118     c                   end
069800110118     c                   end
069900110118     c**********************************************************************
070000040415     C* Se ho SOLO la spunta 4 ma la spedizione non è in distinta
070100040408     C* segnalo l'errore
070200110118     C                   IF        s_arbndc = 0
070300040408     C                   IF        wtrda1 = 'N'
070400040415     C                   MOVEL(p)  ERR(12)       WDETER
070500101124     c                   movel     '012'         tbeke2
070600101126     c                   eval      sfgs=s_brvfgs
070700101126     c                   eval      snpg=s_brvnpg
070800101126     c                   eval      snfv=s_brvnfv
070900040419     c                   eval      shms=s_brvhms
071000110117     c                   eval      sdfv=d59ddt
071100040408     C                   EXSR      STAMPA
071200040408     C                   GOTO      NEWSGN
071300040408     C* Se ho la spunta 4 e la spunta 3 ma la spedizione non è in distinta
071400040408     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
071500040408     C* spunta 4 > di ora spunta 3
071600040408     c                   else
071700110118     c
071800040409     c                   if        s_brvnps <> brvnps or
071900040409     c                             (s_brvnps = brvnps and s_brvhms > brvhms)
072000101124     C                   MOVEL(p)  ERR(12)       WDETER
072100101124     c                   movel     '012'         tbeke2
072200040419     c                   eval      shms=s_brvhms
072300101126     c                   eval      sfgs=s_brvfgs
072400101126     c                   eval      snpg=s_brvnpg
072500101126     c                   eval      snfv=s_brvnfv
072600101126     c                   eval      shms=s_brvhms
072700110117     c                   eval      sdfv=d59ddt
072800040415     C                   EXSR      STAMPA
072900040419     C                   MOVEL(p)  ERR(8)        WDETER
073000101124     c                   movel     '008'         tbeke2
073100040419     c                   eval      sfgs=fvvfgs
073200040419     c                   eval      snpg=fvvnpg
073300040419     c                   eval      snfv=fvvnfv
073400110117     c                   eval      sdfv=fvvdfv
073500040419     c                   eval      shms=brvhms
073600040419     c                   movel     brvnps        wnrp
073700040415     c                   seton                                        02
073800040409     C                   EXSR      STAMPA
073900040415     c                   setoff                                       02
074000040415     C                   GOTO      NEWSGN
074100040415     c                   else
074200040408     C                   GOTO      NEWSGN
074300040408     c                   end
074400040408     C                   END
074500040409     C                   END
074600040408     c*
074700031013     c                   eval      okmic = 'S'
074800031009     C* Se è stato spuntato con la distinta in esame controllo
074900031009     C* se segnacollo già consegnato
075000030729     C     ARTDCM        IFNE      0
075100040415     C                   MOVEL(P)  ERR(6)        WDETER
075200101124     c                   movel     '006'         tbeke2
075300040419     c                   eval      shms=s_brvhms
075400101126     c                   eval      sfgs=s_brvfgs
075500101126     c                   eval      snpg=s_brvnpg
075600101126     c                   eval      snfv=s_brvnfv
075700110117     c                   eval      sdfv=d59ddt
075800950526     C                   EXSR      STAMPA
075900950614     C                   GOTO      NEWSGN
076000030729     C                   END
076100040421     C* Se ho la spunta 4 e la spunta 3 e la spedizione è in distinta
076200040421     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
076300040421     C* spunta 3 > di ora spunta 4
076400040421     c                   if        s_brvnps <> brvnps or
076500040421     c                             (s_brvnps = brvnps and s_brvhms < brvhms)
076600030729     C     WTRDA1        IFEQ      'S'
076700040408     C     BRVNFV        ANDNE     0
076800960507     C     FVVFCF        IFNE      'S'
076900040415     C                   MOVEL(P)  ERR(7)        WDETER
077000101124     c                   movel     '007'         tbeke2
077100040419     c                   eval      shms=s_brvhms
077200101126     c                   eval      sfgs=s_brvfgs
077300101126     c                   eval      snpg=s_brvnpg
077400101126     c                   eval      snfv=s_brvnfv
077500110117     c                   eval      sdfv=d59ddt
077600040409     C                   EXSR      STAMPA
077700040415     C                   MOVEL(P)  ERR(8)        Wdeter
077800101124     c                   movel     '008'         tbeke2
077900040419     c                   eval      sfgs=fvvfgs
078000040419     c                   eval      snpg=fvvnpg
078100040419     c                   eval      snfv=fvvnfv
078200110117     c                   eval      sdfv=fvvdfv
078300040419     c                   eval      shms=brvhms
078400040419     c                   movel     brvnps        wnrp
078500040415     c                   seton                                        02
078600950612     C                   EXSR      STAMPA
078700040415     c                   setoff                                       02
078800960507     C*  Richiamo TRUL11 per reperimento della fase
078900960507     C                   CLEAR                   DSFASE
079000960507     C                   MOVEL     '3'           WNPG
079100960507     C                   MOVEL     'A'           WSPG
079200960507     C                   CALL      'TRUL11R'
079300960507     C                   PARM                    DSFASE
079400950918     C                   Z-ADD     135           WCAA
079500950920     C                   Z-ADD     BRVNFV        WNFV
079600950918     C                   Z-ADD     WNRP1         WNPS
079700960507     C                   Z-ADD     FVVFGS        WFGS
079800950915     C                   EXSR      WRTANM
079900031009     C                   GOTO      NEWSGN
080000030729     C                   END
080100030729     C                   END
080200040421     C                   END
080300950526     C*
080400030729     C                   END
080500950526     C*  Leggo successivo segnacollo
080600950614     C     NEWSGN        TAG
080700950526     C     KART          READE     FNART01L                               32
080800950526     C*
080900950526     C                   ENDSR
081000031009     C*--------------------------------------------------------------*
081100040408     C*  srfibrv cerco spunta nel foglio 4/3 nel gg. della distinta che
081200040408     c*          sto quadrando
081300031009     C*--------------------------------------------------------------*
081400040408     C     srfibrv       BEGSR
081500040408     c                   movel     'N'           okspu             1
081600040409     c                   clear                   dsbrvc
081700040409     c                   clear                   dsfvvc
081800031009     C*
081900070129     C     KBRV2         SETll     FNBRV09L
082000040408     c                   do        *hival
082100070129     C     KBRV2         READe     FNBRV09L
082200070129     c                   if        %eof(fnbrv09l)
082300040408     c                   leave
082400040408     c                   end
082500040408     C                   Z-ADD     BRVNFV        KNFV1
082600040408     C     KFVV          CHAIN     FNFVV01L
082700040408     c                   if        %found(fnfvv01l) and fvvdfv = d59ddt
082800040408     C                   MOVEL     'S'           okspu
082900040409     c                   if        brvhms > c_brvhms
083000040409     c                   eval      dsbrvc = dsbrv
083100040409     c                   eval      dsfvvc = dsfvv
083200040408     c                   end
083300040409     c                   end
083400040408     C                   ENDDO
083500040409     c*
083600040409     c                   eval      dsbrv = dsbrvc
083700040409     c                   eval      dsfvv = dsfvvc
083800040408     C*
083900040408     C                   ENDSR
084000040408     C*--------------------------------------------------------------*
084100040408     C*  AGGEVE: scrivo evento MIC sia per distinte manuali che x quelle
084200040408     c*          da spunte
084300040408     C*--------------------------------------------------------------*
084400040408     C     AGGEVE        BEGSR
084500040408     C*
084600131104     C                   clear                   dEVB01
084700131104     C                   move      *zeros        §NOTNDC
084800131104     C                   move      *zeros        §NOTPDC
084900131104     C                   move      ARBPDC        §NOTPDC
085000131104     C                   move      ARBNDC        §NOTNDC
085100131104     C                   MOVEL     Devb01        dEVB01sav
085200031009     C* Nel momento in cui metto in consegna la bolla controllo se
085300031009     C* ho scritto evento NIC provvisorio nell'archivio eventi della
085400031009     C* QTEMP e nel caso lo cancello
085500031009     C                   Z-ADD     ARBAAS        KAAS
085600031009     C                   Z-ADD     ARBLNP        KLNP
085700031009     C                   Z-ADD     ARBNRS        KNRS
085800031009     C                   Z-ADD     ARBNSP        KNSP
085900031009     C     KLBL          CHAIN     FNLBL01L                           32
086000031009     C     *IN32         IFEQ      '0'
086100031009     C     LBLLAP        ANDEQ     LBLLAN
086200031009     C                   Z-ADD     LBLAAP        KAAS
086300031009     C                   Z-ADD     LBLLPP        KLNP
086400031009     C                   Z-ADD     LBLNRP        KNRS
086500031009     C                   Z-ADD     LBLNSP        KNSP
086600031009     C     KARB          CHAIN(N)  FNARB01L                           32
086700031009     C     ARBCCA        IFEQ      '2'
086800031009     C     ARBCCA        OREQ      '6'
086900031009     C                   Z-ADD     LBLAAN        KAAS
087000031009     C                   Z-ADD     LBLLPN        KLNP
087100031009     C                   Z-ADD     LBLNRN        KNRS
087200031009     C                   Z-ADD     LBLNSN        KNSP
087300031009     C     KARB          CHAIN(N)  FNARB01L                           32
087400031009     C                   SETON                                        32
087500031009     C                   END
087600031009     C                   END
087700031009     C     *IN32         IFEQ      '0'
087800031009     C     LBLLAP        ANDEQ     LBLLAN
087900031009     C                   Z-ADD     LBLAAP        KAAS
088000031009     C                   Z-ADD     LBLLPP        KLNP
088100031009     C                   Z-ADD     LBLNRP        KNRS
088200031009     C                   Z-ADD     LBLNSP        KNSP
088300031009     C                   ELSE
088400031009     C                   Z-ADD     ARBAAS        KAAS
088500031009     C                   Z-ADD     ARBLNP        KLNP
088600031009     C                   Z-ADD     ARBNRS        KNRS
088700031009     C                   Z-ADD     ARBNSP        KNSP
088800031009     C                   END
088900130326     C     KEVB1         CHAIN     WFNEVB01L                          36
089000031009     C     EVBCEV        IFEQ      'NIC'
089100130326     C  N36              DELETE    WFNEVB01L
089200031009     C                   ELSE
089300031009     C  N36              EXCEPT    SPED1
089400031009     C                   END
089500131218      **
089600031009     C* Aggiorno evento di messa in consegna
089700031009     C                   MOVEL     'MIC'         KCEV
089800031009     C     KEVB2         SETGT     FNEVB04L
089900031009     C     KEVB2         READPE    FNEVB04L                               37
090000131218      **
090100031009     C     *IN37         IFEQ      '1'
090200031009     C     EVBDEV        ORNE      D59DDT
090300131218      **
090400031009     C                   MOVEL     'MIC'         EVBCEV
090500031009     C                   Z-ADD     D59AMG        EVBDTV
090600031009     C                   TIME                    EVBORV
090700031009     C                   Z-ADD     D59FGS        EVBFLE
090800031009     C                   MOVE      D59CDU        EVBCDU
090900031009     C                   Z-ADD     KAAS          EVBAAS
091000031009     C                   Z-ADD     KLNP          EVBLNP
091100031009     C                   Z-ADD     KNRS          EVBNRS
091200031009     C                   Z-ADD     KNSP          EVBNSP
091300031009     C                   Z-ADD     D59DDT        EVBDEV
091400031009     C                   Z-ADD     0             EVBDTR
091500031009     C                   MOVEL     *BLANKS       EVBFTR
091600031009     C                   Z-ADD     0             EVBDTC
091700131104     C                   MOVEL     Devb01sav     EVBNOT
091800031009     C     D59FPP        IFEQ      'M'
091900031009     C                   MOVEL     0800          EVBHEV
092000031009     C                   ELSE
092100031009     C                   MOVEL     1200          EVBHEV
092200031009     C                   END
092300031009     C                   WRITE     FNEVB000
092400131218      **
092500031009     C                   ELSE
092600131218      **
092700031009     C     EVBCEV        IFEQ      'MIC'
092800031009     C     EVBDEV        ANDEQ     D59DDT
092900031009     C*
093000031009     C     EVBDTR        IFEQ      0
093100131218     C*
093200131218     C* riaggancia il record  FISICO x aggiornarlo
093300131218     c     evb_nrr       chain     fnevb00F
093400131218     C*
093500131218     c                   if        %Found(fnevb00F)
093600031009     C* Se non è stato trasmesso aggiorno la data e l'ora di
093700031009     C* di immissione evento con quella del giorno + altri dati
093800031009     C                   Z-ADD     D59AMG        EVBDTV
093900031009     C                   TIME                    EVBORV
094000031009     C                   Z-ADD     D59FGS        EVBFLE
094100031009     C                   MOVE      D59CDU        EVBCDU
094200131104     C                   MOVEL     Devb01sav     EVBNOT
094300031009     C     D59FPP        IFEQ      'M'
094400031009     C                   MOVEL     0800          EVBHEV
094500031009     C                   ELSE
094600031009     C                   MOVEL     1200          EVBHEV
094700031009     C                   END
094800031009     C                   MOVEL     *BLANKS       EVBDTC
094900031009     C                   UPDATE    FNEVB000
095000131218     c                   end
095100131218      **
095200031009     C                   ELSE
095300131218      **
095400031009     C* Se l'evento MIC esiste già ed è già stato trasmesso lo
095500031009     C* riscrivo solo se c'è un NIC successivo
095600031029     C                   MOVE      EVBDEV        xdtMIC
095700031029     C                   MOVE      EVBHEV        xoraMIC
095800031029     C                   MOVE      EVBdtv        xdtwrtMIC
095900031029     C                   MOVE      EVBorv        xorawrtMIC
096000131218      **
096100031009     C                   MOVEL     'NIC'         KCEV
096200031009     C     KEVB2         SETGT     FNEVB04L
096300031009     C     KEVB2         READPE    FNEVB04L                               37
096400131218      **
096500031009     C     *IN37         IFEQ      '0'
096600031009     C     EVBDEV        ANDEQ     D59DDT
096700131218      **
096800031029     C                   MOVE      EVBDEV        xdtNIC
096900031029     C                   MOVE      EVBHEV        xoraNIC
097000031029     C                   MOVE      EVBdtv        xdtwrtNIC
097100031029     C                   MOVE      EVBorv        xorawrtNIC
097200031029     C     dsxMIC        IFLE      dsxNIC
097300031009     C                   MOVEL     'MIC'         EVBCEV
097400031009     C                   Z-ADD     D59AMG        EVBDTV
097500031009     C                   TIME                    EVBORV
097600031009     C                   Z-ADD     D59FGS        EVBFLE
097700031009     C                   MOVE      D59CDU        EVBCDU
097800031009     C                   Z-ADD     KAAS          EVBAAS
097900031009     C                   Z-ADD     KLNP          EVBLNP
098000031009     C                   Z-ADD     KNRS          EVBNRS
098100031009     C                   Z-ADD     KNSP          EVBNSP
098200031009     C                   Z-ADD     D59DDT        EVBDEV
098300031009     C                   Z-ADD     0             EVBDTR
098400031009     C                   MOVEL     *BLANKS       EVBFTR
098500031009     C     D59FPP        IFEQ      'M'
098600031009     C                   MOVEL     0800          EVBHEV
098700031009     C                   ELSE
098800031009     C                   MOVEL     1200          EVBHEV
098900031009     C                   END
099000031009     C                   Z-ADD     0             EVBDTC
099100131104     C                   MOVEL     Devb01sav     EVBNOT
099200031009     C                   WRITE     FNEVB000
099300131218      **
099400031009     C                   END
099500031009     C*
099600031009     C                   END
099700031009     C                   END
099800031009     C                   END
099900031009     C                   END
100000031020     c* mi reimposto la ds di FNARB originale
100100031020     c                   movel     dsarbs        dsarb
100200031009     C*
100300031009     C                   ENDSR
100400031009     C*--------------------------------------------------------------*
100500100514     C*  sr7r  particolarità consegna
100600031009     C*--------------------------------------------------------------*
100700100514     C     sr7r          BEGSR
100800031009     c*
100900100514     C                   eval      kcod = '7R'
101000100514     C                   eval      kkey = arbgma
101100100514     c                   eval      kkut = 1
101200100514     C     ktab1         chain     Tabel00f
101300100514     c                   if        %found(tabel00f)
101400100514     C                   MOVEL     TBLUNI        DS7R
101500100514     c                   end
101600100514     C*
101700100514     C                   ENDSR
101800100514     C*--------------------------------------------------------------*
101900100514     C*  aggorm: Aggiorno ORM e sue fasi
102000100514     C*--------------------------------------------------------------*
102100100514     C     aggorm        BEGSR
102200100514     c*
102300031009     c                   eval      i99tla = 'L'
102400031020     c                   eval      i99aas = arbaas
102500031020     c                   eval      i99lnp = arblnp
102600031020     c                   eval      i99nrs = arbnrs
102700031020     c                   eval      i99nsp = arbnsp
102800031020     c* metto in distinta
102900071001     c                   eval      i99fgs = d59fgs
103000031020     c                   if        i99fao = 400
103100031020     c                   eval      i99ndc = arbndc
103200031020     c                   eval      i99ddc = arbddc
103300031009     c                   eval      i99npg = 4
103400031020     c                   end
103500031020     c                   eval      i99comit = '1'
103600071001     c                   eval      i99cmt = 'N'
103700031009     c                   call      'FNLR99R'
103800031009     c                   parm                    kpjba
103900031009     c                   parm                    fnlr99ds
104000031009     c*
104100031009     C                   ENDSR
104200110118     C*--------------------------------------------------------------*
104300110118     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
104400110118     c* dal controllo a video
104500110118     C*--------------------------------------------------------------*
104600110118     C     srls46        BEGSR
104700110118     c*
104800110118     c                   clear                   fnls46ds
104900110118     c                   clear                   kpjbu
105000110118     c                   eval      ILS46NPGA =  brvnpg
105100110118     c                   eval      ILS46NFVA =  brvnfv
105200110118     c                   eval      ILS46FGSA =  brvfgs
105300110120     c                   eval      ILS46NPGB =  s_brvnpg
105400110120     c                   eval      ILS46NFVB =  s_brvnfv
105500110120     c                   eval      ILS46FGSB =  s_brvfgs
105600110121     c                   eval      ILS46AMB =  'QD'
105700110121     c                   eval      ILS46aupr=  'SE'
105800110120     c                   eval      ILS46FLS =  Kfls
105900110118     c                   eval      ILS46LNA =  Klna
106000110118     c                   eval      ILS46NRS =  Knrs
106100110118     c                   eval      ILS46NSC =  Knsc
106200110120     c                   eval      ILS46TELA = 'M'
106300110118     c*
106400110118     c                   call      'FNLS46R'
106500110118     c                   parm                    kpjba
106600110118     c                   parm                    fnls46ds
106700110118     c*
106800110118     C                   ENDSR
106900990204     C*--------------------------------------------------------------*
107000990204     C*  TESDAN: Test gestione danni
107100990204     C*--------------------------------------------------------------*
107200990204     C     TESDAN        BEGSR
107300990204     C*
107400990204     C*  Flag pratica danno = 'N'
107500990204     C                   MOVE      'N'           WDAVM             1
107600990204     C*  Se il collo non è stato spuntato su quel foglio di
107700990204     C*  uscita controllo se c'è una pratica C.A. aperta
107800990204     C     KDCD          CHAIN     FNDCD02L                           38
107900990204     C*  Se esiste C.A. aperta controllo il tipo di anomalia
108000990204     C     *IN38         IFEQ      '0'
108100990204     C     DCDATB        ANDNE     'A'
108200990204     C     DCDDCH        IFEQ      0
108300990204     C                   MOVE      TPC(1)        WTIPCA
108400990204     C* Aggancio testata
108500990204     C                   MOVE      DCDAAC        KAAC
108600990204     C                   MOVE      DCDFIL        KFIL
108700990204     C                   MOVE      DCDNCA        KNCA
108800990204     C     KDCT          CHAIN     FNDCT01L                           38
108900990204     C     *IN38         DOWEQ     '0'
109000990204     C     DCTATB        ANDEQ     'A'
109100990204     C     *IN38         OREQ      '0'
109200990204     C     DCTDCH        ANDNE     0
109300990204     C     KDCT          READE     FNDCT01L                               38
109400990204     C                   END
109500990204     C                   Z-ADD     1             YY                4 0
109600990204     C     DCTTAD        LOOKUP    TAD(YY)                                39
109700990204     C     *IN39         IFEQ      '1'
109800990204     C                   MOVEL     DTA(YY)       DTAD
109900990204     C     DRAGR         IFEQ      'M'
110000990204     C     DRAGR         OREQ      'V'
110100031009     C                   MOVE      'S'           WDAVM
110200990204     C                   END                                                    Manc.o Ava D/R
110300990204     C                   END                                                    Trv TAD
110400990204     C*
110500990204     C                   ELSE
110600990204     C                   MOVE      TPC(2)        WTIPCA
110700990204     C                   END                                                    CA Aperta
110800990204     C                   END                                                    CA no annul
110900990204     C*
111000990204     C                   ENDSR
111100950525     C*--------------------------------------------------------------*
111200950525     C*  GESANO: Gestione anomalie
111300950525     C*--------------------------------------------------------------*
111400950525     C     GESANO        BEGSR
111500950525     C*  Se non ho trovato errore bloccante
111600950911     C                   SETOFF                                       031417
111700031009     C                   Z-ADD     1             Y
111800950526     C     WCDANO        LOOKUP    C4H(Y)                                 32
111900950525     C     *IN32         IFEQ      '1'
112000950908     C     S4H(Y)        ANDNE     'N'
112100950908     C                   MOVEL     'S'           D59ANM
112200031009     C                   MOVEL     D4H(Y)        DESERR
112300950525     C*  Trovato errore bloccante !!
112400950525     C     T4H(Y)        IFEQ      'B'
112500950525     C                   MOVEL     'S'           WERRBL
112600030801     C*  Se in consegna su altra distinta
112700950529     C     C4H(Y)        IFEQ      '03'
112800950525     C                   MOVEL     'S'           WNODIS
112900950526     C                   SETON                                        03
113000950606     C                   MOVEL     *BLANKS       APDRSC
113100021204     C     kapd          CHAIN     fiapd01L                           37
113200031009     C                   MOVEL     APDRSC        WPADRO
113300950525     C                   END
113400950526     C                   END
113500950911     C*  Se ho sbloccato il C/Assegno senalo importo e divisa
113600950911     C     C4H(Y)        IFEQ      '07'
113700950911     C                   SETON                                        17
113800950911     C                   Z-ADD     0             AR9CAS
113900950911     C                   MOVEL     *BLANKS       AR9VCA
114000051115     C     KARB          CHAIN     FIAR901L                           37
114100950911     C                   END
114200030801     C*  Decodificare la consegna richiesta
114300950529     C     C4H(Y)        IFEQ      '10'
114400950526     C                   MOVEA     *BLANKS       RIC
114500950526     C                   Z-ADD     ARBDCR        G02INV
114600950526     C                   MOVEL     '3'           G02ERR
114700950526     C                   CALL      'XSRDA8'
114800950526     C                   PARM                    WLBDA8
114900950526     C                   MOVE      G02DAT        WDCR              8
115000950526     C                   MOVEA     WDCR          DCR
115100950526     C     ARBTCR        IFEQ      'D'
115200950526     C                   MOVEA     COST3         RIC
115300950526     C                   MOVEL     DCR(1)        RIC(9)
115400950526     C                   MOVEL     DCR(2)        RIC(10)
115500950526     C                   MOVEL     '/'           RIC(11)
115600950526     C                   MOVEL     DCR(3)        RIC(12)
115700950526     C                   MOVEL     DCR(4)        RIC(13)
115800950526     C                   MOVEL     '/'           RIC(14)
115900950526     C                   MOVEA     DCR(5)        RIC(15)
116000950526     C                   ELSE
116100950526     C                   MOVEA     COST4         RIC
116200950526     C                   MOVEL     DCR(1)        RIC(4)
116300950526     C                   MOVEL     DCR(2)        RIC(5)
116400950526     C                   MOVEL     '/'           RIC(6)
116500950526     C                   MOVEL     DCR(3)        RIC(7)
116600950526     C                   MOVEL     DCR(4)        RIC(8)
116700950526     C                   MOVEL     '/'           RIC(9)
116800950526     C                   MOVEA     DCR(5)        RIC(10)
116900950526     C                   END
117000950526     C                   MOVEA     RIC           WRIC             18
117100950526     C                   MOVE      WRIC          DESERR
117200950525     C                   END
117300950615     C*  Se non sono riuscita ad andare in consegna con l'ultima figlia
117400950615     C*  segnalo l'errore.
117500950615     C     C4H(Y)        IFEQ      '14'
117600950615     C     KLBL          SETGT     FNLBL02L
117700950615     C     KLBL          READPE    FNLBL02L                               32
117800950615     C                   SETON                                        14
117900950615     C                   END
118000030725     c* stampa testata
118100110112     c                   if        d59sqa =  ' ' or
118200110112     c                             d59sqa =  'T'
118300030728     c                   exsr      states
118400101124     c* stampa
118500030728     C                   WRITE     ERRARB
118600101201     c                   end
118700101124     c* video
118800101201     c                   if        d59sqa =  'S' or
118900101201     c                             d59sqa =  'T'
119000101124     c                   EVAL      tbeke2 = 'H' + C4H(Y)
119100101124     c                   exsr      srsqa
119200101124     c                   end
119300950612     C                   ADD       1             D59LIN
119400110414     C                   CLEAR                   TIPERR
119500960319     C                   Z-ADD     0             WAAS
119600960319     C                   Z-ADD     0             WLNP
119700960319     C                   Z-ADD     0             WNRS
119800960319     C                   Z-ADD     0             WNSP
119900030728     C                   Z-ADD     0             Wlna
120000030728     C                   Z-ADD     0             Wdat
120100030729     C                   Z-ADD     0             WNCD
120200030729     C                   Z-ADD     0             WNCA
120300030729     C                   Z-ADD     0             WZNC
120400030729     C                   MOVEL     *BLANKS       WRSD
120500030729     C                   MOVEL     *BLANKS       WBAR
120600950526     C                   END
120700950526     C*
120800950525     C*
120900950525     C                   ENDSR
121000950526     C*--------------------------------------------------------------*
121100030725     C*  STAMPA: Stampa testata
121200950526     C*--------------------------------------------------------------*
121300030728     C     STATES        BEGSR
121400110112     c                   if        d59sqa =  ' ' or
121500110112     c                             d59sqa =  'T'
121600030728     C* SE LA RIGA DI STAMPA È MAGGIORE DI SEI VUOL DIRE CHE HO GIÀ STAMPATO
121700030728     C                   IF        D59LIN = 0 OR
121800030728     C                             D59LIN > 54
121900030728     C                   SETON                                        80
122000030729     c                   else
122100030729     c* stampo testatina spunte doppie a metà pagina
122200030729     c                   if        *in20 and *in21
122300030729     c                   write     testad
122400030729     c                   write     testad1
122500030729     C                   ADD       4             D59LIN
122600030729     c                   setoff                                       21
122700030728     C                   END
122800030729     C                   END
122900030725     C*
123000030728     C                   IF        *IN80
123100030725     C                   ADD       1             D59PAG
123200030728     C                   WRITE     TESTA
123300050502     c                   if        d59lsr = 'S' and d59pag = 1
123400050422     C                   write     testal
123500050426     c                   end
123600030729     C                   Z-ADD     6             D59LIN
123700030728     C                   SETOFF                                       80
123800030728     C* SPUNTE DOPPIE
123900030729     C                   IF        *IN20
124000040930     C                   WRITE     TESTAD2
124100040930     C                   WRITE     TESTAD1
124200030911     C                   Z-ADD     7             D59LIN
124300030911     C                   SETOFF                                       21
124400030729     C                   else
124500030729     C                   WRITE     TESTA1
124600030728     C                   END
124700030729     C                   END
124800110112     C                   END
124900030728     C*
125000030725     C                   ENDSR
125100030725     C*--------------------------------------------------------------*
125200030725     C*  STAMPA: Stampa dettaglio segnacolli
125300030725     C*--------------------------------------------------------------*
125400030725     C     STAMPA        BEGSR
125500101126     C*  Se non è stato stampato "ANOMALIE DA SPUNTE:" lo stampo
125600101201     C                   IF        werrpz = 'N' and
125700101201     c                             (d59sqa =  ' ' or
125800101201     c                             d59sqa =  'T')
125900950526     C                   MOVEL     '12'          WCDANO
126000950526     C                   MOVEL     'S'           WERRPZ
126100950526     C                   EXSR      GESANO
126200950526     C                   END
126300950908     C*----------------------------------------------------*
126400950908     C*  Controllo se devo stampare le anomalie
126500101126     C*    S4H(Y)        IFNE      'N'
126600950908     C*----------------------------------------------------*
126700030725     c*
126800030725     c                   exsr      states
126900040419     c* Imposto il numero della pistola nel campo di stampa
127000950530     C                   MOVEA     WDETER        DER
127100040409     C     '.xx '        SCAN      WDETER        XX                2 0    01
127200040409     c                   if        *in01
127300950530     C                   ADD       1             XX
127400950530     C                   MOVEA     WNRP          DER(XX)
127500950530     C                   MOVEA     DER           WDETER
127600950530     C                   END
127700040419     c* Imposto il numero del foglio nel campo di stampa
127800040419     C                   MOVEA     WDETER        DER
127900040419     C     'xxx-'        SCAN      WDETER        XX                2 0    01
128000040419     c                   if        *in01
128100040421     c                   movel(p)  sfgs          com04             4
128200040421     C                   MOVEA     com04         DER(XX)
128300040420     C                   ADD       4             XX
128400040419     c                   movel     snpg          com01             1
128500040419     C                   MOVEA     com01         DER(XX)
128600040420     C                   ADD       2             XX
128700040419     c                   movel     snfv          com05             5
128800040419     C                   MOVEA     com05         DER(XX)
128900040419     C                   MOVEA     DER           WDETER
129000040419     C                   END
129100110117     c* Imposto l'ora del foglio nel campo di stampa
129200040419     C                   MOVEA     WDETER        DER
129300040419     C     'xx:'         SCAN      WDETER        XX                2 0    01
129400040419     c                   if        *in01
129500040419     c                   movel     shms          com02             2
129600040419     C                   MOVEA     com02         DER(XX)
129700040420     C                   ADD       3             XX
129800070130     c                   move      shms          com04             4
129900070130     c                   movel     com04         com02
130000040419     C                   MOVEA     com02         DER(XX)
130100040419     C                   MOVEA     DER           WDETER
130200040419     C                   END
130300040419     c*
130400950612     C                   ADD       1             D59LIN
130500101125     c* stampa
130600101201     c                   if        (d59sqa =  ' ' or
130700101201     c                             d59sqa =  'T')
130800950919     C     WTRSPU        IFEQ      'S'
130900030728     C                   EVAL      WDETER1 = WDETER
131000030728     C                   EVAL      WTIPCA1 = WTIPCA
131100030728     C                   WRITE     ERRAR1
131200950919     C                   ELSE
131300030728     C                   WRITE     ERRART
131400101125     C                   END
131500101201     c                   end
131600101124     c* video
131700101201     c                   if        (d59sqa =  'S' or
131800101201     c                             d59sqa =  'T')
131900101124     c                   exsr      srsqa
132000101124     c                   end
132100950908     C*----------------------------------------------------*
132200101126     C*                  END
132300950908     C*----------------------------------------------------*
132400950526     C                   ENDSR
132500950526     C*--------------------------------------------------------------*
132600950526     C*  STPTES: Stampo dati testata
132700950526     C*--------------------------------------------------------------*
132800950526     C     STPTES        BEGSR
132900030728     C* AUTOTRASPORTATORE
133000031009     C                   Z-ADD     D59PDR        PRTPDR
133100031009     C                   MOVEL(P)  D59DSF        PRTDSF
133200050422     C                   MOVEL(P)  D59DSF        PRTDSF2
133300030728     C* DISTINTA
133400031009     C                   Z-ADD     D59NDT        PRTNDT
133500110209     C                   Z-ADD     D59NDT        PRTNDT3
133600110209     C*                  eval      PRTNDT2 =d59bcd
133700110209     C                   move      d59bcd        PRTNDT2
133800031009     C                   Z-ADD     D59FGS        PRTFGS
133900030728     C                   Z-ADD     D59DDT        G02INV
134000030728     C                   MOVEL     '3'           G02ERR
134100030728     C                   CALL      'XSRDA8'
134200030728     C                   PARM                    WLBDA8
134300031009     C                   Z-ADD     G02DAT        STCDDC
134400950526     C*
134500030723     C                   MOVEL     CMAT          TPPRE
134600030723     C     D59FPP        IFEQ      'P'
134700031009     C                   MOVEL     CPOM          TPPRE
134800030723     C                   END
134900030728     C*
135000950526     C*
135100950526     C                   ENDSR
135200101124     C*-----------------------------------------------------*
135300101124     C* memorizzo l'errore nel file di work                 *
135400101124     C*-----------------------------------------------------*
135500101124     C     srsqa         BEGSR
135600101124     c                   clear                   dsqa
135700101124     c                   eval      tbecod = 'SQA'
135800101124     c                   eval      tbeke1 = 'FNLRF7R2'
135900101124     c     ktbe          chain     tntbe01l
136000101124     c                   if        %found(tntbe01l)
136100101124     c                   movel     tbeuni        dsqa
136200101124     c                   if        D§SQAELA = 'S'
136300101124     c* TESTATA
136400101124     c                   clear                   wfnlrq1ds
136500101124     c                   eval      wlrq1ndc = d59ndt
136600101124     c                   eval      wlrq1fgs = d59fgs
136700101124     c                   eval      wlrq1pdr = d59pdr
136800101125     c                   eval      wlrq1aas = s_arbAAS
136900101125     c                   eval      wlrq1LNP = s_arbLNP
137000101125     c                   eval      wlrq1NRS = s_arbNRS
137100101125     c                   eval      wlrq1NSP = s_arbNSP
137200101124     c*
137300101124     c     kwf10         chain     wfnlrq11l
137400101124     c                   if        not %found(wfnlrq11l)
137500101124     c                   movel     tbeke2        wlrq1cer
137600101124     c                   eval      wlrq1sce = D§SQASCE
137700101126     c                   if        D§SQATPD = 'S'
137800101126     c                   eval      wlrq1err = D§SQADES
137900101126     c                   else
138000101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
138100101126     c                   eval      wlrq1err = deserr
138200101126     c                   else
138300101126     c                   eval      wlrq1err = wdeter
138400101126     c                   end
138500101126     c                   end
138600101124     c                   write     wfnlrq10
138700101124     c                   else
138800101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
138900101124     c                   clear                   wlrq1cer
139000101124     c                   clear                   wlrq1sce
139100101124     c                   update    wfnlrq10
139200101124     c                   end
139300101124     c* dettaglio
139400101124     c                   clear                   wfnlrq2ds
139500101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
139600101124     c                   eval      wlrq2aas = wlrq1aas
139700101124     c                   eval      wlrq2lnp = wlrq1lnp
139800101124     c                   eval      wlrq2nrs = wlrq1nrs
139900101124     c                   eval      wlrq2nsp = wlrq1nsp
140000101124     c                   eval      wlrq2ndc = wlrq1ndc
140100101124     c                   eval      wlrq2fgs = wlrq1fgs
140200101124     c                   eval      wlrq2pdr = wlrq1pdr
140300101124     c                   movel     tbeke2        wlrq2cer
140400101124     c                   eval      wlrq2sce = D§SQASCE
140500101126     c                   if        D§SQATPD = 'S'
140600101126     c                   eval      wlrq2err = D§SQADES
140700101126     c                   else
140800101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
140900101126     c                   eval      wlrq2err = deserr
141000101126     c                   else
141100101126     c                   eval      wlrq2err = wdeter
141200101126     c                   end
141300101126     c                   end
141400101125     c* gli errori nella tabella 4H sono a livello di bolla quindi
141500101126     c* non ho le informazioni del segnacollo
141600101125     c                   if        %subst(tbeke2: 1: 1) <> 'H'
141700101124     c                   eval      wlrq2nsc = artnsc
141800110104     c                   eval      wlrq2fls = artfls
141900110126     c                   eval      wlrq2nrc = artnrs
142000110131     c                   if        tbeke2 <> '010'
142100110104     c                   eval      wlrq2flv = sfgs
142200101125     c                   eval      wlrq2nfv = snfv
142300110119     c                   eval      wlrq2dfv = sdfv
142400101125     c                   eval      wlrq2npg = snpg
142500101126     c                   eval      wlrq2hms = shms
142600101126     c                   if        wnrp <> ' '
142700101126     c                   movel     wnrp          wlrq2nps
142800101126     c                   end
142900110131     c                   end
143000110207     c*                  else
143100110207     c*                  eval      wlrq2flv = s_arbifp
143200110207     c*                  eval      wlrq2nfv = s_arbndc
143300110207     c*                  eval      wlrq2dfv = s_arbddc
143400110207     c*                  if        wlrq2nfv <> 0
143500110207     c*                  eval      wlrq2npg = 4
143600110207     c*                  end
143700101126     c                   end
143800101125     c*
143900101124     c                   write     wfnlrq20
144000101124     c                   end
144100101124     c                   end
144200101124     C                   ENDSR
144300950526     C*--------------------------------------------------------------*
144400950526     C*  DLTARZ: Cancello archivio dett.segnacolli/stop
144500950526     C*--------------------------------------------------------------*
144600950526     C     DLTARZ        BEGSR
144700950526     C*
144800950526     C     *LOVAL        SETLL     FNARBZ0F
144900950526     C                   READ      FNARBZ0F                               31
145000950526     C     *IN31         DOWEQ     '0'
145100950526     C                   DELETE    FNARBZ0F
145200950526     C                   READ      FNARBZ0F                               31
145300950526     C                   END
145400950526     C*
145500950526     C                   ENDSR
145600950614     C*--------------------------------------------------------------*
145700950614     C*  WRTANM: Scrivo anomalia collo fuori posto
145800950614     C*--------------------------------------------------------------*
145900950915     C     WRTANM        BEGSR
146000950915     C*
146100950919     C     ARTDCM        IFEQ      0
146200950915     C                   MOVEL     'N'           WAGANO            1
146300950918     C                   Z-ADD     WCAA          KCAA
146400990519     C                   CLEAR                   FNANM000
146500990519     C     KANM          CHAIN     FNANM02L                           34
146600950915     C     *IN34         DOWEQ     '0'
146700990519     C     ANMDAO        IFEQ      D59DDT
146800031009     C                   MOVEL     'S'           WAGANO
146900950915     C     ANMDCH        IFNE      0
147000960507     C                   MOVEL     WFASE         ANMFSA
147100960507     C                   Z-ADD     WFGS          ANMCDU
147200960507     C                   Z-ADD     WNFV          ANMNFV
147300950918     C                   Z-ADD     WNPS          ANMNPS
147400950915     C                   Z-ADD     0             ANMDCH
147500950915     C                   MOVEL     *BLANKS       ANMFSC
147600950915     C                   MOVEL     *BLANKS       ANMCCH
147700950915     C                   EXCEPT    AGGANM
147800960429     C                   ELSE
147900030721     C                   unlock    fnanm02l
148000950915     C                   END
148100950915     C                   END
148200990519     C     KANM          READE     FNANM02L                               34
148300950915     C                   END
148400950915     C*
148500950915     C     WAGANO        IFEQ      'N'
148600990519     C                   CLEAR                   FNANM000
148700960507     C                   MOVEL     WFASE         ANMFSA
148800960507     C                   Z-ADD     WFGS          ANMCDU
148900950915     C                   Z-ADD     ARTFLS        ANMFLS
149000950915     C                   Z-ADD     ARTNRS        ANMNRS
149100950915     C                   Z-ADD     ARTLNA        ANMLNA
149200950915     C                   Z-ADD     ARTNSC        ANMSCN
149300950915     C                   Z-ADD     ARBAAS        ANMAAS
149400950915     C                   Z-ADD     ARBLNP        ANMLNP
149500950915     C                   Z-ADD     ARBNSP        ANMNSP
149600950918     C                   Z-ADD     WCAA          ANMCAA
149700950918     C                   Z-ADD     WNFV          ANMNFV
149800950918     C                   Z-ADD     WNPS          ANMNPS
149900950915     C                   Z-ADD     D59DDT        ANMDAO
150000950915     C                   Z-ADD     SIMFEL        ANMFLE
150100950915     C                   MOVEL     'F'           ANMTAN
150200950915     C                   MOVEL     'S'           ANMSAN
150300950915     C                   MOVEL     'I'           ANMAIE
150400040721     c                   eval      anmft4 = §AR5BVA
150500040721     c                   movel     woggi         com6              6
150600040721     c                   clear                   comsei            6
150700040721     c                   eval      comsei = %subst(com6: 5: 2)+
150800040721     c                             %subst(com6: 3: 2)+
150900040721     c                             %subst(com6: 1: 2)
151000040721     c                   movel     comsei        §ANMDAC
151100040721     c                   movel     times         §ANMORC
151200040721     c                   movel     danmflo       anmflo
151300040721     c                   eval      anmfl4 = arbznc
151400040721     c                   if        arbznc=0
151500040721     c                   eval      anmfl4 = 999
151600040721     c                   end
151700990519     C                   WRITE     FNANM000
151800950915     C                   END
151900950919     C*
152000950919     C                   END                                                    ARTDCM = 0
152100950614     C*
152200950915     C                   ENDSR
152300980310     C*--------------------------------------------------------------*
152400980310     C*  WRTNIC: Leggo NIC provvisori e li scrivo in definitivo
152500980310     C*--------------------------------------------------------------*
152600980310     C     WRTNIC        BEGSR
152700980310     C*
152800980310     C* Controllo quali sono gli eventi di esclusi dalla consegna che
152900980310     C* non sono stati cancellati e li riporto in definitivo
153000130326     C     *LOVAL        SETLL     WFNEVB01L
153100130326     C                   READ      WfNEVB01L                              36
153200980310     C     *IN36         DOWEQ     '0'
153300031013     C                   MOVEL     DSEVB         DSEVBS
153400980310     C* Aggancio file eventi per controllare se l'evento di messa in
153500980310     C* consegna sulla bolla era già stato trasmesso
153600990803     C                   Z-ADD     EVBAAS        KAAS
153700980310     C                   Z-ADD     EVBLNP        KLNP
153800980310     C                   Z-ADD     EVBNRS        KNRS
153900980310     C                   Z-ADD     EVBNSP        KNSP
154000031020     C* riaggancio la spedizione dell'evento NIC rimasto
154100031020     C     KARB          CHAIN     FNARB01L                           33
154200100514     c                   clear                   ds7r
154300100514     c                   if        arbgma <> ' '
154400100514     c                   exsr      sr7r
154500100514     c                   end
154600031020     c* se spedizione con ritiro contestuale alla consegna
154700100514     c                   if        §7RRC= 'S' and not *in33
154800031020     c                   clear                   fnlr99ds
154900031020     c                   eval      i99fao = 390
155000031020     c                   exsr      AGGORM
155100031020     c                   end
155200031020     c* proseguo con la scrittura del NIC/cancellazione del MIC
155300980310     C                   MOVEL     'MIC'         KCEV
155400990803     C     KEVB2         SETGT     FNEVB04L
155500990803     C     KEVB2         READPE    FNEVB04L                               37
155600131218     C*--
155700980310     C     *IN37         IFEQ      '0'
155800990803     C     EVBDEV        ANDEQ     D59DDT
155900131218     C*--
156000980310     C* se l'evento MIC non era stato trasmesso lo annullo fisicamente
156100980310     C     EVBDTR        IFEQ      0
156200131218     C*
156300131218     C* riaggancia il record  FISICO x aggiornarlo
156400131218     c     evb_nrr       delete    fnevb00F
156500131218     C*--
156600131218     C**************     DELETE    FNEVB04L
156700131218     C*
156800980310     C                   ELSE
156900131218     C*
157000031029     C                   MOVEL     EVBDEV        WDTMIC           12 0
157100031029     C                   MOVE      EVBHEV        WDTMIC
157200980312     C* altrimenri controllo se esiste già un evento NIC con data = a
157300980312     C* quella del foglio di consegna
157400990803     C                   Z-ADD     EVBAAS        KAAS
157500980312     C                   Z-ADD     EVBLNP        KLNP
157600980312     C                   Z-ADD     EVBNRS        KNRS
157700980312     C                   Z-ADD     EVBNSP        KNSP
157800980312     C                   MOVEL     'NIC'         KCEV
157900990803     C     KEVB2         SETGT     FNEVB04L
158000990803     C     KEVB2         READPE    FNEVB04L                               37
158100131218      *--
158200980312     C     *IN37         IFEQ      '0'
158300990803     C     EVBDEV        ANDEQ     D59DDT
158400131218      *--
158500980312     C* Se c'è evento NIC controllo se già trasmesso
158600980312     C     EVBDTR        IFEQ      0
158700131218     C*
158800131218     C* riaggancia il record  FISICO x aggiornarlo
158900131218     c     evb_nrr       chain     fnevb00F
159000131218      *--
159100131218     c                   if        %Found(fnevb00F)
159200980312     C* Se non è ancora stato trasmesso aggiorno data/ora immissione
159300980312     C                   Z-ADD     D59AMG        EVBDTV
159400980312     C                   TIME                    EVBORV
159500980312     C                   Z-ADD     D59FGS        EVBFLE
159600980312     C                   MOVE      D59CDU        EVBCDU
159700980312     C     FVVFPP        IFEQ      'M'
159800980312     C                   MOVEL     0800          EVBHEV
159900980312     C                   ELSE
160000980312     C                   MOVEL     1200          EVBHEV
160100980312     C                   END
160200990803     C                   Z-ADD     0             EVBDTC
160300131218      *--
160400990803     C                   UPDATE    FNEVB000
160500131218     c                   end
160600131218      *--
160700980312     C                   ELSE
160800131218      *--
160900980312     C* Se è già stato trasmesso controllo se data/ora immissione
161000980312     C* > MIC
161100031029     C                   MOVEL     EVBDEV        WDTNIC           12 0
161200031029     C                   MOVE      EVBHEV        WDTNIC
161300031029     C     WDTNIC        IFLE      WDTMIC
161400980312     C* Se la data non è più alta scrivo un nuovo NIC
161500031013     C                   MOVEL     DSEVBS        DSEVB
161600990803     C                   WRITE     FNEVB000
161700980312     C                   END                                                    Data MIC >= NIC
161800980312     C*
161900980312     C                   END                                                    NIC Trasmesso
162000980312     C*
162100980312     C                   ELSE                                                   Nic Trovato
162200031013     C                   MOVEL     DSEVBS        DSEVB
162300990803     C                   WRITE     FNEVB000
162400980312     C                   END                                                    Nic Trovato
162500980312     C                   END                                                    Mic trasmesso
162600980312     C                   END                                                    Mic Trovato
162700980310     C* Cancello evento provvisiorio
162800990803     C                   DELETE    FNEVBPRV
162900980310     C* leggo evento provvisiorio successivo
163000130326     C                   READ      WFNEVB01L                              36
163100980310     C                   END
163200980310     C*
163300980310     C                   ENDSR
163400030805     C**********************************************************************
163500030805     C* calcolo il numero dei bancali   -----------------------------*
163600030805     C**********************************************************************
163700030805     C     srnban        BEGSR
163800030805      * Bancali
163900030805     c                   If        %Subst(Arbgva:1:1) = 'B'
164000030805     c                   Clear                   dAr5Ban
164100030805     C                   move      'BAN'         ar5trd
164200030805     c     kFiar5        Chain     Fiar501l
164300030805     c                   If        %Found(Fiar501l)
164400030805     c                   Movel     Ar5Uni        dAr5Ban
164500030805     c                   EndIf
164600030805     c                   add       §Ar5Nba       nban
164700030805     c                   add       §ar5Nb2       nban
164800030805     c                   EndIf
164900030805      * Bancali Bartolini
165000030805     c                   If        %Subst(Arbgva:1:1) = 'O'
165100030805     c                   Clear                   dAr5BNB
165200030805     C                   move      'BNB'         ar5trd
165300030805     c     kFiar5        Chain     Fiar501l
165400030805     c                   If        %Found(Fiar501l)
165500030805     c                   Movel     Ar5Uni        dAr5BNB
165600030805     c                   EndIf
165700030805     c                   add       §Ar5bNba      nban
165800030805     c                   add       §ar5bNb2      nban
165900030805     c                   EndIf
166000040721      * Merce di valore
166100040721     c                   Clear                   dAr5gen
166200040721     C                   move      'GEN'         ar5trd
166300040721     c     kFiar5        Chain     Fiar501l
166400040721     c                   If        %Found(Fiar501l)
166500040721     c                   Movel     Ar5Uni        dAr5gen
166600040721     c                   EndIf
166700030805     c                   ENDSR
166800010620     C*--------------------------------------------------------------*
166900010620     C*  DOPPIE: STAMPO EVENTUALI SPUNTE DI CONSEGNA DOPPIE
167000010620     C*--------------------------------------------------------------*
167100010620     C     DOPPIE        BEGSR
167200010620     C*
167300010620     C                   Z-ADD     D59NDT        KNFV
167400010620     C                   Z-ADD     4             KNPG
167500030729     C                   SETON                                        2021
167600010620     C*
167700070129     C     KBRVE         SETLL     FNBRVE1L
167800070129     C     KBRVE         READE     FNBRVE1L                               30
167900010620    2C     *IN30         DOWEQ     *OFF
168000010620     C                   CLEAR                   WGIR
168100010620     C                   CLEAR                   SAWNPS
168200010622     C                   CLEAR                   SAWNFV
168300010622     C                   CLEAR                   SAWFGS
168400130327     C                   SETOFF                                       19
168500070129     C     KBRVE2        SETLL     FNBRVE2L
168600070129     C     KBRVE2        READE     FNBRVE2L                               31
168700010620    3C     *IN31         DOWEQ     *OFF
168800010620     C* Considero solo se data foglio >= data distinta in quadratura
168900010702    4C     D59FGS        IFNE      BREFGS
169000010702     C     KNFV          ORNE      BRENFV
169100010620     C                   CLEAR                   DSLV53
169200010620     C                   MOVEL     'V'           D53TFO
169300010620     C                   MOVEL     KNPG          D53NPG
169400010620     C                   Z-ADD     SIMFEL        D53FEL
169500010702     C                   Z-ADD     BRENFV        D53NFV
169600010702     C                   Z-ADD     BREFGS        D53FGS
169700010620     C                   CALL      'FNLV53R'
169800010620     C                   PARM                    DSLV53
169900010702     C                   Z-ADD     D53DFV        SA1DFV
170000010620     C                   ELSE
170100010702     C                   Z-ADD     D59DDT        SA1DFV
170200010620    4C                   ENDIF
170300010702    4C     D59FGS        IFEQ      BREFGS
170400010702     C     KNFV          ANDEQ     BRENFV
170500010620     C     D53ERR        OREQ      *BLANKS
170600010702     C     SA1DFV        ANDGE     D59DDT
170700010703    5C     SAWNFV        IFGT      *ZEROS
170800010622     C* Stessa pistola e foglio: devo verificare se superati i minuti
170900010622     c* di tolleranza previsti fra una spunta e l'altra
171000010620     C                   CLEAR                   WPRT
171100010702    6C     SAWNPS        IFEQ      BRENPS
171200010702     C     SAWNFV        ANDEQ     BRENFV
171300010702     C     SAWFGS        ANDEQ     BREFGS
171400010620     C                   CLEAR                   WXDTOR
171500010702     C                   MOVEL     BREDFS        WXDT1
171600010702     C                   MOVEL     BREHMS        WXHM1
171700010620     C                   MOVEL     SAWDFS        WXDT2
171800010620     C                   MOVEL     SAWHMS        WXHM2
171900010620     C                   MOVEL     'D'           WXFLG
172000010620     C                   CALL      'XDTORA'
172100010620     C                   PARM                    WXDTOR
172200010620     C     WXMIN         IFGT      §7AMNP
172300010620     C                   MOVEL     '1'           WPRT
172400010620     C                   ENDIF
172500010620   x6C                   ELSE
172600010622     c* se pistole o foglio diversi stampo sempre
172700010620     C                   MOVEL     '1'           WPRT              1
172800010620    6C                   ENDIF
172900010620    6C     WPRT          IFEQ      '1'
173000010620     c* se sono al primo giro di stampa devo stampare sia la spunta pre
173100010620     c* cedente che la spunta successiva
173200010620    7C     WGIR          IFEQ      ' '
173300010620     C                   MOVEL     DSBRVE        W1BRVE
173400010620     C                   MOVEL     WSBRVE        DSBRVE
173500010620     C                   Z-ADD     SAWDFV        WDFV
173600030729     C                   SETOff                                       19
173700010620     C                   EXSR      SPRDOP
173800010620     C                   MOVEL     W1BRVE        DSBRVE
173900010620     C                   MOVE      '1'           WGIR              1
174000010620    7C                   ENDIF
174100010702     C                   Z-ADD     SA1DFV        WDFV
174200030729     C                   SETON                                        19
174300010620     C                   EXSR      SPRDOP
174400010620   X6C                   ELSE
174500010620     C                   MOVEL     DSBRVE        WSBRVE
174600010620    6C                   ENDIF
174700010620   X5C                   ELSE
174800010620     C* se sono al primo giro di lettura memorizzo la spunta
174900010620     C                   MOVEL     DSBRVE        WSBRVE
175000010620    5C                   ENDIF
175100010620     C* memorizzo in ogni caso il numero pistola e data/ora fisica spun
175200010702     C                   Z-ADD     BRENPS        SAWNPS
175300010702     C                   Z-ADD     BREFGS        SAWFGS
175400010702     C                   Z-ADD     BRENFV        SAWNFV
175500010702     C                   Z-ADD     BREDFS        SAWDFS
175600010702     C                   Z-ADD     BREHMS        SAWHMS
175700010702     C                   Z-ADD     SA1DFV        SAWDFV
175800010620    4C                   ENDIF
175900070129     C     KBRVE2        READE     FNBRVE2L                               31
176000010620    3C                   ENDDO
176100010620     C* Mi posiziono sempre su un nuovo segnacollo
176200070129     C     KBRVE1        SETGT     FNBRVE1L
176300070129     C     KBRVE         READE     FNBRVE1L                               30
176400010620    2C                   ENDDO
176500010711     C*
176600010620     C                   ENDSR
176700010620     C*
176800010620     C*--------------------------------------------------------------*
176900010620     C* STAMPA SPUNTA DOPPIA
177000010620     C*--------------------------------------------------------------*
177100010620     C     SPRDOP        BEGSR
177200010712     C                   MOVEL     'S'           D59ANM
177300030728     C                   EXSR      STATES
177400010711     C*
177500010702     C                   CLEAR                   P1CMSG
177600030729     C* SEGNACOLLO SOLO PER 19 Off
177700030729     C     *IN19         IFEQ      *Off
177800010702     C                   Z-ADD     BRELNP        P1CLNP
177900010702     C                   Z-ADD     BRELNA        P1CLNA
178000010702     C                   Z-ADD     BRENRS        P1CNRS
178100010702     C                   Z-ADD     BRENSC        P1CNSC
178200010702     C* VEDO SE COLLO IN IMA
178300010702     C                   Z-ADD     3             KNPG
178400070129     C     KBRV9         SETGT     FNBRV09L
178500070129     C     KBRV9         READPE    FNBRV09L                               32
178600010702     C     *IN32         DOWEQ     *OFF
178700010702     C     P1CMSG        ANDEQ     *BLANKS
178800010702     C* IL foglio deve avere sottocategoria arrivi ed avere data foglio
178900010702     C* maggiore uguale alla data del foglio in fase di quadratura
179000010702     C                   CLEAR                   DSLV53
179100010702     C                   MOVEL     'V'           D53TFO
179200010702     C                   MOVEL     KNPG          D53NPG
179300010702     C                   Z-ADD     SIMFEL        D53FEL
179400010702     C                   Z-ADD     BRVNFV        D53NFV
179500010702     C                   Z-ADD     BRVFGS        D53FGS
179600010702     C                   CALL      'FNLV53R'
179700010702     C                   PARM                    DSLV53
179800010702     C     D53ERR        IFEQ      *BLANKS
179900010702     C     D53DFV        ANDGE     D59DDT
180000010702     C     D53SPG        ANDEQ     'A'
180100010702     C                   MOVEA     COS           W33
180200010702     C                   MOVE      BRVFGS        W003A             3
180300010702     C                   MOVEA     W003A         W33(8)
180400010702     C                   MOVE      BRVNFV        W006A             6
180500010712     C                   EXSR      EDITA
180600010702     C                   MOVEA     W006A         W33(12)
180700010702     C                   MOVE      D53DFV        W0040             4 0
180800010702     C                   MOVEL     W0040         WMMDFV            2
180900010702     C                   MOVE      W0040         WGGDFV            2
181000010702     C                   MOVEA     WGGDFV        W33(23)
181100010702     C                   MOVEA     WMMDFV        W33(26)
181200010702     C                   MOVE      BRVNPS        W002A             2
181300010702     C                   MOVEA     W002A         W33(32)
181400031009     C                   MOVEA     W33           P1CMSG
181500010702     C                   ENDIF
181600070129     C     KBRV9         READPE    FNBRV09L                               32
181700010702     C                   ENDDO
181800010620     C                   ENDIF
181900110518     C* Reimposto KNPG in quanto usato nel ciclo di lettura di fnbrve
182000110518     C                   Z-ADD     4             KNPG
182100010620     C*
182200010702     C                   Z-ADD     BREFGS        P1CFGS
182300010702     C                   Z-ADD     BRENFV        P1CNFV
182400010620     C* DATA DISTINTA
182500010620     C                   Z-ADD     WDFV          G02INV
182600010620     C                   MOVE      *ZEROS        G02DAT
182700010620     C                   MOVEL     3             G02ERR
182800010620     C                   CALL      'XSRDA8'
182900010620     C                   PARM                    WLBDA8
183000010620     C                   Z-ADD     G02DAT        P1CDFV
183100010620     C* AUTOTRASPORTATORE
183200110518     C     KDST          CHAIN     FiDST01L                           32
183300010620    1C     *IN32         IFEQ      *OFF
183400010711     C                   Z-ADD     DSTPDR        P1CPDR
183500021204     C     kapdd         CHAIN     fiapd01L                           32
183600010620    2C     *IN32         IFEQ      *OFF
183700031009     C                   MOVEL     APDRSC        P1DPDR
183800010620   X2C                   ELSE
183900010620     C                   CLEAR                   P1DPDR
184000010620    2C                   ENDIF
184100010620   X1C                   ELSE
184200010620     C                   CLEAR                   P1CPDR
184300010620     C                   ENDIF
184400010620     C* DATA IMMISSIONE
184500010702     C                   Z-ADD     BREDFS        G02INV
184600010620     C                   MOVE      *ZEROS        G02DAT
184700010620     C                   MOVEL     3             G02ERR
184800010620     C                   CALL      'XSRDA8'
184900010620     C                   PARM                    WLBDA8
185000010620     C                   Z-ADD     G02DAT        P1CDFS
185100010620     C*
185200010702     C                   Z-ADD     BREHMS        P1CHMS
185300010702     C                   Z-ADD     BRENPS        P1CNPS
185400030728     C                   WRITE     ERRDOP
185500010711     C                   ADD       1             D59LIN
185600010620     C                   ENDSR
185700010712     C*--------------------------------------------------------------*
185800010712     C* SOPPRESSIONE ZERI NON SIGNIFICATIVI DA CAMPO LUNGO 6
185900010712     C*--------------------------------------------------------------*
186000010712     C     EDITA         BEGSR
186100010712     C*
186200010712     C                   MOVE      *BLANKS       WFINE             1
186300010712     C                   MOVEA     W006A         W06
186400010712     C                   Z-ADD     1             II                1 0
186500010712     C     WFINE         DOWEQ     *BLANKS
186600010712     C     II            ANDLE     5
186700010712     C     W06(II)       IFEQ      '0'
186800010712     C                   MOVE      *BLANKS       W06(II)
186900010712     C                   ELSE
187000010712     C                   MOVE      '1'           WFINE
187100010712     C                   END
187200010712     C                   ADD       1             II
187300010712     C                   ENDDO
187400010712     C                   MOVEA     W06           W006A
187500010712     C                   ENDSR
187600950526     C*--------------------------------------------------------------*
187700950526     C*  *INZSR: Operazioni iniziali
187800950526     C*--------------------------------------------------------------*
187900950526     C     *INZSR        BEGSR
188000950526     C*------------------------------------------------------*
188100030722     c     *dtaara       define    §azute        azuteds
188200030722     c     *dtaara       define    §datiute      ddatiute
188300030722     C                   in(E)     *dtaara
188400030722     C                   IF        %Error  or  RSUT = *blanks
188500030722     C                   call      'TIBS34R'
188600030722     C                   parm                    Tibs34Ds
188700030722     C                   in        *dtaara
188800030722     c                   ENDIF
188900950612     C                   MOVE      SIMFEL        KFLE
189000950526     C*------------------------------------------------------*
189100950526     C*  Definisco chiavi di accesso
189200110518     C     KDST          KLIST
189300110518     C                   KFLD                    knpg
189400110518     C                   KFLD                    brenfv
189500110518     C                   KFLD                    BREFGS
189600030805     C     Kfiar5        KLIST
189700030805     C                   KFLD                    arbaas
189800030805     C                   KFLD                    arblnp
189900030805     C                   KFLD                    arbnrs
190000030805     C                   KFLD                    arbnsp
190100030805     C                   KFLD                    ar5trd
190200950526     C     KARB          KLIST
190300950526     C                   KFLD                    KAAS
190400950526     C                   KFLD                    KLNP
190500950526     C                   KFLD                    KNRS
190600950526     C                   KFLD                    KNSP
190700950606     C     KLBL          KLIST
190800950606     C                   KFLD                    KAAS
190900950606     C                   KFLD                    KLNP
191000950606     C                   KFLD                    KNRS
191100950606     C                   KFLD                    KNSP
191200950526     C     KART          KLIST
191300950526     C                   KFLD                    KAAS
191400950526     C                   KFLD                    KLNP
191500950526     C                   KFLD                    KNRS
191600950526     C                   KFLD                    KNSP
191700950605     C     KFVV          KLIST
191800950605     C                   KFLD                    KNPG
191900950605     C                   KFLD                    KNFV1
192000080229     C                   KFLD                    brvfgs
192100950526     C     KBRV1         KLIST
192200950526     C                   KFLD                    KNPG
192300950526     C                   KFLD                    KNFV
192400950526     C                   KFLD                    KFGS
192500950526     C                   KFLD                    KFLS
192600950526     C                   KFLD                    KLNA
192700950526     C                   KFLD                    KNRS
192800950526     C                   KFLD                    KNSC
192900950526     C     KBRV2         KLIST
193000950526     C                   KFLD                    KNPG
193100950526     C                   KFLD                    KFLS
193200950526     C                   KFLD                    KLNA
193300950526     C                   KFLD                    KNRS
193400950526     C                   KFLD                    KNSC
193500040420     C                   KFLD                    KFLE
193600010702     C     KBRV9         KLIST
193700010702     C                   KFLD                    KNPG
193800010702     C                   KFLD                    BRELNP
193900010702     C                   KFLD                    BRELNA
194000010702     C                   KFLD                    BRENRS
194100010702     C                   KFLD                    BRENSC
194200010702     C                   KFLD                    KFLE
194300990204     C     KDCD          KLIST
194400990204     C                   KFLD                    KFLS
194500990204     C                   KFLD                    KLNA
194600990204     C                   KFLD                    KNRS
194700990204     C                   KFLD                    KNSC
194800990204     C     KDCT          KLIST
194900990204     C                   KFLD                    KAAC
195000990204     C                   KFLD                    KFIL
195100990204     C                   KFLD                    KNCA
195200950526     C     KTAB          KLIST
195300950526     C                   KFLD                    KKUT
195400950526     C                   KFLD                    KCOD
195500010620     C     KTAB1         KLIST
195600010620     C                   KFLD                    KKUT
195700010620     C                   KFLD                    KCOD
195800010620     C                   KFLD                    KKEY
195900950915     C     KANM          KLIST
196000950915     C                   KFLD                    KFLS
196100950915     C                   KFLD                    KLNA
196200950915     C                   KFLD                    KNRS
196300950915     C                   KFLD                    KNSC
196400950915     C                   KFLD                    KCAA
196500980310     C     KEVB1         KLIST
196600990803     C                   KFLD                    KAAS
196700980310     C                   KFLD                    KLNP
196800980310     C                   KFLD                    KNRS
196900980310     C                   KFLD                    KNSP
197000980310     C     KEVB2         KLIST
197100990803     C                   KFLD                    KAAS
197200980310     C                   KFLD                    KLNP
197300980310     C                   KFLD                    KNRS
197400980310     C                   KFLD                    KNSP
197500980310     C                   KFLD                    KCEV
197600010620     C     KBRVE         KLIST
197700010620     C                   KFLD                    KNPG
197800010620     C                   KFLD                    KNFV
197900010620     C                   KFLD                    D59FGS
198000010620     C     KBRVE1        KLIST
198100010620     C                   KFLD                    KNPG
198200010620     C                   KFLD                    KNFV
198300010620     C                   KFLD                    D59FGS
198400010702     C                   KFLD                    BRELNP
198500010702     C                   KFLD                    BRELNA
198600010702     C                   KFLD                    BRENRS
198700010702     C                   KFLD                    BRENSC
198800010620     C     KBRVE2        KLIST
198900010702     C                   KFLD                    BRENPG
199000010702     C                   KFLD                    BRELNP
199100010702     C                   KFLD                    BRELNA
199200010702     C                   KFLD                    BRENRS
199300010702     C                   KFLD                    BRENSC
199400021204     C*
199500021204     C     Kapd          KLIST
199600021204     C                   KFLD                    apdtip
199700021204     C                   KFLD                    arbpdc
199800021204     C*
199900021204     C     Kapd59        KLIST
200000021204     C                   KFLD                    apdtip
200100021204     C                   KFLD                    d59pdr
200200021204     C*
200300021204     C     Kapdd         KLIST
200400021204     C                   KFLD                    apdtip
200500021204     C                   KFLD                    dstpdr
200600021204     c                   movel     'A'           apdtip
200700101124     C     Ktbe          KLIST
200800101124     C                   KFLD                    tbecod
200900101124     C                   KFLD                    tbeke1
201000101124     C                   KFLD                    tbeke2
201100101124     C     Kwf10         KLIST
201200101124     C                   KFLD                    WLRQ1AAS
201300101124     C                   KFLD                    WLRQ1LNP
201400101124     C                   KFLD                    WLRQ1NRS
201500101124     C                   KFLD                    WLRQ1NSP
201600101124     C                   KFLD                    WLRQ1FGS
201700101124     C                   KFLD                    WLRQ1NDC
201800101124     C                   KFLD                    WLRQ1PDR
201900950526     C*------------------------------------------------------*
202000950526     C*  Definisco variabili
202100030728     C     *LIKE         DEFINE    BRVNPS        BRVNPS1
202200030728     C     *LIKE         DEFINE    D59NDT        EXNDT
202300950526     C     *LIKE         DEFINE    ARTAAS        KAAS
202400950526     C     *LIKE         DEFINE    ARTLNP        KLNP
202500950526     C     *LIKE         DEFINE    ARTNRS        KNRS
202600950526     C     *LIKE         DEFINE    ARTNSP        KNSP
202700950526     C     *LIKE         DEFINE    ARTFLS        KFLS
202800950526     C     *LIKE         DEFINE    ARTLNA        KLNA
202900950526     C     *LIKE         DEFINE    ARTNSC        KNSC
203000950526     C     *LIKE         DEFINE    BRVNPG        KNPG
203100950526     C     *LIKE         DEFINE    BRVNFV        KNFV
203200950526     C     *LIKE         DEFINE    BRVFGS        KFGS
203300950612     C     *LIKE         DEFINE    BRVFLE        KFLE
203400951121     C     *LIKE         DEFINE    ARBRAG        KRAG
203500950526     C     *LIKE         DEFINE    TBLKUT        KKUT
203600950526     C     *LIKE         DEFINE    TBLCOD        KCOD
203700950915     C     *LIKE         DEFINE    ANMCAA        KCAA
203800980310     C     *LIKE         DEFINE    EVBCEV        KCEV
203900950918     C     *LIKE         DEFINE    ANMCAA        WCAA
204000950918     C     *LIKE         DEFINE    ANMNFV        WNFV
204100950918     C     *LIKE         DEFINE    ANMNPS        WNPS
204200960507     C     *LIKE         DEFINE    FVVFGS        WFGS
204300950605     C     *LIKE         DEFINE    FVVNFV        KNFV1
204400990204     C     *LIKE         DEFINE    TBECOD        KCODE
204500990204     C     *LIKE         DEFINE    DCTAAC        KAAC
204600990204     C     *LIKE         DEFINE    DCTFIL        KFIL
204700990204     C     *LIKE         DEFINE    DCTNCA        KNCA
204800010620     C     *LIKE         DEFINE    DSBRVE        WSBRVE
204900010620     C     *LIKE         DEFINE    DSBRVE        W1BRVE
205000010620     C     *LIKE         DEFINE    BRVNPS        SAWNPS
205100010622     C     *LIKE         DEFINE    BRVFGS        SAWFGS
205200010622     C     *LIKE         DEFINE    BRVNFV        SAWNFV
205300010620     C     *LIKE         DEFINE    BRVDFS        SAWDFS
205400010620     C     *LIKE         DEFINE    BRVHMS        SAWHMS
205500010620     C     *LIKE         DEFINE    D53DFV        SAWDFV
205600010620     C     *LIKE         DEFINE    D53DFV        WDFV
205700010702     C     *LIKE         DEFINE    D53DFV        SA1DFV
205800010620     C     *LIKE         DEFINE    TBLKEY        KKEY
205900950526     C*
206000950526     C                   TIME                    TIMES             6 0
206100950526     C                   TIME                    WHDAT            12 0
206200950526     C                   MOVE      WHDAT         WOGGI             6 0
206300010622     C                   TIME                    W0140            14 0
206400010622     C                   MOVE      W0140         WDTGIO            8 0
206500031009     C                   MOVEL     *ALL'-'       CMP132
206600031009     C                   MOVEL     *ALL'-'       CMP198
206700031009     C                   MOVEL     *ALL'-'       CMP132D
206800031009     C                   MOVEL     *ALL'-'       CMP198D
206900950526     C*
207000030722     C*  Carico dati tabella 4H anomalie da spunte
207100950526     C                   Z-ADD     1             KKUT
207200950526     C                   MOVEL     '4H'          KCOD
207300070601     C                   Z-ADD     0             X                 3 0
207400950526     C     KTAB          CHAIN     TABEL00F                           31
207500950526     C     *IN31         DOWEQ     '0'
207600950526     C     X             ANDLT     100
207700950526     C                   ADD       1             X
207800950526     C                   MOVEL     TBLUNI        DS4H
207900950526     C                   MOVEL     TBLKEY        C4H(X)
208000950526     C                   MOVEL     §4HDES        D4H(X)
208100950526     C                   MOVEL     §4HTPB        T4H(X)
208200950908     C                   MOVEL     §4HSTP        S4H(X)
208300950526     C     KTAB          READE     TABEL00F                               31
208400950526     C                   END
208500010620     C* Chaino tabella variabili pgm arrivi per reperire minuti di
208600010620     C* tolleranza
208700010620     C                   MOVEL     '7A'          KCOD
208800010620     C                   MOVEL(P)  '2       '    KKEY
208900010620     C     KTAB1         CHAIN     TABEL                              31
209000010620     C  N31              MOVEL     TBLUNI        DS7A2
209100010620     C   31              Z-ADD     1             §7AMNP
209200030728     C                   OPEN      FNLRF7P
209300030722     C*  Carico dati tabella DTA anomalie danni
209400990204     C                   Z-ADD     1             KKUT
209500990204     C                   MOVEL     'TAD'         KCODE
209600990204     C                   Z-ADD     0             X
209700990204     C     KCODE         CHAIN     TNTBE01L                           31
209800990204     C     *IN31         DOWEQ     '0'
209900990204     C     X             ANDLT     100
210000990204     C     TBESIF        IFEQ      KNSIF
210100990204     C     TBESIF        OREQ      *BLANKS
210200990204     C                   ADD       1             X
210300990204     C                   MOVEL     TBEKE1        TAD(X)
210400990204     C                   MOVEL     TBEUNI        DTA(X)
210500990204     C                   END
210600990204     C     KCODE         READE     TNTBE01L                               31
210700990204     C                   END
210800030703     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
210900040827     C                   CLEAR                   FIP
211000040827     C     *LOVAL        SETLL     AZORG01L
211100040827     C                   DO        *HIVAL
211200040827     C                   READ      AZORG01L                               95
211300040827     C   95              LEAVE
211400040827     C                   MOVEL     ORGDE3        OG143
211500040827     C     §OgNTW        IFEQ      'PPT'
211600040827     C                   Z-ADD     1             z                 3 0
211700040827     C     *ZEROS        LOOKUP    FIP(z)                                 23
211800040827     C   23              Z-ADD     ORGFIL        FIP(z)
211900040827     C                   END
212000040827     C                   END
212100950526     C*
212200950526     C                   ENDSR
212300950526     O*--------------------------------------------------------------*
212400950526     O*   STAMPA                                                     *
212500950526     O*--------------------------------------------------------------*
212600030728     OSYSPRT    E            SPED1       1
212700990803     O                       EVBAAS        Z      4
212800980325     O                       EVBLNP        Z     16
212900980325     O                       EVBNRS        Z     19
213000980325     O                       EVBNSP        Z     27
213100980325     O                                           80 'DELETE ERRATO'
213200950526     O*--------------------------------------------------------------*
213300950526     O*   AGGIORNAMENTI                                              *
213400950526     O*--------------------------------------------------------------*
213500951109     OFNARB000  E            AGGARB
213600950526     O                       ARBDDC
213700950526     O                       ARBNDC
213800020917     O                       ARBIFP
213900950526     O                       ARBPDC
214000950526     O                       ARBSTP
214100140410     O                       ARBNMI
214200140410     O                       ARBFDC
214300140410     O                       ARBNSS
214400990519     OFNANM000  E            AGGANM
214500950915     O                       ANMDCH
214600950915     O                       ANMFSC
214700950915     O                       ANMCCH
214800950918     O                       ANMNFV
214900950918     O                       ANMNPS
215000010702**
215100040409collo                    spuntato su distinta Nr.       pistola nr.xx
215200040421collo  NON + USATO       non spuntato in uscita ma arrivato pistola nr.xx
215300040421collo  NON + USATO       non spuntato in uscita non arrivato ma partito pistola nr.xx
215400040415VUOTO
215500040421collo  NON + USATO       non spuntato in uscita non arrivato e non partito
215600040419collo                    spuntato in uscita pistola nr.xx ore xx:xx ma già consegnato
215700040415collo                    spuntato in uscita pistola nr.xx ore xx:xx
215800040415                         e su foglio xxx-x-xxxxx pistola nr.xx ore xx:xx
215900040419collo                    non spuntato in uscita ma su f. xxx-x-xxxxx pist. nr.xx ore xx:xx
216000081013collo                    collo doppio
216100040421VUOTO
216200040419collo                    NON IN DISTINTA ma spuntato in uscita pistola nr.xx ore xx:xx
216300101124VUOTO
216400990204**
216500040421        Pratica C.A. aperta:  NON + USATO
216600040421        Pratica C.A. chiusa:  NON + USATO
216700010702**
216800010702In IMA XXX XXXXXX Del XX/XX PstXX
