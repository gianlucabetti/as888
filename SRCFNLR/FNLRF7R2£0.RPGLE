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
017100131104     c*
017200031029     D DSxmic          DS
017300031029     D  xdtmic                        8  0
017400031029     D  xoramic                       4  0
017500031029     D  xdtwrtmic                     8  0
017600031029     D  xorawrtmic                    6  0
017700031029     D DSxnic          DS
017800031029     D  xdtnic                        8  0
017900031029     D  xoranic                       4  0
018000031029     D  xdtwrtnic                     8  0
018100031029     D  xorawrtnic                    6  0
018200950526     D*--------------------------------------------------------------*
018300950526     D*  Costanti
018400950526     D COST3           C                   CONST('DOPO IL')
018500950526     D COST4           C                   CONST('IL')
018600030723     D CMAT            C                   CONST('MATTINO   ')
018700030723     D CPOM            C                   CONST('POMERIGGIO')
018800030805     d nban            S              5  0
018900040419     d sfgs            s                   like(fvvfgs)
019000040419     d snpg            s                   like(fvvnpg)
019100040419     d snfv            s                   like(fvvnfv)
019200110117     d sdfv            s                   like(fvvdfv)
019300040419     d shms            s                   like(brvhms)
019400950525      *--------------------------------------------------------------*
019500950525      * Ridenomino i campi dei codici anomalie in schiere
019600950525      *--------------------------------------------------------------*
019700950525     IFNARBY00
019800950525     I              ARBA01                      CDA(1)
019900950525     I              ARBA02                      CDA(2)
020000950525     I              ARBA03                      CDA(3)
020100950525     I              ARBA04                      CDA(4)
020200950525     I              ARBA05                      CDA(5)
020300950525     I              ARBA06                      CDA(6)
020400950525     I              ARBA07                      CDA(7)
020500950525     I              ARBA08                      CDA(8)
020600950525     I              ARBA09                      CDA(9)
020700950525     I              ARBA10                      CDA(10)
020800010702      *--------------------------------------------------------------*
020900010702      * Ridenomino tutti i campi DEL FILE FIBRVE
021000010702      *--------------------------------------------------------------*
021100070129     I*FIBRVE01
021200070129     I*             BRVATB                      BREATB
021300070129     I*             BRVFGS                      BREFGS
021400070129     I*             BRVNPG                      BRENPG
021500070129     I*             BRVNFV                      BRENFV
021600070129     I*             BRVPES                      BREPES
021700070129     I*             BRVLNP                      BRELNP
021800070129     I*             BRVLNA                      BRELNA
021900070129     I*             BRVNRS                      BRENRS
022000070129     I*             BRVNSC                      BRENSC
022100070129     I*             BRVZNC                      BREZNC
022200070129     I*             BRVNPS                      BRENPS
022300070129     I*             BRVCAN                      BRECAN
022400070129     I*             BRVVUC                      BREVUC
022500070129     I*             BRVPUC                      BREPUC
022600070129     I*             BRVDCS                      BREDCS
022700070129     I*             BRVHCS                      BREHCS
022800070129     I*             BRVDFS                      BREDFS
022900070129     I*             BRVHMS                      BREHMS
023000070129     I*             BRVATR                      BREATR
023100070129     I*             BRVFTR                      BREFTR
023200070129     I*             BRVDTR                      BREDTR
023300070129     I*             BRVHTR                      BREHTR
023400070129     I*             BRVFLE                      BREFLE
023500070129     I*FIBRVE02
023600070129     I*             BRVATB                      BREATB
023700070129     I*             BRVFGS                      BREFGS
023800070129     I*             BRVNPG                      BRENPG
023900070129     I*             BRVNFV                      BRENFV
024000070129     I*             BRVPES                      BREPES
024100070129     I*             BRVLNP                      BRELNP
024200070129     I*             BRVLNA                      BRELNA
024300070129     I*             BRVNRS                      BRENRS
024400070129     I*             BRVNSC                      BRENSC
024500070129     I*             BRVZNC                      BREZNC
024600070129     I*             BRVNPS                      BRENPS
024700070129     I*             BRVCAN                      BRECAN
024800070129     I*             BRVVUC                      BREVUC
024900070129     I*             BRVPUC                      BREPUC
025000070129     I*             BRVDCS                      BREDCS
025100070129     I*             BRVHCS                      BREHCS
025200070129     I*             BRVDFS                      BREDFS
025300070129     I*             BRVHMS                      BREHMS
025400070129     I*             BRVATR                      BREATR
025500070129     I*             BRVFTR                      BREFTR
025600070129     I*             BRVDTR                      BREDTR
025700070129     I*             BRVHTR                      BREHTR
025800070129     I*             BRVFLE                      BREFLE
025900950525     C*--------------------------------------------------------------*
026000950525     C*  Ciclo principale
026100950525     C*--------------------------------------------------------------*
026200950526     C     *ENTRY        PLIST
026300950526     C                   PARM                    KPJBA
026400030721     C                   MOVEL(p)  KPJBU         FNLR59
026500030805     c                   clear                   nban
026600050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
026700050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
026800050204     C* AUTOTRASPORTATORE
026900050204     C                   IF        D59STP = 'A'
027000050204     C                   SETON                                        61
027100050204     C                   SETOFF                                       62
027200050204     C                   ELSE
027300050204     C                   SETOFF                                       61
027400050204     C                   SETON                                        62
027500050204     C                   END
027600040930     c*
027700040930     c                   movel(p)  'FNLRF7R'     nompgm           10
027800030721     C*  A fine elaborazione leggo archivio eventi e scrivo
027900030721     C*  NIC in definitivo
028000030721     C     D59TPR        IFeq      'U'
028100030721     C                   EXSR      WRTNIC
028200030721     C                   MOVEL     'C'           D53TLA
028300030721     C                   CALL      'FNLV53R'
028400030721     C                   PARM                    DSLV53
028500030721     C                   SETON                                        LR
028600030721     c                   return
028700030721     c                   end
028800030729     C* spengo indicatore utilizzati nella routine di stampa
028900030729     C* spunte doppie.
029000030729     C                   SETOFF                                       2021
029100950526     C*  Valorizzo campi testata
029200950526     C                   EXSR      STPTES
029300040825     C     D59NDT        IFNE      EXNDT
029400040825     C                   Z-ADD     D59NDT        EXNDT
029500040825     C                   END
029600031009     C*  Leggo FNARBY dove ho memorizzato le spedizioni della distinta
029700031009     c*  con o senza errori
029800950529     C     *LOVAL        SETLL     FNARBY1L
029900031009     c                   do        *hival
030000031009     C                   READ      FNARBY1L
030100031009     c                   if        %eof(fnarby1l)
030200031009     c                   leave
030300031009     c                   end
030400950525     C                   EXSR      GESSPE
030500950525     C                   DELETE    FNARBY1L
030600030805     C                   ENDdo
030700030825     C*  Controllo se numero bancali caricati è maggiore a quello
030800110518     C*  in FIQDT
030900030825     C     nban          IFgt      D59NBL
031000030805     C                   Z-ADD     1             Y                 3 0
031100030805     C     '16'          LOOKUP    C4H(Y)                                 32
031200031016     C*  Controllo se si deve stampare l'aomalia
031300030805     C     S4H(Y)        IFNE      'N'
031400031016     C                   MOVEL     D4H(Y)        DErrq
031500110414     C                   CLEAR                   Terrq
031600031016     C                   Z-ADD     nban          WNRB1
031700031016     C                   Z-ADD     D59NBL        WNRB2
031800030805     C                   MOVEL     'S'           D59ANM
031900101201     c                   if        d59sqa =  ' ' or
032000101201     c                             d59sqa =  'T'
032100101125     c                   exsr      states
032200030805     C                   write     ERRQUA
032300101125     c                   end
032400030805     C                   ADD       2             D59LIN
032500030805     C                   END
032600030805     C                   END
032700010620     C*  Routine di stampa spunte consegna doppie
032800101201     c                   if        d59sqa =  ' ' or
032900101201     c                             d59sqa =  'T'
033000010620     C                   EXSR      DOPPIE
033100101125     c                   end
033200950526     C*  Loop x eventuale cancellazione FNARBZ
033300950526     C                   EXSR      DLTARZ
033400030721     C                   MOVEL(p)  FNLR59        KPJBU
033500950525     C                   SETON                                        RT
033600950525     C*--------------------------------------------------------------*
033700950526     C*  GESSPE: Gestione spedizione
033800950525     C*--------------------------------------------------------------*
033900950526     C     GESSPE        BEGSR
034000950525     C*  Cerco la spedizione su FNARB
034100950525     C                   Z-ADD     ARBAAS        KAAS
034200950525     C                   Z-ADD     ARBLNP        KLNP
034300950525     C                   Z-ADD     ARBNRS        KNRS
034400950525     C                   Z-ADD     ARBNSP        KNSP
034500950525     C     KARB          CHAIN     FNARB01L                           33
034600030825     c                   if        not *in33
034700031009     c* mi salvo la spedizione
034800031009     C                   MOVEL     DSARB         DSARBS
034900030805     C* CALCOLO IL NUMERO DEI BANCALI
035000030805     c                   exsr      srnban
035100031009     c* IMPOSTA I CAMPI IN STAMPA
035200110414     C                   MOVEL     ARBTSP        TIPERR
035300031009     C                   Z-ADD     ARBAAS        WAAS
035400031009     C                   Z-ADD     ARBLNP        WLNP
035500031009     C                   Z-ADD     ARBNRS        WNRS
035600031009     C                   Z-ADD     ARBNSP        WNSP
035700030731     C                   Z-ADD     ARBlna        Wlna
035800031009     C                   MOVEL     ARBRSD        WRSD
035900031009     C                   Z-ADD     ARBNCD        WNCD
036000031009     C                   Z-ADD     ARBNCA        WNCA
036100031009     C                   Z-ADD     ARBZNC        WZNC
036200030805     C                   movel     ARBmgs        Wcommes           2 0
036300030805     C                   move      ARBmgs        Wcomgg            2 0
036400031009     c                   movel     wcomgg        wdat
036500030805     c                   move      wcommes       wdat
036600031009     C* segnacolli non consecutivi
036700031009     C     ARBFNS        COMP      'S'                                    44
036800031009     C                   IF        NOT *IN44
036900031009     C                   MOVEL     '/'           WBAR
037000031009     C                   ELSE
037100031009     C                   CLEAR                   WBAR
037200031009     C                   END
037300030825     C                   END
037400031009     C* LOOP SU ERRORI
037500950525     C                   MOVEL     'N'           WERRBL            1
037600950525     C                   MOVEL     'N'           WNODIS            1
037700950525     C                   Z-ADD     1             X                 3 0
037800010702     C     X             DOWLE     10
037900950525     C     CDA(X)        ANDNE     *BLANKS
038000950530     C                   MOVEL     CDA(X)        WCDANO            2
038100950525     C                   EXSR      GESANO
038200950530     C                   ADD       1             X
038300031009     C                   ENDdo
038400950525     C*  Controllo se adesso è già stata messa in consegna
038500950529     C     ARBNDC        IFNE      *ZEROS
038600950530     C     ARBDCM        ANDEQ     0
038700950529     C     ARBNDC        ANDNE     D59NDT
038800950525     C     WNODIS        ANDEQ     'N'
038900031009     C                   MOVEL     '03'          WCDANO
039000950525     C                   EXSR      GESANO
039100040713     c* rendo bloccante l'errore anche se in tabella è stato deciso di
039200040713     c* non stamparlo
039300040713     C                   MOVEL     'S'           WERRBL
039400950525     C                   END
039500070530     c* METTE IN DISTINTA LA SPEDIZIONE se pgm "spunte"
039600040827     c                   movel     arbaas        arbdsp            8 0
039700040827     c                   move      arbmgs        arbdsp
039800031016     C* Se non ho trovato errore bloccante aggiorno la distinta
039900030721     C     WERRBL        IFEQ      'N'
040000070601     c                   if        arbngd=0 and arbndc = 0 and
040100070530     c                             (kcdaz='LR59' or kcdaz='LR60')
040200020917     C                   Z-ADD     D59FGS        ARBIFP
040300020917     C                   Z-ADD     D59NDT        ARBNDC
040400950525     C                   Z-ADD     D59DDT        ARBDDC
040500950525     C                   Z-ADD     D59PDR        ARBPDC
040600140410     C                   movel     *blanks       ARBfdc
040700140410     c                   z-add     0             arbnmi
040800140410     c                   z-add     0             arbnss
040900951121     C*  Attribuisco lo stop
041000951211     C     ARBKCS        IFNE      0
041100951211     C                   MOVEL     *BLANKS       KRAG
041200951211     C                   MOVEL     ARBKCS        KRAG
041300951211     C                   ELSE
041400951121     C                   MOVEL     ARBRSD        KRAG
041500951122     C                   MOVE      ARBIND        KRAG
041600951211     C                   END
041700951121     C     KRAG          CHAIN     FNARBZ0F                           32
041800950526     C*  Aggiorno la  bolla
041900950526     C                   EXCEPT    AGGARB
042000031013     C                   MOVEL     DSARB         DSARBS
042100031020     C                   END
042200031009     C*  DETTAGLIO SEGNACOLLI
042300980330     C                   Z-ADD     ARBAAS        KAAS
042400980330     C                   Z-ADD     ARBLNP        KLNP
042500980330     C                   Z-ADD     ARBNRS        KNRS
042600980330     C                   Z-ADD     ARBNSP        KNSP
042700950614     C                   MOVEL     'N'           WERRPZ            1
042800031009     C                   MOVEL     'N'           OKMIC             1
042900950614     C     KART          CHAIN     FNART01L                           32
043000030721     C*  Se non esiste e spedizione figlia mi posiziono su mamma
043100950614     C     *IN32         IFEQ      '1'
043200950606     C     KLBL          CHAIN     FNLBL01L                           32
043300950606     C     *IN32         IFEQ      '0'
043400950606     C     LBLLAP        ANDEQ     LBLLAN
043500950606     C                   Z-ADD     LBLAAP        KAAS
043600950606     C                   Z-ADD     LBLLPP        KLNP
043700950606     C                   Z-ADD     LBLNRP        KNRS
043800950606     C                   Z-ADD     LBLNSP        KNSP
043900950606     C     KARB          CHAIN     FNARB01L                           32
044000980430     C     ARBCCA        IFNE      '2'
044100980430     C     ARBCCA        ANDNE     '6'
044200950614     C     KART          CHAIN     FNART01L                           32
044300980430     C                   ELSE
044400980430     C                   Z-ADD     LBLAAN        KAAS
044500980430     C                   Z-ADD     LBLLPN        KLNP
044600980430     C                   Z-ADD     LBLNRN        KNRS
044700980430     C                   Z-ADD     LBLNSN        KNSP
044800980430     C     KARB          CHAIN     FNARB01L                           32
044900950614     C                   END
045000980430     C                   END
045100950614     C                   END
045200950614     C*  Loop su dettaglio segnacolli per verifcare se la bolla
045300950614     C*  è consegnata parzialmente su altra distinta
045400950526     C     *IN32         DOWEQ     '0'
045500000705     C                   EXSR      GESDET
045600031009     C                   ENDdo
045700070604     c* SCRIVO EVENTO MIC E ORM CONTESTUALE ALLA CONSEGNA
045800031020     c                   movel     dsarbs        dsarb
045900070530     c                   if        okmic = 'S'
046000100514     c                   clear                   ds7r
046100100514     c                   if        arbgma <> ' '
046200100514     c                   exsr      sr7r
046300100514     c                   end
046400100514     c                   if        §7RRC ='S'
046500031020     c                   clear                   fnlr99ds
046600071002     c                   eval      i99nftl= 99999
046700071002     c                   eval      i99fao = 400
046800031009     c                   exsr      aggorm
046900031020     c                   end
047000031020     c                   exsr      aggeve
047100070601     c*                  end
047200070530     c                   end
047300950526     C*
047400951109     C                   ELSE
047500030721     C  N33              unlock    fnarb01l
047600950526     C                   END
047700950526     C*
047800950526     C                   ENDSR
047900031003     C*--------------------------------------------------------------*
048000031003     C*  GESDET: Gestione dettaglio segnacolli
048100031003     C*--------------------------------------------------------------*
048200031003     C     GESDET        BEGSR
048300950526     C*
048400040409     C                   SETOFF                                       02
048500031009     C                   MOVEL     *BLANKS       WTIPCA
048600031009     C                   MOVEL     *BLANKS       WDETER
048700950530     C                   MOVEL     *BLANKS       WNRP              2
048800031009     C                   Z-ADD     0             WNRP1             2 0
048900950614     C                   MOVEL     'N'           WTRDAT            1
049000950614     C                   MOVEL     'N'           WTRDA1            1
049100950918     C                   MOVEL     'N'           WTRSPU            1
049200950526     C*  Controllo se non presente su distinta
049300950526     C                   Z-ADD     4             KNPG
049400950526     C                   Z-ADD     D59NDT        KNFV
049500950526     C                   Z-ADD     D59FGS        KFGS
049600950526     C                   Z-ADD     ARBLNA        KLNA
049700950526     C                   Z-ADD     ARBFLS        KFLS
049800950526     C                   Z-ADD     ARBNRS        KNRS
049900950526     C                   Z-ADD     ARTNSC        KNSC
050000000706      *
050100070129     C     KBRV1         CHAIN     FNBRV01L                           33
050200040408     c* NON HO LA SPUNTA 4
050300030729     C     *IN33         IFEQ      '1'
050400040421     c* solo se la spedizione è in distinta
050500040507     c                   if        s_arbndc  <> 0
050600990204     C* Controllo Dati Danni
050700040420     C*                  EXSR      TESDAN
050800950526     C*  Se non è presente su questa distinta verifico se è
050900950526     C*  in consegna su di un'altra
051000040408     c                   exsr      srfibrv
051100040408     c                   movel     okspu         wtrdat
051200030801     C*    se su altra distinta
051300030729     C     WTRDAT        IFEQ      'S'
051400950526     C     BRVNFV        ANDNE     0
051500040409     C                   MOVEL(p)  ERR(1)        WDETER
051600101124     c                   movel     '001'         tbeke2
051700040419     c                   movel     fvvnfv        com05
051800040419     c                   eval      %subst(wdeter: 51: 5)= com05
051900040409     c                   eval      %subst(wdeter: 71: 20)= fvvdsf
052000101126     c                   eval      sfgs=fvvfgs
052100101126     c                   eval      snpg=fvvnpg
052200101126     c                   eval      snfv=fvvnfv
052300110117     c                   eval      sdfv=fvvdfv
052400101126     c                   eval      shms=brvhms
052500950614     C                   MOVEL     BRVNPS        WNRP
052600950526     C                   EXSR      STAMPA
052700960507     C*  Richiamo TRUL11 per reperimento della fase
052800960507     C                   CLEAR                   DSFASE
052900960507     C                   MOVEL     '4'           WNPG
053000960507     C                   CALL      'TRUL11R'
053100960507     C                   PARM                    DSFASE
053200950919     C                   MOVEL     'S'           WTRSPU
053300950918     C                   Z-ADD     150           WCAA
053400950918     C                   Z-ADD     BRVNFV        WNFV
053500950918     C                   Z-ADD     BRVNPS        WNPS
053600960507     C                   Z-ADD     FVVFGS        WFGS
053700950918     C                   EXSR      WRTANM
053800030729     C                   END
053900950614     C*  Controllo se collo spuntato in I.M.A.
054000950614     C                   Z-ADD     3             KNPG
054100040408     c                   exsr      srfibrv
054200040408     c                   movel     okspu         wtrda1
054300040408     C*
054400950918     C                   MOVEL     *BLANKS       WDETER
054500950919     C                   SETOFF                                       02
054600950614     C                   Z-ADD     4             KNPG
054700030801     c*    se in inventario IMA
054800030729     C     WTRDA1        IFEQ      'S'
054900950614     C     BRVNFV        ANDNE     0
055000950614     C                   MOVEL     BRVNPS        WNRP
055100030801     c*    ...e su altra distinta
055200950919     C     WTRSPU        IFEQ      'S'
055300960507     C     FVVFCF        IFNE      'S'
055400040421     C                   MOVEL(p)  ERR(8)        WDETER
055500101124     c                   movel     '008'         tbeke2
055600040419     c                   eval      sfgs=fvvfgs
055700040419     c                   eval      snpg=fvvnpg
055800040419     c                   eval      snfv=fvvnfv
055900110117     c                   eval      sdfv=fvvdfv
056000040419     c                   eval      shms=brvhms
056100960507     C                   END
056200950919     C                   ELSE
056300030801     c*    ...no su altra distinta
056400040415     C                   MOVEL(p)  ERR(9)        WDETER
056500101124     c                   movel     '009'         tbeke2
056600040419     c                   eval      sfgs=fvvfgs
056700040419     c                   eval      snpg=fvvnpg
056800040419     c                   eval      snfv=fvvnfv
056900110117     c                   eval      sdfv=fvvdfv
057000040419     c                   eval      shms=brvhms
057100950919     C                   END
057200030801     c*
057300960507     C     WTRSPU        IFNE      'S'
057400960507     C     FVVFCF        ORNE      'S'
057500950614     C                   EXSR      STAMPA
057600960507     C*  Richiamo TRUL11 per reperimento della fase
057700960507     C                   CLEAR                   DSFASE
057800960507     C                   MOVEL     '3'           WNPG
057900960507     C                   MOVEL     'A'           WSPG
058000960507     C                   CALL      'TRUL11R'
058100960507     C                   PARM                    DSFASE
058200950919     C                   MOVEL     'S'           WTRSPU
058300950918     C                   Z-ADD     135           WCAA
058400950920     C                   Z-ADD     BRVNFV        WNFV
058500950918     C                   Z-ADD     WNRP1         WNPS
058600960507     C                   Z-ADD     FVVFGS        WFGS
058700950915     C                   EXSR      WRTANM
058800960507     C                   END
058900030729     C                   END
059000950918     C     WTRSPU        IFEQ      'S'
059100950918     C                   GOTO      NEWSGN
059200950918     C                   END
059300950526     C*  Controllo se il collo è arrivato
059400040419     C*    ARTDAM        IFNE      0
059500040419     C*    ARTDCM        ANDEQ     0
059600040419     C*    WDAVM         ANDEQ     'N'
059700040419     C*                  MOVEL(p)  ERR(2)        WDETER
059800040419     C*                  MOVEL     ARTNAP        WNRP
059900040419     C*                  EXSR      STAMPA
060000040419     C*                  GOTO      NEWSGN
060100040419     C*                  END
060200950526     C*  Collo partito e non arrivato
060300040419     C*    ARTDFV        IFNE      0
060400040419     C*    ARTDCM        ANDEQ     0
060500040419     C*    WDAVM         ANDEQ     'N'
060600040419     C*                  MOVEL(p)  ERR(3)        WDETER
060700040419     C*                  MOVEL     ARTNPP        WNRP
060800040419     C*                  EXSR      STAMPA
060900040419     C*                  GOTO      NEWSGN
061000040419     C*                  END
061100950526     C*  Collo non arrivato e non partito
061200040420     C*    ARTDCM        IFEQ      0
061300040420     C*    WDAVM         ANDEQ     'N'
061400040420     C*                  MOVEL(p)  ERR(5)        WDETER
061500040420     C*                  EXSR      STAMPA
061600040420     C*                  GOTO      NEWSGN
061700040420     C*                  END
061800950526     C*
061900040421     C                   end
062000030729     C                   ELSE
062100081103     c*
062200081103     C                   MOVEL     BRVNPS        WNRP
062300081103     C                   Z-ADD     BRVNPS        WNRP1
062400081103     C                   eval      dsbrvs = dsbrv
062500040408     c* HO LA SPUNTA 4
062600081014     c* per prima memorizzo segnazione di probabile etichetta doppia
062700081014     c                   clear                   fnlrv7ds
062800081014     c                   eval      ILRV7FLS=kfls
062900081014     c                   eval      ILRV7LNA=klna
063000081014     c                   eval      ILRV7NRS=knrs
063100081014     c                   eval      ILRV7NSC=knsc
063200081014     c                   movel(p)  fnlrv7ds      kpjbu
063300081014     c                   call      'FNLRV7R'
063400081014     c                   parm                    kpjba
063500081014     c                   movel     kpjbu         fnlrv7ds
063600081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
063700090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
063800081014     C                   MOVEL(p)  ERR(10)       WDETER
063900101124     c                   movel     '010'         tbeke2
064000101126     c                   eval      shms=s_brvhms
064100101126     c                   eval      sfgs=s_brvfgs
064200101126     c                   eval      snpg=s_brvnpg
064300110117     c                   eval      snfv=s_brvnfv
064400110117     c                   eval      sdfv=d59ddt
064500081014     C                   EXSR      STAMPA
064600081014     C                   END
064700040408     C* Controllo se collo spuntato anche in I.M.A.
064800040408     C                   Z-ADD     3             KNPG
064900040408     c                   exsr      srfibrv
065000040408     c                   movel     okspu         wtrda1
065100040408     C                   Z-ADD     4             KNPG
065200110118     c**********************************************************************
065300110118     c* se sono chiamato dalla sistemazione controllo aut, non segnalo
065400110118     c* l'errore e tento di cancellare la spunta + vecchia tra la 3 e la 4
065500110120     c                   if        kcdaz = 'LRQ1' and wtrda1 = 'S'
065600110118     c                   exsr      srls46
065700110118     c* se no errore ed effettuato l'aggiornamento proseguo
065800110118     c                   if        OLS46ERR = ' ' and
065900110118     c                             OLS46ANN = 'S'
066000110118     c* se cancellato la 3 e resta la 4 verifico se bolla in distinta ok
066100110118     c* segnalo errore
066200110118     c                   if        s_brvhms > brvhms
066300110118     c                   if        s_arbndc = 0
066400110118     c                   eval      wtrda1 = 'N'
066500110118     c                   clear                   dsbrv
066600110118     c                   clear                   dsfvv
066700110118     c                   else
066800110118     C                   GOTO      NEWSGN
066900110118     c                   end
067000110118     c                   else
067100110118     c* se cancellato la 4 e resta la 3 verifico se bolla in distinta errore
067200110118     c* altrimenti ok
067300110118     c                   if        s_arbndc = 0
067400110118     C                   GOTO      NEWSGN
067500110118     c                   else
067600110118     C                   MOVEL(p)  ERR(9)        WDETER
067700110118     c                   movel     '009'         tbeke2
067800110118     c                   eval      sfgs=fvvfgs
067900110118     c                   eval      snpg=fvvnpg
068000110118     c                   eval      snfv=fvvnfv
068100110118     c                   eval      sdfv=fvvdfv
068200110118     c                   eval      shms=brvhms
068300110118     C                   EXSR      STAMPA
068400110118     C                   GOTO      NEWSGN
068500110118     c                   end
068600110118     c                   end
068700110118     c                   end
068800110118     c                   end
068900110118     c**********************************************************************
069000040415     C* Se ho SOLO la spunta 4 ma la spedizione non è in distinta
069100040408     C* segnalo l'errore
069200110118     C                   IF        s_arbndc = 0
069300040408     C                   IF        wtrda1 = 'N'
069400040415     C                   MOVEL(p)  ERR(12)       WDETER
069500101124     c                   movel     '012'         tbeke2
069600101126     c                   eval      sfgs=s_brvfgs
069700101126     c                   eval      snpg=s_brvnpg
069800101126     c                   eval      snfv=s_brvnfv
069900040419     c                   eval      shms=s_brvhms
070000110117     c                   eval      sdfv=d59ddt
070100040408     C                   EXSR      STAMPA
070200040408     C                   GOTO      NEWSGN
070300040408     C* Se ho la spunta 4 e la spunta 3 ma la spedizione non è in distinta
070400040408     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
070500040408     C* spunta 4 > di ora spunta 3
070600040408     c                   else
070700110118     c
070800040409     c                   if        s_brvnps <> brvnps or
070900040409     c                             (s_brvnps = brvnps and s_brvhms > brvhms)
071000101124     C                   MOVEL(p)  ERR(12)       WDETER
071100101124     c                   movel     '012'         tbeke2
071200040419     c                   eval      shms=s_brvhms
071300101126     c                   eval      sfgs=s_brvfgs
071400101126     c                   eval      snpg=s_brvnpg
071500101126     c                   eval      snfv=s_brvnfv
071600101126     c                   eval      shms=s_brvhms
071700110117     c                   eval      sdfv=d59ddt
071800040415     C                   EXSR      STAMPA
071900040419     C                   MOVEL(p)  ERR(8)        WDETER
072000101124     c                   movel     '008'         tbeke2
072100040419     c                   eval      sfgs=fvvfgs
072200040419     c                   eval      snpg=fvvnpg
072300040419     c                   eval      snfv=fvvnfv
072400110117     c                   eval      sdfv=fvvdfv
072500040419     c                   eval      shms=brvhms
072600040419     c                   movel     brvnps        wnrp
072700040415     c                   seton                                        02
072800040409     C                   EXSR      STAMPA
072900040415     c                   setoff                                       02
073000040415     C                   GOTO      NEWSGN
073100040415     c                   else
073200040408     C                   GOTO      NEWSGN
073300040408     c                   end
073400040408     C                   END
073500040409     C                   END
073600040408     c*
073700031013     c                   eval      okmic = 'S'
073800031009     C* Se è stato spuntato con la distinta in esame controllo
073900031009     C* se segnacollo già consegnato
074000030729     C     ARTDCM        IFNE      0
074100040415     C                   MOVEL(P)  ERR(6)        WDETER
074200101124     c                   movel     '006'         tbeke2
074300040419     c                   eval      shms=s_brvhms
074400101126     c                   eval      sfgs=s_brvfgs
074500101126     c                   eval      snpg=s_brvnpg
074600101126     c                   eval      snfv=s_brvnfv
074700110117     c                   eval      sdfv=d59ddt
074800950526     C                   EXSR      STAMPA
074900950614     C                   GOTO      NEWSGN
075000030729     C                   END
075100040421     C* Se ho la spunta 4 e la spunta 3 e la spedizione è in distinta
075200040421     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
075300040421     C* spunta 3 > di ora spunta 4
075400040421     c                   if        s_brvnps <> brvnps or
075500040421     c                             (s_brvnps = brvnps and s_brvhms < brvhms)
075600030729     C     WTRDA1        IFEQ      'S'
075700040408     C     BRVNFV        ANDNE     0
075800960507     C     FVVFCF        IFNE      'S'
075900040415     C                   MOVEL(P)  ERR(7)        WDETER
076000101124     c                   movel     '007'         tbeke2
076100040419     c                   eval      shms=s_brvhms
076200101126     c                   eval      sfgs=s_brvfgs
076300101126     c                   eval      snpg=s_brvnpg
076400101126     c                   eval      snfv=s_brvnfv
076500110117     c                   eval      sdfv=d59ddt
076600040409     C                   EXSR      STAMPA
076700040415     C                   MOVEL(P)  ERR(8)        Wdeter
076800101124     c                   movel     '008'         tbeke2
076900040419     c                   eval      sfgs=fvvfgs
077000040419     c                   eval      snpg=fvvnpg
077100040419     c                   eval      snfv=fvvnfv
077200110117     c                   eval      sdfv=fvvdfv
077300040419     c                   eval      shms=brvhms
077400040419     c                   movel     brvnps        wnrp
077500040415     c                   seton                                        02
077600950612     C                   EXSR      STAMPA
077700040415     c                   setoff                                       02
077800960507     C*  Richiamo TRUL11 per reperimento della fase
077900960507     C                   CLEAR                   DSFASE
078000960507     C                   MOVEL     '3'           WNPG
078100960507     C                   MOVEL     'A'           WSPG
078200960507     C                   CALL      'TRUL11R'
078300960507     C                   PARM                    DSFASE
078400950918     C                   Z-ADD     135           WCAA
078500950920     C                   Z-ADD     BRVNFV        WNFV
078600950918     C                   Z-ADD     WNRP1         WNPS
078700960507     C                   Z-ADD     FVVFGS        WFGS
078800950915     C                   EXSR      WRTANM
078900031009     C                   GOTO      NEWSGN
079000030729     C                   END
079100030729     C                   END
079200040421     C                   END
079300950526     C*
079400030729     C                   END
079500950526     C*  Leggo successivo segnacollo
079600950614     C     NEWSGN        TAG
079700950526     C     KART          READE     FNART01L                               32
079800950526     C*
079900950526     C                   ENDSR
080000031009     C*--------------------------------------------------------------*
080100040408     C*  srfibrv cerco spunta nel foglio 4/3 nel gg. della distinta che
080200040408     c*          sto quadrando
080300031009     C*--------------------------------------------------------------*
080400040408     C     srfibrv       BEGSR
080500040408     c                   movel     'N'           okspu             1
080600040409     c                   clear                   dsbrvc
080700040409     c                   clear                   dsfvvc
080800031009     C*
080900070129     C     KBRV2         SETll     FNBRV09L
081000040408     c                   do        *hival
081100070129     C     KBRV2         READe     FNBRV09L
081200070129     c                   if        %eof(fnbrv09l)
081300040408     c                   leave
081400040408     c                   end
081500040408     C                   Z-ADD     BRVNFV        KNFV1
081600040408     C     KFVV          CHAIN     FNFVV01L
081700040408     c                   if        %found(fnfvv01l) and fvvdfv = d59ddt
081800040408     C                   MOVEL     'S'           okspu
081900040409     c                   if        brvhms > c_brvhms
082000040409     c                   eval      dsbrvc = dsbrv
082100040409     c                   eval      dsfvvc = dsfvv
082200040408     c                   end
082300040409     c                   end
082400040408     C                   ENDDO
082500040409     c*
082600040409     c                   eval      dsbrv = dsbrvc
082700040409     c                   eval      dsfvv = dsfvvc
082800040408     C*
082900040408     C                   ENDSR
083000040408     C*--------------------------------------------------------------*
083100040408     C*  AGGEVE: scrivo evento MIC sia per distinte manuali che x quelle
083200040408     c*          da spunte
083300040408     C*--------------------------------------------------------------*
083400040408     C     AGGEVE        BEGSR
083500040408     C*
083600131104     C                   clear                   dEVB01
083700131104     C                   move      *zeros        §NOTNDC
083800131104     C                   move      *zeros        §NOTPDC
083900131104     C                   move      ARBPDC        §NOTPDC
084000131104     C                   move      ARBNDC        §NOTNDC
084100131104     C                   MOVEL     Devb01        dEVB01sav
084200031009     C* Nel momento in cui metto in consegna la bolla controllo se
084300031009     C* ho scritto evento NIC provvisorio nell'archivio eventi della
084400031009     C* QTEMP e nel caso lo cancello
084500031009     C                   Z-ADD     ARBAAS        KAAS
084600031009     C                   Z-ADD     ARBLNP        KLNP
084700031009     C                   Z-ADD     ARBNRS        KNRS
084800031009     C                   Z-ADD     ARBNSP        KNSP
084900031009     C     KLBL          CHAIN     FNLBL01L                           32
085000031009     C     *IN32         IFEQ      '0'
085100031009     C     LBLLAP        ANDEQ     LBLLAN
085200031009     C                   Z-ADD     LBLAAP        KAAS
085300031009     C                   Z-ADD     LBLLPP        KLNP
085400031009     C                   Z-ADD     LBLNRP        KNRS
085500031009     C                   Z-ADD     LBLNSP        KNSP
085600031009     C     KARB          CHAIN(N)  FNARB01L                           32
085700031009     C     ARBCCA        IFEQ      '2'
085800031009     C     ARBCCA        OREQ      '6'
085900031009     C                   Z-ADD     LBLAAN        KAAS
086000031009     C                   Z-ADD     LBLLPN        KLNP
086100031009     C                   Z-ADD     LBLNRN        KNRS
086200031009     C                   Z-ADD     LBLNSN        KNSP
086300031009     C     KARB          CHAIN(N)  FNARB01L                           32
086400031009     C                   SETON                                        32
086500031009     C                   END
086600031009     C                   END
086700031009     C     *IN32         IFEQ      '0'
086800031009     C     LBLLAP        ANDEQ     LBLLAN
086900031009     C                   Z-ADD     LBLAAP        KAAS
087000031009     C                   Z-ADD     LBLLPP        KLNP
087100031009     C                   Z-ADD     LBLNRP        KNRS
087200031009     C                   Z-ADD     LBLNSP        KNSP
087300031009     C                   ELSE
087400031009     C                   Z-ADD     ARBAAS        KAAS
087500031009     C                   Z-ADD     ARBLNP        KLNP
087600031009     C                   Z-ADD     ARBNRS        KNRS
087700031009     C                   Z-ADD     ARBNSP        KNSP
087800031009     C                   END
087900130326     C     KEVB1         CHAIN     WFNEVB01L                          36
088000031009     C     EVBCEV        IFEQ      'NIC'
088100130326     C  N36              DELETE    WFNEVB01L
088200031009     C                   ELSE
088300031009     C  N36              EXCEPT    SPED1
088400031009     C                   END
088500131218      **
088600031009     C* Aggiorno evento di messa in consegna
088700031009     C                   MOVEL     'MIC'         KCEV
088800031009     C     KEVB2         SETGT     FNEVB04L
088900031009     C     KEVB2         READPE    FNEVB04L                               37
089000131218      **
089100031009     C     *IN37         IFEQ      '1'
089200031009     C     EVBDEV        ORNE      D59DDT
089300131218      **
089400031009     C                   MOVEL     'MIC'         EVBCEV
089500031009     C                   Z-ADD     D59AMG        EVBDTV
089600031009     C                   TIME                    EVBORV
089700031009     C                   Z-ADD     D59FGS        EVBFLE
089800031009     C                   MOVE      D59CDU        EVBCDU
089900031009     C                   Z-ADD     KAAS          EVBAAS
090000031009     C                   Z-ADD     KLNP          EVBLNP
090100031009     C                   Z-ADD     KNRS          EVBNRS
090200031009     C                   Z-ADD     KNSP          EVBNSP
090300031009     C                   Z-ADD     D59DDT        EVBDEV
090400031009     C                   Z-ADD     0             EVBDTR
090500031009     C                   MOVEL     *BLANKS       EVBFTR
090600031009     C                   Z-ADD     0             EVBDTC
090700131104     C                   MOVEL     Devb01sav     EVBNOT
090800031009     C     D59FPP        IFEQ      'M'
090900031009     C                   MOVEL     0800          EVBHEV
091000031009     C                   ELSE
091100031009     C                   MOVEL     1200          EVBHEV
091200031009     C                   END
091300031009     C                   WRITE     FNEVB000
091400131218      **
091500031009     C                   ELSE
091600131218      **
091700031009     C     EVBCEV        IFEQ      'MIC'
091800031009     C     EVBDEV        ANDEQ     D59DDT
091900031009     C*
092000031009     C     EVBDTR        IFEQ      0
092100131218     C*
092200131218     C* riaggancia il record  FISICO x aggiornarlo
092300131218     c     evb_nrr       chain     fnevb00F
092400131218     C*
092500131218     c                   if        %Found(fnevb00F)
092600031009     C* Se non è stato trasmesso aggiorno la data e l'ora di
092700031009     C* di immissione evento con quella del giorno + altri dati
092800031009     C                   Z-ADD     D59AMG        EVBDTV
092900031009     C                   TIME                    EVBORV
093000031009     C                   Z-ADD     D59FGS        EVBFLE
093100031009     C                   MOVE      D59CDU        EVBCDU
093200131104     C                   MOVEL     Devb01sav     EVBNOT
093300031009     C     D59FPP        IFEQ      'M'
093400031009     C                   MOVEL     0800          EVBHEV
093500031009     C                   ELSE
093600031009     C                   MOVEL     1200          EVBHEV
093700031009     C                   END
093800031009     C                   MOVEL     *BLANKS       EVBDTC
093900031009     C                   UPDATE    FNEVB000
094000131218     c                   end
094100131218      **
094200031009     C                   ELSE
094300131218      **
094400031009     C* Se l'evento MIC esiste già ed è già stato trasmesso lo
094500031009     C* riscrivo solo se c'è un NIC successivo
094600031029     C                   MOVE      EVBDEV        xdtMIC
094700031029     C                   MOVE      EVBHEV        xoraMIC
094800031029     C                   MOVE      EVBdtv        xdtwrtMIC
094900031029     C                   MOVE      EVBorv        xorawrtMIC
095000131218      **
095100031009     C                   MOVEL     'NIC'         KCEV
095200031009     C     KEVB2         SETGT     FNEVB04L
095300031009     C     KEVB2         READPE    FNEVB04L                               37
095400131218      **
095500031009     C     *IN37         IFEQ      '0'
095600031009     C     EVBDEV        ANDEQ     D59DDT
095700131218      **
095800031029     C                   MOVE      EVBDEV        xdtNIC
095900031029     C                   MOVE      EVBHEV        xoraNIC
096000031029     C                   MOVE      EVBdtv        xdtwrtNIC
096100031029     C                   MOVE      EVBorv        xorawrtNIC
096200031029     C     dsxMIC        IFLE      dsxNIC
096300031009     C                   MOVEL     'MIC'         EVBCEV
096400031009     C                   Z-ADD     D59AMG        EVBDTV
096500031009     C                   TIME                    EVBORV
096600031009     C                   Z-ADD     D59FGS        EVBFLE
096700031009     C                   MOVE      D59CDU        EVBCDU
096800031009     C                   Z-ADD     KAAS          EVBAAS
096900031009     C                   Z-ADD     KLNP          EVBLNP
097000031009     C                   Z-ADD     KNRS          EVBNRS
097100031009     C                   Z-ADD     KNSP          EVBNSP
097200031009     C                   Z-ADD     D59DDT        EVBDEV
097300031009     C                   Z-ADD     0             EVBDTR
097400031009     C                   MOVEL     *BLANKS       EVBFTR
097500031009     C     D59FPP        IFEQ      'M'
097600031009     C                   MOVEL     0800          EVBHEV
097700031009     C                   ELSE
097800031009     C                   MOVEL     1200          EVBHEV
097900031009     C                   END
098000031009     C                   Z-ADD     0             EVBDTC
098100131104     C                   MOVEL     Devb01sav     EVBNOT
098200031009     C                   WRITE     FNEVB000
098300131218      **
098400031009     C                   END
098500031009     C*
098600031009     C                   END
098700031009     C                   END
098800031009     C                   END
098900031009     C                   END
099000031020     c* mi reimposto la ds di FNARB originale
099100031020     c                   movel     dsarbs        dsarb
099200031009     C*
099300031009     C                   ENDSR
099400031009     C*--------------------------------------------------------------*
099500100514     C*  sr7r  particolarità consegna
099600031009     C*--------------------------------------------------------------*
099700100514     C     sr7r          BEGSR
099800031009     c*
099900100514     C                   eval      kcod = '7R'
100000100514     C                   eval      kkey = arbgma
100100100514     c                   eval      kkut = 1
100200100514     C     ktab1         chain     Tabel00f
100300100514     c                   if        %found(tabel00f)
100400100514     C                   MOVEL     TBLUNI        DS7R
100500100514     c                   end
100600100514     C*
100700100514     C                   ENDSR
100800100514     C*--------------------------------------------------------------*
100900100514     C*  aggorm: Aggiorno ORM e sue fasi
101000100514     C*--------------------------------------------------------------*
101100100514     C     aggorm        BEGSR
101200100514     c*
101300031009     c                   eval      i99tla = 'L'
101400031020     c                   eval      i99aas = arbaas
101500031020     c                   eval      i99lnp = arblnp
101600031020     c                   eval      i99nrs = arbnrs
101700031020     c                   eval      i99nsp = arbnsp
101800031020     c* metto in distinta
101900071001     c                   eval      i99fgs = d59fgs
102000031020     c                   if        i99fao = 400
102100031020     c                   eval      i99ndc = arbndc
102200031020     c                   eval      i99ddc = arbddc
102300031009     c                   eval      i99npg = 4
102400031020     c                   end
102500031020     c                   eval      i99comit = '1'
102600071001     c                   eval      i99cmt = 'N'
102700031009     c                   call      'FNLR99R'
102800031009     c                   parm                    kpjba
102900031009     c                   parm                    fnlr99ds
103000031009     c*
103100031009     C                   ENDSR
103200110118     C*--------------------------------------------------------------*
103300110118     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
103400110118     c* dal controllo a video
103500110118     C*--------------------------------------------------------------*
103600110118     C     srls46        BEGSR
103700110118     c*
103800110118     c                   clear                   fnls46ds
103900110118     c                   clear                   kpjbu
104000110118     c                   eval      ILS46NPGA =  brvnpg
104100110118     c                   eval      ILS46NFVA =  brvnfv
104200110118     c                   eval      ILS46FGSA =  brvfgs
104300110120     c                   eval      ILS46NPGB =  s_brvnpg
104400110120     c                   eval      ILS46NFVB =  s_brvnfv
104500110120     c                   eval      ILS46FGSB =  s_brvfgs
104600110121     c                   eval      ILS46AMB =  'QD'
104700110121     c                   eval      ILS46aupr=  'SE'
104800110120     c                   eval      ILS46FLS =  Kfls
104900110118     c                   eval      ILS46LNA =  Klna
105000110118     c                   eval      ILS46NRS =  Knrs
105100110118     c                   eval      ILS46NSC =  Knsc
105200110120     c                   eval      ILS46TELA = 'M'
105300110118     c*
105400110118     c                   call      'FNLS46R'
105500110118     c                   parm                    kpjba
105600110118     c                   parm                    fnls46ds
105700110118     c*
105800110118     C                   ENDSR
105900990204     C*--------------------------------------------------------------*
106000990204     C*  TESDAN: Test gestione danni
106100990204     C*--------------------------------------------------------------*
106200990204     C     TESDAN        BEGSR
106300990204     C*
106400990204     C*  Flag pratica danno = 'N'
106500990204     C                   MOVE      'N'           WDAVM             1
106600990204     C*  Se il collo non è stato spuntato su quel foglio di
106700990204     C*  uscita controllo se c'è una pratica C.A. aperta
106800990204     C     KDCD          CHAIN     FNDCD02L                           38
106900990204     C*  Se esiste C.A. aperta controllo il tipo di anomalia
107000990204     C     *IN38         IFEQ      '0'
107100990204     C     DCDATB        ANDNE     'A'
107200990204     C     DCDDCH        IFEQ      0
107300990204     C                   MOVE      TPC(1)        WTIPCA
107400990204     C* Aggancio testata
107500990204     C                   MOVE      DCDAAC        KAAC
107600990204     C                   MOVE      DCDFIL        KFIL
107700990204     C                   MOVE      DCDNCA        KNCA
107800990204     C     KDCT          CHAIN     FNDCT01L                           38
107900990204     C     *IN38         DOWEQ     '0'
108000990204     C     DCTATB        ANDEQ     'A'
108100990204     C     *IN38         OREQ      '0'
108200990204     C     DCTDCH        ANDNE     0
108300990204     C     KDCT          READE     FNDCT01L                               38
108400990204     C                   END
108500990204     C                   Z-ADD     1             YY                4 0
108600990204     C     DCTTAD        LOOKUP    TAD(YY)                                39
108700990204     C     *IN39         IFEQ      '1'
108800990204     C                   MOVEL     DTA(YY)       DTAD
108900990204     C     DRAGR         IFEQ      'M'
109000990204     C     DRAGR         OREQ      'V'
109100031009     C                   MOVE      'S'           WDAVM
109200990204     C                   END                                                    Manc.o Ava D/R
109300990204     C                   END                                                    Trv TAD
109400990204     C*
109500990204     C                   ELSE
109600990204     C                   MOVE      TPC(2)        WTIPCA
109700990204     C                   END                                                    CA Aperta
109800990204     C                   END                                                    CA no annul
109900990204     C*
110000990204     C                   ENDSR
110100950525     C*--------------------------------------------------------------*
110200950525     C*  GESANO: Gestione anomalie
110300950525     C*--------------------------------------------------------------*
110400950525     C     GESANO        BEGSR
110500950525     C*  Se non ho trovato errore bloccante
110600950911     C                   SETOFF                                       031417
110700031009     C                   Z-ADD     1             Y
110800950526     C     WCDANO        LOOKUP    C4H(Y)                                 32
110900950525     C     *IN32         IFEQ      '1'
111000950908     C     S4H(Y)        ANDNE     'N'
111100950908     C                   MOVEL     'S'           D59ANM
111200031009     C                   MOVEL     D4H(Y)        DESERR
111300950525     C*  Trovato errore bloccante !!
111400950525     C     T4H(Y)        IFEQ      'B'
111500950525     C                   MOVEL     'S'           WERRBL
111600030801     C*  Se in consegna su altra distinta
111700950529     C     C4H(Y)        IFEQ      '03'
111800950525     C                   MOVEL     'S'           WNODIS
111900950526     C                   SETON                                        03
112000950606     C                   MOVEL     *BLANKS       APDRSC
112100021204     C     kapd          CHAIN     fiapd01L                           37
112200031009     C                   MOVEL     APDRSC        WPADRO
112300950525     C                   END
112400950526     C                   END
112500950911     C*  Se ho sbloccato il C/Assegno senalo importo e divisa
112600950911     C     C4H(Y)        IFEQ      '07'
112700950911     C                   SETON                                        17
112800950911     C                   Z-ADD     0             AR9CAS
112900950911     C                   MOVEL     *BLANKS       AR9VCA
113000051115     C     KARB          CHAIN     FIAR901L                           37
113100950911     C                   END
113200030801     C*  Decodificare la consegna richiesta
113300950529     C     C4H(Y)        IFEQ      '10'
113400950526     C                   MOVEA     *BLANKS       RIC
113500950526     C                   Z-ADD     ARBDCR        G02INV
113600950526     C                   MOVEL     '3'           G02ERR
113700950526     C                   CALL      'XSRDA8'
113800950526     C                   PARM                    WLBDA8
113900950526     C                   MOVE      G02DAT        WDCR              8
114000950526     C                   MOVEA     WDCR          DCR
114100950526     C     ARBTCR        IFEQ      'D'
114200950526     C                   MOVEA     COST3         RIC
114300950526     C                   MOVEL     DCR(1)        RIC(9)
114400950526     C                   MOVEL     DCR(2)        RIC(10)
114500950526     C                   MOVEL     '/'           RIC(11)
114600950526     C                   MOVEL     DCR(3)        RIC(12)
114700950526     C                   MOVEL     DCR(4)        RIC(13)
114800950526     C                   MOVEL     '/'           RIC(14)
114900950526     C                   MOVEA     DCR(5)        RIC(15)
115000950526     C                   ELSE
115100950526     C                   MOVEA     COST4         RIC
115200950526     C                   MOVEL     DCR(1)        RIC(4)
115300950526     C                   MOVEL     DCR(2)        RIC(5)
115400950526     C                   MOVEL     '/'           RIC(6)
115500950526     C                   MOVEL     DCR(3)        RIC(7)
115600950526     C                   MOVEL     DCR(4)        RIC(8)
115700950526     C                   MOVEL     '/'           RIC(9)
115800950526     C                   MOVEA     DCR(5)        RIC(10)
115900950526     C                   END
116000950526     C                   MOVEA     RIC           WRIC             18
116100950526     C                   MOVE      WRIC          DESERR
116200950525     C                   END
116300950615     C*  Se non sono riuscita ad andare in consegna con l'ultima figlia
116400950615     C*  segnalo l'errore.
116500950615     C     C4H(Y)        IFEQ      '14'
116600950615     C     KLBL          SETGT     FNLBL02L
116700950615     C     KLBL          READPE    FNLBL02L                               32
116800950615     C                   SETON                                        14
116900950615     C                   END
117000030725     c* stampa testata
117100110112     c                   if        d59sqa =  ' ' or
117200110112     c                             d59sqa =  'T'
117300030728     c                   exsr      states
117400101124     c* stampa
117500030728     C                   WRITE     ERRARB
117600101201     c                   end
117700101124     c* video
117800101201     c                   if        d59sqa =  'S' or
117900101201     c                             d59sqa =  'T'
118000101124     c                   EVAL      tbeke2 = 'H' + C4H(Y)
118100101124     c                   exsr      srsqa
118200101124     c                   end
118300950612     C                   ADD       1             D59LIN
118400110414     C                   CLEAR                   TIPERR
118500960319     C                   Z-ADD     0             WAAS
118600960319     C                   Z-ADD     0             WLNP
118700960319     C                   Z-ADD     0             WNRS
118800960319     C                   Z-ADD     0             WNSP
118900030728     C                   Z-ADD     0             Wlna
119000030728     C                   Z-ADD     0             Wdat
119100030729     C                   Z-ADD     0             WNCD
119200030729     C                   Z-ADD     0             WNCA
119300030729     C                   Z-ADD     0             WZNC
119400030729     C                   MOVEL     *BLANKS       WRSD
119500030729     C                   MOVEL     *BLANKS       WBAR
119600950526     C                   END
119700950526     C*
119800950525     C*
119900950525     C                   ENDSR
120000950526     C*--------------------------------------------------------------*
120100030725     C*  STAMPA: Stampa testata
120200950526     C*--------------------------------------------------------------*
120300030728     C     STATES        BEGSR
120400110112     c                   if        d59sqa =  ' ' or
120500110112     c                             d59sqa =  'T'
120600030728     C* SE LA RIGA DI STAMPA È MAGGIORE DI SEI VUOL DIRE CHE HO GIÀ STAMPATO
120700030728     C                   IF        D59LIN = 0 OR
120800030728     C                             D59LIN > 54
120900030728     C                   SETON                                        80
121000030729     c                   else
121100030729     c* stampo testatina spunte doppie a metà pagina
121200030729     c                   if        *in20 and *in21
121300030729     c                   write     testad
121400030729     c                   write     testad1
121500030729     C                   ADD       4             D59LIN
121600030729     c                   setoff                                       21
121700030728     C                   END
121800030729     C                   END
121900030725     C*
122000030728     C                   IF        *IN80
122100030725     C                   ADD       1             D59PAG
122200030728     C                   WRITE     TESTA
122300050502     c                   if        d59lsr = 'S' and d59pag = 1
122400050422     C                   write     testal
122500050426     c                   end
122600030729     C                   Z-ADD     6             D59LIN
122700030728     C                   SETOFF                                       80
122800030728     C* SPUNTE DOPPIE
122900030729     C                   IF        *IN20
123000040930     C                   WRITE     TESTAD2
123100040930     C                   WRITE     TESTAD1
123200030911     C                   Z-ADD     7             D59LIN
123300030911     C                   SETOFF                                       21
123400030729     C                   else
123500030729     C                   WRITE     TESTA1
123600030728     C                   END
123700030729     C                   END
123800110112     C                   END
123900030728     C*
124000030725     C                   ENDSR
124100030725     C*--------------------------------------------------------------*
124200030725     C*  STAMPA: Stampa dettaglio segnacolli
124300030725     C*--------------------------------------------------------------*
124400030725     C     STAMPA        BEGSR
124500101126     C*  Se non è stato stampato "ANOMALIE DA SPUNTE:" lo stampo
124600101201     C                   IF        werrpz = 'N' and
124700101201     c                             (d59sqa =  ' ' or
124800101201     c                             d59sqa =  'T')
124900950526     C                   MOVEL     '12'          WCDANO
125000950526     C                   MOVEL     'S'           WERRPZ
125100950526     C                   EXSR      GESANO
125200950526     C                   END
125300950908     C*----------------------------------------------------*
125400950908     C*  Controllo se devo stampare le anomalie
125500101126     C*    S4H(Y)        IFNE      'N'
125600950908     C*----------------------------------------------------*
125700030725     c*
125800030725     c                   exsr      states
125900040419     c* Imposto il numero della pistola nel campo di stampa
126000950530     C                   MOVEA     WDETER        DER
126100040409     C     '.xx '        SCAN      WDETER        XX                2 0    01
126200040409     c                   if        *in01
126300950530     C                   ADD       1             XX
126400950530     C                   MOVEA     WNRP          DER(XX)
126500950530     C                   MOVEA     DER           WDETER
126600950530     C                   END
126700040419     c* Imposto il numero del foglio nel campo di stampa
126800040419     C                   MOVEA     WDETER        DER
126900040419     C     'xxx-'        SCAN      WDETER        XX                2 0    01
127000040419     c                   if        *in01
127100040421     c                   movel(p)  sfgs          com04             4
127200040421     C                   MOVEA     com04         DER(XX)
127300040420     C                   ADD       4             XX
127400040419     c                   movel     snpg          com01             1
127500040419     C                   MOVEA     com01         DER(XX)
127600040420     C                   ADD       2             XX
127700040419     c                   movel     snfv          com05             5
127800040419     C                   MOVEA     com05         DER(XX)
127900040419     C                   MOVEA     DER           WDETER
128000040419     C                   END
128100110117     c* Imposto l'ora del foglio nel campo di stampa
128200040419     C                   MOVEA     WDETER        DER
128300040419     C     'xx:'         SCAN      WDETER        XX                2 0    01
128400040419     c                   if        *in01
128500040419     c                   movel     shms          com02             2
128600040419     C                   MOVEA     com02         DER(XX)
128700040420     C                   ADD       3             XX
128800070130     c                   move      shms          com04             4
128900070130     c                   movel     com04         com02
129000040419     C                   MOVEA     com02         DER(XX)
129100040419     C                   MOVEA     DER           WDETER
129200040419     C                   END
129300040419     c*
129400950612     C                   ADD       1             D59LIN
129500101125     c* stampa
129600101201     c                   if        (d59sqa =  ' ' or
129700101201     c                             d59sqa =  'T')
129800950919     C     WTRSPU        IFEQ      'S'
129900030728     C                   EVAL      WDETER1 = WDETER
130000030728     C                   EVAL      WTIPCA1 = WTIPCA
130100030728     C                   WRITE     ERRAR1
130200950919     C                   ELSE
130300030728     C                   WRITE     ERRART
130400101125     C                   END
130500101201     c                   end
130600101124     c* video
130700101201     c                   if        (d59sqa =  'S' or
130800101201     c                             d59sqa =  'T')
130900101124     c                   exsr      srsqa
131000101124     c                   end
131100950908     C*----------------------------------------------------*
131200101126     C*                  END
131300950908     C*----------------------------------------------------*
131400950526     C                   ENDSR
131500950526     C*--------------------------------------------------------------*
131600950526     C*  STPTES: Stampo dati testata
131700950526     C*--------------------------------------------------------------*
131800950526     C     STPTES        BEGSR
131900030728     C* AUTOTRASPORTATORE
132000031009     C                   Z-ADD     D59PDR        PRTPDR
132100031009     C                   MOVEL(P)  D59DSF        PRTDSF
132200050422     C                   MOVEL(P)  D59DSF        PRTDSF2
132300030728     C* DISTINTA
132400031009     C                   Z-ADD     D59NDT        PRTNDT
132500110209     C                   Z-ADD     D59NDT        PRTNDT3
132600110209     C*                  eval      PRTNDT2 =d59bcd
132700110209     C                   move      d59bcd        PRTNDT2
132800031009     C                   Z-ADD     D59FGS        PRTFGS
132900030728     C                   Z-ADD     D59DDT        G02INV
133000030728     C                   MOVEL     '3'           G02ERR
133100030728     C                   CALL      'XSRDA8'
133200030728     C                   PARM                    WLBDA8
133300031009     C                   Z-ADD     G02DAT        STCDDC
133400950526     C*
133500030723     C                   MOVEL     CMAT          TPPRE
133600030723     C     D59FPP        IFEQ      'P'
133700031009     C                   MOVEL     CPOM          TPPRE
133800030723     C                   END
133900030728     C*
134000950526     C*
134100950526     C                   ENDSR
134200101124     C*-----------------------------------------------------*
134300101124     C* memorizzo l'errore nel file di work                 *
134400101124     C*-----------------------------------------------------*
134500101124     C     srsqa         BEGSR
134600101124     c                   clear                   dsqa
134700101124     c                   eval      tbecod = 'SQA'
134800101124     c                   eval      tbeke1 = 'FNLRF7R2'
134900101124     c     ktbe          chain     tntbe01l
135000101124     c                   if        %found(tntbe01l)
135100101124     c                   movel     tbeuni        dsqa
135200101124     c                   if        D§SQAELA = 'S'
135300101124     c* TESTATA
135400101124     c                   clear                   wfnlrq1ds
135500101124     c                   eval      wlrq1ndc = d59ndt
135600101124     c                   eval      wlrq1fgs = d59fgs
135700101124     c                   eval      wlrq1pdr = d59pdr
135800101125     c                   eval      wlrq1aas = s_arbAAS
135900101125     c                   eval      wlrq1LNP = s_arbLNP
136000101125     c                   eval      wlrq1NRS = s_arbNRS
136100101125     c                   eval      wlrq1NSP = s_arbNSP
136200101124     c*
136300101124     c     kwf10         chain     wfnlrq11l
136400101124     c                   if        not %found(wfnlrq11l)
136500101124     c                   movel     tbeke2        wlrq1cer
136600101124     c                   eval      wlrq1sce = D§SQASCE
136700101126     c                   if        D§SQATPD = 'S'
136800101126     c                   eval      wlrq1err = D§SQADES
136900101126     c                   else
137000101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
137100101126     c                   eval      wlrq1err = deserr
137200101126     c                   else
137300101126     c                   eval      wlrq1err = wdeter
137400101126     c                   end
137500101126     c                   end
137600101124     c                   write     wfnlrq10
137700101124     c                   else
137800101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
137900101124     c                   clear                   wlrq1cer
138000101124     c                   clear                   wlrq1sce
138100101124     c                   update    wfnlrq10
138200101124     c                   end
138300101124     c* dettaglio
138400101124     c                   clear                   wfnlrq2ds
138500101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
138600101124     c                   eval      wlrq2aas = wlrq1aas
138700101124     c                   eval      wlrq2lnp = wlrq1lnp
138800101124     c                   eval      wlrq2nrs = wlrq1nrs
138900101124     c                   eval      wlrq2nsp = wlrq1nsp
139000101124     c                   eval      wlrq2ndc = wlrq1ndc
139100101124     c                   eval      wlrq2fgs = wlrq1fgs
139200101124     c                   eval      wlrq2pdr = wlrq1pdr
139300101124     c                   movel     tbeke2        wlrq2cer
139400101124     c                   eval      wlrq2sce = D§SQASCE
139500101126     c                   if        D§SQATPD = 'S'
139600101126     c                   eval      wlrq2err = D§SQADES
139700101126     c                   else
139800101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
139900101126     c                   eval      wlrq2err = deserr
140000101126     c                   else
140100101126     c                   eval      wlrq2err = wdeter
140200101126     c                   end
140300101126     c                   end
140400101125     c* gli errori nella tabella 4H sono a livello di bolla quindi
140500101126     c* non ho le informazioni del segnacollo
140600101125     c                   if        %subst(tbeke2: 1: 1) <> 'H'
140700101124     c                   eval      wlrq2nsc = artnsc
140800110104     c                   eval      wlrq2fls = artfls
140900110126     c                   eval      wlrq2nrc = artnrs
141000110131     c                   if        tbeke2 <> '010'
141100110104     c                   eval      wlrq2flv = sfgs
141200101125     c                   eval      wlrq2nfv = snfv
141300110119     c                   eval      wlrq2dfv = sdfv
141400101125     c                   eval      wlrq2npg = snpg
141500101126     c                   eval      wlrq2hms = shms
141600101126     c                   if        wnrp <> ' '
141700101126     c                   movel     wnrp          wlrq2nps
141800101126     c                   end
141900110131     c                   end
142000110207     c*                  else
142100110207     c*                  eval      wlrq2flv = s_arbifp
142200110207     c*                  eval      wlrq2nfv = s_arbndc
142300110207     c*                  eval      wlrq2dfv = s_arbddc
142400110207     c*                  if        wlrq2nfv <> 0
142500110207     c*                  eval      wlrq2npg = 4
142600110207     c*                  end
142700101126     c                   end
142800101125     c*
142900101124     c                   write     wfnlrq20
143000101124     c                   end
143100101124     c                   end
143200101124     C                   ENDSR
143300950526     C*--------------------------------------------------------------*
143400950526     C*  DLTARZ: Cancello archivio dett.segnacolli/stop
143500950526     C*--------------------------------------------------------------*
143600950526     C     DLTARZ        BEGSR
143700950526     C*
143800950526     C     *LOVAL        SETLL     FNARBZ0F
143900950526     C                   READ      FNARBZ0F                               31
144000950526     C     *IN31         DOWEQ     '0'
144100950526     C                   DELETE    FNARBZ0F
144200950526     C                   READ      FNARBZ0F                               31
144300950526     C                   END
144400950526     C*
144500950526     C                   ENDSR
144600950614     C*--------------------------------------------------------------*
144700950614     C*  WRTANM: Scrivo anomalia collo fuori posto
144800950614     C*--------------------------------------------------------------*
144900950915     C     WRTANM        BEGSR
145000950915     C*
145100950919     C     ARTDCM        IFEQ      0
145200950915     C                   MOVEL     'N'           WAGANO            1
145300950918     C                   Z-ADD     WCAA          KCAA
145400990519     C                   CLEAR                   FNANM000
145500990519     C     KANM          CHAIN     FNANM02L                           34
145600950915     C     *IN34         DOWEQ     '0'
145700990519     C     ANMDAO        IFEQ      D59DDT
145800031009     C                   MOVEL     'S'           WAGANO
145900950915     C     ANMDCH        IFNE      0
146000960507     C                   MOVEL     WFASE         ANMFSA
146100960507     C                   Z-ADD     WFGS          ANMCDU
146200960507     C                   Z-ADD     WNFV          ANMNFV
146300950918     C                   Z-ADD     WNPS          ANMNPS
146400950915     C                   Z-ADD     0             ANMDCH
146500950915     C                   MOVEL     *BLANKS       ANMFSC
146600950915     C                   MOVEL     *BLANKS       ANMCCH
146700950915     C                   EXCEPT    AGGANM
146800960429     C                   ELSE
146900030721     C                   unlock    fnanm02l
147000950915     C                   END
147100950915     C                   END
147200990519     C     KANM          READE     FNANM02L                               34
147300950915     C                   END
147400950915     C*
147500950915     C     WAGANO        IFEQ      'N'
147600990519     C                   CLEAR                   FNANM000
147700960507     C                   MOVEL     WFASE         ANMFSA
147800960507     C                   Z-ADD     WFGS          ANMCDU
147900950915     C                   Z-ADD     ARTFLS        ANMFLS
148000950915     C                   Z-ADD     ARTNRS        ANMNRS
148100950915     C                   Z-ADD     ARTLNA        ANMLNA
148200950915     C                   Z-ADD     ARTNSC        ANMSCN
148300950915     C                   Z-ADD     ARBAAS        ANMAAS
148400950915     C                   Z-ADD     ARBLNP        ANMLNP
148500950915     C                   Z-ADD     ARBNSP        ANMNSP
148600950918     C                   Z-ADD     WCAA          ANMCAA
148700950918     C                   Z-ADD     WNFV          ANMNFV
148800950918     C                   Z-ADD     WNPS          ANMNPS
148900950915     C                   Z-ADD     D59DDT        ANMDAO
149000950915     C                   Z-ADD     SIMFEL        ANMFLE
149100950915     C                   MOVEL     'F'           ANMTAN
149200950915     C                   MOVEL     'S'           ANMSAN
149300950915     C                   MOVEL     'I'           ANMAIE
149400040721     c                   eval      anmft4 = §AR5BVA
149500040721     c                   movel     woggi         com6              6
149600040721     c                   clear                   comsei            6
149700040721     c                   eval      comsei = %subst(com6: 5: 2)+
149800040721     c                             %subst(com6: 3: 2)+
149900040721     c                             %subst(com6: 1: 2)
150000040721     c                   movel     comsei        §ANMDAC
150100040721     c                   movel     times         §ANMORC
150200040721     c                   movel     danmflo       anmflo
150300040721     c                   eval      anmfl4 = arbznc
150400040721     c                   if        arbznc=0
150500040721     c                   eval      anmfl4 = 999
150600040721     c                   end
150700990519     C                   WRITE     FNANM000
150800950915     C                   END
150900950919     C*
151000950919     C                   END                                                    ARTDCM = 0
151100950614     C*
151200950915     C                   ENDSR
151300980310     C*--------------------------------------------------------------*
151400980310     C*  WRTNIC: Leggo NIC provvisori e li scrivo in definitivo
151500980310     C*--------------------------------------------------------------*
151600980310     C     WRTNIC        BEGSR
151700980310     C*
151800980310     C* Controllo quali sono gli eventi di esclusi dalla consegna che
151900980310     C* non sono stati cancellati e li riporto in definitivo
152000130326     C     *LOVAL        SETLL     WFNEVB01L
152100130326     C                   READ      WfNEVB01L                              36
152200980310     C     *IN36         DOWEQ     '0'
152300031013     C                   MOVEL     DSEVB         DSEVBS
152400980310     C* Aggancio file eventi per controllare se l'evento di messa in
152500980310     C* consegna sulla bolla era già stato trasmesso
152600990803     C                   Z-ADD     EVBAAS        KAAS
152700980310     C                   Z-ADD     EVBLNP        KLNP
152800980310     C                   Z-ADD     EVBNRS        KNRS
152900980310     C                   Z-ADD     EVBNSP        KNSP
153000031020     C* riaggancio la spedizione dell'evento NIC rimasto
153100031020     C     KARB          CHAIN     FNARB01L                           33
153200100514     c                   clear                   ds7r
153300100514     c                   if        arbgma <> ' '
153400100514     c                   exsr      sr7r
153500100514     c                   end
153600031020     c* se spedizione con ritiro contestuale alla consegna
153700100514     c                   if        §7RRC= 'S' and not *in33
153800031020     c                   clear                   fnlr99ds
153900031020     c                   eval      i99fao = 390
154000031020     c                   exsr      AGGORM
154100031020     c                   end
154200031020     c* proseguo con la scrittura del NIC/cancellazione del MIC
154300980310     C                   MOVEL     'MIC'         KCEV
154400990803     C     KEVB2         SETGT     FNEVB04L
154500990803     C     KEVB2         READPE    FNEVB04L                               37
154600131218     C*--
154700980310     C     *IN37         IFEQ      '0'
154800990803     C     EVBDEV        ANDEQ     D59DDT
154900131218     C*--
155000980310     C* se l'evento MIC non era stato trasmesso lo annullo fisicamente
155100980310     C     EVBDTR        IFEQ      0
155200131218     C*
155300131218     C* riaggancia il record  FISICO x aggiornarlo
155400131218     c     evb_nrr       delete    fnevb00F
155500131218     C*--
155600131218     C**************     DELETE    FNEVB04L
155700131218     C*
155800980310     C                   ELSE
155900131218     C*
156000031029     C                   MOVEL     EVBDEV        WDTMIC           12 0
156100031029     C                   MOVE      EVBHEV        WDTMIC
156200980312     C* altrimenri controllo se esiste già un evento NIC con data = a
156300980312     C* quella del foglio di consegna
156400990803     C                   Z-ADD     EVBAAS        KAAS
156500980312     C                   Z-ADD     EVBLNP        KLNP
156600980312     C                   Z-ADD     EVBNRS        KNRS
156700980312     C                   Z-ADD     EVBNSP        KNSP
156800980312     C                   MOVEL     'NIC'         KCEV
156900990803     C     KEVB2         SETGT     FNEVB04L
157000990803     C     KEVB2         READPE    FNEVB04L                               37
157100131218      *--
157200980312     C     *IN37         IFEQ      '0'
157300990803     C     EVBDEV        ANDEQ     D59DDT
157400131218      *--
157500980312     C* Se c'è evento NIC controllo se già trasmesso
157600980312     C     EVBDTR        IFEQ      0
157700131218     C*
157800131218     C* riaggancia il record  FISICO x aggiornarlo
157900131218     c     evb_nrr       chain     fnevb00F
158000131218      *--
158100131218     c                   if        %Found(fnevb00F)
158200980312     C* Se non è ancora stato trasmesso aggiorno data/ora immissione
158300980312     C                   Z-ADD     D59AMG        EVBDTV
158400980312     C                   TIME                    EVBORV
158500980312     C                   Z-ADD     D59FGS        EVBFLE
158600980312     C                   MOVE      D59CDU        EVBCDU
158700980312     C     FVVFPP        IFEQ      'M'
158800980312     C                   MOVEL     0800          EVBHEV
158900980312     C                   ELSE
159000980312     C                   MOVEL     1200          EVBHEV
159100980312     C                   END
159200990803     C                   Z-ADD     0             EVBDTC
159300131218      *--
159400990803     C                   UPDATE    FNEVB000
159500131218     c                   end
159600131218      *--
159700980312     C                   ELSE
159800131218      *--
159900980312     C* Se è già stato trasmesso controllo se data/ora immissione
160000980312     C* > MIC
160100031029     C                   MOVEL     EVBDEV        WDTNIC           12 0
160200031029     C                   MOVE      EVBHEV        WDTNIC
160300031029     C     WDTNIC        IFLE      WDTMIC
160400980312     C* Se la data non è più alta scrivo un nuovo NIC
160500031013     C                   MOVEL     DSEVBS        DSEVB
160600990803     C                   WRITE     FNEVB000
160700980312     C                   END                                                    Data MIC >= NIC
160800980312     C*
160900980312     C                   END                                                    NIC Trasmesso
161000980312     C*
161100980312     C                   ELSE                                                   Nic Trovato
161200031013     C                   MOVEL     DSEVBS        DSEVB
161300990803     C                   WRITE     FNEVB000
161400980312     C                   END                                                    Nic Trovato
161500980312     C                   END                                                    Mic trasmesso
161600980312     C                   END                                                    Mic Trovato
161700980310     C* Cancello evento provvisiorio
161800990803     C                   DELETE    FNEVBPRV
161900980310     C* leggo evento provvisiorio successivo
162000130326     C                   READ      WFNEVB01L                              36
162100980310     C                   END
162200980310     C*
162300980310     C                   ENDSR
162400030805     C**********************************************************************
162500030805     C* calcolo il numero dei bancali   -----------------------------*
162600030805     C**********************************************************************
162700030805     C     srnban        BEGSR
162800030805      * Bancali
162900030805     c                   If        %Subst(Arbgva:1:1) = 'B'
163000030805     c                   Clear                   dAr5Ban
163100030805     C                   move      'BAN'         ar5trd
163200030805     c     kFiar5        Chain     Fiar501l
163300030805     c                   If        %Found(Fiar501l)
163400030805     c                   Movel     Ar5Uni        dAr5Ban
163500030805     c                   EndIf
163600030805     c                   add       §Ar5Nba       nban
163700030805     c                   add       §ar5Nb2       nban
163800030805     c                   EndIf
163900030805      * Bancali Bartolini
164000030805     c                   If        %Subst(Arbgva:1:1) = 'O'
164100030805     c                   Clear                   dAr5BNB
164200030805     C                   move      'BNB'         ar5trd
164300030805     c     kFiar5        Chain     Fiar501l
164400030805     c                   If        %Found(Fiar501l)
164500030805     c                   Movel     Ar5Uni        dAr5BNB
164600030805     c                   EndIf
164700030805     c                   add       §Ar5bNba      nban
164800030805     c                   add       §ar5bNb2      nban
164900030805     c                   EndIf
165000040721      * Merce di valore
165100040721     c                   Clear                   dAr5gen
165200040721     C                   move      'GEN'         ar5trd
165300040721     c     kFiar5        Chain     Fiar501l
165400040721     c                   If        %Found(Fiar501l)
165500040721     c                   Movel     Ar5Uni        dAr5gen
165600040721     c                   EndIf
165700030805     c                   ENDSR
165800010620     C*--------------------------------------------------------------*
165900010620     C*  DOPPIE: STAMPO EVENTUALI SPUNTE DI CONSEGNA DOPPIE
166000010620     C*--------------------------------------------------------------*
166100010620     C     DOPPIE        BEGSR
166200010620     C*
166300010620     C                   Z-ADD     D59NDT        KNFV
166400010620     C                   Z-ADD     4             KNPG
166500030729     C                   SETON                                        2021
166600010620     C*
166700070129     C     KBRVE         SETLL     FNBRVE1L
166800070129     C     KBRVE         READE     FNBRVE1L                               30
166900010620    2C     *IN30         DOWEQ     *OFF
167000010620     C                   CLEAR                   WGIR
167100010620     C                   CLEAR                   SAWNPS
167200010622     C                   CLEAR                   SAWNFV
167300010622     C                   CLEAR                   SAWFGS
167400130327     C                   SETOFF                                       19
167500070129     C     KBRVE2        SETLL     FNBRVE2L
167600070129     C     KBRVE2        READE     FNBRVE2L                               31
167700010620    3C     *IN31         DOWEQ     *OFF
167800010620     C* Considero solo se data foglio >= data distinta in quadratura
167900010702    4C     D59FGS        IFNE      BREFGS
168000010702     C     KNFV          ORNE      BRENFV
168100010620     C                   CLEAR                   DSLV53
168200010620     C                   MOVEL     'V'           D53TFO
168300010620     C                   MOVEL     KNPG          D53NPG
168400010620     C                   Z-ADD     SIMFEL        D53FEL
168500010702     C                   Z-ADD     BRENFV        D53NFV
168600010702     C                   Z-ADD     BREFGS        D53FGS
168700010620     C                   CALL      'FNLV53R'
168800010620     C                   PARM                    DSLV53
168900010702     C                   Z-ADD     D53DFV        SA1DFV
169000010620     C                   ELSE
169100010702     C                   Z-ADD     D59DDT        SA1DFV
169200010620    4C                   ENDIF
169300010702    4C     D59FGS        IFEQ      BREFGS
169400010702     C     KNFV          ANDEQ     BRENFV
169500010620     C     D53ERR        OREQ      *BLANKS
169600010702     C     SA1DFV        ANDGE     D59DDT
169700010703    5C     SAWNFV        IFGT      *ZEROS
169800010622     C* Stessa pistola e foglio: devo verificare se superati i minuti
169900010622     c* di tolleranza previsti fra una spunta e l'altra
170000010620     C                   CLEAR                   WPRT
170100010702    6C     SAWNPS        IFEQ      BRENPS
170200010702     C     SAWNFV        ANDEQ     BRENFV
170300010702     C     SAWFGS        ANDEQ     BREFGS
170400010620     C                   CLEAR                   WXDTOR
170500010702     C                   MOVEL     BREDFS        WXDT1
170600010702     C                   MOVEL     BREHMS        WXHM1
170700010620     C                   MOVEL     SAWDFS        WXDT2
170800010620     C                   MOVEL     SAWHMS        WXHM2
170900010620     C                   MOVEL     'D'           WXFLG
171000010620     C                   CALL      'XDTORA'
171100010620     C                   PARM                    WXDTOR
171200010620     C     WXMIN         IFGT      §7AMNP
171300010620     C                   MOVEL     '1'           WPRT
171400010620     C                   ENDIF
171500010620   x6C                   ELSE
171600010622     c* se pistole o foglio diversi stampo sempre
171700010620     C                   MOVEL     '1'           WPRT              1
171800010620    6C                   ENDIF
171900010620    6C     WPRT          IFEQ      '1'
172000010620     c* se sono al primo giro di stampa devo stampare sia la spunta pre
172100010620     c* cedente che la spunta successiva
172200010620    7C     WGIR          IFEQ      ' '
172300010620     C                   MOVEL     DSBRVE        W1BRVE
172400010620     C                   MOVEL     WSBRVE        DSBRVE
172500010620     C                   Z-ADD     SAWDFV        WDFV
172600030729     C                   SETOff                                       19
172700010620     C                   EXSR      SPRDOP
172800010620     C                   MOVEL     W1BRVE        DSBRVE
172900010620     C                   MOVE      '1'           WGIR              1
173000010620    7C                   ENDIF
173100010702     C                   Z-ADD     SA1DFV        WDFV
173200030729     C                   SETON                                        19
173300010620     C                   EXSR      SPRDOP
173400010620   X6C                   ELSE
173500010620     C                   MOVEL     DSBRVE        WSBRVE
173600010620    6C                   ENDIF
173700010620   X5C                   ELSE
173800010620     C* se sono al primo giro di lettura memorizzo la spunta
173900010620     C                   MOVEL     DSBRVE        WSBRVE
174000010620    5C                   ENDIF
174100010620     C* memorizzo in ogni caso il numero pistola e data/ora fisica spun
174200010702     C                   Z-ADD     BRENPS        SAWNPS
174300010702     C                   Z-ADD     BREFGS        SAWFGS
174400010702     C                   Z-ADD     BRENFV        SAWNFV
174500010702     C                   Z-ADD     BREDFS        SAWDFS
174600010702     C                   Z-ADD     BREHMS        SAWHMS
174700010702     C                   Z-ADD     SA1DFV        SAWDFV
174800010620    4C                   ENDIF
174900070129     C     KBRVE2        READE     FNBRVE2L                               31
175000010620    3C                   ENDDO
175100010620     C* Mi posiziono sempre su un nuovo segnacollo
175200070129     C     KBRVE1        SETGT     FNBRVE1L
175300070129     C     KBRVE         READE     FNBRVE1L                               30
175400010620    2C                   ENDDO
175500010711     C*
175600010620     C                   ENDSR
175700010620     C*
175800010620     C*--------------------------------------------------------------*
175900010620     C* STAMPA SPUNTA DOPPIA
176000010620     C*--------------------------------------------------------------*
176100010620     C     SPRDOP        BEGSR
176200010712     C                   MOVEL     'S'           D59ANM
176300030728     C                   EXSR      STATES
176400010711     C*
176500010702     C                   CLEAR                   P1CMSG
176600030729     C* SEGNACOLLO SOLO PER 19 Off
176700030729     C     *IN19         IFEQ      *Off
176800010702     C                   Z-ADD     BRELNP        P1CLNP
176900010702     C                   Z-ADD     BRELNA        P1CLNA
177000010702     C                   Z-ADD     BRENRS        P1CNRS
177100010702     C                   Z-ADD     BRENSC        P1CNSC
177200010702     C* VEDO SE COLLO IN IMA
177300010702     C                   Z-ADD     3             KNPG
177400070129     C     KBRV9         SETGT     FNBRV09L
177500070129     C     KBRV9         READPE    FNBRV09L                               32
177600010702     C     *IN32         DOWEQ     *OFF
177700010702     C     P1CMSG        ANDEQ     *BLANKS
177800010702     C* IL foglio deve avere sottocategoria arrivi ed avere data foglio
177900010702     C* maggiore uguale alla data del foglio in fase di quadratura
178000010702     C                   CLEAR                   DSLV53
178100010702     C                   MOVEL     'V'           D53TFO
178200010702     C                   MOVEL     KNPG          D53NPG
178300010702     C                   Z-ADD     SIMFEL        D53FEL
178400010702     C                   Z-ADD     BRVNFV        D53NFV
178500010702     C                   Z-ADD     BRVFGS        D53FGS
178600010702     C                   CALL      'FNLV53R'
178700010702     C                   PARM                    DSLV53
178800010702     C     D53ERR        IFEQ      *BLANKS
178900010702     C     D53DFV        ANDGE     D59DDT
179000010702     C     D53SPG        ANDEQ     'A'
179100010702     C                   MOVEA     COS           W33
179200010702     C                   MOVE      BRVFGS        W003A             3
179300010702     C                   MOVEA     W003A         W33(8)
179400010702     C                   MOVE      BRVNFV        W006A             6
179500010712     C                   EXSR      EDITA
179600010702     C                   MOVEA     W006A         W33(12)
179700010702     C                   MOVE      D53DFV        W0040             4 0
179800010702     C                   MOVEL     W0040         WMMDFV            2
179900010702     C                   MOVE      W0040         WGGDFV            2
180000010702     C                   MOVEA     WGGDFV        W33(23)
180100010702     C                   MOVEA     WMMDFV        W33(26)
180200010702     C                   MOVE      BRVNPS        W002A             2
180300010702     C                   MOVEA     W002A         W33(32)
180400031009     C                   MOVEA     W33           P1CMSG
180500010702     C                   ENDIF
180600070129     C     KBRV9         READPE    FNBRV09L                               32
180700010702     C                   ENDDO
180800010620     C                   ENDIF
180900110518     C* Reimposto KNPG in quanto usato nel ciclo di lettura di fnbrve
181000110518     C                   Z-ADD     4             KNPG
181100010620     C*
181200010702     C                   Z-ADD     BREFGS        P1CFGS
181300010702     C                   Z-ADD     BRENFV        P1CNFV
181400010620     C* DATA DISTINTA
181500010620     C                   Z-ADD     WDFV          G02INV
181600010620     C                   MOVE      *ZEROS        G02DAT
181700010620     C                   MOVEL     3             G02ERR
181800010620     C                   CALL      'XSRDA8'
181900010620     C                   PARM                    WLBDA8
182000010620     C                   Z-ADD     G02DAT        P1CDFV
182100010620     C* AUTOTRASPORTATORE
182200110518     C     KDST          CHAIN     FiDST01L                           32
182300010620    1C     *IN32         IFEQ      *OFF
182400010711     C                   Z-ADD     DSTPDR        P1CPDR
182500021204     C     kapdd         CHAIN     fiapd01L                           32
182600010620    2C     *IN32         IFEQ      *OFF
182700031009     C                   MOVEL     APDRSC        P1DPDR
182800010620   X2C                   ELSE
182900010620     C                   CLEAR                   P1DPDR
183000010620    2C                   ENDIF
183100010620   X1C                   ELSE
183200010620     C                   CLEAR                   P1CPDR
183300010620     C                   ENDIF
183400010620     C* DATA IMMISSIONE
183500010702     C                   Z-ADD     BREDFS        G02INV
183600010620     C                   MOVE      *ZEROS        G02DAT
183700010620     C                   MOVEL     3             G02ERR
183800010620     C                   CALL      'XSRDA8'
183900010620     C                   PARM                    WLBDA8
184000010620     C                   Z-ADD     G02DAT        P1CDFS
184100010620     C*
184200010702     C                   Z-ADD     BREHMS        P1CHMS
184300010702     C                   Z-ADD     BRENPS        P1CNPS
184400030728     C                   WRITE     ERRDOP
184500010711     C                   ADD       1             D59LIN
184600010620     C                   ENDSR
184700010712     C*--------------------------------------------------------------*
184800010712     C* SOPPRESSIONE ZERI NON SIGNIFICATIVI DA CAMPO LUNGO 6
184900010712     C*--------------------------------------------------------------*
185000010712     C     EDITA         BEGSR
185100010712     C*
185200010712     C                   MOVE      *BLANKS       WFINE             1
185300010712     C                   MOVEA     W006A         W06
185400010712     C                   Z-ADD     1             II                1 0
185500010712     C     WFINE         DOWEQ     *BLANKS
185600010712     C     II            ANDLE     5
185700010712     C     W06(II)       IFEQ      '0'
185800010712     C                   MOVE      *BLANKS       W06(II)
185900010712     C                   ELSE
186000010712     C                   MOVE      '1'           WFINE
186100010712     C                   END
186200010712     C                   ADD       1             II
186300010712     C                   ENDDO
186400010712     C                   MOVEA     W06           W006A
186500010712     C                   ENDSR
186600950526     C*--------------------------------------------------------------*
186700950526     C*  *INZSR: Operazioni iniziali
186800950526     C*--------------------------------------------------------------*
186900950526     C     *INZSR        BEGSR
187000950526     C*------------------------------------------------------*
187100030722     c     *dtaara       define    §azute        azuteds
187200030722     c     *dtaara       define    §datiute      ddatiute
187300030722     C                   in(E)     *dtaara
187400030722     C                   IF        %Error  or  RSUT = *blanks
187500030722     C                   call      'TIBS34R'
187600030722     C                   parm                    Tibs34Ds
187700030722     C                   in        *dtaara
187800030722     c                   ENDIF
187900950612     C                   MOVE      SIMFEL        KFLE
188000950526     C*------------------------------------------------------*
188100950526     C*  Definisco chiavi di accesso
188200110518     C     KDST          KLIST
188300110518     C                   KFLD                    knpg
188400110518     C                   KFLD                    brenfv
188500110518     C                   KFLD                    BREFGS
188600030805     C     Kfiar5        KLIST
188700030805     C                   KFLD                    arbaas
188800030805     C                   KFLD                    arblnp
188900030805     C                   KFLD                    arbnrs
189000030805     C                   KFLD                    arbnsp
189100030805     C                   KFLD                    ar5trd
189200950526     C     KARB          KLIST
189300950526     C                   KFLD                    KAAS
189400950526     C                   KFLD                    KLNP
189500950526     C                   KFLD                    KNRS
189600950526     C                   KFLD                    KNSP
189700950606     C     KLBL          KLIST
189800950606     C                   KFLD                    KAAS
189900950606     C                   KFLD                    KLNP
190000950606     C                   KFLD                    KNRS
190100950606     C                   KFLD                    KNSP
190200950526     C     KART          KLIST
190300950526     C                   KFLD                    KAAS
190400950526     C                   KFLD                    KLNP
190500950526     C                   KFLD                    KNRS
190600950526     C                   KFLD                    KNSP
190700950605     C     KFVV          KLIST
190800950605     C                   KFLD                    KNPG
190900950605     C                   KFLD                    KNFV1
191000080229     C                   KFLD                    brvfgs
191100950526     C     KBRV1         KLIST
191200950526     C                   KFLD                    KNPG
191300950526     C                   KFLD                    KNFV
191400950526     C                   KFLD                    KFGS
191500950526     C                   KFLD                    KFLS
191600950526     C                   KFLD                    KLNA
191700950526     C                   KFLD                    KNRS
191800950526     C                   KFLD                    KNSC
191900950526     C     KBRV2         KLIST
192000950526     C                   KFLD                    KNPG
192100950526     C                   KFLD                    KFLS
192200950526     C                   KFLD                    KLNA
192300950526     C                   KFLD                    KNRS
192400950526     C                   KFLD                    KNSC
192500040420     C                   KFLD                    KFLE
192600010702     C     KBRV9         KLIST
192700010702     C                   KFLD                    KNPG
192800010702     C                   KFLD                    BRELNP
192900010702     C                   KFLD                    BRELNA
193000010702     C                   KFLD                    BRENRS
193100010702     C                   KFLD                    BRENSC
193200010702     C                   KFLD                    KFLE
193300990204     C     KDCD          KLIST
193400990204     C                   KFLD                    KFLS
193500990204     C                   KFLD                    KLNA
193600990204     C                   KFLD                    KNRS
193700990204     C                   KFLD                    KNSC
193800990204     C     KDCT          KLIST
193900990204     C                   KFLD                    KAAC
194000990204     C                   KFLD                    KFIL
194100990204     C                   KFLD                    KNCA
194200950526     C     KTAB          KLIST
194300950526     C                   KFLD                    KKUT
194400950526     C                   KFLD                    KCOD
194500010620     C     KTAB1         KLIST
194600010620     C                   KFLD                    KKUT
194700010620     C                   KFLD                    KCOD
194800010620     C                   KFLD                    KKEY
194900950915     C     KANM          KLIST
195000950915     C                   KFLD                    KFLS
195100950915     C                   KFLD                    KLNA
195200950915     C                   KFLD                    KNRS
195300950915     C                   KFLD                    KNSC
195400950915     C                   KFLD                    KCAA
195500980310     C     KEVB1         KLIST
195600990803     C                   KFLD                    KAAS
195700980310     C                   KFLD                    KLNP
195800980310     C                   KFLD                    KNRS
195900980310     C                   KFLD                    KNSP
196000980310     C     KEVB2         KLIST
196100990803     C                   KFLD                    KAAS
196200980310     C                   KFLD                    KLNP
196300980310     C                   KFLD                    KNRS
196400980310     C                   KFLD                    KNSP
196500980310     C                   KFLD                    KCEV
196600010620     C     KBRVE         KLIST
196700010620     C                   KFLD                    KNPG
196800010620     C                   KFLD                    KNFV
196900010620     C                   KFLD                    D59FGS
197000010620     C     KBRVE1        KLIST
197100010620     C                   KFLD                    KNPG
197200010620     C                   KFLD                    KNFV
197300010620     C                   KFLD                    D59FGS
197400010702     C                   KFLD                    BRELNP
197500010702     C                   KFLD                    BRELNA
197600010702     C                   KFLD                    BRENRS
197700010702     C                   KFLD                    BRENSC
197800010620     C     KBRVE2        KLIST
197900010702     C                   KFLD                    BRENPG
198000010702     C                   KFLD                    BRELNP
198100010702     C                   KFLD                    BRELNA
198200010702     C                   KFLD                    BRENRS
198300010702     C                   KFLD                    BRENSC
198400021204     C*
198500021204     C     Kapd          KLIST
198600021204     C                   KFLD                    apdtip
198700021204     C                   KFLD                    arbpdc
198800021204     C*
198900021204     C     Kapd59        KLIST
199000021204     C                   KFLD                    apdtip
199100021204     C                   KFLD                    d59pdr
199200021204     C*
199300021204     C     Kapdd         KLIST
199400021204     C                   KFLD                    apdtip
199500021204     C                   KFLD                    dstpdr
199600021204     c                   movel     'A'           apdtip
199700101124     C     Ktbe          KLIST
199800101124     C                   KFLD                    tbecod
199900101124     C                   KFLD                    tbeke1
200000101124     C                   KFLD                    tbeke2
200100101124     C     Kwf10         KLIST
200200101124     C                   KFLD                    WLRQ1AAS
200300101124     C                   KFLD                    WLRQ1LNP
200400101124     C                   KFLD                    WLRQ1NRS
200500101124     C                   KFLD                    WLRQ1NSP
200600101124     C                   KFLD                    WLRQ1FGS
200700101124     C                   KFLD                    WLRQ1NDC
200800101124     C                   KFLD                    WLRQ1PDR
200900950526     C*------------------------------------------------------*
201000950526     C*  Definisco variabili
201100030728     C     *LIKE         DEFINE    BRVNPS        BRVNPS1
201200030728     C     *LIKE         DEFINE    D59NDT        EXNDT
201300950526     C     *LIKE         DEFINE    ARTAAS        KAAS
201400950526     C     *LIKE         DEFINE    ARTLNP        KLNP
201500950526     C     *LIKE         DEFINE    ARTNRS        KNRS
201600950526     C     *LIKE         DEFINE    ARTNSP        KNSP
201700950526     C     *LIKE         DEFINE    ARTFLS        KFLS
201800950526     C     *LIKE         DEFINE    ARTLNA        KLNA
201900950526     C     *LIKE         DEFINE    ARTNSC        KNSC
202000950526     C     *LIKE         DEFINE    BRVNPG        KNPG
202100950526     C     *LIKE         DEFINE    BRVNFV        KNFV
202200950526     C     *LIKE         DEFINE    BRVFGS        KFGS
202300950612     C     *LIKE         DEFINE    BRVFLE        KFLE
202400951121     C     *LIKE         DEFINE    ARBRAG        KRAG
202500950526     C     *LIKE         DEFINE    TBLKUT        KKUT
202600950526     C     *LIKE         DEFINE    TBLCOD        KCOD
202700950915     C     *LIKE         DEFINE    ANMCAA        KCAA
202800980310     C     *LIKE         DEFINE    EVBCEV        KCEV
202900950918     C     *LIKE         DEFINE    ANMCAA        WCAA
203000950918     C     *LIKE         DEFINE    ANMNFV        WNFV
203100950918     C     *LIKE         DEFINE    ANMNPS        WNPS
203200960507     C     *LIKE         DEFINE    FVVFGS        WFGS
203300950605     C     *LIKE         DEFINE    FVVNFV        KNFV1
203400990204     C     *LIKE         DEFINE    TBECOD        KCODE
203500990204     C     *LIKE         DEFINE    DCTAAC        KAAC
203600990204     C     *LIKE         DEFINE    DCTFIL        KFIL
203700990204     C     *LIKE         DEFINE    DCTNCA        KNCA
203800010620     C     *LIKE         DEFINE    DSBRVE        WSBRVE
203900010620     C     *LIKE         DEFINE    DSBRVE        W1BRVE
204000010620     C     *LIKE         DEFINE    BRVNPS        SAWNPS
204100010622     C     *LIKE         DEFINE    BRVFGS        SAWFGS
204200010622     C     *LIKE         DEFINE    BRVNFV        SAWNFV
204300010620     C     *LIKE         DEFINE    BRVDFS        SAWDFS
204400010620     C     *LIKE         DEFINE    BRVHMS        SAWHMS
204500010620     C     *LIKE         DEFINE    D53DFV        SAWDFV
204600010620     C     *LIKE         DEFINE    D53DFV        WDFV
204700010702     C     *LIKE         DEFINE    D53DFV        SA1DFV
204800010620     C     *LIKE         DEFINE    TBLKEY        KKEY
204900950526     C*
205000950526     C                   TIME                    TIMES             6 0
205100950526     C                   TIME                    WHDAT            12 0
205200950526     C                   MOVE      WHDAT         WOGGI             6 0
205300010622     C                   TIME                    W0140            14 0
205400010622     C                   MOVE      W0140         WDTGIO            8 0
205500031009     C                   MOVEL     *ALL'-'       CMP132
205600031009     C                   MOVEL     *ALL'-'       CMP198
205700031009     C                   MOVEL     *ALL'-'       CMP132D
205800031009     C                   MOVEL     *ALL'-'       CMP198D
205900950526     C*
206000030722     C*  Carico dati tabella 4H anomalie da spunte
206100950526     C                   Z-ADD     1             KKUT
206200950526     C                   MOVEL     '4H'          KCOD
206300070601     C                   Z-ADD     0             X                 3 0
206400950526     C     KTAB          CHAIN     TABEL00F                           31
206500950526     C     *IN31         DOWEQ     '0'
206600950526     C     X             ANDLT     100
206700950526     C                   ADD       1             X
206800950526     C                   MOVEL     TBLUNI        DS4H
206900950526     C                   MOVEL     TBLKEY        C4H(X)
207000950526     C                   MOVEL     §4HDES        D4H(X)
207100950526     C                   MOVEL     §4HTPB        T4H(X)
207200950908     C                   MOVEL     §4HSTP        S4H(X)
207300950526     C     KTAB          READE     TABEL00F                               31
207400950526     C                   END
207500010620     C* Chaino tabella variabili pgm arrivi per reperire minuti di
207600010620     C* tolleranza
207700010620     C                   MOVEL     '7A'          KCOD
207800010620     C                   MOVEL(P)  '2       '    KKEY
207900010620     C     KTAB1         CHAIN     TABEL                              31
208000010620     C  N31              MOVEL     TBLUNI        DS7A2
208100010620     C   31              Z-ADD     1             §7AMNP
208200030728     C                   OPEN      FNLRF7P
208300030722     C*  Carico dati tabella DTA anomalie danni
208400990204     C                   Z-ADD     1             KKUT
208500990204     C                   MOVEL     'TAD'         KCODE
208600990204     C                   Z-ADD     0             X
208700990204     C     KCODE         CHAIN     TNTBE01L                           31
208800990204     C     *IN31         DOWEQ     '0'
208900990204     C     X             ANDLT     100
209000990204     C     TBESIF        IFEQ      KNSIF
209100990204     C     TBESIF        OREQ      *BLANKS
209200990204     C                   ADD       1             X
209300990204     C                   MOVEL     TBEKE1        TAD(X)
209400990204     C                   MOVEL     TBEUNI        DTA(X)
209500990204     C                   END
209600990204     C     KCODE         READE     TNTBE01L                               31
209700990204     C                   END
209800030703     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
209900040827     C                   CLEAR                   FIP
210000040827     C     *LOVAL        SETLL     AZORG01L
210100040827     C                   DO        *HIVAL
210200040827     C                   READ      AZORG01L                               95
210300040827     C   95              LEAVE
210400040827     C                   MOVEL     ORGDE3        OG143
210500040827     C     §OgNTW        IFEQ      'PPT'
210600040827     C                   Z-ADD     1             z                 3 0
210700040827     C     *ZEROS        LOOKUP    FIP(z)                                 23
210800040827     C   23              Z-ADD     ORGFIL        FIP(z)
210900040827     C                   END
211000040827     C                   END
211100950526     C*
211200950526     C                   ENDSR
211300950526     O*--------------------------------------------------------------*
211400950526     O*   STAMPA                                                     *
211500950526     O*--------------------------------------------------------------*
211600030728     OSYSPRT    E            SPED1       1
211700990803     O                       EVBAAS        Z      4
211800980325     O                       EVBLNP        Z     16
211900980325     O                       EVBNRS        Z     19
212000980325     O                       EVBNSP        Z     27
212100980325     O                                           80 'DELETE ERRATO'
212200950526     O*--------------------------------------------------------------*
212300950526     O*   AGGIORNAMENTI                                              *
212400950526     O*--------------------------------------------------------------*
212500951109     OFNARB000  E            AGGARB
212600950526     O                       ARBDDC
212700950526     O                       ARBNDC
212800020917     O                       ARBIFP
212900950526     O                       ARBPDC
213000950526     O                       ARBSTP
213100140410     O                       ARBNMI
213200140410     O                       ARBFDC
213300140410     O                       ARBNSS
213400990519     OFNANM000  E            AGGANM
213500950915     O                       ANMDCH
213600950915     O                       ANMFSC
213700950915     O                       ANMCCH
213800950918     O                       ANMNFV
213900950918     O                       ANMNPS
214000010702**
214100040409collo                    spuntato su distinta Nr.       pistola nr.xx
214200040421collo  NON + USATO       non spuntato in uscita ma arrivato pistola nr.xx
214300040421collo  NON + USATO       non spuntato in uscita non arrivato ma partito pistola nr.xx
214400040415VUOTO
214500040421collo  NON + USATO       non spuntato in uscita non arrivato e non partito
214600040419collo                    spuntato in uscita pistola nr.xx ore xx:xx ma già consegnato
214700040415collo                    spuntato in uscita pistola nr.xx ore xx:xx
214800040415                         e su foglio xxx-x-xxxxx pistola nr.xx ore xx:xx
214900040419collo                    non spuntato in uscita ma su f. xxx-x-xxxxx pist. nr.xx ore xx:xx
215000081013collo                    collo doppio
215100040421VUOTO
215200040419collo                    NON IN DISTINTA ma spuntato in uscita pistola nr.xx ore xx:xx
215300101124VUOTO
215400990204**
215500040421        Pratica C.A. aperta:  NON + USATO
215600040421        Pratica C.A. chiusa:  NON + USATO
215700010702**
215800010702In IMA XXX XXXXXX Del XX/XX PstXX
