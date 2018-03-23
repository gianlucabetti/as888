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
002800030721     FFNEVB04L  UF A E           K DISK    commit
002900130326     FwFNEVB01L UF   E           K DISK
003000990803     F                                     RENAME(FNEVB000:FNEVBPRV)
003100070129     FFNBRVE1L  IF   E           K DISK
003200070129     F                                     RENAME(FNBRV000:FNBRVE01)
003300070129     F                                     prefix(BRE:3)
003400070129     FFNBRVE2L  IF   E           K DISK
003500070129     F                                     RENAME(FNBRV000:FNBRVE02)
003600070129     F                                     prefix(BRE:3)
003700110518     FFiDST01L  IF   E           K DISK
003800030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
003900950612     F                                     USROPN
004000980325     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
004100101124     Fwfnlrq11l uf a E           k DISK
004200101124     Fwfnlrq20f o    E             DISK
004300950525      *--------------------------------------------------------------*
004400950525      * Schiere
004500950525      *--------------------------------------------------------------*
004600950526     D RIC             S              1    DIM(18)
004700950526     D DCR             S              1    DIM(8)
004800950525     D CDA             S              2    DIM(10)
004900950525     D C4H             S              2    DIM(100)
005000950525     D D4H             S             70    DIM(100)
005100950525     D T4H             S              1    DIM(100)
005200950908     D S4H             S              1    DIM(100)
005300990204     D TAD             S              2    DIM(100)
005400990204     D DTA             S            256    DIM(100)
005500040419     D ERR             S             90    DIM(13) CTDATA PERRCD(1)
005600990204     D TPC             S             29    DIM(2) CTDATA PERRCD(1)
005700950605     D DER             S              1    DIM(90)
005800010702     D COS             S              1    DIM(33) CTDATA PERRCD(33)
005900010702     D W33             S              1    DIM(33)
006000010712     D W06             S              1    DIM(6)
006100030703     c* poste
006200040827     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
006300950525      *--------------------------------------------------------------*
006400950525      *   DS
006500950525      *--------------------------------------------------------------*
006600990204     D DTAD          E DS
006700990204     D  DDESC        E                     EXTFLD(§TADDESC)
006800990204     D  DRAGR        E                     EXTFLD(§TADRAGR)
006900990204     D  DCDNC        E                     EXTFLD(§TADCDNC)
007000990204     D  DCTLT        E                     EXTFLD(§TADCTLT)
007100990204     D  DCTLS        E                     EXTFLD(§TADCTLS)
007200990204     D  DDAGG        E                     EXTFLD(§TADDAGG)
007300990204     D  DMTVD        E                     EXTFLD(§TADMTVD)
007400990204     D  DDCDN        E                     EXTFLD(§TADDCDN)
007500990204     D  DTICO        E                     EXTFLD(§TADTICO)
007600990204     D  DFDDT        E                     EXTFLD(§TADFDDT)
007700990204     D  DFMED        E                     EXTFLD(§TADFMED)
007800990204     D  DPZDO        E                     EXTFLD(§TADPZDO)
007900990204     D  DKGDO        E                     EXTFLD(§TADKGDO)
008000990204     D  DEVFO        E                     EXTFLD(§TADEVFO)
008100990204     D  DIMPR        E                     EXTFLD(§TADIMPR)
008200990204     D  DTISI        E                     EXTFLD(§TADTISI)
008300990204     D  DDEST        E                     EXTFLD(§TADDEST)
008400990204     D  DRITR        E                     EXTFLD(§TADRITR)
008500990519     D  DDESI        E                     EXTFLD(§TADDESI)
008600990519     D  DCARE        E                     EXTFLD(§TADCARE)
008700990519     D  DCIDD        E                     EXTFLD(§TADCIDD)
008800101124     D wfnlrq1ds     e ds                  extname(wfnlrq10F)
008900101124     D wfnlrq2ds     e ds                  extname(wfnlrq20F)
009000101124     D dsqa          e ds
009100950525     D WLBDA8          DS                  INZ
009200950525     D  G02DAT                 1      8  0
009300950525     D  G02INV                 9     16  0
009400950525     D  G02ERR                17     17
009500950525     D  G02TGI                18     22  0
009600950525     D KPJBA         E DS
009700040721     D Danmflo       e ds
009800040721     D Dar5gen       e ds
009900040721     D DSARB         e ds                  extname(FNARB00F)
010000031009     D DSARBS        e ds                  extname(FNARB00F) PREFIX(S_)
010100031013     D DSEVB         e ds                  extname(FNEVB00F)
010200031013     D DSEVBS        e ds                  extname(FNEVB00F) PREFIX(S_)
010300070129     D DSbrv         e ds                  extname(Fnbrv00F)
010400070129     D DSbrvs        e ds                  extname(fnbrv00F) PREFIX(S_)
010500070129     D DSbrvc        e ds                  extname(fnbrv00F) PREFIX(c_)
010600040409     D DSfvv         e ds                  extname(fnfvv00F)
010700040409     D DSfvvc        e ds                  extname(fnfvv00F) prefix(c_)
010800030722     D ddatiute      e ds
010900031009     D azuteds       e ds                  extname(AZUTE00F)
011000030722     D tibs34ds      E DS                  inz
011100031006     D fnlr99ds      E DS                  inz
011200081014     D fnlrv7ds      e ds                  inz
011300110118     D fnls46ds      e ds                  inz
011400950525     D FNLR59        E DS                  EXTNAME(FNLR59DS)
011500950526     D DS4H          E DS
011600040827     D OG143         E DS
011700070129     D DSBRVE        E DS                  EXTNAME(FNBRVE0F)
011800070129     d                                     prefix(BRE:3)
011900070129     D* BREATB       E                     EXTFLD(BRVATB)
012000070129     D* BREFGS       E                     EXTFLD(BRVFGS)
012100070129     D* BRENPG       E                     EXTFLD(BRVNPG)
012200070129     D* BRENFV       E                     EXTFLD(BRVNFV)
012300070129     D* BREPES       E                     EXTFLD(BRVPES)
012400070129     D* BRELNP       E                     EXTFLD(BRVLNP)
012500070129     D* BRELNA       E                     EXTFLD(BRVLNA)
012600070129     D* BRENRS       E                     EXTFLD(BRVNRS)
012700070129     D* BRENSC       E                     EXTFLD(BRVNSC)
012800070129     D* BREZNC       E                     EXTFLD(BRVZNC)
012900070129     D* BRENPS       E                     EXTFLD(BRVNPS)
013000070129     D* BRECAN       E                     EXTFLD(BRVCAN)
013100070129     D* BREVUC       E                     EXTFLD(BRVVUC)
013200070129     D* BREPUC       E                     EXTFLD(BRVPUC)
013300070129     D* BREDCS       E                     EXTFLD(BRVDCS)
013400070129     D* BREHCS       E                     EXTFLD(BRVHCS)
013500070129     D* BREDFS       E                     EXTFLD(BRVDFS)
013600070129     D* BREHMS       E                     EXTFLD(BRVHMS)
013700070129     D* BREATR       E                     EXTFLD(BRVATR)
013800070129     D* BREFTR       E                     EXTFLD(BRVFTR)
013900070129     D* BREDTR       E                     EXTFLD(BRVDTR)
014000070129     D* BREHTR       E                     EXTFLD(BRVHTR)
014100070129     D* BREFLE       E                     EXTFLD(BRVFLE)
014200010620     D DSLV53        E DS                  EXTNAME(FNLV53DS)
014300010620     D DS7A2         E DS
014400100514     D DS7r          E DS
014500030805     D dar5ban       E DS                  inz
014600030805     D dar5bnb       E DS                  inz
014700010620     D* DS PER XDTORA  - DIFFERENZA FRA DUE DATA/ORA
014800010620     D WXDTOR          DS
014900010620     D  WXDT1                  1      8  0
015000010620     D  WXHM1                  9     12  0
015100010620     D  WXDT2                 13     20  0
015200010620     D  WXHM2                 21     24  0
015300010620     D  WXFLG                 25     25
015400010620     D  WXMIN                 26     30  0
015500960507     D DSFASE          DS
015600960507     D  WTLA                   1      1
015700960507     D  WNPG                   2      2
015800960507     D  WSPG                   3      3
015900960507     D  WFASE                  4      4
016000131104     c*
016100131104     d dEVB01        e ds
016200131104     d dEVB01sav       s                   like(dEVB01)
016300131104     c*
016400031029     D DSxmic          DS
016500031029     D  xdtmic                        8  0
016600031029     D  xoramic                       4  0
016700031029     D  xdtwrtmic                     8  0
016800031029     D  xorawrtmic                    6  0
016900031029     D DSxnic          DS
017000031029     D  xdtnic                        8  0
017100031029     D  xoranic                       4  0
017200031029     D  xdtwrtnic                     8  0
017300031029     D  xorawrtnic                    6  0
017400950526     D*--------------------------------------------------------------*
017500950526     D*  Costanti
017600950526     D COST3           C                   CONST('DOPO IL')
017700950526     D COST4           C                   CONST('IL')
017800030723     D CMAT            C                   CONST('MATTINO   ')
017900030723     D CPOM            C                   CONST('POMERIGGIO')
018000030805     d nban            S              5  0
018100040419     d sfgs            s                   like(fvvfgs)
018200040419     d snpg            s                   like(fvvnpg)
018300040419     d snfv            s                   like(fvvnfv)
018400110117     d sdfv            s                   like(fvvdfv)
018500040419     d shms            s                   like(brvhms)
018600950525      *--------------------------------------------------------------*
018700950525      * Ridenomino i campi dei codici anomalie in schiere
018800950525      *--------------------------------------------------------------*
018900950525     IFNARBY00
019000950525     I              ARBA01                      CDA(1)
019100950525     I              ARBA02                      CDA(2)
019200950525     I              ARBA03                      CDA(3)
019300950525     I              ARBA04                      CDA(4)
019400950525     I              ARBA05                      CDA(5)
019500950525     I              ARBA06                      CDA(6)
019600950525     I              ARBA07                      CDA(7)
019700950525     I              ARBA08                      CDA(8)
019800950525     I              ARBA09                      CDA(9)
019900950525     I              ARBA10                      CDA(10)
020000010702      *--------------------------------------------------------------*
020100010702      * Ridenomino tutti i campi DEL FILE FIBRVE
020200010702      *--------------------------------------------------------------*
020300070129     I*FIBRVE01
020400070129     I*             BRVATB                      BREATB
020500070129     I*             BRVFGS                      BREFGS
020600070129     I*             BRVNPG                      BRENPG
020700070129     I*             BRVNFV                      BRENFV
020800070129     I*             BRVPES                      BREPES
020900070129     I*             BRVLNP                      BRELNP
021000070129     I*             BRVLNA                      BRELNA
021100070129     I*             BRVNRS                      BRENRS
021200070129     I*             BRVNSC                      BRENSC
021300070129     I*             BRVZNC                      BREZNC
021400070129     I*             BRVNPS                      BRENPS
021500070129     I*             BRVCAN                      BRECAN
021600070129     I*             BRVVUC                      BREVUC
021700070129     I*             BRVPUC                      BREPUC
021800070129     I*             BRVDCS                      BREDCS
021900070129     I*             BRVHCS                      BREHCS
022000070129     I*             BRVDFS                      BREDFS
022100070129     I*             BRVHMS                      BREHMS
022200070129     I*             BRVATR                      BREATR
022300070129     I*             BRVFTR                      BREFTR
022400070129     I*             BRVDTR                      BREDTR
022500070129     I*             BRVHTR                      BREHTR
022600070129     I*             BRVFLE                      BREFLE
022700070129     I*FIBRVE02
022800070129     I*             BRVATB                      BREATB
022900070129     I*             BRVFGS                      BREFGS
023000070129     I*             BRVNPG                      BRENPG
023100070129     I*             BRVNFV                      BRENFV
023200070129     I*             BRVPES                      BREPES
023300070129     I*             BRVLNP                      BRELNP
023400070129     I*             BRVLNA                      BRELNA
023500070129     I*             BRVNRS                      BRENRS
023600070129     I*             BRVNSC                      BRENSC
023700070129     I*             BRVZNC                      BREZNC
023800070129     I*             BRVNPS                      BRENPS
023900070129     I*             BRVCAN                      BRECAN
024000070129     I*             BRVVUC                      BREVUC
024100070129     I*             BRVPUC                      BREPUC
024200070129     I*             BRVDCS                      BREDCS
024300070129     I*             BRVHCS                      BREHCS
024400070129     I*             BRVDFS                      BREDFS
024500070129     I*             BRVHMS                      BREHMS
024600070129     I*             BRVATR                      BREATR
024700070129     I*             BRVFTR                      BREFTR
024800070129     I*             BRVDTR                      BREDTR
024900070129     I*             BRVHTR                      BREHTR
025000070129     I*             BRVFLE                      BREFLE
025100950525     C*--------------------------------------------------------------*
025200950525     C*  Ciclo principale
025300950525     C*--------------------------------------------------------------*
025400950526     C     *ENTRY        PLIST
025500950526     C                   PARM                    KPJBA
025600030721     C                   MOVEL(p)  KPJBU         FNLR59
025700030805     c                   clear                   nban
025800050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
025900050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
026000050204     C* AUTOTRASPORTATORE
026100050204     C                   IF        D59STP = 'A'
026200050204     C                   SETON                                        61
026300050204     C                   SETOFF                                       62
026400050204     C                   ELSE
026500050204     C                   SETOFF                                       61
026600050204     C                   SETON                                        62
026700050204     C                   END
026800040930     c*
026900040930     c                   movel(p)  'FNLRF7R'     nompgm           10
027000030721     C*  A fine elaborazione leggo archivio eventi e scrivo
027100030721     C*  NIC in definitivo
027200030721     C     D59TPR        IFeq      'U'
027300030721     C                   EXSR      WRTNIC
027400030721     C                   MOVEL     'C'           D53TLA
027500030721     C                   CALL      'FNLV53R'
027600030721     C                   PARM                    DSLV53
027700030721     C                   SETON                                        LR
027800030721     c                   return
027900030721     c                   end
028000030729     C* spengo indicatore utilizzati nella routine di stampa
028100030729     C* spunte doppie.
028200030729     C                   SETOFF                                       2021
028300950526     C*  Valorizzo campi testata
028400950526     C                   EXSR      STPTES
028500040825     C     D59NDT        IFNE      EXNDT
028600040825     C                   Z-ADD     D59NDT        EXNDT
028700040825     C                   END
028800031009     C*  Leggo FNARBY dove ho memorizzato le spedizioni della distinta
028900031009     c*  con o senza errori
029000950529     C     *LOVAL        SETLL     FNARBY1L
029100031009     c                   do        *hival
029200031009     C                   READ      FNARBY1L
029300031009     c                   if        %eof(fnarby1l)
029400031009     c                   leave
029500031009     c                   end
029600950525     C                   EXSR      GESSPE
029700950525     C                   DELETE    FNARBY1L
029800030805     C                   ENDdo
029900030825     C*  Controllo se numero bancali caricati è maggiore a quello
030000110518     C*  in FIQDT
030100030825     C     nban          IFgt      D59NBL
030200030805     C                   Z-ADD     1             Y                 3 0
030300030805     C     '16'          LOOKUP    C4H(Y)                                 32
030400031016     C*  Controllo se si deve stampare l'aomalia
030500030805     C     S4H(Y)        IFNE      'N'
030600031016     C                   MOVEL     D4H(Y)        DErrq
030700110414     C                   CLEAR                   Terrq
030800031016     C                   Z-ADD     nban          WNRB1
030900031016     C                   Z-ADD     D59NBL        WNRB2
031000030805     C                   MOVEL     'S'           D59ANM
031100101201     c                   if        d59sqa =  ' ' or
031200101201     c                             d59sqa =  'T'
031300101125     c                   exsr      states
031400030805     C                   write     ERRQUA
031500101125     c                   end
031600030805     C                   ADD       2             D59LIN
031700030805     C                   END
031800030805     C                   END
031900010620     C*  Routine di stampa spunte consegna doppie
032000101201     c                   if        d59sqa =  ' ' or
032100101201     c                             d59sqa =  'T'
032200010620     C                   EXSR      DOPPIE
032300101125     c                   end
032400950526     C*  Loop x eventuale cancellazione FNARBZ
032500950526     C                   EXSR      DLTARZ
032600030721     C                   MOVEL(p)  FNLR59        KPJBU
032700950525     C                   SETON                                        RT
032800950525     C*--------------------------------------------------------------*
032900950526     C*  GESSPE: Gestione spedizione
033000950525     C*--------------------------------------------------------------*
033100950526     C     GESSPE        BEGSR
033200950525     C*  Cerco la spedizione su FNARB
033300950525     C                   Z-ADD     ARBAAS        KAAS
033400950525     C                   Z-ADD     ARBLNP        KLNP
033500950525     C                   Z-ADD     ARBNRS        KNRS
033600950525     C                   Z-ADD     ARBNSP        KNSP
033700950525     C     KARB          CHAIN     FNARB01L                           33
033800030825     c                   if        not *in33
033900031009     c* mi salvo la spedizione
034000031009     C                   MOVEL     DSARB         DSARBS
034100030805     C* CALCOLO IL NUMERO DEI BANCALI
034200030805     c                   exsr      srnban
034300031009     c* IMPOSTA I CAMPI IN STAMPA
034400110414     C                   MOVEL     ARBTSP        TIPERR
034500031009     C                   Z-ADD     ARBAAS        WAAS
034600031009     C                   Z-ADD     ARBLNP        WLNP
034700031009     C                   Z-ADD     ARBNRS        WNRS
034800031009     C                   Z-ADD     ARBNSP        WNSP
034900030731     C                   Z-ADD     ARBlna        Wlna
035000031009     C                   MOVEL     ARBRSD        WRSD
035100031009     C                   Z-ADD     ARBNCD        WNCD
035200031009     C                   Z-ADD     ARBNCA        WNCA
035300031009     C                   Z-ADD     ARBZNC        WZNC
035400030805     C                   movel     ARBmgs        Wcommes           2 0
035500030805     C                   move      ARBmgs        Wcomgg            2 0
035600031009     c                   movel     wcomgg        wdat
035700030805     c                   move      wcommes       wdat
035800031009     C* segnacolli non consecutivi
035900031009     C     ARBFNS        COMP      'S'                                    44
036000031009     C                   IF        NOT *IN44
036100031009     C                   MOVEL     '/'           WBAR
036200031009     C                   ELSE
036300031009     C                   CLEAR                   WBAR
036400031009     C                   END
036500030825     C                   END
036600031009     C* LOOP SU ERRORI
036700950525     C                   MOVEL     'N'           WERRBL            1
036800950525     C                   MOVEL     'N'           WNODIS            1
036900950525     C                   Z-ADD     1             X                 3 0
037000010702     C     X             DOWLE     10
037100950525     C     CDA(X)        ANDNE     *BLANKS
037200950530     C                   MOVEL     CDA(X)        WCDANO            2
037300950525     C                   EXSR      GESANO
037400950530     C                   ADD       1             X
037500031009     C                   ENDdo
037600950525     C*  Controllo se adesso è già stata messa in consegna
037700950529     C     ARBNDC        IFNE      *ZEROS
037800950530     C     ARBDCM        ANDEQ     0
037900950529     C     ARBNDC        ANDNE     D59NDT
038000950525     C     WNODIS        ANDEQ     'N'
038100031009     C                   MOVEL     '03'          WCDANO
038200950525     C                   EXSR      GESANO
038300040713     c* rendo bloccante l'errore anche se in tabella è stato deciso di
038400040713     c* non stamparlo
038500040713     C                   MOVEL     'S'           WERRBL
038600950525     C                   END
038700070530     c* METTE IN DISTINTA LA SPEDIZIONE se pgm "spunte"
038800040827     c                   movel     arbaas        arbdsp            8 0
038900040827     c                   move      arbmgs        arbdsp
039000031016     C* Se non ho trovato errore bloccante aggiorno la distinta
039100030721     C     WERRBL        IFEQ      'N'
039200070601     c                   if        arbngd=0 and arbndc = 0 and
039300070530     c                             (kcdaz='LR59' or kcdaz='LR60')
039400020917     C                   Z-ADD     D59FGS        ARBIFP
039500020917     C                   Z-ADD     D59NDT        ARBNDC
039600950525     C                   Z-ADD     D59DDT        ARBDDC
039700950525     C                   Z-ADD     D59PDR        ARBPDC
039800951121     C*  Attribuisco lo stop
039900951211     C     ARBKCS        IFNE      0
040000951211     C                   MOVEL     *BLANKS       KRAG
040100951211     C                   MOVEL     ARBKCS        KRAG
040200951211     C                   ELSE
040300951121     C                   MOVEL     ARBRSD        KRAG
040400951122     C                   MOVE      ARBIND        KRAG
040500951211     C                   END
040600951121     C     KRAG          CHAIN     FNARBZ0F                           32
040700950526     C*  Aggiorno la  bolla
040800950526     C                   EXCEPT    AGGARB
040900031013     C                   MOVEL     DSARB         DSARBS
041000031020     C                   END
041100031009     C*  DETTAGLIO SEGNACOLLI
041200980330     C                   Z-ADD     ARBAAS        KAAS
041300980330     C                   Z-ADD     ARBLNP        KLNP
041400980330     C                   Z-ADD     ARBNRS        KNRS
041500980330     C                   Z-ADD     ARBNSP        KNSP
041600950614     C                   MOVEL     'N'           WERRPZ            1
041700031009     C                   MOVEL     'N'           OKMIC             1
041800950614     C     KART          CHAIN     FNART01L                           32
041900030721     C*  Se non esiste e spedizione figlia mi posiziono su mamma
042000950614     C     *IN32         IFEQ      '1'
042100950606     C     KLBL          CHAIN     FNLBL01L                           32
042200950606     C     *IN32         IFEQ      '0'
042300950606     C     LBLLAP        ANDEQ     LBLLAN
042400950606     C                   Z-ADD     LBLAAP        KAAS
042500950606     C                   Z-ADD     LBLLPP        KLNP
042600950606     C                   Z-ADD     LBLNRP        KNRS
042700950606     C                   Z-ADD     LBLNSP        KNSP
042800950606     C     KARB          CHAIN     FNARB01L                           32
042900980430     C     ARBCCA        IFNE      '2'
043000980430     C     ARBCCA        ANDNE     '6'
043100950614     C     KART          CHAIN     FNART01L                           32
043200980430     C                   ELSE
043300980430     C                   Z-ADD     LBLAAN        KAAS
043400980430     C                   Z-ADD     LBLLPN        KLNP
043500980430     C                   Z-ADD     LBLNRN        KNRS
043600980430     C                   Z-ADD     LBLNSN        KNSP
043700980430     C     KARB          CHAIN     FNARB01L                           32
043800950614     C                   END
043900980430     C                   END
044000950614     C                   END
044100950614     C*  Loop su dettaglio segnacolli per verifcare se la bolla
044200950614     C*  è consegnata parzialmente su altra distinta
044300950526     C     *IN32         DOWEQ     '0'
044400000705     C                   EXSR      GESDET
044500031009     C                   ENDdo
044600070604     c* SCRIVO EVENTO MIC E ORM CONTESTUALE ALLA CONSEGNA
044700031020     c                   movel     dsarbs        dsarb
044800070530     c                   if        okmic = 'S'
044900100514     c                   clear                   ds7r
045000100514     c                   if        arbgma <> ' '
045100100514     c                   exsr      sr7r
045200100514     c                   end
045300100514     c                   if        §7RRC ='S'
045400031020     c                   clear                   fnlr99ds
045500071002     c                   eval      i99nftl= 99999
045600071002     c                   eval      i99fao = 400
045700031009     c                   exsr      aggorm
045800031020     c                   end
045900031020     c                   exsr      aggeve
046000070601     c*                  end
046100070530     c                   end
046200950526     C*
046300951109     C                   ELSE
046400030721     C  N33              unlock    fnarb01l
046500950526     C                   END
046600950526     C*
046700950526     C                   ENDSR
046800031003     C*--------------------------------------------------------------*
046900031003     C*  GESDET: Gestione dettaglio segnacolli
047000031003     C*--------------------------------------------------------------*
047100031003     C     GESDET        BEGSR
047200950526     C*
047300040409     C                   SETOFF                                       02
047400031009     C                   MOVEL     *BLANKS       WTIPCA
047500031009     C                   MOVEL     *BLANKS       WDETER
047600950530     C                   MOVEL     *BLANKS       WNRP              2
047700031009     C                   Z-ADD     0             WNRP1             2 0
047800950614     C                   MOVEL     'N'           WTRDAT            1
047900950614     C                   MOVEL     'N'           WTRDA1            1
048000950918     C                   MOVEL     'N'           WTRSPU            1
048100950526     C*  Controllo se non presente su distinta
048200950526     C                   Z-ADD     4             KNPG
048300950526     C                   Z-ADD     D59NDT        KNFV
048400950526     C                   Z-ADD     D59FGS        KFGS
048500950526     C                   Z-ADD     ARBLNA        KLNA
048600950526     C                   Z-ADD     ARBFLS        KFLS
048700950526     C                   Z-ADD     ARBNRS        KNRS
048800950526     C                   Z-ADD     ARTNSC        KNSC
048900000706      *
049000070129     C     KBRV1         CHAIN     FNBRV01L                           33
049100040408     c* NON HO LA SPUNTA 4
049200030729     C     *IN33         IFEQ      '1'
049300040421     c* solo se la spedizione è in distinta
049400040507     c                   if        s_arbndc  <> 0
049500990204     C* Controllo Dati Danni
049600040420     C*                  EXSR      TESDAN
049700950526     C*  Se non è presente su questa distinta verifico se è
049800950526     C*  in consegna su di un'altra
049900040408     c                   exsr      srfibrv
050000040408     c                   movel     okspu         wtrdat
050100030801     C*    se su altra distinta
050200030729     C     WTRDAT        IFEQ      'S'
050300950526     C     BRVNFV        ANDNE     0
050400040409     C                   MOVEL(p)  ERR(1)        WDETER
050500101124     c                   movel     '001'         tbeke2
050600040419     c                   movel     fvvnfv        com05
050700040419     c                   eval      %subst(wdeter: 51: 5)= com05
050800040409     c                   eval      %subst(wdeter: 71: 20)= fvvdsf
050900101126     c                   eval      sfgs=fvvfgs
051000101126     c                   eval      snpg=fvvnpg
051100101126     c                   eval      snfv=fvvnfv
051200110117     c                   eval      sdfv=fvvdfv
051300101126     c                   eval      shms=brvhms
051400950614     C                   MOVEL     BRVNPS        WNRP
051500950526     C                   EXSR      STAMPA
051600960507     C*  Richiamo TRUL11 per reperimento della fase
051700960507     C                   CLEAR                   DSFASE
051800960507     C                   MOVEL     '4'           WNPG
051900960507     C                   CALL      'TRUL11R'
052000960507     C                   PARM                    DSFASE
052100950919     C                   MOVEL     'S'           WTRSPU
052200950918     C                   Z-ADD     150           WCAA
052300950918     C                   Z-ADD     BRVNFV        WNFV
052400950918     C                   Z-ADD     BRVNPS        WNPS
052500960507     C                   Z-ADD     FVVFGS        WFGS
052600950918     C                   EXSR      WRTANM
052700030729     C                   END
052800950614     C*  Controllo se collo spuntato in I.M.A.
052900950614     C                   Z-ADD     3             KNPG
053000040408     c                   exsr      srfibrv
053100040408     c                   movel     okspu         wtrda1
053200040408     C*
053300950918     C                   MOVEL     *BLANKS       WDETER
053400950919     C                   SETOFF                                       02
053500950614     C                   Z-ADD     4             KNPG
053600030801     c*    se in inventario IMA
053700030729     C     WTRDA1        IFEQ      'S'
053800950614     C     BRVNFV        ANDNE     0
053900950614     C                   MOVEL     BRVNPS        WNRP
054000030801     c*    ...e su altra distinta
054100950919     C     WTRSPU        IFEQ      'S'
054200960507     C     FVVFCF        IFNE      'S'
054300040421     C                   MOVEL(p)  ERR(8)        WDETER
054400101124     c                   movel     '008'         tbeke2
054500040419     c                   eval      sfgs=fvvfgs
054600040419     c                   eval      snpg=fvvnpg
054700040419     c                   eval      snfv=fvvnfv
054800110117     c                   eval      sdfv=fvvdfv
054900040419     c                   eval      shms=brvhms
055000960507     C                   END
055100950919     C                   ELSE
055200030801     c*    ...no su altra distinta
055300040415     C                   MOVEL(p)  ERR(9)        WDETER
055400101124     c                   movel     '009'         tbeke2
055500040419     c                   eval      sfgs=fvvfgs
055600040419     c                   eval      snpg=fvvnpg
055700040419     c                   eval      snfv=fvvnfv
055800110117     c                   eval      sdfv=fvvdfv
055900040419     c                   eval      shms=brvhms
056000950919     C                   END
056100030801     c*
056200960507     C     WTRSPU        IFNE      'S'
056300960507     C     FVVFCF        ORNE      'S'
056400950614     C                   EXSR      STAMPA
056500960507     C*  Richiamo TRUL11 per reperimento della fase
056600960507     C                   CLEAR                   DSFASE
056700960507     C                   MOVEL     '3'           WNPG
056800960507     C                   MOVEL     'A'           WSPG
056900960507     C                   CALL      'TRUL11R'
057000960507     C                   PARM                    DSFASE
057100950919     C                   MOVEL     'S'           WTRSPU
057200950918     C                   Z-ADD     135           WCAA
057300950920     C                   Z-ADD     BRVNFV        WNFV
057400950918     C                   Z-ADD     WNRP1         WNPS
057500960507     C                   Z-ADD     FVVFGS        WFGS
057600950915     C                   EXSR      WRTANM
057700960507     C                   END
057800030729     C                   END
057900950918     C     WTRSPU        IFEQ      'S'
058000950918     C                   GOTO      NEWSGN
058100950918     C                   END
058200950526     C*  Controllo se il collo è arrivato
058300040419     C*    ARTDAM        IFNE      0
058400040419     C*    ARTDCM        ANDEQ     0
058500040419     C*    WDAVM         ANDEQ     'N'
058600040419     C*                  MOVEL(p)  ERR(2)        WDETER
058700040419     C*                  MOVEL     ARTNAP        WNRP
058800040419     C*                  EXSR      STAMPA
058900040419     C*                  GOTO      NEWSGN
059000040419     C*                  END
059100950526     C*  Collo partito e non arrivato
059200040419     C*    ARTDFV        IFNE      0
059300040419     C*    ARTDCM        ANDEQ     0
059400040419     C*    WDAVM         ANDEQ     'N'
059500040419     C*                  MOVEL(p)  ERR(3)        WDETER
059600040419     C*                  MOVEL     ARTNPP        WNRP
059700040419     C*                  EXSR      STAMPA
059800040419     C*                  GOTO      NEWSGN
059900040419     C*                  END
060000950526     C*  Collo non arrivato e non partito
060100040420     C*    ARTDCM        IFEQ      0
060200040420     C*    WDAVM         ANDEQ     'N'
060300040420     C*                  MOVEL(p)  ERR(5)        WDETER
060400040420     C*                  EXSR      STAMPA
060500040420     C*                  GOTO      NEWSGN
060600040420     C*                  END
060700950526     C*
060800040421     C                   end
060900030729     C                   ELSE
061000081103     c*
061100081103     C                   MOVEL     BRVNPS        WNRP
061200081103     C                   Z-ADD     BRVNPS        WNRP1
061300081103     C                   eval      dsbrvs = dsbrv
061400040408     c* HO LA SPUNTA 4
061500081014     c* per prima memorizzo segnazione di probabile etichetta doppia
061600081014     c                   clear                   fnlrv7ds
061700081014     c                   eval      ILRV7FLS=kfls
061800081014     c                   eval      ILRV7LNA=klna
061900081014     c                   eval      ILRV7NRS=knrs
062000081014     c                   eval      ILRV7NSC=knsc
062100081014     c                   movel(p)  fnlrv7ds      kpjbu
062200081014     c                   call      'FNLRV7R'
062300081014     c                   parm                    kpjba
062400081014     c                   movel     kpjbu         fnlrv7ds
062500081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
062600090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
062700081014     C                   MOVEL(p)  ERR(10)       WDETER
062800101124     c                   movel     '010'         tbeke2
062900101126     c                   eval      shms=s_brvhms
063000101126     c                   eval      sfgs=s_brvfgs
063100101126     c                   eval      snpg=s_brvnpg
063200110117     c                   eval      snfv=s_brvnfv
063300110117     c                   eval      sdfv=d59ddt
063400081014     C                   EXSR      STAMPA
063500081014     C                   END
063600040408     C* Controllo se collo spuntato anche in I.M.A.
063700040408     C                   Z-ADD     3             KNPG
063800040408     c                   exsr      srfibrv
063900040408     c                   movel     okspu         wtrda1
064000040408     C                   Z-ADD     4             KNPG
064100110118     c**********************************************************************
064200110118     c* se sono chiamato dalla sistemazione controllo aut, non segnalo
064300110118     c* l'errore e tento di cancellare la spunta + vecchia tra la 3 e la 4
064400110120     c                   if        kcdaz = 'LRQ1' and wtrda1 = 'S'
064500110118     c                   exsr      srls46
064600110118     c* se no errore ed effettuato l'aggiornamento proseguo
064700110118     c                   if        OLS46ERR = ' ' and
064800110118     c                             OLS46ANN = 'S'
064900110118     c* se cancellato la 3 e resta la 4 verifico se bolla in distinta ok
065000110118     c* segnalo errore
065100110118     c                   if        s_brvhms > brvhms
065200110118     c                   if        s_arbndc = 0
065300110118     c                   eval      wtrda1 = 'N'
065400110118     c                   clear                   dsbrv
065500110118     c                   clear                   dsfvv
065600110118     c                   else
065700110118     C                   GOTO      NEWSGN
065800110118     c                   end
065900110118     c                   else
066000110118     c* se cancellato la 4 e resta la 3 verifico se bolla in distinta errore
066100110118     c* altrimenti ok
066200110118     c                   if        s_arbndc = 0
066300110118     C                   GOTO      NEWSGN
066400110118     c                   else
066500110118     C                   MOVEL(p)  ERR(9)        WDETER
066600110118     c                   movel     '009'         tbeke2
066700110118     c                   eval      sfgs=fvvfgs
066800110118     c                   eval      snpg=fvvnpg
066900110118     c                   eval      snfv=fvvnfv
067000110118     c                   eval      sdfv=fvvdfv
067100110118     c                   eval      shms=brvhms
067200110118     C                   EXSR      STAMPA
067300110118     C                   GOTO      NEWSGN
067400110118     c                   end
067500110118     c                   end
067600110118     c                   end
067700110118     c                   end
067800110118     c**********************************************************************
067900040415     C* Se ho SOLO la spunta 4 ma la spedizione non è in distinta
068000040408     C* segnalo l'errore
068100110118     C                   IF        s_arbndc = 0
068200040408     C                   IF        wtrda1 = 'N'
068300040415     C                   MOVEL(p)  ERR(12)       WDETER
068400101124     c                   movel     '012'         tbeke2
068500101126     c                   eval      sfgs=s_brvfgs
068600101126     c                   eval      snpg=s_brvnpg
068700101126     c                   eval      snfv=s_brvnfv
068800040419     c                   eval      shms=s_brvhms
068900110117     c                   eval      sdfv=d59ddt
069000040408     C                   EXSR      STAMPA
069100040408     C                   GOTO      NEWSGN
069200040408     C* Se ho la spunta 4 e la spunta 3 ma la spedizione non è in distinta
069300040408     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
069400040408     C* spunta 4 > di ora spunta 3
069500040408     c                   else
069600110118     c
069700040409     c                   if        s_brvnps <> brvnps or
069800040409     c                             (s_brvnps = brvnps and s_brvhms > brvhms)
069900101124     C                   MOVEL(p)  ERR(12)       WDETER
070000101124     c                   movel     '012'         tbeke2
070100040419     c                   eval      shms=s_brvhms
070200101126     c                   eval      sfgs=s_brvfgs
070300101126     c                   eval      snpg=s_brvnpg
070400101126     c                   eval      snfv=s_brvnfv
070500101126     c                   eval      shms=s_brvhms
070600110117     c                   eval      sdfv=d59ddt
070700040415     C                   EXSR      STAMPA
070800040419     C                   MOVEL(p)  ERR(8)        WDETER
070900101124     c                   movel     '008'         tbeke2
071000040419     c                   eval      sfgs=fvvfgs
071100040419     c                   eval      snpg=fvvnpg
071200040419     c                   eval      snfv=fvvnfv
071300110117     c                   eval      sdfv=fvvdfv
071400040419     c                   eval      shms=brvhms
071500040419     c                   movel     brvnps        wnrp
071600040415     c                   seton                                        02
071700040409     C                   EXSR      STAMPA
071800040415     c                   setoff                                       02
071900040415     C                   GOTO      NEWSGN
072000040415     c                   else
072100040408     C                   GOTO      NEWSGN
072200040408     c                   end
072300040408     C                   END
072400040409     C                   END
072500040408     c*
072600031013     c                   eval      okmic = 'S'
072700031009     C* Se è stato spuntato con la distinta in esame controllo
072800031009     C* se segnacollo già consegnato
072900030729     C     ARTDCM        IFNE      0
073000040415     C                   MOVEL(P)  ERR(6)        WDETER
073100101124     c                   movel     '006'         tbeke2
073200040419     c                   eval      shms=s_brvhms
073300101126     c                   eval      sfgs=s_brvfgs
073400101126     c                   eval      snpg=s_brvnpg
073500101126     c                   eval      snfv=s_brvnfv
073600110117     c                   eval      sdfv=d59ddt
073700950526     C                   EXSR      STAMPA
073800950614     C                   GOTO      NEWSGN
073900030729     C                   END
074000040421     C* Se ho la spunta 4 e la spunta 3 e la spedizione è in distinta
074100040421     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
074200040421     C* spunta 3 > di ora spunta 4
074300040421     c                   if        s_brvnps <> brvnps or
074400040421     c                             (s_brvnps = brvnps and s_brvhms < brvhms)
074500030729     C     WTRDA1        IFEQ      'S'
074600040408     C     BRVNFV        ANDNE     0
074700960507     C     FVVFCF        IFNE      'S'
074800040415     C                   MOVEL(P)  ERR(7)        WDETER
074900101124     c                   movel     '007'         tbeke2
075000040419     c                   eval      shms=s_brvhms
075100101126     c                   eval      sfgs=s_brvfgs
075200101126     c                   eval      snpg=s_brvnpg
075300101126     c                   eval      snfv=s_brvnfv
075400110117     c                   eval      sdfv=d59ddt
075500040409     C                   EXSR      STAMPA
075600040415     C                   MOVEL(P)  ERR(8)        Wdeter
075700101124     c                   movel     '008'         tbeke2
075800040419     c                   eval      sfgs=fvvfgs
075900040419     c                   eval      snpg=fvvnpg
076000040419     c                   eval      snfv=fvvnfv
076100110117     c                   eval      sdfv=fvvdfv
076200040419     c                   eval      shms=brvhms
076300040419     c                   movel     brvnps        wnrp
076400040415     c                   seton                                        02
076500950612     C                   EXSR      STAMPA
076600040415     c                   setoff                                       02
076700960507     C*  Richiamo TRUL11 per reperimento della fase
076800960507     C                   CLEAR                   DSFASE
076900960507     C                   MOVEL     '3'           WNPG
077000960507     C                   MOVEL     'A'           WSPG
077100960507     C                   CALL      'TRUL11R'
077200960507     C                   PARM                    DSFASE
077300950918     C                   Z-ADD     135           WCAA
077400950920     C                   Z-ADD     BRVNFV        WNFV
077500950918     C                   Z-ADD     WNRP1         WNPS
077600960507     C                   Z-ADD     FVVFGS        WFGS
077700950915     C                   EXSR      WRTANM
077800031009     C                   GOTO      NEWSGN
077900030729     C                   END
078000030729     C                   END
078100040421     C                   END
078200950526     C*
078300030729     C                   END
078400950526     C*  Leggo successivo segnacollo
078500950614     C     NEWSGN        TAG
078600950526     C     KART          READE     FNART01L                               32
078700950526     C*
078800950526     C                   ENDSR
078900031009     C*--------------------------------------------------------------*
079000040408     C*  srfibrv cerco spunta nel foglio 4/3 nel gg. della distinta che
079100040408     c*          sto quadrando
079200031009     C*--------------------------------------------------------------*
079300040408     C     srfibrv       BEGSR
079400040408     c                   movel     'N'           okspu             1
079500040409     c                   clear                   dsbrvc
079600040409     c                   clear                   dsfvvc
079700031009     C*
079800070129     C     KBRV2         SETll     FNBRV09L
079900040408     c                   do        *hival
080000070129     C     KBRV2         READe     FNBRV09L
080100070129     c                   if        %eof(fnbrv09l)
080200040408     c                   leave
080300040408     c                   end
080400040408     C                   Z-ADD     BRVNFV        KNFV1
080500040408     C     KFVV          CHAIN     FNFVV01L
080600040408     c                   if        %found(fnfvv01l) and fvvdfv = d59ddt
080700040408     C                   MOVEL     'S'           okspu
080800040409     c                   if        brvhms > c_brvhms
080900040409     c                   eval      dsbrvc = dsbrv
081000040409     c                   eval      dsfvvc = dsfvv
081100040408     c                   end
081200040409     c                   end
081300040408     C                   ENDDO
081400040409     c*
081500040409     c                   eval      dsbrv = dsbrvc
081600040409     c                   eval      dsfvv = dsfvvc
081700040408     C*
081800040408     C                   ENDSR
081900040408     C*--------------------------------------------------------------*
082000040408     C*  AGGEVE: scrivo evento MIC sia per distinte manuali che x quelle
082100040408     c*          da spunte
082200040408     C*--------------------------------------------------------------*
082300040408     C     AGGEVE        BEGSR
082400040408     C*
082500131104     C                   clear                   dEVB01
082600131104     C                   move      *zeros        §NOTNDC
082700131104     C                   move      *zeros        §NOTPDC
082800131104     C                   move      ARBPDC        §NOTPDC
082900131104     C                   move      ARBNDC        §NOTNDC
083000131104     C                   MOVEL     Devb01        dEVB01sav
083100031009     C* Nel momento in cui metto in consegna la bolla controllo se
083200031009     C* ho scritto evento NIC provvisorio nell'archivio eventi della
083300031009     C* QTEMP e nel caso lo cancello
083400031009     C                   Z-ADD     ARBAAS        KAAS
083500031009     C                   Z-ADD     ARBLNP        KLNP
083600031009     C                   Z-ADD     ARBNRS        KNRS
083700031009     C                   Z-ADD     ARBNSP        KNSP
083800031009     C     KLBL          CHAIN     FNLBL01L                           32
083900031009     C     *IN32         IFEQ      '0'
084000031009     C     LBLLAP        ANDEQ     LBLLAN
084100031009     C                   Z-ADD     LBLAAP        KAAS
084200031009     C                   Z-ADD     LBLLPP        KLNP
084300031009     C                   Z-ADD     LBLNRP        KNRS
084400031009     C                   Z-ADD     LBLNSP        KNSP
084500031009     C     KARB          CHAIN(N)  FNARB01L                           32
084600031009     C     ARBCCA        IFEQ      '2'
084700031009     C     ARBCCA        OREQ      '6'
084800031009     C                   Z-ADD     LBLAAN        KAAS
084900031009     C                   Z-ADD     LBLLPN        KLNP
085000031009     C                   Z-ADD     LBLNRN        KNRS
085100031009     C                   Z-ADD     LBLNSN        KNSP
085200031009     C     KARB          CHAIN(N)  FNARB01L                           32
085300031009     C                   SETON                                        32
085400031009     C                   END
085500031009     C                   END
085600031009     C     *IN32         IFEQ      '0'
085700031009     C     LBLLAP        ANDEQ     LBLLAN
085800031009     C                   Z-ADD     LBLAAP        KAAS
085900031009     C                   Z-ADD     LBLLPP        KLNP
086000031009     C                   Z-ADD     LBLNRP        KNRS
086100031009     C                   Z-ADD     LBLNSP        KNSP
086200031009     C                   ELSE
086300031009     C                   Z-ADD     ARBAAS        KAAS
086400031009     C                   Z-ADD     ARBLNP        KLNP
086500031009     C                   Z-ADD     ARBNRS        KNRS
086600031009     C                   Z-ADD     ARBNSP        KNSP
086700031009     C                   END
086800130326     C     KEVB1         CHAIN     WFNEVB01L                          36
086900031009     C     EVBCEV        IFEQ      'NIC'
087000130326     C  N36              DELETE    WFNEVB01L
087100031009     C                   ELSE
087200031009     C  N36              EXCEPT    SPED1
087300031009     C                   END
087400031009     C* Aggiorno evento di messa in consegna
087500031009     C                   MOVEL     'MIC'         KCEV
087600031009     C     KEVB2         SETGT     FNEVB04L
087700031009     C     KEVB2         READPE    FNEVB04L                               37
087800031009     C     *IN37         IFEQ      '1'
087900031009     C     EVBDEV        ORNE      D59DDT
088000031009     C                   MOVEL     'MIC'         EVBCEV
088100031009     C                   Z-ADD     D59AMG        EVBDTV
088200031009     C                   TIME                    EVBORV
088300031009     C                   Z-ADD     D59FGS        EVBFLE
088400031009     C                   MOVE      D59CDU        EVBCDU
088500031009     C                   Z-ADD     KAAS          EVBAAS
088600031009     C                   Z-ADD     KLNP          EVBLNP
088700031009     C                   Z-ADD     KNRS          EVBNRS
088800031009     C                   Z-ADD     KNSP          EVBNSP
088900031009     C                   Z-ADD     D59DDT        EVBDEV
089000031009     C                   Z-ADD     0             EVBDTR
089100031009     C                   MOVEL     *BLANKS       EVBFTR
089200031009     C                   Z-ADD     0             EVBDTC
089300131104     C                   MOVEL     Devb01sav     EVBNOT
089400031009     C     D59FPP        IFEQ      'M'
089500031009     C                   MOVEL     0800          EVBHEV
089600031009     C                   ELSE
089700031009     C                   MOVEL     1200          EVBHEV
089800031009     C                   END
089900031009     C                   WRITE     FNEVB000
090000031009     C                   ELSE
090100031009     C     EVBCEV        IFEQ      'MIC'
090200031009     C     EVBDEV        ANDEQ     D59DDT
090300031009     C*
090400031009     C     EVBDTR        IFEQ      0
090500031009     C* Se non è stato trasmesso aggiorno la data e l'ora di
090600031009     C* di immissione evento con quella del giorno + altri dati
090700031009     C                   Z-ADD     D59AMG        EVBDTV
090800031009     C                   TIME                    EVBORV
090900031009     C                   Z-ADD     D59FGS        EVBFLE
091000031009     C                   MOVE      D59CDU        EVBCDU
091100131104     C                   MOVEL     Devb01sav     EVBNOT
091200031009     C     D59FPP        IFEQ      'M'
091300031009     C                   MOVEL     0800          EVBHEV
091400031009     C                   ELSE
091500031009     C                   MOVEL     1200          EVBHEV
091600031009     C                   END
091700031009     C                   MOVEL     *BLANKS       EVBDTC
091800031009     C                   UPDATE    FNEVB000
091900031009     C                   ELSE
092000031009     C* Se l'evento MIC esiste già ed è già stato trasmesso lo
092100031009     C* riscrivo solo se c'è un NIC successivo
092200031029     C                   MOVE      EVBDEV        xdtMIC
092300031029     C                   MOVE      EVBHEV        xoraMIC
092400031029     C                   MOVE      EVBdtv        xdtwrtMIC
092500031029     C                   MOVE      EVBorv        xorawrtMIC
092600031009     C                   MOVEL     'NIC'         KCEV
092700031009     C     KEVB2         SETGT     FNEVB04L
092800031009     C     KEVB2         READPE    FNEVB04L                               37
092900031009     C     *IN37         IFEQ      '0'
093000031009     C     EVBDEV        ANDEQ     D59DDT
093100031029     C                   MOVE      EVBDEV        xdtNIC
093200031029     C                   MOVE      EVBHEV        xoraNIC
093300031029     C                   MOVE      EVBdtv        xdtwrtNIC
093400031029     C                   MOVE      EVBorv        xorawrtNIC
093500031029     C     dsxMIC        IFLE      dsxNIC
093600031009     C                   MOVEL     'MIC'         EVBCEV
093700031009     C                   Z-ADD     D59AMG        EVBDTV
093800031009     C                   TIME                    EVBORV
093900031009     C                   Z-ADD     D59FGS        EVBFLE
094000031009     C                   MOVE      D59CDU        EVBCDU
094100031009     C                   Z-ADD     KAAS          EVBAAS
094200031009     C                   Z-ADD     KLNP          EVBLNP
094300031009     C                   Z-ADD     KNRS          EVBNRS
094400031009     C                   Z-ADD     KNSP          EVBNSP
094500031009     C                   Z-ADD     D59DDT        EVBDEV
094600031009     C                   Z-ADD     0             EVBDTR
094700031009     C                   MOVEL     *BLANKS       EVBFTR
094800031009     C     D59FPP        IFEQ      'M'
094900031009     C                   MOVEL     0800          EVBHEV
095000031009     C                   ELSE
095100031009     C                   MOVEL     1200          EVBHEV
095200031009     C                   END
095300031009     C                   Z-ADD     0             EVBDTC
095400131104     C                   MOVEL     Devb01sav     EVBNOT
095500031009     C                   WRITE     FNEVB000
095600031009     C                   END
095700031009     C*
095800031009     C                   END
095900031009     C                   END
096000031009     C                   END
096100031009     C                   END
096200031020     c* mi reimposto la ds di FNARB originale
096300031020     c                   movel     dsarbs        dsarb
096400031009     C*
096500031009     C                   ENDSR
096600031009     C*--------------------------------------------------------------*
096700100514     C*  sr7r  particolarità consegna
096800031009     C*--------------------------------------------------------------*
096900100514     C     sr7r          BEGSR
097000031009     c*
097100100514     C                   eval      kcod = '7R'
097200100514     C                   eval      kkey = arbgma
097300100514     c                   eval      kkut = 1
097400100514     C     ktab1         chain     Tabel00f
097500100514     c                   if        %found(tabel00f)
097600100514     C                   MOVEL     TBLUNI        DS7R
097700100514     c                   end
097800100514     C*
097900100514     C                   ENDSR
098000100514     C*--------------------------------------------------------------*
098100100514     C*  aggorm: Aggiorno ORM e sue fasi
098200100514     C*--------------------------------------------------------------*
098300100514     C     aggorm        BEGSR
098400100514     c*
098500031009     c                   eval      i99tla = 'L'
098600031020     c                   eval      i99aas = arbaas
098700031020     c                   eval      i99lnp = arblnp
098800031020     c                   eval      i99nrs = arbnrs
098900031020     c                   eval      i99nsp = arbnsp
099000031020     c* metto in distinta
099100071001     c                   eval      i99fgs = d59fgs
099200031020     c                   if        i99fao = 400
099300031020     c                   eval      i99ndc = arbndc
099400031020     c                   eval      i99ddc = arbddc
099500031009     c                   eval      i99npg = 4
099600031020     c                   end
099700031020     c                   eval      i99comit = '1'
099800071001     c                   eval      i99cmt = 'N'
099900031009     c                   call      'FNLR99R'
100000031009     c                   parm                    kpjba
100100031009     c                   parm                    fnlr99ds
100200031009     c*
100300031009     C                   ENDSR
100400110118     C*--------------------------------------------------------------*
100500110118     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
100600110118     c* dal controllo a video
100700110118     C*--------------------------------------------------------------*
100800110118     C     srls46        BEGSR
100900110118     c*
101000110118     c                   clear                   fnls46ds
101100110118     c                   clear                   kpjbu
101200110118     c                   eval      ILS46NPGA =  brvnpg
101300110118     c                   eval      ILS46NFVA =  brvnfv
101400110118     c                   eval      ILS46FGSA =  brvfgs
101500110120     c                   eval      ILS46NPGB =  s_brvnpg
101600110120     c                   eval      ILS46NFVB =  s_brvnfv
101700110120     c                   eval      ILS46FGSB =  s_brvfgs
101800110121     c                   eval      ILS46AMB =  'QD'
101900110121     c                   eval      ILS46aupr=  'SE'
102000110120     c                   eval      ILS46FLS =  Kfls
102100110118     c                   eval      ILS46LNA =  Klna
102200110118     c                   eval      ILS46NRS =  Knrs
102300110118     c                   eval      ILS46NSC =  Knsc
102400110120     c                   eval      ILS46TELA = 'M'
102500110118     c*
102600110118     c                   call      'FNLS46R'
102700110118     c                   parm                    kpjba
102800110118     c                   parm                    fnls46ds
102900110118     c*
103000110118     C                   ENDSR
103100990204     C*--------------------------------------------------------------*
103200990204     C*  TESDAN: Test gestione danni
103300990204     C*--------------------------------------------------------------*
103400990204     C     TESDAN        BEGSR
103500990204     C*
103600990204     C*  Flag pratica danno = 'N'
103700990204     C                   MOVE      'N'           WDAVM             1
103800990204     C*  Se il collo non è stato spuntato su quel foglio di
103900990204     C*  uscita controllo se c'è una pratica C.A. aperta
104000990204     C     KDCD          CHAIN     FNDCD02L                           38
104100990204     C*  Se esiste C.A. aperta controllo il tipo di anomalia
104200990204     C     *IN38         IFEQ      '0'
104300990204     C     DCDATB        ANDNE     'A'
104400990204     C     DCDDCH        IFEQ      0
104500990204     C                   MOVE      TPC(1)        WTIPCA
104600990204     C* Aggancio testata
104700990204     C                   MOVE      DCDAAC        KAAC
104800990204     C                   MOVE      DCDFIL        KFIL
104900990204     C                   MOVE      DCDNCA        KNCA
105000990204     C     KDCT          CHAIN     FNDCT01L                           38
105100990204     C     *IN38         DOWEQ     '0'
105200990204     C     DCTATB        ANDEQ     'A'
105300990204     C     *IN38         OREQ      '0'
105400990204     C     DCTDCH        ANDNE     0
105500990204     C     KDCT          READE     FNDCT01L                               38
105600990204     C                   END
105700990204     C                   Z-ADD     1             YY                4 0
105800990204     C     DCTTAD        LOOKUP    TAD(YY)                                39
105900990204     C     *IN39         IFEQ      '1'
106000990204     C                   MOVEL     DTA(YY)       DTAD
106100990204     C     DRAGR         IFEQ      'M'
106200990204     C     DRAGR         OREQ      'V'
106300031009     C                   MOVE      'S'           WDAVM
106400990204     C                   END                                                    Manc.o Ava D/R
106500990204     C                   END                                                    Trv TAD
106600990204     C*
106700990204     C                   ELSE
106800990204     C                   MOVE      TPC(2)        WTIPCA
106900990204     C                   END                                                    CA Aperta
107000990204     C                   END                                                    CA no annul
107100990204     C*
107200990204     C                   ENDSR
107300950525     C*--------------------------------------------------------------*
107400950525     C*  GESANO: Gestione anomalie
107500950525     C*--------------------------------------------------------------*
107600950525     C     GESANO        BEGSR
107700950525     C*  Se non ho trovato errore bloccante
107800950911     C                   SETOFF                                       031417
107900031009     C                   Z-ADD     1             Y
108000950526     C     WCDANO        LOOKUP    C4H(Y)                                 32
108100950525     C     *IN32         IFEQ      '1'
108200950908     C     S4H(Y)        ANDNE     'N'
108300950908     C                   MOVEL     'S'           D59ANM
108400031009     C                   MOVEL     D4H(Y)        DESERR
108500950525     C*  Trovato errore bloccante !!
108600950525     C     T4H(Y)        IFEQ      'B'
108700950525     C                   MOVEL     'S'           WERRBL
108800030801     C*  Se in consegna su altra distinta
108900950529     C     C4H(Y)        IFEQ      '03'
109000950525     C                   MOVEL     'S'           WNODIS
109100950526     C                   SETON                                        03
109200950606     C                   MOVEL     *BLANKS       APDRSC
109300021204     C     kapd          CHAIN     fiapd01L                           37
109400031009     C                   MOVEL     APDRSC        WPADRO
109500950525     C                   END
109600950526     C                   END
109700950911     C*  Se ho sbloccato il C/Assegno senalo importo e divisa
109800950911     C     C4H(Y)        IFEQ      '07'
109900950911     C                   SETON                                        17
110000950911     C                   Z-ADD     0             AR9CAS
110100950911     C                   MOVEL     *BLANKS       AR9VCA
110200051115     C     KARB          CHAIN     FIAR901L                           37
110300950911     C                   END
110400030801     C*  Decodificare la consegna richiesta
110500950529     C     C4H(Y)        IFEQ      '10'
110600950526     C                   MOVEA     *BLANKS       RIC
110700950526     C                   Z-ADD     ARBDCR        G02INV
110800950526     C                   MOVEL     '3'           G02ERR
110900950526     C                   CALL      'XSRDA8'
111000950526     C                   PARM                    WLBDA8
111100950526     C                   MOVE      G02DAT        WDCR              8
111200950526     C                   MOVEA     WDCR          DCR
111300950526     C     ARBTCR        IFEQ      'D'
111400950526     C                   MOVEA     COST3         RIC
111500950526     C                   MOVEL     DCR(1)        RIC(9)
111600950526     C                   MOVEL     DCR(2)        RIC(10)
111700950526     C                   MOVEL     '/'           RIC(11)
111800950526     C                   MOVEL     DCR(3)        RIC(12)
111900950526     C                   MOVEL     DCR(4)        RIC(13)
112000950526     C                   MOVEL     '/'           RIC(14)
112100950526     C                   MOVEA     DCR(5)        RIC(15)
112200950526     C                   ELSE
112300950526     C                   MOVEA     COST4         RIC
112400950526     C                   MOVEL     DCR(1)        RIC(4)
112500950526     C                   MOVEL     DCR(2)        RIC(5)
112600950526     C                   MOVEL     '/'           RIC(6)
112700950526     C                   MOVEL     DCR(3)        RIC(7)
112800950526     C                   MOVEL     DCR(4)        RIC(8)
112900950526     C                   MOVEL     '/'           RIC(9)
113000950526     C                   MOVEA     DCR(5)        RIC(10)
113100950526     C                   END
113200950526     C                   MOVEA     RIC           WRIC             18
113300950526     C                   MOVE      WRIC          DESERR
113400950525     C                   END
113500950615     C*  Se non sono riuscita ad andare in consegna con l'ultima figlia
113600950615     C*  segnalo l'errore.
113700950615     C     C4H(Y)        IFEQ      '14'
113800950615     C     KLBL          SETGT     FNLBL02L
113900950615     C     KLBL          READPE    FNLBL02L                               32
114000950615     C                   SETON                                        14
114100950615     C                   END
114200030725     c* stampa testata
114300110112     c                   if        d59sqa =  ' ' or
114400110112     c                             d59sqa =  'T'
114500030728     c                   exsr      states
114600101124     c* stampa
114700030728     C                   WRITE     ERRARB
114800101201     c                   end
114900101124     c* video
115000101201     c                   if        d59sqa =  'S' or
115100101201     c                             d59sqa =  'T'
115200101124     c                   EVAL      tbeke2 = 'H' + C4H(Y)
115300101124     c                   exsr      srsqa
115400101124     c                   end
115500950612     C                   ADD       1             D59LIN
115600110414     C                   CLEAR                   TIPERR
115700960319     C                   Z-ADD     0             WAAS
115800960319     C                   Z-ADD     0             WLNP
115900960319     C                   Z-ADD     0             WNRS
116000960319     C                   Z-ADD     0             WNSP
116100030728     C                   Z-ADD     0             Wlna
116200030728     C                   Z-ADD     0             Wdat
116300030729     C                   Z-ADD     0             WNCD
116400030729     C                   Z-ADD     0             WNCA
116500030729     C                   Z-ADD     0             WZNC
116600030729     C                   MOVEL     *BLANKS       WRSD
116700030729     C                   MOVEL     *BLANKS       WBAR
116800950526     C                   END
116900950526     C*
117000950525     C*
117100950525     C                   ENDSR
117200950526     C*--------------------------------------------------------------*
117300030725     C*  STAMPA: Stampa testata
117400950526     C*--------------------------------------------------------------*
117500030728     C     STATES        BEGSR
117600110112     c                   if        d59sqa =  ' ' or
117700110112     c                             d59sqa =  'T'
117800030728     C* SE LA RIGA DI STAMPA È MAGGIORE DI SEI VUOL DIRE CHE HO GIÀ STAMPATO
117900030728     C                   IF        D59LIN = 0 OR
118000030728     C                             D59LIN > 54
118100030728     C                   SETON                                        80
118200030729     c                   else
118300030729     c* stampo testatina spunte doppie a metà pagina
118400030729     c                   if        *in20 and *in21
118500030729     c                   write     testad
118600030729     c                   write     testad1
118700030729     C                   ADD       4             D59LIN
118800030729     c                   setoff                                       21
118900030728     C                   END
119000030729     C                   END
119100030725     C*
119200030728     C                   IF        *IN80
119300030725     C                   ADD       1             D59PAG
119400030728     C                   WRITE     TESTA
119500050502     c                   if        d59lsr = 'S' and d59pag = 1
119600050422     C                   write     testal
119700050426     c                   end
119800030729     C                   Z-ADD     6             D59LIN
119900030728     C                   SETOFF                                       80
120000030728     C* SPUNTE DOPPIE
120100030729     C                   IF        *IN20
120200040930     C                   WRITE     TESTAD2
120300040930     C                   WRITE     TESTAD1
120400030911     C                   Z-ADD     7             D59LIN
120500030911     C                   SETOFF                                       21
120600030729     C                   else
120700030729     C                   WRITE     TESTA1
120800030728     C                   END
120900030729     C                   END
121000110112     C                   END
121100030728     C*
121200030725     C                   ENDSR
121300030725     C*--------------------------------------------------------------*
121400030725     C*  STAMPA: Stampa dettaglio segnacolli
121500030725     C*--------------------------------------------------------------*
121600030725     C     STAMPA        BEGSR
121700101126     C*  Se non è stato stampato "ANOMALIE DA SPUNTE:" lo stampo
121800101201     C                   IF        werrpz = 'N' and
121900101201     c                             (d59sqa =  ' ' or
122000101201     c                             d59sqa =  'T')
122100950526     C                   MOVEL     '12'          WCDANO
122200950526     C                   MOVEL     'S'           WERRPZ
122300950526     C                   EXSR      GESANO
122400950526     C                   END
122500950908     C*----------------------------------------------------*
122600950908     C*  Controllo se devo stampare le anomalie
122700101126     C*    S4H(Y)        IFNE      'N'
122800950908     C*----------------------------------------------------*
122900030725     c*
123000030725     c                   exsr      states
123100040419     c* Imposto il numero della pistola nel campo di stampa
123200950530     C                   MOVEA     WDETER        DER
123300040409     C     '.xx '        SCAN      WDETER        XX                2 0    01
123400040409     c                   if        *in01
123500950530     C                   ADD       1             XX
123600950530     C                   MOVEA     WNRP          DER(XX)
123700950530     C                   MOVEA     DER           WDETER
123800950530     C                   END
123900040419     c* Imposto il numero del foglio nel campo di stampa
124000040419     C                   MOVEA     WDETER        DER
124100040419     C     'xxx-'        SCAN      WDETER        XX                2 0    01
124200040419     c                   if        *in01
124300040421     c                   movel(p)  sfgs          com04             4
124400040421     C                   MOVEA     com04         DER(XX)
124500040420     C                   ADD       4             XX
124600040419     c                   movel     snpg          com01             1
124700040419     C                   MOVEA     com01         DER(XX)
124800040420     C                   ADD       2             XX
124900040419     c                   movel     snfv          com05             5
125000040419     C                   MOVEA     com05         DER(XX)
125100040419     C                   MOVEA     DER           WDETER
125200040419     C                   END
125300110117     c* Imposto l'ora del foglio nel campo di stampa
125400040419     C                   MOVEA     WDETER        DER
125500040419     C     'xx:'         SCAN      WDETER        XX                2 0    01
125600040419     c                   if        *in01
125700040419     c                   movel     shms          com02             2
125800040419     C                   MOVEA     com02         DER(XX)
125900040420     C                   ADD       3             XX
126000070130     c                   move      shms          com04             4
126100070130     c                   movel     com04         com02
126200040419     C                   MOVEA     com02         DER(XX)
126300040419     C                   MOVEA     DER           WDETER
126400040419     C                   END
126500040419     c*
126600950612     C                   ADD       1             D59LIN
126700101125     c* stampa
126800101201     c                   if        (d59sqa =  ' ' or
126900101201     c                             d59sqa =  'T')
127000950919     C     WTRSPU        IFEQ      'S'
127100030728     C                   EVAL      WDETER1 = WDETER
127200030728     C                   EVAL      WTIPCA1 = WTIPCA
127300030728     C                   WRITE     ERRAR1
127400950919     C                   ELSE
127500030728     C                   WRITE     ERRART
127600101125     C                   END
127700101201     c                   end
127800101124     c* video
127900101201     c                   if        (d59sqa =  'S' or
128000101201     c                             d59sqa =  'T')
128100101124     c                   exsr      srsqa
128200101124     c                   end
128300950908     C*----------------------------------------------------*
128400101126     C*                  END
128500950908     C*----------------------------------------------------*
128600950526     C                   ENDSR
128700950526     C*--------------------------------------------------------------*
128800950526     C*  STPTES: Stampo dati testata
128900950526     C*--------------------------------------------------------------*
129000950526     C     STPTES        BEGSR
129100030728     C* AUTOTRASPORTATORE
129200031009     C                   Z-ADD     D59PDR        PRTPDR
129300031009     C                   MOVEL(P)  D59DSF        PRTDSF
129400050422     C                   MOVEL(P)  D59DSF        PRTDSF2
129500030728     C* DISTINTA
129600031009     C                   Z-ADD     D59NDT        PRTNDT
129700110209     C                   Z-ADD     D59NDT        PRTNDT3
129800110209     C*                  eval      PRTNDT2 =d59bcd
129900110209     C                   move      d59bcd        PRTNDT2
130000031009     C                   Z-ADD     D59FGS        PRTFGS
130100030728     C                   Z-ADD     D59DDT        G02INV
130200030728     C                   MOVEL     '3'           G02ERR
130300030728     C                   CALL      'XSRDA8'
130400030728     C                   PARM                    WLBDA8
130500031009     C                   Z-ADD     G02DAT        STCDDC
130600950526     C*
130700030723     C                   MOVEL     CMAT          TPPRE
130800030723     C     D59FPP        IFEQ      'P'
130900031009     C                   MOVEL     CPOM          TPPRE
131000030723     C                   END
131100030728     C*
131200950526     C*
131300950526     C                   ENDSR
131400101124     C*-----------------------------------------------------*
131500101124     C* memorizzo l'errore nel file di work                 *
131600101124     C*-----------------------------------------------------*
131700101124     C     srsqa         BEGSR
131800101124     c                   clear                   dsqa
131900101124     c                   eval      tbecod = 'SQA'
132000101124     c                   eval      tbeke1 = 'FNLRF7R2'
132100101124     c     ktbe          chain     tntbe01l
132200101124     c                   if        %found(tntbe01l)
132300101124     c                   movel     tbeuni        dsqa
132400101124     c                   if        D§SQAELA = 'S'
132500101124     c* TESTATA
132600101124     c                   clear                   wfnlrq1ds
132700101124     c                   eval      wlrq1ndc = d59ndt
132800101124     c                   eval      wlrq1fgs = d59fgs
132900101124     c                   eval      wlrq1pdr = d59pdr
133000101125     c                   eval      wlrq1aas = s_arbAAS
133100101125     c                   eval      wlrq1LNP = s_arbLNP
133200101125     c                   eval      wlrq1NRS = s_arbNRS
133300101125     c                   eval      wlrq1NSP = s_arbNSP
133400101124     c*
133500101124     c     kwf10         chain     wfnlrq11l
133600101124     c                   if        not %found(wfnlrq11l)
133700101124     c                   movel     tbeke2        wlrq1cer
133800101124     c                   eval      wlrq1sce = D§SQASCE
133900101126     c                   if        D§SQATPD = 'S'
134000101126     c                   eval      wlrq1err = D§SQADES
134100101126     c                   else
134200101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
134300101126     c                   eval      wlrq1err = deserr
134400101126     c                   else
134500101126     c                   eval      wlrq1err = wdeter
134600101126     c                   end
134700101126     c                   end
134800101124     c                   write     wfnlrq10
134900101124     c                   else
135000101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
135100101124     c                   clear                   wlrq1cer
135200101124     c                   clear                   wlrq1sce
135300101124     c                   update    wfnlrq10
135400101124     c                   end
135500101124     c* dettaglio
135600101124     c                   clear                   wfnlrq2ds
135700101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
135800101124     c                   eval      wlrq2aas = wlrq1aas
135900101124     c                   eval      wlrq2lnp = wlrq1lnp
136000101124     c                   eval      wlrq2nrs = wlrq1nrs
136100101124     c                   eval      wlrq2nsp = wlrq1nsp
136200101124     c                   eval      wlrq2ndc = wlrq1ndc
136300101124     c                   eval      wlrq2fgs = wlrq1fgs
136400101124     c                   eval      wlrq2pdr = wlrq1pdr
136500101124     c                   movel     tbeke2        wlrq2cer
136600101124     c                   eval      wlrq2sce = D§SQASCE
136700101126     c                   if        D§SQATPD = 'S'
136800101126     c                   eval      wlrq2err = D§SQADES
136900101126     c                   else
137000101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
137100101126     c                   eval      wlrq2err = deserr
137200101126     c                   else
137300101126     c                   eval      wlrq2err = wdeter
137400101126     c                   end
137500101126     c                   end
137600101125     c* gli errori nella tabella 4H sono a livello di bolla quindi
137700101126     c* non ho le informazioni del segnacollo
137800101125     c                   if        %subst(tbeke2: 1: 1) <> 'H'
137900101124     c                   eval      wlrq2nsc = artnsc
138000110104     c                   eval      wlrq2fls = artfls
138100110126     c                   eval      wlrq2nrc = artnrs
138200110131     c                   if        tbeke2 <> '010'
138300110104     c                   eval      wlrq2flv = sfgs
138400101125     c                   eval      wlrq2nfv = snfv
138500110119     c                   eval      wlrq2dfv = sdfv
138600101125     c                   eval      wlrq2npg = snpg
138700101126     c                   eval      wlrq2hms = shms
138800101126     c                   if        wnrp <> ' '
138900101126     c                   movel     wnrp          wlrq2nps
139000101126     c                   end
139100110131     c                   end
139200110207     c*                  else
139300110207     c*                  eval      wlrq2flv = s_arbifp
139400110207     c*                  eval      wlrq2nfv = s_arbndc
139500110207     c*                  eval      wlrq2dfv = s_arbddc
139600110207     c*                  if        wlrq2nfv <> 0
139700110207     c*                  eval      wlrq2npg = 4
139800110207     c*                  end
139900101126     c                   end
140000101125     c*
140100101124     c                   write     wfnlrq20
140200101124     c                   end
140300101124     c                   end
140400101124     C                   ENDSR
140500950526     C*--------------------------------------------------------------*
140600950526     C*  DLTARZ: Cancello archivio dett.segnacolli/stop
140700950526     C*--------------------------------------------------------------*
140800950526     C     DLTARZ        BEGSR
140900950526     C*
141000950526     C     *LOVAL        SETLL     FNARBZ0F
141100950526     C                   READ      FNARBZ0F                               31
141200950526     C     *IN31         DOWEQ     '0'
141300950526     C                   DELETE    FNARBZ0F
141400950526     C                   READ      FNARBZ0F                               31
141500950526     C                   END
141600950526     C*
141700950526     C                   ENDSR
141800950614     C*--------------------------------------------------------------*
141900950614     C*  WRTANM: Scrivo anomalia collo fuori posto
142000950614     C*--------------------------------------------------------------*
142100950915     C     WRTANM        BEGSR
142200950915     C*
142300950919     C     ARTDCM        IFEQ      0
142400950915     C                   MOVEL     'N'           WAGANO            1
142500950918     C                   Z-ADD     WCAA          KCAA
142600990519     C                   CLEAR                   FNANM000
142700990519     C     KANM          CHAIN     FNANM02L                           34
142800950915     C     *IN34         DOWEQ     '0'
142900990519     C     ANMDAO        IFEQ      D59DDT
143000031009     C                   MOVEL     'S'           WAGANO
143100950915     C     ANMDCH        IFNE      0
143200960507     C                   MOVEL     WFASE         ANMFSA
143300960507     C                   Z-ADD     WFGS          ANMCDU
143400960507     C                   Z-ADD     WNFV          ANMNFV
143500950918     C                   Z-ADD     WNPS          ANMNPS
143600950915     C                   Z-ADD     0             ANMDCH
143700950915     C                   MOVEL     *BLANKS       ANMFSC
143800950915     C                   MOVEL     *BLANKS       ANMCCH
143900950915     C                   EXCEPT    AGGANM
144000960429     C                   ELSE
144100030721     C                   unlock    fnanm02l
144200950915     C                   END
144300950915     C                   END
144400990519     C     KANM          READE     FNANM02L                               34
144500950915     C                   END
144600950915     C*
144700950915     C     WAGANO        IFEQ      'N'
144800990519     C                   CLEAR                   FNANM000
144900960507     C                   MOVEL     WFASE         ANMFSA
145000960507     C                   Z-ADD     WFGS          ANMCDU
145100950915     C                   Z-ADD     ARTFLS        ANMFLS
145200950915     C                   Z-ADD     ARTNRS        ANMNRS
145300950915     C                   Z-ADD     ARTLNA        ANMLNA
145400950915     C                   Z-ADD     ARTNSC        ANMSCN
145500950915     C                   Z-ADD     ARBAAS        ANMAAS
145600950915     C                   Z-ADD     ARBLNP        ANMLNP
145700950915     C                   Z-ADD     ARBNSP        ANMNSP
145800950918     C                   Z-ADD     WCAA          ANMCAA
145900950918     C                   Z-ADD     WNFV          ANMNFV
146000950918     C                   Z-ADD     WNPS          ANMNPS
146100950915     C                   Z-ADD     D59DDT        ANMDAO
146200950915     C                   Z-ADD     SIMFEL        ANMFLE
146300950915     C                   MOVEL     'F'           ANMTAN
146400950915     C                   MOVEL     'S'           ANMSAN
146500950915     C                   MOVEL     'I'           ANMAIE
146600040721     c                   eval      anmft4 = §AR5BVA
146700040721     c                   movel     woggi         com6              6
146800040721     c                   clear                   comsei            6
146900040721     c                   eval      comsei = %subst(com6: 5: 2)+
147000040721     c                             %subst(com6: 3: 2)+
147100040721     c                             %subst(com6: 1: 2)
147200040721     c                   movel     comsei        §ANMDAC
147300040721     c                   movel     times         §ANMORC
147400040721     c                   movel     danmflo       anmflo
147500040721     c                   eval      anmfl4 = arbznc
147600040721     c                   if        arbznc=0
147700040721     c                   eval      anmfl4 = 999
147800040721     c                   end
147900990519     C                   WRITE     FNANM000
148000950915     C                   END
148100950919     C*
148200950919     C                   END                                                    ARTDCM = 0
148300950614     C*
148400950915     C                   ENDSR
148500980310     C*--------------------------------------------------------------*
148600980310     C*  WRTNIC: Leggo NIC provvisori e li scrivo in definitivo
148700980310     C*--------------------------------------------------------------*
148800980310     C     WRTNIC        BEGSR
148900980310     C*
149000980310     C* Controllo quali sono gli eventi di esclusi dalla consegna che
149100980310     C* non sono stati cancellati e li riporto in definitivo
149200130326     C     *LOVAL        SETLL     WFNEVB01L
149300130326     C                   READ      WfNEVB01L                              36
149400980310     C     *IN36         DOWEQ     '0'
149500031013     C                   MOVEL     DSEVB         DSEVBS
149600980310     C* Aggancio file eventi per controllare se l'evento di messa in
149700980310     C* consegna sulla bolla era già stato trasmesso
149800990803     C                   Z-ADD     EVBAAS        KAAS
149900980310     C                   Z-ADD     EVBLNP        KLNP
150000980310     C                   Z-ADD     EVBNRS        KNRS
150100980310     C                   Z-ADD     EVBNSP        KNSP
150200031020     C* riaggancio la spedizione dell'evento NIC rimasto
150300031020     C     KARB          CHAIN     FNARB01L                           33
150400100514     c                   clear                   ds7r
150500100514     c                   if        arbgma <> ' '
150600100514     c                   exsr      sr7r
150700100514     c                   end
150800031020     c* se spedizione con ritiro contestuale alla consegna
150900100514     c                   if        §7RRC= 'S' and not *in33
151000031020     c                   clear                   fnlr99ds
151100031020     c                   eval      i99fao = 390
151200031020     c                   exsr      AGGORM
151300031020     c                   end
151400031020     c* proseguo con la scrittura del NIC/cancellazione del MIC
151500980310     C                   MOVEL     'MIC'         KCEV
151600990803     C     KEVB2         SETGT     FNEVB04L
151700990803     C     KEVB2         READPE    FNEVB04L                               37
151800980310     C     *IN37         IFEQ      '0'
151900990803     C     EVBDEV        ANDEQ     D59DDT
152000980310     C* se l'evento MIC non era stato trasmesso lo annullo fisicamente
152100980310     C     EVBDTR        IFEQ      0
152200990803     C                   DELETE    FNEVB04L
152300980310     C                   ELSE
152400031029     C                   MOVEL     EVBDEV        WDTMIC           12 0
152500031029     C                   MOVE      EVBHEV        WDTMIC
152600980312     C* altrimenri controllo se esiste già un evento NIC con data = a
152700980312     C* quella del foglio di consegna
152800990803     C                   Z-ADD     EVBAAS        KAAS
152900980312     C                   Z-ADD     EVBLNP        KLNP
153000980312     C                   Z-ADD     EVBNRS        KNRS
153100980312     C                   Z-ADD     EVBNSP        KNSP
153200980312     C                   MOVEL     'NIC'         KCEV
153300990803     C     KEVB2         SETGT     FNEVB04L
153400990803     C     KEVB2         READPE    FNEVB04L                               37
153500980312     C     *IN37         IFEQ      '0'
153600990803     C     EVBDEV        ANDEQ     D59DDT
153700980312     C* Se c'è evento NIC controllo se già trasmesso
153800980312     C     EVBDTR        IFEQ      0
153900980312     C* Se non è ancora stato trasmesso aggiorno data/ora immissione
154000980312     C                   Z-ADD     D59AMG        EVBDTV
154100980312     C                   TIME                    EVBORV
154200980312     C                   Z-ADD     D59FGS        EVBFLE
154300980312     C                   MOVE      D59CDU        EVBCDU
154400980312     C     FVVFPP        IFEQ      'M'
154500980312     C                   MOVEL     0800          EVBHEV
154600980312     C                   ELSE
154700980312     C                   MOVEL     1200          EVBHEV
154800980312     C                   END
154900990803     C                   Z-ADD     0             EVBDTC
155000990803     C                   UPDATE    FNEVB000
155100980312     C                   ELSE
155200980312     C* Se è già stato trasmesso controllo se data/ora immissione
155300980312     C* > MIC
155400031029     C                   MOVEL     EVBDEV        WDTNIC           12 0
155500031029     C                   MOVE      EVBHEV        WDTNIC
155600031029     C     WDTNIC        IFLE      WDTMIC
155700980312     C* Se la data non è più alta scrivo un nuovo NIC
155800031013     C                   MOVEL     DSEVBS        DSEVB
155900990803     C                   WRITE     FNEVB000
156000980312     C                   END                                                    Data MIC >= NIC
156100980312     C*
156200980312     C                   END                                                    NIC Trasmesso
156300980312     C*
156400980312     C                   ELSE                                                   Nic Trovato
156500031013     C                   MOVEL     DSEVBS        DSEVB
156600990803     C                   WRITE     FNEVB000
156700980312     C                   END                                                    Nic Trovato
156800980312     C                   END                                                    Mic trasmesso
156900980312     C                   END                                                    Mic Trovato
157000980310     C* Cancello evento provvisiorio
157100990803     C                   DELETE    FNEVBPRV
157200980310     C* leggo evento provvisiorio successivo
157300130326     C                   READ      WFNEVB01L                              36
157400980310     C                   END
157500980310     C*
157600980310     C                   ENDSR
157700030805     C**********************************************************************
157800030805     C* calcolo il numero dei bancali   -----------------------------*
157900030805     C**********************************************************************
158000030805     C     srnban        BEGSR
158100030805      * Bancali
158200030805     c                   If        %Subst(Arbgva:1:1) = 'B'
158300030805     c                   Clear                   dAr5Ban
158400030805     C                   move      'BAN'         ar5trd
158500030805     c     kFiar5        Chain     Fiar501l
158600030805     c                   If        %Found(Fiar501l)
158700030805     c                   Movel     Ar5Uni        dAr5Ban
158800030805     c                   EndIf
158900030805     c                   add       §Ar5Nba       nban
159000030805     c                   add       §ar5Nb2       nban
159100030805     c                   EndIf
159200030805      * Bancali Bartolini
159300030805     c                   If        %Subst(Arbgva:1:1) = 'O'
159400030805     c                   Clear                   dAr5BNB
159500030805     C                   move      'BNB'         ar5trd
159600030805     c     kFiar5        Chain     Fiar501l
159700030805     c                   If        %Found(Fiar501l)
159800030805     c                   Movel     Ar5Uni        dAr5BNB
159900030805     c                   EndIf
160000030805     c                   add       §Ar5bNba      nban
160100030805     c                   add       §ar5bNb2      nban
160200030805     c                   EndIf
160300040721      * Merce di valore
160400040721     c                   Clear                   dAr5gen
160500040721     C                   move      'GEN'         ar5trd
160600040721     c     kFiar5        Chain     Fiar501l
160700040721     c                   If        %Found(Fiar501l)
160800040721     c                   Movel     Ar5Uni        dAr5gen
160900040721     c                   EndIf
161000030805     c                   ENDSR
161100010620     C*--------------------------------------------------------------*
161200010620     C*  DOPPIE: STAMPO EVENTUALI SPUNTE DI CONSEGNA DOPPIE
161300010620     C*--------------------------------------------------------------*
161400010620     C     DOPPIE        BEGSR
161500010620     C*
161600010620     C                   Z-ADD     D59NDT        KNFV
161700010620     C                   Z-ADD     4             KNPG
161800030729     C                   SETON                                        2021
161900010620     C*
162000070129     C     KBRVE         SETLL     FNBRVE1L
162100070129     C     KBRVE         READE     FNBRVE1L                               30
162200010620    2C     *IN30         DOWEQ     *OFF
162300010620     C                   CLEAR                   WGIR
162400010620     C                   CLEAR                   SAWNPS
162500010622     C                   CLEAR                   SAWNFV
162600010622     C                   CLEAR                   SAWFGS
162700130327     C                   SETOFF                                       19
162800070129     C     KBRVE2        SETLL     FNBRVE2L
162900070129     C     KBRVE2        READE     FNBRVE2L                               31
163000010620    3C     *IN31         DOWEQ     *OFF
163100010620     C* Considero solo se data foglio >= data distinta in quadratura
163200010702    4C     D59FGS        IFNE      BREFGS
163300010702     C     KNFV          ORNE      BRENFV
163400010620     C                   CLEAR                   DSLV53
163500010620     C                   MOVEL     'V'           D53TFO
163600010620     C                   MOVEL     KNPG          D53NPG
163700010620     C                   Z-ADD     SIMFEL        D53FEL
163800010702     C                   Z-ADD     BRENFV        D53NFV
163900010702     C                   Z-ADD     BREFGS        D53FGS
164000010620     C                   CALL      'FNLV53R'
164100010620     C                   PARM                    DSLV53
164200010702     C                   Z-ADD     D53DFV        SA1DFV
164300010620     C                   ELSE
164400010702     C                   Z-ADD     D59DDT        SA1DFV
164500010620    4C                   ENDIF
164600010702    4C     D59FGS        IFEQ      BREFGS
164700010702     C     KNFV          ANDEQ     BRENFV
164800010620     C     D53ERR        OREQ      *BLANKS
164900010702     C     SA1DFV        ANDGE     D59DDT
165000010703    5C     SAWNFV        IFGT      *ZEROS
165100010622     C* Stessa pistola e foglio: devo verificare se superati i minuti
165200010622     c* di tolleranza previsti fra una spunta e l'altra
165300010620     C                   CLEAR                   WPRT
165400010702    6C     SAWNPS        IFEQ      BRENPS
165500010702     C     SAWNFV        ANDEQ     BRENFV
165600010702     C     SAWFGS        ANDEQ     BREFGS
165700010620     C                   CLEAR                   WXDTOR
165800010702     C                   MOVEL     BREDFS        WXDT1
165900010702     C                   MOVEL     BREHMS        WXHM1
166000010620     C                   MOVEL     SAWDFS        WXDT2
166100010620     C                   MOVEL     SAWHMS        WXHM2
166200010620     C                   MOVEL     'D'           WXFLG
166300010620     C                   CALL      'XDTORA'
166400010620     C                   PARM                    WXDTOR
166500010620     C     WXMIN         IFGT      §7AMNP
166600010620     C                   MOVEL     '1'           WPRT
166700010620     C                   ENDIF
166800010620   x6C                   ELSE
166900010622     c* se pistole o foglio diversi stampo sempre
167000010620     C                   MOVEL     '1'           WPRT              1
167100010620    6C                   ENDIF
167200010620    6C     WPRT          IFEQ      '1'
167300010620     c* se sono al primo giro di stampa devo stampare sia la spunta pre
167400010620     c* cedente che la spunta successiva
167500010620    7C     WGIR          IFEQ      ' '
167600010620     C                   MOVEL     DSBRVE        W1BRVE
167700010620     C                   MOVEL     WSBRVE        DSBRVE
167800010620     C                   Z-ADD     SAWDFV        WDFV
167900030729     C                   SETOff                                       19
168000010620     C                   EXSR      SPRDOP
168100010620     C                   MOVEL     W1BRVE        DSBRVE
168200010620     C                   MOVE      '1'           WGIR              1
168300010620    7C                   ENDIF
168400010702     C                   Z-ADD     SA1DFV        WDFV
168500030729     C                   SETON                                        19
168600010620     C                   EXSR      SPRDOP
168700010620   X6C                   ELSE
168800010620     C                   MOVEL     DSBRVE        WSBRVE
168900010620    6C                   ENDIF
169000010620   X5C                   ELSE
169100010620     C* se sono al primo giro di lettura memorizzo la spunta
169200010620     C                   MOVEL     DSBRVE        WSBRVE
169300010620    5C                   ENDIF
169400010620     C* memorizzo in ogni caso il numero pistola e data/ora fisica spun
169500010702     C                   Z-ADD     BRENPS        SAWNPS
169600010702     C                   Z-ADD     BREFGS        SAWFGS
169700010702     C                   Z-ADD     BRENFV        SAWNFV
169800010702     C                   Z-ADD     BREDFS        SAWDFS
169900010702     C                   Z-ADD     BREHMS        SAWHMS
170000010702     C                   Z-ADD     SA1DFV        SAWDFV
170100010620    4C                   ENDIF
170200070129     C     KBRVE2        READE     FNBRVE2L                               31
170300010620    3C                   ENDDO
170400010620     C* Mi posiziono sempre su un nuovo segnacollo
170500070129     C     KBRVE1        SETGT     FNBRVE1L
170600070129     C     KBRVE         READE     FNBRVE1L                               30
170700010620    2C                   ENDDO
170800010711     C*
170900010620     C                   ENDSR
171000010620     C*
171100010620     C*--------------------------------------------------------------*
171200010620     C* STAMPA SPUNTA DOPPIA
171300010620     C*--------------------------------------------------------------*
171400010620     C     SPRDOP        BEGSR
171500010712     C                   MOVEL     'S'           D59ANM
171600030728     C                   EXSR      STATES
171700010711     C*
171800010702     C                   CLEAR                   P1CMSG
171900030729     C* SEGNACOLLO SOLO PER 19 Off
172000030729     C     *IN19         IFEQ      *Off
172100010702     C                   Z-ADD     BRELNP        P1CLNP
172200010702     C                   Z-ADD     BRELNA        P1CLNA
172300010702     C                   Z-ADD     BRENRS        P1CNRS
172400010702     C                   Z-ADD     BRENSC        P1CNSC
172500010702     C* VEDO SE COLLO IN IMA
172600010702     C                   Z-ADD     3             KNPG
172700070129     C     KBRV9         SETGT     FNBRV09L
172800070129     C     KBRV9         READPE    FNBRV09L                               32
172900010702     C     *IN32         DOWEQ     *OFF
173000010702     C     P1CMSG        ANDEQ     *BLANKS
173100010702     C* IL foglio deve avere sottocategoria arrivi ed avere data foglio
173200010702     C* maggiore uguale alla data del foglio in fase di quadratura
173300010702     C                   CLEAR                   DSLV53
173400010702     C                   MOVEL     'V'           D53TFO
173500010702     C                   MOVEL     KNPG          D53NPG
173600010702     C                   Z-ADD     SIMFEL        D53FEL
173700010702     C                   Z-ADD     BRVNFV        D53NFV
173800010702     C                   Z-ADD     BRVFGS        D53FGS
173900010702     C                   CALL      'FNLV53R'
174000010702     C                   PARM                    DSLV53
174100010702     C     D53ERR        IFEQ      *BLANKS
174200010702     C     D53DFV        ANDGE     D59DDT
174300010702     C     D53SPG        ANDEQ     'A'
174400010702     C                   MOVEA     COS           W33
174500010702     C                   MOVE      BRVFGS        W003A             3
174600010702     C                   MOVEA     W003A         W33(8)
174700010702     C                   MOVE      BRVNFV        W006A             6
174800010712     C                   EXSR      EDITA
174900010702     C                   MOVEA     W006A         W33(12)
175000010702     C                   MOVE      D53DFV        W0040             4 0
175100010702     C                   MOVEL     W0040         WMMDFV            2
175200010702     C                   MOVE      W0040         WGGDFV            2
175300010702     C                   MOVEA     WGGDFV        W33(23)
175400010702     C                   MOVEA     WMMDFV        W33(26)
175500010702     C                   MOVE      BRVNPS        W002A             2
175600010702     C                   MOVEA     W002A         W33(32)
175700031009     C                   MOVEA     W33           P1CMSG
175800010702     C                   ENDIF
175900070129     C     KBRV9         READPE    FNBRV09L                               32
176000010702     C                   ENDDO
176100010620     C                   ENDIF
176200110518     C* Reimposto KNPG in quanto usato nel ciclo di lettura di fnbrve
176300110518     C                   Z-ADD     4             KNPG
176400010620     C*
176500010702     C                   Z-ADD     BREFGS        P1CFGS
176600010702     C                   Z-ADD     BRENFV        P1CNFV
176700010620     C* DATA DISTINTA
176800010620     C                   Z-ADD     WDFV          G02INV
176900010620     C                   MOVE      *ZEROS        G02DAT
177000010620     C                   MOVEL     3             G02ERR
177100010620     C                   CALL      'XSRDA8'
177200010620     C                   PARM                    WLBDA8
177300010620     C                   Z-ADD     G02DAT        P1CDFV
177400010620     C* AUTOTRASPORTATORE
177500110518     C     KDST          CHAIN     FiDST01L                           32
177600010620    1C     *IN32         IFEQ      *OFF
177700010711     C                   Z-ADD     DSTPDR        P1CPDR
177800021204     C     kapdd         CHAIN     fiapd01L                           32
177900010620    2C     *IN32         IFEQ      *OFF
178000031009     C                   MOVEL     APDRSC        P1DPDR
178100010620   X2C                   ELSE
178200010620     C                   CLEAR                   P1DPDR
178300010620    2C                   ENDIF
178400010620   X1C                   ELSE
178500010620     C                   CLEAR                   P1CPDR
178600010620     C                   ENDIF
178700010620     C* DATA IMMISSIONE
178800010702     C                   Z-ADD     BREDFS        G02INV
178900010620     C                   MOVE      *ZEROS        G02DAT
179000010620     C                   MOVEL     3             G02ERR
179100010620     C                   CALL      'XSRDA8'
179200010620     C                   PARM                    WLBDA8
179300010620     C                   Z-ADD     G02DAT        P1CDFS
179400010620     C*
179500010702     C                   Z-ADD     BREHMS        P1CHMS
179600010702     C                   Z-ADD     BRENPS        P1CNPS
179700030728     C                   WRITE     ERRDOP
179800010711     C                   ADD       1             D59LIN
179900010620     C                   ENDSR
180000010712     C*--------------------------------------------------------------*
180100010712     C* SOPPRESSIONE ZERI NON SIGNIFICATIVI DA CAMPO LUNGO 6
180200010712     C*--------------------------------------------------------------*
180300010712     C     EDITA         BEGSR
180400010712     C*
180500010712     C                   MOVE      *BLANKS       WFINE             1
180600010712     C                   MOVEA     W006A         W06
180700010712     C                   Z-ADD     1             II                1 0
180800010712     C     WFINE         DOWEQ     *BLANKS
180900010712     C     II            ANDLE     5
181000010712     C     W06(II)       IFEQ      '0'
181100010712     C                   MOVE      *BLANKS       W06(II)
181200010712     C                   ELSE
181300010712     C                   MOVE      '1'           WFINE
181400010712     C                   END
181500010712     C                   ADD       1             II
181600010712     C                   ENDDO
181700010712     C                   MOVEA     W06           W006A
181800010712     C                   ENDSR
181900950526     C*--------------------------------------------------------------*
182000950526     C*  *INZSR: Operazioni iniziali
182100950526     C*--------------------------------------------------------------*
182200950526     C     *INZSR        BEGSR
182300950526     C*------------------------------------------------------*
182400030722     c     *dtaara       define    §azute        azuteds
182500030722     c     *dtaara       define    §datiute      ddatiute
182600030722     C                   in(E)     *dtaara
182700030722     C                   IF        %Error  or  RSUT = *blanks
182800030722     C                   call      'TIBS34R'
182900030722     C                   parm                    Tibs34Ds
183000030722     C                   in        *dtaara
183100030722     c                   ENDIF
183200950612     C                   MOVE      SIMFEL        KFLE
183300950526     C*------------------------------------------------------*
183400950526     C*  Definisco chiavi di accesso
183500110518     C     KDST          KLIST
183600110518     C                   KFLD                    knpg
183700110518     C                   KFLD                    brenfv
183800110518     C                   KFLD                    BREFGS
183900030805     C     Kfiar5        KLIST
184000030805     C                   KFLD                    arbaas
184100030805     C                   KFLD                    arblnp
184200030805     C                   KFLD                    arbnrs
184300030805     C                   KFLD                    arbnsp
184400030805     C                   KFLD                    ar5trd
184500950526     C     KARB          KLIST
184600950526     C                   KFLD                    KAAS
184700950526     C                   KFLD                    KLNP
184800950526     C                   KFLD                    KNRS
184900950526     C                   KFLD                    KNSP
185000950606     C     KLBL          KLIST
185100950606     C                   KFLD                    KAAS
185200950606     C                   KFLD                    KLNP
185300950606     C                   KFLD                    KNRS
185400950606     C                   KFLD                    KNSP
185500950526     C     KART          KLIST
185600950526     C                   KFLD                    KAAS
185700950526     C                   KFLD                    KLNP
185800950526     C                   KFLD                    KNRS
185900950526     C                   KFLD                    KNSP
186000950605     C     KFVV          KLIST
186100950605     C                   KFLD                    KNPG
186200950605     C                   KFLD                    KNFV1
186300080229     C                   KFLD                    brvfgs
186400950526     C     KBRV1         KLIST
186500950526     C                   KFLD                    KNPG
186600950526     C                   KFLD                    KNFV
186700950526     C                   KFLD                    KFGS
186800950526     C                   KFLD                    KFLS
186900950526     C                   KFLD                    KLNA
187000950526     C                   KFLD                    KNRS
187100950526     C                   KFLD                    KNSC
187200950526     C     KBRV2         KLIST
187300950526     C                   KFLD                    KNPG
187400950526     C                   KFLD                    KFLS
187500950526     C                   KFLD                    KLNA
187600950526     C                   KFLD                    KNRS
187700950526     C                   KFLD                    KNSC
187800040420     C                   KFLD                    KFLE
187900010702     C     KBRV9         KLIST
188000010702     C                   KFLD                    KNPG
188100010702     C                   KFLD                    BRELNP
188200010702     C                   KFLD                    BRELNA
188300010702     C                   KFLD                    BRENRS
188400010702     C                   KFLD                    BRENSC
188500010702     C                   KFLD                    KFLE
188600990204     C     KDCD          KLIST
188700990204     C                   KFLD                    KFLS
188800990204     C                   KFLD                    KLNA
188900990204     C                   KFLD                    KNRS
189000990204     C                   KFLD                    KNSC
189100990204     C     KDCT          KLIST
189200990204     C                   KFLD                    KAAC
189300990204     C                   KFLD                    KFIL
189400990204     C                   KFLD                    KNCA
189500950526     C     KTAB          KLIST
189600950526     C                   KFLD                    KKUT
189700950526     C                   KFLD                    KCOD
189800010620     C     KTAB1         KLIST
189900010620     C                   KFLD                    KKUT
190000010620     C                   KFLD                    KCOD
190100010620     C                   KFLD                    KKEY
190200950915     C     KANM          KLIST
190300950915     C                   KFLD                    KFLS
190400950915     C                   KFLD                    KLNA
190500950915     C                   KFLD                    KNRS
190600950915     C                   KFLD                    KNSC
190700950915     C                   KFLD                    KCAA
190800980310     C     KEVB1         KLIST
190900990803     C                   KFLD                    KAAS
191000980310     C                   KFLD                    KLNP
191100980310     C                   KFLD                    KNRS
191200980310     C                   KFLD                    KNSP
191300980310     C     KEVB2         KLIST
191400990803     C                   KFLD                    KAAS
191500980310     C                   KFLD                    KLNP
191600980310     C                   KFLD                    KNRS
191700980310     C                   KFLD                    KNSP
191800980310     C                   KFLD                    KCEV
191900010620     C     KBRVE         KLIST
192000010620     C                   KFLD                    KNPG
192100010620     C                   KFLD                    KNFV
192200010620     C                   KFLD                    D59FGS
192300010620     C     KBRVE1        KLIST
192400010620     C                   KFLD                    KNPG
192500010620     C                   KFLD                    KNFV
192600010620     C                   KFLD                    D59FGS
192700010702     C                   KFLD                    BRELNP
192800010702     C                   KFLD                    BRELNA
192900010702     C                   KFLD                    BRENRS
193000010702     C                   KFLD                    BRENSC
193100010620     C     KBRVE2        KLIST
193200010702     C                   KFLD                    BRENPG
193300010702     C                   KFLD                    BRELNP
193400010702     C                   KFLD                    BRELNA
193500010702     C                   KFLD                    BRENRS
193600010702     C                   KFLD                    BRENSC
193700021204     C*
193800021204     C     Kapd          KLIST
193900021204     C                   KFLD                    apdtip
194000021204     C                   KFLD                    arbpdc
194100021204     C*
194200021204     C     Kapd59        KLIST
194300021204     C                   KFLD                    apdtip
194400021204     C                   KFLD                    d59pdr
194500021204     C*
194600021204     C     Kapdd         KLIST
194700021204     C                   KFLD                    apdtip
194800021204     C                   KFLD                    dstpdr
194900021204     c                   movel     'A'           apdtip
195000101124     C     Ktbe          KLIST
195100101124     C                   KFLD                    tbecod
195200101124     C                   KFLD                    tbeke1
195300101124     C                   KFLD                    tbeke2
195400101124     C     Kwf10         KLIST
195500101124     C                   KFLD                    WLRQ1AAS
195600101124     C                   KFLD                    WLRQ1LNP
195700101124     C                   KFLD                    WLRQ1NRS
195800101124     C                   KFLD                    WLRQ1NSP
195900101124     C                   KFLD                    WLRQ1FGS
196000101124     C                   KFLD                    WLRQ1NDC
196100101124     C                   KFLD                    WLRQ1PDR
196200950526     C*------------------------------------------------------*
196300950526     C*  Definisco variabili
196400030728     C     *LIKE         DEFINE    BRVNPS        BRVNPS1
196500030728     C     *LIKE         DEFINE    D59NDT        EXNDT
196600950526     C     *LIKE         DEFINE    ARTAAS        KAAS
196700950526     C     *LIKE         DEFINE    ARTLNP        KLNP
196800950526     C     *LIKE         DEFINE    ARTNRS        KNRS
196900950526     C     *LIKE         DEFINE    ARTNSP        KNSP
197000950526     C     *LIKE         DEFINE    ARTFLS        KFLS
197100950526     C     *LIKE         DEFINE    ARTLNA        KLNA
197200950526     C     *LIKE         DEFINE    ARTNSC        KNSC
197300950526     C     *LIKE         DEFINE    BRVNPG        KNPG
197400950526     C     *LIKE         DEFINE    BRVNFV        KNFV
197500950526     C     *LIKE         DEFINE    BRVFGS        KFGS
197600950612     C     *LIKE         DEFINE    BRVFLE        KFLE
197700951121     C     *LIKE         DEFINE    ARBRAG        KRAG
197800950526     C     *LIKE         DEFINE    TBLKUT        KKUT
197900950526     C     *LIKE         DEFINE    TBLCOD        KCOD
198000950915     C     *LIKE         DEFINE    ANMCAA        KCAA
198100980310     C     *LIKE         DEFINE    EVBCEV        KCEV
198200950918     C     *LIKE         DEFINE    ANMCAA        WCAA
198300950918     C     *LIKE         DEFINE    ANMNFV        WNFV
198400950918     C     *LIKE         DEFINE    ANMNPS        WNPS
198500960507     C     *LIKE         DEFINE    FVVFGS        WFGS
198600950605     C     *LIKE         DEFINE    FVVNFV        KNFV1
198700990204     C     *LIKE         DEFINE    TBECOD        KCODE
198800990204     C     *LIKE         DEFINE    DCTAAC        KAAC
198900990204     C     *LIKE         DEFINE    DCTFIL        KFIL
199000990204     C     *LIKE         DEFINE    DCTNCA        KNCA
199100010620     C     *LIKE         DEFINE    DSBRVE        WSBRVE
199200010620     C     *LIKE         DEFINE    DSBRVE        W1BRVE
199300010620     C     *LIKE         DEFINE    BRVNPS        SAWNPS
199400010622     C     *LIKE         DEFINE    BRVFGS        SAWFGS
199500010622     C     *LIKE         DEFINE    BRVNFV        SAWNFV
199600010620     C     *LIKE         DEFINE    BRVDFS        SAWDFS
199700010620     C     *LIKE         DEFINE    BRVHMS        SAWHMS
199800010620     C     *LIKE         DEFINE    D53DFV        SAWDFV
199900010620     C     *LIKE         DEFINE    D53DFV        WDFV
200000010702     C     *LIKE         DEFINE    D53DFV        SA1DFV
200100010620     C     *LIKE         DEFINE    TBLKEY        KKEY
200200950526     C*
200300950526     C                   TIME                    TIMES             6 0
200400950526     C                   TIME                    WHDAT            12 0
200500950526     C                   MOVE      WHDAT         WOGGI             6 0
200600010622     C                   TIME                    W0140            14 0
200700010622     C                   MOVE      W0140         WDTGIO            8 0
200800031009     C                   MOVEL     *ALL'-'       CMP132
200900031009     C                   MOVEL     *ALL'-'       CMP198
201000031009     C                   MOVEL     *ALL'-'       CMP132D
201100031009     C                   MOVEL     *ALL'-'       CMP198D
201200950526     C*
201300030722     C*  Carico dati tabella 4H anomalie da spunte
201400950526     C                   Z-ADD     1             KKUT
201500950526     C                   MOVEL     '4H'          KCOD
201600070601     C                   Z-ADD     0             X                 3 0
201700950526     C     KTAB          CHAIN     TABEL00F                           31
201800950526     C     *IN31         DOWEQ     '0'
201900950526     C     X             ANDLT     100
202000950526     C                   ADD       1             X
202100950526     C                   MOVEL     TBLUNI        DS4H
202200950526     C                   MOVEL     TBLKEY        C4H(X)
202300950526     C                   MOVEL     §4HDES        D4H(X)
202400950526     C                   MOVEL     §4HTPB        T4H(X)
202500950908     C                   MOVEL     §4HSTP        S4H(X)
202600950526     C     KTAB          READE     TABEL00F                               31
202700950526     C                   END
202800010620     C* Chaino tabella variabili pgm arrivi per reperire minuti di
202900010620     C* tolleranza
203000010620     C                   MOVEL     '7A'          KCOD
203100010620     C                   MOVEL(P)  '2       '    KKEY
203200010620     C     KTAB1         CHAIN     TABEL                              31
203300010620     C  N31              MOVEL     TBLUNI        DS7A2
203400010620     C   31              Z-ADD     1             §7AMNP
203500030728     C                   OPEN      FNLRF7P
203600030722     C*  Carico dati tabella DTA anomalie danni
203700990204     C                   Z-ADD     1             KKUT
203800990204     C                   MOVEL     'TAD'         KCODE
203900990204     C                   Z-ADD     0             X
204000990204     C     KCODE         CHAIN     TNTBE01L                           31
204100990204     C     *IN31         DOWEQ     '0'
204200990204     C     X             ANDLT     100
204300990204     C     TBESIF        IFEQ      KNSIF
204400990204     C     TBESIF        OREQ      *BLANKS
204500990204     C                   ADD       1             X
204600990204     C                   MOVEL     TBEKE1        TAD(X)
204700990204     C                   MOVEL     TBEUNI        DTA(X)
204800990204     C                   END
204900990204     C     KCODE         READE     TNTBE01L                               31
205000990204     C                   END
205100030703     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
205200040827     C                   CLEAR                   FIP
205300040827     C     *LOVAL        SETLL     AZORG01L
205400040827     C                   DO        *HIVAL
205500040827     C                   READ      AZORG01L                               95
205600040827     C   95              LEAVE
205700040827     C                   MOVEL     ORGDE3        OG143
205800040827     C     §OgNTW        IFEQ      'PPT'
205900040827     C                   Z-ADD     1             z                 3 0
206000040827     C     *ZEROS        LOOKUP    FIP(z)                                 23
206100040827     C   23              Z-ADD     ORGFIL        FIP(z)
206200040827     C                   END
206300040827     C                   END
206400950526     C*
206500950526     C                   ENDSR
206600950526     O*--------------------------------------------------------------*
206700950526     O*   STAMPA                                                     *
206800950526     O*--------------------------------------------------------------*
206900030728     OSYSPRT    E            SPED1       1
207000990803     O                       EVBAAS        Z      4
207100980325     O                       EVBLNP        Z     16
207200980325     O                       EVBNRS        Z     19
207300980325     O                       EVBNSP        Z     27
207400980325     O                                           80 'DELETE ERRATO'
207500950526     O*--------------------------------------------------------------*
207600950526     O*   AGGIORNAMENTI                                              *
207700950526     O*--------------------------------------------------------------*
207800951109     OFNARB000  E            AGGARB
207900950526     O                       ARBDDC
208000950526     O                       ARBNDC
208100020917     O                       ARBIFP
208200950526     O                       ARBPDC
208300950526     O                       ARBSTP
208400990519     OFNANM000  E            AGGANM
208500950915     O                       ANMDCH
208600950915     O                       ANMFSC
208700950915     O                       ANMCCH
208800950918     O                       ANMNFV
208900950918     O                       ANMNPS
209000010702**
209100040409collo                    spuntato su distinta Nr.       pistola nr.xx
209200040421collo  NON + USATO       non spuntato in uscita ma arrivato pistola nr.xx
209300040421collo  NON + USATO       non spuntato in uscita non arrivato ma partito pistola nr.xx
209400040415VUOTO
209500040421collo  NON + USATO       non spuntato in uscita non arrivato e non partito
209600040419collo                    spuntato in uscita pistola nr.xx ore xx:xx ma già consegnato
209700040415collo                    spuntato in uscita pistola nr.xx ore xx:xx
209800040415                         e su foglio xxx-x-xxxxx pistola nr.xx ore xx:xx
209900040419collo                    non spuntato in uscita ma su f. xxx-x-xxxxx pist. nr.xx ore xx:xx
210000081013collo                    collo doppio
210100040421VUOTO
210200040419collo                    NON IN DISTINTA ma spuntato in uscita pistola nr.xx ore xx:xx
210300101124VUOTO
210400990204**
210500040421        Pratica C.A. aperta:  NON + USATO
210600040421        Pratica C.A. chiusa:  NON + USATO
210700010702**
210800010702In IMA XXX XXXXXX Del XX/XX PstXX
