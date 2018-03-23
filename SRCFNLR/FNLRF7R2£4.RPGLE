000100950525     H DECEDIT('0,') DATEDIT(*DMY.)
000200950525      *---------*----------------------------------------------------*
000300030707      * FNLRF7R2*                                                    *
000400950525      *---------*                                                    *
000500950525      *         - PREPARAZIONE DISTINTA DI CONSEGNA -                *
000600030703      *         --    DA SPUNTE (solo per poste)   --                *
000700950525      *--------------------------------------------------------------*
000800030805     FFIar501L  IF   E           K DISK
000900950526     FFNARBY1L  UF   E           K DISK
001000140507     FFNARBZ0F  UF   E           K DISK    usropn
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
030600140507      **
030700140507     c* Apertura condizionata del file degli STOP per ottimizzare le prestazioni.
030800140507     c*  Se richiamato da azione LR59 o LR60 DEVE  utilizzare e fare il calcolo degli STOP.
030900140507     c*   Quindi Apriamo il file.
031000140507   <-c                   if        kcDAZ = 'LR59' or
031100140507   <-c                             kcDAZ = 'LR60'
031200140507   <-c                   open      fnarbZ0F                             99
031300140507   <-c                   end
031400140507      **
031500031009     C*  Leggo FNARBY dove ho memorizzato le spedizioni della distinta
031600031009     c*  con o senza errori
031700950529     C     *LOVAL        SETLL     FNARBY1L
031800031009     c                   do        *hival
031900031009     C                   READ      FNARBY1L
032000031009     c                   if        %eof(fnarby1l)
032100031009     c                   leave
032200031009     c                   end
032300950525     C                   EXSR      GESSPE
032400950525     C                   DELETE    FNARBY1L
032500030805     C                   ENDdo
032600030825     C*  Controllo se numero bancali caricati è maggiore a quello
032700110518     C*  in FIQDT
032800030825     C     nban          IFgt      D59NBL
032900030805     C                   Z-ADD     1             Y                 3 0
033000030805     C     '16'          LOOKUP    C4H(Y)                                 32
033100031016     C*  Controllo se si deve stampare l'aomalia
033200030805     C     S4H(Y)        IFNE      'N'
033300031016     C                   MOVEL     D4H(Y)        DErrq
033400110414     C                   CLEAR                   Terrq
033500031016     C                   Z-ADD     nban          WNRB1
033600031016     C                   Z-ADD     D59NBL        WNRB2
033700030805     C                   MOVEL     'S'           D59ANM
033800101201     c                   if        d59sqa =  ' ' or
033900101201     c                             d59sqa =  'T'
034000101125     c                   exsr      states
034100030805     C                   write     ERRQUA
034200101125     c                   end
034300030805     C                   ADD       2             D59LIN
034400030805     C                   END
034500030805     C                   END
034600010620     C*  Routine di stampa spunte consegna doppie
034700101201     c                   if        d59sqa =  ' ' or
034800101201     c                             d59sqa =  'T'
034900010620     C                   EXSR      DOPPIE
035000101125     c                   end
035100950526     C*  Loop x eventuale cancellazione FNARBZ
035200140507      *  solo
035300140507     c*  Se richiamato da azione LR59 o LR60 DEVE  utilizzare
035400140507     c*  e fare il calcolo degli STOP.
035500140507   <-c                   if         kcdaz='LR59' or kcdaz='LR60'
035600950526     C                   EXSR      DLTARZ
035700140507   <-c                   end
035800140507      *
035900030721     C                   MOVEL(p)  FNLR59        KPJBU
036000950525     C                   SETON                                        RT
036100950525     C*--------------------------------------------------------------*
036200950526     C*  GESSPE: Gestione spedizione
036300950525     C*--------------------------------------------------------------*
036400950526     C     GESSPE        BEGSR
036500950525     C*  Cerco la spedizione su FNARB
036600950525     C                   Z-ADD     ARBAAS        KAAS
036700950525     C                   Z-ADD     ARBLNP        KLNP
036800950525     C                   Z-ADD     ARBNRS        KNRS
036900950525     C                   Z-ADD     ARBNSP        KNSP
037000950525     C     KARB          CHAIN     FNARB01L                           33
037100030825     c                   if        not *in33
037200031009     c* mi salvo la spedizione
037300031009     C                   MOVEL     DSARB         DSARBS
037400030805     C* CALCOLO IL NUMERO DEI BANCALI
037500030805     c                   exsr      srnban
037600031009     c* IMPOSTA I CAMPI IN STAMPA
037700110414     C                   MOVEL     ARBTSP        TIPERR
037800031009     C                   Z-ADD     ARBAAS        WAAS
037900031009     C                   Z-ADD     ARBLNP        WLNP
038000031009     C                   Z-ADD     ARBNRS        WNRS
038100031009     C                   Z-ADD     ARBNSP        WNSP
038200030731     C                   Z-ADD     ARBlna        Wlna
038300031009     C                   MOVEL     ARBRSD        WRSD
038400031009     C                   Z-ADD     ARBNCD        WNCD
038500031009     C                   Z-ADD     ARBNCA        WNCA
038600031009     C                   Z-ADD     ARBZNC        WZNC
038700030805     C                   movel     ARBmgs        Wcommes           2 0
038800030805     C                   move      ARBmgs        Wcomgg            2 0
038900031009     c                   movel     wcomgg        wdat
039000030805     c                   move      wcommes       wdat
039100031009     C* segnacolli non consecutivi
039200031009     C     ARBFNS        COMP      'S'                                    44
039300031009     C                   IF        NOT *IN44
039400031009     C                   MOVEL     '/'           WBAR
039500031009     C                   ELSE
039600031009     C                   CLEAR                   WBAR
039700031009     C                   END
039800030825     C                   END
039900031009     C* LOOP SU ERRORI
040000950525     C                   MOVEL     'N'           WERRBL            1
040100950525     C                   MOVEL     'N'           WNODIS            1
040200950525     C                   Z-ADD     1             X                 3 0
040300010702     C     X             DOWLE     10
040400950525     C     CDA(X)        ANDNE     *BLANKS
040500950530     C                   MOVEL     CDA(X)        WCDANO            2
040600950525     C                   EXSR      GESANO
040700950530     C                   ADD       1             X
040800031009     C                   ENDdo
040900950525     C*  Controllo se adesso è già stata messa in consegna
041000950529     C     ARBNDC        IFNE      *ZEROS
041100950530     C     ARBDCM        ANDEQ     0
041200950529     C     ARBNDC        ANDNE     D59NDT
041300950525     C     WNODIS        ANDEQ     'N'
041400031009     C                   MOVEL     '03'          WCDANO
041500950525     C                   EXSR      GESANO
041600040713     c* rendo bloccante l'errore anche se in tabella è stato deciso di
041700040713     c* non stamparlo
041800040713     C                   MOVEL     'S'           WERRBL
041900950525     C                   END
042000070530     c* METTE IN DISTINTA LA SPEDIZIONE se pgm "spunte"
042100040827     c                   movel     arbaas        arbdsp            8 0
042200040827     c                   move      arbmgs        arbdsp
042300031016     C* Se non ho trovato errore bloccante aggiorno la distinta
042400030721     C     WERRBL        IFEQ      'N'
042500070601     c                   if        arbngd=0 and arbndc = 0 and
042600070530     c                             (kcdaz='LR59' or kcdaz='LR60')
042700020917     C                   Z-ADD     D59FGS        ARBIFP
042800020917     C                   Z-ADD     D59NDT        ARBNDC
042900950525     C                   Z-ADD     D59DDT        ARBDDC
043000950525     C                   Z-ADD     D59PDR        ARBPDC
043100140410     C                   movel     *blanks       ARBfdc
043200140410     c                   z-add     0             arbnmi
043300140410     c                   z-add     0             arbnss
043400951121     C*  Attribuisco lo stop
043500951211     C     ARBKCS        IFNE      0
043600951211     C                   MOVEL     *BLANKS       KRAG
043700951211     C                   MOVEL     ARBKCS        KRAG
043800951211     C                   ELSE
043900951121     C                   MOVEL     ARBRSD        KRAG
044000951122     C                   MOVE      ARBIND        KRAG
044100951211     C                   END
044200951121     C     KRAG          CHAIN     FNARBZ0F                           32
044300950526     C*  Aggiorno la  bolla
044400950526     C                   EXCEPT    AGGARB
044500031013     C                   MOVEL     DSARB         DSARBS
044600031020     C                   END
044700031009     C*  DETTAGLIO SEGNACOLLI
044800980330     C                   Z-ADD     ARBAAS        KAAS
044900980330     C                   Z-ADD     ARBLNP        KLNP
045000980330     C                   Z-ADD     ARBNRS        KNRS
045100980330     C                   Z-ADD     ARBNSP        KNSP
045200950614     C                   MOVEL     'N'           WERRPZ            1
045300031009     C                   MOVEL     'N'           OKMIC             1
045400950614     C     KART          CHAIN     FNART01L                           32
045500030721     C*  Se non esiste e spedizione figlia mi posiziono su mamma
045600950614     C     *IN32         IFEQ      '1'
045700950606     C     KLBL          CHAIN     FNLBL01L                           32
045800950606     C     *IN32         IFEQ      '0'
045900950606     C     LBLLAP        ANDEQ     LBLLAN
046000950606     C                   Z-ADD     LBLAAP        KAAS
046100950606     C                   Z-ADD     LBLLPP        KLNP
046200950606     C                   Z-ADD     LBLNRP        KNRS
046300950606     C                   Z-ADD     LBLNSP        KNSP
046400950606     C     KARB          CHAIN     FNARB01L                           32
046500980430     C     ARBCCA        IFNE      '2'
046600980430     C     ARBCCA        ANDNE     '6'
046700950614     C     KART          CHAIN     FNART01L                           32
046800980430     C                   ELSE
046900980430     C                   Z-ADD     LBLAAN        KAAS
047000980430     C                   Z-ADD     LBLLPN        KLNP
047100980430     C                   Z-ADD     LBLNRN        KNRS
047200980430     C                   Z-ADD     LBLNSN        KNSP
047300980430     C     KARB          CHAIN     FNARB01L                           32
047400950614     C                   END
047500980430     C                   END
047600950614     C                   END
047700950614     C*  Loop su dettaglio segnacolli per verifcare se la bolla
047800950614     C*  è consegnata parzialmente su altra distinta
047900950526     C     *IN32         DOWEQ     '0'
048000000705     C                   EXSR      GESDET
048100031009     C                   ENDdo
048200070604     c* SCRIVO EVENTO MIC E ORM CONTESTUALE ALLA CONSEGNA
048300031020     c                   movel     dsarbs        dsarb
048400070530     c                   if        okmic = 'S'
048500100514     c                   clear                   ds7r
048600100514     c                   if        arbgma <> ' '
048700100514     c                   exsr      sr7r
048800100514     c                   end
048900100514     c                   if        §7RRC ='S'
049000031020     c                   clear                   fnlr99ds
049100071002     c                   eval      i99nftl= 99999
049200071002     c                   eval      i99fao = 400
049300031009     c                   exsr      aggorm
049400031020     c                   end
049500031020     c                   exsr      aggeve
049600070601     c*                  end
049700070530     c                   end
049800950526     C*
049900951109     C                   ELSE
050000030721     C  N33              unlock    fnarb01l
050100950526     C                   END
050200950526     C*
050300950526     C                   ENDSR
050400031003     C*--------------------------------------------------------------*
050500031003     C*  GESDET: Gestione dettaglio segnacolli
050600031003     C*--------------------------------------------------------------*
050700031003     C     GESDET        BEGSR
050800950526     C*
050900040409     C                   SETOFF                                       02
051000031009     C                   MOVEL     *BLANKS       WTIPCA
051100031009     C                   MOVEL     *BLANKS       WDETER
051200950530     C                   MOVEL     *BLANKS       WNRP              2
051300031009     C                   Z-ADD     0             WNRP1             2 0
051400950614     C                   MOVEL     'N'           WTRDAT            1
051500950614     C                   MOVEL     'N'           WTRDA1            1
051600950918     C                   MOVEL     'N'           WTRSPU            1
051700950526     C*  Controllo se non presente su distinta
051800950526     C                   Z-ADD     4             KNPG
051900950526     C                   Z-ADD     D59NDT        KNFV
052000950526     C                   Z-ADD     D59FGS        KFGS
052100950526     C                   Z-ADD     ARBLNA        KLNA
052200950526     C                   Z-ADD     ARBFLS        KFLS
052300950526     C                   Z-ADD     ARBNRS        KNRS
052400950526     C                   Z-ADD     ARTNSC        KNSC
052500000706      *
052600070129     C     KBRV1         CHAIN     FNBRV01L                           33
052700040408     c* NON HO LA SPUNTA 4
052800030729     C     *IN33         IFEQ      '1'
052900040421     c* solo se la spedizione è in distinta
053000040507     c                   if        s_arbndc  <> 0
053100990204     C* Controllo Dati Danni
053200040420     C*                  EXSR      TESDAN
053300950526     C*  Se non è presente su questa distinta verifico se è
053400950526     C*  in consegna su di un'altra
053500040408     c                   exsr      srfibrv
053600040408     c                   movel     okspu         wtrdat
053700030801     C*    se su altra distinta
053800030729     C     WTRDAT        IFEQ      'S'
053900950526     C     BRVNFV        ANDNE     0
054000040409     C                   MOVEL(p)  ERR(1)        WDETER
054100101124     c                   movel     '001'         tbeke2
054200040419     c                   movel     fvvnfv        com05
054300040419     c                   eval      %subst(wdeter: 51: 5)= com05
054400040409     c                   eval      %subst(wdeter: 71: 20)= fvvdsf
054500101126     c                   eval      sfgs=fvvfgs
054600101126     c                   eval      snpg=fvvnpg
054700101126     c                   eval      snfv=fvvnfv
054800110117     c                   eval      sdfv=fvvdfv
054900101126     c                   eval      shms=brvhms
055000950614     C                   MOVEL     BRVNPS        WNRP
055100950526     C                   EXSR      STAMPA
055200960507     C*  Richiamo TRUL11 per reperimento della fase
055300960507     C                   CLEAR                   DSFASE
055400960507     C                   MOVEL     '4'           WNPG
055500960507     C                   CALL      'TRUL11R'
055600960507     C                   PARM                    DSFASE
055700950919     C                   MOVEL     'S'           WTRSPU
055800950918     C                   Z-ADD     150           WCAA
055900950918     C                   Z-ADD     BRVNFV        WNFV
056000950918     C                   Z-ADD     BRVNPS        WNPS
056100960507     C                   Z-ADD     FVVFGS        WFGS
056200950918     C                   EXSR      WRTANM
056300030729     C                   END
056400950614     C*  Controllo se collo spuntato in I.M.A.
056500950614     C                   Z-ADD     3             KNPG
056600040408     c                   exsr      srfibrv
056700040408     c                   movel     okspu         wtrda1
056800040408     C*
056900950918     C                   MOVEL     *BLANKS       WDETER
057000950919     C                   SETOFF                                       02
057100950614     C                   Z-ADD     4             KNPG
057200030801     c*    se in inventario IMA
057300030729     C     WTRDA1        IFEQ      'S'
057400950614     C     BRVNFV        ANDNE     0
057500950614     C                   MOVEL     BRVNPS        WNRP
057600030801     c*    ...e su altra distinta
057700950919     C     WTRSPU        IFEQ      'S'
057800960507     C     FVVFCF        IFNE      'S'
057900040421     C                   MOVEL(p)  ERR(8)        WDETER
058000101124     c                   movel     '008'         tbeke2
058100040419     c                   eval      sfgs=fvvfgs
058200040419     c                   eval      snpg=fvvnpg
058300040419     c                   eval      snfv=fvvnfv
058400110117     c                   eval      sdfv=fvvdfv
058500040419     c                   eval      shms=brvhms
058600960507     C                   END
058700950919     C                   ELSE
058800030801     c*    ...no su altra distinta
058900040415     C                   MOVEL(p)  ERR(9)        WDETER
059000101124     c                   movel     '009'         tbeke2
059100040419     c                   eval      sfgs=fvvfgs
059200040419     c                   eval      snpg=fvvnpg
059300040419     c                   eval      snfv=fvvnfv
059400110117     c                   eval      sdfv=fvvdfv
059500040419     c                   eval      shms=brvhms
059600950919     C                   END
059700030801     c*
059800960507     C     WTRSPU        IFNE      'S'
059900960507     C     FVVFCF        ORNE      'S'
060000950614     C                   EXSR      STAMPA
060100960507     C*  Richiamo TRUL11 per reperimento della fase
060200960507     C                   CLEAR                   DSFASE
060300960507     C                   MOVEL     '3'           WNPG
060400960507     C                   MOVEL     'A'           WSPG
060500960507     C                   CALL      'TRUL11R'
060600960507     C                   PARM                    DSFASE
060700950919     C                   MOVEL     'S'           WTRSPU
060800950918     C                   Z-ADD     135           WCAA
060900950920     C                   Z-ADD     BRVNFV        WNFV
061000950918     C                   Z-ADD     WNRP1         WNPS
061100960507     C                   Z-ADD     FVVFGS        WFGS
061200950915     C                   EXSR      WRTANM
061300960507     C                   END
061400030729     C                   END
061500950918     C     WTRSPU        IFEQ      'S'
061600950918     C                   GOTO      NEWSGN
061700950918     C                   END
061800950526     C*  Controllo se il collo è arrivato
061900040419     C*    ARTDAM        IFNE      0
062000040419     C*    ARTDCM        ANDEQ     0
062100040419     C*    WDAVM         ANDEQ     'N'
062200040419     C*                  MOVEL(p)  ERR(2)        WDETER
062300040419     C*                  MOVEL     ARTNAP        WNRP
062400040419     C*                  EXSR      STAMPA
062500040419     C*                  GOTO      NEWSGN
062600040419     C*                  END
062700950526     C*  Collo partito e non arrivato
062800040419     C*    ARTDFV        IFNE      0
062900040419     C*    ARTDCM        ANDEQ     0
063000040419     C*    WDAVM         ANDEQ     'N'
063100040419     C*                  MOVEL(p)  ERR(3)        WDETER
063200040419     C*                  MOVEL     ARTNPP        WNRP
063300040419     C*                  EXSR      STAMPA
063400040419     C*                  GOTO      NEWSGN
063500040419     C*                  END
063600950526     C*  Collo non arrivato e non partito
063700040420     C*    ARTDCM        IFEQ      0
063800040420     C*    WDAVM         ANDEQ     'N'
063900040420     C*                  MOVEL(p)  ERR(5)        WDETER
064000040420     C*                  EXSR      STAMPA
064100040420     C*                  GOTO      NEWSGN
064200040420     C*                  END
064300950526     C*
064400040421     C                   end
064500030729     C                   ELSE
064600081103     c*
064700081103     C                   MOVEL     BRVNPS        WNRP
064800081103     C                   Z-ADD     BRVNPS        WNRP1
064900081103     C                   eval      dsbrvs = dsbrv
065000040408     c* HO LA SPUNTA 4
065100081014     c* per prima memorizzo segnazione di probabile etichetta doppia
065200081014     c                   clear                   fnlrv7ds
065300081014     c                   eval      ILRV7FLS=kfls
065400081014     c                   eval      ILRV7LNA=klna
065500081014     c                   eval      ILRV7NRS=knrs
065600081014     c                   eval      ILRV7NSC=knsc
065700081014     c                   movel(p)  fnlrv7ds      kpjbu
065800081014     c                   call      'FNLRV7R'
065900081014     c                   parm                    kpjba
066000081014     c                   movel     kpjbu         fnlrv7ds
066100081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
066200090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
066300081014     C                   MOVEL(p)  ERR(10)       WDETER
066400101124     c                   movel     '010'         tbeke2
066500101126     c                   eval      shms=s_brvhms
066600101126     c                   eval      sfgs=s_brvfgs
066700101126     c                   eval      snpg=s_brvnpg
066800110117     c                   eval      snfv=s_brvnfv
066900110117     c                   eval      sdfv=d59ddt
067000081014     C                   EXSR      STAMPA
067100081014     C                   END
067200040408     C* Controllo se collo spuntato anche in I.M.A.
067300040408     C                   Z-ADD     3             KNPG
067400040408     c                   exsr      srfibrv
067500040408     c                   movel     okspu         wtrda1
067600040408     C                   Z-ADD     4             KNPG
067700110118     c**********************************************************************
067800110118     c* se sono chiamato dalla sistemazione controllo aut, non segnalo
067900110118     c* l'errore e tento di cancellare la spunta + vecchia tra la 3 e la 4
068000110120     c                   if        kcdaz = 'LRQ1' and wtrda1 = 'S'
068100110118     c                   exsr      srls46
068200110118     c* se no errore ed effettuato l'aggiornamento proseguo
068300110118     c                   if        OLS46ERR = ' ' and
068400110118     c                             OLS46ANN = 'S'
068500110118     c* se cancellato la 3 e resta la 4 verifico se bolla in distinta ok
068600110118     c* segnalo errore
068700110118     c                   if        s_brvhms > brvhms
068800110118     c                   if        s_arbndc = 0
068900110118     c                   eval      wtrda1 = 'N'
069000110118     c                   clear                   dsbrv
069100110118     c                   clear                   dsfvv
069200110118     c                   else
069300110118     C                   GOTO      NEWSGN
069400110118     c                   end
069500110118     c                   else
069600110118     c* se cancellato la 4 e resta la 3 verifico se bolla in distinta errore
069700110118     c* altrimenti ok
069800110118     c                   if        s_arbndc = 0
069900110118     C                   GOTO      NEWSGN
070000110118     c                   else
070100110118     C                   MOVEL(p)  ERR(9)        WDETER
070200110118     c                   movel     '009'         tbeke2
070300110118     c                   eval      sfgs=fvvfgs
070400110118     c                   eval      snpg=fvvnpg
070500110118     c                   eval      snfv=fvvnfv
070600110118     c                   eval      sdfv=fvvdfv
070700110118     c                   eval      shms=brvhms
070800110118     C                   EXSR      STAMPA
070900110118     C                   GOTO      NEWSGN
071000110118     c                   end
071100110118     c                   end
071200110118     c                   end
071300110118     c                   end
071400110118     c**********************************************************************
071500040415     C* Se ho SOLO la spunta 4 ma la spedizione non è in distinta
071600040408     C* segnalo l'errore
071700110118     C                   IF        s_arbndc = 0
071800040408     C                   IF        wtrda1 = 'N'
071900040415     C                   MOVEL(p)  ERR(12)       WDETER
072000101124     c                   movel     '012'         tbeke2
072100101126     c                   eval      sfgs=s_brvfgs
072200101126     c                   eval      snpg=s_brvnpg
072300101126     c                   eval      snfv=s_brvnfv
072400040419     c                   eval      shms=s_brvhms
072500110117     c                   eval      sdfv=d59ddt
072600040408     C                   EXSR      STAMPA
072700040408     C                   GOTO      NEWSGN
072800040408     C* Se ho la spunta 4 e la spunta 3 ma la spedizione non è in distinta
072900040408     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
073000040408     C* spunta 4 > di ora spunta 3
073100040408     c                   else
073200110118     c
073300040409     c                   if        s_brvnps <> brvnps or
073400040409     c                             (s_brvnps = brvnps and s_brvhms > brvhms)
073500101124     C                   MOVEL(p)  ERR(12)       WDETER
073600101124     c                   movel     '012'         tbeke2
073700040419     c                   eval      shms=s_brvhms
073800101126     c                   eval      sfgs=s_brvfgs
073900101126     c                   eval      snpg=s_brvnpg
074000101126     c                   eval      snfv=s_brvnfv
074100101126     c                   eval      shms=s_brvhms
074200110117     c                   eval      sdfv=d59ddt
074300040415     C                   EXSR      STAMPA
074400040419     C                   MOVEL(p)  ERR(8)        WDETER
074500101124     c                   movel     '008'         tbeke2
074600040419     c                   eval      sfgs=fvvfgs
074700040419     c                   eval      snpg=fvvnpg
074800040419     c                   eval      snfv=fvvnfv
074900110117     c                   eval      sdfv=fvvdfv
075000040419     c                   eval      shms=brvhms
075100040419     c                   movel     brvnps        wnrp
075200040415     c                   seton                                        02
075300040409     C                   EXSR      STAMPA
075400040415     c                   setoff                                       02
075500040415     C                   GOTO      NEWSGN
075600040415     c                   else
075700040408     C                   GOTO      NEWSGN
075800040408     c                   end
075900040408     C                   END
076000040409     C                   END
076100040408     c*
076200031013     c                   eval      okmic = 'S'
076300031009     C* Se è stato spuntato con la distinta in esame controllo
076400031009     C* se segnacollo già consegnato
076500030729     C     ARTDCM        IFNE      0
076600040415     C                   MOVEL(P)  ERR(6)        WDETER
076700101124     c                   movel     '006'         tbeke2
076800040419     c                   eval      shms=s_brvhms
076900101126     c                   eval      sfgs=s_brvfgs
077000101126     c                   eval      snpg=s_brvnpg
077100101126     c                   eval      snfv=s_brvnfv
077200110117     c                   eval      sdfv=d59ddt
077300950526     C                   EXSR      STAMPA
077400950614     C                   GOTO      NEWSGN
077500030729     C                   END
077600040421     C* Se ho la spunta 4 e la spunta 3 e la spedizione è in distinta
077700040421     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
077800040421     C* spunta 3 > di ora spunta 4
077900040421     c                   if        s_brvnps <> brvnps or
078000040421     c                             (s_brvnps = brvnps and s_brvhms < brvhms)
078100030729     C     WTRDA1        IFEQ      'S'
078200040408     C     BRVNFV        ANDNE     0
078300960507     C     FVVFCF        IFNE      'S'
078400040415     C                   MOVEL(P)  ERR(7)        WDETER
078500101124     c                   movel     '007'         tbeke2
078600040419     c                   eval      shms=s_brvhms
078700101126     c                   eval      sfgs=s_brvfgs
078800101126     c                   eval      snpg=s_brvnpg
078900101126     c                   eval      snfv=s_brvnfv
079000110117     c                   eval      sdfv=d59ddt
079100040409     C                   EXSR      STAMPA
079200040415     C                   MOVEL(P)  ERR(8)        Wdeter
079300101124     c                   movel     '008'         tbeke2
079400040419     c                   eval      sfgs=fvvfgs
079500040419     c                   eval      snpg=fvvnpg
079600040419     c                   eval      snfv=fvvnfv
079700110117     c                   eval      sdfv=fvvdfv
079800040419     c                   eval      shms=brvhms
079900040419     c                   movel     brvnps        wnrp
080000040415     c                   seton                                        02
080100950612     C                   EXSR      STAMPA
080200040415     c                   setoff                                       02
080300960507     C*  Richiamo TRUL11 per reperimento della fase
080400960507     C                   CLEAR                   DSFASE
080500960507     C                   MOVEL     '3'           WNPG
080600960507     C                   MOVEL     'A'           WSPG
080700960507     C                   CALL      'TRUL11R'
080800960507     C                   PARM                    DSFASE
080900950918     C                   Z-ADD     135           WCAA
081000950920     C                   Z-ADD     BRVNFV        WNFV
081100950918     C                   Z-ADD     WNRP1         WNPS
081200960507     C                   Z-ADD     FVVFGS        WFGS
081300950915     C                   EXSR      WRTANM
081400031009     C                   GOTO      NEWSGN
081500030729     C                   END
081600030729     C                   END
081700040421     C                   END
081800950526     C*
081900030729     C                   END
082000950526     C*  Leggo successivo segnacollo
082100950614     C     NEWSGN        TAG
082200950526     C     KART          READE     FNART01L                               32
082300950526     C*
082400950526     C                   ENDSR
082500031009     C*--------------------------------------------------------------*
082600040408     C*  srfibrv cerco spunta nel foglio 4/3 nel gg. della distinta che
082700040408     c*          sto quadrando
082800031009     C*--------------------------------------------------------------*
082900040408     C     srfibrv       BEGSR
083000040408     c                   movel     'N'           okspu             1
083100040409     c                   clear                   dsbrvc
083200040409     c                   clear                   dsfvvc
083300031009     C*
083400070129     C     KBRV2         SETll     FNBRV09L
083500040408     c                   do        *hival
083600070129     C     KBRV2         READe     FNBRV09L
083700070129     c                   if        %eof(fnbrv09l)
083800040408     c                   leave
083900040408     c                   end
084000040408     C                   Z-ADD     BRVNFV        KNFV1
084100040408     C     KFVV          CHAIN     FNFVV01L
084200040408     c                   if        %found(fnfvv01l) and fvvdfv = d59ddt
084300040408     C                   MOVEL     'S'           okspu
084400040409     c                   if        brvhms > c_brvhms
084500040409     c                   eval      dsbrvc = dsbrv
084600040409     c                   eval      dsfvvc = dsfvv
084700040408     c                   end
084800040409     c                   end
084900040408     C                   ENDDO
085000040409     c*
085100040409     c                   eval      dsbrv = dsbrvc
085200040409     c                   eval      dsfvv = dsfvvc
085300040408     C*
085400040408     C                   ENDSR
085500040408     C*--------------------------------------------------------------*
085600040408     C*  AGGEVE: scrivo evento MIC sia per distinte manuali che x quelle
085700040408     c*          da spunte
085800040408     C*--------------------------------------------------------------*
085900040408     C     AGGEVE        BEGSR
086000040408     C*
086100131104     C                   clear                   dEVB01
086200131104     C                   move      *zeros        §NOTNDC
086300131104     C                   move      *zeros        §NOTPDC
086400131104     C                   move      ARBPDC        §NOTPDC
086500131104     C                   move      ARBNDC        §NOTNDC
086600131104     C                   MOVEL     Devb01        dEVB01sav
086700031009     C* Nel momento in cui metto in consegna la bolla controllo se
086800031009     C* ho scritto evento NIC provvisorio nell'archivio eventi della
086900031009     C* QTEMP e nel caso lo cancello
087000031009     C                   Z-ADD     ARBAAS        KAAS
087100031009     C                   Z-ADD     ARBLNP        KLNP
087200031009     C                   Z-ADD     ARBNRS        KNRS
087300031009     C                   Z-ADD     ARBNSP        KNSP
087400031009     C     KLBL          CHAIN     FNLBL01L                           32
087500031009     C     *IN32         IFEQ      '0'
087600031009     C     LBLLAP        ANDEQ     LBLLAN
087700031009     C                   Z-ADD     LBLAAP        KAAS
087800031009     C                   Z-ADD     LBLLPP        KLNP
087900031009     C                   Z-ADD     LBLNRP        KNRS
088000031009     C                   Z-ADD     LBLNSP        KNSP
088100031009     C     KARB          CHAIN(N)  FNARB01L                           32
088200031009     C     ARBCCA        IFEQ      '2'
088300031009     C     ARBCCA        OREQ      '6'
088400031009     C                   Z-ADD     LBLAAN        KAAS
088500031009     C                   Z-ADD     LBLLPN        KLNP
088600031009     C                   Z-ADD     LBLNRN        KNRS
088700031009     C                   Z-ADD     LBLNSN        KNSP
088800031009     C     KARB          CHAIN(N)  FNARB01L                           32
088900031009     C                   SETON                                        32
089000031009     C                   END
089100031009     C                   END
089200031009     C     *IN32         IFEQ      '0'
089300031009     C     LBLLAP        ANDEQ     LBLLAN
089400031009     C                   Z-ADD     LBLAAP        KAAS
089500031009     C                   Z-ADD     LBLLPP        KLNP
089600031009     C                   Z-ADD     LBLNRP        KNRS
089700031009     C                   Z-ADD     LBLNSP        KNSP
089800031009     C                   ELSE
089900031009     C                   Z-ADD     ARBAAS        KAAS
090000031009     C                   Z-ADD     ARBLNP        KLNP
090100031009     C                   Z-ADD     ARBNRS        KNRS
090200031009     C                   Z-ADD     ARBNSP        KNSP
090300031009     C                   END
090400130326     C     KEVB1         CHAIN     WFNEVB01L                          36
090500031009     C     EVBCEV        IFEQ      'NIC'
090600130326     C  N36              DELETE    WFNEVB01L
090700031009     C                   ELSE
090800031009     C  N36              EXCEPT    SPED1
090900031009     C                   END
091000131218      **
091100031009     C* Aggiorno evento di messa in consegna
091200031009     C                   MOVEL     'MIC'         KCEV
091300031009     C     KEVB2         SETGT     FNEVB04L
091400031009     C     KEVB2         READPE    FNEVB04L                               37
091500131218      **
091600031009     C     *IN37         IFEQ      '1'
091700031009     C     EVBDEV        ORNE      D59DDT
091800131218      **
091900031009     C                   MOVEL     'MIC'         EVBCEV
092000031009     C                   Z-ADD     D59AMG        EVBDTV
092100031009     C                   TIME                    EVBORV
092200031009     C                   Z-ADD     D59FGS        EVBFLE
092300031009     C                   MOVE      D59CDU        EVBCDU
092400031009     C                   Z-ADD     KAAS          EVBAAS
092500031009     C                   Z-ADD     KLNP          EVBLNP
092600031009     C                   Z-ADD     KNRS          EVBNRS
092700031009     C                   Z-ADD     KNSP          EVBNSP
092800031009     C                   Z-ADD     D59DDT        EVBDEV
092900031009     C                   Z-ADD     0             EVBDTR
093000031009     C                   MOVEL     *BLANKS       EVBFTR
093100031009     C                   Z-ADD     0             EVBDTC
093200131104     C                   MOVEL     Devb01sav     EVBNOT
093300031009     C     D59FPP        IFEQ      'M'
093400031009     C                   MOVEL     0800          EVBHEV
093500031009     C                   ELSE
093600031009     C                   MOVEL     1200          EVBHEV
093700031009     C                   END
093800031009     C                   WRITE     FNEVB000
093900131218      **
094000031009     C                   ELSE
094100131218      **
094200031009     C     EVBCEV        IFEQ      'MIC'
094300031009     C     EVBDEV        ANDEQ     D59DDT
094400031009     C*
094500031009     C     EVBDTR        IFEQ      0
094600131218     C*
094700131218     C* riaggancia il record  FISICO x aggiornarlo
094800131218     c     evb_nrr       chain     fnevb00F
094900131218     C*
095000131218     c                   if        %Found(fnevb00F)
095100031009     C* Se non è stato trasmesso aggiorno la data e l'ora di
095200031009     C* di immissione evento con quella del giorno + altri dati
095300031009     C                   Z-ADD     D59AMG        EVBDTV
095400031009     C                   TIME                    EVBORV
095500031009     C                   Z-ADD     D59FGS        EVBFLE
095600031009     C                   MOVE      D59CDU        EVBCDU
095700131104     C                   MOVEL     Devb01sav     EVBNOT
095800031009     C     D59FPP        IFEQ      'M'
095900031009     C                   MOVEL     0800          EVBHEV
096000031009     C                   ELSE
096100031009     C                   MOVEL     1200          EVBHEV
096200031009     C                   END
096300031009     C                   MOVEL     *BLANKS       EVBDTC
096400031009     C                   UPDATE    FNEVB000
096500131218     c                   end
096600131218      **
096700031009     C                   ELSE
096800131218      **
096900031009     C* Se l'evento MIC esiste già ed è già stato trasmesso lo
097000031009     C* riscrivo solo se c'è un NIC successivo
097100031029     C                   MOVE      EVBDEV        xdtMIC
097200031029     C                   MOVE      EVBHEV        xoraMIC
097300031029     C                   MOVE      EVBdtv        xdtwrtMIC
097400031029     C                   MOVE      EVBorv        xorawrtMIC
097500131218      **
097600031009     C                   MOVEL     'NIC'         KCEV
097700031009     C     KEVB2         SETGT     FNEVB04L
097800031009     C     KEVB2         READPE    FNEVB04L                               37
097900131218      **
098000031009     C     *IN37         IFEQ      '0'
098100031009     C     EVBDEV        ANDEQ     D59DDT
098200131218      **
098300031029     C                   MOVE      EVBDEV        xdtNIC
098400031029     C                   MOVE      EVBHEV        xoraNIC
098500031029     C                   MOVE      EVBdtv        xdtwrtNIC
098600031029     C                   MOVE      EVBorv        xorawrtNIC
098700031029     C     dsxMIC        IFLE      dsxNIC
098800031009     C                   MOVEL     'MIC'         EVBCEV
098900031009     C                   Z-ADD     D59AMG        EVBDTV
099000031009     C                   TIME                    EVBORV
099100031009     C                   Z-ADD     D59FGS        EVBFLE
099200031009     C                   MOVE      D59CDU        EVBCDU
099300031009     C                   Z-ADD     KAAS          EVBAAS
099400031009     C                   Z-ADD     KLNP          EVBLNP
099500031009     C                   Z-ADD     KNRS          EVBNRS
099600031009     C                   Z-ADD     KNSP          EVBNSP
099700031009     C                   Z-ADD     D59DDT        EVBDEV
099800031009     C                   Z-ADD     0             EVBDTR
099900031009     C                   MOVEL     *BLANKS       EVBFTR
100000031009     C     D59FPP        IFEQ      'M'
100100031009     C                   MOVEL     0800          EVBHEV
100200031009     C                   ELSE
100300031009     C                   MOVEL     1200          EVBHEV
100400031009     C                   END
100500031009     C                   Z-ADD     0             EVBDTC
100600131104     C                   MOVEL     Devb01sav     EVBNOT
100700031009     C                   WRITE     FNEVB000
100800131218      **
100900031009     C                   END
101000031009     C*
101100031009     C                   END
101200031009     C                   END
101300031009     C                   END
101400031009     C                   END
101500031020     c* mi reimposto la ds di FNARB originale
101600031020     c                   movel     dsarbs        dsarb
101700031009     C*
101800031009     C                   ENDSR
101900031009     C*--------------------------------------------------------------*
102000100514     C*  sr7r  particolarità consegna
102100031009     C*--------------------------------------------------------------*
102200100514     C     sr7r          BEGSR
102300031009     c*
102400100514     C                   eval      kcod = '7R'
102500100514     C                   eval      kkey = arbgma
102600100514     c                   eval      kkut = 1
102700100514     C     ktab1         chain     Tabel00f
102800100514     c                   if        %found(tabel00f)
102900100514     C                   MOVEL     TBLUNI        DS7R
103000100514     c                   end
103100100514     C*
103200100514     C                   ENDSR
103300100514     C*--------------------------------------------------------------*
103400100514     C*  aggorm: Aggiorno ORM e sue fasi
103500100514     C*--------------------------------------------------------------*
103600100514     C     aggorm        BEGSR
103700100514     c*
103800031009     c                   eval      i99tla = 'L'
103900031020     c                   eval      i99aas = arbaas
104000031020     c                   eval      i99lnp = arblnp
104100031020     c                   eval      i99nrs = arbnrs
104200031020     c                   eval      i99nsp = arbnsp
104300031020     c* metto in distinta
104400071001     c                   eval      i99fgs = d59fgs
104500031020     c                   if        i99fao = 400
104600031020     c                   eval      i99ndc = arbndc
104700031020     c                   eval      i99ddc = arbddc
104800031009     c                   eval      i99npg = 4
104900031020     c                   end
105000031020     c                   eval      i99comit = '1'
105100071001     c                   eval      i99cmt = 'N'
105200031009     c                   call      'FNLR99R'
105300031009     c                   parm                    kpjba
105400031009     c                   parm                    fnlr99ds
105500031009     c*
105600031009     C                   ENDSR
105700110118     C*--------------------------------------------------------------*
105800110118     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
105900110118     c* dal controllo a video
106000110118     C*--------------------------------------------------------------*
106100110118     C     srls46        BEGSR
106200110118     c*
106300110118     c                   clear                   fnls46ds
106400110118     c                   clear                   kpjbu
106500110118     c                   eval      ILS46NPGA =  brvnpg
106600110118     c                   eval      ILS46NFVA =  brvnfv
106700110118     c                   eval      ILS46FGSA =  brvfgs
106800110120     c                   eval      ILS46NPGB =  s_brvnpg
106900110120     c                   eval      ILS46NFVB =  s_brvnfv
107000110120     c                   eval      ILS46FGSB =  s_brvfgs
107100110121     c                   eval      ILS46AMB =  'QD'
107200110121     c                   eval      ILS46aupr=  'SE'
107300110120     c                   eval      ILS46FLS =  Kfls
107400110118     c                   eval      ILS46LNA =  Klna
107500110118     c                   eval      ILS46NRS =  Knrs
107600110118     c                   eval      ILS46NSC =  Knsc
107700110120     c                   eval      ILS46TELA = 'M'
107800110118     c*
107900110118     c                   call      'FNLS46R'
108000110118     c                   parm                    kpjba
108100110118     c                   parm                    fnls46ds
108200110118     c*
108300110118     C                   ENDSR
108400990204     C*--------------------------------------------------------------*
108500990204     C*  TESDAN: Test gestione danni
108600990204     C*--------------------------------------------------------------*
108700990204     C     TESDAN        BEGSR
108800990204     C*
108900990204     C*  Flag pratica danno = 'N'
109000990204     C                   MOVE      'N'           WDAVM             1
109100990204     C*  Se il collo non è stato spuntato su quel foglio di
109200990204     C*  uscita controllo se c'è una pratica C.A. aperta
109300990204     C     KDCD          CHAIN     FNDCD02L                           38
109400990204     C*  Se esiste C.A. aperta controllo il tipo di anomalia
109500990204     C     *IN38         IFEQ      '0'
109600990204     C     DCDATB        ANDNE     'A'
109700990204     C     DCDDCH        IFEQ      0
109800990204     C                   MOVE      TPC(1)        WTIPCA
109900990204     C* Aggancio testata
110000990204     C                   MOVE      DCDAAC        KAAC
110100990204     C                   MOVE      DCDFIL        KFIL
110200990204     C                   MOVE      DCDNCA        KNCA
110300990204     C     KDCT          CHAIN     FNDCT01L                           38
110400990204     C     *IN38         DOWEQ     '0'
110500990204     C     DCTATB        ANDEQ     'A'
110600990204     C     *IN38         OREQ      '0'
110700990204     C     DCTDCH        ANDNE     0
110800990204     C     KDCT          READE     FNDCT01L                               38
110900990204     C                   END
111000990204     C                   Z-ADD     1             YY                4 0
111100990204     C     DCTTAD        LOOKUP    TAD(YY)                                39
111200990204     C     *IN39         IFEQ      '1'
111300990204     C                   MOVEL     DTA(YY)       DTAD
111400990204     C     DRAGR         IFEQ      'M'
111500990204     C     DRAGR         OREQ      'V'
111600031009     C                   MOVE      'S'           WDAVM
111700990204     C                   END                                                    Manc.o Ava D/R
111800990204     C                   END                                                    Trv TAD
111900990204     C*
112000990204     C                   ELSE
112100990204     C                   MOVE      TPC(2)        WTIPCA
112200990204     C                   END                                                    CA Aperta
112300990204     C                   END                                                    CA no annul
112400990204     C*
112500990204     C                   ENDSR
112600950525     C*--------------------------------------------------------------*
112700950525     C*  GESANO: Gestione anomalie
112800950525     C*--------------------------------------------------------------*
112900950525     C     GESANO        BEGSR
113000950525     C*  Se non ho trovato errore bloccante
113100950911     C                   SETOFF                                       031417
113200031009     C                   Z-ADD     1             Y
113300950526     C     WCDANO        LOOKUP    C4H(Y)                                 32
113400950525     C     *IN32         IFEQ      '1'
113500950908     C     S4H(Y)        ANDNE     'N'
113600950908     C                   MOVEL     'S'           D59ANM
113700031009     C                   MOVEL     D4H(Y)        DESERR
113800950525     C*  Trovato errore bloccante !!
113900950525     C     T4H(Y)        IFEQ      'B'
114000950525     C                   MOVEL     'S'           WERRBL
114100030801     C*  Se in consegna su altra distinta
114200950529     C     C4H(Y)        IFEQ      '03'
114300950525     C                   MOVEL     'S'           WNODIS
114400950526     C                   SETON                                        03
114500950606     C                   MOVEL     *BLANKS       APDRSC
114600021204     C     kapd          CHAIN     fiapd01L                           37
114700031009     C                   MOVEL     APDRSC        WPADRO
114800950525     C                   END
114900950526     C                   END
115000950911     C*  Se ho sbloccato il C/Assegno senalo importo e divisa
115100950911     C     C4H(Y)        IFEQ      '07'
115200950911     C                   SETON                                        17
115300950911     C                   Z-ADD     0             AR9CAS
115400950911     C                   MOVEL     *BLANKS       AR9VCA
115500051115     C     KARB          CHAIN     FIAR901L                           37
115600950911     C                   END
115700030801     C*  Decodificare la consegna richiesta
115800950529     C     C4H(Y)        IFEQ      '10'
115900950526     C                   MOVEA     *BLANKS       RIC
116000950526     C                   Z-ADD     ARBDCR        G02INV
116100950526     C                   MOVEL     '3'           G02ERR
116200950526     C                   CALL      'XSRDA8'
116300950526     C                   PARM                    WLBDA8
116400950526     C                   MOVE      G02DAT        WDCR              8
116500950526     C                   MOVEA     WDCR          DCR
116600950526     C     ARBTCR        IFEQ      'D'
116700950526     C                   MOVEA     COST3         RIC
116800950526     C                   MOVEL     DCR(1)        RIC(9)
116900950526     C                   MOVEL     DCR(2)        RIC(10)
117000950526     C                   MOVEL     '/'           RIC(11)
117100950526     C                   MOVEL     DCR(3)        RIC(12)
117200950526     C                   MOVEL     DCR(4)        RIC(13)
117300950526     C                   MOVEL     '/'           RIC(14)
117400950526     C                   MOVEA     DCR(5)        RIC(15)
117500950526     C                   ELSE
117600950526     C                   MOVEA     COST4         RIC
117700950526     C                   MOVEL     DCR(1)        RIC(4)
117800950526     C                   MOVEL     DCR(2)        RIC(5)
117900950526     C                   MOVEL     '/'           RIC(6)
118000950526     C                   MOVEL     DCR(3)        RIC(7)
118100950526     C                   MOVEL     DCR(4)        RIC(8)
118200950526     C                   MOVEL     '/'           RIC(9)
118300950526     C                   MOVEA     DCR(5)        RIC(10)
118400950526     C                   END
118500950526     C                   MOVEA     RIC           WRIC             18
118600950526     C                   MOVE      WRIC          DESERR
118700950525     C                   END
118800950615     C*  Se non sono riuscita ad andare in consegna con l'ultima figlia
118900950615     C*  segnalo l'errore.
119000950615     C     C4H(Y)        IFEQ      '14'
119100950615     C     KLBL          SETGT     FNLBL02L
119200950615     C     KLBL          READPE    FNLBL02L                               32
119300950615     C                   SETON                                        14
119400950615     C                   END
119500030725     c* stampa testata
119600110112     c                   if        d59sqa =  ' ' or
119700110112     c                             d59sqa =  'T'
119800030728     c                   exsr      states
119900101124     c* stampa
120000030728     C                   WRITE     ERRARB
120100101201     c                   end
120200101124     c* video
120300101201     c                   if        d59sqa =  'S' or
120400101201     c                             d59sqa =  'T'
120500101124     c                   EVAL      tbeke2 = 'H' + C4H(Y)
120600101124     c                   exsr      srsqa
120700101124     c                   end
120800950612     C                   ADD       1             D59LIN
120900110414     C                   CLEAR                   TIPERR
121000960319     C                   Z-ADD     0             WAAS
121100960319     C                   Z-ADD     0             WLNP
121200960319     C                   Z-ADD     0             WNRS
121300960319     C                   Z-ADD     0             WNSP
121400030728     C                   Z-ADD     0             Wlna
121500030728     C                   Z-ADD     0             Wdat
121600030729     C                   Z-ADD     0             WNCD
121700030729     C                   Z-ADD     0             WNCA
121800030729     C                   Z-ADD     0             WZNC
121900030729     C                   MOVEL     *BLANKS       WRSD
122000030729     C                   MOVEL     *BLANKS       WBAR
122100950526     C                   END
122200950526     C*
122300950525     C*
122400950525     C                   ENDSR
122500950526     C*--------------------------------------------------------------*
122600030725     C*  STAMPA: Stampa testata
122700950526     C*--------------------------------------------------------------*
122800030728     C     STATES        BEGSR
122900110112     c                   if        d59sqa =  ' ' or
123000110112     c                             d59sqa =  'T'
123100030728     C* SE LA RIGA DI STAMPA È MAGGIORE DI SEI VUOL DIRE CHE HO GIÀ STAMPATO
123200030728     C                   IF        D59LIN = 0 OR
123300030728     C                             D59LIN > 54
123400030728     C                   SETON                                        80
123500030729     c                   else
123600030729     c* stampo testatina spunte doppie a metà pagina
123700030729     c                   if        *in20 and *in21
123800030729     c                   write     testad
123900030729     c                   write     testad1
124000030729     C                   ADD       4             D59LIN
124100030729     c                   setoff                                       21
124200030728     C                   END
124300030729     C                   END
124400030725     C*
124500030728     C                   IF        *IN80
124600030725     C                   ADD       1             D59PAG
124700030728     C                   WRITE     TESTA
124800050502     c                   if        d59lsr = 'S' and d59pag = 1
124900050422     C                   write     testal
125000050426     c                   end
125100030729     C                   Z-ADD     6             D59LIN
125200030728     C                   SETOFF                                       80
125300030728     C* SPUNTE DOPPIE
125400030729     C                   IF        *IN20
125500040930     C                   WRITE     TESTAD2
125600040930     C                   WRITE     TESTAD1
125700030911     C                   Z-ADD     7             D59LIN
125800030911     C                   SETOFF                                       21
125900030729     C                   else
126000030729     C                   WRITE     TESTA1
126100030728     C                   END
126200030729     C                   END
126300110112     C                   END
126400030728     C*
126500030725     C                   ENDSR
126600030725     C*--------------------------------------------------------------*
126700030725     C*  STAMPA: Stampa dettaglio segnacolli
126800030725     C*--------------------------------------------------------------*
126900030725     C     STAMPA        BEGSR
127000101126     C*  Se non è stato stampato "ANOMALIE DA SPUNTE:" lo stampo
127100101201     C                   IF        werrpz = 'N' and
127200101201     c                             (d59sqa =  ' ' or
127300101201     c                             d59sqa =  'T')
127400950526     C                   MOVEL     '12'          WCDANO
127500950526     C                   MOVEL     'S'           WERRPZ
127600950526     C                   EXSR      GESANO
127700950526     C                   END
127800950908     C*----------------------------------------------------*
127900950908     C*  Controllo se devo stampare le anomalie
128000101126     C*    S4H(Y)        IFNE      'N'
128100950908     C*----------------------------------------------------*
128200030725     c*
128300030725     c                   exsr      states
128400040419     c* Imposto il numero della pistola nel campo di stampa
128500950530     C                   MOVEA     WDETER        DER
128600040409     C     '.xx '        SCAN      WDETER        XX                2 0    01
128700040409     c                   if        *in01
128800950530     C                   ADD       1             XX
128900950530     C                   MOVEA     WNRP          DER(XX)
129000950530     C                   MOVEA     DER           WDETER
129100950530     C                   END
129200040419     c* Imposto il numero del foglio nel campo di stampa
129300040419     C                   MOVEA     WDETER        DER
129400040419     C     'xxx-'        SCAN      WDETER        XX                2 0    01
129500040419     c                   if        *in01
129600040421     c                   movel(p)  sfgs          com04             4
129700040421     C                   MOVEA     com04         DER(XX)
129800040420     C                   ADD       4             XX
129900040419     c                   movel     snpg          com01             1
130000040419     C                   MOVEA     com01         DER(XX)
130100040420     C                   ADD       2             XX
130200040419     c                   movel     snfv          com05             5
130300040419     C                   MOVEA     com05         DER(XX)
130400040419     C                   MOVEA     DER           WDETER
130500040419     C                   END
130600110117     c* Imposto l'ora del foglio nel campo di stampa
130700040419     C                   MOVEA     WDETER        DER
130800040419     C     'xx:'         SCAN      WDETER        XX                2 0    01
130900040419     c                   if        *in01
131000040419     c                   movel     shms          com02             2
131100040419     C                   MOVEA     com02         DER(XX)
131200040420     C                   ADD       3             XX
131300070130     c                   move      shms          com04             4
131400070130     c                   movel     com04         com02
131500040419     C                   MOVEA     com02         DER(XX)
131600040419     C                   MOVEA     DER           WDETER
131700040419     C                   END
131800040419     c*
131900950612     C                   ADD       1             D59LIN
132000101125     c* stampa
132100101201     c                   if        (d59sqa =  ' ' or
132200101201     c                             d59sqa =  'T')
132300950919     C     WTRSPU        IFEQ      'S'
132400030728     C                   EVAL      WDETER1 = WDETER
132500030728     C                   EVAL      WTIPCA1 = WTIPCA
132600030728     C                   WRITE     ERRAR1
132700950919     C                   ELSE
132800030728     C                   WRITE     ERRART
132900101125     C                   END
133000101201     c                   end
133100101124     c* video
133200101201     c                   if        (d59sqa =  'S' or
133300101201     c                             d59sqa =  'T')
133400101124     c                   exsr      srsqa
133500101124     c                   end
133600950908     C*----------------------------------------------------*
133700101126     C*                  END
133800950908     C*----------------------------------------------------*
133900950526     C                   ENDSR
134000950526     C*--------------------------------------------------------------*
134100950526     C*  STPTES: Stampo dati testata
134200950526     C*--------------------------------------------------------------*
134300950526     C     STPTES        BEGSR
134400030728     C* AUTOTRASPORTATORE
134500031009     C                   Z-ADD     D59PDR        PRTPDR
134600031009     C                   MOVEL(P)  D59DSF        PRTDSF
134700050422     C                   MOVEL(P)  D59DSF        PRTDSF2
134800030728     C* DISTINTA
134900031009     C                   Z-ADD     D59NDT        PRTNDT
135000110209     C                   Z-ADD     D59NDT        PRTNDT3
135100110209     C*                  eval      PRTNDT2 =d59bcd
135200110209     C                   move      d59bcd        PRTNDT2
135300031009     C                   Z-ADD     D59FGS        PRTFGS
135400030728     C                   Z-ADD     D59DDT        G02INV
135500030728     C                   MOVEL     '3'           G02ERR
135600030728     C                   CALL      'XSRDA8'
135700030728     C                   PARM                    WLBDA8
135800031009     C                   Z-ADD     G02DAT        STCDDC
135900950526     C*
136000030723     C                   MOVEL     CMAT          TPPRE
136100030723     C     D59FPP        IFEQ      'P'
136200031009     C                   MOVEL     CPOM          TPPRE
136300030723     C                   END
136400030728     C*
136500950526     C*
136600950526     C                   ENDSR
136700101124     C*-----------------------------------------------------*
136800101124     C* memorizzo l'errore nel file di work                 *
136900101124     C*-----------------------------------------------------*
137000101124     C     srsqa         BEGSR
137100101124     c                   clear                   dsqa
137200101124     c                   eval      tbecod = 'SQA'
137300101124     c                   eval      tbeke1 = 'FNLRF7R2'
137400101124     c     ktbe          chain     tntbe01l
137500101124     c                   if        %found(tntbe01l)
137600101124     c                   movel     tbeuni        dsqa
137700101124     c                   if        D§SQAELA = 'S'
137800101124     c* TESTATA
137900101124     c                   clear                   wfnlrq1ds
138000101124     c                   eval      wlrq1ndc = d59ndt
138100101124     c                   eval      wlrq1fgs = d59fgs
138200101124     c                   eval      wlrq1pdr = d59pdr
138300101125     c                   eval      wlrq1aas = s_arbAAS
138400101125     c                   eval      wlrq1LNP = s_arbLNP
138500101125     c                   eval      wlrq1NRS = s_arbNRS
138600101125     c                   eval      wlrq1NSP = s_arbNSP
138700101124     c*
138800101124     c     kwf10         chain     wfnlrq11l
138900101124     c                   if        not %found(wfnlrq11l)
139000101124     c                   movel     tbeke2        wlrq1cer
139100101124     c                   eval      wlrq1sce = D§SQASCE
139200101126     c                   if        D§SQATPD = 'S'
139300101126     c                   eval      wlrq1err = D§SQADES
139400101126     c                   else
139500101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
139600101126     c                   eval      wlrq1err = deserr
139700101126     c                   else
139800101126     c                   eval      wlrq1err = wdeter
139900101126     c                   end
140000101126     c                   end
140100101124     c                   write     wfnlrq10
140200101124     c                   else
140300101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
140400101124     c                   clear                   wlrq1cer
140500101124     c                   clear                   wlrq1sce
140600101124     c                   update    wfnlrq10
140700101124     c                   end
140800101124     c* dettaglio
140900101124     c                   clear                   wfnlrq2ds
141000101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
141100101124     c                   eval      wlrq2aas = wlrq1aas
141200101124     c                   eval      wlrq2lnp = wlrq1lnp
141300101124     c                   eval      wlrq2nrs = wlrq1nrs
141400101124     c                   eval      wlrq2nsp = wlrq1nsp
141500101124     c                   eval      wlrq2ndc = wlrq1ndc
141600101124     c                   eval      wlrq2fgs = wlrq1fgs
141700101124     c                   eval      wlrq2pdr = wlrq1pdr
141800101124     c                   movel     tbeke2        wlrq2cer
141900101124     c                   eval      wlrq2sce = D§SQASCE
142000101126     c                   if        D§SQATPD = 'S'
142100101126     c                   eval      wlrq2err = D§SQADES
142200101126     c                   else
142300101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
142400101126     c                   eval      wlrq2err = deserr
142500101126     c                   else
142600101126     c                   eval      wlrq2err = wdeter
142700101126     c                   end
142800101126     c                   end
142900101125     c* gli errori nella tabella 4H sono a livello di bolla quindi
143000101126     c* non ho le informazioni del segnacollo
143100101125     c                   if        %subst(tbeke2: 1: 1) <> 'H'
143200101124     c                   eval      wlrq2nsc = artnsc
143300110104     c                   eval      wlrq2fls = artfls
143400110126     c                   eval      wlrq2nrc = artnrs
143500110131     c                   if        tbeke2 <> '010'
143600110104     c                   eval      wlrq2flv = sfgs
143700101125     c                   eval      wlrq2nfv = snfv
143800110119     c                   eval      wlrq2dfv = sdfv
143900101125     c                   eval      wlrq2npg = snpg
144000101126     c                   eval      wlrq2hms = shms
144100101126     c                   if        wnrp <> ' '
144200101126     c                   movel     wnrp          wlrq2nps
144300101126     c                   end
144400110131     c                   end
144500110207     c*                  else
144600110207     c*                  eval      wlrq2flv = s_arbifp
144700110207     c*                  eval      wlrq2nfv = s_arbndc
144800110207     c*                  eval      wlrq2dfv = s_arbddc
144900110207     c*                  if        wlrq2nfv <> 0
145000110207     c*                  eval      wlrq2npg = 4
145100110207     c*                  end
145200101126     c                   end
145300101125     c*
145400101124     c                   write     wfnlrq20
145500101124     c                   end
145600101124     c                   end
145700101124     C                   ENDSR
145800950526     C*--------------------------------------------------------------*
145900950526     C*  DLTARZ: Cancello archivio dett.segnacolli/stop
146000950526     C*--------------------------------------------------------------*
146100950526     C     DLTARZ        BEGSR
146200950526     C*
146300950526     C     *LOVAL        SETLL     FNARBZ0F
146400950526     C                   READ      FNARBZ0F                               31
146500950526     C     *IN31         DOWEQ     '0'
146600950526     C                   DELETE    FNARBZ0F
146700950526     C                   READ      FNARBZ0F                               31
146800950526     C                   END
146900950526     C*
147000950526     C                   ENDSR
147100950614     C*--------------------------------------------------------------*
147200950614     C*  WRTANM: Scrivo anomalia collo fuori posto
147300950614     C*--------------------------------------------------------------*
147400950915     C     WRTANM        BEGSR
147500950915     C*
147600950919     C     ARTDCM        IFEQ      0
147700950915     C                   MOVEL     'N'           WAGANO            1
147800950918     C                   Z-ADD     WCAA          KCAA
147900990519     C                   CLEAR                   FNANM000
148000990519     C     KANM          CHAIN     FNANM02L                           34
148100950915     C     *IN34         DOWEQ     '0'
148200990519     C     ANMDAO        IFEQ      D59DDT
148300031009     C                   MOVEL     'S'           WAGANO
148400950915     C     ANMDCH        IFNE      0
148500960507     C                   MOVEL     WFASE         ANMFSA
148600960507     C                   Z-ADD     WFGS          ANMCDU
148700960507     C                   Z-ADD     WNFV          ANMNFV
148800950918     C                   Z-ADD     WNPS          ANMNPS
148900950915     C                   Z-ADD     0             ANMDCH
149000950915     C                   MOVEL     *BLANKS       ANMFSC
149100950915     C                   MOVEL     *BLANKS       ANMCCH
149200950915     C                   EXCEPT    AGGANM
149300960429     C                   ELSE
149400030721     C                   unlock    fnanm02l
149500950915     C                   END
149600950915     C                   END
149700990519     C     KANM          READE     FNANM02L                               34
149800950915     C                   END
149900950915     C*
150000950915     C     WAGANO        IFEQ      'N'
150100990519     C                   CLEAR                   FNANM000
150200960507     C                   MOVEL     WFASE         ANMFSA
150300960507     C                   Z-ADD     WFGS          ANMCDU
150400950915     C                   Z-ADD     ARTFLS        ANMFLS
150500950915     C                   Z-ADD     ARTNRS        ANMNRS
150600950915     C                   Z-ADD     ARTLNA        ANMLNA
150700950915     C                   Z-ADD     ARTNSC        ANMSCN
150800950915     C                   Z-ADD     ARBAAS        ANMAAS
150900950915     C                   Z-ADD     ARBLNP        ANMLNP
151000950915     C                   Z-ADD     ARBNSP        ANMNSP
151100950918     C                   Z-ADD     WCAA          ANMCAA
151200950918     C                   Z-ADD     WNFV          ANMNFV
151300950918     C                   Z-ADD     WNPS          ANMNPS
151400950915     C                   Z-ADD     D59DDT        ANMDAO
151500950915     C                   Z-ADD     SIMFEL        ANMFLE
151600950915     C                   MOVEL     'F'           ANMTAN
151700950915     C                   MOVEL     'S'           ANMSAN
151800950915     C                   MOVEL     'I'           ANMAIE
151900040721     c                   eval      anmft4 = §AR5BVA
152000040721     c                   movel     woggi         com6              6
152100040721     c                   clear                   comsei            6
152200040721     c                   eval      comsei = %subst(com6: 5: 2)+
152300040721     c                             %subst(com6: 3: 2)+
152400040721     c                             %subst(com6: 1: 2)
152500040721     c                   movel     comsei        §ANMDAC
152600040721     c                   movel     times         §ANMORC
152700040721     c                   movel     danmflo       anmflo
152800040721     c                   eval      anmfl4 = arbznc
152900040721     c                   if        arbznc=0
153000040721     c                   eval      anmfl4 = 999
153100040721     c                   end
153200990519     C                   WRITE     FNANM000
153300950915     C                   END
153400950919     C*
153500950919     C                   END                                                    ARTDCM = 0
153600950614     C*
153700950915     C                   ENDSR
153800980310     C*--------------------------------------------------------------*
153900980310     C*  WRTNIC: Leggo NIC provvisori e li scrivo in definitivo
154000980310     C*--------------------------------------------------------------*
154100980310     C     WRTNIC        BEGSR
154200980310     C*
154300980310     C* Controllo quali sono gli eventi di esclusi dalla consegna che
154400980310     C* non sono stati cancellati e li riporto in definitivo
154500130326     C     *LOVAL        SETLL     WFNEVB01L
154600130326     C                   READ      WfNEVB01L                              36
154700980310     C     *IN36         DOWEQ     '0'
154800031013     C                   MOVEL     DSEVB         DSEVBS
154900980310     C* Aggancio file eventi per controllare se l'evento di messa in
155000980310     C* consegna sulla bolla era già stato trasmesso
155100990803     C                   Z-ADD     EVBAAS        KAAS
155200980310     C                   Z-ADD     EVBLNP        KLNP
155300980310     C                   Z-ADD     EVBNRS        KNRS
155400980310     C                   Z-ADD     EVBNSP        KNSP
155500031020     C* riaggancio la spedizione dell'evento NIC rimasto
155600031020     C     KARB          CHAIN     FNARB01L                           33
155700100514     c                   clear                   ds7r
155800100514     c                   if        arbgma <> ' '
155900100514     c                   exsr      sr7r
156000100514     c                   end
156100031020     c* se spedizione con ritiro contestuale alla consegna
156200100514     c                   if        §7RRC= 'S' and not *in33
156300031020     c                   clear                   fnlr99ds
156400031020     c                   eval      i99fao = 390
156500031020     c                   exsr      AGGORM
156600031020     c                   end
156700031020     c* proseguo con la scrittura del NIC/cancellazione del MIC
156800980310     C                   MOVEL     'MIC'         KCEV
156900990803     C     KEVB2         SETGT     FNEVB04L
157000990803     C     KEVB2         READPE    FNEVB04L                               37
157100131218     C*--
157200980310     C     *IN37         IFEQ      '0'
157300990803     C     EVBDEV        ANDEQ     D59DDT
157400131218     C*--
157500980310     C* se l'evento MIC non era stato trasmesso lo annullo fisicamente
157600980310     C     EVBDTR        IFEQ      0
157700131218     C*
157800131218     C* riaggancia il record  FISICO x aggiornarlo
157900131218     c     evb_nrr       delete    fnevb00F
158000131218     C*--
158100131218     C**************     DELETE    FNEVB04L
158200131218     C*
158300980310     C                   ELSE
158400131218     C*
158500031029     C                   MOVEL     EVBDEV        WDTMIC           12 0
158600031029     C                   MOVE      EVBHEV        WDTMIC
158700980312     C* altrimenri controllo se esiste già un evento NIC con data = a
158800980312     C* quella del foglio di consegna
158900990803     C                   Z-ADD     EVBAAS        KAAS
159000980312     C                   Z-ADD     EVBLNP        KLNP
159100980312     C                   Z-ADD     EVBNRS        KNRS
159200980312     C                   Z-ADD     EVBNSP        KNSP
159300980312     C                   MOVEL     'NIC'         KCEV
159400990803     C     KEVB2         SETGT     FNEVB04L
159500990803     C     KEVB2         READPE    FNEVB04L                               37
159600131218      *--
159700980312     C     *IN37         IFEQ      '0'
159800990803     C     EVBDEV        ANDEQ     D59DDT
159900131218      *--
160000980312     C* Se c'è evento NIC controllo se già trasmesso
160100980312     C     EVBDTR        IFEQ      0
160200131218     C*
160300131218     C* riaggancia il record  FISICO x aggiornarlo
160400131218     c     evb_nrr       chain     fnevb00F
160500131218      *--
160600131218     c                   if        %Found(fnevb00F)
160700980312     C* Se non è ancora stato trasmesso aggiorno data/ora immissione
160800980312     C                   Z-ADD     D59AMG        EVBDTV
160900980312     C                   TIME                    EVBORV
161000980312     C                   Z-ADD     D59FGS        EVBFLE
161100980312     C                   MOVE      D59CDU        EVBCDU
161200980312     C     FVVFPP        IFEQ      'M'
161300980312     C                   MOVEL     0800          EVBHEV
161400980312     C                   ELSE
161500980312     C                   MOVEL     1200          EVBHEV
161600980312     C                   END
161700990803     C                   Z-ADD     0             EVBDTC
161800131218      *--
161900990803     C                   UPDATE    FNEVB000
162000131218     c                   end
162100131218      *--
162200980312     C                   ELSE
162300131218      *--
162400980312     C* Se è già stato trasmesso controllo se data/ora immissione
162500980312     C* > MIC
162600031029     C                   MOVEL     EVBDEV        WDTNIC           12 0
162700031029     C                   MOVE      EVBHEV        WDTNIC
162800031029     C     WDTNIC        IFLE      WDTMIC
162900980312     C* Se la data non è più alta scrivo un nuovo NIC
163000031013     C                   MOVEL     DSEVBS        DSEVB
163100990803     C                   WRITE     FNEVB000
163200980312     C                   END                                                    Data MIC >= NIC
163300980312     C*
163400980312     C                   END                                                    NIC Trasmesso
163500980312     C*
163600980312     C                   ELSE                                                   Nic Trovato
163700031013     C                   MOVEL     DSEVBS        DSEVB
163800990803     C                   WRITE     FNEVB000
163900980312     C                   END                                                    Nic Trovato
164000980312     C                   END                                                    Mic trasmesso
164100980312     C                   END                                                    Mic Trovato
164200980310     C* Cancello evento provvisiorio
164300990803     C                   DELETE    FNEVBPRV
164400980310     C* leggo evento provvisiorio successivo
164500130326     C                   READ      WFNEVB01L                              36
164600980310     C                   END
164700980310     C*
164800980310     C                   ENDSR
164900030805     C**********************************************************************
165000030805     C* calcolo il numero dei bancali   -----------------------------*
165100030805     C**********************************************************************
165200030805     C     srnban        BEGSR
165300030805      * Bancali
165400030805     c                   If        %Subst(Arbgva:1:1) = 'B'
165500030805     c                   Clear                   dAr5Ban
165600030805     C                   move      'BAN'         ar5trd
165700030805     c     kFiar5        Chain     Fiar501l
165800030805     c                   If        %Found(Fiar501l)
165900030805     c                   Movel     Ar5Uni        dAr5Ban
166000030805     c                   EndIf
166100030805     c                   add       §Ar5Nba       nban
166200030805     c                   add       §ar5Nb2       nban
166300030805     c                   EndIf
166400030805      * Bancali Bartolini
166500030805     c                   If        %Subst(Arbgva:1:1) = 'O'
166600030805     c                   Clear                   dAr5BNB
166700030805     C                   move      'BNB'         ar5trd
166800030805     c     kFiar5        Chain     Fiar501l
166900030805     c                   If        %Found(Fiar501l)
167000030805     c                   Movel     Ar5Uni        dAr5BNB
167100030805     c                   EndIf
167200030805     c                   add       §Ar5bNba      nban
167300030805     c                   add       §ar5bNb2      nban
167400030805     c                   EndIf
167500040721      * Merce di valore
167600040721     c                   Clear                   dAr5gen
167700040721     C                   move      'GEN'         ar5trd
167800040721     c     kFiar5        Chain     Fiar501l
167900040721     c                   If        %Found(Fiar501l)
168000040721     c                   Movel     Ar5Uni        dAr5gen
168100040721     c                   EndIf
168200030805     c                   ENDSR
168300010620     C*--------------------------------------------------------------*
168400010620     C*  DOPPIE: STAMPO EVENTUALI SPUNTE DI CONSEGNA DOPPIE
168500010620     C*--------------------------------------------------------------*
168600010620     C     DOPPIE        BEGSR
168700010620     C*
168800010620     C                   Z-ADD     D59NDT        KNFV
168900010620     C                   Z-ADD     4             KNPG
169000030729     C                   SETON                                        2021
169100010620     C*
169200070129     C     KBRVE         SETLL     FNBRVE1L
169300070129     C     KBRVE         READE     FNBRVE1L                               30
169400010620    2C     *IN30         DOWEQ     *OFF
169500010620     C                   CLEAR                   WGIR
169600010620     C                   CLEAR                   SAWNPS
169700010622     C                   CLEAR                   SAWNFV
169800010622     C                   CLEAR                   SAWFGS
169900130327     C                   SETOFF                                       19
170000070129     C     KBRVE2        SETLL     FNBRVE2L
170100070129     C     KBRVE2        READE     FNBRVE2L                               31
170200010620    3C     *IN31         DOWEQ     *OFF
170300010620     C* Considero solo se data foglio >= data distinta in quadratura
170400010702    4C     D59FGS        IFNE      BREFGS
170500010702     C     KNFV          ORNE      BRENFV
170600010620     C                   CLEAR                   DSLV53
170700010620     C                   MOVEL     'V'           D53TFO
170800010620     C                   MOVEL     KNPG          D53NPG
170900010620     C                   Z-ADD     SIMFEL        D53FEL
171000010702     C                   Z-ADD     BRENFV        D53NFV
171100010702     C                   Z-ADD     BREFGS        D53FGS
171200010620     C                   CALL      'FNLV53R'
171300010620     C                   PARM                    DSLV53
171400010702     C                   Z-ADD     D53DFV        SA1DFV
171500010620     C                   ELSE
171600010702     C                   Z-ADD     D59DDT        SA1DFV
171700010620    4C                   ENDIF
171800010702    4C     D59FGS        IFEQ      BREFGS
171900010702     C     KNFV          ANDEQ     BRENFV
172000010620     C     D53ERR        OREQ      *BLANKS
172100010702     C     SA1DFV        ANDGE     D59DDT
172200010703    5C     SAWNFV        IFGT      *ZEROS
172300010622     C* Stessa pistola e foglio: devo verificare se superati i minuti
172400010622     c* di tolleranza previsti fra una spunta e l'altra
172500010620     C                   CLEAR                   WPRT
172600010702    6C     SAWNPS        IFEQ      BRENPS
172700010702     C     SAWNFV        ANDEQ     BRENFV
172800010702     C     SAWFGS        ANDEQ     BREFGS
172900010620     C                   CLEAR                   WXDTOR
173000010702     C                   MOVEL     BREDFS        WXDT1
173100010702     C                   MOVEL     BREHMS        WXHM1
173200010620     C                   MOVEL     SAWDFS        WXDT2
173300010620     C                   MOVEL     SAWHMS        WXHM2
173400010620     C                   MOVEL     'D'           WXFLG
173500010620     C                   CALL      'XDTORA'
173600010620     C                   PARM                    WXDTOR
173700010620     C     WXMIN         IFGT      §7AMNP
173800010620     C                   MOVEL     '1'           WPRT
173900010620     C                   ENDIF
174000010620   x6C                   ELSE
174100010622     c* se pistole o foglio diversi stampo sempre
174200010620     C                   MOVEL     '1'           WPRT              1
174300010620    6C                   ENDIF
174400010620    6C     WPRT          IFEQ      '1'
174500010620     c* se sono al primo giro di stampa devo stampare sia la spunta pre
174600010620     c* cedente che la spunta successiva
174700010620    7C     WGIR          IFEQ      ' '
174800010620     C                   MOVEL     DSBRVE        W1BRVE
174900010620     C                   MOVEL     WSBRVE        DSBRVE
175000010620     C                   Z-ADD     SAWDFV        WDFV
175100030729     C                   SETOff                                       19
175200010620     C                   EXSR      SPRDOP
175300010620     C                   MOVEL     W1BRVE        DSBRVE
175400010620     C                   MOVE      '1'           WGIR              1
175500010620    7C                   ENDIF
175600010702     C                   Z-ADD     SA1DFV        WDFV
175700030729     C                   SETON                                        19
175800010620     C                   EXSR      SPRDOP
175900010620   X6C                   ELSE
176000010620     C                   MOVEL     DSBRVE        WSBRVE
176100010620    6C                   ENDIF
176200010620   X5C                   ELSE
176300010620     C* se sono al primo giro di lettura memorizzo la spunta
176400010620     C                   MOVEL     DSBRVE        WSBRVE
176500010620    5C                   ENDIF
176600010620     C* memorizzo in ogni caso il numero pistola e data/ora fisica spun
176700010702     C                   Z-ADD     BRENPS        SAWNPS
176800010702     C                   Z-ADD     BREFGS        SAWFGS
176900010702     C                   Z-ADD     BRENFV        SAWNFV
177000010702     C                   Z-ADD     BREDFS        SAWDFS
177100010702     C                   Z-ADD     BREHMS        SAWHMS
177200010702     C                   Z-ADD     SA1DFV        SAWDFV
177300010620    4C                   ENDIF
177400070129     C     KBRVE2        READE     FNBRVE2L                               31
177500010620    3C                   ENDDO
177600010620     C* Mi posiziono sempre su un nuovo segnacollo
177700070129     C     KBRVE1        SETGT     FNBRVE1L
177800070129     C     KBRVE         READE     FNBRVE1L                               30
177900010620    2C                   ENDDO
178000010711     C*
178100010620     C                   ENDSR
178200010620     C*
178300010620     C*--------------------------------------------------------------*
178400010620     C* STAMPA SPUNTA DOPPIA
178500010620     C*--------------------------------------------------------------*
178600010620     C     SPRDOP        BEGSR
178700010712     C                   MOVEL     'S'           D59ANM
178800030728     C                   EXSR      STATES
178900010711     C*
179000010702     C                   CLEAR                   P1CMSG
179100030729     C* SEGNACOLLO SOLO PER 19 Off
179200030729     C     *IN19         IFEQ      *Off
179300010702     C                   Z-ADD     BRELNP        P1CLNP
179400010702     C                   Z-ADD     BRELNA        P1CLNA
179500010702     C                   Z-ADD     BRENRS        P1CNRS
179600010702     C                   Z-ADD     BRENSC        P1CNSC
179700010702     C* VEDO SE COLLO IN IMA
179800010702     C                   Z-ADD     3             KNPG
179900070129     C     KBRV9         SETGT     FNBRV09L
180000070129     C     KBRV9         READPE    FNBRV09L                               32
180100010702     C     *IN32         DOWEQ     *OFF
180200010702     C     P1CMSG        ANDEQ     *BLANKS
180300010702     C* IL foglio deve avere sottocategoria arrivi ed avere data foglio
180400010702     C* maggiore uguale alla data del foglio in fase di quadratura
180500010702     C                   CLEAR                   DSLV53
180600010702     C                   MOVEL     'V'           D53TFO
180700010702     C                   MOVEL     KNPG          D53NPG
180800010702     C                   Z-ADD     SIMFEL        D53FEL
180900010702     C                   Z-ADD     BRVNFV        D53NFV
181000010702     C                   Z-ADD     BRVFGS        D53FGS
181100010702     C                   CALL      'FNLV53R'
181200010702     C                   PARM                    DSLV53
181300010702     C     D53ERR        IFEQ      *BLANKS
181400010702     C     D53DFV        ANDGE     D59DDT
181500010702     C     D53SPG        ANDEQ     'A'
181600010702     C                   MOVEA     COS           W33
181700010702     C                   MOVE      BRVFGS        W003A             3
181800010702     C                   MOVEA     W003A         W33(8)
181900010702     C                   MOVE      BRVNFV        W006A             6
182000010712     C                   EXSR      EDITA
182100010702     C                   MOVEA     W006A         W33(12)
182200010702     C                   MOVE      D53DFV        W0040             4 0
182300010702     C                   MOVEL     W0040         WMMDFV            2
182400010702     C                   MOVE      W0040         WGGDFV            2
182500010702     C                   MOVEA     WGGDFV        W33(23)
182600010702     C                   MOVEA     WMMDFV        W33(26)
182700010702     C                   MOVE      BRVNPS        W002A             2
182800010702     C                   MOVEA     W002A         W33(32)
182900031009     C                   MOVEA     W33           P1CMSG
183000010702     C                   ENDIF
183100070129     C     KBRV9         READPE    FNBRV09L                               32
183200010702     C                   ENDDO
183300010620     C                   ENDIF
183400110518     C* Reimposto KNPG in quanto usato nel ciclo di lettura di fnbrve
183500110518     C                   Z-ADD     4             KNPG
183600010620     C*
183700010702     C                   Z-ADD     BREFGS        P1CFGS
183800010702     C                   Z-ADD     BRENFV        P1CNFV
183900010620     C* DATA DISTINTA
184000010620     C                   Z-ADD     WDFV          G02INV
184100010620     C                   MOVE      *ZEROS        G02DAT
184200010620     C                   MOVEL     3             G02ERR
184300010620     C                   CALL      'XSRDA8'
184400010620     C                   PARM                    WLBDA8
184500010620     C                   Z-ADD     G02DAT        P1CDFV
184600010620     C* AUTOTRASPORTATORE
184700110518     C     KDST          CHAIN     FiDST01L                           32
184800010620    1C     *IN32         IFEQ      *OFF
184900010711     C                   Z-ADD     DSTPDR        P1CPDR
185000021204     C     kapdd         CHAIN     fiapd01L                           32
185100010620    2C     *IN32         IFEQ      *OFF
185200031009     C                   MOVEL     APDRSC        P1DPDR
185300010620   X2C                   ELSE
185400010620     C                   CLEAR                   P1DPDR
185500010620    2C                   ENDIF
185600010620   X1C                   ELSE
185700010620     C                   CLEAR                   P1CPDR
185800010620     C                   ENDIF
185900010620     C* DATA IMMISSIONE
186000010702     C                   Z-ADD     BREDFS        G02INV
186100010620     C                   MOVE      *ZEROS        G02DAT
186200010620     C                   MOVEL     3             G02ERR
186300010620     C                   CALL      'XSRDA8'
186400010620     C                   PARM                    WLBDA8
186500010620     C                   Z-ADD     G02DAT        P1CDFS
186600010620     C*
186700010702     C                   Z-ADD     BREHMS        P1CHMS
186800010702     C                   Z-ADD     BRENPS        P1CNPS
186900030728     C                   WRITE     ERRDOP
187000010711     C                   ADD       1             D59LIN
187100010620     C                   ENDSR
187200010712     C*--------------------------------------------------------------*
187300010712     C* SOPPRESSIONE ZERI NON SIGNIFICATIVI DA CAMPO LUNGO 6
187400010712     C*--------------------------------------------------------------*
187500010712     C     EDITA         BEGSR
187600010712     C*
187700010712     C                   MOVE      *BLANKS       WFINE             1
187800010712     C                   MOVEA     W006A         W06
187900010712     C                   Z-ADD     1             II                1 0
188000010712     C     WFINE         DOWEQ     *BLANKS
188100010712     C     II            ANDLE     5
188200010712     C     W06(II)       IFEQ      '0'
188300010712     C                   MOVE      *BLANKS       W06(II)
188400010712     C                   ELSE
188500010712     C                   MOVE      '1'           WFINE
188600010712     C                   END
188700010712     C                   ADD       1             II
188800010712     C                   ENDDO
188900010712     C                   MOVEA     W06           W006A
189000010712     C                   ENDSR
189100950526     C*--------------------------------------------------------------*
189200950526     C*  *INZSR: Operazioni iniziali
189300950526     C*--------------------------------------------------------------*
189400950526     C     *INZSR        BEGSR
189500950526     C*------------------------------------------------------*
189600030722     c     *dtaara       define    §azute        azuteds
189700030722     c     *dtaara       define    §datiute      ddatiute
189800030722     C                   in(E)     *dtaara
189900030722     C                   IF        %Error  or  RSUT = *blanks
190000030722     C                   call      'TIBS34R'
190100030722     C                   parm                    Tibs34Ds
190200030722     C                   in        *dtaara
190300030722     c                   ENDIF
190400950612     C                   MOVE      SIMFEL        KFLE
190500950526     C*------------------------------------------------------*
190600950526     C*  Definisco chiavi di accesso
190700110518     C     KDST          KLIST
190800110518     C                   KFLD                    knpg
190900110518     C                   KFLD                    brenfv
191000110518     C                   KFLD                    BREFGS
191100030805     C     Kfiar5        KLIST
191200030805     C                   KFLD                    arbaas
191300030805     C                   KFLD                    arblnp
191400030805     C                   KFLD                    arbnrs
191500030805     C                   KFLD                    arbnsp
191600030805     C                   KFLD                    ar5trd
191700950526     C     KARB          KLIST
191800950526     C                   KFLD                    KAAS
191900950526     C                   KFLD                    KLNP
192000950526     C                   KFLD                    KNRS
192100950526     C                   KFLD                    KNSP
192200950606     C     KLBL          KLIST
192300950606     C                   KFLD                    KAAS
192400950606     C                   KFLD                    KLNP
192500950606     C                   KFLD                    KNRS
192600950606     C                   KFLD                    KNSP
192700950526     C     KART          KLIST
192800950526     C                   KFLD                    KAAS
192900950526     C                   KFLD                    KLNP
193000950526     C                   KFLD                    KNRS
193100950526     C                   KFLD                    KNSP
193200950605     C     KFVV          KLIST
193300950605     C                   KFLD                    KNPG
193400950605     C                   KFLD                    KNFV1
193500080229     C                   KFLD                    brvfgs
193600950526     C     KBRV1         KLIST
193700950526     C                   KFLD                    KNPG
193800950526     C                   KFLD                    KNFV
193900950526     C                   KFLD                    KFGS
194000950526     C                   KFLD                    KFLS
194100950526     C                   KFLD                    KLNA
194200950526     C                   KFLD                    KNRS
194300950526     C                   KFLD                    KNSC
194400950526     C     KBRV2         KLIST
194500950526     C                   KFLD                    KNPG
194600950526     C                   KFLD                    KFLS
194700950526     C                   KFLD                    KLNA
194800950526     C                   KFLD                    KNRS
194900950526     C                   KFLD                    KNSC
195000040420     C                   KFLD                    KFLE
195100010702     C     KBRV9         KLIST
195200010702     C                   KFLD                    KNPG
195300010702     C                   KFLD                    BRELNP
195400010702     C                   KFLD                    BRELNA
195500010702     C                   KFLD                    BRENRS
195600010702     C                   KFLD                    BRENSC
195700010702     C                   KFLD                    KFLE
195800990204     C     KDCD          KLIST
195900990204     C                   KFLD                    KFLS
196000990204     C                   KFLD                    KLNA
196100990204     C                   KFLD                    KNRS
196200990204     C                   KFLD                    KNSC
196300990204     C     KDCT          KLIST
196400990204     C                   KFLD                    KAAC
196500990204     C                   KFLD                    KFIL
196600990204     C                   KFLD                    KNCA
196700950526     C     KTAB          KLIST
196800950526     C                   KFLD                    KKUT
196900950526     C                   KFLD                    KCOD
197000010620     C     KTAB1         KLIST
197100010620     C                   KFLD                    KKUT
197200010620     C                   KFLD                    KCOD
197300010620     C                   KFLD                    KKEY
197400950915     C     KANM          KLIST
197500950915     C                   KFLD                    KFLS
197600950915     C                   KFLD                    KLNA
197700950915     C                   KFLD                    KNRS
197800950915     C                   KFLD                    KNSC
197900950915     C                   KFLD                    KCAA
198000980310     C     KEVB1         KLIST
198100990803     C                   KFLD                    KAAS
198200980310     C                   KFLD                    KLNP
198300980310     C                   KFLD                    KNRS
198400980310     C                   KFLD                    KNSP
198500980310     C     KEVB2         KLIST
198600990803     C                   KFLD                    KAAS
198700980310     C                   KFLD                    KLNP
198800980310     C                   KFLD                    KNRS
198900980310     C                   KFLD                    KNSP
199000980310     C                   KFLD                    KCEV
199100010620     C     KBRVE         KLIST
199200010620     C                   KFLD                    KNPG
199300010620     C                   KFLD                    KNFV
199400010620     C                   KFLD                    D59FGS
199500010620     C     KBRVE1        KLIST
199600010620     C                   KFLD                    KNPG
199700010620     C                   KFLD                    KNFV
199800010620     C                   KFLD                    D59FGS
199900010702     C                   KFLD                    BRELNP
200000010702     C                   KFLD                    BRELNA
200100010702     C                   KFLD                    BRENRS
200200010702     C                   KFLD                    BRENSC
200300010620     C     KBRVE2        KLIST
200400010702     C                   KFLD                    BRENPG
200500010702     C                   KFLD                    BRELNP
200600010702     C                   KFLD                    BRELNA
200700010702     C                   KFLD                    BRENRS
200800010702     C                   KFLD                    BRENSC
200900021204     C*
201000021204     C     Kapd          KLIST
201100021204     C                   KFLD                    apdtip
201200021204     C                   KFLD                    arbpdc
201300021204     C*
201400021204     C     Kapd59        KLIST
201500021204     C                   KFLD                    apdtip
201600021204     C                   KFLD                    d59pdr
201700021204     C*
201800021204     C     Kapdd         KLIST
201900021204     C                   KFLD                    apdtip
202000021204     C                   KFLD                    dstpdr
202100021204     c                   movel     'A'           apdtip
202200101124     C     Ktbe          KLIST
202300101124     C                   KFLD                    tbecod
202400101124     C                   KFLD                    tbeke1
202500101124     C                   KFLD                    tbeke2
202600101124     C     Kwf10         KLIST
202700101124     C                   KFLD                    WLRQ1AAS
202800101124     C                   KFLD                    WLRQ1LNP
202900101124     C                   KFLD                    WLRQ1NRS
203000101124     C                   KFLD                    WLRQ1NSP
203100101124     C                   KFLD                    WLRQ1FGS
203200101124     C                   KFLD                    WLRQ1NDC
203300101124     C                   KFLD                    WLRQ1PDR
203400950526     C*------------------------------------------------------*
203500950526     C*  Definisco variabili
203600030728     C     *LIKE         DEFINE    BRVNPS        BRVNPS1
203700030728     C     *LIKE         DEFINE    D59NDT        EXNDT
203800950526     C     *LIKE         DEFINE    ARTAAS        KAAS
203900950526     C     *LIKE         DEFINE    ARTLNP        KLNP
204000950526     C     *LIKE         DEFINE    ARTNRS        KNRS
204100950526     C     *LIKE         DEFINE    ARTNSP        KNSP
204200950526     C     *LIKE         DEFINE    ARTFLS        KFLS
204300950526     C     *LIKE         DEFINE    ARTLNA        KLNA
204400950526     C     *LIKE         DEFINE    ARTNSC        KNSC
204500950526     C     *LIKE         DEFINE    BRVNPG        KNPG
204600950526     C     *LIKE         DEFINE    BRVNFV        KNFV
204700950526     C     *LIKE         DEFINE    BRVFGS        KFGS
204800950612     C     *LIKE         DEFINE    BRVFLE        KFLE
204900951121     C     *LIKE         DEFINE    ARBRAG        KRAG
205000950526     C     *LIKE         DEFINE    TBLKUT        KKUT
205100950526     C     *LIKE         DEFINE    TBLCOD        KCOD
205200950915     C     *LIKE         DEFINE    ANMCAA        KCAA
205300980310     C     *LIKE         DEFINE    EVBCEV        KCEV
205400950918     C     *LIKE         DEFINE    ANMCAA        WCAA
205500950918     C     *LIKE         DEFINE    ANMNFV        WNFV
205600950918     C     *LIKE         DEFINE    ANMNPS        WNPS
205700960507     C     *LIKE         DEFINE    FVVFGS        WFGS
205800950605     C     *LIKE         DEFINE    FVVNFV        KNFV1
205900990204     C     *LIKE         DEFINE    TBECOD        KCODE
206000990204     C     *LIKE         DEFINE    DCTAAC        KAAC
206100990204     C     *LIKE         DEFINE    DCTFIL        KFIL
206200990204     C     *LIKE         DEFINE    DCTNCA        KNCA
206300010620     C     *LIKE         DEFINE    DSBRVE        WSBRVE
206400010620     C     *LIKE         DEFINE    DSBRVE        W1BRVE
206500010620     C     *LIKE         DEFINE    BRVNPS        SAWNPS
206600010622     C     *LIKE         DEFINE    BRVFGS        SAWFGS
206700010622     C     *LIKE         DEFINE    BRVNFV        SAWNFV
206800010620     C     *LIKE         DEFINE    BRVDFS        SAWDFS
206900010620     C     *LIKE         DEFINE    BRVHMS        SAWHMS
207000010620     C     *LIKE         DEFINE    D53DFV        SAWDFV
207100010620     C     *LIKE         DEFINE    D53DFV        WDFV
207200010702     C     *LIKE         DEFINE    D53DFV        SA1DFV
207300010620     C     *LIKE         DEFINE    TBLKEY        KKEY
207400950526     C*
207500950526     C                   TIME                    TIMES             6 0
207600950526     C                   TIME                    WHDAT            12 0
207700950526     C                   MOVE      WHDAT         WOGGI             6 0
207800010622     C                   TIME                    W0140            14 0
207900010622     C                   MOVE      W0140         WDTGIO            8 0
208000031009     C                   MOVEL     *ALL'-'       CMP132
208100031009     C                   MOVEL     *ALL'-'       CMP198
208200031009     C                   MOVEL     *ALL'-'       CMP132D
208300031009     C                   MOVEL     *ALL'-'       CMP198D
208400950526     C*
208500030722     C*  Carico dati tabella 4H anomalie da spunte
208600950526     C                   Z-ADD     1             KKUT
208700950526     C                   MOVEL     '4H'          KCOD
208800070601     C                   Z-ADD     0             X                 3 0
208900950526     C     KTAB          CHAIN     TABEL00F                           31
209000950526     C     *IN31         DOWEQ     '0'
209100950526     C     X             ANDLT     100
209200950526     C                   ADD       1             X
209300950526     C                   MOVEL     TBLUNI        DS4H
209400950526     C                   MOVEL     TBLKEY        C4H(X)
209500950526     C                   MOVEL     §4HDES        D4H(X)
209600950526     C                   MOVEL     §4HTPB        T4H(X)
209700950908     C                   MOVEL     §4HSTP        S4H(X)
209800950526     C     KTAB          READE     TABEL00F                               31
209900950526     C                   END
210000010620     C* Chaino tabella variabili pgm arrivi per reperire minuti di
210100010620     C* tolleranza
210200010620     C                   MOVEL     '7A'          KCOD
210300010620     C                   MOVEL(P)  '2       '    KKEY
210400010620     C     KTAB1         CHAIN     TABEL                              31
210500010620     C  N31              MOVEL     TBLUNI        DS7A2
210600010620     C   31              Z-ADD     1             §7AMNP
210700030728     C                   OPEN      FNLRF7P
210800030722     C*  Carico dati tabella DTA anomalie danni
210900990204     C                   Z-ADD     1             KKUT
211000990204     C                   MOVEL     'TAD'         KCODE
211100990204     C                   Z-ADD     0             X
211200990204     C     KCODE         CHAIN     TNTBE01L                           31
211300990204     C     *IN31         DOWEQ     '0'
211400990204     C     X             ANDLT     100
211500990204     C     TBESIF        IFEQ      KNSIF
211600990204     C     TBESIF        OREQ      *BLANKS
211700990204     C                   ADD       1             X
211800990204     C                   MOVEL     TBEKE1        TAD(X)
211900990204     C                   MOVEL     TBEUNI        DTA(X)
212000990204     C                   END
212100990204     C     KCODE         READE     TNTBE01L                               31
212200990204     C                   END
212300030703     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
212400040827     C                   CLEAR                   FIP
212500040827     C     *LOVAL        SETLL     AZORG01L
212600040827     C                   DO        *HIVAL
212700040827     C                   READ      AZORG01L                               95
212800040827     C   95              LEAVE
212900040827     C                   MOVEL     ORGDE3        OG143
213000040827     C     §OgNTW        IFEQ      'PPT'
213100040827     C                   Z-ADD     1             z                 3 0
213200040827     C     *ZEROS        LOOKUP    FIP(z)                                 23
213300040827     C   23              Z-ADD     ORGFIL        FIP(z)
213400040827     C                   END
213500040827     C                   END
213600950526     C*
213700950526     C                   ENDSR
213800950526     O*--------------------------------------------------------------*
213900950526     O*   STAMPA                                                     *
214000950526     O*--------------------------------------------------------------*
214100030728     OSYSPRT    E            SPED1       1
214200990803     O                       EVBAAS        Z      4
214300980325     O                       EVBLNP        Z     16
214400980325     O                       EVBNRS        Z     19
214500980325     O                       EVBNSP        Z     27
214600980325     O                                           80 'DELETE ERRATO'
214700950526     O*--------------------------------------------------------------*
214800950526     O*   AGGIORNAMENTI                                              *
214900950526     O*--------------------------------------------------------------*
215000951109     OFNARB000  E            AGGARB
215100950526     O                       ARBDDC
215200950526     O                       ARBNDC
215300020917     O                       ARBIFP
215400950526     O                       ARBPDC
215500950526     O                       ARBSTP
215600140410     O                       ARBNMI
215700140410     O                       ARBFDC
215800140410     O                       ARBNSS
215900990519     OFNANM000  E            AGGANM
216000950915     O                       ANMDCH
216100950915     O                       ANMFSC
216200950915     O                       ANMCCH
216300950918     O                       ANMNFV
216400950918     O                       ANMNPS
216500010702**
216600040409collo                    spuntato su distinta Nr.       pistola nr.xx
216700040421collo  NON + USATO       non spuntato in uscita ma arrivato pistola nr.xx
216800040421collo  NON + USATO       non spuntato in uscita non arrivato ma partito pistola nr.xx
216900040415VUOTO
217000040421collo  NON + USATO       non spuntato in uscita non arrivato e non partito
217100040419collo                    spuntato in uscita pistola nr.xx ore xx:xx ma già consegnato
217200040415collo                    spuntato in uscita pistola nr.xx ore xx:xx
217300040415                         e su foglio xxx-x-xxxxx pistola nr.xx ore xx:xx
217400040419collo                    non spuntato in uscita ma su f. xxx-x-xxxxx pist. nr.xx ore xx:xx
217500081013collo                    collo doppio
217600040421VUOTO
217700040419collo                    NON IN DISTINTA ma spuntato in uscita pistola nr.xx ore xx:xx
217800101124VUOTO
217900990204**
218000040421        Pratica C.A. aperta:  NON + USATO
218100040421        Pratica C.A. chiusa:  NON + USATO
218200010702**
218300010702In IMA XXX XXXXXX Del XX/XX PstXX
