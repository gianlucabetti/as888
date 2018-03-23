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
012100140611     D fnls46ds1     e ds                  inz
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
029300140612     c*
029400140612     c                   clear                   fnls46ds1
029500140612     c                   movel     'C'           ils46tla
029600140612     c                   call      'FNLS46R1'
029700140612     c                   parm                    kpjba
029800140612     c                   parm                    fnls46ds1
029900140430     c                   eval      kpjbu=s_kpjbu
030000140430     c
030100030721     C                   SETON                                        LR
030200030721     c                   return
030300030721     c                   end
030400030729     C* spengo indicatore utilizzati nella routine di stampa
030500030729     C* spunte doppie.
030600030729     C                   SETOFF                                       2021
030700950526     C*  Valorizzo campi testata
030800950526     C                   EXSR      STPTES
030900040825     C     D59NDT        IFNE      EXNDT
031000040825     C                   Z-ADD     D59NDT        EXNDT
031100040825     C                   END
031200140507      **
031300140507     c* Apertura condizionata del file degli STOP per ottimizzare le prestazioni.
031400140507     c*  Se richiamato da azione LR59 o LR60 DEVE  utilizzare e fare il calcolo degli STOP.
031500140507     c*   Quindi Apriamo il file.
031600140507   <-c                   if        kcDAZ = 'LR59' or
031700140507   <-c                             kcDAZ = 'LR60'
031800140507   <-c                   open      fnarbZ0F                             99
031900140507   <-c                   end
032000140507      **
032100031009     C*  Leggo FNARBY dove ho memorizzato le spedizioni della distinta
032200031009     c*  con o senza errori
032300950529     C     *LOVAL        SETLL     FNARBY1L
032400031009     c                   do        *hival
032500031009     C                   READ      FNARBY1L
032600031009     c                   if        %eof(fnarby1l)
032700031009     c                   leave
032800031009     c                   end
032900950525     C                   EXSR      GESSPE
033000950525     C                   DELETE    FNARBY1L
033100030805     C                   ENDdo
033200030825     C*  Controllo se numero bancali caricati è maggiore a quello
033300110518     C*  in FIQDT
033400030825     C     nban          IFgt      D59NBL
033500030805     C                   Z-ADD     1             Y                 3 0
033600030805     C     '16'          LOOKUP    C4H(Y)                                 32
033700031016     C*  Controllo se si deve stampare l'aomalia
033800030805     C     S4H(Y)        IFNE      'N'
033900031016     C                   MOVEL     D4H(Y)        DErrq
034000110414     C                   CLEAR                   Terrq
034100031016     C                   Z-ADD     nban          WNRB1
034200031016     C                   Z-ADD     D59NBL        WNRB2
034300030805     C                   MOVEL     'S'           D59ANM
034400101201     c                   if        d59sqa =  ' ' or
034500101201     c                             d59sqa =  'T'
034600101125     c                   exsr      states
034700030805     C                   write     ERRQUA
034800101125     c                   end
034900030805     C                   ADD       2             D59LIN
035000030805     C                   END
035100030805     C                   END
035200010620     C*  Routine di stampa spunte consegna doppie
035300101201     c                   if        d59sqa =  ' ' or
035400101201     c                             d59sqa =  'T'
035500010620     C                   EXSR      DOPPIE
035600101125     c                   end
035700950526     C*  Loop x eventuale cancellazione FNARBZ
035800140507      *  solo
035900140507     c*  Se richiamato da azione LR59 o LR60 DEVE  utilizzare
036000140507     c*  e fare il calcolo degli STOP.
036100140507   <-c                   if         kcdaz='LR59' or kcdaz='LR60'
036200950526     C                   EXSR      DLTARZ
036300140507   <-c                   end
036400140507      *
036500030721     C                   MOVEL(p)  FNLR59        KPJBU
036600950525     C                   SETON                                        RT
036700950525     C*--------------------------------------------------------------*
036800950526     C*  GESSPE: Gestione spedizione
036900950525     C*--------------------------------------------------------------*
037000950526     C     GESSPE        BEGSR
037100950525     C*  Cerco la spedizione su FNARB
037200950525     C                   Z-ADD     ARBAAS        KAAS
037300950525     C                   Z-ADD     ARBLNP        KLNP
037400950525     C                   Z-ADD     ARBNRS        KNRS
037500950525     C                   Z-ADD     ARBNSP        KNSP
037600950525     C     KARB          CHAIN     FNARB01L                           33
037700030825     c                   if        not *in33
037800031009     c* mi salvo la spedizione
037900031009     C                   MOVEL     DSARB         DSARBS
038000030805     C* CALCOLO IL NUMERO DEI BANCALI
038100030805     c                   exsr      srnban
038200031009     c* IMPOSTA I CAMPI IN STAMPA
038300110414     C                   MOVEL     ARBTSP        TIPERR
038400031009     C                   Z-ADD     ARBAAS        WAAS
038500031009     C                   Z-ADD     ARBLNP        WLNP
038600031009     C                   Z-ADD     ARBNRS        WNRS
038700031009     C                   Z-ADD     ARBNSP        WNSP
038800030731     C                   Z-ADD     ARBlna        Wlna
038900031009     C                   MOVEL     ARBRSD        WRSD
039000031009     C                   Z-ADD     ARBNCD        WNCD
039100031009     C                   Z-ADD     ARBNCA        WNCA
039200031009     C                   Z-ADD     ARBZNC        WZNC
039300030805     C                   movel     ARBmgs        Wcommes           2 0
039400030805     C                   move      ARBmgs        Wcomgg            2 0
039500031009     c                   movel     wcomgg        wdat
039600030805     c                   move      wcommes       wdat
039700031009     C* segnacolli non consecutivi
039800031009     C     ARBFNS        COMP      'S'                                    44
039900031009     C                   IF        NOT *IN44
040000031009     C                   MOVEL     '/'           WBAR
040100031009     C                   ELSE
040200031009     C                   CLEAR                   WBAR
040300031009     C                   END
040400030825     C                   END
040500031009     C* LOOP SU ERRORI
040600950525     C                   MOVEL     'N'           WERRBL            1
040700950525     C                   MOVEL     'N'           WNODIS            1
040800950525     C                   Z-ADD     1             X                 3 0
040900010702     C     X             DOWLE     10
041000950525     C     CDA(X)        ANDNE     *BLANKS
041100950530     C                   MOVEL     CDA(X)        WCDANO            2
041200950525     C                   EXSR      GESANO
041300950530     C                   ADD       1             X
041400031009     C                   ENDdo
041500950525     C*  Controllo se adesso è già stata messa in consegna
041600950529     C     ARBNDC        IFNE      *ZEROS
041700950530     C     ARBDCM        ANDEQ     0
041800950529     C     ARBNDC        ANDNE     D59NDT
041900950525     C     WNODIS        ANDEQ     'N'
042000031009     C                   MOVEL     '03'          WCDANO
042100950525     C                   EXSR      GESANO
042200040713     c* rendo bloccante l'errore anche se in tabella è stato deciso di
042300040713     c* non stamparlo
042400040713     C                   MOVEL     'S'           WERRBL
042500950525     C                   END
042600070530     c* METTE IN DISTINTA LA SPEDIZIONE se pgm "spunte"
042700040827     c                   movel     arbaas        arbdsp            8 0
042800040827     c                   move      arbmgs        arbdsp
042900031016     C* Se non ho trovato errore bloccante aggiorno la distinta
043000030721     C     WERRBL        IFEQ      'N'
043100070601     c                   if        arbngd=0 and arbndc = 0 and
043200070530     c                             (kcdaz='LR59' or kcdaz='LR60')
043300020917     C                   Z-ADD     D59FGS        ARBIFP
043400020917     C                   Z-ADD     D59NDT        ARBNDC
043500950525     C                   Z-ADD     D59DDT        ARBDDC
043600950525     C                   Z-ADD     D59PDR        ARBPDC
043700140410     C                   movel     *blanks       ARBfdc
043800140410     c                   z-add     0             arbnmi
043900140410     c                   z-add     0             arbnss
044000951121     C*  Attribuisco lo stop
044100951211     C     ARBKCS        IFNE      0
044200951211     C                   MOVEL     *BLANKS       KRAG
044300951211     C                   MOVEL     ARBKCS        KRAG
044400951211     C                   ELSE
044500951121     C                   MOVEL     ARBRSD        KRAG
044600951122     C                   MOVE      ARBIND        KRAG
044700951211     C                   END
044800951121     C     KRAG          CHAIN     FNARBZ0F                           32
044900950526     C*  Aggiorno la  bolla
045000950526     C                   EXCEPT    AGGARB
045100031013     C                   MOVEL     DSARB         DSARBS
045200031020     C                   END
045300031009     C*  DETTAGLIO SEGNACOLLI
045400980330     C                   Z-ADD     ARBAAS        KAAS
045500980330     C                   Z-ADD     ARBLNP        KLNP
045600980330     C                   Z-ADD     ARBNRS        KNRS
045700980330     C                   Z-ADD     ARBNSP        KNSP
045800950614     C                   MOVEL     'N'           WERRPZ            1
045900031009     C                   MOVEL     'N'           OKMIC             1
046000950614     C     KART          CHAIN     FNART01L                           32
046100030721     C*  Se non esiste e spedizione figlia mi posiziono su mamma
046200950614     C     *IN32         IFEQ      '1'
046300950606     C     KLBL          CHAIN     FNLBL01L                           32
046400950606     C     *IN32         IFEQ      '0'
046500950606     C     LBLLAP        ANDEQ     LBLLAN
046600950606     C                   Z-ADD     LBLAAP        KAAS
046700950606     C                   Z-ADD     LBLLPP        KLNP
046800950606     C                   Z-ADD     LBLNRP        KNRS
046900950606     C                   Z-ADD     LBLNSP        KNSP
047000950606     C     KARB          CHAIN     FNARB01L                           32
047100980430     C     ARBCCA        IFNE      '2'
047200980430     C     ARBCCA        ANDNE     '6'
047300950614     C     KART          CHAIN     FNART01L                           32
047400980430     C                   ELSE
047500980430     C                   Z-ADD     LBLAAN        KAAS
047600980430     C                   Z-ADD     LBLLPN        KLNP
047700980430     C                   Z-ADD     LBLNRN        KNRS
047800980430     C                   Z-ADD     LBLNSN        KNSP
047900980430     C     KARB          CHAIN     FNARB01L                           32
048000950614     C                   END
048100980430     C                   END
048200950614     C                   END
048300950614     C*  Loop su dettaglio segnacolli per verifcare se la bolla
048400950614     C*  è consegnata parzialmente su altra distinta
048500950526     C     *IN32         DOWEQ     '0'
048600000705     C                   EXSR      GESDET
048700031009     C                   ENDdo
048800070604     c* SCRIVO EVENTO MIC E ORM CONTESTUALE ALLA CONSEGNA
048900031020     c                   movel     dsarbs        dsarb
049000070530     c                   if        okmic = 'S'
049100100514     c                   clear                   ds7r
049200100514     c                   if        arbgma <> ' '
049300100514     c                   exsr      sr7r
049400100514     c                   end
049500100514     c                   if        §7RRC ='S'
049600031020     c                   clear                   fnlr99ds
049700071002     c                   eval      i99nftl= 99999
049800071002     c                   eval      i99fao = 400
049900031009     c                   exsr      aggorm
050000031020     c                   end
050100031020     c                   exsr      aggeve
050200070601     c*                  end
050300070530     c                   end
050400950526     C*
050500951109     C                   ELSE
050600030721     C  N33              unlock    fnarb01l
050700950526     C                   END
050800950526     C*
050900950526     C                   ENDSR
051000031003     C*--------------------------------------------------------------*
051100031003     C*  GESDET: Gestione dettaglio segnacolli
051200031003     C*--------------------------------------------------------------*
051300031003     C     GESDET        BEGSR
051400950526     C*
051500040409     C                   SETOFF                                       02
051600031009     C                   MOVEL     *BLANKS       WTIPCA
051700031009     C                   MOVEL     *BLANKS       WDETER
051800950530     C                   MOVEL     *BLANKS       WNRP              2
051900031009     C                   Z-ADD     0             WNRP1             2 0
052000950614     C                   MOVEL     'N'           WTRDAT            1
052100950614     C                   MOVEL     'N'           WTRDA1            1
052200950918     C                   MOVEL     'N'           WTRSPU            1
052300950526     C*  Controllo se non presente su distinta
052400950526     C                   Z-ADD     4             KNPG
052500950526     C                   Z-ADD     D59NDT        KNFV
052600950526     C                   Z-ADD     D59FGS        KFGS
052700950526     C                   Z-ADD     ARBLNA        KLNA
052800950526     C                   Z-ADD     ARBFLS        KFLS
052900950526     C                   Z-ADD     ARBNRS        KNRS
053000950526     C                   Z-ADD     ARTNSC        KNSC
053100000706      *
053200070129     C     KBRV1         CHAIN     FNBRV01L                           33
053300040408     c* NON HO LA SPUNTA 4
053400030729     C     *IN33         IFEQ      '1'
053500040421     c* solo se la spedizione è in distinta
053600040507     c                   if        s_arbndc  <> 0
053700990204     C* Controllo Dati Danni
053800040420     C*                  EXSR      TESDAN
053900950526     C*  Se non è presente su questa distinta verifico se è
054000950526     C*  in consegna su di un'altra
054100040408     c                   exsr      srfibrv
054200040408     c                   movel     okspu         wtrdat
054300030801     C*    se su altra distinta
054400030729     C     WTRDAT        IFEQ      'S'
054500950526     C     BRVNFV        ANDNE     0
054600040409     C                   MOVEL(p)  ERR(1)        WDETER
054700101124     c                   movel     '001'         tbeke2
054800040419     c                   movel     fvvnfv        com05
054900040419     c                   eval      %subst(wdeter: 51: 5)= com05
055000040409     c                   eval      %subst(wdeter: 71: 20)= fvvdsf
055100101126     c                   eval      sfgs=fvvfgs
055200101126     c                   eval      snpg=fvvnpg
055300101126     c                   eval      snfv=fvvnfv
055400110117     c                   eval      sdfv=fvvdfv
055500101126     c                   eval      shms=brvhms
055600950614     C                   MOVEL     BRVNPS        WNRP
055700950526     C                   EXSR      STAMPA
055800960507     C*  Richiamo TRUL11 per reperimento della fase
055900960507     C                   CLEAR                   DSFASE
056000960507     C                   MOVEL     '4'           WNPG
056100960507     C                   CALL      'TRUL11R'
056200960507     C                   PARM                    DSFASE
056300950919     C                   MOVEL     'S'           WTRSPU
056400950918     C                   Z-ADD     150           WCAA
056500950918     C                   Z-ADD     BRVNFV        WNFV
056600950918     C                   Z-ADD     BRVNPS        WNPS
056700960507     C                   Z-ADD     FVVFGS        WFGS
056800950918     C                   EXSR      WRTANM
056900030729     C                   END
057000950614     C*  Controllo se collo spuntato in I.M.A.
057100950614     C                   Z-ADD     3             KNPG
057200040408     c                   exsr      srfibrv
057300040408     c                   movel     okspu         wtrda1
057400040408     C*
057500950918     C                   MOVEL     *BLANKS       WDETER
057600950919     C                   SETOFF                                       02
057700950614     C                   Z-ADD     4             KNPG
057800030801     c*    se in inventario IMA
057900030729     C     WTRDA1        IFEQ      'S'
058000950614     C     BRVNFV        ANDNE     0
058100950614     C                   MOVEL     BRVNPS        WNRP
058200030801     c*    ...e su altra distinta
058300950919     C     WTRSPU        IFEQ      'S'
058400960507     C     FVVFCF        IFNE      'S'
058500040421     C                   MOVEL(p)  ERR(8)        WDETER
058600101124     c                   movel     '008'         tbeke2
058700040419     c                   eval      sfgs=fvvfgs
058800040419     c                   eval      snpg=fvvnpg
058900040419     c                   eval      snfv=fvvnfv
059000110117     c                   eval      sdfv=fvvdfv
059100040419     c                   eval      shms=brvhms
059200960507     C                   END
059300950919     C                   ELSE
059400030801     c*    ...no su altra distinta
059500040415     C                   MOVEL(p)  ERR(9)        WDETER
059600101124     c                   movel     '009'         tbeke2
059700040419     c                   eval      sfgs=fvvfgs
059800040419     c                   eval      snpg=fvvnpg
059900040419     c                   eval      snfv=fvvnfv
060000110117     c                   eval      sdfv=fvvdfv
060100040419     c                   eval      shms=brvhms
060200950919     C                   END
060300030801     c*
060400960507     C     WTRSPU        IFNE      'S'
060500960507     C     FVVFCF        ORNE      'S'
060600950614     C                   EXSR      STAMPA
060700960507     C*  Richiamo TRUL11 per reperimento della fase
060800960507     C                   CLEAR                   DSFASE
060900960507     C                   MOVEL     '3'           WNPG
061000960507     C                   MOVEL     'A'           WSPG
061100960507     C                   CALL      'TRUL11R'
061200960507     C                   PARM                    DSFASE
061300950919     C                   MOVEL     'S'           WTRSPU
061400950918     C                   Z-ADD     135           WCAA
061500950920     C                   Z-ADD     BRVNFV        WNFV
061600950918     C                   Z-ADD     WNRP1         WNPS
061700960507     C                   Z-ADD     FVVFGS        WFGS
061800950915     C                   EXSR      WRTANM
061900960507     C                   END
062000030729     C                   END
062100950918     C     WTRSPU        IFEQ      'S'
062200950918     C                   GOTO      NEWSGN
062300950918     C                   END
062400950526     C*  Controllo se il collo è arrivato
062500040419     C*    ARTDAM        IFNE      0
062600040419     C*    ARTDCM        ANDEQ     0
062700040419     C*    WDAVM         ANDEQ     'N'
062800040419     C*                  MOVEL(p)  ERR(2)        WDETER
062900040419     C*                  MOVEL     ARTNAP        WNRP
063000040419     C*                  EXSR      STAMPA
063100040419     C*                  GOTO      NEWSGN
063200040419     C*                  END
063300950526     C*  Collo partito e non arrivato
063400040419     C*    ARTDFV        IFNE      0
063500040419     C*    ARTDCM        ANDEQ     0
063600040419     C*    WDAVM         ANDEQ     'N'
063700040419     C*                  MOVEL(p)  ERR(3)        WDETER
063800040419     C*                  MOVEL     ARTNPP        WNRP
063900040419     C*                  EXSR      STAMPA
064000040419     C*                  GOTO      NEWSGN
064100040419     C*                  END
064200950526     C*  Collo non arrivato e non partito
064300040420     C*    ARTDCM        IFEQ      0
064400040420     C*    WDAVM         ANDEQ     'N'
064500040420     C*                  MOVEL(p)  ERR(5)        WDETER
064600040420     C*                  EXSR      STAMPA
064700040420     C*                  GOTO      NEWSGN
064800040420     C*                  END
064900950526     C*
065000040421     C                   end
065100030729     C                   ELSE
065200081103     c*
065300081103     C                   MOVEL     BRVNPS        WNRP
065400081103     C                   Z-ADD     BRVNPS        WNRP1
065500081103     C                   eval      dsbrvs = dsbrv
065600040408     c* HO LA SPUNTA 4
065700081014     c* per prima memorizzo segnazione di probabile etichetta doppia
065800081014     c                   clear                   fnlrv7ds
065900081014     c                   eval      ILRV7FLS=kfls
066000081014     c                   eval      ILRV7LNA=klna
066100081014     c                   eval      ILRV7NRS=knrs
066200081014     c                   eval      ILRV7NSC=knsc
066300081014     c                   movel(p)  fnlrv7ds      kpjbu
066400081014     c                   call      'FNLRV7R'
066500081014     c                   parm                    kpjba
066600081014     c                   movel     kpjbu         fnlrv7ds
066700081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
066800090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
066900081014     C                   MOVEL(p)  ERR(10)       WDETER
067000101124     c                   movel     '010'         tbeke2
067100101126     c                   eval      shms=s_brvhms
067200101126     c                   eval      sfgs=s_brvfgs
067300101126     c                   eval      snpg=s_brvnpg
067400110117     c                   eval      snfv=s_brvnfv
067500110117     c                   eval      sdfv=d59ddt
067600081014     C                   EXSR      STAMPA
067700081014     C                   END
067800040408     C* Controllo se collo spuntato anche in I.M.A.
067900040408     C                   Z-ADD     3             KNPG
068000040408     c                   exsr      srfibrv
068100040408     c                   movel     okspu         wtrda1
068200040408     C                   Z-ADD     4             KNPG
068300110118     c**********************************************************************
068400110118     c* se sono chiamato dalla sistemazione controllo aut, non segnalo
068500110118     c* l'errore e tento di cancellare la spunta + vecchia tra la 3 e la 4
068600110120     c                   if        kcdaz = 'LRQ1' and wtrda1 = 'S'
068700110118     c                   exsr      srls46
068800110118     c* se no errore ed effettuato l'aggiornamento proseguo
068900110118     c                   if        OLS46ERR = ' ' and
069000110118     c                             OLS46ANN = 'S'
069100110118     c* se cancellato la 3 e resta la 4 verifico se bolla in distinta ok
069200110118     c* segnalo errore
069300110118     c                   if        s_brvhms > brvhms
069400110118     c                   if        s_arbndc = 0
069500110118     c                   eval      wtrda1 = 'N'
069600110118     c                   clear                   dsbrv
069700110118     c                   clear                   dsfvv
069800110118     c                   else
069900110118     C                   GOTO      NEWSGN
070000110118     c                   end
070100110118     c                   else
070200110118     c* se cancellato la 4 e resta la 3 verifico se bolla in distinta errore
070300110118     c* altrimenti ok
070400110118     c                   if        s_arbndc = 0
070500110118     C                   GOTO      NEWSGN
070600110118     c                   else
070700110118     C                   MOVEL(p)  ERR(9)        WDETER
070800110118     c                   movel     '009'         tbeke2
070900110118     c                   eval      sfgs=fvvfgs
071000110118     c                   eval      snpg=fvvnpg
071100110118     c                   eval      snfv=fvvnfv
071200110118     c                   eval      sdfv=fvvdfv
071300110118     c                   eval      shms=brvhms
071400110118     C                   EXSR      STAMPA
071500110118     C                   GOTO      NEWSGN
071600110118     c                   end
071700110118     c                   end
071800110118     c                   end
071900110118     c                   end
072000110118     c**********************************************************************
072100040415     C* Se ho SOLO la spunta 4 ma la spedizione non è in distinta
072200040408     C* segnalo l'errore
072300110118     C                   IF        s_arbndc = 0
072400040408     C                   IF        wtrda1 = 'N'
072500040415     C                   MOVEL(p)  ERR(12)       WDETER
072600101124     c                   movel     '012'         tbeke2
072700101126     c                   eval      sfgs=s_brvfgs
072800101126     c                   eval      snpg=s_brvnpg
072900101126     c                   eval      snfv=s_brvnfv
073000040419     c                   eval      shms=s_brvhms
073100110117     c                   eval      sdfv=d59ddt
073200040408     C                   EXSR      STAMPA
073300040408     C                   GOTO      NEWSGN
073400040408     C* Se ho la spunta 4 e la spunta 3 ma la spedizione non è in distinta
073500040408     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
073600040408     C* spunta 4 > di ora spunta 3
073700040408     c                   else
073800110118     c
073900040409     c                   if        s_brvnps <> brvnps or
074000040409     c                             (s_brvnps = brvnps and s_brvhms > brvhms)
074100101124     C                   MOVEL(p)  ERR(12)       WDETER
074200101124     c                   movel     '012'         tbeke2
074300040419     c                   eval      shms=s_brvhms
074400101126     c                   eval      sfgs=s_brvfgs
074500101126     c                   eval      snpg=s_brvnpg
074600101126     c                   eval      snfv=s_brvnfv
074700101126     c                   eval      shms=s_brvhms
074800110117     c                   eval      sdfv=d59ddt
074900040415     C                   EXSR      STAMPA
075000040419     C                   MOVEL(p)  ERR(8)        WDETER
075100101124     c                   movel     '008'         tbeke2
075200040419     c                   eval      sfgs=fvvfgs
075300040419     c                   eval      snpg=fvvnpg
075400040419     c                   eval      snfv=fvvnfv
075500110117     c                   eval      sdfv=fvvdfv
075600040419     c                   eval      shms=brvhms
075700040419     c                   movel     brvnps        wnrp
075800040415     c                   seton                                        02
075900040409     C                   EXSR      STAMPA
076000040415     c                   setoff                                       02
076100040415     C                   GOTO      NEWSGN
076200040415     c                   else
076300040408     C                   GOTO      NEWSGN
076400040408     c                   end
076500040408     C                   END
076600040409     C                   END
076700040408     c*
076800031013     c                   eval      okmic = 'S'
076900031009     C* Se è stato spuntato con la distinta in esame controllo
077000031009     C* se segnacollo già consegnato
077100030729     C     ARTDCM        IFNE      0
077200040415     C                   MOVEL(P)  ERR(6)        WDETER
077300101124     c                   movel     '006'         tbeke2
077400040419     c                   eval      shms=s_brvhms
077500101126     c                   eval      sfgs=s_brvfgs
077600101126     c                   eval      snpg=s_brvnpg
077700101126     c                   eval      snfv=s_brvnfv
077800110117     c                   eval      sdfv=d59ddt
077900950526     C                   EXSR      STAMPA
078000950614     C                   GOTO      NEWSGN
078100030729     C                   END
078200040421     C* Se ho la spunta 4 e la spunta 3 e la spedizione è in distinta
078300040421     C* segnalo l'errore se: pistola >< oppure se stessa pistola ma ora
078400040421     C* spunta 3 > di ora spunta 4
078500040421     c                   if        s_brvnps <> brvnps or
078600040421     c                             (s_brvnps = brvnps and s_brvhms < brvhms)
078700030729     C     WTRDA1        IFEQ      'S'
078800040408     C     BRVNFV        ANDNE     0
078900960507     C     FVVFCF        IFNE      'S'
079000040415     C                   MOVEL(P)  ERR(7)        WDETER
079100101124     c                   movel     '007'         tbeke2
079200040419     c                   eval      shms=s_brvhms
079300101126     c                   eval      sfgs=s_brvfgs
079400101126     c                   eval      snpg=s_brvnpg
079500101126     c                   eval      snfv=s_brvnfv
079600110117     c                   eval      sdfv=d59ddt
079700040409     C                   EXSR      STAMPA
079800040415     C                   MOVEL(P)  ERR(8)        Wdeter
079900101124     c                   movel     '008'         tbeke2
080000040419     c                   eval      sfgs=fvvfgs
080100040419     c                   eval      snpg=fvvnpg
080200040419     c                   eval      snfv=fvvnfv
080300110117     c                   eval      sdfv=fvvdfv
080400040419     c                   eval      shms=brvhms
080500040419     c                   movel     brvnps        wnrp
080600040415     c                   seton                                        02
080700950612     C                   EXSR      STAMPA
080800040415     c                   setoff                                       02
080900960507     C*  Richiamo TRUL11 per reperimento della fase
081000960507     C                   CLEAR                   DSFASE
081100960507     C                   MOVEL     '3'           WNPG
081200960507     C                   MOVEL     'A'           WSPG
081300960507     C                   CALL      'TRUL11R'
081400960507     C                   PARM                    DSFASE
081500950918     C                   Z-ADD     135           WCAA
081600950920     C                   Z-ADD     BRVNFV        WNFV
081700950918     C                   Z-ADD     WNRP1         WNPS
081800960507     C                   Z-ADD     FVVFGS        WFGS
081900950915     C                   EXSR      WRTANM
082000031009     C                   GOTO      NEWSGN
082100030729     C                   END
082200030729     C                   END
082300040421     C                   END
082400950526     C*
082500030729     C                   END
082600950526     C*  Leggo successivo segnacollo
082700950614     C     NEWSGN        TAG
082800950526     C     KART          READE     FNART01L                               32
082900950526     C*
083000950526     C                   ENDSR
083100031009     C*--------------------------------------------------------------*
083200040408     C*  srfibrv cerco spunta nel foglio 4/3 nel gg. della distinta che
083300040408     c*          sto quadrando
083400031009     C*--------------------------------------------------------------*
083500040408     C     srfibrv       BEGSR
083600040408     c                   movel     'N'           okspu             1
083700040409     c                   clear                   dsbrvc
083800040409     c                   clear                   dsfvvc
083900031009     C*
084000070129     C     KBRV2         SETll     FNBRV09L
084100040408     c                   do        *hival
084200070129     C     KBRV2         READe     FNBRV09L
084300070129     c                   if        %eof(fnbrv09l)
084400040408     c                   leave
084500040408     c                   end
084600040408     C                   Z-ADD     BRVNFV        KNFV1
084700040408     C     KFVV          CHAIN     FNFVV01L
084800040408     c                   if        %found(fnfvv01l) and fvvdfv = d59ddt
084900040408     C                   MOVEL     'S'           okspu
085000040409     c                   if        brvhms > c_brvhms
085100040409     c                   eval      dsbrvc = dsbrv
085200040409     c                   eval      dsfvvc = dsfvv
085300040408     c                   end
085400040409     c                   end
085500040408     C                   ENDDO
085600040409     c*
085700040409     c                   eval      dsbrv = dsbrvc
085800040409     c                   eval      dsfvv = dsfvvc
085900040408     C*
086000040408     C                   ENDSR
086100040408     C*--------------------------------------------------------------*
086200040408     C*  AGGEVE: scrivo evento MIC sia per distinte manuali che x quelle
086300040408     c*          da spunte
086400040408     C*--------------------------------------------------------------*
086500040408     C     AGGEVE        BEGSR
086600040408     C*
086700131104     C                   clear                   dEVB01
086800131104     C                   move      *zeros        §NOTNDC
086900131104     C                   move      *zeros        §NOTPDC
087000131104     C                   move      ARBPDC        §NOTPDC
087100131104     C                   move      ARBNDC        §NOTNDC
087200131104     C                   MOVEL     Devb01        dEVB01sav
087300031009     C* Nel momento in cui metto in consegna la bolla controllo se
087400031009     C* ho scritto evento NIC provvisorio nell'archivio eventi della
087500031009     C* QTEMP e nel caso lo cancello
087600031009     C                   Z-ADD     ARBAAS        KAAS
087700031009     C                   Z-ADD     ARBLNP        KLNP
087800031009     C                   Z-ADD     ARBNRS        KNRS
087900031009     C                   Z-ADD     ARBNSP        KNSP
088000031009     C     KLBL          CHAIN     FNLBL01L                           32
088100031009     C     *IN32         IFEQ      '0'
088200031009     C     LBLLAP        ANDEQ     LBLLAN
088300031009     C                   Z-ADD     LBLAAP        KAAS
088400031009     C                   Z-ADD     LBLLPP        KLNP
088500031009     C                   Z-ADD     LBLNRP        KNRS
088600031009     C                   Z-ADD     LBLNSP        KNSP
088700031009     C     KARB          CHAIN(N)  FNARB01L                           32
088800031009     C     ARBCCA        IFEQ      '2'
088900031009     C     ARBCCA        OREQ      '6'
089000031009     C                   Z-ADD     LBLAAN        KAAS
089100031009     C                   Z-ADD     LBLLPN        KLNP
089200031009     C                   Z-ADD     LBLNRN        KNRS
089300031009     C                   Z-ADD     LBLNSN        KNSP
089400031009     C     KARB          CHAIN(N)  FNARB01L                           32
089500031009     C                   SETON                                        32
089600031009     C                   END
089700031009     C                   END
089800031009     C     *IN32         IFEQ      '0'
089900031009     C     LBLLAP        ANDEQ     LBLLAN
090000031009     C                   Z-ADD     LBLAAP        KAAS
090100031009     C                   Z-ADD     LBLLPP        KLNP
090200031009     C                   Z-ADD     LBLNRP        KNRS
090300031009     C                   Z-ADD     LBLNSP        KNSP
090400031009     C                   ELSE
090500031009     C                   Z-ADD     ARBAAS        KAAS
090600031009     C                   Z-ADD     ARBLNP        KLNP
090700031009     C                   Z-ADD     ARBNRS        KNRS
090800031009     C                   Z-ADD     ARBNSP        KNSP
090900031009     C                   END
091000130326     C     KEVB1         CHAIN     WFNEVB01L                          36
091100031009     C     EVBCEV        IFEQ      'NIC'
091200130326     C  N36              DELETE    WFNEVB01L
091300031009     C                   ELSE
091400031009     C  N36              EXCEPT    SPED1
091500031009     C                   END
091600131218      **
091700031009     C* Aggiorno evento di messa in consegna
091800031009     C                   MOVEL     'MIC'         KCEV
091900031009     C     KEVB2         SETGT     FNEVB04L
092000031009     C     KEVB2         READPE    FNEVB04L                               37
092100131218      **
092200031009     C     *IN37         IFEQ      '1'
092300031009     C     EVBDEV        ORNE      D59DDT
092400131218      **
092500031009     C                   MOVEL     'MIC'         EVBCEV
092600031009     C                   Z-ADD     D59AMG        EVBDTV
092700031009     C                   TIME                    EVBORV
092800031009     C                   Z-ADD     D59FGS        EVBFLE
092900031009     C                   MOVE      D59CDU        EVBCDU
093000031009     C                   Z-ADD     KAAS          EVBAAS
093100031009     C                   Z-ADD     KLNP          EVBLNP
093200031009     C                   Z-ADD     KNRS          EVBNRS
093300031009     C                   Z-ADD     KNSP          EVBNSP
093400031009     C                   Z-ADD     D59DDT        EVBDEV
093500031009     C                   Z-ADD     0             EVBDTR
093600031009     C                   MOVEL     *BLANKS       EVBFTR
093700031009     C                   Z-ADD     0             EVBDTC
093800131104     C                   MOVEL     Devb01sav     EVBNOT
093900031009     C     D59FPP        IFEQ      'M'
094000031009     C                   MOVEL     0800          EVBHEV
094100031009     C                   ELSE
094200031009     C                   MOVEL     1200          EVBHEV
094300031009     C                   END
094400031009     C                   WRITE     FNEVB000
094500131218      **
094600031009     C                   ELSE
094700131218      **
094800031009     C     EVBCEV        IFEQ      'MIC'
094900031009     C     EVBDEV        ANDEQ     D59DDT
095000031009     C*
095100031009     C     EVBDTR        IFEQ      0
095200131218     C*
095300131218     C* riaggancia il record  FISICO x aggiornarlo
095400131218     c     evb_nrr       chain     fnevb00F
095500131218     C*
095600131218     c                   if        %Found(fnevb00F)
095700031009     C* Se non è stato trasmesso aggiorno la data e l'ora di
095800031009     C* di immissione evento con quella del giorno + altri dati
095900031009     C                   Z-ADD     D59AMG        EVBDTV
096000031009     C                   TIME                    EVBORV
096100031009     C                   Z-ADD     D59FGS        EVBFLE
096200031009     C                   MOVE      D59CDU        EVBCDU
096300131104     C                   MOVEL     Devb01sav     EVBNOT
096400031009     C     D59FPP        IFEQ      'M'
096500031009     C                   MOVEL     0800          EVBHEV
096600031009     C                   ELSE
096700031009     C                   MOVEL     1200          EVBHEV
096800031009     C                   END
096900031009     C                   MOVEL     *BLANKS       EVBDTC
097000031009     C                   UPDATE    FNEVB000
097100131218     c                   end
097200131218      **
097300031009     C                   ELSE
097400131218      **
097500031009     C* Se l'evento MIC esiste già ed è già stato trasmesso lo
097600031009     C* riscrivo solo se c'è un NIC successivo
097700031029     C                   MOVE      EVBDEV        xdtMIC
097800031029     C                   MOVE      EVBHEV        xoraMIC
097900031029     C                   MOVE      EVBdtv        xdtwrtMIC
098000031029     C                   MOVE      EVBorv        xorawrtMIC
098001151221     C                   MOVE      EVBorv        xorawrtMIC
098100131218      **
098200031009     C                   MOVEL     'NIC'         KCEV
098300031009     C     KEVB2         SETGT     FNEVB04L
098400031009     C     KEVB2         READPE    FNEVB04L                               37
098500131218      **
098600031009     C     *IN37         IFEQ      '0'
098700031009     C     EVBDEV        ANDEQ     D59DDT
098800131218      **
098900031029     C                   MOVE      EVBDEV        xdtNIC
099000031029     C                   MOVE      EVBHEV        xoraNIC
099100031029     C                   MOVE      EVBdtv        xdtwrtNIC
099200031029     C                   MOVE      EVBorv        xorawrtNIC
099201151221     c* se la data di scrittura del MIC è minore del NIC oppure
099202151221     c* se sono uguali e la distinta del MIC è diversa da quella
099203151221     c* della bolla
099204151221     C                   IF        dsxMIC < dsxNIC or
099205151221     C                             (dsxNIC = dsxMIC and
099206151221     C                             §notNDC <> %editc(ARBNDC:'X'))
099400031009     C                   MOVEL     'MIC'         EVBCEV
099500031009     C                   Z-ADD     D59AMG        EVBDTV
099600031009     C                   TIME                    EVBORV
099700031009     C                   Z-ADD     D59FGS        EVBFLE
099800031009     C                   MOVE      D59CDU        EVBCDU
099900031009     C                   Z-ADD     KAAS          EVBAAS
100000031009     C                   Z-ADD     KLNP          EVBLNP
100100031009     C                   Z-ADD     KNRS          EVBNRS
100200031009     C                   Z-ADD     KNSP          EVBNSP
100300031009     C                   Z-ADD     D59DDT        EVBDEV
100400031009     C                   Z-ADD     0             EVBDTR
100500031009     C                   MOVEL     *BLANKS       EVBFTR
100600031009     C     D59FPP        IFEQ      'M'
100700031009     C                   MOVEL     0800          EVBHEV
100800031009     C                   ELSE
100900031009     C                   MOVEL     1200          EVBHEV
101000031009     C                   END
101100031009     C                   Z-ADD     0             EVBDTC
101200131104     C                   MOVEL     Devb01sav     EVBNOT
101300031009     C                   WRITE     FNEVB000
101400131218      **
101500031009     C                   END
101600031009     C*
101700031009     C                   END
101800031009     C                   END
101900031009     C                   END
102000031009     C                   END
102100031020     c* mi reimposto la ds di FNARB originale
102200031020     c                   movel     dsarbs        dsarb
102300031009     C*
102400031009     C                   ENDSR
102500031009     C*--------------------------------------------------------------*
102600100514     C*  sr7r  particolarità consegna
102700031009     C*--------------------------------------------------------------*
102800100514     C     sr7r          BEGSR
102900031009     c*
103000100514     C                   eval      kcod = '7R'
103100100514     C                   eval      kkey = arbgma
103200100514     c                   eval      kkut = 1
103300100514     C     ktab1         chain     Tabel00f
103400100514     c                   if        %found(tabel00f)
103500100514     C                   MOVEL     TBLUNI        DS7R
103600100514     c                   end
103700100514     C*
103800100514     C                   ENDSR
103900100514     C*--------------------------------------------------------------*
104000100514     C*  aggorm: Aggiorno ORM e sue fasi
104100100514     C*--------------------------------------------------------------*
104200100514     C     aggorm        BEGSR
104300100514     c*
104400031009     c                   eval      i99tla = 'L'
104500031020     c                   eval      i99aas = arbaas
104600031020     c                   eval      i99lnp = arblnp
104700031020     c                   eval      i99nrs = arbnrs
104800031020     c                   eval      i99nsp = arbnsp
104900031020     c* metto in distinta
105000071001     c                   eval      i99fgs = d59fgs
105100031020     c                   if        i99fao = 400
105200031020     c                   eval      i99ndc = arbndc
105300031020     c                   eval      i99ddc = arbddc
105400031009     c                   eval      i99npg = 4
105500031020     c                   end
105600031020     c                   eval      i99comit = '1'
105700071001     c                   eval      i99cmt = 'N'
105800031009     c                   call      'FNLR99R'
105900031009     c                   parm                    kpjba
106000031009     c                   parm                    fnlr99ds
106100031009     c*
106200031009     C                   ENDSR
106300110118     C*--------------------------------------------------------------*
106400110118     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
106500110118     c* dal controllo a video
106600110118     C*--------------------------------------------------------------*
106700110118     C     srls46        BEGSR
106800110118     c*
106900140611     c                   clear                   fnls46ds1
107000110118     c                   clear                   kpjbu
107100110118     c                   eval      ILS46NPGA =  brvnpg
107200110118     c                   eval      ILS46NFVA =  brvnfv
107300110118     c                   eval      ILS46FGSA =  brvfgs
107400140611     c                   eval      ILS46dfvA =  d59ddt
107500140611     c                   eval      ILS46dfvB =  d59ddt
107600110120     c                   eval      ILS46NPGB =  s_brvnpg
107700110120     c                   eval      ILS46NFVB =  s_brvnfv
107800110120     c                   eval      ILS46FGSB =  s_brvfgs
107900110121     c                   eval      ILS46AMB =  'QD'
108000110121     c                   eval      ILS46aupr=  'SE'
108100110120     c                   eval      ILS46FLS =  Kfls
108200110118     c                   eval      ILS46LNA =  Klna
108300110118     c                   eval      ILS46NRS =  Knrs
108400110118     c                   eval      ILS46NSC =  Knsc
108500110120     c                   eval      ILS46TELA = 'M'
108600110118     c*
108700140611     c                   call      'FNLS46R1'
108800110118     c                   parm                    kpjba
108900140611     c                   parm                    fnls46ds1
109000110118     c*
109100110118     C                   ENDSR
109200990204     C*--------------------------------------------------------------*
109300990204     C*  TESDAN: Test gestione danni
109400990204     C*--------------------------------------------------------------*
109500990204     C     TESDAN        BEGSR
109600990204     C*
109700990204     C*  Flag pratica danno = 'N'
109800990204     C                   MOVE      'N'           WDAVM             1
109900990204     C*  Se il collo non è stato spuntato su quel foglio di
110000990204     C*  uscita controllo se c'è una pratica C.A. aperta
110100990204     C     KDCD          CHAIN     FNDCD02L                           38
110200990204     C*  Se esiste C.A. aperta controllo il tipo di anomalia
110300990204     C     *IN38         IFEQ      '0'
110400990204     C     DCDATB        ANDNE     'A'
110500990204     C     DCDDCH        IFEQ      0
110600990204     C                   MOVE      TPC(1)        WTIPCA
110700990204     C* Aggancio testata
110800990204     C                   MOVE      DCDAAC        KAAC
110900990204     C                   MOVE      DCDFIL        KFIL
111000990204     C                   MOVE      DCDNCA        KNCA
111100990204     C     KDCT          CHAIN     FNDCT01L                           38
111200990204     C     *IN38         DOWEQ     '0'
111300990204     C     DCTATB        ANDEQ     'A'
111400990204     C     *IN38         OREQ      '0'
111500990204     C     DCTDCH        ANDNE     0
111600990204     C     KDCT          READE     FNDCT01L                               38
111700990204     C                   END
111800990204     C                   Z-ADD     1             YY                4 0
111900990204     C     DCTTAD        LOOKUP    TAD(YY)                                39
112000990204     C     *IN39         IFEQ      '1'
112100990204     C                   MOVEL     DTA(YY)       DTAD
112200990204     C     DRAGR         IFEQ      'M'
112300990204     C     DRAGR         OREQ      'V'
112400031009     C                   MOVE      'S'           WDAVM
112500990204     C                   END                                                    Manc.o Ava D/R
112600990204     C                   END                                                    Trv TAD
112700990204     C*
112800990204     C                   ELSE
112900990204     C                   MOVE      TPC(2)        WTIPCA
113000990204     C                   END                                                    CA Aperta
113100990204     C                   END                                                    CA no annul
113200990204     C*
113300990204     C                   ENDSR
113400950525     C*--------------------------------------------------------------*
113500950525     C*  GESANO: Gestione anomalie
113600950525     C*--------------------------------------------------------------*
113700950525     C     GESANO        BEGSR
113800950525     C*  Se non ho trovato errore bloccante
113900950911     C                   SETOFF                                       031417
114000031009     C                   Z-ADD     1             Y
114100950526     C     WCDANO        LOOKUP    C4H(Y)                                 32
114200950525     C     *IN32         IFEQ      '1'
114300950908     C     S4H(Y)        ANDNE     'N'
114400950908     C                   MOVEL     'S'           D59ANM
114500031009     C                   MOVEL     D4H(Y)        DESERR
114600950525     C*  Trovato errore bloccante !!
114700950525     C     T4H(Y)        IFEQ      'B'
114800950525     C                   MOVEL     'S'           WERRBL
114900030801     C*  Se in consegna su altra distinta
115000950529     C     C4H(Y)        IFEQ      '03'
115100950525     C                   MOVEL     'S'           WNODIS
115200950526     C                   SETON                                        03
115300950606     C                   MOVEL     *BLANKS       APDRSC
115400021204     C     kapd          CHAIN     fiapd01L                           37
115500031009     C                   MOVEL     APDRSC        WPADRO
115600950525     C                   END
115700950526     C                   END
115800950911     C*  Se ho sbloccato il C/Assegno senalo importo e divisa
115900950911     C     C4H(Y)        IFEQ      '07'
116000950911     C                   SETON                                        17
116100950911     C                   Z-ADD     0             AR9CAS
116200950911     C                   MOVEL     *BLANKS       AR9VCA
116300051115     C     KARB          CHAIN     FIAR901L                           37
116400950911     C                   END
116500030801     C*  Decodificare la consegna richiesta
116600950529     C     C4H(Y)        IFEQ      '10'
116700950526     C                   MOVEA     *BLANKS       RIC
116800950526     C                   Z-ADD     ARBDCR        G02INV
116900950526     C                   MOVEL     '3'           G02ERR
117000950526     C                   CALL      'XSRDA8'
117100950526     C                   PARM                    WLBDA8
117200950526     C                   MOVE      G02DAT        WDCR              8
117300950526     C                   MOVEA     WDCR          DCR
117400950526     C     ARBTCR        IFEQ      'D'
117500950526     C                   MOVEA     COST3         RIC
117600950526     C                   MOVEL     DCR(1)        RIC(9)
117700950526     C                   MOVEL     DCR(2)        RIC(10)
117800950526     C                   MOVEL     '/'           RIC(11)
117900950526     C                   MOVEL     DCR(3)        RIC(12)
118000950526     C                   MOVEL     DCR(4)        RIC(13)
118100950526     C                   MOVEL     '/'           RIC(14)
118200950526     C                   MOVEA     DCR(5)        RIC(15)
118300950526     C                   ELSE
118400950526     C                   MOVEA     COST4         RIC
118500950526     C                   MOVEL     DCR(1)        RIC(4)
118600950526     C                   MOVEL     DCR(2)        RIC(5)
118700950526     C                   MOVEL     '/'           RIC(6)
118800950526     C                   MOVEL     DCR(3)        RIC(7)
118900950526     C                   MOVEL     DCR(4)        RIC(8)
119000950526     C                   MOVEL     '/'           RIC(9)
119100950526     C                   MOVEA     DCR(5)        RIC(10)
119200950526     C                   END
119300950526     C                   MOVEA     RIC           WRIC             18
119400950526     C                   MOVE      WRIC          DESERR
119500950525     C                   END
119600950615     C*  Se non sono riuscita ad andare in consegna con l'ultima figlia
119700950615     C*  segnalo l'errore.
119800950615     C     C4H(Y)        IFEQ      '14'
119900950615     C     KLBL          SETGT     FNLBL02L
120000950615     C     KLBL          READPE    FNLBL02L                               32
120100950615     C                   SETON                                        14
120200950615     C                   END
120300030725     c* stampa testata
120400110112     c                   if        d59sqa =  ' ' or
120500110112     c                             d59sqa =  'T'
120600030728     c                   exsr      states
120700101124     c* stampa
120800030728     C                   WRITE     ERRARB
120900101201     c                   end
121000101124     c* video
121100101201     c                   if        d59sqa =  'S' or
121200101201     c                             d59sqa =  'T'
121300101124     c                   EVAL      tbeke2 = 'H' + C4H(Y)
121400101124     c                   exsr      srsqa
121500101124     c                   end
121600950612     C                   ADD       1             D59LIN
121700110414     C                   CLEAR                   TIPERR
121800960319     C                   Z-ADD     0             WAAS
121900960319     C                   Z-ADD     0             WLNP
122000960319     C                   Z-ADD     0             WNRS
122100960319     C                   Z-ADD     0             WNSP
122200030728     C                   Z-ADD     0             Wlna
122300030728     C                   Z-ADD     0             Wdat
122400030729     C                   Z-ADD     0             WNCD
122500030729     C                   Z-ADD     0             WNCA
122600030729     C                   Z-ADD     0             WZNC
122700030729     C                   MOVEL     *BLANKS       WRSD
122800030729     C                   MOVEL     *BLANKS       WBAR
122900950526     C                   END
123000950526     C*
123100950525     C*
123200950525     C                   ENDSR
123300950526     C*--------------------------------------------------------------*
123400030725     C*  STAMPA: Stampa testata
123500950526     C*--------------------------------------------------------------*
123600030728     C     STATES        BEGSR
123700110112     c                   if        d59sqa =  ' ' or
123800110112     c                             d59sqa =  'T'
123900030728     C* SE LA RIGA DI STAMPA È MAGGIORE DI SEI VUOL DIRE CHE HO GIÀ STAMPATO
124000030728     C                   IF        D59LIN = 0 OR
124100030728     C                             D59LIN > 54
124200030728     C                   SETON                                        80
124300030729     c                   else
124400030729     c* stampo testatina spunte doppie a metà pagina
124500030729     c                   if        *in20 and *in21
124600030729     c                   write     testad
124700030729     c                   write     testad1
124800030729     C                   ADD       4             D59LIN
124900030729     c                   setoff                                       21
125000030728     C                   END
125100030729     C                   END
125200030725     C*
125300030728     C                   IF        *IN80
125400030725     C                   ADD       1             D59PAG
125500030728     C                   WRITE     TESTA
125600050502     c                   if        d59lsr = 'S' and d59pag = 1
125700050422     C                   write     testal
125800050426     c                   end
125900030729     C                   Z-ADD     6             D59LIN
126000030728     C                   SETOFF                                       80
126100030728     C* SPUNTE DOPPIE
126200030729     C                   IF        *IN20
126300040930     C                   WRITE     TESTAD2
126400040930     C                   WRITE     TESTAD1
126500030911     C                   Z-ADD     7             D59LIN
126600030911     C                   SETOFF                                       21
126700030729     C                   else
126800030729     C                   WRITE     TESTA1
126900030728     C                   END
127000030729     C                   END
127100110112     C                   END
127200030728     C*
127300030725     C                   ENDSR
127400030725     C*--------------------------------------------------------------*
127500030725     C*  STAMPA: Stampa dettaglio segnacolli
127600030725     C*--------------------------------------------------------------*
127700030725     C     STAMPA        BEGSR
127800101126     C*  Se non è stato stampato "ANOMALIE DA SPUNTE:" lo stampo
127900101201     C                   IF        werrpz = 'N' and
128000101201     c                             (d59sqa =  ' ' or
128100101201     c                             d59sqa =  'T')
128200950526     C                   MOVEL     '12'          WCDANO
128300950526     C                   MOVEL     'S'           WERRPZ
128400950526     C                   EXSR      GESANO
128500950526     C                   END
128600950908     C*----------------------------------------------------*
128700950908     C*  Controllo se devo stampare le anomalie
128800101126     C*    S4H(Y)        IFNE      'N'
128900950908     C*----------------------------------------------------*
129000030725     c*
129100030725     c                   exsr      states
129200040419     c* Imposto il numero della pistola nel campo di stampa
129300950530     C                   MOVEA     WDETER        DER
129400040409     C     '.xx '        SCAN      WDETER        XX                2 0    01
129500040409     c                   if        *in01
129600950530     C                   ADD       1             XX
129700950530     C                   MOVEA     WNRP          DER(XX)
129800950530     C                   MOVEA     DER           WDETER
129900950530     C                   END
130000040419     c* Imposto il numero del foglio nel campo di stampa
130100040419     C                   MOVEA     WDETER        DER
130200040419     C     'xxx-'        SCAN      WDETER        XX                2 0    01
130300040419     c                   if        *in01
130400040421     c                   movel(p)  sfgs          com04             4
130500040421     C                   MOVEA     com04         DER(XX)
130600040420     C                   ADD       4             XX
130700040419     c                   movel     snpg          com01             1
130800040419     C                   MOVEA     com01         DER(XX)
130900040420     C                   ADD       2             XX
131000040419     c                   movel     snfv          com05             5
131100040419     C                   MOVEA     com05         DER(XX)
131200040419     C                   MOVEA     DER           WDETER
131300040419     C                   END
131400110117     c* Imposto l'ora del foglio nel campo di stampa
131500040419     C                   MOVEA     WDETER        DER
131600040419     C     'xx:'         SCAN      WDETER        XX                2 0    01
131700040419     c                   if        *in01
131800040419     c                   movel     shms          com02             2
131900040419     C                   MOVEA     com02         DER(XX)
132000040420     C                   ADD       3             XX
132100070130     c                   move      shms          com04             4
132200070130     c                   movel     com04         com02
132300040419     C                   MOVEA     com02         DER(XX)
132400040419     C                   MOVEA     DER           WDETER
132500040419     C                   END
132600040419     c*
132700950612     C                   ADD       1             D59LIN
132800101125     c* stampa
132900101201     c                   if        (d59sqa =  ' ' or
133000101201     c                             d59sqa =  'T')
133100950919     C     WTRSPU        IFEQ      'S'
133200030728     C                   EVAL      WDETER1 = WDETER
133300030728     C                   EVAL      WTIPCA1 = WTIPCA
133400030728     C                   WRITE     ERRAR1
133500950919     C                   ELSE
133600030728     C                   WRITE     ERRART
133700101125     C                   END
133800101201     c                   end
133900101124     c* video
134000101201     c                   if        (d59sqa =  'S' or
134100101201     c                             d59sqa =  'T')
134200101124     c                   exsr      srsqa
134300101124     c                   end
134400950908     C*----------------------------------------------------*
134500101126     C*                  END
134600950908     C*----------------------------------------------------*
134700950526     C                   ENDSR
134800950526     C*--------------------------------------------------------------*
134900950526     C*  STPTES: Stampo dati testata
135000950526     C*--------------------------------------------------------------*
135100950526     C     STPTES        BEGSR
135200030728     C* AUTOTRASPORTATORE
135300031009     C                   Z-ADD     D59PDR        PRTPDR
135400031009     C                   MOVEL(P)  D59DSF        PRTDSF
135500050422     C                   MOVEL(P)  D59DSF        PRTDSF2
135600030728     C* DISTINTA
135700031009     C                   Z-ADD     D59NDT        PRTNDT
135800110209     C                   Z-ADD     D59NDT        PRTNDT3
135900110209     C*                  eval      PRTNDT2 =d59bcd
136000110209     C                   move      d59bcd        PRTNDT2
136100031009     C                   Z-ADD     D59FGS        PRTFGS
136200030728     C                   Z-ADD     D59DDT        G02INV
136300030728     C                   MOVEL     '3'           G02ERR
136400030728     C                   CALL      'XSRDA8'
136500030728     C                   PARM                    WLBDA8
136600031009     C                   Z-ADD     G02DAT        STCDDC
136700950526     C*
136800030723     C                   MOVEL     CMAT          TPPRE
136900030723     C     D59FPP        IFEQ      'P'
137000031009     C                   MOVEL     CPOM          TPPRE
137100030723     C                   END
137200030728     C*
137300950526     C*
137400950526     C                   ENDSR
137500101124     C*-----------------------------------------------------*
137600101124     C* memorizzo l'errore nel file di work                 *
137700101124     C*-----------------------------------------------------*
137800101124     C     srsqa         BEGSR
137900101124     c                   clear                   dsqa
138000101124     c                   eval      tbecod = 'SQA'
138100101124     c                   eval      tbeke1 = 'FNLRF7R2'
138200101124     c     ktbe          chain     tntbe01l
138300101124     c                   if        %found(tntbe01l)
138400101124     c                   movel     tbeuni        dsqa
138500101124     c                   if        D§SQAELA = 'S'
138600101124     c* TESTATA
138700101124     c                   clear                   wfnlrq1ds
138800101124     c                   eval      wlrq1ndc = d59ndt
138900101124     c                   eval      wlrq1fgs = d59fgs
139000101124     c                   eval      wlrq1pdr = d59pdr
139100101125     c                   eval      wlrq1aas = s_arbAAS
139200101125     c                   eval      wlrq1LNP = s_arbLNP
139300101125     c                   eval      wlrq1NRS = s_arbNRS
139400101125     c                   eval      wlrq1NSP = s_arbNSP
139500101124     c*
139600101124     c     kwf10         chain     wfnlrq11l
139700101124     c                   if        not %found(wfnlrq11l)
139800101124     c                   movel     tbeke2        wlrq1cer
139900101124     c                   eval      wlrq1sce = D§SQASCE
140000101126     c                   if        D§SQATPD = 'S'
140100101126     c                   eval      wlrq1err = D§SQADES
140200101126     c                   else
140300101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
140400101126     c                   eval      wlrq1err = deserr
140500101126     c                   else
140600101126     c                   eval      wlrq1err = wdeter
140700101126     c                   end
140800101126     c                   end
140900101124     c                   write     wfnlrq10
141000101124     c                   else
141100101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
141200101124     c                   clear                   wlrq1cer
141300101124     c                   clear                   wlrq1sce
141400101124     c                   update    wfnlrq10
141500101124     c                   end
141600101124     c* dettaglio
141700101124     c                   clear                   wfnlrq2ds
141800101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
141900101124     c                   eval      wlrq2aas = wlrq1aas
142000101124     c                   eval      wlrq2lnp = wlrq1lnp
142100101124     c                   eval      wlrq2nrs = wlrq1nrs
142200101124     c                   eval      wlrq2nsp = wlrq1nsp
142300101124     c                   eval      wlrq2ndc = wlrq1ndc
142400101124     c                   eval      wlrq2fgs = wlrq1fgs
142500101124     c                   eval      wlrq2pdr = wlrq1pdr
142600101124     c                   movel     tbeke2        wlrq2cer
142700101124     c                   eval      wlrq2sce = D§SQASCE
142800101126     c                   if        D§SQATPD = 'S'
142900101126     c                   eval      wlrq2err = D§SQADES
143000101126     c                   else
143100101126     c                   if        %subst(tbeke2: 1: 1) = 'H'
143200101126     c                   eval      wlrq2err = deserr
143300101126     c                   else
143400101126     c                   eval      wlrq2err = wdeter
143500101126     c                   end
143600101126     c                   end
143700101125     c* gli errori nella tabella 4H sono a livello di bolla quindi
143800101126     c* non ho le informazioni del segnacollo
143900101125     c                   if        %subst(tbeke2: 1: 1) <> 'H'
144000101124     c                   eval      wlrq2nsc = artnsc
144100110104     c                   eval      wlrq2fls = artfls
144200110126     c                   eval      wlrq2nrc = artnrs
144300110131     c                   if        tbeke2 <> '010'
144400110104     c                   eval      wlrq2flv = sfgs
144500101125     c                   eval      wlrq2nfv = snfv
144600110119     c                   eval      wlrq2dfv = sdfv
144700101125     c                   eval      wlrq2npg = snpg
144800101126     c                   eval      wlrq2hms = shms
144900101126     c                   if        wnrp <> ' '
145000101126     c                   movel     wnrp          wlrq2nps
145100101126     c                   end
145200110131     c                   end
145300110207     c*                  else
145400110207     c*                  eval      wlrq2flv = s_arbifp
145500110207     c*                  eval      wlrq2nfv = s_arbndc
145600110207     c*                  eval      wlrq2dfv = s_arbddc
145700110207     c*                  if        wlrq2nfv <> 0
145800110207     c*                  eval      wlrq2npg = 4
145900110207     c*                  end
146000101126     c                   end
146100101125     c*
146200101124     c                   write     wfnlrq20
146300101124     c                   end
146400101124     c                   end
146500101124     C                   ENDSR
146600950526     C*--------------------------------------------------------------*
146700950526     C*  DLTARZ: Cancello archivio dett.segnacolli/stop
146800950526     C*--------------------------------------------------------------*
146900950526     C     DLTARZ        BEGSR
147000950526     C*
147100950526     C     *LOVAL        SETLL     FNARBZ0F
147200950526     C                   READ      FNARBZ0F                               31
147300950526     C     *IN31         DOWEQ     '0'
147400950526     C                   DELETE    FNARBZ0F
147500950526     C                   READ      FNARBZ0F                               31
147600950526     C                   END
147700950526     C*
147800950526     C                   ENDSR
147900950614     C*--------------------------------------------------------------*
148000950614     C*  WRTANM: Scrivo anomalia collo fuori posto
148100950614     C*--------------------------------------------------------------*
148200950915     C     WRTANM        BEGSR
148300950915     C*
148400950919     C     ARTDCM        IFEQ      0
148500950915     C                   MOVEL     'N'           WAGANO            1
148600950918     C                   Z-ADD     WCAA          KCAA
148700990519     C                   CLEAR                   FNANM000
148800990519     C     KANM          CHAIN     FNANM02L                           34
148900950915     C     *IN34         DOWEQ     '0'
149000990519     C     ANMDAO        IFEQ      D59DDT
149100031009     C                   MOVEL     'S'           WAGANO
149200950915     C     ANMDCH        IFNE      0
149300960507     C                   MOVEL     WFASE         ANMFSA
149400960507     C                   Z-ADD     WFGS          ANMCDU
149500960507     C                   Z-ADD     WNFV          ANMNFV
149600950918     C                   Z-ADD     WNPS          ANMNPS
149700950915     C                   Z-ADD     0             ANMDCH
149800950915     C                   MOVEL     *BLANKS       ANMFSC
149900950915     C                   MOVEL     *BLANKS       ANMCCH
150000950915     C                   EXCEPT    AGGANM
150100960429     C                   ELSE
150200030721     C                   unlock    fnanm02l
150300950915     C                   END
150400950915     C                   END
150500990519     C     KANM          READE     FNANM02L                               34
150600950915     C                   END
150700950915     C*
150800950915     C     WAGANO        IFEQ      'N'
150900990519     C                   CLEAR                   FNANM000
151000960507     C                   MOVEL     WFASE         ANMFSA
151100960507     C                   Z-ADD     WFGS          ANMCDU
151200950915     C                   Z-ADD     ARTFLS        ANMFLS
151300950915     C                   Z-ADD     ARTNRS        ANMNRS
151400950915     C                   Z-ADD     ARTLNA        ANMLNA
151500950915     C                   Z-ADD     ARTNSC        ANMSCN
151600950915     C                   Z-ADD     ARBAAS        ANMAAS
151700950915     C                   Z-ADD     ARBLNP        ANMLNP
151800950915     C                   Z-ADD     ARBNSP        ANMNSP
151900950918     C                   Z-ADD     WCAA          ANMCAA
152000950918     C                   Z-ADD     WNFV          ANMNFV
152100950918     C                   Z-ADD     WNPS          ANMNPS
152200950915     C                   Z-ADD     D59DDT        ANMDAO
152300950915     C                   Z-ADD     SIMFEL        ANMFLE
152400950915     C                   MOVEL     'F'           ANMTAN
152500950915     C                   MOVEL     'S'           ANMSAN
152600950915     C                   MOVEL     'I'           ANMAIE
152700040721     c                   eval      anmft4 = §AR5BVA
152800040721     c                   movel     woggi         com6              6
152900040721     c                   clear                   comsei            6
153000040721     c                   eval      comsei = %subst(com6: 5: 2)+
153100040721     c                             %subst(com6: 3: 2)+
153200040721     c                             %subst(com6: 1: 2)
153300040721     c                   movel     comsei        §ANMDAC
153400040721     c                   movel     times         §ANMORC
153500040721     c                   movel     danmflo       anmflo
153600040721     c                   eval      anmfl4 = arbznc
153700040721     c                   if        arbznc=0
153800040721     c                   eval      anmfl4 = 999
153900040721     c                   end
154000990519     C                   WRITE     FNANM000
154100950915     C                   END
154200950919     C*
154300950919     C                   END                                                    ARTDCM = 0
154400950614     C*
154500950915     C                   ENDSR
154600980310     C*--------------------------------------------------------------*
154700980310     C*  WRTNIC: Leggo NIC provvisori e li scrivo in definitivo
154800980310     C*--------------------------------------------------------------*
154900980310     C     WRTNIC        BEGSR
155000980310     C*
155100980310     C* Controllo quali sono gli eventi di esclusi dalla consegna che
155200980310     C* non sono stati cancellati e li riporto in definitivo
155300130326     C     *LOVAL        SETLL     WFNEVB01L
155400130326     C                   READ      WfNEVB01L                              36
155500980310     C     *IN36         DOWEQ     '0'
155600031013     C                   MOVEL     DSEVB         DSEVBS
155700980310     C* Aggancio file eventi per controllare se l'evento di messa in
155800980310     C* consegna sulla bolla era già stato trasmesso
155900990803     C                   Z-ADD     EVBAAS        KAAS
156000980310     C                   Z-ADD     EVBLNP        KLNP
156100980310     C                   Z-ADD     EVBNRS        KNRS
156200980310     C                   Z-ADD     EVBNSP        KNSP
156300031020     C* riaggancio la spedizione dell'evento NIC rimasto
156400031020     C     KARB          CHAIN     FNARB01L                           33
156500100514     c                   clear                   ds7r
156600100514     c                   if        arbgma <> ' '
156700100514     c                   exsr      sr7r
156800100514     c                   end
156900031020     c* se spedizione con ritiro contestuale alla consegna
157000100514     c                   if        §7RRC= 'S' and not *in33
157100031020     c                   clear                   fnlr99ds
157200031020     c                   eval      i99fao = 390
157300031020     c                   exsr      AGGORM
157400031020     c                   end
157500031020     c* proseguo con la scrittura del NIC/cancellazione del MIC
157600980310     C                   MOVEL     'MIC'         KCEV
157700990803     C     KEVB2         SETGT     FNEVB04L
157800990803     C     KEVB2         READPE    FNEVB04L                               37
157900131218     C*--
158000980310     C     *IN37         IFEQ      '0'
158100990803     C     EVBDEV        ANDEQ     D59DDT
158200131218     C*--
158300980310     C* se l'evento MIC non era stato trasmesso lo annullo fisicamente
158400980310     C     EVBDTR        IFEQ      0
158500131218     C*
158600131218     C* riaggancia il record  FISICO x aggiornarlo
158700131218     c     evb_nrr       delete    fnevb00F
158800131218     C*--
158900131218     C**************     DELETE    FNEVB04L
159000131218     C*
159100980310     C                   ELSE
159200131218     C*
159300031029     C                   MOVEL     EVBDEV        WDTMIC           12 0
159400031029     C                   MOVE      EVBHEV        WDTMIC
159500980312     C* altrimenri controllo se esiste già un evento NIC con data = a
159600980312     C* quella del foglio di consegna
159700990803     C                   Z-ADD     EVBAAS        KAAS
159800980312     C                   Z-ADD     EVBLNP        KLNP
159900980312     C                   Z-ADD     EVBNRS        KNRS
160000980312     C                   Z-ADD     EVBNSP        KNSP
160100980312     C                   MOVEL     'NIC'         KCEV
160200990803     C     KEVB2         SETGT     FNEVB04L
160300990803     C     KEVB2         READPE    FNEVB04L                               37
160400131218      *--
160500980312     C     *IN37         IFEQ      '0'
160600990803     C     EVBDEV        ANDEQ     D59DDT
160700131218      *--
160800980312     C* Se c'è evento NIC controllo se già trasmesso
160900980312     C     EVBDTR        IFEQ      0
161000131218     C*
161100131218     C* riaggancia il record  FISICO x aggiornarlo
161200131218     c     evb_nrr       chain     fnevb00F
161300131218      *--
161400131218     c                   if        %Found(fnevb00F)
161500980312     C* Se non è ancora stato trasmesso aggiorno data/ora immissione
161600980312     C                   Z-ADD     D59AMG        EVBDTV
161700980312     C                   TIME                    EVBORV
161800980312     C                   Z-ADD     D59FGS        EVBFLE
161900980312     C                   MOVE      D59CDU        EVBCDU
162000980312     C     FVVFPP        IFEQ      'M'
162100980312     C                   MOVEL     0800          EVBHEV
162200980312     C                   ELSE
162300980312     C                   MOVEL     1200          EVBHEV
162400980312     C                   END
162500990803     C                   Z-ADD     0             EVBDTC
162600131218      *--
162700990803     C                   UPDATE    FNEVB000
162800131218     c                   end
162900131218      *--
163000980312     C                   ELSE
163100131218      *--
163200980312     C* Se è già stato trasmesso controllo se data/ora immissione
163300980312     C* > MIC
163400031029     C                   MOVEL     EVBDEV        WDTNIC           12 0
163500031029     C                   MOVE      EVBHEV        WDTNIC
163600031029     C     WDTNIC        IFLE      WDTMIC
163700980312     C* Se la data non è più alta scrivo un nuovo NIC
163800031013     C                   MOVEL     DSEVBS        DSEVB
163900990803     C                   WRITE     FNEVB000
164000980312     C                   END                                                    Data MIC >= NIC
164100980312     C*
164200980312     C                   END                                                    NIC Trasmesso
164300980312     C*
164400980312     C                   ELSE                                                   Nic Trovato
164500031013     C                   MOVEL     DSEVBS        DSEVB
164600990803     C                   WRITE     FNEVB000
164700980312     C                   END                                                    Nic Trovato
164800980312     C                   END                                                    Mic trasmesso
164900980312     C                   END                                                    Mic Trovato
165000980310     C* Cancello evento provvisiorio
165100990803     C                   DELETE    FNEVBPRV
165200980310     C* leggo evento provvisiorio successivo
165300130326     C                   READ      WFNEVB01L                              36
165400980310     C                   END
165500980310     C*
165600980310     C                   ENDSR
165700030805     C**********************************************************************
165800030805     C* calcolo il numero dei bancali   -----------------------------*
165900030805     C**********************************************************************
166000030805     C     srnban        BEGSR
166100030805      * Bancali
166200030805     c                   If        %Subst(Arbgva:1:1) = 'B'
166300030805     c                   Clear                   dAr5Ban
166400030805     C                   move      'BAN'         ar5trd
166500030805     c     kFiar5        Chain     Fiar501l
166600030805     c                   If        %Found(Fiar501l)
166700030805     c                   Movel     Ar5Uni        dAr5Ban
166800030805     c                   EndIf
166900030805     c                   add       §Ar5Nba       nban
167000030805     c                   add       §ar5Nb2       nban
167100030805     c                   EndIf
167200030805      * Bancali Bartolini
167300030805     c                   If        %Subst(Arbgva:1:1) = 'O'
167400030805     c                   Clear                   dAr5BNB
167500030805     C                   move      'BNB'         ar5trd
167600030805     c     kFiar5        Chain     Fiar501l
167700030805     c                   If        %Found(Fiar501l)
167800030805     c                   Movel     Ar5Uni        dAr5BNB
167900030805     c                   EndIf
168000030805     c                   add       §Ar5bNba      nban
168100030805     c                   add       §ar5bNb2      nban
168200030805     c                   EndIf
168300040721      * Merce di valore
168400040721     c                   Clear                   dAr5gen
168500040721     C                   move      'GEN'         ar5trd
168600040721     c     kFiar5        Chain     Fiar501l
168700040721     c                   If        %Found(Fiar501l)
168800040721     c                   Movel     Ar5Uni        dAr5gen
168900040721     c                   EndIf
169000030805     c                   ENDSR
169100010620     C*--------------------------------------------------------------*
169200010620     C*  DOPPIE: STAMPO EVENTUALI SPUNTE DI CONSEGNA DOPPIE
169300010620     C*--------------------------------------------------------------*
169400010620     C     DOPPIE        BEGSR
169500010620     C*
169600010620     C                   Z-ADD     D59NDT        KNFV
169700010620     C                   Z-ADD     4             KNPG
169800030729     C                   SETON                                        2021
169900010620     C*
170000070129     C     KBRVE         SETLL     FNBRVE1L
170100070129     C     KBRVE         READE     FNBRVE1L                               30
170200010620    2C     *IN30         DOWEQ     *OFF
170300010620     C                   CLEAR                   WGIR
170400010620     C                   CLEAR                   SAWNPS
170500010622     C                   CLEAR                   SAWNFV
170600010622     C                   CLEAR                   SAWFGS
170700130327     C                   SETOFF                                       19
170800070129     C     KBRVE2        SETLL     FNBRVE2L
170900070129     C     KBRVE2        READE     FNBRVE2L                               31
171000010620    3C     *IN31         DOWEQ     *OFF
171100010620     C* Considero solo se data foglio >= data distinta in quadratura
171200010702    4C     D59FGS        IFNE      BREFGS
171300010702     C     KNFV          ORNE      BRENFV
171400010620     C                   CLEAR                   DSLV53
171500010620     C                   MOVEL     'V'           D53TFO
171600010620     C                   MOVEL     KNPG          D53NPG
171700010620     C                   Z-ADD     SIMFEL        D53FEL
171800010702     C                   Z-ADD     BRENFV        D53NFV
171900010702     C                   Z-ADD     BREFGS        D53FGS
172000010620     C                   CALL      'FNLV53R'
172100010620     C                   PARM                    DSLV53
172200010702     C                   Z-ADD     D53DFV        SA1DFV
172300010620     C                   ELSE
172400010702     C                   Z-ADD     D59DDT        SA1DFV
172500010620    4C                   ENDIF
172600010702    4C     D59FGS        IFEQ      BREFGS
172700010702     C     KNFV          ANDEQ     BRENFV
172800010620     C     D53ERR        OREQ      *BLANKS
172900010702     C     SA1DFV        ANDGE     D59DDT
173000010703    5C     SAWNFV        IFGT      *ZEROS
173100010622     C* Stessa pistola e foglio: devo verificare se superati i minuti
173200010622     c* di tolleranza previsti fra una spunta e l'altra
173300010620     C                   CLEAR                   WPRT
173400010702    6C     SAWNPS        IFEQ      BRENPS
173500010702     C     SAWNFV        ANDEQ     BRENFV
173600010702     C     SAWFGS        ANDEQ     BREFGS
173700010620     C                   CLEAR                   WXDTOR
173800010702     C                   MOVEL     BREDFS        WXDT1
173900010702     C                   MOVEL     BREHMS        WXHM1
174000010620     C                   MOVEL     SAWDFS        WXDT2
174100010620     C                   MOVEL     SAWHMS        WXHM2
174200010620     C                   MOVEL     'D'           WXFLG
174300010620     C                   CALL      'XDTORA'
174400010620     C                   PARM                    WXDTOR
174500010620     C     WXMIN         IFGT      §7AMNP
174600010620     C                   MOVEL     '1'           WPRT
174700010620     C                   ENDIF
174800010620   x6C                   ELSE
174900010622     c* se pistole o foglio diversi stampo sempre
175000010620     C                   MOVEL     '1'           WPRT              1
175100010620    6C                   ENDIF
175200010620    6C     WPRT          IFEQ      '1'
175300010620     c* se sono al primo giro di stampa devo stampare sia la spunta pre
175400010620     c* cedente che la spunta successiva
175500010620    7C     WGIR          IFEQ      ' '
175600010620     C                   MOVEL     DSBRVE        W1BRVE
175700010620     C                   MOVEL     WSBRVE        DSBRVE
175800010620     C                   Z-ADD     SAWDFV        WDFV
175900030729     C                   SETOff                                       19
176000010620     C                   EXSR      SPRDOP
176100010620     C                   MOVEL     W1BRVE        DSBRVE
176200010620     C                   MOVE      '1'           WGIR              1
176300010620    7C                   ENDIF
176400010702     C                   Z-ADD     SA1DFV        WDFV
176500030729     C                   SETON                                        19
176600010620     C                   EXSR      SPRDOP
176700010620   X6C                   ELSE
176800010620     C                   MOVEL     DSBRVE        WSBRVE
176900010620    6C                   ENDIF
177000010620   X5C                   ELSE
177100010620     C* se sono al primo giro di lettura memorizzo la spunta
177200010620     C                   MOVEL     DSBRVE        WSBRVE
177300010620    5C                   ENDIF
177400010620     C* memorizzo in ogni caso il numero pistola e data/ora fisica spun
177500010702     C                   Z-ADD     BRENPS        SAWNPS
177600010702     C                   Z-ADD     BREFGS        SAWFGS
177700010702     C                   Z-ADD     BRENFV        SAWNFV
177800010702     C                   Z-ADD     BREDFS        SAWDFS
177900010702     C                   Z-ADD     BREHMS        SAWHMS
178000010702     C                   Z-ADD     SA1DFV        SAWDFV
178100010620    4C                   ENDIF
178200070129     C     KBRVE2        READE     FNBRVE2L                               31
178300010620    3C                   ENDDO
178400010620     C* Mi posiziono sempre su un nuovo segnacollo
178500070129     C     KBRVE1        SETGT     FNBRVE1L
178600070129     C     KBRVE         READE     FNBRVE1L                               30
178700010620    2C                   ENDDO
178800010711     C*
178900010620     C                   ENDSR
179000010620     C*
179100010620     C*--------------------------------------------------------------*
179200010620     C* STAMPA SPUNTA DOPPIA
179300010620     C*--------------------------------------------------------------*
179400010620     C     SPRDOP        BEGSR
179500010712     C                   MOVEL     'S'           D59ANM
179600030728     C                   EXSR      STATES
179700010711     C*
179800010702     C                   CLEAR                   P1CMSG
179900030729     C* SEGNACOLLO SOLO PER 19 Off
180000030729     C     *IN19         IFEQ      *Off
180100010702     C                   Z-ADD     BRELNP        P1CLNP
180200010702     C                   Z-ADD     BRELNA        P1CLNA
180300010702     C                   Z-ADD     BRENRS        P1CNRS
180400010702     C                   Z-ADD     BRENSC        P1CNSC
180500010702     C* VEDO SE COLLO IN IMA
180600010702     C                   Z-ADD     3             KNPG
180700070129     C     KBRV9         SETGT     FNBRV09L
180800070129     C     KBRV9         READPE    FNBRV09L                               32
180900010702     C     *IN32         DOWEQ     *OFF
181000010702     C     P1CMSG        ANDEQ     *BLANKS
181100010702     C* IL foglio deve avere sottocategoria arrivi ed avere data foglio
181200010702     C* maggiore uguale alla data del foglio in fase di quadratura
181300010702     C                   CLEAR                   DSLV53
181400010702     C                   MOVEL     'V'           D53TFO
181500010702     C                   MOVEL     KNPG          D53NPG
181600010702     C                   Z-ADD     SIMFEL        D53FEL
181700010702     C                   Z-ADD     BRVNFV        D53NFV
181800010702     C                   Z-ADD     BRVFGS        D53FGS
181900010702     C                   CALL      'FNLV53R'
182000010702     C                   PARM                    DSLV53
182100010702     C     D53ERR        IFEQ      *BLANKS
182200010702     C     D53DFV        ANDGE     D59DDT
182300010702     C     D53SPG        ANDEQ     'A'
182400010702     C                   MOVEA     COS           W33
182500010702     C                   MOVE      BRVFGS        W003A             3
182600010702     C                   MOVEA     W003A         W33(8)
182700010702     C                   MOVE      BRVNFV        W006A             6
182800010712     C                   EXSR      EDITA
182900010702     C                   MOVEA     W006A         W33(12)
183000010702     C                   MOVE      D53DFV        W0040             4 0
183100010702     C                   MOVEL     W0040         WMMDFV            2
183200010702     C                   MOVE      W0040         WGGDFV            2
183300010702     C                   MOVEA     WGGDFV        W33(23)
183400010702     C                   MOVEA     WMMDFV        W33(26)
183500010702     C                   MOVE      BRVNPS        W002A             2
183600010702     C                   MOVEA     W002A         W33(32)
183700031009     C                   MOVEA     W33           P1CMSG
183800010702     C                   ENDIF
183900070129     C     KBRV9         READPE    FNBRV09L                               32
184000010702     C                   ENDDO
184100010620     C                   ENDIF
184200110518     C* Reimposto KNPG in quanto usato nel ciclo di lettura di fnbrve
184300110518     C                   Z-ADD     4             KNPG
184400010620     C*
184500010702     C                   Z-ADD     BREFGS        P1CFGS
184600010702     C                   Z-ADD     BRENFV        P1CNFV
184700010620     C* DATA DISTINTA
184800010620     C                   Z-ADD     WDFV          G02INV
184900010620     C                   MOVE      *ZEROS        G02DAT
185000010620     C                   MOVEL     3             G02ERR
185100010620     C                   CALL      'XSRDA8'
185200010620     C                   PARM                    WLBDA8
185300010620     C                   Z-ADD     G02DAT        P1CDFV
185400010620     C* AUTOTRASPORTATORE
185500110518     C     KDST          CHAIN     FiDST01L                           32
185600010620    1C     *IN32         IFEQ      *OFF
185700010711     C                   Z-ADD     DSTPDR        P1CPDR
185800021204     C     kapdd         CHAIN     fiapd01L                           32
185900010620    2C     *IN32         IFEQ      *OFF
186000031009     C                   MOVEL     APDRSC        P1DPDR
186100010620   X2C                   ELSE
186200010620     C                   CLEAR                   P1DPDR
186300010620    2C                   ENDIF
186400010620   X1C                   ELSE
186500010620     C                   CLEAR                   P1CPDR
186600010620     C                   ENDIF
186700010620     C* DATA IMMISSIONE
186800010702     C                   Z-ADD     BREDFS        G02INV
186900010620     C                   MOVE      *ZEROS        G02DAT
187000010620     C                   MOVEL     3             G02ERR
187100010620     C                   CALL      'XSRDA8'
187200010620     C                   PARM                    WLBDA8
187300010620     C                   Z-ADD     G02DAT        P1CDFS
187400010620     C*
187500010702     C                   Z-ADD     BREHMS        P1CHMS
187600010702     C                   Z-ADD     BRENPS        P1CNPS
187700030728     C                   WRITE     ERRDOP
187800010711     C                   ADD       1             D59LIN
187900010620     C                   ENDSR
188000010712     C*--------------------------------------------------------------*
188100010712     C* SOPPRESSIONE ZERI NON SIGNIFICATIVI DA CAMPO LUNGO 6
188200010712     C*--------------------------------------------------------------*
188300010712     C     EDITA         BEGSR
188400010712     C*
188500010712     C                   MOVE      *BLANKS       WFINE             1
188600010712     C                   MOVEA     W006A         W06
188700010712     C                   Z-ADD     1             II                1 0
188800010712     C     WFINE         DOWEQ     *BLANKS
188900010712     C     II            ANDLE     5
189000010712     C     W06(II)       IFEQ      '0'
189100010712     C                   MOVE      *BLANKS       W06(II)
189200010712     C                   ELSE
189300010712     C                   MOVE      '1'           WFINE
189400010712     C                   END
189500010712     C                   ADD       1             II
189600010712     C                   ENDDO
189700010712     C                   MOVEA     W06           W006A
189800010712     C                   ENDSR
189900950526     C*--------------------------------------------------------------*
190000950526     C*  *INZSR: Operazioni iniziali
190100950526     C*--------------------------------------------------------------*
190200950526     C     *INZSR        BEGSR
190300950526     C*------------------------------------------------------*
190400030722     c     *dtaara       define    §azute        azuteds
190500030722     c     *dtaara       define    §datiute      ddatiute
190600030722     C                   in(E)     *dtaara
190700030722     C                   IF        %Error  or  RSUT = *blanks
190800030722     C                   call      'TIBS34R'
190900030722     C                   parm                    Tibs34Ds
191000030722     C                   in        *dtaara
191100030722     c                   ENDIF
191200950612     C                   MOVE      SIMFEL        KFLE
191300950526     C*------------------------------------------------------*
191400950526     C*  Definisco chiavi di accesso
191500110518     C     KDST          KLIST
191600110518     C                   KFLD                    knpg
191700110518     C                   KFLD                    brenfv
191800110518     C                   KFLD                    BREFGS
191900030805     C     Kfiar5        KLIST
192000030805     C                   KFLD                    arbaas
192100030805     C                   KFLD                    arblnp
192200030805     C                   KFLD                    arbnrs
192300030805     C                   KFLD                    arbnsp
192400030805     C                   KFLD                    ar5trd
192500950526     C     KARB          KLIST
192600950526     C                   KFLD                    KAAS
192700950526     C                   KFLD                    KLNP
192800950526     C                   KFLD                    KNRS
192900950526     C                   KFLD                    KNSP
193000950606     C     KLBL          KLIST
193100950606     C                   KFLD                    KAAS
193200950606     C                   KFLD                    KLNP
193300950606     C                   KFLD                    KNRS
193400950606     C                   KFLD                    KNSP
193500950526     C     KART          KLIST
193600950526     C                   KFLD                    KAAS
193700950526     C                   KFLD                    KLNP
193800950526     C                   KFLD                    KNRS
193900950526     C                   KFLD                    KNSP
194000950605     C     KFVV          KLIST
194100950605     C                   KFLD                    KNPG
194200950605     C                   KFLD                    KNFV1
194300080229     C                   KFLD                    brvfgs
194400950526     C     KBRV1         KLIST
194500950526     C                   KFLD                    KNPG
194600950526     C                   KFLD                    KNFV
194700950526     C                   KFLD                    KFGS
194800950526     C                   KFLD                    KFLS
194900950526     C                   KFLD                    KLNA
195000950526     C                   KFLD                    KNRS
195100950526     C                   KFLD                    KNSC
195200950526     C     KBRV2         KLIST
195300950526     C                   KFLD                    KNPG
195400950526     C                   KFLD                    KFLS
195500950526     C                   KFLD                    KLNA
195600950526     C                   KFLD                    KNRS
195700950526     C                   KFLD                    KNSC
195800040420     C                   KFLD                    KFLE
195900010702     C     KBRV9         KLIST
196000010702     C                   KFLD                    KNPG
196100010702     C                   KFLD                    BRELNP
196200010702     C                   KFLD                    BRELNA
196300010702     C                   KFLD                    BRENRS
196400010702     C                   KFLD                    BRENSC
196500010702     C                   KFLD                    KFLE
196600990204     C     KDCD          KLIST
196700990204     C                   KFLD                    KFLS
196800990204     C                   KFLD                    KLNA
196900990204     C                   KFLD                    KNRS
197000990204     C                   KFLD                    KNSC
197100990204     C     KDCT          KLIST
197200990204     C                   KFLD                    KAAC
197300990204     C                   KFLD                    KFIL
197400990204     C                   KFLD                    KNCA
197500950526     C     KTAB          KLIST
197600950526     C                   KFLD                    KKUT
197700950526     C                   KFLD                    KCOD
197800010620     C     KTAB1         KLIST
197900010620     C                   KFLD                    KKUT
198000010620     C                   KFLD                    KCOD
198100010620     C                   KFLD                    KKEY
198200950915     C     KANM          KLIST
198300950915     C                   KFLD                    KFLS
198400950915     C                   KFLD                    KLNA
198500950915     C                   KFLD                    KNRS
198600950915     C                   KFLD                    KNSC
198700950915     C                   KFLD                    KCAA
198800980310     C     KEVB1         KLIST
198900990803     C                   KFLD                    KAAS
199000980310     C                   KFLD                    KLNP
199100980310     C                   KFLD                    KNRS
199200980310     C                   KFLD                    KNSP
199300980310     C     KEVB2         KLIST
199400990803     C                   KFLD                    KAAS
199500980310     C                   KFLD                    KLNP
199600980310     C                   KFLD                    KNRS
199700980310     C                   KFLD                    KNSP
199800980310     C                   KFLD                    KCEV
199900010620     C     KBRVE         KLIST
200000010620     C                   KFLD                    KNPG
200100010620     C                   KFLD                    KNFV
200200010620     C                   KFLD                    D59FGS
200300010620     C     KBRVE1        KLIST
200400010620     C                   KFLD                    KNPG
200500010620     C                   KFLD                    KNFV
200600010620     C                   KFLD                    D59FGS
200700010702     C                   KFLD                    BRELNP
200800010702     C                   KFLD                    BRELNA
200900010702     C                   KFLD                    BRENRS
201000010702     C                   KFLD                    BRENSC
201100010620     C     KBRVE2        KLIST
201200010702     C                   KFLD                    BRENPG
201300010702     C                   KFLD                    BRELNP
201400010702     C                   KFLD                    BRELNA
201500010702     C                   KFLD                    BRENRS
201600010702     C                   KFLD                    BRENSC
201700021204     C*
201800021204     C     Kapd          KLIST
201900021204     C                   KFLD                    apdtip
202000021204     C                   KFLD                    arbpdc
202100021204     C*
202200021204     C     Kapd59        KLIST
202300021204     C                   KFLD                    apdtip
202400021204     C                   KFLD                    d59pdr
202500021204     C*
202600021204     C     Kapdd         KLIST
202700021204     C                   KFLD                    apdtip
202800021204     C                   KFLD                    dstpdr
202900021204     c                   movel     'A'           apdtip
203000101124     C     Ktbe          KLIST
203100101124     C                   KFLD                    tbecod
203200101124     C                   KFLD                    tbeke1
203300101124     C                   KFLD                    tbeke2
203400101124     C     Kwf10         KLIST
203500101124     C                   KFLD                    WLRQ1AAS
203600101124     C                   KFLD                    WLRQ1LNP
203700101124     C                   KFLD                    WLRQ1NRS
203800101124     C                   KFLD                    WLRQ1NSP
203900101124     C                   KFLD                    WLRQ1FGS
204000101124     C                   KFLD                    WLRQ1NDC
204100101124     C                   KFLD                    WLRQ1PDR
204200950526     C*------------------------------------------------------*
204300950526     C*  Definisco variabili
204400030728     C     *LIKE         DEFINE    BRVNPS        BRVNPS1
204500030728     C     *LIKE         DEFINE    D59NDT        EXNDT
204600950526     C     *LIKE         DEFINE    ARTAAS        KAAS
204700950526     C     *LIKE         DEFINE    ARTLNP        KLNP
204800950526     C     *LIKE         DEFINE    ARTNRS        KNRS
204900950526     C     *LIKE         DEFINE    ARTNSP        KNSP
205000950526     C     *LIKE         DEFINE    ARTFLS        KFLS
205100950526     C     *LIKE         DEFINE    ARTLNA        KLNA
205200950526     C     *LIKE         DEFINE    ARTNSC        KNSC
205300950526     C     *LIKE         DEFINE    BRVNPG        KNPG
205400950526     C     *LIKE         DEFINE    BRVNFV        KNFV
205500950526     C     *LIKE         DEFINE    BRVFGS        KFGS
205600950612     C     *LIKE         DEFINE    BRVFLE        KFLE
205700951121     C     *LIKE         DEFINE    ARBRAG        KRAG
205800950526     C     *LIKE         DEFINE    TBLKUT        KKUT
205900950526     C     *LIKE         DEFINE    TBLCOD        KCOD
206000950915     C     *LIKE         DEFINE    ANMCAA        KCAA
206100980310     C     *LIKE         DEFINE    EVBCEV        KCEV
206200950918     C     *LIKE         DEFINE    ANMCAA        WCAA
206300950918     C     *LIKE         DEFINE    ANMNFV        WNFV
206400950918     C     *LIKE         DEFINE    ANMNPS        WNPS
206500960507     C     *LIKE         DEFINE    FVVFGS        WFGS
206600950605     C     *LIKE         DEFINE    FVVNFV        KNFV1
206700990204     C     *LIKE         DEFINE    TBECOD        KCODE
206800990204     C     *LIKE         DEFINE    DCTAAC        KAAC
206900990204     C     *LIKE         DEFINE    DCTFIL        KFIL
207000990204     C     *LIKE         DEFINE    DCTNCA        KNCA
207100010620     C     *LIKE         DEFINE    DSBRVE        WSBRVE
207200010620     C     *LIKE         DEFINE    DSBRVE        W1BRVE
207300010620     C     *LIKE         DEFINE    BRVNPS        SAWNPS
207400010622     C     *LIKE         DEFINE    BRVFGS        SAWFGS
207500010622     C     *LIKE         DEFINE    BRVNFV        SAWNFV
207600010620     C     *LIKE         DEFINE    BRVDFS        SAWDFS
207700010620     C     *LIKE         DEFINE    BRVHMS        SAWHMS
207800010620     C     *LIKE         DEFINE    D53DFV        SAWDFV
207900010620     C     *LIKE         DEFINE    D53DFV        WDFV
208000010702     C     *LIKE         DEFINE    D53DFV        SA1DFV
208100010620     C     *LIKE         DEFINE    TBLKEY        KKEY
208200950526     C*
208300950526     C                   TIME                    TIMES             6 0
208400950526     C                   TIME                    WHDAT            12 0
208500950526     C                   MOVE      WHDAT         WOGGI             6 0
208600010622     C                   TIME                    W0140            14 0
208700010622     C                   MOVE      W0140         WDTGIO            8 0
208800031009     C                   MOVEL     *ALL'-'       CMP132
208900031009     C                   MOVEL     *ALL'-'       CMP198
209000031009     C                   MOVEL     *ALL'-'       CMP132D
209100031009     C                   MOVEL     *ALL'-'       CMP198D
209200950526     C*
209300030722     C*  Carico dati tabella 4H anomalie da spunte
209400950526     C                   Z-ADD     1             KKUT
209500950526     C                   MOVEL     '4H'          KCOD
209600070601     C                   Z-ADD     0             X                 3 0
209700950526     C     KTAB          CHAIN     TABEL00F                           31
209800950526     C     *IN31         DOWEQ     '0'
209900950526     C     X             ANDLT     100
210000950526     C                   ADD       1             X
210100950526     C                   MOVEL     TBLUNI        DS4H
210200950526     C                   MOVEL     TBLKEY        C4H(X)
210300950526     C                   MOVEL     §4HDES        D4H(X)
210400950526     C                   MOVEL     §4HTPB        T4H(X)
210500950908     C                   MOVEL     §4HSTP        S4H(X)
210600950526     C     KTAB          READE     TABEL00F                               31
210700950526     C                   END
210800010620     C* Chaino tabella variabili pgm arrivi per reperire minuti di
210900010620     C* tolleranza
211000010620     C                   MOVEL     '7A'          KCOD
211100010620     C                   MOVEL(P)  '2       '    KKEY
211200010620     C     KTAB1         CHAIN     TABEL                              31
211300010620     C  N31              MOVEL     TBLUNI        DS7A2
211400010620     C   31              Z-ADD     1             §7AMNP
211500030728     C                   OPEN      FNLRF7P
211600030722     C*  Carico dati tabella DTA anomalie danni
211700990204     C                   Z-ADD     1             KKUT
211800990204     C                   MOVEL     'TAD'         KCODE
211900990204     C                   Z-ADD     0             X
212000990204     C     KCODE         CHAIN     TNTBE01L                           31
212100990204     C     *IN31         DOWEQ     '0'
212200990204     C     X             ANDLT     100
212300990204     C     TBESIF        IFEQ      KNSIF
212400990204     C     TBESIF        OREQ      *BLANKS
212500990204     C                   ADD       1             X
212600990204     C                   MOVEL     TBEKE1        TAD(X)
212700990204     C                   MOVEL     TBEUNI        DTA(X)
212800990204     C                   END
212900990204     C     KCODE         READE     TNTBE01L                               31
213000990204     C                   END
213100030703     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
213200040827     C                   CLEAR                   FIP
213300040827     C     *LOVAL        SETLL     AZORG01L
213400040827     C                   DO        *HIVAL
213500040827     C                   READ      AZORG01L                               95
213600040827     C   95              LEAVE
213700040827     C                   MOVEL     ORGDE3        OG143
213800040827     C     §OgNTW        IFEQ      'PPT'
213900040827     C                   Z-ADD     1             z                 3 0
214000040827     C     *ZEROS        LOOKUP    FIP(z)                                 23
214100040827     C   23              Z-ADD     ORGFIL        FIP(z)
214200040827     C                   END
214300040827     C                   END
214400950526     C*
214500950526     C                   ENDSR
214600950526     O*--------------------------------------------------------------*
214700950526     O*   STAMPA                                                     *
214800950526     O*--------------------------------------------------------------*
214900030728     OSYSPRT    E            SPED1       1
215000990803     O                       EVBAAS        Z      4
215100980325     O                       EVBLNP        Z     16
215200980325     O                       EVBNRS        Z     19
215300980325     O                       EVBNSP        Z     27
215400980325     O                                           80 'DELETE ERRATO'
215500950526     O*--------------------------------------------------------------*
215600950526     O*   AGGIORNAMENTI                                              *
215700950526     O*--------------------------------------------------------------*
215800951109     OFNARB000  E            AGGARB
215900950526     O                       ARBDDC
216000950526     O                       ARBNDC
216100020917     O                       ARBIFP
216200950526     O                       ARBPDC
216300950526     O                       ARBSTP
216400140410     O                       ARBNMI
216500140410     O                       ARBFDC
216600140410     O                       ARBNSS
216700990519     OFNANM000  E            AGGANM
216800950915     O                       ANMDCH
216900950915     O                       ANMFSC
217000950915     O                       ANMCCH
217100950918     O                       ANMNFV
217200950918     O                       ANMNPS
217300010702**
217400040409collo                    spuntato su distinta Nr.       pistola nr.xx
217500040421collo  NON + USATO       non spuntato in uscita ma arrivato pistola nr.xx
217600040421collo  NON + USATO       non spuntato in uscita non arrivato ma partito pistola nr.xx
217700040415VUOTO
217800040421collo  NON + USATO       non spuntato in uscita non arrivato e non partito
217900040419collo                    spuntato in uscita pistola nr.xx ore xx:xx ma già consegnato
218000040415collo                    spuntato in uscita pistola nr.xx ore xx:xx
218100040415                         e su foglio xxx-x-xxxxx pistola nr.xx ore xx:xx
218200040419collo                    non spuntato in uscita ma su f. xxx-x-xxxxx pist. nr.xx ore xx:xx
218300081013collo                    collo doppio
218400040421VUOTO
218500040419collo                    NON IN DISTINTA ma spuntato in uscita pistola nr.xx ore xx:xx
218600101124VUOTO
218700990204**
218800040421        Pratica C.A. aperta:  NON + USATO
218900040421        Pratica C.A. chiusa:  NON + USATO
219000010702**
219100010702In IMA XXX XXXXXX Del XX/XX PstXX
