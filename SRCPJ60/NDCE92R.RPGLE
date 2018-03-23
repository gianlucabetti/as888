000100981216     H*PARMS DFTACTGRP(*NO)                ACTGRP(QILE) OPTION(*NOXREF)
000200981216     H*PARMS BNDDIR(PJXBND PJCBND)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400951002
000500950906     FANCTL02L  IF   E           K DISK    USROPN
000600991103     FNDPEE01L  UF A E           K DISK    USROPN
000700951002     FNDEFA08L  IF   E           K DISK    USROPN
000800951002     FNDREG01L  IF   E           K DISK    USROPN
000900051214C1893FNdPas01L  IF   E           K DISK
001000051214  "  FNdPpa01L  IF   E           K DISK
001100051214C1893FNdPps01L  IF   E           K DISK
001200951002
001300000000     D KPJBA         E DS
001400950907     D ANG0E1DS      E DS                  INZ
001500950907     D NDCEEADS      E DS                  INZ
001600950906     D NDCE90DS      E DS                  INZ
001700950906     D NDCE92DS      E DS                  INZ
001800950906     D DVCOBADS      E DS                  INZ
001900950907     D XATBDS        E DS                  INZ
002000950907     D XSOC001DS     E DS                  INZ
002100951016     D    FgO1                        1    overlay(XscFgO)
002200950906     D TE              S              1    DIM(9)                               TIPI EFFETTI
002300950906     D SPA             S              8    DIM(9)                               SOTTOCONTO PORTAF
002400030319C1638D CPA             S              6    DIM(9)                               SOTTOCONTO PORTAF
002500950906     D SEB             S              8    DIM(9)                               CONTROPART. BOLLO
002600950906     D CTL             S              8    DIM(50)                              CARTELLI
002700950906     D ABI             S              5    DIM(50)                              A.B.I.
002800950929     D BANCA           S              4    DIM(50)                              A.B.I.
002900950906     D I               S              3  0
003000950906     D KPJBUS          S                   LIKE(KPJBU)
003100950906     D XPASOC          S              3
003200950906     D XPAPAR          S              8
003300950906     D XPAOUT          S           2000
003400950906     D XPAERR          S              1
003500950906     D XPARIC          S              1
003600950906     D udateiso        s               d   datfmt(*iso)
003700950906     D WRKILD          S                   LIKE(E92ILD)
003800950906     D WRKABI          S                   LIKE(EFAABI)
003900950906     D WRKCAB          S                   LIKE(EFACAB)
004000970311     D wrkccb          S                   like(efaccb)
004100950907     D WRKCTL          S                   LIKE(CTLCTL)
004200950929     D WRKBANCA        S                   LIKE(EFABANCA)
004300950907     D WRKINCTL        S              1    INZ(*OFF)
004400950907     D WRKINABI        S              1    INZ(*OFF)
004500950929     D WRKINBANCA      S              1    INZ(*OFF)
004600950907     D WRKEFAOK        S              1    INZ(*OFF)
004700981029     D*
004800981029     D* Reperimento rapporto bancario da conto
004900981029     D ND016DS       E DS                  inz
005000981029     D*
005100981029     D Wkcc            S                   like(cobkcb)    based(PKcc)
005200981029     D Wksc            S                   like(cobksb)    based(PKsc)
005300981029     D WBanca          S                   like(efabanca)  based(PBanca)
005400981029     D Wlinea          S                   like(efalinea)  based(PLinea)
005500981029     D* Definizioni parametri standard chiamate moduli
005600981029     D Esito           S              1
005700981029     D gesmsg          S              1
005800981029     D lenout          S              9B 0
005900981029     D Data            S             10
006000981029     D strutturaO      S             10
006100981029     D Operazione      S              2
006200981029     D LenIn           S              9B 0
006300981029     D refresh         S              1
006400981029     D uso             S              1
006500981029     DOutput           S           4000
006600061121     D*-------------
006700061121C1977D* Controllo conti
006800061121  "  D ND002DS       E DS                  inz
006900061121  "  D Categorie             131    250
007000061121  "  D* Schiera categorie da ND002DS
007100061121  "  D DsCateg         DS           120
007200061121  "  D SkCateg                        4    dim(30)
007300061121C1977D DataIso         s               d   datfmt(*iso)
007400061121      *-------------
007500951002      /eject
007600000000     C******************************************************
007700000000     C*          F L U S S O   P R I N C I P A L E         *
007800000000     C******************************************************
007900951002
008000951002     C*Ricevo parametri di presentazione.
008100950906     C                   MOVEL     KPJBU         NDCE90DS
008200951002
008300951002     C*Reperisco tabella effetto
008400950906     C                   clear                   xatbds
008500950906     C                   eval      xtbazi        = xscsoc
008600950906     C                   eval      xtbcod        = '0E1'
008700950906     C                   eval      xtbkey        = *zeros
008800950906     C                   eval      xtbric        = '1'
008900950907     C                   Z-ADD     1             I
009000981029     C*
009100950908     C     I             DOWLE     9
009200950908     C     TE(I)         ANDNE     *BLANKS
009300981029     C*
009400950906     C                   move      TE(I)         xtbkey
009500961007     C                   CALLb     'XATB'
009600950906     C                   PARM                    XATBDS
009700950906     C     xtberr        ifeq      *on
009800950906     C                   clear                   ANG0E1DS
009900950906     C                   ELSE
010000950906     C                   MOVEL     xtbuni        ANG0E1DS
010100950906     C                   ENDIF
010200950906     C                   MOVEL     SPT0E1        SPA(I)
010300030319C1638C                   MOVEL     CPT0E1        CPA(I)
010400981029     c*
010500981029     C* Contropartita spese bolli
010600950907     C                   SELECT
010700950907     C     E90TBL        WHENEQ    'V'                                          * VIRTUALE
010800950906     C                   MOVEL     SEB0E1        SEB(I)
010900950907     C     E90TBL        WHENEQ    'A'                                          * AZIENDALE
011000950907     C                   MOVEL     EASPB         SEB(I)
011100950907     C                   ENDSL                                                  * E90TBL
011200981029     C*
011300950907     C                   ADD       1             I
011400950906     C                   ENDDO
011500981029     C*
011600951002      /EJECT
011700000000     C* PULISCO CAMPI DI LAVORO ---------------------------*
011800951002
011900950906     C* VERIFICO SELEZIONE CARTELLO -----------------------*
012000950906     C                   MOVEA     E92SCHCTL     CTL
012100950906     C                   MOVEL     *OFF          WRKINCTL
012200950906     C     E92SCHCTL     IFNE      *BLANKS
012300950906     C                   MOVEL     *ON           WRKINCTL
012400950906     C                   OPEN      ANCTL02L
012500950906     C                   ENDIF
012600951002
012700950906     C* VERIFICO SELEZIONE A.B.I. -------------------------*
012800950906     C                   MOVEA     E92SCHABI     ABI
012900950906     C                   MOVEL     *OFF          WRKINABI
013000950906     C     E92SCHABI     IFNE      *BLANKS
013100950906     C                   MOVEL     *ON           WRKINABI
013200950906     C                   ENDIF
013300951002
013400950929     C* VERIFICO SELEZIONE BANCA  -------------------------*
013500950929     C                   MOVEA     E92SCHBANC    BANCA
013600950929     C                   MOVEL     *OFF          WRKINBANCA
013700950929     C     E92SCHBANC    IFNE      *BLANKS
013800950929     C                   MOVEL     *ON           WRKINBANCA
013900950929     C                   ENDIF
014000951002      /space
014100950906     C                   OPEN      NDPEE01L
014200951002     C                   OPEN      NDEFA08L
014300951002     C                   OPEN      NDREG01L
014400991103     c*
014500991103     c* se richiesto di non eliminare gli effetti estratti precedentemente,
014600991103     c* trovo l'importo limite distinta già estratto
014700991103 1   c                   if        E92Eee = 'N'
014800991103     c     xscsoc        setll     Ndpee01l
014900991103 2   c                   do        *hival
015000991103     c     xscsoc        reade     NdPee01l                               21
015100991103     c* non metto gestione record allocato perchè non si può lavorare in due
015200991103     c* sulla stessa società e quindi non ce n'è bisogno
015300991103     c   21              leave
015400991103     c                   add       PeeImp        WrkIld
015500991103     c* se supera il nuovo limite distinta, cancello i record
015600991103 3   c                   if        e92ild <> 0 and wrkild > e92ild
015700991103     c                   delete    Ndpee01l
015800991103     c                   sub       peeimp        Wrkild
015900991103 3e  c                   endif
016000991103     c*
016100991103 2e  c                   enddo
016200991103     c                   else
016300991103     C                   Z-ADD     0             WRKILD                         * TOTALE DISTIN
016400991103 1e  c                   endif
016500991103     c*
016600991103     C     E92ILD        IFEQ      0
016700991103     C                   MOVE      *HIVAL        E92ILD
016800991103     C                   END
016900951002     C*
017000951002     C* Estrazione effetti da presentare.
017100950906     C                   EXSR      ESTRAZIONE
017200951002     C*
017300950906     C                   CLOSE     NDPEE01L
017400951002     C                   CLOSE     NDEFA08L
017500951002     C                   CLOSE     NDREG01L
017600950906     C                   CLOSE     ANCTL02L
017700951002      /space
017800951002     C* Fine.
017900951002     C* Ordinamento effetti estratti su NDPEE.
018000960216     C                   CALL      'NDCE9BR'                            LR
018100901003     C                   PARM                    KPJBA
018200970206    >C                   parm      *off          flgricar          1
018300951002     C*
018400950908     C                   SETON                                        LR
018500951002     C*----------------------------------------------------*
018600951002      /title estrazione
018700951002     C*----------------------------------------------------*
018800950906     C* E S T R A Z I O N E *                              *
018900951002     C*----------------------------------------------------*
019000950906     C     ESTRAZIONE    BEGSR
019100951002     C*
019200950907     C                   MOVEL     E90CTB        EFACTB                         *
019300950906     C                   MOVEL     '1'           EFASTATUS                      *
019400950929     C                   MOVEL     '0'           EFAESITO                       * NON INSOLUTO
019500950907     C                   MOVEL     E92DIV        EFADIVISA                      * NON INSOLUTO
019600080715D2413 * gli effetti vanno estratti in ordine di data scadenza, non di
019700080715  "   * tipo effetto e data scadenza, quindi modificata v.l. ndefa08l
019800080715D2413 * e modificato ciclo di lettura
019900080714D2413x***                Z-ADD     1             I
020000080714  "  x***  I             DOWLE     9
020100080714D2413x***  TE(I)         ANDNE     *BLANKS
020200981029     C*
020300080715D2413x***                MOVE      TE(I)         EFAMDPAG
020400080715  "  c* in te(1) c'è il tipo effetto + basso, quindi parto da quello
020500080715D2413c                   MOVE      TE(1)         EFAMDPAG
020600950906     C                   MOVEL     E92DSI        EFADTSCA
020700951002     C*
020800951002     C     K04EFA08      SETLL     NDEFA08L
020900951002     C     K03EFA08      READE     NDEFA08L                               10
021000000000     C     *IN10         DOWEQ     '0'
021100951016     C* test scadenza:
021200950906     C     EFADTSCA      ANDLE     E92DSF
021300050110D1720C* controllo se effetto già estratto
021400050110  "  C                   Z-ADD     EFANREFF      PEENEF
021500050110  "  C     K02PEE01      CHAIN(N)  NDPEE01L
021600050110D1720c                   if        not %found(ndpee01l)
021700080715     c*
021800951016     C* test unità:
021900951016     C                   movel     *off          *in88
022000960207     C                   IF        *in50 = *on and e90unitaes <> *blanks
022100951016     C     k02reg01      chain     ndreg000                           88
022200960207     C  n88              if        regunita <> e90unitaes
022300951016     C                   movel     *on           *in88
022400951016     C                   endif
022500951016     C                   endif
022600080715     c*
022700080715D2413C* test tipo effetto
022800080715  "  C                   Z-ADD     1             I
022900080715  "  C     EfaMdPag      LOOKUP    TE(I)                                  11
023000080715D2413C  n11              movel     *on           *in88
023100080715     c*
023200960306     C* test capoconto
023300960306     C                   IF        e92kcc <> *blanks and efakcc <> E92kcc
023400960306     C                   movel     *on           *in88
023500960306     C                   endif
023600061121C1977C* test categoria anagrafica
023700061121  "  c                   if        E92CtgAna <> *blanks and
023800061121  "  c                             EaPosAtt <> *blanks
023900061121  "  c                   exsr      CtrCtgAna
024000061121C1977C                   endif
024100051214     C*
024200051214C1893C* controlli per effetti monopartita
024300051214C1893c                   if        EfaMulPar <>'1'
024400960306     C* test data partita
024500960306     C                   IF        efadtpar > E92Dtpar
024600960306     C                   movel     *on           *in88
024700960306     C                   endif
024800051214C1893c* controllo categoria
024900051214  "  c                   if        E92Ctg1 <> *blanks or
025000051214  "  c                             E92Ctg2 <> *blanks or
025100051214  "  c                             E92Ctg3 <> *blanks
025200051214  "  c                   eval      PpaSocieta=E90Societa
025300051214  "  c                   eval      PpaCtb    =E90Ctb
025400051214  "  c                   eval      Ppakcc    =Efakcc
025500051214  "  c                   eval      Ppaksc    =Efaksc
025600051214  "  c                   eval      PpaDtPar  =EfaDtPar
025700051214  "  c                   eval      PpaNrPar  =EfaNrPar
025800051214  "  c                   eval      PpaSerPar =EfaSerPar
025900051214  "  c     K07Ppa01      chain     Ndppa01l
026000051214  "  c                   if        %found(Ndppa01L)
026100051214  "  C                   Eval      PPsSys     =  PPaSys
026200051214  "  C                   Eval      PPsNrAsInt =  PPaNrAsInt
026300051214  "  c     K02PPs01      Chain     NdPPs01L
026400051214  "  c                   if        %found(Ndpps01L)
026500051214  "  c                   select
026600051214  "  c                   when      E92Ctg1    <> *blanks and
026700051214  "  c                             PpsCtgAna1 <> e92ctg1
026800051214  "  C                   movel     *on           *in88
026900051214  "  c                   when      e92ctg2    <> *blanks and
027000051214  "  c                             PpsCtgAna2 <> e92ctg2
027100051214  "  C                   movel     *on           *in88
027200051214  "  c                   when      e92ctg3    <> *blanks and
027300051214  "  c                             PpsCtgAna3 <> e92ctg3
027400051214  "  C                   movel     *on           *in88
027500051214  "  c                   endsl
027600051215  "  c* se non trova PPS ma c'è la selezione, non si estrae
027700051215  "  c                   else
027800051215  "  C                   movel     *on           *in88
027900051214  "  c                   endif
028000051215  "  c* se non trova PPA ma c'è la selezione, non si estrae
028100051215  "  c                   else
028200051215  "  C                   movel     *on           *in88
028300051214  "  c                   endif
028400051214  "  c                   endif
028500051214  "  c*
028600051214  "  c                   else
028700051214  "  c*
028800051214  "  c* controlli per effetto multipartita
028900051214  "  c                   exsr      CtrMulPar
029000051214C1893C                   endif
029100051214     C*
029200971211     C* test importo in divisa
029300971211     c                   if        efaimportd <> *zeros
029400971211     C                   IF        efaimportd > E92Impefa  or
029500971211     C                             efaimportd < E92Impefda
029600971211     C                   movel     *on           *in88
029700971211     C                   endif
029800971211     C                   endif
029900980129     c*
030000980129     C* test presentazione tipo registrazione effetti è valorizzato:
030100980129     C* se tipo registrazione è 'P'(provvisorio) e la data EFADTEFF = *loval
030200980129     C* il record viene estratto come Provvisorio
030300980129     C* se tipo registrazione è 'D'(definitivo) e la data EFADTEFF <> *loval
030400980129     C* il record viene estratto come Definitivo
030500980129     C* se tipo registrazione non è 'D' e non è 'P' il record viene estratto
030600980129     C*
030700980129     C                   IF        e92tiporeg > *blanks
030800980129     C                   IF        e92tiporeg = 'P' and efadteff <> *loval
030900980129     C                   movel     *on           *in88
031000980129     C                   endif
031100980129     C                   IF        e92tiporeg = 'D' and efadteff = *loval
031200980129     C                   movel     *on           *in88
031300980129     C                   endif
031400980129     C                   endif
031500951002     C*Scrivo NDPEE.
031600951016     C  n88              EXSR      WRTPEE
031700050110     c*
031800050110D1720C                   endif
031900050110     c*
032000951002     C     K03EFA08      READE     NDEFA08L                               10
032100950906     C                   ENDDO                                                  *IN10 DOWEQ '0'
032200981029     C*
032300080714D2413x***                ADD       1             I                              *
032400080714D2413x***                ENDDO                                                  *
032500951002     C*
032600000000     C                   ENDSR
032700051214C1893C************************************************************
032800051214     C* Controlli partite degli effetti multipartita
032900051214C1893C************************************************************
033000051214     C     CtrMulPar     BEGSR
033100051214     c*
033200051214     c* da fare solo se presenti parzializzazioni sulle categorie
033300051214     c                   if        e92ctg1 <> *blanks or
033400051214     c                             e92ctg2 <> *blanks or
033500051214     c                             e92ctg3 <> *blanks
033600051214     c* o sulla data partita
033700051214     c                             or E92DtPar <> *hival
033800051214     c*
033900051214     C                   Eval      passys=efasys
034000051214     C                   Eval      pasnrasreg=efanrasreg
034100051214     C                   Eval      pasnrrigam=1
034200051214    >C     K03Pas01      SETLL     NdPas01l
034300051214     c                   DO        *HIVAL
034400051214    >C     K03pas01      Reade     ndpas01l                               21
034500051214     c   21              LEAVE
034600051214     c* data partita
034700051214 1b  C                   If        PasDtPar >   E92DtPar
034800051214     C                   movel     *on           *in88
034900051214     c* basta che una partita non rientri nel range che l'effetto non
035000051214     c* va considerato
035100051214     c                   leave
035200051214 1e  C                   Endif
035300051214     c*
035400051214     c* categoria di partita
035500051214     c                   if        e92ctg1 <> *blanks or
035600051214     c                             e92ctg2 <> *blanks or
035700051214     c                             e92ctg3 <> *blanks
035800051214     c                   eval      PpaSocieta=E90Societa
035900051214     c                   eval      PpaCtb    =E90Ctb
036000051214     c                   eval      Ppakcc    =Efakcc
036100051214     c                   eval      Ppaksc    =Efaksc
036200051214     c                   eval      PpaDtPar  =PasDtPar
036300051214     c                   eval      PpaNrPar  =PasNrPar
036400051214     c                   eval      PpaSerPar =PasSerPar
036500051214     c     K07Ppa01      chain     Ndppa01l
036600051214     c                   if        %found(Ndppa01L)
036700051214     C                   Eval      PPsSys     =  PPaSys
036800051214     C                   Eval      PPsNrAsInt =  PPaNrAsInt
036900051214     c     K02PPs01      Chain     NdPPs01L
037000051214     c                   if        %found(Ndpps01L)
037100051214     c                   select
037200051214     c                   when      e92ctg1    <> *blanks and
037300051214     c                             PpsCtgAna1 <> e92ctg1
037400051214     C                   movel     *on           *in88
037500051214     c                   leave
037600051214     c                   when      e92ctg2    <> *blanks and
037700051214     c                             PpsCtgAna2 <> e92ctg2
037800051214     C                   movel     *on           *in88
037900051214     c                   leave
038000051214     c                   when      e92ctg3    <> *blanks and
038100051214     c                             PpsCtgAna3 <> e92ctg3
038200051214     C                   movel     *on           *in88
038300051214     c                   leave
038400051214     c                   endsl
038500051215     c* se non trova PPS ma c'è la selezione, non si estrae
038600051215     c                   else
038700051215     C                   movel     *on           *in88
038800051214     c                   endif
038900051215     c* se non trova PPA ma c'è la selezione, non si estrae
039000051215     c                   else
039100051215     C                   movel     *on           *in88
039200051214     c                   endif
039300051214     c                   endif
039400051214     c*
039500051214     c                   enddo
039600051214     c                   endif
039700051214     c*
039800051214C1893c                   ENDSR
039900061121     c*
040000061121C1977C************************************************************
040100061121     C* Controlli partite degli effetti multipartita
040200061121C1977C************************************************************
040300061121     C     CtrCtgAna     BEGSR
040400061121     c*
040500061121     c                   move      EaPosAtt      Pos               2 0
040600061121     c                   clear                   Attrib            4
040700061121     C                   Clear                   ND002DS
040800061121     C                   EVAL      LenOut = %Size(ND002DS)
040900061121     C                   eval      DataIso=*loval
041000061121     C                   CallB     'NDMVC002'
041100061121     C                   PARM                    xscsoc
041200061121     C                   PARM                    efakcc
041300061121     C                   PARM                    efaksc
041400061121     C                   PARM                    DataIso
041500061121     C                   PARM      *OFF          GesMsg
041600061121     C                   PARM      *OFF          Esito
041700061121     C                   PARM      'ND002DS'     StrutturaO
041800061121     C                   PARM                    ND002DS
041900061121     C                   PARM                    LenOut
042000061121     C*
042100061121     c                   if         esito = *off
042200061121     c                   movel     Categorie     DsCateg
042300061121     c                   eval      Attrib   = SkCateg(Pos)
042400061121     c                   endif
042500061121     c* se attributo <>
042600061121     c                   if        Attrib <> E92CtgAna
042700061121     c                   seton                                        88
042800061121     c                   endif
042900061121     c*
043000061121c1977c                   endsr
043100951002     C*----------------------------------------------------*
043200950906     C* W R T P E E *      SCRITTURA NDPEE00F              *
043300951002     C*----------------------------------------------------*
043400000000     C     WRTPEE        BEGSR
043500951002     C*
043600950907     C                   CLEAR                   WRKCTL
043700950907     C                   CLEAR                   WRKABI
043800950907     C                   CLEAR                   WRKCAB
043900970311     C                   clear                   wrkccb
044000950929     C                   CLEAR                   WRKBANCA
044100951003     C*
044200981029     C* Reperisco ABI dell'effetto o del cliente e anche la banca del client
044300951003     C                   EXSR      RTVABI
044400951002     C*
044500950906     C                   MOVEL     *ON           WRKEFAOK
044600951003     C*
044700951003     C     WRKINCTL      IFEQ      *ON
044800951003     C     WRKINABI      OREQ      *ON
044900951003     C     WRKINBANCA    OREQ      *ON
045000951003     C                   MOVEL     *OFF          WRKEFAOK
045100951003     C                   ENDIF
045200981029     C*
045300951003     C* Selezione per cartello/abi se ABI =/ da blanks.
045400950906     C     WRKABI        IFNE      *BLANKS
045500951003     C*cartello.
045600950906     C     WRKINCTL      IFEQ      *ON
045700950906     C     WRKEFAOK      ANDEQ     *OFF
045800950906     C                   EXSR      SELCTL
045900950906     C                   ENDIF
046000951003     C*cod.abi.
046100950906     C     WRKINABI      IFEQ      *ON
046200950906     C     WRKEFAOK      ANDEQ     *OFF
046300950906     C                   EXSR      SELABI
046400950906     C                   ENDIF
046500951003     C*
046600950906     C                   ENDIF
046700981029     C*
046800951003     C* Selezione per banca.
046900951003     C     WRKINBANCA    IFEQ      *ON
047000951003     C     WRKEFAOK      ANDEQ     *OFF
047100951003     C                   EXSR      SELBANCA
047200951003     C                   ENDIF
047300981029     C*
047400950907     C     WRKEFAOK      IFEQ      *ON
047500950907     C                   ADD       EFAIMPORTD    WRKILD                         * TOTALE DIST.
047600950906     C     WRKILD        IFLE      E92ILD                                       * SOTTO IL LIM.
047700950907     C                   MOVEL     EFASOCIETA    PEESOCIETA
047800000000     C                   Z-ADD     EFANREFF      PEENEF
047900991103     C     K02PEE01      CHAIN(N)  NDPEE01L                           10
048000951002     C*
048100000000     C     *IN10         IFEQ      '1'
048200960207     c                   if        e90unitaes = *blanks and *in50 = *on
048300960207     C     K02REG01      CHAIN     NDREG01L                           10
048400960207     c                   endif
048500950907     C     *NOKEY        CLEAR                   NDPEE000
048600951016     C                   IF        *in50 = *on
048700950907     C                   MOVEL     REGUNITA      PEEUNITA
048800951016     C                   else
048900951016     C                   MOVEL     *blanks       PEEUNITA
049000951016     C                   endif
049100000000     C                   MOVEL     EFAMDPAG      PEETIF
049200950907     C                   Z-ADD     1             I
049300950906     C     PEETIF        LOOKUP    TE(I)                                  11
049400030319C1638C   11              MOVEL     CPA(I)        PEECPA
049500950907     C   11              MOVEL     SPA(I)        PEESPA
049600950907     C   11              MOVEL     SEB(I)        PEESEB
049700000000     C                   MOVEL     EFAITAEST     PEECLS
049800000000     C                   MOVEL     EFAKCC        PEECCL
049900000000     C                   MOVEL     EFAKSC        PEESCL
050000000000     C                   Z-ADD     EFANRPAR      PEENRP
050100000000     C                   MOVEL     EFADTPAR      PEEDRP
050200000000     C                   Z-ADD     EFAIMPORTO    PEEIMP
050300000000     C                   MOVEL     EFADIVISA     PEEDIV
050400000000     C                   Z-ADD     EFAIMPORTD    PEEIM2
050500000000     C                   MOVEL     EFADTSCA      PEEDSC
050600000000     C                   MOVEL     EFADTSCA      PEEDVE
050700950907     C                   MOVEL     WRKCTL        PEECTL
050800950907     C                   MOVEL     WRKABI        PEEABI
050900950907     C                   MOVEL     WRKCAB        PEECAB
051000970311     C                   movel     wrkccb        peeccb
051100951002     C                   MOVEL     WRKBANCA      PEEBANCA
051200970227     C                   MOVEL     Efatpsupel    PEEtpsupel
051300951002     C*scrivo NDPEE
051400950906     C                   WRITE     NDPEE000
051500000000     C                   END
051600901002     C                   ELSE
051700950907     C                   SUB       EFAIMPORTD    WRKILD
051800950906     C                   END                                                    * WRKILD
051900950907     C                   ENDIF                                                  * E92DIV
052000951002     C*
052100000000     C                   ENDSR
052200981029     c/eject
052300951002     C*----------------------------------------------------*
052400950906     C* R T V A B I *                                      *
052500951002     C*----------------------------------------------------*
052600950906     C     RTVABI        BEGSR
052700981029      *
052800090504>>>>>C*  banca già attribuita
052900090504>>>>>c                   if        efabanca <> '    '
053000090504>>>>>c                   eval      wrkbanca = efabanca
053100090504>>>>>c                   endif
053200951002     C* codice ABI immesso.
053300980121if01 C     EFAABI        IFNE      *BLANKS
053400950906     C                   EVAL      WRKABI        = EFAABI
053500950906     C                   EVAL      WRKCAB        = EFACAB
053600970311     C                   eval      wrkccb        = efaccb
053700951002     C* codice ABI non immesso.
053800980121el01 C                   ELSE
053900980121     c*
054000950906     C                   CLEAR                   DVCOBADS
054100980121if02 c                   if        EFAcess       = *off
054200950906     C                   EVAL      COBSOC        = XSCSOC
054300960328     C                   EVAL      COBCOF        = 'C'
054400950906     C                   EVAL      COBKSC        = EFAKSC
054500951002      *tipo appoggio
054600980121if03 C     EFATPAPPO     IFNE      *BLANKS
054700950906     C                   EVAL      COBTAP        = EFATPAPPO
054800980121el03 C                   ELSE
054900950906     C                   EVAL      COBTAP        = EATPAPPO
055000980121en03 C                   ENDIF
055100950906     C                   EVAL      COBDAL        = EFADTSCA
055200950911     C                   CALL      'DVCOBA'
055300950906     C                   PARM                    DVCOBADS
055400981029     C* se non reperito, provo con appoggio standard
055500980121if03 C     COBERR        IFEQ      *ON
055600950906     C                   EVAL      COBTAP        = *BLANKS
055700950911     C                   CALL      'DVCOBA'
055800950906     C                   PARM                    DVCOBADS
055900980121en03 c                   ENDIF
056000980121if03 C     COBERR        IFEQ      *OFF
056100950906     C                   EVAL      WRKABI        = COBABI
056200950906     C                   EVAL      WRKCAB        = COBCAB
056300970311     C                   eval      wrkccb        = cobccb
056400980121en03 C                   ENDIF
056500951002     C*
056600981029     c* reperisco banca del cliente per effettuare la selezione
056700981029if03 c                   if        cobkcb <> *blanks and
056800981029     c                             cobksb <> *blanks
056900981029     C* chiamo il driver
057000981029     C                   EVAL      PBanca = *null
057100981029     C                   EVAL      PLinea = *null
057200981029     C                   EVAL      PKcc = %addr(cobkcb)
057300981029     C                   EVAL      PKsc = %addr(cobksb)
057400981029     C*
057500981029     C                   EVAL      LenOut = %Size(ND016DS)
057600981029     C                   CALLB     'NDDVABRA'
057700981029     C                   PARM      '01'          Operazione
057800981029     C                   PARM                    Xscsoc
057900981029     C                   PARM                    WBanca
058000981029     C                   PARM                    WLinea
058100981029     C                   PARM                    WKcc
058200981029     C                   PARM                    WKsc
058300981029     C                   PARM      'ND016DS'     StrutturaO
058400981029     C                   PARM                    LenOut
058500981029     C                   PARM                    Esito
058600981029     C                   PARM                    ND016DS
058700981029     c                   if        esito = *off
058800981029     c                   eval      wrkbanca = Banca016
058900981029     c                   endif
059000981029en03 c                   endif
059100980121en02 c                   endif
059200980121     C*
059300980121en01 C                   ENDIF
059400951002     C*
059500950906     C                   ENDSR
059600951002     C*----------------------------------------------------*
059700950906      /TITLE SELEZIONE CARTELLO
059800951002     C*----------------------------------------------------*
059900950906     C* S E L C T L *                                      *
060000951002     C*----------------------------------------------------*
060100950906     C     SELCTL        BEGSR
060200951002     C*
060300950907     C     WRKABI        IFNE      *BLANKS
060400951002     C*
060500950906     C                   EVAL      CTLABI        = WRKABI
060600021023D1219X**** K01CTL02      CHAIN     ANCTL02L                           10
060700021023  "  C     K01CTL02      setll     ANCTL02L                           10
060800021023  "  c                   do        *hival
060900040305  "  C     K01CTL02      reade     ANCTL02L                               10
061000021023  "  c   10              leave
061100021023D1219X***  *IN10         IFEQ      *OFF
061200021023D1219C* se udate compresa fra le date di validità del carrello
061300021023  "  c                   if        udateiso >= ctldin and
061400021023D1219c                             udateiso <= ctldfi
061500021023     c*
061600950907     C     CTLCTL        LOOKUP    CTL                                    11
061700950906     C     *IN11         IFEQ      *ON
061800950906     C                   MOVEL     *ON           WRKEFAOK
061900021031D1219C                   EVAL      WRKCTL        = CTLCTL
062000021023D1219c                   leave
062100951003     C                   ELSE
062200951003     C                   MOVEL     *OFF          WRKEFAOK
062300950906     C                   ENDIF
062400021023     c*
062500021023     c                   ENDIF
062600021023     c*
062700021023D1219C                   enddo
062800951002     C*
062900950907     C                   ENDIF
063000951002     C*
063100950906     C                   ENDSR
063200951002     C*----------------------------------------------------*
063300950911      /TITLE SELEZIONE A.B.I.
063400951002     C*----------------------------------------------------*
063500950911     C* S E L A B I *                                      *
063600951002     C*----------------------------------------------------*
063700950906     C     SELABI        BEGSR
063800951002     C*
063900950907     C     WRKABI        IFNE      *BLANKS
064000950907     C     WRKABI        LOOKUP    ABI                                    11
064100950906     C     *IN11         IFEQ      *ON
064200950906     C                   MOVEL     *ON           WRKEFAOK
064300951003     C                   ELSE
064400951003     C                   MOVEL     *OFF          WRKEFAOK
064500950906     C                   ENDIF
064600950907     C                   ENDIF
064700951002     C*
064800950906     C                   ENDSR
064900951002     C*----------------------------------------------------*
065000950929      /TITLE SELEZIONE BANCA
065100951002     C*----------------------------------------------------*
065200950929     C* S E L B A N C A *                                  *
065300951002     C*----------------------------------------------------*
065400950929     C     SELBANCA      BEGSR
065500951003     C*
065600950929     C     WRKBANCA      IFNE      *BLANKS
065700950929     C     WRKBANCA      LOOKUP    BANCA                                  11
065800950929     C     *IN11         IFEQ      *ON
065900950929     C                   MOVEL     *ON           WRKEFAOK
066000951003     C                   ELSE
066100951003     C                   MOVEL     *OFF          WRKEFAOK
066200950929     C                   ENDIF
066300950929     C                   ENDIF
066400951002     C*
066500950929     C                   ENDSR
066600951002     C*----------------------------------------------------*
066700951002      /title definizioni iniziali
066800951002     C*----------------------------------------------------*
066900950906     C* * I N Z S R *                                      *
067000951002     C*----------------------------------------------------*
067100950906     C     *INZSR        BEGSR
067200951002     C*
067300000000     C     *ENTRY        PLIST
067400000000     C                   PARM                    KPJBA
067500951002     C*
067600950906     C                   EVAL      KPJBUS        = KPJBU
067700950906     C                   CALLB     'XSOC'
067800950906     C                   PARM      'SOC001'      TIPXSC            6
067900950906     C                   PARM                    SOCXSC            3
068000950906     C                   PARM                    CDSXSC            9 0
068100950906     C                   PARM                    MODXSC            3
068200950906     C                   PARM                    RTNXSC            1
068300950906     C     XSOC001DS     PARM                    XSOCDS         1000
068400950906     C                   PARM                    KPJBA
068500950906     C                   EVAL      KPJBU         = KPJBUS
068600991103     c*
068700991103     C*Reperisco parametri di estrazione.
068800991103     C                   CALLb     'XPAR'
068900991103     C                   PARM      XSCSOC        XPASOC
069000991103     C                   PARM      'NDCE92'      XPAPAR
069100991103     C     NDCE92DS      PARM                    XPAOUT
069200991103     C                   PARM                    XPAERR
069300991103     C                   PARM      *BLANKS       XPARIC
069400991103     C     xpaerr        ifeq      '1'
069500991103     C                   ELSE
069600991103     C                   MOVEL     E92TE1        TE(1)
069700991103     C                   MOVEL     E92TE2        TE(2)
069800991103     C                   MOVEL     E92TE3        TE(3)
069900991103     C                   MOVEL     E92TE4        TE(4)
070000991103     C                   MOVEL     E92TE5        TE(5)
070100991103     C                   MOVEL     E92TE6        TE(6)
070200991103     C                   MOVEL     E92TE7        TE(7)
070300991103     C                   MOVEL     E92TE8        TE(8)
070400991103     C                   MOVEL     E92TE9        TE(9)
070500991103     C                   endif
070600951016     C*
070700951016     C* unità obbligatoria per questa società: on  50
070800951016     C                   IF        FgO1 =  *on
070900951016     C                   movel     *on           *in50
071000951016     C                   ENDIF
071100951002
071200000000     C* CHIAVI ============================================*
071300950906     C     K01CTL02      KLIST
071400950906     C                   KFLD                    CTLABI
071500951002     C*
071600951002     C     K02REG01      KLIST
071700951002     C                   KFLD                    efaSYS
071800951002     C                   KFLD                    efaNRASREG
071900951002     C*
072000950906     C     K04EFA08      KLIST
072100950906     C                   KFLD                    XSCSOC
072200950907     C                   KFLD                    EFACTB
072300950906     C                   KFLD                    EFADIVISA
072400080714D2413x*****              KFLD                    EFAMDPAG
072500950906     C                   KFLD                    EFADTSCA
072600080714D2413c                   KFLD                    EFAMDPAG
072700951002     C*
072800950906     C     K03EFA08      KLIST
072900950906     C                   KFLD                    XSCSOC
073000950907     C                   KFLD                    EFACTB
073100950906     C                   KFLD                    EFADIVISA
073200080714D2413x*****              KFLD                    EFAMDPAG
073300951002     C*
073400950907     C     K02PEE01      KLIST
073500950907     C                   KFLD                    PEESOCIETA
073600000000     C                   KFLD                    PEENEF
073700051214C1893C* ndpas01L
073800051214  "  C     K03pas01      KLIST
073900051214  "  C                   KFLD                    passys
074000051214  "  C                   KFLD                    pasnrasreg
074100051214  "  C                   KFLD                    pasnrrigam
074200051214  "  C* NdPPa01L
074300051214  "  C     K07PPa01      Klist
074400051214  "  C                   Kfld                    PPaSocieta
074500051214  "  C                   Kfld                    PPaCtb
074600051214  "  C                   Kfld                    PPaKcc
074700051214  "  C                   Kfld                    PPaKsc
074800051214  "  C                   Kfld                    PPaDtPar
074900051214  "  C                   Kfld                    PPaNrPar
075000051214  "  C                   Kfld                    PPaSerPar
075100051214  "  C* NdPPs01L
075200051214  "  C     K02PPs01      Klist
075300051214  "  C                   Kfld                    PPsSys
075400051214C1893C                   Kfld                    PPsNrAsInt
075500951002
075600961007     C                   CALLb     'XPAR'
075700950906     C                   PARM      XSCSOC        XPASOC
075800950906     C                   PARM      'NDCEEA'      XPAPAR
075900950906     C     NDCEEADS      PARM                    XPAOUT
076000950906     C                   PARM                    XPAERR
076100950906     C                   PARM      *BLANKS       XPARIC
076200950906     C     xpaerr        ifeq      '1'
076300950906     C                   endif
076400951002
076500000000     C* DEFINIZIONE CAMPI DI LAVORO =======================*
076600950906     C     *EUR          MOVEL     *DATE         UDATEISO
076700951002     C*
076800000000     C                   ENDSR
076900951002     C*----------------------------------------------------*
