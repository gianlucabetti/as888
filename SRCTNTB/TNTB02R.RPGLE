000100980909
000200980909      *------------------------------------------------------------------------*
000300990519      * GESTIONE TABELLA CODICI DOCUMENTI DA RICHIEDERE                        *
000400980909      *------------------------------------------------------------------------*
000500980909
000600980909      *--------------------------------------------
000700980909      *  RIEPILOGO INDICATORI
000800980909      *--------------------------------------------
000900981102      * 01 - RECORD annullato
001000981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001100990305      * 30 - Comodo
001200980909      *--------------------------------------------
001300980909
001400980506     H DECEDIT('0,') DATEDIT(*DMY.)
001500980909
001600981117     FTNTB02D   CF   E             WORKSTN
001700981102     FTNTBE01L  UF A E           K DISK
001800980909
001900981102      *--------------------------------------------
002000981117     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG VIDEO
002100981102      *
002200981102     D Kttcod          S                   LIKE(TBEcod)
002300981117     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000CDD')
002400981117     D Kcod            S                   LIKE(TBEcod)  INZ('CDD')
002500981102     D Kke1            S                   LIKE(TBEke1)
002600981102     D Kke2            S                   LIKE(TBEke2)
002700981102     D Klin            S                   LIKE(TBElin)
002800981102     D Ksif            S                   LIKE(TBEsif)
002900981102     D Wtibs02r        S              1
003000990305     D* schiera tipi anomalie
003100990308     D WTA             S              1    DIM(3)
003200981102      *
003300980507     D KPJBA         E DS
003400981117     D DCDD          E DS
003500981102     D UT§DSE0F      E DS
003600981102     D TIBS02DS      E DS
003700981102      *
003800981102     D                SDS
003900981102     D  VTCPGM                 1     10
004000980507      *--------------------------------------------
004100980909
004200980506      * EMISSIONE PRIMA VIDEATA
004300981117     C                   DO        *hival
004400981117     C                   EXFMT     TN02D01
004500980930      *
004600980909     C                   clear                   V1CMSG
004700981102     C                   eval      *in28 = *off
004800980930      *
004900980506      * F3=FINE
005000981117     c   KC              leave
005100981102      *
005200981102      * CONTROLLI VIDEATA
005300981102     C                   EXSR      CTRD01
005400981102      *
005500981117     C   28              iter
005600980506      *
005700980909      * IMPOSTO CAMPI SECONDA VIDEATA
005800980909     C                   EXSR      RIED02
005900981117      *
006000981117      * GESTIONE SECONDA VIDEATA
006100981117     C                   EXSR      GESD02
006200980507      *
006300981117     c                   enddo
006400980506      *
006500981102      *
006600981102     c                   IF        Wtibs02r = 'S'
006700981102     c                   clear                   tibs02ds
006800981102     c                   movel     'C'           t02tla
006900981102     c                   call      'TIBS02R'
007000981102     c                   parm                    kpjba
007100981102     c                   parm                    tibs02ds
007200981102     c                   ENDIF
007300981102      *
007400980506     C                   EVAL      *INLR = *ON
007500981102      *****************************************************************
007600981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
007700981102      *****************************************************************
007800981102     C     CTRD01        BEGSR
007900981102      *
008000981102     c                   clear                   TIBS02DS
008100981102      *
008200981117     C     '?'           SCAN      V1Ccdd                                 30
008300981102      *  Ricerca
008400981102    1C                   IF        *IN30 = *ON
008500981102     c                   eval      WTIBS02R = 'S'
008600981102     c                   movel     'R'           t02mod
008700981102     c                   movel     knsif         t02sif
008800981102     C                   movel     Kcod          t02cod
008900981102     C                   CALL      'TIBS02R'
009000981102     c                   parm                    KPJBA
009100981102     c                   parm                    TIBS02DS
009200981102      *
009300981102    2C                   IF        T02err = *blanks
009400981117     C                   movel     T02ke1        V1Ccdd
009500981102     C                   ENDIF
009600981102     C                   ENDIF
009700981102      *
009800981102      *   Controlli di validità'
009900981117    2C                   IF        V1Ccdd = *blanks  or  T02err <> *blanks
010000981102     C                   movel     MSG(1)        V1CMSG
010100981102     c                   eval      *in28 = *on
010200981102     C                   ENDIF
010300981102      *
010400981102     C                   ENDSR
010500980506      *****************************************************************
010600980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
010700980506      *****************************************************************
010800980506     C     RIED02        BEGSR
010900980909      *
011000981102     c                   clear                   TNTBE000
011100981117     c                   clear                   DCDD
011200980909     c                   eval      *in01 = *off
011300981117     c                   movel(P)  V1Ccdd        Kke1
011400981117     c                   movel     V1Ccdd        V2Ccdd
011500990712     c                   movel     'S'           V2Cftt
011600980930      *
011700981102     c     KTBE          CHAIN     TNTBE000                           02
011800990712     C  n02              movel     tbeftt        V2Cftt
011900990712     C  n02              z-add     tbeflt        V2Cflt
012000980506      *
012100981117     c  N02              movel     TBEuni        DCDD
012200981102     c                   eval      *in01 = (TBEatb <> *blanks)
012300980909      *
012400981117     C                   movel     V1Ccdd        V2Ccdd
012500981117     C                   movel     §CDDdesc      V2Cdes
012600990519     C                   movel     §CDDengl      V2Ceng
012700981229     C                   movel     §CDDrdnr      V2Crnr
012800981229     C                   movel     §CDDrdsr      V2Crsr
012900981117     C                   movel     §CDDPDAT      V2CPda
013000981117     C                   movel     §CDDPNUM      V2CPnu
013100981117     C                   movel     §CDDPIMP      V2CPim
013200981118     C                   movel     §CDDPint      V2CPin
013300990519     C                   movel     §CDDsopo      V2Cspo
013400990305     C                   movea     §CDDvtad      WTA
013500990305     c                   clear                   v2cman
013600990305     c                   clear                   v2camm
013700990305     c                   clear                   v2cava
013800990305     c* Mancanza
013900990305     c     'M'           lookup    wta                                    30
014000990305     c   30              movel     'S'           v2cman
014100990305     c* Ammanco
014200990305     c     'A'           lookup    wta                                    30
014300990305     c   30              movel     'S'           v2camm
014400990305     c* Avaria
014500990305     c     'V'           lookup    wta                                    30
014600990305     c   30              movel     'S'           v2cava
014700990308     c*
014800990308     C                   move      §CDDlimr      V2Cimr
014900990305     c*
015000980506      *
015100980506     C                   ENDSR
015200981117      *****************************************************************
015300981117      *   ROUTINE PER GESTIONE SECONDA VIDEATA
015400981117      *****************************************************************
015500981117     C     GESD02        BEGSR
015600981117      *
015700981117      * EMISSIONE
015800981117     C                   DO        *HIVAL
015900981117     C                   EXFMT     TN02D02
016000981117      *
016100981117     C                   CLEAR                   V2CMSG
016200981117     c                   eval      *in28 = *off
016300981117      *
016400981117      * F3=FINE
016500981117     c   KC              leave
016600981117      *
016700981117      * F12=RITORNO
016800981117     c   KL              leave
016900981117      *
017000981117      * CONTROLLI VIDEATA
017100981117     C                   EXSR      CTRD02
017200981117      * errori
017300981117     C   28              iter
017400981117      *
017500981117      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma; F16(kq)=Annullamento
017600981117     c                   IF        *inKE = *ON  or  *inKF = *ON  or  *inKQ = *ON
017700981117     c                   exsr      RIECDD
017800981117     C                   leave
017900981117     c                   ENDIF
018000981117      *
018100981117     c                   enddo
018200981117      *
018300981117     C                   endsr
018400980506      *****************************************************************
018500980506      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
018600980506      *****************************************************************
018700980506     C     CTRD02        BEGSR
018800980507      *
018900981102      *  Descrizione obbligatoria
019000981117     c                   if        V2Cdes = *blanks
019100981102     c                   seton                                        2840
019200981102     C                   movel     MSG(2)        V2Cmsg
019300980909     C                   ENDIF
019400981102      *
019500981102      *  Tipo trasmissione
019600981102     c                   if        V2Cftt = *blanks  and  V2Cflt <> *zeros
019700981102     c                   seton                                        2842
019800981117     C                   movel     MSG(3)        V2Cmsg
019900981102     C                   ENDIF
020000980909      *
020100980909     C                   ENDSR
020200980507      *****************************************************************
020300980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
020400980507      *****************************************************************
020500981117     C     RIECDD        BEGSR
020600980909      *
020700981117     C                   movel     V2Cdes        §cdddesc
020800990519     C                   movel     V2Ceng        §cddengl
020900981229     C                   movel     V2Crnr        §cddrdnr
021000981229     C                   movel     V2Crsr        §cddrdsr
021100981117     C                   movel     V2Cpda        §cddpdat
021200981117     C                   movel     V2Cpnu        §cddpnum
021300981117     C                   movel     V2Cpim        §cddpimp
021400981118     C                   movel     V2Cpin        §cddpint
021500990519     C                   movel     V2Cspo        §cddsopo
021600990305     c                   clear                   wta
021700990305     C                   if        V2Cman = 'S'
021800990305     c                   movel     'M'           Wta(1)
021900990305     c                   endif
022000981102      *
022100990305     C                   if        V2Camm = 'S'
022200990305     c                   movel     'A'           Wta(2)
022300990305     c                   endif
022400990305      *
022500990305     C                   if        V2Cava = 'S'
022600990305     c                   movel     'V'           Wta(3)
022700990305     c                   endif
022800990305      *
022900990305     c                   movea     wta           §cddvtad
023000990308      *
023100990308     C                   z-add     V2Cimr        §cddlimr
023200990308      *
023300981117     c                   movel     DCDD          TBEuni
023400981102      *
023500981102     C                   movel     V2Cftt        TBEftt
023600981102     C                   z-add     V2Cflt        TBEflt
023700980507      *
023800981102     C   KQ              movel     'A'           TBEatb
023900981102     C   KE              movel     *blanks       TBEatb
024000980909      *
024100981102     c                   clear                   TBEftr
024200981102     c                   clear                   TBEdtr
024300981102      *
024400981102     c                   IF        *IN02 = *ON
024500981102     c                   movel     Ksif          TBEsif
024600981102     c                   movel     Klin          TBElin
024700981102     c                   movel     Kcod          TBEcod
024800981117     C                   movel     V1CCDD        TBEke1
024900981102     C                   WRITE     TNTBE000
025000981102      *
025100981102     c                   ELSE
025200981102     C                   UPDATE    TNTBE000
025300981102     c                   ENDIF
025400980507      *
025500980507     C                   ENDSR
025600980506      *****************************************************************
025700980506      *   ROUTINE INIZIALE
025800980506      *****************************************************************
025900980506     C     *INZSR        BEGSR
026000980506      *
026100980506     C     *ENTRY        PLIST
026200980506     C                   PARM                    KPJBA
026300981102      *
026400981102     C                   Z-ADD     1             CODUT
026500981102     C                   CALL      'X§PARUT'
026600981102     C                   PARM                    UT§DSE0F
026700981102     C                   MOVEL     RAGUT         RSUT
026800981102      *
026900981102      * Controllo Testata Tabella
027000981102     C     KTBETT        CHAIN     TNTBE000                           28
027100981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
027200981117     C                   eval      *inlr = *on
027300981102     c                   ELSE
027400981102     c                   movel     TBEsif        Ksif
027500981102     c                   movel     TBElin        Klin
027600981102     c                   ENDIF
027700981102      *
027800981102     C     KTBE          klist
027900981102     C                   KFLD                    Kcod
028000981102     C                   KFLD                    Kke1
028100981102     C                   KFLD                    Kke2
028200981102     C                   KFLD                    Klin
028300981102     C                   KFLD                    Ksif
028400981102      *
028500981102     C     KTBETT        klist
028600981102     C                   KFLD                    KTTcod
028700981102     C                   KFLD                    KTTke1
028800981102      *
028900980506     C                   ENDSR
029000980909      *****************************************************************
029100980909** MSG  (Lungh. 78)                                                          *
029200981117Immettere il Codice Documento                                                  1
029300981117Immettere la descrizione del documento                                         2
029400981117Tipo trasmissione incongruente con P.O. trasmissione                           3
