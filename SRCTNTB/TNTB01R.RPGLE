000100980909      *------------------------------------------------------------------------*
000200981102      * GESTIONE TABELLA TIPO ANOMALIA                                         *
000300980909      *------------------------------------------------------------------------*
000400980909
000500980909      *--------------------------------------------
000600980909      *  RIEPILOGO INDICATORI
000700980909      *--------------------------------------------
000800981102      * 01 - RECORD annullato
000900981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001000980909      *--------------------------------------------
001100980909
001200980506     H DECEDIT('0,') DATEDIT(*DMY.)
001300980909
001400981102     FTNTB01D   CF   E             WORKSTN
001500981102     FTNTBE01L  UF A E           K DISK
001600980909
001700981102      *--------------------------------------------
001800981102     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
001900981102      *
002000981102     D Kttcod          S                   LIKE(TBEcod)
002100981102     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000TAD')
002200981102     D Kcod            S                   LIKE(TBEcod)  INZ('TAD')
002300981102     D Kke1            S                   LIKE(TBEke1)
002400981102     D Kke2            S                   LIKE(TBEke2)
002500981102     D Klin            S                   LIKE(TBElin)
002600981102     D Ksif            S                   LIKE(TBEsif)
002700981102     D Wtibs02r        S              1
002800981102      *
002900980507     D KPJBA         E DS
003000981102     D DTAD          E DS
003100981102     D UT§DSE0F      E DS
003200981102     D TIBS02DS      E DS
003300981102      *
003400981102     D                SDS
003500981102     D  VTCPGM                 1     10
003600980507      *--------------------------------------------
003700980909
003800980506      * EMISSIONE PRIMA VIDEATA
003900980506     C     EMD01         TAG
004000981102     C                   EXFMT     TN01D01
004100980930      *
004200980909     C                   clear                   V1CMSG
004300981102     C                   eval      *in28 = *off
004400980930      *
004500980506      * F3=FINE
004600980909     c   KC              goto      fine
004700981102      *
004800981102      * CONTROLLI VIDEATA
004900981102     C                   EXSR      CTRD01
005000981102      *
005100981102     C   28              goto      emd01
005200980506      *
005300980909      * IMPOSTO CAMPI SECONDA VIDEATA
005400980909     C                   EXSR      RIED02
005500980507      *
005600980507      * -------------------  GESTIONE SECONDA VIDEATA --------------------------
005700980506      *
005800980507      * EMISSIONE
005900980506     C     EMD02         TAG
006000981102     C                   EXFMT     TN01D02
006100980909      *
006200980506     C                   CLEAR                   V2CMSG
006300981102     c                   eval      *in28 = *off
006400981102     C                   setoff                                       404142
006500980506      *
006600980506      * F3=FINE
006700980909     c   KC              goto      fine
006800981102      *
006900981102      * F12=RITORNO
007000981102     c   KL              goto      emd01
007100980507      *
007200980506      * CONTROLLI VIDEATA
007300980506     C                   EXSR      CTRD02
007400980717      * errori
007500980909     C   28              GOTO      EMD02
007600980909      *
007700980909      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma; F16(kq)=Annullamento
007800980909     c                   IF        *inKE = *ON  or  *inKF = *ON  or  *inKQ = *ON
007900981102     c                   exsr      RIETAD
008000980909     C                   goto      EMD01
008100980909     c                   ENDIF
008200980909      *
008300980909     C                   goto      EMD02
008400980506      *
008500980506     C     FINE          TAG
008600981102      *
008700981102     c                   IF        Wtibs02r = 'S'
008800981102     c                   clear                   tibs02ds
008900981102     c                   movel     'C'           t02tla
009000981102     c                   call      'TIBS02R'
009100981102     c                   parm                    kpjba
009200981102     c                   parm                    tibs02ds
009300981102     c                   ENDIF
009400981102      *
009500980506     C                   EVAL      *INLR = *ON
009600981102      *****************************************************************
009700981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
009800981102      *****************************************************************
009900981102     C     CTRD01        BEGSR
010000981102      *
010100981102     c                   clear                   TIBS02DS
010200981102      *
010300981102     C     '?'           SCAN      V1CTAD                                 30
010400981102      *  Ricerca
010500981102    1C                   IF        *IN30 = *ON
010600981102     c                   eval      WTIBS02R = 'S'
010700981102     c                   movel     'R'           t02mod
010800981102     c                   movel     knsif         t02sif
010900981102     C                   movel     Kcod          t02cod
011000981102     C                   CALL      'TIBS02R'
011100981102     c                   parm                    KPJBA
011200981102     c                   parm                    TIBS02DS
011300981102      *
011400981102    2C                   IF        T02err = *blanks
011500981102     C                   movel     T02ke1        V1Ctad
011600981102     C                   ENDIF
011700981102     C                   ENDIF
011800981102      *
011900981102      *   Controlli di validità'
012000981102    2C                   IF        V1CTAD = *blanks  or  T02err <> *blanks
012100981102     C                   movel     MSG(1)        V1CMSG
012200981102     c                   eval      *in28 = *on
012300981102     C                   ENDIF
012400981102      *
012500981102     C                   ENDSR
012600980506      *****************************************************************
012700980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
012800980506      *****************************************************************
012900980506     C     RIED02        BEGSR
013000980909      *
013100981102     c                   clear                   TNTBE000
013200981102     c                   clear                   DTAD
013300980909     c                   eval      *in01 = *off
013400981102     c                   movel(P)  V1Ctad        Kke1
013500981102     c                   movel     V1Ctad        V2Ctad
013600980930      *
013700981102     c     KTBE          CHAIN     TNTBE000                           02
013800980506      *
013900981102     c  N02              movel     TBEuni        DTAD
014000981102     c                   eval      *in01 = (TBEatb <> *blanks)
014100980909      *
014200981102     C                   movel     V1Ctad        V2Ctad
014300981102     C                   movel     §TADdesc      V2Cdesc
014400981102     C                   movel     §TADragr      V2Cragr
014500981102     C                   movel     §TADdagg      V2Cdagg
014600981102     C                   movel     §TADmtvd      V2Cmtvd
014700981102     C                   movel     §TADdcdn      V2Cdcdn
014800981102     C                   movel     §TADtico      V2Ctico
014900981102     C                   movel     §TADfddt      V2Cfddt
015000981102     C                   movel     §TADfmed      V2Cfmed
015100981102     C                   movel     §TADpzdo      V2Cpzdo
015200981102     C                   movel     §TADkgdo      V2Ckgdo
015300981102     C                   movel     §TADevfo      V2Cevfo
015400981102     C                   movel     §TADimpr      V2Cimpr
015500981102     C                   movel     §TADtisi      V2Ctisi
015600990118     C                   movel     §TADdest      V2Cdest
015700990126     C                   movel     §TADritr      V2Critr
015800990302     C                   movel     §TADdesi      V2Cdesi
015900990304     C                   movel     §TADcare      V2Ccare
016000990323     C                   movel     §TADcidd      V2Ccidd
016100981102     C                   movel     tbeftt        V2Cftt
016200981102     C                   z-add     tbeflt        V2Cflt
016300980506      *
016400980506     C                   ENDSR
016500980506      *****************************************************************
016600980506      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
016700980506      *****************************************************************
016800980506     C     CTRD02        BEGSR
016900980507      *
017000981102      *  Descrizione obbligatoria
017100981102     c                   if        V2Cdesc = *blanks
017200981102     c                   seton                                        2840
017300981102     C                   movel     MSG(2)        V2Cmsg
017400980909     C                   ENDIF
017500981102      *
017600981102      *  Evento fortuito SI solo se ragruppamento Evento
017700981102     c                   if        V2Cevfo = 'S'  and  V2Cragr <> 'E'
017800981102     c                   seton                                        2841
017900981102     C                   movel     MSG(3)        V2Cmsg
018000981102     C                   ENDIF
018100981102      *
018200981102      *  Tipo trasmissione
018300981102     c                   if        V2Cftt = *blanks  and  V2Cflt <> *zeros
018400981102     c                   seton                                        2842
018500981102     C                   movel     MSG(4)        V2Cmsg
018600981102     C                   ENDIF
018700980909      *
018800980909     C                   ENDSR
018900980507      *****************************************************************
019000980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
019100980507      *****************************************************************
019200981102     C     RIETAD        BEGSR
019300980909      *
019400981102     C                   movel     V2Cdesc       §TADdesc
019500981102     C                   movel     V2Cragr       §TADragr
019600981102     C                   movel     V2Cdagg       §TADdagg
019700981102     C                   movel     V2Cmtvd       §TADmtvd
019800981102     C                   movel     V2Cdcdn       §TADdcdn
019900981102     C                   movel     V2Ctico       §TADtico
020000981102     C                   movel     V2Cfddt       §TADfddt
020100981102     C                   movel     V2Cfmed       §TADfmed
020200981102     C                   movel     V2Cpzdo       §TADpzdo
020300981102     C                   movel     V2Ckgdo       §TADkgdo
020400981102     C                   movel     V2Cevfo       §TADevfo
020500981102     C                   movel     V2Cimpr       §TADimpr
020600981102     C                   movel     V2Ctisi       §TADtisi
020700990118     C                   movel     V2Cdest       §TADdest
020800990126     C                   movel     V2Critr       §TADritr
020900990302     C                   movel     V2Cdesi       §TADdesi
021000990304     C                   movel     V2Ccare       §TADcare
021100990323     C                   movel     V2Ccidd       §TADcidd
021200981102      *
021300981102     c                   movel     DTAD          TBEuni
021400981102      *
021500981102     C                   movel     V2Cftt        TBEftt
021600981102     C                   z-add     V2Cflt        TBEflt
021700980507      *
021800981102     C   KQ              movel     'A'           TBEatb
021900981102     C   KE              movel     *blanks       TBEatb
022000980909      *
022100981102     c                   clear                   TBEftr
022200981102     c                   clear                   TBEdtr
022300981102      *
022400981102     c                   IF        *IN02 = *ON
022500981102     c                   movel     Ksif          TBEsif
022600981102     c                   movel     Klin          TBElin
022700981102     c                   movel     Kcod          TBEcod
022800981102     C                   movel     V1Ctad        TBEke1
022900981102     C                   WRITE     TNTBE000
023000981102      *
023100981102     c                   ELSE
023200981102     C                   UPDATE    TNTBE000
023300981102     c                   ENDIF
023400980507      *
023500980507     C                   ENDSR
023600980506      *****************************************************************
023700980506      *   ROUTINE INIZIALE
023800980506      *****************************************************************
023900980506     C     *INZSR        BEGSR
024000980506      *
024100980506     C     *ENTRY        PLIST
024200980506     C                   PARM                    KPJBA
024300981102      *
024400981102     C                   Z-ADD     1             CODUT
024500981102     C                   CALL      'X§PARUT'
024600981102     C                   PARM                    UT§DSE0F
024700981102     C                   MOVEL     RAGUT         RSUT
024800981102      *
024900981102      * Controllo Testata Tabella
025000981102     C     KTBETT        CHAIN     TNTBE000                           28
025100981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
025200981102     C                   goto      fine
025300981102     c                   ELSE
025400981102     c                   movel     TBEsif        Ksif
025500981102     c                   movel     TBElin        Klin
025600981102     c                   ENDIF
025700981102      *
025800981102     C     KTBE          klist
025900981102     C                   KFLD                    Kcod
026000981102     C                   KFLD                    Kke1
026100981102     C                   KFLD                    Kke2
026200981102     C                   KFLD                    Klin
026300981102     C                   KFLD                    Ksif
026400981102      *
026500981102     C     KTBETT        klist
026600981102     C                   KFLD                    KTTcod
026700981102     C                   KFLD                    KTTke1
026800981102      *
026900980506     C                   ENDSR
027000980909      *****************************************************************
027100980909** MSG  (Lungh. 78)                                                          *
027200981102Immettere il Tipo Anomalia                                                     1
027300981102Immettere la descrizione del tipo anomalia                                     2
027400981102Evento fortuito valido solo per Ragruppamento Eventi                           3
027500981102Tipo trasmissione incongruente con P.O. trasmissione                           4
