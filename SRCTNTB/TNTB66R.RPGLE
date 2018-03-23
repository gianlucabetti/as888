000100980909      *------------------------------------------------------------------------*
000200080423      * GESTIONE TABELLA DPP : Conversione e controllo peso DPD                *
000300980909      *------------------------------------------------------------------------*
000400980909
000500980909      *--------------------------------------------
000600980909      *  RIEPILOGO INDICATORI
000700980909      *--------------------------------------------
000800981102      * 01 - RECORD annullato
000900000207      * 02 - Acceso IMMISSIONE - Spento MANUTENZIONE
001000990305      * 30 - Comodo
001100080423      * 50 - Errore Peso Mancante
001200080423      * 51 - Errore Tolleranza Mancante
001300080423      * 52 - Errore
001400080423      * 53 - Errore
001500080423      * 54 - Errore
001600080423      * 55 - Errore
001700080423      * 56 - Errore
001800080423      * 57 - Errore
001900080423      * 58 - Errore
002000080423      * 59 - Errore
002100080423      * 60 - Errore
002200080423      * 61 - Errore
002300080423      * 62 - Errore
002400080423      * 63 - Errore
002500000217      * 64 - Errore
002600080423      * 65 - Errore
002700980909      *--------------------------------------------
002800980506     H DECEDIT('0,') DATEDIT(*DMY.)
002900980909
003000080423     FTNTB66D   CF   E             WORKSTN
003100981102     FTNTBE01L  UF A E           K DISK
003200000207     FTABEL00F  IF   E           K DISK
003300981102      *--------------------------------------------
003400000221     D MSG             S             78    DIM(18) CTDATA PERRCD(1)             MSG VIDEO
003500981102      *
003600981102     D Kttcod          S                   LIKE(TBEcod)
003700080423     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000DPP')
003800080423     D Kcod            S                   LIKE(TBEcod)  INZ('DPP')
003900981102     D Kke1            S                   LIKE(TBEke1)
004000981102     D Kke2            S                   LIKE(TBEke2)
004100981102     D Klin            S                   LIKE(TBElin)
004200981102     D Ksif            S                   LIKE(TBEsif)
004300000207     D Klcod           S                   LIKE(TBLcod)
004400000207     D Klkey           S                   LIKE(TBLkey)
004500981102      *
004600000207     D Wdes            s             30
004700000207     D XX              s              3  0
004800000207      *
004900980507     D KPJBA         E DS
005000080423     D DDPP          E DS
005100000204     D TIBS02DS      E DS
005200981102     D UT§DSE0F      E DS
005300980507      *--------------------------------------------
005400980506      * EMISSIONE PRIMA VIDEATA
005500981117     C                   DO        *hival
005600080423     C                   EXFMT     TN66D01
005700980930      *
005800980909     C                   clear                   V1CMSG
005900981102     C                   eval      *in28 = *off
006000991006     C                   eval      *in90 = *off
006100980506      * F3=FINE
006200981117     c   KC              leave
006300981102      *
006400981102      * CONTROLLI VIDEATA
006500981102     C                   EXSR      CTRD01
006600981102      *
006700000204     C                   IF        *in90 = *on  or  *in28 = *on
006800000204     C                   iter
006900000204     C                   ENDIF
007000980506      *
007100980909      * IMPOSTO CAMPI SECONDA VIDEATA
007200980909     C                   EXSR      RIED02
007300981117      *
007400981117      * GESTIONE SECONDA VIDEATA
007500981117     C                   EXSR      GESD02
007600981117     c                   enddo
007700981102      *
007800980506     C                   EVAL      *INLR = *ON
007900981102      *****************************************************************
008000981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
008100981102      *****************************************************************
008200981102     C     CTRD01        BEGSR
008300000204      *
008400000204      *  Ricerca
008500000204     C     '?'           SCAN      V1Cke1                                 31
008600000204      *
008700000204    1C                   IF        *IN31 = *ON
008800000204     c                   clear                   TIBS02DS
008900000204     c                   movel     'R'           t02mod
009000080423     C                   movel     'DPP'         t02cod
009100000204     c                   movel     knsif         t02sif
009200000204     C                   CALL      'TIBS02R'
009300000204     c                   parm                    KPJBA
009400000204     c                   parm                    TIBS02DS
009500000204      *
009600000204     c                   IF        t02err <> *blanks
009700000204     C                   movel     MSG(1)        V1CMSG
009800000204     C                   ELSE
009900000204     c                   eval      *in90 = *on
010000000204     C                   movel     t02ke1        V1Cke1
010100080423     c                   movel     t02uni        dDPP
010200000204     C                   ENDIF
010300000204      *
010400000204     C                   ENDIF
010500000207      *
010600000207      * Controllo chiave non deve essere blank
010700000207     C                   IF        V1Cmsg = *blanks  and  V1cke1 = *blanks
010800000207     C                   movel     MSG(1)        V1CMSG
010900000207     C                   ENDIF
011000000204      *
011100000204     c                   eval      *in28 = (V1cmsg <> *blanks)
011200000204      *
011300981102     C                   ENDSR
011400980506      *****************************************************************
011500980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
011600980506      *****************************************************************
011700980506     C     RIED02        BEGSR
011800980909      *
011900981102     c                   clear                   TNTBE000
012000080423     c                   clear                   dDPP
012100991001     c
012200980909     c                   eval      *in01 = *off
012300000204     c                   eval      KKE1 = V1Cke1
012400080423     c                   eval      KKE2 = *blank
012500980930      *
012600981102     c     KTBE          CHAIN     TNTBE000                           02
012700980506      *
012800080423     c  N02              movel     TBEuni        DDPP
012900981102     c                   eval      *in01 = (TBEatb <> *blanks)
013000980909      *
013100080423     C                   move      §DPPPKB       V2Cpes
013200080423     C                   move      §DPPPKC       V2Ctol
013300080423     C                   move      §DPPFMS       V2Cifm
013400141209     C                   move      §DPPVLB       V2Cvol
013500000207      *
013600980506     C                   ENDSR
013700981117      *****************************************************************
013800981117      *   ROUTINE PER GESTIONE SECONDA VIDEATA
013900981117      *****************************************************************
014000981117     C     GESD02        BEGSR
014100981117      *
014200981117      * EMISSIONE
014300981117     C                   DO        *HIVAL
014400080423     C                   EXFMT     TN66D02
014500981117      *
014600000204     C                   clear                   V2CMSG
014700981117     c                   eval      *in28 = *off
014800000204     c                   eval      *in90 = *off
014900000217     c     50            DO        65            XX
015000000207     c                   eval      *IN(XX) = *OFF
015100000207     c                   ENDDO
015200981117      * F3=FINE
015300981117     c   KC              leave
015400981117      * F12=RITORNO
015500981117     c   KL              leave
015600981117      *
015700981117      * CONTROLLI VIDEATA
015800000204     C                   EXSR      CTRD02
015900000204      *
016000000204     C                   IF        *in90 = *on  or  *in28 = *on
016100000204     C                   iter
016200000204     C                   ENDIF
016300981117      *
016400000204      * GESTIONE CONFERME: F5(ke)=Ripristino;F6(kf)=Conferma;F16(kq)=Annulla
016500000204     c                   IF        *inKE = *ON  or  *inKF = *ON
016600000204     c                                          or  *inKQ = *ON
016700981117     c                   exsr      RIECDD
016800981117     C                   leave
016900981117     c                   ENDIF
017000981117      *
017100981117     c                   enddo
017200981117      *
017300981117     C                   endsr
017400000204      *****************************************************************
017500000204      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
017600000204      *****************************************************************
017700000204     C     CTRD02        BEGSR
017800000207      *
017900080423      * Peso        obbligatoria
018000080423     c                   IF        V2CPES = *zeros
018100080423     C                   eval      V2Cmsg = msg(3)
018200080423     C                   eval      *in50  = *on
018300080423     C                   goto      Ectrd02
018400000204     C                   ENDIF
018500000204      *
018600080423      * Tolleranza  obbligatoria
018700080423     c                   IF        V2CTOL = *zeros
018800080423     C                   eval      V2Cmsg = msg(3)
018900080423     C                   eval      *in51  = *on
019000080423     C                   goto      Ectrd02
019100080423     C                   ENDIF
019200080423      *
019300000204     C     ECTRD02       TAG
019400000204     c                   eval      *in28 = (V2cmsg <> *blanks)
019500000204      *
019600000204     C                   ENDSR
019700980507      *****************************************************************
019800980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
019900980507      *****************************************************************
020000981117     C     RIECDD        BEGSR
020100980909      *
020200080423     C                   move      V2Cpes        §DPPPKB
020300080423     C                   move      V2Ctol        §DPPPKC
020400080423     C                   move      V2Cifm        §DPPFMS
020500141209     C                   move      V2Cvol        §DPPVLB
020600990308      *
020700080423     c                   movel(p)  DDPP          TBEuni
020800981102      *
020900000204     C                   eval      TBEftt = 'S'
021000980507      *
021100981102     C   KQ              movel     'A'           TBEatb
021200000204     C   KE              clear                   TBEatb
021300980909      *
021400981102     c                   clear                   TBEftr
021500981102      *
021600981102     c                   IF        *IN02 = *ON
021700000204     C                   clear                   TBEflt
021800991001     c                   clear                   TBEdtr
021900000204     c                   eval      TBEsif = Ksif
022000000204     c                   eval      TBElin = Klin
022100000204     c                   eval      TBEcod = Kcod
022200000204     C                   eval      TBEke1 = V1Cke1
022300080423     C                   eval      TBEke2 = *blank
022400981102     C                   WRITE     TNTBE000
022500981102      *
022600981102     c                   ELSE
022700981102     C                   UPDATE    TNTBE000
022800981102     c                   ENDIF
022900980507      *
023000980507     C                   ENDSR
023100980506      *****************************************************************
023200980506      *   ROUTINE INIZIALE
023300980506      *****************************************************************
023400980506     C     *INZSR        BEGSR
023500980506      *
023600980506     C     *ENTRY        PLIST
023700980506     C                   PARM                    KPJBA
023800981102      *
023900981102     C                   Z-ADD     1             CODUT
024000981102     C                   CALL      'X§PARUT'
024100981102     C                   PARM                    UT§DSE0F
024200981102     C                   MOVEL     RAGUT         RSUT
024300981102      *
024400981102      * Controllo Testata Tabella
024500981102     C     KTBETT        CHAIN     TNTBE000                           28
024600981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
024700981117     C                   eval      *inlr = *on
024800981102     c                   ELSE
024900981102     c                   movel     TBEsif        Ksif
025000981102     c                   movel     TBElin        Klin
025100981102     c                   ENDIF
025200981102      *
025300981102     C     KTBE          klist
025400981102     C                   KFLD                    Kcod
025500981102     C                   KFLD                    Kke1
025600981102     C                   KFLD                    Kke2
025700981102     C                   KFLD                    Klin
025800981102     C                   KFLD                    Ksif
025900981102      *
026000981102     C     KTBETT        klist
026100981102     C                   KFLD                    KTTcod
026200000207     c                   KFLD                    KTTke1
026300000207      *
026400000207     C     KTAB          KLIST
026500000207     C                   KFLD                    CODUT
026600000207     C                   KFLD                    KlCOD
026700000207     C                   KFLD                    KlKEY
026800981102      *
026900980506     C                   ENDSR
027000980909      *****************************************************************
027100000207** MSG  8E/L78                                                               *
027200080423Immettere un Tipo di Peso                                                      1
027300080423Immettere cod. aggiuntivo                                                      2
027400080423Campo Obbligatorio                                                             3
027500080423                                                                               4
027600080423                                                                               5
027700080423                                                                               6
027800080423                                                                               7
027900080423                                                                               8
028000080423                                                                               9
028100080423                                                                               10
028200080423                                                                               11
028300080423                                                                               12
028400080423                                                                               13
028500080423                                                                               8
028600080423                                                                               15
028700080423                                                                               16
028800080423                                                                               17
028900080423                                                                               18
