000100980909      *------------------------------------------------------------------------*
000200080423      * GESTIONE TABELLA MRD : Causali di Mancati ritiri  Bartolini-DPD        *
000300980909      *------------------------------------------------------------------------*
000400980909
000500980909      *--------------------------------------------
000600980909      *  RIEPILOGO INDICATORI
000700980909      *--------------------------------------------
000800981102      * 01 - RECORD annullato
000900000207      * 02 - Acceso IMMISSIONE - Spento MANUTENZIONE
001000990305      * 30 - Comodo
001100080423      * 50 - Errore Descr.Bartolini mancante
001200080423      * 51 - Errore Cod.DPD mancante
001300080423      * 52 - Errore Descr.DPD mancante
001400080423      * 53 - Errore Fase Bartolini Mancante
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
002600000217      * 65 - Errore
002700980909      *--------------------------------------------
002800980506     H DECEDIT('0,') DATEDIT(*DMY.)
002900980909
003000080423     FTNTB65D   CF   E             WORKSTN
003100981102     FTNTBE01L  UF A E           K DISK
003200000207     FTABEL00F  IF   E           K DISK
003300981102      *--------------------------------------------
003400000221     D MSG             S             78    DIM(18) CTDATA PERRCD(1)             MSG VIDEO
003500981102      *
003600981102     D Kttcod          S                   LIKE(TBEcod)
003700080423     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000MRD')
003800080423     D Kcod            S                   LIKE(TBEcod)  INZ('MRD')
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
005000080423     D DMRD          E DS
005100080423     D DCMR          E DS
005200080423     D DDIR          E DS
005300000204     D TIBS02DS      E DS
005400981102     D UT§DSE0F      E DS
005500980507      *--------------------------------------------
005600980506      * EMISSIONE PRIMA VIDEATA
005700981117     C                   DO        *hival
005800080423     C                   EXFMT     TN65D01
005900980930      *
006000980909     C                   clear                   V1CMSG
006100981102     C                   eval      *in28 = *off
006200991006     C                   eval      *in90 = *off
006300980506      * F3=FINE
006400981117     c   KC              leave
006500981102      *
006600981102      * CONTROLLI VIDEATA
006700981102     C                   EXSR      CTRD01
006800981102      *
006900000204     C                   IF        *in90 = *on  or  *in28 = *on
007000000204     C                   iter
007100000204     C                   ENDIF
007200980506      *
007300980909      * IMPOSTO CAMPI SECONDA VIDEATA
007400980909     C                   EXSR      RIED02
007500981117      *
007600981117      * GESTIONE SECONDA VIDEATA
007700981117     C                   EXSR      GESD02
007800981117     c                   enddo
007900981102      *
008000980506     C                   EVAL      *INLR = *ON
008100981102      *****************************************************************
008200981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
008300981102      *****************************************************************
008400981102     C     CTRD01        BEGSR
008500000204      *
008600000204      *  Ricerca
008700000204     C     '?'           SCAN      V1Cke1                                 31
008800000204      *
008900000204    1C                   IF        *IN31 = *ON
009000000204     c                   clear                   TIBS02DS
009100000204     c                   movel     'R'           t02mod
009200080423     C                   movel     'MRD'         t02cod
009300000204     c                   movel     knsif         t02sif
009400000204     C                   CALL      'TIBS02R'
009500000204     c                   parm                    KPJBA
009600000204     c                   parm                    TIBS02DS
009700000204      *
009800000204     c                   IF        t02err <> *blanks
009900000204     C                   movel     MSG(1)        V1CMSG
010000000204     C                   ELSE
010100000204     c                   eval      *in90 = *on
010200000204     C                   movel     t02ke1        V1Cke1
010300000204     C                   movel     t02ke2        V1Cke2
010400080423     c                   movel     t02uni        dMRD
010500000204     C                   ENDIF
010600000204      *
010700000204     C                   ENDIF
010800080423      *
010900080423      * Controlla esistenza sulla tab.CMR degli ORM Bartolini
011000080423      *
011100080423     c                   clear                   TIBS02DS
011200080423     c                   movel     'C'           t02mod
011300080423     C                   movel     'CMR'         t02cod
011400080423     c                   movel     knsif         t02sif
011500080423     C                   movel     V1Cke1        T02ke1
011600080423     C                   CALL      'TIBS02R'
011700080423     c                   parm                    KPJBA
011800080423     c                   parm                    TIBS02DS
011900080423     c                   IF        t02err <> *blanks
012000080423     C                   movel     MSG(1)        V1CMSG
012100080423     C                   ENDIF
012200080423      *
012300000207      *
012400080423      * Controlli V1cke2: nessun controllo
012500000207     C                   SELECT
012600000207     C                   WHEN       V1Cmsg <> *blanks
012700000207     C                   WHEN       V1Cke2 = *blanks
012800000207     C                   OTHER
012900000207     C                   ENDSL
013000000207      *
013100000207      * Controllo chiave non deve essere blank
013200000207     C                   IF        V1Cmsg = *blanks  and  V1cke1 = *blanks
013300000207     C                                               and  V1Cke2 = *blanks
013400000207     C                   movel     MSG(1)        V1CMSG
013500000207     C                   ENDIF
013600000204      *
013700000204     c                   eval      *in28 = (V1cmsg <> *blanks)
013800000204      *
013900981102     C                   ENDSR
014000980506      *****************************************************************
014100980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
014200980506      *****************************************************************
014300980506     C     RIED02        BEGSR
014400980909      *
014500981102     c                   clear                   TNTBE000
014600080423     c                   clear                   dMRD
014700991001     c
014800980909     c                   eval      *in01 = *off
014900000204     c                   eval      KKE1 = V1Cke1
015000000204     c                   eval      KKE2 = V1Cke2
015100980930      *
015200981102     c     KTBE          CHAIN     TNTBE000                           02
015300980506      *
015400080423     c  N02              movel     TBEuni        Dmrd
015500981102     c                   eval      *in01 = (TBEatb <> *blanks)
015600980909      *
015700080423     C                   eval      V2Cdes = §MRDDES
015800080423     C                   move      §MRDCAR       V2Cdpd
015900080423     C                   eval      V2ddpd = §MRDDESD
016000080423     C                   move      §MRDFAR       V2Cfas
016100000207      *
016200980506     C                   ENDSR
016300981117      *****************************************************************
016400981117      *   ROUTINE PER GESTIONE SECONDA VIDEATA
016500981117      *****************************************************************
016600981117     C     GESD02        BEGSR
016700981117      *
016800981117      * EMISSIONE
016900981117     C                   DO        *HIVAL
017000080423     C                   EXFMT     TN65D02
017100981117      *
017200000204     C                   clear                   V2CMSG
017300981117     c                   eval      *in28 = *off
017400000204     c                   eval      *in90 = *off
017500000217     c     50            DO        65            XX
017600000207     c                   eval      *IN(XX) = *OFF
017700000207     c                   ENDDO
017800981117      * F3=FINE
017900981117     c   KC              leave
018000981117      * F12=RITORNO
018100981117     c   KL              leave
018200981117      *
018300981117      * CONTROLLI VIDEATA
018400000204     C                   EXSR      CTRD02
018500000204      *
018600000204     C                   IF        *in90 = *on  or  *in28 = *on
018700000204     C                   iter
018800000204     C                   ENDIF
018900981117      *
019000000204      * GESTIONE CONFERME: F5(ke)=Ripristino;F6(kf)=Conferma;F16(kq)=Annulla
019100000204     c                   IF        *inKE = *ON  or  *inKF = *ON
019200000204     c                                          or  *inKQ = *ON
019300981117     c                   exsr      RIECDD
019400981117     C                   leave
019500981117     c                   ENDIF
019600981117      *
019700981117     c                   enddo
019800981117      *
019900981117     C                   endsr
020000000204      *****************************************************************
020100000204      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
020200000204      *****************************************************************
020300000204     C     CTRD02        BEGSR
020400000207      *
020500080423      * Descrizione obbligatoria
020600080423     c                   IF        V2CDES = *blanks
020700080423     C********           eval      V2Cmsg = msg(3)
020800080423     C********           eval      *in50  = *on
020900080423     C********           goto      Ectrd02
021000080423     c********           ELSE
021100080423      ********
021200080423     c                   clear                   TIBS02DS
021300080423     c                   movel     'C'           t02mod
021400080423     C                   movel     'CMR'         t02cod
021500080423     c                   movel     knsif         t02sif
021600080423     C                   movel     V1Cke1        T02ke1
021700080423     C                   CALL      'TIBS02R'
021800080423     c                   parm                    KPJBA
021900080423     c                   parm                    TIBS02DS
022000080423     c                   IF        t02err = *blanks
022100080423     c                   movel     t02uni        Dcmr
022200080423     c                   movel     D§CMRDES      V2cdes
022300080423     C                   ENDIF
022400080423      *
022500000204     C                   ENDIF
022600000204      *
022700080423      * Codice DPD  obbligatorio
022800080423     c                   IF        V2Cdpd = *zeros
022900080423     C                   eval      V2Cmsg = msg(3)
023000080423     C                   eval      *in51  = *on
023100080423     C                   goto      Ectrd02
023200080423     C                   Else
023300080423     c                   clear                   TIBS02DS
023400080423     c                   movel     'C'           t02mod
023500080423     C                   movel     'DIR'         t02cod
023600080423     c                   movel     knsif         t02sif
023700080423     C                   movel     V2Cdpd        T02ke2
023800080423     C                   CALL      'TIBS02R'
023900080423     c                   parm                    KPJBA
024000080423     c                   parm                    TIBS02DS
024100080423     c                   IF        t02err <>*blanks
024200080423     C                   eval      V2Cmsg = msg(5)
024300080423     C                   eval      *in51  = *on
024400080423     C                   goto      Ectrd02
024500080423     c                   End
024600080423     c                   IF        t02err = *blanks and V2ddpd = *blank
024700080423     c                   movel     t02uni        Ddir
024800080423     c                   movel     §DIRDES       V2ddpd
024900080423     C                   ENDIF
025000080423     C                   ENDIF
025100080423      *
025200080423      * Descrizione obbligatoria
025300080423     c                   IF        V2ddpd = *blanks
025400080423     C                   eval      V2Cmsg = msg(3)
025500080423     C                   eval      *in52  = *on
025600080423     C                   goto      Ectrd02
025700080423     C                   ENDIF
025800080423      *
025900080423      * Fase        obbligatoria
026000080423     c******             IF        V2cfas = *zeros
026100080423     C******             eval      V2Cmsg = msg(3)
026200080423     C******             eval      *in53  = *on
026300080423     C******             goto      Ectrd02
026400080423     C******             ENDIF
026500080423      *
026600000204     C     ECTRD02       TAG
026700000204     c                   eval      *in28 = (V2cmsg <> *blanks)
026800000204      *
026900000204     C                   ENDSR
027000980507      *****************************************************************
027100980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
027200980507      *****************************************************************
027300981117     C     RIECDD        BEGSR
027400980909      *
027500080423     C                   eval      §MRDDES = V2Cdes
027600080423     C                   move      V2Cdpd        §MRDCAR
027700080423     C                   eval      §MRDDESD= V2ddpd
027800080423     C                   move      V2Cfas        §MRDFAR
027900990308      *
028000080423     c                   movel(p)  Dmrd          TBEuni
028100981102      *
028200000204     C                   eval      TBEftt = 'S'
028300980507      *
028400981102     C   KQ              movel     'A'           TBEatb
028500000204     C   KE              clear                   TBEatb
028600980909      *
028700981102     c                   clear                   TBEftr
028800981102      *
028900981102     c                   IF        *IN02 = *ON
029000000204     C                   clear                   TBEflt
029100991001     c                   clear                   TBEdtr
029200000204     c                   eval      TBEsif = Ksif
029300000204     c                   eval      TBElin = Klin
029400000204     c                   eval      TBEcod = Kcod
029500000204     C                   eval      TBEke1 = V1Cke1
029600000204     C                   eval      TBEke2 = V1Cke2
029700981102     C                   WRITE     TNTBE000
029800981102      *
029900981102     c                   ELSE
030000981102     C                   UPDATE    TNTBE000
030100981102     c                   ENDIF
030200980507      *
030300980507     C                   ENDSR
030400980506      *****************************************************************
030500980506      *   ROUTINE INIZIALE
030600980506      *****************************************************************
030700980506     C     *INZSR        BEGSR
030800980506      *
030900980506     C     *ENTRY        PLIST
031000980506     C                   PARM                    KPJBA
031100981102      *
031200981102     C                   Z-ADD     1             CODUT
031300981102     C                   CALL      'X§PARUT'
031400981102     C                   PARM                    UT§DSE0F
031500981102     C                   MOVEL     RAGUT         RSUT
031600981102      *
031700981102      * Controllo Testata Tabella
031800981102     C     KTBETT        CHAIN     TNTBE000                           28
031900981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
032000981117     C                   eval      *inlr = *on
032100981102     c                   ELSE
032200981102     c                   movel     TBEsif        Ksif
032300981102     c                   movel     TBElin        Klin
032400981102     c                   ENDIF
032500981102      *
032600981102     C     KTBE          klist
032700981102     C                   KFLD                    Kcod
032800981102     C                   KFLD                    Kke1
032900981102     C                   KFLD                    Kke2
033000981102     C                   KFLD                    Klin
033100981102     C                   KFLD                    Ksif
033200981102      *
033300981102     C     KTBETT        klist
033400981102     C                   KFLD                    KTTcod
033500000207     c                   KFLD                    KTTke1
033600000207      *
033700000207     C     KTAB          KLIST
033800000207     C                   KFLD                    CODUT
033900000207     C                   KFLD                    KlCOD
034000000207     C                   KFLD                    KlKEY
034100981102      *
034200980506     C                   ENDSR
034300980909      *****************************************************************
034400000207** MSG  8E/L78                                                               *
034500110512Immettere un  Cod.Mancato Ritiro BRT presente su tab.CMR                       1
034600080423Immettere cod. aggiuntivo                                                      2
034700080423Campo Obbligatorio                                                             3
034800080423Codice Errato o non presente in tabella CMR                                    4
034900080423Codice Errato o non presente in tabella DIR                                    5
035000080423                                                                               6
035100080423                                                                               7
035200080423                                                                               8
035300080423                                                                               9
035400080423                                                                               10
035500080423                                                                               11
035600080423                                                                               12
035700080423                                                                               13
035800080423                                                                               8
035900080423                                                                               15
036000080423                                                                               16
036100080423                                                                               17
036200080423                                                                               18
