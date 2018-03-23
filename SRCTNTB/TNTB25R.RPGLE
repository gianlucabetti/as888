000100980909      *------------------------------------------------------------------------*
000200020227      * GESTIONE TABELLA definizioni x Fedex                                   *
000300980909      *------------------------------------------------------------------------*
000400980909
000500980909      *--------------------------------------------
000600980909      *  RIEPILOGO INDICATORI
000700980909      *--------------------------------------------
000800981102      * 01 - RECORD annullato
000900981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001000990305      * 30 - Comodo
001100980909      *--------------------------------------------
001200980909
001300980506     H DECEDIT('0,') DATEDIT(*DMY.)
001400980909
001500020227     FTNTB25D   CF   E             WORKSTN
001600981102     FTNTBE01L  UF A E           K DISK
001700980909
001800981102      *--------------------------------------------
001900020227     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
002000981102      *
002100981102     D Kttcod          S                   LIKE(TBEcod)
002200020227     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000FED')
002300020227     D Kcod            S                   LIKE(TBEcod)  INZ('FED')
002400981102     D Kke1            S                   LIKE(TBEke1)
002500981102     D Kke2            S                   LIKE(TBEke2)
002600981102     D Klin            S                   LIKE(TBElin)
002700981102     D Ksif            S                   LIKE(TBEsif)
002800020227     D Wtibs02r        S              1
002900990305     D* schiera tipi anomalie
003000990308     D WTA             S              1    DIM(3)
003100981102      *
003200980507     D KPJBA         E DS
003300020227     D DFEDnm        E DS
003400020227     D DFEDcd        E DS
003500981102     D UT§DSE0F      E DS
003600020227     D TIBS02DS      E DS
003700981102      *
003800021028     D SCAN01DS        DS
003900021028     D  V4CDE1                 1     25
004000021028     D  V4CDE2                26     50
004100021028     D  V4CDE3                51     75
004200021028     D  V4CDE4                76    100
004300021028     D  V4CDE5               101    125
004400021028      *
004500020227     D                SDS
004600981102     D  VTCPGM                 1     10
004700020227      *=============================================================
004800020227      *  CHIAVI DEI RECORDS di TABELLA
004900020227      *=============================================================
005000020227
005100020227     D Responsabile    C                   CONST('NOME RESP.     ')
005200111102     D Codice          C                   CONST('COD.BARTOLINI  ')
005300021028     D Scan_01         C                   CONST('SCAN.01        ')
005400020227
005500020227      *=============================================================
005600980506      * EMISSIONE PRIMA VIDEATA
005700981117     C                   DO        *hival
005800020227     c                   clear                   v1cfed
005900020227     c                   movel     '?'           v1cfed
006000020227      *
006100020227      * non è possibile immettere
006200020227      *
006300020227     C                   EXFMT     TN25D01
006400020227      *
006500980909     C                   clear                   V1CMSG
006600981102     C                   eval      *in28 = *off
006700980506      * F3=FINE
006800981117     c   KC              leave
006900981102      *
007000981102      * CONTROLLI VIDEATA
007100981102     C                   EXSR      CTRD01
007200981102      *
007300981117     C   28              iter
007400980506      *
007500020227     c                   select
007600020227     c     v1cfed        wheneq    responsabile
007700020227      * 2° video
007800020227     C                   EXSR      RIED02
007900020227     C                   EXSR      GESD02
008000020227      *
008100020227     c     v1cfed        wheneq    codice
008200020227      * 3° video
008300020227     C                   EXSR      RIED03
008400020227     C                   EXSR      GESD03
008500021028      *
008600021028     c     v1cfed        wheneq    Scan_01
008700021028      * 4° video
008800021028     C                   EXSR      RIED04
008900021028     C                   EXSR      GESD04
009000020227      *
009100020227     C                   ENDsl
009200980507      *
009300981117     c                   enddo
009400980506      *
009500020227     c                   IF        Wtibs02r = 'S'
009600020227     c                   clear                   tibs02ds
009700020227     c                   movel     'C'           t02tla
009800020227     c                   call      'TIBS02R'
009900981102     c                   parm                    kpjba
010000020227     c                   parm                    tibs02ds
010100981102     c                   ENDIF
010200981102      *
010300980506     C                   EVAL      *INLR = *ON
010400981102      *****************************************************************
010500981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
010600981102      *****************************************************************
010700981102     C     CTRD01        BEGSR
010800981102      *
010900020227     c                   clear                   TIBS02DS
011000020227     c                   clear                   video             1
011100981102      *
011200020227     C     '?'           SCAN      V1Cfed                                 30
011300981102      *  Ricerca
011400981102    1C                   IF        *IN30 = *ON
011500020227     c                   eval      WTIBS02R = 'S'
011600020227     c                   movel     'R'           t02mod
011700020227     c                   movel     knsif         t02sif
011800020227     C                   movel     Kcod          t02cod
011900020227     C                   CALL      'TIBS02R'
012000981102     c                   parm                    KPJBA
012100020227     c                   parm                    TIBS02DS
012200981102      *
012300020227    2C                   IF        T02err = *blanks
012400020227     C                   movel     T02ke1        V1Cfed
012500981102     C                   ENDIF
012600981102     C                   ENDIF
012700981102      *
012800981102      *   Controlli di validità'
012900020227    2C                   IF        V1Cfed = *blanks  or  T02err <> *blanks
013000981102     C                   movel     MSG(1)        V1CMSG
013100981102     c                   eval      *in28 = *on
013200981102     C                   ENDIF
013300981102      *
013400981102     C                   ENDSR
013500980506      *****************************************************************
013600980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
013700980506      *****************************************************************
013800980506     C     RIED02        BEGSR
013900980909      *
014000981102     c                   clear                   TNTBE000
014100020227     c                   clear                   DFEDnm
014200980909     c                   eval      *in01 = *off
014300020227     c                   movel(P)  V1Cfed        Kke1
014400020227     c                   movel     V1Cfed        V2Cfed
014500990712     c                   movel     'S'           V2Cftt
014600980930      *
014700981102     c     KTBE          CHAIN     TNTBE000                           02
014800990712     C  n02              movel     tbeftt        V2Cftt
014900990712     C  n02              z-add     tbeflt        V2Cflt
015000980506      *
015100020227     c  N02              movel     TBEuni        DFEDnm
015200981102     c                   eval      *in01 = (TBEatb <> *blanks)
015300980909      *
015400020227     C                   movel     V1Cfed        V2Cfed
015500020227     C                   movel     §FEDresp      V2Cdes
015600020227     C                   movel     §FEDtel       V2Ctel
015700980506      *
015800980506     C                   ENDSR
015900981117      *****************************************************************
016000981117      *   ROUTINE PER GESTIONE SECONDA VIDEATA
016100981117      *****************************************************************
016200981117     C     GESD02        BEGSR
016300981117      *
016400981117      * EMISSIONE
016500981117     C                   DO        *HIVAL
016600020227     C                   EXFMT     TN25D02
016700981117      *
016800981117     C                   CLEAR                   V2CMSG
016900981117     c                   eval      *in28 = *off
017000981117      * F3=FINE
017100981117     c   KC              leave
017200981117      * F12=RITORNO
017300981117     c   KL              leave
017400981117      *
017500981117      * CONTROLLI VIDEATA
017600981117     C                   EXSR      CTRD02
017700981117      * errori
017800981117     C   28              iter
017900981117      *
018000020227      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma;
018100020227     c                   IF        *inKE = *ON  or  *inKF = *ON
018200020227     c************                              or  *inKQ = *ON
018300020227     c                   exsr      RIEFED
018400981117     C                   leave
018500981117     c                   ENDIF
018600981117      *
018700981117     c                   enddo
018800981117      *
018900981117     C                   endsr
019000980506      *****************************************************************
019100980506      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
019200980506      *****************************************************************
019300980506     C     CTRD02        BEGSR
019400980507      *
019500981102      *  Descrizione obbligatoria
019600981117     c                   if        V2Cdes = *blanks
019700981102     c                   seton                                        2840
019800981102     C                   movel     MSG(2)        V2Cmsg
019900020227     c                   goto      endct2
020000980909     C                   ENDIF
020100981102      *
020200020227      *  Telefono    obbligatorio
020300020227     c                   if        V2Ctel = *blanks
020400020227     c                   seton                                        2841
020500020227     C                   movel     MSG(3)        V2Cmsg
020600020227     c                   goto      endct2
020700020227     C                   ENDIF
020800980909      *
020900020227     C     endct2        ENDSR
021000020227      *****************************************************************
021100020227      *   ROUTINE PER CARICAMENTO terza   VIDEATA DA FILE
021200020227      *****************************************************************
021300020227     C     RIED03        BEGSR
021400020227      *
021500020227     c                   clear                   TNTBE000
021600020227     c                   clear                   DFEDcd
021700020227     c                   eval      *in01 = *off
021800020227     c                   movel(P)  V1Cfed        Kke1
021900020227     c                   movel     V1Cfed        V3Cfed
022000020227     c                   movel     'S'           V3Cftt
022100020227      *
022200020227     c     KTBE          CHAIN     TNTBE000                           02
022300020227     C  n02              movel     tbeftt        V3Cftt
022400020227     C  n02              z-add     tbeflt        V3Cflt
022500020227      *
022600020227     c  N02              movel     TBEuni        DFEDcd
022700020227     c                   eval      *in01 = (TBEatb <> *blanks)
022800020227      *
022900020227     C                   movel     V1Cfed        V3Cfed
023000020227     C                   movel     §FEDcodx      V3Ccod
023100020418     C                   movel     §EURcodx      V3Cine
023200020227      *
023300020227     C                   ENDSR
023400020227      *****************************************************************
023500020227      *   ROUTINE PER GESTIONE terza   VIDEATA
023600020227      *****************************************************************
023700020227     C     GESD03        BEGSR
023800020227      *
023900020227      * EMISSIONE
024000020227     C                   DO        *HIVAL
024100020227     C                   EXFMT     TN25D03
024200020227      *
024300020227     C                   CLEAR                   V3CMSG
024400020227     c                   eval      *in28 = *off
024500020227      * F3=FINE
024600020227     c   KC              leave
024700020227      * F12=RITORNO
024800020227     c   KL              leave
024900020227      *
025000020227      * CONTROLLI VIDEATA
025100020227     C                   EXSR      CTRD03
025200020227      * errori
025300020227     C   28              iter
025400020227      *
025500020227      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma;
025600020227     c                   IF        *inKE = *ON  or  *inKF = *ON
025700020227     c********                                  or  *inKQ = *ON
025800020227     c                   exsr      RIEFED
025900020227     C                   leave
026000020227     c                   ENDIF
026100020227      *
026200020227     c                   enddo
026300020227      *
026400020227     C                   endsr
026500020227      *****************************************************************
026600020227      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
026700020227      *****************************************************************
026800020227     C     CTRD03        BEGSR
026900020227      *
027000020227      *  Codice Bartolini x Fedex obbligatorio
027100020227     c                   if        V3Ccod = *blanks
027200020227     c                   seton                                        2840
027300020227     C                   movel     MSG(4)        V3Cmsg
027400020227     c                   goto      endct3
027500020227     C                   ENDIF
027600020227      *
027700020227     C     endct3        ENDSR
027800021028      *****************************************************************
027900021028      *   ROUTINE PER CARICAMENTO quarta  VIDEATA DA FILE
028000021028      *****************************************************************
028100021028     C     RIED04        BEGSR
028200021028      *
028300021028     c                   clear                   TNTBE000
028400021028     c                   clear                   DFEDcd
028500021028     c                   eval      *in01 = *off
028600021028     c                   movel(P)  V1Cfed        Kke1
028700021028     c                   movel     V1Cfed        V4Cfed
028800021028     c                   movel     'S'           V4Cftt
028900021028      *
029000021028     c     KTBE          CHAIN     TNTBE000                           02
029100021028     C  n02              movel     tbeftt        V4Cftt
029200021028     C  n02              z-add     tbeflt        V4Cflt
029300021028      *
029400021028     c  N02              movel     TBEuni        SCAN01DS
029500021028     c                   eval      *in01 = (TBEatb <> *blanks)
029600021028      *
029700021028     C                   movel     V1Cfed        V4Cfed
029800021028      *
029900021028     C                   ENDSR
030000021028      *****************************************************************
030100021028      *   ROUTINE PER GESTIONE quarta  VIDEATA
030200021028      *****************************************************************
030300021028     C     GESD04        BEGSR
030400021028      *
030500021028      * EMISSIONE
030600021028     C                   DO        *HIVAL
030700021028     C                   EXFMT     TN25D04
030800021028      *
030900021028     C                   CLEAR                   V4CMSG
031000021028     c                   eval      *in28 = *off
031100021028      * F3=FINE
031200021028     c   KC              leave
031300021028      * F12=RITORNO
031400021028     c   KL              leave
031500021028      *
031600021028      * CONTROLLI VIDEATA
031700021028     C                   EXSR      CTRD04
031800021028      * errori
031900021028     C   28              iter
032000021028      *
032100021028      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma;
032200021028     c                   IF        *inKE = *ON  or  *inKF = *ON
032300021028     c********                                  or  *inKQ = *ON
032400021028     c                   exsr      RIEFED
032500021028     C                   leave
032600021028     c                   ENDIF
032700021028      *
032800021028     c                   enddo
032900021028      *
033000021028     C                   endsr
033100021028      *****************************************************************
033200021028      *   ROUTINE PER CONTROLLO quarta  VIDEATA
033300021028      *****************************************************************
033400021028     C     CTRD04        BEGSR
033500021028      *
033600021028      *  Codice Bartolini x Fedex obbligatorio
033700021028     c                   if        v4Cde1 = *blanks
033800021028     c                   seton                                        2840
033900021028     C                   movel     MSG(4)        V4Cmsg
034000021028     c                   goto      endct4
034100021028     C                   ENDIF
034200021028      *
034300021028     C     endct4        ENDSR
034400980507      *****************************************************************
034500980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
034600980507      *****************************************************************
034700020227     C     RIEFED        BEGSR
034800020227      *
034900020227     c                   select
035000020227     c     v1cfed        wheneq    responsabile
035100980909      *
035200020227     C                   movel     V2Cdes        §FEDresp
035300020227     C                   movel     V2Ctel        §FEDtel
035400020227     c                   movel     DFEDnm        TBEuni
035500020227      *
035600020227     C                   movel     V2Cftt        TBEftt
035700020227     C                   z-add     V2Cflt        TBEflt
035800020227      *
035900020227     c     v1cfed        wheneq    codice
036000990308      *
036100020227     C                   movel     V3Ccod        §FEDcodx
036200020418     C                   movel     V3Cine        §EURcodx
036300020227     c                   movel     DFEDcd        TBEuni
036400020227      *
036500020227     C                   movel     V3Cftt        TBEftt
036600020227     C                   z-add     V3Cflt        TBEflt
036700021028      *
036800021028     c     v1cfed        wheneq    Scan_01
036900021028      *
037000021028     c                   movel     SCAN01DS      TBEuni
037100021028      *
037200021028     C                   movel     V4Cftt        TBEftt
037300021028     C                   z-add     V4Cflt        TBEflt
037400980507      *
037500020227     c                   endsl
037600020227      *
037700020227     C** KQ              movel     'A'           TBEatb
037800981102     C   KE              movel     *blanks       TBEatb
037900980909      *
038000981102     c                   clear                   TBEftr
038100981102     c                   clear                   TBEdtr
038200981102      *
038300981102     c                   IF        *IN02 = *ON
038400981102     c                   movel     Ksif          TBEsif
038500981102     c                   movel     Klin          TBElin
038600981102     c                   movel     Kcod          TBEcod
038700020227     C                   movel     V1CFED        TBEke1
038800981102     C                   WRITE     TNTBE000
038900981102      *
039000981102     c                   ELSE
039100981102     C                   UPDATE    TNTBE000
039200981102     c                   ENDIF
039300980507      *
039400980507     C                   ENDSR
039500980506      *****************************************************************
039600980506      *   ROUTINE INIZIALE
039700980506      *****************************************************************
039800980506     C     *INZSR        BEGSR
039900980506      *
040000980506     C     *ENTRY        PLIST
040100980506     C                   PARM                    KPJBA
040200981102      *
040300981102     C                   Z-ADD     1             CODUT
040400981102     C                   CALL      'X§PARUT'
040500981102     C                   PARM                    UT§DSE0F
040600981102     C                   MOVEL     RAGUT         RSUT
040700981102      *
040800981102      * Controllo Testata Tabella
040900981102     C     KTBETT        CHAIN     TNTBE000                           28
041000981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
041100981117     C                   eval      *inlr = *on
041200981102     c                   ELSE
041300981102     c                   movel     TBEsif        Ksif
041400981102     c                   movel     TBElin        Klin
041500981102     c                   ENDIF
041600981102      *
041700981102     C     KTBE          klist
041800981102     C                   KFLD                    Kcod
041900981102     C                   KFLD                    Kke1
042000981102     C                   KFLD                    Kke2
042100981102     C                   KFLD                    Klin
042200981102     C                   KFLD                    Ksif
042300981102      *
042400981102     C     KTBETT        klist
042500981102     C                   KFLD                    KTTcod
042600981102     C                   KFLD                    KTTke1
042700981102      *
042800980506     C                   ENDSR
042900980909      *****************************************************************
043000980909** MSG  (Lungh. 78)                                                          *
043100020227Immettere il Codice Riferimento                                                1
043200020227Immettere il Nome del Responsabile                                             2
043300020227Immettere il numero di Telefono del Responsabile                               3
043400110512Immettere il codice con il quale Fedex identifica BRT                          4
