000100980909      *------------------------------------------------------------------------*
000200000204      * GESTIONE TABELLA DIR : Scan ed eventi ritornati da DPD                 *
000300980909      *------------------------------------------------------------------------*
000400980909
000500980909      *--------------------------------------------
000600980909      *  RIEPILOGO INDICATORI
000700980909      *--------------------------------------------
000800981102      * 01 - RECORD annullato
000900000207      * 02 - Acceso IMMISSIONE - Spento MANUTENZIONE
001000990305      * 30 - Comodo
001100000207      * 50 - Errore Cod. Aggiuntivo Alfabetico
001200000207      * 51 - Errore Cod. Anomalia Segnacollo
001300000207      * 52 - Errore Cod. Evento Bartolini
001400000207      * 53 - Errore Scrivere Evento
001500000207      * 54 - Errore Scrivere Evento 1 volta
001600000207      * 55 - Errore Blocco in Distinta
001700000207      * 56 - Errore Giorni Attesa per Distinta
001800000207      * 57 - Errore Mantenere Cod. Aggiuntivo per 2a Elaborazione
001900000208      * 58 - Errore Tipologia
002000000208      * 59 - Errore consegna particolare
002100000217      * 60 - Errore scrittura file errori
002200000217      * 61 - Errore codice errore
002300000221      * 62 - Errore Scan per attesa
002400000221      * 63 - Errore Cancello record 2a elaborazione
002500000217      * 64 - Errore
002600000217      * 65 - Errore
002700980909      *--------------------------------------------
002800000204
002900980506     H DECEDIT('0,') DATEDIT(*DMY.)
003000980909
003100000204     FTNTB07D   CF   E             WORKSTN
003200981102     FTNTBE01L  UF A E           K DISK
003300000207     FTABEL00F  IF   E           K DISK
003400981102      *--------------------------------------------
003500000221     D MSG             S             78    DIM(18) CTDATA PERRCD(1)             MSG VIDEO
003600981102      *
003700981102     D Kttcod          S                   LIKE(TBEcod)
003800000204     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000DIR')
003900000204     D Kcod            S                   LIKE(TBEcod)  INZ('DIR')
004000981102     D Kke1            S                   LIKE(TBEke1)
004100981102     D Kke2            S                   LIKE(TBEke2)
004200981102     D Klin            S                   LIKE(TBElin)
004300981102     D Ksif            S                   LIKE(TBEsif)
004400000207     D Klcod           S                   LIKE(TBLcod)
004500000207     D Klkey           S                   LIKE(TBLkey)
004600981102      *
004700000207     D Wdes            s             30
004800000207     D XX              s              3  0
004900000207      *
005000980507     D KPJBA         E DS
005100000204     D DDIR          E DS
005200000217     D DDER          E DS
005300000210     D DS1P          E DS
005400000204     D TIBS02DS      E DS
005500981102     D UT§DSE0F      E DS
005600980507      *--------------------------------------------
005700980506      * EMISSIONE PRIMA VIDEATA
005800981117     C                   DO        *hival
005900000204     C                   EXFMT     TN07D01
006000980930      *
006100980909     C                   clear                   V1CMSG
006200981102     C                   eval      *in28 = *off
006300991006     C                   eval      *in90 = *off
006400980930      *
006500980506      * F3=FINE
006600981117     c   KC              leave
006700981102      *
006800981102      * CONTROLLI VIDEATA
006900981102     C                   EXSR      CTRD01
007000981102      *
007100000204     C                   IF        *in90 = *on  or  *in28 = *on
007200000204     C                   iter
007300000204     C                   ENDIF
007400980506      *
007500980909      * IMPOSTO CAMPI SECONDA VIDEATA
007600980909     C                   EXSR      RIED02
007700981117      *
007800981117      * GESTIONE SECONDA VIDEATA
007900981117     C                   EXSR      GESD02
008000981117     c                   enddo
008100981102      *
008200980506     C                   EVAL      *INLR = *ON
008300981102      *****************************************************************
008400981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
008500981102      *****************************************************************
008600981102     C     CTRD01        BEGSR
008700000204      *
008800000204      *  Ricerca
008900000204     C     '?'           SCAN      V1Cke1                                 31
009000000204      *
009100000204    1C                   IF        *IN31 = *ON
009200000204     c                   clear                   TIBS02DS
009300000204     c                   movel     'R'           t02mod
009400000207     C                   movel     'DIR'         t02cod
009500000204     c                   movel     knsif         t02sif
009600000204     C                   CALL      'TIBS02R'
009700000204     c                   parm                    KPJBA
009800000204     c                   parm                    TIBS02DS
009900000204      *
010000000204     c                   IF        t02err <> *blanks
010100000204     C                   movel     MSG(1)        V1CMSG
010200000204     C                   ELSE
010300000204     c                   eval      *in90 = *on
010400000204     C                   movel     t02ke1        V1Cke1
010500000204     C                   movel     t02ke2        V1Cke2
010600000204     c                   movel     t02uni        ddir
010700000204     C                   ENDIF
010800000204      *
010900000204     C                   ENDIF
011000000207      *
011100000207      * Controlli V1cke2: deve essere blank o un numero > 0
011200000207     C                   SELECT
011300000207     C                   WHEN       V1Cmsg <> *blanks
011400000207     C                   WHEN       V1Cke2 = *blanks
011500060710     C                   WHEN       V1Cke2 = '000'
011600000207     C                   movel     MSG(10)       V1CMSG
011700000207     C                   WHEN       %subst(V1Cke2:1:1) >= *zeros  AND
011800060710     C                              %subst(V1Cke2:2:1) >= *zeros  and
011900060710     C                              %subst(V1Cke2:3:1) >= *zeros
012000000207     C                   OTHER
012100000207     C                   movel     MSG(10)       V1CMSG
012200000207     C                   ENDSL
012300000207      *
012400000207      * Controllo chiave non deve essere blank
012500000207     C                   IF        V1Cmsg = *blanks  and  V1cke1 = *blanks
012600000207     C                                               and  V1Cke2 = *blanks
012700000207     C                   movel     MSG(1)        V1CMSG
012800000207     C                   ENDIF
012900000204      *
013000000204     c                   eval      *in28 = (V1cmsg <> *blanks)
013100000204      *
013200981102     C                   ENDSR
013300980506      *****************************************************************
013400980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
013500980506      *****************************************************************
013600980506     C     RIED02        BEGSR
013700980909      *
013800981102     c                   clear                   TNTBE000
013900000204     c                   clear                   ddir
014000000210     C                   clear                   V2Dtc1
014100000210     C                   clear                   V2DCAN
014200000207     C                   clear                   V2Dcev
014300000217     C                   clear                   V2Dcer
014400991001     c
014500980909     c                   eval      *in01 = *off
014600000204     c                   eval      KKE1 = V1Cke1
014700000204     c                   eval      KKE2 = V1Cke2
014800980930      *
014900981102     c     KTBE          CHAIN     TNTBE000                           02
015000980506      *
015100000204     c  N02              movel     TBEuni        Ddir
015200981102     c                   eval      *in01 = (TBEatb <> *blanks)
015300980909      *
015400000204     C                   eval      V2Ccaa = §DIRcaa
015500000204     C                   eval      V2Cdes = §DIRdes
015600000204     C                   eval      V2Ccev = §DIRcev
015700000204     C                   eval      V2Ctpe = §DIRtpe
015800000204     C                   eval      V2Cgga = §DIRgga
015900000204     C                   eval      V2Casc = §DIRasc
016000060628     C                   eval      V2Cmic = §DIRmic
016100000207     C                   eval      V2Caca = §DIRaca
016200000204     C                   eval      V2Cdis = §DIRdis
016300000204     C                   eval      V2Cbld = §DIRbld
016400000204     C                   eval      V2Ceve = §DIReve
016500000204     C                   eval      V2Cevu = §DIRevu
016600000204     C                   eval      V2Carr = §DIRarr
016700000204     C                   eval      V2Ccan = §DIRcan
016800160908     C                   eval      V2rec  = §DIRCBR
016900000208     C                   eval      V2Ctc1 = §DIRtc1
017000000217     C                   eval      V2Cerr = §DIRerr
017100000217     C                   MOVEL     §DIRcer       V2Ccer
017200000221     C                   eval      V2Cc2e = §DIRc2e
017300000207      *
017400000207     C                   IF        V2Ccan <> *blanks
017500000207     C                   movel     '3E'          KLCOD
017600000207     C                   movel(P)  V2Ccan        KLKEY
017700000207     C     KTAB          CHAIN     TABEL00F
017800000207     C                   IF        %found  and  TBLflg = *blanks
017900000207     C                   movel     TBLUNI        V2DCAN
018000000207     C                   ENDIF
018100000207     C                   ENDIF
018200000208      *
018300000208     C                   IF        V2Ctc1 <> *blanks
018400000208     C                   movel     '1P'          KLCOD
018500000208     C                   movel(P)  V2Ctc1        KLKEY
018600000208     C     KTAB          CHAIN     TABEL00F
018700000208     C                   IF        %found  and  TBLflg = *blanks
018800000208     C                   movel     TBLUNI        V2Dtc1
018900000208     C                   ENDIF
019000000208     C                   ENDIF
019100980506      *
019200000207     C                   IF        V2Ccev <> *blanks
019300000207     C                   movel     '2A'          KLCOD
019400000207     C                   movel(P)  V2Ccev        KLKEY
019500000207     C     KTAB          CHAIN     TABEL00F
019600000207     C                   IF        %found  and  TBLflg = *blanks
019700000207     C                   movel     TBLUNI        V2Dcev
019800000207     C                   ENDIF
019900000207     C                   ENDIF
020000000217      *
020100000217     C                   IF        V2Ccer <> *blanks
020200000217     c                   clear                   TIBS02DS
020300000217     c                   movel     'C'           t02mod
020400000217     C                   movel     'DER'         t02cod
020500000217     c                   movel     knsif         t02sif
020600000217     C                   movel     V2Ccer        T02ke1
020700000217     C                   CALL      'TIBS02R'
020800000217     c                   parm                    KPJBA
020900000217     c                   parm                    TIBS02DS
021000000217      *
021100000217     c                   IF        t02err = *blanks
021200000217     c                   movel     t02uni        DDER
021300000217     c                   movel     §DERdes       V2DCer
021400000217     C                   ENDIF
021500000217     C                   ENDIF
021600000207      *
021700980506     C                   ENDSR
021800981117      *****************************************************************
021900981117      *   ROUTINE PER GESTIONE SECONDA VIDEATA
022000981117      *****************************************************************
022100981117     C     GESD02        BEGSR
022200981117      *
022300981117      * EMISSIONE
022400981117     C                   DO        *HIVAL
022500000204     C                   EXFMT     TN07D02
022600981117      *
022700000204     C                   clear                   V2CMSG
022800981117     c                   eval      *in28 = *off
022900000204     c                   eval      *in90 = *off
023000000217     c     50            DO        65            XX
023100000207     c                   eval      *IN(XX) = *OFF
023200000207     c                   ENDDO
023300981117      *
023400981117      * F3=FINE
023500981117     c   KC              leave
023600981117      *
023700981117      * F12=RITORNO
023800981117     c   KL              leave
023900981117      *
024000981117      * CONTROLLI VIDEATA
024100000204     C                   EXSR      CTRD02
024200000204      *
024300000204     C                   IF        *in90 = *on  or  *in28 = *on
024400000204     C                   iter
024500000204     C                   ENDIF
024600981117      *
024700000204      * GESTIONE CONFERME: F5(ke)=Ripristino;F6(kf)=Conferma;F16(kq)=Annulla
024800000204     c                   IF        *inKE = *ON  or  *inKF = *ON
024900000204     c                                          or  *inKQ = *ON
025000981117     c                   exsr      RIECDD
025100981117     C                   leave
025200981117     c                   ENDIF
025300981117      *
025400981117     c                   enddo
025500981117      *
025600981117     C                   endsr
025700000204      *****************************************************************
025800000204      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
025900000204      *****************************************************************
026000000204     C     CTRD02        BEGSR
026100000207      *
026200000208      * TIPOLOGIA obbligatoria se scrivere in distinta impostata
026300000208     c                   IF        V2Cdis <> *blanks  and  V2Ctpe = *blanks
026400000208     C                   eval      V2Cmsg = msg(11)
026500000208     C                   eval      *in58  = *on
026600000204     C                   goto      Ectrd02
026700000204     C                   ENDIF
026800000208      *
026900000208      * COD. AGGIUNTIVO ALFABETICO solo se KE2 impostata e viceversa
027000000208     c                   IF        (V2Ccaa = *blanks and V1Cke2 <> *blanks)
027100000208     c                                            OR
027200000208     c                             (V2Ccaa <> *blanks and V1Cke2 = *blanks)
027300000208     C                   eval      V2Cmsg = msg(2)
027400000208     C                   eval      *in50  = *on
027500000208     C                   goto      Ectrd02
027600000208     C                   ENDIF
027700000204      *
027800000207      * COD. ANOMALIA SEGNACOLLO
027900000208     C                   clear                   V2DCAN
028000000207     C                   IF        V2Ccan <> *blanks
028100000207      *   Ricerca in tabella
028200000207     C     '?'           SCAN      V2Ccan                                 31
028300000207      *
028400000207     C                   IF        *IN31 = *ON
028500000207     C                   CALL      'X§TABER'
028600000207     C                   parm                    CODUT
028700000207     C                   parm      '3E'          KLcod
028800000207     C                   parm      *blanks       KLkey
028900000207     C                   parm      *blanks       Wdes
029000000207      *
029100000207     C                   movel     klKEY         V2Ccan
029200000207      *
029300000207     C                   If        V2Ccan <> *BLANKS
029400000207     C                   movel     WDES          V2Dcan
029500000207     C                   Endif
029600000207      *
029700000207     c                   eval       *in90 = *on
029800000207     C                   ENDIF
029900000207      *
030000000207     C                   IF        V2Ccan <> *blanks
030100000207     C                   movel     '3E'          KLCOD
030200000207     C                   movel(P)  V2Ccan        KLKEY
030300000207     C     KTAB          CHAIN     TABEL00F
030400000207     C                   IF        %found  and  TBLflg = *blanks
030500000207     C                   movel     TBLUNI        V2DCAN
030600000207     C                   ELSE
030700000207     C                   clear                   V2DCAN
030800000207     C                   eval      *in28 = *on
030900000207     C                   eval      *in51 = *on
031000000207     C                   movel     MSG(8)        V2CMSG
031100000207     C                   goto      Ectrd02
031200000207     C                   ENDIF
031300000207     C                   ENDIF
031400000207      *
031500000207     C                   ENDIF
031600000208      *
031700000210      * COD. CONSEGNA PARTICOLARE - Deve essere inseribile in arrivo
031800000208     C                   clear                   V2Dtc1
031900000208     C                   IF        V2Ctc1 <> *blanks
032000000208      *   Ricerca in tabella
032100000208     C     '?'           SCAN      V2Ctc1                                 31
032200000208      *
032300000208     C                   IF        *IN31 = *ON
032400000208     C                   CALL      'X§TABER'
032500000208     C                   parm                    CODUT
032600000208     C                   parm      '1P'          KLcod
032700000208     C                   parm      *blanks       KLkey
032800000208     C                   parm      *blanks       Wdes
032900000208      *
033000000208     C                   movel     klKEY         V2Ctc1
033100000208      *
033200000208     C                   If        V2Ctc1 <> *BLANKS
033300000208     C                   movel     WDES          V2Dtc1
033400000208     C                   Endif
033500000208      *
033600000208     c                   eval       *in90 = *on
033700000208     C                   ENDIF
033800000208      *
033900000208     C                   IF        V2Ctc1 <> *blanks
034000000208     C                   movel     '1P'          KLCOD
034100000208     C                   movel(P)  V2Ctc1        KLKEY
034200000208     C     KTAB          CHAIN     TABEL00F
034300000210     C                   IF         %found  and  TBLflg = *blanks
034400000210     C                   movel     TBLUNI        DS1P
034500000210     C                   movel     §1Pdes        V2Dtc1
034600000210     C                   If         §1Puar <> 'S'
034700000210     C                   movel     MSG(12)       V2CMSG
034800000210     C                   Endif
034900000210      *
035000000208     C                   ELSE
035100000210     C                   movel     MSG(12)       V2CMSG
035200000208     C                   ENDIF
035300000208     C                   ENDIF
035400000208      *
035500000208     C                   ENDIF
035600000210      *
035700000210     C                   IF        V2Cmsg <> *blanks
035800000210     C                   eval      *in28 = *on
035900000210     C                   eval      *in59 = *on
036000000210     C                   goto      Ectrd02
036100000210     C                   ENDIF
036200000207      *
036300000207      * COD. EVENTO BARTOLINI
036400000208     C                   clear                   V2Dcev
036500000207     C                   IF        V2Ccev <> *blanks
036600000207      *   Ricerca in tabella
036700000207     C     '?'           SCAN      V2Ccev                                 31
036800000207      *
036900000207     C                   IF        *IN31 = *ON
037000000207     C                   CALL      'X§TABER'
037100000207     C                   parm                    CODUT
037200000207     C                   parm      '2A'          KLcod
037300000207     C                   parm      *blanks       KLkey
037400000207     C                   parm      *blanks       Wdes
037500000207      *
037600000207     C                   movel     klkey         V2Ccev
037700000207      *
037800000207     C                   If        V2Ccev <> *BLANKS
037900000207     C                   movel     wDES          V2Dcev
038000000207     C                   Endif
038100000207      *
038200000207     c                   eval       *in90 = *on
038300000207     C                   ENDIF
038400000207      *
038500000207     C                   IF        V2Ccev <> *blanks
038600000207     C                   movel     '2A'          KLCOD
038700000207     C                   movel(P)  V2Ccev        KLKEY
038800000207     C     KTAB          CHAIN     TABEL00F
038900000207     C                   IF        %found  and  TBLflg = *blanks
039000000207     C                   movel     TBLUNI        V2Dcev
039100000207     C                   ELSE
039200000207     C                   clear                   V2Dcev
039300000207     C                   eval      *in28 = *on
039400000207     C                   eval      *in52 = *on
039500000207     C                   movel     MSG(9)        V2CMSG
039600000207     C                   goto      Ectrd02
039700000207     C                   ENDIF
039800000207     C                   ENDIF
039900000207      *
040000000207     C                   ENDIF
040100000207      *
040200000208      * SCRIVERE EVENTO solo se evento BARTOLINI impostato
040300000208     c                   IF        V2Ceve <> *blanks  and  V2Ccev = *blanks
040400000204     C                   eval      *in53  = *on
040500000204     C                   eval      V2Cmsg = msg(3)
040600000204     C                   goto      Ectrd02
040700000204     C                   ENDIF
040800000204      *
040900000208      * SCRIVERE EVENTO 1 VOLTA solo se scrivere evento impostato
041000000208     c                   IF        V2Cevu <> *blanks  and  V2Ceve = *blanks
041100000204     C                   eval      *in54  = *on
041200000204     C                   eval      V2Cmsg = msg(4)
041300000204     C                   goto      Ectrd02
041400000204     C                   ENDIF
041500000204      *
041600000204      * BLOCCO IN DISTINTA solo se scrivere in distinta impostato
041700000204     c                   IF        V2Cbld <> *blanks  and  V2Cdis = *blanks
041800000204     C                   eval      *in55  = *on
041900000204     C                   eval      V2Cmsg = msg(5)
042000000204     C                   goto      Ectrd02
042100000204     C                   ENDIF
042200000204      *
042300000204      * GIORNI ATTESA solo se scrivere in distinta impostato
042400060720     c                   IF        V2Cgga <> *zeros  and  V2Casc = *blanks
042500000204     C                   eval      *in56  = *on
042600000204     C                   eval      V2Cmsg = msg(6)
042700000204     C                   goto      Ectrd02
042800000204     C                   ENDIF
042900000221      *
043000000221      * CODICE AGGIUNTIVO PER 2a ELABORAZ. solo se scan impostato in chiave
043100000221     c                   IF        V2Cc2e <> *blanks  and  V1Cke1 = *blanks
043200000221     C                   eval      *in63  = *on
043300000221     C                   eval      V2Cmsg = msg(18)
043400000221     C                   goto      Ectrd02
043500000221     C                   ENDIF
043600000204      *
043700000221      * CODICE AGGIUNTIVO PER 2a ELABORAZ. solo se scan impostato in chiave
043800000221     c                   IF        V2Casc <> *blanks  and  V1Cke1 = *blanks
043900000221     C                   eval      *in62  = *on
044000000221     C                   eval      V2Cmsg = msg(17)
044100000204     C                   goto      Ectrd02
044200000204     C                   ENDIF
044300000221      *
044400000221      * MANTENERE COD.AGGI. PER 2a ELABORAZ. solo se nuovo scan impostato
044500000221     c                   IF        V2Caca <> *blanks  and  V2Casc = *blanks
044600000221     C                   eval      *in57  = *on
044700000221     C                   eval      V2Cmsg = msg(7)
044800000221     C                   goto      Ectrd02
044900000221     C                   ENDIF
045000000217      *
045100000218      * SCRITTURA FILE ERRORE solo se non effettuate scelte
045200000218     c                   IF        V2Cerr <> *blanks  and
045300000218     c                             (V2Carr <> *blanks or V2Ccan <> *blanks or
045400000218     c                              V2Ceve <> *blanks or V2Cdis <> *blanks or
045500000218     c                              V2Casc <> *blanks)
045600000217     C                   eval      *in60  = *on
045700000218     C                   eval      V2Cmsg = msg(16)
045800000217     C                   goto      Ectrd02
045900000217     C                   ENDIF
046000000218      *
046100000218      * SCRITTURA FILE ERRORE solo se codice errore impostato
046200000218     c                   IF        V2Cerr <> *blanks  and  V2Ccer = *blanks
046300000218     C                   eval      *in60  = *on
046400000218     C                   eval      V2Cmsg = msg(13)
046500000218     C                   goto      Ectrd02
046600000218     C                   ENDIF
046700000217      *
046800000217      * CODICE ERRORE
046900000217     C                   clear                   V2Dcer
047000000217     C                   IF        V2Ccer <> *blanks
047100000217      *   Ricerca in tabella
047200000217     C     '?'           SCAN      V2Ccer                                 31
047300000217      *
047400000217     C                   IF        *IN31 = *ON
047500000217     c                   clear                   TIBS02DS
047600000217     c                   movel     'R'           t02mod
047700000217     C                   movel     'DER'         t02cod
047800000217     c                   movel     knsif         t02sif
047900000217     C                   CALL      'TIBS02R'
048000000217     c                   parm                    KPJBA
048100000217     c                   parm                    TIBS02DS
048200000217      *
048300000218     c                   eval      *in61 = *on
048400000217     c                   IF        t02err <> *blanks
048500000218     C                   movel     MSG(14)       V2CMSG
048600000217     C                   ELSE
048700000217     c                   eval      *in90 = *on
048800000217     C                   movel     t02ke1        V2Ccer
048900000217     c                   movel     t02uni        DDER
049000000217     c                   movel     §DERdes       V2DCer
049100000217     C                   ENDIF
049200000217      *
049300000217     C                   ELSE
049400000217      *
049500000217     c                   clear                   TIBS02DS
049600000217     c                   movel     'C'           t02mod
049700000217     C                   movel     'DER'         t02cod
049800000217     c                   movel     knsif         t02sif
049900000217     C                   movel     V2Ccer        T02ke1
050000000217     C                   CALL      'TIBS02R'
050100000217     c                   parm                    KPJBA
050200000217     c                   parm                    TIBS02DS
050300000217      *
050400000217     c                   IF        t02err <> *blanks
050500000218     C                   movel     MSG(14)       V2CMSG
050600000218     c                   eval      *in61 = *on
050700000217     C                   ELSE
050800000217     c                   movel     t02uni        DDER
050900000217     c                   movel     §DERdes       V2DCer
051000000217     C                   ENDIF
051100000217      *
051200000217     C                   ENDIF
051300000217      *
051400000217     C                   ENDIF
051500000217      *
051600000217     c                   IF        V2Cmsg <> *blanks
051700000217     C                   goto      Ectrd02
051800000217     C                   ENDIF
051900000218      *
052000000218      * CODICE ERRORE solo se scrittura file errore impostato
052100000218     c                   IF        V2Cerr = *blanks  and  V2Ccer <> *blanks
052200000218     C                   eval      *in61  = *on
052300000218     C                   eval      V2Cmsg = msg(15)
052400000218     C                   goto      Ectrd02
052500000218     C                   ENDIF
052600000204      *
052700000204     C     ECTRD02       TAG
052800000204     c                   eval      *in28 = (V2cmsg <> *blanks)
052900000204      *
053000000204     C                   ENDSR
053100980507      *****************************************************************
053200980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
053300980507      *****************************************************************
053400981117     C     RIECDD        BEGSR
053500980909      *
053600000204     C                   eval      §DIRcaa = V2Ccaa
053700000204     C                   eval      §DIRdes = V2Cdes
053800000204     C                   eval      §DIRcev = V2Ccev
053900000204     C                   eval      §DIRtpe = V2Ctpe
054000000204     C                   eval      §DIRgga = V2Cgga
054100000204     C                   eval      §DIRasc = V2Casc
054200060628     C                   eval      §DIRMIC = V2Cmic
054300000207     C                   eval      §DIRaca = V2Caca
054400000204     C                   eval      §DIRdis = V2Cdis
054500000204     C                   eval      §DIRbld = V2Cbld
054600000204     C                   eval      §DIReve = V2Ceve
054700000204     C                   eval      §DIRevu = V2Cevu
054800000204     C                   eval      §DIRarr = V2Carr
054900160908     C                   eval      §DIRCBR = V2rec
055000000204     C                   eval      §DIRcan = V2Ccan
055100000208     C                   eval      §DIRtc1 = V2Ctc1
055200000217     C                   eval      §DIRerr = V2Cerr
055300000217     C                   MOVEL     V2Ccer        §DIRcer
055400000221     C                   eval      §DIRc2e = V2Cc2e
055500990308      *
055600000204     c                   movel(p)  Ddir          TBEuni
055700981102      *
055800000204     C                   eval      TBEftt = 'S'
055900980507      *
056000981102     C   KQ              movel     'A'           TBEatb
056100000204     C   KE              clear                   TBEatb
056200980909      *
056300981102     c                   clear                   TBEftr
056400981102      *
056500981102     c                   IF        *IN02 = *ON
056600000204     C                   clear                   TBEflt
056700991001     c                   clear                   TBEdtr
056800000204     c                   eval      TBEsif = Ksif
056900000204     c                   eval      TBElin = Klin
057000000204     c                   eval      TBEcod = Kcod
057100000204     C                   eval      TBEke1 = V1Cke1
057200000204     C                   eval      TBEke2 = V1Cke2
057300981102     C                   WRITE     TNTBE000
057400981102      *
057500981102     c                   ELSE
057600981102     C                   UPDATE    TNTBE000
057700981102     c                   ENDIF
057800980507      *
057900980507     C                   ENDSR
058000980506      *****************************************************************
058100980506      *   ROUTINE INIZIALE
058200980506      *****************************************************************
058300980506     C     *INZSR        BEGSR
058400980506      *
058500980506     C     *ENTRY        PLIST
058600980506     C                   PARM                    KPJBA
058700981102      *
058800981102     C                   Z-ADD     1             CODUT
058900981102     C                   CALL      'X§PARUT'
059000981102     C                   PARM                    UT§DSE0F
059100981102     C                   MOVEL     RAGUT         RSUT
059200981102      *
059300981102      * Controllo Testata Tabella
059400981102     C     KTBETT        CHAIN     TNTBE000                           28
059500981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
059600981117     C                   eval      *inlr = *on
059700981102     c                   ELSE
059800981102     c                   movel     TBEsif        Ksif
059900981102     c                   movel     TBElin        Klin
060000981102     c                   ENDIF
060100981102      *
060200981102     C     KTBE          klist
060300981102     C                   KFLD                    Kcod
060400981102     C                   KFLD                    Kke1
060500981102     C                   KFLD                    Kke2
060600981102     C                   KFLD                    Klin
060700981102     C                   KFLD                    Ksif
060800981102      *
060900981102     C     KTBETT        klist
061000981102     C                   KFLD                    KTTcod
061100000207     c                   KFLD                    KTTke1
061200000207      *
061300000207     C     KTAB          KLIST
061400000207     C                   KFLD                    CODUT
061500000207     C                   KFLD                    KlCOD
061600000207     C                   KFLD                    KlKEY
061700981102      *
061800980506     C                   ENDSR
061900980909      *****************************************************************
062000000207** MSG  8E/L78                                                               *
062100000204Immettere uno Scan e/o un Codice Aggiuntivo                                    1
062200000204Immettere cod. aggiuntivo alfabetico se impostato in chiave                    2
062300110512Indicare "Scrivere Evento" solo se impostato "Evento BRT"                      3
062400000207Indicare "Scrivere una volta" solo se impostato "Scrivere Evento"              4
062500000207Indicare "Blocco in distinta" solo se impostato "Elaboro in distinta"          5
062600060720Indicare "Giorni attesa" solo se presente Nuovo SCAN da generare               6
062700000207Indicare "Mantenere cod.aggiuntivo" solo se impostato "Nuovo Scan"             7
062800000218Codice anomalia segnacollo inesistente                                         8
062900110512Codice evento BRT inesistente                                                  9
063000000207Codice Aggiuntivo non valido                                                   10
063100000210Indicare "Tipologia" se impostato "Elaboro in distinta"                        11
063200000210Codice consegna particolare errato o non utilizzabile in arrivo                12
063300000217Non è possibile impostare la scrittura file errori senza codice errore         13
063400000218Codice errore inesistente                                                      8
063500000217Non è possibile indicare il codice errore se non si scrive il file errori      15
063600000218Non è possibile scrivere il file errori se effettuate altre scelte             16
063700000221Solo se in chiave ho il codice scan è possibile la 2a elaborazione             17
063800000221Solo se in chiave ho il codice scan si può impostare la cancellazione 2a Elab. 18
