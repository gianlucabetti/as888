000100980909
000200980909      *------------------------------------------------------------------------*
000300020130      * GESTIONE TABELLA NUMERO POLIZZA ASSICURATIVA                           *
000400980909      *------------------------------------------------------------------------*
000500980909
000600980909      *--------------------------------------------
000700980909      *  RIEPILOGO INDICATORI
000800980909      *--------------------------------------------
000900981102      * 01 - RECORD annullato
001000981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001100021114      * 05 - Richiesto il cambio anno
001200021114      * 13 - ERRORE Anno errato
001300021114      * 14 - ERRORE Primo   Numero Polizza Assicurativa
001400021114      * 15 - ERRORE Secondo Numero Polizza Assicurativa
001500980909      *--------------------------------------------
001600980909
001700980506     H DECEDIT('0,') DATEDIT(*DMY.)
001800980909
001900020130     FTNTB21D   CF   E             WORKSTN
002000990222     FTNTBE01L  UF A E           K DISK
002100980909
002200981102      *--------------------------------------------
002300990222     D MSG             S             78    DIM(2) CTDATA PERRCD(1)              MSG VIDEO
002400981102      *
002500981102     D Kttcod          S                   LIKE(TBEcod)
002600020130     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000DPZ')
002700020130     D Kcod            S                   LIKE(TBEcod)  INZ('DPZ')
002800990222      *
002900990222     d                 DS                  INZ
003000990222     d  Kke1                               LIKE(TBEKE1)
003100990222     d  Ditta                  1      3
003200990222     d  Anno                   4      7s 0
003300990222      *
003400981102     D Kke2            S                   LIKE(TBEke2)
003500981102     D Klin            S                   LIKE(TBElin)
003600981102     D Ksif            S                   LIKE(TBEsif)
003700021114     D Wnpl            S                   LIKE(§dpznpla) inz
003800990222     D W0140           S             14  0
003900990222     D Wdtgio          S              8  0
004000981102      *
004100020130     d ddpz          e ds
004200980507     D KPJBA         E DS
004300981102     D UT§DSE0F      E DS
004400020130     d cncr80        e ds
004500981102     D TIBS02DS      E DS
004600981102      *
004700981102     D                SDS
004800981102     D  VTCPGM                 1     10
004900980507      *--------------------------------------------
005000980909
005100980506      * EMISSIONE PRIMA VIDEATA
005200981117     C                   DO        *hival
005300020130     C                   EXFMT     TN21D01
005400980930      *
005500980909     C                   clear                   V1CMSG
005600981102     C                   eval      *in28 = *off
005700990222     C                   eval      *in13 = *off
005800021114     C                   eval      *in14 = *off
005900980930      *
006000980506      * F3=FINE
006100981117     c   KC              leave
006200990222      *
006300990222      * F18=Cambio Anno
006400990222     c   KS              seton                                        05
006500990222     c   KS              iter
006600990222      *
006700990222      * CONTROLLI VIDEATA
006800990222     C   05              EXSR      CTRANN
006900990222     C                   if        *in05
007000990222     c  n28              setoff                                       05
007100990222     c                   iter
007200990222     c                   endif
007300981102      *
007400981102      * CONTROLLI VIDEATA
007500990222     C  n05              EXSR      CTRD01
007600981102      *
007700981117     C   28              iter
007800990222      *
007900990222      * F6=CONFERMA
008000990222     C   KF              EXSR      updd01
008100990222     c   KF              leave
008200980507      *
008300981117     c                   enddo
008400981102      *
008500980506     C                   EVAL      *INLR = *ON
008600990222      *****************************************************************
008700990222      *   ROUTINE PER controllo anno
008800990222      *****************************************************************
008900990222     C     ctrann        BEGSR
009000020130
009100020130      * verifico se inserito l'anno
009200020130     c                   if        v21ann = 0
009300021114     c                             or V21ann < 1998
009400021114     c                             or V21ann > (*Year + 1)
009500990222     c                   seton                                        2813
009600021114     c                   movel     msg(1)        v1cmsg
009700990222     c                   else
009800021114      * recupero i numeri:
009900021114      * - Primo   numeratore (BAR)
010000021114     c                   eval      anno    = v21ann
010100021114     c                   eval      ditta   = v21dit
010200990222     c                   exsr      recupe
010300021114     c                   eval      v21npl  = §dpznpla
010400021114      * - Secondo numeratore (FED)
010500021114     c                   eval      anno    = v21ann
010600021114     c                   eval      ditta   = v21dit2
010700021114     c                   exsr      recupe
010800021114     c                   eval      v21npl2 = §dpznpla
010900990222     c                   endif
011000020130
011100990222     c                   endsr
011200981117      *****************************************************************
011300990222      *   ROUTINE PER GESTIONE VIDEATA
011400981117      *****************************************************************
011500990222     C     ctrd01        BEGSR
011600020130
011700021114      * Primo   numeratore (BAR)
011800020130     c                   if        V21npl = *blanks
011900021114     c                   seton                                        2814
012000021114     c                   movel     msg(2)        V1Cmsg
012100020130     c                   endif
012200021114      * Secondo numeratore (FED)
012300021114     c                   if        V21npl2 = *blanks
012400021114     c                   seton                                        2815
012500021114     c                   movel     msg(2)        V1Cmsg
012600021114     c                   endif
012700020130
012800980909     C                   ENDSR
012900980507      *****************************************************************
013000980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
013100980507      *****************************************************************
013200990222     C     updd01        BEGSR
013300021114      * Primo   numeratore (BAR)
013400021114     c                   eval      anno  = v21ann
013500021114     c                   eval      ditta = v21dit
013600021114     c                   eval      Wnpl  = v21npl
013700021114     c                   exsr      UpdD01_a
013800021114      * Secondo numeratore (FED)
013900021114     c                   eval      anno  = v21ann
014000021114     c                   eval      ditta = v21dit2
014100021114     c                   eval      Wnpl  = v21npl2
014200021114     c                   exsr      UpdD01_a
014300021114     C                   ENDSR
014400021114      *****************************************************************
014500021114      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
014600021114      *****************************************************************
014700021114     C     updd01_a      BEGSR
014800020130
014900020130     c                   clear                   ddpz
015000020130
015100021114     c***                eval      anno  = v21ann
015200021114     c***                eval      ditta = v21dit
015300020130     c     ktbe          chain     tntbe01l
015400020130
015500020130     c                   if        %found(tntbe01l)
015600021114     c***                eval      §dpznpla = v21npl
015700021114     c                   eval      §dpznpla = Wnpl
015800020130     c                   movel(p)  ddpz          tbeuni
015900020130     c                   update    tntbe000
016000020130
016100020130     c                   else
016200020130
016300020130     c                   clear                   tntbe000
016400020130     c                   eval      tbecod = Kcod
016500020130     c                   eval      tbeke1 = Kke1
016600021114     c***                eval      §dpznpla = v21npl
016700021114     c                   eval      §dpznpla = Wnpl
016800020130     c                   movel(p)  ddpz          tbeuni
016900020130     c                   write     tntbe000
017000990222     c                   endif
017100020130
017200980507     C                   ENDSR
017300990222      *****************************************************************
017400990222      *   ROUTINE RECUPERO NUMERATORI
017500990222      *****************************************************************
017600990222     C     RECUPE        BEGSR
017700020130
017800020130     c                   clear                   ddpz
017900021114     c***                eval      anno  = v21ann
018000021114     c***                eval      ditta = v21dit
018100020130     c     ktbe          chain     tntbe01l
018200020130     c                   if        %found(tntbe01l)
018300020130     c                   movel     tbeuni        ddpz
018400020130     c                   endif
018500021114     c***                eval      v21npl = §dpznpla
018600020130
018700990222     C                   ENDSR
018800980506      *****************************************************************
018900980506      *   ROUTINE INIZIALE
019000980506      *****************************************************************
019100980506     C     *INZSR        BEGSR
019200980506      *
019300980506     C     *ENTRY        PLIST
019400980506     C                   PARM                    KPJBA
019500981102      *
019600981102     C                   Z-ADD     1             CODUT
019700981102     C                   CALL      'X§PARUT'
019800981102     C                   PARM                    UT§DSE0F
019900981102     C                   MOVEL     RAGUT         RSUT
020000020130     C                   MOVEL     REC80         CNCR80
020100020201
020200020130     c                   eval      v21dit = 'BAR'
020300021114     c                   eval      v21dit2 = 'FED'
020400021114     C                   eval      *in14 = *on
020500020130
020600990222      * reperisco data e ora
020700990222     C                   TIME                    W0140
020800990222      * UDATE IN GGMMAAAA
020900990222     C                   MOVE      W0140         WDTGIO
021000981102      *
021100981102      * Controllo Testata Tabella
021200981102     C     KTBETT        CHAIN     TNTBE000                           28
021300981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
021400981117     C                   eval      *inlr = *on
021500981102     c                   ELSE
021600981102     c                   movel     TBEsif        Ksif
021700981102     c                   movel     TBElin        Klin
021800981102     c                   ENDIF
021900981102      *
022000981102     C     KTBE          klist
022100981102     C                   KFLD                    Kcod
022200981102     C                   KFLD                    Kke1
022300981102     C                   KFLD                    Kke2
022400981102     C                   KFLD                    Klin
022500981102     C                   KFLD                    Ksif
022600981102      *
022700981102     C     KTBETT        klist
022800981102     C                   KFLD                    KTTcod
022900981102     C                   KFLD                    KTTke1
023000990222      *
023100990222      * recupero i record principali
023200020130     c                   move      wdtgio        v21ann
023300990222      *
023400990222     c                   exsr      recupe
023500990222      *
023600980506     C                   ENDSR
023700980909      *****************************************************************
023800980909** MSG  (Lungh. 78)                                                          *
023900021114Anno Errato                                                                    1
024000021114Inserire il numero polizza assicurativa                                        2
