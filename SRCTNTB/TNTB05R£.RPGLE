000100980909
000200980909      *------------------------------------------------------------------------*
000300990222      * GESTIONE TABELLA NUMERATORI AVVISO DANNO                               *
000400980909      *------------------------------------------------------------------------*
000500980909
000600980909      *--------------------------------------------
000700980909      *  RIEPILOGO INDICATORI
000800980909      *--------------------------------------------
000900981102      * 01 - RECORD annullato
001000981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001100980909      *--------------------------------------------
001200980909
001300980506     H DECEDIT('0,') DATEDIT(*DMY.)
001400980909
001500990222     FTNTB05D   CF   E             WORKSTN
001600990222     FTNTBE01L  UF A E           K DISK
001700980909
001800981102      *--------------------------------------------
001900990222     D MSG             S             78    DIM(2) CTDATA PERRCD(1)              MSG VIDEO
002000981102      *
002100981102     D Kttcod          S                   LIKE(TBEcod)
002200990222     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000PAD')
002300990222     D Kcod            S                   LIKE(TBEcod)  INZ('PAD')
002400990222      *
002500990222     d                 DS                  INZ
002600990222     d  Kke1                               LIKE(TBEKE1)
002700990222     d  Ditta                  1      3
002800990222     d  Anno                   4      7s 0
002900990222      *
003000981102     D Kke2            S                   LIKE(TBEke2)
003100981102     D Klin            S                   LIKE(TBElin)
003200981102     D Ksif            S                   LIKE(TBEsif)
003300990222     D svbar           S              7  0
003400990222     D svsdi           S              7  0
003500021114     D svfed           S              7  0
003600990222     D W0140           S             14  0
003700990222     D Wdtgio          S              8  0
003800981102      *
003900980507     D KPJBA         E DS
004000981102     D UT§DSE0F      E DS
004100981102     D TIBS02DS      E DS
004200981102      *
004300981102     D                SDS
004400981102     D  VTCPGM                 1     10
004500980507      *--------------------------------------------
004600980909
004700980506      * EMISSIONE PRIMA VIDEATA
004800981117     C                   DO        *hival
004900990222     C                   EXFMT     TN05D01
005000980930      *
005100980909     C                   clear                   V1CMSG
005200981102     C                   eval      *in28 = *off
005300990222     C                   eval      *in11 = *off
005400990222     C                   eval      *in12 = *off
005500990222     C                   eval      *in13 = *off
005600980930      *
005700980506      * F3=FINE
005800981117     c   KC              leave
005900990222      *
006000990222      * F18=Cambio Anno
006100990222     c   KS              seton                                        05
006200990222     c   KS              iter
006300990222      *
006400990222      * CONTROLLI VIDEATA
006500990222     C   05              EXSR      CTRANN
006600990222     C                   if        *in05
006700990222     c  n28              setoff                                       05
006800990222     c                   iter
006900990222     c                   endif
007000981102      *
007100981102      * CONTROLLI VIDEATA
007200990222     C  n05              EXSR      CTRD01
007300981102      *
007400981117     C   28              iter
007500990222      *
007600990222      * F6=CONFERMA
007700990222     C   KF              EXSR      updd01
007800990222     c   KF              leave
007900980507      *
008000981117     c                   enddo
008100981102      *
008200980506     C                   EVAL      *INLR = *ON
008300990222      *****************************************************************
008400990222      *   ROUTINE PER controllo anno
008500990222      *****************************************************************
008600990222     C     ctrann        BEGSR
008700990222      *
008800990222     c* verifico se inserito l'anno
008900990222     c                   if        v5anno = 0
009000021114     c                             or V5anno < 1998
009100021114     c                             or V5anno > (*Year + 1)
009200990222     c                   seton                                        2813
009300021114     c                   movel     msg(1)        v1cmsg
009400990222     c                   else
009500990222     c* recupero i numeri
009600990222     c                   exsr      recupe
009700990222     c                   endif
009800990222     c*
009900990222     c                   endsr
010000981117      *****************************************************************
010100990222      *   ROUTINE PER GESTIONE VIDEATA
010200981117      *****************************************************************
010300990222     C     ctrd01        BEGSR
010400980507      *
010500990222      *  Numeri non devono essere minori a quello precedente
010600990222      * Bartolini
010700990222     c                   if        V5bnum < svbar
010800990222     c                   seton                                        2811
010900021114     C                   movel     MSG(2)        V1Cmsg
011000980909     C                   ENDIF
011100981102      *
011200990222      * SDI
011300990222     c                   if        V5snum < svsdi and not *in28
011400990222     c                   seton                                        2812
011500021114     C                   movel     MSG(2)        V1Cmsg
011600990222     C                   ENDIF
011700021114      *
011800021114      * FEDEX
011900021114     C                   If        V5Fnum < svfed and not *in28
012000021114     C                   seton                                        2814
012100021114     C                   movel     MSG(2)        V1Cmsg
012200021114     C                   Endif
012300980909      *
012400980909     C                   ENDSR
012500980507      *****************************************************************
012600980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
012700980507      *****************************************************************
012800990222     C     updd01        BEGSR
012900990222      *
013000990222     c                   eval      anno = v5anno
013100980909      *
013200990222      * Bartolini
013300990222     c                   eval      ditta =  'BAR'
013400990222     c     ktbe          chain     tntbe01l                           30
013500990222     c  n30              movel     v5bnum        tbeuni
013600990222     c  n30              update    tntbe000
013700990222      *
013800990222     c                   if        *in30
013900990222     c                   clear                   TNTBE000
014000990222     C                   MOVEL     Kcod          TBEcod
014100990222     C                   MOVEL     Kke1          TBEke1
014200990222     C                   MOVEL(P)  v5bnum        TBEuni
014300990222     C                   WRITE     TNTBE000
014400990222     c                   endif
014500990222      * SDI
014600990222     c                   eval      ditta =  'SDI'
014700990222     c     ktbe          chain     tntbe01l                           30
014800990222     c  n30              movel     v5snum        tbeuni
014900990222     c  n30              update    tntbe000
015000990222      *
015100990222     c                   if        *in30
015200990222     c                   clear                   TNTBE000
015300990222     C                   MOVEL     Kcod          TBEcod
015400990222     C                   MOVEL     Kke1          TBEke1
015500990222     C                   MOVEL(P)  v5snum        TBEuni
015600990222     C                   WRITE     TNTBE000
015700990222     c                   endif
015800021114      * FEDEX
015900021114     c                   eval      ditta =  'FED'
016000021114     c     ktbe          chain     tntbe01l                           30
016100021114     c  n30              movel     V5Fnum        TBEuni
016200021114     c  n30              UPDATE    TNTBE000
016300021114      *
016400021114     c                   if        *in30
016500021114     c                   clear                   TNTBE000
016600021114     C                   movel     Kcod          TBEcod
016700021114     C                   movel     Kke1          TBEke1
016800021114     C                   movel(P)  V5Fnum        TBEuni
016900021114     C                   WRITE     TNTBE000
017000021114     c                   endif
017100980909      *
017200980507     C                   ENDSR
017300990222      *****************************************************************
017400990222      *   ROUTINE RECUPERO NUMERATORI
017500990222      *****************************************************************
017600990222     C     RECUPE        BEGSR
017700990222      *
017800990222     c                   eval      anno = v5anno
017900990222      * Bartolini
018000990222     c                   eval      ditta = 'BAR'
018100990222     c     ktbe          chain     tntbe01l                           30
018200990222     c  n30              movel     tbeuni        svbar
018300021114     c   30              clear                   svbar
018400990222      * SDI
018500990222     c                   eval      ditta = 'SDI'
018600990222     c     ktbe          chain     tntbe01l                           30
018700990222     c  n30              movel     tbeuni        svsdi
018800021114     c   30              clear                   svsdi
018900021114      * FEDEX
019000021114     c                   eval      ditta = 'FED'
019100021114     c     ktbe          chain     tntbe01l                           30
019200021114     c  n30              movel     tbeuni        svfed
019300021114     c   30              clear                   svfed
019400990222      *
019500990222     c                   z-add     svbar         v5bnum
019600990222     c                   z-add     svsdi         v5snum
019700021114     c                   z-add     svfed         v5fnum
019800990222      *
019900990222     C                   ENDSR
020000980506      *****************************************************************
020100980506      *   ROUTINE INIZIALE
020200980506      *****************************************************************
020300980506     C     *INZSR        BEGSR
020400980506      *
020500980506     C     *ENTRY        PLIST
020600980506     C                   PARM                    KPJBA
020700981102      *
020800981102     C                   Z-ADD     1             CODUT
020900981102     C                   CALL      'X§PARUT'
021000981102     C                   PARM                    UT§DSE0F
021100981102     C                   MOVEL     RAGUT         RSUT
021200990222      * reperisco data e ora
021300990222     C                   TIME                    W0140
021400990222      * UDATE IN GGMMAAAA
021500990222     C                   MOVE      W0140         WDTGIO
021600981102      *
021700981102      * Controllo Testata Tabella
021800981102     C     KTBETT        CHAIN     TNTBE000                           28
021900981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
022000981117     C                   eval      *inlr = *on
022100981102     c                   ELSE
022200981102     c                   movel     TBEsif        Ksif
022300981102     c                   movel     TBElin        Klin
022400981102     c                   ENDIF
022500981102      *
022600981102     C     KTBE          klist
022700981102     C                   KFLD                    Kcod
022800981102     C                   KFLD                    Kke1
022900981102     C                   KFLD                    Kke2
023000981102     C                   KFLD                    Klin
023100981102     C                   KFLD                    Ksif
023200981102      *
023300981102     C     KTBETT        klist
023400981102     C                   KFLD                    KTTcod
023500981102     C                   KFLD                    KTTke1
023600990222      *
023700990222      * recupero i record principali
023800990222     c                   move      wdtgio        v5anno
023900990222      *
024000990222     c                   exsr      recupe
024100990222      *
024200980506     C                   ENDSR
024300980909      *****************************************************************
024400980909** MSG  (Lungh. 78)                                                          *
024500021114Anno Errato                                                                    1
024600021114Il numero non può essere minore del precedente                                 2
