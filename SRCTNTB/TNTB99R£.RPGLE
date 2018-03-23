000100000804      *------------------------------------------------------------------------*
000200090617      * GESTIONE TABELLA CLIENTI incasso contrassegni con contratto standard   *
000300980909      *------------------------------------------------------------------------*
000400110808
000500130410      *?
001100980909
001200980909      *--------------------------------------------
001300980909      *  RIEPILOGO INDICATORI
001400980909      *--------------------------------------------
001500981102      * 01 - RECORD annullato
001600981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001700980909      *--------------------------------------------
001800980909
001900980506     H DECEDIT('0,') DATEDIT(*DMY.)
002000980909
002100130410     FTNTB98D   CF   E             WORKSTN sfile(TB98dsf:nrr)
002200981102     FTNTBE01L  UF A E           K DISK
002300990217     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
002400980909
002500981102      *--------------------------------------------
002600130410     D Kcod            S                   LIKE(TBEcod)   inz('CCA')
002700990217     D Kke1            S                   LIKE(TBEke1)
002800990217     D Kke2            S                   LIKE(TBEke2)
002900990217     D Klin            S                   LIKE(TBElin)
003000990217     D Ksif            S                   LIKE(TBEsif)
003100990217     d TIBS69DS      E DS
003200990217     d DS_cnaco      E DS                  extname(CNACO00F)
003300990217     d DS_cnind      E DS                  extname(CNIND00F)
003400990217     d DS_cnclp      E DS                  extname(CNCLP00F)
003500990217     d DS_fncls      E DS                  extname(FNCLS00F)
003600990217     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
003700981102      *
003800980507     D KPJBA         E DS
003900981102     D UT§DSE0F      E DS
004000990219     D  TCU                  398    697    DIM(50)                              TAB FLAG CNT
004100990219     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    TAB CAPO CONTO
004200990217     D TCUDS           DS
004300990217     D  F1                     1      1
004400990217     D  F3                     3      3
004500990217     D  F2                     2      2
004600990217     D  F4                     4      4
004700990217     D  F56                    5      6
004800980507      *--------------------------------------------
004900990219      * Carico tutti i record presenti
005000990217     c     inizio        tag
005100990217     c                   EXSR      CARICA
005200990217     c                   clear                   v1cksc
005300990217     c                   clear                   v1dksc
005400980909
005500980506      * EMISSIONE PRIMA VIDEATA
005600990217     C     for01         TAG
005700130410     c                   write     TB98z01
005800130410     C                   EXFMT     TB98dct
005900980909     C                   clear                   V1CMSG
006000990217     C                   setoff                                       9028
006100980930      *
006200980506      * F3=FINE
006300980909     c   KC              goto      fine
006400981102      *
006500981102      * CONTROLLI VIDEATA
006600981102     C                   EXSR      CTRD01
006700990217     C   90              goto      for01
006800980506      *
006900990219      * f6 - conferma   F16 - annullamento
007000990217     c                   if        *inkf  OR
007100990217     c                             *inkq
007200990217     c                   EXSR      CONFERMA
007300990217     c                   goto      inizio
007400990217     c                   endif
007500990217     C                   goto      for01
007600980506      *
007700980506     C     FINE          TAG
007800981102      *
007900980506     C                   EVAL      *INLR = *ON
008000990217      *****************************************************************
008100990217      *   carico tutti gli elementi della tabella
008200990217      *****************************************************************
008300990217     c     CARICA        BEGSR
008400990217     c                   clear                   NRR               4 0
008500990217     c
008600990217     c                   eval      *in35='1'
008700130410     c                   write     TB98dct
008800990217     c                   eval      *in35='0'
008900990217     c
009000990217     c     ktbe2         chain     tntbe02l                           30
009100990217     c                   dow       not *in30
009200990217     c                   if        tbeatb=' '
009300990219      *
009400990217     c                   movel     tbeke1        vscksc
009500990217     c
009600990217     C                   clear                   TIBS69DS
009700990217     c                   z-add     vscksc        I69kac
009800990217      *
009900990217     C                   CALL      'TIBS69R'
010000990217     C                   PARM                    tibs69DS
010100990217     C                   PARM                    DS_cnaco
010200990217     C                   PARM                    DS_cnind
010300990217     C                   PARM                    DS_cnclp
010400990217     C                   PARM                    DS_fncls
010500990217    2c                   if        o69err=' '
010600990217     c                   movel     acorag        vsdksc
010700990217    2c                   endif
010800990217     c
010900990217     c                   add       1             nrr
011000130410     c                   write     TB98dsf
011100990217     c                   endif
011200990217     c
011300990217     c     ktbe2         reade     tntbe02l                               30
011400990217     c                   enddo
011500990217     c
011600990217     c
011700990219      * Se ci sono dei record visualizzo il sfl
011800990217     c                   if        nrr>0
011900990217     c                   eval      nrr=1
012000990217     c                   eval      *in36='1'
012100990217     c                   endif
012200990217     c
012300990217     c                   ENDSR
012400981102      *****************************************************************
012500981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
012600981102      *****************************************************************
012700981102     C     CTRD01        BEGSR
012800990217     c                   if        *inkg
012900990217     c                   movel     rsut          paxdut           30
013000001003     c                   movel(p)  v1dksc        paxdmt           48
013100990219      * PAXSTA=9 ESCLUDO ANNULLATI
013200990217     C                   Z-ADD     9             PAXSTA
013300990217     c                   movel     kci           paxccc            4 0
013400990217     c                   clear                   paxdit
013500000804     C                   z-add     1             paxnum
013600000804     C                   CALL      'XALFA3BR'
013700990217     C                   PARM                    PAXDUT
013800990217     C                   PARM                    CODUT
013900990217     C                   PARM                    PAXDMT
014000990217     C                   PARM                    PAXCCC
014100990217     C                   PARM                    PAXSTA            1 0
014200990217     C                   PARM                    PAXFLR           90
014300990217     C                   PARM                    PAXDIT            3
014400000804     C                   PARM                    PAXNUM            2 0
014500000804     C                   PARM                    PAXKCM           80
014600000804     C                   PARM                    PAXKSM          140
014700000804     C                   PARM                    PAXKDM           60
014800990217    2C     PAXSTA        IFGT      -1
014900000804     C                   MOVEL     paxksm        v1cksc
015000000804     C                   MOVEL     paxdmt        v1dksc
015100990217     c                   endif
015200990219      *
015300990217     c                   seton                                        90
015400990217     c                   goto      endct1
015500990217     c                   endif
015600990217     c
015700990219      * Controllo ksc
015800990217     c                   if        v1cksc>0
015900990217     C                   clear                   TIBS69DS
016000990217     c                   z-add     v1cksc        I69kac
016100990217      *
016200990217     C                   CALL      'TIBS69R'
016300990217     C                   PARM                    tibs69DS
016400990217     C                   PARM                    DS_cnaco
016500990217     C                   PARM                    DS_cnind
016600990217     C                   PARM                    DS_cnclp
016700990217     C                   PARM                    DS_fncls
016800990217    2c                   if        o69err<>' '
016900990217     c                   movel     msg(1)        v1cmsg
017000990217     c                   seton                                        9028
017100990217     c                   goto      endct1
017200990217     c                   else
017300990217     c                   movel     acorag        v1dksc
017400990217    2c                   endif
017500990217    2c                   endif
017600990217     c     endct1        ENDSR
017700981102      *
017800980507      *****************************************************************
017900980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
018000980507      *****************************************************************
018100990217     C     CONFERMA      BEGSR
018200990217     c                   if        v1cksc>0
018300990217     c                   movel(p)  v1cksc        Kke1
018400990217     c     ktbe          chain     tntbe01l                           31
018500990217     c   31              clear                   tntbe000
018600990219      *
018700990217     c                   movel(p)  v1dksc        TBEuni
018800990217     c                   movel     'S'           tbeftt
018900980507      *
019000981102     C   KQ              movel     'A'           TBEatb
019100990217     C  NKQ              movel     *blanks       TBEatb
019200980909      *
019300981102     c                   clear                   TBEftr
019400981102     c                   clear                   TBEdtr
019500981102      *
019600990217     c                   IF        *IN31 = *ON
019700981102     c                   movel     Ksif          TBEsif
019800981102     c                   movel     Klin          TBElin
019900981102     c                   movel     Kcod          TBEcod
020000990217     c                   movel     Ksif          TBEsif
020100990217     c                   movel     Kke2          TBEke2
020200990217     c                   movel     v1cksc        TBEke1
020300981102     C                   WRITE     TNTBE000
020400981102      *
020500981102     c                   ELSE
020600981102     C                   UPDATE    TNTBE000
020700981102     c                   ENDIF
020800990217     c                   ENDIF
020900980507      *
021000980507     C                   ENDSR
021100980506      *****************************************************************
021200980506      *   ROUTINE INIZIALE
021300980506      *****************************************************************
021400980506     C     *INZSR        BEGSR
021500980506      *
021600980506     C     *ENTRY        PLIST
021700980506     C                   PARM                    KPJBA
021800981102      *
021900981102     C                   Z-ADD     1             CODUT
022000981102     C                   CALL      'X§PARUT'
022100981102     C                   PARM                    UT§DSE0F
022200981102     C                   MOVEL     RAGUT         RSUT
022300990217     c
022400990217     C*--- RICERCA CAPOCONTI
022500990217     C                   DO        50            X                 2 0
022600990217     C                   MOVE      TCU(X)        TCUDS
022700990217     C     F56           CABNE     'CG'          END1
022800990217     C     F4            COMP      '1'                                    21
022900990217     C     F4            COMP      '2'                                    22
023000990217     C     F4            COMP      '3'                                    23
023100990217     C     F4            COMP      '6'                                    27
023200990217     C** 1 CLIENTI   21
023300990217     C** 2 FORNITORI 22
023400990217     C** 3 AGENTI    23
023500990217     C     F3            COMP      '0'                                242425
023600990217     C     F3            COMP      'I'                                    26
023700990217     C** 0 ITALIA   25
023800990217     C** 1 ESTERO   24
023900990217     ** I CAPO CONTO IVA
024000990217     C   21
024100990217     CAN 24              Z-ADD     KCU(X)        KCE               4 0
024200990217     C   21
024300990217     CAN 25              Z-ADD     KCU(X)        KCI               4 0
024400990217     C   22
024500990217     CAN 24              Z-ADD     KCU(X)        KFE               4 0
024600990217     C   22
024700990217     CAN 25              Z-ADD     KCU(X)        KFI               4 0
024800990217     C   23
024900990217     CAN 24              Z-ADD     KCU(X)        KAE               4 0
025000990217     C   23
025100990217     CAN 25              Z-ADD     KCU(X)        KAI               4 0
025200990217     C   26              Z-ADD     KCU(X)        KIVA              4 0
025300990217     C   27              Z-ADD     KCU(X)        KBNA              4 0
025400990217     C     END1          TAG
025500990217     C                   END
025600990217     C                   SETOFF                                       212223
025700990217     C                   SETOFF                                       242526
025800990217     C                   SETOFF                                       27
025900981102      *
026000981102     C     KTBE          klist
026100981102     C                   KFLD                    Kcod
026200990217     C                   KFLD                    Kke1
026300990217     C                   KFLD                    Kke2
026400981102     C                   KFLD                    Klin
026500981102     C                   KFLD                    Ksif
026600990219      *
026700990217     c     KTBE2         klist
026800990217     C                   KFLD                    Kcod
026900990217     C                   KFLD                    Klin
027000990217     C                   KFLD                    Ksif
027100981102      *
027200980506     C                   ENDSR
027300980909      *****************************************************************
027400980909** MSG  (Lungh. 78)                                                          *
027500990217Codice cliente inesistente                                                     1
