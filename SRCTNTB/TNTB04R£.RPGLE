000100000804      *------------------------------------------------------------------------*
000200990219      * GESTIONE TABELLA CLIENTI CON PARTICOLARITA' PER DANNI                  *
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
001400990219     FTNTB04D   CF   E             WORKSTN sfile(TB04dsf:nrr)
001500981102     FTNTBE01L  UF A E           K DISK
001600990217     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
001700980909
001800981102      *--------------------------------------------
001900990219     D Kcod            S                   LIKE(TBEcod)   inz('DPC')
002000990217     D Kke1            S                   LIKE(TBEke1)
002100990217     D Kke2            S                   LIKE(TBEke2)
002200990217     D Klin            S                   LIKE(TBElin)
002300990217     D Ksif            S                   LIKE(TBEsif)
002400990217     d TIBS69DS      E DS
002500990217     d DS_cnaco      E DS                  extname(CNACO00F)
002600990217     d DS_cnind      E DS                  extname(CNIND00F)
002700990217     d DS_cnclp      E DS                  extname(CNCLP00F)
002800990217     d DS_fncls      E DS                  extname(FNCLS00F)
002900990217     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
003000981102      *
003100980507     D KPJBA         E DS
003200981102     D UT§DSE0F      E DS
003300990219     D  TCU                  398    697    DIM(50)                              TAB FLAG CNT
003400990219     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    TAB CAPO CONTO
003500990217     D TCUDS           DS
003600990217     D  F1                     1      1
003700990217     D  F3                     3      3
003800990217     D  F2                     2      2
003900990217     D  F4                     4      4
004000990217     D  F56                    5      6
004100980507      *--------------------------------------------
004200990219      * Carico tutti i record presenti
004300990217     c     inizio        tag
004400990217     c                   EXSR      CARICA
004500990217     c                   clear                   v1cksc
004600990217     c                   clear                   v1dksc
004700980909
004800980506      * EMISSIONE PRIMA VIDEATA
004900990217     C     for01         TAG
005000990219     c                   write     TB04z01
005100990219     C                   EXFMT     TB04dct
005200980909     C                   clear                   V1CMSG
005300990217     C                   setoff                                       9028
005400980930      *
005500980506      * F3=FINE
005600980909     c   KC              goto      fine
005700981102      *
005800981102      * CONTROLLI VIDEATA
005900981102     C                   EXSR      CTRD01
006000990217     C   90              goto      for01
006100980506      *
006200990219      * f6 - conferma   F16 - annullamento
006300990217     c                   if        *inkf  OR
006400990217     c                             *inkq
006500990217     c                   EXSR      CONFERMA
006600990217     c                   goto      inizio
006700990217     c                   endif
006800990217     C                   goto      for01
006900980506      *
007000980506     C     FINE          TAG
007100981102      *
007200980506     C                   EVAL      *INLR = *ON
007300990217      *****************************************************************
007400990217      *   carico tutti gli elementi della tabella
007500990217      *****************************************************************
007600990217     c     CARICA        BEGSR
007700990217     c                   clear                   NRR               4 0
007800990217     c
007900990217     c                   eval      *in35='1'
008000990219     c                   write     TB04dct
008100990217     c                   eval      *in35='0'
008200990217     c
008300990217     c     ktbe2         chain     tntbe02l                           30
008400990217     c                   dow       not *in30
008500990217     c                   if        tbeatb=' '
008600990219      *
008700990217     c                   movel     tbeke1        vscksc
008800990217     c
008900990217     C                   clear                   TIBS69DS
009000990217     c                   z-add     vscksc        I69kac
009100990217      *
009200990217     C                   CALL      'TIBS69R'
009300990217     C                   PARM                    tibs69DS
009400990217     C                   PARM                    DS_cnaco
009500990217     C                   PARM                    DS_cnind
009600990217     C                   PARM                    DS_cnclp
009700990217     C                   PARM                    DS_fncls
009800990217    2c                   if        o69err=' '
009900990217     c                   movel     acorag        vsdksc
010000990217    2c                   endif
010100990217     c
010200990217     c                   add       1             nrr
010300990219     c                   write     TB04dsf
010400990217     c                   endif
010500990217     c
010600990217     c     ktbe2         reade     tntbe02l                               30
010700990217     c                   enddo
010800990217     c
010900990217     c
011000990219      * Se ci sono dei record visualizzo il sfl
011100990217     c                   if        nrr>0
011200990217     c                   eval      nrr=1
011300990217     c                   eval      *in36='1'
011400990217     c                   endif
011500990217     c
011600990217     c                   ENDSR
011700981102      *****************************************************************
011800981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
011900981102      *****************************************************************
012000981102     C     CTRD01        BEGSR
012100990217     c                   if        *inkg
012200990217     c                   movel     rsut          paxdut           30
012300001003     c                   movel(p)  v1dksc        paxdmt           48
012400990219      * PAXSTA=9 ESCLUDO ANNULLATI
012500990217     C                   Z-ADD     9             PAXSTA
012600990217     c                   movel     kci           paxccc            4 0
012700990217     c                   clear                   paxdit
012800000804     C                   z-add     1             paxnum
012900000804     C                   CALL      'XALFA3BR'
013000990217     C                   PARM                    PAXDUT
013100990217     C                   PARM                    CODUT
013200990217     C                   PARM                    PAXDMT
013300990217     C                   PARM                    PAXCCC
013400990217     C                   PARM                    PAXSTA            1 0
013500990217     C                   PARM                    PAXFLR           90
013600990217     C                   PARM                    PAXDIT            3
013700000804     C                   PARM                    PAXNUM            2 0
013800000804     C                   PARM                    PAXKCM           80
013900000804     C                   PARM                    PAXKSM          140
014000000804     C                   PARM                    PAXKDM           60
014100990217    2C     PAXSTA        IFGT      -1
014200000804     C                   MOVEL     paxksm        v1cksc
014300000804     C                   MOVEL     paxdmt        v1dksc
014400990217     c                   endif
014500990219      *
014600990217     c                   seton                                        90
014700990217     c                   goto      endct1
014800990217     c                   endif
014900990217     c
015000990219      * Controllo ksc
015100990217     c                   if        v1cksc>0
015200990217     C                   clear                   TIBS69DS
015300990217     c                   z-add     v1cksc        I69kac
015400990217      *
015500990217     C                   CALL      'TIBS69R'
015600990217     C                   PARM                    tibs69DS
015700990217     C                   PARM                    DS_cnaco
015800990217     C                   PARM                    DS_cnind
015900990217     C                   PARM                    DS_cnclp
016000990217     C                   PARM                    DS_fncls
016100990217    2c                   if        o69err<>' '
016200990217     c                   movel     msg(1)        v1cmsg
016300990217     c                   seton                                        9028
016400990217     c                   goto      endct1
016500990217     c                   else
016600990217     c                   movel     acorag        v1dksc
016700990217    2c                   endif
016800990217    2c                   endif
016900990217     c     endct1        ENDSR
017000981102      *
017100980507      *****************************************************************
017200980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
017300980507      *****************************************************************
017400990217     C     CONFERMA      BEGSR
017500990217     c                   if        v1cksc>0
017600990217     c                   movel(p)  v1cksc        Kke1
017700990217     c     ktbe          chain     tntbe01l                           31
017800990217     c   31              clear                   tntbe000
017900990219      *
018000990217     c                   movel(p)  v1dksc        TBEuni
018100990217     c                   movel     'S'           tbeftt
018200980507      *
018300981102     C   KQ              movel     'A'           TBEatb
018400990217     C  NKQ              movel     *blanks       TBEatb
018500980909      *
018600981102     c                   clear                   TBEftr
018700981102     c                   clear                   TBEdtr
018800981102      *
018900990217     c                   IF        *IN31 = *ON
019000981102     c                   movel     Ksif          TBEsif
019100981102     c                   movel     Klin          TBElin
019200981102     c                   movel     Kcod          TBEcod
019300990217     c                   movel     Ksif          TBEsif
019400990217     c                   movel     Kke2          TBEke2
019500990217     c                   movel     v1cksc        TBEke1
019600981102     C                   WRITE     TNTBE000
019700981102      *
019800981102     c                   ELSE
019900981102     C                   UPDATE    TNTBE000
020000981102     c                   ENDIF
020100990217     c                   ENDIF
020200980507      *
020300980507     C                   ENDSR
020400980506      *****************************************************************
020500980506      *   ROUTINE INIZIALE
020600980506      *****************************************************************
020700980506     C     *INZSR        BEGSR
020800980506      *
020900980506     C     *ENTRY        PLIST
021000980506     C                   PARM                    KPJBA
021100981102      *
021200981102     C                   Z-ADD     1             CODUT
021300981102     C                   CALL      'X§PARUT'
021400981102     C                   PARM                    UT§DSE0F
021500981102     C                   MOVEL     RAGUT         RSUT
021600990217     c
021700990217     C*--- RICERCA CAPOCONTI
021800990217     C                   DO        50            X                 2 0
021900990217     C                   MOVE      TCU(X)        TCUDS
022000990217     C     F56           CABNE     'CG'          END1
022100990217     C     F4            COMP      '1'                                    21
022200990217     C     F4            COMP      '2'                                    22
022300990217     C     F4            COMP      '3'                                    23
022400990217     C     F4            COMP      '6'                                    27
022500990217     C** 1 CLIENTI   21
022600990217     C** 2 FORNITORI 22
022700990217     C** 3 AGENTI    23
022800990217     C     F3            COMP      '0'                                242425
022900990217     C     F3            COMP      'I'                                    26
023000990217     C** 0 ITALIA   25
023100990217     C** 1 ESTERO   24
023200990217     ** I CAPO CONTO IVA
023300990217     C   21
023400990217     CAN 24              Z-ADD     KCU(X)        KCE               4 0
023500990217     C   21
023600990217     CAN 25              Z-ADD     KCU(X)        KCI               4 0
023700990217     C   22
023800990217     CAN 24              Z-ADD     KCU(X)        KFE               4 0
023900990217     C   22
024000990217     CAN 25              Z-ADD     KCU(X)        KFI               4 0
024100990217     C   23
024200990217     CAN 24              Z-ADD     KCU(X)        KAE               4 0
024300990217     C   23
024400990217     CAN 25              Z-ADD     KCU(X)        KAI               4 0
024500990217     C   26              Z-ADD     KCU(X)        KIVA              4 0
024600990217     C   27              Z-ADD     KCU(X)        KBNA              4 0
024700990217     C     END1          TAG
024800990217     C                   END
024900990217     C                   SETOFF                                       212223
025000990217     C                   SETOFF                                       242526
025100990217     C                   SETOFF                                       27
025200981102      *
025300981102     C     KTBE          klist
025400981102     C                   KFLD                    Kcod
025500990217     C                   KFLD                    Kke1
025600990217     C                   KFLD                    Kke2
025700981102     C                   KFLD                    Klin
025800981102     C                   KFLD                    Ksif
025900990219      *
026000990217     c     KTBE2         klist
026100990217     C                   KFLD                    Kcod
026200990217     C                   KFLD                    Klin
026300990217     C                   KFLD                    Ksif
026400981102      *
026500980506     C                   ENDSR
026600980909      *****************************************************************
026700980909** MSG  (Lungh. 78)                                                          *
026800990217Codice cliente inesistente                                                     1
