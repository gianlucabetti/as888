000100000804      *------------------------------------------------------------------------*
000200090617      * GESTIONE TABELLA CLIENTI incasso contrassegni con contratto standard   *
000300980909      *------------------------------------------------------------------------*
000400110808
000500130410      *?
000600980909
000700980909      *--------------------------------------------
000800980909      *  RIEPILOGO INDICATORI
000900980909      *--------------------------------------------
001000981102      * 01 - RECORD annullato
001100981102      * 02 - acceso IMMISSIONE - spento MANUTENZIOE
001200980909      *--------------------------------------------
001300980909
001400980506     H DECEDIT('0,') DATEDIT(*DMY.)
001500980909
001600130410     FTNTB98D   CF   E             WORKSTN sfile(TB98dsf:nrr)
001700981102     FTNTBE01L  UF A E           K DISK
001800990217     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
001900980909
002000981102      *--------------------------------------------
002100130410     D Kcod            S                   LIKE(TBEcod)   inz('CCA')
002200990217     D Kke1            S                   LIKE(TBEke1)
002300990217     D Kke2            S                   LIKE(TBEke2)
002400990217     D Klin            S                   LIKE(TBElin)
002500990217     D Ksif            S                   LIKE(TBEsif)
002600131009     d dcca          E DS
002700131009     d TIBS69DS      E DS
002800990217     d DS_cnaco      E DS                  extname(CNACO00F)
002900990217     d DS_cnind      E DS                  extname(CNIND00F)
003000990217     d DS_cnclp      E DS                  extname(CNCLP00F)
003100990217     d DS_fncls      E DS                  extname(FNCLS00F)
003200990217     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
003300981102      *
003400980507     D KPJBA         E DS
003500981102     D UT§DSE0F      E DS
003600990219     D  TCU                  398    697    DIM(50)                              TAB FLAG CNT
003700990219     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    TAB CAPO CONTO
003800990217     D TCUDS           DS
003900990217     D  F1                     1      1
004000990217     D  F3                     3      3
004100990217     D  F2                     2      2
004200990217     D  F4                     4      4
004300990217     D  F56                    5      6
004400980507      *--------------------------------------------
004500990219      * Carico tutti i record presenti
004600990217     c     inizio        tag
004700990217     c                   EXSR      CARICA
004800990217     c                   clear                   v1cksc
004900990217     c                   clear                   v1dksc
005000131009     c                   clear                   v1cksctm
005100131009     c                   clear                   v1dksctm
005200980909
005300980506      * EMISSIONE PRIMA VIDEATA
005400990217     C     for01         TAG
005500130410     c                   write     TB98z01
005600130410     C                   EXFMT     TB98dct
005700980909     C                   clear                   V1CMSG
005800990217     C                   setoff                                       9028
005900980930      *
006000980506      * F3=FINE
006100980909     c   KC              goto      fine
006200981102      *
006300981102      * CONTROLLI VIDEATA
006400981102     C                   EXSR      CTRD01
006500990217     C   90              goto      for01
006600980506      *
006700990219      * f6 - conferma   F16 - annullamento
006800990217     c                   if        *inkf  OR
006900990217     c                             *inkq
007000990217     c                   EXSR      CONFERMA
007100990217     c                   goto      inizio
007200990217     c                   endif
007300990217     C                   goto      for01
007400980506      *
007500980506     C     FINE          TAG
007600981102      *
007700980506     C                   EVAL      *INLR = *ON
007800990217      *****************************************************************
007900990217      *   carico tutti gli elementi della tabella
008000990217      *****************************************************************
008100990217     c     CARICA        BEGSR
008200990217     c                   clear                   NRR               4 0
008300990217     c
008400990217     c                   eval      *in35='1'
008500130410     c                   write     TB98dct
008600990217     c                   eval      *in35='0'
008700990217     c
008800990217     c     ktbe2         chain     tntbe02l                           30
008900990217     c                   dow       not *in30
009000990217     c                   if        tbeatb=' '
009100990219      *
009200990217     c                   movel     tbeke1        vscksc
009300131009     c                   movel     tbeuni        dcca
009400131009     c                   movel     §ccaksctm     vscksctm
009500131010     c     vscksctm      comp      0                                      88
009600990217     c
009700990217     C                   clear                   TIBS69DS
009800131010     C                   clear                   ds_cnaco
009900131010     C                   clear                   ds_cnind
010000131010     C                   clear                   ds_cnclp
010100131010     C                   clear                   ds_fncls
010200990217     c                   z-add     vscksc        I69kac
010300990217      *
010400990217     C                   CALL      'TIBS69R'
010500990217     C                   PARM                    tibs69DS
010600990217     C                   PARM                    DS_cnaco
010700990217     C                   PARM                    DS_cnind
010800990217     C                   PARM                    DS_cnclp
010900990217     C                   PARM                    DS_fncls
011000990217    2c                   if        o69err=' '
011100990217     c                   movel     acorag        vsdksc
011200131009     c                   else
011300131009     c                   clear                   vsdksc
011400990217    2c                   endif
011500131009     c* cliente tm che sostituisce bm
011600131009     C                   clear                   TIBS69DS
011700131010     C                   clear                   ds_cnaco
011800131010     C                   clear                   ds_cnind
011900131010     C                   clear                   ds_cnclp
012000131010     C                   clear                   ds_fncls
012100131009     c                   z-add     vscksctm      I69kac
012200131009      *
012300131009     C                   CALL      'TIBS69R'
012400131009     C                   PARM                    tibs69DS
012500131009     C                   PARM                    DS_cnaco
012600131009     C                   PARM                    DS_cnind
012700131009     C                   PARM                    DS_cnclp
012800131009     C                   PARM                    DS_fncls
012900131009    2c                   if        o69err=' '
013000131009     c                   movel     acorag        vsdksctm
013100131009     c                   else
013200131009     c                   clear                   vsdksctm
013300131009    2c                   endif
013400990217     c
013500990217     c                   add       1             nrr
013600130410     c                   write     TB98dsf
013700990217     c                   endif
013800990217     c
013900990217     c     ktbe2         reade     tntbe02l                               30
014000990217     c                   enddo
014100990217     c
014200990217     c
014300990219      * Se ci sono dei record visualizzo il sfl
014400990217     c                   if        nrr>0
014500990217     c                   eval      nrr=1
014600990217     c                   eval      *in36='1'
014700990217     c                   endif
014800990217     c
014900990217     c                   ENDSR
015000981102      *****************************************************************
015100981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
015200981102      *****************************************************************
015300981102     C     CTRD01        BEGSR
015400990217     c                   if        *inkg
015500990217     c                   movel     rsut          paxdut           30
015600131010     c                   if        rtncsrfld = 'V1CKSC'
015700131010     c                             or rtncsrfld = 'V1DKSC'
015800001003     c                   movel(p)  v1dksc        paxdmt           48
015900131009     c                   end
016000131010     c                   if        rtncsrfld = 'V1CKSCTM'
016100131010     c                             or rtncsrfld = 'V1DKSCTM'
016200131009     c                   movel(p)  v1dksctm      paxdmt           48
016300131009     c                   end
016400990219      * PAXSTA=9 ESCLUDO ANNULLATI
016500990217     C                   Z-ADD     9             PAXSTA
016600990217     c                   movel     kci           paxccc            4 0
016700990217     c                   clear                   paxdit
016800000804     C                   z-add     1             paxnum
016900000804     C                   CALL      'XALFA3BR'
017000990217     C                   PARM                    PAXDUT
017100990217     C                   PARM                    CODUT
017200990217     C                   PARM                    PAXDMT
017300990217     C                   PARM                    PAXCCC
017400990217     C                   PARM                    PAXSTA            1 0
017500990217     C                   PARM                    PAXFLR           90
017600990217     C                   PARM                    PAXDIT            3
017700000804     C                   PARM                    PAXNUM            2 0
017800000804     C                   PARM                    PAXKCM           80
017900000804     C                   PARM                    PAXKSM          140
018000000804     C                   PARM                    PAXKDM           60
018100990217    2C     PAXSTA        IFGT      -1
018200131010     c                   if        rtncsrfld = 'V1CKSC'
018300131010     c                             or rtncsrfld = 'V1DKSC'
018400000804     C                   MOVEL     paxksm        v1cksc
018500000804     C                   MOVEL     paxdmt        v1dksc
018600990217     c                   endif
018700131010     c                   if        rtncsrfld = 'V1CKSCTM'
018800131010     c                             or rtncsrfld = 'V1DKSCTM'
018900131009     C                   MOVEL     paxksm        v1cksctm
019000131009     C                   MOVEL     paxdmt        v1dksctm
019100131009     c                   endif
019200131009     c                   endif
019300990219      *
019400990217     c                   seton                                        90
019500990217     c                   goto      endct1
019600990217     c                   endif
019700990217     c
019800990219      * Controllo ksc
019900990217     c                   if        v1cksc>0
020000990217     C                   clear                   TIBS69DS
020100131010     C                   clear                   ds_cnaco
020200131010     C                   clear                   ds_cnind
020300131010     C                   clear                   ds_cnclp
020400131010     C                   clear                   ds_fncls
020500990217     c                   z-add     v1cksc        I69kac
020600990217      *
020700990217     C                   CALL      'TIBS69R'
020800990217     C                   PARM                    tibs69DS
020900990217     C                   PARM                    DS_cnaco
021000990217     C                   PARM                    DS_cnind
021100990217     C                   PARM                    DS_cnclp
021200990217     C                   PARM                    DS_fncls
021300990217    2c                   if        o69err<>' '
021400990217     c                   movel     msg(1)        v1cmsg
021500990217     c                   seton                                        9028
021600990217     c                   goto      endct1
021700990217     c                   else
021800990217     c                   movel     acorag        v1dksc
021900990217    2c                   endif
022000990217    2c                   endif
022100131009      * Controllo ksc tm
022200131009     c                   if        v1cksctm>0
022300131009     C                   clear                   TIBS69DS
022400131010     C                   clear                   ds_cnaco
022500131010     C                   clear                   ds_cnind
022600131010     C                   clear                   ds_cnclp
022700131010     C                   clear                   ds_fncls
022800131009     c                   z-add     v1cksctm      I69kac
022900131009      *
023000131009     C                   CALL      'TIBS69R'
023100131009     C                   PARM                    tibs69DS
023200131009     C                   PARM                    DS_cnaco
023300131009     C                   PARM                    DS_cnind
023400131009     C                   PARM                    DS_cnclp
023500131009     C                   PARM                    DS_fncls
023600131009    2c                   if        o69err<>' '
023700131009     c                   movel     msg(1)        v1cmsg
023800131009     c                   seton                                        9028
023900131009     c                   goto      endct1
024000131009     c                   else
024100131009     c                   movel     acorag        v1dksctm
024200131009    2c                   endif
024300131009    2c                   endif
024400990217     c     endct1        ENDSR
024500981102      *
024600980507      *****************************************************************
024700980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
024800980507      *****************************************************************
024900990217     C     CONFERMA      BEGSR
025000990217     c                   if        v1cksc>0
025100990217     c                   movel(p)  v1cksc        Kke1
025200990217     c     ktbe          chain     tntbe01l                           31
025300990217     c   31              clear                   tntbe000
025400990219      *
025500131009     c                   movel(p)  v1dksc        §ccadesc
025600131010     c                   move      v1cksctm      §ccaksctm
025700131009     c                   movel(p)  dcca          TBEuni
025800990217     c                   movel     'S'           tbeftt
025900980507      *
026000981102     C   KQ              movel     'A'           TBEatb
026100990217     C  NKQ              movel     *blanks       TBEatb
026200980909      *
026300981102     c                   clear                   TBEftr
026400981102     c                   clear                   TBEdtr
026500981102      *
026600990217     c                   IF        *IN31 = *ON
026700131010     c                   movel     'GE'          TBEapl
026800131010     c                   movel     Ksif          TBEsif
026900981102     c                   movel     Klin          TBElin
027000981102     c                   movel     Kcod          TBEcod
027100990217     c                   movel     Ksif          TBEsif
027200990217     c                   movel     Kke2          TBEke2
027300990217     c                   movel     v1cksc        TBEke1
027400981102     C                   WRITE     TNTBE000
027500981102      *
027600981102     c                   ELSE
027700981102     C                   UPDATE    TNTBE000
027800981102     c                   ENDIF
027900990217     c                   ENDIF
028000980507      *
028100980507     C                   ENDSR
028200980506      *****************************************************************
028300980506      *   ROUTINE INIZIALE
028400980506      *****************************************************************
028500980506     C     *INZSR        BEGSR
028600980506      *
028700980506     C     *ENTRY        PLIST
028800980506     C                   PARM                    KPJBA
028900981102      *
029000981102     C                   Z-ADD     1             CODUT
029100981102     C                   CALL      'X§PARUT'
029200981102     C                   PARM                    UT§DSE0F
029300981102     C                   MOVEL     RAGUT         RSUT
029400990217     c
029500990217     C*--- RICERCA CAPOCONTI
029600990217     C                   DO        50            X                 2 0
029700990217     C                   MOVE      TCU(X)        TCUDS
029800990217     C     F56           CABNE     'CG'          END1
029900990217     C     F4            COMP      '1'                                    21
030000990217     C     F4            COMP      '2'                                    22
030100990217     C     F4            COMP      '3'                                    23
030200990217     C     F4            COMP      '6'                                    27
030300990217     C** 1 CLIENTI   21
030400990217     C** 2 FORNITORI 22
030500990217     C** 3 AGENTI    23
030600990217     C     F3            COMP      '0'                                242425
030700990217     C     F3            COMP      'I'                                    26
030800990217     C** 0 ITALIA   25
030900990217     C** 1 ESTERO   24
031000990217     ** I CAPO CONTO IVA
031100990217     C   21
031200990217     CAN 24              Z-ADD     KCU(X)        KCE               4 0
031300990217     C   21
031400990217     CAN 25              Z-ADD     KCU(X)        KCI               4 0
031500990217     C   22
031600990217     CAN 24              Z-ADD     KCU(X)        KFE               4 0
031700990217     C   22
031800990217     CAN 25              Z-ADD     KCU(X)        KFI               4 0
031900990217     C   23
032000990217     CAN 24              Z-ADD     KCU(X)        KAE               4 0
032100990217     C   23
032200990217     CAN 25              Z-ADD     KCU(X)        KAI               4 0
032300990217     C   26              Z-ADD     KCU(X)        KIVA              4 0
032400990217     C   27              Z-ADD     KCU(X)        KBNA              4 0
032500990217     C     END1          TAG
032600990217     C                   END
032700990217     C                   SETOFF                                       212223
032800990217     C                   SETOFF                                       242526
032900990217     C                   SETOFF                                       27
033000981102      *
033100981102     C     KTBE          klist
033200981102     C                   KFLD                    Kcod
033300990217     C                   KFLD                    Kke1
033400990217     C                   KFLD                    Kke2
033500981102     C                   KFLD                    Klin
033600981102     C                   KFLD                    Ksif
033700990219      *
033800990217     c     KTBE2         klist
033900990217     C                   KFLD                    Kcod
034000990217     C                   KFLD                    Klin
034100990217     C                   KFLD                    Ksif
034200981102      *
034300980506     C                   ENDSR
034400980909      *****************************************************************
034500980909** MSG  (Lungh. 78)                                                          *
034600990217Codice cliente inesistente                                                     1
