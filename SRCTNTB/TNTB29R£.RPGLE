000100021022      *------------------------------------------------------------------------*
000200021022      * GESTIONE TABELLA CLIENTI CON PARTICOLARITA' PER DENUNCIA "NON EVENTI"  *
000300021022      *------------------------------------------------------------------------*
000400021022
000500021022      *--------------------------------------------
000600021022      *  RIEPILOGO INDICATORI
000700021022      *--------------------------------------------
000800021022      * 01 - RECORD annullato
000900021022      * 02 - acceso IMMISSIONE - spento MANUTENZIONE
001000021022      *--------------------------------------------
001100021022
001200021022     H DECEDIT('0,') DATEDIT(*DMY.)
001300021022
001400021022     FTNTB29D   CF   E             WORKSTN sfile(TB29dsf:nrr)
001500021022     FTNTBE01L  UF A E           K DISK
001600021022     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
001700021022
001800021022      *--------------------------------------------
001900021022     D Kcod            S                   LIKE(TBEcod)   inz('DDM')
002000021022     D Kke1            S                   LIKE(TBEke1)
002100021022     D Kke2            S                   LIKE(TBEke2)
002200021022     D Klin            S                   LIKE(TBElin)
002300021022     D Ksif            S                   LIKE(TBEsif)
002400021022     d TIBS69DS      E DS
002500021022     d DS_cnaco      E DS                  extname(CNACO00F)
002600021022     d DS_cnind      E DS                  extname(CNIND00F)
002700021022     d DS_cnclp      E DS                  extname(CNCLP00F)
002800021022     d DS_fncls      E DS                  extname(FNCLS00F)
002900021022     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
003000021022      *
003100021022     D KPJBA         E DS
003200021022     D UT§DSE0F      E DS
003300021022     D  TCU                  398    697    DIM(50)                              TAB FLAG CNT
003400021022     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    TAB CAPO CONTO
003500021022     D TCUDS           DS
003600021022     D  F1                     1      1
003700021022     D  F3                     3      3
003800021022     D  F2                     2      2
003900021022     D  F4                     4      4
004000021022     D  F56                    5      6
004100021022      *--------------------------------------------
004200021022      * Carico tutti i record presenti
004300021022     c     inizio        tag
004400021022     c                   EXSR      CARICA
004500021022     c                   clear                   v1cksc
004600021022     c                   clear                   v1dksc
004700021022
004800021022      * EMISSIONE PRIMA VIDEATA
004900021022     C     for01         TAG
005000021022     c                   write     TB29z01
005100021022     C                   EXFMT     TB29dct
005200021022     C                   clear                   V1CMSG
005300021022     C                   setoff                                       9028
005400021022      *
005500021022      * F3=FINE
005600021022     c   KC              goto      fine
005700021022      *
005800021022      * CONTROLLI VIDEATA
005900021022     C                   EXSR      CTRD01
006000021022     C   90              goto      for01
006100021022      *
006200021022      * f6 - conferma   F16 - annullamento
006300021022     c                   if        *inkf  OR
006400021022     c                             *inkq
006500021022     c                   EXSR      CONFERMA
006600021022     c                   goto      inizio
006700021022     c                   endif
006800021022     C                   goto      for01
006900021022      *
007000021022     C     FINE          TAG
007100021022      *
007200021022     C                   EVAL      *INLR = *ON
007300021022      *****************************************************************
007400021022      *   carico tutti gli elementi della tabella
007500021022      *****************************************************************
007600021022     c     CARICA        BEGSR
007700021022     c                   clear                   NRR               4 0
007800021022     c
007900021022     c                   eval      *in35='1'
008000021022     c                   write     TB29dct
008100021022     c                   eval      *in35='0'
008200021022     c
008300021022     c     ktbe2         chain     tntbe02l                           30
008400021022     c                   dow       not *in30
008500021022     c                   if        tbeatb=' '
008600021022      *
008700021022     c                   movel     tbeke1        vscksc
008800021022     c
008900021022     C                   clear                   TIBS69DS
009000021022     c                   z-add     vscksc        I69kac
009100021022      *
009200021022     C                   CALL      'TIBS69R'
009300021022     C                   PARM                    tibs69DS
009400021022     C                   PARM                    DS_cnaco
009500021022     C                   PARM                    DS_cnind
009600021022     C                   PARM                    DS_cnclp
009700021022     C                   PARM                    DS_fncls
009800021022    2c                   if        o69err=' '
009900021022     c                   movel     acorag        vsdksc
010000021022    2c                   endif
010100021022     c
010200021022     c                   add       1             nrr
010300021022     c                   write     TB29dsf
010400021022     c                   endif
010500021022     c
010600021022     c     ktbe2         reade     tntbe02l                               30
010700021022     c                   enddo
010800021022     c
010900021022     c
011000021022      * Se ci sono dei record visualizzo il sfl
011100021022     c                   if        nrr>0
011200021022     c                   eval      nrr=1
011300021022     c                   eval      *in36='1'
011400021022     c                   endif
011500021022     c
011600021022     c                   ENDSR
011700021022      *****************************************************************
011800021022      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
011900021022      *****************************************************************
012000021022     C     CTRD01        BEGSR
012100021022     c                   if        *inkg
012200021022     c                   movel     rsut          paxdut           30
012300021022     c                   movel(p)  v1dksc        paxdmt           48
012400021022      * PAXSTA=9 ESCLUDO ANNULLATI
012500021022     C                   Z-ADD     9             PAXSTA
012600021022     c                   movel     kci           paxccc            4 0
012700021022     c                   clear                   paxdit
012800021022     C                   z-add     1             paxnum
012900021022     C                   CALL      'XALFA3BR'
013000021022     C                   PARM                    PAXDUT
013100021022     C                   PARM                    CODUT
013200021022     C                   PARM                    PAXDMT
013300021022     C                   PARM                    PAXCCC
013400021022     C                   PARM                    PAXSTA            1 0
013500021022     C                   PARM                    PAXFLR           90
013600021022     C                   PARM                    PAXDIT            3
013700021022     C                   PARM                    PAXNUM            2 0
013800021022     C                   PARM                    PAXKCM           80
013900021022     C                   PARM                    PAXKSM          140
014000021022     C                   PARM                    PAXKDM           60
014100021022    2C     PAXSTA        IFGT      -1
014200021022     C                   MOVEL     paxksm        v1cksc
014300021022     C                   MOVEL     paxdmt        v1dksc
014400021022     c                   endif
014500021022      *
014600021022     c                   seton                                        90
014700021022     c                   goto      endct1
014800021022     c                   endif
014900021022     c
015000021022      * Controllo ksc
015100021022     c                   if        v1cksc>0
015200021022     C                   clear                   TIBS69DS
015300021022     c                   z-add     v1cksc        I69kac
015400021022      *
015500021022     C                   CALL      'TIBS69R'
015600021022     C                   PARM                    tibs69DS
015700021022     C                   PARM                    DS_cnaco
015800021022     C                   PARM                    DS_cnind
015900021022     C                   PARM                    DS_cnclp
016000021022     C                   PARM                    DS_fncls
016100021022    2c                   if        o69err<>' '
016200021022     c                   movel     msg(1)        v1cmsg
016300021022     c                   seton                                        9028
016400021022     c                   goto      endct1
016500021022     c                   else
016600021022     c                   movel     acorag        v1dksc
016700021022    2c                   endif
016800021022    2c                   endif
016900021022     c     endct1        ENDSR
017000021022      *
017100021022      *****************************************************************
017200021022      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
017300021022      *****************************************************************
017400021022     C     CONFERMA      BEGSR
017500021022     c                   if        v1cksc>0
017600021022     c                   movel(p)  v1cksc        Kke1
017700021022     c     ktbe          chain     tntbe01l                           31
017800021022     c   31              clear                   tntbe000
017900021022      *
018000021022     c                   movel(p)  v1dksc        TBEuni
018100021022     c                   movel     'S'           tbeftt
018200021022      *
018300021022     C   KQ              movel     'A'           TBEatb
018400021022     C  NKQ              movel     *blanks       TBEatb
018500021022      *
018600021022     c                   clear                   TBEftr
018700021022     c                   clear                   TBEdtr
018800021022      *
018900021022     c                   IF        *IN31 = *ON
019000021022     c                   movel     Ksif          TBEsif
019100021022     c                   movel     Klin          TBElin
019200021022     c                   movel     Kcod          TBEcod
019300021022     c                   movel     Ksif          TBEsif
019400021022     c                   movel     Kke2          TBEke2
019500021022     c                   movel     v1cksc        TBEke1
019600021022     C                   WRITE     TNTBE000
019700021022      *
019800021022     c                   ELSE
019900021022     C                   UPDATE    TNTBE000
020000021022     c                   ENDIF
020100021022     c                   ENDIF
020200021022      *
020300021022     C                   ENDSR
020400021022      *****************************************************************
020500021022      *   ROUTINE INIZIALE
020600021022      *****************************************************************
020700021022     C     *INZSR        BEGSR
020800021022      *
020900021022     C     *ENTRY        PLIST
021000021022     C                   PARM                    KPJBA
021100021022      *
021200021022     C                   CALL      'X§PARUT'
021300021022     C                   PARM                    UT§DSE0F
021400021022     C                   MOVEL     RAGUT         RSUT
021500021022     c
021600021022     C*--- RICERCA CAPOCONTI
021700021022     C                   DO        50            X                 2 0
021800021022     C                   MOVE      TCU(X)        TCUDS
021900021022     C     F56           CABNE     'CG'          END1
022000021022     C     F4            COMP      '1'                                    21
022100021022     C     F4            COMP      '2'                                    22
022200021022     C     F4            COMP      '3'                                    23
022300021022     C     F4            COMP      '6'                                    27
022400021022     C** 1 CLIENTI   21
022500021022     C** 2 FORNITORI 22
022600021022     C** 3 AGENTI    23
022700021022     C     F3            COMP      '0'                                242425
022800021022     C     F3            COMP      'I'                                    26
022900021022     C** 0 ITALIA   25
023000021022     C** 1 ESTERO   24
023100021022     C** I CAPO CONTO IVA
023200021022     C   21
023300021022     CAN 24              Z-ADD     KCU(X)        KCE               4 0
023400021022     C   21
023500021022     CAN 25              Z-ADD     KCU(X)        KCI               4 0
023600021022     C   22
023700021022     CAN 24              Z-ADD     KCU(X)        KFE               4 0
023800021022     C   22
023900021022     CAN 25              Z-ADD     KCU(X)        KFI               4 0
024000021022     C   23
024100021022     CAN 24              Z-ADD     KCU(X)        KAE               4 0
024200021022     C   23
024300021022     CAN 25              Z-ADD     KCU(X)        KAI               4 0
024400021022     C   26              Z-ADD     KCU(X)        KIVA              4 0
024500021022     C   27              Z-ADD     KCU(X)        KBNA              4 0
024600021022     C     END1          TAG
024700021022     C                   END
024800021022     C                   SETOFF                                       212223
024900021022     C                   SETOFF                                       242526
025000021022     C                   SETOFF                                       27
025100021022      *
025200021022     C     KTBE          klist
025300021022     C                   KFLD                    Kcod
025400021022     C                   KFLD                    Kke1
025500021022     C                   KFLD                    Kke2
025600021022     C                   KFLD                    Klin
025700021022     C                   KFLD                    Ksif
025800021022      *
025900021022     c     KTBE2         klist
026000021022     C                   KFLD                    Kcod
026100021022     C                   KFLD                    Klin
026200021022     C                   KFLD                    Ksif
026300021022      *
026400021022     C                   ENDSR
026500021022      *****************************************************************
026600021022** MSG  (Lungh. 78)                                                          *
026700021022Codice cliente inesistente                                                     1
