000100000630      **************************************************************************
000200000630      *                                                                        *
000300000630      *   *----------------------------------------------------------------*   *
000400000630      *    PULIZIA FILES PER SEDE - CALCOLA DATE E LANCIA I PROGRAMMI BATCH    *
000500000630      *   *----------------------------------------------------------------*   *
000600000630      *                                                                        *
000700000630      **************************************************************************
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000630     FTABEL00F  IF   E           K DISK
001000000630      *
001100000630      * DS PASSAGGIO PARAMETRI
001200000630      *
001300000630     D PARAM           DS                  INZ
001400000630     D  DATANM                 1      8  0                                      DATA PUL. FNANM00F
001500000630     D  ANSAC0                 9     12  0                                      ANNO PUL. TFSAC00F
001600101129     D  vuoto                 13     16  0                                      ANNO PUL. TFSAC00F
001700050404     d  datrcq                17     24  0
001800050404     d  datlac                25     32  0
001900050404     d  datlao                33     40  0
002000060908     d  DatRFT                        8  0 inz
002100070111     d  DatLAD                        8s 0 inz
002101131202     d  Datvrb                        8s 0 inz
002200000630      *
002300000630      * DS PER CONTROLLO E INVERSIONE DATE
002400000630      *
002500000630     D WLBDAT          DS
002600000630     D  G02DAT                 1      8  0
002700000630     D  G02INV                 9     16  0
002800000630     D  G02ERR                17     17
002900000630     D  G02TGI                18     22  0
003000000630     D WGIDAT          DS
003100000630     D  GIODAT                 1      8  0
003200000630     D  GIOINV                 9     16  0
003300000630     D  GIOTGI                17     21  0
003400000630     D KPJBA         E DS
003500000630      *
003600000630      * DS PER TABELLA 5A (TABEL00F)
003700000630      *
003800000630     D DS5AS1        E DS
003900050401
004000050401     d w_datrcq        s               d   datfmt(*iso)
004001131202     d w_datiso        s               d   datfmt(*iso)
004100050404     d sav_tgi         s                   like(g02tgi)
004200101022     d datvabwk        s              8  0
004300000630      **************************************************************************
004400000630      *                                                                        *
004500000630      *         O P E R A Z I O N I   I N I Z I A L I                          *
004600000630      *                                                                        *
004700000630      **************************************************************************
004800000630     C     *ENTRY        PLIST
004900000630     C                   PARM                    KPJBA
005000000630     C                   Z-ADD     1             CODUT
005100000630      *
005200000630     C     KTAB          KLIST
005300000630     C                   KFLD                    CODUT             1 0
005400000630     C                   KFLD                    COD
005500000630     C                   KFLD                    KEY
005600000630      *
005700000630     C     *LIKE         DEFINE    TBLCOD        COD
005800000630     C     *LIKE         DEFINE    TBLKEY        KEY
005900000630      *
006000000630      * RECUPERO DA TABELLA 5A I PARAMETRI DI PULIZIA
006100000630      *
006200000630     C                   MOVEL     '5A'          COD
006300000630     C                   MOVEL     'SEDE1   '    KEY
006400000630     C     KTAB          CHAIN     TABEL00F                           30
006500000630     C  N30              MOVEL     TBLUNI        DS5AS1
006600000630     C   30              CLEAR                   DS5AS1
006700000630      *
006800000630      * INIZIALIZZO DATE
006900000630      *
007000000630     C                   Z-ADD     0             DATANM
007100000630     C                   Z-ADD     0             ANSAC0
007200050404     c                   z-add     0             datrcq
007300050401     C                   Z-ADD     0             datlac
007400050401     C                   Z-ADD     0             datlao
007500070111     c*** già così:      clear                   DatLAD
007600000630      **************************************************************************
007700000630      *                                                                        *
007800000630      *         R O U T I N E   P R I N C I P A L E                            *
007900000630      *                                                                        *
008000000630      **************************************************************************
008100000630      *
008200000630      * RICAVO IL NR. DI GIORNI DALLA DATA ODIERNA
008300000630      *
008400000630     C                   TIME                    WTIME            14 0          ORA E DATA
008500000630     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
008600000630     C                   MOVE      WDATE         G02DAT
008700000630     C                   MOVEL     *BLANK        G02ERR
008800000630     C                   CALL      'XSRDA8'
008900000630     C                   PARM                    WLBDAT
009000050404     c                   z-add     g02tgi        sav_tgi
009100000630      *
009200000630      * DATA DI PULIZIA DEL FILE FNANM00F (PER DATA CHIUSURA ANOMALIA)
009300000630      * ==============================================================
009400000630      *
009500000630     C     G02TGI        SUB       §5AS11        GIOTGI
009600000630     C                   CALL      'XSRGI8'
009700000630     C                   PARM                    WGIDAT
009800000630     C                   Z-ADD     GIODAT        WRKDAT            8 0          GG/MM/AAAA
009900000630     C*
010000000630     C     WRKDAT        IFGT      0
010100000630     C                   MOVE      WRKDAT        G02DAT
010200000630     C                   MOVEL     *BLANK        G02ERR
010300000630     C                   CALL      'XSRDA8'
010400000630     C                   PARM                    WLBDAT
010500000630     C     G02ERR        IFEQ      '1'
010600000630     C                   Z-ADD     0             DATANM                         AAAA/MM/GG
010700000630      *                  ------------------------------
010800000630     C                   ELSE
010900000630     C                   Z-ADD     G02INV        DATANM                         AAAA/MM/GG
011000000630      *                  ------------------------------
011100000630     C                   ENDIF
011200000630     C                   ENDIF
011300000630      *
011400000630      * ANNO DI PULIZIA DEL FILE TFSAC00F
011500000630      * =================================
011600000630      *
011700000630     C     *YEAR         SUB       §5AS12        ANSAC0
011800050401      *
011900050401      * Data di pulizia del file WFRCQ10F
012000050401      * =================================
012100050401     c                   z-add     §5as1rcq      ms                2 0
012200050401     c     *eur          move      wdate         w_datrcq
012300050401     c* sottraggo i mesi alla data corrente
012400050401     c     w_datrcq      subdur    ms:*months    W_datrcq
012500050401     c                   move      W_datrcq      datrcq
012600050401     c* vado a inizio mese
012700050401     c                   move      01            datrcq
012800050401      *
012900050401      * Data di pulizia del file TILAC00f
013000050401      * =================================
013100050404     C     sav_tgi       SUB       §5AS1LAC      GIOTGI
013200050401     C                   CALL      'XSRGI8'
013300050401     C                   PARM                    WGIDAT
013400050401     C                   Z-ADD     GIODAT        WRKDAT            8 0          GG/MM/AAAA
013500050401     C*
013600050401     C     WRKDAT        IFGT      0
013700050404     c                   clear                   wlbdat
013800050401     C                   MOVE      WRKDAT        G02DAT
013900050401     C                   MOVEL     *BLANK        G02ERR
014000050401     C                   CALL      'XSRDA8'
014100050401     C                   PARM                    WLBDAT
014200050401     C     G02ERR        IFEQ      '1'
014300050401     C                   Z-ADD     0             DATLAC                         AAAA/MM/GG
014400050401      *                  ------------------------------
014500050401     C                   ELSE
014600050401     C                   Z-ADD     G02INV        DATLAC                         AAAA/MM/GG
014700050401      *                  ------------------------------
014800050401     C                   ENDIF
014900050401     C                   ENDIF
015000050401      *
015100050401      * Data di pulizia del file TILAO00f
015200050401      * =================================
015300050404     C     sav_tgi       SUB       §5AS1LAO      GIOTGI
015400050401     C                   CALL      'XSRGI8'
015500050401     C                   PARM                    WGIDAT
015600050401     C                   Z-ADD     GIODAT        WRKDAT            8 0          GG/MM/AAAA
015700050401     C*
015800050401     C     WRKDAT        IFGT      0
015900050404     c                   clear                   wlbdat
016000050401     C                   MOVE      WRKDAT        G02DAT
016100050401     C                   MOVEL     *BLANK        G02ERR
016200050401     C                   CALL      'XSRDA8'
016300050401     C                   PARM                    WLBDAT
016400050401     C     G02ERR        IFEQ      '1'
016500050401     C                   Z-ADD     0             DATLAO                         AAAA/MM/GG
016600050401      *                  ------------------------------
016700050401     C                   ELSE
016800050401     C                   Z-ADD     G02INV        DATLAO                         AAAA/MM/GG
016900050401      *                  ------------------------------
017000050401     C                   ENDIF
017100050401     C                   ENDIF
017200070111      *
017300070111      * Data di pulizia del file TILAD00F
017400070111      * =================================
017500070111     c     Sav_tgi       sub       §5AS1lad      GIOtgi
017600070111     c                   call      'XSRGI8'
017700070111     c                   parm                    WGIdat
017800070111     c                   z-add     GIOdat        WrkDat            8 0          GG/MM/AAAA
017900070111      *
018000070111if 1 c                   if        WrkDat > *zeros
018100070111     c                   clear                   WLBdat
018200070111     c                   eval      G02dat = WrkDat
018300070111     c                   call      'XSRDA8'
018400070111     c                   parm                    WLBdat
018500070111if 2 c                   if        G02err = *on
018600070111     c                   clear                   DatLAD                         AAAA/MM/GG
018700070111x   2c                   else
018800070111     c                   eval      DatLAD = G02inv                              AAAA/MM/GG
018900070111e   2c                   endif
019000070111e   1c                   endif
019100060908      *
019200060908      * Data di pulizia del WorkFile WFRFT00F
019300060908      * =====================================
019400060908     c     Sav_tgi       sub       §5AS1rft      GIOtgi
019500060908     c                   call      'XSRGI8'
019600060908     c                   parm                    WGIdat
019700060908     c                   z-add     GIOdat        WrkDat            8 0          GG/MM/AAAA
019800060908      *
019900060908if 1 c                   if        WrkDat > *zeros
020000060908     c                   clear                   WLBdat
020100060908     c                   eval      G02dat = WrkDAt
020200060908     c                   call      'XSRDA8'
020300060908     c                   parm                    WLBdat
020400060908if 2 c                   if        G02err = *on
020500060908     c                   clear                   DatRFT                         AAAA/MM/GG
020600060908x   2c                   else
020700060908     c                   eval      DatRFT = G02inv                              AAAA/MM/GG
020800060908e   2c                   endif
020900060908e   1c                   endif
021000101022      *
021100101022      * Data di pulizia del     File FIVABWKF
021200101022      * =====================================
021300101022     c     Sav_tgi       sub       §5AS1vabwk    GIOtgi
021400101022     c                   call      'XSRGI8'
021500101022     c                   parm                    WGIdat
021600101022     c                   z-add     GIOdat        WrkDat            8 0          GG/MM/AAAA
021700101022      *
021800101022if 1 c                   if        WrkDat > *zeros
021900101022     c                   clear                   WLBdat
022000101022     c                   eval      G02dat = WrkDAt
022100101022     c                   call      'XSRDA8'
022200101022     c                   parm                    WLBdat
022300101022if 2 c                   if        G02err = *on
022400101022     c                   clear                   DatRFT                         AAAA/MM/GG
022500101022x   2c                   else
022600101022     c                   eval      DatVABWK= G02inv                              AAAA/MM/GG
022700101022e   2c                   endif
022800101022e   1c                   endif
022801131202
022802131211      * Data di pulizia del file TIVRB00F
022803131202      * =================================
022804131202     c                   z-add     §5asvrb       ms                2 0
022805131202     c     *eur          move      wdate         w_datiso
022806131202     c* sottraggo i mesi alla data corrente
022807131202     c     w_datiso      subdur    ms:*months    W_datiso
022808131202     c                   move      W_datiso      datvrb
022809131202     c* vado a inizio mese
022810131202     c                   move      01            datvrb
022811131202
022900000630      * LANCIO PULIZIA DEI VARI FILES
023000000630      * =============================
023100000630      *
023200000630     C     DATANM        IFGT      0
023300000630     C                   MOVEL     PARAM         KPJBU
023400041008     C                   CALL      'TNSD61R'                                    FNANM00F
023500000630      *                  -------------------                                    ========
023600000630     C                   PARM                    KPJBA
023700000630     C                   ENDIF
023800000630      *
023900000630     C     ANSAC0        IFGT      0
024000000630     C                   MOVEL     PARAM         KPJBU
024100041008     C                   CALL      'TNSD62R'                                    TFSAC00F
024200000630      *                  -------------------                                    ========
024300000630     C                   PARM                    KPJBA
024400000630     C                   ENDIF
024500050404     c*
024600050404     C     datrcq        IFGT      0
024700050404     C                   MOVEL     PARAM         KPJBU
024800050404     C                   CALL      'TNSD64R'                                    TFSAC00F
024900050404      *                  -------------------                                    ========
025000050404     C                   PARM                    KPJBA
025100050404     C                   ENDIF
025200050404
025300050404     C     datlac        IFGT      0
025400050404     C                   MOVEL     PARAM         KPJBU
025500050404     C                   CALL      'TNSD65R'                                    TFSAC00F
025600050404      *                  -------------------                                    ========
025700050404     C                   PARM                    KPJBA
025800120709
025900120709     C                   CALL      'TNSD69R'                                    TIJDC00F
026000120709      *                  -------------------                                    ========
026100120709     C                   PARM                    KPJBA
026200050404     C                   ENDIF
026300050404
026400050404     C     datlao        IFGT      0
026500050404     C                   MOVEL     PARAM         KPJBU
026600050404     C                   CALL      'TNSD66R'                                    TFSAC00F
026700050404      *                  -------------------                                    ========
026800050404     C                   PARM                    KPJBA
026900050404     C                   ENDIF
027000070111      * File TILAD00F
027100070111     c                   if        DatLAD > *zeros
027200070111     c                   movel(p)  DatLAD        KPJBU
027300070111     c                   call      'TNSD68R'                                    WFRTF00F
027400070111     c                   parm                    KPJBA                          ========
027500120709     c                   call      'TNSD70R'                                    TIJDD00F
027600120709     c                   parm                    KPJBA                          ========
027700070111     c                   endif
027800060908
027900060908      * File WFRTF00F
028000060908     c                   if        DatRFT > *zeros
028100060908     c                   movel(p)  DatRFT        KPJBU
028200060908     c                   call      'TNSD67R'                                    WFRTF00F
028300060908     c                   parm                    KPJBA                          ========
028400060908     c                   endif
028500101022      * File FIVABWKF
028600101022     c                   if        DatVABWK > *zeros
028700101022     c                   movel(p)  DatVABWK      KPJBU
028800101022     c                   call      'TNSD54R'                                    FIVABWKF
028900101022     c                   parm                    KPJBA                          ========
029000101022     c                   endif
029001131202     c* File TIVRB00F
029002131202     C     datvrb        IFGT      0
029003131211     C                   MOVEL(p)  datvrb        KPJBU
029004131202     C                   CALL      'TNSD71R'                                    TIVRB00F
029005131202      *                  -------------------                                    ========
029006131202     C                   PARM                    KPJBA
029007131202     C                   ENDIF
029100000630      *
029200000630     C                   SETON                                        LR
