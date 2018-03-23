000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300030326      * TNSC78R                                                      *
000400930510      *        TRASMISSIONE                                          *
000500030326      *        FIVABCAF A 001                                        *
000600900606?     *--------------------------------------------------------------*
000700030326     D DSCMZ         E DS                  INZ
000800030326     d KPJBA         E DS
000900030326     d tibs55ds      E DS
001000971121?     *--------------------------------------------------------------*
001100030326     C     *ENTRY        PLIST
001200030326     C                   PARM                    KPJBA
001300030326?     *--------------------------------------------------------------*
001400020805     C                   CLEAR                   TIBS55ds
001500971121     C                   MOVEL     'L'           I50TLA
001600971121     C                   MOVEL     KNSIF         I50PSI
001700030326     C                   MOVEL     046           I50PPO
001800030326     C                   MOVEL     001           I50APO
001900020805     C                   CALL      'TIBS55R'
002000020805     C                   PARM                    TIBS55ds
002100030326     C* GUARDO SE IL MEMBRO è VUOTO - FIVABCAF
002200030326     C                   MOVEL(P)  'FIVABCAF'    FILE             10
002300030326     C                   MOVEL(p)  'FIVABCAF'    MBR              10
002400030326     C                   MOVEL(p)  O50PLG        LIBR             10
002500030326     C                   CALL      'TRUL51C'
002600030326     C                   PARM                    LIBR
002700030326     C                   PARM                    FILE
002800030326     C                   PARM                    MBR
002900030326     C                   PARM                    NRR              10 0
003000030326      *
003100030326     c                   if        nrr > 0
003200030326     C                   CLEAR                   DsCmz
003300030326     C                   MOVEL     O50LCL        CMZLCL            1
003400030326     C                   MOVE      '001'         CMZDST
003500030326     C                   MOVEL     O50PLG        CMZLBD
003600030326     C                   MOVEL     O50ALG        CMZLBA
003700030326     C                   MOVEL     O50ASI        CMZSYF
003800030326     C                   MOVEL     'FIVABCAF'    CMZFLD           10
003900030326     C                   MOVEL     'FIVAB00F'    CMZFLA           10
004000030326     C                   MOVE      '0000'        CMZORI
004100030326     C                   MOVE      '0000000'     CMZDTI
004200030326     C                   MOVE      'A'           CMZADD
004300030326     C                   MOVE      'S'           CMZCLR
004400030326     C                   MOVE      'S'           CMZFTR
004500030326     C                   MOVE      *BLANK        CMZERR
004600030326     C                   MOVE      'N'           CMZURG
004700030326     C                   MOVEL     'FIVABCAF'    CMZMBD
004800030326     C                   MOVEL     'FIVAB00F'    CMZMBA
004900030326     C                   move      nrr           CMZNRR
005000030326     C                   CLEAR                   W236
005100030326      *
005200030326     C                   CALL      'TIBS53C'
005300030326     C                   PARM                    CMZLCL
005400030326     C                   PARM                    CMZDST
005500030326     C                   PARM                    CMZFLD
005600030326     C                   PARM                    CMZMBD
005700030326     C                   PARM                    CMZLBD
005800030326     C                   PARM                    CMZFLA
005900030326     C                   PARM                    CMZMBA
006000030326     C                   PARM                    CMZLBA
006100030326     C                   PARM                    CMZORI
006200030326     C                   PARM                    CMZDTI
006300030326     C                   PARM                    CMZURG
006400030326     C                   PARM                    CMZADD
006500030326     C                   PARM                    CMZCLR
006600030326     C                   PARM                    CMZFTR
006700030326     C                   PARM                    CMZERR
006800030326     C                   PARM                    CMZAZN
006900030326     C                   PARM                    CMZSYF
007000030326     C                   PARM                    CMZNRR
007100030326     C                   PARM                    CMZPRR
007200030326     C                   PARM                    W236            236
007300030326     c                   end
007400030331     C* GUARDO SE IL MEMBRO è VUOTO - FIVADCAF
007500030331     C                   MOVEL(P)  'FIVADCAF'    FILE             10
007600030331     C                   MOVEL(p)  'FIVADCAF'    MBR              10
007700030331     C                   MOVEL(p)  O50PLG        LIBR             10
007800030331     C                   CALL      'TRUL51C'
007900030331     C                   PARM                    LIBR
008000030331     C                   PARM                    FILE
008100030331     C                   PARM                    MBR
008200030331     C                   PARM                    NRR              10 0
008300030331      *
008400030331     c                   if        nrr > 0
008500030529     C*                  CLEAR                   DsCmz
008600030331     C                   MOVEL     O50LCL        CMZLCL            1
008700030331     C                   MOVE      '001'         CMZDST
008800030331     C                   MOVEL     O50PLG        CMZLBD
008900030331     C                   MOVEL     O50ALG        CMZLBA
009000030331     C                   MOVEL     O50ASI        CMZSYF
009100030331     C                   MOVEL     'FIVADCAF'    CMZFLD           10
009200030331     C                   MOVEL     'FIVAD00F'    CMZFLA           10
009300030331     C                   MOVE      '0000'        CMZORI
009400030331     C                   MOVE      '0000000'     CMZDTI
009500030331     C                   MOVE      'A'           CMZADD
009600030331     C                   MOVE      'S'           CMZCLR
009700030331     C                   MOVE      'S'           CMZFTR
009800030331     C                   MOVE      *BLANK        CMZERR
009900030331     C                   MOVE      'N'           CMZURG
010000030331     C                   MOVEL     'FIVADCAF'    CMZMBD
010100030331     C                   MOVEL     'FIVAD00F'    CMZMBA
010200030331     C                   move      nrr           CMZNRR
010300030331     C                   CLEAR                   W236
010400030331      *
010500030331     C                   CALL      'TIBS53C'
010600030331     C                   PARM                    CMZLCL
010700030331     C                   PARM                    CMZDST
010800030331     C                   PARM                    CMZFLD
010900030331     C                   PARM                    CMZMBD
011000030331     C                   PARM                    CMZLBD
011100030331     C                   PARM                    CMZFLA
011200030331     C                   PARM                    CMZMBA
011300030331     C                   PARM                    CMZLBA
011400030331     C                   PARM                    CMZORI
011500030331     C                   PARM                    CMZDTI
011600030331     C                   PARM                    CMZURG
011700030331     C                   PARM                    CMZADD
011800030331     C                   PARM                    CMZCLR
011900030331     C                   PARM                    CMZFTR
012000030331     C                   PARM                    CMZERR
012100030331     C                   PARM                    CMZAZN
012200030331     C                   PARM                    CMZSYF
012300030331     C                   PARM                    CMZNRR
012400030331     C                   PARM                    CMZPRR
012500030331     C                   PARM                    W236            236
012600030331     c                   end
012700030326     C                   SETON                                        LR
