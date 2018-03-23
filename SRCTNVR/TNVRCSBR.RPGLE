000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200050411     FTITAS30C  IF   E           K DISK
000300050411     FTNCSB05L  IF   E           K DISK
000400050411     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000500050411     FPRTF198X  O    F  198        PRINTER OFLIND(*INOG)
000600050411     FQSYSPRT   O    F  132        PRINTER
000700050411      *------------------------------------------------------------------------*
000800050411     C     kcsb          klist
000900050411     C                   kfld                    KAAS              4 0
001000050411     C                   kfld                    Kmgs              4 0
001100050411
001200050411     C     kTAS          klist
001300050411     C                   kfld                    csbAAS
001400050411     C                   kfld                    csbLNP
001500050411     C                   kfld                    csbNRS
001600050411     C                   kfld                    csbNSP
001700050411     C                   kfld                    csbTBL
001800050411
001900050411     C                   SETON                                        OFOG
002000050411     c                   z-add     2005          KAAS
002100050411     c                   z-add     0320          Kmgs
002200050411
002300050411     c     kcsb          setll     tncsb05l
002400050411     c     kaas          reade     tncsb05l
002500050411    1c                   DOW       not %EOF(TNCSB05L)
002600050411    2c                   IF        %SUBST(CSBTBL:1:1) = 'A'
002700050411      *
002800050411     c                   move      CSBCDI        W040              4 0
002900050411    3c                   IF        w040 <> 9999
003000050411     c     KTAS          chain     titas30c
003100050411    4c                   select
003200050411   x4c                   when      NOT %FOUND(TITAS30C)
003300050411     c                   EXCEPT    ERRORE
003400050411   x4c                   when      csbcdi <> tasccm
003500050411     c                   exsr      stampa
003600050411    4c                   ENDSL
003700050411      *
003800050411    3c                   ENDIF
003900050411    2c                   ENDIF
004000050411     c     kaas          reade     tncsb05l
004100050411    1c                   ENDDO
004200050411      *
004300050411     C                   EXCEPT    TESTA_PAG
004400050411     C                   EXCEPT    TOT_PAG
004500050411     C                   EXCEPT    TESTA_nPAG
004600050411     C                   EXCEPT    TOT_nPAG
004700050411     C                   EVAL      *INLR = *ON
004800050411
004900050411     C     STAMPA        BEGSR
005000050411      *
005100050411     C                   IF            CSBBNA <> *zeros
005200050411     C                   IF            csbbna <> 9999999
005300050411     C                   ADD       CSBCAS        PAGA_BAR         12 3
005400050411     C                   ELSE
005500050411     C                   ADD       CSBCAS        PAGA_MIT         12 3
005600050411     C                   ENDIF
005700050411     C   OF              EXCEPT    TESTA_PAG
005800050411     c                   eval      *inof = *off
005900050411     C                   EXCEPT    DETTA_PAG
006000050411      *
006100050411     C                   ELSE
006200050411     C                   IF              csbtpi <> 'M'
006300050411     C                   ADD       CSBCAS        NPAGA_BAR        12 3
006400050411     C                   ELSE
006500050411     C                   ADD       CSBCAS        NPAGA_MIT        12 3
006600050411     C                   ENDIF
006700050411     C   OG              EXCEPT    TESTA_NPAG
006800050411     c                   eval      *inog = *off
006900050411     C                   EXCEPT    DETTA_NPAG
007000050411     C                   ENDIF
007100050411      *
007200050411     C                   ENDSR
007300050411
007400050411     OQSYSPRT   E            ERRORE      2
007500050411     O                                           30 'TNVRCSBR: ERRORE MANCA BOL-
007600050411     O                                              LLA:'
007700050411     O                       CSBAAS           +   3
007800050411     O                       CSBLNP           +   3
007900050411     O                       CSBNRS           +   3
008000050411     O                       CSBNSP           +   3
008100050411     O                       CSBTBL           +   3
008200050411     OPRTF198   E            TESTA_PAG      1 02
008300050411     O                                           60 'TNVRCSBR: C/ASSEGNI RIMBOR-
008400050411     O                                              SATI'
008500050411     OPRTF198   E            DETTA_PAG   1
008600050411     O                       CSBAAS           +   3
008700050411     O                       CSBLNP           +   3
008800050411     O                       CSBNRS           +   3
008900050411     O                       CSBNSP           +   3
009000050411     O                       CSBTBL           +   3
009100050411     O                       CSBCDI           +   3
009200050411     O                       TASCCM           +   3
009300050411     O                       CSBBNA           +   3
009400050411     O                       CSBCAS        2  +   3
009500050411     O                       CSBVCA           +   3
009600050411     OPRTF198   E            TOT_PAG     1
009700050411     O                                              'Rimborsati a mittente:'
009800050411     O                       PAGA_MIT      2  +   3
009900050411     O                                        +   5 'Rimborsati da Bartolini'
010000050411     O                       PAGA_BAR      2  +   3
010100050411      *
010200050411     OPRTF198x  E            TESTA_nPAG     1 02
010300050411     O                                           60 'TNVRCSBR: C/ASSEGNI DA RIM-
010400050411     O                                              BORSARE'
010500050411     OPRTF198x  E            DETTA_nPAG  1
010600050411     O                       CSBAAS           +   3
010700050411     O                       CSBLNP           +   3
010800050411     O                       CSBNRS           +   3
010900050411     O                       CSBNSP           +   3
011000050411     O                       CSBTBL           +   3
011100050411     O                       CSBCDI           +   3
011200050411     O                       TASCCM           +   3
011300050411     O                       csbtpi           +   3
011400050411     O                       CSBCAS        2  +   3
011500050411     O                       CSBVCA           +   3
011600050411     OPRTF198x  E            TOT_nPAG    1
011700050411     O                                              'Da rimborsare a mittente:'
011800050411     O                       nPAGA_MIT     2  +   3
011900050411     O                                        +   5 'Da rimborsare Bartolini'
012000050411     O                       nPAGA_BAR     2  +   3
