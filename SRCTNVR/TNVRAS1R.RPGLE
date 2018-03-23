000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030929     FTITAS31C  IF   E           K DISK
000300030929     FTIASC02L  IF   E           K DISK
000400980709     FTABEL00F  IF   E           K DISK
000500030929     FTIASC999  O    E             DISK
000600030929     F                                     RENAME(TIASC000:TN999)
000700980709     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000800980709     D* TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
000900980709     D TBN             S              2    DIM(20)                              TIPI BOLLA NO ASSIC
001000980709     D DSTB          E DS
001100961108     C*
001200980703     C     KTAS          KLIST
001300980703     C                   KFLD                    WKSC
001400980703     C                   KFLD                    DFT
001500980703     C     KASC          KLIST
001600980703     C                   KFLD                    TASAAS
001700980703     C                   KFLD                    TASLNP
001800980703     C                   KFLD                    TASNRS
001900980703     C                   KFLD                    TASNSP
002000980703     C                   KFLD                    TASTBL
002100980709     C     KTAB          KLIST
002200980709     C                   KFLD                    CODUT             1 0
002300980709     C                   KFLD                    COD               2
002400980709     C                   Z-ADD     1             CODUT
002500030929     C                   Z-ADD     20030730      DFT               8 0
002600980709     C**
002700980709     C                   MOVEL     'TB'          COD
002800980709     C                   Z-ADD     0             K                 4 0
002900980709     C     KTAB          SETLL     TABEL                                  07
003000980709     C     KTAB          READE     TABEL                                  07
003100980709    1C     *IN07         DOWEQ     *OFF
003200980709    2C     TBLKEY        IFNE      *BLANKS
003300980709     C     TBLFLG        ANDEQ     ' '
003400980709     C                   MOVEL     TBLUNI        DSTB
003500980709    3C     §TBFAS        IFEQ      '0'
003600980709     C                   ADD       1             K
003700980709     C                   MOVEL     TBLKEY        TBN(K)
003800980709    3C                   ENDIF
003900980709    2C                   ENDIF
004000980709     C     KTAB          READE     TABEL                                  07
004100980709    1C                   ENDDO
004200980703     C**
004300030929     C     *LOVAL        SETLL     TITAS31C
004400030929     C                   READ      TITAS31C                               30
004500980703     C**
004600970129    1C     *IN30         DOWEQ     *OFF
004700980703     C                   Z-ADD     TASKSC        WKSC              7 0
004800980703     C**
004900030929     C     KTAS          SETLL     TITAS31C
005000030929     C     WKSC          READE     TITAS31C                               31
005100980703     C     *IN31         DOWEQ     *OFF
005200030929     C     TASDFT        IFGT      20030802
005300980703     C                   SETON                                        31
005400980703     C                   ELSE
005500980703     C**
005600980703     C     TASIAS        IFGT      0
005700980709     C     TASTBL        LOOKUP    TBN                                    05
005800980709     C     *IN05         IFEQ      *OFF
005900030929     C     KASC          CHAIN     TIASC000                           90
006000980709     C     *IN90         IFEQ      *ON
006100980709     C                   EXCEPT    ERR1
006200980709     C                   ADD       1             ERR               4 0
006300980709     C**
006400980709     C                   ELSE
006500980709     C                   MOVE      TASKSC        W0040             4 0
006600980709     C     W0040         IFEQ      9999
006700980709     C                   ADD       1             CON99             4 0
006800980709     C                   WRITE     TN999
006900980709     C                   ELSE
007000980709     C                   ADD       1             CON88             4 0
007100980709     C                   ENDIF
007200980709     C                   ENDIF
007300980709     C                   ENDIF
007400980709     C                   ENDIF
007500980703     C**
007600030929     C     WKSC          READE     TITAS31C                               31
007700980703     C                   ENDIF
007800980703     C                   ENDDO
007900030929     C     WKSC          SETGT     TITAS31C
008000030929     C                   READ      TITAS31C                               30
008100980703     C                   ENDDO
008200000000     C*
008300980709     C                   EXCEPT    STAMPA
008400000000     C                   SETON                                        LR
008500980709     OPRTF198   E            ERR1        1
008600980709     O                                         +  1 'BOLLA:'
008700980709     O                       TASKSC        Z   +  2
008800980709     O                       TASAAS        Z   +  2
008900980709     O                       TASLNP        Z   +  2
009000980709     O                       TASNRS        Z   +  2
009100980709     O                       TASNSP        Z   +  2
009200980709     O                       TASTBL            +  2
009300980709     O                       TASLNA        Z   +  2
009400980709     O                       TASIAS        4   +  2
009500980709     O                       TASNFT        Z   +  2
009600980709     O                       TASDFT            +  2 '    /  /  '
009700980709     O                                         +  2 'NON TROVATO TNASC'
009800980709     OPRTF198   E            STAMPA      1
009900980709     O                                         +  1 'TOTALE BOLLE ERRATE:'
010000980709     O                       ERR           Z   +  2
010100980709     O                                         +  5 'TOTALE BOLLE 9999'
010200980709     O                       CON99         Z   +  2
010300980709     O                                         +  5 'TOTALE BOLLE ALTRE'
010400980709     O                       CON88         Z   +  2
