000100000707     H dftactgrp(*yes)
000200970526     H*------------------------------------------------------------------------*
000300991115     H* Esecuzione OPZIONI Gestione log TIVLT00F - Strategi Upload
000400970526     H*------------------------------------------------------------------------*
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900970526     F*------------
001000000707     Ftivlt03l  UF A E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400991112     D*------------
001500991112     D* ARCHITETTURA
001600991112     D*------------
001700991112     D KPJBA         E DS
001800991110     D*------------
001900991112     D* PASSAGGIO PARAMETRI
002000991110     D*------------
002100991112     D PARAM           DS                  INZ
002200991112     D  PLIB                   1     10
002300991112     D  POBJ                  11     20
002400991112     D  PMBR                  21     30
002500991112     D  PDAT                  31     38  0
002600991112     D  PORA                  39     44  0
002700991112     D  PKSC                  45     52
002800991115     D  PSTS                  53     53
002900991112     D  PTIP                  54     55
003000991112     D  PSQZ                  59     61  0
003100991112     D  POPZ                  62     63
003200000629     D  PFID                  64     73
003300000629     D  PMBD                  74     83
003400000629
003500000629     D* Informazioni sul job
003600000629     D psds           sds
003700000629     D  psjobname            244    253
003800000629     D  psuser               254    263
003900000629     D  psjobnbr             264    269s 0
004000000629
004100000707     D* Variabili di wrk
004200000707     D  maximum        s                   like(vltsqz) inz(*zeros)
004300000707
004400970526     C*------------------------------------------------------------------------*
004500970526     C* MAIN LINES
004600970526     C*------------------------------------------------------------------------*
004700970526     C*
004800991115     C                   Z-ADD     PDAT          KLTdat
004900991115     C                   Z-ADD     PORA          KLTora
005000991115     C                   MOVEL     PKSC          KLTksc
005100991115     C                   MOVEL     PSTS          KLTsts
005200991115     C                   MOVEL     PTIP          KLTtip
005300991115     C                   Z-ADD     PSQZ          KLTsqz
005400991112     C*
005500991112     C* VERIFICA DI SICUREZZA LETTURA FILE DI LOG
005600991112     C*
005700991115     C     KEYvlt        CHAIN     tivlt03l                           99
005800991112     C     *IN99         IFEQ      *OFF
005900991112     C*
006000991112     C     POPZ          IFEQ      ' 1'
006100991115     C     VLTsts        IFEQ      '2'
006200991115     C     VLTsts        OREQ      '4'
006300991115     C                   MOVEL     '4'           VLTsts
006400991115     C                   UPDATE    tivlt000
006500991115     C                   EXSR      NEWLOG
006600991115     C                   ENDIF
006700991115     C     VLTsts        IFEQ      '1'
006800991115     C     VLTsts        OREQ      '3'
006900991115     C                   MOVEL     '3'           VLTsts
007000991115     C                   UPDATE    tivlt000
007100991115     C                   EXSR      NEWLOG
007200991115     C                   ENDIF
007300991112     C                   ENDIF
007400991112     C*
007500991112     C     POPZ          IFEQ      ' 4'
007600991115     C                   DELETE    tivlt000
007700991112     C                   ENDIF
007800000629     C*
007900000629     C     POPZ          IFEQ      '11'
008000000629     C     VLTsts        IFEQ      '2'
008100000629     C     VLTsts        OREQ      '4'
008200000629     C                   MOVEL     '4'           VLTsts
008300000629     C                   UPDATE    tivlt000
008400010131     C***                EXSR      NEWLOG
008500000629     C                   ENDIF
008600000629     C     VLTsts        IFEQ      '1'
008700000629     C     VLTsts        OREQ      '3'
008800000629     C                   MOVEL     '3'           VLTsts
008900000629     C                   UPDATE    tivlt000
009000010131     C***                EXSR      NEWLOG
009100000629     C                   ENDIF
009200000629     C                   ENDIF
009300991112     C*
009400991112     C     POPZ          IFEQ      '14'
009500991115     C                   ADD       1             VLTsqz
009600991115     C                   MOVEL     *BLANKS       VLTfls
009700991115     C                   UPDATE    tivlt000
009800991112     C                   ENDIF
009900991112     C*
010000991112     C                   ENDIF
010100991112     C*
010200970526     C* FINE PGM
010300970526     C                   SETON                                        LR
010400991112     C*------------------------------------------------------------------------*
010500991112     C* NEWLOG - MODIFICA CAMPI RECORD E SUA SCRITTURA
010600991112     C*------------------------------------------------------------------------*
010700991112     C     NEWLOG        BEGSR
010800991112     C*
010900000707     C*
011000000707     C/EXEC SQL
011100000707     C+ declare C1 cursor for select max(vltsqz) from tivlt00f where
011200000707     C+ vltdat = :PDAT and vltora = :PORA and vltksc = :PKSC and
011300000707     C+ vlttip = :PTIP
011400000707     C+ for fetch only
011500000707     C/END-EXEC
011600000707     C
011700000707     C/EXEC SQL
011800000707     C+ open C1
011900000707     C/END-EXEC
012000000707     C
012100000707     C/EXEC SQL
012200000707     C+ Fetch C1 into :maximum
012300000707     C/END-EXEC
012400000707     C*
012500000707     C*
012600000707     C                   IF        sqlcod = *zeros
012700000707     C                   ADD       1             maximum
012800000707     C                   Z-ADD     maximum       VLTsqz
012900000713     C                   MOVEL     '0'           VLTsts
013000000714     C                   EVAL      VLTmsg =
013100020909     C                              '(SEDE)'
013200000714     C                              + '/'
013300000714     C                              + %trim(psjobname)
013400000714     C                              + '/'
013500000714     C                              + %trim(psuser)
013600000714     C                              + '/'
013700000714     C                              + %trim(%editc(psjobnbr:'Z'))
013800000726     C                   WRITE     tivlt000
013900000707     C                   ENDIF
014000000707     C
014100000707     C/EXEC SQL
014200000707     C+ close C1
014300000707     C/END-EXEC
014400000707     C
014500991112     C*
014600991112     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* *INZSR - OPERAZIONI INIZIALI
014900970526     C*------------------------------------------------------------------------*
015000970526     C     *INZSR        BEGSR
015100970526     C*---
015200970526     C* RICEVIMENTO PARAMETRI
015300970526     C*---
015400970526     C     *ENTRY        PLIST
015500970612     C                   PARM                    KPJBA
015600991112     C                   MOVEL     KPJBU         PARAM
015700991108     C*---
015800991115     C     *LIKE         DEFINE    vltdat        kltdat
015900991115     C     *LIKE         DEFINE    vltora        kltora
016000991115     C     *LIKE         DEFINE    vltksc        kltksc
016100991115     C     *LIKE         DEFINE    vltsts        kltsts
016200991115     C     *LIKE         DEFINE    vlttip        klttip
016300991115     C     *LIKE         DEFINE    vltsqz        kltsqz
016400970526     C*---
016500970526     C* CHIAVI DI LETTURA
016600970526     C*---
016700991115     C* LETTURA tivlt03L -
016800991115     C     KEYvlt        KLIST
016900991115     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
017000991115     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
017100991115     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
017200991115     C                   KFLD                    KLTtip                         *PARTENZA/ARRIVO
017300991115     C                   KFLD                    KLTsqz                         *PARTENZA/ARRIVO
017400991115     C                   KFLD                    KLTsts                         *PARTENZA/ARRIVO
017500991112     C*
017600991112     C                   ENDSR
