000100970526     H*------------------------------------------------------------------------*
000200991115     H* Esecuzione OPZIONI Gestione log TIVLT00F - Strategi Upload
000300970526     H*------------------------------------------------------------------------*
000400170118     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900000707     Ftivlt03l  UF A E           K DISK
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300991112     D*------------
001400991112     D* ARCHITETTURA
001500991112     D*------------
001600991112     D KPJBA         E DS
001700991110     D*------------
001800991112     D* PASSAGGIO PARAMETRI
001900991110     D*------------
002000991112     D PARAM           DS                  INZ
002100991112     D  PLIB                   1     10
002200991112     D  POBJ                  11     20
002300991112     D  PMBR                  21     30
002400991112     D  PDAT                  31     38  0
002500991112     D  PORA                  39     44  0
002600991112     D  PKSC                  45     52
002700991115     D  PSTS                  53     53
002800991112     D  PTIP                  54     55
002900991112     D  PSQZ                  59     61  0
003000991112     D  POPZ                  62     63
003100000629     D  PFID                  64     73
003200000629     D  PMBD                  74     83
003300000629
003400000629     D* Informazioni sul job
003500000629     D psds           sds
003600000629     D  psjobname            244    253
003700000629     D  psuser               254    263
003800000629     D  psjobnbr             264    269s 0
003900000629
004000000707     D* Variabili di wrk
004100000707     D  maximum        s                   like(vltsqz) inz(*zeros)
004200000707
004300970526     C*------------------------------------------------------------------------*
004400970526     C* MAIN LINES
004500970526     C*------------------------------------------------------------------------*
004600170118     C*
004700170118     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004800170118     C
004900170118     C/EXEC SQL
005000170118     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005100170118     C/END-EXEC
005200170118     C*
005300991115     C                   Z-ADD     PDAT          KLTdat
005400991115     C                   Z-ADD     PORA          KLTora
005500991115     C                   MOVEL     PKSC          KLTksc
005600991115     C                   MOVEL     PSTS          KLTsts
005700991115     C                   MOVEL     PTIP          KLTtip
005800991115     C                   Z-ADD     PSQZ          KLTsqz
005900991112     C*
006000991112     C* VERIFICA DI SICUREZZA LETTURA FILE DI LOG
006100991112     C*
006200991115     C     KEYvlt        CHAIN     tivlt03l                           99
006300991112     C     *IN99         IFEQ      *OFF
006400991112     C*
006500991112     C     POPZ          IFEQ      ' 1'
006600991115     C     VLTsts        IFEQ      '2'
006700991115     C     VLTsts        OREQ      '4'
006800991115     C                   MOVEL     '4'           VLTsts
006900991115     C                   UPDATE    tivlt000
007000991115     C                   EXSR      NEWLOG
007100991115     C                   ENDIF
007200991115     C     VLTsts        IFEQ      '1'
007300991115     C     VLTsts        OREQ      '3'
007400991115     C                   MOVEL     '3'           VLTsts
007500991115     C                   UPDATE    tivlt000
007600991115     C                   EXSR      NEWLOG
007700991115     C                   ENDIF
007800991112     C                   ENDIF
007900991112     C*
008000991112     C     POPZ          IFEQ      ' 4'
008100991115     C                   DELETE    tivlt000
008200991112     C                   ENDIF
008300000629     C*
008400000629     C     POPZ          IFEQ      '11'
008500000629     C     VLTsts        IFEQ      '2'
008600000629     C     VLTsts        OREQ      '4'
008700000629     C                   MOVEL     '4'           VLTsts
008800000629     C                   UPDATE    tivlt000
008900000629     C                   ENDIF
009000000629     C     VLTsts        IFEQ      '1'
009100000629     C     VLTsts        OREQ      '3'
009200000629     C                   MOVEL     '3'           VLTsts
009300000629     C                   UPDATE    tivlt000
009400000629     C                   ENDIF
009500000629     C                   ENDIF
009600991112     C*
009700991112     C     POPZ          IFEQ      '14'
009800991115     C                   ADD       1             VLTsqz
009900991115     C                   MOVEL     *BLANKS       VLTfls
010000991115     C                   UPDATE    tivlt000
010100991112     C                   ENDIF
010200991112     C*
010300991112     C                   ENDIF
010400991112     C*
010500970526     C* FINE PGM
010600970526     C                   SETON                                        LR
010700991112     C*------------------------------------------------------------------------*
010800991112     C* NEWLOG - MODIFICA CAMPI RECORD E SUA SCRITTURA
010900991112     C*------------------------------------------------------------------------*
011000991112     C     NEWLOG        BEGSR
011100991112     C*
011200000707     C*
011300000707     C/EXEC SQL
011400000707     C+ declare C1 cursor for select max(vltsqz) from tivlt00f where
011500000707     C+ vltdat = :PDAT and vltora = :PORA and vltksc = :PKSC and
011600000707     C+ vlttip = :PTIP
011700000707     C+ for fetch only
011800000707     C/END-EXEC
011900000707     C
012000000707     C/EXEC SQL
012100000707     C+ open C1
012200000707     C/END-EXEC
012300000707     C
012400000707     C/EXEC SQL
012500000707     C+ Fetch C1 into :maximum
012600000707     C/END-EXEC
012700000707     C*
012800000707     C*
012900000707     C                   IF        sqlcod = *zeros
013000000707     C                   ADD       1             maximum
013100000707     C                   Z-ADD     maximum       VLTsqz
013200000713     C                   MOVEL     '0'           VLTsts
013300000714     C                   EVAL      VLTmsg =
013400020909     C                              '(SEDE)'
013500000714     C                              + '/'
013600000714     C                              + %trim(psjobname)
013700000714     C                              + '/'
013800000714     C                              + %trim(psuser)
013900000714     C                              + '/'
014000000714     C                              + %trim(%editc(psjobnbr:'Z'))
014100000726     C                   WRITE     tivlt000
014200000707     C                   ENDIF
014300000707     C
014400000707     C/EXEC SQL
014500000707     C+ close C1
014600000707     C/END-EXEC
014700000707     C
014800991112     C*
014900991112     C                   ENDSR
015000970526     C*------------------------------------------------------------------------*
015100970526     C* *INZSR - OPERAZIONI INIZIALI
015200970526     C*------------------------------------------------------------------------*
015300970526     C     *INZSR        BEGSR
015400970526     C*---
015500970526     C* RICEVIMENTO PARAMETRI
015600970526     C*---
015700970526     C     *ENTRY        PLIST
015800970612     C                   PARM                    KPJBA
015900991112     C                   MOVEL     KPJBU         PARAM
016000991108     C*---
016100991115     C     *LIKE         DEFINE    vltdat        kltdat
016200991115     C     *LIKE         DEFINE    vltora        kltora
016300991115     C     *LIKE         DEFINE    vltksc        kltksc
016400991115     C     *LIKE         DEFINE    vltsts        kltsts
016500991115     C     *LIKE         DEFINE    vlttip        klttip
016600991115     C     *LIKE         DEFINE    vltsqz        kltsqz
016700970526     C*---
016800970526     C* CHIAVI DI LETTURA
016900970526     C*---
017000991115     C* LETTURA tivlt03L -
017100991115     C     KEYvlt        KLIST
017200991115     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
017300991115     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
017400991115     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
017500991115     C                   KFLD                    KLTtip                         *PARTENZA/ARRIVO
017600991115     C                   KFLD                    KLTsqz                         *PARTENZA/ARRIVO
017700991115     C                   KFLD                    KLTsts                         *PARTENZA/ARRIVO
017800991112     C*
017900991112     C                   ENDSR
