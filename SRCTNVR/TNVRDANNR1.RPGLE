000100020730      *-------------------------------------------------------------------------
000200020731      * PULISCE FILE SATELLITI DI FNDCT SE NON TROVA LA TESTATA
000300020731      *-------------------------------------------------------------------------
000400020731      * File elaborati:
000500020731      *    FNDCT/DCD/DCC/DCF/DCL/DCR/DPT/DKA
000600020730      *-------------------------------------------------------------------------
000700020730
000800020730     h DECEDIT('0,') DATEDIT(*DMY.)
000900020731
001000020731     ffndct01l  IF A E           K DISK
001100020731     ffndcc01l  UF A E           K DISK
001200020731     ffndcd01l  UF A E           K DISK
001300020731     ffndcf01l  UF A E           K DISK
001400020731     ffndcl01l  UF A E           K DISK
001500020731     ffndcr01l  UF A E           K DISK
001600020731     ffndpt01l  UF A E           K DISK
001700020731     ffndka01l  UF A E           K DISK
001800991202      *
001900991202     fprtf198   O    F  198        PRINTER  oflind(*INOF)
002000020731      *-------------------------------------------------------------------------
002100020731     d kpjba         E DS
002200991202     D                 ds
002300991202     d  wdctaac                1      4  0
002400991202     d  wdctfil                5      7  0
002500991202     d  wdctnca                8     14  0
002600991202     d  knkt                   1     14
002700020731      *-------------------------------------------------------------------------
002800020731      * Main lines
002900020731      *-------------------------------------------------------------------------
003000020731
003100020731      *  FNDCC
003200020731     c     *loval        setll     fndcc01l
003300020731     c                   read      fndcc01l
003400020731    1c                   DOW       not %EOF(FNDCC01L)
003500020731     c     kdcc          setll     fndct000
003600020731     C                   IF        not %EQUAL
003700020731     c*?                 delete    fndcc01l
003800020731     c                   except    edcc
003900020731     c                   ENDIF
004000020731     c                   read      fndcc01l
004100020731    1c                   ENDDO
004200020731
004300020731      *  FNDCD
004400020731     c     *loval        setll     fndcd01l
004500020731     c                   read      fndcd01l
004600020731    1c                   DOW       not %EOF(FNDCD01L)
004700020731     c     kdcd          setll     fndct000
004800020731     C                   IF        not %EQUAL
004900020731     c*?                 delete    fndcd01l
005000020731     c                   except    edcd
005100020731     c                   ENDIF
005200020731     c                   read      fndcd01l
005300020731    1c                   ENDDO
005400020731
005500020731      *  FNDCF
005600020731     c     *loval        setll     fndcf01l
005700020731     c                   read      fndcf01l
005800020731    1c                   DOW       not %EOF(FNDCf01L)
005900020731     c     kdcf          setll     fndct000
006000020731     C                   IF        not %EQUAL
006100020731     c*?                 delete    fndcf01l
006200020731     c                   except    edcf
006300020731     c                   ENDIF
006400020731     c                   read      fndcf01l
006500020731    1c                   ENDDO
006600020731
006700020731      *  FNDCL
006800020731     c     *loval        setll     fndcl01l
006900020731     c                   read      fndcl01l
007000020731    1c                   DOW       not %EOF(FNDCl01L)
007100020731     c     kdcl          setll     fndct000
007200020731     C                   IF        not %EQUAL
007300020731     c*?                 delete    fndcl01l
007400020731     c                   except    edcl
007500020731     c                   ENDIF
007600020731     c                   read      fndcl01l
007700020731    1c                   ENDDO
007800020731e   1
007900020731      *  FNDCR
008000020731     c     *loval        setll     fndcr01l
008100020731     c                   read      fndcr01l
008200020731    1c                   DOW       not %EOF(FNDCr01L)
008300020731     c     kdcr          setll     fndct000
008400020731     C                   IF        not %EQUAL
008500020731     c*?                 delete    fndcr01l
008600020731     c                   except    edcr
008700020731     c                   ENDIF
008800020731     c                   read      fndcr01l
008900020731    1c                   ENDDO
009000020731
009100020731      *  FNDPT
009200020731     c     *loval        setll     fndpt01l
009300020731     c                   read      fndpt01l
009400020731    1c                   DOW       not %EOF(FNDpt01L)
009500020731     c     kdpt          setll     fndct000
009600020731     C                   IF        not %EQUAL
009700020731     c*?                 delete    fndpt01l
009800020731     c                   except    edpt
009900020731     c                   ENDIF
010000020731     c                   read      fndpt01l
010100020731    1c                   ENDDO
010200020731e   1
010300020731      *  FNDKA
010400020731     c     *loval        setll     fndka01l
010500020731     c                   read      fndka01l
010600020731    1c                   DOW       not %EOF(FNDka01L)
010700020731     c     kdka          setll     fndct000
010800020731     C                   IF        not %EQUAL
010900020731     c*?                 delete    fndka01l
011000020731     c                   except    edka
011100020731     c                   ENDIF
011200020731     c                   read      fndka01l
011300020731    1c                   ENDDO
011400020731
011500020731     c                   eval      *inlr = *on
011600970605     c*-------------------------------------------------------------------------
011700970605     c* *inzsr - operazioni iniziali
011800970605     c*-------------------------------------------------------------------------
011900970605     c     *inzsr        BEGSR
012000020731
012100020731     c     kdcc          KLIST
012200020731     c                   KFLD                    dccaac
012300020731     c                   KFLD                    dccfil
012400020731     c                   KFLD                    dccnca
012500020731     c     kdcd          KLIST
012600020731     c                   KFLD                    dcdaac
012700020731     c                   KFLD                    dcdfil
012800020731     c                   KFLD                    dcdnca
012900020731     c     kdcf          KLIST
013000020731     c                   KFLD                    dcfaac
013100020731     c                   KFLD                    dcffil
013200020731     c                   KFLD                    dcfnca
013300020731     c     kdcl          KLIST
013400020731     c                   KFLD                    dclaac
013500020731     c                   KFLD                    dclfil
013600020731     c                   KFLD                    dclnca
013700020731     c     kdcr          KLIST
013800020731     c                   KFLD                    dcraac
013900020731     c                   KFLD                    dcrfil
014000020731     c                   KFLD                    dcrnca
014100020731     c     kdct          KLIST
014200020731     c                   KFLD                    dctaac
014300020731     c                   KFLD                    dctfil
014400020731     c                   KFLD                    dctnca
014500020731     c     kdpt          KLIST
014600020731     c                   KFLD                    dptaac
014700020731     c                   KFLD                    dptfil
014800020731     c                   KFLD                    dptnca
014900020731     c     kdka          KLIST
015000020731     c                   KFLD                    dkaaac
015100020731     c                   KFLD                    dkafil
015200020731     c                   KFLD                    dkanca
015300020731
015400970605     c                   ENDSR
015500991202      *-------------------------------------------------------------------------
015600020731     OPRTF198   E            edcc        2
015700020731     o                                        +   2 'FNDCC'
015800020731     o                       DCcAAC           +   3
015900020731     o                       DCcfil        Z  +   1
016000020731     o                       DCcnca        Z  +   1
016100020731      *
016200020731     O          E            edcd        2
016300020731     o                                        +   2 'FNDCD'
016400020731     o                       DCdAAC           +   3
016500020731     o                       DCdfil        Z  +   1
016600020731     o                       DCdnca        Z  +   1
016700020731      *
016800020731     O          E            edcf        2
016900020731     o                                        +   2 'FNDCF'
017000020731     o                       DCfAAC           +   3
017100020731     o                       DCffil        Z  +   1
017200020731     o                       DCfnca        Z  +   1
017300020731      *
017400020731     O          E            edcl        2
017500020731     o                                        +   2 'FNDCL'
017600020731     o                       DClAAC           +   3
017700020731     o                       DClfil        Z  +   1
017800020731     o                       DClnca        Z  +   1
017900020731      *
018000020731     O          E            edcr        2
018100020731     o                                        +   2 'FNDCR'
018200020731     o                       DCrAAC           +   3
018300020731     o                       DCrfil        Z  +   1
018400020731     o                       DCrnca        Z  +   1
018500020731      *
018600020731     O          E            edpt        2
018700020731     o                                        +   2 'FNDPT'
018800020731     o                       DptAAC           +   3
018900020731     o                       Dptfil        Z  +   1
019000020731     o                       Dptnca        Z  +   1
019100020731      *
019200020731     O          E            edka        2
019300020731     o                                        +   2 'FNDKA'
019400020731     o                       DkaAAC           +   3
019500020731     o                       Dkafil        Z  +   1
019600020731     o                       Dkanca        Z  +   1
