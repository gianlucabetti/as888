000100940912     H*------------------------------------------------------------------------*
000200930128     H DECEDIT('0,') DATEDIT(*DMY.)
000300060621     H*------------------------------------------------------------------------*
000400060621     FPRTF198   O    F  300        PRINTER OFLIND(*INOF)
000500060621     FTITA430C  IF   E           K DISK
000600060621     D*------------------------------------------------------------------------*
000700060621     D DS_TAS        E DS                  EXTNAME(TITAS00F) INZ
000800060621     D TISIE3DS      E DS                  INZ
000900060621     D DSBL4I        E DS                  INZ
001000940912     C*------------------------------------------------------------------------*
001100080306     C*
001200080306     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001300080306     C
001400080306     C/EXEC SQL
001500080306     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
001600080306     C/END-EXEC
001700080306     C*
001800070821     C*
001900070821     C* Ricevimento parametri d lancio
002000070821     C     *ENTRY        PLIST
002100070821     C                   PARM                    In_DataRifAlf     8
002200080325     C                   PARM                    In_StrBolleAlf    8
002300070821     C                   MOVE(P)   In_DataRifAlf In_DataRif        8 0
002400080325     C                   MOVE(P)   In_StrBolleAlfIn_StrBolle       8 0
002500070821     C*
002600070821     C* Creazione recordset estratto bolle interessate alla verifica
002700070821     C*
002800040304     C/EXEC SQL
002900060621     C+ DECLARE C1 CURSOR FOR
003000080325     C+ SELECT * FROM TITAS00F
003100080325     C+ WHERE TASLNA IN
003200080325     C+ (190, 191, 192, 193, 194, 195, 196, 197)
003300080325     C+ AND   TASAAS*10000+TASMGS >= :In_StrBolle
003400080325     C+ AND   TASCCA <> '2'
003500080325     C+ AND   TASTBL     NOT IN ('A3', 'F3')
003600080325     C+ UNION
003700080306     C+ SELECT * FROM TITAS10F
003800080306     C+ WHERE TASLNA IN
003900080306     C+ (190, 191, 192, 193, 194, 195, 196, 197)
004000080325     C+ AND   TASAAS*10000+TASMGS >= :In_StrBolle
004100080306     C+ AND   TASCCA <> '2'
004200070822     C+ AND   TASTBL     NOT IN ('A3', 'F3')
004300080307     C+ ORDER BY TASNZD, TASCAD, TASLOD
004400040304     C/END-EXEC
004500040304     c*
004600040304     C/EXEC SQL
004700060621     C+ OPEN C1
004800040304     C/END-EXEC
004900060621     C
005000060621     C/EXEC SQL
005100060621     C+ FETCH NEXT FROM C1 INTO :DS_TAS
005200060621     C/END-EXEC
005300060621     C
005400060621     C                   DOW       sqlcod = *zeros
005500060621     C*
005600060621     C                   CLEAR                   TISIE3DS
005700060621     C                   EVAL      ISIE3TLA  = 'E'
005800070821     C                   EVAL      ISIE3DRI  = In_DataRif
005900070821     C                   EVAL      ISIE3DSP  = In_DataRif
006000060621     C                   EVAL      ISIE3NZD  = tasNZD
006100060621     C                   EVAL      ISIE3CAD  = tasCAD
006200060621     C                   EVAL      ISIE3CTY  = tasLOD
006300060621     C*
006400060621     C                   IF        tasPKB > 3
006500060621     C                   EVAL      ISIE3SRV  = 101
006600060621     C                   ELSE
006700060621     C                   EVAL      ISIE3SRV  = 136
006800060621     C                   ENDIF
006900060621     C*
007000060621     C                   EVAL      ISIE3LNP  = tasLNP
007100060621     C                   EVAL      ISIE3EXC  = 'N'
007200060621     C*
007300060621     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD
007400060621     C                   EVAL      ISIE3TLA = 'E'
007500060621     C                   CALL      'TISIE3R'
007600060621     C                   PARM                    TISIE3DS
007700070822     C*
007800060621     C* Se ok verifico che il depot d arrivo calcolato sia = a quello già memorizzato
007900070822     C                   MOVEL     *blanks       wCHANGE           1
008000070822     C                   CLEAR                   DSBL4I
008100060621     C                   EVAL      ta4TRC = 'I'
008200060621     C     KEYta430_C    CHAIN     TITA430C
008300060621     C                   IF        %found(TITA430C)
008400060621     C                   EVAL      DSBL4I = ta4NOT
008500070822     C                   IF        OSIE3DDEP <> §B4IDA
008600070822     C                   MOVEL     '?'           wCHANGE
008700080306     C                   EXCEPT    RIGA
008800060621     C                   ENDIF
008900060621     C                   ENDIF
009000070822     C*
009100060621     C
009200040304     C/EXEC SQL
009300060621     C+ FETCH NEXT FROM C1 INTO :DS_TAS
009400040304     C/END-EXEC
009500060621     C
009600040304     C*
009700060621     C                   ENDDO
009800060621     C
009900040304     C/EXEC SQL
010000060621     C+ CLOSE C1
010100040304     C/END-EXEC
010200060621     C*
010300060621     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD SOLO X CHIUSURA
010400060621     C                   CLEAR                   TISIE3DS
010500060621     C                   EVAL      ISIE3TLA = 'C'
010600060621     C                   CALL      'TISIE3R'
010700060621     C                   PARM                    TISIE3DS
010800060621     C*
010900060621     C                   SETON                                        LR
011000060621     C*------------------------------------------------------------------------*
011100040304     C     *INZSR        BEGSR
011200060621     C*
011300060621     C* TITA430C - Completa
011400060621     C     KEYta430_C    KLIST
011500060621     C                   KFLD                    tasAAS
011600060621     C                   KFLD                    tasLNP
011700060621     C                   KFLD                    tasNRS
011800060621     C                   KFLD                    tasNSP
011900060621     C                   KFLD                    ta4TRC
012000060621     C*
012100040304     C                   ENDSR
012200060621     C*------------------------------------------------------------------------*
012300060621     OPRTF198   E            RIGA           1
012400070704     O                       wCHANGE             +1
012500060621     O                       TASAAS              +1
012600060621     O                       TASLNP              +1
012700060621     O                       TASNRS              +1
012800060621     O                       TASNSP              +1
012900060621     O                       §B4IDA              +5
013000060621     O                       ISIE3DRI            +5
013100060621     O                       ISIE3NZD            +1
013200060621     O                       ISIE3CAD            +1
013300060621     O                       ISIE3CTY            +1
013400060621     O                       ISIE3SRV            +1
013500060621     O                       ISIE3LNP            +1
013600060621     O                       USIE3CAD            +1
013700060621     O                       OSIE3DDEP           +5
013800060621     O                       OSIE3ERR            +5
013900060621     O                       OSIE3ERRL           +1
014000060621     O                       OSIE3ERRD           +1
014100940912     O*------------------------------------------------------------------------*
