000100940912     H*------------------------------------------------------------------------*
000200930128     H DECEDIT('0,') DATEDIT(*DMY.)
000300060621     H*------------------------------------------------------------------------*
000400060621     FPRTF198   O    F  300        PRINTER OFLIND(*INOF)
000500060621     D*------------------------------------------------------------------------*
000600060621     D DS_TAS        E DS                  EXTNAME(TITAS00F) INZ
000700080307     D TISI95DS      E DS                  INZ
000800940912     C*------------------------------------------------------------------------*
000900080306     C*
001000080306     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001100080306     C
001200080306     C/EXEC SQL
001300080306     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
001400080306     C/END-EXEC
001500080306     C*
001600070821     C*
001700070821     C* Creazione recordset estratto bolle interessate alla verifica
001800070821     C*
001900040304     C/EXEC SQL
002000060621     C+ DECLARE C1 CURSOR FOR
002100080306     C+ SELECT * FROM TITAS10F
002200080307     C+ WHERE
002300080307     C+       TASAAS*10000+TASMGS >= 20080201
002400080307     C+ AND   TASNZD  = '   '
002500080307     C+ AND   TASFFD  = ' '
002600080307     C+ AND   TASTBL  NOT IN ('A3', 'F3')
002700080307     C+ ORDER BY TASNZD, TASCAD, TASLOD, TASPKB, TASVLB
002800040304     C/END-EXEC
002900040304     c*
003000040304     C/EXEC SQL
003100060621     C+ OPEN C1
003200040304     C/END-EXEC
003300060621     C
003400060621     C/EXEC SQL
003500060621     C+ FETCH NEXT FROM C1 INTO :DS_TAS
003600060621     C/END-EXEC
003700060621     C
003800060621     C                   DOW       sqlcod = *zeros
003900060621     C*
004000080307     C                   MOVEL     *zeros        wLNA_STD          3 0
004100080307     C                   MOVEL     *zeros        wLNA_TST          3 0
004200080307     C                   MOVEL     *blanks       wCHANGE           2
004300080307     C                   CLEAR                   TISI95DS
004400080307     C*
004500080307     C                   EVAL      I95TLA = *blanks
004600080307     C                   EVAL      I95TCN = '7'
004700080307     C                   EVAL      I95NAR = TASNZD
004800080307     C                   EVAL      I95CAP = TASCAD
004900080307     C                   EVAL      I95LOC = TASLOD
005000080307     C                   EVAL      I95PRV = TASPRD
005100080307     C                   EVAL      I95DAT = TASAAS*10000+TASMGS
005200080307     C                   EVAL      I95LKG = TASPKB
005300080307     C                   EVAL      I95LMC = TASVLB
005400080307     C                   EVAL      I95FFD = TASFFD
005500080307     C                   EVAL      I95TSP = TASTSP
005600080307     C                   EVAL      I95TFP = TASTFP
005700080307     C                   EVAL      I95FRT = 'S'
005800080307     C                   EVAL      I95FRE = 'S'
005900080307     C                   EVAL      %subst(I95FI2:1:1) = TASFTC
006000080307     C                   EVAL      %subst(I95FI2:2:1) = TASTC2
006100080307     C*
006200080307     C                   CALL      'TISI95R'
006300080307     C                   PARM                    TISI95DS
006400080307     C*
006500080307     C                   IF        O95ERR <> *blanks
006600080307     C                   MOVEL     'E1'          wCHANGE
006700080307     C                   ELSE
006800080307     C                   EVAL      wLNA_STD = O95LNA
006900080307     C*
007000080307     C                   CALL      'TISI95RTST'
007100080307     C                   PARM                    TISI95DS
007200080307     C*
007300080307     C                   IF        O95ERR <> *blanks
007400080307     C                   MOVEL     'E2'          wCHANGE
007500080307     C                   ELSE
007600080307     C                   EVAL      wLNA_TST = O95LNA
007700080307     C*
007800080307     C                   IF        wLNA_STD <> wLNA_TST
007900070822     C                   MOVEL     '?'           wCHANGE
008000080306     C                   EXCEPT    RIGA
008100060621     C                   ENDIF
008200080307     C                   ENDIF
008300080307     C                   ENDIF
008400070822     C*
008500060621     C
008600040304     C/EXEC SQL
008700060621     C+ FETCH NEXT FROM C1 INTO :DS_TAS
008800040304     C/END-EXEC
008900060621     C
009000040304     C*
009100060621     C                   ENDDO
009200060621     C
009300040304     C/EXEC SQL
009400060621     C+ CLOSE C1
009500040304     C/END-EXEC
009600060621     C*
009700080307     C                   CLEAR                   TISI95DS
009800080307     C                   EVAL      I95TLA = 'C'
009900080307     C                   CALL      'TISI95R'
010000080307     C                   PARM                    TISI95DS
010100080307     C*
010200080307     C                   CLEAR                   TISI95DS
010300080307     C                   EVAL      I95TLA = 'C'
010400080307     C                   CALL      'TISI95RTST'
010500080307     C                   PARM                    TISI95DS
010600060621     C*
010700060621     C                   SETON                                        LR
010800060621     C*------------------------------------------------------------------------*
010900040304     C     *INZSR        BEGSR
011000080307     C*
011100060621     C*
011200040304     C                   ENDSR
011300060621     C*------------------------------------------------------------------------*
011400060621     OPRTF198   E            RIGA           1
011500070704     O                       wCHANGE             +1
011600080307     O                       TASAAS              +3
011700080307     O                       TASLNP              +1
011800080307     O                       TASNRS              +1
011900080307     O                       TASNSP              +1
012000080307     O                       TASPRD              +5
012100080307     O                       TASCAD              +1
012200080307     O                       TASLOD              +1
012300080307     O                       TASPKB              +1
012400080307     O                       TASVLB              +1
012500080307     O                       wLNA_STD            +5
012600080307     O                       wLNA_TST            +5
