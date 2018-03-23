000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600070604     D*------------------------------------------------------------------------*
000700070604     D* VARIABILI D WRK
000800070604     D*------------------
000900070604     D findata         S               D   DATFMT(*ISO) INZ(D'1999-01-01')
001000970526     D*------------------------------------------------------------------------*
001100991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
001200970527     D*------------------
001300970527     D WLBDA8          DS                  INZ
001400970527     D  G08DAT                 1      8  0
001500970527     D  G08INV                 9     16  0
001600970527     D  G08ERR                17     17
001700970527     D  G08TGI                18     22  0
001800970526     C*------------------------------------------------------------------------*
001900970526     C* MAIN LINES
002000970526     C*------------------------------------------------------------------------*
002100080404     C*
002200080404     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
002300080404     C
002400080404     C/EXEC SQL
002500080404     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002600080404     C/END-EXEC
002700970526     C*
002800070604     C                   EXSR      Clean_VIN
002900070604     C                   EXSR      Clean_VGD
003000070604     C*
003100970526     C* FINE PGM
003200970526     C                   SETON                                        LR
003300000103     C*------------------------------------------------------------------------*
003400070604     C* CLEAN_VIN - FORZATURA PULIZIA FILE GENERICO UPLOAD (TIVIN)
003500000103     C*------------------------------------------------------------------------*
003600070604     C     Clean_VIN     BEGSR
003700070925     C*
003800070925     C/EXEC SQL
003900070925     C+ DELETE FROM TIVIN00S WHERE
004000090402     C+ VINTIP IN ('SI', 'GI', '**')
004100070925     C+ AND VINDAT < :tieni02gg
004200070925     C/END-EXEC
004300070604     C*
004400060509     C/EXEC SQL
004500070604     C+ DELETE FROM TIVIN00S WHERE
004600070925     C+ VINTIP IN ('A1', 'A2', 'A3', 'C1', 'C2', 'C3', 'C4', 'C5')
004700070604     C+ AND VINDAT < :tieni05gg
004800060509     C/END-EXEC
004900070604     C*
005000070604     C/EXEC SQL
005100070604     C+ DELETE FROM TIVIN00S WHERE
005200070604     C+ VINTIP IN ('EI')
005300070604     C+ AND VINDAT < :tieni10gg
005400070604     C/END-EXEC
005500060509     C*
005600000103     C                   ENDSR
005700070604     C*------------------------------------------------------------------------*
005800070604     C* CLEAN_VGD - FORZATURA PULIZIA FILE GENERICO DOWNLOAD (TIVGD)
005900070604     C*------------------------------------------------------------------------*
006000070604     C     Clean_VGD     BEGSR
006100070604     C*
006200070604     C/EXEC SQL
006300070604     C+ DELETE FROM TIVGD00F WHERE
006400170119     C+ VGDDAT < :tieni02gg AND
006500170119     C+ VGDSTO = 'S' AND
006600170119     C+ VGDTIP IN ('DC', 'CC', 'JC', 'DA', 'DL')
006700070604     C/END-EXEC
006800070925     C*
006900070925     C/EXEC SQL
007000070925     C+ DELETE FROM TIVGD00F WHERE
007100170119     C+ VGDDAT < :tieni03gg AND
007200170119     C+ VGDSTO = 'S' AND
007300170119     C+ VGDTIP IN ('SO', 'GO')
007400070925     C/END-EXEC
007500070604     C*
007600070604     C/EXEC SQL
007700070604     C+ DELETE FROM TIVGD00F WHERE
007800170119     C+ VGDDAT < :tieni05gg AND
007900170119     C+ VGDSTO = 'S' AND
008000070925     C+ VGDTIP IN ('VF', 'G1', 'L1', 'L2', 'L3', 'L4',
008100170119     C+ 'L5', 'L6', 'ST', 'W1', 'LD', 'FP', '$3', 'EC')
008200070604     C/END-EXEC
008300070604     C*
008400070604     C/EXEC SQL
008500070604     C+ DELETE FROM TIVGD00F WHERE
008600170119     C+ VGDDAT < :tieni10gg AND
008700170119     C+ VGDSTO = 'S' AND
008800170119     C+ VGDTIP IN ('EO', 'EA', 'EU')
008900070604     C/END-EXEC
009000070604     C*
009100070604     C                   ENDSR
009200970526     C*------------------------------------------------------------------------*
009300970526     C* *INZSR - OPERAZIONI INIZIALI
009400970526     C*------------------------------------------------------------------------*
009500970526     C     *INZSR        BEGSR
009600970526     C*
009700970526     C* CALCOLA LA DATA CORRENTE
009800970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
009900970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
010000970526     C                   Z-ADD     WN8           G08DAT
010100970526     C                   Z-ADD     *ZEROS        G08INV
010200970526     C                   MOVEL     '0'           G08ERR
010300970526     C                   CALL      'XSRDA8'
010400970526     C                   PARM                    WLBDA8
010500970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
010600010226     C*
010700010226     C     *ISO          MOVE      DATCOR        findata
010800070604     C     findata       SUBDUR    02:*D         findata
010900070604     C     *ISO          MOVE      findata       tieni02gg         8 0
011000070925     C*
011100070925     C     *ISO          MOVE      DATCOR        findata
011200070925     C     findata       SUBDUR    03:*D         findata
011300070925     C     *ISO          MOVE      findata       tieni03gg         8 0
011400070604     C*
011500070604     C     *ISO          MOVE      DATCOR        findata
011600070604     C     findata       SUBDUR    05:*D         findata
011700070604     C     *ISO          MOVE      findata       tieni05gg         8 0
011800070604     C*
011900070604     C     *ISO          MOVE      DATCOR        findata
012000070604     C     findata       SUBDUR    10:*D         findata
012100070604     C     *ISO          MOVE      findata       tieni10gg         8 0
012200991230     C*
012300991230     C                   ENDSR
