000100161205     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000
000400000000
000500000000
000600000000     C*-----------------------------------------------------------------------------
000700000000     C*
000800000000     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
000900000000     C
001000000000     C/EXEC SQL
001100000000     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
001200000000     C/END-EXEC
001300000000     C*
001400160906     C* Monitoro l'intero flusso del *pgm
001500160906     C                   monitor
001600170426     C
001700170426     C/EXEC SQL
001800170426     C+ UPDATE TIVGD00F SET VGDSTO = 'G'
001900170426     C+ WHERE VGDSTO = '?' AND VGDDAT < DECIMAL(CURRENT_DATE, 8, 0)
002000170426     C+ WITH CS
002100170426     C/END-EXEC
002200170426     C
002300170426     C/EXEC SQL
002400170426     C+ COMMIT
002500170426     C/END-EXEC
002600170426     C
002700160906     C*
002800160906     C* Gestisco eventuale errore
002900160906     C                   on-error
003000160906     C                   eval      prmesito = '2'
003100171003     C                   dump(A)
003200160906     C*
003300160906     C* Interrompo monitoraggio errori
003400160906     C                   endmon
003500000000     C*
003600000000     C                   seton                                        lr
003700000000     C*-----------------------------------------------------------------------------
003800000000
003900000000
004000000000
004100000000      /TITLE Routine di *init PGM
004200000000     C*
004300000000     C     *inzsr        begsr
004400000000     C*
004500000000     C* Passaggio parametri
004600000000     C     *entry        plist
004700000000     C                   parm                    prmesito          1
004800160906     C*
004900000000     C                   endsr
005000160906     C*
