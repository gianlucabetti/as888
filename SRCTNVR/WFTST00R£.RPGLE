000100000000     H*------------------------------------------------------------------------*
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*------------------------------------------------------------------------*
000400000000     F* DATA BASE
000500000000     F*------------------------------------------------------------------------*
000600111005     FWFTST00F  UF A E             DISK
000700000000     C*------------------------------------------------------------------------*
000800011123     C
000900000000     C*------------------------------------------------------------------------*
001000111005     C*
001100111005     C     *ENTRY        PLIST
001200111005     C                   PARM                    wLancio           1
001300111005     C*
001400111005     C                   z-add     100000        wTotLancio01     10 0
001500111005     C                   z-add     200000        wTotLancio03     10 0
001600111005     C                   z-add     200000        wStrLancio03     10 0
001700111005     C*
001800111005     C* Lancio 1 => inserimento 10.000.000 d records
001900111005     C                   if        wLancio = '1'
002000111005     C                   z-add     *zeros        wCurrRecord01    10 0
002100111005     C                   dow       wCurrRecord01 < wTotLancio01
002200111005     C                   add       1             wCurrRecord01
002300111005     C                   eval      §TSTPRG = wCurrRecord01
002400111005     C                   write     wftst000
002500111005     C                   enddo
002600111005     C                   endif
002700111005     C*
002800111005     C* Lancio 2 => eliminazone 1 record si e 2 no (ergo 5.000.000 records)
002900111005     C                   if        wLancio = '2'
003000111005     C                   z-add     *zeros        wCurrRecord02     1 0
003100111005     C     *start        setll     wftst00f
003200111005     C                   read      wftst00f
003300111005     C                   dow       not %eof(wftst00f)
003400111005     C                   add       1             wCurrRecord02
003500111005     C                   select
003600111005     C                   when      wCurrRecord02 = 1
003700111005     C                   when      wCurrRecord02 = 2
003800111005     C                   delete    wftst00f
003900111005     C                   other
004000111005     C                   z-add     *zeros        wCurrRecord02
004100111005     C                   endsl
004200111005     C                   read      wftst00f
004300111005     C                   enddo
004400111005     C                   endif
004500111005     C*
004600111005     C* Lancio 3 => eliminazone / inserimento a randazzo
004700111005     C                   if        wLancio = '3'
004800111005     C                   z-add     *zeros        wCurrRecord03     1 0
004900111005     C                   z-add     *zeros        wCurrRec         10 0
005000111005     C                   dow       wCurrRec <= wTotLancio03
005100111005     C                   add       1             wCurrRec
005200111005     C                   add       1             wCurrRecord03
005300111005     C                   select
005400111005     C                   when      wCurrRecord03 = 1
005500111005     C                   when      wCurrRecord03 = 2
005600111005     C                   when      wCurrRecord03 = 3
005700111005     C                   add       1             wStrLancio03
005800111005     C                   eval      §TSTPRG = wStrLancio03
005900111005     C                   write     wftst000
006000111005     C                   other
006100111005     C                   z-add     *zeros        wCurrRecord03
006200111005     C                   endsl
006300111005     C                   enddo
006400111005     C                   endif
006500010808     C*
006600010808     C                   SETON                                            LR
