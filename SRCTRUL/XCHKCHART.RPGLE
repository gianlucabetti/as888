000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300040212
000400040212     D InTxt           S             30
000500040212     D TxtInOut        S           2048
000600040212     D ElencoChar      S            256
000700040212     D TipoElenco      S              1
000800040212     D CharSost        S              1
000900040212     D UpperCase       S              1
001000040212     D ChkNull         S              1
001100040212     D CharNull        S              1
001200040212     D Esito           S              1
001300001120
001400000411
001500000414     C*------------------------------------------------------------------------*
001600000414     C* MAIN LINE
001700000414     C*------------------------------------------------------------------------*
001800040212     C*
001900040212     C* Imposto i parametri x il *pgm d "bonifica"
002000040212     C                   EVAL      TxtInOut   = InTxt
002100040226     C                   EVAL      ElencoChar = *blanks
002200040212     C                   EVAL      TipoElenco = *blanks
002300040212     C                   EVAL      CharSost   = '§'
002400040226     C                   EVAL      UpperCase  = *blanks
002500040212     C                   EVAL      ChkNull    = '1'
002600040212     C                   EVAL      CharNull   = '#'
002700040212     C                   EVAL      Esito      = *blanks
002800040212     C*
002900040212     C* Effettuo la chiamata al *pgm d "bonifica"
003000040212     C                   CALL      'XCHKCHAR'
003100040212     C                   PARM                    TxtInOut
003200040212     C                   PARM                    ElencoChar
003300040212     C                   PARM                    TipoElenco
003400040212     C                   PARM                    CharSost
003500040212     C                   PARM                    UpperCase
003600040212     C                   PARM                    ChkNull
003700040212     C                   PARM                    CharNull
003800040212     C                   PARM                    Esito
003900040212     C*
004000040212     C                   EVAL      InTxt = TxtInOut
004100040212     C*
004200040212     C* Visualizzo la stringa così "bonificata"
004300040212     C     InTxt         DSPLY
004400001120     C*
004500001120     C                   SETON                                        LR
004600000411
004700001120
004800001120
004900001120
005000000414     C*------------------------------------------------------------------------*
005100000414     C* *INZSR - OPERAZIONI INIZIALI
005200000414     C*------------------------------------------------------------------------*
005300000414     C     *INZSR        BEGSR
005400001120     C*
005500000414     C*--------------------
005600000414     C* RICEVIMENTO PARAMETRI
005700000414     C*--------------------
005800000414     C     *ENTRY        PLIST
005900040212     C                   PARM                    InTxt
006000000414     C*
006100000414     C                   ENDSR
