000100000000****************************************************************
000200000000      ****************************************************************
000300040130     D*
000400040130     D File            S             10
000500040130     D Job             S             10
000600040130     D User            S             10
000700040130     D JobX            S              6
000800060510     D SplfXx          S              6
000900040130
001000040130     D* Parametri x modifiche attributi spool
001100040130     D PARAM           S            262    DIM(20)
001200040130
001300040130     D* DS scomposizione parametri x modifiche attributi spool
001400040130     D SPOOLATR        DS
001500040130     D  POSDA                  1      4  0
001600040130     D  LUNG                   5      7  0
001700040130     D  VALORE                 8    262
001800040130
001900000000      ****************************************************************
002000040130     C                   CLEAR                   PARAM
002100040202     C*
002200040202     C* Modifico l'utente proprietario dello spool "destinazione"
002300040202     C                   CLEAR                   SPOOLATR
002400040202     C                   EVAL      POSDA = 58
002500040202     C                   EVAL      LUNG  = 10
002600040217     C                   EVAL      VALORE = 'EDPGA'
002700040202     C                   EVAL      PARAM(1) = SPOOLATR
002800040202     C*
002900040202     C* Modifico la OUTQ dello spool "destinazione"
003000040202     C                   CLEAR                   SPOOLATR
003100040202     C                   EVAL      POSDA = 190
003200040202     C                   EVAL      LUNG  = 10
003300040202     C                   EVAL      VALORE = 'EMAILIN'
003400040202     C                   EVAL      PARAM(2) = SPOOLATR
003500040202     C*
003600040202     C* Modifico la libreria della OUTQ dello spool "destinazione"
003700040202     C                   CLEAR                   SPOOLATR
003800040202     C                   EVAL      POSDA = 200
003900040202     C                   EVAL      LUNG  = 10
004000040202     C                   EVAL      VALORE = 'GAITRAOBJ'
004100040202     C                   EVAL      PARAM(3) = SPOOLATR
004200040130     C*
004300040130     C* Modifico il parametro USRDFNDTA (dati utente estesi)
004400040130     C                   CLEAR                   SPOOLATR
004500040130     C                   EVAL      POSDA = 3460
004600040130     C                   EVAL      LUNG  = 255
004700040130     C                   EVAL      VALORE = 'Questa è 1 prova... meravigliosa!'
004800040202     C                   EVAL      PARAM(4) = SPOOLATR
004900040130     C*
005000040130     C                   CALL      'XDUPSPLFR'
005100040130     C                   PARM                    File
005200040130     C                   PARM                    Job
005300040130     C                   PARM                    User
005400040130     C                   PARM                    JobX
005500040130     C                   PARM                    SplfXx
005600040130     C                   PARM                    PARAM
005700000000      *
005800000000     C                   SETON                                        LR
005900040130      ***************************************************************
006000040130      * INITIALIZATION SUBROUTINE                                   *
006100040130      ***************************************************************
006200040130     C     *INZSR        BEGSR
006300040130      *
006400040130     C     *ENTRY        PLIST
006500040130     C                   PARM                    File
006600040130     C                   PARM                    Job
006700040130     C                   PARM                    User
006800040130     C                   PARM                    JobX
006900040130     C                   PARM                    SplfXx
007000040130      *
007100040130     C                   ENDSR
