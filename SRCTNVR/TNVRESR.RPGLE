000100950202     H DECEDIT('0,') DATEDIT(*DMY.)
000200950202      **************************************************************************
000300021023     FFIBRV01L  UF   E           K DISK
000400950512     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500021023     F*
000600021023     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
000700021023     D DSLV55        E DS                  EXTNAME(FNLV55DS)
000800950202     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
000900021023     C                   CLEAR                   SAVFGS
001000021023     C                   CLEAR                   SAVDRF
001100950202     C*
001200950202     C* MI POSIZIONO PER DATA INIZIALE E LEGGO SEQUENZIALMENTE
001300021023     C     *LOVAL        SETLL     FIBRV01L
001400021023     C                   READ      FIBRV01L                               31
001500950202     C*
001600950512    1C     *IN31         DOWEQ     *OFF
001700021023     C                   IF        BRVDCS>0
001800021023     C                   MOVEL     brvdcs        WDRF              8 0
001900021023     C                   ELSE
002000021023     C                   MOVEL     brvdFS        WDRF
002100021023     C                   ENDIF
002200021023     c*
002300021023     c                   if        wdrf>20021014
002400021023     C*
002500021023     C                   IF        BRVFGS<>SAVFGS OR WDRF<> SAVDRF
002600021023     C                   CLEAR                   DSLV55
002700021023     C                   MOVEL     'P'           D55TPT
002800021023     C                   MOVEL     brvfgs        D55LIN
002900021023     C                   MOVEL     WDRF          D55DRF
003000021023     C                   MOVEL     brvfgs        SAVFGS            3 0
003100021023     C                   MOVEL     WDRF          SAVDRF            8 0
003200021023     C                   CALL      'FNLV55R'
003300021023     C                   PARM                    DSLV55
003400021023     C                   ENDIF
003500021023     C*
003600021023     C     D55ERR        IFEQ      ' '
003700021023     C     d55tfp        ifne      brvfle
003800021023     c                   except    stampa
003900021023     c                   add       1             agg               5 0
004000021023     C                   Z-ADD     D55TFP        BRVFLE
004100021023     C                   UPDATE    FIBRV000
004200021023     c                   endif
004300021023     C*
004400021023     c                   else
004500021023     c                   except    errore
004600021023     c                   endif
004700021023     c                   endif
004800021023     c*
004900021023     C                   READ      FIBRV01L                               31
005000021023     c                   enddo
005100000630     C*
005200000630     C                   EXCEPT    CONTA
005300021023     C*
005400021023     C                   CLEAR                   DSLV55
005500021023     C                   MOVEL     'C'           D55TLA
005600021023     C                   CALL      'FNLV55R'
005700021023     C                   PARM                    DSLV55
005800950512     C                   SETON                                        LR
005900950202     C*
006000021023     OQSYSPRT   E            stampa      1
006100000630     O                                              'SPUNTA:'
006200021023     O                       BRVnpg        Z   +  1
006300021023     O                       BRVfgs        Z   +  1
006400021023     O                       BRVNFV        Z   +  1
006500000630     O                       BRVLNP            +  1
006600000630     O                       BRVLNA        Z   +  1
006700000630     O                       BRVNRS        Z   +  1
006800000630     O                       BRVNSC        Z   +  1
006900021023     O                       BRVdcs            +  1 '    /  /  '
007000021023     O                       BRVfle            +  1
007100021023     O                                         +  5 'terminal corretto'
007200021023     O                       d55tfp            +  1
007300021023     OQSYSPRT   E            errore      1
007400021023     O                                              'SPUNTA:'
007500021023     O                       BRVnpg        Z   +  1
007600021023     O                       BRVfgs        Z   +  1
007700021023     O                       BRVNFV        Z   +  1
007800021023     O                       BRVLNP            +  1
007900021023     O                       BRVLNA        Z   +  1
008000021023     O                       BRVNRS        Z   +  1
008100021023     O                       BRVNSC        Z   +  1
008200021023     O                       BRVdcs            +  1 '    /  /  '
008300021023     O                       BRVfle            +  1
008400021023     O                                         +  5 'terminal non trovato'
008500000630     OQSYSPRT   E            CONTA       1
008600000630     O                                              'TOTALE:'
008700021023     O                       agg           Z   +  2
