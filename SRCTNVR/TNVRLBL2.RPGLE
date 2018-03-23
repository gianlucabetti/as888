000100950323     H DECEDIT('0,') DATEDIT(*DMY.)
000200041119     FFNLBL02L  uF a E           K DISK    usropn
000300041119     D DSBS50        E DS                  EXTNAME(TIBS55DS)
000400041122     D OVRlbl2l        S              1    DIM(39) CTDATA PERRCD(39)
000500041122     D Clbl2l          C                   CONST('/FNLBL02L)')
000600041119     c
000700950627     C*
000800041119     C     *loval        setll     FNLBL02l
000900041119     C                   read      FNLBL02l
001000950328     C*
001100950612    1C     *IN31         DOWEQ     *OFF
001200041119     c* se secondo flaggato con T flaggo anche il primo
001300041119     c                   if        (lblft2='R'  or lblft2='T')   and
001400041119     c                             lblft1=' '
001500041119     c                   eval      lblft1='U'
001600041119     c                   eval      lbldt1=lbldt2
001700041119     c                   update    fnlbl000
001800041119     c                   endif
001900041119     c
002000041119     C                   READ      FNLBL02l                               31
002100950627     C                   ENDDO
002200950627     C*
002300950201     C                   SETON                                        LR
002400041119     C*---------------------------------------------------------------
002500041119     C* PER FARLO FUNZIONARE SIA IN PROVA CHE IN BUONA
002600041119     C     *INZSR        begsr
002700041119     C
002800041119     C* overdbf  DI GAITRA FNLBL02L
002900041119     C                   CLEAR                   DSBS50
003000041119     c                   movel     'L'           i50tla
003100041119     c                   Movel     046           I50apo
003200041119     C                   CALL      'TIBS55R'
003300041119     C                   PARM                    DSBS50
003400041119     c                   movea     o50Alg        OVRLBL2L(30)
003500041119     C                   SETOFF                                       01
003600041119     C                   CLEAR                   COMMAN           80
003700041119     C                   MOVEA     ovrlbl2l      COMMAN
003800041122     c                   eval      comman=%trim(comman)+clbl2l
003900041119     C                   CALL      'QCMDEXC'
004000041119     C                   PARM                    COMMAN
004100041119     C                   PARM      50            LUNG             15 5
004200041119     C*
004300041119     C                   OPEN      FNLBL02L
004400041119     c
004500041119     C                   ENDSR
004600041119**         OVRLBL2L
004700041119OVRDBF FILE(FNLBL02L) TOFILE(XXXXXXXXXX/FNLBL02L)
