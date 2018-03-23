000100910110     H DECEDIT('0,') DATEDIT(*DMY.)
000200020905?
000300910110     FKFSIF01L  IF   E           K DISK
000400020905     FKPLIN01L  IF   E           K DISK    usropn
000500020905?
000600020905     d Klingua         s              1    inz('1')
000700020905     D Lista           S            220    INZ                                  colli x Segnacolli
000800020906     D WlistaL         S             11    DIM(20) INZ                          colli x Segnacolli
000900020905     d lenght          S             15  5 INZ(350)                             *Lunghezza cmd CLP
001000020905     d comman          S            350                                         *valore cmd CLP
001100020905                                                                                *valore cmd CLP
001200020905     D KPLINDS       E DS                  EXTNAME(KPLIN00F)
001300020905     d  listaL                12    211    DIM(20) INZ
001400020905?
001500020905     C     PSYF          CHAIN     KFSIF01L
001600020905
001700020905     C                   SELECT
001800020905     C                   WHEN      not %FOUND
001900020905     C                   WHEN      KLBSI <> *BLANKS
002000020906     C                   movel     KLBSI         Lista
002100020905     C                   WHEN      newmodulo = 'S'
002200020905     c                   open      KPLIN01L
002300020905     C     KPLIN         chain     KPLIN01L
002400020905     C                   If        %FOUND
002500020906     C                   movel     listaL        WlistaL
002600020906     C                   movea     WlistaL       LISTA
002700020905     c                   Endif
002800020905     C                   ENDSL
002900020905
003000020905     c                   eval      comman = 'CHGLIBL LIBL(QTEMP     ' +
003100020905     c                                     lista + ' PROBAS     QGPL)'
003200020905     c                   CALL      'QCMDEXC'                            20
003300020905     c                   PARM                    comman
003400020905     c                   PARM                    lenght
003500020905
003600020905     C                   eval      *inlr = *on
003700020905      *---------------------------------------------------------------
003800020905     C     *ENTRY        PLIST                                                  *
003900020905     C                   PARM                    PSYF             10            *
004000020905     C                   PARM                    NEWMODULO         1            *
004100020905?
004200020905     C     kplin         KLIST
004300020905     C                   KFLD                    PSYF
004400020905     C                   KFLD                    klingua
