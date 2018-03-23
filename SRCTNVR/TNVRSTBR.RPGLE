000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010618     H*---------------------------------------------------------------*
000300940930     H*             -  PULIZIA STORICO BOLLE PARTENZE  -   ESEGUE     *
000400000000     H*---------------------------------------------------------------*
000500010618     FFNSTB01L  UF   E           K DISK
000600010618     FFNEVB01L  UF   E           K DISK
000700960118     FFNBL401L  UF   E           K DISK
000800010618      *---------------------------------------------------------------*
000900010618     D FNLGZ5DS      E DS
001000010618      *---------------------------------------------------------------*
001100010618     C     *loval        SETLL     FNSTB000
001200010618     C                   READ      FNSTB000
001300940930     C*
001400010618     C                   DOW       NOT %EOF(FNSTB01L)
001500010618     c                   EXSR      ELABORA
001600010618     C                   READ      FNSTB000
001700010618     c                   ENDDO
001800010618      *
001900010618     C                   eval      *inlr = *on
002000010618      *---------------------------------------------------------------------
002100010618      *    E L A B O R A
002200010618      *---------------------------------------------------------------------
002300010618     c     elabora       begsr
002400010618      *
002500010618      * Pulisco giacenza
002600010618     c                   z-add     stbaas        iz5aas
002700010618     c                   z-add     stblnp        iz5lnp
002800010618     c                   z-add     stbnrs        iz5nrs
002900010618     c                   z-add     stbnsp        iz5nsp
003000010618     c                   call      'FNLGZ6R'
003100010618     c                   PARM                    FNLGZ5DS
003200010618      *
003300010618      * CANCELLO GLI EVENTI
003400010618     C     Kstb          SETLL     FNEVB01L
003500010618     C     Kstb          reade     FNEVB01L
003600010618    5C                   DOW       not %EOF(FNEVB01L)
003700990810     C                   DELETE    FNEVB000
003800010618     C     Kstb          reade     FNEVB01L
003900950524    6C                   ENDDO
004000950524     C*
004100960118     C* CANCELLO LE NOTE IN FNBL4
004200010618     C     Kstb          SETLL     FNbl401L
004300010618     C     Kstb          reade     FNbl401L
004400010618    5C                   DOW       not %EOF(FNbl401L)
004500010618     C                   DELETE    FNbl4000
004600010618     C     Kstb          reade     FNbl401L
004700010618    6C                   ENDDO
004800950524     C* CANCELLO LA BOLLA
004900950524     C                   DELETE    FNSTB000
005000961204     C*
005100010618      *
005200010618     c                   endsr
005300940930     C*---------------------------------------------------------------*
005400960118     C     KSTB          KLIST                                                  FNGCP01L
005500941021     C                   KFLD                    STBAAS                         ANNO DI 4 CIFRE
005600941021     C                   KFLD                    STBLNP
005700941021     C                   KFLD                    STBNRS
005800941021     C                   KFLD                    STBNSP
