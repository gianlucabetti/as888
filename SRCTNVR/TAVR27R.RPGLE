000100040329     H DECEDIT('0,') DATEDIT(*DMY.)
000200040329     H*         *----------------------------------------------------*
000300040329     H*       comodo
000400040329     H*--------------------------------------------------------------*
000500040329     FTNOFM01L  IF   E           K DISK
000600040329     FTNVIS02L  IF   E           K DISK
000700040329     F                                     RENAME(TNVIS000:TNVIS002)
000800040329     F                                     INFDS(TAR)
000900040329     FTNCPS04L  IF   E           K DISK
001000040329     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
001100040329     D TAR             DS
001200040329     D  VISNRR               397    400B 0
001300040329     D BS56DS        E DS                  EXTNAME(TIBS56DS)
001400040329     d  FIL                    7    756  0 DIM(250)
001500040329     C     KCPS          KLIST
001600040329     C                   KFLD                    VISNRV
001700040329     C                   KFLD                    KFST
001800040329     C* DEFINIZIONE CAMPI
001900040329     C     *LIKE         DEFINE    CPSFST        KFST
002000040329     C                   MOVEL     '2 '          KFST
002100040329      *
002200040329     C                   CLEAR                   BS56DS
002300040329     C                   MOVEL     059           I56PPO
002400040329     C                   MOVEL     'POA'         I56MOD
002500040329     C                   CALL      'TIBS56R'
002600040329     C                   PARM                    BS56DS
002700040329     C*
002800040329     C     *LOVAL        SETLL     TNVIS002
002900040329     C                   READ      TNVIS002                               30
003000040329     C* 30 ON - FINE LETTURA
003100040329    1C     *IN30         DOWEQ     '0'
003200040329     C     VISNRV        SETLL     TNTAM000                               32
003300040329      *
003400040329     c     *in32         ifeq      *off
003500040329     C     KCPS          CHAIN     TNCPS04L
003600040329      *
003700040329      *
003800040329    1C                   DOW       not %EOF(TNCPS04L)
003900040329      * Controllo che il commerciale appartenga al mio AS
004000040329     C                   MOVEL     CPSCMM        W0030             3 0
004100040329     c                   except    detta
004200040329     C     w0030         lookup    Fil                                    31
004300040329     C   31              leave
004400040329     C     KCPS          READE     TNCPS04L
004500040329   e1C                   ENDDO
004600040329     c                   endif
004700040329     C                   READ      TNVIS002                               30
004800040329   e1C                   ENDDO
004900040329     C*
005000040329     c                   seton                                        lr
005100040329     Oprtf198   E            detta       1
005200040329     O                       cpscmm           +   1
005300040329     O                       w0030            +   3
005400040329     O                       visnrv           +   3
