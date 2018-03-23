000100950323     H DECEDIT('0,') DATEDIT(*DMY.)
000200041117     FFNLBL02L  IF   E           K DISK
000300041119     drecordlbl      E DS                  extname(fnlbl00f)
000400950627     C*
000500041117     C     *LOVAL        SETLL     FNLBL02L
000600041117     C                   READ      FNLBL02L                               31
000700950328     C*
000800950612    1C     *IN31         DOWEQ     *OFF
000900041119     C                   call      'FNLG25R'
001000041119     c                   parm                    recordlbl
001100041119     c                   parm      ' '           i25tla            1
001200950627     C*
001300041119     C                   READ      FNLBL02L                               31
001400950627     C                   ENDDO
001500950627     C*
001600950201     C                   SETON                                        LR
