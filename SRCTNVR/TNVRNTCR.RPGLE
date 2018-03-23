000100001215     fTFNTC00F  IP   E             DISK    rename(tfntc:tfntcDEL) prefix(D_)
000200001215     FTFNTC01L  UF A E           K DISK
000300001215
000400001215     d DEL_TFNTC     E DS                  extname(TFNTC00F) prefix(D_)
000500001215     d OLD_TFNTC     E DS                  extname(TFNTC00F)
000600001215
000700001215      *
000800001215     c     kntc          klist
000900001215     c                   kfld                    D_NTCapl
001000001215     c                   kfld                    D_NTCnk1
001100001215     c                   kfld                    D_NTCnk2
001200001215     c                   kfld                    D_NTCtnt
001300001215      *
001400001215
001500001215     C                   IF        D_NTCapl = 'C'
001600001215      *
001700001215     C                   IF        D_NTCtnt = 'CO'  or  D_NTCtnt = 'IN'         Parametri elaborazio
001800001215     C                   movel     DEL_TFNTC     OLD_TFNTC                      Subfile
001900001215     c                   WRITE     TFNTC
002000001215      *
002100001215     C                   ELSE
002200001215      *
002300001215     C     KNTC          CHAIN     TFNTC
002400001215     C                   IF        NOT %FOUND
002500001215     C                   movel     DEL_TFNTC     OLD_TFNTC                      Subfile
002600001215     c                   WRITE     TFNTC
002700001215     C                   ENDIF
002800001215      *
002900001215     C                   ENDIF
003000001215      *
003100001215     C                   ENDIF
