000100040330     D NSpediz         S             12
000200040330     D esito           S              1
000300040330     D tis174dsi     E DS                  INZ
000400040330     D tis174dso     E DS                  INZ
000500040330
000600040330     C     *ENTRY        PLIST
000700040330     C     NSPEDIZI74    PARM                    NSPEDIZ
000800040330
000900040330     c                   CALL      'TIS774R'
001000040330     c                   PARM      *BLANK        esito
001100040330     c                   PARM                    tis174dsi
001200040330     c                   PARM                    tis174dso
001300040330
001400040330     C                   EVAL      *INLR = *ON
