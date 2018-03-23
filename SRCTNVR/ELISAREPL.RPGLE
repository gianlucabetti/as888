000100071112     H DECEDIT('0,')
000200080930     D stringa         S             15    inz('PROVA, S.R.L.')
000300080930     D stringa2        S             15
000400080930     c                   eval      stringa2=%trim(stringa:',.')
000500071112     C
000600080930     C     stringa2      DSPLY
000700020502     C                   SETON                                        LR
