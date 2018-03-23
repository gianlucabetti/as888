000100071112     H DECEDIT('0,')
000200080219     d tutti           s             10  0
000300080219     d campo           s              1    dim(5)
000400080219     c
000500080219      /free
000600080219       campo(1)='B'  ;
000700080219       campo(2)='A'  ;
000800080219       campo(3)='B'  ;
000900080219       campo(4)='B'  ;
001000080219       campo(5)='A'  ;
001100080219       tutti= %lookup ('A':campo)   ;
001200080219      /end-free
001300071112     C
001400080219     C     tutti         DSPLY
001500020502     C                   SETON                                        LR
