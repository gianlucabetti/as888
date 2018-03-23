000100071112     H DECEDIT('0,')
000200080325     ffnbrv07l  if   e           k disk
000300080219     d tutti           s             10  0
000400080219     c
000500080219      /free
000600080325       setll *loval   fnbrv07l      ;
000700080325       read           fnbrv07l      ;
000800080325       dow    not %eof(fnbrv07l)    ;
000900080325       leave    ;
001000080325       read           fnbrv07l      ;
001100080325       enddo   ;
001200080219      /end-free
001300071112     C
001400020502     C                   SETON                                        LR
