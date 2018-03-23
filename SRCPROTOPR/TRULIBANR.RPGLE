000100100730      // -?Coordinate bancarie
000200100730     d TrulIbanR       pr                  extpgm('TRULIBANR')
000300100730     d  rqsOpCode                    10a   const
000400100730     d  rpyEsito                     10i 0
000500100730     d  rqsFormatName                10a   const
000600100730     d  rqsData                     256a   options(*varsize)
000700100730     d  rqsDataSize                  10i 0 const
000800100730     d  rpyFormatName                10a   const
000900100730     d                                     options(*nopass)
001000100730     d  rpyData                     256a   options(*varsize:*nopass)
001100100730     d  rpyDataSize                  10i 0 const
001200100730     d                                     options(*nopass)
