000100000000     H Option( *SrcStmt )
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500100612     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
000600100621     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
000700100612
000800100922     D Par_InetHost    S             32A
000900100612
001000100612
001100100612
001200100612     C     *ENTRY        PLIST
001300100922     C                   PARM                    Par_InetHost
001400100612     C*
001500100922     C                   eval      Par_InetHost = %trim(Par_InetHost)
001600100922     C                   eval      pInInetHost = Par_InetHost
001700090520     C*
001800090610
001900090610      /FREE
002000090610
002100090710
002200110225         if UBDNSLKP_Retrieve(pInInetHost : pOutInetFound : pOutInetType) = 0;
002300100922            Dsply (%subst(Par_InetHost:1:25) + ' ' +
002400100922                   %subst(pOutInetFound:1:25));
002500090710         endif;
002600100612
002700090610
002800090610      /END-FREE
002900000000     **
003000090520     C                   Seton                                        LR
003100000000     **
