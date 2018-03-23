000100120223     ***************************************************************************
000200120223     **
000300180112     ** Tester di tis7dp01r
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000900180112     Ftis7dpt1d CF   E             WORKSTN
001000120223
001100180112      /copy gaitrasrc/srcconst,tis7dp01r
001200140520
001300180112     D tis7dp0i0     E DS                  INZ(*EXTDFT) QUALIFIED
001400180112     D tis7dp0o0     E DS                  qualified
001700180112     D tis7dp0m0     E DS                  INZ(*EXTDFT) QUALIFIED
001900140522
003100120223     ***************************************************************************
003200120223
003300180112      /copy gaitrasrc/srcprotopr,tis7dp01r
003400140520
003500140520       //--------------------------------------------------------------
003600140520       //?Definizione parametri programma.
003700140520       //--------------------------------------------------------------
003800140520     D prmRqsOpCode    s             10I 0
003900140520     D prmRpyOpCode    s             10I 0
004000140520     D prmRpyIdMsg     s             10I 0
004100140520     D prmRqsFormato   s             10A
004200140520     D prmRqsData      s          32767
004300140520     D prmRqsDataSize  s             10I 0
004400140520     D prmRpyFormato   s             10A
004500140520     D prmRpyData      s          32767
004600140520     D prmRpyDataSize  s             10I 0
004700140520     D prmRpyFormMsg   s             10A
004800140520     D prmRpyMessage   s          32767
004900140520     D prmRpyMsgSize   s             10I 0
005000140520     D prmRpyFormForz  s             10A
005100140520     D prmRpyForzatu   s          32767
005200140520     D prmRpyForSize   s             10I 0
005300140520
005400120223     ***************************************************************************
005500120223     **
005600120223     ** Main.
005700120223     **
005800120223     ***************************************************************************
005900120223
007800120224
007900120224       *INLR = *ON;
008000140521       *In01 = *On  ;
008100120224
008300120312
008400120306 1     dow  not *inkc   ;
008500120306
008600140521       exfmt  LRYT0D ;
008700140521
008800140521       if *inkj=*on  ;
008900140521       clear  LRYT0D ;
009600140521       endif  ;
009700140522       clear prmrpyidmsg ;
009800140522       clear prmrpyopcode;
009900140522       clear idmsg ;
010000140522       clear opcode;
010100120306
010200140521 2     if not *inkc  and not *inkj ;
010300120305
010600180112       prmrqsOpCode = tis7dp01_RQSOPCODE_GET_IDBOLLA    ;
010700180112       prmrqsformato='TIS7DP0I0'  ;
010800180112       prmrpyformato='TIS7DP0O0'  ;
010900180112       prmRpyFormMsg='TIS7DP0M0'  ;
011000120224
011100180112       RESET tis7dp0i0;
011200120306
011400140520       if versione<> ' ' ;
011500180112       tis7dp0i0.versione  = versione ;
011600140520       endif  ;
011700180112       tis7dp0i0.liniso2   = idlingua ;
011800180112       tis7dp0i0.parceldpd = parceldpd;
012000120306
012100180112         prmrqsData = tis7dp0i0;
012200180112         prmrqsDatasize = %SIZE(tis7dp0i0);
012300180112         prmrpyDatasize = %SIZE(tis7dp0o0);
012400180112         prmRpyMsgSize  = %SIZE(tis7dp0m0);
012600120224
012800180112         tis7dp01_GetIdb(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
012900140520                         prmRqsFormato : prmRqsData : prmRqsDataSize:
013000140520                         prmRpyFormato : prmRpyData : prmRpyDataSize:
013100140520                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
013200140520         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
013300120224
013400140520         clear outputmsg ;
013500140520
013600140520         opcode= prmrpyopcode  ;
013700140520         idmsg = prmrpyidmsg   ;
013800140520
013900180112 5       if   prmrpyOpCode = TIS7DP01_RPYOPCODE_ERROR  ;
014000180115           outputmsg=%subst(prmrpymessage:175:1040) ;
014100140520 5       else ;
014200180112         tis7dp0o0 = prmrpyData;
014300180112         formato   = tis7dp0o0.formato  ;
014400180112         aas       = tis7dp0o0.idb_aas    ;
014500180112         lnp       = tis7dp0o0.idb_lnp    ;
014600180112         nrs       = tis7dp0o0.idb_nrs    ;
014700180112         nsp       = tis7dp0o0.idb_nsp    ;
014701180112         endif;
014702180112         endif;
015900140522
028700120306
028800120306 1        enddo  ;
028900120224
029000120224      /END-FREE
