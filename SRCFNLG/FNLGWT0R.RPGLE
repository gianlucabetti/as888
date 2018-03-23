000100120223     ***************************************************************************
000200120223     **
000300170216     ** Tester di FNLgw00R
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700170216     FfnlGWt0d  CF   E             WORKSTN
000800120223
000900170216      /copy gaitrasrc/srcconst,FNLgw00R
001000170216      /copy gaitrasrc/srcprotopr,Fnlgw00r
001100140520
001200170216     D fnlgw00i0     E DS                  INZ(*EXTDFT) QUALIFIED
001300170216     D fnlgw00o0     E DS                  qualified
001400170216     D fnlgw00m0     E DS                  INZ(*EXTDFT) QUALIFIED
001500140522
001600140522     D savksu          s                   like(ksu)
001700140522     D savidb          s                   like(didbolla)
001800140522     D savbrt          s                   like(dbrtcode)
001900170220     D savkgiac        s                   like(dkeygiac)
002000120223     ***************************************************************************
002100120223
002200140520
002300140520       //--------------------------------------------------------------
002400140520       //?Definizione parametri programma.
002500140520       //--------------------------------------------------------------
002600140520     D prmRqsOpCode    s             10I 0
002700140520     D prmRpyOpCode    s             10I 0
002800140520     D prmRpyIdMsg     s             10I 0
002900140520     D prmRqsFormato   s             10A
003000140520     D prmRqsData      s          32767
003100140520     D prmRqsDataSize  s             10I 0
003200140520     D prmRpyFormato   s             10A
003300140520     D prmRpyData      s          32767
003400140520     D prmRpyDataSize  s             10I 0
003500140520     D prmRpyFormMsg   s             10A
003600140520     D prmRpyMessage   s          32767
003700140520     D prmRpyMsgSize   s             10I 0
003800140520
003900120223     ***************************************************************************
004000120223     **
004100120223     ** Main.
004200120223     **
004300120223     ***************************************************************************
004400120223
004500120223      /FREE
004600120224
004700120224       *INLR = *ON;
004800140521       *In01 = *On  ;
004900120224
005000140611       ksu='00011639'  ;
005100120312
005200120306 1     dow  not *inkc   ;
005300120306
005400140521       exfmt  LRYT0D ;
005500140521
005600140521       if *inkj=*on  ;
005700140521       clear  LRYT0D ;
005800140522         ksu=savksu  ;
005900140522         didbolla =savidb ;
006000140522         dbrtcode =savbrt ;
006100170220         dkeygiac =savkgiac ;
006200140522         clear savksu ;
006300140522         clear savidb ;
006400140522         clear savbrt ;
006500170220         clear savkgiac ;
006600140521       endif  ;
006700140522       clear prmrpyidmsg ;
006800140522       clear prmrpyopcode;
006900140522       clear idmsg ;
007000140522       clear opcode;
007100120306
007200140521 2     if not *inkc  and not *inkj ;
007300140520
007400170217       prmrqsOpCode = FNLGW00_RQSOPCODE_GET_POSSIBILI_DISPO;
007500170216       prmrqsformato='FNLGW00I0'  ;
007600170216       prmrpyformato='FNLGW00O0'  ;
007700170216       prmRpyFormMsg='FNLGW00M0'  ;
007800120224
007900170216       RESET fnlGW00i0;
008000120306
008100170216       fnlGW00i0.ksu       = ksu    ;
008200140520       if versione<> ' ' ;
008300170216       fnlGW00i0.versione  = versione ;
008400140520       endif  ;
008500170216       fnlGW00i0.liniso2   = idlingua ;
008600170216       fnlGW00i0.idbolla   = didbolla;
008700170216       fnlGW00i0.brtcode   = dbrtcode;
008800170220       fnlGW00i0.keygiac   = dkeygiac;
008900120306
009000170216         prmrqsData = fnlGW00i0;
009100170216         prmrqsDatasize = %SIZE(fnlGW00i0);
009200170216         prmrpyDatasize = %SIZE(fnlGW00o0);
009300170216         prmRpyMsgSize  = %SIZE(fnlGW00m0);
009400120224
009500170216 4       if   *inkf    ;
009600170216         FnlGW00_dispo(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
009700170216                       prmRqsFormato : prmRqsData : prmRqsDataSize:
009800170216                       prmRpyFormato : prmRpyData : prmRpyDataSize:
009900170216                       prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
010000120224
010100140520         clear outputmsg ;
010200140520
010300140520         opcode= prmrpyopcode  ;
010400140520         idmsg = prmrpyidmsg   ;
010500140520
010600170216 5       if   prmrpyOpCode = FNLGW00_RPYOPCODE_ERROR  ;
010700140611           outputmsg=%subst(prmrpymessage:1785:1040) ;
010800140520 5       else ;
010900170216         fnlgw00o0 = prmrpyData;
011000170216       //formato   = fnlgw00o0.formato  ;
011100170216       //BUTRESO    =fnlgw00o0.BUTRESO  ;
011200170216       //BUTCON     =fnlgw00o0.BUTCON   ;
011300170216       //BUTCONAPP  =fnlgw00o0.BUTCONAPP;
011400170216       //BUTCONDCRI =fnlgw00o0.BUTCONDCRI;
011500170216       //BUTCONDCRD =fnlgw00o0.BUTCONDCRD;
011600170216       //BUTCONIND  =fnlgw00o0.BUTCONIND;
011700170216       //BUTFD      =fnlgw00o0.BUTFD;
011800170216         vout      = fnlgw00o0  ;
011900140522
012000140522         // salvo ksu idbolla e brtcod
012100140522         savksu=ksu  ;
012200140522         savidb=didbolla ;
012300140522         savbrt=dbrtcode ;
012400170220         savkgiac=dkeygiac ;
012500170216         endif  ;
012600140521
012700170216 5       endif  ;
012800140522
012900140522       *in01=*on    ;
013000170216 2     endif  ;
013100170216 1     enddo;
013200120224
013300120224      /END-FREE
