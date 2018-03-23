000100120223     ***************************************************************************
000200120223     **
000300140929     ** Tester di TRULORIR
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700140929     Ftruloridt CF   E             WORKSTN
000800120223
000900140930      /copy gaitrasrc/srcconst,truloriR
001000140520
001100140929     D trulorii      E DS                  INZ(*EXTDFT) QUALIFIED
001200140929     D trulorio      E DS                  qualified
001300140929     D trulorim      E DS                  INZ(*EXTDFT) QUALIFIED
001400140522
001500140522     D Wlibfil         S             21
001600140522     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
001700140522     D Wlibfil201      S             21    INZ('FILTRA201/        ')
001800120223     ***************************************************************************
001900120223
002000140930      /copy gaitrasrc/srcprotopr,trulorir
002100140520
002200140520       //--------------------------------------------------------------
002300140520       //?Definizione parametri programma.
002400140520       //--------------------------------------------------------------
002500140520     D prmRqsOpCode    s             10I 0
002600140520     D prmRpyOpCode    s             10I 0
002700140520     D prmRpyIdMsg     s             10I 0
002800140520     D prmRqsFormato   s             10A
002900140520     D prmRqsData      s          32767
003000140520     D prmRqsDataSize  s             10I 0
003100140520     D prmRpyFormato   s             10A
003200140520     D prmRpyData      s          32767
003300140520     D prmRpyDataSize  s             10I 0
003400140520     D prmRpyFormMsg   s             10A
003500140520     D prmRpyMessage   s          32767
003600140520     D prmRpyMsgSize   s             10I 0
003700140520
003800120223     ***************************************************************************
003900120223     **
004000120223     ** Main.
004100120223     **
004200120223     ***************************************************************************
004300120223
004400120223      /FREE
004500120224
004600120224       *INLR = *ON;
004700140521       *In01 = *On  ;
004800120312
004900120306 1     dow  not *inkc   ;
005000120306
005100140929       exfmt  uloridt;
005200140521
005300140521       if *inkj=*on  ;
005400140930         clear uloridt;
005500140929         clear filiale;
005600140521       endif  ;
005700140522       clear prmrpyidmsg ;
005800140522       clear prmrpyopcode;
005900140522       clear idmsg ;
006000140522       clear opcode;
006100120306
006200140521 2     if not *inkc  and not *inkj ;
006300140520
006400140929       prmrqsOpCode = TRULORI_RQSOPCODE_GET_FILIALE    ;
006500140929       prmrqsformato='TRULORII '  ;
006600140929       prmrpyformato='TRULORIO '  ;
006700140929       prmRpyFormMsg='TRULORIM '  ;
006800120224
006900140929       RESET TRULORIi ;
007000120306
007100140520       if versione<> ' ' ;
007200140929       TRULORIi.versione  = versione ;
007300140520       endif  ;
007400140929       TRULORIi.liniso2   = idlingua ;
007500140929       TRULORIi.FILIALE   = filiale ;
007600120306
007700140930         prmrqsData = trulorii;
007800140929         prmrqsDatasize = %SIZE(trulorii);
007900140929         prmrpyDatasize = %SIZE(trulorio);
008000140929         prmRpyMsgSize  = %SIZE(trulorim);
008100120224
008200140520 4       if   *inkf    ;
008300140930         GET_filiale(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
008400140520                         prmRqsFormato : prmRqsData : prmRqsDataSize:
008500140520                         prmRpyFormato : prmRpyData : prmRpyDataSize:
008600140520                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
008700120224
008800140520         clear outputmsg ;
008900140520
009000140520         opcode= prmrpyopcode  ;
009100140520         idmsg = prmrpyidmsg   ;
009200140520
009300140930 5       if   prmrpyOpCode = TRULORI_RPYOPCODE_ERROR  ;
009400140611           outputmsg=%subst(prmrpymessage:1785:1040) ;
009500140520 5       else ;
009600140930         trulorio  = prmrpyData;
009700140930         formato   = trulorio.formato  ;
009800140930         outputmsg=trulorio ;
009900140930  5       endif  ;
010000140930  4       endif  ;
010100120306
010200140522       if not *inkf  ;
010300140521       *in01=*off ;
010400140522       endif  ;
010500120306
010600120306 2         endif  ;
010700120306
010800120306 1        enddo  ;
010900120224
011000120224      /END-FREE
