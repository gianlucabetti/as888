000100120223     ***************************************************************************
000200120223     **
000300140520     ** Tester di FNLRY00R
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700140522     FFnarb01l  IF   E           K DISK    extfile(wlibfil) usropn
000800140522     FFiar501l  IF   E           K DISK    extfile(wlibfil) usropn
000900140521     Ffnlryt0d  CF   E             WORKSTN
001000120223
001100140520      /copy gaitrasrc/srcconst,FNLRY00R
001200140520
001300140520     D fnlry00i0     E DS                  INZ(*EXTDFT) QUALIFIED
001400140520     D fnlry00o0     E DS                  qualified
001500140520     D fnlry00i1     E DS                  INZ(*EXTDFT) QUALIFIED
001600140520     D fnlry00o1     E DS                  qualified
001700140522     D fnlry00m0     E DS                  INZ(*EXTDFT) QUALIFIED
001800140522     D fnlry00f0     E DS                  INZ(*EXTDFT) QUALIFIED
001900140522
002000140522     D dar5gen       E DS
002100140522     D savksu          s                   like(ksu)
002200140522     D savidb          s                   like(didbolla)
002300140522     D savbrt          s                   like(dbrtcode)
002400140522     d kaas            s                   like(arbaas)
002500140522     d klnp            s                   like(arblnp)
002600140522     d knrs            s                   like(arbnrs)
002700140522     d knsp            s                   like(arbnsp)
002800140522     D Wlibfil         S             21
002900140522     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
003000140522     D Wlibfil201      S             21    INZ('FILTRA201/        ')
003100120223     ***************************************************************************
003200120223
003300140520      /copy gaitrasrc/srcprotopr,Fnlry00r
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
006000120223      /FREE
006100140522       //?Apertura file
006200140522         wlibfil=wlibfil201;
006300140522         %subst(wlibfil:11:8)='FNARB01L';
006400140522         open(e) fnarb01l;
006500140522         if not %open(fnarb01l);
006600140522            wlibfil=wlibfilprd;
006700140522            %subst(wlibfil:11:8)='FNARB01L';
006800140522            open fnarb01l;
006900140522         endif;
007000140522         wlibfil=wlibfil201;
007100140522         %subst(wlibfil:11:8)='FIAR501L';
007200140522         open(e) fiar501l;
007300140522         if not %open(fiar501l);
007400140522            wlibfil=wlibfilprd;
007500140522            %subst(wlibfil:11:8)='FIAR501L';
007600140522            open fiar501l;
007700140522         endif;
007800120224
007900120224       *INLR = *ON;
008000140521       *In01 = *On  ;
008100120224
008200140522       ksu='00632910'  ;
008300120312
008400120306 1     dow  not *inkc   ;
008500120306
008600140521       exfmt  LRYT0D ;
008700140521
008800140521       if *inkj=*on  ;
008900140521       clear  LRYT0D ;
009000140522         ksu=savksu  ;
009100140522         didbolla =savidb ;
009200140522         dbrtcode =savbrt ;
009300140522         clear savksu ;
009400140522         clear savidb ;
009500140522         clear savbrt ;
009600140521       endif  ;
009700140522       clear prmrpyidmsg ;
009800140522       clear prmrpyopcode;
009900140522       clear idmsg ;
010000140522       clear opcode;
010100120306
010200140521 2     if not *inkc  and not *inkj ;
010300120305
010400140520 3     if tipoela=' ' and tipodispo=' '  ;
010500140520
010600140520       prmrqsOpCode = FNLRY00_RQSOPCODE_GET_ISTRUCO    ;
010700140522       prmrqsformato='FNLRY00I0'  ;
010800140522       prmrpyformato='FNLRY00O0'  ;
010900120224
011000140520       RESET fnlry00i0;
011100120306
011200140520       fnlry00i0.ksu       = ksu    ;
011300140520       if versione<> ' ' ;
011400140520       fnlry00i0.versione  = versione ;
011500140520       endif  ;
011600140520       fnlry00i0.liniso2   = idlingua ;
011700140520       fnlry00i0.idbolla   = didbolla;
011800140520       fnlry00i0.brtcode   = dbrtcode;
011900120306
012000140520         prmrqsData = fnlry00i0;
012100140520         prmrqsDatasize = %SIZE(fnlry00i0);
012200140522         prmrpyDatasize = %SIZE(fnlry00o0);
012300140522         prmRpyMsgSize  = %SIZE(fnlry00m0);
012400140522         prmRpyForSize  = %SIZE(fnlry00f0);
012500120224
012600140520 4       if   *inkf    ;
012700140520         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
012800140520                         prmRqsFormato : prmRqsData : prmRqsDataSize:
012900140520                         prmRpyFormato : prmRpyData : prmRpyDataSize:
013000140520                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
013100140520         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
013200120224
013300140520         clear outputmsg ;
013400140520
013500140520         opcode= prmrpyopcode  ;
013600140520         idmsg = prmrpyidmsg   ;
013700140520
013800140520 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
013900140520           outputmsg=prmrpymessage ;
014000140520 5       else ;
014100140520         fnlry00o0 = prmrpyData;
014200140520         formato   = fnlry00o0.formato  ;
014300140520         butistrcon= fnlry00o0.butistrcon ;
014400140520         butappunt = fnlry00o0.butappunt  ;
014500140520         butferdep = fnlry00o0.butferdep  ;
014600140520         butindiriz= fnlry00o0.butindiriz ;
014700140520         butaltro  = fnlry00o0.butaltro   ;
014800140520         oraristdi = fnlry00o0.oraristdi  ;
014900140520         oraristdf = fnlry00o0.oraristdf  ;
015000140520         consrich  = fnlry00o0.dtconsric+ ' ' +fnlry00o0.orconsric+' '+
015100140520                     fnlry00o0.tpconsric ;
015200140521         destmail  = fnlry00o0.destmail  ;
015300140521         destcell  = fnlry00o0.destcell  ;
015301140522         msgfdmitt = fnlry00o0.msgfdmitt ;
015400140522
015500140522         // salvo ksu idbolla e brtcod
015600140522         savksu=ksu  ;
015700140522         savidb=didbolla ;
015800140522         savbrt=dbrtcode ;
015900140522         // Se non ci sono errori reperisco da ARB i dati mancanti
016000140522         if didbolla>*zeros ;
016100140522         kaas= %int(%subst(didbolla:1:2));
016200140522         kaas=kaas+2000                  ;
016300140522         klnp= %int(%subst(didbolla:3:3));
016400140522         knrs= %int(%subst(didbolla:6:2));
016500140522         knsp= %int(%subst(didbolla:8:7));
016600140522         endif  ;
016700140522
016800140522         if dbrtcode>*zeros ;
016900140522         kaas= %int(%subst(dbrtcode:1:2));
017000140522         kaas=kaas+2000                  ;
017100140522         klnp= %int(%subst(dbrtcode:3:3));
017200140522         knrs= %int(%subst(dbrtcode:6:2));
017300140522         knsp= %int(%subst(dbrtcode:8:7));
017400140522         endif  ;
017500140522
017600140522         chain (kaas:klnp:knrs:knsp) fnarb01l  ;
017700140522         if %found(fnarb01l)  ;
017800140522         rsd=arbrsd  ;
017900140522         ind=arbind ;
018000140522         lod=arblod ;
018100140522         prd=arbprd ;
018200140522         cad=arbcad ;
018300140522         chain (kaas:klnp:knrs:knsp:'GEN') fiar501l  ;
018400140522         if %found(fiar501l)  ;
018500140522         dar5gen=ar5uni ;
018600140522         refercons=§ar5ref  ;
018700140522         telcons=§ar5teld ;
018800140522         endif  ;
018900140522         endif  ;
019000140521
019100140520 5     endif  ;
019200140522
019300140522       *in01=*on    ;
019400140520 4     endif  ;
019500140521 x3    else   ;
019600140521       prmrqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO    ;
019700140522       prmrqsformato='FNLRY00I1'  ;
019800140522       prmrpyformato='FNLRY00O1'  ;
019900140521
020000140521       RESET fnlry00i1;
020100140521
020200140521       fnlry00i1.ksu       = ksu    ;
0203001405214      if versione<> ' ' ;
020400140521       fnlry00i1.versione  = versione ;
0205001405214      endif  ;
020600140521       fnlry00i1.liniso2   = idlingua ;
020700140521       fnlry00i1.idbolla   = didbolla;
020800140521       fnlry00i1.brtcode   = dbrtcode;
020900140521       fnlry00i1.tipoela   = tipoela ;
021000140521       fnlry00i1.refercons = refercons ;
021100140522       fnlry00i1.telefcons = telcons   ;
021200140521       fnlry00i1.destmail  = destmail  ;
021300140521       fnlry00i1.destcell  = destcell  ;
021400140522       fnlry00i1.destcell  = destcell  ;
021500140521       fnlry00i1.tipodispo = tipodispo ;
021600140521       fnlry00i1.dtconsric = dtconsric ;
021700140521       fnlry00i1.orconsric = orconsric ;
021800140521       fnlry00i1.prconsric = prconsric ;
021900140521       fnlry00i1.rsd       = rsd       ;
022000140521       fnlry00i1.ind       = ind       ;
022100140521       fnlry00i1.lod       = lod       ;
022200140521       fnlry00i1.cad       = cad       ;
022300140521       fnlry00i1.prd       = prd       ;
022400140521       fnlry00i1.nzd       = nzd       ;
022500140521       fnlry00i1.altro     = altro     ;
022600140521
022700140521         prmrqsData = fnlry00i1;
022800140521         prmrqsDatasize = %SIZE(fnlry00i1);
022900140522         prmrpyDatasize = %SIZE(fnlry00o1);
023000140522         prmRpyMsgSize  = %SIZE(fnlry00m0);
023100140522         prmRpyForSize  = %SIZE(fnlry00f0);
023200140521
023300140521 4       if   *inkf    ;
023400140521         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
023500140521                         prmRqsFormato : prmRqsData : prmRqsDataSize:
023600140521                         prmRpyFormato : prmRpyData : prmRpyDataSize:
023700140521                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
023800140521         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
023900140521
024000140521         clear outputmsg ;
024100140521
024200140521         opcode= prmrpyopcode  ;
024300140521         idmsg = prmrpyidmsg   ;
024400140521
024500140521 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
024600140521           outputmsg=prmrpymessage ;
024700140521 x5      else ;
024800140521         fnlry00o1 = prmrpyData;
024900140521         formato   = fnlry00o1.formato  ;
025000140521           outputmsg=fnlry00o1.msgconferm;
025100140521         fdidfil   = fnlry00o1.fdidfil  ;
025200140522
025300140522         savksu=ksu  ;
025400140522         savidb=didbolla ;
025500140522         savbrt=dbrtcode ;
025600140522
025700140521 5       endif  ;
025800140522        *in01=*on    ;
025900140521 4       endif  ;
026000140521
026100140520 3     endif  ;
026200120306
026300140522       if not *inkf  ;
026400140521       *in01=*off ;
026500140522       endif  ;
026600120306
026700120306 2         endif  ;
026800120306
026900120306 1        enddo  ;
027000120224
027100120224      /END-FREE
