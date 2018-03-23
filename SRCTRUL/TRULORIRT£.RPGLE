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
008200140611       ksu='00011639'  ;
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
010900140529       prmRpyFormMsg='FNLRY00M0'  ;
011000120224
011100140520       RESET fnlry00i0;
011200120306
011300140520       fnlry00i0.ksu       = ksu    ;
011400140520       if versione<> ' ' ;
011500140520       fnlry00i0.versione  = versione ;
011600140520       endif  ;
011700140520       fnlry00i0.liniso2   = idlingua ;
011800140520       fnlry00i0.idbolla   = didbolla;
011900140520       fnlry00i0.brtcode   = dbrtcode;
012000120306
012100140520         prmrqsData = fnlry00i0;
012200140520         prmrqsDatasize = %SIZE(fnlry00i0);
012300140522         prmrpyDatasize = %SIZE(fnlry00o0);
012400140522         prmRpyMsgSize  = %SIZE(fnlry00m0);
012500140522         prmRpyForSize  = %SIZE(fnlry00f0);
012600120224
012700140520 4       if   *inkf    ;
012800140520         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
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
013900140520 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
014000140611           outputmsg=%subst(prmrpymessage:1785:1040) ;
014100140520 5       else ;
014200140520         fnlry00o0 = prmrpyData;
014300140520         formato   = fnlry00o0.formato  ;
014400140520         butistrcon= fnlry00o0.butistrcon ;
014500140520         butappunt = fnlry00o0.butappunt  ;
014600140520         butferdep = fnlry00o0.butferdep  ;
014700140520         butindiriz= fnlry00o0.butindiriz ;
014800140520         butaltro  = fnlry00o0.butaltro   ;
014900140603         oraristdi = fnlry00o0.oraritndi  ;
015000140603         oraristdf = fnlry00o0.oraritndf  ;
015100140520         consrich  = fnlry00o0.dtconsric+ ' ' +fnlry00o0.orconsric+' '+
015200140520                     fnlry00o0.tpconsric ;
015201140925         oraristFi = fnlry00o0.oraritnfi  ;
015202140925         oraristff = fnlry00o0.oraritnff  ;
015300140521         destmail  = fnlry00o0.destmail  ;
015400140521         destcell  = fnlry00o0.destcell  ;
015500140604         mmintervc = fnlry00o0.mmintervc ;
015600140604         dateconst = fnlry00o0.dateconst ;
015700140604         nrdate    = fnlry00o0.nrdate    ;
015701140925         dateconti = fnlry00o0.dateconti ;
015702140925         nrdatei   = fnlry00o0.nrdatei   ;
015800140522         msgfdmitt = fnlry00o0.msgfdmitt ;
015801140616         msgfdcond = fnlry00o0.msgfdcond ;
015802140925         outputmsg = 'Messaggio preferenza orario: ' +  fnlry00o0.msgprfora ;
015900140522
016000140522         // salvo ksu idbolla e brtcod
016100140522         savksu=ksu  ;
016200140522         savidb=didbolla ;
016300140522         savbrt=dbrtcode ;
016400140522         // Se non ci sono errori reperisco da ARB i dati mancanti
016500140522         if didbolla>*zeros ;
016600140522         kaas= %int(%subst(didbolla:1:2));
016700140522         kaas=kaas+2000                  ;
016800140522         klnp= %int(%subst(didbolla:3:3));
016900140522         knrs= %int(%subst(didbolla:6:2));
017000140522         knsp= %int(%subst(didbolla:8:7));
017100140522         endif  ;
017200140522
017300140522         if dbrtcode>*zeros ;
017400140522         kaas= %int(%subst(dbrtcode:1:2));
017500140522         kaas=kaas+2000                  ;
017600140522         klnp= %int(%subst(dbrtcode:3:3));
017700140522         knrs= %int(%subst(dbrtcode:6:2));
017800140522         knsp= %int(%subst(dbrtcode:8:7));
017900140522         endif  ;
018000140522
018100140522         chain (kaas:klnp:knrs:knsp) fnarb01l  ;
018200140522         if %found(fnarb01l)  ;
018300140522         rsd=arbrsd  ;
018400140522         ind=arbind ;
018500140522         lod=arblod ;
018600140522         prd=arbprd ;
018700140522         cad=arbcad ;
018800140522         chain (kaas:klnp:knrs:knsp:'GEN') fiar501l  ;
018900140522         if %found(fiar501l)  ;
019000140522         dar5gen=ar5uni ;
019100140522         refercons=§ar5ref  ;
019200140522         telcons=§ar5teld ;
019300140522         endif  ;
019400140522         endif  ;
019500140521
019600140520 5     endif  ;
019700140522
019800140522       *in01=*on    ;
019900140520 4     endif  ;
020000140521 x3    else   ;
020100140521       prmrqsOpCode = FNLRY00_RQSOPCODE_PUT_ISTRUCO    ;
020200140522       prmrqsformato='FNLRY00I1'  ;
020300140522       prmrpyformato='FNLRY00O1'  ;
020400140529       prmRpyFormMsg='FNLRY00M0'  ;
020500140521
020600140521       RESET fnlry00i1;
020700140521
020800140521       fnlry00i1.ksu       = ksu    ;
0209001405214      if versione<> ' ' ;
021000140521       fnlry00i1.versione  = versione ;
0211001405214      endif  ;
021200140521       fnlry00i1.liniso2   = idlingua ;
021300140521       fnlry00i1.idbolla   = didbolla;
021400140521       fnlry00i1.brtcode   = dbrtcode;
021500140521       fnlry00i1.tipoela   = tipoela ;
021600140523       fnlry00i1.newrefcons= refercons ;
021700140523       fnlry00i1.newtelcons= telcons   ;
021800140523       fnlry00i1.newdesmail= destmail  ;
021900140523       fnlry00i1.newdescell= destcell  ;
022000140521       fnlry00i1.tipodispo = tipodispo ;
022001140925       if tipodispo='1' ;
022100140523       fnlry00i1.newdtcrich= dtconsric ;
022200140523       fnlry00i1.neworcrich= orconsric ;
022300140521       fnlry00i1.prconsric = prconsric ;
022301140925       else;
022302140925       if tipodispo='3';
022303140925       fnlry00i1.newdtcrici= dtconsric ;
022304140925       fnlry00i1.neworcrici= orconsric ;
022305140925       fnlry00i1.prconsrici= prconsric ;
022306140925       endif;
022307140925       endif;
022400140523       fnlry00i1.newrsd    = rsd       ;
022500140523       fnlry00i1.newind    = ind       ;
022600140523       fnlry00i1.newlod    = lod       ;
022700140523       fnlry00i1.newcad    = cad       ;
022800140523       fnlry00i1.newprd    = prd       ;
022900140523       fnlry00i1.newnzd    = nzd       ;
023000140521       fnlry00i1.altro     = altro     ;
023100140523
023200140523       fnlry00i1.oldrefcons= §ar5ref   ;
023300140523       fnlry00i1.oldtelcons= §ar5teld  ;
023400140523       fnlry00i1.olddesmail= fnlry00o0.destmail    ;
023500140523       fnlry00i1.olddescell= fnlry00o0.destcell    ;
023600140523       fnlry00i1.olddtcrich= fnlry00o0.dtconsric   ;
023700140523       fnlry00i1.oldorcrich= fnlry00o0.orconsric   ;
023800140523       fnlry00i1.oldtpcrich= fnlry00o0.tpconsric   ;
023900140523       fnlry00i1.oldrsd    = arbrsd       ;
024000140523       fnlry00i1.oldind    = arbind       ;
024100140523       fnlry00i1.oldlod    = arblod       ;
024200140523       fnlry00i1.oldcad    = arbcad       ;
024300140523       fnlry00i1.oldprd    = arbprd       ;
024400140523       fnlry00i1.oldnzd    = arbnzd    ;
024500140521
024600140521         prmrqsData = fnlry00i1;
024700140521         prmrqsDatasize = %SIZE(fnlry00i1);
024800140522         prmrpyDatasize = %SIZE(fnlry00o1);
024900140522         prmRpyMsgSize  = %SIZE(fnlry00m0);
025000140522         prmRpyForSize  = %SIZE(fnlry00f0);
025100140521
025200140521 4       if   *inkf    ;
025300140521         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
025400140521                         prmRqsFormato : prmRqsData : prmRqsDataSize:
025500140521                         prmRpyFormato : prmRpyData : prmRpyDataSize:
025600140521                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
025700140521         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
025800140521
025900140521         clear outputmsg ;
026000140521
026100140521         opcode= prmrpyopcode  ;
026200140521         idmsg = prmrpyidmsg   ;
026300140521
026400140521 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
026500140611           outputmsg=%subst(prmrpymessage:1785:1040) ;
026600140521 x5      else ;
026700140521         fnlry00o1 = prmrpyData;
026800140521         formato   = fnlry00o1.formato  ;
026900140521           outputmsg=fnlry00o1.msgconferm;
027000140521         fdidfil   = fnlry00o1.fdidfil  ;
027100140522
027200140522         savksu=ksu  ;
027300140522         savidb=didbolla ;
027400140522         savbrt=dbrtcode ;
027500140522
027600140521 5       endif  ;
027700140522        *in01=*on    ;
027800140521 4       endif  ;
027900140521
028000140520 3     endif  ;
028100120306
028200140522       if not *inkf  ;
028300140521       *in01=*off ;
028400140522       endif  ;
028500120306
028600120306 2         endif  ;
028700120306
028800120306 1        enddo  ;
028900120224
029000120224      /END-FREE
