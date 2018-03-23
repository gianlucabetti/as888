000100120223     ***************************************************************************
000200120223     **
000300160314     ** Tester di TRTC0T0R
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700140522     FFnarb01l  IF   E           K DISK    extfile(wlibfil) usropn
000900160314     FTRTC0t0d  CF   E             WORKSTN
001000120223
001100160314      /copy gaitrasrc/srcconst,TRTC0TTR
001200140520
001300160314     D TRTC0TTi0     E DS                  INZ(*EXTDFT) QUALIFIED
001400160314     D TRTC0TTo0     E DS                  qualified
001900140522
002400140522     d kaas            s                   like(arbaas)
002500140522     d klnp            s                   like(arblnp)
002600140522     d knrs            s                   like(arbnrs)
002700140522     d knsp            s                   like(arbnsp)
002701160314
002800140522     D Wlibfil         S             21
002900140522     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
003000140522     D Wlibfil201      S             21    INZ('FILTRA201/        ')
003100120223     ***************************************************************************
003200120223
003300160314      /copy gaitrasrc/srcprotopr,TRTC0TTr
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
007800120224
007900120224       *INLR = *ON;
008000140521       *In01 = *On  ;
008100120224
008400120306 1     dow  not *inkc   ;
008500120306
008600140521       exfmt  LRYT0D ;
008700140521
009700140522       clear prmrpyidmsg ;
009800140522       clear prmrpyopcode;
010100120306
010200160314 2     if not *inkc  ;
010500140520
010600160314         prmrqsOpCode = *ZERO       ;
010700160314         prmrqsformato='TRTC0TTI0'  ;
010800160314         prmrpyformato='TRTC0TTO0'  ;
011000120224
011100160314         RESET TRTC0TTi0;
011200120306
011400160314         if versione<> ' ' ;
011500160314           TRTC0TTi0.versione  = versione ;
011600160314         endif  ;
011601160314
011700160314         TRTC0TTi0.liniso2   = idlingua ;
011800160314         TRTC0TTi0.idbolla   = didbolla;
012000120306
012100160314         prmrqsData = TRTC0TTi0;
012200160314         prmrqsDatasize = %SIZE(TRTC0TTI0);
012300160314         prmrpyDatasize = %SIZE(TRTC0TTO0);
012600120224
012700160314 4       if  *inkf    ;
012800160314           TRTC0TT_HTTP(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
012900160314                        prmRqsFormato : prmRqsData : prmRqsDataSize:
013000160314                        prmRpyFormato : prmRpyData : prmRpyDataSize) ;
013300120224
013400160314           clear outputmsg ;
013500140520
013600160314           opcode= prmrpyopcode  ;
013700160314           idmsg = prmrpyidmsg   ;
013800140520
013900160314 5         if  prmrpyOpCode = TRTC0TT_RPYOPCODE_ERROR  ;
014000160314             outputmsg=%subst(prmrpymessage:1785:1040) ;
014100160314 5         else ;
014200160314             TRTC0TTo0 = prmrpyData;
014300160314             formato   = TRTC0TTo0.formato  ;
015802160314             outputmsg = TRTC0TTo0.linkPTN  ;
015900140522
016400160314            // Se non ci sono errori reperisco da ARB i dati mancanti
016500160314             if didbolla>*zeros ;
016600160314                kaas= %int(%subst(didbolla:1:2));
016700160314                Kaas=kaas+2000                  ;
016800160314                klnp= %int(%subst(didbolla:3:3));
016900160314                knrs= %int(%subst(didbolla:6:2));
017000160314                knsp= %int(%subst(didbolla:8:7));
017100160314             endif  ;
018000140522
018100160314             chain (kaas:klnp:knrs:knsp) fnarb01l  ;
018200160314             if %found(fnarb01l)  ;
019300160314             endif  ;
019400160314           endif  ;
019500140521
019600160314 5       endif  ;
019700140522
019800160314         *in01=*on    ;
019900140520 4     endif  ;
020500140521
028200140522       if not *inkf  ;
028300160314          *in01=*off ;
028400140522       endif  ;
028500120306
028800160314 1     enddo  ;
028900120224
029000120224      /END-FREE
