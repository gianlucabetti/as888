000100170310     H DFTACTGRP(*NO) ACTGRP(*NEW)
000200140818     Fwftis77f  o  a e             disk    usropn
000300140814     F
000400170316     D brtcode         S             19
000500040330     D esito           S              1
000600140814     D tis778dsi     E DS                  qualified INZ(*EXTDFT)
000700140814     D tis778dso     E DS                  INZ(*EXTDFT)
000800140530     D tis778dsM     E DS                  QUALIFIED INZ(*EXTDFT)
000900140530     D this            DS                  QUALIFIED INZ
001000140530     D  rpyOpCode...
001100140530     D                               10I 0
001200140530     D  rpyIdMsg...
001300140530     D                               10I 0
001400140530     D  e...
001500140530     D                                3U 0
001600140530     D  m...
001700140530     D                                3U 0
001800140530     D  idCampo...
001900140530     D                               10A
002000140530     D  idMsg...
002100140530     D                                7A
002200140530     D  textMsg...
002300140530     D                              255A
002400140530     D  errWarn...
002500140530     D                                1A
002600140818     d
002700140818      * - Campi per esecuzione comandi
002800140818     d Qcmd            s            150    inz
002900140818     d Qlen            s             15  5 inz(150)
003000140818      *
003100140530      /copy gaitrasrc/srcprotopr,tis778R
003200140530      /copy gaitrasrc/srcconst,tis778R
003300040330
003400040330     C     *ENTRY        PLIST
003500170316     C                   PARM                    brtcode
003600140818      *
003700140818     c                   eval      Qcmd ='CHKOBJ OBJ(GAITRAGRPS) OBJTYPE(*LIB)'
003800140818
003900140818     c                   call      'QCMDEXC'                            90
004000140818     c                   parm                    Qcmd
004100140818     c                   parm                    Qlen
004200140818      *
004300140818     C                   if        not *in90
004400140818     c                   eval      Qcmd ='ADDLIBLE FILTRAPRD *LAST   '
004500140818     c                   call      'QCMDEXC'                            90
004600140818     c                   parm                    Qcmd
004700140818     c                   parm                    Qlen
004800140818
004900140818     c                   else
005000140818
005100140818     c                   eval      Qcmd ='ADDLIBLE FILTRA201  *LAST   '
005200140818     c                   call      'QCMDEXC'                            90
005300140818     c                   parm                    Qcmd
005400140818     c                   parm                    Qlen
005500140818
005600140818     c                   endif
005700140818
005800140818     c                   eval      Qcmd ='ADDLIBLE STRATEDEV         '
005900140818     c                   call      'QCMDEXC'                            90
006000140818     c                   parm                    Qcmd
006100140818     c                   parm                    Qlen
006200140818     c
006300140818     c                   open      wftis77f
006400040330
006500140530      /free
006600140818
006700140818
006800170316       reset tis778dsi  ;
006900151216          %subst(tis778dsi:32:9)='         '  ;
007000170316       if %subst(brtcode:13:1)<>' ' ;
007100170316       tis778dsi.nspedizI74 = %subst(brtcode:3:12) ;
007200170316       tis778dsi.brtcodeI74 = brtcode;
007300170316       else  ;
007400170316       tis778dsi.nspedizI74 = %subst(brtcode:1:12) ;
007500170316       endif ;
007600170310         // iis778dsi.kscI74 = '00481604';
007700140530        MONITOR;
007800140530         TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
007900140530                        : this.rpyOpCode
008000140530                        : this.rpyIdMsg
008100140603                        : TIS778DSI.formatoi74
008200140530                        : TIS778DSI
008300140530                        : %SIZE(TIS778DSI)
008400140814                        : formatoo74
008500140530                        : TIS778DSO
008600140530                        : %SIZE(TIS778DSO)
008700140530                        : TIS778DSM.formato
008800140530                        : TIS778DSM
008900140530                        : %SIZE(TIS778DSM)
009000140530                        );
009100140530         ON-ERROR;
009200140530           this.rpyOpCode = TIS778_RPYOPCODE_ERROR;
009300140818         ENDMON;
009400160817      //     if rifero74>0  ;
009500140814            wfpgm='78' ;
009600140814           portoo74_1=portoo74 ;
009700140818           rifero74n =rifero74 ;
009800140818           iaso74_n  =iaso74   ;
009900140814           DATO74_10=DATO74;
010000140814           ORAO74_10=ORAO74;
010100140814           OPEO74_10=OPEO74;
010200140814           EVEO74_10=EVEO74;
010300140814           NOTAO74_10=NOTAO74;
010400140814           write wftis7700  ;
010500160817      //     endif ;
010600140814
010700140814     c
010800140530     C                   EVAL      *INLR = *ON
