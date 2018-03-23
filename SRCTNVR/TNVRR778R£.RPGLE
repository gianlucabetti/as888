000100140818     Fwftis77f  o  a e             disk    usropn
000200140814     F
000300040330     D NSpediz         S             12
000400040330     D esito           S              1
000500140814     D tis778dsi     E DS                  qualified INZ(*EXTDFT)
000600140814     D tis778dso     E DS                  INZ(*EXTDFT)
000700140530     D tis778dsM     E DS                  QUALIFIED INZ(*EXTDFT)
000800140530     D this            DS                  QUALIFIED INZ
000900140530     D  rpyOpCode...
001000140530     D                               10I 0
001100140530     D  rpyIdMsg...
001200140530     D                               10I 0
001300140530     D  e...
001400140530     D                                3U 0
001500140530     D  m...
001600140530     D                                3U 0
001700140530     D  idCampo...
001800140530     D                               10A
001900140530     D  idMsg...
002000140530     D                                7A
002100140530     D  textMsg...
002200140530     D                              255A
002300140530     D  errWarn...
002400140530     D                                1A
002500140818     d
002600140818      * - Campi per esecuzione comandi
002700140818     d Qcmd            s            150    inz
002800140818     d Qlen            s             15  5 inz(150)
002900140818      *
003000140530      /copy gaitrasrc/srcprotopr,tis778R
003100140530      /copy gaitrasrc/srcconst,tis778R
003200040330
003300040330     C     *ENTRY        PLIST
003400140530     C                   PARM                    NSPEDIZ
003500140818      *
003600140818     c                   eval      Qcmd ='CHKOBJ OBJ(GAITRAGRPS) OBJTYPE(*LIB)'
003700140818
003800140818     c                   call      'QCMDEXC'                            90
003900140818     c                   parm                    Qcmd
004000140818     c                   parm                    Qlen
004100140818      *
004200140818     C                   if        not *in90
004300140818     c                   eval      Qcmd ='ADDLIBLE FILTRAPRD *LAST   '
004400140818     c                   call      'QCMDEXC'                            90
004500140818     c                   parm                    Qcmd
004600140818     c                   parm                    Qlen
004700140818
004800140818     c                   else
004900140818
005000140818     c                   eval      Qcmd ='ADDLIBLE FILTRA201  *LAST   '
005100140818     c                   call      'QCMDEXC'                            90
005200140818     c                   parm                    Qcmd
005300140818     c                   parm                    Qlen
005400140818
005500140818     c                   endif
005600140818
005700140818     c                   eval      Qcmd ='ADDLIBLE STRATEDEV         '
005800140818     c                   call      'QCMDEXC'                            90
005900140818     c                   parm                    Qcmd
006000140818     c                   parm                    Qlen
006100140818     c
006200140818     c                   open      wftis77f
006300040330
006400140530      /free
006500140818
006600140818
006700140814       tis778dsi.nspedizI74 = nspediz;
006800140530        MONITOR;
006900140530         TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
007000140530                        : this.rpyOpCode
007100140530                        : this.rpyIdMsg
007200140603                        : TIS778DSI.formatoi74
007300140530                        : TIS778DSI
007400140530                        : %SIZE(TIS778DSI)
007500140814                        : formatoo74
007600140530                        : TIS778DSO
007700140530                        : %SIZE(TIS778DSO)
007800140530                        : TIS778DSM.formato
007900140530                        : TIS778DSM
008000140530                        : %SIZE(TIS778DSM)
008100140530                        );
008200140530         ON-ERROR;
008300140530           this.rpyOpCode = TIS778_RPYOPCODE_ERROR;
008400140818         ENDMON;
008500140814           if rifero74>0  ;
008600140814            wfpgm='78' ;
008700140814           portoo74_1=portoo74 ;
008800140818           rifero74n =rifero74 ;
008900140818           iaso74_n  =iaso74   ;
009000140814           DATO74_10=DATO74;
009100140814           ORAO74_10=ORAO74;
009200140814           OPEO74_10=OPEO74;
009300140814           EVEO74_10=EVEO74;
009400140814           NOTAO74_10=NOTAO74;
009500140814           write wftis7700  ;
009600140814           endif ;
009700140814
009800140530      /end-free
009900140814     c                   call      'TNVRX74R'
010000140814     c                   parm                    nspediz
010100140814     c
010200140530     C                   EVAL      *INLR = *ON
