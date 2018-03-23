000100040330     D NSpediz         S             12
000200040330     D esito           S              1
000300140530     D tis778dsi     E DS                  QUALIFIED INZ(*EXTDFT)
000400140530     D tis778dso     E DS                  QUALIFIED INZ(*EXTDFT)
000500140530     D tis778dsM     E DS                  QUALIFIED INZ(*EXTDFT)
000600140530     D this            DS                  QUALIFIED INZ
000700140530     D  rpyOpCode...
000800140530     D                               10I 0
000900140530     D  rpyIdMsg...
001000140530     D                               10I 0
001100140530     D  e...
001200140530     D                                3U 0
001300140530     D  m...
001400140530     D                                3U 0
001500140530     D  idCampo...
001600140530     D                               10A
001700140530     D  idMsg...
001800140530     D                                7A
001900140530     D  textMsg...
002000140530     D                              255A
002100140530     D  errWarn...
002200140530     D                                1A
002300140530      /copy gaitrasrc/srcprotopr,tis778R
002400140530      /copy gaitrasrc/srcconst,tis778R
002500040330
002600040330     C     *ENTRY        PLIST
002700140530     C                   PARM                    NSPEDIZ
002800040330
002900140530      /free
003000140530       tis778dsi.nspedizI74 = nspediz;
003100140530        MONITOR;
003200140530         TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
003300140530                        : this.rpyOpCode
003400140530                        : this.rpyIdMsg
003500140603                        : TIS778DSI.formatoi74
003600140530                        : TIS778DSI
003700140530                        : %SIZE(TIS778DSI)
003800140603                        : TIS778DSO.formatoo74
003900140530                        : TIS778DSO
004000140530                        : %SIZE(TIS778DSO)
004100140530                        : TIS778DSM.formato
004200140530                        : TIS778DSM
004300140530                        : %SIZE(TIS778DSM)
004400140530                        );
004500140530         ON-ERROR;
004600140530           this.rpyOpCode = TIS778_RPYOPCODE_ERROR;
004700140530         ENDMON;
004800140530      /end-free
004900140530     C                   EVAL      *INLR = *ON
