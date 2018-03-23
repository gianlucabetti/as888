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
002202160804     D rpyOpCode       S             10I 0
002204160804     D rpyIdMsg        S             10I 0
002205160804
002300140530      /copy gaitrasrc/srcprotopr,tis778R
002400140530      /copy gaitrasrc/srcconst,tis778R
002500040330
002600040330     C     *ENTRY        PLIST
002700140530     C                   PARM                    NSPEDIZ
002701160804     C                   PARM                    rpyOpCode
002702160804     C                   PARM                    rpyIdMsg
002703160804     C                   PARM                    rpyData        7227
002800040330
003000160804       tis778dsi.nspedizI74 = nspediz;
003001160804
003100160804       MONITOR;
003200160804         TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
003300160804                        : this.rpyOpCode
003400160804                        : this.rpyIdMsg
003500160804                        : TIS778DSI.formatoi74
003600160804                        : TIS778DSI
003700160804                        : %SIZE(TIS778DSI)
003800160804                        : TIS778DSO.formatoo74
003900160804                        : TIS778DSO
004000160804                        : %SIZE(TIS778DSO)
004100160804                        : TIS778DSM.formato
004200160804                        : TIS778DSM
004300160804                        : %SIZE(TIS778DSM)
004400160804                        );
004500160804         ON-ERROR;
004600160804           this.rpyOpCode = TIS778_RPYOPCODE_ERROR;
004700160804       ENDMON;
004701160804
004702160804       IF %PARMS() > 1;
004703160804         rpyOpCode = this.rpyOpCode;
004704160804         rpyIdMsg = this.rpyIdMsg;
004705160804         rpyData = tis778dso;
004706160804       ENDIF;
004707160804
004900160804       *INLR = *ON;
