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
002300170522     D rpyDta          S                   LIKE(tis778dso)
002400160804     D rpyOpCode       S             10I 0
002500160804     D rpyIdMsg        S             10I 0
002600160804
002700140530      /copy gaitrasrc/srcprotopr,tis778R
002800140530      /copy gaitrasrc/srcconst,tis778R
002900040330
003000170523       //clear tis778dsi;
003100170522
003200170522       tis778dsi.formatoI74 = 'TIS778DSI ';
003300170522
003400170522       tis778dsi.versionI74 = 'C';
003500170523       tis778dsi.nspedizI74 = '154000066779';
003600170523       //tis778dsi.aasI74     = 2017;
003700170522       tis778dsi.langI74    = 'it';
003800160804
003900160804       MONITOR;
004000160804         TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
004100160804                        : this.rpyOpCode
004200160804                        : this.rpyIdMsg
004300160804                        : TIS778DSI.formatoi74
004400160804                        : TIS778DSI
004500160804                        : %SIZE(TIS778DSI)
004600160804                        : TIS778DSO.formatoo74
004700160804                        : TIS778DSO
004800160804                        : %SIZE(TIS778DSO)
004900160804                        : TIS778DSM.formato
005000160804                        : TIS778DSM
005100160804                        : %SIZE(TIS778DSM)
005200160804                        );
005300160804         ON-ERROR;
005400160804           this.rpyOpCode = TIS778_RPYOPCODE_ERROR;
005500160804       ENDMON;
005600160804
005700170522       rpyOpCode = this.rpyOpCode;
005800170522       rpyIdMsg = this.rpyIdMsg;
005900170522       rpyDta = tis778dso;
006000160804
006100170523       RETURN;
