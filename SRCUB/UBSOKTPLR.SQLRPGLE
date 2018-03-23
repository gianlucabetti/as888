000100151125     H Option(*SrcStmt)
000200160324     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300151126
000400151125     **-----------------------------------------------------------------------**
000500160324     ** TRANSPALLET PESATORE - LOGGING MESSAGGI RICEVUTI
000600151126     **-----------------------------------------------------------------------**
000700160324
000800160324
000900160324     D*-----------
001000160324     D* VARIABILI D WRK
001100160324     D*-----------
001200160324     D pApplType       S             20A
001300160324     D pServerAddr     S             15A
001400160324     D pClientAddr     S             15A
001500160324     D pSktType        S              3A
001600160324     D pSktPort        S              5U 0
001700160324     D pFlgCtrl        S              1N
001800160324     D pMessage        S           1024A
001900160324     D pResponse       S           1024A
002000160323
002100160324     D wSktPort        S              5A
002200160324     D wFlgCtrl        S              1A
002300151012
002400160324
002500160324     C*
002600160324     C     *ENTRY        PLIST
002700160324     C                   PARM                    pApplType
002800160324     C                   PARM                    pServerAddr
002900160324     C                   PARM                    pClientAddr
003000160324     C                   PARM                    pSktType
003100160324     C                   PARM                    pSktPort
003200160324     C                   PARM                    pFlgCtrl
003300160324     C                   PARM                    pMessage
003400160324     C                   PARM                    pResponse
003500160324     C*
003600160324     C* Monitoro tutto
003700160324     C                   MONITOR
003800160324     C*
003900160324     C                   EVAL      wSktPort = %editc(pSktPort:'X')
004000160324     C                   EVAL      wFlgCtrl = pFlgCtrl
004100160324     C*
004200160324     C/EXEC SQL
004300160324     C+ INSERT INTO WFUBSOKTF (
004400160324     C+ SOKTAPPTAG,
004500160324     C+ SOKTSRVIP,
004600160324     C+ SOKTCLTIP,
004700160324     C+ SOKTSOKTYP,
004800160324     C+ SOKTSOKPRT,
004900160324     C+ SOKTESITO,
005000160324     C+ SOKTCNTMSG,
005100160324     C+ SOKTRESP)
005200160324     C+ VALUES (
005300160324     C+ :pApplType,
005400160324     C+ :pServerAddr,
005500160324     C+ :pClientAddr,
005600160324     C+ :pSktType,
005700160324     C+ :wSktPort,
005800160324     C+ :wFlgCtrl,
005900160324     C+ trim(:pMessage),
006000160324     C+ trim(:pResponse)
006100160324     C+ )
006200160324     C/END-EXEC
006300160324     C
006400160324     C                   ON-ERROR
006500160324     C
006600160324     C                   ENDMON
006700160324     C*
006800160324     C                   RETURN
006900160324     C**
007000160324
