000100091126     H DFTACTGRP(*NO) BNDDIR('TIBS') ACTGRP(*CALLER)
000200091027
000300091027     ***************************************************************************
000400091027     **
000500091027     ** Procedure.
000600091027     **
000700091027     ***************************************************************************
000800170720     D/COPY GAITRASRC/SRCPROTOPR,TIS7000031
000900170720     D/COPY GAITRASRC/SRCPROTOPI,TIS7000031
001000091026
001100091026
001200091026     ***************************************************************************
001300091026     **
001400091026     ** Campi.
001500091026     **
001600091026     ***************************************************************************
001700091026     D RQSOPC          S             10A                                        * REQUEST OPCODE
001800091026     D RQSDTA          S           9999A                                        * REQUEST DATA
001900091026     D RQSLEN          S              4P 0                                      * REQUEST DATA LENGT
002000091026     D RPYOPC          S             10A                                        * REPLY OPCODE
002100091026     D RPYDTA          S           9999A                                        * REPLY DATA
002200091026     D RPYLEN          S              4P 0                                      * REPLY DATA LENGTH
002300091026
002400091026     ***************************************************************************
002500091026     **
002600091026     ** Parametri.
002700091026     **
002800091026     ***************************************************************************
002900091026     C     *ENTRY        PLIST
003000091026     C                   PARM                    RQSOPC
003100091026     C                   PARM                    RQSDTA
003200091026     C                   PARM                    RQSLEN
003300091026     C                   PARM                    RPYOPC
003400091026     C                   PARM                    RPYDTA
003500091026     C                   PARM                    RPYLEN
003600091026
003700091026     ***************************************************************************
003800091026     **
003900091026     ** Main.
004000091026     **
004100091026     ***************************************************************************
004200091026      /FREE
004300091026
004400091026       RESET rpyOpc;
004500091026       RESET rpyDta;
004600091026       RESET rpyLen;
004700091026       RESET getTempiO;
004800170720       RESET getTempiO2;
004900091026       RESET errori;
005000170720       RESET esito;
005100091026
005200170720       DataRif   = *zeros;
005300091026       getTempiI = rqsDta;
005400091030
005500170720       TIS7000031(DataRif
005600170720                 :getTempiI
005700170720                 :getTempiO
005800170720                 :getTempiO2
005900170720                 :errori
006000170720                 :esito);
006100170720
006200170720       // Verifico esito elaborazione
006300170720       select;
006400170720
006500170720         // OK
006600170720         when esito = 1;
006700170720            rpyOpc = 'DONE';
006800170720            rpyDta = getTempiO;
006900170720            rpyLen = %SIZE(getTempiO);
007000170720
007100170720         // Altrimenti
007200170720         other;
007300170720            rpyOpc = 'ERROR';
007400170720            rpyDta = errori;
007500170720            rpyLen = %LEN(errori);
007600170720
007700170720       endsl;
007800091026
007900091026       RETURN;
008000091026
008100091030
008200091030
008300091026       //***********************************************************************
008400091026       //
008500091026       // Inizializzazioni.
008600091026       //
008700091026       //***********************************************************************
008800091026
008900091026       BEGSR *INZSR;
009000091026
009100091026       ENDSR;
009200170720
009300091026
009400091026      /END-FREE
