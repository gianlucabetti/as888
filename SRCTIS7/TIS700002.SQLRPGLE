000100091026     H DFTACTGRP(*NO)
000200091026
000300091026     ***************************************************************************
000400091026     **
000500091026     ** Procedure.
000600091026     **
000700091026     ***************************************************************************
000800091026     D/COPY GAITRASRC/SRCPROTOPR,TISI96R
000900091026
001000091026     ***************************************************************************
001100091026     **
001200091026     ** Strutture dati.
001300091026     **
001400091026     ***************************************************************************
001500091026     D cap             DS                  QUALIFIED
001600091026     D                                     DIM(999)
001700091026     D  cap                           9A   VARYING
001800091026     D cndizion      E DS                  QUALIFIED
001900091026     D                                     BASED(nullPtr)
002000091026     D getCapLocI      DS                  QUALIFIED
002100091026     D  siglaProvincia...
002200091026     D                                2A
002300091026     D  localita...
002400091026     D                               35A
002500091026     D  separatore...
002600091026     D                                1A
002700091026     D getCapLocO      DS                  QUALIFIED
002800091026     D                                     INZ
002900091026     D  count                         3S 0
003000091026     D                                1A   INZ('|')
003100091026     D  capLista                   9995A
003200091026
003300091026     ***************************************************************************
003400091026     **
003500091026     ** Campi.
003600091026     **
003700091026     ***************************************************************************
003800091026     D i               S             10I 0
003900091104     D capLista        S           9995A   VARYING
004000091026     D RQSOPC          S             10                                         * REQUEST OPCODE
004100091026     D RQSDTA          S           9999                                         * REQUEST DATA
004200091026     D RQSLEN          S              4P 0                                      * REQUEST DATA LENGT
004300091026     D RPYOPC          S             10                                         * REPLY OPCODE
004400091026     D RPYDTA          S           9999                                         * REPLY DATA
004500091026     D RPYLEN          S              4P 0                                      * REPLY DATA LENGTH
004600091026     D versioneCappario...
004700091026     D                 S                   LIKE(cndizion.$Ver)
004800091026
004900091026     ***************************************************************************
005000091026     **
005100091026     ** Parametri.
005200091026     **
005300091026     ***************************************************************************
005400091026     C     *ENTRY        PLIST
005500091026     C                   PARM                    RQSOPC
005600091026     C                   PARM                    RQSDTA
005700091026     C                   PARM                    RQSLEN
005800091026     C                   PARM                    RPYOPC
005900091026     C                   PARM                    RPYDTA
006000091026     C                   PARM                    RPYLEN
006100091026
006200091026     ***************************************************************************
006300091026     **
006400091026     ** Main.
006500091026     **
006600091026     ***************************************************************************
006700091026      /FREE
006800091026
006900091026       RESET rpyOpc;
007000091026       RESET rpyDta;
007100091026       RESET rpyLen;
007200091026
007300091026       getCapLocI = rqsDta;
007400091104
007500091104       EXEC SQL
007600091104        DECLARE CAP NO SCROLL CURSOR FOR
007700091104          WITH AZCPL00F_WITH (CPLVER, CPLNAR, CPLCAP)
007800091104          AS ( SELECT AZCPL00F.CPLVER, AZCPL00F.CPLNAR, AZCPL00F.CPLCAP
007900091104               FROM AZCPL00F
008000091104               WHERE AZCPL00F.CPLVER = :versioneCappario
008100091104                 AND AZCPL00F.CPLNAR = ''
008200091104                 AND AZCPL00F.CPLLOC = :getCapLocI.localita
008300091104                 AND AZCPL00F.CPLFI1 = ''
008400091104             )
008500091104          SELECT RTRIM(AZCPL00F_WITH.CPLCAP)
008600091104          FROM AZCPL00F_WITH
008700091104          JOIN AZCPC00F
008800091104            ON AZCPC00F.CPCVER = AZCPL00F_WITH.CPLVER
008900091104           AND AZCPC00F.CPCNAR = AZCPL00F_WITH.CPLNAR
009000091104           AND AZCPC00F.CPCPRV = :getCapLocI.siglaProvincia
009100091104           AND AZCPC00F.CPCCAP = AZCPL00F_WITH.CPLCAP
009200091104          WHERE AZCPC00F.CPCFI1 = ''
009300091104          ORDER BY AZCPL00F_WITH.CPLCAP
009400091104          FETCH FIRST 999 ROWS ONLY
009500091104          FOR READ ONLY
009600091104       ;
009700091026
009800091026       EXEC SQL
009900091026        OPEN CAP
010000091026       ;
010100091026
010200091026       SELECT;
010300091026         WHEN sqlState > '03';
010400091026           DUMP(A);
010500091029           rpyOpc = 'ERROR';
010600091026           RETURN;
010700091026       ENDSL;
010800091026
010900091026       RESET getCapLocO;
011000091026       RESET capLista;
011100091026
011200091026       EXEC SQL
011300091026        FETCH NEXT FROM CAP
011400091026          FOR 999 ROWS
011500091026          INTO :cap
011600091026       ;
011700091026
011800091026       SELECT;
011900091026         WHEN sqlState = '02000';
012000091026           rpyOpc = 'DONE';
012100091026           rpyLen = %SIZE(getCapLocO);
012200091026         WHEN sqlState > '03';
012300091026           DUMP(A);
012400091029           rpyOpc = 'ERROR';
012500091026         OTHER;
012600091026           getCapLocO.count = sqlErrD(3);
012700091105           capLista = '[';
012800091026           FOR i = 1 TO sqlErrD(3);
012900091105             IF i > 1;
013000091105               capLista += getCapLocI.separatore;
013100091105             ENDIF;
013200091105             capLista += '''' + cap(i).cap + '''';
013300091026           ENDFOR;
013400091105           capLista += ']';
013500091026           rpyOpc = 'DONE';
013600091026           getCapLocO.capLista = capLista;
013700091026           rpyDta = getCapLocO;
013800091026           rpyLen = %SIZE(getCapLocO);
013900091026       ENDSL;
014000091026
014100091026       EXEC SQL
014200091026        CLOSE CAP
014300091026       ;
014400091026
014500091026       RETURN;
014600091026
014700091026       //***********************************************************************
014800091026       //
014900091026       // Inizializzazioni.
015000091026       //
015100091026       //***********************************************************************
015200091026
015300091026       BEGSR *INZSR;
015400091026
015500091026         GetVersioneCappario(%DEC(%DATE() : *ISO) : versioneCappario);
015600091026
015700091026       ENDSR;
015800091026
015900091026      /END-FREE
