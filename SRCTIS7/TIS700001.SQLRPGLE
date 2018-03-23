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
001500091026     D cndizion      E DS                  QUALIFIED
001600091026     D                                     BASED(nullPtr)
001700091026     D getLocPrvI      DS                  QUALIFIED
001800091026     D  siglaProvincia...
001900091026     D                                2A
002000091026     D  separatore...
002100091026     D                                1A
002200091026     D getLocPrvO      DS                  QUALIFIED
002300091026     D                                     INZ
002400091026     D  count                         3S 0
002500091026     D                                1A   INZ('|')
002600091026     D  localitaLista              9995A
002700091026     D localita        DS                  QUALIFIED
002800091026     D                                     DIM(999)
002900091026     D  localita                     35A   VARYING
003000091026
003100091026     ***************************************************************************
003200091026     **
003300091026     ** Campi.
003400091026     **
003500091026     ***************************************************************************
003600091026     D i               S             10I 0
003700091104     D localitaLista   S           9995A   VARYING
003800091026     D RQSOPC          S             10                                         * REQUEST OPCODE
003900091026     D RQSDTA          S           9999                                         * REQUEST DATA
004000091026     D RQSLEN          S              4P 0                                      * REQUEST DATA LENGT
004100091026     D RPYOPC          S             10                                         * REPLY OPCODE
004200091026     D RPYDTA          S           9999                                         * REPLY DATA
004300091026     D RPYLEN          S              4P 0                                      * REPLY DATA LENGTH
004400091026     D versioneCappario...
004500091026     D                 S                   LIKE(cndizion.$Ver)
004600091026
004700091026     ***************************************************************************
004800091026     **
004900091026     ** Parametri.
005000091026     **
005100091026     ***************************************************************************
005200091026     C     *ENTRY        PLIST
005300091026     C                   PARM                    RQSOPC
005400091026     C                   PARM                    RQSDTA
005500091026     C                   PARM                    RQSLEN
005600091026     C                   PARM                    RPYOPC
005700091026     C                   PARM                    RPYDTA
005800091026     C                   PARM                    RPYLEN
005900091026
006000091026     ***************************************************************************
006100091026     **
006200091026     ** Main.
006300091026     **
006400091026     ***************************************************************************
006500091026      /FREE
006600091026
006700091026       RESET rpyOpc;
006800091026       RESET rpyDta;
006900091026       RESET rpyLen;
007000091026
007100091026       getLocPrvI = rqsDta;
007200091104
007300091104       EXEC SQL
007400091104        DECLARE LOCALITA NO SCROLL CURSOR FOR
007500091104          WITH AZCPC00F_WITH (CPCVER, CPCNAR, CPCCAP)
007600091104          AS ( SELECT AZCPC00F.CPCVER, AZCPC00F.CPCNAR, AZCPC00F.CPCCAP
007700091104               FROM AZCPC00F
007800091104               WHERE AZCPC00F.CPCVER = :versioneCappario
007900091104                 AND AZCPC00F.CPCNAR = ''
008000091104                 AND AZCPC00F.CPCPRV = :getLocPrvI.siglaProvincia
008100091104                 AND AZCPC00F.CPCFI1 = ''
008200091104          )
008300091104          SELECT RTRIM(AZCPL00F.CPLLOC)
008400091104          FROM AZCPC00F_WITH
008500091104          JOIN AZCPL00F
008600091104            ON AZCPC00F_WITH.CPCVER = AZCPL00F.CPLVER
008700091104           AND AZCPC00F_WITH.CPCNAR = AZCPL00F.CPLNAR
008800091104           AND AZCPC00F_WITH.CPCCAP = AZCPL00F.CPLCAP
008900091104          WHERE AZCPL00F.CPLFI1 = ''
009000091104          GROUP BY AZCPL00F.CPLLOC
009100091104          ORDER BY AZCPL00F.CPLLOC
009200091104          FETCH FIRST 999 ROWS ONLY
009300091104       ;
009400091026
009500091026       EXEC SQL
009600091026        OPEN LOCALITA
009700091026       ;
009800091026
009900091026       SELECT;
010000091026         WHEN sqlState > '03';
010100091026           DUMP(A);
010200091029           rpyOpc = 'ERROR';
010300091026           RETURN;
010400091026       ENDSL;
010500091026
010600091026       RESET getLocPrvO;
010700091026       RESET localitaLista;
010800091026
010900091026       EXEC SQL
011000091026        FETCH NEXT FROM LOCALITA
011100091026          FOR 999 ROWS
011200091026          INTO :localita
011300091026       ;
011400091026
011500091026       SELECT;
011600091026         WHEN sqlState = '02000';
011700091026           rpyOpc = 'DONE';
011800091026           rpyLen = %SIZE(getLocPrvO);
011900091026         WHEN sqlState > '03';
012000091026           DUMP(A);
012100091029           rpyOpc = 'ERROR';
012200091026         OTHER;
012300091105           localitaLista = '[';
012400091026           FOR i = 1 TO sqlErrD(3);
012500091105             IF i > 1;
012600091105               localitaLista += getLocPrvI.separatore;
012700091105             ENDIF;
012800091105             localitaLista += '''' + localita(i).localita + '''';
012900091026           ENDFOR;
013000091105           localitaLista += ']';
013100091105           getLocPrvO.count = sqlErrD(3);
013200091026           rpyOpc = 'DONE';
013300091026           getLocPrvO.localitaLista = localitaLista;
013400091026           rpyDta = getLocPrvO;
013500091026           rpyLen = %SIZE(getLocPrvO);
013600091026       ENDSL;
013700091026
013800091026       EXEC SQL
013900091026        CLOSE LOCALITA
014000091026       ;
014100091026
014200091026       RETURN;
014300091026
014400091026       //***********************************************************************
014500091026       //
014600091026       // Inizializzazioni.
014700091026       //
014800091026       //***********************************************************************
014900091026
015000091026       BEGSR *INZSR;
015100091026
015200091026         GetVersioneCappario(%DEC(%DATE() : *ISO) : versioneCappario);
015300091026
015400091026       ENDSR;
015500091026
015600091026      /END-FREE
