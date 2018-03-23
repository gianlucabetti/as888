000100091103     H DFTACTGRP(*NO) BNDDIR('TIBS')
000200091103
000300091103     D/COPY GAITRASRC/SRCPROTOPR,TIBS0700R
000400091103     D rpyTabel00f   E DS                  EXTNAME(tabel00f)
000500091103     D rpyOpCode       S             10A
000600091103     D rpyEsito        S             10I 0
000700091103     D rqsOrderBy      S            256A   INZ('SUBSTR(TBLUNI, 1, 25)')
000800091103     D rqsWhere        S            256A   INZ('SUBSTR(TBLUNI, 57, 1) = ''E''')
000900091103     D prmKut          S                   LIKE(tblKut)
001000091103     D prmCod          S                   LIKE(tblCod)
001100091103
001200091103     C     *ENTRY        PLIST
001300091103     C                   PARM                    prmKut
001400091103     C                   PARM                    prmCod
001500091103
001600091103      /FREE
001700091103
001800091103       *INLR = *ON;
001900091103
002000091103       openSqlTabel00f( 'SQLOPEN2'
002100091103                      : prmKut
002200091103                      : prmCod
002300091103                      : *BLANK
002400091103                      : 0
002500091103                      : rpyOpCode
002600091103                      : rpyEsito
002700091103                      : rqsOrderBy
002800091103                      : %SIZE(rqsOrderBy)
002900091103                      : rqswHERE
003000091103                      : %SIZE(rqsWhere)
003100091103                      );
003200091103
003300091103       IF rpyEsito < 0;
003400091103         RETURN;
003500091103       ENDIF;
003600091103
003700091103       DOU rpyEsito = 100;
003800091103
003900091103         fetchTabel00f( 'FETCHNEXT'
004000091103                      : 'TABEL'
004100091103                      : rpyOpCode
004200091103                      : rpyEsito
004300091103                      : rpyTabel00f
004400091103                      );
004500091103
004600091103         IF rpyEsito = 100 OR rpyEsito < 0;
004700091103           LEAVE;
004800091103         ENDIF;
004900091103
005000091103       ENDDO;
005100091103
005200091103       closeSqlTabel00f();
005300091103
005400091103      /END-FREE
