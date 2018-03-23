000100091028     D*--------------------------------------------------
000200091028     D* Procedure name: openTabel
000300091028     D* Purpose:        OPEN TABEL00F
000400091028     D* Returns:
000500091028     D* Parameter:      rpyOpCode => Codice risposta.
000600091028     D* Parameter:      rpyEsito => Esito.
000700091028     D*--------------------------------------------------
000800091028     D openTabel00f    PR
000900091028     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
001000091028     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
001100091028
001200091028     D*--------------------------------------------------
001300091028     D* Procedure name: closeTabel
001400091028     D* Purpose:        CLOSE TABEL00F
001500091028     D* Returns:
001600091028     D* Parameter:      rpyOpCode => Codice risposta.
001700091028     D* Parameter:      rpyEsito => Esito
001800091028     D*--------------------------------------------------
001900091028     D closeTabel00f   PR
002000091028     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002100091028     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
002200091028
002300091028     D*--------------------------------------------------
002400091028     D* Procedure name: chainTabel
002500091028     D* Purpose:        CHAIN TABEL00F
002600091028     D* Returns:
002700091028     D* Parameter:      rqsOpCode => Codice operativo
002800091028     D* Parameter:      rqsKut => Lingua
002900091028     D* Parameter:      rqsCod => Codice tabella
003000091028     D* Parameter:      rqsKey => Codice elemento.
003100091028     D* Parameter:      rpyOpCode => Codice risposta.
003200091028     D* Parameter:      rpyEsito => Esito
003300091028     D* Parameter:      rpyTabel00f
003400091028     D*--------------------------------------------------
003500091028     D chainTabel00f   PR                  LIKE(rpyTabel00f)
003600091028     D  rqsOpCode                    10A   CONST
003700091028     D  rqsKut                        1P 0
003800091028     D  rqsCod                        2A   CONST
003900091028     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
004000091028     D  rqsLengthOfKey...
004100091028     D                               10I 0 CONST
004200091028     D  rqsFormat                    10A   CONST
004300091028     D  rpyOpCode                    10A
004400091028     D  rpyEsito                     10I 0
004500091028     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
004600091028     D                                     LIKE(rpyTabel00f)
004700091028
004800091028     D*--------------------------------------------------
004900091028     D* Procedure name: setTabel
005000091028     D* Purpose:        SETLL TABEL00F
005100091028     D* Returns:
005200091028     D* Parameter:      rqsOpCode => Codice operativo
005300091028     D* Parameter:      rqsKut => Lingua
005400091028     D* Parameter:      rqsCod => Codice tabella
005500091028     D* Parameter:      rqsKey => Codice elemento.
005600091028     D* Parameter:      rpyOpCode => Codice risposta.
005700091028     D* Parameter:      rpyEsito => Esito
005800091028     D*--------------------------------------------------
005900091028     D setTabel00f     PR
006000091028     D  rqsOpCode                    10A   CONST
006100091028     D  rqsKut                        1P 0
006200091028     D  rqsCod                        2A   CONST
006300091028     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
006400091028     D  rqsLengthOfKey...
006500091028     D                               10I 0 CONST
006600091028     D  rpyOpCode                    10A
006700091028     D  rpyEsito                     10I 0
006800091028
006900091028     D*--------------------------------------------------
007000091028     D* Procedure name: readTabel
007100091028     D* Purpose:        READ{E} TABEL00F
007200091028     D* Returns:
007300091028     D* Parameter:      rqsKut => Lingua
007400091028     D* Parameter:      rqsCod => Codice tabella
007500091028     D* Parameter:      rqsKey => Codice elemento.
007600091028     D* Parameter:      rpyOpCode => Codice risposta.
007700091028     D* Parameter:      rpyEsito => Esito
007800091028     D* Parameter:      rpyTabel00f
007900091028     D*--------------------------------------------------
008000091028     D readTabel00f    PR                  LIKE(rpyTabel00f)
008100091028     D  rqsOpCode                    10A   CONST
008200091028     D  rqsKut                        1P 0
008300091028     D  rqsCod                        2A   CONST
008400091028     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
008500091028     D  rqsLengthOfKey...
008600091028     D                               10I 0 CONST
008700091028     D  rqsFormat                    10A   CONST
008800091028     D  rpyOpCode                    10A
008900091028     D  rpyEsito                     10I 0
009000091028     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
009100091028     D                                     LIKE(rpyTabel00f)
009200091028
009300091028     D*--------------------------------------------------
009400091028     D* Procedure name: openSqlTabel00f
009500091028     D* Purpose:        PREPARE e OPEN cursore TABEL00F
009600091028     D* Returns:
009700091028     D* Parameter:      rqsOpCode => Codice operativo
009800091028     D* Parameter:      rqsKut => Lingua
009900091028     D* Parameter:      rqsLibrary => Libreria
010000091028     D* Parameter:      rqsCod => Codice tabella
010100091028     D* Parameter:      rqsKey => Codice elemento.
010200091028     D* Parameter:      rpyOpCode => Codice risposta.
010300091028     D* Parameter:      rpyEsito => Esito
010400091028     D*--------------------------------------------------
010500091028     D openSqlTabel00f...
010600091028     D                 PR
010700091028     D  rqsOpCode                    10A   CONST
010800091028     D  rqsKut                        1P 0
010900091028     D  rqsCod                        2A   CONST
011000091028     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
011100091028     D  rqsLengthOfKey...
011200091028     D                               10I 0 CONST
011300091028     D  rpyOpCode                    10A
011400091028     D  rpyEsito                     10I 0
011500091028     D  rqsOrderBy                  256A   OPTIONS(*NOPASS:*OMIT:*VARSIZE)
011600091028     D  rqsLengthOfOrderBy...
011700091028     D                               10I 0 OPTIONS(*NOPASS:*OMIT) CONST
011800091102     D  rqsWhere                    256A   OPTIONS(*NOPASS:*OMIT:*VARSIZE)
011900091102     D  rqsLengthOfWhere...
012000091102     D                               10I 0 OPTIONS(*NOPASS:*OMIT) CONST
012100091028
012200091028     D*--------------------------------------------------
012300091028     D* Procedure name: fetchTabel00f
012400091028     D* Purpose:        FETCH cursore TABEL00F
012500091028     D* Returns:        TABEL00F
012600091028     D* Parameter:      rqsOpCode => Codice operativo
012700091028     D* Parameter:      rpyOpCode => Codice risposta.
012800091028     D* Parameter:      rpyEsito => Esito
012900091028     D* Parameter:      rpyTabel00f
013000091028     D*--------------------------------------------------
013100091028     D fetchTabel00f   PR                  LIKE(rpyTabel00f)
013200091028     D  rqsOpCode                    10A   CONST
013300091028     D  rqsFormat                    10A   CONST
013400091028     D  rpyOpCode                    10A
013500091028     D  rpyEsito                     10I 0
013600091028     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
013700091028     D                                     LIKE(rpyTabel00f)
013800091028
013900091028     D*--------------------------------------------------
014000091028     D* Procedure name: closeSqlTabel00f
014100091028     D* Purpose:        CLOSE cursore TABEL00F
014200091028     D* Returns:
014300091028     D*--------------------------------------------------
014400091028     D closeSqlTabel00f...
014500091028     D                 PR
014600091028     D  rpyOpCode                    10A   OPTIONS(*OMIT:*NOPASS)
014700091028     D  rpyEsito                     10I 0 OPTIONS(*OMIT:*NOPASS)
014800091028
014900091028     D*--------------------------------------------------
015000091028     D* Procedure name: getTabel00fLibrary
015100091028     D* Purpose:        Restituisce la libreria di TABEL00F
015200091028     D* Returns:        Libreria
015300091028     D* Parameter:      rpyOpCode => Codice risposta.
015400091028     D* Parameter:      rpyEsito => Esito
015500091028     D* Parameter:      rpyLibrary => Libreria
015600091028     D*--------------------------------------------------
015700091028     D getTabel00fLibrary...
015800091028     D                 PR            10A
015900091028     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
016000091028     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
016100091028     D  rpyLibrary                   10A   OPTIONS(*NOPASS:*OMIT)
016200091028
016300091028     D*--------------------------------------------------
016400091028     D* Procedure name: setTabel00fLibrary
016500091028     D* Purpose:        Imposta la libreria di TABEL00F
016600091028     D* Returns:
016700091028     D* Parameter:      rqsLibrary => Libreria
016800091028     D* Parameter:      rpyOpCode => Codice risposta
016900091028     D* Parameter:      rpyEsito => Esito
017000091028     D*--------------------------------------------------
017100091028     D setTabel00fLibrary...
017200091028     D                 PR
017300091028     D  rqsLibrary                   10A   CONST
017400091028     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
017500091028     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
017600091028
