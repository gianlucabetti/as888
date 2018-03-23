000100060317     H NOMAIN
000200060413
000300060413      //********************************************************************************************
000400060413      //
000500060413      // Definizione strutture dati.
000600060413      //
000700060413      //********************************************************************************************
000800060413     D cndizion      E DS
000900060413     D                                     BASED(nullPtr)
001000060413     D                                     PREFIX(diz)
001100060413
001200060413      //********************************************************************************************
001300060413      //
001400060413      // Definizione variabili globali.
001500060413      //
001600060413      //********************************************************************************************
001700060413     D nullPtr         S               *
001800060414     D orgFc0          S
001900060414     D                                     LIKE(diz$Fil)
002000060413
002100060414     D*--------------------------------------------------
002200060414     D* Procedure name: getAreaAmministrativa
002300060414     D* Purpose:        Reperisce area amministrativa.
002400060414     D* Returns:
002500060414     D* Parameter:      opCode
002600060414     D* Parameter:      po
002700060414     D* Parameter:      poCtb => PO per contabilità (voce 120 organigramma)
002800060414     D* Parameter:      poArea
002900060414     D* Parameter:      rtnCode
003000060414     D* Parameter:      rtnEsito
003100060414     D*--------------------------------------------------
003200060414     D getAreaAmministrativa...
003300060414     D                 PR
003400060414     D  opCode                       10A
003500060414     D  po                                 LIKE(diz$Fil)
003600060414     D  poCtb                              LIKE(diz$Fil)
003700060414     D  poArea                             LIKE(diz$Fil)
003800060414     D  rtnCode                      10A
003900060414     D  rtnEsito                     10I 0
004000060414
004100060414     P*--------------------------------------------------
004200060414     P* Procedure name: getAreaAmministrativa
004300060414     P* Purpose:        Reperisce area amministrativa.
004400060414     P* Returns:
004500060414     P* Parameter:      opCode
004600060414     P* Parameter:      po
004700060414     P* Parameter:      poCtb => PO per contabilità (voce 120 organigramma)
004800060414     P* Parameter:      poArea
004900060414     P* Parameter:      rtnCode
005000060414     P* Parameter:      rtnEsito
005100060414     P*--------------------------------------------------
005200060414     P getAreaAmministrativa...
005300060414     P                 B                   EXPORT
005400060414     D getAreaAmministrativa...
005500060414     D                 PI
005600060414     D  opCode                       10A
005700060414     D  po                                 LIKE(diz$Fil)
005800060414     D  poCtb                              LIKE(diz$Fil)
005900060414     D  poArea                             LIKE(diz$Fil)
006000060414     D  rtnCode                      10A
006100060414     D  rtnEsito                     10I 0
006200060414
006300060414
006400060414      /FREE
006500060414
006600060414       CLEAR poCtb;
006700060414       CLEAR poArea;
006800060414       CLEAR rtnCode;
006900060414       CLEAR rtnEsito;
007000060414
007100060414       IF opCode = 'INIZIO';
007200060414      /END-FREE
007300060414     C/EXEC SQL
007400060414     C+ SELECT ORGFC0
007500060414     C+ INTO :orgFc0
007600060414     C+ FROM AZORG00F
007700060414     C+ WHERE ORGFIL = :po AND ORGFVA = ' '
007800060414     C/END-EXEC
007900060414      /FREE
008000060414         IF sqlCod < 0;
008100060414           rtnCode = 'ERRSQL';
008200060414           rtnEsito = sqlCod;
008300060414           RETURN;
008400060414         ENDIF;
008500060414         IF orgFc0 = 0;
008600060414           rtnCode = 'ERRORGFC0';
008700060414           rtnEsito = -1;
008800060414           RETURN;
008900060414         ENDIF;
009000060414      /END-FREE
009100060414     C/EXEC SQL
009200060414     C+ DECLARE AREA CURSOR FOR
009300060414     C+ SELECT ORGFC0, ORGFIL
009400060414     C+ FROM AZORG00F
009500060414     C+ WHERE ORGFC0 = :orgFc0 AND ORGFVA = ' '
009600060414     C+ ORDER BY ORGFIL
009700060414     C/END-EXEC
009800060414     C
009900060414     C/EXEC SQL
010000060414     C+ OPEN AREA
010100060414     C/END-EXEC
010200060414      /FREE
010300060414         IF sqlCod < 0;
010400060414           rtnCode = 'ERRSQL';
010500060414           rtnEsito = sqlCode;
010600060414           RETURN;
010700060414         ENDIF;
010800060414         opCode = 'CONTINUA';
010900060414       ENDIF;
011000060414
011100060414       IF opCode = 'CONTINUA';
011200060414      /END-FREE
011300060414     C/EXEC SQL
011400060414     C+ FETCH NEXT FROM AREA INTO :poCtb, :poArea
011500060414     C/END-EXEC
011600060414      /FREE
011700060414         SELECT;
011800060414           WHEN sqlCod = 100;
011900060414             rtnCode = 'EOF';
012000060414             opCode = 'FINE';
012100060414           WHEN sqlCod < 0;
012200060414             rtnCode = 'ERRSQL';
012300060414             rtnEsito = sqlCod;
012400060414             opCode = 'FINE';
012500060414         ENDSL;
012600060414       ENDIF;
012700060414
012800060414       IF opCode = 'FINE';
012900060414      /END-FREE
013000060414     C/EXEC SQL
013100060414     C+ CLOSE AREA
013200060414     C/END-EXEC
013300060414      /FREE
013400060414       ENDIF;
013500060414
013600060414       RETURN;
013700060414
013800060414      /END-FREE
013900060414     P getAreaAmministrativa...
014000060414     P                 E
