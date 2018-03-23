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
001800060413
001900060413     D*--------------------------------------------------
002000060413     D* Procedure name: chkQuadraturaGiornalieraEseguita
002100060413     D* Purpose:
002200060413     D* Returns:        Esito
002300060413     D* Parameter:      po
002400060413     D* Parameter:      dataQuadratura
002500060413     D* Parameter:      esito
002600060413     D*--------------------------------------------------
002700060413     D chkQuadraturaGiornalieraEseguita...
002800060413     D                 PR            10I 0
002900060413     D po                                  LIKE(diz$Fil)
003000060413     D dataQuadratura                10D   DATFMT(*ISO)
003100060413     D esito                         10I 0
003200060413
003300060413     P*--------------------------------------------------
003400060413     P* Procedure name: chkQuadraturaGiornalieraEseguita
003500060413     P* Purpose:
003600060413     P* Returns:        Esito
003700060413     P* Parameter:      po
003800060413     P* Parameter:      dataQuadratura
003900060413     P* Parameter:      esito
004000060413     P*--------------------------------------------------
004100060413     P chkQuadraturaGiornalieraEseguita...
004200060413     P                 B                   EXPORT
004300060413     D chkQuadraturaGiornalieraEseguita...
004400060413     D                 PI            10I 0
004500060413     D po                                  LIKE(diz$Fil)
004600060413     D dataQuadratura                10D   DATFMT(*ISO)
004700060413     D esito                         10I 0
004800060413
004900060413     D count           S             10I 0
005000060413     D qigDta          S
005100060413     D                                     LIKE(diz$Da8)
005200060413
005300060413     C/EXEC SQL
005400060413     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
005500060413     C/END-EXEC
005600060413      /FREE
005700060413       CLEAR esito;
005800060413       qigDta = (%SUBDT(dataQuadratura:*YEARS) * 10000)
005900060413       + (%SUBDT(dataQuadratura:*MONTHS) * 100) + %SUBDT(dataQuadratura:*DAYS);
006000060413      /END-FREE
006100060413     C/EXEC SQL
006200060413     C+ SELECT COUNT(*)
006300060413     C+ INTO :count
006400060413     C+ FROM YFQIG00F
006500060413     C+ WHERE QIGANN = ' ' AND QIGFLE = :po AND QIGDTA = :qigDta
006600060413     C/END-EXEC
006700060413      /FREE
006800060413       IF count > 0;
006900060413         esito = 1;
007000060413       ENDIF;
007100060413       RETURN esito;
007200060413      /END-FREE
007300060413     P chkQuadraturaGiornalieraEseguita...
007400060413     P                 E
