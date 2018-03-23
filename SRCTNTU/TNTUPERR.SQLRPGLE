000400030115
000500051121      /TITLE Controllo sovrapposizione periodo validità
000600030115
000700030115     D Errore          C                   '1'
000800030115     D Eseguito        C                   '0'
000900030115
000901051201     D tntuperds     E DS
001800051121     D tntua00F      E DS
001900030115     D                                     INZ
001901051124     D tntut00F      E DS
001902051124     D                                     INZ
002000030115
002100030115     C     *ENTRY        PLIST
002200030115     ** Parametri di input
002300051201     C                   PARM                    tntuperds
002900030115
003000030115     C                   EXSR      ChkPrm
003001051124     c                   select
003002051201     c                   when      §perfile = 'TNTUA00F'
003100051121     C                   EXSR      Rtvtntua
003101051201     c                   when      §perfile = 'TNTUT00F'
003102051124     C                   EXSR      Rtvtntut
003103051124     c                   endsl
003200030115     C                   EXSR      Uscita
003300030115
003400030115     ***********************************************************************
003500030115     **
003600051121     ** Reperisco tntua00F.
003700030115     **
003800030115     ***********************************************************************
003900051121     C     Rtvtntua      BEGSR
004000030115
004100030115     C/EXEC SQL
004200060117     C+ DECLARE TNTUA CURSOR FOR SELECT * FROM tntua00F WHERE tuaatt =
004300060118     C+ :§peratt                       AND rrn(tntua00f) <> :§pernrr
004301060118     C+ and tuaatb = ' ' and ((:§perDti BETWEEN tuadti AND tuadtf) or
004401060118     C+ (:§perDtf BETWEEN tuadti AND tuadtf) or (:§perdti < tuadti and
004402060118     C+ :§perdtf > tuadtf))
004500030115     C/END-EXEC
004600030115
004700030115     C/EXEC SQL
004800051121     C+ OPEN tntua
004900030115     C/END-EXEC
005000030115
005100030115     C/EXEC SQL
005200051121     C+ FETCH NEXT FROM tntua INTO :tntua00F
005300030115     C/END-EXEC
005400030115
005500030115     C                   SELECT
005600030115     C                   WHEN      SQLCod = 100
005700051201     C                   EVAL      §perEsi = Errore
005800030115     C                   WHEN      SQLCod < 0
005900030115     C                   EVAL      *INH1 = *ON
006000030115     C                   ENDSL
006100030115
006200030115     C/EXEC SQL
006300051121     C+ CLOSE tntua
006400030115     C/END-EXEC
006500030115
006600030115     C                   ENDSR
006601051124     ***********************************************************************
006602051124     **
006603051124     ** Reperisco tntut00F.
006604051124     **
006605051124     ***********************************************************************
006606051124     C     Rtvtntut      BEGSR
006607051124
006608051124     C/EXEC SQL
006609051124     C+ DECLARE TNTUT CURSOR FOR SELECT * FROM tntut00F WHERE tutatt =
006610060118     C+ :§Peratt AND tutprg = :§perprg and
006611060118     C+ tutatb = ' ' and tutfil = :§perfil and tutcli
006612060117     C+ =:§percli and rrn(tntut00f) <> :§pernrr
006613060118     C+ AND ((:§perDti BETWEEN tutdti AND tutdtf) or
006614060118     C+ (:§perDtf BETWEEN tutdti AND tutdtf) or
006615060118     C+ (:§perDti < tutdti AND :§perdtf > tutdtf))
006616051124     C/END-EXEC
006617051124
006618051124     C/EXEC SQL
006619051124     C+ OPEN tntut
006620051124     C/END-EXEC
006621051124
006622051124     C/EXEC SQL
006623051124     C+ FETCH NEXT FROM tntut INTO :tntut00F
006624051124     C/END-EXEC
006625051124
006626051124     C                   SELECT
006627051124     C                   WHEN      SQLCod = 100
006628051201     C                   EVAL      §perEsi = Errore
006629051124     C                   WHEN      SQLCod < 0
006630051124     C                   EVAL      *INH1 = *ON
006631051124     C                   ENDSL
006632051124
006633051124     C/EXEC SQL
006634051124     C+ CLOSE tntut
006635051124     C/END-EXEC
006636051124
006637051124     C                   ENDSR
006700030115
006800030115     ***********************************************************************
006900030115     **
007000030115     ** Controllo parametri ricevuti.
007100030115     **
007200030115     ***********************************************************************
007300030115     C     ChkPrm        BEGSR
007400030115
007500051201     C                   CLEAR                   §PerEsi
007600051121     C                   CLEAR                   tntua00F
007601051124     C                   CLEAR                   tntut00F
007700030115
007800051201     C                   IF        §Peratt = *BLANK
008100051201     C                   EVAL      §PerEsi = Errore
008200030115     C                   EXSR      Uscita
008300030115     C                   ENDIF
008400030115
008500030115     C                   ENDSR
008600030115
008700030115     ***********************************************************************
008800030115     **
008900030115     ** Uscita.
009000030115     **
009100030115     ***********************************************************************
009200030115     C     Uscita        BEGSR
009300030115
009400051201     C                   IF        §PerEsi = *BLANK
009500051201     C                   EVAL      §PerEsi = Eseguito
009600030115     C                   ENDIF
009700030115
009800030115     C                   RETURN
009900030115
010000030115     C                   ENDSR
