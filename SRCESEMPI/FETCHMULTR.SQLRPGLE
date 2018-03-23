000100020703      /TITLE Esempio scrittura subfile a pagine con FETCH multipla.
000200020703
000300020703     FFetchMultDCF   E             WORKSTN
000400020703     F                                     SFILE(S01:S01RRN)
000500020703     F                                     INDDS(IndAra)
000600020703
000700020703     D IndAra          DS
000800020703     D  SflDsp                 1      1N
000900020703     D  SflDspCtl              2      2N
001000020703     D  SflClr                 3      3N
001100020703     D  SflEnd                 4      4N
001200020703     D  RollUp                 5      5N
001300020703
001400020703     D QADBIFld        DS
001500020703     D                                     OCCURS(20)
001600020703     D                                     INZ
001700020703     D  DBILib                       10
001800020703     D  DBIFil                       10
001900020703     D  DBIFmP                        2B 0
002000020703     D  DBIFmt                       10
002100020703     D  DBIPos                        4B 0
002200020703     D  DBIFld                       10
002300020808     D  DBIILn                        9B 0
002400020808     D  DBIITp                        1
002500020808     D  DBITyp                        8
002600020703     D  DBITxt                       50
002700020703
002800020703     D Rows            S              5I 0
002900020703     D                                     INZ(%ELEM(QADBIFld))
003000020702     D I               S              5I 0
003100020702
003200020703     C/EXEC SQL
003300020703     C+ DECLARE QADBIFLD CURSOR FOR SELECT DBILIB, DBIFIL, DBIFMP,
003400020808     C+ DBIFMT, DBIPOS, DBIFLD, DBIILN, DBIITP, DBITYP, IFNULL(DBITXT,
003500020808     C+ ' ') FROM QADBIFLD WHERE DBILIB = 'QSYS' ORDER BY DBILIB, DBIFIL,
003600020808     C+ DBIFMP, DBIFMT, DBIPOS FOR READ ONLY OPTIMIZE FOR 20 ROWS
003700020702     C/END-EXEC
003800020702
003900020703     C/EXEC SQL
004000020703     C+ OPEN QADBIFLD
004100020702     C/END-EXEC
004200020702
004300020703     C                   DOU       SflEnd
004400020702
004500020703     C/EXEC SQL
004600020703     C+ FETCH NEXT FROM QADBIFLD FOR :Rows ROWS INTO :QADBIFld
004700020702     C/END-EXEC
004800020702
004900020702     C                   SELECT
005000020703     C                   WHEN      SQLCod = 100
005100020703     C                   EVAL      SflEnd = *ON
005200020703     C                   WHEN      SQLCod < 0
005300020702     C                   EVAL      *INH1 = *ON
005400020703     C                   EVAL      SflEnd = *ON
005500020702     C                   OTHER
005600020703     C                   EXSR      WrtS01
005700020702     C                   ENDSL
005800020703
005900020808     ** Emissione subfile.
006000020703     C                   EXSR      FmtC01
006100020702
006200020702     C                   ENDDO
006300020702
006400020703     C/EXEC SQL
006500020703     C+ CLOSE QADBIFLD
006600020702     C/END-EXEC
006700020703
006800020703     C                   EVAL      *INLR = *ON
006900020702
007000020702     ***********************************************************************
007100020702     **
007200020703     ** Scrittura subfile.
007300020702     **
007400020702     ***********************************************************************
007500020703     C     WrtS01        BEGSR
007600020702
007700020703     ** Scrivo tanti record del subfile quante sono le righe ricevute.
007800020703     C                   CLEAR                   I
007900020703     C                   DOU       I = SQLEr3 OR S01RRN = 9999
008000020703     C                   EVAL      I = I + 1
008100020703     C     I             OCCUR     QADBIFld
008200020703     C                   EVAL      S01Txt = DBITxt
008300020703     C                   EVAL      S01RRN = S01RRN + 1
008400020703     C                   WRITE     S01
008500020702     C                   ENDDO
008600020808
008700020808     ** Se ho ricevuto meno righe della pagina, ho finito il caricamento.
008800020808     C                   EVAL      SflEnd = (SQLEr3 < Rows OR S01RRN = 9999)
008900020702
009000020702     C                   ENDSR
009100020703
009200020703     ***********************************************************************
009300020703     **
009400020703     ** Emissione subfile.
009500020703     **
009600020703     ***********************************************************************
009700020703     C     FmtC01        BEGSR
009800020703
009900020808     ** Visualizzo il subfile control.
010000020703     C                   EVAL      SflDspCtl = *ON
010100020808
010200020808     ** Visualizzo il subfile se ho scritto almeno 1 record.
010300020703     C                   EVAL      SflDsp = (S01RRN > 0)
010400020808
010500020808     ** Visualizzo l'ultima pagina appena scritta.
010600020808     C                   EVAL      C01RcdNbr = S01RRN
010700020703
010800020703     C                   DOU       RollUp OR *INKC
010900020703
011000020703     C                   WRITE     F01
011100020703     C                   EXFMT     C01
011200020703
011300020703     C                   SELECT
011400020808     ** Quando l'utente preme F3 fermo il caricamento del subfile.
011500020703     C                   WHEN      *INKC
011600020703     C                   EVAL      SflEnd = *ON
011700020808     ** Quando l'utente preme ROLLUP continuo il caricamento del subfile.
011800020703     C                   WHEN      RollUp
011900020808     ** Quando l'utente preme INVIO rimango sulla pagina visualizzata.
012000020703     C                   OTHER
012100020709     C                   IF        C01CsrRRN > 0
012200020703     C                   EVAL      C01RcdNbr = C01CsrRRN
012300020709     C                   ENDIF
012400020703     C                   ENDSL
012500020703
012600020703     C                   ENDDO
012700020703
012800020703     C                   ENDSR
