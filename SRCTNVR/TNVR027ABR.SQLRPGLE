000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300080225      * Il pgm annulla tutto ciò che riguarda le tariffe Poste
000400080225      *----------------------------------------------------------------
000500080226     fwfvr027f  o  a e           k disk    rename(wfvr027f:wf027)
000600080225     f                                     prefix(x_)
000700080225     F*---------------------------------------------------------------------------------
000800080225     D fifpd         E DS                  extname(fifpd00f)
000900080225     D fifpt         E DS                  extname(fifpt00f)
001000080225     D fifgt         E DS                  extname(fifgt00f)
001100080225     F*---------------------------------------------------------------------------------
001200080225
001300080225     D Nome_Campi      S              1
001400080225     D Numero          S              7  0
001500080225     D WrkSqlCmd       S           1024
001600080225
001700020205     c**********************************************************************
001800060127     c* MI COMPONGO L'ISTRUZIONE SQL
001900060127     c**********************************************************************
002000080225     c                   seton                                        LR
002100080225
002200080225
002300080225      * ?======================================================
002400080225      * ?Stampa prima di aggiornare con gli SQL
002500080225      * ?======================================================
002600080225
002700080225      * ?DETTAGLIO delle tariffe POSTE
002800080225     c                   eval      WrkSqlCmd ='SELECT * from ' +
002900080225     c                             ' Fifpd00f where '+
003000080225     c                             'digits(FpdPDR)concat(FpdSML)concat' +
003100080225     c                             '(FpdTSR)concat(digits(FpdCTR))'+
003200080225     c                             'concat(digits(FpdPRG)) in (SELECT ' +
003300080225     c                             'digits(FGTPDR)concat(FGTSML)concat' +
003400080225     c                             '(FGTTSR)concat(digits(FGTCTR))'+
003500080225     c                             'concat(digits(FGTPRG)) '+
003600080225     c                             'FROM FIFGT00F WHERE FGTFTP = ''O'' )'
003700080225      * Lettura
003800080225     C/EXEC SQL
003900080225     C+ PREPARE S1 FROM : WrkSqlCmd
004000080225     C/END-EXEC
004100080225
004200080225     C/EXEC SQL
004300080225     C+ DECLARE A1 CURSOR FOR S1
004400080225     C/END-EXEC
004500080225
004600080225     C/EXEC SQL
004700080225     C+ OPEN A1
004800080225     C/END-EXEC
004900080225     C                   DOU       SqlCod <> 0
005000080225     C/EXEC SQL
005100080225     C+ FETCH NEXT FROM A1 INTO :fifpd
005200080225     C/END-EXEC
005300080225     C                   SELECT
005400080225     c*
005500080225     C                   WHEN      SqlCod = 0
005600080225     c*
005700080225     c                   if        Nome_Campi = ' '
005800080225      * ? Nome Campi
005900080226     c                   eval      x_wfvr027f = 'File;Autista;Simulato;TipoServ'
006000080225     c                             + ';CodTar;Progr;Dettaglio;'
006100080226     c                   write     wf027
006200080225     c                   eval      Nome_Campi = 'X'
006300080225     c                   end
006400080225      * ?
006500080225      * ? Scrive i records sul file
006600080226     c                   eval      x_wfvr027f = 'FPD;' + %editc(FPDPDR:'X') +
006700080225     c                             ';' + FPDSML + ';' + FPDTSR + ';' +
006800080225     c                             %editc(FPDCTR:'X') +';'+ %editc(FPDPRG:'X')+
006900080225     c                             ';' + FPDCTD +';'
007000080226     c                   write     wf027
007100080225      * ?
007200080225      * ? EoF
007300080225     C                   WHEN      SqlCod = 100
007400080225     c                   leave
007500080225      * ? Errori
007600080225     C                   WHEN      SqlCod <> 0
007700080225      **
007800080225     C                   ENDSL
007900080225      **
008000080225     C                   ENDDO
008100080225     c                   eval      Nome_Campi = 'X'
008200080225     C/EXEC SQL
008300080225     C+ CLOSE A1
008400080225     C/END-EXEC
008500080226
008600080226
008700080226      * ?TESTATA DETTAGLIO delle tariffe POSTE
008800080226     c                   eval      WrkSqlCmd ='SELECT * from ' +
008900080226     c                             ' Fifpt00f where '+
009000080226     c                             'digits(FptPDR)concat(FptSML)concat' +
009100080226     c                             '(FptTSR)concat(digits(FptCTR))'+
009200080226     c                             'concat(digits(FptPRG)) in (SELECT ' +
009300080226     c                             'digits(FGTPDR)concat(FGTSML)concat' +
009400080226     c                             '(FGTTSR)concat(digits(FGTCTR))'+
009500080226     c                             'concat(digits(FGTPRG)) '+
009600080226     c                             'FROM FIFGT00F WHERE FGTFTP = ''O'' )'
009700080226      * Lettura
009800080226     C/EXEC SQL
009900080226     C+ PREPARE X1 FROM :WrkSqlCmd
010000080226     C/END-EXEC
010100080226
010200080226     C/EXEC SQL
010300080226     C+ DECLARE B1 CURSOR FOR X1
010400080226     C/END-EXEC
010500080226
010600080226     C/EXEC SQL
010700080226     C+ OPEN B1
010800080226     C/END-EXEC
010900080226     C                   DOU       SqlCod <> 0
011000080226     C/EXEC SQL
011100080226     C+ FETCH NEXT FROM B1 INTO :fifpt
011200080226     C/END-EXEC
011300080226     C                   SELECT
011400080226     c*
011500080226     C                   WHEN      SqlCod = 0
011600080226      * ?
011700080226      * ? Scrive i records sul file
011800080226     c                   eval      x_wfvr027f = 'FPT;' + %editc(FPTPDR:'X') +
011900080226     c                             ';' + FPTSML + ';' + FPTTSR + ';' +
012000080226     c                             %editc(FPTCTR:'X') +';'+ %editc(FPTPRG:'X')+
012100080226     c                             ';' + FPTCTD +';'
012200080226     c                   write     wf027
012300080226      * ?
012400080226      * ? EoF
012500080226     C                   WHEN      SqlCod = 100
012600080226     c                   leave
012700080226      * ? Errori
012800080226     C                   WHEN      SqlCod <> 0
012900080226      **
013000080226     C                   ENDSL
013100080226      **
013200080226     C                   ENDDO
013300080226      **
013400080226     C/EXEC SQL
013500080226     C+ CLOSE B1
013600080226     C/END-EXEC
013700080226
013800080226
013900080226      * ?TESTATA delle tariffe POSTE
014000080226     c                   eval      WrkSqlCmd ='SELECT * ' +
014100080226     c                             'FROM FIFGT00F WHERE FGTFTP = ''O'' '
014200080226      * Lettura
014300080226     C/EXEC SQL
014400080226     C+ PREPARE Z1 FROM :WrkSqlCmd
014500080226     C/END-EXEC
014600080226
014700080226     C/EXEC SQL
014800080226     C+ DECLARE C1 CURSOR FOR Z1
014900080226     C/END-EXEC
015000080226
015100080226     C/EXEC SQL
015200080226     C+ OPEN C1
015300080226     C/END-EXEC
015400080226     C                   DOU       SqlCod <> 0
015500080226     C/EXEC SQL
015600080226     C+ FETCH NEXT FROM C1 INTO :fifgt
015700080226     C/END-EXEC
015800080226     C                   SELECT
015900080226     c*
016000080226     C                   WHEN      SqlCod = 0
016100080226      * ?
016200080226      * ? Scrive i records sul file
016300080226     c                   eval      x_wfvr027f = 'FGT;' + %editc(FGTPDR:'X') +
016400080226     c                             ';' + FGTSML + ';' + FGTTSR + ';' +
016500080226     c                             %editc(FGTCTR:'X') +';'+ %editc(FGTPRG:'X')+
016600080226     c                             ';'
016700080226     c                   write     wf027
016800080226      * ?
016900080226      * ? EoF
017000080226     C                   WHEN      SqlCod = 100
017100080226     c                   leave
017200080226      * ? Errori
017300080226     C                   WHEN      SqlCod <> 0
017400080226      **
017500080226     C                   ENDSL
017600080226      **
017700080226     C                   ENDDO
017800080226      **
017900080226     C/EXEC SQL
018000080226     C+ CLOSE C1
018100080226     C/END-EXEC
018200080226
018300080225
018400080225     c     FINE          tag
018500080226      * ?----------------------------------------------------------  **
