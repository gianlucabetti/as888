000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300080225      * Il pgm annulla tutto ciò che riguarda le tariffe Poste
000400080225      *----------------------------------------------------------------
000500080225     fwfvr025f  o  a e           k disk    rename(wfvr025f:wf025)
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
002200080225      * ?Esegue un controllo se ci sono delle valorizzazioni Poste in atto
002300080225     C/EXEC SQL
002400080225     C+  select Count(*) into :NUMERO from fiftd00f
002500080225     C+  where digits(ftdpdr) concat ftdctp in (
002600080225     C+  SELECT digits(fgtpdr) concat fgtctr FROM FIFGT00F
002700080225     C+  WHERE FGTFTP = 'O' and fgtsml=' '  )
002800080225     C/END-EXEC
002900080225      * ?Solo se non ci sono records su FTD da valorizzare.
003000080225     c                   If        numero > 0
003100080225      /FREE
003200080225         dsply 'NON ESEGUITO ANNULLAMENTO TARIFFE';
003300080225      /END-FREE
003400080225     c                   goto      fine
003500080225     c                   endIF
003600080225
003700080225      * ?======================================================
003800080225      * ?Stampa prima di aggiornare con gli SQL
003900080225      * ?======================================================
004000080225
004100080225      * ?DETTAGLIO delle tariffe POSTE
004200080225     c                   eval      WrkSqlCmd ='SELECT * from ' +
004300080225     c                             ' Fifpd00f where '+
004400080225     c                             'digits(FpdPDR)concat(FpdSML)concat' +
004500080225     c                             '(FpdTSR)concat(digits(FpdCTR))'+
004600080225     c                             'concat(digits(FpdPRG)) in (SELECT ' +
004700080225     c                             'digits(FGTPDR)concat(FGTSML)concat' +
004800080225     c                             '(FGTTSR)concat(digits(FGTCTR))'+
004900080225     c                             'concat(digits(FGTPRG)) '+
005000080225     c                             'FROM FIFGT00F WHERE FGTFTP = ''O'' )'
005100080225      * Lettura
005200080225     C/EXEC SQL
005300080225     C+ PREPARE S1 FROM : WrkSqlCmd
005400080225     C/END-EXEC
005500080225
005600080225     C/EXEC SQL
005700080225     C+ DECLARE A1 CURSOR FOR S1
005800080225     C/END-EXEC
005900080225
006000080225     C/EXEC SQL
006100080225     C+ OPEN A1
006200080225     C/END-EXEC
006300080225     C                   DOU       SqlCod <> 0
006400080225     C/EXEC SQL
006500080225     C+ FETCH NEXT FROM A1 INTO :fifpd
006600080225     C/END-EXEC
006700080225     C                   SELECT
006800080225     c*
006900080225     C                   WHEN      SqlCod = 0
007000080225     c*
007100080225     c                   if        Nome_Campi = ' '
007200080225      * ? Nome Campi
007300080225     c                   eval      x_wfvr025f = 'File;Autista;Simulato;TipoServ'
007400080225     c                             + ';CodTar;Progr;Dettaglio;'
007500080225     c                   write     wf025
007600080225     c                   eval      Nome_Campi = 'X'
007700080225     c                   end
007800080225      * ?
007900080225      * ? Scrive i records sul file
008000080225     c                   eval      x_wfvr025f = 'FPD;' + %editc(FPDPDR:'X') +
008100080225     c                             ';' + FPDSML + ';' + FPDTSR + ';' +
008200080225     c                             %editc(FPDCTR:'X') +';'+ %editc(FPDPRG:'X')+
008300080225     c                             ';' + FPDCTD +';'
008400080225     c                   write     wf025
008500080225      * ?
008600080225      * ? EoF
008700080225     C                   WHEN      SqlCod = 100
008800080225     c                   leave
008900080225      * ? Errori
009000080225     C                   WHEN      SqlCod <> 0
009100080225      **
009200080225     C                   ENDSL
009300080225      **
009400080225     C                   ENDDO
009500080225     c                   eval      Nome_Campi = 'X'
009600080225     C/EXEC SQL
009700080225     C+ CLOSE A1
009800080225     C/END-EXEC
009900080226
010000080226
010100080226      * ?TESTATA DETTAGLIO delle tariffe POSTE
010200080226     c                   eval      WrkSqlCmd ='SELECT * from ' +
010300080226     c                             ' Fifpt00f where '+
010400080226     c                             'digits(FptPDR)concat(FptSML)concat' +
010500080226     c                             '(FptTSR)concat(digits(FptCTR))'+
010600080226     c                             'concat(digits(FptPRG)) in (SELECT ' +
010700080226     c                             'digits(FGTPDR)concat(FGTSML)concat' +
010800080226     c                             '(FGTTSR)concat(digits(FGTCTR))'+
010900080226     c                             'concat(digits(FGTPRG)) '+
011000080226     c                             'FROM FIFGT00F WHERE FGTFTP = ''O'' )'
011100080226      * Lettura
011200080226     C/EXEC SQL
011300080226     C+ PREPARE X1 FROM :WrkSqlCmd
011400080226     C/END-EXEC
011500080226
011600080226     C/EXEC SQL
011700080226     C+ DECLARE B1 CURSOR FOR X1
011800080226     C/END-EXEC
011900080226
012000080226     C/EXEC SQL
012100080226     C+ OPEN B1
012200080226     C/END-EXEC
012300080226     C                   DOU       SqlCod <> 0
012400080226     C/EXEC SQL
012500080226     C+ FETCH NEXT FROM B1 INTO :fifpt
012600080226     C/END-EXEC
012700080226     C                   SELECT
012800080226     c*
012900080226     C                   WHEN      SqlCod = 0
013000080226      * ?
013100080226      * ? Scrive i records sul file
013200080226     c                   eval      x_wfvr025f = 'FPT;' + %editc(FPTPDR:'X') +
013300080226     c                             ';' + FPTSML + ';' + FPTTSR + ';' +
013400080226     c                             %editc(FPTCTR:'X') +';'+ %editc(FPTPRG:'X')+
013500080226     c                             ';'
013600080226     c                   write     wf025
013700080226      * ?
013800080226      * ? EoF
013900080226     C                   WHEN      SqlCod = 100
014000080226     c                   leave
014100080226      * ? Errori
014200080226     C                   WHEN      SqlCod <> 0
014300080226      **
014400080226     C                   ENDSL
014500080226      **
014600080226     C                   ENDDO
014700080226      **
014800080226     C/EXEC SQL
014900080226     C+ CLOSE B1
015000080226     C/END-EXEC
015100080226
015200080226
015300080226      * ?TESTATA delle tariffe POSTE
015400080226     c                   eval      WrkSqlCmd ='SELECT * ' +
015500080226     c                             'FROM FIFGT00F WHERE FGTFTP = ''O'' '
015600080226      * Lettura
015700080226     C/EXEC SQL
015800080226     C+ PREPARE Z1 FROM :WrkSqlCmd
015900080226     C/END-EXEC
016000080226
016100080226     C/EXEC SQL
016200080226     C+ DECLARE C1 CURSOR FOR Z1
016300080226     C/END-EXEC
016400080226
016500080226     C/EXEC SQL
016600080226     C+ OPEN C1
016700080226     C/END-EXEC
016800080226     C                   DOU       SqlCod <> 0
016900080226     C/EXEC SQL
017000080226     C+ FETCH NEXT FROM C1 INTO :fifgt
017100080226     C/END-EXEC
017200080226     C                   SELECT
017300080226     c*
017400080226     C                   WHEN      SqlCod = 0
017500080226      * ?
017600080226      * ? Scrive i records sul file
017700080226     c                   eval      x_wfvr025f = 'FGT;' + %editc(FGTPDR:'X') +
017800080226     c                             ';' + FGTSML + ';' + FGTTSR + ';' +
017900080226     c                             %editc(FGTCTR:'X') +';'+ %editc(FGTPRG:'X')+
018000080226     c                             ';'
018100080226     c                   write     wf025
018200080226      * ?
018300080226      * ? EoF
018400080226     C                   WHEN      SqlCod = 100
018500080226     c                   leave
018600080226      * ? Errori
018700080226     C                   WHEN      SqlCod <> 0
018800080226      **
018900080226     C                   ENDSL
019000080226      **
019100080226     C                   ENDDO
019200080226      **
019300080226     C/EXEC SQL
019400080226     C+ CLOSE C1
019500080226     C/END-EXEC
019600080226
019700080226
019800080225
019900080225      * ?======================================================
020000080225      * ?Esegue annullamenti logici con gli SQL
020100080225      * ?======================================================
020200080225
020300080225      * ?Annulla DETTAGLIO delle tariffe POSTE
020400020328     C/EXEC SQL
020500080225     C+  update  fifpd00f set FPDATB='A' where
020600080225     C+  digits(FpdPDR)concat(FpdSML)concat(FpdTSR)concat
020700080225     C+  (digits(FpdCTR))concat(digits(FpdPRG)) in ( SELECT
020800080225     C+  digits(FGTPDR)concat(FGTSML)concat(FGTTSR)concat
020900080225     C+  (digits(FGTCTR))concat(digits(FGTPRG))
021000080225     C+  FROM FIFGT00F WHERE FGTFTP = 'O' )
021100020328     C/END-EXEC
021200080225
021300080225      * ?Annulla TESTATA DETTAGLIO delle tariffe POSTE
021400080225     C/EXEC SQL
021500080225     C+  update  fifpt00f set FPTATB='A' where
021600080225     C+  digits(FpTPDR)concat(FpTSML)concat(FpTTSR)concat
021700080225     C+  (digits(FpTCTR))concat(digits(FpTPRG)) in ( SELECT
021800080225     C+  digits(FGTPDR)concat(FGTSML)concat(FGTTSR)concat
021900080225     C+  (digits(FGTCTR))concat(digits(FGTPRG))
022000080225     C+  FROM FIFGT00F WHERE FGTFTP = 'O' )
022100080225     C/END-EXEC
022200080225
022300080225      * ?Annulla TESTATE tariffe POSTE
022400080225     C/EXEC SQL
022500080225     C+  update FIFGT00F set FGTATB='A' WHERE FGTFTP = 'O'
022600080225     C/END-EXEC
022700080225
022800080225      * ?======================================================
022900080225
023000080225      * ?----------------------------------------------------------  **
023100080225     c     FINE          tag
023200030429      *
023300020205     C**-------------------------------------------------------------**
023400000607     c     *inzsr        begsr
023500060125     C**-------------------------------------------------------------**
023600080225      *
023700080225      *
023800020402     c                   endsr
023900080225     C**-------------------------------------------------------------**
