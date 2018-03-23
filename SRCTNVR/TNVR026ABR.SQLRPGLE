000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300080225      * Il pgm annulla tutto ciò che riguarda le tariffe Poste
000400080225      *----------------------------------------------------------------
000500080226     fwfvr026f  o  a e           k disk    rename(wfvr026f:wf026)
000600080225     f                                     prefix(x_)
000700080225     F*---------------------------------------------------------------------------------
000800080225     D fifpt         E DS                  extname(fifpt00f)
000900080225     F*---------------------------------------------------------------------------------
001000080225
001100080225     D Nome_Campi      S              1
001200080225     D Numero          S              7  0
001300080225     D WrkSqlCmd       S           1024
001400080225
001500020205     c**********************************************************************
001600060127     c* MI COMPONGO L'ISTRUZIONE SQL
001700060127     c**********************************************************************
001800080225     c                   seton                                        LR
001900080225
002000080225      * ?Esegue un controllo se ci sono delle valorizzazioni Poste in atto
002100080225     C/EXEC SQL
002200080226     C+  select Count(*) into :NUMERO from
002300080226     C+  fictd00f WHERE CTDVOC > 399
002400080226     C+  and CTDDDC >20071231
002500080225     C/END-EXEC
002600080225      * ?Solo se non ci sono records su FTD da valorizzare.
002700080225     c                   If        numero > 0
002800080225      /FREE
002900080226         dsply 'NON ESEGUITO ANNULLAMENTO TARIFFE COOP';
003000080225      /END-FREE
003100080225     c                   goto      fine
003200080225     c                   endIF
003300080226
003400080225      * ?======================================================
003500080225      * ?Stampa prima di aggiornare con gli SQL
003600080225      * ?======================================================
003700080226
003800080226      * ?TESTATA DETTAGLIO delle tariffe POSTE
003900080226     c                   eval      WrkSqlCmd ='SELECT * from Fifpt00f where '+
004000080226     c                             'FptTSR not in (''C'', ''R'', ''G'') ' +
004100080226     c                             'and fptctd between ''400'' and ''499''  '
004200080226      * Lettura
004300080226     C/EXEC SQL
004400080226     C+ PREPARE X1 FROM :WrkSqlCmd
004500080226     C/END-EXEC
004600080226
004700080226     C/EXEC SQL
004800080226     C+ DECLARE B1 CURSOR FOR X1
004900080226     C/END-EXEC
005000080226
005100080226     C/EXEC SQL
005200080226     C+ OPEN B1
005300080226     C/END-EXEC
005400080226     C                   DOU       SqlCod <> 0
005500080226     C/EXEC SQL
005600080226     C+ FETCH NEXT FROM B1 INTO :fifpt
005700080226     C/END-EXEC
005800080226     C                   SELECT
005900080226     c*
006000080226     C                   WHEN      SqlCod = 0
006100080226     c*
006200080226     c                   if        Nome_Campi = ' '
006300080226      * ? Nome Campi
006400080226     c                   eval      x_wfvr026f = 'File;Autista;Simulato;TipoServ'
006500080226     c                             + ';CodTar;Progr;Dettaglio;'
006600080226     c                   write     wf026
006700080226     c                   eval      Nome_Campi = 'X'
006800080226     c                   end
006900080226      * ?
007000080226      * ?
007100080226      * ? Scrive i records sul file
007200080226     c                   eval      x_wfvr026f = 'FPT;' + %editc(FPTPDR:'X') +
007300080226     c                             ';' + FPTSML + ';' + FPTTSR + ';' +
007400080226     c                             %editc(FPTCTR:'X') +';'+ %editc(FPTPRG:'X')+
007500080226     c                             ';'+ FPTCTD + ';'
007600080226     c                   write     wf026
007700080226      * ?
007800080226      * ? EoF
007900080226     C                   WHEN      SqlCod = 100
008000080226     c                   leave
008100080226      * ? Errori
008200080226     C                   WHEN      SqlCod <> 0
008300080226      **
008400080226     C                   ENDSL
008500080226      **
008600080226     C                   ENDDO
008700080226      **
008800080226     C/EXEC SQL
008900080226     C+ CLOSE B1
009000080226     C/END-EXEC
009100080226
009200080226
009300080225
009400080225      * ?======================================================
009500080225      * ?Esegue annullamenti logici con gli SQL
009600080225      * ?======================================================
009700080225
009800080225      * ?Annulla TESTATA DETTAGLIO delle tariffe POSTE
009900080225     C/EXEC SQL
010000080225     C+  update  fifpt00f set FPTATB='A' where
010100080226     C+  FptTSR not in ('C', 'R', 'G')
010200080226     C+  and fptctd between '400' and '499'
010300080225     C/END-EXEC
010400080225
010500080225      * ?======================================================
010600080225
010700080225     c     FINE          tag
010800030429      *
010900020205     C**-------------------------------------------------------------**
011000000607     c     *inzsr        begsr
011100060125     C**-------------------------------------------------------------**
011200080225      *
011300080225      *
011400020402     c                   endsr
011500080225     C**-------------------------------------------------------------**
