000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060203      * Il pgm mette FTDATB = 'A' sulle bolle di sede che non hanno il record
000500060203      *  corrispondente in filiale.
000900060203      *   Serve x la pulizia Bolle di filiale
001500060125      *****************************************************************
003000030429     F*---------------------------------------------------------------------------------
003302060203     ffiftd05l  uf   e           k disk    rename(fiftd000:ftd05)
003400030429     F*---------------------------------------------------------------------------------
004601060203     D fiftd         E DS                  extname(fiftd00f)
006105060125      *
006800020328     D WrkSqlCmd       S           1024
007201060125      *
007800020205     c**********************************************************************
007802060127     c* MI COMPONGO L'ISTRUZIONE SQL
007803060203     c                   EVAL      WRKSQLCMD = 'SELECT * FROM ' +
007804060203     c                             'unitragru/fiftd05l WHERE ' +
007805060203     c                             'digits(FTDPDR)!!FTDTSR!!digits(FTDNDC)!!' +
007806060203     c                             'digits(FTDDDC)!!digits(FTDAAS)!!' +
007807060203     c                             'digits(FTDLNP)!!digits(FTDNRS)!!' +
007808060203     c                             'digits(FTDNSP) not in (SELECT ' +
007809060203     c                             'digits(FTDPDR)!!FTDTSR!!digits(FTDNDC)!!' +
007810060203     c                             'digits(FTDDDC)!!digits(FTDAAS)!!' +
007811060203     c                             'digits(FTDLNP)!!digits(FTDNRS)!!' +
007812060203     c                             'digits(FTDNSP) from edpab/fiftd05l)'
007814060127     c**********************************************************************
007900060203     c* Lettura fifTD00F
008000020328     C/EXEC SQL
008100020328     C+ PREPARE S1 FROM :WrkSqlCmd
008200020328     C/END-EXEC
008300020328
008400020328     C/EXEC SQL
008500020328     C+ DECLARE A1 CURSOR FOR S1
008600020328     C/END-EXEC
008700020328
008800020328     C/EXEC SQL
008900020328     C+ OPEN A1
009000020328     C/END-EXEC
009200020328     C                   DOU       SqlCod <> 0
009300020328     C/EXEC SQL
009400060203     C+ FETCH NEXT FROM A1 INTO :fiftd
009500020328     C/END-EXEC
009700020328     C                   SELECT
009701060125     c*
009800020328     C                   WHEN      SqlCod = 0
010201060125     c*
010202060203      * ? Flagga FIFTD di sede con FTDATB = 'A'
010205060203     C     Kftd          chain     Fiftd05l
010207060203     c                   if        %Found(Fiftd05l)
010208060203     c                   EvaL      ftdATB='A'
010209060203     c                   update    FTD05
010236060203     c                   EnD
011401060125      * ?
012602060125      * ? EoF
012700020328     C                   WHEN      SqlCod = 100
012701060125      **
012702060125      * ? Errori
012800020328     C                   WHEN      SqlCod <> 0
012801060125      **
012900020328     C                   ENDSL
012901060125      **
013000020328     C                   ENDDO
013200020328     C/EXEC SQL
013300020328     C+ CLOSE A1
013400020328     C/END-EXEC
015000030429      *
015100000607     c                   seton                                        lr
031500020205     C**-------------------------------------------------------------**
034900000607     c     *inzsr        begsr
034901060125     C**-------------------------------------------------------------**
035304060127     c     kftd          klist
035305060203     c                   kfld                    FTDPDR
035306060203     c                   kfld                    FTDTSR
035307060203     c                   kfld                    FTDNDC
035308060203     c                   kfld                    FTDDDC
035309060203     c                   kfld                    FTDAAS
035310060203     c                   kfld                    FTDLNP
035311060203     c                   kfld                    FTDNRS
035312060203     c                   kfld                    FTDNSP
037600030429      *----------*
041400020402     c                   endsr
041401060203     C**-------------------------------------------------------------**
