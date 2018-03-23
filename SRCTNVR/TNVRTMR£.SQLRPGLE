000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000500060905     fteetc01l  uf   e           k disk
000501060905     fteets01l  uf   e           k disk
000502060905     fteett01l  uf   e           k disk
001000060905     D teetcds       e ds                  extname(teetc00f)
001001060905     D teetcdsf      e ds                  extname(teetc00f) prefix(f)
001002060905     D teetcdss      e ds                  extname(teetc00f) prefix(s)
001003060905     d dsf             s                   like(etcdsf)
001004060905     d cto             s                   like(etccto)
001005060905     d ksc             s                   like(etcksc)
001006060905     d ctr             s                   like(etcctr)
001007060905     d prg             s                   like(etcprg)
001008060905     d lnp             s                   like(etclnp)
001009060906     d conta           s              5  0
001700020702     c*
001800060905     c* legge tutti i record di TEETC
001900060905     c* per il tipo T = tariffa e tiene quella manutenzionata x ultima
002000020702     c*
002100020702     C/EXEC SQL
002200060905     C+ DECLARE A1 CURSOR FOR select etccto,
002300060905     C+        ETCKSC, ETCCTR, ETCPRG, ETCLNP, count(*) FROM teetc00f WHERE
002301060905     C+ ETCCTO = 'T' GROUP BY etccto, ETCKSC, ETCCTR, ETCPRG, ETCLNP HAVING
002302060905     C+ count(*) > 1 ORDER BY etccto, ETCKSC, ETCCTR, ETCPRG, ETCLNP
002400020702     C/END-EXEC
002500020702
002600020702     C/EXEC SQL
002700020702     C+ OPEN A1
002800020702     C/END-EXEC
002900020702
003000060905     C                   do        *hival
003100020702
003200020702     C/EXEC SQL
003300060906     C+ FETCH NEXT FROM A1 INTO :cto, :ksc, :ctr, :prg, :lnp, :conta
003400020702     C/END-EXEC
003500020702
003600020702     C                   select
003700020821     C                   when      sqlcod <> 0
003800060905     C                   leave
003900020821     C                   other
003901060905     c                   clear                   teetcds
003902060905     c                   clear                   teetcdss
003903060905     c                   clear                   teetcdsf
003904060905     c                   eval      dsf = 'F'
005700060905     C     ketc          chain     teetc01l
005800060905     c                   if        %found(teetc01l)
006000060905     c                   eval      teetcdsf= teetcds
006100020821     c                   end
006101060905     c                   eval      dsf = 'S'
006102060905     C     ketc          chain     teetc01l
006103060905     c                   if        %found(teetc01l)
006104060905     c                   eval      teetcdss= teetcds
006105060905     c                   end
006107060906     c                   if        setcduv >= setcdir
006108060906     c                   if        setcduv >=fetcduv and
006109060906     c                             setcduv >=fetcdir
006110060905     c                   eval      dsf = 'F'
006111060905     c                   exsr      sraggio
006112060905     c                   iter
006113060906     c                   end
006114060906     c                   end
006115060906     c*
006116060906     c                   if        setcduv < setcdir
006117060906     c                   if        setcdir >=fetcduv and
006118060906     c                             setcdir >=fetcdir
006119060906     c                   eval      dsf = 'F'
006120060906     c                   exsr      sraggio
006121060906     c                   iter
006122060906     c                   end
006123060906     c                   end
006124060906     c                   if        fetcduv >= fetcdir
006125060906     c                   if        fetcduv >=setcduv and
006126060906     c                             fetcduv >=setcdir
006127060906     c                   eval      dsf = 'S'
006128060906     c                   exsr      sraggio
006129060906     c                   iter
006130060906     c                   end
006131060906     c                   end
006132060906     c*
006133060906     c                   if        fetcduv < fetcdir
006134060906     c                   if        fetcdir >=setcduv and
006135060906     c                             fetcdir >=setcdir
006136060906     c                   eval      dsf = 'S'
006137060906     c                   exsr      sraggio
006138060906     c                   iter
006139060906     c                   end
006200060906     C                   end
006201060906     C                   endsl
006300020702     C*
006400020702     C                   enddo
006500020702
006600020702     C/EXEC SQL
006700020702     C+ CLOSE A1
006800020702     C/END-EXEC
006801060906     c* a questo punto imposto tutto con flag sede
006802060906     C/EXEC SQL
006804060906     C+ UPDATE TEETC00F SET ETCDSF = 'S' WHERE etcdsf = 'F'
006807060906     C/END-EXEC
006808060906     C/EXEC SQL
006809060906     C+ UPDATE TEETS00F SET ETSDSF = 'S' WHERE etsdsf = 'F'
006810060906     C/END-EXEC
006811060906     C/EXEC SQL
006812060906     C+ UPDATE TEETT00F SET ETTDSF = 'S' WHERE ettdsf = 'F'
006813060906     C/END-EXEC
006814060905     c                   seton                                        lr
006900000607      *-----------------------------------------------
007000060905     c     sraggio       begsr
007100000607      *-----------------------------------------------
007101060905     c     ketc          delete    teetc01l
007102060905     c*
007103060905     c     ketc          setll     teets01l
007104060905     c                   do        *hival
007105060905     c     ketc          reade     teets01l
007106060905     c                   if        %eof(teets01l)
007107060905     c                   leave
007108060905     c                   end
007109060905     c                   delete    teets000
007110060905     c                   enddo
007111060905     c     ketc          setll     teett01l
007112060905     c                   do        *hival
007113060905     c     ketc          reade     teett01l
007114060905     c                   if        %eof(teett01l)
007115060905     c                   leave
007116060905     c                   end
007117060905     c                   delete    teett000
007118060905     c                   enddo
007119060905     c                   endsr
007142060905      *-----------------------------------------------
007143060905     c     *inzsr        begsr
007144060905      *-----------------------------------------------
007500020402     c*
007501060905     c     ketc          klist
007502060905     c                   kfld                    dsf
007503060905     c                   kfld                    cto
007504060905     c                   kfld                    ksc
007505060905     c                   kfld                    ctr
007506060905     c                   kfld                    prg
007507060905     c                   kfld                    lnp
008000020402     c                   endsr
