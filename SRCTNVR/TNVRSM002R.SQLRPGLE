000100080108      *PARMS  CLOSQLCSR(*ENDMOD)  DBGVIEW(*SOURCE)  DYNUSRPRF(*OWNER)
000200080108     h decedit('0,') datedit(*dmy/)
000300080108
000400080108     fFNMSA12L  Uf A e           k disk    rename(FNMSA000:FNMSA100)
000500080108
000600080108     d Status         sds
000700080108     d  SDSprm           *parms
000800080108      *
000900080108     d ds_FNMSA      e ds                  extname(FNMSA10F)
001000080108     d                                     prefix(SQL:3)
001100080108     d                                     inz
001200080108      *
001300080108     d $EoF            s               n   inz(*off)
001400080108
001500080108      * - Record da aggiornare (chainato)
001600080108     c     k06msa12      klist                                                  *fnmsa12l
001700080108     c                   kfld                    SQLapl                          -tipo applicazione
001800080108     c                   kfld                    SQLfil                          -filiale
001900080108     c                   kfld                    SQLdfv                          -data foglio
002000080108     c                   kfld                    SQLnpg                          -categoria foglio
002100080108     c                   kfld                    SQLdfv                          -data lettura
002200080108     c                   kfld                    SQLorl                          -ora  lettura
002300080108      * - Record da sommare e cancellare (letto via SQL)
002400080108     c     k06msa12e     klist                                                  *fnmsa12l
002500080108     c                   kfld                    SQLapl                          -tipo applicazione
002600080108     c                   kfld                    SQLfil                          -filiale
002700080108     c                   kfld                    SQLdfv                          -data foglio
002800080108     c                   kfld                    SQLnpg                          -categoria foglio
002900080108     c                   kfld                    SQLdtl                          -data lettura
003000080108     c                   kfld                    SQLorl                          -ora  lettura
003100080108      *
003200080108     c                   exsr      OpenCursor
003300080108      *
003400080108     c                   dow       $EoF = *off
003500080108     c                   exsr      ReadCursor
003600080108     c                   enddo
003700080108      *
003800080108     c                   exsr      CloseCursor
003900080108      *
004000080108     c                   eval      *inLR = *on
004100080108
004200080108      *---------------------------------------------------------------*
004300080108      *?Apertura cursore.                                            ?*
004400080108      *---------------------------------------------------------------*
004500080108     c     OpenCursor    BEGSR
004600080108      *
004700080108     c/exec sql
004800080108     c+ declare A1 cursor for
004900080108     c+ select  *
005000080108     c+ from    FNMSA10F
005100080108     c+ where   substr(char(MSAdtl), 1, 4) <> substr(char(MSAdfv), 1, 4)
005200080108     c+   and   substr(char(MSAdtl), 5, 4) =  substr(char(MSAdfv), 5, 4)
005300080108     c+ for     FETCH  only
005400080108     c/end-exec
005500080108      *
005600080108     c/exec sql
005700080108     c+ open A1
005800080108     c/end-exec
005900080108      *
006000080108     c                   ENDSR
006100080108
006200080108      *---------------------------------------------------------------*
006300080108      *?Lettura cursore.                                             ?*
006400080108      *---------------------------------------------------------------*
006500080108     c     ReadCursor    BEGSR
006600080108      *
006700080108     c                   clear                   ds_FNMSA
006800080108      *
006900080108     c/exec sql
007000080108     c+ fetch next from A1 into :ds_FNMSA
007100080108     c/end-exec
007200080108      *
007300080108sel 1c                   select
007400080108w   1c                   when      SQLcod  =  100
007500080108     c                   eval      $EoF    =  *on
007600080108w   1c                   when      SQLcod  <  *zeros
007700080108     c                   eval      $Eof    =  *on
007800080108x   1c                   other
007900080108     c                   exsr      ElabRec
008000080108e   1c                   endsl
008100080108      *
008200080108     c                   ENDSR
008300080108
008400080108      *---------------------------------------------------------------*
008500080108      *?Chiusura cursore.                                            ?*
008600080108      *---------------------------------------------------------------*
008700080108     c     CloseCursor   BEGSR
008800080108      *
008900080108     c/exec sql
009000080108     c+ close A1
009100080108     c/end-exec
009200080108      *
009300080108     c                   ENDSR
009400080108
009500080108      *---------------------------------------------------------------*
009600080108      *?Elaborazione singolo record.                                 ?*
009700080108      *---------------------------------------------------------------*
009800080108     c     ElabRec       BEGSR
009900080108      *
010000080108      * Aggiornamento record con l'anno corretto nella data
010100080108      *
010200080108     c     k06msa12      chain     FNMSA100
010300080108      *
010400080108if  2c                   IF        not %found(FNMSA12L)
010500080108     c                   clear                   FNMSA100
010600080108     c                   eval      MSAapl  = SQLapl
010700080108     c                   eval      MSAfil  = SQLfil
010800080108     c                   eval      MSAdtl  = SQLdfv
010900080108     c                   eval      MSAorl  = SQLorl
011000080108     c                   eval      MSAdfv  = SQLdfv
011100080108     c                   eval      MSAnpg  = SQLnpg
011200080108     c                   exsr      sr_RieMSA
011300080108      *                  __________________
011400080108     c                   write     FNMSA100
011500080108      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
011600080108x   2c                   ELSE
011700080108     c                   exsr      sr_RieMSA
011800080108      *                  __________________
011900080108     c                   update    FNMSA100
012000080108      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
012100080108e   2c                   ENDIF
012200080108      *
012300080108      * Cancellazione record con l'anno errata nella data
012400080108      *
012500080108     c     k06msa12e     chain     FNMSA100
012600080108     c                   if        %found(FNMSA12L)
012700080108     c                   delete    FNMSA100
012800080108     c                   endif
012900080108      *
013000080108     c                   ENDSR
013100080108
013200080108      *---------------------------------------------------------------*
013300080108      *?Aggiornamento campi nel rec. FNMSA10F                        ?*
013400080108      *---------------------------------------------------------------*
013500080108     c     sr_RieMSA     BEGSR
013600080108      *
013700080108     c                   add       SQLpes        MSApes
013800080108     c                   add       SQLvol        MSAvol
013900080108     c                   add       SQLodsdiv     MSAodsdiv
014000080108     c                   add       SQLodsdnr     MSAodsdnr
014100080108     c                   add       SQLodsdna     MSAodsdna
014200080108     c                   add       SQLodsftd     MSAodsftd
014300080108     c                   add       SQLodsids     MSAodsids
014400080108     c                   add       SQLodsuds     MSAodsuds
014500080108     c                   add       SQLodsdne     MSAodsdne
014600080108     c                   add       SQLodsciv     MSAodsciv
014700080108     c                   add       SQLodsdnq     MSAodsdnq
014800080108     c                   add       SQLodsust     MSAodsust
014900080108     c                   add       SQLdtsnts     MSAdtsnts
015000080108     c                   add       SQLdtstok     MSAdtstok
015100080108     c                   add       SQLdtsila     MSAdtsila
015200080108     c                   add       SQLdtsnrl     MSAdtsnrl
015300080108     c                   add       SQLdtsndf     MSAdtsndf
015400080108     c                   add       SQLps1dnr     MSAps1dnr
015500080108     c                   add       SQLps1dts     MSAps1dts
015600080108     c                   add       SQLps1dtl     MSAps1dtl
015700080108     c                   add       SQLps1nsc     MSAps1nsc
015800080108     c                   add       SQLps1nre     MSAps1nre
015900080108     c                   add       SQLps1dok     MSAps1dok
016000080108     c                   add       SQLps1dno     MSAps1dno
016100080108     c                   add       SQLps2dnr     MSAps2dnr
016200080108     c                   add       SQLps2dts     MSAps2dts
016300080108     c                   add       SQLps2dtl     MSAps2dtl
016400080108     c                   add       SQLps2nsc     MSAps2nsc
016500080108     c                   add       SQLps2nre     MSAps2nre
016600080108     c                   add       SQLps2dok     MSAps2dok
016700080108     c                   add       SQLps2dno     MSAps2dno
016800080108     c                   add       SQLpesnes     MSApesnes
016900080108     c                   add       SQLpesind     MSApesind
017000080108     c                   add       SQLpesune     MSApesune
017100080108     c                   add       SQLpusnes     MSApusnes
017200080108     c                   add       SQLpussor     MSApussor
017300080108     c                   add       SQLpuslos     MSApuslos
017400080108     c                   add       SQLpep001     MSApep001
017500080108     c                   add       SQLpep002     MSApep002
017600080108     c                   add       SQLpep003     MSApep003
017700080108     c                   add       SQLpep004     MSApep004
017800080108     c                   add       SQLpep005     MSApep005
017900080108     c                   add       SQLpep006     MSApep006
018000080108     c                   add       SQLpep007     MSApep007
018100080108     c                   add       SQLpep008     MSApep008
018200080108     c                   add       SQLpep009     MSApep009
018300080108     c                   add       SQLpep010     MSApep010
018400080108     c                   add       SQLpep011     MSApep011
018500080108     c                   add       SQLpep012     MSApep012
018600080108     c                   add       SQLpep013     MSApep013
018700080108     c                   add       SQLpep014     MSApep014
018800080108     c                   add       SQLpep015     MSApep015
018900080108     c                   add       SQLpep016     MSApep016
019000080108     c                   add       SQLpep017     MSApep017
019100080108     c                   add       SQLpep018     MSApep018
019200080108     c                   add       SQLpep019     MSApep019
019300080108     c                   add       SQLpep020     MSApep020
019400080108     c                   add       SQLpad001     MSApad001
019500080108     c                   add       SQLpad002     MSApad002
019600080108     c                   add       SQLpad003     MSApad003
019700080108     c                   add       SQLpad004     MSApad004
019800080108     c                   add       SQLpad005     MSApad005
019900080108     c                   add       SQLpad006     MSApad006
020000080108     c                   add       SQLpad007     MSApad007
020100080108     c                   add       SQLpad008     MSApad008
020200080108     c                   add       SQLpad009     MSApad009
020300080108     c                   add       SQLpad010     MSApad010
020400080108     c                   add       SQLpad011     MSApad011
020500080108     c                   add       SQLpad012     MSApad012
020600080108     c                   add       SQLpad013     MSApad013
020700080108     c                   add       SQLpad014     MSApad014
020800080108     c                   add       SQLpad015     MSApad015
020900080108     c                   add       SQLpad016     MSApad016
021000080108     c                   add       SQLpad017     MSApad017
021100080108     c                   add       SQLpad018     MSApad018
021200080108     c                   add       SQLpad019     MSApad019
021300080108     c                   add       SQLpad020     MSApad020
021400080108     c                   add       SQLpad021     MSApad021
021500080108     c                   add       SQLpad022     MSApad022
021600080108     c                   add       SQLpad023     MSApad023
021700080108     c                   add       SQLpad024     MSApad024
021800080108     c                   add       SQLpad025     MSApad025
021900080108     c                   add       SQLpad026     MSApad026
022000080108     c                   add       SQLpad027     MSApad027
022100080108     c                   add       SQLpad028     MSApad028
022200080108     c                   add       SQLpad029     MSApad029
022300080108     c                   add       SQLpad030     MSApad030
022400080108     c                   add       SQLpad031     MSApad031
022500080108     c                   add       SQLpad032     MSApad032
022600080108     c                   add       SQLpad033     MSApad033
022700080108     c                   add       SQLpad034     MSApad034
022800080108     c                   add       SQLpad035     MSApad035
022900080108     c                   add       SQLpad036     MSApad036
023000080108     c                   add       SQLpad037     MSApad037
023100080108     c                   add       SQLpad038     MSApad038
023200080108     c                   add       SQLpad039     MSApad039
023300080108     c                   add       SQLpad040     MSApad040
023400080108     c                   add       SQLpad041     MSApad041
023500080108     c                   add       SQLpad042     MSApad042
023600080108     c                   add       SQLpad043     MSApad043
023700080108     c                   add       SQLpad044     MSApad044
023800080108     c                   add       SQLpad045     MSApad045
023900080108     c                   add       SQLpad046     MSApad046
024000080108     c                   add       SQLpad047     MSApad047
024100080108     c                   add       SQLpad048     MSApad048
024200080108     c                   add       SQLpad049     MSApad049
024300080108     c                   add       SQLpad050     MSApad050
024400080108     c                   add       SQLpad051     MSApad051
024500080108     c                   add       SQLpad052     MSApad052
024600080108     c                   add       SQLpad053     MSApad053
024700080108     c                   add       SQLpad054     MSApad054
024800080108     c                   add       SQLpad055     MSApad055
024900080108     c                   add       SQLpad056     MSApad056
025000080108     c                   add       SQLpad057     MSApad057
025100080108     c                   add       SQLpad058     MSApad058
025200080108     c                   add       SQLpad059     MSApad059
025300080108     c                   add       SQLpad060     MSApad060
025400080108     c                   add       SQLtcl        MSAtcl
025500080108     c                   add       SQLcsd        MSAcsd
025600080108     c                   add       SQLccp        MSAccp
025700080108     c                   add       SQLccv        MSAccv
025800080108     c                   add       SQLnls        MSAnls
025900080108     c                   add       SQLnlp        MSAnlp
026000080108      *
026100080108     c                   ENDSR
