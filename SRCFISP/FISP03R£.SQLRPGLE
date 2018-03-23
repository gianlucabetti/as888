000100060918      //********************************************************************************************
000200060918      //
000300060918      // Definizione variabili.
000400060918      //
000500060918      //********************************************************************************************
000600061025     FFNfgw01l  IF   E           k DISK
000700061017     D Profilo         S             10
000800061031     D IDdispositivo   S             35
000900061017     D RTNesito        S             10I 0
001000061017     D RTNopcode       S             10
001100061017     D RTNstatus       S             10I 0
001200061025     D RIC_Fog         S             10I 0 inz
001300061025     D RIC_Lna         S             10I 0 inz
001400061025
001500061025     D DSLna           DS                  occurs(5000) inz
001600061025     D  DSFV_tipo_a                   1s 0
001700061025     D  DSFV_nume_a                   9s 0
001800061025     D  DSFV_lna                      3s 0
001900061025
002000061025     d $FFV            s              3    dim(300) inz(*zeros)
002100061025     D ddatiute      e ds
002200061025     D azuteds       e ds                  extname(AZUTE00F)
002300061025     D tibs34ds      E DS                  inz
002400061025     d fnfgvds       e ds                  extname(fnfgv00f)
002500061025     d xx              s              3  0 inz
002600060918      //********************************************************************************************
002700060918      //
002800060918      // Definizione parametri procedura.
002900060918      //
003000060918      //********************************************************************************************
003100060918     C     *ENTRY        PLIST
003200061017     C                   PARM                    Profilo
003300061031     C                   PARM                    IDdispositivo
003400061017     C                   PARM                    rtnesito
003500061017     C                   PARM                    rtnopcode
003600061017     C                   PARM                    rtnstatus
003700061025     c*
003800061025     c     K02FGW01      klist
003900061025     c                   kfld                    FGVnfv
004000061025     c                   kfld                    FGVlnp
004100060918      /FREE
004200060918
004300060918       //*******************************************************************************************
004400060918       //
004500060918       // Main.
004600060918       //
004700060918       //*******************************************************************************************
004800061017       CLEAR rtnesito;
004900061017       CLEAR rtnopcode;
005000061017       CLEAR rtnstatus;
005100061026       *INLR = *ON;
005200060918
005300060918      /END-FREE
005400061025
005500061025     c     *dtaara       define    §azute        azuteds
005600061025     c     *dtaara       define    §datiute      ddatiute
005700061025
005800061025     c                   IF        Profilo = *blanks
005900061025     c                   eval      RTNstatus = 99
006000061025     c                   eval      RTNopcode = 'ERRORE '
006100061025     c                   eval      RTNesito  = -1
006200061025     c                   return
006300061025     c                   ENDIF
006400061025      * verifico se reperire dtaara: a cambio profilo, se non presente o
006500061025      *  vuota
006600061025     C                   in(E)     *dtaara
006700070207     C                   IF        %subst(PROFILO:1:5) <> DUTute  or
006800061025     C                             %Error  or  RSUT = *blanks
006900070207     c                   eval      i34ute=%subst(PROFILO:1:5)
007000061025     C                   call      'TIBS34R'
007100061025     C                   parm                    Tibs34Ds
007200061025     C                   in        *dtaara
007300061025     c                   ENDIF
007400061025      *-- Verifica errori
007500070209     c                   IF        DUTERR = 'E' or simfel=0
007600061025     c                   eval      RTNstatus = 99
007700061025     c                   eval      RTNopcode = 'ERRORE '
007800061025     c                   eval      RTNesito  = -1
007900070209     c                   return
008000061025     c                   ENDIF
008100061025     c*
008200061026     C/EXEC SQL
008300070116     C+ DECLARE FOGLI CURSOR FOR
008400061026     C+ SELECT FVVNPG, FVVNFV, FVVDSF
008500061026     C+ FROM FNFVV00F
008600061026     C+ WHERE fvvfsc = 'S' and fvvatb<>'*' and fvvfcf<>'S' and
008700061026     C+ (fvvfgs = :dutpou or (fvvfgs=:simfel and fvvnpg='3' and fvvspg='P')
008800061026     c+ )
008900061026     c+ union
009000061026     c+ SELECT '1' as CATEG_FOGLIO, fgvnfv, orgdes
009100061026     c+ from fnfgv00f join azorg00f on fgvlna=orgfil
009200061027     c+ where fgvlnp=:simfel and fgvttr <>'I' and fgvatb<>'*' and fgvfcf=' '
009300061026     C+ FOR READ ONLY
009400061026     C/END-EXEC
009500061026     C
009600061026     C/EXEC SQL
009700061026     C+ OPEN FOGLI
009800061026     C/END-EXEC
009900061027
010000061027      /FREE
010100061027
010200061027       select;
010300061027       When sqlCode = 100;
010400061027         RTNstatus = sqlCode;
010500061027         RTNopcode = 'SQLCODE';
010600061027         RTNesito  = -1;
010700061027         RETURN;
010800061027       When sqlCode < 0;
010900061027         RTNstatus = sqlCode;
011000061027         RTNopcode = 'SQLCODE';
011100061027         RTNesito  = -1;
011200061027         RETURN;
011300061027       Endsl;
011400061027
011500061027      /END-FREE
011600061025     c                   exsr      sr_sqlfgv
011700061025
011800060918     C/EXEC SQL
011900061026     C+ SET RESULT SETS FOR RETURN TO CLIENT
012000061027     c+                 array :DSlna  for :RIC_lna rows,
012100061027     C+                 cursor fogli
012200061026     C/END-EXEC
012300060918      /FREE
012400061017
012500061016       IF sqlCode < 0;
012600061017         RTNstatus = sqlCode;
012700061017         RTNopcode = 'SQLCODE';
012800061017         RTNesito  = -1;
012900060918         RETURN;
013000060918       ENDIF;
013100060918
013200060918       RETURN;
013300060918
013400060918      /END-FREE
013500061025     c**********************************************************************
013600061025     c     Sr_Sqlfgv     begsr
013700061027     C/EXEC SQL
013800061027     C+ DECLARE FGV CURSOR FOR
013900061027     C+ SELECT fgvlnp, fgvnfv, fgvlna, fgvffv, fgvff2
014000061027     c+ from fnfgv00f
014100061027     c+ where fgvlnp=:simfel and fgvttr <>'I' and fgvatb<>'*' and fgvfcf=' '
014200061027     C+ FOR READ ONLY
014300061027     C/END-EXEC
014400061025      /FREE
014500061025
014600061025       select;
014700061025       When sqlCode = 100;
014800061025         RTNstatus = sqlCode;
014900061025         RTNopcode = 'SQLCODE';
015000061025         RTNesito  = -1;
015100061025         RETURN;
015200061025       When sqlCode < 0;
015300061025         RTNstatus = sqlCode;
015400061025         RTNopcode = 'SQLCODE';
015500061025         RTNesito  = -1;
015600061025         RETURN;
015700061025       Endsl;
015800061025
015900061025      /END-FREE
016000061027
016100061025     C/EXEC SQL
016200061027     C+ Open FGV
016300061025     C/END-EXEC
016400061025     c                   Do        *hival                                       *record letto
016500061025     C/EXEC SQL
016600061027     C+ FETCH NEXT FROM FGV INTO :fgvlnp, :fgvnfv, :fgvlna, :fgvffv,
016700061025     C+ :fgvff2
016800061025     C/END-EXEC
016900061025     c                   select
017000061025     c                   when      sqlcod=100
017100061025     c                   leave
017200061025     c                   when      sqlcod< 0
017300061025      /FREE
017400061025         RTNstatus = sqlCode;
017500061025         RTNopcode = 'SQLCODE';
017600061025         RTNesito  = -1;
017700061025         RETURN;
017800061025      /END-FREE
017900061025     c                   other
018000061025     c                   reset                   $FFV
018100061025     c                   reset                   XX
018200061025     c* memorizzo anche le lna
018300061025     c     K02FGW01      chain     FNFGW000
018400061025if  3c                   if        not %found(FNFGW01L)
018500061025     c                             or  FGWATB <> *blank
018600061025     c                   clear                   FGWff3
018700061025     c                   clear                   FGWff4
018800061025e   3c                   endif
018900061025     c                   move      FGVlna        $FFV(1)
019000061025     c                   movea     FGVffv        $FFV(2)
019100061025     c                   movea     FGVff2        $FFV(82)
019200061025     c                   movea     FGWff3        $FFV(151)
019300061025     c                   movea     FGWff4        $FFV(231)
019400061025     c*
019500061025do  1c                   DOW       xx       < %elem($FFV)
019600061025     c                   add       1             xx
019700061025if  2c                   if        $FFV(xx) > *zeros
019800061025     c                   add       1             ric_lna
019900061025     c     ric_lna       occur     dslna
020000061026     c                   z-add     1             dsfv_tipo_a
020100061026     c                   z-add     fgvnfv        dsfv_nume_a
020200061025     c                   move      $FFV(xx)      dsfv_lna
020300061025e   2c                   endif
020400061025e   1c                   ENDDO
020500061025     c                   endsl
020600061025
020700061025     c                   EndDo
020800061025     c*
020900061025
021000061025     C/EXEC SQL
021100061027     C+ close FGV
021200061025     C/END-EXEC
021300061025
021400061025     c                   endsr
