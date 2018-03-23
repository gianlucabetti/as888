000100080605     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700061025     FFNfgw01l  IF   E           k DISK
000800061017     D Profilo         S             10
000900061031     D IDdispositivo   S             35
001000061017     D RTNesito        S             10I 0
001100061017     D RTNopcode       S             10
001200061017     D RTNstatus       S             10I 0
001300061025     D RIC_Fog         S             10I 0 inz
001400061025     D RIC_Lna         S             10I 0 inz
001500061025
001600061025     D DSLna           DS                  occurs(5000) inz
001700061025     D  DSFV_tipo_a                   1s 0
001800061025     D  DSFV_nume_a                   9s 0
001900061025     D  DSFV_lna                      3s 0
002000061025
002100061025     d $FFV            s              3    dim(300) inz(*zeros)
002200061025     D ddatiute      e ds
002300061025     D azuteds       e ds                  extname(AZUTE00F)
002400061025     D tibs34ds      E DS                  inz
002500061025     d fnfgvds       e ds                  extname(fnfgv00f)
002600061025     d xx              s              3  0 inz
002700060918      //********************************************************************************************
002800060918      //
002900060918      // Definizione parametri procedura.
003000060918      //
003100060918      //********************************************************************************************
003200060918     C     *ENTRY        PLIST
003300061017     C                   PARM                    Profilo
003400061031     C                   PARM                    IDdispositivo
003500061017     C                   PARM                    rtnesito
003600061017     C                   PARM                    rtnopcode
003700061017     C                   PARM                    rtnstatus
003800061025     c*
003900061025     c     K02FGW01      klist
004000061025     c                   kfld                    FGVnfv
004100061025     c                   kfld                    FGVlnp
004200060918      /FREE
004300060918
004400060918       //*******************************************************************************************
004500060918       //
004600060918       // Main.
004700060918       //
004800060918       //*******************************************************************************************
004900061017       CLEAR rtnesito;
005000061017       CLEAR rtnopcode;
005100061017       CLEAR rtnstatus;
005200061026       *INLR = *ON;
005300080605
005400080605       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005500060918
005600060918      /END-FREE
005700061025
005800061025     c     *dtaara       define    §azute        azuteds
005900061025     c     *dtaara       define    §datiute      ddatiute
006000061025
006100061025     c                   IF        Profilo = *blanks
006200061025     c                   eval      RTNstatus = 99
006300061025     c                   eval      RTNopcode = 'ERRORE '
006400061025     c                   eval      RTNesito  = -1
006500061025     c                   return
006600061025     c                   ENDIF
006700061025      * verifico se reperire dtaara: a cambio profilo, se non presente o
006800061025      *  vuota
006900061025     C                   in(E)     *dtaara
007000070207     C                   IF        %subst(PROFILO:1:5) <> DUTute  or
007100061025     C                             %Error  or  RSUT = *blanks
007200070207     c                   eval      i34ute=%subst(PROFILO:1:5)
007300061025     C                   call      'TIBS34R'
007400061025     C                   parm                    Tibs34Ds
007500061025     C                   in        *dtaara
007600061025     c                   ENDIF
007700061025      *-- Verifica errori
007800070209     c                   IF        DUTERR = 'E' or simfel=0
007900061025     c                   eval      RTNstatus = 99
008000061025     c                   eval      RTNopcode = 'ERRORE '
008100061025     c                   eval      RTNesito  = -1
008200070209     c                   return
008300061025     c                   ENDIF
008400061025     c*
008500061026     C/EXEC SQL
008600070116     C+ DECLARE FOGLI CURSOR FOR
008700061026     C+ SELECT FVVNPG, FVVNFV, FVVDSF
008800061026     C+ FROM FNFVV00F
008900061026     C+ WHERE fvvfsc = 'S' and fvvatb<>'*' and fvvfcf<>'S' and
009000061026     C+ (fvvfgs = :dutpou or (fvvfgs=:simfel and fvvnpg='3' and fvvspg='P')
009100061026     c+ )
009200061026     c+ union
009300061026     c+ SELECT '1' as CATEG_FOGLIO, fgvnfv, orgdes
009400061026     c+ from fnfgv00f join azorg00f on fgvlna=orgfil
009500061027     c+ where fgvlnp=:simfel and fgvttr <>'I' and fgvatb<>'*' and fgvfcf=' '
009600061026     C+ FOR READ ONLY
009700061026     C/END-EXEC
009800061026     C
009900061026     C/EXEC SQL
010000061026     C+ OPEN FOGLI
010100061026     C/END-EXEC
010200061027
010300061027      /FREE
010400061027
010500061027       select;
010600061027       When sqlCode = 100;
010700061027         RTNstatus = sqlCode;
010800061027         RTNopcode = 'SQLCODE';
010900061027         RTNesito  = -1;
011000061027         RETURN;
011100061027       When sqlCode < 0;
011200061027         RTNstatus = sqlCode;
011300061027         RTNopcode = 'SQLCODE';
011400061027         RTNesito  = -1;
011500061027         RETURN;
011600061027       Endsl;
011700061027
011800061027      /END-FREE
011900061025     c                   exsr      sr_sqlfgv
012000061025
012100060918     C/EXEC SQL
012200061026     C+ SET RESULT SETS FOR RETURN TO CLIENT
012300061027     c+                 array :DSlna  for :RIC_lna rows,
012400061027     C+                 cursor fogli
012500061026     C/END-EXEC
012600060918      /FREE
012700061017
012800061016       IF sqlCode < 0;
012900061017         RTNstatus = sqlCode;
013000061017         RTNopcode = 'SQLCODE';
013100061017         RTNesito  = -1;
013200060918         RETURN;
013300060918       ENDIF;
013400060918
013500060918       RETURN;
013600060918
013700060918      /END-FREE
013800061025     c**********************************************************************
013900061025     c     Sr_Sqlfgv     begsr
014000061027     C/EXEC SQL
014100061027     C+ DECLARE FGV CURSOR FOR
014200061027     C+ SELECT fgvlnp, fgvnfv, fgvlna, fgvffv, fgvff2
014300061027     c+ from fnfgv00f
014400061027     c+ where fgvlnp=:simfel and fgvttr <>'I' and fgvatb<>'*' and fgvfcf=' '
014500061027     C+ FOR READ ONLY
014600061027     C/END-EXEC
014700061025      /FREE
014800061025
014900061025       select;
015000061025       When sqlCode = 100;
015100061025         RTNstatus = sqlCode;
015200061025         RTNopcode = 'SQLCODE';
015300061025         RTNesito  = -1;
015400061025         RETURN;
015500061025       When sqlCode < 0;
015600061025         RTNstatus = sqlCode;
015700061025         RTNopcode = 'SQLCODE';
015800061025         RTNesito  = -1;
015900061025         RETURN;
016000061025       Endsl;
016100061025
016200061025      /END-FREE
016300061027
016400061025     C/EXEC SQL
016500061027     C+ Open FGV
016600061025     C/END-EXEC
016700061025     c                   Do        *hival                                       *record letto
016800061025     C/EXEC SQL
016900061027     C+ FETCH NEXT FROM FGV INTO :fgvlnp, :fgvnfv, :fgvlna, :fgvffv,
017000061025     C+ :fgvff2
017100061025     C/END-EXEC
017200061025     c                   select
017300061025     c                   when      sqlcod=100
017400061025     c                   leave
017500061025     c                   when      sqlcod< 0
017600061025      /FREE
017700061025         RTNstatus = sqlCode;
017800061025         RTNopcode = 'SQLCODE';
017900061025         RTNesito  = -1;
018000061025         RETURN;
018100061025      /END-FREE
018200061025     c                   other
018300061025     c                   reset                   $FFV
018400061025     c                   reset                   XX
018500061025     c* memorizzo anche le lna
018600061025     c     K02FGW01      chain     FNFGW000
018700061025if  3c                   if        not %found(FNFGW01L)
018800061025     c                             or  FGWATB <> *blank
018900061025     c                   clear                   FGWff3
019000061025     c                   clear                   FGWff4
019100061025e   3c                   endif
019200061025     c                   move      FGVlna        $FFV(1)
019300061025     c                   movea     FGVffv        $FFV(2)
019400061025     c                   movea     FGVff2        $FFV(82)
019500061025     c                   movea     FGWff3        $FFV(151)
019600061025     c                   movea     FGWff4        $FFV(231)
019700061025     c*
019800061025do  1c                   DOW       xx       < %elem($FFV)
019900061025     c                   add       1             xx
020000061025if  2c                   if        $FFV(xx) > *zeros
020100061025     c                   add       1             ric_lna
020200061025     c     ric_lna       occur     dslna
020300061026     c                   z-add     1             dsfv_tipo_a
020400061026     c                   z-add     fgvnfv        dsfv_nume_a
020500061025     c                   move      $FFV(xx)      dsfv_lna
020600061025e   2c                   endif
020700061025e   1c                   ENDDO
020800061025     c                   endsl
020900061025
021000061025     c                   EndDo
021100061025     c*
021200061025
021300061025     C/EXEC SQL
021400061027     C+ close FGV
021500061025     C/END-EXEC
021600061025
021700061025     c                   endsr
