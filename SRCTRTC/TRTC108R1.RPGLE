000100930128      ***********************************************************************
000200010315      *  scrive il log per la trasmissione via Download Ftp.                *
000300930128      ***********************************************************************
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500960528     F*---------------------------------------------------------------------*
000600960528     F*  DATA BASE                                                          *
000700960528     F*---------------------------------------------------------------------*
000800050503     Ftivir02L  IF   E           K DISK
000900010315     FTIVLT00F  O  A E             DISK
001000010315     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001100010315     F                                     PREFIX(f_) usropn
001200010404      *---------------------------------------------------------------------*
001300010315     D dwlprg          s             10    INZ(*all'0')
001400010315     D wrkprg          s              8  0 INZ(*zeros)
001500050901     D MembrCli        s              8    INZ(*all'0')
001600050901     D tipof           s              2    INZ('EO')
001700050901     D key15           s             15
001800050901     D codcli          s              8
001900050901     D msgerr          s                   like(vltmsg)
002000030213      *---------------------------------------------------------------------*
002100050503     D Var_ISV         s                   like(Virfi1)
002200010315     C*---------------------------------------------------------------------*
002300960528     C* Ciclo principale
002400960528     C*---------------------------------------------------------------------*
002500050901     C     *entry        plist
002600050901     C                   parm                    sinfo             1
002700050901     C                   parm                    numfile           7
002800050901     C                   parm                    mbrfile          10
002900050901      *
003000050503     c     kvir02        klist
003100050503     C                   kfld                    virKSC
003200050503     C                   kfld                    virTIP
003300050503      *
003400010315     C                   TIME                    FLD14            14 0
003500010315     C                   MOVE      FLD14         data              8
003600010315     C                   clear                   data8             8
003700010315     C                   eval      data8 = %subst(data:5:4)+%subst(data:3:2)+
003800010315     C                                     %subst(data:1:2)
003900010315     C                   MOVEL     FLD14         ora               6 0
004000050901      * richiamo pgm per reperimento cliente
004100050901     c                   CLEAR                   tiporcv
004200050901     c                   CLEAR                   clisnd
004300050906     c                   CLEAR                   sigsnd
004400050901     c                   call      'TRTC100AR'
004500050901     c                   parm      'S'           sndrcv            1
004600050901     c                   parm                    tiporcv           4
004700050901     c                   parm                    clisnd            7
004800050906     c                   parm                    sigsnd            2
004900030213      * ----------------
005000010427      * reperisce il codice cliente di SEND es.Sernam <M3300011__>
005100050901      * esamina il parametro passato per stabilire se ambiente di TEST
005200050901     c                   if        sinfo <> 'P'
005300051111     C                   eval      MembrCli = '0EDI0OUT'
005400050901     C                   eval      codcli = '0' + clisnd
005500050901     c                   else
005600051111     C                   eval      MembrCli = 'PEDI0OUT'
005700050901     C                   eval      codcli = 'P' + clisnd
005800050901     c                   endif
005900051214      **
006000050901      *prende numeratore strategi
006100050901     C                   exsr      calprog
006200050901
006300050503     C                   MOVEL     CODCLI        virKSC
006400050503     C                   MOVEL     TipoF         virTIP
006500050503     c     kvir02        chain     tivir02l
006600051215     c                   move      'OF'          Var_ISV
006700110406      ***** essendo il 0EDI0OUT sempre e solo "OF"
006800050503     c                   if        %Found(tivir02l)
006900110406     C                   if          MembrCli <> '0EDI0OUT' and
007000110406     C                               MembrCli <> 'PEDI0OUT'
007100110406     c                   move      VirFI1        Var_ISV
007200110406     C                   End
007300050503     C                   End
007400010404      *
007500010404      * Se c'è allora può scriverlo per il Download.
007600010315     C                   CLEAR                   TIVLT000
007700051110     C****               MOVEL     CODCLI        vltKSC
007800051110     C****               MOVEL     CODCLI        vltKSU
007900051110     C                   MOVEL     '0EDI0OUT'    vltKSC
008000051110     C                   MOVEL     '0EDI0OUT'    vltKSU
008100010427     C                   MOVEL     TipoF         vltTIP
008200060522     C                   MOVEL     'TISNDEDI'    vltFLD
008300050901      * determina nome membro
008400050901     c                   select
008500050901      * IFCSUM
008600050901     c                   when      tiporcv = 'TT00'
008700050906     c                   eval      vltmbd = sigsnd + 'U' + numfile
008800050901      * IFTSTA
008900050901     c                   when      tiporcv = 'ST00'
009000050906     c                   eval      vltmbd = sigsnd + 'A' + numfile
009100050901     c                   endsl
009200050901     C
009300010406     C                   MOVEL     'D'           vltSND
009400050901      * gestione errore anagrafico
009500050901     c                   if        msgerr <> *blank
009600050901     c                   movel(p)  msgerr        vltmsg
009700050901     c                   move      2             vltsts
009800050901     c                   else
009900010315     C                   MOVE      0             vltSTS
010000050901     c                   endif
010100010315     C                   MOVE      Data8         vltDAT
010200010315     C                   MOVE      Ora           vltORA
010300050503     C                   MOVEL     Var_ISV       vltISV
010400010315     C                   MOVEL     'F'           vltTIA
010500010315     C                   MOVEL     '01'          vltFL2
010600050912     C                   MOVEL     '*'           vltFLS
010700050912     C                   MOVEL     '*'           vltMBS
010800010315     C                   MOVEL     dwlprg        vltPRG
010900010315      *
011000010315     C                   WRITE     TIVLT000
011100010315     C*
011200050901     C                   MOVEL     vltmbd        mbrfile
011300960830     C                   SETON                                        LR
011400960927     C*------------------------------------------------------*
011500010404     C*  prepara il file
011600010404     C*------------------------------------------------------*
011700010315     C     calprog       begsr
011800010315     C*
011900010315     C                   open      tis7prgf
012000010315     C                   read(e)   tis7prgf
012100010315     C                   if        not %error
012200010315     C                   eval      dwlprg = f_tis7prgf
012300010315     C*
012400010315     C                   move(p)   dwlprg        wrkprg
012500010315     C                   add       1             wrkprg
012600010315     C                   move(p)   wrkprg        dwlprg
012700051215     C                   movel     'OF'          dwlprg
012800010315     C*
012900010315     C                   eval      f_tis7prgf = dwlprg
013000010315     C                   update    tis7prg0
013100010315     C                   endif
013200010315     C                   close     tis7prgf
013300010315     C*
013400010315     C                   endsr
