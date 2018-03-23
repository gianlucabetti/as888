000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140805     ftitas26c  if   e           k disk
000300140805     ftfnlvp11l if   e           k disk
000400140805     ffidst01l  if   e           k disk    usropn
000500140805     ffiar531c  uf   e           k disk
000600140805      *
000700140805     d kpjba         e ds
000800140324     d dar5gen       e ds
000900140805     d ddstflr       e ds
001000140805     D wpda            S              1                                         QCAEXEC
001100140805     D Cmd             S             48                                         QCAEXEC
001200140805     D Cmd1            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
001300140805     D Cmd2            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
001400140805      *
001500140805     Ifiar5000      50
001600140805     Ifiar5p00      51
001700000607      *--------------------------------------------------------------------
001800040409      * lettura
001900140806     c     20140701      setll     titas26c
002000000607     c                   do        *hival
002100140805     c                   read      titas26c
002200140805     c                   if        %eof(titas26c)
002300000607     c                   leave
002400000607     c                   end
002500140805     c                   if        tasndc = 9999999
002600140805     c                   iter
002700140805     c                   end
002800140805     c                   z-add     tasndc        wtasndc
002900140805     c                   if        tasdcm < 20140801
003000140805     c     ktfnlv        chain     tfnlvp11l
003100140805     c                   setoff                                       5051
003200140324     c     kar531        chain     fiar531c
003300140324     c                   if        %found(fiar531c)
003400140805     c                   if        %found(tfnlvp11l)
003500140324     c                   move      ar5uni        dar5gen
003600140805     c                   if        §ar5pdaco = *blank
003700140805     c                   iter
003800140805     c                   endif
003900140805     c                   move      *blank        §ar5pdaco
004000140324     c                   movel     dar5gen       ar5uni
004100140805     C   50              UPDATE    fiar5000
004200140805     C   51              UPDATE    fiar5p00
004300140805     c                   else
004400140805     c                   move      ar5uni        dar5gen
004500140805     c                   if        §ar5pdaco = 'NO'
004600140805     c                   iter
004700140805     c                   endif
004800140805     c                   move      'NO'          §ar5pdaco
004900140805     c                   movel     dar5gen       ar5uni
005000140805     C   50              UPDATE    fiar5000
005100140805     C   51              UPDATE    fiar5p00
005200051130     c                   end
005300140324     c                   end
005400140805      * da fidst
005500140805     c                   else
005600140805     c                   movel     taspdr        kfgs
005700140805     c     kdst          chain     fidst01l
005800140805     c                   if        %found(fidst01l)
005900140805     c                   movel     dstflr        ddstflr
006000140805     c                   eval      wpda = 'N'
006100140805     c                   if        §DSTTSTPDA = *blank and
006200140805     c                             (dstpda = 'C' or dstpda = 'E') or
006300140805     c                             §DSTTSTPDA = 'O' and
006400140805     c                             (dstpda = 'C' or dstpda = 'E')
006500140805     c                   eval      wpda = 'S'
006600140805     c                   endif
006700140805     c                   setoff                                       5051
006800140805     c     kar531        chain     fiar531c
006900140805     c                   if        %found(fiar531c)
007000140805     c                   if        wpda = 'S'
007100140805     c                   move      ar5uni        dar5gen
007200140805     c                   if        §ar5pdaco = *blank
007300140805     c                   iter
007400140805     c                   endif
007500140805     c                   move      *blank        §ar5pdaco
007600140805     c                   movel     dar5gen       ar5uni
007700140805     C   50              UPDATE    fiar5000
007800140805     C   51              UPDATE    fiar5p00
007900140805     c                   else
008000140805     c                   move      ar5uni        dar5gen
008100140805     c                   if        §ar5pdaco = 'NO'
008200140805     c                   iter
008300140805     c                   endif
008400140805     c                   move      'NO'          §ar5pdaco
008500140805     c                   movel     dar5gen       ar5uni
008600140805     C   50              UPDATE    fiar5000
008700140805     C   51              UPDATE    fiar5p00
008800140805     c                   end
008900140805     c                   end
009000140805     c                   end
009100140805     c                   end
009200140805      *
009300000607
009400051130     c                   enddo
009500000607     c                   seton                                        lr
009600140805      *---------------------------------------------------------------------
009700140805     C     comando       BEGSR
009800140805      *---------------------------------------------------------------------
009900140805     c                   if        %subst(knsif:7:1) = 'P'
010000140805     C                   movea     cmd1          cmd
010100140805     c                   else
010200140805     C                   movea     cmd2          cmd
010300140805     c                   endif
010400140805     C                   movel(p)  cmd           comman
010500140805     C                   call      'QCMDEXC'
010600140805     C                   parm                    comman           80
010700140805     C                   parm      48            lung             15 5
010800140805     C*
010900140805     C                   ENDSR
011000140218      *-----------------------------------------------
011100140218     c     *inzsr        begsr
011200140218      *-----------------------------------------------
011300140218     c
011400140805     C     *ENTRY        PLIST
011500140805     C                   PARM                    KPJBA
011600140324     c     kar531        klist
011700140805     c                   kfld                    tasAAS
011800140805     c                   kfld                    tasLNP
011900140805     c                   kfld                    tasNRS
012000140805     c                   kfld                    tasNSP
012100140324     c                   kfld                    wTRD
012200140324     c                   move      'GEN'         wtrd              3
012300140324      *
012400140805     c     ktfnlv        klist
012500140805     c                   kfld                    tasAAS
012600140805     c                   kfld                    tasLNP
012700140805     c                   kfld                    tasNRS
012800140805     c                   kfld                    tasNSP
012900140805     c                   kfld                    tasddc
013000140805     c                   kfld                    wtasndc           6 0
013100140805     c                   kfld                    taspdc
013200140805     c     kdst          klist
013300140805     c                   kfld                    knpg              1 0
013400140805     c                   kfld                    wtasndc
013500140805     c                   kfld                    kfgs              3 0
013600140805     c                   z-add     4             knpg
013700140805     c                   exsr      comando
013800140805     c                   open      fidst01l
013900140218     c                   endsr
014000140805      *------------------------------------------------------
014100140805**         CMD1
014200140805OVRDBF FILE(Fidst01l) TOFILE(FILTRAprd/Fidst01l)
014300140805**         CMD2
014400140805OVRDBF FILE(Fidst01l) TOFILE(FILTRA201/Fidst01l)
