000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140805     ftitas26c  if   e           k disk
000300140805     ftfnlvp11l if   e           k disk
000400140805     ffiar531c  uf   e           k disk
000500140805      *
000600140805     d kpjba         e ds
000700140324     d dar5gen       e ds
000800140806     D wpda            S              1
000900140805      *
001000140805     Ifiar5000      50
001100140805     Ifiar5p00      51
001200000607      *--------------------------------------------------------------------
001300040409      * lettura
001400140806     c     20140402      setll     titas26c
001500000607     c                   do        *hival
001600140805     c                   read      titas26c
001700140805     c                   if        %eof(titas26c)
001800000607     c                   leave
001900000607     c                   end
002000140806      *fino a luglio
002100140806     c                   if        tasdcm = 20140601
002200140806     c                   leave
002300140806     c                   end
002400140806      *
002500140805     c                   if        tasndc = 9999999
002600140805     c                   iter
002700140805     c                   end
002800140806      *
002900140805     c                   z-add     tasndc        wtasndc
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
005400140805      *
005500000607
005600051130     c                   enddo
005700000607     c                   seton                                        lr
005800140218      *-----------------------------------------------
005900140218     c     *inzsr        begsr
006000140218      *-----------------------------------------------
006100140218     c
006200140805     C     *ENTRY        PLIST
006300140805     C                   PARM                    KPJBA
006400140324     c     kar531        klist
006500140805     c                   kfld                    tasAAS
006600140805     c                   kfld                    tasLNP
006700140805     c                   kfld                    tasNRS
006800140805     c                   kfld                    tasNSP
006900140324     c                   kfld                    wTRD
007000140324     c                   move      'GEN'         wtrd              3
007100140324      *
007200140805     c     ktfnlv        klist
007300140805     c                   kfld                    tasAAS
007400140805     c                   kfld                    tasLNP
007500140805     c                   kfld                    tasNRS
007600140805     c                   kfld                    tasNSP
007700140805     c                   kfld                    tasddc
007800140805     c                   kfld                    wtasndc           6 0
007900140805     c                   kfld                    taspdc
008000140218     c                   endsr
