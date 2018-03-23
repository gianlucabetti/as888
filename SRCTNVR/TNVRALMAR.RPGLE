000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500060519      *       AGGIORNA TITAI                                          *
000600031105      *                                                               *
000700031105      *****************************************************************
000800140325     FTITAS30c  uF   E           K DISK
000900140324     Falma_c5   iF   E             DISK    rename(alma_c5:alma000)
001000140324     fQSYSPRT   O    F  132        PRINTER  oflind(*INOF)
001100140327     fQSYSPRT1  O    F  132        PRINTER  oflind(*INOe)
001200140324     f
001300140324     ITITAS000      02
001400140324     ITITAS010      03
001500140324     ITITASP00      04
001600031105
001700060519     c     KSPE          Klist
001800140324     c                   Kfld                    aas
001900140324     c                   Kfld                    lnp
002000140324     c                   Kfld                    nrs
002100140324     c                   Kfld                    nsp
002200050302      *
002300060228     c                   do        *hival
002400140324     c                   read      alma_c5
002500060228
002600140324     c                   if        %eof(alma_c5)
002700060228     c                   leave
002800060228     c                   endif
002900060228
003000140324     c                   setoff                                       020304
003100140324     c     KSPE          CHAIN     TITAs30c
003200140324     c                   IF        %found(titas30c)
003300060228
003400140324     c                   select
003500140324     c                   when      taslna<>lna or tasrmn<>tasrmn
003600140324     c                   except    errore1
003700140324     c                   add       1             err1              7 0
003800140324     c                   when      tasccm<>2552852 and tasccm<>2552853
003900140324     c                   except    errore2
004000140324     c                   add       1             err2              7 0
004100140324     c                   when      tasgma<>'  '
004200140324     c                   except    errore3
004300140324     c                   add       1             err3              7 0
004400140324     c                   other
004500140324     c* aggiorno
004600140324     c                   add       1             aggiorna          7 0
004700140325     c                   if        tasccm=2552852
004800140325     c                   eval      tasgma='C5'
004900140325     c                   endif
005000140325     c                   if        tasccm=2552853
005100140325     c                   eval      tasgma='C4'
005200140325     c                   endif
005300140325     c   02              update    titas000
005400140325     c   03              update    titas010
005500140324     c
005600140324     c                   endsl
005700140325     c                   else
005800140325     c                   except    errore4
005900140325     c                   add       1             err4              7 0
006000140325     c*
006100140324     c                   endif
006200060228
006300060228     c                   enddo
006400140324     c
006500140324     c                   except    totag
006600060228
006700050302     C                   EVAL      *inlr = *on
006800140324     OQSYSPRT   E            errore1     1
006900140324     O                                        +   0 'Bolla con altro'
007000140324     O                       tasaas           +   2
007100140324     O                       taslnp           +   2
007200140324     O                       tasnrs        z  +   2
007300140324     O                       tasnsp        z  +   2
007400140324     O                       tasccm           +   2
007500140324     o                       taslna           +   2
007600140324     o                       tasrmn        z  +   2
007700140324     o                                        +   2 'lna'
007800140324     o                       lna              +   2
007900140324     o                                        +   2 'RMN'
008000140324     o                       rmn           z  +   2
008100140324     OQSYSPRT   E            errore2     1
008200140324     O                                        +   0 'Bolla con altro'
008300140324     O                       tasaas           +   2
008400140324     O                       taslnp           +   2
008500140324     O                       tasnrs        z  +   2
008600140324     O                       tasnsp        z  +   2
008700140324     O                       tasccm           +   2
008800140324     o                                        +   2 'CCM'
008900140324     OQSYSPRT   E            errore3     1
009000140324     O                                        +   0 'Bolla con altro'
009100140324     O                       tasaas           +   2
009200140324     O                       taslnp           +   2
009300140324     O                       tasnrs        z  +   2
009400140324     O                       tasnsp        z  +   2
009500140324     O                       tasccm           +   2
009600140324     o                                        +   2 'GMA'
009700140324     O                       tasgma           +   2
009800140324     OQSYSPRT   E            totag       1
009900140324     O                                        +   0 'totale aggiornati'
010000140324     O                       aggiorna      z  +   2
010100140324     O                                        +   2 'err LNA o RMN'
010200140324     O                       err1          z  +   2
010300140324     O                                        +   2 'CCM'
010400140324     O                       err2          z  +   2
010500140324     O                                        +   2 'già GMA'
010600140324     O                       err3          z  +   2
010700140325     O                                        +   2 'inesistenti'
010800140325     O                       err4          z  +   2
010900140327     OQSYSPRT1  E            errore4     1
011000140327     O                                        +   0 'Bolla inesistente'
011100140327     O                       aas              +   2
011200140327     O                       lnp              +   2
011300140327     O                       nrs           z  +   2
011400140327     O                       nsp           z  +   2
011500140327     o                                        +   2 'LNA'
011600140327     O                       lna              +   2
011700140327     o                                        +   2 'RMN'
011800140327     O                       rmn              +   2
