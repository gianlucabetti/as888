000100011113     fTiTAS38c  IF   E           K DISK
000200011113     F                                     IGNORE(TITAS000)
000300011113     F                                     IGNORE(TITASP00)
000400011113     ftiTAS00f  UF a E           K DISK
000500011112      *
000600011113     c     ktas          klist
000700011113     c                   kfld                    tasaas
000800011113     c                   kfld                    tasmgs
000900011113      *
001000011113     c                   eval      tasaas = 2001
001100011113     c                   eval      tasmgs = 1000
001200011113      *
001300011113     c     ktas          setll     titas38c
001400011113      *
001500011113     c                   do        *hival
001600011113      *
001700011113     c                   read      titas38c
001800011113      *
001900011113     c                   if        %eof(titas38c)
002000011113     c                   leave
002100011113     c                   endif
002200011112      *
002300011112      * se lnp <> 180,anno spedizione = 2001, spedizioni di ottobre , imponibile > 0
002400011112      *    peso CML maggiore del peso fatturato  oppure numero colli peso rilevato = totale colli
002500011112     c                   if        taslnp <> 180 and tasaas = 2001 and
002600011112     c                             tasmgs > 1001 and tasmgs < 1031 and
002700011112     c                             tasimv > 0 and ((taspkc > taspkf) or
002800011113     c                             (taspkc < taspkf and tasncp = tasncl))
002900011113      *
003000011113     c                   clear                   tasfic
003100011113     c                   clear                   tasfts
003200011113     c                   clear                   tasdiv
003300011113     c                   clear                   taspor
003400011113     c                   clear                   tassv1
003500011113     c                   clear                   tasva1
003600011113     c                   clear                   tassv2
003700011113     c                   clear                   tasva2
003800011113     c                   clear                   tassv3
003900011113     c                   clear                   tasva3
004000011113     c                   clear                   tasimv
004100011113     c                   clear                   tasdft
004200011113     c                   clear                   tasnft
004300011113     c                   clear                   tasfiv
004400011113     c                   if        tasfcm = 'F'
004500011113     c                   clear                   tasias
004600011113     c                   clear                   tasvas
004700011113     c                   clear                   tasfcm
004800011113     c                   endif
004900011113      * sposto il peso cml nel peso da fatturare
005000011113     c                   z-add     taspkc        taspkf
005100011113      *
005200011113     c                   write     titas000
005300011113      *
005400011113     c                   endif
005500011113      *
005600011113     c                   enddo
005700011113      *
005800011113     c                   seton                                        lr
