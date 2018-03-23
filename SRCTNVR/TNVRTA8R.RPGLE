000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300170103     FTITAS38c  IF   E           k DISK
000400140313     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500140313     D                 DS
000600140313     D  tasaas                 1      4  0
000700140313     D  taslnp                 5      7  0
000800140313     D  tasnrs                 8      9  0
000900140313     D  tasnsp                10     16  0
001000140313     D  sped                   1     16  0
001100140313     d
001200031105
001300050302      *
001400170103     c     *entry        plist
001500170103     c                   parm                    paas              4
001600170103     c                   parm                    pmm               2
001700170103     c
001800170103     c                   movel     paas          kaas              4 0
001900170103     c                   movel     pmm           kmm               2 0
002000170103
002100140312     c     ktas          klist
002200170103     c                   kfld                    kaas
002300170103     c                   kfld                    kmgs              4 0
002400170103     c                   clear                   kfine             4 0
002500170103
002600170103     c                   eval      kmgs=kmm*100+01
002700170103     c                   eval      kfine=kmm*100+31
002800140313
002900140313     C                   TIME                    WTIME            14 0
003000140313     C                   MOVE      WTIME         WDATE             8 0
003100140312     c
003200170103     c                   clear                   kpeso            15 1
003300170103     c                   clear                   kcolli            9 0
003400170103     c                   clear                   medio             7 1
003500170103     c     ktas          setll     titas38c
003600170103     c     kaas          reade     titas38c
003700170103
003800170103     c                   dow       not %eof(titas38c)
003900170103     c                   if        tasmgs>kfine
004000170103     c                   leave
004100170103     c                   endif
004200170103
004300170103     c                   if        taspkc>0 and taspkc<=60
004400170103     c                   eval(h)   kpeso=kpeso+taspkc
004500170103     c                   eval      kcolli=kcolli+tasncp
004600170103     c                   endif
004700170103     c
004800170103     c     kaas          reade     titas38c
004900140312     c                   enddo
005000060228
005100170103     c                   eval(h)   medio=kpeso/kcolli
005200060228
005300140313     c                   except    totale
005400140313
005500050302     C                   EVAL      *inlr = *on
005600140313     OQSYSPRT   E            totale           02
005700170103     o                                              'Peso medio VDL mese '
005800170103     O                       kmm               +  1
005900170103     o                                         +  1 '/'
006000170103     O                       kaas              +  1
006100170103     O                       medio         4   +  4
006200170103     o                                         +  1 'kg'
006300170103     o                                         +  4 'totale peso'
006400170103     o                       kpeso         4   +  1
006500170103     o                                         +  1 'per colli'
006600170103     o                       kcolli        2   +  1
006700170103     o                                         +  4 'Data elaborazione'
006800170103     O                       wdate             +  1 '  /  /    '
