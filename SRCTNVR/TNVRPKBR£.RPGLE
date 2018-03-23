000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300110922     fFNblp31l  iF   E           K DISK    rename(fnblp000:fnblp031)
000400110923     fFNblp01l  uF   E           K DISK
000500110923     fFNarb01l  uF   E           K DISK
000600110923     ftitas30c  uF   E           K DISK
000700080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800080320     d
000900080320     ititas000      01
001000980611      * ---------------------------------------------------------
001100100325     C     Kbolla        KLIST
001200080212     C                   KFLD                    blpAAS
001300080212     C                   KFLD                    blpLNP
001400080212     C                   KFLD                    blpNRS
001500080212     C                   KFLD                    blpNSP
001600100325     C     Kblp          KLIST
001700110922     C                   KFLD                    klnp              3 0
001800110922     C                   KFLD                    kaas              4 0
001900110922     C                   KFLD                    kmgs              4 0
002000110922     C     Kblp2         KLIST
002100110922     C                   KFLD                    klnp              3 0
002200110922     C                   KFLD                    kaas              4 0
002300041222     C
002400110922     c                   z-add     2011          kaas
002500110922     c                   z-add     166           klnp
002600110922     c                   z-add     0901          kmgs
002700110922     c                   z-add     0             totale            5 0
002800110922     c                   z-add     0             totvlf            5 0
002900110923     c                   z-add     0             totK              5 0
003000110923     c                   z-add     0             tot01             5 0
003100110922     c                   except    testa
003200041222     C
003300110922     c     kblp          SETLL     fnblp31l
003400110922     c     kblp2         reade     fnblp31l
003500100325     c
003600110922    1c                   dow       not %eof(fnblp31l)
003700110922     c                   if        blpmgs>0921
003800110922     c                   leave
003900110922     c                   endif
004000110922     c
004100110922    2c                   if        blpksc=1661076
004200110922
004300110922     c* c'e' volume VDL
004400110922    3c                   if        blpvlc>0 and blpncr=blpncl
004500110922     c                   eval(h)   newvlb=blpvlc
004600110922     c                   else
004700110922     C                   Z-ADD     0,01          newvlb            5 3
004800110923     c                   eval      tot01 =tot01 +1
004900110922    3c                   endif
005000100325     c
005100110922     c                   setoff                                       20
005200110922    4c                   if        blpvlf<>blpvlb
005300110922     c                   seton                                        20
005400110922     c                   endif
005500110922     c
005600100325     c                   except    bolla
005700110923     c
005800110922     c                   eval      totale=totale+1
005900110922     c
006000110922     C     Kbolla        chain     fnblp01l
006100110922    3c                   if        %found(fnblp01l)
006200110923     c
006300110923     c                   if        blpfvb='K'
006400110923     c                   eval      totK  =totK  +1
006500110923     c                   endif
006600100325     c
006700110922     c* Aggiorno i campi vol da fatturare solo se uguale a vol bollettato
006800110922    4c                   if        blpvlf=blpvlb
006900110922     c                   z-add     newvlb        blpvlf
007000110922     c                   eval      blpfvf='R'
007100110922     c                   eval      totVLF=totVLF+1
007200110922    4c                   endif
007300110922    c
007400100325     c                   z-add     newvlb        blpvlb
007500110922     c                   eval      blpfvb='R'
007600110923     c                   update    fnblp000
007700110922    3c                   endif
007800110922     c*
007900080212     c                   setoff                                       01
008000100325     C     Kbolla        chain     fnarb01l
008100110922    3c                   if        %found(fnarb01l)
008200110922     c                   movel     blpfvb        arbfvb
008300110922     c                   movel     blpfvf        arbfvf
008400100325     c                   z-add     blpvlb        arbvlb
008500100325     c                   z-add     blpvlf        arbvlf
008600110923     c                   update    fnarb000
008700110922    3c                   endif
008800100325
008900100325     C     Kbolla        chain     titas30c
009000110922    3c                   if        %found(titas30c)
009100110922     c                   movel     blpfvb        tasfvb
009200110922     c                   movel     blpfvf        tasfvf
009300100325     c                   z-add     blpvlb        tasvlb
009400100325     c                   z-add     blpvlf        tasvlf
009500110923     c                   update    titas000
009600110922    3c                   endif
009700110922    2c                   endif
009800100325
009900110922     c     kblp2         reade     fnblp31l
010000100325    1c                   enddo
010100100325     c
010200110922     c                   except    fine
010300080212     c                   seton                                        lr
010400100325     c*-----------------------------------------------------------------
010500110922     OQSYSPRT   E            testa       1
010600110922     O                                        +  25 'Bolle  1661076 -'
010700110922     O                                              ' Lodeman'
010800110922     OQSYSPRT   E            bolla       1
010900110922     O                       blpaas        z  +   1
011000110922     O                       blplnp        z  +   1
011100110922     O                       blpnrs        z  +   1
011200110922     O                       blpnsp        2  +   1
011300110922     o                                        +   1 'vlb'
011400100325     O                       blpFVB           +   1
011500100325     O                       blpvlb        2  +   0
011600110922     o                                        +   2 'vdl'
011700110922     O                       blpvlc        2  +   1
011800110922     o                                        +   2 'ncR/ncL'
011900110922     O                       blpncr        2  +   1
012000110922     o                                        +   0 '/'
012100110922     O                       blpncl        2  +   0
012200110922     o                                        +   2 'Vol Agg'
012300110922     O                       newvlb        2  +   1
012400110922     o               20                       +   1 'VLF div.'
012500110922     O               20      blpFVF           +   1
012600110922     O               20      blpvlf        2  +   0
012700110922     OQSYSPRT   E            fine        2
012800110922     O                                              'Totale bolle aggiornate:'
012900110922     O                       totale        2  +   1
013000110922     O                                        +   3 'di cui anche BLPVLF:'
013100110922     O                       totvlf        2  +   1
013200110923     OQSYSPRT   E            fine        2
013300110923     O                                        +  10 'di cui con volume K:'
013400110923     O                       totK          2  +   1
013500110923     O                                        +   3 'di cui con volume impostat'
013600110923     O                                        +   0 'o a 0,01'
013700110923     O                       tot01         2  +   1
