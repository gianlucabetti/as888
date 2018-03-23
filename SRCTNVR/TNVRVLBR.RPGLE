000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300170328     fvolumi    iF   E             DISK
000400170329     fFNblp33l  uF   E           K DISK
000500170329     fFNarb01l  uF   E           K DISK
000600170329     ftitas30c  uF   E           K DISK
000700080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800080320     d
000900170328     doldvlf           s                   like(blpvlf)
001000170328     doldfvf           s                   like(blpfvf)
001100170328     ititas000      10
001200980611      * ---------------------------------------------------------
001300170328     c     *entry        plist
001400170328     c                   parm                    stasino           1
001500100325     C     Kbolla        KLIST
001600080212     C                   KFLD                    blpAAS
001700080212     C                   KFLD                    blpLNP
001800080212     C                   KFLD                    blpNRS
001900080212     C                   KFLD                    blpNSP
002000041222     C
002100110922     c                   z-add     0             totale            5 0
002200170328     c                   z-add     0             totalee           5 0
002300170328     c                   z-add     0             totalec           5 0
002400170328     c                   z-add     0             totalet           5 0
002500170328     c                   clear                   rifalfa          12
002600110922     c                   except    testa
002700041222     C
002800170328     c                   read      volumi
002900100325     c
003000170328    1c                   dow       not %eof(volumi)
003100170328     c                   clear                   alfa7
003200170328     c                   movel     F2            alfa7             7
003300170328    2c                   if        %subst(f1:1:12)>*zeros
003400170328     c                   eval      rifalfa=%subst(F1:1:12)
003500170328     c                   move      rifalfa       w0150            15 0
003600170328
003700170328     c     w0150         chain     fnblp33l
003800170328    3c                   if        not %found(fnblp33l) or
003900170328     C                             alfa7  <> %SUBST(blprma:1:7)
004000170328     c                             or f3>50 or F3<=0
004100170328     c                   setoff                                       010203
004200170328     c                   if        not %found(fnblp33l)
004300170328     c                   seton                                        01
004400170328     c                   eLSE
004500170328     c                   if        alfa7<>%SUBST(blprma:1:7)
004600170328     c                   seton                                        02
004700170328     c                   else
004800170328     c                   seton                                        03
004900170328     c                   endif
005000170328     c                   endif
005100170328     c
005200170328     c                   except    errore
005300170328     c                   eval      totalee=totalee+1
005400170328   x3c                   else
005500170328     c                   setoff                                       10
005600170328     C     Kbolla        chain     titas30c
005700170328
005800170328    4c                   if        %found(titas30c)and tasfic<>' '
005900170328     c                   except    errorec
006000170328     c                   eval      totalec=totalec+1
006100170328   x4c                   else
006200110922     c
006300170328    5c                   if        blpfvf='T'
006400170328     c                   except    bollat
006500170328     c                   eval      totalet=totalet+1
006600170328    5c                   endif
006700170328     c* salvo volume precedente
006800170328     c                   eval      oldfvf=blpfvf
006900170328     c                   eval      oldvlf=blpvlf
007000170328     c
007100170328     c                   eval(h)   blpvlf=F3
007200170328     c                   eval      blpfvf='R'
007300170328     c                   eval(h)   blpvlb=F3
007400170328     c                   eval      blpfvb='R'
007500110923     c
007600110922     c                   eval      totale=totale+1
007700170328     c                   if        stasino='S'
007800170328     c                   except    bolla
007900170328     c                   endif
008000100325     c
008100170329     c                   update    fnblp000
008200110922     c*
008300100325     C     Kbolla        chain     fnarb01l
008400170328    6c                   if        %found(fnarb01l)
008500110922     c                   movel     blpfvb        arbfvb
008600110922     c                   movel     blpfvf        arbfvf
008700100325     c                   z-add     blpvlb        arbvlb
008800100325     c                   z-add     blpvlf        arbvlf
008900170329     c                   update    fnarb000
009000170328    6c                   endif
009100100325
009200170328    6c                   if        %found(titas30c)
009300110922     c                   movel     blpfvb        tasfvb
009400110922     c                   movel     blpfvf        tasfvf
009500100325     c                   z-add     blpvlb        tasvlb
009600100325     c                   z-add     blpvlf        tasvlf
009700170329     c   10              update    titas000
009800170328    6c                   endif
009900170328    4c                   endif
010000170328    3c                   endif
010100170328    2c                   endif
010200100325
010300170328     c                   read      volumi
010400100325    1c                   enddo
010500100325     c
010600110922     c                   except    fine
010700080212     c                   seton                                        lr
010800100325     c*-----------------------------------------------------------------
010900110922     OQSYSPRT   E            testa       1
011000170328     O                                        +  25 'CORREZIONE VOLUMI'
011100170328     OQSYSPRT   E            errore      1
011200170328     O                       F1               +   0
011300170328     O                       F2               +   2
011400170328     O                       F3            4  +   2
011500170328     o                     01                   110 'errore su rmn non trovato'
011600170328     o                     02                   110 'errore su rma<> da bolla '
011700170328     o                     03                   110 'errore su volume         '
011800170328     OQSYSPRT   E            errorec     1
011900170328     O                       blpaas        z  +   1
012000170328     O                       blplnp        z  +   1
012100170328     O                       blpnrs        z  +   1
012200170328     O                       blpnsp        2  +   1
012300170328     O                       blpmgs           +   1
012400170328     o                                           60 'già contabilizzata       '
012500170328     OQSYSPRT   E            bollaT      1
012600110922     O                       blpaas        z  +   1
012700110922     O                       blplnp        z  +   1
012800110922     O                       blpnrs        z  +   1
012900110922     O                       blpnsp        2  +   1
013000170328     O                       blpncl        2  +   1
013100110922     o                                        +   1 'vlb'
013200100325     O                       blpFVB           +   1
013300100325     O                       blpvlb        2  +   0
013400170328     o                                        +   1 'VLF'
013500170328     O                       blpFVF           +   1
013600170328     O                       blpvlf        2  +   0
013700170328     OQSYSPRT   E            bolla       1
013800170328     O                       blpaas        z  +   1
013900170328     O                       blplnp        z  +   1
014000170328     O                       blpnrs        z  +   1
014100170328     O                       blpnsp        2  +   1
014200170328     O                       blpncl        2  +   1
014300170328     o                                        +   1 'vlb'
014400170328     O                       blpFVB           +   1
014500170328     O                       blpvlb        2  +   1
014600170328     o                                        +   1 'VLF old.'
014700170328     O                       oldfvf           +   1
014800170328     O                       oldvlf        2  +   1
014900170328     o                                        +   1 'VLF new.'
015000170328     O                       blpFVF           +   1
015100170328     O                       blpvlf        2  +   0
015200110922     OQSYSPRT   E            fine        2
015300170328     O                                              'Totale bolle errate:    '
015400170328     O                       totalee       2  +   1
015500170328     O                                        +   2 'totale bolle contabilizz'
015600170328     O                       totalec       2  +   1
015700170328     OQSYSPRT   E            fine        2
015800170328     O                                              'Totale bolle con vol. T '
015900170328     O                       totalet       2  +   1
016000170328     O                                        +   2 'totale bolle aggiornate '
016100170328     O                       totale        2  +   1
