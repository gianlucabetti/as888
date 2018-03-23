000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300100325     fceva_pkb  iF   E             DISK    rename(ceva_pkb:ceva000)
000400100325     f                                     infds(ceva01)
000500100325     fFNblp33l  uF   E           K DISK
000600100325     fFNarb01l  uF   E           K DISK
000700100325     ftitas30c  uF   E           K DISK
000800080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900100325     FQSYSPRTx  O    F  132        PRINTER OFLIND(*INOe)
001000080320     d
001100100325     D ceva01          DS
001200100325     D  cevNRR               397    400B 0
001300100325     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
001400100325     D                                     TUVWYXZ')
001500100325     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
001600100325     D                                     tuvwyxz')
001700080320     ititas000      01
001800980611      * ---------------------------------------------------------
001900100325     C     Kbolla        KLIST
002000080212     C                   KFLD                    blpAAS
002100080212     C                   KFLD                    blpLNP
002200080212     C                   KFLD                    blpNRS
002300080212     C                   KFLD                    blpNSP
002400100325     C     Kblp          KLIST
002500100325     C                   KFLD                    krmn
002600100325     C                   KFLD                    kanno             4 0
002700041222     C
002800100325     C                   MOVEL     CMAIUS        MA               26
002900100325     C                   MOVEL     CMINUS        MI               26
003000100325     c                   z-add     2010          kanno
003100041222     C
003200100325     c     *START        SETLL     ceva_pkb
003300100325     c                   read      ceva_pkb
003400100325     c
003500100325    1c                   dow       not %eof(ceva_pkb)
003600100325     c                   clear                   krmn
003700100325     c                   movel     docsap        w010a            10
003800100325     c                   move      w010a         krmn             15 0
003900100325     c                   clear                   waggioblp         1
004000100325     c
004100100325     c     kblp          setll     fnblp33l
004200100325     c     kblp          reade     fnblp33l
004300100325     c* Se alla prima read non trovo nulla, stampo msg diiverso
004400100325     c
004500100325    2c                   if        %eof(fnblp33l)
004600100325     c                   except    Nessuna
004700100325     c                   else
004800100325     c
004900100325    2c                   dow       not %eof(fnblp33l)
005000100325     c                   move      blprma        wprg             10
005100100325     c                   movel     rsd           cevarsd          30
005200100325     c                   movel     blprsd        bolrsd           30
005300100325     C     MI:MA         XLATE     cevarsd       cevarsd
005400100325     c
005500100325     c* Deve corrispondere: il rif alfanumerico, il codi mittente e il destinatario
005600100325    3c                   if        wprg =prg  and %subst(blprsm:1:4)='CEVA' and
005700100325     c                             bolrsd =cevarsd  and blpmgs>=0301
005800100325     c
005900100325     c* Calcolo nuovo pvolume se automatico
006000100325     c                   clear                   newvlb
006100100325     c                   clear                   newvlf
006200100325     c                   if        pkb<>blppkb and blpfvb='K'
006300100325     C     blppkb        DIV(H)    blpvlb        CLIKPM            7 1
006400100325     C     pkb           DIV(H)    CLIKPM        newvlb            5 3
006500100325     C*
006600100325     C     newvlb        IFEQ      0
006700100325     C                   Z-ADD     0,001         newvlb
006800100325     C                   END
006900100325     c                   endif
007000100325     c                   if        pkb<>blppkf and blpfvf='K'
007100100325     C     blppkf        DIV(H)    blpvlf        CLIKPM            7 1
007200100325     C     pkb           DIV(H)    CLIKPM        newvlf            5 3
007300100325     C*
007400100325     C     newvlf        IFEQ      0
007500100325     C                   Z-ADD     0,001         newvlf
007600100325     C                   END
007700100325     c                   endif
007800100325-1   c
007900100325     c* Stampo la bolla e la differenza di peso nel pkb e pkf se uguale??
008000100325     c                   except    bolla
008100100325     c
008200100325     c* Aggiorno i campi
008300100325     c                   z-add(h)  pkb           blppkb
008400100325     c                   z-add(h)  pkb           blppkf
008500100325     c                   if        newvlb>0
008600100325     c                   z-add     newvlb        blpvlb
008700100325     c                   endif
008800100325     c                   if        newvlf>0
008900100325     c                   z-add     newvlf        blpvlf
009000100325     c                   endif
009100100325     c*
009200080212     c                   setoff                                       01
009300100325     C     Kbolla        chain     fnarb01l
009400100325    4c                   if        %found(fnarb01l)
009500100325     c                   z-add     blppkb        arbpkb
009600100325     c                   z-add     blppkf        arbpkf
009700100325     c                   z-add     blpvlb        arbvlb
009800100325     c                   z-add     blpvlf        arbvlf
009900100325     c                   update    fnarb000
010000100325    4c                   endif
010100100325
010200100325     C     Kbolla        chain     titas30c
010300100325    4c                   if        %found(titas30c)
010400100325     c                   z-add     blppkb        taspkb
010500100325     c                   z-add     blppkf        taspkf
010600100325     c                   z-add     blpvlb        tasvlb
010700100325     c                   z-add     blpvlf        tasvlf
010800100325     c                   update    titas000
010900100325    4c                   endif
011000100325
011100100325     c                   update    fnblp000
011200100325     c
011300100325     c                   eval      Waggioblp='S'
011400100325     c                   leave
011500100325
011600100325   x3c                   else
011700100325     c     kblp          reade     fnblp33l
011800100325    3c                   endif
011900100325    2c                   enddo
012000100325     c
012100100325     c                   if        Waggioblp=' '
012200100325     c                   except    Notrov
012300100325     c                   endif
012400100325     c                   endif
012500080320     c
012600100325     c                   read      ceva_pkb
012700100325    1c                   enddo
012800100325     c
012900080212     c                   seton                                        lr
013000100325     c*-----------------------------------------------------------------
013100100325     OQSYSPRT   E            bolla       1
013200100325     O                                              'Bolla CEVA:'
013300100325     O                       blpaas           +   1
013400100325     O                       blplnp           +   1
013500100325     O                       blpnrs           +   1
013600100325     O                       blpnsp           +   1
013700100325     o                                        +   2 'Riferimenti'
013800100325     O                       blprmn        z  +   1
013900100325     O                       blprma           +   1
014000100325     OQSYSPRT   E            bolla       1
014100100325     o                                        +  20 'PKB/PKF'
014200100325     O                       blppkb        2  +   1
014300100325     o                                        +   0 '/'
014400100325     O                       blppkf        2  +   0
014500100325     o                                        +   2 'Peso corretto'
014600100325     O                       pkb           2  +   1
014700100325     o                                        +   2 'VLB/VLF'
014800100325     O                       blpFVB           +   1
014900100325     O                       blpvlb        2  +   0
015000100325     o                                        +   0 '/'
015100100325     O                       blpFVF           +   0
015200100325     O                       blpvlf        2  +   0
015300100325     o                                        +   2 'Vol corretto'
015400100325     O                       newvlb        2  +   1
015500100325     o                                        +   0 '/'
015600100325     O                       newvlf        2  +   0
015700100325     OQSYSPRTx  E            Notrov      1
015800100325     O                                              'Bolla non trovata '
015900100325     o                       cevnrr        z  +   1
016000100325     O                       docsap           +   2
016100100325     O                       prg              +   1
016200100325     O                       RSD              +   1
016300100325     o                                        +   2 'Peso corretto'
016400100325     O                       pkb           2  +   1
016500100325     OQSYSPRTx  E            Nessuna     1
016600100325     O                                              'Nessuna bolla trov'
016700100325     o                       cevnrr        z  +   1
016800100325     O                       docsap           +   2
016900100325     O                       prg              +   1
017000100325     O                       RSD              +   1
017100100325     o                                        +   2 'Peso corretto'
017200100325     O                       pkb           2  +   1
