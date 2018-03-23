000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300160104     Ffnblp00f  iF   E             DISK    rename(fnblp000:fnblpf00)
000400160104     FFNblp79l  uF   E           K DISK    infds(fnblpinf)
000500091125     FFNblt01l  iF   E           K DISK
000600160104     Ffiar501l  iF   E           K DISK
000700160104     Ffiar401l  iF   E           K DISK
000800160104     Ffisgn05l  iF   E           K DISK
000900160104     FFNblt27l  iF   E           K DISK    rename(fnblt000:fnblt027)
001000091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100091125
001200130909     D fnblpinf        ds
001300130909     D  fnblpnrri            397    400i 0
001400021214      *--------------------------------------------------
001500091125     c     kblt          klist
001600091125     c                   kfld                    blpaas
001700091125     c                   kfld                    blplnp
001800091125     c                   kfld                    blpnrs
001900091125     c                   kfld                    blpnsp
002000160104     c     kbltnew       klist
002100160104     c                   kfld                    bltaas
002200160104     c                   kfld                    bltlnp
002300160104     c                   kfld                    bltnrs
002400160104     c                   kfld                    bltnsp
002500160104     c     knew          klist
002600160104     c                   kfld                    kaas
002700160104     c                   kfld                    blplnp
002800160104     c                   kfld                    blpnrs
002900160104     c                   kfld                    blpnsp
003000160104     c     kblt27        klist
003100160104     c                   kfld                    blpfls
003200160104     c                   kfld                    blplna
003300160104     c                   kfld                    blpnrs
003400160104     c                   kfld                    blpncd
003500160104     c     kblp79        klist
003600160104     c                   kfld                    ktfp              3 0
003700160104     c                   kfld                    knfv              5 0
003800070913     c
003900160104     c                   z-add     2016          kaas              4 0
004000160104     c                   z-add     089           ktfp
004100160104     c                   clear                   knfv
004200160104     c     kblp79        setll     fnblp79l
004300160104     c     kblp79        reade     fnblp79l
004400051110
004500160104    1c                   DOW       not %EOF(FNblp79l)
004600160104     c                   if        blpctm<>'4 ' and blpctm<>'4F'
004700160104     c                             and blpaas=2016
004800091125
004900160104     c     kblt          chain     fnblt01l
005000160104     c                   if        not %found(fnblt01l)
005100160104     c     kblt27        chain     fnblt27l
005200160104     c                   if        not %found(fnblt27l)
005300160104     c                   except    errno
005400160104     c                   add       1             contano           9 0
005500160104     c                   else
005600160104     c                   except    erralt
005700160104     c                   add       1             contaalt          9 0
005800160104     c
005900160104     c* aggiorno il numero di spedizione da blt a blp
006000160104     c                   eval      blpnsp=bltnsp
006100160104     c                   except    aggbol
006200160104     c
006300160104     c                   endif
006400160104     c                   endif
006500160104     c                   endif
006600091125     c
006700160104     c     kblp79        reade     fnblp79l
006800091125     c                   enddo
006900021216      *
007000160104     c                   except    tot
007100021214     C                   eval       *inlr = *on
007200160104     OQSYSPRT   E            errno       1
007300091125     O                       blpaas           +   1
007400091125     O                       blplnp           +   1
007500091125     O                       blpnrs           +   1
007600091125     O                       blpnsp        2  +   1
007700160104     O                       blplna           +   1
007800160104     O                       blpncd        z  +   1
007900160104     O                       blpnca        z  +   1
008000160104     O                                        +   2 'no dett colli'
008100160104     OQSYSPRT   E            erralt      1
008200160104     O                       blpaas           +   1
008300160104     O                       blplnp           +   1
008400160104     O                       blpnrs           +   1
008500160104     O                       blpnsp        2  +   1
008600160104     O                       blplna           +   1
008700160104     O                       blpncd        z  +   1
008800160104     O                       blpnca        z  +   1
008900160104     O                                        +   2 'dett:'
009000160104     O                       bltaas        z  +   1
009100160104     O                       bltnsp        z  +   1
009200160104     O                       bltlna        z  +   1
009300160104     O                       bltnsc        z  +   1
009400160104     OQSYSPRT   E            ar5ok       1
009500160104     O                                              'AR5 ok:'
009600160104     O                       ar5aas           +   1
009700160104     O                       ar5lnp           +   1
009800160104     O                       ar5nrs           +   1
009900160104     O                       ar5nsp        2  +   1
010000160104     O                       ar5trd           +   1
010100160104     OQSYSPRT   E            ar5nook     1
010200160104     O                                              'AR5 no ok:'
010300160104     O                       ar5aas           +   1
010400160104     O                       ar5lnp           +   1
010500160104     O                       ar5nrs           +   1
010600160104     O                       ar5nsp        2  +   1
010700160104     O                       ar5trd           +   1
010800160104     OQSYSPRT   E            ar4ok       1
010900160104     O                                              'AR4 ok:'
011000160104     O                       ar4aas           +   1
011100160104     O                       ar4lnp           +   1
011200160104     O                       ar4nrs           +   1
011300160104     O                       ar4nsp        2  +   1
011400160104     O                       ar4trc           +   1
011500160104     OQSYSPRT   E            ar4nook     1
011600160104     O                                              'AR4 no ok:'
011700160104     O                       ar4aas           +   1
011800160104     O                       ar4lnp           +   1
011900160104     O                       ar4nrs           +   1
012000160104     O                       ar4nsp        2  +   1
012100160104     O                       ar4trc           +   1
012200160104     OQSYSPRT   E            sgnok       1
012300160104     O                                              'SGN ok:'
012400160104     O                       sgnaas           +   1
012500160104     O                       sgnlnp           +   1
012600160104     O                       sgnnrs           +   1
012700160104     O                       sgnnsp        2  +   1
012800160104     OQSYSPRT   E            sgnnook     1
012900160104     O                                              'SGN no ok:'
013000160104     O                                              'Bolla:'
013100160104     O                       sgnaas           +   1
013200160104     O                       sgnlnp           +   1
013300160104     O                       sgnnrs           +   1
013400160104     O                       sgnnsp        2  +   1
013500160104     OQSYSPRT   E            tot         1
013600160104     O                                              'non trovato BLT'
013700160104     O                       contano       2  +   1
013800160104     O                                        +   1 'aggiornati'
013900160104     O                       contaalt      2  +   1
014000160104     o
014100160104     OFNblp000  E            AGGbol
014200160104     O                       blpnsp
