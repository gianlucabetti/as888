000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300130628     fwpanta    uF   E           K DISK    rename(wpanta:wpanta00)
000400130628     fFNblp01l  uF   E           K DISK
000500130628     fFNarb01l  uF   E           K DISK
000600130628     ftitas30c  uF   E           K DISK
000700080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800080320     d
000900080320     ititas000      01
001000980611      * ---------------------------------------------------------
001100100325     C     Kbolla        KLIST
001200080212     C                   KFLD                    blpAAS
001300080212     C                   KFLD                    blpLNP
001400080212     C                   KFLD                    blpNRS
001500080212     C                   KFLD                    blpNSP
001600041222     C
001700130628     c                   read      wpanta
001800100325     c
001900130628    1c                   dow       not %eof(wpanta)
002000110922     c
002100130628     c                   z-add     anno          blpaas
002200130628     c                   z-add     lnp           blplnp
002300130628     c                   z-add     serie         blpnrs
002400130628     c                   z-add     spedizione    blpnsp
002500130628     c
002600130628     C     Kbolla        chain     fnblp01l
002700130628    2c                   if        not %found(fnblp01l)
002800130628     c                   except    err1
002900130628   x2c                   else
003000130628
003100130628   2ac                   if        blpcbo <>'1 ' and blpcbo<>'4 ' and
003200130628     c                             blpcbo<>'Y '  and blpcbo<>'5 ' and
003300130628     c                             blpcbo<>'$ '
003400130628     c                   add       1             totnoFR           7 0
003500130628     c                   except    err3
003600130628  x2ac                   else
003700130628
003800130628    3c                   if        blpksc<>0055178
003900130628     c                   except    err2
004000130628   x3c                   else
004100130628     c* aggiorno
004200130628     c                   z-add     pesfat        blppkb
004300130628     c                   z-add     pesfat        blppkf
004400130628     c                   update    fnblp000
004500130628     c
004600130628     c                   setoff                                       1011
004700130628     C     Kbolla        chain     fnarb01l
004800130628    5c                   if        %found(fnarb01l)
004900130628     c                   z-add     pesfat        arbpkb
005000130628     c                   z-add     pesfat        arbpkf
005100130628     c                   update    fnarb000
005200130628     c                   seton                                        10
005300130628    5c                   endif
005400130628     c
005500110922
005600100325     C     Kbolla        chain     titas30c
005700130628    5c                   if        %found(titas30c)
005800130628     c                   z-add     taspkf        oldpkf            7 1
005900130628     c                   z-add     taspkb        oldpkb            7 1
006000130628     c                   movel     taspkf        pkf               7
006100130628     c* aggiorno ilpeso OLB alla fibne del deestinatario
006200130628     c                   eval      %subst(dest:28:8)=%subst(pkf:1:6)+','+
006300130628     c                             %subst(pkf:7:1)
006400130628     c                   update    wpanta00
006500130628
006600130628     c                   z-add     pesfat        taspkb
006700130628     c                   z-add     pesfat        taspkf
006800130628     c                   seton                                        11
006900130628     c                   update    titas000
007000130628    5c                   endif
007100130628
007200130628     c                   add       1             totale            7 0
007300130628     c                   except    ok
007400130628
007500130628    3c                   endif
007600130628   2ac                   endif
007700130628    2c                   endif
007800130628
007900130628     c                   read      wpanta
008000100325    1c                   enddo
008100100325     c
008200110922     c                   except    fine
008300080212     c                   seton                                        lr
008400100325     c*-----------------------------------------------------------------
008500110922     OQSYSPRT   E            testa       1
008600130628     O                                        +  25 'Bolle  0055178 -'
008700130628     O                                              ' Micys'
008800130628     OQSYSPRT   E            err1        1
008900130628     O                       anno          z  +   1
009000130628     O                       lnp           z  +   1
009100130628     O                       serie         z  +   1
009200130628     O                       spedizione    2  +   1
009300130628     O                       dest             +   1
009400130628     o                                        +   1 'Non trovata'
009500130628     OQSYSPRT   E            err2        1
009600130628     O                       anno          z  +   1
009700130628     O                       lnp           z  +   1
009800130628     O                       serie         z  +   1
009900130628     O                       spedizione    2  +   1
010000130628     O                       pesfat        2  +   1
010100130628     o                                        +   1 'BLP: ksc'
010200130628     O                       blpksc           +   1
010300130628     o                                        +   1 'blpPKF'
010400130628     O                       blppkf        2  +   1
010500130628     o                                        +   1 'blpVDL'
010600130628     O                       blppkc        2  +   1
010700130628     OQSYSPRT   E            err3        1
010800130628     O                       anno          z  +   1
010900130628     O                       lnp           z  +   1
011000130628     O                       serie         z  +   1
011100130628     O                       spedizione    2  +   1
011200130628     O                       pesfat        2  +   1
011300130628     o                                        +   1 'BLP: ksc'
011400130628     O                       blpksc           +   1
011500130628     o                                        +   1 'cbo:'
011600130628     O                       blpcbo           +   1
011700130628     o                                        +   1 'blpPKF'
011800130628     O                       blppkf        2  +   1
011900130628     o                                        +   1 'blpVDL'
012000130628     O                       blppkc        2  +   1
012100130628     o
012200130628     OQSYSPRT   E            ok          1
012300130628     O                       anno          z  +   1
012400130628     O                       lnp           z  +   1
012500130628     O                       serie         z  +   1
012600130628     O                       spedizione    2  +   1
012700130628     o                                        +   2 'peso da aggiornare'
012800130628     O                       pesfat        2  +   0
012900130628     o                                        +   2 'peso bolla'
013000130628     O                       oldpkb        2  +   1
013100130628     O                       oldpkf        2  +   1
013200130628     o               10                       +   2 'ARB'
013300130628     o               11                       +   2 'TAS'
013400110922     OQSYSPRT   E            fine        2
013500110922     O                                              'Totale bolle aggiornate:'
013600110922     O                       totale        2  +   1
013700130628     O                                        +   2 'Bolle non AGG non F1 :'
013800130628     O                       totnoFR       2  +   1
