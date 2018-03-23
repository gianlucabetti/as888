000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     FFNARB54L  IF   E           K DISK
000300120117     FFNARB01L  IF   E           K DISK    rename(fnarb000:fnarb001)
000400150422     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
000500070316     d
000600070316     D fnlr76ds      E DS
000700070316     D DSRECARB      E DS                  extname(fnarb00f)
000800120117     d
000900120117     d sped_n          ds
001000120117     d kaas                    1      4  0
001100120117     d klnp                    5      7  0
001200120117     d knrs                    8      9  0
001300120117     d knsp                   10     16  0
001400120117     c
001500070316     c     *entry        plist
001600070316     c                   parm                    data              8
001700070420     c                   parm                    plna              3
001800070830     c* Soglia data e ora alfa (data di 8A  ora di 4A)
001900070830     C                   parm                    soglia           12
002000120117     C                   parm                    sped             16
002100070830     C
002200150422     c                   if        %parms>2
002300150422     c                   movel     soglia        dssoglia
002400150422     c                   else
002500150422     c                   clear                   dssoglia
002600150422     c                   endif
002700150422     c
002800070830     C                   EXCEPT    TESTA
002900070830     c
003000150422    1c                   if        %parms>2 and soglia<>*blanks
003100070830     c                   movel     soglia        dssoglia
003200120117    1c                   endif
003300150422    1c                   if        %parms>2 and sped   >*zeros
003400120117     c                   movel     sped          sped_n
003500120117     c     karb01        chain     fnarb01l
003600120117     c                   movel     kaas          ilr76aas
003700120117     c                   movel     klnp          ilr76lnp
003800120117     c                   movel     knrs          ilr76nrs
003900120117     c                   movel     knsp          ilr76nsp
004000120117     c                   movel     'S'           ilr76sppar
004100120117     c                   movel     data          ilr76dco
004200150422    2c                   if        %parms>2 and soglia<>*blanks
004300120117     c                   CALL      'FNLR76R'
004400120117     C                   PARM                    FNLR76DS
004500120117     C                   PARM                    DSRECARB
004600120117     C                   PARM                    DSsoglia         12 0
004700120117   x2c                   else
004800120117     c                   clear                   dssoglia
004900120117     c                   CALL      'FNLR76R'
005000120117     C                   PARM                    FNLR76DS
005100120117     C                   PARM                    DSRECARB
005200120117     C                   PARM                    DSsoglia         12 0
005300120117    2c                   endif
005400120117     c* wetto in stampa
005500120117     c                   except    bolla
005600120117   x1c                   else
005700070316     c
005800070316     c     karb          klist
005900070316     c                   kfld                    lna               3 0
006000070316     c                   kfld                    dcm               8 0
006100120117     c
006200120117     c     karb01        klist
006300120117     c                   kfld                    kaas
006400120117     c                   kfld                    klnp
006500120117     c                   kfld                    knrs
006600120117     c                   kfld                    knsp
006700120117     c
006800070420     c                   movel     plna          lna
006900070316     c                   clear                   dcm
007000070316     c     karb          setll     fnarb54l
007100070316     c     karb          reade     fnarb54l
007200120117    2c                   dow       not %eof(fnarb54l)
007300070316     C                   CLEAR                   fnlr76ds
007400070316     c                   movel     arbaas        ilr76aas
007500070316     c                   movel     arblnp        ilr76lnp
007600070316     c                   movel     arbnrs        ilr76nrs
007700070316     c                   movel     arbnsp        ilr76nsp
007800070316     c                   movel     'S'           ilr76sppar
007900070316     c                   movel     data          ilr76dco
008000150422    3c                   if        %parms>2 and soglia<>*blanks
008100070316     c                   CALL      'FNLR76R'
008200070316     C                   PARM                    FNLR76DS
008300070316     C                   PARM                    DSRECARB
008400070830     C                   PARM                    DSsoglia         12 0
008500070830     c                   else
008600070830     c                   clear                   dssoglia
008700070830     c                   CALL      'FNLR76R'
008800070830     C                   PARM                    FNLR76DS
008900070830     C                   PARM                    DSRECARB
009000070830     C                   PARM                    DSsoglia         12 0
009100120117    3c                   endif
009200070316     c* wetto in stampa
009300070316     c                   except    bolla
009400070316     c     karb          reade     fnarb54l
009500120117    2c                   enddo
009600120117    1c                   endif
009700020503     C*
009800020502     C                   SETON                                        LR
009900070316     C*
010000150422     Oprtf198   E            testa       1  1
010100070830     o                                              'DATA CONSEGNABILITA'':'
010200070830     O                       DATA             +   1
010300070830     o                                        +   3 'LINEA ARRIVO:'
010400070830     O                       PLNA             +   1
010500070830     o                                        +   3 'SOGLIA:'
010600150422     O                       dsSOGLIA         +   1
010700150422     Oprtf198   E            BOLLA       1
010800070316     O                       arbaas
010900070316     O                       arblnp           +   1
011000070316     O                       arbnrs        z  +   1
011100070316     O                       arbnsp        z  +   1
011200070830     O                       arbmgs           +   1 '  /  '
011300070831     O                       arblna           +   1
011400101022     O                       arbcbo           +   1
011500070316     o                                        +   1 'Colli'
011600070316     O                       arbncl        z  +   1
011700070316     o                                        +   1 'Da cons'
011800070316     O                       olr76cDACO    z  +   1
011900150422     o                                        +   1 'Dicui da 1g'
012000070316     O                       olr76cDC1G    z  +   1
012100150422     o                                        +   1 'Dicui 88 no arr'
012200150422     O                       olr76cDC88    z  +   1
012300150422     o                                        +   1 'NonCons'
012400070316     O                       olr76cNOCO    z  +   1
012500150422     o                                        +   1 'C.A.'
012600070316     O                       olr76cCA      z  +   1
012700070316     o                                        +   1 'consegnati'
012800070316     O                       olr76cdcm     z  +   1
