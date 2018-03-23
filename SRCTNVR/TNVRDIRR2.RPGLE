000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300150114     fFNARB01L  iF   E           K DISK
000400141118     fFNblp01L  IF   E           K DISK
000500150114     fFNlbl01L  IF   E           K DISK
000600150113     fFiar403L  IF   E           K DISK
000700990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000800980611      * ---------------------------------------------------------
000900141118     C     Klbl          KLIST
001000150114     C                   KFLD                    AR4AAS
001100150114     C                   KFLD                    AR4LNP
001200150114     C                   KFLD                    AR4NRS
001300150114     C                   KFLD                    AR4NSP
001400141118     C     Kblp          KLIST
001500150114     C                   KFLD                    wann
001600150114     C                   KFLD                    wlnp
001700150114     C                   KFLD                    wnrs
001800150114     C                   KFLD                    wnsp
001900150113     C     Kar4          KLIST
002000150113     C                   KFLD                    Ktrc
002100150113     C                   KFLD                    Kdes
002200150113     c
002300150113     c                   clear                   ktrc              1
002400150113     c                   clear                   kdes             30
002500150114
002600150113     c                   movel     'EX SPED.'    kdes
002700150113     c                   eval      ktrc='8'
002800150113     c
002900150113     c     kar4          setll     fiar403l
003000150113     c     ktrc          reade     fiar403l
003100150113     c
003200150113    1C                   DOW       NOT %EOF(Fiar403l)
003300150114     c                   if        %subst(ar4n30:1:8)<>'EX SPED.'
003400150114     c                   leave
003500150114     c                   endif
003600150114
003700150114     c                   if         %subst(ar4n30:9:2)='14'
003800150114     c                              or %subst(ar4n30:9:2)='15'
003900150113     c
004000150114     c     klbl          chain     fnlbl01l
004100150114    3c                   if        not %found(fnlbl01l)
004200150114     c     klbl          chain     fnblp01l
004300150114    4c                   if        not %found(fnblp01l)
004400150114     c                   clear                   blplna
004500150114     c                   clear                   blpdcm
004600150114     c                   endif
004700150114     c
004800150114     c                   if        blpctm<>'4 ' and blpctm<>'4F'
004900141118     c                   except    err1
005000150114     c                   endif
005100141118   x3c                   else
005200150114     c                   if        blpctm<>'4 ' and blpctm<>'4F'
005300150114     c* prendo il num spedizione della mamma
005400150114     c                   clear                   ann               2
005500150114     c                   clear                   lnp               3
005600150114     c                   clear                   nrs               2
005700150114     c                   clear                   nsp               7
005800150114     c                   eval      ann=%subst(ar4n30:9:2)
005900150114     c                   eval      lnp=%subst(ar4n30:12:3)
006000150114     c                   eval      nrs=%subst(ar4n30:16:2)
006100150114     c                   eval      nsp=%subst(ar4n30:19:7)
006200150114     c                   if        ann>*zeros and  lnp>*zeros and nrs>*zeros and
006300150114     c                             nsp>*zeros and (ann='14' or ann='15')
006400150114     c                   z-add     2014          wann              4 0
006500150114     c                   move      ann           wann
006600150114     c                   movel     lnp           wlnp              3 0
006700150114     c                   movel     nrs           wnrs              2 0
006800150114     c                   movel     nsp           wnsp              7 0
006900150114     C     Kblp          chain     FNarb01l
007000150114    4c                   if        %found(fnarb01l) and arbcca=' '
007100141118     c                   except    err2
007200141118    4c                   endif
007300141118    3c                   endif
007400150114    3c                   endif
007500150114    3c                   endif
007600150114    3c                   endif
007700141118     c
007800150114     c     ktrc          reade     fiar403l
007900141118    1c                   enddo
008000141118     c
008100041222     C
008200041222     c
008300041222     c                   seton                                        lr
008400041222     c
008500041222     c
008600980611      *------------------------------------------------------------
008700141118     OPRTF198   E            err1        1
008800141118     O                                        +   0 'bolla DIR senza LBL'
008900150114     O                       ar4aas           +   4
009000150114     O                       ar4lnp           +   2
009100150114     O                       ar4nrs        z  +   2
009200150114     O                       ar4nsp        z  +   2
009300150114     O                       blplna           +   2
009400150114     O                       blpdcm           +   2 '    /  /  '
009500150114     O                       ar4n30           +   2
009600141118     OPRTF198   E            err2        1
009700150114     O                                        +   0 'bolla DIR senza ARB chiuso'
009800141118     O                       arbaas           +   4
009900141118     O                       arblnp           +   2
010000141118     O                       arbnrs        z  +   2
010100141118     O                       arbnsp        z  +   2
010200141118     O                       arblna           +   2
010300141118     O                       arbdcm           +   2 '    /  /  '
010400150114     O                       arbcca           +   2
