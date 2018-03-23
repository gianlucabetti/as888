000100030604     H Decedit('0,') Datedit(*DMY.)
000200030219      *---------------------------------------------------------------*
000300030604      *   x Fedex:                                                    *
000400030604      *   Modifica volume su fnart e fnarb                            *
000500030219      *---------------------------------------------------------------*
000600030219     fFnarb01l  uf   e           k Disk
000700030530     fFnart01l  uf   e           K Disk
000800030219     fFiar601l  if   e           k Disk
000900030219      *---------------------------------------------------------------*
001000030219     d kar6trc         s                   Like(Ar6Trc) Inz('1')
001100030219
001200030219     d Param           ds
001300030219     d  Aas                    1      4  0
001400030219     d  Lnp                    5      7  0
001500030219     d  Nrs                    8      9  0
001600030219     d  Nsp                   10     16  0
001700030604     d  VolMCt                17     21  3
001800030219
001900030219     d Fnlv20ds      e ds
002000030219
002100030219     d Kpjba         e ds
002200030219      *---------------------------------------------------------------*
002300030219     c     *Entry        Plist
002400030219     c                   Parm                    Kpjba
002500030219
002600030219     c                   Movel     Kpjbu         Param
002700030219
002800030219     c     kbolla        Klist
002900030219     c                   Kfld                    Aas
003000030219     c                   Kfld                    Lnp
003100030219     c                   Kfld                    Nrs
003200030219     c                   Kfld                    Nsp
003300030219
003400030219     c     kfiar6        Klist
003500030219     c                   Kfld                    ArbAas
003600030219     c                   Kfld                    ArbLnp
003700030219     c                   Kfld                    ArbNrs
003800030219     c                   Kfld                    ArbNsp
003900030219
004000030219      * Leggo la bolla
004100030219     c     kbolla        Chain     Fnarb01l
004200030219If  1c                   If        %Found(Fnarb01l)
004300030604      *
004400030604      * imposta il volume unitario di ciascun collo e il relativo resto
004500030604      * della divisione da riportare sul primo collo di ART
004600030605     c     volMCt        div       ArbNCL        volUNI            7 6
004700030605     c                   mvr                     volRES            7 6
004800030604      *
004900030219      * Data fattura
005000030219     c     kfiar6        Chain     Fiar601l
005100030219     c                   If        Not %Found(Fiar601l)
005200030219     c                   Clear                   Ar6Dft
005300030219     c                   EndIf
005400030219
005500030219     c                   Clear                   Fnlv20ds
005600030219
005700030219      * Leggo i colli
005800030219     c     kbolla        Setll     Fnart01l
005900030219Do  2c                   Do        *Hival
006000030219     c     kbolla        Reade     Fnart01l
006100030219     c                   If        %Eof(Fnart01l)
006200030219     c                   Leave
006300030219     c                   EndIf
006400030219
006500030219      * Devo calcolare il Volume CML sulla bolla
006600030604     c                   Z-Add     volUNI        ArtVuc
006700030604      *
006800030604      * solo sul primo aggiunge subito il resto
006900030604     c                   if        d20ncr = 0
007000030604     c                   Add       volRES        ArtVuc
007100030604     c                   end
007200030604      *
007300030219     c                   Add       1             d20ncr
007400030530     c                   Eval      ArtFvc = 'A'
007500030530     c                   Update    Fnart000
007600030219
007700030219    2c                   EndDo
007800030219
007900030219      * Volume CML
008000030604If  2c                   If        VolMCt > 0
008100030219     c                   Eval      d20tbo = 'A'
008200030219     c                   Eval      d20ftr = 'S'
008300030219     c                   Eval      d20tvl = 'C'
008400030604     c                   Z-Add(h)  VolMCt        d20vlu
008500030219     c                   Eval      d20fvb = ArbFvb
008600030219     c                   Eval      d20vlb = ArbVlb
008700030219     c                   Eval      d20fvf = ArbFvf
008800030219     c                   Eval      d20vlf = ArbVlf
008900030219     c                   Eval      d20aas = ArbAas
009000030219     c                   Eval      d20lnp = ArbLnp
009100030219     c                   Eval      d20nrs = ArbNrs
009200030219     c                   Eval      d20nsp = ArbNsp
009300030219     c                   Eval      d20cbo = ArbCbo
009400030219     c                   Eval      d20lna = ArbLna
009500030219     c                   Eval      d20ncl = ArbNcl
009600030219     c                   Eval      d20dft = Ar6Dft
009700030219     c                   Eval      d20dte = ArbDbr
009800030219     c                   Movel(p)  Fnlv20ds      Kpjbu
009900030219     c                   Call      'FNLV20R'
010000030219     c                   Parm                    Kpjba
010100030219     c                   Movel     Kpjbu         Fnlv20ds
010200030219      * Se non c'è errore aggiorno i dati della bolla
010300030219If  3c                   If        d20err <> '1'
010400030219     c                   Eval      ArbNcr = d20ncr
010500030219     c                   Eval      ArbVlc = d20vlu
010600030219     c                   Eval      ArbFvf = d20fvf
010700030219     c                   Eval      ArbVlf = d20vlf
010800030219   x3c                   Else
010900030604     c                   IF        ArbFbs = *Blanks
011000030604     c                   Eval      ArbFbs = 'V'
011100041105     c                   else
011200041105     c
011300041105     c                   if        arbfbs<>'V'
011400041105     c                   Eval      ArbFbs = 'E'
011500041105     c                   endif
011600030604     c                   EndIf
011700030219    3c                   EndIf
011800030219    2c                   EndIf
011900030219
012000030219     c                   Update    Fnarb000
012100030219
012200030219    1c                   EndIf
012300030219
012400030219     c                   Eval      *InLr = *On
