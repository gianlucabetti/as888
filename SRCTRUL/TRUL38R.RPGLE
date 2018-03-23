000100040505     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040505      *------------------------------------------------------------------------*
000300040505      *
000400050413      *                     RICERCA GIACENZE APERTE
000500040505      *
000600040505      *------------------------------------------------------------------------*
000700040505
000800050413     fFnlbl01l  if   e           k Disk
000900050413     fFnlbl02l  if   e           k Disk    Rename(fnlbl000:fnlbl02)
001000050413     fTigcp01l  if   e           k Disk
001100040505
001200040505      *------------------------------------------------------------------------*
001300040505      *   S C H I E R E
001400040505      *------------------------------------------------------------------------*
001500040505
001600040505      *------------------------------------------------------------------------*
001700040505      *   V A R I A B I L I
001800040505      *------------------------------------------------------------------------*
001900050413     d kaas            s                   like(gcpaas)
002000050413     d kfrg            s                   like(gcpfrg) inz
002100050413     d klnp            s                   like(gcplnp)
002200050413     d knrs            s                   like(gcpnrs)
002300050413     d knsp            s                   like(gcpnsp)
002400040505
002500040505      *------------------------------------------------------------------------*
002600040505      *   D S   I N T E R N E / E S T E R N E
002700040505      *------------------------------------------------------------------------*
002800040505
002900050907     d Trul38ds      e ds
003000050217
003100050217     d Azuteds       e ds                  extname(Azute00f)
003200050217     d dDatiute      e ds
003300050217     d dute01        e ds
003400050217     d Tibs34ds      e ds
003500040505
003600040505      *------------------------------------------------------------------------*
003700040505      *   C O S A T A N T I
003800040505      *------------------------------------------------------------------------*
003900040505
004000040505      *------------------------------------------------------------------------*
004100040505      *  RIEPILOGO INDICATORI
004200040505      *------------------------------------------------------------------------*
004300040505
004400040505      *------------------------------------------------------------------------*
004500040505
004600040505     c     *Entry        Plist
004700050907     c                   Parm                    Trul38ds
004800050217
004900050217     c     *dtaara       define    §azute        azuteds
005000050217     c     *dtaara       define    §datiute      ddatiute
005100050217     c                   in(E)     *dtaara
005200050217     c                   If        %error  or RSUT = *blanks
005300050217     c                   Clear                   Tibs34ds
005400050217     c                   Call      'TIBS34R'
005500050217     c                   Parm                    Tibs34ds
005600050217     c                   In        *dtaara
005700050217     c                   EndIf
005800040505
005900040505     c     kSped         klist
006000050413     c                   kfld                    Iul37Aas
006100050413     c                   kfld                    Iul37Lnp
006200050413     c                   kfld                    Iul37Nrs
006300050413     c                   kfld                    Iul37Nsp
006400040505
006500050413     c     kGiac         klist
006600050413     c                   kfld                    kaas
006700050413     c                   kfld                    klnp
006800050413     c                   kfld                    knrs
006900050413     c                   kfld                    knsp
007000050413     c                   kfld                    kfrg
007100040505
007200050413      * Iul37Tla = ' '  Elaboro e chiudo con RETURN
007300050413      * Iul37Tla = 'L'  Elaboro e chiudo con LR
007400050413      * Iul37Tla = 'C'            chiudo con LR  solo per chiusura file
007500040505
007600050413if  1c                   If        Iul37Tla = 'C'
007700040505     c                   Eval      *InLr = *On
007800040505
007900040505   x1c                   Else
008000040505
008100050413     c                   Clear                   Oul37Fgc
008200050413     c                   Clear                   Oul37Fas
008300050413     c                   Clear                   Oul37Err
008400050413     c                   Clear                   Oul37Msg
008500040505
008600050413if  2c                   If        Iul37Tla = 'L'
008700040505     c                   Eval      *InLr = *On
008800040505
008900040809e   2c                   EndIf
009000040505
009100040505      * Controllo se passata la spedizione
009200050413if  3c                   If        Iul37Aas = *Zeros or
009300050413     c                             Iul37Lnp = *Zeros or
009400050413     c                             Iul37Nsp = *Zeros
009500050413     c                   Eval      Oul37Err = '1'
009600050413     c                   Eval      Oul37Msg = 'Non è stata passata la bolla'
009700040505     c                   GoTo      Noelab
009800040505e   3c                   EndIf
009900040505
010000040505      * Elaborazioni
010100050413     c                   ExSr      Sr_Cerca
010200040505
010300040505     c     Noelab        Tag
010400040505
010500040505     c                   Return
010600040505
010700040505e   1c                   EndIf
010800040505
010900040505      *------------------------------------------------------------------------*
011000050413      * - Cerco se c'è una giacenza aperta
011100040505      *------------------------------------------------------------------------*
011200050413     c     Sr_Cerca      BegSr
011300050413
011400050413     c     kSped         Chain     Fnlbl01l
011500050413     c                   If        %Found(Fnlbl01l)
011600050413     c                   Eval      kaas = lblaao
011700050413     c                   Eval      klnp = lbllpo
011800050413     c                   Eval      knrs = lblnro
011900050413     c                   Eval      knsp = lblnso
012000050413     c                   Else
012100050413     c     kSped         Chain     Fnlbl02l
012200050413     c                   If        %Found(Fnlbl02l)
012300050413     c                   Eval      kaas = lblaao
012400050413     c                   Eval      klnp = lbllpo
012500050413     c                   Eval      knrs = lblnro
012600050413     c                   Eval      knsp = lblnso
012700050413     c                   Else
012800050413     c                   Eval      kaas = iul37aas
012900050413     c                   Eval      klnp = iul37lnp
013000050413     c                   Eval      knrs = iul37nrs
013100050413     c                   Eval      knsp = iul37nsp
013200050413     c                   EndIf
013300050413     c                   EndIf
013400040505
013500050413      * sempre con la spedizione ORIGINALE
013600050413     c     kGiac         Chain     Tigcp01l
013700050413     c                   If        %Found(Tigcp01l) and
013800050413     c                             gcpatb = *Blanks and gcpfas < 60
013900050413     c                   Eval      oul37fgc = 'S'
014000050413     c                   Eval      oul37fas = gcpfas
014100050413     c                   EndIf
014200040505
014300040505     c                   EndSr
