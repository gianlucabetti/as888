000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020703
000300020619      * TNVR28R *----------------------------------------------------*
000400020703      * sposta tariffe in gru del sistema informativo SDI
000500020114      *--------------------------------------------------------------*
000600020703
000700020703      * Tariffe clienti SDI
000800020703     fTntamSdi  if   e           k disk    rename(Tntam000:Tntam01)
000900020703     f                                     prefix(S_)
001000020703     fTitadSdi  if   e           k disk    rename(titad000:titad01)
001100020703     f                                     prefix(S_)
001200020703     fTitptSdi  if   e           k disk    rename(titpt000:titpt01)
001300020703     f                                     prefix(S_)
001400020703     fTitpdSdi  if   e           k disk    rename(titpd000:titpd01)
001500020703     f                                     prefix(S_)
001600020703     fTitgcSdi  if   e           k disk    rename(Titgc000:Titgc01)
001700020703     f                                     prefix(S_)
001800020703
001900020703      * Tariffe clienti GRU (ex Bartolini)
002000020729     fTntam01l  if a e           k disk
002100020729     fTitad04l  if a e           k disk
002200020729     fTitpt01l  if a e           k disk
002300020729     fTitpd01l  if a e           k disk
002400020729     fTitgc01l  if a e           k disk
002500020703
002600020704     d xx              s              2  0
002700020704     d W_poKsc         s              3  0
002800020704
002900020704     d Kksc            s                   Like(TamKsc)
003000020704
003100020704     d DsTam         e ds                  ExtName(Tntam01l)
003200020704     d DsTamS        e ds                  ExtName(Tntam01l) prefix(S_)
003300020703
003400020704     d DsTad         e ds                  ExtName(Titad04l)
003500020704     d DsTadS        e ds                  ExtName(Titad04l) prefix(S_)
003600020703
003700020704     d DsTpt         e ds                  ExtName(Titpt01l)
003800020704     d DsTptS        e ds                  ExtName(Titpt01l) prefix(S_)
003900020703
004000020704     d DsTpd         e ds                  ExtName(Titpd01l)
004100020704     d DsTpdS        e ds                  ExtName(Titpd01l) prefix(S_)
004200020703
004300020704     d DsTgc         e ds                  ExtName(Titgc01l)
004400020704     d DsTgcS        e ds                  ExtName(Titgc01l) prefix(S_)
004500020703
004600020704     d Ut§Dse0f      e ds
004700020704     d Cncr80        e ds
004800020704     d Trul06Ds      e ds
004900020704     d  Lin                    1     90  0 Dim(30)
005000020704     d KpjBa         e ds
005100020703
005200020704      * Prima devo eliminare dalle tariffe in GRU le tariffe che hanno come p.o. cliente
005300020704      * un p.o. SDI
005400020704     c                   ExSr      Sr_Elimina
005500020704
005600020704      * Poi scrive le tariffe della SDI nei files della GRU
005700020704     c                   ExSr      Sr_Scrivi
005800020704
005900020704     c                   Eval      *InLr = *on
006000020704
006100020704      *---------------------------------------------------------------*
006200020704      * Routine per eliminare i dati dalla GRU
006300020704      *---------------------------------------------------------------*
006400020704     c     Sr_Elimina    BegSr
006500020704
006600020704      * Leggo i terminal SDI
006700020704     c                   Do        30            xx
006800020704     c                   If        Lin(xx) = *Zeros
006900020704     c                   Leave
007000020704     c                   EndIf
007100020704      * Imposto la chiave
007200020704     c                   Clear                   Kksc
007300020704     c                   Movel     Lin(xx)       Kksc
007400020704
007500020704      * Lettura di TnTam
007600020704     c     kksc          Setll     Tntam01l
007700020703     c                   Do        *Hival
007800020704     c                   Read      Tntam01l
007900020703      * Fine File
008000020704     c                   If        %Eof(Tntam01l)
008100020703     c                   Leave
008200020703     c                   EndIf
008300020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
008400020704     c                   Movel     TamKsc        W_poKsc
008500020704     c                   If        W_poKsc <> Lin(xx)
008600020704     c                   Leave
008700020704     c                   EndIf
008800020704      * Cancello i records
008900020703     c                   EndDo
009000020704
009100020704      * Lettura di TiTad
009200020704     c     Kksc          Setll     Titad04l
009300020704     c                   Do        *Hival
009400020704     c                   Read      Titad04l
009500020704      * Fine File
009600020704     c                   If        %Eof(Titad04l)
009700020704     c                   Leave
009800020704     c                   EndIf
009900020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
010000020704     c                   Movel     TadKsc        W_poKsc
010100020704     c                   If        W_poKsc <> Lin(xx)
010200020704     c                   Leave
010300020704     c                   EndIf
010400020704      * Cancello i records
010500020704     c                   EndDo
010600020704
010700020704      * Lettura di TiTpt
010800020704     c     Kksc          Setll     Titpt01l
010900020704     c                   Do        *Hival
011000020704     c                   Read      Titpt01l
011100020704      * Fine File
011200020704     c                   If        %Eof(Titpt01l)
011300020704     c                   Leave
011400020704     c                   EndIf
011500020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
011600020704     c                   Movel     TptKsc        W_poKsc
011700020704     c                   If        W_poKsc <> Lin(xx)
011800020704     c                   Leave
011900020704     c                   EndIf
012000020704      * Cancello i records
012100020704     c                   EndDo
012200020704
012300020704      * Lettura di TiTpd
012400020704     c     Kksc          Setll     Titpd01l
012500020704     c                   Do        *Hival
012600020704     c                   Read      Titpd01l
012700020704      * Fine File
012800020704     c                   If        %Eof(Titpd01l)
012900020704     c                   Leave
013000020704     c                   EndIf
013100020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
013200020704     c                   Movel     TpdKsc        W_poKsc
013300020704     c                   If        W_poKsc <> Lin(xx)
013400020704     c                   Leave
013500020704     c                   EndIf
013600020704      * Cancello i records
013700020704     c                   EndDo
013800020704
013900020704      * Lettura di TiTGc
014000020704     c     Kksc          Setll     Titgc01l
014100020704     c                   Do        *Hival
014200020704     c                   Read      Titgc01l
014300020704      * Fine File
014400020704     c                   If        %Eof(Titgc01l)
014500020704     c                   Leave
014600020704     c                   EndIf
014700020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
014800020704     c                   Movel     TgcKsc        W_poKsc
014900020704     c                   If        W_poKsc <> Lin(xx)
015000020704     c                   Leave
015100020704     c                   EndIf
015200020704      * Cancello i records
015300020704     c                   EndDo
015400020704
015500020704     c                   EndDo
015600020703
015700020704     c                   EndSr
015800020704      *---------------------------------------------------------------*
015900020704      * Routine di scrittura dei files in GRU
016000020704      *---------------------------------------------------------------*
016100020704     c     Sr_Scrivi     BegSr
016200020704
016300020704      * Leggo i terminal SDI
016400020704     c                   Do        30            xx
016500020704     c                   If        Lin(xx) = *Zeros
016600020704     c                   Leave
016700020704     c                   EndIf
016800020704      * Imposto la chiave
016900020704     c                   Clear                   Kksc
017000020704     c                   Movel     Lin(xx)       Kksc
017100020704
017200020704      * Lettura di TnTam
017300020704     c     Kksc          Setll     TntamSdi
017400020704     c                   Do        *Hival
017500020704     c                   Read      TntamSdi
017600020704      * Fine File
017700020704     c                   If        %Eof(TntamSdi)
017800020704     c                   Leave
017900020704     c                   EndIf
018000020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
018100020704     c                   Movel     S_TamKsc      W_poKsc
018200020704     c                   If        W_poKsc <> Lin(xx)
018300020704     c                   Leave
018400020704     c                   EndIf
018500020704      * Scrivo in GRU i records di TntamSdi e files collegati
018600020729     c                   setoff                                       30
018700020729     c   30
018800020729     cann30              ExSr      Sr_WrtTar
018900020704
019000020704     c                   EndDo
019100020704
019200020704     c                   EndDo
019300020704
019400020704     c                   EndSr
019500020115      *---------------------------------------------------------------*
019600020704      * Routine di scrittura di TnTam e files collegati
019700020115      *---------------------------------------------------------------*
019800020704     c     Sr_WrtTar     BegSr
019900020703
020000020704      * Lettura di TiTad
020100020704     c     kTar          Setll     TitadSdi
020200020704     c                   Do        *Hival
020300020704     c     Ktar          Reade     TitadSdi
020400020703      * Fine File
020500020704     c                   If        %Eof(TitadSdi)
020600020704     c                   Leave
020700020704     c                   EndIf
020800020703      * Scrivo File in GRU
020900020704     c                   Eval      DsTad = DsTadS
021000020704     c                   Write     Titad000
021100020703     c                   EndDo
021200020703
021300020704      * Lettura di TiTpt
021400020704     c     kTar          Setll     TitptSdi
021500020704     c                   Do        *Hival
021600020704     c     Ktar          Reade     TitptSdi
021700020703      * Fine File
021800020704     c                   If        %Eof(TitptSdi)
021900020704     c                   Leave
022000020704     c                   Endif
022100020703      * Scrivo File in GRU
022200020704     c                   Eval      DsTpt = DsTptS
022300020704     c                   Write     Titpt000
022400020703
022500020703     c                   EndDo
022600020703
022700020704      * Lettura di TiTpd
022800020704     c     kTar          Setll     TitpdSdi
022900020704     c                   Do        *Hival
023000020704     c     Ktar          Reade     TitpdSdi
023100020703      * Fine File
023200020704     c                   If        %Eof(TitpdSdi)
023300020704     c                   Leave
023400020704     c                   Endif
023500020703      * Scrivo File in GRU
023600020704     c                   Eval      DsTpd = DsTpdS
023700020704     c                   Write     Titpd000
023800020703
023900020703     c                   EndDo
024000020703
024100020704      * Lettura di TiTgc
024200020704     c     kTar          Setll     TitgcSdi
024300020704     c                   Do        *Hival
024400020704     c     Ktar          Reade     TitgcSdi
024500020703      * Fine File
024600020704     c                   If        %Eof(TitgcSdi)
024700020704     c                   Leave
024800020704     c                   Endif
024900020703      * Scrivo File in GRU
025000020704     c                   Eval      DsTgc = DsTgcS
025100020704     c                   Write     Titgc000
025200020703
025300020703     c                   EndDo
025400020703
025500020703      * Tariffe
025600020704     c                   Eval      DsTam = DsTamS
025700020704     c                   Write     Tntam000
025800020703
025900020115     C                   EndSr
026000020703      *---------------------------------------------------------------*
026100020703      * Routine Iniziale
026200020703      *---------------------------------------------------------------*
026300020703     c     *InzSr        BegSr
026400020704
026500020704     c                   eval      codut = 1
026600020704     c                   call      'X§PARUT'
026700020704     c                   parm                    Ut§Dse0f
026800020704     c                   movel     rec80         Cncr80
026900020704
027000020704      * carico i p.o. del terminal
027100020704     c                   clear                   Trul06Ds
027200020704     c                   eval      D06Cod = '£1'
027300020704     c                   movel     SimFel        D06KeY
027400020704     c                   movel     Trul06Ds      KpjBu
027500020704     c                   call      'TRUL06R'
027600020704     c                   parm                    KpjBa
027700020704     c                   movel     KpjBu         Trul06Ds
027800020704
027900020703     c     kTar          Klist
028000020704     c                   Kfld                    S_TamKsc
028100020704     c                   Kfld                    S_TamCtr
028200020704     c                   Kfld                    S_TamPrg
028300020703
028400020703     c                   EndSr
