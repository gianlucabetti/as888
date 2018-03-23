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
002000020704     fTntam01l  uf a e           k disk
002100020704     fTitad04l  uf a e           k disk
002200020704     fTitpt01l  uf a e           k disk
002300020704     fTitpd01l  uf a e           k disk
002400020704     fTitgc01l  uf a e           k disk
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
008900020704     c                   Delete    Tntam000
009000020703     c                   EndDo
009100020704
009200020704      * Lettura di TiTad
009300020704     c     Kksc          Setll     Titad04l
009400020704     c                   Do        *Hival
009500020704     c                   Read      Titad04l
009600020704      * Fine File
009700020704     c                   If        %Eof(Titad04l)
009800020704     c                   Leave
009900020704     c                   EndIf
010000020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
010100020704     c                   Movel     TadKsc        W_poKsc
010200020704     c                   If        W_poKsc <> Lin(xx)
010300020704     c                   Leave
010400020704     c                   EndIf
010500020704      * Cancello i records
010600020704     c                   Delete    Titad000
010700020704     c                   EndDo
010800020704
010900020704      * Lettura di TiTpt
011000020704     c     Kksc          Setll     Titpt01l
011100020704     c                   Do        *Hival
011200020704     c                   Read      Titpt01l
011300020704      * Fine File
011400020704     c                   If        %Eof(Titpt01l)
011500020704     c                   Leave
011600020704     c                   EndIf
011700020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
011800020704     c                   Movel     TptKsc        W_poKsc
011900020704     c                   If        W_poKsc <> Lin(xx)
012000020704     c                   Leave
012100020704     c                   EndIf
012200020704      * Cancello i records
012300020704     c                   Delete    Titpt000
012400020704     c                   EndDo
012500020704
012600020704      * Lettura di TiTpd
012700020704     c     Kksc          Setll     Titpd01l
012800020704     c                   Do        *Hival
012900020704     c                   Read      Titpd01l
013000020704      * Fine File
013100020704     c                   If        %Eof(Titpd01l)
013200020704     c                   Leave
013300020704     c                   EndIf
013400020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
013500020704     c                   Movel     TpdKsc        W_poKsc
013600020704     c                   If        W_poKsc <> Lin(xx)
013700020704     c                   Leave
013800020704     c                   EndIf
013900020704      * Cancello i records
014000020704     c                   Delete    Titpd000
014100020704     c                   EndDo
014200020704
014300020704      * Lettura di TiTGc
014400020704     c     Kksc          Setll     Titgc01l
014500020704     c                   Do        *Hival
014600020704     c                   Read      Titgc01l
014700020704      * Fine File
014800020704     c                   If        %Eof(Titgc01l)
014900020704     c                   Leave
015000020704     c                   EndIf
015100020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
015200020704     c                   Movel     TgcKsc        W_poKsc
015300020704     c                   If        W_poKsc <> Lin(xx)
015400020704     c                   Leave
015500020704     c                   EndIf
015600020704      * Cancello i records
015700020704     c                   Delete    Titgc000
015800020704     c                   EndDo
015900020704
016000020704     c                   EndDo
016100020703
016200020704     c                   EndSr
016300020704      *---------------------------------------------------------------*
016400020704      * Routine di scrittura dei files in GRU
016500020704      *---------------------------------------------------------------*
016600020704     c     Sr_Scrivi     BegSr
016700020704
016800020704      * Leggo i terminal SDI
016900020704     c                   Do        30            xx
017000020704     c                   If        Lin(xx) = *Zeros
017100020704     c                   Leave
017200020704     c                   EndIf
017300020704      * Imposto la chiave
017400020704     c                   Clear                   Kksc
017500020704     c                   Movel     Lin(xx)       Kksc
017600020704
017700020704      * Lettura di TnTam
017800020704     c     Kksc          Setll     TntamSdi
017900020704     c                   Do        *Hival
018000020704     c                   Read      TntamSdi
018100020704      * Fine File
018200020704     c                   If        %Eof(TntamSdi)
018300020704     c                   Leave
018400020704     c                   EndIf
018500020704      * Il p.o. del cliente deve essere uguale all'emento che ho impostato nella chiave
018600020704     c                   Movel     S_TamKsc      W_poKsc
018700020704     c                   If        W_poKsc <> Lin(xx)
018800020704     c                   Leave
018900020704     c                   EndIf
019000020704      * Scrivo in GRU i records di TntamSdi e files collegati
019100020704     c                   ExSr      Sr_WrtTar
019200020704
019300020704     c                   EndDo
019400020704
019500020704     c                   EndDo
019600020704
019700020704     c                   EndSr
019800020115      *---------------------------------------------------------------*
019900020704      * Routine di scrittura di TnTam e files collegati
020000020115      *---------------------------------------------------------------*
020100020704     c     Sr_WrtTar     BegSr
020200020703
020300020704      * Lettura di TiTad
020400020704     c     kTar          Setll     TitadSdi
020500020704     c                   Do        *Hival
020600020704     c     Ktar          Reade     TitadSdi
020700020703      * Fine File
020800020704     c                   If        %Eof(TitadSdi)
020900020704     c                   Leave
021000020704     c                   EndIf
021100020703      * Scrivo File in GRU
021200020704     c                   Eval      DsTad = DsTadS
021300020704     c                   Write     Titad000
021400020703     c                   EndDo
021500020703
021600020704      * Lettura di TiTpt
021700020704     c     kTar          Setll     TitptSdi
021800020704     c                   Do        *Hival
021900020704     c     Ktar          Reade     TitptSdi
022000020703      * Fine File
022100020704     c                   If        %Eof(TitptSdi)
022200020704     c                   Leave
022300020704     c                   Endif
022400020703      * Scrivo File in GRU
022500020704     c                   Eval      DsTpt = DsTptS
022600020704     c                   Write     Titpt000
022700020703
022800020703     c                   EndDo
022900020703
023000020704      * Lettura di TiTpd
023100020704     c     kTar          Setll     TitpdSdi
023200020704     c                   Do        *Hival
023300020704     c     Ktar          Reade     TitpdSdi
023400020703      * Fine File
023500020704     c                   If        %Eof(TitpdSdi)
023600020704     c                   Leave
023700020704     c                   Endif
023800020703      * Scrivo File in GRU
023900020704     c                   Eval      DsTpd = DsTpdS
024000020704     c                   Write     Titpd000
024100020703
024200020703     c                   EndDo
024300020703
024400020704      * Lettura di TiTgc
024500020704     c     kTar          Setll     TitgcSdi
024600020704     c                   Do        *Hival
024700020704     c     Ktar          Reade     TitgcSdi
024800020703      * Fine File
024900020704     c                   If        %Eof(TitgcSdi)
025000020704     c                   Leave
025100020704     c                   Endif
025200020703      * Scrivo File in GRU
025300020704     c                   Eval      DsTgc = DsTgcS
025400020704     c                   Write     Titgc000
025500020703
025600020703     c                   EndDo
025700020703
025800020703      * Tariffe
025900020704     c                   Eval      DsTam = DsTamS
026000020704     c                   Write     Tntam000
026100020703
026200020115     C                   EndSr
026300020703      *---------------------------------------------------------------*
026400020703      * Routine Iniziale
026500020703      *---------------------------------------------------------------*
026600020703     c     *InzSr        BegSr
026700020704
026800020704     c                   eval      codut = 1
026900020704     c                   call      'X§PARUT'
027000020704     c                   parm                    Ut§Dse0f
027100020704     c                   movel     rec80         Cncr80
027200020704
027300020704      * carico i p.o. del terminal
027400020704     c                   clear                   Trul06Ds
027500020704     c                   eval      D06Cod = '£1'
027600020704     c                   movel     SimFel        D06KeY
027700020704     c                   movel     Trul06Ds      KpjBu
027800020704     c                   call      'TRUL06R'
027900020704     c                   parm                    KpjBa
028000020704     c                   movel     KpjBu         Trul06Ds
028100020704
028200020703     c     kTar          Klist
028300020704     c                   Kfld                    S_TamKsc
028400020704     c                   Kfld                    S_TamCtr
028500020704     c                   Kfld                    S_TamPrg
028600020703
028700020703     c                   EndSr
