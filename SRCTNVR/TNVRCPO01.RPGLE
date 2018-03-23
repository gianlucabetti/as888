000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200020925
000300020925      * -------------------------------------------------------------*
000400020925      * Sflegga i potenziali dell'As e delete gli altri
000500020925      * -------------------------------------------------------------*
000600020925
000700020925     fTncpo01l  uf   e           k Disk
000800020925     fTncps11l  uf   e           k Disk
000900020925     fTncpi01l  uf   e           k Disk
001000020925
001100020925     d Kapl            s                   Like(CpiApl) Inz('P')
001200020925     d Knk1            s                   Like(CpiNk1)
001300020925
001400020925     d Tibs56Ds      e ds
001500020925     d  skfel                  7    756  0 dim(250)
001600020925     d AzuteDs       e ds                  ExtName(Azute00f)
001700020925     d Ddatiute      e ds
001800020925     d Tibs34Ds      e ds                  Inz
001900020925
002000020925      * Reperisco i dati utente
002100020925     c     *Dtaara       Define    §azute        AzuteDs
002200020925     c     *Dtaara       Define    §datiute      Ddatiute
002300020925
002400020925     c                   In(E)     *Dtaara
002500020925     c                   If        %Error or Rsut = *Blanks
002600020925     c                   Call      'TIBS34R'
002700020925     c                   Parm                    Tibs34Ds
002800020925     c                   In        *Dtaara
002900020925     c                   EndIf
003000020925
003100020925      * Reperisco tutti i p.o. dell'As
003200020925     c                   Clear                   Tibs56Ds
003300020925     c                   Eval      I56ppo = Simfel
003400020925     c                   Eval      I56mod = 'POA'
003500020925     c                   Call      'TIBS56R'
003600020925     c                   Parm                    Tibs56Ds
003700020925
003800020925     c     Kcpi          Klist
003900020925     c                   Kfld                    Kapl
004000020925     c                   Kfld                    Knk1
004100020925
004200020925      * TNCPO
004300020925     c     *Loval        Setll     Tncpo01l
004400020925     c                   Do        *Hival
004500020925     c                   Read      Tncpo01L
004600020925      * Fine file esco
004700020925     c                   If        %Eof(Tncpo01l)
004800020925     c                   Leave
004900020925     c                   EndIf
005000020925
005100020925     c                   Movel     CpoCpo        Knk1
005200010219
005300020925      * Potenziale non in carico ad un p.o. dell'AS deleto
005400020925     c     CpoFlt        Lookup    Skfel                                  30
005500020925     c                   If        Not *in30
005600020925     c                   Exsr      Elimina
005700020925      * Potenziale in carico ad un p.o. dell'AS sflaggo per la trasmissione
005800020925     c                   Else
005900020925     c                   Exsr      Aggiorna
006000020925     c                   EndIf
006100020925
006200020925     c                   EndDo
006300010718
006400020925     c                   Eval      *InLr = *on
006500020925
006600020925      *---------------------------------------------------------------*
006700020925      * Elimino i potenziali che non fanno parte dell'As              *
006800020925      *---------------------------------------------------------------*
006900020925     c     Elimina       BegSr
007000020925
007100020925      * TNCPS
007200020925     c     CpoCpo        Setll     Tncps11l
007300020925     c                   Do        *Hival
007400020925     c     CpoCpo        Reade     Tncps11l
007500020925      * Fine file esco
007600020925     c                   If        %Eof(Tncps11l)
007700020925     c                   Leave
007800020925     c                   EndIf
007900020925     c                   Delete    Tncps000
008000020925     c                   EndDo
008100020925
008200020925      * TNCPS
008300020925     c     Kcpi          Setll     Tncpi01l
008400020925     c                   Do        *Hival
008500020925     c     Kcpi          Reade     Tncpi01l
008600020925      * Fine file esco
008700020925     c                   If        %Eof(Tncpi01l)
008800020925     c                   Leave
008900020925     c                   EndIf
009000020925     c                   Delete    Tncpi000
009100020925     c                   EndDo
009200020925
009300020925      * TNCPO
009400020925     c                   Delete    Tncpo000
009500020925
009600020925     c                   EndSr
009700020925
009800020925      *---------------------------------------------------------------*
009900020925      * Aggiorno i potenziali che fanno parte dell'As                 *
010000020925      *---------------------------------------------------------------*
010100020925     c     Aggiorna      BegSr
010200020925
010300020925      * TNCPS
010400020925     c     CpoCpo        Setll     Tncps11l
010500020925     c                   Do        *Hival
010600020925     c     CpoCpo        Reade     Tncps11l
010700020925      * Fine file esco
010800020925     c                   If        %Eof(Tncps11l)
010900020925     c                   Leave
011000020925     c                   EndIf
011100020925     c                   Clear                   CpsFtr
011200020925     c                   Update    Tncps000
011300020925     c                   EndDo
011400020925
011500020925      * TNCPS
011600020925     c     Kcpi          Setll     Tncpi01l
011700020925     c                   Do        *Hival
011800020925     c     Kcpi          Reade     Tncpi01l
011900020925      * Fine file esco
012000020925     c                   If        %Eof(Tncpi01l)
012100020925     c                   Leave
012200020925     c                   EndIf
012300020925     c                   Clear                   CpiFtr
012400020925     c                   Update    Tncpi000
012500020925     c                   EndDo
012600020925
012700020925      * TNCPO
012800020925     c                   Clear                   CpoFtr
012900020925     c                   Update    Tncpo000
013000020925
013100020925     c                   EndSr
