000100050705     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050705
000300050705      *------------------------------------------------------------------------*
000400050705      *                                                                        *
000500051111      *                 RIPRISTINO TARIFFE ANNULLATE       ?                   *
000600050705      *                                                                        *
000700050705      *------------------------------------------------------------------------*
000800050705
000900051111     fTntam01l  uf   e           k disk
001000051111     fTitad04l  uf   e           k disk
001100051111     fTitpt01l  uf   e           k disk
001200051111     fTitpd01l  uf   e           k disk
001300051111     fTitgc01l  uf   e           k disk
001400051111     fTitav01l  uf   e           k disk
001500051111     fTeetc02l  uf   e           k disk
001600051111     fTeets01l  uf   e           k disk
001601061009     fTeets11l  uf   e           k disk    rename(teets000:teets100)
001700051111     fTeett01l  uf   e           k disk
001800051111     fTnta99d   cf   e             workstn
001900050705
002000050705      *------------------------------------------------------------------------*
002100050705      *  RIEPILOGO INDICATORI
002200050705      *------------------------------------------------------------------------*
002300051111      * 28 - Errore generico
002400051111      * 40 - Tariffa inesistente
002500050705      *------------------------------------------------------------------------*
002600050705
002700050705      *   V A R I A B I L I
002800050819
002900050705      *   S C H I E R E
003000051111     d msg             s             30    Dim(10) ctdata perrcd(1)
003100050705
003200050705      *   D S   I N T E R N E / E S T E R N E
003300050705
003400050705      *------------------------------------------------------------------------*
003500050819
003600051111
003700051111     c                   Clear                   w01ksc
003800051111     c                   Clear                   w01ctr
003900051111     c                   Clear                   w01prg
004000051111
004100051111     c                   Do        *Hival
004200050706
004300050706      * Emetto la videata
004400051111     c                   Exfmt     ta9901w
004500051111
004600050819     c                   Eval      *In28 = *Off
004700051111     c                   Clear                   w01msg
004800051111
004900050819      * F3=Fine
005000051111     c   kc              Leave
005100051111
005200050819      * Controllo
005300051111     c                   ExSr      Sr_Ctrw01
005400051111     c   28              Iter
005500051111
005600050712      * per enter devo restare nella videata
005700051111     c  nkf              Iter
005800051111
005900050711      * F6=Conferma
006000051111     c                   If        *InKf
006100051111     c                   ExSr      Sr_Conferma
006200051111     c                   Leave
006300051111     c                   EndIf
006400051111
006500051111     c                   EndDo
006600050819
006700050819     c                   Eval      *InLr = *On
006800050705
006900050819      *------------------------------------------------------------------------*
007000051111      * CONTROLLO
007100050819      *------------------------------------------------------------------------*
007200051111     c     Sr_Ctrw01     BegSr
007300050707
007400051111     c                   Eval      *In40 = *Off
007500051111
007600051111if  1c                   If        w01ksc = *Zeros
007700051111     c                   Eval      *In28 = *On
007800051111     c                   Eval      *In40 = *On
007900051111     c                   Eval      w01msg = msg(01)
008000050705     c                   Leavesr
008100051111    1c                   EndIf
008200051111
008300051111     c     kTntam        Chain(n)  Tntam01l
008400051111if  1c                   If        Not %Found(Tntam01l)
008500050705     c                   Eval      *In28 = *On
008600051111     c                   Eval      *In40 = *On
008700051111     c                   Eval      w01msg = msg(02)
008800050705     c                   Leavesr
008900051111    1c                   EndIf
009000051111
009100051111if  1c                   If        tamatb = *Blanks
009200050705     c                   Eval      *In28 = *On
009300051111     c                   Eval      *In40 = *On
009400051111     c                   Eval      w01msg = msg(03)
009500050705     c                   Leavesr
009600051111    1c                   EndIf
009700050707
009800050819     c                   EndSr
009900050729
010000050729      *------------------------------------------------------------------------*
010100050729      * CONFERMA
010200050729      *------------------------------------------------------------------------*
010300050729     c     Sr_Conferma   BegSr
010400051111
010500051111     c     kTntam        Chain     Tntam01l
010600051111     c                   If        %Found(Tntam01l) and tamatb <> *Blanks
010700051111     c                   Clear                   tamatb
010800051111     c                   Update    Tntam000
010900051111     c                   EndIf
011000051111
011100051111     c     kTntam        Setll     Titad04l
011200051111     c                   Do        *Hival
011300051111     c     kTntam        Reade     Titad04l
011400051111     c                   If        %Eof(Titad04l)
011500051111     c                   Leave
011600051111     c                   EndIf
011700051111     c                   If        tadatb = *Blanks
011800051111     c                   Iter
011900051111     c                   EndIf
012000051111     c                   Clear                   tadatb
012100051111     c                   Update    Titad000
012200051111     c                   EndDo
012300051111
012400051111     c     kTntam        Setll     Titpt01l
012500051111     c                   Do        *Hival
012600051111     c     kTntam        Reade     Titpt01l
012700051111     c                   If        %Eof(Titpt01l)
012800051111     c                   Leave
012900051111     c                   EndIf
013000051111     c                   If        tptatb = *Blanks
013100051111     c                   Iter
013200051111     c                   EndIf
013300051111     c                   Clear                   tptatb
013400051111     c                   Update    Titpt000
013500051111     c                   EndDo
013600051111
013700051111     c     kTntam        Setll     Titpd01l
013800051111     c                   Do        *Hival
013900051111     c     kTntam        Reade     Titpd01l
014000051111     c                   If        %Eof(Titpd01l)
014100051111     c                   Leave
014200051111     c                   EndIf
014300051111     c                   If        tpdatb = *Blanks
014400051111     c                   Iter
014500051111     c                   EndIf
014600051111     c                   Clear                   tpdatb
014700051111     c                   Update    Titpd000
014800051111     c                   EndDo
014900051111
015000051111     c     kTntam        Chain     Titgc01l
015100051111     c                   If        %Found(Titgc01l) and tgcatb <> *Blanks
015200051111     c                   Clear                   tgcatb
015300051111     c                   Update    Titgc000
015400051111     c                   EndIf
015500051111
015600051111     c     kTntam        Setll     Titav01l
015700051111     c                   Do        *Hival
015800051111     c     kTntam        Reade     Titav01l
015900051111     c                   If        %Eof(Titav01l)
016000051111     c                   Leave
016100051111     c                   EndIf
016200051111     c                   If        tavatb = *Blanks
016300051111     c                   Iter
016400051111     c                   EndIf
016500051111     c                   Clear                   tavatb
016600051111     c                   Update    Titav000
016700051111     c                   EndDo
016800051111     c* DATI TIPO
016900051111     c     kTntam        Setll     Teetc02l
017000051111     c                   Do        *Hival
017100051111     c     kTntam        Reade     Teetc02l
017200051111     c                   If        %Eof(Teetc02l)
017300051111     c                   Leave
017400051111     c                   EndIf
017500051111     c                   If        etccto <> 'T'
017600051111     c                   Iter
017700051111     c                   EndIf
017800051111     c* DATI TIPO SCAGLIONI
017900051111     c     ketc          Setll     Teets01l
018000051111     c                   Do        *Hival
018100051111     c     ketc          Reade     Teets01l
018200051111     c                   If        %Eof(Teets01l)
018300051111     c                   Leave
018400051111     c                   EndIf
018500051111     c                   Clear                   etsatb
018600051111     c                   Update    Teets000
018700051111     c                   EndDo
018701061009     c* DATI TIPO SCAGLIONI x competenze
018702061009     c     ketc          Setll     Teets11l
018703061009     c                   Do        *Hival
018704061009     c     ketc          Reade     Teets11l
018705061009     c                   If        %Eof(Teets11l)
018706061009     c                   Leave
018707061009     c                   EndIf
018708061009     c                   Clear                   etsatb
018709061009     c                   Update    Teets100
018710061009     c                   EndDo
018800051111     c* DATI TIPO CODICE TASSAZIONE
018900051111     c     ketc          Setll     Teett01l
019000051111     c                   Do        *Hival
019100051111     c     ketc          Reade     Teett01l
019200051111     c                   If        %Eof(Teett01l)
019300051111     c                   Leave
019400051111     c                   EndIf
019500051111     c                   Clear                   ettatb
019600051111     c                   Update    Teett000
019700051111     c                   EndDo
019800051111     c*
019900051111     c                   Clear                   etcatb
020000051111     c                   Update    Teetc000
020100051111     c                   EndDo
020200051111
020300050729     c                   EndSr
020400050819
020500050705      *------------------------------------------------------------------------*
020600050705      * ROUTINE INIZIALE
020700050705      *------------------------------------------------------------------------*
020800050705     c     *Inzsr        BegSr
020900050705
021000050705      * KLIST
021100050705     c     kTntam        klist
021200051111     c                   kfld                    w01ksc
021300051111     c                   kfld                    w01ctr
021400051111     c                   kfld                    w01prg
021500051111     c     ketc          klist
021600051111     c                   kfld                    ETcDSF
021700051111     c                   kfld                    ETcCTO
021800051111     c                   kfld                    ETcKSC
021900051111     c                   kfld                    ETcCTR
022000051111     c                   kfld                    ETcPRG
022100051111     c                   kfld                    ETcLNP
022200050705
022300050705     c                   EndSr
022400050705      *------------------------------------------------------------------------*
022500050705
022600051111** MSG  Lungh. 30            *
022700051111Immettere la tariffa          01
022800051111Tariffa non trovata           02
022900051111Tariffa non annullata         03
