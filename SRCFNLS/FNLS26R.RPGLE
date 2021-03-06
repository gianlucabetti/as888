000100040226     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200040226
000300040226      *------------------------------------------------------------------------*
000400040226      *            REPERISCE RIFERIMENTI MITTENTE DA BOLLA RC
000500040226      *------------------------------------------------------------------------*
000600040226
000700040226      * INPUT
000800040226      * -----
000900040226      *  I26TLA   Tipo di lancio
001000040226      *  I26AAS   Anno spedizione
001100040226      *  I26LNP   Linea di partenza
001200040226      *  I26NRS   Serie
001300040226      *  I26NSP   Spedizione
001400040226
001500040226      * OUTPUT
001600040226      * ------
001700040226      *  O26ERR   Codice di errore:
001800040226      *           00 = Nessun errore
001900040226      *           01 = Non trovata bolla
002000040226      *  O26MSG   Messaggio di errore
002100040226      *  O26RMA   Riferimento mittente alfanumerico
002200040226      *  O26RMN   Riferimento mittente numerico
002300040226
002400040226      *------------------------------------------------------------------------*
002500040226
002600040226     fFnarb01l  if   e           k Disk
002700040226
002800040226      *------------------------------------------------------------------------*
002900040226      *   S C H I E R E
003000040226      *------------------------------------------------------------------------*
003100040226
003200040226      * Messaggi di errore
003300040226     d msg             s             78    Dim(05) Ctdata Perrcd(1)
003400040226
003500040226      *------------------------------------------------------------------------*
003600040226      *   D S   I N T E R N E / E S T E R N E
003700040226      *------------------------------------------------------------------------*
003800040226
003900040226      * Passaggio Parametri
004000040226     d Fnls26ds      e ds
004100040226
004200040226     d Kpjba         e ds
004300040226
004400040226      *------------------------------------------------------------------------*
004500040226
004600040226     c     *Entry        Plist
004700040226     c                   Parm                    Fnls26ds
004800040226
004900040226      * Controllo il tipo lancio
005000040226if  1c                   If        I26Tla <> *Blanks and I26Tla <> 'C' and
005100040226     c                             I26Tla <> 'L'
005200040226     c                   Clear                   I26Tla
005300040226    1c                   EndIf
005400040226
005500040226      * Elaboro
005600040226if  1c                   If        I26Tla <> 'C'
005700040226     c                   ExSr      Sr_Repdati
005800040226    1c                   EndIf
005900040226
006000040226      * Fine
006100040226     c                   ExSr      Sr_EndPgm
006200040226
006300040226      *------------------------------------------------------------------------*
006400040226      * Reperisco i dati
006500040226      *------------------------------------------------------------------------*
006600040226     c     Sr_Repdati    BegSr
006700040226
006800040226      * pulisco i campi di output
006900040226     c                   Clear                   O26Err
007000040226     c                   Clear                   O26Msg
007100040226     c                   Clear                   O26Rma
007200040226     c                   Clear                   O26Rmn
007300040226
007400040226      * preparo la chiave
007500040227     c                   If        I26Aas < 49
007600040226     c                   Movel     20            ArbAas
007700040226     c                   Else
007800040226     c                   Movel     19            ArbAas
007900040226     c                   EndIf
008000040226     c                   Move      I26Aas        ArbAas
008100040226     c                   Move      I26Lnp        ArbLnp
008200040226     c                   Move      I26Nrs        ArbNrs
008300040226     c                   Move      I26Nsp        ArbNsp
008400040226      * cerco la bolla
008500040226     c     kArb          Chain     Fnarb01l
008600040226      * trovata imposto i campi di output
008700040226if  1c                   If        %Found(Fnarb01l)
008800040226     c                   Eval      O26Rma = ARbRma
008900040226     c                   Eval      O26Rmn = ArbRmn
009000040226      * non trova errore
009100040226   x1c                   Else
009200040226     c                   Eval      O26Err = 01
009300040226    1c                   EndIf
009400040226
009500040226     c                   EndSr
009600040226
009700040226      *------------------------------------------------------------------------*
009800040226      * Fine programma
009900040226      *------------------------------------------------------------------------*
010000040226     c     Sr_EndPgm     BegSr
010100040226
010200040226      * restituzione dei dati
010300040226sel 1c                   Select
010400040226
010500040226w   1c                   When      O26Err = *Zeros and I26Tla <> 'C'
010600040226     c                   Eval      O26Rma = ArbRma
010700040226     c                   Eval      O26Rmn = ArbRmn
010800040226
010900040226w   1c                   When      O26Err > *Zeros
011000040226     c                   Eval      O26Msg = msg(O26Err)
011100040226
011200040226    1c                   EndSl
011300040226
011400040226if  1c                   If        I26Tla = *Blanks
011500040226     c                   Eval      *InRt = *On
011600040226   x1c                   Else
011700040226     c                   Eval      *InLr = *On
011800040226    1c                   EndIf
011900040226
012000040226     c                   Return
012100040226
012200040226     c                   EndSr
012300040226
012400040226      *------------------------------------------------------------------------*
012500040226      * Operazioni iniziali
012600040226      *------------------------------------------------------------------------*
012700040226     c     *Inzsr        BegSr
012800040226
012900040226      * Klist
013000040226
013100040226     c     kArb          Klist
013200040226     c                   Kfld                    ArbAas
013300040226     c                   Kfld                    ArbLnp
013400040226     c                   Kfld                    ArbNrs
013500040226     c                   Kfld                    ArbNsp
013600040226
013700040226     c                   EndSr
013800040226
013900040226** Msg                                                                       *
014000040226FNLS24R - Bolla non trovata                                                   01
