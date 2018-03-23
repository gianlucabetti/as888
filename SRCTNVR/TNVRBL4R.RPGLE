000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031022      *       TRASCODIFICA BL4 RCD "E" IN VAT RCD "C"                 *
000600031017      *                                                               *
000700031017      *---------------------------------------------------------------*
000800031017
000900031021     fFnbl401l  uf   e           k Disk
001000031021     fFnblp01l  if   e           k Disk
001100031021     fFivab04l  if   e           k Disk
001200031021     fFivat00f  o    e             Disk
001300031017     fQsysprt   o    f  132        Printer Oflind(*InOf)
001400031017
001500031017     c     kBlp          klist
001600031017     c                   kfld                    Bl4Aas
001700031017     c                   kfld                    Bl4Lnp
001800031017     c                   kfld                    Bl4Nrs
001900031017     c                   kfld                    Bl4Nsp
002000031017
002100031017     c     *Loval        Setll     Fnbl401l
002200031021do  1c                   Do        *Hival
002300031017     c                   Read      Fnbl401l
002400031017     c                   If        %Eof(Fnbl401l)
002500031017     c                   Leave
002600031017     c                   EndIf
002700031017      * Escludo gli annullati
002800031017     c                   If        Bl4Atb <> *Blanks
002900031017     c                   Iter
003000031017     c                   EndIf
003100031021      * Solo i tipi record 'E'
003200031021     c                   If        Bl4Trc <> 'E'
003300031021     c                   Iter
003400031021     c                   EndIf
003500031017
003600031021      * Non deve avere già la bolla
003700031017     c     kBlp          Setll     Fnblp01l
003800031021if  2c                   If        Not %Equal(Fnblp01l)
003900031021      * Deve esistere il relativo VAB
004000031021     c     kBlp          Chain     Fivab04l
004100031021if  3c                   If        %Found(Fnblp01l)
004200031021      * Trascodifico BL4 in VAT
004300031021     c                   If        *In90 = *Off or *InOf
004400031021     c                   Except    Testa
004500031021     c  n90              Eval      *In90 = *On
004600031021     c                   EndIf
004700031021     c                   Except    Riga
004800031021     c                   Goto      no
004900031021     c                   Clear                   Fivat000
005000031021     c                   Eval      VatFgs = VabFgs
005100031021     c                   Eval      VatCcm = VabCcm
005200031021     c                   Eval      VatLnp = Bl4Lnp
005300031021     c                   Eval      VatAas = Bl4Aas
005400031021     c                   Eval      VatNrs = Bl4Nrs
005500031021     c                   Eval      VatNsp = Bl4Nsp
005600031022     c                   Eval      VatTrc = 'C'
005700031021     c                   Eval      VatNot = Bl4Not
005800031021     c                   Write     Fivat000
005900031021     c                   Delete    Fnbl4000
006000031021     c     no            tag
006100031021    3c                   EndIf
006200031021    2c                   EndIf
006300031017
006400031021    1c                   EndDo
006500031017
006600031017     c                   Eval      *InLr = *On
006700031017
006800031017     oQsysprt   e            Testa          2 02
006900031021     o                                         +  5 'Trascodifica BL4 rcd "E"'
007000031022     o                                         +  1 'in VAT rcd "C"'
007100031021     o                                           85 'TNVRBL4R'
007200031017     o                       *Date         y    110
007300031017     o                                          128 'Pag.'
007400031017     o                       Page          z    132
007500031021     o          e            Riga           1
007600031017     o                                           +1 'Spedizione:'
007700031017     o                       Bl4Aas              +1
007800031017     o                       Bl4Lnp              +1
007900031017     o                       Bl4Nrs        z     +1
008000031017     o                       Bl4Nsp        z     +1
008100031017     o                                           +2 'TRC:'
008200031017     o                       Bl4Trc              +1
008300031017     o                                           +1 'BL4NOT:'
008400031017     o                       Bl4Not              +1
