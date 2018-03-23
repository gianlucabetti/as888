000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020703
000300020710      * TNVR39R *------------------------------------------------------*
000400020710      * Elimina i records che non fanno parte del terminal di partenza
000500020710      *----------------------------------------------------------------*
000600020703
000700020710     fTnass00f  up a e             Disk
000800020710
000900020710     fQsysprt   o    f  132        Printer OfLind(*InOf)
001000020710
001100020710     d W_poKsc         s              3  0
001200020710     d W_Prima         s              1
001300020703
001400020704     d Ut§Dse0f      e ds
001500020704     d Cncr80        e ds
001600020704     d Trul06Ds      e ds
001700020704     d  Lin                    1     90  0 Dim(30)
001800020704     d KpjBa         e ds
001900020703
002000020704      * Prima devo eliminare dalle tariffe in GRU le tariffe che hanno come p.o. cliente
002100020704      * un p.o. SDI
002200020704     c                   ExSr      Sr_Elimina
002300020704
002400020704     c                   Eval      *InLr = *on
002500020704
002600020704      *---------------------------------------------------------------*
002700020710      * Routine per eliminare i records che non sono del terminal
002800020704      *---------------------------------------------------------------*
002900020704     c     Sr_Elimina    BegSr
003000020704
003100020710      * Il p.o. del cliente se diverso all'emento della £1
003200020710     c                   Movel     AssKsc        W_poKsc
003300020710     c     W_poKsc       Lookup    Lin                                    30
003400020710      * Cancello il record
003500020710     c                   If        not *In30
003600020710     c                   If        W_Prima = *off
003700020710     c                   Except    Testa
003800020710     c                   eval      W_Prima = *on
003900020710     c                   EndIf
004000020710     c   of              Except    Testa
004100020710     c                   Except    Riga
004200020710     c                   Delete    Tnass000
004300020710     c                   EndIf
004400020703
004500020704     c                   EndSr
004600020703      *---------------------------------------------------------------*
004700020703      * Routine Iniziale
004800020703      *---------------------------------------------------------------*
004900020703     c     *InzSr        BegSr
005000020704
005100020710     c                   Eval      Codut = 1
005200020710     c                   Call      'X§PARUT'
005300020710     c                   Parm                    Ut§Dse0f
005400020710     c                   Movel     Rec80         Cncr80
005500020704
005600020704      * carico i p.o. del terminal
005700020710     c                   Clear                   Trul06Ds
005800020710     c                   Eval      D06Cod = '£1'
005900020710     c                   Movel     SimFel        D06KeY
006000020710     c                   Movel     Trul06Ds      KpjBu
006100020710     c                   Call      'TRUL06R'
006200020710     c                   Parm                    KpjBa
006300020710     c                   Movel     KpjBu         Trul06Ds
006400020703
006500020703     c                   EndSr
006600020710
006700020710     oQsysprt   e            Testa          2 02
006800020710     o                                         + 10 'Cancellazione Records '
006900020710     o                                         +  1 'in Tnass che non sono'
007000020710     o                                         +  1 'del terminal di partenza'
007100020710     o                                         +  8 'Tnvr39r'
007200020710     o                       Udate             +  5 '  /  /  '
007300020710     o                                         +  5 'Pag.'
007400020710     o                       Page          Z
007500020710     o          e            Riga           1
007600020710     o                       AssKsc            + 11
007700020710     O                       AssCtr            +  4
007800020710     O                       AssPrg            +  4
