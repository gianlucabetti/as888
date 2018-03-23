000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031017      *       ELIMINA BL4/AR4 SENZA BOLLA                             *
000600031017      *                                                               *
000700031017      *---------------------------------------------------------------*
000800031017
000900031017     fFnbl401l  uf   e           k Disk    Usropn
001000031017     fFnblp01l  if   e           k Disk    Usropn
001100031017     fFivab04l  if   e           k Disk    Usropn
001200031017     fFnar401l  uf   e           k Disk    Usropn
001300031017     fFnarb01l  if   e           k Disk    Usropn
001400031017     fQsysprt   o    f  132        Printer Oflind(*InOf)
001500031017
001600031017     d noblp           s              1
001700031017
001800031017     c     *Entry        Plist
001900031017     c                   Parm                    File              3
002000031017     c                   Parm                    Aggio             1
002100031017
002200031017     c     kBlp          klist
002300031017     c                   kfld                    Bl4Aas
002400031017     c                   kfld                    Bl4Lnp
002500031017     c                   kfld                    Bl4Nrs
002600031017     c                   kfld                    Bl4Nsp
002700031017
002800031017     c     kArb          klist
002900031017     c                   kfld                    Ar4Aas
003000031017     c                   kfld                    Ar4Lnp
003100031017     c                   kfld                    Ar4Nrs
003200031017     c                   kfld                    Ar4Nsp
003300031017
003400031017      * Richiesta la pulizia di FNBL400F
003500031017if  1c                   If        File = 'BL4'
003600031017     c                   Open      Fnbl401l
003700031017     c                   Open      Fnblp01l
003800031017     c                   Open      Fivab04l
003900031017
004000031017     c     *Loval        Setll     Fnbl401l
004100031017do  2c                   Do        *Hival
004200031017     c                   Read      Fnbl401l
004300031017     c                   If        %Eof(Fnbl401l)
004400031017     c                   Leave
004500031017     c                   EndIf
004600040213      * Elaboro fino al 2004 escluso
004700040213     c                   If        Bl4Aas >= 2004
004800031017     c                   Leave
004900031017     c                   EndIf
005000031017      * Escludo gli annullati
005100031017     c                   If        Bl4Atb <> *Blanks
005200031017     c                   Iter
005300031017     c                   EndIf
005400031017
005500031017     c                   Clear                   noblp
005600031017      * Cerco la bolla
005700031017     c     kBlp          Setll     Fnblp01l
005800031017     c                   If        Not %Equal(Fnblp01l)
005900031017     c                   Eval      Noblp = '1'
006000031017     c                   EndIf
006100031017      * Se tipo record 'E' controllo se esiste il record nel VAB
006200031017if  3c                   If        Bl4Trc = 'E'
006300031017     c                   Clear                   Noblp
006400031017     c     kBlp          Setll     Fivab04l
006500031017     c                   If        Not %Equal(Fnblp01l)
006600031017     c                   Eval      Noblp = '1'
006700031017     c                   EndIf
006800031017    3c                   EndIf
006900031017      * Cancello il record in FNBL4
007000031017if  3c                   If        Noblp = '1'
007100031017     c                   If        *In90 = *Off or *InOf
007200031017     c                   Except    Testa
007300031017     c  n90              Eval      *In90 = *On
007400031017     c                   EndIf
007500031017     c                   Except    RigaBl4
007600031017     c                   If        Aggio = 'S'
007700031017     c                   Delete    Fnbl4000
007800031017     c                   EndIf
007900031017    3c                   EndIf
008000031017
008100031017    2c                   EndDo
008200031017    1c                   EndIf
008300031017
008400031017
008500031017      * Richiesta la pulizia di FNAR400F
008600031017if  1c                   If        File = 'AR4'
008700031017     c                   Open      Fnar401l
008800031017     c                   Open      Fnarb01l
008900031017
009000031017     c     *Loval        Setll     Fnar401l
009100031017do  2c                   Do        *Hival
009200031017     c                   Read      Fnar401l
009300031017     c                   If        %Eof(Fnar401l)
009400031017     c                   Leave
009500031017     c                   EndIf
009600040213      * Elaboro fino al 2004 escluso
009700040213     c                   If        Ar4Aas >= 2004
009800031017     c                   Leave
009900031017     c                   EndIf
010000031017      * Escludo gli annullati
010100031017     c                   If        Ar4Atb <> *Blanks
010200031017     c                   Iter
010300031017     c                   EndIf
010400031017      * Cerco la bolla
010500031017     c     kArb          Setll     Fnarb01l
010600031017if  3c                   If        Not %Equal(Fnarb01l)
010700031017      * Cancello il record in FNAR4
010800031017     c                   If        *In90 = *Off or *InOf
010900031017     c                   Except    Testa
011000031017     c  n90              Eval      *In90 = *On
011100031017     c                   EndIf
011200031017     c                   Except    RigaAr4
011300031017     c                   If        Aggio = 'S'
011400031017     c                   Delete    Fnar4000
011500031017     c                   EndIf
011600031017    3c                   EndIf
011700031017
011800031017    2c                   EndDo
011900031017    1c                   EndIf
012000031017
012100031017     c                   Eval      *InLr = *On
012200031017
012300031017     oQsysprt   e            Testa          2 02
012400031017     o                                         +  5 'Pulizia FNBL4/FNAR4 senza '
012500031017     o                                         +  1 'Record corrispondente in '
012600031017     o                                         +  1 'FNBLP/FNARB'
012700031017     o                                           85 'TNVRR4R'
012800031017     o                       *Date         y    110
012900031017     o                                          128 'Pag.'
013000031017     o                       Page          z    132
013100031017     o          e            RigaBl4        1
013200031017     o                                           +1 'Spedizione:'
013300031017     o                       Bl4Aas              +1
013400031017     o                       Bl4Lnp              +1
013500031017     o                       Bl4Nrs        z     +1
013600031017     o                       Bl4Nsp        z     +1
013700031017     o                                           +2 'TRC:'
013800031017     o                       Bl4Trc              +1
013900031017     o                                           +1 'BL4NOT:'
014000031017     o                       Bl4Not              +1
014100031017     o          e            RigaAr4        1
014200031017     o                                           +1 'Spedizione:'
014300031017     o                       Ar4Aas              +1
014400031017     o                       Ar4Lnp              +1
014500031017     o                       Ar4Nrs        z     +1
014600031017     o                       Ar4Nsp        z     +1
014700031017     o                                           +2 'TRC:'
014800031017     o                       Ar4Trc              +1
014900020603     o                                           +1 'AR4NOT:'
015000031017     o                       Ar4Not              +1
