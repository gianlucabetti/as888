000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031017      *---------------------------------------------------------------*
000600031017
000700180116     fFnblp33l  if   e           k Disk    prefix(b33:3)
000800180116     fFnblp01l  if   e           k Disk    rename(fnblp000:fnblp001)
000900180116     ffirab04l  uf   e           k Disk
001000031017     fQsysprt   o    f  132        Printer Oflind(*InOf)
001100180115
001200180115     d kpjba         E DS
001300180115     C     *ENTRY        PLIST
001400180115     C                   PARM                    KPJBA
001500031017
001600180115     c     kvab          klist
001700180115     c                   kfld                    kfgs              3 0
001800180115     c                   kfld                    rabccm
001900180115     c                   kfld                    rabrmnr
002000180115     c     krab          klist
002100180115     c                   kfld                    klnp              3 0
002200180115     c                   kfld                    kaas              4 0
002300180115     c                   kfld                    kccm              7 0
002400180116     c     kblp          klist
002500180116     c                   kfld                    rabaas
002600180116     c                   kfld                    rablnp
002700180116     c                   kfld                    rabnrs
002800180116     c                   kfld                    rabnsp
002900180116     c     kblp33        klist
003000180116     c                   kfld                    rabrmnr
003100180116     c                   kfld                    rabaas
003200180115     c
003300180115     c                   eval      klnp=063
003400180115     c                   eval      kfgs=788
003500180115     c                   eval      kaas=2018
003600180115     c                   eval      kccm=1663899
003700180115     c                   except    testa
003800180115       // Se richiesto file pulisco il lancio precedente per filiale
003900180115     c
004000180115     c     krab          Setll     firab04l
004100031021do  1c                   Do        *Hival
004200180115     c     krab          Reade     Firab04l
004300180115    2c                   If        %Eof(Firab04l)
004400031017     c                   Leave
004500180115    2c                   EndIf
004600180115     c
004700180116     c* Se la sped non la trovo devo sistemare
004800180116     c     kblp          chain     fnblp01l
004900180116     c
005000180116    2c                   if        not %found(fnblp01l)
005100180116     c* bolla ex accorpata
005200180116    3c                   if        rabrmn<>rabrmnr
005300180116     c     kblp33        chain     fnblp33l
005400180116
005500180116    4c                   if        %found(fnblp33l)
005600180116     c                   eval      rabaas=8888
005700180116     c                   update    firab000
005800180116     c                   except    trov
005900180116     c
006000180116     c* scrivo nuovo record come bolle "accorpante"
006100180116     c**                 eval      rabaas=8018
006200180116     c**                 eval      rabnrs=b33nrs
006300180116     c**                 eval      rabnsp=b33nsp
006400180116     c**                 eval      rabrmn=b33rmn
006500180116     c**                 eval      rabrmnr=b33rmn
006600180116     c**                 eval      rabrma=b33rma
006700180116     c**                 eval      rabrmar=b33rma
006800180116     c**                 write     firab000
006900180116     c
007000180116     c                   add       1             princip           7 0
007100180116     c
007200180116   x4c                   else
007300180116     c*
007400180116     c                   except    trovacc
007500180116     c                   eval      rabaas=9999
007600180116     c                   update    firab000
007700180116     c                   add       1             accorp            7 0
007800180116    4c                   endif
007900180115     c
008000180115     c
008100180116     c                   else
008200180116     c                   except    princc
008300180116     c                   add       1             princnook         7 0
008400180116     c* dovrebe già esistere qundi faccio una delete (ovvero rabaas=7777)
008500180116     c                   eval      rabaas=7777
008600180116     c                   update    firab000
008700180116    3c                   EndIf
008800180116    2c                   EndIf
008900180115     c
009000031017
009100180115    1c                   EndDo
009200180115     c                   except    tot
009300031017
009400031017     c                   Eval      *InLr = *On
009500031017
009600031017     oQsysprt   e            Testa          2 02
009700180115     o                                         +  5 'Bolle amazon 14/01/18'
009800031017     o                       *Date         y    110
009900031017     o                                          128 'Pag.'
010000031017     o                       Page          z    132
010100180115     o          e            TROV           1
010200031017     o                                           +1 'Spedizione:'
010300180116     o                                           +1 '8888'
010400180115     o                       RABLnp              +1
010500180115     o                       RABNrs        z     +1
010600180115     o                       RABNsp        z     +1
010700180115     o                       RABDAT        z     +1
010800180115     o                       RABrmn        z     +1
010900180116     o                       RABrmnr       z     +1
011000180116     o                                           +1 'diventata accorpante'
011100180116     o          e            TROVacc        1
011200180116     o                                           +1 'Spedizione:'
011300180116     o                                           +1 '9999'
011400180116     o                       RABLnp              +1
011500180116     o                       RABNrs        z     +1
011600180116     o                       RABNsp        z     +1
011700180116     o                       RABDAT        z     +1
011800180116     o                       RABrmn        z     +1
011900180116     o                       RABrmnr       z     +1
012000180116     o                                           +1 'nuovamente accorpata'
012100180116     o          e            princc         1
012200180116     o                                           +1 'Spedizione:'
012300180116     o                                           +1 '7777'
012400180116     o                       RABLnp              +1
012500180116     o                       RABNrs        z     +1
012600180116     o                       RABNsp        z     +1
012700180116     o                       RABDAT        z     +1
012800180116     o                       RABrmn        z     +1
012900180116     o                       RABrmnr       z     +1
013000180116     o                                           +1 'accorpante cambiata'
013100180115     o
013200180115     o          e            tot            1
013300180116     o                                           +1 'Tot accorpate'
013400180116     o                       accorp        z     +1
013500180116     o                                           +1 'tot diventate princ'
013600180116     o                       princip       z     +1
013700180116     o                                           +1 'accorpante cambiate'
013800180116     o                       princnook     z     +1
