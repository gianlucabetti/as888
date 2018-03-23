000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031017      *---------------------------------------------------------------*
000600031017
000700180115     fFnblp33l  if   e           k Disk
000800180115     fedivab3l  uf   e           k Disk
000900180115     fedivat1l  uf   e           k Disk
001000180115     ffirab04l  if   e           k Disk
001100031017     fQsysprt   o    f  132        Printer Oflind(*InOf)
001200180115
001300180115     d kpjba         E DS
001400180115     C     *ENTRY        PLIST
001500180115     C                   PARM                    KPJBA
001600031017
001700180115     c     kvab          klist
001800180115     c                   kfld                    kfgs              3 0
001900180115     c                   kfld                    rabccm
002000180115     c                   kfld                    rabrmnr
002100180115     c     krab          klist
002200180115     c                   kfld                    klnp              3 0
002300180115     c                   kfld                    kaas              4 0
002400180115     c                   kfld                    kccm              7 0
002500180115     c     kvat          klist
002600180115     c                   kfld                    vabfgs
002700180115     c                   kfld                    vabccm
002800180115     c                   kfld                    vabcmr
002900180115     c                   kfld                    vabcnt
003000180115     c                   kfld                    vabaas
003100180115     c                   kfld                    vablnp
003200180115     c                   kfld                    vabnrs
003300180115     c                   kfld                    vabnsp
003400180115     c
003500180115     c                   eval      klnp=063
003600180115     c                   eval      kfgs=788
003700180115     c                   eval      kaas=2018
003800180115     c                   eval      kccm=1663899
003900180115     c                   except    testa
004000180115       // Se richiesto file pulisco il lancio precedente per filiale
004100180115     c
004200180115     c     krab          Setll     firab04l
004300031021do  1c                   Do        *Hival
004400180115     c     krab          Reade     Firab04l
004500180115    2c                   If        %Eof(Firab04l)
004600031017     c                   Leave
004700180115    2c                   EndIf
004800180115     c
004900180115     c
005000180115    2c                   if        rabdat=20180114   and rabrmn<>raBRMNR
005100180115     c                             and  rabnrs=0
005200180115     c* se si tratta di bolla da partire e confermata oggi con un collo
005300180115     c     rabrmn        chain     fnblp33l
005400180115     c                   if        %found(fnblp33l)  and blpaas=2018
005500180116     c                             and blpnrs=0
005600180115     c                             and blpmgs=0115 and blpncl=1
005700180115     c                   add       1             bolle             9 0
005800180115     c     kvab          chain     edivab3l
005900180115    3c                   if        not %found(edivab3l)
006000180115     c                   except    errno
006100180115     c                   add       1             errate1           7 0
006200180115   x3c                   else
006300180115     C                   EXCEPT    TROV
006400180115     c     kvat          setll     edivat1l
006500180115     c     kvat          reade     edivat1l
006600180115     c                   dow       not %eof(edivat1l)
006700180115     c                   eval      vatfgs=063
006800180116     c                   update    edivat00
006900180115
007000180115     c     kvat          reade     edivat1l
007100180115     c                   enddo
007200180115
007300180115     c                   eval      vabfgs=063
007400180116     c                   update    edivab00
007500180115     c                   add       1             conta             7 0
007600180115     c
007700180115    3c                   EndIf
007800180115    3c                   EndIf
007900180115    2c                   EndIf
008000180115     c
008100031017
008200180115    1c                   EndDo
008300180115     c                   except    tot
008400031017
008500031017     c                   Eval      *InLr = *On
008600031017
008700031017     oQsysprt   e            Testa          2 02
008800180115     o                                         +  5 'Bolle amazon 14/01/18'
008900031017     o                       *Date         y    110
009000031017     o                                          128 'Pag.'
009100031017     o                       Page          z    132
009200180115     o          e            TROV           1
009300031017     o                                           +1 'Spedizione:'
009400180115     o                       RABAas              +1
009500180115     o                       RABLnp              +1
009600180115     o                       RABNrs        z     +1
009700180115     o                       RABNsp        z     +1
009800180115     o                       RABDAT        z     +1
009900180115     o                       RABrmn        z     +1
010000180115     o                                           +2 'segnac'
010100180115     o                       VABlna        z     +1
010200180115     o                       vabncd        z     +1
010300180115     o                                           +1 '/'
010400180115     o                       vabnca        z     +1
010500180115     o
010600180115     o          e            errno          1
010700180115     o                                           +1 'Spedizione:'
010800180115     o                       RABAas              +1
010900180115     o                       RABLnp              +1
011000180115     o                       RABNrs        z     +1
011100180115     o                       RABNsp        z     +1
011200180115     o                       RABrmn        z     +1
011300180115     o                       RABrma              +1
011400180115     o                       RABCCM              +1
011500180115     o                       RABRMNR             +1
011600180115     o                                           +2 'non trovata'
011700180115     o          e            tot            1
011800180115     o                                           +1 'Totale elaborate'
011900180115     o                       bolle         z     +1
012000180115     o                                           +1 'Totale cancellate e abilit'
012100180115     o                       conta         z     +1
012200180115     o                                           +1 'erorri lna'                   ù
012300180115     o                       errate1       z     +1
