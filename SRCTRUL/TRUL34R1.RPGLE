000100040315     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040316
000300040316      *------------------------------------------------------------------------*
000400040316      *                                                                        *
000500040512      *   STACCA NUMERO PER NUOVA SPEDIZIONE SU TITAS00F                       *
000600040316      *                                                                        *
000700040316      *------------------------------------------------------------------------*
000800040512
000900040512     fTitas30c  if   e           k Disk
001000040512     fFnblp01l  if   e           k Disk    usropn
001100040316
001200040316      *------------------------------------------------------------------------*
001300040316      *   S C H I E R E
001400040316      *------------------------------------------------------------------------*
001500040512     d cmd             s             60    dim(02) ctdata perrcd(1)
001600040316
001700040316      *------------------------------------------------------------------------*
001800040316      *   V A R I A B I L I
001900040316      *------------------------------------------------------------------------*
002000040326     d comman          s             80
002100040326     d lenght          s             15  5 inz(80)
002200040512     d wlib            s             10
002300040512     d wlib1           s             10
002400040316
002500040316      *------------------------------------------------------------------------*
002600040316      *   D S   I N T E R N E / E S T E R N E
002700040316      *------------------------------------------------------------------------*
002800040512     d Kpjba         e ds
002900040512     d Trul33ds      e ds
003000040512     d Trul34ds      e ds
003100040316
003200040316      *------------------------------------------------------------------------*
003300040316      *   C O S A T A N T I
003400040316      *------------------------------------------------------------------------*
003500040316
003600040316      *------------------------------------------------------------------------*
003700040316      *  RIEPILOGO INDICATORI
003800040316      *------------------------------------------------------------------------*
003900040512      * 99 - ERRORE ovrdbf
004000040316
004100040316      *------------------------------------------------------------------------*
004200040316
004300040512      * Eseguo le ovrdbf sul file dei numeratori di gruppo
004400040512     c                   Clear                   comman
004500040512     c                   Movel(p)  cmd(1)        comman
004600040512     c                   Eval      %Subst(comman:13:8) = 'AZNUM01L'
004700040512     c                   Eval      %Subst(comman:30:10) = wlib
004800040512     c                   Eval      comman =
004900040512     c                             %trim(comman) + '/AZNUM01L)'
005000040512     c                   Call      'QCMDEXC'                            99
005100040512     c                   Parm                    comman
005200040512     c                   Parm                    lenght
005300040512     c                   If        *In99
005400040512     c                   Goto      Fine
005500040512     c                   EndIf
005600040512
005700040512     c                   Clear                   comman
005800040512     c                   Movel(p)  cmd(1)        comman
005900040512     c                   Eval      %Subst(comman:13:8) = 'AZNUM02L'
006000040512     c                   Eval      %Subst(comman:30:10) = wlib
006100040512     c                   Eval      comman =
006200040512     c                             %trim(comman) + '/AZNUM02L)'
006300040512     c                   Call      'QCMDEXC'                            99
006400040512     c                   Parm                    comman
006500040512     c                   Parm                    lenght
006600040512     c                   If        *In99
006700040512     c                   Eval      O34Err = 1
006800040512     c                   Eval      O34Msg = 'OVRDBF non riuscita'
006900040512     c                   Goto      Fine
007000040512     c                   EndIf
007100040512
007200040512      * Richiamo il trul33ds per staccare il nuovo numero della spedizione
007300040512     c                   Do        *Hival
007400040512     c                   Clear                   Trul33ds
007500040512     c                   Eval      i33cnu = 302
007600040512     c                   Eval      i33num = 1
007700040531     c                   Eval      i33po1 = I34Lnp
007800040512     c                   Movel(p)  Trul33ds      kpjbu
007900040512     c                   Call      'TRUL33R'
008000040512     c                   Parm                    kpjba
008100040512     c                   Eval      Trul33ds = kpjbu
008200040512     c                   If        o33err <> *Zeros
008300040512     c                   Eval      O34Err = 1
008400040512     c                   Eval      O34Msg = 'OVRDBF non riuscita'
008500040512     c                   Goto      Fine
008600040512     c                   EndIf
008700040512     c                   Eval      o34Nsp = o33Nrf
008800040512      * Devo controllare che la spedizione non esista già su TITAS
008900040512     c     kSped         Setll     Titas30c
009000040512     c                   If        %Equal(Titas30c)
009100040512     c                   Iter
009200040512     c                   EndIf
009300040512      * Devo controllare che la spedizione non esista già su FNBLP
009400040512     c     kSped         Setll     Fnblp01l
009500040512     c                   If        %Equal(Fnblp01l)
009600040512     c                   Iter
009700040512     c                   EndIf
009800040512     c                   Leave
009900040512     c                   EndDo
010000040316
010100040316     c     Fine          Tag
010200040512
010300040512      * cancello le OVRDBF
010400040512     c                   Clear                   comman
010500040512     c                   Movel(p)  cmd(2)        comman
010600040512     c                   Call      'QCMDEXC'                            99
010700040512     c                   Parm                    comman
010800040512     c                   Parm                    lenght
010900040316
011000040316     c                   Eval      *InLr = *On
011100040318
011200040316      *------------------------------------------------------------------------*
011300040316      * - ROUTINE INIZIALE
011400040316      *------------------------------------------------------------------------*
011500040316     c     *Inzsr        BegSr
011600040316
011700040316     c     *Entry        Plist
011800040512     c                   Parm                    kpjba
011900040512     c                   Parm                    Trul34ds
012000040326
012100040326      * Se sono in ambiente buono - GAITRAGRU
012200040512     c                   If        knsif = 'GAITRA201 '
012300041130     c                   Eval      wlib = 'UNITRAGRU '
012400040512     c                   Eval      wlib1 = 'FILTRA201 '
012500040512     c                   EndIf
012600040326      *  Se sono in ambiente di prova - GAITRAGRPS
012700040512     c                   If        knsif = 'GAITRAPSP '
012800041130     c                   Eval      wlib = 'UNITRAGRP '
012900040512     c                   Eval      wlib1 = 'FILTRAPRD '
013000040326     c                   EndIf
013100040512
013200040512     c     kSped         klist
013300040512     c                   kfld                    i34Aas
013400040512     c                   kfld                    i34Lnp
013500040512     c                   kfld                    i34Nrs
013600040512     c                   kfld                    o34Nsp
013700040512
013800040512      * Apro il Fnblp di filiale
013900040512     c                   If        Not %Open(Fnblp01l)
014000040512     c                   Clear                   comman
014100040512     c                   Movel(p)  cmd(1)        comman
014200040512     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
014300040512     c                   Eval      %Subst(comman:30:10) = wlib1
014400040512     c                   Eval      comman =
014500040512     c                             %trim(comman) + '/FNBLP01L)'
014600040512     c                   Call      'QCMDEXC'                            99
014700040512     c                   Parm                    comman
014800040512     c                   Parm                    lenght
014900040512     c                   If        *In99
015000040512     c                   Eval      O34Err = 1
015100040512     c                   Eval      O34Msg = 'OVRDBF non riuscita'
015200040512     c                   Goto      Fine
015300040512     c                   EndIf
015400040512     c  n99              Open      Fnblp01l
015500040512     c                   EndIf
015600040326
015700040512     c                   EndSr
015800040326      *------------------------------------------------------------------------*
015900040326** cmd
016000040512OVRDBF FILE(        ) TOFILE(
016100040512DLTOVR FILE(*ALL)
