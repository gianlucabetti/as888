000100040315     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040316
000300040316      *------------------------------------------------------------------------*
000400040316      *                                                                        *
000500040512      *   STACCA NUMERO PER NUOVA SPEDIZIONE SU TITAS00F                       *
000600040316      *                                                                        *
000700040316      *------------------------------------------------------------------------*
000800040512
000900040601     fFlnuf01l  uf   e           k Disk    usropn
001000040512     fTitas30c  if   e           k Disk
001100040512     fFnblp01l  if   e           k Disk    usropn
001200040601     fTabel00f  if   e           k Disk    usropn
001300040316
001400040316      *------------------------------------------------------------------------*
001500040316      *   S C H I E R E
001600040316      *------------------------------------------------------------------------*
001700040512     d cmd             s             60    dim(02) ctdata perrcd(1)
001800040316
001900040316      *------------------------------------------------------------------------*
002000040316      *   V A R I A B I L I
002100040316      *------------------------------------------------------------------------*
002200040326     d comman          s             80
002300040601     d conta           s              2  0
002400040601     d kAaa            s                   like(NufAaa)
002500040601     d kCnu            s                   like(NufCnu)
002600040601     d kFil            s                   like(NufFil)
002700040601     d kFl2            s                   like(NufFl2)
002800040326     d lenght          s             15  5 inz(80)
002900040601     d pa4cnu          s              3s 0
003000040512     d wlib            s             10
003100040601     d wlib1           s             10
003200040316
003300040316      *------------------------------------------------------------------------*
003400040316      *   D S   I N T E R N E / E S T E R N E
003500040316      *------------------------------------------------------------------------*
003600040512     d Kpjba         e ds
003700040512     d Trul34ds      e ds
003800040316
003900040316      *------------------------------------------------------------------------*
004000040316      *   C O S A T A N T I
004100040316      *------------------------------------------------------------------------*
004200040316
004300040316      *------------------------------------------------------------------------*
004400040316      *  RIEPILOGO INDICATORI
004500040316      *------------------------------------------------------------------------*
004600040512      * 99 - ERRORE ovrdbf
004700040316
004800040316      *------------------------------------------------------------------------*
004900040316
005000040512
005100040601      * Stacco il numero da FLNUF numeratore 3 (bolle di filiale)
005200040601     c                   Move      I34Aas        kAaa
005300040601     c                   Eval      kCnu = 3
005400040601     c                   Eval      kFil = I34Lnp
005500040601     c                   Clear                   kFl2
005600040601     c                   Clear                   conta
005700040601     c                   Do        *Hival
005800040601     c                   add       1             conta
005900040601     c     kNuf          Chain(e)  Flnuf01l
006000040601      * Allocato esco
006100040601     c                   If        %Error
006200040601     c                   Eval      O34Err = 1
006300040601     c                   Eval      O34Msg = 'Numeratore allocato'
006400040601     c                   Goto      Fine
006500040601     c                   EndIf
006600040601      * Non trovo numeratore per la prima volta lo creo
006700040601     c                   If        Not %Found(Flnuf01l) and conta = 1
006800040601     c                   Z-add     kCnu          Pa4Cnu
006900040601     c                   Call      'FNLVCNR'
007000040601     c                   Parm                    Pa4Cnu
007100040601     c                   Iter
007200040601     c                   EndIf
007300040601      * Non trovo numeratore per la seconda volta esco
007400040601     c                   If        Not %Found(Flnuf01l) and conta > 1
007500040601     c                   Eval      O34Err = 1
007600040601     c                   Eval      O34Msg = 'Non trovato numeratore'
007700040601     c                   Goto      Fine
007800040601     c                   EndIf
007900040601      * Incremento numeratore
008000040601     c                   Add       1             NufNum
008100040601     c                   Update    Flnuf
008200040601     c                   Z-add     NufNum        O34Nsp
008300040512      * Devo controllare che la spedizione non esista già su TITAS
008400040512     c     kSped         Setll     Titas30c
008500040512     c                   If        %Equal(Titas30c)
008600040512     c                   Iter
008700040512     c                   EndIf
008800040512      * Devo controllare che la spedizione non esista già su FNBLP
008900040512     c     kSped         Setll     Fnblp01l
009000040512     c                   If        %Equal(Fnblp01l)
009100040512     c                   Iter
009200040512     c                   EndIf
009300040512     c                   Leave
009400040512     c                   EndDo
009500040316
009600040316     c     Fine          Tag
009700040512
009800040512      * cancello le OVRDBF
009900040512     c                   Clear                   comman
010000040512     c                   Movel(p)  cmd(2)        comman
010100040512     c                   Call      'QCMDEXC'                            99
010200040512     c                   Parm                    comman
010300040512     c                   Parm                    lenght
010400040316
010500040316     c                   Eval      *InLr = *On
010600040318
010700040316      *------------------------------------------------------------------------*
010800040316      * - ROUTINE INIZIALE
010900040316      *------------------------------------------------------------------------*
011000040316     c     *Inzsr        BegSr
011100040316
011200040316     c     *Entry        Plist
011300040512     c                   Parm                    kpjba
011400040512     c                   Parm                    Trul34ds
011500040326
011600040326      * Se sono in ambiente buono - GAITRAGRU
011700040512     c                   If        knsif = 'GAITRA201 '
011800040601     c                   Eval      wlib = 'FILTRA201 '
011900040601     c                   Eval      wlib1 = 'FILTRAGRU '
012000040512     c                   EndIf
012100040326      *  Se sono in ambiente di prova - GAITRAGRPS
012200040512     c                   If        knsif = 'GAITRAPSP '
012300040601     c                   Eval      wlib = 'FILTRAPRD '
012400040601     c                   Eval      wlib1 = 'FILTRAGRPF'
012500040326     c                   EndIf
012600040601
012700040601     c     kNuf          klist
012800040601     c                   kfld                    kAaa
012900040601     c                   kfld                    kCnu
013000040601     c                   kfld                    KFil
013100040601     c                   kfld                    kFl2
013200040512
013300040512     c     kSped         klist
013400040512     c                   kfld                    i34Aas
013500040512     c                   kfld                    i34Lnp
013600040512     c                   kfld                    i34Nrs
013700040512     c                   kfld                    o34Nsp
013800040601
013900040601      * Apro il Flnuf di filiale
014000040601     c                   If        Not %Open(Flnuf01l)
014100040601     c                   Clear                   comman
014200040601     c                   Movel(p)  cmd(1)        comman
014300040601     c                   Eval      %Subst(comman:13:8) = 'FLNUF01L'
014400040601     c                   Eval      %Subst(comman:30:10) = wlib
014500040601     c                   Eval      comman =
014600040601     c                             %trim(comman) + '/FLNUF01L)'
014700040601     c                   Call      'QCMDEXC'                            99
014800040601     c                   Parm                    comman
014900040601     c                   Parm                    lenght
015000040601     c                   If        *In99
015100040601     c                   Eval      O34Err = 1
015200040601     c                   Eval      O34Msg = 'OVRDBF non riuscita'
015300040601     c                   Goto      Fine
015400040601     c                   EndIf
015500040601     c  n99              Open      Flnuf01l
015600040601     c                   EndIf
015700040512
015800040512      * Apro il Fnblp di filiale
015900040512     c                   If        Not %Open(Fnblp01l)
016000040512     c                   Clear                   comman
016100040512     c                   Movel(p)  cmd(1)        comman
016200040512     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
016300040601     c                   Eval      %Subst(comman:30:10) = wlib
016400040512     c                   Eval      comman =
016500040512     c                             %trim(comman) + '/FNBLP01L)'
016600040512     c                   Call      'QCMDEXC'                            99
016700040512     c                   Parm                    comman
016800040512     c                   Parm                    lenght
016900040512     c                   If        *In99
017000040512     c                   Eval      O34Err = 1
017100040512     c                   Eval      O34Msg = 'OVRDBF non riuscita'
017200040512     c                   Goto      Fine
017300040512     c                   EndIf
017400040512     c  n99              Open      Fnblp01l
017500040512     c                   EndIf
017600040601
017700040601      * Apro il Tabel di filiale
017800040601     c                   If        Not %Open(Tabel00f)
017900040601     c                   Clear                   comman
018000040601     c                   Movel(p)  cmd(1)        comman
018100040601     c                   Eval      %Subst(comman:13:8) = 'TABEL00F'
018200040601     c                   Eval      %Subst(comman:30:10) = wlib1
018300040601     c                   Eval      comman =
018400040601     c                             %trim(comman) + '/TABEL00F)'
018500040601     c                   Call      'QCMDEXC'                            99
018600040601     c                   Parm                    comman
018700040601     c                   Parm                    lenght
018800040601     c                   If        *In99
018900040601     c                   Eval      O34Err = 1
019000040601     c                   Eval      O34Msg = 'OVRDBF non riuscita'
019100040601     c                   Goto      Fine
019200040601     c                   EndIf
019300040601     c  n99              Open      Tabel00f
019400040601     c                   EndIf
019500040326
019600040512     c                   EndSr
019700040326      *------------------------------------------------------------------------*
019800040326** cmd
019900040512OVRDBF FILE(        ) TOFILE(
020000040512DLTOVR FILE(*ALL)
