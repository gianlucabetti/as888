000100050412     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200050408
000300050408      *------------------------------------------------------------------------*
000400050408      *
000500050408      *  Archiviazione C.A. chiuse
000600050408      *
000700050408      *------------------------------------------------------------------------*
000800050408
000900050607     fFndet01l  uf   e           k disk
001000050607     fFndet02l  uf   e           k disk    rename(fndet000:fndet02)
001100050607     fFndet00s  o  a e           k disk    rename(fndet000:fndets)
001200050607     fFnded01l  uf   e           k disk
001300050607     fFnded00s  o  a e           k disk    rename(fnded000:fndeds)
001400050607     fFndcs01l  uf   e           k disk
001500050607     fFndcs00s  o  a e           k disk    rename(fndcs000:fndcss)
001600050613     fFndct03l  if   e           k disk
001700050613     fFndct05l  if   e           k disk    rename(fndct000:fndct05)
001800050408
001900050408      *------------------------------------------------------------------------*
002000050408      *  RIEPILOGO INDICATORI
002100050408      *------------------------------------------------------------------------*
002200050504
002300050504      *------------------------------------------------------------------------*
002400050504      *   S C H I E R E
002500050504      *------------------------------------------------------------------------*
002600050408
002700050408      *------------------------------------------------------------------------*
002800050408      *   V A R I A B I L I
002900050408      *------------------------------------------------------------------------*
003000050607     d kdcsnkt         s                   like(dcsnkt)
003100050607     d kdcstpd         s                   like(dcstpd)
003200050408
003300050408      *------------------------------------------------------------------------*
003400050408      *   D S   I N T E R N E / E S T E R N E
003500050408      *------------------------------------------------------------------------*
003600050408     d parm01          ds
003700050516     d  waaca                         4  0
003800050607
003900050607     d                 ds
004000050613     d  detaae                 1      4  0
004100050613     d  detnev                 5     11  0
004200050613     d  dsnumev                1     11  0
004300050408
004400050412     d azuteds       e ds                  extname(AZUTE00F)
004500050412     d ddatiute      e ds
004600050408     d kpjba         e ds
004700050412     d tibs34ds      e ds                  inz
004800050408
004900050408      *------------------------------------------------------------------------*
005000050414
005100050613      * 1° giro per anno evento (incluse quindi le denunce)
005200050613     c     waaca         Setll     Fndet01l
005300050411     c                   Do        *Hival
005400050613     c     waaca         Reade     Fndet01l
005500050415
005600050411      * Fine file
005700050613     c                   If        %Eof(Fndet01l)
005800050411     c                   Leave
005900050411     c                   EndIf
006000050613
006100050613      * Controllo se ci sono ancora c.a. legate all'evento
006200050613      * se ce ne sono non devo cancellare l'evento
006300050613     c     kFndev        Setll     Fndct03l
006400050613     c                   If        %Equal(Fndct03l)
006500050613     c                   Iter
006600050613     c                   EndIf
006700050613
006800050613      * se c'è la denuncia controllo se esistono ancora c.a. legate alla denuncia
006900050613      * se ce ne sono non devo cancellare la denuncia
007000050613     c                   If        detnde > 0
007100050613     c     kFnded        Setll     Fndct05l
007200050613     c                   If        %Equal(Fndct05l)
007300050613     c                   Iter
007400050613     c                   EndIf
007500050613     c                   EndIf
007600050411
007700050613      * Storicizzo e cancello gli eventi/denuncia
007800050613     c                   ExSr      Storico
007900050411
008000050411     c                   EndDo
008100050613
008200050613      * 2° giro per anno denuncia (solo le denunce non devo elaborare se c'è evento)
008300050613     c     waaca         Setll     Fndet02l
008400050613     c                   Do        *Hival
008500050613     c     waaca         Reade     Fndet02l
008600050613
008700050613      * Fine file
008800050613     c                   If        %Eof(Fndet02l)
008900050613     c                   Leave
009000050613     c                   EndIf
009100050613
009200050613      * Escludo se n. evento impostato
009300050613     c                   If        detnev > 0
009400050613     c                   Iter
009500050613     c                   EndIf
009600050613
009700050613      * controllo se esistono ancora c.a. legate alla denuncia
009800050613      * se ce ne sono non devo cancellare la denuncia
009900050613     c     kFnded        Setll     Fndct05l
010000050613     c                   If        %Equal(Fndct05l)
010100050613     c                   Iter
010200050613     c                   EndIf
010300050613
010400050613      * Storicizzo e cancello le denunce
010500050613
010600050613      * scrivo lo storico
010700050613     c                   write     fndets
010800050613      * cancello
010900050613     c                   delete    fndet02
011000050613
011100050613     c                   EndDo
011200050411
011300050411     c                   Eval      *InLr = *On
011400050414
011500050411      *------------------------------------------------------------------------*
011600050613      * STORICIZZA E CANCELLA GLI EVENTI/DENUNCIA
011700050411      *------------------------------------------------------------------------*
011800050613     c     Storico       BegSr
011900050607
012000050613      * FNDED
012100050613     c     kFndev        Setll     Fnded01l
012200050607     c                   Do        *Hival
012300050613     c     kFndev        Reade     Fnded01l
012400050613     c                   If        %Eof(Fnded01l)
012500050607     c                   Leave
012600050607     c                   EndIf
012700050607      * scrivo lo storico
012800050613     c                   write     fndeds
012900050607      * cancello
013000050613     c                   delete    fnded000
013100050607     c                   EndDo
013200050607
013300050607      * FNDCS
013400050613     c                   Eval      kdcstpd = 'D'
013500050613     c                   Movel(p)  dsnumev       kdcsnkt
013600050607     c     kFndcs        Setll     Fndcs01l
013700050607     c                   Do        *Hival
013800050607     c     kFndcs        Reade     Fndcs01l
013900050607     c                   If        %Eof(Fndcs01l)
014000050607     c                   Leave
014100050607     c                   EndIf
014200050607      * scrivo lo storico
014300050607     c                   write     fndcss
014400050607      * cancello
014500050607     c                   delete    fndcs000
014600050607     c                   EndDo
014700050607
014800050613      * FNDET
014900050607      * scrivo lo storico
015000050613     c                   write     fndets
015100050607      * cancello
015200050613     c                   delete    fndet000
015300050408
015400050411     c                   EndSr
015500050414
015600050411      *------------------------------------------------------------------------*
015700050411      * ROUTINE INIZIALE
015800050411      *------------------------------------------------------------------------*
015900050411     c     *Inzsr        BegSr
016000050411
016100050411     c     *Entry        Plist
016200050411     c                   Parm                    kpjba
016300050411     c                   Movel     kpjbu         parm01
016400050411
016500050411     c     *dtaara       define    §azute        azuteds
016600050411     c     *dtaara       define    §datiute      ddatiute
016700050411
016800050411     c                   in(E)     *dtaara
016900050411     c                   IF        %ERROR or RSUT = *blanks
017000050411     c                   clear                   Tibs34Ds
017100050411     c                   call      'TIBS34R'
017200050411     c                   parm                    Tibs34Ds
017300050411     c                   in        *dtaara
017400050411     c                   ENDIF
017500050411
017600050613     c     kFndev        klist
017700050613     c                   kfld                    detaae
017800050613     c                   kfld                    detnev
017900050613
018000050613     c     kFnded        klist
018100050613     c                   kfld                    detaad
018200050613     c                   kfld                    detpod
018300050613     c                   kfld                    detnde
018400050607
018500050607     c     kFndcs        klist
018600050607     c                   kfld                    kdcstpd
018700050607     c                   kfld                    kdcsnkt
018800050413
018900050411     c                   EndSr
