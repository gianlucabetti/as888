000100050412     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200050408
000300050408      *------------------------------------------------------------------------*
000400050408      *
000500050408      *  Archiviazione C.A. chiuse
000600050408      *
000700050408      *------------------------------------------------------------------------*
000800050408
000900050607     fFndct01l  uf   e           k disk
001000050607     fFndct02l  if   e           k disk    rename(fndct000:fndct02)
001100050607     fFndct00s  o  a e           k disk    rename(fndct000:fndcts)
001200050607     fFndcc01l  uf   e           k disk
001300050607     fFndcc00s  o  a e           k disk    rename(fndcc000:fndccs)
001400050607     fFndcd01l  uf   e           k disk
001500050607     fFndcd00s  o  a e           k disk    rename(fndcd000:fndcds)
001600050607     fFndcf01l  uf   e           k disk
001700050607     fFndcf00s  o  a e           k disk    rename(fndcf000:fndcfs)
001800050607     fFndck01l  uf   e           k disk
001900050607     fFndck00s  o  a e           k disk    rename(fndck000:fndcks)
002000050607     fFndcl01l  uf   e           k disk
002100050607     fFndcl00s  o  a e           k disk    rename(fndcl000:fndcls)
002200050607     fFndcr01l  uf   e           k disk
002300050607     fFndcr00s  o  a e           k disk    rename(fndcr000:fndcrs)
002400050607     fFndcs01l  uf   e           k disk
002500050607     fFndcs00s  o  a e           k disk    rename(fndcs000:fndcss)
002600050607     fFndka01L  uf   e           k disk
002700050607     fFndka00s  o  a e           k disk    rename(fndka000:fndkas)
002800050607     fFndpt01l  uf   e           k disk
002900050607     fFndpt00s  o  a e           k disk    rename(fndpt000:fndpts)
003000050607     fTntmd01l  uf   e           k disk
003100050607     fTntmd00s  o  a e           k disk    rename(tntmd000:tntmds)
003200050408
003300050408      *------------------------------------------------------------------------*
003400050408      *  RIEPILOGO INDICATORI
003500050408      *------------------------------------------------------------------------*
003600050504
003700050504      *------------------------------------------------------------------------*
003800050504      *   S C H I E R E
003900050504      *------------------------------------------------------------------------*
004000050408
004100050408      *------------------------------------------------------------------------*
004200050408      *   V A R I A B I L I
004300050408      *------------------------------------------------------------------------*
004400050607     d kdcsnkt         s                   like(dcsnkt)
004500050607     d kdcstpd         s                   like(dcstpd)
004600050607     d savaas          s                   like(dctaas)
004700050607     d savlnp          s                   like(dctlnp)
004800050607     d savnrs          s                   like(dctnrs)
004900050607     d savnsp          s                   like(dctnsp)
005000050408
005100050408      *------------------------------------------------------------------------*
005200050408      *   D S   I N T E R N E / E S T E R N E
005300050408      *------------------------------------------------------------------------*
005400050408     d parm01          ds
005500050516     d  waaca                         4  0
005600050607
005700050607     d                 ds
005800050607     d  dctaac                 1      4  0
005900050607     d  dctfil                 5      7  0
006000050607     d  dctnca                 8     14  0
006100050607     d  dsnumca                1     14  0
006200050408
006300050412     d azuteds       e ds                  extname(AZUTE00F)
006400050412     d ddatiute      e ds
006500050408     d kpjba         e ds
006600050412     d tibs34ds      e ds                  inz
006700050408
006800050408      *------------------------------------------------------------------------*
006900050414
007000050526     c     waaca         Setll     Fndct01l
007100050411     c                   Do        *Hival
007200050526     c     waaca         Reade     Fndct01l
007300050415
007400050411      * Fine file
007500050516     c                   If        %Eof(Fndct01l)
007600050411     c                   Leave
007700050411     c                   EndIf
007800050503
007900050526      * Escludo se non è chiusa la c.a.
008000050526      * (teoricamente sono tutte chiuse quando archiviamo ma non si sa mai....)
008100050526     c                   If        dctdch = *Zeros
008200050526     c                   Iter
008300050526     c                   EndIf
008400050411
008500050526      * Storicizzo e cancello le c.a.
008600050607     c                   ExSr      Storico
008700050411
008800050411     c                   EndDo
008900050411
009000050411     c                   Eval      *InLr = *On
009100050414
009200050411      *------------------------------------------------------------------------*
009300050607      * STORICIZZA E CANCELLA LA C.A.
009400050411      *------------------------------------------------------------------------*
009500050607     c     Storico       BegSr
009600050607
009700050607      * FNDCC
009800050607     c     kFndc         Setll     Fndcc01l
009900050607     c                   Do        *Hival
010000050607     c     kFndc         Reade     Fndcc01l
010100050607     c                   If        %Eof(Fndcc01l)
010200050607     c                   Leave
010300050607     c                   EndIf
010400050607      * scrivo lo storico
010500050607     c                   write     fndccs
010600050607      * cancello
010700050607     c                   delete    fndcc000
010800050607     c                   EndDo
010900050607
011000050607      * FNDCD
011100050607     c     kFndc         Setll     Fndcd01l
011200050607     c                   Do        *Hival
011300050607     c     kFndc         Reade     Fndcd01l
011400050607     c                   If        %Eof(Fndcd01l)
011500050607     c                   Leave
011600050607     c                   EndIf
011700050607      * scrivo lo storico
011800050607     c                   write     fndcds
011900050607      * cancello
012000050607     c                   delete    fndcd000
012100050607     c                   EndDo
012200050607
012300050607      * FNDCF
012400050607     c     kFndc         Setll     Fndcf01l
012500050607     c                   Do        *Hival
012600050607     c     kFndc         Reade     Fndcf01l
012700050607     c                   If        %Eof(Fndcf01l)
012800050607     c                   Leave
012900050607     c                   EndIf
013000050607      * scrivo lo storico
013100050607     c                   write     fndcfs
013200050607      * cancello
013300050607     c                   delete    fndcf000
013400050607     c                   EndDo
013500050607
013600050607      * FNDCK
013700050607     c     kFndc         Setll     Fndck01l
013800050607     c                   Do        *Hival
013900050607     c     kFndc         Reade     Fndck01l
014000050607     c                   If        %Eof(Fndck01l)
014100050607     c                   Leave
014200050607     c                   EndIf
014300050607      * scrivo lo storico
014400050607     c                   write     fndcks
014500050607      * cancello
014600050607     c                   delete    fndck000
014700050607     c                   EndDo
014800050607
014900050607      * FNDCL
015000050607     c     kFndc         Setll     Fndcl01l
015100050607     c                   Do        *Hival
015200050607     c     kFndc         Reade     Fndcl01l
015300050607     c                   If        %Eof(Fndcl01l)
015400050607     c                   Leave
015500050607     c                   EndIf
015600050607      * scrivo lo storico
015700050607     c                   write     fndcls
015800050607      * cancello
015900050607     c                   delete    fndcl000
016000050607     c                   EndDo
016100050607
016200050607      * FNDCR
016300050607     c     kFndc         Setll     Fndcr01l
016400050607     c                   Do        *Hival
016500050607     c     kFndc         Reade     Fndcr01l
016600050607     c                   If        %Eof(Fndcr01l)
016700050607     c                   Leave
016800050607     c                   EndIf
016900050607      * scrivo lo storico
017000050607     c                   write     fndcrs
017100050607      * cancello
017200050607     c                   delete    fndcr000
017300050607     c                   EndDo
017400050607
017500050607      * FNDCS
017600050607     c                   Eval      kdcstpd = 'C'
017700050607     c                   Movel(p)  dsnumca       kdcsnkt
017800050607     c     kFndcs        Setll     Fndcs01l
017900050607     c                   Do        *Hival
018000050607     c     kFndcs        Reade     Fndcs01l
018100050607     c                   If        %Eof(Fndcs01l)
018200050607     c                   Leave
018300050607     c                   EndIf
018400050607      * scrivo lo storico
018500050607     c                   write     fndcss
018600050607      * cancello
018700050607     c                   delete    fndcs000
018800050607     c                   EndDo
018900050607
019000050607      * FNDKA
019100050607     c     kFndc         Setll     Fndka01l
019200050607     c                   Do        *Hival
019300050607     c     kFndc         Reade     Fndka01l
019400050607     c                   If        %Eof(Fndka01l)
019500050607     c                   Leave
019600050607     c                   EndIf
019700050607      * scrivo lo storico
019800050607     c                   write     fndkas
019900050607      * cancello
020000050607     c                   delete    fndka000
020100050607     c                   EndDo
020200050607
020300050607      * FNDPT
020400050607     c     kFndc         Setll     Fndpt01l
020500050607     c                   Do        *Hival
020600050607     c     kFndc         Reade     Fndpt01l
020700050607     c                   If        %Eof(Fndpt01l)
020800050607     c                   Leave
020900050607     c                   EndIf
021000050607      * scrivo lo storico
021100050607     c                   write     fndpts
021200050607      * cancello
021300050607     c                   delete    fndpt000
021400050607     c                   EndDo
021500050607
021600050607      * mi salvo il n. della spedizione
021700050607     c                   Eval      savaas = dctaas
021800050607     c                   Eval      savlnp = dctlnp
021900050607     c                   Eval      savnrs = dctnrs
022000050607     c                   Eval      savnsp = dctnsp
022100050607
022200050607      * FNDCT
022300050607      * scrivo lo storico
022400050607     c                   write     fndcts
022500050607      * cancello
022600050607     c                   delete    fndct000
022700050607
022800050607      * controllo se esistono c.a. legate alla spedizione
022900050607     c     kFndct2       Setll     Fndct02l
023000050607     c                   If        %equal(Fndct02l)
023100050607     c                   Leavesr
023200050607     c                   EndIf
023300050607      * non ho trovato più c.a. legate alla spedizione
023400050607      * salvo e cancello TNTMD
023500050607     c                   write     tntmds
023600050607     c                   delete    tntmd000
023700050408
023800050411     c                   EndSr
023900050414
024000050411      *------------------------------------------------------------------------*
024100050411      * ROUTINE INIZIALE
024200050411      *------------------------------------------------------------------------*
024300050411     c     *Inzsr        BegSr
024400050411
024500050411     c     *Entry        Plist
024600050411     c                   Parm                    kpjba
024700050411     c                   Movel     kpjbu         parm01
024800050411
024900050411     c     *dtaara       define    §azute        azuteds
025000050411     c     *dtaara       define    §datiute      ddatiute
025100050411
025200050411     c                   in(E)     *dtaara
025300050411     c                   IF        %ERROR or RSUT = *blanks
025400050411     c                   clear                   Tibs34Ds
025500050411     c                   call      'TIBS34R'
025600050411     c                   parm                    Tibs34Ds
025700050411     c                   in        *dtaara
025800050411     c                   ENDIF
025900050411
026000050414     c     kFndc         klist
026100050413     c                   kfld                    dctaac
026200050413     c                   kfld                    dctfil
026300050413     c                   kfld                    dctnca
026400050607
026500050607     c     kFndcs        klist
026600050607     c                   kfld                    kdcstpd
026700050607     c                   kfld                    kdcsnkt
026800050607
026900050607     c     kFndct2       klist
027000050607     c                   kfld                    savaas
027100050607     c                   kfld                    savlnp
027200050607     c                   kfld                    savnrs
027300050607     c                   kfld                    savnsp
027400050413
027500050411     c                   EndSr
