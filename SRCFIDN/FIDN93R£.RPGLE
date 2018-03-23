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
008400051206
008500051206      * Escludo se è ancora in gestione al p.o.
008600051206      * (teoricamente sono tutte in gestione alla sede quando archiviamo ma non si sa mai....)
008700051206     c                   If        dctdt2 = *Zeros
008800051206     c                   Iter
008900051206     c                   EndIf
009000050411
009100050526      * Storicizzo e cancello le c.a.
009200050607     c                   ExSr      Storico
009300050411
009400050411     c                   EndDo
009500050411
009600050411     c                   Eval      *InLr = *On
009700050414
009800050411      *------------------------------------------------------------------------*
009900050607      * STORICIZZA E CANCELLA LA C.A.
010000050411      *------------------------------------------------------------------------*
010100050607     c     Storico       BegSr
010200050607
010300050607      * FNDCC
010400050607     c     kFndc         Setll     Fndcc01l
010500050607     c                   Do        *Hival
010600050607     c     kFndc         Reade     Fndcc01l
010700050607     c                   If        %Eof(Fndcc01l)
010800050607     c                   Leave
010900050607     c                   EndIf
011000050607      * scrivo lo storico
011100050607     c                   write     fndccs
011200050607      * cancello
011300050607     c                   delete    fndcc000
011400050607     c                   EndDo
011500050607
011600050607      * FNDCD
011700050607     c     kFndc         Setll     Fndcd01l
011800050607     c                   Do        *Hival
011900050607     c     kFndc         Reade     Fndcd01l
012000050607     c                   If        %Eof(Fndcd01l)
012100050607     c                   Leave
012200050607     c                   EndIf
012300050607      * scrivo lo storico
012400050607     c                   write     fndcds
012500050607      * cancello
012600050607     c                   delete    fndcd000
012700050607     c                   EndDo
012800050607
012900050607      * FNDCF
013000050607     c     kFndc         Setll     Fndcf01l
013100050607     c                   Do        *Hival
013200050607     c     kFndc         Reade     Fndcf01l
013300050607     c                   If        %Eof(Fndcf01l)
013400050607     c                   Leave
013500050607     c                   EndIf
013600050607      * scrivo lo storico
013700050607     c                   write     fndcfs
013800050607      * cancello
013900050607     c                   delete    fndcf000
014000050607     c                   EndDo
014100050607
014200050607      * FNDCK
014300050607     c     kFndc         Setll     Fndck01l
014400050607     c                   Do        *Hival
014500050607     c     kFndc         Reade     Fndck01l
014600050607     c                   If        %Eof(Fndck01l)
014700050607     c                   Leave
014800050607     c                   EndIf
014900050607      * scrivo lo storico
015000050607     c                   write     fndcks
015100050607      * cancello
015200050607     c                   delete    fndck000
015300050607     c                   EndDo
015400050607
015500050607      * FNDCL
015600050607     c     kFndc         Setll     Fndcl01l
015700050607     c                   Do        *Hival
015800050607     c     kFndc         Reade     Fndcl01l
015900050607     c                   If        %Eof(Fndcl01l)
016000050607     c                   Leave
016100050607     c                   EndIf
016200050607      * scrivo lo storico
016300050607     c                   write     fndcls
016400050607      * cancello
016500050607     c                   delete    fndcl000
016600050607     c                   EndDo
016700050607
016800050607      * FNDCR
016900050607     c     kFndc         Setll     Fndcr01l
017000050607     c                   Do        *Hival
017100050607     c     kFndc         Reade     Fndcr01l
017200050607     c                   If        %Eof(Fndcr01l)
017300050607     c                   Leave
017400050607     c                   EndIf
017500050607      * scrivo lo storico
017600050607     c                   write     fndcrs
017700050607      * cancello
017800050607     c                   delete    fndcr000
017900050607     c                   EndDo
018000050607
018100050607      * FNDCS
018200050607     c                   Eval      kdcstpd = 'C'
018300050607     c                   Movel(p)  dsnumca       kdcsnkt
018400050607     c     kFndcs        Setll     Fndcs01l
018500050607     c                   Do        *Hival
018600050607     c     kFndcs        Reade     Fndcs01l
018700050607     c                   If        %Eof(Fndcs01l)
018800050607     c                   Leave
018900050607     c                   EndIf
019000050607      * scrivo lo storico
019100050607     c                   write     fndcss
019200050607      * cancello
019300050607     c                   delete    fndcs000
019400050607     c                   EndDo
019500050607
019600050607      * FNDKA
019700050607     c     kFndc         Setll     Fndka01l
019800050607     c                   Do        *Hival
019900050607     c     kFndc         Reade     Fndka01l
020000050607     c                   If        %Eof(Fndka01l)
020100050607     c                   Leave
020200050607     c                   EndIf
020300050607      * scrivo lo storico
020400050607     c                   write     fndkas
020500050607      * cancello
020600050607     c                   delete    fndka000
020700050607     c                   EndDo
020800050607
020900050607      * FNDPT
021000050607     c     kFndc         Setll     Fndpt01l
021100050607     c                   Do        *Hival
021200050607     c     kFndc         Reade     Fndpt01l
021300050607     c                   If        %Eof(Fndpt01l)
021400050607     c                   Leave
021500050607     c                   EndIf
021600050607      * scrivo lo storico
021700050607     c                   write     fndpts
021800050607      * cancello
021900050607     c                   delete    fndpt000
022000050607     c                   EndDo
022100050607
022200050607      * mi salvo il n. della spedizione
022300050607     c                   Eval      savaas = dctaas
022400050607     c                   Eval      savlnp = dctlnp
022500050607     c                   Eval      savnrs = dctnrs
022600050607     c                   Eval      savnsp = dctnsp
022700050607
022800050607      * FNDCT
022900050607      * scrivo lo storico
023000050607     c                   write     fndcts
023100050607      * cancello
023200050607     c                   delete    fndct000
023300050607
023400050607      * controllo se esistono c.a. legate alla spedizione
023500050607     c     kFndct2       Setll     Fndct02l
023600050607     c                   If        %equal(Fndct02l)
023700050607     c                   Leavesr
023800050607     c                   EndIf
023900050607      * non ho trovato più c.a. legate alla spedizione
024000050613     c     kFndct2       Chain     Tntmd01l
024100050613     c                   If        %Found(Tntmd01l)
024200050607      * salvo e cancello TNTMD
024300050607     c                   write     tntmds
024400050607     c                   delete    tntmd000
024500050613     c                   EndIf
024600050408
024700050411     c                   EndSr
024800050414
024900050411      *------------------------------------------------------------------------*
025000050411      * ROUTINE INIZIALE
025100050411      *------------------------------------------------------------------------*
025200050411     c     *Inzsr        BegSr
025300050411
025400050411     c     *Entry        Plist
025500050411     c                   Parm                    kpjba
025600050411     c                   Movel     kpjbu         parm01
025700050411
025800050411     c     *dtaara       define    §azute        azuteds
025900050411     c     *dtaara       define    §datiute      ddatiute
026000050411
026100050411     c                   in(E)     *dtaara
026200050411     c                   IF        %ERROR or RSUT = *blanks
026300050411     c                   clear                   Tibs34Ds
026400050411     c                   call      'TIBS34R'
026500050411     c                   parm                    Tibs34Ds
026600050411     c                   in        *dtaara
026700050411     c                   ENDIF
026800050411
026900050414     c     kFndc         klist
027000050413     c                   kfld                    dctaac
027100050413     c                   kfld                    dctfil
027200050413     c                   kfld                    dctnca
027300050607
027400050607     c     kFndcs        klist
027500050607     c                   kfld                    kdcstpd
027600050607     c                   kfld                    kdcsnkt
027700050607
027800050607     c     kFndct2       klist
027900050607     c                   kfld                    savaas
028000050607     c                   kfld                    savlnp
028100050607     c                   kfld                    savnrs
028200050607     c                   kfld                    savnsp
028300050413
028400050411     c                   EndSr
