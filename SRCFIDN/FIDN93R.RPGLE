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
003000070105     ffndra01l  uf   e           k disk
003100070105     ffndra00s  o  a e           k disk    rename(fndra000:fndras)
003200050607     fTntmd01l  uf   e           k disk
003300050607     fTntmd00s  o  a e           k disk    rename(tntmd000:tntmds)
003400050408
003500050408      *------------------------------------------------------------------------*
003600050408      *  RIEPILOGO INDICATORI
003700050408      *------------------------------------------------------------------------*
003800050504
003900050504      *------------------------------------------------------------------------*
004000050504      *   S C H I E R E
004100050504      *------------------------------------------------------------------------*
004200050408
004300050408      *------------------------------------------------------------------------*
004400050408      *   V A R I A B I L I
004500050408      *------------------------------------------------------------------------*
004600050607     d kdcsnkt         s                   like(dcsnkt)
004700050607     d kdcstpd         s                   like(dcstpd)
004800050607     d savaas          s                   like(dctaas)
004900050607     d savlnp          s                   like(dctlnp)
005000050607     d savnrs          s                   like(dctnrs)
005100050607     d savnsp          s                   like(dctnsp)
005200050408
005300050408      *------------------------------------------------------------------------*
005400050408      *   D S   I N T E R N E / E S T E R N E
005500050408      *------------------------------------------------------------------------*
005600050408     d parm01          ds
005700050516     d  waaca                         4  0
005800050607
005900050607     d                 ds
006000050607     d  dctaac                 1      4  0
006100050607     d  dctfil                 5      7  0
006200050607     d  dctnca                 8     14  0
006300050607     d  dsnumca                1     14  0
006400050408
006500050412     d azuteds       e ds                  extname(AZUTE00F)
006600050412     d ddatiute      e ds
006700050408     d kpjba         e ds
006800050412     d tibs34ds      e ds                  inz
006900050408
007000050408      *------------------------------------------------------------------------*
007100050414
007200050526     c     waaca         Setll     Fndct01l
007300050411     c                   Do        *Hival
007400050526     c     waaca         Reade     Fndct01l
007500050415
007600050411      * Fine file
007700050516     c                   If        %Eof(Fndct01l)
007800050411     c                   Leave
007900050411     c                   EndIf
008000050503
008100050526      * Escludo se non è chiusa la c.a.
008200050526      * (teoricamente sono tutte chiuse quando archiviamo ma non si sa mai....)
008300050526     c                   If        dctdch = *Zeros
008400050526     c                   Iter
008500050526     c                   EndIf
008600051206
008700051206      * Escludo se è ancora in gestione al p.o.
008800051206      * (teoricamente sono tutte in gestione alla sede quando archiviamo ma non si sa mai....)
008900051206     c                   If        dctdt2 = *Zeros
009000051206     c                   Iter
009100051206     c                   EndIf
009200050411
009300050526      * Storicizzo e cancello le c.a.
009400050607     c                   ExSr      Storico
009500050411
009600050411     c                   EndDo
009700050411
009800050411     c                   Eval      *InLr = *On
009900050414
010000050411      *------------------------------------------------------------------------*
010100050607      * STORICIZZA E CANCELLA LA C.A.
010200050411      *------------------------------------------------------------------------*
010300050607     c     Storico       BegSr
010400050607
010500050607      * FNDCC
010600050607     c     kFndc         Setll     Fndcc01l
010700050607     c                   Do        *Hival
010800050607     c     kFndc         Reade     Fndcc01l
010900050607     c                   If        %Eof(Fndcc01l)
011000050607     c                   Leave
011100050607     c                   EndIf
011200050607      * scrivo lo storico
011300050607     c                   write     fndccs
011400050607      * cancello
011500050607     c                   delete    fndcc000
011600050607     c                   EndDo
011700050607
011800050607      * FNDCD
011900050607     c     kFndc         Setll     Fndcd01l
012000050607     c                   Do        *Hival
012100050607     c     kFndc         Reade     Fndcd01l
012200050607     c                   If        %Eof(Fndcd01l)
012300050607     c                   Leave
012400050607     c                   EndIf
012500050607      * scrivo lo storico
012600050607     c                   write     fndcds
012700050607      * cancello
012800050607     c                   delete    fndcd000
012900050607     c                   EndDo
013000050607
013100050607      * FNDCF
013200050607     c     kFndc         Setll     Fndcf01l
013300050607     c                   Do        *Hival
013400050607     c     kFndc         Reade     Fndcf01l
013500050607     c                   If        %Eof(Fndcf01l)
013600050607     c                   Leave
013700050607     c                   EndIf
013800050607      * scrivo lo storico
013900050607     c                   write     fndcfs
014000050607      * cancello
014100050607     c                   delete    fndcf000
014200050607     c                   EndDo
014300050607
014400050607      * FNDCK
014500050607     c     kFndc         Setll     Fndck01l
014600050607     c                   Do        *Hival
014700050607     c     kFndc         Reade     Fndck01l
014800050607     c                   If        %Eof(Fndck01l)
014900050607     c                   Leave
015000050607     c                   EndIf
015100050607      * scrivo lo storico
015200050607     c                   write     fndcks
015300050607      * cancello
015400050607     c                   delete    fndck000
015500050607     c                   EndDo
015600050607
015700050607      * FNDCL
015800050607     c     kFndc         Setll     Fndcl01l
015900050607     c                   Do        *Hival
016000050607     c     kFndc         Reade     Fndcl01l
016100050607     c                   If        %Eof(Fndcl01l)
016200050607     c                   Leave
016300050607     c                   EndIf
016400050607      * scrivo lo storico
016500050607     c                   write     fndcls
016600050607      * cancello
016700050607     c                   delete    fndcl000
016800050607     c                   EndDo
016900050607
017000050607      * FNDCR
017100050607     c     kFndc         Setll     Fndcr01l
017200050607     c                   Do        *Hival
017300050607     c     kFndc         Reade     Fndcr01l
017400050607     c                   If        %Eof(Fndcr01l)
017500050607     c                   Leave
017600050607     c                   EndIf
017700050607      * scrivo lo storico
017800050607     c                   write     fndcrs
017900050607      * cancello
018000050607     c                   delete    fndcr000
018100050607     c                   EndDo
018200050607
018300050607      * FNDCS
018400050607     c                   Eval      kdcstpd = 'C'
018500050607     c                   Movel(p)  dsnumca       kdcsnkt
018600050607     c     kFndcs        Setll     Fndcs01l
018700050607     c                   Do        *Hival
018800050607     c     kFndcs        Reade     Fndcs01l
018900050607     c                   If        %Eof(Fndcs01l)
019000050607     c                   Leave
019100050607     c                   EndIf
019200050607      * scrivo lo storico
019300050607     c                   write     fndcss
019400050607      * cancello
019500050607     c                   delete    fndcs000
019600050607     c                   EndDo
019700050607
019800050607      * FNDKA
019900050607     c     kFndc         Setll     Fndka01l
020000050607     c                   Do        *Hival
020100050607     c     kFndc         Reade     Fndka01l
020200050607     c                   If        %Eof(Fndka01l)
020300050607     c                   Leave
020400050607     c                   EndIf
020500050607      * scrivo lo storico
020600050607     c                   write     fndkas
020700050607      * cancello
020800050607     c                   delete    fndka000
020900050607     c                   EndDo
021000050607
021100050607      * FNDPT
021200050607     c     kFndc         Setll     Fndpt01l
021300050607     c                   Do        *Hival
021400050607     c     kFndc         Reade     Fndpt01l
021500050607     c                   If        %Eof(Fndpt01l)
021600050607     c                   Leave
021700050607     c                   EndIf
021800050607      * scrivo lo storico
021900050607     c                   write     fndpts
022000050607      * cancello
022100050607     c                   delete    fndpt000
022200050607     c                   EndDo
022300070105
022400070105      * FNDRA
022500070105     c     kfndc         setll     fndra01l
022600070105     c                   do        *hival
022700070105     c     kfndc         reade     fndra01l
022800070105     c                   if        %eof(fndra01l)
022900070105     c                   leave
023000070105     c                   endif
023100070105      * scrivo lo storico
023200070105     c                   write     fndras
023300070105      * cancello
023400070105     c                   delete    fndra000
023500070105     c                   enddo
023600050607
023700050607      * mi salvo il n. della spedizione
023800050607     c                   Eval      savaas = dctaas
023900050607     c                   Eval      savlnp = dctlnp
024000050607     c                   Eval      savnrs = dctnrs
024100050607     c                   Eval      savnsp = dctnsp
024200050607
024300050607      * FNDCT
024400050607      * scrivo lo storico
024500050607     c                   write     fndcts
024600050607      * cancello
024700050607     c                   delete    fndct000
024800050607
024900050607      * controllo se esistono c.a. legate alla spedizione
025000050607     c     kFndct2       Setll     Fndct02l
025100050607     c                   If        %equal(Fndct02l)
025200050607     c                   Leavesr
025300050607     c                   EndIf
025400050607      * non ho trovato più c.a. legate alla spedizione
025500050613     c     kFndct2       Chain     Tntmd01l
025600050613     c                   If        %Found(Tntmd01l)
025700050607      * salvo e cancello TNTMD
025800050607     c                   write     tntmds
025900050607     c                   delete    tntmd000
026000050613     c                   EndIf
026100050408
026200050411     c                   EndSr
026300050414
026400050411      *------------------------------------------------------------------------*
026500050411      * ROUTINE INIZIALE
026600050411      *------------------------------------------------------------------------*
026700050411     c     *Inzsr        BegSr
026800050411
026900050411     c     *Entry        Plist
027000050411     c                   Parm                    kpjba
027100050411     c                   Movel     kpjbu         parm01
027200050411
027300050411     c     *dtaara       define    §azute        azuteds
027400050411     c     *dtaara       define    §datiute      ddatiute
027500050411
027600050411     c                   in(E)     *dtaara
027700050411     c                   IF        %ERROR or RSUT = *blanks
027800050411     c                   clear                   Tibs34Ds
027900050411     c                   call      'TIBS34R'
028000050411     c                   parm                    Tibs34Ds
028100050411     c                   in        *dtaara
028200050411     c                   ENDIF
028300050411
028400050414     c     kFndc         klist
028500050413     c                   kfld                    dctaac
028600050413     c                   kfld                    dctfil
028700050413     c                   kfld                    dctnca
028800050607
028900050607     c     kFndcs        klist
029000050607     c                   kfld                    kdcstpd
029100050607     c                   kfld                    kdcsnkt
029200050607
029300050607     c     kFndct2       klist
029400050607     c                   kfld                    savaas
029500050607     c                   kfld                    savlnp
029600050607     c                   kfld                    savnrs
029700050607     c                   kfld                    savnsp
029800050413
029900050411     c                   EndSr
