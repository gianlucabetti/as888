000100050428     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050428      *------------------------------------------------------------------------*
000300050428      *   Crea workfile con clienti che hanno mandato in tariffa
000400050428      *------------------------------------------------------------------------*
000500040511
000600050427     fCnaco00f  if   e           k disk
000700050427     fCnclp00f  if   e           k disk
000800050427     fCnind00f  if   e           k disk
000900050510     fTntam04l  if   e           k disk
001000050427     fTitpt01l  if   e           k disk
001100050428     fWfcma00f  o    e             disk
001200040511
001300040511      *------------------------------------------------------------------------*
001400040511      *   S C H I E R E
001500040511      *------------------------------------------------------------------------*
001600040511
001700040511      *------------------------------------------------------------------------*
001800040511      *   V A R I A B I L I
001900040511      *------------------------------------------------------------------------*
002000050510     d §ctr            s                   like(tamctr)
002100050901     d §tip            s                   like(tamfie)
002200050510     d wass            s              1    inz('0')
002300050427     d w0040           s              4  0
002400040511
002500040511      *------------------------------------------------------------------------*
002600040511      *   D S   I N T E R N E / E S T E R N E
002700040511      *------------------------------------------------------------------------*
002800050510     d parm01          ds
002900050510     d  wdatda                        8  0
003000050510     d  wdatal                        8  0
003100050510     d  wcbloc                        1
003200050510     d  wcnew                         1
003300050901     d  wctip                         1
003400050921     d  wctbl                         1
003500050510
003600050427     d wlbdat          ds                  inz
003700050427     d  g02dat                 1      8  0
003800050427     d  g02inv                 9     16  0
003900050427     d  g02err                17     17
004000050427     d  g02tgi                18     22  0
004100050428
004200050428     d kpjba         e ds
004300050429     d dgei          e ds
004400050901     d dsta01        e ds
004500050429     d tibs02ds      e ds
004600050509     d tibs10ds      e ds
004700050427
004800040511      *------------------------------------------------------------------------*
004900040511      *  RIEPILOGO INDICATORI
005000040511      *------------------------------------------------------------------------*
005100040511
005200040511      *------------------------------------------------------------------------*
005300050428
005400050428     c     *Entry        Plist
005500050428     c                   Parm                    kpjba
005600050510
005700050510     c                   Eval      Parm01 = Kpjbu
005800050428
005900050427     c     kcli          klist
006000050427     c                   kfld                    acokut
006100050427     c                   kfld                    acokcc
006200050427     c                   kfld                    acoksc
006300050427
006400050427     c     ktam          klist
006500050427     c                   kfld                    tamksc
006600050427     c                   kfld                    tamctr
006700050427     c                   kfld                    tamprg
006800050429
006900050429     c                   Clear                   tibs02ds
007000050429
007100050429     c                   Eval      t02tla = 'L'
007200050429     c                   Eval      t02mod = 'C'
007300050429     c                   Eval      t02sif = knsif
007400050429     c                   Eval      t02cod = 'GEI'
007500050429     c                   Eval      t02ke1 = 'EUR'
007600050429     c                   Call      'TIBS02R'
007700050429     c                   Parm                    kpjba
007800050429     c                   Parm                    tibs02ds
007900050429
008000050429     c                   Eval      dgei = t02uni
008100040511
008200050427     c     *loval        Setll     cnaco00f
008300040511     c                   Do        *Hival
008400050427     c                   Read      cnaco00f
008500040511
008600040511      * fine file
008700050427     c                   If        %Eof(cnaco00f)
008800040511     c                   Leave
008900040511     c                   EndIf
009000050427
009100050427      * solo clienti
009200050427     c                   If        acokcc <> 151
009300050427     c                   Iter
009400050427     c                   EndIf
009500050510
009600050510      * no 8888 e 9999
009700050510     c                   Move      acoksc        w0040
009800050510     c                   If        w0040 = 8888 or w0040 = 9999
009900050510     c                   Iter
010000050510     c                   EndIf
010100050427
010200050510      * clienti bloccati
010300130315     c                   If        wcbloc = 'N' and acoabl <> *blanks
010400050307     c                   Iter
010500050307     c                   EndIf
010600050427
010700050510     c     kcli          Chain     cnclp00f
010800050510
010900050510      * clienti nuovi
011000050510     c                   If        wcnew = 'N' and
011100050510     c                             (not %Found(cnclp00f) or clpdps = *Zeros)
011200050510     c                   Iter
011300050510     c                   EndIf
011400050510
011500050510      * clienti con data ultima spedizioni nel range richiesto
011600050427     c                   If        %Found(cnclp00f) and clpdus > *Zeros
011700050427      * controllo la data lunga 8
011800050428     c                   Clear                   wlbdat
011900050427     c                   Z-add     clpdus        g02inv
012000050427     c                   Eval      g02err = '3'
012100050428     c                   Call      'XSRDA8'
012200050428     c                   Parm                    wlbdat
012300050510     c                   If        g02inv < wdatda
012400050427     c                   Iter
012500050427     c                   EndIf
012600050510     c                   If        g02inv > wdatal
012700050510     c                   Iter
012800050510     c                   EndIf
012900050427     c                   EndIf
013000050510
013100050510     c                   Eval      §ctr = 999
013200050427
013300050509      * cerco le tariffe del cliente
013400050510     c     acoksc        Setll     Tntam04l
013500050510     c                   If        Not %equal(tntam04l)
013600050427     c                   Iter
013700050427     c                   EndIf
013800050427     c                   Do        *Hival
013900050510     c     acoksc        Reade     Tntam04l
014000050427      * fine file
014100050510     c                   If        %Eof(tntam04l)
014200050427     c                   Leave
014300050427     c                   EndIf
014400050510
014500050510      * cambio tariffa
014600050510     c                   If        tamctr <> §ctr
014700050901      * verifico la tipologia della tariffa dpd fedex italia estero
014800050901     c                   movel     tamflo        dsta01
014900050901      * imposto il tipo servizio
015000050901     c                   select
015100050901
015200050901  1A c                   When      §tadpd = 'S'
015300050901     c                   move      'D'           §tip
015400050901
015500050901  1A c                   When      §tafed = 'S'
015600050901     c                   move      'F'           §tip
015700050901
015800050901     c                   other
015900050901     c                   movel     tamfie        §tip
016000050901     c                   endsl
016100050901      * se non richieste tutte verifico il tipo
016200050901     c                   if        wctip <> §tip and wctip <> 'T'
016300050901     c                   iter
016400050901     c                   endif
016500050921      * se richiesto escludo le tariffe bloccate
016600050921     c                   If        wctbl = 'N' and tambap = 'B'
016700050921     c                   Iter
016800050921     c                   EndIf
016900050427
017000050509      * cerco le tariffe particolari
017100050427     c     ktam          Setll     Titpt01l
017200050427     c                   Do        *Hival
017300050427     c     ktam          Reade     Titpt01l
017400050427      * fine file
017500050427     c                   If        %Eof(titpt01l)
017600050427     c                   Leave
017700050427     c                   EndIf
017800050509      * non annullate
017900050509     c                   If        tamatb <> *Blanks
018000050509     c                   Iter
018100050509     c                   EndIf
018200050427      * controllo se ha tariffa 'C' '8' 'R' '9'
018300050427     c                   If        tptftc <> 'C' and tptftc <> '8' and
018400050427     c                             tptftc <> 'R' and tptftc <> '9'
018500050427     c                   Iter
018600050427     c                   EndIf
018700050526      * se tariffa 'R' '9' e valore merce è <= 6,2 euro non considero
018800050429     c                   If        (tptftc = 'R' or tptftc = '9') and
018900060220     c***                           tptvlm <= §gelrp
019000060220     c                              tptvlm <= 6,2
019100050509     c                   Iter
019200050429     c                   EndIf
019300050509      * scrivo il file x ogni tariffa particolare 'C' '8' 'R' '9'
019400050509      * che trovo
019500050509     c                   ExSr      Scrivi
019600050510
019700050510     c                   EndDo
019800050510
019900050510     c                   Eval      §ctr = tamctr
020000050510      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
020100050510      * poi però devo elaborare anche la tariffa precedente che è in vigore ora
020200050510     c                   If        tamddt > *date
020300050510     c                   Eval      §ctr = 999
020400050510     c                   EndIf
020500050510     c                   EndIf
020600050427
020700050427     c                   EndDo
020800050427
020900050427     c                   EndDo
021000040511
021100040511     c                   Eval      *InLr = *On
021200050509
021300050509      *------------------------------------------------------------------------*
021400050509      * Scrivo il file
021500050509      *------------------------------------------------------------------------*
021600050509     c     Scrivi        BegSr
021700050509
021800050509     c                   Clear                   wfcma000
021900050509      * utente/data elaborazione
022000050509     c                   Eval      cmapru = knmus
022100050509     c                   Eval      cmadte = *date
022200050509      * cliente
022300050509     c                   Eval      cmaksc = acoksc
022400050509     c                   Eval      cmarag = acorag
022500050509     c     kcli          Chain     Cnind00f
022600050509     c                   If        Not %Found(Cnind00f)
022700050509     c                   Leavesr
022800050509     c                   EndIf
022900050509     c                   Eval      cmavia = indvia
023000050509     c                   Eval      cmaloc = indcit
023100050509     c                   Eval      cmacap = indcap
023200050509     c                   Eval      cmaprv = indprv
023300050509     c                   Eval      cmanaz = indsta
023400050509      * tariffa
023500050509     c                   Eval      cmactr = tamctr
023600050509     c                   Eval      cmaprg = tamprg
023700050509     c                   Eval      cmaddt = tamddt
023800050509     c                   Eval      cmadst = tamdst
023900050509     c                   Eval      cmaftc = tptftc
024000050509     c                   Eval      cmafvm = tptfvm
024100050509     c                   Eval      cmavlm = tptvlm
024200050509     c                   Eval      cmaapl = tptapl
024300050526     c                   Eval      cmatma = tpttma
024400050509      * controllo se il cliente è un papà
024500050509     c                   Clear                   Tibs10ds
024600050509     c                   Eval      d10tle = 'ST'
024700050509     c                   Eval      d10paf = 'F'
024800050509     c                   Eval      d10cod = tamksc
024900050509     c                   Eval      d10drf = *date
025000050509     c                   Call      'TIBS10R'
025100050509     c                   Parm                    Tibs10ds
025200050509     c                   If        d10err = *Blanks
025300050509     c                   Move      d10cop        cmakcu
025400050509     c                   Else
025500050509      * controllo se il codice inserito è un figlio
025600050509     c                   Clear                   Tibs10ds
025700050509     c                   Eval      d10tle = 'ST'
025800050509     c                   Eval      d10paf = 'P'
025900050509     c                   Eval      d10cod = tamksc
026000050509     c                   Eval      d10drf = *date
026100050509     c                   Call      'TIBS10R'
026200050509     c                   Parm                    Tibs10ds
026300050509     c                   If        d10err = *Blanks
026400050509     c                   Move      d10cop        cmakcu
026500050509     c                   Else
026600050509     c                   Eval      cmakcu = tamksc
026700050509     c                   EndIf
026800050509     c                   EndIf
026900050509      * scrivo file
027000050509     c                   Write     wfcma000
027100050509
027200050509     c                   EndSr
