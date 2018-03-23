000100041014     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200041012
000300041012      *------------------------------------------------------------------------*
000400041012      *
000500041012      *  Aggiorna quadrature AUT FIQDT - FIDQO da distinte ORM
000600041012      *
000700041012      *------------------------------------------------------------------------*
000800041018     fFiqdt02l  uf   e           k disk
000900041018     fFiqdo01l  uf a e           k disk
001000041012     fFnorf05l  if   e           k disk
001100041020     fAzcln01l  if   e           k disk
001200041013
001300041020     d kann            s                   like(ClnAnn)
001400041020     d kmes            s                   like(ClnMes)
001500041020     d ktfa            s                   like(ClnTfa)
001600041020     d ktfp            s                   like(ClnTfp)
001700041018     d wcar            s                   like(QdoCar)
001800041020     d wdata           s                   Like(QdtDaeOr)
001900041020     d wdataiso        s               d   datfmt(*iso)
002000041020     d wdataiso2       s               d   datfmt(*iso)
002100041015     d wormaf          s                   like(QdtOrmAf)
002200041015     d wormfa          s                   like(QdtOrmfa)
002300041015     d wormnf          s                   like(QdtOrmNf)
002400041018     d writnb          s                   like(QdoRitNb)
002500041018     d w0060           s              6  0
002600041018     d w390            s              1    Inz('0')
002700041018     d w600            s              1    Inz('0')
002800041018
002900041018     d keyorm          ds
003000041018     d  OrfPoe
003100041018     d  OrfNsr
003200041018     d  OrfNor
003300041018     d  OrfNrv
003400041018
003500041018     d savkeyorm       ds
003600041018     d  savpoe                        3  0
003700041018     d  savnsr                        2  0
003800041018     d  savnor                        7  0
003900041018     d  savnrv                        2  0
004000041020
004100041020     d wxsrlav8        ds
004200041020     d  dadata                 1      8  0
004300041020     d  adata                  9     16  0
004400041020     d  gioLav                17     21  0
004500041012
004600041012     d Azuteds       e ds                  extname(Azute00f)
004700041020     d dCra          e ds
004800041012     d dDatiute      e ds
004900041013     d Fnlrf9ds      e ds
005000041012     d Kpjba         e ds
005100041020     d Tibs02ds      e ds
005200041012     d Tibs34ds      e ds
005300041012
005400041012      *------------------------------------------------------------------------*
005500041012
005600041013     c   01kQdt02        Setll     Fiqdt02l
005700041013     c  n01rf9dae        Setll     Fiqdt02l
005800041012     c                   Do        *Hival
005900041013     c   01kQdt02        Reade     Fiqdt02l
006000041013     c  n01rf9dae        Reade     Fiqdt02l
006100041013
006200041012      * Fine file
006300041012     c                   If        %eof(Fiqdt02l)
006400041012     c                   Leave
006500041012     c                   EndIf
006600041020
006700041020      * scarto le distinte con data > al D-Day
006800041020      * se non è una rielaborazione
006900041020     c                   If        QdtDdc > wdata and Not *In01
007000041020     c                   Iter
007100041020     c                   EndIf
007200041018
007300041018     c                   Clear                   wormaf
007400041018     c                   Clear                   wormfa
007500041018     c                   Clear                   wormnf
007600041013
007700041013      * se sto rielaborando devo prima cancellare tutti i dettagli ORM della distinta
007800041013     c   01              ExSr      Elimina
007900041012
008000041013      * elaboro le fasi degli ORM x aggiornare i file
008100041012     c                   ExSr      Elabora
008200041018
008300041018      * Aggiorno la distinta
008400041018     c                   If        wormaf <> *Zeros or wormfa <> *Zeros or
008500041018     c                             wormnf <> *Zeros
008600041018     c                   Eval      QdtOrmAf = wormaf
008700041018     c                   Eval      QdtOrmFa = wormfa
008800041018     c                   Eval      QdtOrmNf = wormnf
008900041018     c                   Eval      QdtDaeOr = *Date
009000041018     c                   Time                    w0060
009100041018     c                   Movel     w0060         QdtOraOr
009200041018     c                   Update    Fiqdt000
009300041018     c                   EndIf
009400041012
009500041012     c                   EndDo
009600041012
009700041012     c                   Eval      *InLr = *On
009800041013
009900041013      *------------------------------------------------------------------------*
010000041013      * - Elimina i dettagli ORM
010100041013      *------------------------------------------------------------------------*
010200041013     c     Elimina       BegSr
010300041013
010400041018     c     kQdo01        Setll     Fiqdo01l
010500041013     c                   Do        *Hival
010600041018     c     kQdo01        Reade     Fiqdo01l
010700041013      * Fine file
010800041013     c                   If        %eof(Fiqdo01l)
010900041013     c                   Leave
011000041013     c                   EndIf
011100041013
011200041013     c                   Delete    Fiqdo000
011300041013     c                   EndDo
011400041015
011500041015      * pulisco anche i dati memorizzati su FIQDT
011600041015     c                   Clear                   QdtDaeOr
011700041015     c                   Clear                   QdtOraOr
011800041015     c                   Clear                   QdtOrmAf
011900041015     c                   Clear                   QdtOrmFa
012000041015     c                   Clear                   QdtOrmNf
012100041013
012200041013     c                   EndSr
012300041012
012400041012      *------------------------------------------------------------------------*
012500041012      * - Elabora ORM
012600041012      *------------------------------------------------------------------------*
012700041012     c     Elabora       BegSr
012800041012
012900041018      * Fasi ORM fatte con la distinta interessata
013000041012     c     kOrf05        Setll     Fnorf05l
013100041018
013200041018      * non trovo fasi non elaboro
013300041018     c                   If        Not %equal(Fnorf05l)
013400041018     c                   Leavesr
013500041018     c                   EndIf
013600041018
013700041018     c                   Do        *Hival
013800041018     c     kOrf05        Reade     Fnorf05l
013900041015
014000041018      * fine file
014100041018     c                   If        %eof(Fnorf05l)
014200041018     c                   Leave
014300041013     c                   EndIf
014400041018
014500041018      * elaboro fasi a rottura ORM
014600041018     c                   If        savkeyorm <> keyorm
014700041018      * solo se la prima fase dell'ORM fatta con la distinta è la 400
014800041018      * elaboro le fasi
014900041018     c                   If        OrfFar = 400
015000041018     c                   ExSr      Fasi
015100041018     c                   EndIf
015200041018      * mi salvo il n. ORM
015300041018     c                   Eval      savpoe = OrfPoe
015400041018     c                   Eval      savnsr = OrfNsr
015500041018     c                   Eval      savnor = OrfNor
015600041018     c                   Eval      savnrv = OrfNrv
015700041018     c                   EndIf
015800041018
015900041018     c                   EndDo
016000041018
016100041018     c                   EndSr
016200041018
016300041018      *------------------------------------------------------------------------*
016400041018      * - Elabora le fasi dell'ORM
016500041018      *------------------------------------------------------------------------*
016600041018     c     Fasi          BegSr
016700041018
016800041018     c                   Clear                   wcar
016900041018     c                   Clear                   writnb
017000041018     c                   Reset                   w600
017100041018     c                   Reset                   w390
017200041013
017300041018      * Leggo le fasi dell'ORM fatte con la distinta interessata
017400041018     c     kOrf0501      Setll     Fnorf05l
017500041018     c                   Do        *Hival
017600041018     c     kOrf0501      Reade     Fnorf05l
017700041018      * fine file
017800041018     c                   If        %eof(Fnorf05l)
017900041018     c                   Leave
018000041018     c                   EndIf
018100041018
018200041018      * memorizzo che ho fatto la fase 600
018300041018     c                   If        OrfFar = 600
018400041018     c                   Eval      w600 = *On
018500041018     c                   EndIf
018600041018
018700041018     c                   EndDo
018800041018
018900041018      * controllo l'ultima fase letta dell'ORM ed in base a questo aggiorno i dati
019000041013     c                   Select
019100041018      * fase 200 dirottato --> conteggio l'ORM come assegnato e come non fatto
019200041018      *                        aggiorno il dettaglio con causale NQ'
019300041014     c                   when      OrfFar = 200
019400041018     c                   Add       1             wormnf
019500041018     c                   Eval      wcar = 'NQ'
019600041018     c                   ExSr      Dettaglio
019700041018      * fase 300 stampato --> conteggio l'ORM come assegnato e come non fatto
019800041018      *                       aggiorno il dettaglio con causale NQ'
019900041013     c                   When      OrfFar = 300
020000041018     c                   Add       1             wormnf
020100041018     c                   Eval      wcar = 'NQ'
020200041018     c                   ExSr      Dettaglio
020300041018      * fase 390 disassegnato --> non devo conteggiare l'ORM
020400041018     c                   When      OrfFar = 390
020500041018     c                   Eval      w390 = *On
020600041018      * fase 400 assegnato --> conteggio l'ORM come assegnato e come non fatto
020700041018      *                        aggiorno il dettaglio con causale NQ'
020800041018     c                   When      OrfFar = 400
020900041018     c                   Add       1             wormnf
021000041018     c                   Eval      wcar = 'NQ'
021100041018     c                   ExSr      Dettaglio
021200041018      * fase 500 non fatto --> conteggio l'ORM come assegnato e come non fatto
021300041018      *                        aggiorno il dettaglio con la causale di mancato ritiro
021400041018     c                   When      OrfFar = 500
021500041015     c                   Add       1             wormnf
021600041015     c                   Eval      wcar = OrfCar
021700041018     c                   ExSr      Dettaglio
021800041018      * fase 600 fatto --> conteggio l'ORM come assegnato e come fatto
021900041018      *                    aggiorno il dettaglio con il flag di non bollettato
022000041013     c                   when      OrfFar = 600
022100041015     c                   Add       1             wormfa
022200041018     c                   Eval      writnb = 'S'
022300041018     c                   ExSr      Dettaglio
022400041018      * fase 900/910 bollettato -->
022500041018     c                   when      OrfFar = 900 or OrfFar = 910
022600041018      * se ho trovato la fase 600 conteggio l'ORM come fatto
022700041018     c                   If        w600 = *On
022800041018     c                   add       1             wormfa
022900041018      * se non ho trovato la fase 600 conteggio l'ORM come non fatto
023000041018      * e aggiorno il dettaglio come NQ'
023100041018     c                   Else
023200041018     c                   Add       1             wormnf
023300041018     c                   Eval      wcar = 'NQ'
023400041018     c                   ExSr      Dettaglio
023500041018     c                   EndIf
023600041018      * fase 999 non fatto --> conteggio l'ORM come assegnato e come non fatto
023700041018      *                        aggiorno il dettaglio con la causale di mancato ritiro
023800041013     c                   when      OrfFar = 999
023900041018     c                   Add       1             wormnf
024000041018     c                   Eval      wcar = OrfCar
024100041018     c                   ExSr      Dettaglio
024200041013
024300041013     c                   EndSl
024400041018
024500041018      * sommo 1 x ORM affidato
024600041018     c                   If        w390 = *Off
024700041018     c                   Add       1             wormaf
024800041018     c                   EndIf
024900041018
025000041018      * mi riposiziono
025100041018     c     kOrf0501      Setgt     Fnorf05l
025200041013
025300041012     c                   EndSr
025400041014
025500041014      *------------------------------------------------------------------------*
025600041018      * - Dettaglio FIDQO
025700041014      *------------------------------------------------------------------------*
025800041018     c     Dettaglio     BegSr
025900041014
026000041014     c                   Clear                   fiqdo000
026100041014     c                   Eval      QdoFgs = QdtFgs
026200041014     c                   Eval      QdoNfv = QdtNfv
026300041014     c                   Eval      QdoDdc = QdtDdc
026400041014     c                   Eval      QdoPdr = QdtPdr
026500041014     c                   Eval      QdoPoe = OrfPoe
026600041014     c                   Eval      QdoNsr = OrfNsr
026700041014     c                   Eval      QdoNor = OrfNor
026800041014     c                   Eval      QdoNrv = OrfNrv
026900041015     c                   Eval      QdoCar = wcar
027000041018     c                   Eval      QdoRitNb = writnb
027100041014     c                   Write     Fiqdo000
027200041014
027300041014     c                   EndSr
027400041012
027500041012      *------------------------------------------------------------------------*
027600041012      * - ROUTINE INIZIALE
027700041012      *------------------------------------------------------------------------*
027800041012     c     *Inzsr        BegSr
027900041012
028000041012     c     *Entry        Plist
028100041012     c                   Parm                    Kpjba
028200041013
028300041013     c                   If        kpjbu = *Blank
028400041013     c                   Clear                   Fnlrf9ds
028500041013     c                   Else
028600041013     c                   Eval      Fnlrf9ds = kpjbu
028700041013     c                   EndIf
028800041013
028900041013     c                   Eval      *In01 = (rf9mod = 'R')
029000041012
029100041012     c     *dtaara       define    §azute        azuteds
029200041012     c     *dtaara       define    §datiute      ddatiute
029300041012     c                   in(E)     *dtaara
029400041012     c                   If        %error  or RSUT = *blanks
029500041012     c                   Clear                   Tibs34ds
029600041012     c                   Call      'TIBS34R'
029700041012     c                   Parm                    Tibs34ds
029800041012     c                   In        *dtaara
029900041012     c                   EndIf
030000041020
030100041020      * recupero i gg lavorativi da sottrarre ad oggi
030200041020     c                   Clear                   Tibs02ds
030300041020     c                   Clear                   dCra
030400041020     c                   Eval      T02Mod = 'C'
030500041020     c                   Eval      T02Sif = knsif
030600041020     c                   Eval      T02Cod = 'CRA'
030700041020     c                   Eval      T02Ke1 = 'DFT'
030800041020     c                   Call      'TIBS02R'
030900041020     c                   Parm                    kpjba
031000041020     c                   Parm                    Tibs02ds
031100041020     c                   If        T02Err = *Blanks
031200041020     c                   Eval      dCra = T02Uni
031300041020     c                   EndIf
031400041020
031500041020      * Calcolo udate -gg. tabellati
031600041020     c     *iso          Movel     *date         wdataiso
031700041020     c     wdataiso      Subdur    1:*d          wdataiso2
031800041020      * calcolo il D-Day partendo da udate cercando i giorni lavorativi
031900041020      * all'indietro a seconda dei giorni impostati in tabella
032000041020     c                   Do        *Hival
032100041020     c                   Move      wdataiso2     dadata
032200041020     c                   Move      wdataiso      adata
032300041020     c                   Call      'XSRLAV8'
032400041020     c                   Parm                    wxsrlav8
032500041020      * se corrisponde ho trovato il D-Day
032600041020     c                   If        giolav = d§craor
032700041020     c                   Leave
032800041020     c                   Else
032900041020      * altrimenti continuo a sottrarre i giorni da udate
033000041020     c     wdataiso2     subdur    1:*d          wdataiso2
033100041020     c                   Iter
033200041020     c                   EndIf
033300041020     c                   EndDo
033400041020      * ho stabilito il D-DAY
033500041020     c                   Move      wdataiso2     wdata
033600041020     c
033700041012
033800041012     c     kQdt02        klist
033900041013     c                   kfld                    rf9dae
034000041013     c                   kfld                    rf9fgs
034100041013
034200041013     c     kQdo01        klist
034300041013     c                   kfld                    QdtFgs
034400041013     c                   kfld                    QdtNfv
034500041013     c                   kfld                    QdtDdc
034600041013     c                   kfld                    QdtPdr
034700041013
034800041012     c     kOrf05        klist
034900041012     c                   kfld                    QdtFgs
035000041012     c                   kfld                    QdtNfv
035100041012
035200041018     c     kOrf0501      klist
035300041018     c                   kfld                    QdtFgs
035400041018     c                   kfld                    QdtNfv
035500041018     c                   kfld                    OrfPoe
035600041018     c                   kfld                    OrfNsr
035700041018     c                   kfld                    OrfNor
035800041018     c                   kfld                    OrfNrv
035900041018
036000041020     c     kAzcln        klist
036100041020     c                   kfld                    ktfp
036200041020     c                   kfld                    ktfa
036300041020     c                   kfld                    kann
036400041020     c                   kfld                    kmes
036500041020
036600041012     c                   EndSr
