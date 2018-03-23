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
004800170119     d dORF01        e ds
004900041012     d dDatiute      e ds
005000041013     d Fnlrf9ds      e ds
005100041012     d Kpjba         e ds
005200041020     d Tibs02ds      e ds
005300041012     d Tibs34ds      e ds
005400041012
005500041012      *------------------------------------------------------------------------*
005600041012
005700041013     c   01kQdt02        Setll     Fiqdt02l
005800041013     c  n01rf9dae        Setll     Fiqdt02l
005900041012     c                   Do        *Hival
006000041013     c   01kQdt02        Reade     Fiqdt02l
006100041013     c  n01rf9dae        Reade     Fiqdt02l
006200041013
006300041012      * Fine file
006400041012     c                   If        %eof(Fiqdt02l)
006500041012     c                   Leave
006600041012     c                   EndIf
006700041020
006800041020      * scarto le distinte con data > al D-Day
006900041020      * se non è una rielaborazione
007000041020     c                   If        QdtDdc > wdata and Not *In01
007100041020     c                   Iter
007200041020     c                   EndIf
007300041018
007400041018     c                   Clear                   wormaf
007500041018     c                   Clear                   wormfa
007600041018     c                   Clear                   wormnf
007700041013
007800041013      * se sto rielaborando devo prima cancellare tutti i dettagli ORM della distinta
007900041013     c   01              ExSr      Elimina
008000041012
008100041013      * elaboro le fasi degli ORM x aggiornare i file
008200041012     c                   ExSr      Elabora
008300041018
008400041018      * Aggiorno la distinta
008500041018     c                   If        wormaf <> *Zeros or wormfa <> *Zeros or
008600041018     c                             wormnf <> *Zeros
008700041018     c                   Eval      QdtOrmAf = wormaf
008800041018     c                   Eval      QdtOrmFa = wormfa
008900041018     c                   Eval      QdtOrmNf = wormnf
009000050207     c                   EndIf
009100041018     c                   Eval      QdtDaeOr = *Date
009200041018     c                   Time                    w0060
009300041018     c                   Movel     w0060         QdtOraOr
009400041018     c                   Update    Fiqdt000
009500050207     c**!!!              EndIf
009600041012
009700041012     c                   EndDo
009800041012
009900041012     c                   Eval      *InLr = *On
010000041013
010100041013      *------------------------------------------------------------------------*
010200041013      * - Elimina i dettagli ORM
010300041013      *------------------------------------------------------------------------*
010400041013     c     Elimina       BegSr
010500041013
010600041018     c     kQdo01        Setll     Fiqdo01l
010700041013     c                   Do        *Hival
010800041018     c     kQdo01        Reade     Fiqdo01l
010900041013      * Fine file
011000041013     c                   If        %eof(Fiqdo01l)
011100041013     c                   Leave
011200041013     c                   EndIf
011300041013
011400041013     c                   Delete    Fiqdo000
011500041013     c                   EndDo
011600041015
011700041015      * pulisco anche i dati memorizzati su FIQDT
011800041015     c                   Clear                   QdtDaeOr
011900041015     c                   Clear                   QdtOraOr
012000041015     c                   Clear                   QdtOrmAf
012100041015     c                   Clear                   QdtOrmFa
012200041015     c                   Clear                   QdtOrmNf
012300041013
012400041013     c                   EndSr
012500041012
012600041012      *------------------------------------------------------------------------*
012700041012      * - Elabora ORM
012800041012      *------------------------------------------------------------------------*
012900041012     c     Elabora       BegSr
013000041012
013100041018      * Fasi ORM fatte con la distinta interessata
013200041012     c     kOrf05        Setll     Fnorf05l
013300041018
013400041018      * non trovo fasi non elaboro
013500041018     c                   If        Not %equal(Fnorf05l)
013600041018     c                   Leavesr
013700041018     c                   EndIf
013800041018
013900041018     c                   Do        *Hival
014000041018     c     kOrf05        Reade     Fnorf05l
014100041015
014200041018      * fine file
014300041018     c                   If        %eof(Fnorf05l)
014400041018     c                   Leave
014500041013     c                   EndIf
014600041018
014700041018      * elaboro fasi a rottura ORM
014800041018     c                   If        savkeyorm <> keyorm
014900041018      * solo se la prima fase dell'ORM fatta con la distinta è la 400
015000041018      * elaboro le fasi
015100041018     c                   If        OrfFar = 400
015200041018     c                   ExSr      Fasi
015300041018     c                   EndIf
015400041018      * mi salvo il n. ORM
015500041018     c                   Eval      savpoe = OrfPoe
015600041018     c                   Eval      savnsr = OrfNsr
015700041018     c                   Eval      savnor = OrfNor
015800041018     c                   Eval      savnrv = OrfNrv
015900041018     c                   EndIf
016000041018
016100041018     c                   EndDo
016200041018
016300041018     c                   EndSr
016400041018
016500041018      *------------------------------------------------------------------------*
016600041018      * - Elabora le fasi dell'ORM
016700041018      *------------------------------------------------------------------------*
016800041018     c     Fasi          BegSr
016900041018
017000041018     c                   Clear                   wcar
017100041018     c                   Clear                   writnb
017200041018     c                   Reset                   w600
017300041018     c                   Reset                   w390
017400041013
017500041018      * Leggo le fasi dell'ORM fatte con la distinta interessata
017600041018     c     kOrf0501      Setll     Fnorf05l
017700041018     c                   Do        *Hival
017800041018     c     kOrf0501      Reade     Fnorf05l
017900041018      * fine file
018000041018     c                   If        %eof(Fnorf05l)
018100041018     c                   Leave
018200041018     c                   EndIf
018300170119
018400170119     c                   eval      dORF01 = ORFflo
018500041018
018600041018      * memorizzo che ho fatto la fase 600
018700041018     c                   If        OrfFar = 600
018800041018     c                   Eval      w600 = *On
018900041018     c                   EndIf
019000041018
019100041018     c                   EndDo
019200041018
019300041018      * controllo l'ultima fase letta dell'ORM ed in base a questo aggiorno i dati
019400041013     c                   Select
019500041018      * fase 200 dirottato --> conteggio l'ORM come assegnato e come non fatto
019600041018      *                        aggiorno il dettaglio con causale NQ'
019700041014     c                   when      OrfFar = 200
019800041018     c                   Add       1             wormnf
019900041018     c                   Eval      wcar = 'NQ'
020000041018     c                   ExSr      Dettaglio
020100041018      * fase 300 stampato --> conteggio l'ORM come assegnato e come non fatto
020200041018      *                       aggiorno il dettaglio con causale NQ'
020300041013     c                   When      OrfFar = 300
020400041018     c                   Add       1             wormnf
020500041018     c                   Eval      wcar = 'NQ'
020600041018     c                   ExSr      Dettaglio
020700041018      * fase 390 disassegnato --> non devo conteggiare l'ORM
020800170119     c*******            When      OrfFar = 390
020900170119     c*******            Eval      w390 = *On
021000170119
021100170119      * fase 390 disassegnato --> conteggio ORM come assegnato e come fatto/non fatto
021200170119      * in base al flag presente sulla fase
021300170119      * non avendo la causale lo metto come NQ
021400170119     c                   WHEN      ORFfar = 390
021500170119     c                   IF        §ORFfatto = 'S'
021600170119     c                   Add       1             wormfa
021700170119     c                   ENDIF
021800170119     c                   IF        §ORFfatto = 'N'
021900170119     c                   Add       1             wormnf
022000170119     c                   Eval      wcar = 'NQ'
022100170119     c                   ENDIF
022200170119     c                   ExSr      Dettaglio
022300170119
022400041018      * fase 400 assegnato --> conteggio l'ORM come assegnato e come non fatto
022500041018      *                        aggiorno il dettaglio con causale NQ'
022600041018     c                   When      OrfFar = 400
022700041018     c                   Add       1             wormnf
022800041018     c                   Eval      wcar = 'NQ'
022900041018     c                   ExSr      Dettaglio
023000041018      * fase 500 non fatto --> conteggio l'ORM come assegnato e come non fatto
023100041018      *                        aggiorno il dettaglio con la causale di mancato ritiro
023200041018     c                   When      OrfFar = 500
023300041015     c                   Add       1             wormnf
023400041015     c                   Eval      wcar = OrfCar
023500041018     c                   ExSr      Dettaglio
023600041018      * fase 600 fatto --> conteggio l'ORM come assegnato e come fatto
023700041018      *                    aggiorno il dettaglio con il flag di non bollettato
023800041013     c                   when      OrfFar = 600
023900041015     c                   Add       1             wormfa
024000041018     c                   Eval      writnb = 'S'
024100041018     c                   ExSr      Dettaglio
024200041018      * fase 900/910 bollettato -->
024300041018     c                   when      OrfFar = 900 or OrfFar = 910
024400041018      * se ho trovato la fase 600 conteggio l'ORM come fatto
024500041018     c                   If        w600 = *On
024600041018     c                   add       1             wormfa
024700041018      * se non ho trovato la fase 600 conteggio l'ORM come non fatto
024800041018      * e aggiorno il dettaglio come NQ'
024900041018     c                   Else
025000041018     c                   Add       1             wormnf
025100041018     c                   Eval      wcar = 'NQ'
025200041018     c                   ExSr      Dettaglio
025300041018     c                   EndIf
025400041018      * fase 999 non fatto --> conteggio l'ORM come assegnato e come non fatto
025500041018      *                        aggiorno il dettaglio con la causale di mancato ritiro
025600041013     c                   when      OrfFar = 999
025700041018     c                   Add       1             wormnf
025800041018     c                   Eval      wcar = OrfCar
025900041018     c                   ExSr      Dettaglio
026000041013
026100041013     c                   EndSl
026200041018
026300041018      * sommo 1 x ORM affidato
026400041018     c                   If        w390 = *Off
026500041018     c                   Add       1             wormaf
026600041018     c                   EndIf
026700041018
026800041018      * mi riposiziono
026900041018     c     kOrf0501      Setgt     Fnorf05l
027000041013
027100041012     c                   EndSr
027200041014
027300041014      *------------------------------------------------------------------------*
027400041018      * - Dettaglio FIDQO
027500041014      *------------------------------------------------------------------------*
027600041018     c     Dettaglio     BegSr
027700041014
027800041014     c                   Clear                   fiqdo000
027900041014     c                   Eval      QdoFgs = QdtFgs
028000041014     c                   Eval      QdoNfv = QdtNfv
028100041014     c                   Eval      QdoDdc = QdtDdc
028200041014     c                   Eval      QdoPdr = QdtPdr
028300041014     c                   Eval      QdoPoe = OrfPoe
028400041014     c                   Eval      QdoNsr = OrfNsr
028500041014     c                   Eval      QdoNor = OrfNor
028600041014     c                   Eval      QdoNrv = OrfNrv
028700041015     c                   Eval      QdoCar = wcar
028800041018     c                   Eval      QdoRitNb = writnb
028900041014     c                   Write     Fiqdo000
029000041014
029100041014     c                   EndSr
029200041012
029300041012      *------------------------------------------------------------------------*
029400041012      * - ROUTINE INIZIALE
029500041012      *------------------------------------------------------------------------*
029600041012     c     *Inzsr        BegSr
029700041012
029800041012     c     *Entry        Plist
029900041012     c                   Parm                    Kpjba
030000041013
030100041013     c                   If        kpjbu = *Blank
030200041013     c                   Clear                   Fnlrf9ds
030300041013     c                   Else
030400041013     c                   Eval      Fnlrf9ds = kpjbu
030500041013     c                   EndIf
030600041013
030700041013     c                   Eval      *In01 = (rf9mod = 'R')
030800041012
030900041012     c     *dtaara       define    §azute        azuteds
031000041012     c     *dtaara       define    §datiute      ddatiute
031100041012     c                   in(E)     *dtaara
031200041012     c                   If        %error  or RSUT = *blanks
031300041012     c                   Clear                   Tibs34ds
031400041012     c                   Call      'TIBS34R'
031500041012     c                   Parm                    Tibs34ds
031600041012     c                   In        *dtaara
031700041012     c                   EndIf
031800041020
031900041020      * recupero i gg lavorativi da sottrarre ad oggi
032000041020     c                   Clear                   Tibs02ds
032100041020     c                   Clear                   dCra
032200041020     c                   Eval      T02Mod = 'C'
032300041020     c                   Eval      T02Sif = knsif
032400041020     c                   Eval      T02Cod = 'CRA'
032500041020     c                   Eval      T02Ke1 = 'DFT'
032600041020     c                   Call      'TIBS02R'
032700041020     c                   Parm                    kpjba
032800041020     c                   Parm                    Tibs02ds
032900041020     c                   If        T02Err = *Blanks
033000041020     c                   Eval      dCra = T02Uni
033100041020     c                   EndIf
033200041020
033300041020      * Calcolo udate -gg. tabellati
033400041020     c     *iso          Movel     *date         wdataiso
033500041020     c     wdataiso      Subdur    1:*d          wdataiso2
033600041020      * calcolo il D-Day partendo da udate cercando i giorni lavorativi
033700041020      * all'indietro a seconda dei giorni impostati in tabella
033800041020     c                   Do        *Hival
033900041020     c                   Move      wdataiso2     dadata
034000041020     c                   Move      wdataiso      adata
034100041020     c                   Call      'XSRLAV8'
034200041020     c                   Parm                    wxsrlav8
034300041020      * se corrisponde ho trovato il D-Day
034400041020     c                   If        giolav = d§craor
034500041020     c                   Leave
034600041020     c                   Else
034700041020      * altrimenti continuo a sottrarre i giorni da udate
034800041020     c     wdataiso2     subdur    1:*d          wdataiso2
034900041020     c                   Iter
035000041020     c                   EndIf
035100041020     c                   EndDo
035200041020      * ho stabilito il D-DAY
035300041020     c                   Move      wdataiso2     wdata
035400041020     c
035500041012
035600041012     c     kQdt02        klist
035700041013     c                   kfld                    rf9dae
035800041013     c                   kfld                    rf9fgs
035900041013
036000041013     c     kQdo01        klist
036100041013     c                   kfld                    QdtFgs
036200041013     c                   kfld                    QdtNfv
036300041013     c                   kfld                    QdtDdc
036400041013     c                   kfld                    QdtPdr
036500041013
036600041012     c     kOrf05        klist
036700041012     c                   kfld                    QdtFgs
036800041012     c                   kfld                    QdtNfv
036900041012
037000041018     c     kOrf0501      klist
037100041018     c                   kfld                    QdtFgs
037200041018     c                   kfld                    QdtNfv
037300041018     c                   kfld                    OrfPoe
037400041018     c                   kfld                    OrfNsr
037500041018     c                   kfld                    OrfNor
037600041018     c                   kfld                    OrfNrv
037700041018
037800041020     c     kAzcln        klist
037900041020     c                   kfld                    ktfp
038000041020     c                   kfld                    ktfa
038100041020     c                   kfld                    kann
038200041020     c                   kfld                    kmes
038300041020
038400041012     c                   EndSr
