000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300040511      *------------------------------------------------------------------------*
000400040511      * Creazione bolle firmate scanner SEDE
000500040511      *------------------------------------------------------------------------*
000600040511
000700040511      * A T T E N Z I O N E:
000800040511      *---------------------
000900040511      * VALORI DI RIGCAR
001000040511      * D = DANNI
001100040511      * C = C/A OMESSI
001200040511      * F = BOLLE FIRMATE
001300040511      *  1 = BOLLE FIRMATE CON ADDEBITO AL MITTENTE
001400040511      *  2 = BOLLE FIRMATE CON ADDEBITO AL DESTINATARIO
001500040511      * S = BOLLE FIRMATE DA SCANNER
001600040511      *  3 = BOLLE FIRMATE CON ADDEBITO AL MITTENTE - VARIA 'O' DA TARIFFA
001700040511      *  4 = BOLLE FIRMATE CON ADDEBITO AL DESTINATARIO - VARIA 'O' DA TARIFFA
001800040511      *  5 = BOLLE FIRMATE CON ADDEBITO AL MITTENTE - VARIA 'O' NEGATA
001900040511      *  6 = BOLLE FIRMATE CON ADDEBITO AL DESTINATARIO - VARIA 'O' NEGATA
002000040917      * I = POD IMAGE
002100060419      *     rigpru = TILAC bolla con varia da fatturare
002200060419      *     rigpru = TILAC1 bolla con varia negata
002300040511      *------------------------------------------------------------------------*
002400060512     c* Per impostare il campo della DTAS per TNSF20R, il flag S/N DDT
002500060512     c*  deve essere messo in TASDDT. manca anche il campo FBR in
002600060512     c*  §tasflo (sempre della DTAS e non del campo TASFLO di tntas)
002700060512     c* per cui se tornasse utile richiamare tnsf20r, impostare bene i dati
002800060512     c*  della DS
002900060512      *------------------------------------------------------------------------*
003000040511
003100040706     fFirig01l  uf   e           k Disk
003200040511     fTitas30c  if   e           k Disk    prefix(f_)
003300040513     fTitaa30c  if   e           k Disk    prefix(f_)
003400040511     fTabel00f  if   e           k Disk
003500040511     fAzorg01l  if   e           k Disk
003600040511     fTitas00f  o    e           k Disk    rename(Titas000:Titas0)
003700040513     fTitaa00f  o    e           k Disk    rename(Titaa000:Titaa0)
003800040511     fTita400f  o    e           k Disk
003900040511
004000040511      *------------------------------------------------------------------------*
004100040511      *   S C H I E R E
004200040511      *------------------------------------------------------------------------*
004300040511
004400040511      *------------------------------------------------------------------------*
004500040511      *   V A R I A B I L I
004600040511      *------------------------------------------------------------------------*
004700040916     d kazorg          s                   like(OrgFil) inz(102)
004800040921     d wdtrich         s               d   datfmt(*iso)
004900040917     d welab           s              1    inz
005000050131     d whim            s                   like(RigHim)
005100050131     d whmrich         s               t
005200041119     d wnotas          s              1    inz
005300040511     d wokbolla        s              1    inz(*Off)
005400040511     d wora            s              6  0
005500040511     d wrigdue         s                   like(RigDue)  inz
005600040513     d wtrc            s                   like(TaaTrc)
005700040917     d w0060           s              6  0
005800040916     d w0070           s              7  0
005900040916     d w008a           s              8
006000040916     d w0110           s             11  0
006100040511
006200040511      *------------------------------------------------------------------------*
006300040511      *   D S   I N T E R N E / E S T E R N E
006400040511      *------------------------------------------------------------------------*
006500040511     d wnumbolla       ds
006600040511     d  wrigaas                1      2
006700040511     d  wriglnp                3      5
006800040511     d  wrignrs                6      7
006900040511     d  wrignsp                8     14
007000050131
007100050131     d oldbolla        ds
007200050131     d  oldaas                 1      4  0
007300050131     d  oldlnp                 5      7  0
007400050131     d  oldnrs                 8      9  0
007500050131     d  oldnsp                10     16  0
007600050131
007700050131     d newbolla        ds
007800050131     d  rigaas                 1      4  0
007900050131     d  riglnp                 5      7  0
008000050131     d  rignrs                 8      9  0
008100050131     d  rignsp                10     16  0
008200050131
008300050131     d oldrich         ds
008400050131     d  olddim                 1      8  0
008500050131     d  oldhim                 9     12  0
008600050131
008700050131     d newrich         ds
008800050131     d  rigdim                 1      8  0
008900050131     d  righim                 9     12  0
009000040917
009100040917     d                 ds
009200040917     d aoggi                   1      4  0
009300040917     d moggi                   5      6  0
009400040917     d doggi                   7      8  0
009500040917     d woggi                   1      8  0
009600040511
009700040511     d Azuteds       e ds                  extname(Azute00f)
009800040511     d dDatiute      e ds
009900040921     d dLva1         e ds
010000040511     d ds_cnaco      e ds                  extname(Cnaco00f)
010100040511     d ds_cnind      e ds                  extname(Cnind00f)
010200040511     d ds_cnclp      e ds                  extname(Cnclp00f)
010300040511     d ds_fncls      e ds                  extname(Fncls00f)
010400040511     d ds3a          e ds
010500040915     d dtas          e ds
010600040915     d  §tasncl      e                     extfld(tasncl)
010700040915     d  §tasflo      e                     extfld(tasflo)
010800040915     d dtasv         e ds
010900040511     d dta4a         e ds
011000040511     d Fnlv55ds      e ds
011100040511     d Kpjba         e ds
011200040921     d Tibs02ds      e ds
011300040916     d Tibs10ds      e ds
011400040916     d  skfigli               21   5520  0 dim(500)
011500040511     d Tibs34ds      e ds
011600040511     d Tibs69ds      e ds
011700040512     d Trul34ds      e ds
011800040512     d  f_i34tla     e                     extfld(i34tla)
011900040512     d  f_o34err     e                     extfld(o34err)
012000040512     d  f_o34msg     e                     extfld(o34msg)
012100040511
012200040511      *------------------------------------------------------------------------*
012300040511      *  RIEPILOGO INDICATORI
012400040511      *------------------------------------------------------------------------*
012500040917      * 10 = POD IMAGE
012600040916      * 30 = Lookup
012700040511
012800040511      *------------------------------------------------------------------------*
012900040511
013000040706     c     wrigdue       Setll     Firig01l
013100040511     c                   Do        *Hival
013200040706     c     wrigdue       Reade     Firig01l
013300040511
013400040511      * fine file
013500040706     c                   If        %Eof(Firig01l)
013600040511     c                   Leave
013700040511     c                   EndIf
013800040511
013900040511      * elaboro solo le bolle firmate da scanner
014000040917      * e i POD
014100040511     c                   If        RigCar <> '3' and RigCar <> '4' and
014200040511     c                             RigCar <> '5' and RigCar <> '6'
014300040915     c                             and RigCar <> 'I'
014400040511     c                   Iter
014500040511     c                   EndIf
014600040921
014700040921      * POD
014800040921     c                   Eval      *In10 = (RigCar = 'I')
014900050131
015000050131     c                   Move      RigDim        wdtrich
015100050131
015200050131     c                   Select
015300050131
015400050131      * Se non è richiesta POD image elaborazione come prima
015500050131w   1c                   When      Not *In10
015600050131      * elaboro se trovo la bolla su titas
015700050131     c     kTitas        Chain     Titas30c
015800050131     c                   If        Not %Found(Titas30c)
015900050131     c                   Iter
016000050131     c                   EndIf
016100050131
016200050131     c                   ExSr      Elabora
016300050131
016400050131      * Aggiorno Firig se ho generato la nuova spedizione
016500050131     c                   If        wokbolla = *On
016600050131     c                   Eval      RigDue = woggi
016700050131     c                   Update    Firig000
016800050131     c                   EndIf
016900050131
017000050131      * E' una richiesa di POD Image
017100050131   x1c                   Other
017200050131
017300050131      * A rottura di bolla elaboro
017400050131if  2c                   If        newbolla <> oldbolla
017500050131
017600040511      * elaboro se trovo la bolla su titas
017700040511     c     kTitas        Chain     Titas30c
017800040511     c                   If        Not %Found(Titas30c)
017900050131     c                   Iter
018000040511     c                   EndIf
018100050131
018200050131     c                   ExSr      Elabora
018300050131     c                   Eval      oldbolla = newbolla
018400050131     c                   Eval      oldrich = newrich
018500050131
018600050131      * Stessa bolla
018700050131   x2c                   Else
018800050131      * richiesta fatta stesso giorno e ora non elaboro
018900050131if  3c                   If        oldrich = newrich
019000050131     c                   Eval      rigtra = 'N'
019100050131      * richiesta fatta in giorno e ora diversi
019200050131   x3c                   Else
019300050131      * richiesta fatta stesso giorno
019400050131if  4c                   If        olddim = rigdim
019500050131      * devo controllare quanti minuti sono passati tra una richiesta e l'altra
019600050131     c                   Movel     oldhim        w0060
019700050131     c                   Move      w0060         whmrich
019800050131      * sommo 2 minuti al vecchio orario
019900050131     c                   Adddur    2:*minutes    whmrich
020000050131     c                   Move      whmrich       w0060
020100050131     c                   Movel     w0060         whim
020200050131      * se non sono passati 2 minuti non elaboro
020300050131if  5c                   If        righim <= whim
020400050131     c                   Eval      rigtra = 'N'
020500050131      * se sono passati elaboro
020600050131   x5c                   Else
020700050131     c                   ExSr      Elabora
020800050131     c                   Eval      oldbolla = newbolla
020900050131     c                   Eval      oldrich = newrich
021000050131e   5c                   EndIf
021100050131      * richiesta fatta in giorno diverso elaboro
021200050131   x4c                   Else
021300050131     c                   ExSr      Elabora
021400050131     c                   Eval      oldbolla = newbolla
021500050131     c                   Eval      oldrich = newrich
021600050131e   4c                   EndIf
021700050131e   3c                   EndIf
021800050131e   2c                   EndIf
021900050131
022000050131      * Aggiorno Firig sempre anche nel caso di bolla non generata
022100040511     c                   Eval      RigDue = woggi
022200040511     c                   Update    Firig000
022300050131e   1c                   EndSl
022400040511
022500040511     c                   EndDo
022600040511
022700040511     c                   Eval      *InLr = *On
022800040511
022900040511      *------------------------------------------------------------------------*
023000040511      * Elaboro la bolla letta
023100040511      *------------------------------------------------------------------------*
023200040511     c     Elabora       BegSr
023300040511
023400040915     c                   Clear                   dtas
023500040915     c                   Clear                   dtasv
023600040915     c                   Clear                   kpjbu
023700041119     c                   Clear                   wnotas
023800040915
023900040511     c                   Eval      wokbolla = *Off
024000040921
024100040921      * se POD devo controllare se è ancora gratis per i clienti o meno
024200040921     c                   If        *In10 and wdtrich <= §lvadtgrts
024300040921     c                   Eval      RigTra = 'X'
024400040921     c                   Eval      wokbolla = *On
024500040921     c                   GoTo      endelab
024600040921     c                   EndIf
024700040916
024800040917      * ricerco i figli del codice unificante se POD
024900050318     c                   If        *In10 and %subst(RigPru:1:5) <> 'TILAC'
025000040916     c                   Clear                   Tibs10ds
025100040916     c                   Eval      d10drf = *date
025200040916     c                   Eval      d10tle = 'WW'
025300040916     c                   Eval      d10paf = 'F'
025400040916     c                   Eval      w008a = %subst(RigPru:1:8)
025500040916     c                   Move      w008a         w0070
025600040916     c                   Move      w008a         d10cod
025700040916     c                   Call      'TIBS10R'
025800040916     c                   Parm                    Tibs10ds
025900040916     c                   EndIf
026000040511
026100040511      * -- > T I T A S 0 0 0
026200040511      *---------------------
026300040511     c                   Clear                   Titas0
026400040511
026500040511      * Anno
026600040917     c  n10              Z-add     *Year         TasAas
026700040917     c   10              Movel     RigDim        TasAas
026800040511      * Linea di partenza
026900040915     c  n10              Eval      TasLnp = RigPOR
027000040915     c   10              Eval      TasLnp = f_TasLnp
027100040511      * Codice bolla
027200040915     c  n10              Eval      TasCbo = 'FF'
027300040915     c   10              Eval      TasCbo = 'FI'
027400040511      * Tipo bolla
027500040511     c                   Clear                   ds3a
027600040511     c                   Eval      TblCod = '3A'
027700040511     c                   Movel(p)  TasCbo        TblKey
027800040511     c     kTab          Chain     Tabel00f
027900040511     c                   If        Not %Found(Tabel00f) or TblFlg <> *Blanks
028000040511     c                   GoTo      endelab
028100040511     c                   EndIF
028200040511     c                   Eval      ds3a = TblUni
028300040511     c                   Eval      TasTbl = §3atb1
028400040511      * Mese/giorno
028500040917     c  n10              Move      woggi         TasMgs
028600040917     c   10              Move      RigDim        TasMgs
028700040511      * Cliente tassazione
028800040511     c                   If        RigCar = '3' or RigCar = '5'
028900040511     c                   Eval      TasKsc = f_TasCcm
029000040511     c                   EndIf
029100040511     c                   If        RigCar = '4' or RigCar = '6'
029200040511     c                   Eval      TasKsc = f_TasKsc
029300040511     c                   EndIf
029400050318      * POD
029500040916if  1c                   If        *In10
029600050318      * se nel campo del profilo utente c'è scritto 'TILAC' il ksc va preso direttamente
029700050318      * dalla bolla
029800050318if 1ac                   If        %subst(RigPru:1:5) = 'TILAC'
029900050318     c                   Eval      TasKsc = f_TasKsc
030000050318     c                   Eval      TasCtr = f_TasCtr
030100050318  x1ac                   Else
030200040916      * controllo se il cliente che ha effettuato la richiesta è il ksc o il ccm della
030300040916      * bolla
030400040916s   2c                   Select
030500040916w   2c                   When      f_TasKsc = w0070
030600040916     c                   Eval      TasKsc = f_TasKsc
030700040916     c                   Eval      TasCtr = f_TasCtr
030800040916w   2c                   When      f_TasCcm = w0070
030900040916     c                   Eval      TasKsc = f_TasCcm
031000040916     c                   Eval      TasCtr = *Zeros
031100040916   x2c                   Other
031200040916      * altrimenti controllo se è uno dei suoi figli
031300040916     c                   Eval      w0110 = f_TasKsc
031400040916     c     w0110         Lookup    skfigli                                30
031500040916if  3c                   If        *In30
031600040916     c                   Eval      TasKsc = f_TasKsc
031700040916     c                   Eval      TasCtr = f_TasCtr
031800040916   x3c                   Else
031900040916     c                   Eval      w0110 = f_TasCcm
032000040916     c     w0110         Lookup    skfigli                                30
032100040916if  4c                   If        *In30
032200040916     c                   Eval      TasKsc = f_TasCcm
032300040916     c                   Eval      TasCtr = *Zeros
032400040916e   4c                   EndIf
032500040916e   3c                   EndIf
032600040916if  3c                   If        Not *In30
032700040916     c                   Eval      RigTra = 'Y'
032800040916     c                   Eval      wokbolla = *On
032900040916     c                   GoTo      endelab
033000040916e   3c                   EndIf
033100040916e   2c                   EndSl
033200050318e  1ac                   EndIf
033300040916e   1c                   EndIf
033400040511      * Linea di arrivo
033500040915     c  n10              Eval      TasLna = RigPor
033600040915     c   10              Eval      TasLna = f_TasLnp
033700040511      * Numero Colli
033800040511     c                   Eval      TasNcl = 1
033900040511      * Peso bollettato + flag
034000040511     c                   Eval      TasPkb = 1
034100040511     c                   Eval      TasFpb = 'R'
034200040511      * Peso fatturato + flag
034300040511     c                   Eval      TasPkf = 1
034400040511     c                   Eval      TasFpf = 'R'
034500040511      * Volume bollettato + flag
034600040511     c                   Eval      TasVlb = 0,001
034700040511     c                   Eval      TasFvb = 'R'
034800040511      * Volume fatturato + flag
034900040511     c                   Eval      TasVlf = 0,001
035000040511     c                   Eval      TasFvf = 'R'
035100040511      * Codice tariffa
035200040518     c                   If        ((RigCar = '3' or RigCar = '5') and
035300040511     c                              %subst(f_TasTbl:1:1) = 'F') or
035400040518     c                             ((RigCar = '4' or RigCar = '6') and
035500040511     c                              %subst(f_TasTbl:1:1) = 'A')
035600040511     c                   Eval      TasCtr = f_TasCtr
035700040511     c                   EndIf
035800040511      * Tipo servizio
035900040511     c                   Eval      TasTsp = 'C'
036000040511      * Destinatario
036100040511     c                   Clear                   Tibs69ds
036200040511     c                   Z-add     TasKsc        i69kac
036300040511     c                   Z-add     TasKsc        i69kin
036400040511     c                   Call      'TIBS69R'
036500040511     c                   Parm                    Tibs69ds
036600040511     c                   Parm                    ds_cnaco
036700040511     c                   Parm                    ds_cnind
036800040511     c                   Parm                    ds_cnclp
036900040511     c                   Parm                    ds_fncls
037000040511     c                   If        o69err = '1'
037100040511     c                   GoTo      endelab
037200040511     c                   EndIF
037300040511     c                   Eval      TasRsd = AcoRag
037400040511     c                   Eval      TasInd = IndVia
037500040511     c                   Eval      TasCad = IndCae
037600040511     c                   Eval      TasLod = IndCit
037700040511     c                   Eval      TasPrd = IndPrv
037800040511     c                   Eval      TasNzd = IndSta
037900040511      * Codice tassazione destinatario
038000040511     c                   Eval      TasCts = '40'
038100040511      * Flag inoltro
038200040511     c                   Eval      TasFin = 'C'
038300040511      * Cliente mittente
038400040915     c  n10              Movel     RigPor        TasCcm
038500040915     c   10              Movel     TasLnp        TasCcm
038600040511     c                   Move      8888          TasCcm
038700040511      * Riferimento mittente numerico
038800040511     c                   Move      RigAas        wrigaas
038900040511     c                   Move      RigLnp        wriglnp
039000040511     c                   Move      RigNrs        wrignrs
039100040511     c                   Move      RigNsp        wrignsp
039200040511     c                   Move      wnumbolla     TasRmn
039300040511      * Codice tassazione mittente
039400040511     c                   Eval      TasMct = '40'
039500040512      * Flag anteporto
039600040512     c                   Eval      TasFap = 'C'
039700040511      * Natura merce
039800040511     c                   Eval      TasNas = 'BOLLA FIRMATA'
039900040511      * P.o. bollettazione
040000040915     c  n10              Eval      TasFlb = RigPor
040100040915     c   10              Eval      TasFlb = TasLnp
040200040511      * Terminal Arrivo
040300040511     c                   Clear                   Fnlv55ds
040400040511     c                   Eval      d55tpt = 'A'
040500040915     c                   Eval      d55lin = TasLna
040600040511     c                   Eval      d55drf = woggi
040700040511     c                   Call      'FNLV55R'
040800040511     c                   Parm                    Fnlv55ds
040900040511     c                   If        d55err <> *Blanks
041000040511     c                   GoTo      endelab
041100040511     c                   EndIF
041200040511     c                   Eval      TasTfa = d55Tfa
041300040511      * Terminal partenza
041400040511     c                   Eval      d55tpt = 'P'
041500040915     c                   Eval      d55lin = TasLnp
041600040511     c                   Call      'FNLV55R'
041700040511     c                   Parm                    Fnlv55ds
041800040511     c                   If        d55err <> *Blanks
041900040511     c                   GoTo      endelab
042000040511     c                   EndIF
042100040511     c                   Eval      TasTfp = d55Tfp
042200040511      * Data immissione
042300040511     c                   Eval      TasDim = woggi
042400040511      * Sigla operatore
042500040511     c                   Eval      TasSop = '*'
042600040511      * Codice autotrasportatore
042700060711     c                   Movel     TasLnp        TasPdr
042800040511     c                   Move      999           TasPdr
042900040511      * Data/Ora ritiro
043000040511     c                   Eval      TasDrt = woggi
043100040518     c                   Movel     wora          TasHrt
043200040511      * Flag prestazione autotrasportatore
043300040511     c                   Eval      TasFpp = 'P'
043400040511      * Trattamento merce
043500040511     c                   Eval      TasCtm = §3actm
043600040511      * Data Borderò
043700040511     c                   Eval      TasDbr = woggi
043800040511      * Flag merce portata a magazzino
043900040511     c                   Eval      TasFdn = 'S'
044000040511      * Flag ddt si/no
044100040511     c                   Eval      TasLl1 = 'Y'
044200040511      * Nazione e cap mittente
044300040511     c                   Eval      TasNzm = IndSta
044400040511     c                   Eval      TasCam = IndCae
044500040511      * Varia se da impostare a zero
044600040511     c                   If        RigCar = '5' or RigCar = '6'
044700040511     c                   Eval      TasSv1 = 'O'
044800040511     c                   EndIf
044900060419      * Anche per POD
045000060419     c                   if        *in10 and %subst(rigpru:1:6) = 'TILAC1'
045100060419     c                   eval      tassv1 = §3asva
045200060419     c                   endif
045300040511      * Falg bartolini/sdi
045400040511     c                   Eval      TasScl = 'B'
045500040915
045600040917      * Se POD prima di scrivere i file devo verificare che ci sia la tariffa per la varia 'a'
045700040915      * se non c'è la bolla non deve essere creata
045800041203      * 03/12/04 non cerco più se c'è la tariffa la bolla la devo creare sempre
045900041203     c                   Goto      notnsf
046000060419      *----------------------------------
046100040916if  1c                   If        *In10
046200040916     c                   Eval      TasTla = 'L'
046300040916      * imposto la data spedizione
046400040916     c                   Movel     TasAas        TasDsp
046500040916     c                   Move      TasMgs        TasDsp
046600040916     c                   Eval      TasDct = TasDsp
046700040915      * imposto che si tratta di una bolla monovaria
046800040915     c                   If        §3asva <> *Blanks
046900040915     c                   Eval      TasSva = §3asva
047000040915     c                   EndIf
047100040915
047200040915     c                   Call      'TNSF20R'
047300040915     c                   Parm                    kpjba
047400040915     c                   Parm                    dtas
047500040915     c                   Parm                    dtasv
047600040915
047700040915      * se torna errore dalla tassazione non genero la bolla
047800040915     c                   If        TasErr <> *Blanks
047900040917     c                   Eval      RigTra = 'Z'
048000040917     c                   Else
048100040915      * se la varia non è valorizzata non genero la bolla
048200040915     c                   If        TasVa1 <= *Zeros
048300041119     c                   Eval      wnotas = 'S'
048400040915     c                   EndIf
048500040917     c                   EndIf
048600040915
048700040916     c                   Clear                   TasDiv
048800040916     c                   Clear                   TasImv
048900041119     c                   If        wnotas = *blanks
049000040916     c                   Clear                   dtasv
049100041119     c                   EndIf
049200040916e   1c                   EndIf
049300060419      *----------------------------------
049400041203     c     notnsf        Tag
049500040915
049600040601      * Numero spedizione (da Flnuf cod. 3)
049700040513     c                   Clear                   Trul34ds
049800040513     c                   Eval      i34Aas = TasAas
049900040513     c                   Eval      i34Lnp = TasLnp
050000040513     c                   Call      'TRUL34R'
050100040513     c                   Parm                    kpjba
050200040513     c                   Parm                    Trul34ds
050300040513     c                   If        f_o34err <> *Zeros
050400040513     c                   GoTo      endelab
050500040513     c                   EndIF
050600040513     c                   Eval      TasNsp = o34Nsp
050700040511
050800040511
050900040511      * -- > T I T A A 0 0 0
051000040511      *---------------------
051100040511      * Rcd. 'O' - mittente originale
051200040513     c                   Clear                   Titaa0
051300040511
051400040511      * Key spedizione
051500040511     c                   Eval      TaaAas = TasAas
051600040511     c                   Eval      TaaLnp = TasLnp
051700040511     c                   Eval      TaaNrs = TasNrs
051800040511     c                   Eval      TaaNsp = TasNsp
051900040511      * Tipo record
052000040511     c                   Eval      TaaTrc = 'O'
052100040511      * Ragione sociale del destinatario se addebito al mittente
052200040511     c                   If        RigCar = '3' or RigCar = '5'
052300040511     c                   Eval      TaaRsc = f_TasRsd
052400040511     c                   EndIf
052500040511      * Ragione sociale del mittente se addebito al destinatario
052600040511     c                   If        RigCar = '4' or RigCar = '6'
052700040513     c     kTitaa        Chain     Titaa30c
052800040513     c                   If        Not %Found(Titaa30c)
052900040513     c                   Clear                   f_TaaRsc
053000040513     c                   EndIF
053100040513     c                   Eval      TaaRsc = f_TaaRsc
053200040511     c                   EndIf
053300040917      * Ragione sociale del destinatario x POD
053400040915     c                   If        RigCar = 'I'
053500040915     c                   Eval      TaaRsc = f_TasRsd
053600040915     c                   EndIf
053700040513     c                   Write     Titaa0
053800040511
053900040511      * Rcd. 'M' - mittente
054000040513     c                   Clear                   Titaa0
054100040511
054200040511      * Key spedizione
054300040511     c                   Eval      TaaAas = TasAas
054400040511     c                   Eval      TaaLnp = TasLnp
054500040511     c                   Eval      TaaNrs = TasNrs
054600040511     c                   Eval      TaaNsp = TasNsp
054700040511      * Tipo record
054800040511     c                   Eval      TaaTrc = 'M'
054900040511      * Dati del p.o. di richiesta
055000040915     c  n10RigPor        Chain     Azorg01l
055100040916     c   10kAzorg        Chain     Azorg01l
055200040511     c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks
055300040513     c                   Clear                   OrgInd
055400040616     c                   Clear                   OrgCpf
055500040513     c                   Clear                   OrgLoc
055600040513     c                   Clear                   OrgPro
055700040511     c                   EndIF
055800040916     c  n10              Eval      TaaRsc = 'BOLLA FIRMATA - P.O.' + OrgDes
055900040916     c   10              Eval      TaaRsc = 'POD IMAGE'
056000040511     c                   Eval      TaaInd = OrgInd
056100040616     c                   Movel     OrgCpf        TaaCap
056200040511     c                   Eval      TaaLoc = OrgLoc
056300040511     c                   Eval      TaaPrv = OrgPro
056400040513     c                   Write     Titaa0
056500040511
056600040511
056700040511      * -- > T I T A 4 0 0 0
056800040511      *---------------------
056900040511      * Rcd. 'A' - riferimenti
057000040511     c                   Clear                   Tita4000
057100040511     c                   Clear                   dta4a
057200040511
057300040511      * Key spedizione
057400040511     c                   Eval      Ta4Aas = TasAas
057500040511     c                   Eval      Ta4Lnp = TasLnp
057600040511     c                   Eval      Ta4Nrs = TasNrs
057700040511     c                   Eval      Ta4Nsp = TasNsp
057800040511      * Tipo record
057900040511     c                   Eval      Ta4Trc = 'A'
058000040511      * Riferimento mittente alfabetico
058100040511     c                   Eval      §ta4arma = %editc(f_TasRmn:'Z')
058200040511     c                   Eval      Ta4Not = dta4a
058300040511     c                   Write     Tita4000
058400040511
058500040511     c                   Write     Titas0
058600040511
058700040511     c                   Eval      wokbolla = *On
058800040511
058900040511     c     endelab       EndSr
059000040511
059100040511      *------------------------------------------------------------------------*
059200040511      * Routine iniziale
059300040511      *------------------------------------------------------------------------*
059400040511     c     *Inzsr        BegSr
059500040511
059600040511     c     *Entry        Plist
059700040511     c                   Parm                    kpjba
059800040917     c                   eval      welab = %subst(kpjbu:1:1)
059900040511
060000040511     c     *dtaara       define    §azute        azuteds
060100040511     c     *dtaara       define    §datiute      ddatiute
060200040511     c                   in(E)     *dtaara
060300040511     c                   If        %error  or RSUT = *blanks
060400040511     c                   Clear                   Tibs34ds
060500040511     c                   Call      'TIBS34R'
060600040511     c                   Parm                    Tibs34ds
060700040511     c                   In        *dtaara
060800040511     c                   EndIf
060900040921
061000040921      * Recupero la data x sapere fino a quando il POD Image è gratis
061100040921     c                   Clear                   Tibs02ds
061200040921     c                   Clear                   dLva1
061300040921     c                   Eval      T02Mod = 'C'
061400040921     c                   Eval      T02Sif =  knsif
061500040921     c                   Eval      T02Cod = 'LVA'
061600040921     c                   Eval      T02Ke1 = '1'
061700040921     c                   Call      'TIBS02R'
061800040921     c                   Parm                    kpjba
061900040921     c                   Parm                    Tibs02ds
062000040921     c                   If        T02Err = *Blanks
062100040921     c                   Eval      dLva1 = T02Uni
062200040921     c                   EndIf
062300040511
062400040511      * data/ora del job
062500040511     c                   Z-add     *date         woggi
062600040511     c                   Time                    wora
062700040511
062800040511     c     kTitas        klist
062900040511     c                   kfld                    RigAas
063000040511     c                   kfld                    RigLnp
063100040511     c                   kfld                    RigNrs
063200040511     c                   kfld                    RigNsp
063300040511
063400040511     c     kTab          klist
063500040511     c                   kfld                    TblKut
063600040511     c                   kfld                    TblCod
063700040511     c                   kfld                    TblKey
063800040513
063900040513     c     kTitaa        klist
064000040513     c                   kfld                    RigAas
064100040513     c                   kfld                    RigLnp
064200040513     c                   kfld                    RigNrs
064300040513     c                   kfld                    RigNsp
064400040513     c                   kfld                    wtrc
064500040513
064600040511     c                   Eval      TblKut = 1
064700040513     c                   Eval      wtrc = 'M'
064800040511
064900040511     c                   EndSr
