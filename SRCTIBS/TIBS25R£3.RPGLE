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
005700070910     d w0030           s              3  0
005800040917     d w0060           s              6  0
005900040916     d w0070           s              7  0
006000040916     d w008a           s              8
006100040916     d w0110           s             11  0
006200040511
006300040511      *------------------------------------------------------------------------*
006400040511      *   D S   I N T E R N E / E S T E R N E
006500040511      *------------------------------------------------------------------------*
006600040511     d wnumbolla       ds
006700040511     d  wrigaas                1      2
006800040511     d  wriglnp                3      5
006900040511     d  wrignrs                6      7
007000040511     d  wrignsp                8     14
007100050131
007200050131     d oldbolla        ds
007300050131     d  oldaas                 1      4  0
007400050131     d  oldlnp                 5      7  0
007500050131     d  oldnrs                 8      9  0
007600050131     d  oldnsp                10     16  0
007700050131
007800050131     d newbolla        ds
007900050131     d  rigaas                 1      4  0
008000050131     d  riglnp                 5      7  0
008100050131     d  rignrs                 8      9  0
008200050131     d  rignsp                10     16  0
008300050131
008400050131     d oldrich         ds
008500050131     d  olddim                 1      8  0
008600050131     d  oldhim                 9     12  0
008700050131
008800050131     d newrich         ds
008900050131     d  rigdim                 1      8  0
009000050131     d  righim                 9     12  0
009100040917
009200040917     d                 ds
009300040917     d aoggi                   1      4  0
009400040917     d moggi                   5      6  0
009500040917     d doggi                   7      8  0
009600040917     d woggi                   1      8  0
009700040511
009800040511     d Azuteds       e ds                  extname(Azute00f)
009900040511     d dDatiute      e ds
010000040921     d dLva1         e ds
010100040511     d ds_cnaco      e ds                  extname(Cnaco00f)
010200040511     d ds_cnind      e ds                  extname(Cnind00f)
010300040511     d ds_cnclp      e ds                  extname(Cnclp00f)
010400040511     d ds_fncls      e ds                  extname(Fncls00f)
010500040511     d ds3a          e ds
010600040915     d dtas          e ds
010700040915     d  §tasncl      e                     extfld(tasncl)
010800040915     d  §tasflo      e                     extfld(tasflo)
010900040915     d dtasv         e ds
011000040511     d dta4a         e ds
011100040511     d Fnlv55ds      e ds
011200040511     d Kpjba         e ds
011300040921     d Tibs02ds      e ds
011400040916     d Tibs10ds      e ds
011500040916     d  skfigli               21   5520  0 dim(500)
011600040511     d Tibs34ds      e ds
011700040511     d Tibs69ds      e ds
011800040512     d Trul34ds      e ds
011900040512     d  f_i34tla     e                     extfld(i34tla)
012000040512     d  f_o34err     e                     extfld(o34err)
012100040512     d  f_o34msg     e                     extfld(o34msg)
012200040511
012300040511      *------------------------------------------------------------------------*
012400040511      *  RIEPILOGO INDICATORI
012500040511      *------------------------------------------------------------------------*
012600040917      * 10 = POD IMAGE
012700040916      * 30 = Lookup
012800040511
012900040511      *------------------------------------------------------------------------*
013000040511
013100040706     c     wrigdue       Setll     Firig01l
013200040511     c                   Do        *Hival
013300040706     c     wrigdue       Reade     Firig01l
013400040511
013500040511      * fine file
013600040706     c                   If        %Eof(Firig01l)
013700040511     c                   Leave
013800040511     c                   EndIf
013900040511
014000040511      * elaboro solo le bolle firmate da scanner
014100040917      * e i POD
014200040511     c                   If        RigCar <> '3' and RigCar <> '4' and
014300040511     c                             RigCar <> '5' and RigCar <> '6'
014400040915     c                             and RigCar <> 'I'
014500040511     c                   Iter
014600040511     c                   EndIf
014700040921
014800040921      * POD
014900040921     c                   Eval      *In10 = (RigCar = 'I')
015000050131
015100050131     c                   Move      RigDim        wdtrich
015200050131
015300050131     c                   Select
015400050131
015500050131      * Se non è richiesta POD image elaborazione come prima
015600050131w   1c                   When      Not *In10
015700050131      * elaboro se trovo la bolla su titas
015800050131     c     kTitas        Chain     Titas30c
015900050131     c                   If        Not %Found(Titas30c)
016000050131     c                   Iter
016100050131     c                   EndIf
016200050131
016300050131     c                   ExSr      Elabora
016400050131
016500050131      * Aggiorno Firig se ho generato la nuova spedizione
016600050131     c                   If        wokbolla = *On
016700050131     c                   Eval      RigDue = woggi
016800050131     c                   Update    Firig000
016900050131     c                   EndIf
017000050131
017100050131      * E' una richiesa di POD Image
017200050131   x1c                   Other
017300050131
017400050131      * A rottura di bolla elaboro
017500050131if  2c                   If        newbolla <> oldbolla
017600050131
017700040511      * elaboro se trovo la bolla su titas
017800040511     c     kTitas        Chain     Titas30c
017900040511     c                   If        Not %Found(Titas30c)
018000050131     c                   Iter
018100040511     c                   EndIf
018200050131
018300050131     c                   ExSr      Elabora
018400050131     c                   Eval      oldbolla = newbolla
018500050131     c                   Eval      oldrich = newrich
018600050131
018700050131      * Stessa bolla
018800050131   x2c                   Else
018900050131      * richiesta fatta stesso giorno e ora non elaboro
019000050131if  3c                   If        oldrich = newrich
019100050131     c                   Eval      rigtra = 'N'
019200050131      * richiesta fatta in giorno e ora diversi
019300050131   x3c                   Else
019400050131      * richiesta fatta stesso giorno
019500050131if  4c                   If        olddim = rigdim
019600050131      * devo controllare quanti minuti sono passati tra una richiesta e l'altra
019700050131     c                   Movel     oldhim        w0060
019800050131     c                   Move      w0060         whmrich
019900050131      * sommo 2 minuti al vecchio orario
020000090216      * LB 16-02-09 passare da 2 a 30 minuti
020100090216     c**!!!              Adddur    2:*minutes    whmrich
020200090216     c                   Adddur    30:*minutes   whmrich
020300050131     c                   Move      whmrich       w0060
020400050131     c                   Movel     w0060         whim
020500090216      * se non sono passati 30 minuti non elaboro
020600050131if  5c                   If        righim <= whim
020700050131     c                   Eval      rigtra = 'N'
020800050131      * se sono passati elaboro
020900050131   x5c                   Else
021000050131     c                   ExSr      Elabora
021100050131     c                   Eval      oldbolla = newbolla
021200050131     c                   Eval      oldrich = newrich
021300050131e   5c                   EndIf
021400050131      * richiesta fatta in giorno diverso elaboro
021500050131   x4c                   Else
021600050131     c                   ExSr      Elabora
021700050131     c                   Eval      oldbolla = newbolla
021800050131     c                   Eval      oldrich = newrich
021900050131e   4c                   EndIf
022000050131e   3c                   EndIf
022100050131e   2c                   EndIf
022200050131
022300050131      * Aggiorno Firig sempre anche nel caso di bolla non generata
022400040511     c                   Eval      RigDue = woggi
022500040511     c                   Update    Firig000
022600050131e   1c                   EndSl
022700040511
022800040511     c                   EndDo
022900040511
023000040511     c                   Eval      *InLr = *On
023100040511
023200040511      *------------------------------------------------------------------------*
023300040511      * Elaboro la bolla letta
023400040511      *------------------------------------------------------------------------*
023500040511     c     Elabora       BegSr
023600040511
023700040915     c                   Clear                   dtas
023800040915     c                   Clear                   dtasv
023900040915     c                   Clear                   kpjbu
024000041119     c                   Clear                   wnotas
024100040915
024200040511     c                   Eval      wokbolla = *Off
024300040921
024400040921      * se POD devo controllare se è ancora gratis per i clienti o meno
024500040921     c                   If        *In10 and wdtrich <= §lvadtgrts
024600040921     c                   Eval      RigTra = 'X'
024700040921     c                   Eval      wokbolla = *On
024800040921     c                   GoTo      endelab
024900040921     c                   EndIf
025000040916
025100040917      * ricerco i figli del codice unificante se POD
025200050318     c                   If        *In10 and %subst(RigPru:1:5) <> 'TILAC'
025300040916     c                   Clear                   Tibs10ds
025400040916     c                   Eval      d10drf = *date
025500040916     c                   Eval      d10tle = 'WW'
025600040916     c                   Eval      d10paf = 'F'
025700040916     c                   Eval      w008a = %subst(RigPru:1:8)
025800040916     c                   Move      w008a         w0070
025900040916     c                   Move      w008a         d10cod
026000040916     c                   Call      'TIBS10R'
026100040916     c                   Parm                    Tibs10ds
026200040916     c                   EndIf
026300040511
026400040511      * -- > T I T A S 0 0 0
026500040511      *---------------------
026600040511     c                   Clear                   Titas0
026700040511
026800040511      * Anno
026900040917     c  n10              Z-add     *Year         TasAas
027000040917     c   10              Movel     RigDim        TasAas
027100040511      * Linea di partenza
027200040915     c  n10              Eval      TasLnp = RigPOR
027300040915     c   10              Eval      TasLnp = f_TasLnp
027400040511      * Codice bolla
027500040915     c  n10              Eval      TasCbo = 'FF'
027600040915     c   10              Eval      TasCbo = 'FI'
027700040511      * Tipo bolla
027800040511     c                   Clear                   ds3a
027900040511     c                   Eval      TblCod = '3A'
028000040511     c                   Movel(p)  TasCbo        TblKey
028100040511     c     kTab          Chain     Tabel00f
028200040511     c                   If        Not %Found(Tabel00f) or TblFlg <> *Blanks
028300040511     c                   GoTo      endelab
028400040511     c                   EndIF
028500040511     c                   Eval      ds3a = TblUni
028600040511     c                   Eval      TasTbl = §3atb1
028700040511      * Mese/giorno
028800040917     c  n10              Move      woggi         TasMgs
028900040917     c   10              Move      RigDim        TasMgs
029000040511      * Cliente tassazione
029100040511     c                   If        RigCar = '3' or RigCar = '5'
029200040511     c                   Eval      TasKsc = f_TasCcm
029300040511     c                   EndIf
029400040511     c                   If        RigCar = '4' or RigCar = '6'
029500040511     c                   Eval      TasKsc = f_TasKsc
029600040511     c                   EndIf
029700050318      * POD
029800040916if  1c                   If        *In10
029900050318      * se nel campo del profilo utente c'è scritto 'TILAC' il ksc va preso direttamente
030000050318      * dalla bolla
030100050318if 1ac                   If        %subst(RigPru:1:5) = 'TILAC'
030200050318     c                   Eval      TasKsc = f_TasKsc
030300050318     c                   Eval      TasCtr = f_TasCtr
030400050318  x1ac                   Else
030500040916      * controllo se il cliente che ha effettuato la richiesta è il ksc o il ccm della
030600040916      * bolla
030700040916s   2c                   Select
030800040916w   2c                   When      f_TasKsc = w0070
030900040916     c                   Eval      TasKsc = f_TasKsc
031000040916     c                   Eval      TasCtr = f_TasCtr
031100040916w   2c                   When      f_TasCcm = w0070
031200040916     c                   Eval      TasKsc = f_TasCcm
031300040916     c                   Eval      TasCtr = *Zeros
031400040916   x2c                   Other
031500040916      * altrimenti controllo se è uno dei suoi figli
031600040916     c                   Eval      w0110 = f_TasKsc
031700040916     c     w0110         Lookup    skfigli                                30
031800040916if  3c                   If        *In30
031900040916     c                   Eval      TasKsc = f_TasKsc
032000040916     c                   Eval      TasCtr = f_TasCtr
032100040916   x3c                   Else
032200040916     c                   Eval      w0110 = f_TasCcm
032300040916     c     w0110         Lookup    skfigli                                30
032400040916if  4c                   If        *In30
032500040916     c                   Eval      TasKsc = f_TasCcm
032600040916     c                   Eval      TasCtr = *Zeros
032700040916e   4c                   EndIf
032800040916e   3c                   EndIf
032900040916if  3c                   If        Not *In30
033000040916     c                   Eval      RigTra = 'Y'
033100040916     c                   Eval      wokbolla = *On
033200040916     c                   GoTo      endelab
033300040916e   3c                   EndIf
033400040916e   2c                   EndSl
033500050318e  1ac                   EndIf
033600070910      * se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
033700070910     c                   movel     tasksc        w0030
033800070910     c                   if        w0030 = 102 or w0030 = 101
033900070910     c                   eval      rigtra = 'X'
034000070910     c                   leavesr
034100070910     c                   endif
034200070910      *
034300070910e   1c                   EndIf
034400040511      * Linea di arrivo
034500040915     c  n10              Eval      TasLna = RigPor
034600040915     c   10              Eval      TasLna = f_TasLnp
034700040511      * Numero Colli
034800040511     c                   Eval      TasNcl = 1
034900040511      * Peso bollettato + flag
035000040511     c                   Eval      TasPkb = 1
035100040511     c                   Eval      TasFpb = 'R'
035200040511      * Peso fatturato + flag
035300040511     c                   Eval      TasPkf = 1
035400040511     c                   Eval      TasFpf = 'R'
035500040511      * Volume bollettato + flag
035600040511     c                   Eval      TasVlb = 0,001
035700040511     c                   Eval      TasFvb = 'R'
035800040511      * Volume fatturato + flag
035900040511     c                   Eval      TasVlf = 0,001
036000040511     c                   Eval      TasFvf = 'R'
036100040511      * Codice tariffa
036200040518     c                   If        ((RigCar = '3' or RigCar = '5') and
036300040511     c                              %subst(f_TasTbl:1:1) = 'F') or
036400040518     c                             ((RigCar = '4' or RigCar = '6') and
036500040511     c                              %subst(f_TasTbl:1:1) = 'A')
036600040511     c                   Eval      TasCtr = f_TasCtr
036700040511     c                   EndIf
036800040511      * Tipo servizio
036900040511     c                   Eval      TasTsp = 'C'
037000040511      * Destinatario
037100040511     c                   Clear                   Tibs69ds
037200040511     c                   Z-add     TasKsc        i69kac
037300040511     c                   Z-add     TasKsc        i69kin
037400040511     c                   Call      'TIBS69R'
037500040511     c                   Parm                    Tibs69ds
037600040511     c                   Parm                    ds_cnaco
037700040511     c                   Parm                    ds_cnind
037800040511     c                   Parm                    ds_cnclp
037900040511     c                   Parm                    ds_fncls
038000040511     c                   If        o69err = '1'
038100040511     c                   GoTo      endelab
038200040511     c                   EndIF
038300040511     c                   Eval      TasRsd = AcoRag
038400040511     c                   Eval      TasInd = IndVia
038500040511     c                   Eval      TasCad = IndCae
038600040511     c                   Eval      TasLod = IndCit
038700040511     c                   Eval      TasPrd = IndPrv
038800040511     c                   Eval      TasNzd = IndSta
038900040511      * Codice tassazione destinatario
039000040511     c                   Eval      TasCts = '40'
039100040511      * Flag inoltro
039200040511     c                   Eval      TasFin = 'C'
039300040511      * Cliente mittente
039400040915     c  n10              Movel     RigPor        TasCcm
039500040915     c   10              Movel     TasLnp        TasCcm
039600040511     c                   Move      8888          TasCcm
039700040511      * Riferimento mittente numerico
039800040511     c                   Move      RigAas        wrigaas
039900040511     c                   Move      RigLnp        wriglnp
040000040511     c                   Move      RigNrs        wrignrs
040100040511     c                   Move      RigNsp        wrignsp
040200040511     c                   Move      wnumbolla     TasRmn
040300040511      * Codice tassazione mittente
040400040511     c                   Eval      TasMct = '40'
040500040512      * Flag anteporto
040600040512     c                   Eval      TasFap = 'C'
040700040511      * Natura merce
040800040511     c                   Eval      TasNas = 'BOLLA FIRMATA'
040900040511      * P.o. bollettazione
041000040915     c  n10              Eval      TasFlb = RigPor
041100040915     c   10              Eval      TasFlb = TasLnp
041200040511      * Terminal Arrivo
041300040511     c                   Clear                   Fnlv55ds
041400040511     c                   Eval      d55tpt = 'A'
041500040915     c                   Eval      d55lin = TasLna
041600040511     c                   Eval      d55drf = woggi
041700040511     c                   Call      'FNLV55R'
041800040511     c                   Parm                    Fnlv55ds
041900040511     c                   If        d55err <> *Blanks
042000040511     c                   GoTo      endelab
042100040511     c                   EndIF
042200040511     c                   Eval      TasTfa = d55Tfa
042300040511      * Terminal partenza
042400040511     c                   Eval      d55tpt = 'P'
042500040915     c                   Eval      d55lin = TasLnp
042600040511     c                   Call      'FNLV55R'
042700040511     c                   Parm                    Fnlv55ds
042800040511     c                   If        d55err <> *Blanks
042900040511     c                   GoTo      endelab
043000040511     c                   EndIF
043100040511     c                   Eval      TasTfp = d55Tfp
043200040511      * Data immissione
043300040511     c                   Eval      TasDim = woggi
043400040511      * Sigla operatore
043500040511     c                   Eval      TasSop = '*'
043600040511      * Codice autotrasportatore
043700060711     c                   Movel     TasLnp        TasPdr
043800040511     c                   Move      999           TasPdr
043900040511      * Data/Ora ritiro
044000040511     c                   Eval      TasDrt = woggi
044100040518     c                   Movel     wora          TasHrt
044200040511      * Flag prestazione autotrasportatore
044300040511     c                   Eval      TasFpp = 'P'
044400040511      * Trattamento merce
044500040511     c                   Eval      TasCtm = §3actm
044600040511      * Data Borderò
044700040511     c                   Eval      TasDbr = woggi
044800040511      * Flag merce portata a magazzino
044900040511     c                   Eval      TasFdn = 'S'
045000040511      * Flag ddt si/no
045100040511     c                   Eval      TasLl1 = 'Y'
045200040511      * Nazione e cap mittente
045300040511     c                   Eval      TasNzm = IndSta
045400040511     c                   Eval      TasCam = IndCae
045500040511      * Varia se da impostare a zero
045600040511     c                   If        RigCar = '5' or RigCar = '6'
045700040511     c                   Eval      TasSv1 = 'O'
045800040511     c                   EndIf
045900060419      * Anche per POD
046000060419     c                   if        *in10 and %subst(rigpru:1:6) = 'TILAC1'
046100060419     c                   eval      tassv1 = §3asva
046200060419     c                   endif
046300040511      * Falg bartolini/sdi
046400040511     c                   Eval      TasScl = 'B'
046500040915
046600040917      * Se POD prima di scrivere i file devo verificare che ci sia la tariffa per la varia 'a'
046700040915      * se non c'è la bolla non deve essere creata
046800041203      * 03/12/04 non cerco più se c'è la tariffa la bolla la devo creare sempre
046900041203     c                   Goto      notnsf
047000060419      *----------------------------------
047100040916if  1c                   If        *In10
047200040916     c                   Eval      TasTla = 'L'
047300040916      * imposto la data spedizione
047400040916     c                   Movel     TasAas        TasDsp
047500040916     c                   Move      TasMgs        TasDsp
047600040916     c                   Eval      TasDct = TasDsp
047700040915      * imposto che si tratta di una bolla monovaria
047800040915     c                   If        §3asva <> *Blanks
047900040915     c                   Eval      TasSva = §3asva
048000040915     c                   EndIf
048100040915
048200040915     c                   Call      'TNSF20R'
048300040915     c                   Parm                    kpjba
048400040915     c                   Parm                    dtas
048500040915     c                   Parm                    dtasv
048600040915
048700040915      * se torna errore dalla tassazione non genero la bolla
048800040915     c                   If        TasErr <> *Blanks
048900040917     c                   Eval      RigTra = 'Z'
049000040917     c                   Else
049100040915      * se la varia non è valorizzata non genero la bolla
049200040915     c                   If        TasVa1 <= *Zeros
049300041119     c                   Eval      wnotas = 'S'
049400040915     c                   EndIf
049500040917     c                   EndIf
049600040915
049700040916     c                   Clear                   TasDiv
049800040916     c                   Clear                   TasImv
049900041119     c                   If        wnotas = *blanks
050000040916     c                   Clear                   dtasv
050100041119     c                   EndIf
050200040916e   1c                   EndIf
050300060419      *----------------------------------
050400041203     c     notnsf        Tag
050500040915
050600040601      * Numero spedizione (da Flnuf cod. 3)
050700040513     c                   Clear                   Trul34ds
050800040513     c                   Eval      i34Aas = TasAas
050900040513     c                   Eval      i34Lnp = TasLnp
051000040513     c                   Call      'TRUL34R'
051100040513     c                   Parm                    kpjba
051200040513     c                   Parm                    Trul34ds
051300040513     c                   If        f_o34err <> *Zeros
051400040513     c                   GoTo      endelab
051500040513     c                   EndIF
051600040513     c                   Eval      TasNsp = o34Nsp
051700040511
051800040511
051900040511      * -- > T I T A A 0 0 0
052000040511      *---------------------
052100040511      * Rcd. 'O' - mittente originale
052200040513     c                   Clear                   Titaa0
052300040511
052400040511      * Key spedizione
052500040511     c                   Eval      TaaAas = TasAas
052600040511     c                   Eval      TaaLnp = TasLnp
052700040511     c                   Eval      TaaNrs = TasNrs
052800040511     c                   Eval      TaaNsp = TasNsp
052900040511      * Tipo record
053000040511     c                   Eval      TaaTrc = 'O'
053100040511      * Ragione sociale del destinatario se addebito al mittente
053200040511     c                   If        RigCar = '3' or RigCar = '5'
053300040511     c                   Eval      TaaRsc = f_TasRsd
053400040511     c                   EndIf
053500040511      * Ragione sociale del mittente se addebito al destinatario
053600040511     c                   If        RigCar = '4' or RigCar = '6'
053700040513     c     kTitaa        Chain     Titaa30c
053800040513     c                   If        Not %Found(Titaa30c)
053900040513     c                   Clear                   f_TaaRsc
054000040513     c                   EndIF
054100040513     c                   Eval      TaaRsc = f_TaaRsc
054200040511     c                   EndIf
054300040917      * Ragione sociale del destinatario x POD
054400040915     c                   If        RigCar = 'I'
054500040915     c                   Eval      TaaRsc = f_TasRsd
054600040915     c                   EndIf
054700040513     c                   Write     Titaa0
054800040511
054900040511      * Rcd. 'M' - mittente
055000040513     c                   Clear                   Titaa0
055100040511
055200040511      * Key spedizione
055300040511     c                   Eval      TaaAas = TasAas
055400040511     c                   Eval      TaaLnp = TasLnp
055500040511     c                   Eval      TaaNrs = TasNrs
055600040511     c                   Eval      TaaNsp = TasNsp
055700040511      * Tipo record
055800040511     c                   Eval      TaaTrc = 'M'
055900040511      * Dati del p.o. di richiesta
056000040915     c  n10RigPor        Chain     Azorg01l
056100040916     c   10kAzorg        Chain     Azorg01l
056200040511     c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks
056300040513     c                   Clear                   OrgInd
056400040616     c                   Clear                   OrgCpf
056500040513     c                   Clear                   OrgLoc
056600040513     c                   Clear                   OrgPro
056700040511     c                   EndIF
056800040916     c  n10              Eval      TaaRsc = 'BOLLA FIRMATA - P.O.' + OrgDes
056900040916     c   10              Eval      TaaRsc = 'POD IMAGE'
057000040511     c                   Eval      TaaInd = OrgInd
057100040616     c                   Movel     OrgCpf        TaaCap
057200040511     c                   Eval      TaaLoc = OrgLoc
057300040511     c                   Eval      TaaPrv = OrgPro
057400040513     c                   Write     Titaa0
057500040511
057600040511
057700040511      * -- > T I T A 4 0 0 0
057800040511      *---------------------
057900040511      * Rcd. 'A' - riferimenti
058000040511     c                   Clear                   Tita4000
058100040511     c                   Clear                   dta4a
058200040511
058300040511      * Key spedizione
058400040511     c                   Eval      Ta4Aas = TasAas
058500040511     c                   Eval      Ta4Lnp = TasLnp
058600040511     c                   Eval      Ta4Nrs = TasNrs
058700040511     c                   Eval      Ta4Nsp = TasNsp
058800040511      * Tipo record
058900040511     c                   Eval      Ta4Trc = 'A'
059000040511      * Riferimento mittente alfabetico
059100040511     c                   Eval      §ta4arma = %editc(f_TasRmn:'Z')
059200040511     c                   Eval      Ta4Not = dta4a
059300040511     c                   Write     Tita4000
059400040511
059500040511     c                   Write     Titas0
059600040511
059700040511     c                   Eval      wokbolla = *On
059800040511
059900040511     c     endelab       EndSr
060000040511
060100040511      *------------------------------------------------------------------------*
060200040511      * Routine iniziale
060300040511      *------------------------------------------------------------------------*
060400040511     c     *Inzsr        BegSr
060500040511
060600040511     c     *Entry        Plist
060700040511     c                   Parm                    kpjba
060800040917     c                   eval      welab = %subst(kpjbu:1:1)
060900040511
061000040511     c     *dtaara       define    §azute        azuteds
061100040511     c     *dtaara       define    §datiute      ddatiute
061200040511     c                   in(E)     *dtaara
061300040511     c                   If        %error  or RSUT = *blanks
061400040511     c                   Clear                   Tibs34ds
061500040511     c                   Call      'TIBS34R'
061600040511     c                   Parm                    Tibs34ds
061700040511     c                   In        *dtaara
061800040511     c                   EndIf
061900040921
062000040921      * Recupero la data x sapere fino a quando il POD Image è gratis
062100040921     c                   Clear                   Tibs02ds
062200040921     c                   Clear                   dLva1
062300040921     c                   Eval      T02Mod = 'C'
062400040921     c                   Eval      T02Sif =  knsif
062500040921     c                   Eval      T02Cod = 'LVA'
062600040921     c                   Eval      T02Ke1 = '1'
062700040921     c                   Call      'TIBS02R'
062800040921     c                   Parm                    kpjba
062900040921     c                   Parm                    Tibs02ds
063000040921     c                   If        T02Err = *Blanks
063100040921     c                   Eval      dLva1 = T02Uni
063200040921     c                   EndIf
063300040511
063400040511      * data/ora del job
063500040511     c                   Z-add     *date         woggi
063600040511     c                   Time                    wora
063700040511
063800040511     c     kTitas        klist
063900040511     c                   kfld                    RigAas
064000040511     c                   kfld                    RigLnp
064100040511     c                   kfld                    RigNrs
064200040511     c                   kfld                    RigNsp
064300040511
064400040511     c     kTab          klist
064500040511     c                   kfld                    TblKut
064600040511     c                   kfld                    TblCod
064700040511     c                   kfld                    TblKey
064800040513
064900040513     c     kTitaa        klist
065000040513     c                   kfld                    RigAas
065100040513     c                   kfld                    RigLnp
065200040513     c                   kfld                    RigNrs
065300040513     c                   kfld                    RigNsp
065400040513     c                   kfld                    wtrc
065500040513
065600040511     c                   Eval      TblKut = 1
065700040513     c                   Eval      wtrc = 'M'
065800040511
065900040511     c                   EndSr
