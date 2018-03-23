000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300040511      *------------------------------------------------------------------------*
000400090216      * Creazione bolle addebito per Pod Image
000500040511      *------------------------------------------------------------------------*
000600040511
000700090217     fFnrig01l  uf   e           k Disk
000800040511     fTitas30c  if   e           k Disk    prefix(f_)
000900040513     fTitaa30c  if   e           k Disk    prefix(f_)
001000040511     fTabel00f  if   e           k Disk
001100040511     fAzorg01l  if   e           k Disk
001200040511     fTitas00f  o    e           k Disk    rename(Titas000:Titas0)
001300040513     fTitaa00f  o    e           k Disk    rename(Titaa000:Titaa0)
001400040511     fTita400f  o    e           k Disk
001500040511
001600040511      *------------------------------------------------------------------------*
001700040511      *   S C H I E R E
001800040511      *------------------------------------------------------------------------*
001900040511
002000040511      *------------------------------------------------------------------------*
002100040511      *   V A R I A B I L I
002200040511      *------------------------------------------------------------------------*
002300040916     d kazorg          s                   like(OrgFil) inz(102)
002400040921     d wdtrich         s               d   datfmt(*iso)
002500040917     d welab           s              1    inz
002600090216     d wora            s                   like(Rigora)
002700050131     d whmrich         s               t
002800041119     d wnotas          s              1    inz
002900040511     d wokbolla        s              1    inz(*Off)
003000040511     d wora            s              6  0
003100090216     d wrigdela        s                   like(RigDela) inz
003200040513     d wtrc            s                   like(TaaTrc)
003300070910     d w0030           s              3  0
003400040917     d w0060           s              6  0
003500040916     d w0070           s              7  0
003600040916     d w008a           s              8
003700040916     d w0110           s             11  0
003800040511
003900040511      *------------------------------------------------------------------------*
004000040511      *   D S   I N T E R N E / E S T E R N E
004100040511      *------------------------------------------------------------------------*
004200040511     d wnumbolla       ds
004300040511     d  wrigaas                1      2
004400040511     d  wriglnp                3      5
004500040511     d  wrignrs                6      7
004600040511     d  wrignsp                8     14
004700050131
004800050131     d oldbolla        ds
004900050131     d  oldaas                 1      4  0
005000050131     d  oldlnp                 5      7  0
005100050131     d  oldnrs                 8      9  0
005200050131     d  oldnsp                10     16  0
005300050131
005400050131     d newbolla        ds
005500050131     d  rigaas                 1      4  0
005600050131     d  riglnp                 5      7  0
005700050131     d  rignrs                 8      9  0
005800050131     d  rignsp                10     16  0
005900050131
006000050131     d oldrich         ds
006100090216     d  olddata                1      8  0
006200090216     d  oldora                 9     14  0
006300050131
006400050131     d newrich         ds
006500090216     d  rigdata                1      8  0
006600090216     d  rigora                 9     14  0
006700040917
006800040917     d                 ds
006900040917     d aoggi                   1      4  0
007000040917     d moggi                   5      6  0
007100040917     d doggi                   7      8  0
007200040917     d woggi                   1      8  0
007300040511
007400040511     d Azuteds       e ds                  extname(Azute00f)
007500040511     d dDatiute      e ds
007600040511     d ds_cnaco      e ds                  extname(Cnaco00f)
007700040511     d ds_cnind      e ds                  extname(Cnind00f)
007800040511     d ds_cnclp      e ds                  extname(Cnclp00f)
007900040511     d ds_fncls      e ds                  extname(Fncls00f)
008000040511     d ds3a          e ds
008100040915     d dtas          e ds
008200040915     d  §tasncl      e                     extfld(tasncl)
008300040915     d  §tasflo      e                     extfld(tasflo)
008400040915     d dtasv         e ds
008500040511     d dta4a         e ds
008600040511     d Fnlv55ds      e ds
008700040511     d Kpjba         e ds
008800040921     d Tibs02ds      e ds
008900040916     d Tibs10ds      e ds
009000040916     d  skfigli               21   5520  0 dim(500)
009100040511     d Tibs34ds      e ds
009200040511     d Tibs69ds      e ds
009300040512     d Trul34ds      e ds
009400040512     d  f_i34tla     e                     extfld(i34tla)
009500040512     d  f_o34err     e                     extfld(o34err)
009600040512     d  f_o34msg     e                     extfld(o34msg)
009700040511
009800040511      *------------------------------------------------------------------------*
009900040511      *  RIEPILOGO INDICATORI
010000040511      *------------------------------------------------------------------------*
010100040916      * 30 = Lookup
010200040511
010300040511      *------------------------------------------------------------------------*
010400040511
010500090217     c     wrigdela      Setll     Fnrig01l
010600040511     c                   Do        *Hival
010700090217     c     wrigdela      Reade     Fnrig01l
010800040511
010900040511      * fine file
011000090217     c                   If        %Eof(Fnrig01l)
011100040511     c                   Leave
011200040511     c                   EndIf
011300040511
011400090216      * elaboro solo i POD
011500090216     c                   If        rigcau <> 'I'
011600040511     c                   Iter
011700040511     c                   EndIf
011800050131
011900090216     c                   Move      Rigdata       wdtrich
012000050131
012100050131      * A rottura di bolla elaboro
012200050131if  2c                   If        newbolla <> oldbolla
012300050131
012400040511      * elaboro se trovo la bolla su titas
012500040511     c     kTitas        Chain     Titas30c
012600040511     c                   If        Not %Found(Titas30c)
012700050131     c                   Iter
012800040511     c                   EndIf
012900050131
013000050131     c                   ExSr      Elabora
013100050131     c                   Eval      oldbolla = newbolla
013200050131     c                   Eval      oldrich = newrich
013300050131
013400050131      * Stessa bolla
013500050131   x2c                   Else
013600050131      * richiesta fatta stesso giorno e ora non elaboro
013700050131if  3c                   If        oldrich = newrich
013800050131     c                   Eval      rigtra = 'N'
013900050131      * richiesta fatta in giorno e ora diversi
014000050131   x3c                   Else
014100050131      * richiesta fatta stesso giorno
014200090216if  4c                   If        olddata = rigdata
014300050131      * devo controllare quanti minuti sono passati tra una richiesta e l'altra
014400090216     c                   Movel     oldora        whmrich
014500090217      * sommo 30 minuti al vecchio orario
014600090217     c                   Adddur    30:*minutes   whmrich
014700090216     c                   Move      whmrich       wora
014800090217      * se non sono passati 30 minuti non elaboro
014900090216if  5c                   If        rigora <= wora
015000090216     c                   Eval      rignoadd = 'N'
015100050131      * se sono passati elaboro
015200050131   x5c                   Else
015300050131     c                   ExSr      Elabora
015400050131     c                   Eval      oldbolla = newbolla
015500050131     c                   Eval      oldrich = newrich
015600050131e   5c                   EndIf
015700050131      * richiesta fatta in giorno diverso elaboro
015800050131   x4c                   Else
015900050131     c                   ExSr      Elabora
016000050131     c                   Eval      oldbolla = newbolla
016100050131     c                   Eval      oldrich = newrich
016200050131e   4c                   EndIf
016300050131e   3c                   EndIf
016400050131e   2c                   EndIf
016500050131
016600050131      * Aggiorno Firig sempre anche nel caso di bolla non generata
016700090216     c                   Eval      RigDela = woggi
016800090217     c                   Update    Fnrig000
016900050131e   1c                   EndSl
017000040511
017100040511     c                   EndDo
017200040511
017300040511     c                   Eval      *InLr = *On
017400040511
017500040511      *------------------------------------------------------------------------*
017600040511      * Elaboro la bolla letta
017700040511      *------------------------------------------------------------------------*
017800040511     c     Elabora       BegSr
017900040511
018000040915     c                   Clear                   dtas
018100040915     c                   Clear                   dtasv
018200040915     c                   Clear                   kpjbu
018300041119     c                   Clear                   wnotas
018400040915
018500040511     c                   Eval      wokbolla = *Off
018600040916
018700040917      * ricerco i figli del codice unificante se POD
018800090216     c                   If        %subst(RigPru:1:5) <> 'TILAC'
018900040916     c                   Clear                   Tibs10ds
019000040916     c                   Eval      d10drf = *date
019100040916     c                   Eval      d10tle = 'WW'
019200040916     c                   Eval      d10paf = 'F'
019300040916     c                   Eval      w008a = %subst(RigPru:1:8)
019400040916     c                   Move      w008a         w0070
019500040916     c                   Move      w008a         d10cod
019600040916     c                   Call      'TIBS10R'
019700040916     c                   Parm                    Tibs10ds
019800040916     c                   EndIf
019900040511
020000040511      * -- > T I T A S 0 0 0
020100040511      *---------------------
020200040511     c                   Clear                   Titas0
020300040511
020400040511      * Anno
020500090216     c                   Movel     RigData       TasAas
020600040511      * Linea di partenza
020700090216     c                   Eval      TasLnp = f_TasLnp
020800040511      * Codice bolla
020900090216     c                   Eval      TasCbo = 'FI'
021000040511      * Tipo bolla
021100040511     c                   Clear                   ds3a
021200040511     c                   Eval      TblCod = '3A'
021300040511     c                   Movel(p)  TasCbo        TblKey
021400040511     c     kTab          Chain     Tabel00f
021500040511     c                   If        Not %Found(Tabel00f) or TblFlg <> *Blanks
021600040511     c                   GoTo      endelab
021700040511     c                   EndIF
021800040511     c                   Eval      ds3a = TblUni
021900040511     c                   Eval      TasTbl = §3atb1
022000040511      * Mese/giorno
022100090216     c                   Move      RigData       TasMgs
022200090216      * Cliente tassazione + codice tariffa
022300050318      * se nel campo del profilo utente c'è scritto 'TILAC' il ksc va preso direttamente
022400050318      * dalla bolla
022500050318if 1ac                   If        %subst(RigPru:1:5) = 'TILAC'
022600050318     c                   Eval      TasKsc = f_TasKsc
022700050318     c                   Eval      TasCtr = f_TasCtr
022800070910      * se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
022900070910     c                   movel     tasksc        w0030
023000070910     c                   if        w0030 = 102 or w0030 = 101
023100090216     c                   eval      rignoadd = 'X'
023200070910     c                   leavesr
023300070910     c                   endif
023400070910      *
023500040511      * Linea di arrivo
023600090216     c                   Eval      TasLna = f_TasLnp
023700040511      * Numero Colli
023800040511     c                   Eval      TasNcl = 1
023900040511      * Peso bollettato + flag
024000040511     c                   Eval      TasPkb = 1
024100040511     c                   Eval      TasFpb = 'R'
024200040511      * Peso fatturato + flag
024300040511     c                   Eval      TasPkf = 1
024400040511     c                   Eval      TasFpf = 'R'
024500040511      * Volume bollettato + flag
024600040511     c                   Eval      TasVlb = 0,001
024700040511     c                   Eval      TasFvb = 'R'
024800040511      * Volume fatturato + flag
024900040511     c                   Eval      TasVlf = 0,001
025000040511     c                   Eval      TasFvf = 'R'
025100040511      * Tipo servizio
025200040511     c                   Eval      TasTsp = 'C'
025300040511      * Destinatario
025400040511     c                   Clear                   Tibs69ds
025500040511     c                   Z-add     TasKsc        i69kac
025600040511     c                   Z-add     TasKsc        i69kin
025700040511     c                   Call      'TIBS69R'
025800040511     c                   Parm                    Tibs69ds
025900040511     c                   Parm                    ds_cnaco
026000040511     c                   Parm                    ds_cnind
026100040511     c                   Parm                    ds_cnclp
026200040511     c                   Parm                    ds_fncls
026300040511     c                   If        o69err = '1'
026400040511     c                   GoTo      endelab
026500040511     c                   EndIF
026600040511     c                   Eval      TasRsd = AcoRag
026700040511     c                   Eval      TasInd = IndVia
026800040511     c                   Eval      TasCad = IndCae
026900040511     c                   Eval      TasLod = IndCit
027000040511     c                   Eval      TasPrd = IndPrv
027100040511     c                   Eval      TasNzd = IndSta
027200040511      * Codice tassazione destinatario
027300040511     c                   Eval      TasCts = '40'
027400040511      * Flag inoltro
027500040511     c                   Eval      TasFin = 'C'
027600040511      * Cliente mittente
027700090216     c                   Movel     TasLnp        TasCcm
027800040511     c                   Move      8888          TasCcm
027900040511      * Riferimento mittente numerico
028000040511     c                   Move      RigAas        wrigaas
028100040511     c                   Move      RigLnp        wriglnp
028200040511     c                   Move      RigNrs        wrignrs
028300040511     c                   Move      RigNsp        wrignsp
028400040511     c                   Move      wnumbolla     TasRmn
028500040511      * Codice tassazione mittente
028600040511     c                   Eval      TasMct = '40'
028700040512      * Flag anteporto
028800040512     c                   Eval      TasFap = 'C'
028900040511      * Natura merce
029000040511     c                   Eval      TasNas = 'BOLLA FIRMATA'
029100040511      * P.o. bollettazione
029200090216     c                   Eval      TasFlb = TasLnp
029300040511      * Terminal Arrivo
029400040511     c                   Clear                   Fnlv55ds
029500040511     c                   Eval      d55tpt = 'A'
029600040915     c                   Eval      d55lin = TasLna
029700040511     c                   Eval      d55drf = woggi
029800040511     c                   Call      'FNLV55R'
029900040511     c                   Parm                    Fnlv55ds
030000040511     c                   If        d55err <> *Blanks
030100040511     c                   GoTo      endelab
030200040511     c                   EndIF
030300040511     c                   Eval      TasTfa = d55Tfa
030400040511      * Terminal partenza
030500040511     c                   Eval      d55tpt = 'P'
030600040915     c                   Eval      d55lin = TasLnp
030700040511     c                   Call      'FNLV55R'
030800040511     c                   Parm                    Fnlv55ds
030900040511     c                   If        d55err <> *Blanks
031000040511     c                   GoTo      endelab
031100040511     c                   EndIF
031200040511     c                   Eval      TasTfp = d55Tfp
031300040511      * Data immissione
031400040511     c                   Eval      TasDim = woggi
031500040511      * Sigla operatore
031600040511     c                   Eval      TasSop = '*'
031700040511      * Codice autotrasportatore
031800060711     c                   Movel     TasLnp        TasPdr
031900040511     c                   Move      999           TasPdr
032000040511      * Data/Ora ritiro
032100040511     c                   Eval      TasDrt = woggi
032200040518     c                   Movel     wora          TasHrt
032300040511      * Flag prestazione autotrasportatore
032400040511     c                   Eval      TasFpp = 'P'
032500040511      * Trattamento merce
032600040511     c                   Eval      TasCtm = §3actm
032700040511      * Data Borderò
032800040511     c                   Eval      TasDbr = woggi
032900040511      * Flag merce portata a magazzino
033000040511     c                   Eval      TasFdn = 'S'
033100040511      * Flag ddt si/no
033200040511     c                   Eval      TasLl1 = 'Y'
033300040511      * Nazione e cap mittente
033400040511     c                   Eval      TasNzm = IndSta
033500040511     c                   Eval      TasCam = IndCae
033600090216      * Varia se da impostare a zero
033700090216     c                   if        rigtas = 'N'
033800060419     c                   eval      tassv1 = §3asva
033900060419     c                   endif
034000040511      * Falg bartolini/sdi
034100040511     c                   Eval      TasScl = 'B'
034200040915
034300040601      * Numero spedizione (da Flnuf cod. 3)
034400040513     c                   Clear                   Trul34ds
034500040513     c                   Eval      i34Aas = TasAas
034600040513     c                   Eval      i34Lnp = TasLnp
034700040513     c                   Call      'TRUL34R'
034800040513     c                   Parm                    kpjba
034900040513     c                   Parm                    Trul34ds
035000040513     c                   If        f_o34err <> *Zeros
035100040513     c                   GoTo      endelab
035200040513     c                   EndIF
035300040513     c                   Eval      TasNsp = o34Nsp
035400040511
035500040511
035600040511      * -- > T I T A A 0 0 0
035700040511      *---------------------
035800040511      * Rcd. 'O' - mittente originale
035900040513     c                   Clear                   Titaa0
036000040511
036100040511      * Key spedizione
036200040511     c                   Eval      TaaAas = TasAas
036300040511     c                   Eval      TaaLnp = TasLnp
036400040511     c                   Eval      TaaNrs = TasNrs
036500040511     c                   Eval      TaaNsp = TasNsp
036600040511      * Tipo record
036700040511     c                   Eval      TaaTrc = 'O'
036800090217      * Ragione sociale del destinatario
036900040915     c                   Eval      TaaRsc = f_TasRsd
037000040513     c                   Write     Titaa0
037100040511
037200040511      * Rcd. 'M' - mittente
037300040513     c                   Clear                   Titaa0
037400040511
037500040511      * Key spedizione
037600040511     c                   Eval      TaaAas = TasAas
037700040511     c                   Eval      TaaLnp = TasLnp
037800040511     c                   Eval      TaaNrs = TasNrs
037900040511     c                   Eval      TaaNsp = TasNsp
038000040511      * Tipo record
038100040511     c                   Eval      TaaTrc = 'M'
038200040511      * Dati del p.o. di richiesta
038300090217     c     kAzorg        Chain     Azorg01l
038400040511     c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks
038500040513     c                   Clear                   OrgInd
038600040616     c                   Clear                   OrgCpf
038700040513     c                   Clear                   OrgLoc
038800040513     c                   Clear                   OrgPro
038900040511     c                   EndIF
039000090217     c                   Eval      TaaRsc = 'POD IMAGE'
039100040511     c                   Eval      TaaInd = OrgInd
039200040616     c                   Movel     OrgCpf        TaaCap
039300040511     c                   Eval      TaaLoc = OrgLoc
039400040511     c                   Eval      TaaPrv = OrgPro
039500040513     c                   Write     Titaa0
039600040511
039700040511
039800040511      * -- > T I T A 4 0 0 0
039900040511      *---------------------
040000040511      * Rcd. 'A' - riferimenti
040100040511     c                   Clear                   Tita4000
040200040511     c                   Clear                   dta4a
040300040511
040400040511      * Key spedizione
040500040511     c                   Eval      Ta4Aas = TasAas
040600040511     c                   Eval      Ta4Lnp = TasLnp
040700040511     c                   Eval      Ta4Nrs = TasNrs
040800040511     c                   Eval      Ta4Nsp = TasNsp
040900040511      * Tipo record
041000040511     c                   Eval      Ta4Trc = 'A'
041100040511      * Riferimento mittente alfabetico
041200040511     c                   Eval      §ta4arma = %editc(f_TasRmn:'Z')
041300040511     c                   Eval      Ta4Not = dta4a
041400040511     c                   Write     Tita4000
041500040511
041600040511     c                   Write     Titas0
041700040511
041800040511     c                   Eval      wokbolla = *On
041900040511
042000040511     c     endelab       EndSr
042100040511
042200040511      *------------------------------------------------------------------------*
042300040511      * Routine iniziale
042400040511      *------------------------------------------------------------------------*
042500040511     c     *Inzsr        BegSr
042600040511
042700040511     c     *Entry        Plist
042800040511     c                   Parm                    kpjba
042900040917     c                   eval      welab = %subst(kpjbu:1:1)
043000040511
043100040511     c     *dtaara       define    §azute        azuteds
043200040511     c     *dtaara       define    §datiute      ddatiute
043300040511     c                   in(E)     *dtaara
043400040511     c                   If        %error  or RSUT = *blanks
043500040511     c                   Clear                   Tibs34ds
043600040511     c                   Call      'TIBS34R'
043700040511     c                   Parm                    Tibs34ds
043800040511     c                   In        *dtaara
043900040511     c                   EndIf
044000040511
044100040511      * data/ora del job
044200040511     c                   Z-add     *date         woggi
044300040511     c                   Time                    wora
044400040511
044500040511     c     kTitas        klist
044600040511     c                   kfld                    RigAas
044700040511     c                   kfld                    RigLnp
044800040511     c                   kfld                    RigNrs
044900040511     c                   kfld                    RigNsp
045000040511
045100040511     c     kTab          klist
045200040511     c                   kfld                    TblKut
045300040511     c                   kfld                    TblCod
045400040511     c                   kfld                    TblKey
045500040513
045600040513     c     kTitaa        klist
045700040513     c                   kfld                    RigAas
045800040513     c                   kfld                    RigLnp
045900040513     c                   kfld                    RigNrs
046000040513     c                   kfld                    RigNsp
046100040513     c                   kfld                    wtrc
046200040513
046300040511     c                   Eval      TblKut = 1
046400040513     c                   Eval      wtrc = 'M'
046500040511
046600040511     c                   EndSr
