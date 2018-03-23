000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300040511      *------------------------------------------------------------------------*
000400040511      * Creazione bolle firmate scanner SEDE
000500040511      *------------------------------------------------------------------------*
000600040511
000700040511      * A T T E N Z I O N E:
000800040511      *---------------------
000900090423      * VALORI DI RIGCAU
001000040511      * D = DANNI
001100040511      * C = C/A OMESSI
001200040511      * F = BOLLE FIRMATE
001300040511      * S = BOLLE FIRMATE DA SCANNER
001400040511      *------------------------------------------------------------------------*
001500040511
001600090423     fFnrig01l  uf   e           k Disk
001700040511     fTitas30c  if   e           k Disk    prefix(f_)
001800040513     fTitaa30c  if   e           k Disk    prefix(f_)
001900040511     fTabel00f  if   e           k Disk
002000040511     fAzorg01l  if   e           k Disk
002100040511     fTitas00f  o    e           k Disk    rename(Titas000:Titas0)
002200040513     fTitaa00f  o    e           k Disk    rename(Titaa000:Titaa0)
002300040511     fTita400f  o    e           k Disk
002400040511
002500040511      *------------------------------------------------------------------------*
002600040511      *   S C H I E R E
002700040511      *------------------------------------------------------------------------*
002800040511
002900040511      *------------------------------------------------------------------------*
003000040511      *   V A R I A B I L I
003100040511      *------------------------------------------------------------------------*
003200040916     d kazorg          s                   like(OrgFil) inz(102)
003300040511     d wokbolla        s              1    inz(*Off)
003400040511     d wora            s              6  0
003500090423     d wrigdue         s                   like(RigDela)  inz
003600040513     d wtrc            s                   like(TaaTrc)
003700040511
003800040511      *------------------------------------------------------------------------*
003900040511      *   D S   I N T E R N E / E S T E R N E
004000040511      *------------------------------------------------------------------------*
004100040511     d wnumbolla       ds
004200040511     d  wrigaas                1      2
004300040511     d  wriglnp                3      5
004400040511     d  wrignrs                6      7
004500040511     d  wrignsp                8     14
004600040917
004700040917     d                 ds
004800040917     d aoggi                   1      4  0
004900040917     d moggi                   5      6  0
005000040917     d doggi                   7      8  0
005100040917     d woggi                   1      8  0
005200040511
005300040511     d Azuteds       e ds                  extname(Azute00f)
005400040511     d dDatiute      e ds
005500040511     d ds_cnaco      e ds                  extname(Cnaco00f)
005600040511     d ds_cnind      e ds                  extname(Cnind00f)
005700040511     d ds_cnclp      e ds                  extname(Cnclp00f)
005800040511     d ds_fncls      e ds                  extname(Fncls00f)
005900040511     d ds3a          e ds
006000040511     d dta4a         e ds
006100040511     d Fnlv55ds      e ds
006200040511     d Kpjba         e ds
006300040511     d Tibs34ds      e ds
006400040511     d Tibs69ds      e ds
006500040512     d Trul34ds      e ds
006600040512     d  f_i34tla     e                     extfld(i34tla)
006700040512     d  f_o34err     e                     extfld(o34err)
006800040512     d  f_o34msg     e                     extfld(o34msg)
006900040511
007000040511      *------------------------------------------------------------------------*
007100040511
007200090423     c     wrigdue       Setll     Fnrig01l
007300040511     c                   Do        *Hival
007400090423     c     wrigdue       Reade     Fnrig01l
007500040511
007600040511      * fine file
007700090423     c                   If        %Eof(Fnrig01l)
007800040511     c                   Leave
007900040511     c                   EndIf
008000040511
008100040511      * elaboro solo le bolle firmate da scanner
008200090423     c                   If        RigCau <> 'S'
008300040511     c                   Iter
008400040511     c                   EndIf
008500050131
008600050131      * elaboro se trovo la bolla su titas
008700050131     c     kTitas        Chain     Titas30c
008800050131     c                   If        Not %Found(Titas30c)
008900050131     c                   Iter
009000050131     c                   EndIf
009100050131
009200050131     c                   ExSr      Elabora
009300050131
009400090423      * Aggiorno Fnrig se ho generato la nuova spedizione
009500050131     c                   If        wokbolla = *On
009600090423     c                   Eval      RigDela = woggi
009700090423     c                   Update    Fnrig000
009800050131     c                   EndIf
009900040511
010000040511     c                   EndDo
010100040511
010200040511     c                   Eval      *InLr = *On
010300040511
010400040511      *------------------------------------------------------------------------*
010500040511      * Elaboro la bolla letta
010600040511      *------------------------------------------------------------------------*
010700040511     c     Elabora       BegSr
010800040915
010900040511     c                   Eval      wokbolla = *Off
011000040511
011100040511      * -- > T I T A S 0 0 0
011200040511      *---------------------
011300040511     c                   Clear                   Titas0
011400040511
011500040511      * Anno
011600090423     c                   Z-add     *Year         TasAas
011700040511      * Linea di partenza
011800090423     c                   Eval      TasLnp = Rigfil
011900040511      * Codice bolla
012000090423     c                   Eval      TasCbo = 'FF'
012100040511      * Tipo bolla
012200040511     c                   Clear                   ds3a
012300040511     c                   Eval      TblCod = '3A'
012400040511     c                   Movel(p)  TasCbo        TblKey
012500040511     c     kTab          Chain     Tabel00f
012600040511     c                   If        Not %Found(Tabel00f) or TblFlg <> *Blanks
012700040511     c                   GoTo      endelab
012800040511     c                   EndIF
012900040511     c                   Eval      ds3a = TblUni
013000040511     c                   Eval      TasTbl = §3atb1
013100040511      * Mese/giorno
013200090423     c                   Move      woggi         TasMgs
013300040511      * Cliente tassazione
013400090423     c                   Eval      TasKsc = rigksc
013500040511      * Linea di arrivo
013600090423     c                   Eval      TasLna = Rigfil
013700040511      * Numero Colli
013800090423     c                   Eval      TasNcl = rigncl
013900040511      * Peso bollettato + flag
014000040511     c                   Eval      TasPkb = 1
014100040511     c                   Eval      TasFpb = 'R'
014200040511      * Peso fatturato + flag
014300040511     c                   Eval      TasPkf = 1
014400040511     c                   Eval      TasFpf = 'R'
014500040511      * Volume bollettato + flag
014600040511     c                   Eval      TasVlb = 0,001
014700040511     c                   Eval      TasFvb = 'R'
014800040511      * Volume fatturato + flag
014900040511     c                   Eval      TasVlf = 0,001
015000040511     c                   Eval      TasFvf = 'R'
015100040511      * Codice tariffa
015200090423     c                   if        rigctr >= *zeros
015300090423     c                   move      rigctr        TasCtr
015400090423     c                   endif
015500040511      * Tipo servizio
015600040511     c                   Eval      TasTsp = 'C'
015700040511      * Destinatario
015800040511     c                   Clear                   Tibs69ds
015900040511     c                   Z-add     TasKsc        i69kac
016000040511     c                   Z-add     TasKsc        i69kin
016100040511     c                   Call      'TIBS69R'
016200040511     c                   Parm                    Tibs69ds
016300040511     c                   Parm                    ds_cnaco
016400040511     c                   Parm                    ds_cnind
016500040511     c                   Parm                    ds_cnclp
016600040511     c                   Parm                    ds_fncls
016700040511     c                   If        o69err = '1'
016800040511     c                   GoTo      endelab
016900040511     c                   EndIF
017000040511     c                   Eval      TasRsd = AcoRag
017100040511     c                   Eval      TasInd = IndVia
017200040511     c                   Eval      TasCad = IndCae
017300040511     c                   Eval      TasLod = IndCit
017400040511     c                   Eval      TasPrd = IndPrv
017500040511     c                   Eval      TasNzd = IndSta
017600040511      * Codice tassazione destinatario
017700040511     c                   Eval      TasCts = '40'
017800040511      * Flag inoltro
017900040511     c                   Eval      TasFin = 'C'
018000040511      * Cliente mittente
018100090423     c                   Movel     Rigfil        TasCcm
018200040511     c                   Move      8888          TasCcm
018300040511      * Riferimento mittente numerico
018400040511     c                   Move      RigAas        wrigaas
018500040511     c                   Move      RigLnp        wriglnp
018600040511     c                   Move      RigNrs        wrignrs
018700040511     c                   Move      RigNsp        wrignsp
018800040511     c                   Move      wnumbolla     TasRmn
018900040511      * Codice tassazione mittente
019000040511     c                   Eval      TasMct = '40'
019100040512      * Flag anteporto
019200040512     c                   Eval      TasFap = 'C'
019300040511      * Natura merce
019400160411     c************       Eval      TasNas = 'BOLLA FIRMATA'
019500040511      * P.o. bollettazione
019600090423     c                   Eval      TasFlb = Rigfil
019700040511      * Terminal Arrivo
019800040511     c                   Clear                   Fnlv55ds
019900040511     c                   Eval      d55tpt = 'A'
020000040915     c                   Eval      d55lin = TasLna
020100040511     c                   Eval      d55drf = woggi
020200040511     c                   Call      'FNLV55R'
020300040511     c                   Parm                    Fnlv55ds
020400040511     c                   If        d55err <> *Blanks
020500040511     c                   GoTo      endelab
020600040511     c                   EndIF
020700040511     c                   Eval      TasTfa = d55Tfa
020800040511      * Terminal partenza
020900040511     c                   Eval      d55tpt = 'P'
021000040915     c                   Eval      d55lin = TasLnp
021100040511     c                   Call      'FNLV55R'
021200040511     c                   Parm                    Fnlv55ds
021300040511     c                   If        d55err <> *Blanks
021400040511     c                   GoTo      endelab
021500040511     c                   EndIF
021600040511     c                   Eval      TasTfp = d55Tfp
021700040511      * Data immissione
021800040511     c                   Eval      TasDim = woggi
021900040511      * Sigla operatore
022000040511     c                   Eval      TasSop = '*'
022100040511      * Codice autotrasportatore
022200060711     c                   Movel     TasLnp        TasPdr
022300040511     c                   Move      999           TasPdr
022400040511      * Data/Ora ritiro
022500040511     c                   Eval      TasDrt = woggi
022600040518     c                   Movel     wora          TasHrt
022700040511      * Flag prestazione autotrasportatore
022800040511     c                   Eval      TasFpp = 'P'
022900040511      * Trattamento merce
023000040511     c                   Eval      TasCtm = §3actm
023100040511      * Data Borderò
023200040511     c                   Eval      TasDbr = woggi
023300040511      * Flag merce portata a magazzino
023400040511     c                   Eval      TasFdn = 'S'
023500040511      * Flag ddt si/no
023600040511     c                   Eval      TasLl1 = 'Y'
023700040511      * Nazione e cap mittente
023800040511     c                   Eval      TasNzm = IndSta
023900040511     c                   Eval      TasCam = IndCae
024000040511      * Varia se da impostare a zero
024100090423     c                   If        rigtas = 'N'
024200090423     c                   Eval      TasSv1 = §3asva
024300040511     c                   EndIf
024400040511      * Falg bartolini/sdi
024500040511     c                   Eval      TasScl = 'B'
024600040915
024700040601      * Numero spedizione (da Flnuf cod. 3)
024800040513     c                   Clear                   Trul34ds
024900040513     c                   Eval      i34Aas = TasAas
025000040513     c                   Eval      i34Lnp = TasLnp
025100040513     c                   Call      'TRUL34R'
025200040513     c                   Parm                    kpjba
025300040513     c                   Parm                    Trul34ds
025400040513     c                   If        f_o34err <> *Zeros
025500040513     c                   GoTo      endelab
025600040513     c                   EndIF
025700040513     c                   Eval      TasNsp = o34Nsp
025800040511
025900040511
026000040511      * -- > T I T A A 0 0 0
026100040511      *---------------------
026200040511      * Rcd. 'O' - mittente originale
026300040513     c                   Clear                   Titaa0
026400040511
026500040511      * Key spedizione
026600040511     c                   Eval      TaaAas = TasAas
026700040511     c                   Eval      TaaLnp = TasLnp
026800040511     c                   Eval      TaaNrs = TasNrs
026900040511     c                   Eval      TaaNsp = TasNsp
027000040511      * Tipo record
027100040511     c                   Eval      TaaTrc = 'O'
027200040511      * Ragione sociale del destinatario se addebito al mittente
027300090423     c                   If        rigadd = 'M'
027400040511     c                   Eval      TaaRsc = f_TasRsd
027500040511     c                   EndIf
027600040511      * Ragione sociale del mittente se addebito al destinatario
027700090423     c                   If        rigadd = 'D'
027800040513     c     kTitaa        Chain     Titaa30c
027900040513     c                   If        Not %Found(Titaa30c)
028000040513     c                   Clear                   f_TaaRsc
028100040513     c                   EndIF
028200040513     c                   Eval      TaaRsc = f_TaaRsc
028300040511     c                   EndIf
028400040513     c                   Write     Titaa0
028500040511
028600040511      * Rcd. 'M' - mittente
028700040513     c                   Clear                   Titaa0
028800040511
028900040511      * Key spedizione
029000040511     c                   Eval      TaaAas = TasAas
029100040511     c                   Eval      TaaLnp = TasLnp
029200040511     c                   Eval      TaaNrs = TasNrs
029300040511     c                   Eval      TaaNsp = TasNsp
029400040511      * Tipo record
029500040511     c                   Eval      TaaTrc = 'M'
029600040511      * Dati del p.o. di richiesta
029700090423     c     Rigfil        Chain     Azorg01l
029800040511     c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks
029900040513     c                   Clear                   OrgInd
030000040616     c                   Clear                   OrgCpf
030100040513     c                   Clear                   OrgLoc
030200040513     c                   Clear                   OrgPro
030300040511     c                   EndIF
030400090423     c                   Eval      TaaRsc = 'BOLLA FIRMATA - P.O.' + OrgDes
030500040511     c                   Eval      TaaInd = OrgInd
030600040616     c                   Movel     OrgCpf        TaaCap
030700040511     c                   Eval      TaaLoc = OrgLoc
030800040511     c                   Eval      TaaPrv = OrgPro
030900040513     c                   Write     Titaa0
031000040511
031100040511
031200040511      * -- > T I T A 4 0 0 0
031300040511      *---------------------
031400040511      * Rcd. 'A' - riferimenti
031500040511     c                   Clear                   Tita4000
031600040511     c                   Clear                   dta4a
031700040511
031800040511      * Key spedizione
031900040511     c                   Eval      Ta4Aas = TasAas
032000040511     c                   Eval      Ta4Lnp = TasLnp
032100040511     c                   Eval      Ta4Nrs = TasNrs
032200040511     c                   Eval      Ta4Nsp = TasNsp
032300040511      * Tipo record
032400040511     c                   Eval      Ta4Trc = 'A'
032500040511      * Riferimento mittente alfabetico
032600160122     c                   clear                   dta4a
032700040511     c                   Eval      §ta4arma = %editc(f_TasRmn:'Z')
032800160122     c                   Eval      §ta4anas = 'BOLLA FIRMATA'
032900040511     c                   Eval      Ta4Not = dta4a
033000040511     c                   Write     Tita4000
033100040511
033200040511     c                   Write     Titas0
033300040511
033400040511     c                   Eval      wokbolla = *On
033500040511
033600040511     c     endelab       EndSr
033700040511
033800040511      *------------------------------------------------------------------------*
033900040511      * Routine iniziale
034000040511      *------------------------------------------------------------------------*
034100040511     c     *Inzsr        BegSr
034200040511
034300040511     c     *Entry        Plist
034400040511     c                   Parm                    kpjba
034500040511
034600040511     c     *dtaara       define    §azute        azuteds
034700040511     c     *dtaara       define    §datiute      ddatiute
034800040511     c                   in(E)     *dtaara
034900040511     c                   If        %error  or RSUT = *blanks
035000040511     c                   Clear                   Tibs34ds
035100040511     c                   Call      'TIBS34R'
035200040511     c                   Parm                    Tibs34ds
035300040511     c                   In        *dtaara
035400040511     c                   EndIf
035500040511
035600040511      * data/ora del job
035700040511     c                   Z-add     *date         woggi
035800040511     c                   Time                    wora
035900040511
036000040511     c     kTitas        klist
036100040511     c                   kfld                    RigAas
036200040511     c                   kfld                    RigLnp
036300040511     c                   kfld                    RigNrs
036400040511     c                   kfld                    RigNsp
036500040511
036600040511     c     kTab          klist
036700040511     c                   kfld                    TblKut
036800040511     c                   kfld                    TblCod
036900040511     c                   kfld                    TblKey
037000040513
037100040513     c     kTitaa        klist
037200040513     c                   kfld                    RigAas
037300040513     c                   kfld                    RigLnp
037400040513     c                   kfld                    RigNrs
037500040513     c                   kfld                    RigNsp
037600040513     c                   kfld                    wtrc
037700040513
037800040511     c                   Eval      TblKut = 1
037900040513     c                   Eval      wtrc = 'M'
038000040511
038100040511     c                   EndSr
