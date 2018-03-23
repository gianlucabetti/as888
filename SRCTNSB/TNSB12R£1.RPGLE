000100040216     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040216
000300040216      *------------------------------------------------------------------------*
000400040216      *           Elenco spedizioni senza immagine LDV
000500040216      *------------------------------------------------------------------------*
000600040310      * NOTA BENE:*Codice cliente 0892320 => Timberland               *
000700040310      *             codice fissato nella costante "CLITIM"            *
000800040310      *------------------------------------------------------------------------*
000900040216
001000041021     fTitas26c  if   e           k Disk    UsrOpn
001100040630     fTitaa30c  if   e           k Disk    UsrOpn
001200040216     fAzorg01l  if   e           k Disk
001300040217     fFiapd01l  if   e           k Disk
001400041129     fFnlbl02l  if   e           k Disk
001500041129     fFnlbl01l  if   e           k Disk
001600040827     f                                     rename(Fnlbl000:Fnlbl01)
001700101229     fFidst01l  if   e           k Disk    UsrOpn
001800040715     fTabel00f  if   e           k Disk
001900040715     fWfldv00f  o    e           k Disk    UsrOpn
002000060814     fWfldvd0f  o    e           k Disk    UsrOpn
002100040217     fTnsb12p   o    e             Printer Oflind(*In99)
002200040218     fQsysprt   o    f  132        Printer Oflind(*InOf)
002300040216
002400040216      *------------------------------------------------------------------------*
002500040216      *   C A M P I   D I   L A V O R O
002600040216      *------------------------------------------------------------------------*
002700040310     d CliTIM          s              7  0 inz(0892320)
002800040217     d CodLna          s                   Like(TasLna)
002900040715     d Codut           s              1  0 inz(1)
003000040630     d comman          s             80
003100040518     d conta           s              2  0
003200040217     d kApdTip         s                   Like(ApdTip) Inz('A')
003300040518     d kLblAa          s                   Like(LblAap)
003400040518     d kLblLp          s                   Like(LblLpp)
003500040518     d kLblNr          s                   Like(LblNrp)
003600040518     d kLblNs          s                   Like(LblNsp)
003700040217     d kTaaTrc         s                   Like(TaaTrc) Inz('M')
003800040630     d lenght          s             15  5 inz(80)
003900040218     d OldLna          s                   Like(TasLna)
004000040217     d w0040           s              4  0
004100040217     d w0140           s             14  0
004200040218     d wdata           s              8  0
004300040218     d wdataiso        s               d   datfmt(*iso)
004400040630     d wdsd            s              8  0
004500040630     d wdse            s              8  0
004600040630     d wlib            s             10
004700101229     d wlibfil         s             10
004800040217     d wokstp          s              1    Inz(*Off)
004900040519     d wpoccm          s                   Like(TasLnp)
005000040311     d xx              s              3  0 Inz
005100040216     d yy              s              3  0 Inz
005200040311     d jj              s              3  0 Inz
005300040519     d wwp             s              3  0 Inz
005400040519     d wwd             s              3  0 Inz
005500060814     d w0030           s              3  0 Inz
005600040216
005700040216      *------------------------------------------------------------------------*
005800040216      *   S C H I E R E
005900040216      *------------------------------------------------------------------------*
006000060814     d cmd             s             60    dim(06) ctdata perrcd(1)
006100040218     d SkDcm           s              8  0 dim(31)
006200040216     d SkLna           s              3  0 Dim(250)
006300040519     d SkLnpp          s              3  0 Dim(250)
006400040519     d SkLnpd          s              3  0 Dim(250)
006500040310     d SkEEX           s              3  0 Dim(250)
006600040217     d wSkLna          s              3  0 Dim(250)
006700041118     d skcbo           s              2    Dim(999)
006800040216
006900040216      *------------------------------------------------------------------------*
007000040216      *   D S   I N T E R N E / E S T E R N E
007100040216      *------------------------------------------------------------------------*
007200040218     d                 ds
007300040218     d  TasDsp                 1      8  0
007400040218     d  TasAas                 1      4  0
007500040218     d  TasMgs                 5      8  0
007600040218
007700040216     d wlbdat          ds                  Inz
007800040216     d  g02dat                 1      8  0
007900040216     d  g02inv                 9     16  0
008000040216     d  g02err                17     17
008100040216     d  g02tgi                18     22  0
008200040216
008300040216     d Param           ds
008400040216     d  PaLna                  1      3  0
008500040216     d  PaDcd                  4     11  0
008600040216     d  PaDca                 12     19  0
008700040310     d  PaEEX                 20     20
008800040630     d  PaDPD                 21     21
008900040715     d  PaFILE                22     22
009000060814     d  PaFILEd               23     23
009100040216
009200040216     d Trul06ds      e ds
009300040216     d  Lin                    1     90  0 Dim(30)
009400040216
009500040216     d Og143         e ds
009600040219     d Og146         e ds
009700110113     d Og148         e ds
009800040216     d Dtasflo       e ds
009900041118     d ds3a          e ds
010000040217
010100040217     d Tibs69ds      e ds
010200040217     d DS_cnaco      e ds                  Extname(CNACO00F)
010300040217     d DS_cnind      e ds                  Extname(CNIND00F)
010400040217     d DS_cnclp      e ds                  Extname(CNCLP00F)
010500040217     d DS_fncls      e ds                  Extname(FNCLS00F)
010600040727
010700040727     d Tibs02ds      e ds
010800040727     d dVpoPpt       e ds
010900101229
011000101229     d ddstflr       e ds
011100040216
011200040216     d Kpjba         e ds
011300040216
011400040216      *------------------------------------------------------------------------*
011500040216
011600040218      * Ciclo per lna
011700040218do  1c                   Do        250           xx
011800040218     c                   If        SkLna(xx) = *Zeros
011900040218     c                   Leave
012000040218     c                   EndIf
012100040630      * Recupero la data inizio scannerizzazione
012200110113      * e se filiale attiva con PDA consegne
012300040630     c                   Clear                   Og146
012400110113     c                   Clear                   Og148
012500040630     c     SkLna(xx)     Chain     Azorg01l
012600040630     c                   If        %Found(Azorg01l)
012700040630     c                   Movel     OrgDe6        Og146
012800110113     c                   Movel     OrgDe8        Og148
012900040630     c                   EndIf
013000040630     c                   Move      §OgDse        wdse
013100040630     c                   Move      §OgDsd        wdsd
013200040630      * Se non ha ancora iniziato a scannerizzazione leggo un'altra lna
013300040630     c                   If        wdse = *Zeros and PaEEX = *Blanks
013400040630     c                                           and PaDPD = *Blanks
013500040630     c                   Iter
013600040630     c                   EndIf
013700040715      * se richiesta la generazione del file
013800040715if  2c                   If        PaFILE = 'S'
013900040715      * pulisco il rcd
014000040715     c                   Clear                   Wfldv000
014100040715      * recupero la descrizione della lna e la lna
014200040715     c                   Eval      LdvdLna = OrgDes
014300040715     c                   Eval      LdvLna = Sklna(xx)
014400040715      * recupero il distretto e l'area
014500040715     c                   Eval      LdvDis = OrgFl3
014600040715     c                   Eval      LdvAre = OrgCar
014700040715      * e le relative descrizioni
014800040715      * distretto
014900040715     c                   Eval      TblCod = '17'
015000040715     c                   Eval      TblKey = LdvDis
015100040715     c     ktab          Chain     Tabel00f
015200040715     c                   If        %Found(Tabel00f)
015300040715     c                   Movel     TblUni        LdvdDis
015400040715     c                   EndIf
015500040715      * area
015600040715     c                   Eval      TblCod = '05'
015700040715     c                   Movel     LdvAre        TblKey
015800040715     c     ktab          Chain     Tabel00f
015900040715     c                   If        %Found(Tabel00f)
016000040715     c                   Movel     TblUni        LdvdAre
016100040715     c                   EndIf
016200040715e   2c                   EndIf
016300040218      * Ciclo per dcm
016400040218do  2c                   Do        31            yy
016500040218     c                   If        SkDcm(yy) = *Zeros
016600040218     c                   Leave
016700040218     c                   EndIf
016800040630      * Se dcm è minore dell'inizio scannerizzazione leggo un'altra data
016900040630     c                   If        SkDcm(yy) < wdse and PaEEX = *Blanks
017000040630     c                                              and PaDPD = *Blanks
017100040630     c                   Iter
017200040630     c                   EndIf
017300040715      * se richiesta la generazione del file
017400040715if  3c                   If        PaFILE = 'S'
017500040715      * pulisco i totali
017600040715     c                   Clear                   LdvTote
017700040715     c                   Clear                   LdvTotn
017800040715      * recupero la data consegna
017900040715     c                   Eval      LdvDcm = SkDcm(yy)
018000040715e   3c                   EndIF
018100040217      * Leggo TITAS
018200041021     c     kTas          Setll     Titas26c
018300040218do  3c                   Do        *Hival
018400041021     c     kTas          Reade     Titas26c
018500041021if  4c                   If        %Eof(Titas26c)
018600040216     c                   Leave
018700040218    4c                   EndIf
018800040218
018900040218     c                   Movel     TasFlo        dtasflo
019000040218     c                   Eval      *In11 = (§floiml <> *Blanks)
019100040310
019200040310      * RICHIESTA: lnp solo EEX (+ cliente TIMBERLAND)
019300040310    4C                   IF        paEEX = 'S'
019400040310     C                   eval      *in30 = (TASKSC <> CLITIM)
019500040311     c   30TasLnp        Lookup    SkEEX                                  29
019600040311     C   30              eval      *in30 = (*in29 = *OFF)
019700040310     c   30              Iter
019800040310    4c                   EndIf
019900040630
020000040630      * RICHIESTA: lnp solo DPD
020100040630    4c                   If        paDPD = 'S'
020200040630     c     TasLnp        Lookup    SkLnpd                                 30
020300040630     c  n30              Movel     TasCcm        wpoccm
020400040630     c  n30wpoccm        Lookup    SkLnpd                                 30
020500040630     c                   If        not *In30
020600040630     c                   Iter
020700040630     c                   EndIf
020800040630   x4c                   Else
020900040630      * Linea di partenza DPD o p.o. del cliente mittente DPD
021000040630      * escludo se data scannerizzazione a 0 o se dcm minore
021100040630     c     TasLnp        Lookup    SkLnpd                                 30
021200040630     c  n30              Movel     TasCcm        wpoccm
021300040630     c  n30wpoccm        Lookup    SkLnpd                                 30
021400040630     c                   If        *In30 and (wdsd = *Zeros or
021500040630     c                                        SkDcm(yy) < wdsd)
021600040630     c                   Iter
021700040630     c                   EndIf
021800040630    4c                   EndIf
021900040218
022000040519      * Linea di partenza poste escludo
022100040727      * se data spedizione inferiore alla data impostata in tabella VPO
022200040519     c     TasLnp        Lookup    SkLnpp                                 30
022300040729     c                   If        *In30 and TasDsp < §VpoPtl
022400040727     c                   Iter
022500040727     c                   EndIf
022600040310
022700040217      * Tipo bolla
022800041118if  4c**!!!              If        TasTbl = 'A3' or TasTbl = 'F3'
022900041118     c     TasCbo        Lookup    skcbo                                  30
023000041118     c                   If        *In30
023100040218     c   11
023200040218     can 10              ExSr      StampaEdp
023300040217     c                   Iter
023400040218    4c                   EndIf
023500060405      * Codice bolla 'M' prepagati annullati
023600060405if  4c                   if        tascbo = 'M '
023700060405     c   11
023800060405     can 10              ExSr      StampaEdp
023900060405     c                   Iter
024000060405    4c                   EndIf
024100041227      * Anomalia fino al 10/01/05
024200041227     c                   If        TasDsp < 20050110 and
024300041227if  4c                             (TasCca = '1' or TasCca = '2' or
024400041227     c                             TasCca = '3' or TasCca = '5' or
024500041227     c                             TasCca = '6' or TasCca = '7' or
024600041227     c                             TasCca = '9' or TasCca = 'S' or
024700041227     c                             TasCca = 'D' or TasCca = 'E' or
024800041227     c                             TasCca = 'F' or TasCca = 'G' or
024900041227     c                             TasCca = 'H' or TasCca = 'L' or
025000041227     c                             TasCca = 'R')
025100041227     c   11
025200041227     can 10              ExSr      StampaEdp
025300041227     c                   Iter
025400041227    4c                   EndIf
025500041227      * Anomalia dal 10/01/05
025600041227     c                   If        TasDsp >= 20050110 and
025700041227if  4c                             (TasCca = '1' or TasCca = '2' or
025800041221     c                             TasCca = '7' or TasCca = '9' or
025900040820     c                             TasCca = 'D' or TasCca = 'E' or
026000040820     c                             TasCca = 'F' or TasCca = 'G' or
026100040820     c                             TasCca = 'H' or TasCca = 'L' or
026200080623     c                             TasCca = 'R')
026300040218     c   11
026400040218     can 10              ExSr      StampaEdp
026500040217     c                   Iter
026600040218    4c                   EndIf
026700040513      * Bolla mamma
026800040518     c                   Eval      kLblAa = TasAas
026900040518     c                   Eval      kLblLp = TasLnp
027000040518     c                   Eval      kLblNr = TasNrs
027100040518     c                   Eval      kLblNs = TasNsp
027200040518     c     kLbl          Setll     Fnlbl02l
027300040513     c                   If        %Equal(Fnlbl02l)
027400040513     c                   Iter
027500040513     c                   EndIf
027600040518      * Controllo se è una figlia
027700040518     c                   Clear                   conta
027800040518     c     kLbl          Chain     Fnlbl01l
027900040518     c                   If        %Found(Fnlbl01l)
028000040518      * Controllo se la mamma ha generato una figlia successiva
028100040518     c     kLblmf        Setll     Fnlbl02l
028200040518     c                   Do        *Hival
028300040518     c     kLblm         Reade     Fnlbl02l
028400040518     c                   If        %Eof(Fnlbl02l)
028500040518     c                   Leave
028600040518     c                   EndIf
028700040518     c                   Add       1             conta
028800040518     c                   EndDo
028900040518      * se ho contato almeno 2 bolle con la stessa mamma vuol dire che la mamma ha generato una
029000040518      * figlia successiva alla figlia che sto leggendo
029100040518     c                   If        conta > 1
029200040518     c                   Iter
029300040518     c                   EndIf
029400040518     c                   EndIf
029500040518
029600040218      * Autotrasportatore
029700040218     c                   Move      TasPdc        w0040
029800040218if  4c                   If        w0040 = 9876 or TasPdc = 0010103
029900040218     c                   Iter
030000040218    4c                   EndIf
030100040715      * se richiesta la generazione del file
030200040715if  4c                   If        PaFILE = 'S'
030300040715      * sommo le bolle elaborate
030400040715     c                   add       1             LdvTote
030500040715e   4c                   EndIf
030600040217      * Flag immagine
030700040218     c   11              Iter
030800040226
030900040226      * Conto quante bolle
031000040226     c                   Add       1             TotDcm
031100040715      * se richiesta la generazione del file
031200040715if  4c                   If        PaFILE = 'S'
031300040715      * sommo le bolle non scannerizzate
031400040715     c                   add       1             LdvTotn
031500040715e   4c                   EndIf
031600040218
031700060814      * se richiesta la generazione del file dettaglio
031800060814      * scrivo Wfldvd0f
031900060814if  4c                   If        PaFILEd = 'S'
032000060814     c                   ExSr      file_det
032100060814e   4c                   EndIf
032200060814
032300040217      * Stampo
032400040217     c                   ExSr      Stampa
032500040217
032600040218    3c                   EndDo
032700040226
032800040226      * Salto pagina x overflow
032900040226if  3c                   If        TotDcm > *Zeros
033000040226if  4c                   If        *In99
033100040226     c                   Write     Sb12T
033200040226     c                   Write     Sb12T1
033300040226     c                   Write     Sb12T2
033400040226     c                   Eval      *In99 = *Off
033500040226    4c                   EndIf
033600040226      * Stampo il totale per data consegna merce
033700040226     c                   Write     Sb12TDcm
033800040226      * Totale linea arrivo
033900040226     c                   Add       TotDcm        TotLna
034000040226     c                   Clear                   TotDcm
034100040226    3c                   EndIf
034200040715      * se richiesta la generazione del file
034300040715if  4c                   If        PaFILE = 'S'
034400040715      * lo scrivo se ho elaborato qualcosa
034500040715     c                   If        LdvTote > 0
034600040715     c                   Write     Wfldv000
034700040715     c                   EndIf
034800040715e   4c                   EndIf
034900040218
035000040218    2c                   EndDo
035100040226
035200040226      * Salto pagina x overflow
035300040226if  2c                   If        TotLna > *Zeros
035400040226if  3c                   If        *In99
035500040226     c                   Write     Sb12T
035600040226     c                   Write     Sb12T1
035700040226     c                   Write     Sb12T2
035800040226     c                   Eval      *In99 = *Off
035900040226    3c                   EndIf
036000040226      * Stampo il totale per linea di arrivo
036100040226     c                   Write     Sb12TLna
036200040226      * Totale finale
036300040226     c                   Add       TotLna        TotFin
036400040226     c                   Clear                   TotLna
036500040226    2c                   EndIf
036600040217
036700040218    1c                   EndDo
036800040226
036900040226      * Salto pagina x overflow
037000040226if  1c                   If        *In99
037100040226     c                   Write     Sb12T
037200040226     c                   Write     Sb12T1
037300040226     c                   Write     Sb12T2
037400040226     c                   Eval      *In99 = *Off
037500040226    1c                   EndIf
037600040226
037700040226      * Stampo il totale finale
037800040226     c                   If        TotFin > *Zeros
037900040226     c                   Write     Sb12TFin
038000040226     c                   EndIf
038100040218
038200040217      * Fine stampa
038300040217     c                   If        wokstp = *On
038400040217     c                   Write     Sb12e
038500040217     c                   EndIf
038600090720
038700090720      * Stampa per nessuna LDV da scannerizzare
038800090720     c                   If        wokstp = *off
038900090720     c                   Write     Sb12T
039000090720      * Decodifico la lna
039100090720     c                   If        PaLna > *Zeros
039200090720     c                   Eval      CodLna = PaLna
039300090720     c                   Else
039400090720     c                   Eval      CodLna = SkLna(01)
039500090720     c                   EndIf
039600090720     c     CodLna        Chain     Azorg01l
039700090720     c                   If        %Found(Azorg01l)
039800090720     c                   Movel     OrgDes        DesLna
039900090720     c                   EndIf
040000090720     c                   Write     Sb12T1
040100090720     c                   Write     Sb12Fine
040200090720     c                   endif
040300040216
040400040216     c                   Eval      *InLr = *On
040500040216
040600040217      *------------------------------------------------------------------------*
040700060814      * SCRIVO FILE DETTAGLIO
040800040217      *------------------------------------------------------------------------*
040900060814     c     File_det      BegSr
041000040217
041100060814     c                   z-add     TASaas        dLDVaas
041200060814     c                   z-add     TASlnp        dLDVlnp
041300060814     c                   z-add     TASnrs        dLDVnrs
041400060814     c                   z-add     TASnsp        dLDVnsp
041500060814     c                   z-add     TASlna        dLDVlna
041600060814     c                   z-add     TASdcm        dLDVdcm
041700060814
041800060814      * Verifica se p.o. mittente è DPD
041900060814     c                   movel     TASccm        W0030
042000060814     c     W0030         lookup    SkLnpd                                 21
042100060814     c                   If        *in21 = *on
042200060814     c                   movel     'S'           dLDVdpd
042300060814     c                   Else
042400060814     c                   movel     *blank        dLDVdpd
042500060814     c                   Endif
042600060814      *
042700060814     C                   write     WFldvd00
042800060814      *
042900060814     c                   EndSr
043000060814
043100060814      *------------------------------------------------------------------------*
043200060814      * STAMPA
043300060814      *------------------------------------------------------------------------*
043400060814     c     Stampa        BegSr
043500060814
043600040217      * Stampo la testata per la prima volta
043700040217if  1c                   If        wokstp = *Off
043800040217     c                   Write     Sb12T
043900040217      * Decodifico la lna richiesta
044000040310if  2c                   If        PaLna > *Zeros
044100040217     c                   Eval      CodLna = PaLna
044200040217   x2c                   Else
044300040217      * Decodifico la prima lna
044400040218     c                   Eval      CodLna = SkLna(xx)
044500040217    2c                   EndIf
044600040217     c     CodLna        Chain     Azorg01l
044700040217if  2c                   If        %Found(Azorg01l)
044800040217     c                   Movel     OrgDes        DesLna
044900040217    2c                   EndIf
045000040217     c                   Write     Sb12T1
045100040217     c                   Eval      wokstp = *On
045200040217     c                   Write     Sb12T2
045300040218     c                   Eval      OldLna = CodLna
045400040217    1c                   EndIf
045500040218      * Se richieste tutte le linee devo saltare pagina a cambio lna
045600040310if  1c                   If        (PaLna = *Zeros) and
045700040218     c                              OldLna > *Zeros and SkLna(xx) <> OldLna
045800040218     c                   Write     Sb12T
045900040218     c                   Eval      CodLna = SkLna(xx)
046000040218     c     CodLna        Chain     Azorg01l
046100040218if  2c                   If        %Found(Azorg01l)
046200040218     c                   Movel     OrgDes        DesLna
046300040218    2c                   EndIf
046400040218     c                   Write     Sb12T1
046500040218     c                   Write     Sb12T2
046600040218     c                   Eval      OldLna = SkLna(xx)
046700040226     c                   Eval      *In99 = *Off
046800040218    1c                   EndIf
046900040217
047000040217      * Salto pagina x overflow
047100040217if  1c                   If        *In99
047200040217     c                   Write     Sb12T
047300040217     c                   Write     Sb12T1
047400040217     c                   Write     Sb12T2
047500040217     c                   Eval      *In99 = *Off
047600040217    1c                   EndIf
047700040218
047800040218      * Giro data spedizione
047900040218     c                   Clear                   wlbdat
048000040218     c                   Z-add     TasDsp        g02inv
048100040218     c                   Movel     '3'           g02err
048200040218     c                   Call      'XSRDA8'
048300040218     c                   Parm                    wlbdat
048400040218     c                   Z-add     g02dat        DatDsp
048500040217
048600040217      * Mittente
048700040217     c                   Move      TasCcm        w0040
048800040217      * non codificato vado su Titaa
048900040217if  1c                   If        w0040 = *Zeros or w0040 = 8888
049000040217     c     kTaa          Chain     Titaa30c
049100040217if  2c                   If        %Found(Titaa30c)
049200040217     c                   Movel     TaaRsc        RscMit
049300040217    2c                   EndIF
049400040217      * codificato vado su Cnaco
049500040217   x1c                   Else
049600040217     c                   Clear                   Tibs69ds
049700040217     c                   Z-add     TasCcm        I69kac
049800040217     c                   call      'TIBS69R'
049900040217     c                   parm                    tibs69DS
050000040217     c                   parm                    ds_cnaco
050100040217     c                   parm                    ds_cnind
050200040217     c                   parm                    ds_cnclp
050300040217     c                   parm                    ds_fncls
050400040217if  2c                   If        O69err <> '1'
050500040217     c                   Movel     AcoRag        RscMit
050600040217    2c                   EndIf
050700040217    1c                   EndIf
050800040217      * Giro data consegna merce
050900040217     c                   Clear                   wlbdat
051000040217     c                   Z-add     TasDcm        g02inv
051100040217     c                   Movel     '3'           g02err
051200040217     c                   Call      'XSRDA8'
051300040217     c                   Parm                    wlbdat
051400040217     c                   Z-add     g02dat        DatDcm
051500040217      * Giro data distinta
051600040217     c                   Clear                   wlbdat
051700040217     c                   Z-add     TasDdc        g02inv
051800040217     c                   Movel     '3'           g02err
051900040217     c                   Call      'XSRDA8'
052000040217     c                   Parm                    wlbdat
052100040217     c                   Z-add     g02dat        DatDdc
052200101229      * Controllo se distinta a PDA in BUONA
052300110113      * su TITAS campo lungo 7 su FIDST campo lungo 6
052400110113      * se valore supera 999999 non provo a cercare la distinta
052500110113     c                   IF        §ogpdacon <> *blanks
052600110113     c                             and  tasndc <= 999999
052700101229     c                   clear                   ddstflr
052800101229     c                   clear                   flgpda
052900101229     c                   eval       dstnfv = tasndc
053000101229     c                   eval       dstfgs =
053100101229     c                              %dec(%subst(%editc(taspdc:'X'):1:3):3:0)
053200101229     c                   eval       dstnpg = 4
053300101229     c     kfidst        chain     fidst01l
053400101229    5c                   if        %found(fidst01l)
053500101229     c                   eval      ddstflr = dstflr
053600101229      * attiva su PDA e non in test
053700101229    6c                   if        (dstpda = 'C' or dstpda = 'E') and
053800101229      * in TEST
053900101229     c                              §dsttstpda <> 'C' and §dsttstpda <> 'E'
054000101229     c                   eval      flgpda = 'P'
054100101229     c                   endif
054200101229     c                   endif
054300110113     c                   ENDIF
054400040217      * Decodifico autotrasportatore
054500040217     c     kApd          Chain     Fiapd01l
054600040217if  1c                   If        %Found(Fiapd01l)
054700040217     c                   Movel     ApdRsc        DesPdc
054800040217    1c                   EndIf
054900040217
055000040217      * Dettaglio
055100040217     c                   Write     Sb12R
055200040217
055300040217     c                   EndSr
055400040218
055500040218      *------------------------------------------------------------------------*
055600040218      * STAMPA x EDP
055700040218      *------------------------------------------------------------------------*
055800040218     c     StampaEdp     BegSr
055900040218
056000040218     c                   If        *In98 = *Off  or *InOf
056100040218     c                   Except    Testa
056200040218     c  n98              Eval      *In98 = *On
056300040218     c                   EndIf
056400040218
056500040218     c                   Except    Bolla
056600040218
056700040218     c                   EndSr
056800040216
056900040216      *------------------------------------------------------------------------*
057000040216      * ROUTINE INIZIALE
057100040216      *------------------------------------------------------------------------*
057200040216     c     *Inzsr        BegSr
057300040216
057400040216     c     *entry        plist
057500040216     c                   parm                    Kpjba
057600040216     c                   Movel(p)  Kpjbu         Param
057700040702      * Imposto libreria - Se sono in ambiente di prova - GRPS
057800040702     c                   If        %subst(knsif:7:1) = 'P'
057900040702     c                   Eval      wlib = 'GAITRAGRPS'
058000101229     c                   eval      wlibfil = 'FILTRAPRD '
058100040702      *                  - Se sono in ambiente buono - GRU
058200040630     c                   Else
058300040702     c                   Eval      wlib = 'GAITRAGRU '
058400101229     c                   eval      wlibfil = 'FILTRA201 '
058500040630     c                   EndIf
058600040630      * OVRDBF su TITAS e TITAA
058700040630     c                   Clear                   comman
058800040630     c                   Movel(p)  cmd(1)        comman
058900040630     c                   Eval      %Subst(comman:30:10) = wlib
059000040630     c                   Eval      comman =
059100041021     c                             %trim(comman) + '/TITAS26C)'
059200040630     c                   Call      'QCMDEXC'                            99
059300040630     c                   Parm                    comman
059400040630     c                   Parm                    lenght
059500041021     c  n99              Open      Titas26c
059600040630     c                   Movel(p)  cmd(2)        comman
059700040630     c                   Eval      %Subst(comman:30:10) = wlib
059800040630     c                   Eval      comman =
059900040630     c                             %trim(comman) + '/TITAA30C)'
060000040630     c                   Call      'QCMDEXC'                            99
060100040630     c                   Parm                    comman
060200040630     c                   Parm                    lenght
060300040630     c  n99              Open      Titaa30c
060400101229
060500101229      * OVRDBF su FIDST
060600101229     c                   Clear                   comman
060700101229     c                   Movel(p)  cmd(4)        comman
060800101229     c                   Eval      %Subst(comman:30:10) = wlibfil
060900101229     c                   Eval      comman =
061000110113     c                             %trim(comman) + '/FIDST01L)'
061100101229     c                   Call      'QCMDEXC'                            99
061200101229     c                   Parm                    comman
061300101229     c                   Parm                    lenght
061400101229     c  n99              Open      FIDST01L
061500040715
061600040715      * Se richiesta la generazione del file di totali
061700040715     c                   If        PaFILE = 'S'
061800040715      * prima lo pulisco
061900040715     c                   Clear                   comman
062000040715     c                   Movel(p)  cmd(3)        comman
062100040715     c                   Call      'QCMDEXC'
062200040715     c                   Parm                    comman
062300040715     c                   Parm                    lenght
062400040715      * poi lo apro
062500040715     c                   Open      Wfldv00f
062600040715     c                   EndIf
062700040218
062800060814      * Se richiesta la generazione del file di dettaglio
062900060814     c                   If        PaFILEd = 'S'
063000060814      * prima lo pulisco
063100060814     c                   Clear                   comman
063200060814     c                   Movel(p)  cmd(6)        comman
063300060814     c                   Call      'QCMDEXC'
063400060814     c                   Parm                    comman
063500060814     c                   Parm                    lenght
063600060814      * poi lo apro
063700060814     c                   Open      Wfldvd0f
063800060814     c                   EndIf
063900060814
064000040218      * Utente EDP
064100040218     c                   If        %subst(Knmus:1:3) = 'EDP'
064200040218     c                   Eval      *In10 = *On
064300040218     c                   Else
064400040218     c                   Eval      *In10 = *Off
064500040218     c                   EndIF
064600040727
064700040727      * Recupero la data partenza ultima fase poste
064800040727     c                   Clear                   Tibs02ds
064900040727     c                   Clear                   dVpoPpt
065000040727     c                   Eval      T02Mod = 'C'
065100040727     c                   Eval      T02Sif = knsif
065200040727     c                   Eval      T02Cod = 'VPO'
065300040727     c                   Eval      T02Ke1 = 'PPT'
065400040727     c                   Call      'TIBS02R'
065500040727     c                   Parm                    kpjba
065600040727     c                   Parm                    Tibs02ds
065700040727     c                   If        T02Err = *Blanks
065800040727     c                   Eval      dVpoPpt = T02Uni
065900040727     c                   EndIf
066000040216
066100040216     c                   Clear                   Lin
066200040216     c                   Clear                   SkLna
066300040216     c                   Clear                   xx
066400040311     c                   Clear                   jj
066500040519     c                   Clear                   wwp
066600040519     c                   Clear                   wwd
066700040216     c                   Eval      yy = 1
066800040216
066900040311      * Leggo AZORG x caricare delle skiere di lavoro
067000040311      * SKLNA contiene:
067100040311      *      i p.o. validi con scannerizzazione attiva tranne DPD e PPT, nel
067200040630      *      caso che PaEEX = 'S' o PaDPD = 'S' contiene tutti i p.o. tranne DPD e PPT
067300040311      *      CARICO SKLNA SOLO SE PALNA = 0
067400040630      * SKEEX  contiene:
067500040311      *      i p.o. EEX
067600040630      * SKLNPD contiene:
067700040630      *      i p.o. DPD
067800040630      * SKLNPP contiene:
067900040630      *      i p.o. PPT
068000040311
068100040311     c     *Loval        Setll     Azorg01l
068200040311     c                   Read      Azorg01l
068300040311do  1c                   DOW       NOT %Eof(Azorg01l)
068400040311if  2c                   IF        OrgFva = *Blanks  AND
068500040311     c                             (OrgFag = 'F' or OrgFag = 'A')
068600040311     c                   Movel     OrgDe3        Og143
068700040311     c                   Movel     OrgDe6        Og146
068800040311      *
068900040311if  3c                   If        §OgNtw = 'PPT' or  §OgNtw = 'DPD'
069000040519if  4c                   If        §OgNtw = 'PPT'
069100040519     c                   Add       1             wwp
069200040519     c                   Eval      SkLnpp(wwp) = OrgFil
069300040519    4c                   Endif
069400040519if  4c                   If        §OgNtw = 'DPD'
069500040519     c                   Add       1             wwd
069600040519     c                   Eval      SkLnpd(wwd) = OrgFil
069700040519    4c                   Endif
069800040630   x3c                   Else
069900040311      *
070000040311if  4c                   If        §OgNtw = 'EEX'
070100040311     c                   Add       1             jj
070200040311     c                   Eval      SkEEX(jj) = OrgFil
070300040311    4c                   EndIF
070400040311if  4c                   If        Palna = *zeros  AND
070500040630     c                             (PaEEX = 'S'  or PaDPD = 'S' or
070600040311     c                             (§OgDse <> *Blanks and §OgDse <> *Zeros))
070700040311     c                   Add       1             xx
070800040311     c                   Eval      SkLna(xx) = OrgFil
070900040311    4c                   EndIf
071000040311    3c                   EndIf
071100040311      *
071200040311    2c                   EndIf
071300040311     c                   Read      Azorg01l
071400040311    1c                   EndDo
071500040630
071600040310      * Se la linea di arrivo è <> zero carico la sk con la lna richiesta
071700040218      * più i p.o. gestiti
071800040310if  1c                   If        PaLna > *Zeros
071900040216     c                   Eval      SkLna(1) = PaLna
072000040216      * Carico £6 della linea di arrivo richiesta
072100040216     c                   Clear                   Trul06ds
072200040216     c                   Eval      d06cod = '£6'
072300040216     c                   Movel     PaLna         d06key
072400040216     c                   Movel(p)  Trul06ds      Kpjbu
072500040216     c                   Call      'TRUL06R'
072600040216     c                   Parm                    Kpjba
072700040216     c                   Movel     Kpjbu         Trul06ds
072800040216do  2c                   Do        30            xx
072900040216if  3c                   If        Lin(xx) = *Zeros
073000040216     c                   Leave
073100040216    3c                   EndIf
073200040216if  3c                   If        Lin(xx) <> PaLna
073300040216     c                   Add       1             yy
073400040216     c                   Eval      SkLna(yy) = Lin(xx)
073500040216    3c                   EndIf
073600040216    2c                   EndDo
073700040217      * Ordino la sk
073800040217     c                   Movea     SkLna         wSkLna
073900040217     c                   Clear                   SkLna
074000040217     c                   Sorta     wSkLna
074100040217     c                   Clear                   yy
074200040217do  2c                   Do        250           xx
074300040217if  3c                   If        wSkLna(xx) > *Zeros
074400040217     c                   Add       1             yy
074500040217     c                   Eval      SkLna(yy) = wSkLna(xx)
074600040217    3c                   EndIf
074700040217    2c                   EndDo
074800040216    1c                   EndIf
074900040218
075000040218      * Carico una sk con le date da elaborare
075100040218     c                   Clear                   SkDcm
075200040218     c                   Eval      xx = 1
075300040218     c                   Eval      SkDcm(xx) = PaDcd
075400040218if  1c                   If        PaDcd <> PaDca
075500040218     c                   Eval      wdata = PaDcd
075600040218     c     *Iso          Movel     PaDcd         wdataiso
075700040218do  2c                   Dow       wdata < PaDca
075800040218     c                   Eval      xx = xx + 1
075900040218     c                   Adddur    1:*d          wdataiso
076000040218     c                   Move      wdataiso      wdata
076100040218     c                   Eval      SkDcm(xx) = wdata
076200040218    2c                   EndDo
076300040218    1c                   EndIf
076400041118
076500041118      * Carico uns sk con i tipi bolla da scartare
076600041118     c                   Clear                   skcbo
076700041118     c                   Clear                   xx
076800041118     c                   Eval      TblCod = '3A'
076900041118     c     kTab01        Setll     Tabel00f
077000041118     c                   Do        *Hival
077100041118     c     kTab01        Reade     Tabel00f
077200041118     c                   If        %Eof(Tabel00f)
077300041118     c                   Leave
077400041118     c                   EndIf
077500041118     c                   If        TblFlg <> *Blanks
077600041118     c                   Iter
077700041118     c                   EndIf
077800041118     c                   Eval      ds3a = TblUni
077900041118     c                   If        §3aTb1 = 'F3' or §3aTb1 = 'A3'
078000080707     c                             or §3atb1 = 'AR'
078100041118     c                   Eval      xx = xx + 1
078200041118     c                   Movel     TblKey        skcbo(xx)
078300041118     c                   EndIf
078400041118     c                   EndDo
078500040216
078600040216     c     kTas          Klist
078700040218     c                   Kfld                    SkDcm(yy)
078800040218     c                   Kfld                    SkLna(xx)
078900040217
079000040217     c     kTaa          Klist
079100040217     c                   Kfld                    TasAas
079200040217     c                   Kfld                    TasLnp
079300040217     c                   Kfld                    TasNrs
079400040217     c                   Kfld                    TasNsp
079500040217     c                   Kfld                    kTaaTrc
079600101229
079700101229     c     kfidst        klist
079800101229     c                   Kfld                    dstnpg
079900101229     c                   kfld                    dstnfv
080000101229     c                   kfld                    dstfgs
080100040217
080200040217     c     kApd          Klist
080300040217     c                   Kfld                    kApdTip
080400040217     c                   Kfld                    TasPdc
080500040513
080600040518     c     kLbl          Klist
080700040518     c                   Kfld                    kLblAa
080800040518     c                   Kfld                    kLblLp
080900040518     c                   Kfld                    kLblNr
081000040518     c                   Kfld                    kLblNs
081100040518
081200040518     c     kLblm         Klist
081300040518     c                   Kfld                    LblAap
081400040518     c                   Kfld                    LblLpp
081500040518     c                   Kfld                    LblNrp
081600040518     c                   Kfld                    LblNsp
081700040518
081800040518     c     kLblmf        Klist
081900040518     c                   Kfld                    LblAap
082000040518     c                   Kfld                    LblLpp
082100040518     c                   Kfld                    LblNrp
082200040518     c                   Kfld                    LblNsp
082300040518     c                   Kfld                    LblAan
082400040518     c                   Kfld                    LblLpn
082500040518     c                   Kfld                    LblNrn
082600040518     c                   Kfld                    LblNsn
082700040715
082800040715     c     ktab          klist
082900040715     c                   kfld                    codut
083000040715     c                   kfld                    tblcod
083100040715     c                   kfld                    tblkey
083200041118
083300041118     c     ktab01        klist
083400041118     c                   kfld                    codut
083500041118     c                   kfld                    tblcod
083600040216
083700040217      * reperisco data e ora
083800040217     c                   time                    w0140
083900040217      * udate in GGMMAAAA
084000040217     c                   Move      w0140         wdate
084100040217      * ora
084200040217     c                   movel     w0140         wora
084300040217      * Data consegna merce dal
084400040217     c                   Clear                   wlbdat
084500040217     c                   Z-add     PaDcd         g02inv
084600040217     c                   Movel     '3'           g02err
084700040217     c                   Call      'XSRDA8'
084800040217     c                   Parm                    wlbdat
084900040217     c                   Z-add     g02dat        DatDcd
085000040217      * Data consegna merce al
085100040217     c                   Clear                   wlbdat
085200040217     c                   Z-add     PaDca         g02inv
085300040217     c                   Movel     '3'           g02err
085400040217     c                   Call      'XSRDA8'
085500040217     c                   Parm                    wlbdat
085600040217     c                   Z-add     g02dat        DatDca
085700040216
085800040216     c                   EndSr
085900040218
086000040218     oQsysprt   e            Testa          1 02
086100040218     o                                        +  20 'ELENCO SPEDIZIONI SCARTATE'
086200040218     o                                        +   1 'MA CON IMMAGINE LDV'
086300040218     o                                        +   1 'dal'
086400040218     o                       datdcd           +   1 '  /  /    '
086500040218     o                                        +   1 'al'
086600040218     o                       datdca           +   1 '  /  /    '
086700040218     o                                          110 'TNSB12R'
086800040218     o                                          128 'PAG.'
086900040218     o                       Page          Z    132
087000040218     o          e            Testa          2
087100040218     o                       Knsif            +   1
087200040218     o                       Knmus            +   1
087300040218     o                       wdate              110 '  /  /    '
087400040218     o                       wora               132 '  :  :  '
087500040218     o          e            Testa          2
087600040218     o                                        +   4 'Spedizione'
087700040218     o                                        +   7 'Cod.bolla'
087800040218     o                                        +   2 'Anomalia'
087900040218     o                                        +   2 'Autotrasportatore'
088000040218     o          e            Bolla       1
088100040218     o                       TasLnp
088200040218     O                       TasNrs        Z  +   1
088300040218     O                       TasNsp        Z  +   1
088400040218     O                       TasAas           +   1
088500040218     O                       TasCbo           +   5
088600040218     O                       TasCca           +   9
088700040218     O                       TasPdc           +   6
088800040630** cmd
088900041021OVRDBF FILE(TITAS26C) TOFILE(
089000040630OVRDBF FILE(TITAA30C) TOFILE(
089100040715CLRPFM FILE(WFLDV00F)
089200110113OVRDBF FILE(FIDST01L) TOFILE(
089300040827OVRDBF FILE(FNLBL02L) TOFILE(
089400060814CLRPFM FILE(WFLDVD0F)
