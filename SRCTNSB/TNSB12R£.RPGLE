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
009100120913     d  PaKsc                 24     30  0
009200120913     d  PaRag                 31     60
009300040216
009400040216     d Trul06ds      e ds
009500040216     d  Lin                    1     90  0 Dim(30)
009600040216
009700040216     d Og143         e ds
009800040219     d Og146         e ds
009900110113     d Og148         e ds
010000040216     d Dtasflo       e ds
010100041118     d ds3a          e ds
010200040217
010300040217     d Tibs69ds      e ds
010400040217     d DS_cnaco      e ds                  Extname(CNACO00F)
010500040217     d DS_cnind      e ds                  Extname(CNIND00F)
010600040217     d DS_cnclp      e ds                  Extname(CNCLP00F)
010700040217     d DS_fncls      e ds                  Extname(FNCLS00F)
010800040727
010900040727     d Tibs02ds      e ds
011000040727     d dVpoPpt       e ds
011100101229
011200101229     d ddstflr       e ds
011300040216
011400040216     d Kpjba         e ds
011500040216
011600040216      *------------------------------------------------------------------------*
011700040216
011800040218      * Ciclo per lna
011900040218do  1c                   Do        250           xx
012000040218     c                   If        SkLna(xx) = *Zeros
012100040218     c                   Leave
012200040218     c                   EndIf
012300040630      * Recupero la data inizio scannerizzazione
012400110113      * e se filiale attiva con PDA consegne
012500040630     c                   Clear                   Og146
012600110113     c                   Clear                   Og148
012700040630     c     SkLna(xx)     Chain     Azorg01l
012800040630     c                   If        %Found(Azorg01l)
012900040630     c                   Movel     OrgDe6        Og146
013000110113     c                   Movel     OrgDe8        Og148
013100040630     c                   EndIf
013200040630     c                   Move      §OgDse        wdse
013300040630     c                   Move      §OgDsd        wdsd
013400040630      * Se non ha ancora iniziato a scannerizzazione leggo un'altra lna
013500040630     c                   If        wdse = *Zeros and PaEEX = *Blanks
013600040630     c                                           and PaDPD = *Blanks
013700040630     c                   Iter
013800040630     c                   EndIf
013900040715      * se richiesta la generazione del file
014000040715if  2c                   If        PaFILE = 'S'
014100040715      * pulisco il rcd
014200040715     c                   Clear                   Wfldv000
014300040715      * recupero la descrizione della lna e la lna
014400040715     c                   Eval      LdvdLna = OrgDes
014500040715     c                   Eval      LdvLna = Sklna(xx)
014600040715      * recupero il distretto e l'area
014700040715     c                   Eval      LdvDis = OrgFl3
014800040715     c                   Eval      LdvAre = OrgCar
014900040715      * e le relative descrizioni
015000040715      * distretto
015100040715     c                   Eval      TblCod = '17'
015200040715     c                   Eval      TblKey = LdvDis
015300040715     c     ktab          Chain     Tabel00f
015400040715     c                   If        %Found(Tabel00f)
015500040715     c                   Movel     TblUni        LdvdDis
015600040715     c                   EndIf
015700040715      * area
015800040715     c                   Eval      TblCod = '05'
015900040715     c                   Movel     LdvAre        TblKey
016000040715     c     ktab          Chain     Tabel00f
016100040715     c                   If        %Found(Tabel00f)
016200040715     c                   Movel     TblUni        LdvdAre
016300040715     c                   EndIf
016400040715e   2c                   EndIf
016500040218      * Ciclo per dcm
016600040218do  2c                   Do        31            yy
016700040218     c                   If        SkDcm(yy) = *Zeros
016800040218     c                   Leave
016900040218     c                   EndIf
017000040630      * Se dcm è minore dell'inizio scannerizzazione leggo un'altra data
017100040630     c                   If        SkDcm(yy) < wdse and PaEEX = *Blanks
017200040630     c                                              and PaDPD = *Blanks
017300040630     c                   Iter
017400040630     c                   EndIf
017500040715      * se richiesta la generazione del file
017600040715if  3c                   If        PaFILE = 'S'
017700040715      * pulisco i totali
017800040715     c                   Clear                   LdvTote
017900040715     c                   Clear                   LdvTotn
018000040715      * recupero la data consegna
018100040715     c                   Eval      LdvDcm = SkDcm(yy)
018200040715e   3c                   EndIF
018300040217      * Leggo TITAS
018400041021     c     kTas          Setll     Titas26c
018500040218do  3c                   Do        *Hival
018600041021     c     kTas          Reade     Titas26c
018700041021if  4c                   If        %Eof(Titas26c)
018800040216     c                   Leave
018900040218    4c                   EndIf
019000040218
019100040218     c                   Movel     TasFlo        dtasflo
019200040218     c                   Eval      *In11 = (§floiml <> *Blanks)
019300120913
019400120913      * RICHIESTA: Cliente
019500120913    4c                   IF        *in12  and  TASccm <> PaKsc
019600120913     c                   iter
019700120913     c                   ENDIF
019800040310
019900040310      * RICHIESTA: lnp solo EEX (+ cliente TIMBERLAND)
020000040310    4C                   IF        paEEX = 'S'
020100040310     C                   eval      *in30 = (TASKSC <> CLITIM)
020200040311     c   30TasLnp        Lookup    SkEEX                                  29
020300040311     C   30              eval      *in30 = (*in29 = *OFF)
020400040310     c   30              Iter
020500040310    4c                   EndIf
020600040630
020700040630      * RICHIESTA: lnp solo DPD
020800040630    4c                   If        paDPD = 'S'
020900040630     c     TasLnp        Lookup    SkLnpd                                 30
021000040630     c  n30              Movel     TasCcm        wpoccm
021100040630     c  n30wpoccm        Lookup    SkLnpd                                 30
021200040630     c                   If        not *In30
021300040630     c                   Iter
021400040630     c                   EndIf
021500040630   x4c                   Else
021600040630      * Linea di partenza DPD o p.o. del cliente mittente DPD
021700040630      * escludo se data scannerizzazione a 0 o se dcm minore
021800040630     c     TasLnp        Lookup    SkLnpd                                 30
021900040630     c  n30              Movel     TasCcm        wpoccm
022000040630     c  n30wpoccm        Lookup    SkLnpd                                 30
022100040630     c                   If        *In30 and (wdsd = *Zeros or
022200040630     c                                        SkDcm(yy) < wdsd)
022300040630     c                   Iter
022400040630     c                   EndIf
022500040630    4c                   EndIf
022600040218
022700040519      * Linea di partenza poste escludo
022800040727      * se data spedizione inferiore alla data impostata in tabella VPO
022900040519     c     TasLnp        Lookup    SkLnpp                                 30
023000040729     c                   If        *In30 and TasDsp < §VpoPtl
023100040727     c                   Iter
023200040727     c                   EndIf
023300040310
023400040217      * Tipo bolla
023500041118if  4c**!!!              If        TasTbl = 'A3' or TasTbl = 'F3'
023600041118     c     TasCbo        Lookup    skcbo                                  30
023700041118     c                   If        *In30
023800040218     c   11
023900040218     can 10              ExSr      StampaEdp
024000040217     c                   Iter
024100040218    4c                   EndIf
024200060405      * Codice bolla 'M' prepagati annullati
024300060405if  4c                   if        tascbo = 'M '
024400060405     c   11
024500060405     can 10              ExSr      StampaEdp
024600060405     c                   Iter
024700060405    4c                   EndIf
024800041227      * Anomalia fino al 10/01/05
024900041227     c                   If        TasDsp < 20050110 and
025000041227if  4c                             (TasCca = '1' or TasCca = '2' or
025100041227     c                             TasCca = '3' or TasCca = '5' or
025200041227     c                             TasCca = '6' or TasCca = '7' or
025300041227     c                             TasCca = '9' or TasCca = 'S' or
025400041227     c                             TasCca = 'D' or TasCca = 'E' or
025500041227     c                             TasCca = 'F' or TasCca = 'G' or
025600041227     c                             TasCca = 'H' or TasCca = 'L' or
025700041227     c                             TasCca = 'R')
025800041227     c   11
025900041227     can 10              ExSr      StampaEdp
026000041227     c                   Iter
026100041227    4c                   EndIf
026200041227      * Anomalia dal 10/01/05
026300041227     c                   If        TasDsp >= 20050110 and
026400041227if  4c                             (TasCca = '1' or TasCca = '2' or
026500041221     c                             TasCca = '7' or TasCca = '9' or
026600040820     c                             TasCca = 'D' or TasCca = 'E' or
026700040820     c                             TasCca = 'F' or TasCca = 'G' or
026800040820     c                             TasCca = 'H' or TasCca = 'L' or
026900130530     c****                         TasCca = 'R')
027000130530     c                             TasCca = 'R' or TAScca = 'S')
027100040218     c   11
027200040218     can 10              ExSr      StampaEdp
027300040217     c                   Iter
027400040218    4c                   EndIf
027500040513      * Bolla mamma
027600040518     c                   Eval      kLblAa = TasAas
027700040518     c                   Eval      kLblLp = TasLnp
027800040518     c                   Eval      kLblNr = TasNrs
027900040518     c                   Eval      kLblNs = TasNsp
028000040518     c     kLbl          Setll     Fnlbl02l
028100040513     c                   If        %Equal(Fnlbl02l)
028200040513     c                   Iter
028300040513     c                   EndIf
028400040518      * Controllo se è una figlia
028500040518     c                   Clear                   conta
028600040518     c     kLbl          Chain     Fnlbl01l
028700040518     c                   If        %Found(Fnlbl01l)
028800040518      * Controllo se la mamma ha generato una figlia successiva
028900040518     c     kLblmf        Setll     Fnlbl02l
029000040518     c                   Do        *Hival
029100040518     c     kLblm         Reade     Fnlbl02l
029200040518     c                   If        %Eof(Fnlbl02l)
029300040518     c                   Leave
029400040518     c                   EndIf
029500040518     c                   Add       1             conta
029600040518     c                   EndDo
029700040518      * se ho contato almeno 2 bolle con la stessa mamma vuol dire che la mamma ha generato una
029800040518      * figlia successiva alla figlia che sto leggendo
029900040518     c                   If        conta > 1
030000040518     c                   Iter
030100040518     c                   EndIf
030200040518     c                   EndIf
030300040518
030400040218      * Autotrasportatore
030500040218     c                   Move      TasPdc        w0040
030600040218if  4c                   If        w0040 = 9876 or TasPdc = 0010103
030700040218     c                   Iter
030800040218    4c                   EndIf
030900040715      * se richiesta la generazione del file
031000040715if  4c                   If        PaFILE = 'S'
031100040715      * sommo le bolle elaborate
031200040715     c                   add       1             LdvTote
031300040715e   4c                   EndIf
031400040217      * Flag immagine
031500040218     c   11              Iter
031600040226
031700040226      * Conto quante bolle
031800040226     c                   Add       1             TotDcm
031900040715      * se richiesta la generazione del file
032000040715if  4c                   If        PaFILE = 'S'
032100040715      * sommo le bolle non scannerizzate
032200040715     c                   add       1             LdvTotn
032300040715e   4c                   EndIf
032400040218
032500060814      * se richiesta la generazione del file dettaglio
032600060814      * scrivo Wfldvd0f
032700060814if  4c                   If        PaFILEd = 'S'
032800060814     c                   ExSr      file_det
032900060814e   4c                   EndIf
033000060814
033100040217      * Stampo
033200040217     c                   ExSr      Stampa
033300040217
033400040218    3c                   EndDo
033500040226
033600040226      * Salto pagina x overflow
033700040226if  3c                   If        TotDcm > *Zeros
033800040226if  4c                   If        *In99
033900040226     c                   Write     Sb12T
034000040226     c                   Write     Sb12T1
034100040226     c                   Write     Sb12T2
034200040226     c                   Eval      *In99 = *Off
034300040226    4c                   EndIf
034400040226      * Stampo il totale per data consegna merce
034500040226     c                   Write     Sb12TDcm
034600040226      * Totale linea arrivo
034700040226     c                   Add       TotDcm        TotLna
034800040226     c                   Clear                   TotDcm
034900040226    3c                   EndIf
035000040715      * se richiesta la generazione del file
035100040715if  4c                   If        PaFILE = 'S'
035200040715      * lo scrivo se ho elaborato qualcosa
035300040715     c                   If        LdvTote > 0
035400040715     c                   Write     Wfldv000
035500040715     c                   EndIf
035600040715e   4c                   EndIf
035700040218
035800040218    2c                   EndDo
035900040226
036000040226      * Salto pagina x overflow
036100040226if  2c                   If        TotLna > *Zeros
036200040226if  3c                   If        *In99
036300040226     c                   Write     Sb12T
036400040226     c                   Write     Sb12T1
036500040226     c                   Write     Sb12T2
036600040226     c                   Eval      *In99 = *Off
036700040226    3c                   EndIf
036800040226      * Stampo il totale per linea di arrivo
036900040226     c                   Write     Sb12TLna
037000040226      * Totale finale
037100040226     c                   Add       TotLna        TotFin
037200040226     c                   Clear                   TotLna
037300040226    2c                   EndIf
037400040217
037500040218    1c                   EndDo
037600040226
037700040226      * Salto pagina x overflow
037800040226if  1c                   If        *In99
037900040226     c                   Write     Sb12T
038000040226     c                   Write     Sb12T1
038100040226     c                   Write     Sb12T2
038200040226     c                   Eval      *In99 = *Off
038300040226    1c                   EndIf
038400040226
038500040226      * Stampo il totale finale
038600040226     c                   If        TotFin > *Zeros
038700040226     c                   Write     Sb12TFin
038800040226     c                   EndIf
038900040218
039000040217      * Fine stampa
039100040217     c                   If        wokstp = *On
039200040217     c                   Write     Sb12e
039300040217     c                   EndIf
039400090720
039500090720      * Stampa per nessuna LDV da scannerizzare
039600090720     c                   If        wokstp = *off
039700090720     c                   Write     Sb12T
039800090720      * Decodifico la lna
039900090720     c                   If        PaLna > *Zeros
040000090720     c                   Eval      CodLna = PaLna
040100090720     c                   Else
040200090720     c                   Eval      CodLna = SkLna(01)
040300090720     c                   EndIf
040400090720     c     CodLna        Chain     Azorg01l
040500090720     c                   If        %Found(Azorg01l)
040600090720     c                   Movel     OrgDes        DesLna
040700090720     c                   EndIf
040800090720     c                   Write     Sb12T1
040900090720     c                   Write     Sb12Fine
041000090720     c                   endif
041100040216
041200040216     c                   Eval      *InLr = *On
041300040216
041400040217      *------------------------------------------------------------------------*
041500060814      * SCRIVO FILE DETTAGLIO
041600040217      *------------------------------------------------------------------------*
041700060814     c     File_det      BegSr
041800040217
041900060814     c                   z-add     TASaas        dLDVaas
042000060814     c                   z-add     TASlnp        dLDVlnp
042100060814     c                   z-add     TASnrs        dLDVnrs
042200060814     c                   z-add     TASnsp        dLDVnsp
042300060814     c                   z-add     TASlna        dLDVlna
042400060814     c                   z-add     TASdcm        dLDVdcm
042500060814
042600060814      * Verifica se p.o. mittente è DPD
042700060814     c                   movel     TASccm        W0030
042800060814     c     W0030         lookup    SkLnpd                                 21
042900060814     c                   If        *in21 = *on
043000060814     c                   movel     'S'           dLDVdpd
043100060814     c                   Else
043200060814     c                   movel     *blank        dLDVdpd
043300060814     c                   Endif
043400060814      *
043500060814     C                   write     WFldvd00
043600060814      *
043700060814     c                   EndSr
043800060814
043900060814      *------------------------------------------------------------------------*
044000060814      * STAMPA
044100060814      *------------------------------------------------------------------------*
044200060814     c     Stampa        BegSr
044300060814
044400040217      * Stampo la testata per la prima volta
044500040217if  1c                   If        wokstp = *Off
044600040217     c                   Write     Sb12T
044700040217      * Decodifico la lna richiesta
044800040310if  2c                   If        PaLna > *Zeros
044900040217     c                   Eval      CodLna = PaLna
045000040217   x2c                   Else
045100040217      * Decodifico la prima lna
045200040218     c                   Eval      CodLna = SkLna(xx)
045300040217    2c                   EndIf
045400040217     c     CodLna        Chain     Azorg01l
045500040217if  2c                   If        %Found(Azorg01l)
045600040217     c                   Movel     OrgDes        DesLna
045700040217    2c                   EndIf
045800040217     c                   Write     Sb12T1
045900040217     c                   Eval      wokstp = *On
046000040217     c                   Write     Sb12T2
046100040218     c                   Eval      OldLna = CodLna
046200040217    1c                   EndIf
046300040218      * Se richieste tutte le linee devo saltare pagina a cambio lna
046400040310if  1c                   If        (PaLna = *Zeros) and
046500040218     c                              OldLna > *Zeros and SkLna(xx) <> OldLna
046600040218     c                   Write     Sb12T
046700040218     c                   Eval      CodLna = SkLna(xx)
046800040218     c     CodLna        Chain     Azorg01l
046900040218if  2c                   If        %Found(Azorg01l)
047000040218     c                   Movel     OrgDes        DesLna
047100040218    2c                   EndIf
047200040218     c                   Write     Sb12T1
047300040218     c                   Write     Sb12T2
047400040218     c                   Eval      OldLna = SkLna(xx)
047500040226     c                   Eval      *In99 = *Off
047600040218    1c                   EndIf
047700040217
047800040217      * Salto pagina x overflow
047900040217if  1c                   If        *In99
048000040217     c                   Write     Sb12T
048100040217     c                   Write     Sb12T1
048200040217     c                   Write     Sb12T2
048300040217     c                   Eval      *In99 = *Off
048400040217    1c                   EndIf
048500040218
048600040218      * Giro data spedizione
048700040218     c                   Clear                   wlbdat
048800040218     c                   Z-add     TasDsp        g02inv
048900040218     c                   Movel     '3'           g02err
049000040218     c                   Call      'XSRDA8'
049100040218     c                   Parm                    wlbdat
049200040218     c                   Z-add     g02dat        DatDsp
049300040217
049400040217      * Mittente
049500040217     c                   Move      TasCcm        w0040
049600040217      * non codificato vado su Titaa
049700040217if  1c                   If        w0040 = *Zeros or w0040 = 8888
049800040217     c     kTaa          Chain     Titaa30c
049900040217if  2c                   If        %Found(Titaa30c)
050000040217     c                   Movel     TaaRsc        RscMit
050100040217    2c                   EndIF
050200040217      * codificato vado su Cnaco
050300040217   x1c                   Else
050400040217     c                   Clear                   Tibs69ds
050500040217     c                   Z-add     TasCcm        I69kac
050600040217     c                   call      'TIBS69R'
050700040217     c                   parm                    tibs69DS
050800040217     c                   parm                    ds_cnaco
050900040217     c                   parm                    ds_cnind
051000040217     c                   parm                    ds_cnclp
051100040217     c                   parm                    ds_fncls
051200040217if  2c                   If        O69err <> '1'
051300040217     c                   Movel     AcoRag        RscMit
051400040217    2c                   EndIf
051500040217    1c                   EndIf
051600040217      * Giro data consegna merce
051700040217     c                   Clear                   wlbdat
051800040217     c                   Z-add     TasDcm        g02inv
051900040217     c                   Movel     '3'           g02err
052000040217     c                   Call      'XSRDA8'
052100040217     c                   Parm                    wlbdat
052200040217     c                   Z-add     g02dat        DatDcm
052300040217      * Giro data distinta
052400040217     c                   Clear                   wlbdat
052500040217     c                   Z-add     TasDdc        g02inv
052600040217     c                   Movel     '3'           g02err
052700040217     c                   Call      'XSRDA8'
052800040217     c                   Parm                    wlbdat
052900040217     c                   Z-add     g02dat        DatDdc
053000101229      * Controllo se distinta a PDA in BUONA
053100110113      * su TITAS campo lungo 7 su FIDST campo lungo 6
053200110113      * se valore supera 999999 non provo a cercare la distinta
053300110113     c                   IF        §ogpdacon <> *blanks
053400110113     c                             and  tasndc <= 999999
053500101229     c                   clear                   ddstflr
053600101229     c                   clear                   flgpda
053700101229     c                   eval       dstnfv = tasndc
053800101229     c                   eval       dstfgs =
053900101229     c                              %dec(%subst(%editc(taspdc:'X'):1:3):3:0)
054000101229     c                   eval       dstnpg = 4
054100101229     c     kfidst        chain     fidst01l
054200101229    5c                   if        %found(fidst01l)
054300101229     c                   eval      ddstflr = dstflr
054400101229      * attiva su PDA e non in test
054500101229    6c                   if        (dstpda = 'C' or dstpda = 'E') and
054600101229      * in TEST
054700101229     c                              §dsttstpda <> 'C' and §dsttstpda <> 'E'
054800101229     c                   eval      flgpda = 'P'
054900101229     c                   endif
055000101229     c                   endif
055100110113     c                   ENDIF
055200040217      * Decodifico autotrasportatore
055300040217     c     kApd          Chain     Fiapd01l
055400040217if  1c                   If        %Found(Fiapd01l)
055500040217     c                   Movel     ApdRsc        DesPdc
055600040217    1c                   EndIf
055700040217
055800040217      * Dettaglio
055900040217     c                   Write     Sb12R
056000040217
056100040217     c                   EndSr
056200040218
056300040218      *------------------------------------------------------------------------*
056400040218      * STAMPA x EDP
056500040218      *------------------------------------------------------------------------*
056600040218     c     StampaEdp     BegSr
056700040218
056800040218     c                   If        *In98 = *Off  or *InOf
056900040218     c                   Except    Testa
057000040218     c  n98              Eval      *In98 = *On
057100040218     c                   EndIf
057200040218
057300040218     c                   Except    Bolla
057400040218
057500040218     c                   EndSr
057600040216
057700040216      *------------------------------------------------------------------------*
057800040216      * ROUTINE INIZIALE
057900040216      *------------------------------------------------------------------------*
058000040216     c     *Inzsr        BegSr
058100040216
058200040216     c     *entry        plist
058300040216     c                   parm                    Kpjba
058400040216     c                   Movel(p)  Kpjbu         Param
058500040702      * Imposto libreria - Se sono in ambiente di prova - GRPS
058600040702     c                   If        %subst(knsif:7:1) = 'P'
058700040702     c                   Eval      wlib = 'GAITRAGRPS'
058800101229     c                   eval      wlibfil = 'FILTRAPRD '
058900040702      *                  - Se sono in ambiente buono - GRU
059000040630     c                   Else
059100040702     c                   Eval      wlib = 'GAITRAGRU '
059200101229     c                   eval      wlibfil = 'FILTRA201 '
059300040630     c                   EndIf
059400040630      * OVRDBF su TITAS e TITAA
059500040630     c                   Clear                   comman
059600040630     c                   Movel(p)  cmd(1)        comman
059700040630     c                   Eval      %Subst(comman:30:10) = wlib
059800040630     c                   Eval      comman =
059900041021     c                             %trim(comman) + '/TITAS26C)'
060000040630     c                   Call      'QCMDEXC'                            99
060100040630     c                   Parm                    comman
060200040630     c                   Parm                    lenght
060300041021     c  n99              Open      Titas26c
060400040630     c                   Movel(p)  cmd(2)        comman
060500040630     c                   Eval      %Subst(comman:30:10) = wlib
060600040630     c                   Eval      comman =
060700040630     c                             %trim(comman) + '/TITAA30C)'
060800040630     c                   Call      'QCMDEXC'                            99
060900040630     c                   Parm                    comman
061000040630     c                   Parm                    lenght
061100040630     c  n99              Open      Titaa30c
061200101229
061300101229      * OVRDBF su FIDST
061400101229     c                   Clear                   comman
061500101229     c                   Movel(p)  cmd(4)        comman
061600101229     c                   Eval      %Subst(comman:30:10) = wlibfil
061700101229     c                   Eval      comman =
061800110113     c                             %trim(comman) + '/FIDST01L)'
061900101229     c                   Call      'QCMDEXC'                            99
062000101229     c                   Parm                    comman
062100101229     c                   Parm                    lenght
062200101229     c  n99              Open      FIDST01L
062300040715
062400040715      * Se richiesta la generazione del file di totali
062500040715     c                   If        PaFILE = 'S'
062600040715      * prima lo pulisco
062700040715     c                   Clear                   comman
062800040715     c                   Movel(p)  cmd(3)        comman
062900040715     c                   Call      'QCMDEXC'
063000040715     c                   Parm                    comman
063100040715     c                   Parm                    lenght
063200040715      * poi lo apro
063300040715     c                   Open      Wfldv00f
063400040715     c                   EndIf
063500040218
063600060814      * Se richiesta la generazione del file di dettaglio
063700060814     c                   If        PaFILEd = 'S'
063800060814      * prima lo pulisco
063900060814     c                   Clear                   comman
064000060814     c                   Movel(p)  cmd(6)        comman
064100060814     c                   Call      'QCMDEXC'
064200060814     c                   Parm                    comman
064300060814     c                   Parm                    lenght
064400060814      * poi lo apro
064500060814     c                   Open      Wfldvd0f
064600060814     c                   EndIf
064700060814
064800040218      * Utente EDP
064900040218     c                   If        %subst(Knmus:1:3) = 'EDP'
065000040218     c                   Eval      *In10 = *On
065100040218     c                   Else
065200040218     c                   Eval      *In10 = *Off
065300040218     c                   EndIF
065400040727
065500040727      * Recupero la data partenza ultima fase poste
065600040727     c                   Clear                   Tibs02ds
065700040727     c                   Clear                   dVpoPpt
065800040727     c                   Eval      T02Mod = 'C'
065900040727     c                   Eval      T02Sif = knsif
066000040727     c                   Eval      T02Cod = 'VPO'
066100040727     c                   Eval      T02Ke1 = 'PPT'
066200040727     c                   Call      'TIBS02R'
066300040727     c                   Parm                    kpjba
066400040727     c                   Parm                    Tibs02ds
066500040727     c                   If        T02Err = *Blanks
066600040727     c                   Eval      dVpoPpt = T02Uni
066700040727     c                   EndIf
066800040216
066900040216     c                   Clear                   Lin
067000040216     c                   Clear                   SkLna
067100040216     c                   Clear                   xx
067200040311     c                   Clear                   jj
067300040519     c                   Clear                   wwp
067400040519     c                   Clear                   wwd
067500040216     c                   Eval      yy = 1
067600040216
067700040311      * Leggo AZORG x caricare delle skiere di lavoro
067800040311      * SKLNA contiene:
067900040311      *      i p.o. validi con scannerizzazione attiva tranne DPD e PPT, nel
068000040630      *      caso che PaEEX = 'S' o PaDPD = 'S' contiene tutti i p.o. tranne DPD e PPT
068100040311      *      CARICO SKLNA SOLO SE PALNA = 0
068200040630      * SKEEX  contiene:
068300040311      *      i p.o. EEX
068400040630      * SKLNPD contiene:
068500040630      *      i p.o. DPD
068600040630      * SKLNPP contiene:
068700040630      *      i p.o. PPT
068800040311
068900040311     c     *Loval        Setll     Azorg01l
069000040311     c                   Read      Azorg01l
069100040311do  1c                   DOW       NOT %Eof(Azorg01l)
069200040311if  2c                   IF        OrgFva = *Blanks  AND
069300040311     c                             (OrgFag = 'F' or OrgFag = 'A')
069400040311     c                   Movel     OrgDe3        Og143
069500040311     c                   Movel     OrgDe6        Og146
069600040311      *
069700040311if  3c                   If        §OgNtw = 'PPT' or  §OgNtw = 'DPD'
069800040519if  4c                   If        §OgNtw = 'PPT'
069900040519     c                   Add       1             wwp
070000040519     c                   Eval      SkLnpp(wwp) = OrgFil
070100040519    4c                   Endif
070200040519if  4c                   If        §OgNtw = 'DPD'
070300040519     c                   Add       1             wwd
070400040519     c                   Eval      SkLnpd(wwd) = OrgFil
070500040519    4c                   Endif
070600040630   x3c                   Else
070700040311      *
070800040311if  4c                   If        §OgNtw = 'EEX'
070900040311     c                   Add       1             jj
071000040311     c                   Eval      SkEEX(jj) = OrgFil
071100040311    4c                   EndIF
071200040311if  4c                   If        Palna = *zeros  AND
071300040630     c                             (PaEEX = 'S'  or PaDPD = 'S' or
071400040311     c                             (§OgDse <> *Blanks and §OgDse <> *Zeros))
071500040311     c                   Add       1             xx
071600040311     c                   Eval      SkLna(xx) = OrgFil
071700040311    4c                   EndIf
071800040311    3c                   EndIf
071900040311      *
072000040311    2c                   EndIf
072100040311     c                   Read      Azorg01l
072200040311    1c                   EndDo
072300040630
072400040310      * Se la linea di arrivo è <> zero carico la sk con la lna richiesta
072500040218      * più i p.o. gestiti
072600040310if  1c                   If        PaLna > *Zeros
072700040216     c                   Eval      SkLna(1) = PaLna
072800040216      * Carico £6 della linea di arrivo richiesta
072900040216     c                   Clear                   Trul06ds
073000040216     c                   Eval      d06cod = '£6'
073100040216     c                   Movel     PaLna         d06key
073200040216     c                   Movel(p)  Trul06ds      Kpjbu
073300040216     c                   Call      'TRUL06R'
073400040216     c                   Parm                    Kpjba
073500040216     c                   Movel     Kpjbu         Trul06ds
073600040216do  2c                   Do        30            xx
073700040216if  3c                   If        Lin(xx) = *Zeros
073800040216     c                   Leave
073900040216    3c                   EndIf
074000040216if  3c                   If        Lin(xx) <> PaLna
074100040216     c                   Add       1             yy
074200040216     c                   Eval      SkLna(yy) = Lin(xx)
074300040216    3c                   EndIf
074400040216    2c                   EndDo
074500040217      * Ordino la sk
074600040217     c                   Movea     SkLna         wSkLna
074700040217     c                   Clear                   SkLna
074800040217     c                   Sorta     wSkLna
074900040217     c                   Clear                   yy
075000040217do  2c                   Do        250           xx
075100040217if  3c                   If        wSkLna(xx) > *Zeros
075200040217     c                   Add       1             yy
075300040217     c                   Eval      SkLna(yy) = wSkLna(xx)
075400040217    3c                   EndIf
075500040217    2c                   EndDo
075600040216    1c                   EndIf
075700040218
075800040218      * Carico una sk con le date da elaborare
075900040218     c                   Clear                   SkDcm
076000040218     c                   Eval      xx = 1
076100040218     c                   Eval      SkDcm(xx) = PaDcd
076200040218if  1c                   If        PaDcd <> PaDca
076300040218     c                   Eval      wdata = PaDcd
076400040218     c     *Iso          Movel     PaDcd         wdataiso
076500040218do  2c                   Dow       wdata < PaDca
076600040218     c                   Eval      xx = xx + 1
076700040218     c                   Adddur    1:*d          wdataiso
076800040218     c                   Move      wdataiso      wdata
076900040218     c                   Eval      SkDcm(xx) = wdata
077000040218    2c                   EndDo
077100040218    1c                   EndIf
077200041118
077300041118      * Carico uns sk con i tipi bolla da scartare
077400041118     c                   Clear                   skcbo
077500041118     c                   Clear                   xx
077600041118     c                   Eval      TblCod = '3A'
077700041118     c     kTab01        Setll     Tabel00f
077800041118     c                   Do        *Hival
077900041118     c     kTab01        Reade     Tabel00f
078000041118     c                   If        %Eof(Tabel00f)
078100041118     c                   Leave
078200041118     c                   EndIf
078300041118     c                   If        TblFlg <> *Blanks
078400041118     c                   Iter
078500041118     c                   EndIf
078600041118     c                   Eval      ds3a = TblUni
078700041118     c                   If        §3aTb1 = 'F3' or §3aTb1 = 'A3'
078800080707     c                             or §3atb1 = 'AR'
078900041118     c                   Eval      xx = xx + 1
079000041118     c                   Movel     TblKey        skcbo(xx)
079100041118     c                   EndIf
079200041118     c                   EndDo
079300120912
079400120912       //?Controllo se è stato richiesto un cliente
079500120912     c                   eval      *in12 = (PaKsc > 0)
079600120913     c   12              eval      CodKsc = PaKsc
079700120913     c   12              eval      DesKsc = PaRag
079800040216
079900040216     c     kTas          Klist
080000040218     c                   Kfld                    SkDcm(yy)
080100040218     c                   Kfld                    SkLna(xx)
080200040217
080300040217     c     kTaa          Klist
080400040217     c                   Kfld                    TasAas
080500040217     c                   Kfld                    TasLnp
080600040217     c                   Kfld                    TasNrs
080700040217     c                   Kfld                    TasNsp
080800040217     c                   Kfld                    kTaaTrc
080900101229
081000101229     c     kfidst        klist
081100101229     c                   Kfld                    dstnpg
081200101229     c                   kfld                    dstnfv
081300101229     c                   kfld                    dstfgs
081400040217
081500040217     c     kApd          Klist
081600040217     c                   Kfld                    kApdTip
081700040217     c                   Kfld                    TasPdc
081800040513
081900040518     c     kLbl          Klist
082000040518     c                   Kfld                    kLblAa
082100040518     c                   Kfld                    kLblLp
082200040518     c                   Kfld                    kLblNr
082300040518     c                   Kfld                    kLblNs
082400040518
082500040518     c     kLblm         Klist
082600040518     c                   Kfld                    LblAap
082700040518     c                   Kfld                    LblLpp
082800040518     c                   Kfld                    LblNrp
082900040518     c                   Kfld                    LblNsp
083000040518
083100040518     c     kLblmf        Klist
083200040518     c                   Kfld                    LblAap
083300040518     c                   Kfld                    LblLpp
083400040518     c                   Kfld                    LblNrp
083500040518     c                   Kfld                    LblNsp
083600040518     c                   Kfld                    LblAan
083700040518     c                   Kfld                    LblLpn
083800040518     c                   Kfld                    LblNrn
083900040518     c                   Kfld                    LblNsn
084000040715
084100040715     c     ktab          klist
084200040715     c                   kfld                    codut
084300040715     c                   kfld                    tblcod
084400040715     c                   kfld                    tblkey
084500041118
084600041118     c     ktab01        klist
084700041118     c                   kfld                    codut
084800041118     c                   kfld                    tblcod
084900040216
085000040217      * reperisco data e ora
085100040217     c                   time                    w0140
085200040217      * udate in GGMMAAAA
085300040217     c                   Move      w0140         wdate
085400040217      * ora
085500040217     c                   movel     w0140         wora
085600040217      * Data consegna merce dal
085700040217     c                   Clear                   wlbdat
085800040217     c                   Z-add     PaDcd         g02inv
085900040217     c                   Movel     '3'           g02err
086000040217     c                   Call      'XSRDA8'
086100040217     c                   Parm                    wlbdat
086200040217     c                   Z-add     g02dat        DatDcd
086300040217      * Data consegna merce al
086400040217     c                   Clear                   wlbdat
086500040217     c                   Z-add     PaDca         g02inv
086600040217     c                   Movel     '3'           g02err
086700040217     c                   Call      'XSRDA8'
086800040217     c                   Parm                    wlbdat
086900040217     c                   Z-add     g02dat        DatDca
087000040216
087100040216     c                   EndSr
087200040218
087300040218     oQsysprt   e            Testa          1 02
087400040218     o                                        +  20 'ELENCO SPEDIZIONI SCARTATE'
087500040218     o                                        +   1 'MA CON IMMAGINE LDV'
087600040218     o                                        +   1 'dal'
087700040218     o                       datdcd           +   1 '  /  /    '
087800040218     o                                        +   1 'al'
087900040218     o                       datdca           +   1 '  /  /    '
088000040218     o                                          110 'TNSB12R'
088100040218     o                                          128 'PAG.'
088200040218     o                       Page          Z    132
088300040218     o          e            Testa          2
088400040218     o                       Knsif            +   1
088500040218     o                       Knmus            +   1
088600040218     o                       wdate              110 '  /  /    '
088700040218     o                       wora               132 '  :  :  '
088800040218     o          e            Testa          2
088900040218     o                                        +   4 'Spedizione'
089000040218     o                                        +   7 'Cod.bolla'
089100040218     o                                        +   2 'Anomalia'
089200040218     o                                        +   2 'Autotrasportatore'
089300040218     o          e            Bolla       1
089400040218     o                       TasLnp
089500040218     O                       TasNrs        Z  +   1
089600040218     O                       TasNsp        Z  +   1
089700040218     O                       TasAas           +   1
089800040218     O                       TasCbo           +   5
089900040218     O                       TasCca           +   9
090000040218     O                       TasPdc           +   6
090100040630** cmd
090200041021OVRDBF FILE(TITAS26C) TOFILE(
090300040630OVRDBF FILE(TITAA30C) TOFILE(
090400040715CLRPFM FILE(WFLDV00F)
090500110113OVRDBF FILE(FIDST01L) TOFILE(
090600040827OVRDBF FILE(FNLBL02L) TOFILE(
090700060814CLRPFM FILE(WFLDVD0F)
